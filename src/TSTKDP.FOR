

        SUBROUTINE TSTKDP
        USE GLOBALS
        IMPLICIT NONE

        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATCFG.INC'
        INCLUDE 'DATMAC.INC'
        INCLUDE 'DATSUB.INC'
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATSPD.INC'
        INCLUDE 'DATSP1.INC'
        INCLUDE 'DATHGR.INC'

        INTEGER CATLIST


        !

        PRINT *, "Done with Init KDP"
C        CALL GCATLOAD(CATLIST)




        WRITE ( *, '(a)' ) '  Hello, world!'



C	    NRECL = 1
C        LIBLEN = 'LIBLEN/'


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

C       INPUT = "IMTRACE1,,,,,"
C        CALL PROCES

C        INPUT = "PLTIMG,,,,,,"
C        CALL PROCES

C TGR,512.0,,,,,
C  NRD,64.0,,,,,
C  PGR,91.0,,,,,
C  IMTRACE1,,,,,,


C       Provide Continuous Command Line input
        DO WHILE (INPUT.NE."QUIT")

        WRITE( *,2)
2       FORMAT ('> ', $ )
        READ (*, '(a)') INPUT
        PRINT *, "INPUT = ", INPUT
        CALL PROCES
        END DO



C        CALL LLIB

C                        END IF
C

C        CALL LIBLOAD(1)
C        STOP
        END

        SUBROUTINE getOpticalSystemLastSurface(res)
        USE GLOBALS
        IMPLICIT NONE

        INTEGER, intent(inout) :: res

        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATMAI.INC'

        res = INT(SYSTEM(20))

        !PRINT *, "Last Surface index TSTKDP is ", res


        END

        SUBROUTINE getFieldCalcResult(X1, X2, Y, numPts, calcCode)
      ! This routine currently assumes that the AST command was just run and
      ! gets the data from the output arrays.
      IMPLICIT NONE

      integer, intent(in) :: calcCode

      CHARACTER UNN*9,DUNN*12,NNTT1*80,BLNOTE*80,BL20*20
     1,CRANGE*8,B*80,DTY*10,TMY*8,LABX*40,LABY*40
C
      REAL*8 WOR1(0:50),WOR2(0:50),RANGE
     1,FACTY,ORI,DTA11(0:50),DTA22(0:50),DDTA(0:50),ADTA(0:50)
C
      REAL LLIM,ULIM,UFLIM,LFLIM,DELX1,FLDAN(0:50)
C
      COMMON/FIFI/FLDAN
C
      REAL XRAN1,YRAN,YMINJK,XMINJK1,XMAXJK1,YMAXJK
     1,XMINJK2,XMAXJK2,XRAN2,XRAN,XMAXJK,XMINJK,X(1:51)
C
      INTEGER NX,NY,COLPAS,MYJK,DFLAG,I,PNTNUM,NT1ANG,NT1SIZ
C
      COMMON/NUMPNT/PNTNUM,ORI,FACTY
C
      LOGICAL ASTEXT,FLDEXT,DISEXT,FDISEXT
      COMMON/FIELDEXT/ASTEXT,FLDEXT,DISEXT,FDISEXT
C
      COMMON/ABSSS/WOR1,WOR2,DTA11,DTA22,DDTA,ADTA

      REAL, intent(inout) :: X1(0:50), Y(0:50), X2(0:50)
      integer, intent(inout) :: numPts


C
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATHGR.INC'
C

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

        subroutine LogTermFOR(newTxt)
            use handlers, only : updateTerminalLog
            implicit none
            character(len=*) :: newTxt
            call updateTerminalLog(newTxt, "blue")

        end subroutine

        SUBROUTINE getRayTraceOutput(res)
        USE GLOBALS
        IMPLICIT NONE

        REAL*8 res(1:50,0:499)

        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATMAI.INC'

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
              IMPLICIT NONE
              INCLUDE 'DATLEN.INC'

              REAL*8, INTENT(INOUT) :: scaleFactor

              scaleFactor = SCFAY

        END


        SUBROUTINE PROCESKDP(ftext)
          USE GLOBALS
          use strings
          IMPLICIT NONE

        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATCFG.INC'
        INCLUDE 'DATMAC.INC'
        INCLUDE 'DATSUB.INC'
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATSPD.INC'
        INCLUDE 'DATSP1.INC'
        INCLUDE 'DATHGR.INC'

        character(len=*), intent(in) :: ftext
        character(len=80) :: tokens(40)
        integer :: numTokens, i

        !WRITE(OUTLYNE,*) "PROCESKDP START F5 = ", F5
        !CALL SHOWIT(19)

        if(len(ftext) > len(INPUT)) then
            ! Assume this is because of multiple commands.  So split it up
            call parse(ftext, ';', tokens, numTokens) 
            if (i > 0) then 
                  do i = 1,numTokens
                        INPUT = trim(tokens(i))
                        CALL PROCES
                  end do
                  RETURN
            else
                  call LogTermFOR("Warning:  INput string too long!")
            end if
      end if

        INPUT = trim(ftext)
        CALL PROCES

        !WRITE(OUTLYNE,*) "PROCESKDP END F5 = ", F5
        !CALL SHOWIT(19)

        END


