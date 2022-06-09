

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

C        CALL KDPSUB
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

        INTEGER res

        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATMAI.INC'

        res = INT(SYSTEM(20))

        PRINT *, "Last Surface index is ", res


        END

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

        SUBROUTINE getVIECOScaleFactor(scaleFactor)
              IMPLICIT NONE
              INCLUDE 'DATLEN.INC'

              REAL*8, INTENT(INOUT) :: scaleFactor

              scaleFactor = SCFAY

        END


        SUBROUTINE PROCESKDP(ftext)
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

        character(len=100) :: ftext

        WRITE(OUTLYNE,*) "PROCESKDP START F5 = ", F5
        CALL SHOWIT(19)

        INPUT = trim(ftext)
        CALL PROCES

        WRITE(OUTLYNE,*) "PROCESKDP END F5 = ", F5
        CALL SHOWIT(19)

        END
