module parax_calcs

contains

  subroutine calcExitPupil(epRadius, epPosition)

        real :: epRadius, epPosition, imgAngle
        integer :: newImage
        include "DATMAI.INC"
        include "DATLEN.INC"

        newImage = NEWIMG

        IF(ALENS(3,(newImage-1)).EQ.0.0D0.OR.ALENS(3,(newImage-1)).NE.0.0D0.AND..NOT.LDIF2) THEN
!       DIST FROM I-1 TO I IS ZERO OF LDIF2 IS FALSE AND DIST NOT 0
!       DO AN INTERNAL "ASTOP EX" ADJUSTMENT
        IF(DABS(PXTRAY(5,newImage)*1.0D-15).LT.DABS(PXTRAY(6,newImage))) THEN
              PRINT *, "EpRadius ASTOP EX Adjustment"
              epPosition=(PXTRAY(5,newImage)/PXTRAY(6,newImage))

        END IF
        IF(DABS(PXTRAY(5,newImage)*1.0D-15).GE.DABS(PXTRAY(6,newImage))) THEN
!       REF RAY IS TELECENTRIC, SET RAD INFINITY
            epPosition=1.0D20
        END IF
!       USE EXISTING THICKNESS OF NEWIMG-1 AS RAD
        ELSE
        Print *, "Use Existing Thickness for epRadius"
        epPosition=ALENS(3,newImage-1)
                        END IF
        !epPosition = 0.0
        imgAngle =  PXTRAY(2,INT(SYSTEM(20)))

        epRadius = ABS(TAN(imgAngle*3.14159265/180.0)*epPosition)

        PRINT *, "epRadius = ", epRadius
        PRINT *, "epPosition = ", epPosition
        PRINT *, "imgAngle = ", imgAngle



        !PRINT *, "ImageNA? ",

  end subroutine



end module
