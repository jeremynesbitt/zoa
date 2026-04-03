        SUBROUTINE MYGLASS(I)
        use DATLEN
        IMPLICIT NONE
        INTEGER I
C       CONVERTS CATALOG GLASS TO GLASS
C       AUTOMATICALLY
        INCLUDE 'DATMAI.INC'
        GLANAM(I,1)='GLASS'
                        RETURN
                        END
