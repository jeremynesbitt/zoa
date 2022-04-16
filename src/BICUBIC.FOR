        SUBROUTINE BICUBIC(X1,X2,X3,X4,Y1,Y2,F1,F2,F3,F4
     1  ,X,Y,F)
        IMPLICIT NONE
        REAL*8 X1,X2,X3,X4,Y1,Y2,F1,F2,F3,F4,X,Y,F
        REAL*8 M1X,M2X,FX1,FX2,MY
C       X DIRECTION
        M1X=(F2-F1)/(X2-X1)
        M2X=(F4-F3)/(X4-X3)
        FX1=((M1X)*(X-X1))+F1
        FX2=((M2X)*(X-X3))+F3
        MY=(FX2-FX1)/(Y2-Y1)
        F=(MY)*(Y-Y1)+FX1
        RETURN
        END
