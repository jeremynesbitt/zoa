
      SUBROUTINE NEWFIELD
      IMPLICIT NONE
      INTEGER I
      INCLUDE 'DATSUB.INC'
      INCLUDE 'DATMAI.INC'
      INCLUDE 'DATLEN.INC'
                FIELDX(1)=0.0D0
                FIELDX(2)=0.0D0
                FIELDX(3)=0.0D0
                FIELDX(4)=1.0D0
                FIELDX(5)=-1.0D0
                FIELDX(6)=0.0D0
                FIELDX(7)=0.0D0
                FIELDX(8)=0.866D0
                FIELDX(9)=-0.866D0
                FIELDX(10)=0.0D0
                FIELDX(11)=0.0D0
                FIELDX(12)=0.707D0
                FIELDX(13)=-0.707D0
                FIELDX(14)=0.0D0
                FIELDX(15)=0.0D0
                FIELDX(16)=0.5D0
                FIELDX(17)=-0.5D0
                FIELDX(18)=1.0D0
                FIELDX(19)=-1.0D0
                FIELDX(20)=1.0D0
                FIELDX(21)=-1.0D0
                FIELDX(22)=0.866D0
                FIELDX(23)=-0.866D0
                FIELDX(24)=0.866D0
                FIELDX(25)=-0.866D0
                FIELDX(26)=0.707D0
                FIELDX(27)=-0.707D0
                FIELDX(28)=0.707D0
                FIELDX(29)=-0.707D0
                FIELDX(30)=0.5D0
                FIELDX(31)=-0.5D0
                FIELDX(32)=0.5D0
                FIELDX(33)=-0.5D0
                FIELDX(34)=0.612D0
                FIELDX(35)=-0.612D0
                FIELDX(36)=0.612D0
                FIELDX(37)=-0.612D0
                FIELDX(38)=0.354D0
                FIELDX(39)=-0.354D0
                FIELDX(40)=0.354D0
                FIELDX(41)=-0.354D0
                FIELDY(1)=0.0D0
                FIELDY(2)=1.0D0
                FIELDY(3)=-1.0D0
                FIELDY(4)=0.0D0
                FIELDY(5)=0.0D0
                FIELDY(6)=0.866D0
                FIELDY(7)=-0.866D0
                FIELDY(8)=0.0D0
                FIELDY(9)=0.0D0
                FIELDY(10)=0.707D0
                FIELDY(11)=-0.707D0
                FIELDY(12)=0.0D0
                FIELDY(13)=0.D0
                FIELDY(14)=0.5D0
                FIELDY(15)=-0.5D0
                FIELDY(16)=0.0D0
                FIELDY(17)=0.0D0
                FIELDY(18)=1.0D0
                FIELDY(19)=1.0D0
                FIELDY(20)=-1.0D0
                FIELDY(21)=-1.0D0
                FIELDY(22)=0.866D0
                FIELDY(23)=0.866D0
                FIELDY(24)=-0.866D0
                FIELDY(25)=-0.866D0
                FIELDY(26)=0.707D0
                FIELDY(27)=0.707D0
                FIELDY(28)=-0.707D0
                FIELDY(29)=-0.707D0
                FIELDY(30)=0.5D0
                FIELDY(31)=0.5D0
                FIELDY(32)=-0.5D0
                FIELDY(33)=-0.5D0
                FIELDY(34)=0.612D0
                FIELDY(35)=0.612D0
                FIELDY(36)=-0.612D0
                FIELDY(37)=-0.612D0
                FIELDY(38)=0.354D0
                FIELDY(39)=0.354D0
                FIELDY(40)=-0.354D0
                FIELDY(41)=-0.354D0
                FIELDW(1:41)=SYSTEM(11)
                FIELDZ(1:41)=0.0D0
                        RETURN
                        END
      SUBROUTINE NEWRAY
      IMPLICIT NONE
      INTEGER I
      INCLUDE 'DATSUB.INC'
      INCLUDE 'DATMAI.INC'
      INCLUDE 'DATLEN.INC'
          I=41
                RAYW(1:41)=SYSTEM(11)
                RAYW(42:82)=SYSTEM(7)
                RAYW(83:123)=SYSTEM(8)
                RAYX(1)    =0.0D0
                RAYX(1+41) =0.0D0
                RAYX(1+82) =0.0D0
C
                RAYX(2)    =0.0D0
                RAYX(2+41) =0.0D0
                RAYX(2+82) =0.0D0
C
                RAYX(3)    =0.0D0
                RAYX(3+41) =0.0D0
                RAYX(3+82) =0.0D0
C
                RAYX(4)    =1.0D0
                RAYX(4+41) =1.0D0
                RAYX(4+82) =1.0D0
C
                RAYX(5)    =-1.0D0
                RAYX(5+41) =-1.0D0
                RAYX(5+82) =-1.0D0
C
                RAYX(6)    =0.0D0
                RAYX(6+41) =0.0D0
                RAYX(6+82) =0.0D0
C
                RAYX(7)    =0.0D0
                RAYX(7+41) =0.0D0
                RAYX(7+82) =0.0D0
C
                RAYX(8)    =0.866D0
                RAYX(8+41) =0.866D0
                RAYX(8+82) =0.866D0
C
                RAYX(9)    =-0.866D0
                RAYX(9+41) =-0.866D0
                RAYX(9+82) =-0.866D0
C
                RAYX(10)    =0.0D0
                RAYX(10+41) =0.0D0
                RAYX(10+82) =0.0D0
C
                RAYX(11)    =0.0D0
                RAYX(11+41) =0.0D0
                RAYX(11+82) =0.0D0
C
                RAYX(12)    =0.707D0
                RAYX(12+41) =0.707D0
                RAYX(12+82) =0.707D0
C
                RAYX(13)    =-0.707D0
                RAYX(13+41) =-0.707D0
                RAYX(13+82) =-0.707D0
C
                RAYX(14)    =0.0D0
                RAYX(14+41) =0.0D0
                RAYX(14+82) =0.0D0
C
                RAYX(15)    =0.0D0
                RAYX(15+41) =0.0D0
                RAYX(15+82) =0.0D0
C
                RAYX(16)    =0.5D0
                RAYX(16+41) =0.5D0
                RAYX(16+82) =0.5D0
C
                RAYX(17)    =-0.5D0
                RAYX(17+41) =-0.5D0
                RAYX(17+82) =-0.5D0
C
                RAYX(18)    =1.0D0
                RAYX(18+41) =1.0D0
                RAYX(18+82) =1.0D0
C
                RAYX(19)    =-1.0D0
                RAYX(19+41) =-1.0D0
                RAYX(19+82) =-1.0D0
C
                RAYX(20)    =1.0D0
                RAYX(20+41) =1.0D0
                RAYX(20+82) =1.0D0
C
                RAYX(21)    =-1.0D0
                RAYX(21+41) =-1.0D0
                RAYX(21+82) =-1.0D0
C
                RAYX(22)    =0.866D0
                RAYX(22+41) =0.866D0
                RAYX(22+82) =0.866D0
C
                RAYX(23)    =-0.866D0
                RAYX(23+41) =-0.866D0
                RAYX(23+82) =-0.866D0
C
                RAYX(24)    =0.866D0
                RAYX(24+41) =0.866D0
                RAYX(24+82) =0.866D0
C
                RAYX(25)    =-0.866D0
                RAYX(25+41) =-0.866D0
                RAYX(25+82) =-0.866D0
C
                RAYX(26)    =0.707D0
                RAYX(26+41) =0.707D0
                RAYX(26+82) =0.707D0
C
                RAYX(27)    =-0.707D0
                RAYX(27+41) =-0.707D0
                RAYX(27+82) =-0.707D0
C
                RAYX(28)    =0.707D0
                RAYX(28+41) =0.707D0
                RAYX(28+82) =0.707D0
C
                RAYX(29)    =-0.707D0
                RAYX(29+41) =-0.707D0
                RAYX(29+82) =-0.707D0
C
                RAYX(30)    =0.5D0
                RAYX(30+41) =0.5D0
                RAYX(30+82) =0.5D0
C
                RAYX(31)    =-0.5D0
                RAYX(31+41) =-0.5D0
                RAYX(31+82) =-0.5D0
C
                RAYX(32)    =0.5D0
                RAYX(32+41) =0.5D0
                RAYX(32+82) =0.5D0
C
                RAYX(33)    =-0.5D0
                RAYX(33+41) =-0.5D0
                RAYX(33+82) =-0.5D0
C
                RAYX(34)    =0.612D0
                RAYX(34+41) =0.612D0
                RAYX(34+82) =0.612D0
C
                RAYX(35)    =-0.612D0
                RAYX(35+41) =-0.612D0
                RAYX(35+82) =-0.612D0
C
                RAYX(36)    =0.612D0
                RAYX(36+41) =0.612D0
                RAYX(36+82) =0.612D0
C
                RAYX(37)    =-0.612D0
                RAYX(37+41) =-0.612D0
                RAYX(37+82) =-0.612D0
C
                RAYX(38)    =0.354D0
                RAYX(38+41) =0.354D0
                RAYX(38+82) =0.354D0
C
                RAYX(39)    =-0.354D0
                RAYX(39+41) =-0.354D0
                RAYX(39+82) =-0.354D0
C
                RAYX(40)    =0.354D0
                RAYX(40+41) =0.354D0
                RAYX(40+82) =0.354D0
C
                RAYX(41)    =-0.354D0
                RAYX(41+41) =-0.354D0
                RAYX(41+82) =-0.354D0
C
                RAYY(1)    =0.0D0
                RAYY(1+41) =0.0D0
                RAYY(1+82) =0.0D0
C
                RAYY(2)    =1.0D0
                RAYY(2+41) =1.0D0
                RAYY(2+82) =1.0D0
C
                RAYY(3)    =-1.0D0
                RAYY(3+41) =-1.0D0
                RAYY(3+82) =-1.0D0
C
                RAYY(4)    =0.0D0
                RAYY(4+41) =0.0D0
                RAYY(4+82) =0.0D0
C
                RAYY(5)    =0.0D0
                RAYY(5+41) =0.0D0
                RAYY(5+82) =0.0D0
C
                RAYY(6)    =0.866D0
                RAYY(6+41) =0.866D0
                RAYY(6+82) =0.866D0
C
                RAYY(7)    =-0.866D0
                RAYY(7+41) =-0.866D0
                RAYY(7+82) =-0.866D0
C
                RAYY(8)    =0.0D0
                RAYY(8+41) =0.0D0
                RAYY(8+82) =0.0D0
C
                RAYY(9)    =0.0D0
                RAYY(9+41) =0.0D0
                RAYY(9+82) =0.0D0
C
                RAYY(10)    =0.707D0
                RAYY(10+41) =0.707D0
                RAYY(10+82) =0.707D0
C
                RAYY(11)    =-0.707D0
                RAYY(11+41) =-0.707D0
                RAYY(11+82) =-0.707D0
C
                RAYY(12)    =0.0D0
                RAYY(12+41) =0.0D0
                RAYY(12+82) =0.0D0
C
                RAYY(13)    =0.0D0
                RAYY(13+41) =0.0D0
                RAYY(13+82) =0.0D0
C
                RAYY(14)    =0.5D0
                RAYY(14+41) =0.5D0
                RAYY(14+82) =0.5D0
C
                RAYY(15)    =-0.5D0
                RAYY(15+41) =-0.5D0
                RAYY(15+82) =-0.5D0
C
                RAYY(16)    =0.0D0
                RAYY(16+41) =0.0D0
                RAYY(16+82) =0.0D0
C
                RAYY(17)    =0.0D0
                RAYY(17+41) =0.0D0
                RAYY(17+82) =0.0D0
C
                RAYY(18)    =1.0D0
                RAYY(18+41) =1.0D0
                RAYY(18+82) =1.0D0
C
                RAYY(19)    =1.0D0
                RAYY(19+41) =1.0D0
                RAYY(19+82) =1.0D0
C
                RAYY(20)    =-1.0D0
                RAYY(20+41) =-1.0D0
                RAYY(20+82) =-1.0D0
C
                RAYY(21)    =-1.0D0
                RAYY(21+41) =-1.0D0
                RAYY(21+82) =-1.0D0
C
                RAYY(22)    =0.866D0
                RAYY(22+41) =0.866D0
                RAYY(22+82) =0.866D0
C
                RAYY(23)    =0.866D0
                RAYY(23+41) =0.866D0
                RAYY(23+82) =0.866D0
C
                RAYY(24)    =-0.866D0
                RAYY(24+41) =-0.866D0
                RAYY(24+82) =-0.866D0
C
                RAYY(25)    =-0.866D0
                RAYY(25+41) =-0.866D0
                RAYY(25+82) =-0.866D0
C
                RAYY(26)    =0.707D0
                RAYY(26+41) =0.707D0
                RAYY(26+82) =0.707D0
C
                RAYY(27)    =0.707D0
                RAYY(27+41) =0.707D0
                RAYY(27+82) =0.707D0
C
                RAYY(28)    =-0.707D0
                RAYY(28+41) =-0.707D0
                RAYY(28+82) =-0.707D0
C
                RAYY(29)    =-0.707D0
                RAYY(29+41) =-0.707D0
                RAYY(29+82) =-0.707D0
C
                RAYY(30)    =0.5D0
                RAYY(30+41) =0.5D0
                RAYY(30+82) =0.5D0
C
                RAYY(31)    =0.5D0
                RAYY(31+41) =0.5D0
                RAYY(31+82) =0.5D0
C
                RAYY(32)    =-0.5D0
                RAYY(32+41) =-0.5D0
                RAYY(32+82) =-0.5D0
C
                RAYY(33)    =-0.5D0
                RAYY(33+41) =-0.5D0
                RAYY(33+82) =-0.5D0
C
                RAYY(34)    =0.612D0
                RAYY(34+41) =0.612D0
                RAYY(34+82) =0.612D0
C
                RAYY(35)    =0.612D0
                RAYY(35+41) =0.612D0
                RAYY(35+82) =0.612D0
C
                RAYY(36)    =-0.612D0
                RAYY(36+41) =-0.612D0
                RAYY(36+82) =-0.612D0
C
                RAYY(37)    =-0.612D0
                RAYY(37+41) =-0.612D0
                RAYY(37+82) =-0.612D0
C
                RAYY(38)    =0.354D0
                RAYY(38+41) =0.354D0
                RAYY(38+82) =0.354D0
C
                RAYY(39)    =0.354D0
                RAYY(39+41) =0.354D0
                RAYY(39+82) =0.354D0
C
                RAYY(40)    =-0.354D0
                RAYY(40+41) =-0.354D0
                RAYY(40+82) =-0.354D0
C
                RAYY(41)    =-0.354D0
                RAYY(41+41) =-0.354D0
                RAYY(41+82) =-0.354D0
C
                        RETURN
                        END
      SUBROUTINE SELECTKDP(KKDP)
      IMPLICIT NONE
      CHARACTER KKDP*3
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATCFG.INC'
        INCLUDE 'DATMAC.INC'
        INCLUDE 'DATSUB.INC'
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATSPD.INC'
        INCLUDE 'DATSP1.INC'
        IF(F1.EQ.1.AND.F17.EQ.0) THEN
                   KKDP='CMD'
                   RETURN
                   END IF
        IF(F1.EQ.1.AND.F17.EQ.1) THEN
                   KKDP='SPE'
                   RETURN
                   END IF
        IF(F2.EQ.1) THEN
                   KKDP='MAC'
                   RETURN
                   END IF
        IF(F3.EQ.1) THEN
                   KKDP='MED'
                   RETURN
                   END IF
        IF(F5.EQ.1) THEN
                   KKDP='LEN'
                   RETURN
                   END IF
        IF(F6.EQ.1) THEN
                   KKDP='ULN'
                   RETURN
                   END IF
        IF(F7.EQ.1) THEN
                   KKDP='SPS'
                   RETURN
                   END IF
        IF(F8.EQ.1) THEN
                   KKDP='USP'
                   RETURN
                   END IF
        IF(F9.EQ.1) THEN
                   KKDP='FIT'
                   RETURN
                   END IF
        IF(F10.EQ.1) THEN
                   KKDP='CFG'
                   RETURN
                   END IF
        IF(F11.EQ.1) THEN
                   KKDP='UCF'
                   RETURN
                   END IF
        IF(F27.EQ.1) THEN
                   KKDP='MER'
                   RETURN
                   END IF
        IF(F27.EQ.2) THEN
                   KKDP='UMR'
                   RETURN
                   END IF
        IF(F29.EQ.1) THEN
                   KKDP='VAR'
                   RETURN
                   END IF
        IF(F29.EQ.2) THEN
                   KKDP='UVB'
                   RETURN
                   END IF
        IF(F51.EQ.1) THEN
                   KKDP='TVB'
                   RETURN
                   END IF
        IF(F51.EQ.2) THEN
                   KKDP='UTV'
                   RETURN
                   END IF
        IF(F52.EQ.1) THEN
                   KKDP='CMP'
                   RETURN
                   END IF
        IF(F52.EQ.2) THEN
                   KKDP='UCP'
                   RETURN
                   END IF
        IF(F53.EQ.1) THEN
                   KKDP='TOP'
                   RETURN
                   END IF
        IF(F53.EQ.2) THEN
                   KKDP='UTP'
                   RETURN
                   END IF
        IF(F54.EQ.1) THEN
                   KKDP='FOC'
                   RETURN
                   END IF
        IF(F54.EQ.2) THEN
                   KKDP='UFC'
                   RETURN
                   END IF
C     FALL THROUGH OPTION TO CHECK IS MISSED SOMETHING
                   KKDP='CMD'
                        RETURN
                        END
      SUBROUTINE PROGSIZE
      IMPLICIT NONE
        INTEGER N
        CHARACTER*127 CLINE
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATCFG.INC'
        INCLUDE 'DATMAC.INC'
        INCLUDE 'DATSUB.INC'
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATSPD.INC'
        INCLUDE 'DATSP1.INC'
        INCLUDE 'DATHGR.INC'
      WRITE(OUTLYNE,*)'         MAXIMUM LENS SURFACES = ',MAXSUR-1
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'      MAXIMUM NUMBER OF MACROS = ',MAXMAC
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)' MAXIMUM MACRO LINES PER MACRO = ',MAXLIN
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)' MAX# ALTERNATE CONFIGURATIONS = ',MAXCFG
      CALL SHOWIT(1)
      IF(MAXSUR.NE.499) THEN
      OUTLYNE='IN ORDER TO GET'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'500 LENS SURFACES'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'999 MACROS'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'1024 MACRO LINES PER MACRO'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'75 ALTERNATE LENS CONFIGURATIONS'
      CALL SHOWIT(1)
                   END IF
                        RETURN
                        END
      SUBROUTINE CROSS_PRODUCT_C(XP,YP,ZP,X1,Y1,Z1,X2,Y2,Z2)
c     USES A C-UTILITY.C ROUTINE
      IMPLICIT NONE
      INTEGER CrossProduct
c     ml_external CrossProduct
      INTEGER N
      DOUBLE PRECISION A(0:2),B(0:2),C(0:2)
      DOUBLE PRECISION X1,Y1,Z1,X2,Y2,Z2,XP,YP,ZP
      A(0)=X1
      A(1)=Y1
      A(2)=Z1
      B(0)=X2
      B(1)=Y2
      B(2)=Z2
      C(0)=XP
      C(1)=YP
      C(2)=ZP
c     N=CrossProduct(A,B,C)
      XP=C(0)
      YP=C(1)
      ZP=C(2)
      RETURN
      END
      SUBROUTINE CROSS_PRODUCT(XP,YP,ZP,X1,Y1,Z1,X2,Y2,Z2)
      IMPLICIT NONE
      DOUBLE PRECISION X1,Y1,Z1,X2,Y2,Z2,XP,YP,ZP
      XP=(Y1*Z2)-(Z1*Y2)
      YP=(Z1*X2)-(X1*Z2)
      ZP=(X1*Y2)-(Y1*X2)
      RETURN
      END
      SUBROUTINE DOT_PRODUCT(DP,X1,Y1,Z1,X2,Y2,Z2)
      IMPLICIT NONE
      DOUBLE PRECISION X1,Y1,Z1,X2,Y2,Z2,DP
      DP=(X1*X2)+(Y1*Y2)+(Z1*Z2)
      RETURN
      END
