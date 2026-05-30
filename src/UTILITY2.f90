!       SECOND SET OF UTILTIY ROUTINES GO HERE

! SUB SETHED.FOR
SUBROUTINE SETHED
!
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS ROUTINE SETS, CLEARS AND INTERROGATES THE HEADINGS
!       FLAG FOR SINGLE LINE PRINTOUT SUCH AS IN RTG,3
!
!
   IF(SST.EQ.1.OR.SN.EQ.1) THEN
      outlyne=&
      &'"HEADINGS"'
      CALL SHOWIT(1)
      outlyne=&
      &'TAKES NO STRING OR NUMERIC WORD INPUT'
      CALL SHOWIT(1)
      CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
      RETURN
   ELSE
   END IF
   IF(SQ.EQ.1) THEN
      IF(WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
         CALL REPORT_ERROR_AND_FAIL('INVALID QUALIFIER WORD'//'\n'//'RE-ENTER COMMAND', 1)
         RETURN
      ELSE
!       QUALIFIERS VALID PROCEED
      END IF
!       DO QUALIFIERS HERE
!
      IF(WQ.EQ.'ON') THEN
         HEADIN=.TRUE.
      ELSE
      END IF
!
      IF(WQ.EQ.'OFF') THEN
         HEADIN=.FALSE.
      ELSE
      END IF
!
      RETURN
   ELSE
!       NO QUALIFIERS PROCEED
   END IF
   IF( HEADIN) THEN
!       HEADINGS IS ON
      outlyne='"HEADINGS" IS CURRENTLY SET TO "ON"'
      CALL SHOWIT(1)
      RETURN
   ELSE
!       HEADINGS IS OFF
      outlyne='"HEADINGS" IS CURRENTLY SET TO "OFF"'
      CALL SHOWIT(1)
   END IF
   RETURN
END
! SUB SET.FOR
SUBROUTINE SET
!
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS SUBROUTINE IS USED TO SET A NAMED REGISTER (A THROUGH H)
!       THE ACCUMULATOR (NAMED 'BLANK' OR 'ACC' OR 'X')
!       OR THE 'Y','Z','T','IX','IY','IZ',AND 'IT' REGISTERS, AND
!       THE INDEXING REGISTERS I,ITEST,J AND JTEST.
!       THE INDEXING REGISTERS K,KTEST,L AND LTEST.
!       THE INDEXING REGISTERS M,MTEST,N AND NTEST.
!       TO  SPECIFIC NUMERIC VALUES. IF WQ IS BLANK,
!       THE ACCUMULATOR 'X' IS ASSUMED. IF W1 IS BLANK, ZERO IS STORED.
!       REGISTER MEMORY IS PASSED IS MEMORY COMMON.
!
!       WQ IS USED TO CALL THE REGISTER BY NAME.
!
   CHARACTER ACCWRD*8
!
   real(real64) SVAL
!
   LOGICAL GSTRING
!
   INTEGER ACCSUB,ACCCNT
!
   COMMON/ACCSB/ACCWRD
!
   COMMON/ACCSB2/ACCSUB,ACCCNT
!
!
   IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      WRITE(OUTLYNE,*)&
      &'"SET" ONLY TAKES QUALIFIER, STRING AND NUMERIC WORD #1 INPUT'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SST.EQ.1) THEN
      S1=0
      SN=0
      W1=0.0D0
      DF1=1
   END IF
   IF(SST.EQ.0) THEN
      SVAL=W1
   END IF
   IF(SST.EQ.1) THEN
      GSTRING=.FALSE.
      IF(WS(1:8).EQ.'A       ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'B       ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'C       ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'D       ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'E       ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'F       ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'G       ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'H       ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'I       ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'J       ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'K       ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'L       ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'M       ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'N       ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'X       ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'ACC     ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'Y       ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'Z       ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'T       ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'IX      ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'IY      ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'IZ      ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'IT      ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'ITEST   ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'JTEST   ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'KTEST   ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'LTEST   ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'MTEST   ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'NTEST   ') GSTRING=.TRUE.
      IF(WS(1:8).EQ.'A       ') SVAL=REG(1)
      IF(WS(1:8).EQ.'B       ') SVAL=REG(2)
      IF(WS(1:8).EQ.'C       ') SVAL=REG(3)
      IF(WS(1:8).EQ.'D       ') SVAL=REG(4)
      IF(WS(1:8).EQ.'E       ') SVAL=REG(5)
      IF(WS(1:8).EQ.'F       ') SVAL=REG(6)
      IF(WS(1:8).EQ.'G       ') SVAL=REG(7)
      IF(WS(1:8).EQ.'H       ') SVAL=REG(8)
      IF(WS(1:8).EQ.'I       ') SVAL=REG(17)
      IF(WS(1:8).EQ.'J       ') SVAL=REG(19)
      IF(WS(1:8).EQ.'K       ') SVAL=REG(21)
      IF(WS(1:8).EQ.'L       ') SVAL=REG(22)
      IF(WS(1:8).EQ.'M       ') SVAL=REG(23)
      IF(WS(1:8).EQ.'N       ') SVAL=REG(24)
      IF(WS(1:8).EQ.'X       ') SVAL=REG(9)
      IF(WS(1:8).EQ.'ACC     ') SVAL=REG(9)
      IF(WS(1:8).EQ.'Y       ') SVAL=REG(10)
      IF(WS(1:8).EQ.'Z       ') SVAL=REG(11)
      IF(WS(1:8).EQ.'T       ') SVAL=REG(12)
      IF(WS(1:8).EQ.'IX      ') SVAL=REG(13)
      IF(WS(1:8).EQ.'IY      ') SVAL=REG(14)
      IF(WS(1:8).EQ.'IZ      ') SVAL=REG(15)
      IF(WS(1:8).EQ.'IT      ') SVAL=REG(16)
      IF(WS(1:8).EQ.'ITEST   ') SVAL=REG(18)
      IF(WS(1:8).EQ.'JTEST   ') SVAL=REG(20)
      IF(WS(1:8).EQ.'KTEST   ') SVAL=REG(25)
      IF(WS(1:8).EQ.'LTEST   ') SVAL=REG(26)
      IF(WS(1:8).EQ.'MTEST   ') SVAL=REG(27)
      IF(WS(1:8).EQ.'NTEST   ') SVAL=REG(28)
   END IF
   IF(SST.EQ.1.AND..NOT.GSTRING) THEN
      WRITE(OUTLYNE,*)&
      &'INVALID SOURCE REGISTER NAME ENTERED'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   W1=SVAL
   IF(DF1.EQ.1.AND..NOT.GSTRING) W1=0.0D0
   IF(ACCSUB.EQ.1) THEN
      IF(WQ.EQ.'ACC'.OR.WQ.EQ.'X'.OR.WQ.EQ.' ') THEN
         WQ=ACCWRD
         ACCCNT=ACCCNT-1
         IF(ACCCNT.EQ.0) ACCSUB=0
      END IF
   END IF
   IF(WQ.EQ.'A') THEN
      REG(1)=W1
   END IF
   IF(WQ.EQ.'B') THEN
      REG(2)=W1
   END IF
   IF(WQ.EQ.'C') THEN
      REG(3)=W1
   END IF
   IF(WQ.EQ.'D') THEN
      REG(4)=W1
   END IF
   IF(WQ.EQ.'E') THEN
      REG(5)=W1
   END IF
   IF(WQ.EQ.'F') THEN
      REG(6)=W1
   END IF
   IF(WQ.EQ.'G') THEN
      REG(7)=W1
   END IF
   IF(WQ.EQ.'H') THEN
      REG(8)=W1
   END IF
   IF(WQ.EQ.'ACC'.OR.WQ.EQ.'        '.OR.WQ.EQ.'X') THEN
      REG(40)=REG(9)
      REG(9)=W1
   END IF
   IF(WQ.EQ.'Y') THEN
      REG(10)=W1
   END IF
   IF(WQ.EQ.'Z') THEN
      REG(11)=W1
   END IF
   IF(WQ.EQ.'T') THEN
      REG(12)=W1
   END IF
   IF(WQ.EQ.'IX') THEN
      REG(30)=REG(13)
      REG(13)=W1
   END IF
   IF(WQ.EQ.'IY') THEN
      REG(14)=W1
   END IF
   IF(WQ.EQ.'IZ') THEN
      REG(15)=W1
   END IF
   IF(WQ.EQ.'IT') THEN
      REG(16)=W1
   END IF
   IF(WQ.EQ.'I') THEN
      REG(17)=W1
   END IF
   IF(WQ.EQ.'ITEST') THEN
      REG(18)=W1
   END IF
   IF(WQ.EQ.'J') THEN
      REG(19)=W1
   END IF
   IF(WQ.EQ.'JTEST') THEN
      REG(20)=W1
   END IF
   IF(WQ.EQ.'K') THEN
      REG(21)=W1
   END IF
   IF(WQ.EQ.'L') THEN
      REG(22)=W1
   END IF
   IF(WQ.EQ.'M') THEN
      REG(23)=W1
   END IF
   IF(WQ.EQ.'N') THEN
      REG(24)=W1
   END IF
   IF(WQ.EQ.'KTEST') THEN
      REG(25)=W1
   END IF
   IF(WQ.EQ.'LTEST') THEN
      REG(26)=W1
   END IF
   IF(WQ.EQ.'MTEST') THEN
      REG(27)=W1
   END IF
   IF(WQ.EQ.'NTEST') THEN
      REG(28)=W1
   END IF
   IF(WQ.NE.'A'.AND.WQ.NE.'B'.AND.WQ.NE.'C'&
   &.AND.WQ.NE.'D'.AND.WQ.NE.'E'.AND.WQ.NE.'F'&
   &.AND.WQ.NE.'G'.AND.WQ.NE.'H'.AND.WQ.NE.'I'&
   &.AND.WQ.NE.'J'.AND.WQ.NE.'ITEST'.AND.WQ.NE.'JTEST'&
   &.AND.WQ.NE.'X'.AND.WQ.NE.'Y'.AND.WQ.NE.'Z'&
   &.AND.WQ.NE.'T'.AND.WQ.NE.'IX'.AND.WQ.NE.'IY'&
   &.AND.WQ.NE.'IZ'.AND.WQ.NE.'IT'&
   &.AND.WQ.NE.'ACC'.AND.&
   &WQ.NE.'        '.AND.WQ.NE.'K'.AND.WQ.NE.'L'.AND.WQ.NE.'M'&
   &.AND.WQ.NE.'N'.AND.WQ.NE.'KTEST'.AND.WQ.NE.'LTEST'.AND.WQ.NE.&
   &'MTEST'.AND.WQ.NE.'NTEST') THEN
      WRITE(OUTLYNE,*)'INVALID TARGET REGISTER NAME ENTERED'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
   ELSE
   END IF
   RETURN
END
! SUB SAGFLT.FOR
SUBROUTINE SAGFLT(I,X,Y,SAG)
!
   use ieee_arithmetic, only: ieee_is_nan
   use DATLEN
   use DATMAI
   use mod_surface
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SAGFLT.FOR. THIS SUBROUTINE IMPLEMENTS
!       FLAT ASPHERIC SURFACE SAG CALCULATIONS.
!       SAG OF SPECIAL SURFACES IS DONE BY A CALL TO SAGSPC.FOR
!
   INTEGER I
!
   real(real64) Z,X,Y,SAG

!
!
!       SURFACE I IS PLANO AND MAY CONTAIN 2ND, 4TH, 6TH, 8TH AND 10TH ORDER
!       ASPHERIC TERMS AND SPECIAL SURFACE SHAPES.
!
   !PRINT *, "SAGCODE in SAGFLT is ", SAGCODE


   IF(surf_array_parity(I) /= 0) CALL SAGARRAY(I,X,Y)
!
   IF(SAGCODE.EQ.0.OR.SAGCODE.EQ.1) THEN
      SAG=((((X**2)+(Y**2)))*surf_asphere_coeff(I, 2))+&
      &((((X**2)+(Y**2))**2)*surf_asphere_coeff(I, 4))+&
      &((((X**2)+(Y**2))**3)*surf_asphere_coeff(I, 6))+&
      &((((X**2)+(Y**2))**4)*surf_asphere_coeff(I, 8))+&
      &((((X**2)+(Y**2))**5)*surf_asphere_coeff(I, 10))+&
      &((((X**2)+(Y**2))**6)*surf_asphere_coeff(I, 12))+&
      &((((X**2)+(Y**2))**7)*surf_asphere_coeff(I, 14))+&
      &((((X**2)+(Y**2))**8)*surf_asphere_coeff(I, 16))+&
      &((((X**2)+(Y**2))**9)*surf_asphere_coeff(I, 18))+&
      &((((X**2)+(Y**2))**10)*surf_asphere_coeff(I, 20))
      ! JN - for Afocal systems this term can
      ! lead to NaN in gfortran hence this call
      if (ieee_is_nan(SAG)) SAG = 0.0D0
      !PRINT *, "X ", X, "Y ", Y
      !PRINT *, "SAG in SAGFLT SAGCODE 0 is ", SAG
      !PRINT *, "10th order ", ((((X**2)+(Y**2))**10)*surf_asphere_coeff(I, 20))

   ELSE
      SAG=0.0D0
   END IF

!       SPECIAL SURFACE ?
   IF(SAGCODE.EQ.0.OR.SAGCODE.EQ.2) THEN
      IF(surf_special_type(I) /= 0.AND.&
      &surf_special_type(I) /= 6.AND.surf_special_type(I) /= 7.AND.&
      &surf_special_type(I) /= 9.AND.surf_special_type(I) /= 10.AND.&
      &surf_special_type(I) /= 12.AND.surf_special_type(I) /= 13.OR.&
      &surf_default_flag(I) == 1) THEN
         CALL SAGSPC(I,X,Y,Z)
         IF(surf_special_type(I) == 24.AND.FTFL01(2,I).EQ.-1.0D0) THEN
            SAG=Z
         ELSE
            SAG=SAG+Z
         END IF
         IF(surf_paraxial_val(I) == 1) SAG=0.0D0
      END IF
   END IF
   RETURN
END
! SUB SAGANA.FOR
SUBROUTINE SAGANA(CX,CY,KX,KY,X,Y,DX,DY,EX,EY,FX,FY,GX,GY &
&,SAG,I)
!
   use DATLEN
   use DATMAI
   use mod_surface
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   real(real64) CX,CY,KX,KY,X,Y,DX,DY,EX,EY,FX,FY,GX,GY &
   &,SAG,R,R1,R3,R2,R4,Z,XX,YY,SAG1,SAG2
!
   INTEGER I
!
   IF(surf_array_parity(I) /= 0) CALL SAGARRAY(I,X,Y)
!
   R1=(DY*((((1.0D0-DX)*(X**2))+((1.0D0+DX)*(Y**2)))**2))
   R2=(EY*((((1.0D0-EX)*(X**2))+((1.0D0+EX)*(Y**2)))**3))
   R3=(FY*((((1.0D0-FX)*(X**2))+((1.0D0+FX)*(Y**2)))**4))
   R4=(GY*((((1.0D0-GX)*(X**2))+((1.0D0+GX)*(Y**2)))**5))
   R=1.0D0-((KX+1.0D0)*(CX**2)*(X**2))-((KY+1.0D0)*(CY**2)*(Y**2))
   IF(R.LT.0.0D0) THEN
      SAG=0.0D0
      RETURN
   END IF
   IF(SAGCODE.EQ.0.OR.SAGCODE.EQ.1) THEN
      SAG=((((CX*(X**2))+(CY*(Y**2)))/(1.0D0+DSQRT(R))))&
      &+R1+R2+R3+R4
   ELSE
      SAG=0.0D0
   END IF
   IF(SAGCODE.EQ.0.OR.SAGCODE.EQ.2) THEN
!       SPECIAL SURFACE ?
      IF(surf_special_type(I) /= 0.AND.&
      &surf_special_type(I) /= 6.AND.surf_special_type(I) /= 7.AND.&
      &surf_special_type(I) /= 9.AND.surf_special_type(I) /= 10.AND.&
      &surf_special_type(I) /= 12.AND.surf_special_type(I) /= 13.OR.&
      &surf_default_flag(I) == 1) THEN
         CALL SAGSPC(I,X,Y,Z)
         IF(surf_special_type(I) == 24.AND.FTFL01(2,I).EQ.-1.0D0) THEN
            SAG=Z
         ELSE
            SAG=SAG+Z
         END IF
         IF(surf_paraxial_val(I) == 1) SAG=0.0D0
      END IF
   END IF
   RETURN
END
! SUB SAGASP.FOR
SUBROUTINE SAGASP(I,X,Y,SAG)
!
   use DATLEN
   use DATMAI
   use mod_surface
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   INTEGER I
!
   real(real64) X,Y,SAG,C2,RHO2,RHO,Z,R
!
   IF(surf_array_parity(I) /= 0) CALL SAGARRAY(I,X,Y)
!
   RHO2=(X**2)+(Y**2)
   RHO=DSQRT(RHO2)
   C2=surf_curvature(I)**2
   R=1.0D0-((surf_conic(I)+1.0D0)*(C2*RHO2))
   IF(R.LT.0.0D0) THEN
      R=1.0D0-((surf_conic(I)+1))
      IF(R.LT.0.0D0) R=0.0D0
      SAG=(1.0D0/surf_curvature(I))/(1.0D0+DSQRT(R))
      RETURN
   END IF
   IF(SAGCODE.EQ.0.OR.SAGCODE.EQ.1) THEN
      SAG=(surf_curvature(I)*RHO2)/&
      &(1.0D0+DSQRT(R))
      IF(surf_is_asphere(I)) THEN
         SAG=SAG+&
         &(surf_asphere_coeff(I, 4)*(RHO**4))&
         &+(surf_asphere_coeff(I, 6)*(RHO**6))&
         &+(surf_asphere_coeff(I, 8)*(RHO**8))&
         &+(surf_asphere_coeff(I, 10)*(RHO**10))&
         &+(surf_asphere_coeff(I, 12)*(RHO**12))&
         &+(surf_asphere_coeff(I, 14)*(RHO**14))&
         &+(surf_asphere_coeff(I, 16)*(RHO**16))&
         &+(surf_asphere_coeff(I, 18)*(RHO**18))&
         &+(surf_asphere_coeff(I, 20)*(RHO**20))
      END IF
   ELSE
      SAG=0.0D0
   END IF
!
!       SPECIAL SURFACE ?
   IF(SAGCODE.EQ.0.OR.SAGCODE.EQ.2) THEN
      IF(surf_special_type(I) /= 0.AND.&
      &surf_special_type(I) /= 6.AND.surf_special_type(I) /= 7.AND.&
      &surf_special_type(I) /= 9.AND.surf_special_type(I) /= 10.AND.&
      &surf_special_type(I) /= 12.AND.surf_special_type(I) /= 13.OR.&
      &surf_default_flag(I) == 1) THEN
         CALL SAGSPC(I,X,Y,Z)
         IF(surf_special_type(I) == 24.AND.FTFL01(2,I).EQ.-1.0D0) THEN
            SAG=Z
         ELSE
            SAG=SAG+Z
         END IF
         IF(surf_paraxial_val(I) == 1) SAG=0.0D0
      END IF
   END IF
   RETURN
END
! SUB SAGINT.FOR
SUBROUTINE SAGINT(I,X,Y,Z,L1,M1,N1)
!       THIS DOES SPECIAL SURFACES NOW
!
   use DATLEN
   use DATMAI
   use mod_surface
   use mod_system, only: sys_units
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   real(real64) C,K,Z,DELTA,ARG,MAXCLAP,DELCLAP &
   &,ARG1,X,Y,ARG2,CX,CY,DX,DY,EX,EY,FX,FY,GX,GY &
   &,KX,KY,L1,M1,N1
   real(real64) X1,X2,Y1,Y2,Z1,Z2,ARGA,ARGB,ARGC,ARGD
!
   INTEGER I,J,GOO,L,M,N
!
   EXTERNAL ARG1,ARG2
!
!
   IF(sys_units().EQ.1.0D0) DELTA=0.001D0/25.4
   IF(sys_units().EQ.2.0D0) DELTA=0.0001D0
   IF(sys_units().EQ.3.0D0) DELTA=0.001D0
   IF(sys_units().EQ.4.0D0) DELTA=0.000001D0
   X1=X-DELTA
   X2=X+DELTA
   Y1=Y-DELTA
   Y2=Y+DELTA
!
!       FLAT SURFACE (MAYBE ASPHERICS AND SPECIAL SURFACE STUFF)
   IF(surf_curvature(I).EQ.0.0D0 &
   &.AND.surf_toric_flag(I) == 0) THEN
      CALL SAGFLT(I,X,Y,Z)
      CALL SAGFLT(I,X,Y2,Z2)
      CALL SAGFLT(I,X,Y1,Z1)
      M1=-(Z2-Z1)/(2.0D0*DELTA)
      CALL SAGFLT(I,X2,Y,Z2)
      CALL SAGFLT(I,X1,Y,Z1)
      L1=-(Z2-Z1)/(2.0D0*DELTA)
      N1=DSQRT(1.0D0-(M1**2)-(L1**2))
      RETURN
   END IF
!       SPHERICAL, CONIC AND ASPHERIC ROTATIONALLY SYMMETRIC SURFACES
   IF(surf_curvature(I).NE.0.0D0.AND.surf_toric_flag(I) == 0) THEN
      C=surf_curvature(I)
      K=surf_conic(I)
      ARG= ARG1(C,K,X,Y)
      ARGA= ARG1(C,K,X1,Y)
      ARGB= ARG1(C,K,X2,Y)
      ARGC= ARG1(C,K,X,Y1)
      ARGD= ARG1(C,K,X,Y2)
      IF(ARG.LT.0.0D0.OR.ARGA.LT.0.0D0.OR.ARGB.LT.0.0D0.OR.&
      &ARGC.LT.0.0D0.OR.ARGD.LT.0.0D0) THEN
         Z=0.0D0
         L1=0.0D0
         M1=0.0D0
         N1=1.0D0
         Z=0.0D0
         RETURN
      ELSE
!                       PROCEED
         CALL SAGASP(I,X,Y,Z)
         CALL SAGASP(I,X,Y2,Z2)
         CALL SAGASP(I,X,Y1,Z1)
         M1=-(Z2-Z1)/(2.0D0*DELTA)
         CALL SAGASP(I,X2,Y,Z2)
         CALL SAGASP(I,X1,Y,Z1)
         L1=-(Z2-Z1)/(2.0D0*DELTA)
         N1=DSQRT(1.0D0-(M1**2)-(L1**2))
         RETURN
      END IF
   END IF
   IF(surf_toric_flag(I) /= 0) THEN
!       SURFACE I IS TOROIDAL AND MAY BE CONIC AND ANAMORPHIC
!       ASPHERIC
      IF(surf_toric_flag(I) == 1) THEN
!       Y-TORIC
         CY=surf_curvature(I)
         KY=surf_conic(I)
         DY=surf_asphere_coeff(I, 4)
         EY=surf_asphere_coeff(I, 6)
         FY=surf_asphere_coeff(I, 8)
         GY=surf_asphere_coeff(I, 10)
         CX=surf_toric_curvature(I)
         KX=surf_anamorphic_conic(I)
         DX=surf_anamorphic_coeff(I, 4)
         EX=surf_anamorphic_coeff(I, 6)
         FX=surf_anamorphic_coeff(I, 8)
         GX=surf_anamorphic_coeff(I, 10)
      END IF
      IF(surf_toric_flag(I) == 2) THEN
!       X-TORIC
         CX=surf_curvature(I)
         KX=surf_conic(I)
         DX=surf_asphere_coeff(I, 4)
         EX=surf_asphere_coeff(I, 6)
         FX=surf_asphere_coeff(I, 8)
         GX=surf_asphere_coeff(I, 10)
         CY=surf_toric_curvature(I)
         KY=surf_anamorphic_conic(I)
         DY=surf_anamorphic_coeff(I, 4)
         EY=surf_anamorphic_coeff(I, 6)
         FY=surf_anamorphic_coeff(I, 8)
         GY=surf_anamorphic_coeff(I, 10)
      END IF
      ARG=ARG2(CX,CY,KX,KY,X,Y)
      ARGA=ARG2(CX,CY,KX,KY,X1,Y)
      ARGB=ARG2(CX,CY,KX,KY,X2,Y)
      ARGC=ARG2(CX,CY,KX,KY,X,Y1)
      ARGD=ARG2(CX,CY,KX,KY,X,Y2)
      IF(ARG.LT.0.0D0.OR.ARGA.LT.0.0D0.OR.ARGB.LT.0.0D0.OR.&
      &ARGC.LT.0.0D0.OR.ARGD.LT.0.0D0) THEN
         Z=0.0D0
         L1=0.0D0
         M1=0.0D0
         N1=1.0D0
         RETURN
      ELSE
!                       PROCEED
         CALL SAGANA(CX,CY,KX,KY,X,Y,DX,DY,EX,EY,FX,FY,GX,GY,Z,I)
         CALL SAGANA(CX,CY,KX,KY,X,Y2,DX,DY,EX,EY,FX,FY,GX,GY,Z2,I)
         CALL SAGANA(CX,CY,KX,KY,X,Y1,DX,DY,EX,EY,FX,FY,GX,GY,Z1,I)
         M1=-(Z2-Z1)/(2.0D0*DELTA)
         CALL SAGANA(CX,CY,KX,KY,X2,Y,DX,DY,EX,EY,FX,FY,GX,GY,Z2,I)
         CALL SAGANA(CX,CY,KX,KY,X1,Y,DX,DY,EX,EY,FX,FY,GX,GY,Z1,I)
         L1=-(Z2-Z1)/(2.0D0*DELTA)
         N1=DSQRT(1.0D0-(M1**2)-(L1**2))
         RETURN
      END IF
   END IF
END
! SUB SAGITT.FOR
SUBROUTINE SAGITT(I,CA,J,SAG,ETERROR)
!
   use DATLEN
   use DATMAI
   use mod_surface
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SAGITT.
!
   EXTERNAL ARG1,ARG2
!
   LOGICAL ETERROR
!
   real(real64) C,K,SAG,ARG,CA,KX,KY &
   &,ARG1,X,Y,ARG2,CX,CY,DX,DY,EX,EY,FX,FY,GX,GY
!
   INTEGER I,J
!
!
   ETERROR=.FALSE.
!
!     I IS THE SURFACE NUMBER
!     CA IS THE HEIGHT FOR THE CALCULATION
!     J=1 FOR YZ PLANE, J=2 FOR XZ PLANE
   IF(J.EQ.1) THEN
      Y=CA
      X=0.0D0
   ELSE
      Y=0.0D0
      X=CA
   END IF
!       FLAT SURFACE (MAYBE ASPHERICS AND SPECIAL SURFACE STUFF)
   IF(surf_curvature(I).EQ.0.0D0 &
   &.AND.surf_toric_flag(I) == 0) THEN
      CALL SAGFLT(I,X,Y,SAG)
      IF(DABS(SAG).LT.1E-15) SAG=0.0D0
      IF(surf_paraxial_val(I) == 1) SAG=0.0D0
      RETURN
   END IF
!       SPHERICAL, CONIC AND ASPHERIC ROTATIONALLY SYMMETRIC SURFACES
   IF(surf_curvature(I).NE.0.0D0.AND.surf_toric_flag(I) == 0) THEN
      C=surf_curvature(I)
      K=surf_conic(I)
      ARG= ARG1(C,K,X,Y)
      IF(ARG.LT.0.0D0) THEN
         WRITE(OUTLYNE,*)&
         &'WARNING: EDGE THICKNESS CALCULATION ERROR'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'EDGE THICKNESS OPERAND VALUE SET TO ZERO'
         CALL SHOWIT(1)
         ETERROR=.TRUE.
         RETURN
      END IF
!                        PROCEED
      CALL SAGASP(I,X,Y,SAG)
      IF(DABS(SAG).LT.1E-15) SAG=0.0D0
      IF(surf_paraxial_val(I) == 1) SAG=0.0D0
      RETURN
   END IF
   IF(surf_toric_flag(I) /= 0) THEN
!       SURFACE I IS TOROIDAL AND MAY BE CONIC AND ANAMORPHIC
!       ASPHERIC
      IF(surf_toric_flag(I) == 1) THEN
!       Y-TORIC
         CY=surf_curvature(I)
         KY=surf_conic(I)
         DY=surf_asphere_coeff(I, 4)
         EY=surf_asphere_coeff(I, 6)
         FY=surf_asphere_coeff(I, 8)
         GY=surf_asphere_coeff(I, 10)
         CX=surf_toric_curvature(I)
         KX=surf_anamorphic_conic(I)
         DX=surf_anamorphic_coeff(I, 4)
         EX=surf_anamorphic_coeff(I, 6)
         FX=surf_anamorphic_coeff(I, 8)
         GX=surf_anamorphic_coeff(I, 10)
      END IF
      IF(surf_toric_flag(I) == 2) THEN
!       X-TORIC
         CX=surf_curvature(I)
         KX=surf_conic(I)
         DX=surf_asphere_coeff(I, 4)
         EX=surf_asphere_coeff(I, 6)
         FX=surf_asphere_coeff(I, 8)
         GX=surf_asphere_coeff(I, 10)
         CY=surf_toric_curvature(I)
         KY=surf_anamorphic_conic(I)
         DY=surf_anamorphic_coeff(I, 4)
         EY=surf_anamorphic_coeff(I, 6)
         FY=surf_anamorphic_coeff(I, 8)
         GY=surf_anamorphic_coeff(I, 10)
      END IF
      ARG=ARG2(CX,CY,KX,KY,X,Y)
      IF(ARG.LT.0.0D0) THEN
         WRITE(OUTLYNE,*)&
         &'WARNING: EDGE THICKNESS CALCULATION ERROR'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'EDGE THICKNESS OPERAND VALUE SET TO ZERO'
         CALL SHOWIT(1)
         ETERROR=.TRUE.
         RETURN
      END IF
!                       PROCEED
      CALL SAGANA(CX,CY,KX,KY,X,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
      IF(DABS(SAG).LT.1E-15) SAG=0.0D0
      IF(surf_paraxial_val(I) == 1) SAG=0.0D0
      RETURN
   END IF
   IF(surf_paraxial_val(I) == 1) SAG=0.0D0
   RETURN
END
! SUB SAGPLT.FOR
SUBROUTINE SAGPLT(I,X,Y,SAG,NO)
!     I IS SURFACE #
!
   use DATLEN
   use DATMAI
   use mod_surface
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SAGPLT. THIS IS THE SUBROUTINE WHICH
!       HANDLES THE SAG FOR SURFACE PROFILE AND CLAP/COBS PLOTTING.
!
   real(real64) C,K,SAG,ARG &
   &,ARG1,X,Y,ARG2,CX,CY,DX,DY,EX,EY,FX,FY,GX,GY &
   &,KX,KY,VAL
!
   INTEGER I,NO
!
   EXTERNAL ARG1,ARG2
!
!
!       FLAT SURFACE MAYBE ASPHERICS OR SPECIAL SURFACE STUFF
   IF(surf_curvature(I).EQ.0.0D0 &
   &.AND.surf_toric_flag(I) == 0) THEN
      CALL SAGFLT(I,X,Y,SAG)
      IF(DABS(SAG).LT.1.0D-15) SAG=0.0D0
      IF(surf_paraxial_val(I) == 1) SAG=0.0D0
      NO=0
   ELSE
!       NOT PLANO WITH ASPHERICS
   END IF
!       SPHERICAL, CONIC AND ASPHERIC ROTATIONALLY SYMMETRIC SURFACES
   IF(surf_curvature(I).NE.0.0D0.AND.surf_toric_flag(I) == 0) THEN
      C=surf_curvature(I)
      K=surf_conic(I)
      ARG= ARG1(C,K,X,Y)
      IF(ARG.LT.0.0D0) THEN
         NO=1
         SAG=0.0D0
      ELSE
      END IF
!                       PROCEED
      CALL SAGASP(I,X,Y,SAG)
      IF(DABS(SAG).LT.1D-15) SAG=0.0D0
      IF(surf_paraxial_val(I) == 1) SAG=0.0D0
      NO=0
   ELSE
!       NOT ROTATIONALLY SYMMETRIC ASPHERIC
   END IF
   IF(surf_toric_flag(I) /= 0) THEN
!       SURFACE I IS TOROIDAL AND MAY BE CONIC AND ANAMORPHIC
!       ASPHERIC
      IF(surf_toric_flag(I) == 1) THEN
!       Y-TORIC
         CY=surf_curvature(I)
         KY=surf_conic(I)
         DY=surf_asphere_coeff(I, 4)
         EY=surf_asphere_coeff(I, 6)
         FY=surf_asphere_coeff(I, 8)
         GY=surf_asphere_coeff(I, 10)
         CX=surf_toric_curvature(I)
         KX=surf_anamorphic_conic(I)
         DX=surf_anamorphic_coeff(I, 4)
         EX=surf_anamorphic_coeff(I, 6)
         FX=surf_anamorphic_coeff(I, 8)
         GX=surf_anamorphic_coeff(I, 10)
      ELSE
      END IF
      IF(surf_toric_flag(I) == 2) THEN
!       X-TORIC
         CX=surf_curvature(I)
         KX=surf_conic(I)
         DX=surf_asphere_coeff(I, 4)
         EX=surf_asphere_coeff(I, 6)
         FX=surf_asphere_coeff(I, 8)
         GX=surf_asphere_coeff(I, 10)
         CY=surf_toric_curvature(I)
         KY=surf_anamorphic_conic(I)
         DY=surf_anamorphic_coeff(I, 4)
         EY=surf_anamorphic_coeff(I, 6)
         FY=surf_anamorphic_coeff(I, 8)
         GY=surf_anamorphic_coeff(I, 10)
      ELSE
      END IF
      ARG=ARG2(CX,CY,KX,KY,X,Y)
      IF(ARG.LT.0.0D0) THEN
         NO=1
      ELSE
      END IF
!                       PROCEED
      CALL SAGANA(CX,CY,KX,KY,X,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
      IF(DABS(SAG).LT.1D-15) SAG=0.0D0
      IF(surf_paraxial_val(I) == 1) SAG=0.0D0
      NO=0
   ELSE
!       NOT ANAMORPHIC ASPHERIC
   END IF
   IF(surf_paraxial_val(I) == 1) SAG=0.0D0
   RETURN
END
! SUB SAGSPC.FOR
SUBROUTINE SAGSPC(I,X,Y,Z)
   USE GLOBALS
!
   use DATLEN
   use DATMAI
   use mod_surface
   use mod_system, only: sys_units, sys_last_surf, sys_wavelength, sys_wl_ref
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!     THIS IS SUBROUTINE SAGSPC.FOR CALCULATES SAG FOR A SPECIAL SURFACE
!
   EXTERNAL FF2,FF3,FF4,FF5
!
   real(real64) XPASS,YPASS,ZPASS
!
   COMMON/SAGPAS/XPASS,YPASS,ZPASS
!
   INTEGER KLI,II,IPASS1

   COMMON/NEFER/KLI
!
   LOGICAL FUN,GERROR,GERROR1,GERROR2,UERROR
!
   COMMON/COMFUN/FUN
!
   EXTERNAL FNZ1
!
   INTEGER I,III
!
   real(real64) X,Y,Z,THETA,R,FF2,FF3,FF4,XX,YY &
   &,AAAX,AAAY,FF5,RRRHO,JK_WAVE,AMP1,OMEGA1X,OMEGA1Y &
   &,AMP2,OMEGA2X,OMEGA2Y &
   &,AMP3,OMEGA3X,OMEGA3Y &
   &,AMP4,OMEGA4X,OMEGA4Y &
   &,AMP5,OMEGA5X,OMEGA5Y
!
!
   INR=surf_inr_value(I)
!
!     SPECIAL SURFACE TYPE 1
   Z=0.0D0
   IF(surf_special_type(I) == 1) THEN
      DO III=9,48
         IF(FTFL01(III,I).EQ.0.0D0.OR.X.EQ.0.0D0.AND.Y.EQ.0.0D0 &
         &.AND.(III-9).EQ.0) THEN
            Z=Z+&
            &FTFL01(III,I)
         ELSE
            Z=Z+&
            &(FTFL01(III,I)*(((DSQRT((X**2)+(Y**2)))**(III-9))))
         END IF
      END DO
      IF(surf_paraxial_val(I) == 1) Z=0.0D0
      RETURN
   END IF
!     SPECIAL SURFACE TYPE 4
   IF(surf_special_type(I) == 4) THEN
      JK_WAVE=sys_wavelength(INT(sys_wl_ref()))
      AMP1=DABS(FTFL01(1,I)*JK_WAVE*0.5D0)
      AMP2=DABS(FTFL01(4,I)*JK_WAVE*0.5D0)
      AMP3=DABS(FTFL01(7,I)*JK_WAVE*0.5D0)
      AMP4=DABS(FTFL01(10,I)*JK_WAVE*0.5D0)
      AMP5=DABS(FTFL01(13,I)*JK_WAVE*0.5D0)
      IF(sys_units().EQ.1) AMP1=(AMP1*1.0D-4/2.54D0)
      IF(sys_units().EQ.2) AMP1=AMP1*1.0D-4
      IF(sys_units().EQ.3) AMP1=AMP1*1.0D-3
      IF(sys_units().EQ.4) AMP1=AMP1*1.0D-6
      IF(sys_units().EQ.1) AMP2=(AMP2*1.0D-4/2.54D0)
      IF(sys_units().EQ.2) AMP2=AMP2*1.0D-4
      IF(sys_units().EQ.3) AMP2=AMP2*1.0D-3
      IF(sys_units().EQ.4) AMP2=AMP2*1.0D-6
      IF(sys_units().EQ.1) AMP3=(AMP3*1.0D-4/2.54D0)
      IF(sys_units().EQ.2) AMP3=AMP3*1.0D-4
      IF(sys_units().EQ.3) AMP3=AMP3*1.0D-3
      IF(sys_units().EQ.4) AMP3=AMP3*1.0D-6
      IF(sys_units().EQ.1) AMP4=(AMP4*1.0D-4/2.54D0)
      IF(sys_units().EQ.2) AMP4=AMP4*1.0D-4
      IF(sys_units().EQ.3) AMP4=AMP4*1.0D-3
      IF(sys_units().EQ.4) AMP4=AMP4*1.0D-6
      IF(sys_units().EQ.1) AMP5=(AMP5*1.0D-4/2.54D0)
      IF(sys_units().EQ.2) AMP5=AMP5*1.0D-4
      IF(sys_units().EQ.3) AMP5=AMP5*1.0D-3
      IF(sys_units().EQ.4) AMP5=AMP5*1.0D-6
      IF(FTFL01(2,I).EQ.0.0D0) THEN
         OMEGA1X=0.0D0
      ELSE
         OMEGA1X=TWOPII/DABS(FTFL01(2,I))
      END IF
      IF(FTFL01(3,I).EQ.0.0D0) THEN
         OMEGA1Y=0.0D0
      ELSE
         OMEGA1Y=TWOPII/DABS(FTFL01(3,I))
      END IF
      IF(FTFL01(5,I).EQ.0.0D0) THEN
         OMEGA2X=0.0D0
      ELSE
         OMEGA2X=TWOPII/DABS(FTFL01(5,I))
      END IF
      IF(FTFL01(6,I).EQ.0.0D0) THEN
         OMEGA2Y=0.0D0
      ELSE
         OMEGA2Y=TWOPII/DABS(FTFL01(6,I))
      END IF
      IF(FTFL01(8,I).EQ.0.0D0) THEN
         OMEGA3X=0.0D0
      ELSE
         OMEGA3X=TWOPII/DABS(FTFL01(8,I))
      END IF
      IF(FTFL01(9,I).EQ.0.0D0) THEN
         OMEGA3Y=0.0D0
      ELSE
         OMEGA3Y=TWOPII/DABS(FTFL01(9,I))
      END IF
      IF(FTFL01(11,I).EQ.0.0D0) THEN
         OMEGA4X=0.0D0
      ELSE
         OMEGA4X=TWOPII/DABS(FTFL01(11,I))
      END IF
      IF(FTFL01(12,I).EQ.0.0D0) THEN
         OMEGA4Y=0.0D0
      ELSE
         OMEGA4Y=TWOPII/DABS(FTFL01(12,I))
      END IF
      IF(FTFL01(14,I).EQ.0.0D0) THEN
         OMEGA5X=0.0D0
      ELSE
         OMEGA5X=TWOPII/DABS(FTFL01(14,I))
      END IF
      IF(FTFL01(15,I).EQ.0.0D0) THEN
         OMEGA5Y=0.0D0
      ELSE
         OMEGA5Y=TWOPII/DABS(FTFL01(15,I))
      END IF
      Z=(AMP1*(DCOS(OMEGA1X*X)*(DCOS(OMEGA1Y*Y))))&
      &+(AMP2*(DCOS(OMEGA2X*X)*(DCOS(OMEGA2Y*Y))))&
      &+(AMP3*(DCOS(OMEGA3X*X)*(DCOS(OMEGA3Y*Y))))&
      &+(AMP4*(DCOS(OMEGA4X*X)*(DCOS(OMEGA4Y*Y))))&
      &+(AMP5*(DCOS(OMEGA5X*X)*(DCOS(OMEGA5Y*Y))))
      IF(surf_paraxial_val(I) == 1) Z=0.0D0
      RETURN
   END IF
!
!     SPECIAL SURFACE TYPE 16 FRESNEL
   Z=0.0D0
   IF(surf_special_type(I) == 16) THEN
      IF(surf_curvature(I).EQ.0.0D0) THEN
         Z=0.0D0
         IF(surf_paraxial_val(I) == 1) Z=0.0D0
      ELSE
         IF(surf_toric_flag(I) == 0) THEN
            RRRHO=DSQRT((X**2)+(Y**2))
            Z=(surf_curvature(I)*(RRRHO**2))/&
            &(1.0D0+DSQRT(1.0D0-(1.0D0+surf_conic(I))*(surf_curvature(I)**2)*(RRRHO**2)))
            IF(surf_paraxial_val(I) == 1) Z=0.0D0
         END IF
         IF(surf_toric_flag(I) == 1) THEN
            RRRHO=X
            Z=(surf_toric_curvature(I)*(RRRHO**2))/&
            &(1.0D0+DSQRT(1.0D0-(1.0D0+surf_anamorphic_conic(I))*(surf_toric_curvature(I)**2)&
            &*(RRRHO**2)))
            IF(surf_paraxial_val(I) == 1) Z=0.0D0
         END IF
         IF(surf_toric_flag(I) == 2) THEN
            RRRHO=Y
            Z=(surf_toric_curvature(I)*(RRRHO**2))/&
            &(1.0D0+DSQRT(1.0D0-(1.0D0+surf_anamorphic_conic(I))*(surf_toric_curvature(I)**2)&
            &*(RRRHO**2)))
            IF(surf_paraxial_val(I) == 1) Z=0.0D0
         END IF
      END IF
      RETURN
   END IF
!
!     SPECIAL SURFACE TYPE 2
   IF(surf_special_type(I) == 2) THEN
      AAAX=X/INR
      AAAY=Y/INR
      R=DSQRT((AAAX**2)+(AAAY**2))
      IF(DABS(AAAY).GE.DABS(((1.0D35)*AAAX))) THEN
         IF(AAAY.GE.0.0D0) THETA=PII/2.0D0
         IF(AAAY.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
      ELSE
         IF(DABS(AAAY).LE.1.0D-15.AND.DABS(AAAX).LE.1.0D-15) THEN
            THETA=0.0D0
         ELSE
            THETA=DATAN2(AAAY,AAAX)
         END IF
         IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
      END IF
      IF(R.EQ.0.0D0) THETA=0.0D0
      Z=0.0D0
      DO III=1,66
         Z=Z+&
         &(FTFL01(III,I)*(FF2(R,THETA,III)))
      END DO
      IF(surf_paraxial_val(I) == 1) Z=0.0D0
      RETURN
   END IF
!
!     SPECIAL SURFACE TYPE 3
   IF(surf_special_type(I) == 3) THEN
      AAAX=X/INR
      AAAY=Y/INR
      R=DSQRT((AAAX**2)+(AAAY**2))
      IF(DABS(AAAY).GE.DABS(((1.0D35)*AAAX))) THEN
         IF(AAAY.GE.0.0D0) THETA=PII/2.0D0
         IF(AAAY.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
      ELSE
         IF(DABS(AAAY).LE.1.0D-15.AND.DABS(AAAX).LE.1.0D-15) THEN
            THETA=0.0D0
         ELSE
            THETA=DATAN2(AAAY,AAAX)
         END IF
         IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
      END IF
      IF(R.EQ.0.0D0) THETA=0.0D0
      Z=0.0D0
      DO III=1,37
         Z=Z+&
         &(FTFL01(III,I)*(FF3(R,THETA,III)))
      END DO
      IF(surf_paraxial_val(I) == 1) Z=0.0D0
      RETURN
   END IF
!
!
!     SPECIAL SURFACE TYPE 22
   IF(surf_special_type(I) == 22) THEN
      XPASS=X
      YPASS=Y
      GERROR=.FALSE.
      CALL GRIDS(5,I,GERROR)
      IF(.NOT.GERROR) GRIDSUNLOADED22(I)=.FALSE.
      IF(GERROR) THEN
         WRITE(OUTLYNE,*)'NO GRID FILE EXISTS FOR THIS SURFACE'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('NO SAG VALUE COULD BE CALCULATED', 1)
         RETURN
      ELSE
      END IF
      Z=Z+ZPASS
      IF(surf_paraxial_val(I) == 1) Z=0.0D0
      RETURN
   END IF
!
!     DEFORM
   IF(surf_default_flag(I) == 1) THEN
      XPASS=X
      YPASS=Y
      GERROR1=.FALSE.
      GERROR2=.FALSE.
      CALL DEFGRIDS(5,I,GERROR1,GERROR2)
      IF(GERROR1) THEN
         WRITE(OUTLYNE,*)'NO FILE EXISTS FOR THIS DEFORMABLE SURFACE'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('NO SAG VALUE COULD BE CALCULATED', 1)
         RETURN
      ELSE
      END IF
      IF(GERROR2) THEN
         WRITE(OUTLYNE,*)'POINT BEYOND DEFINED DEFORMABLE SURFACE BOUNDARY'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('NO SAG VALUE COULD BE CALCULATED', 1)
         RETURN
      ELSE
      END IF
      Z=Z+ZPASS
      IF(surf_paraxial_val(I) == 1) Z=0.0D0
      RETURN
   END IF
!
!
!     SPECIAL SURFACE TYPE 23
   IF(surf_special_type(I) == 23) THEN
      XPASS=X
      YPASS=Y
      IPASS1=3
      CALL SPL23(I,XPASS,YPASS,ZPASS,IPASS1)
      Z=ZPASS
      IF(surf_paraxial_val(I) == 1) Z=0.0D0
      RETURN
   END IF
!
!     SPECIAL SURFACE TYPE 14
   IF(surf_special_type(I) == 14) THEN
      AAAX=X/INR
      AAAY=Y/INR
      R=DSQRT((AAAX**2)+(AAAY**2))
      IF(DABS(AAAY).GE.DABS(((1.0D35)*AAAX))) THEN
         IF(AAAY.GE.0.0D0) THETA=PII/2.0D0
         IF(AAAY.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
      ELSE
         IF(DABS(AAAY).LE.1.0D-15.AND.DABS(AAAX).LE.1.0D-15) THEN
            THETA=0.0D0
         ELSE
            THETA=DATAN2(AAAY,AAAX)
         END IF
         IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
      END IF
      IF(R.EQ.0.0D0) THETA=0.0D0
      Z=0.0D0
      DO III=1,48
         Z=Z+&
         &(FTFL01(III,I)*(FF5(R,THETA,III)))
      END DO
      IF(surf_paraxial_val(I) == 1) Z=0.0D0
      RETURN
   END IF
!
!     SPECIAL SURFACE TYPE 21
   IF(surf_special_type(I) == 21) THEN
      CALL USERSURF(I,X,Y,Z,UERROR)
      IF(UERROR) Z=0.0D0
      IF(surf_paraxial_val(I) == 1) Z=0.0D0
      RETURN
   END IF
!
!     SPECIAL SURFACE TYPE 8
   IF(surf_special_type(I) == 8) THEN
      XX=X
      YY=Y
      Z=0.0D0
      DO III=1,91
         Z=Z+&
         &(FTFL01(III,I)*(FF4(XX,YY,III)))
      END DO
      IF(surf_paraxial_val(I) == 1) Z=0.0D0
      RETURN
   END IF
!
!     SPECIAL SURFACE TYPE 5
!     USER DEFINED SURFACE
   IF(surf_special_type(I) == 5.OR.surf_special_type(I) == 17) THEN
      REG(40)=REG(9)
      REG(9)=X
      REG(10)=Y
!     THIS SURFACE AUTOMATICALLY USES MACRO FUNCTION FUN10
!     FOR THE INITIAL USER FUCTION TO BE CALLED TO EVALUATE
!     THE Z VALUE CORRESPONDING TO THE AY AND AX VALUES.
!     COEFFICIENTS C1 TO C96 ARE USED BY THE USER IN THE
!     MACRO FUNCTION FUN10 AND ANY MACRO FUNCTION CALLED BY FUN10.
!     THEY ARE STORED INITIALLY IN GENERAL PURPOSE STORAGE REGISTERS
!     NUMBERS 301 TO 396 AND ACCESSED BY THE USER VIA RCL COMMANDS
      GPREG(301)=FTFL01(1,I)
      GPREG(302)=FTFL01(2,I)
      GPREG(303)=FTFL01(3,I)
      GPREG(304)=FTFL01(4,I)
      GPREG(305)=FTFL01(5,I)
      GPREG(306)=FTFL01(6,I)
      GPREG(307)=FTFL01(7,I)
      GPREG(308)=FTFL01(8,I)
      GPREG(309)=FTFL01(9,I)
      GPREG(310)=FTFL01(10,I)
      GPREG(311)=FTFL01(11,I)
      GPREG(312)=FTFL01(12,I)
      GPREG(313)=FTFL01(13,I)
      GPREG(314)=FTFL01(14,I)
      GPREG(315)=FTFL01(15,I)
      GPREG(316)=FTFL01(16,I)
      GPREG(317)=FTFL01(17,I)
      GPREG(318)=FTFL01(18,I)
      GPREG(319)=FTFL01(19,I)
      GPREG(320)=FTFL01(20,I)
      GPREG(321)=FTFL01(21,I)
      GPREG(322)=FTFL01(22,I)
      GPREG(323)=FTFL01(23,I)
      GPREG(324)=FTFL01(24,I)
      GPREG(325)=FTFL01(25,I)
      GPREG(326)=FTFL01(26,I)
      GPREG(327)=FTFL01(27,I)
      GPREG(328)=FTFL01(28,I)
      GPREG(329)=FTFL01(29,I)
      GPREG(330)=FTFL01(30,I)
      GPREG(331)=FTFL01(31,I)
      GPREG(332)=FTFL01(32,I)
      GPREG(333)=FTFL01(33,I)
      GPREG(334)=FTFL01(34,I)
      GPREG(335)=FTFL01(35,I)
      GPREG(336)=FTFL01(36,I)
      GPREG(337)=FTFL01(37,I)
      GPREG(338)=FTFL01(38,I)
      GPREG(339)=FTFL01(39,I)
      GPREG(340)=FTFL01(40,I)
      GPREG(341)=FTFL01(41,I)
      GPREG(342)=FTFL01(42,I)
      GPREG(343)=FTFL01(43,I)
      GPREG(344)=FTFL01(44,I)
      GPREG(345)=FTFL01(45,I)
      GPREG(346)=FTFL01(46,I)
      GPREG(347)=FTFL01(47,I)
      GPREG(348)=FTFL01(48,I)
      GPREG(349)=FTFL01(49,I)
      GPREG(350)=FTFL01(50,I)
      GPREG(351)=FTFL01(51,I)
      GPREG(352)=FTFL01(52,I)
      GPREG(353)=FTFL01(53,I)
      GPREG(354)=FTFL01(54,I)
      GPREG(355)=FTFL01(55,I)
      GPREG(356)=FTFL01(56,I)
      GPREG(357)=FTFL01(57,I)
      GPREG(358)=FTFL01(58,I)
      GPREG(359)=FTFL01(59,I)
      GPREG(360)=FTFL01(60,I)
      GPREG(361)=FTFL01(61,I)
      GPREG(362)=FTFL01(62,I)
      GPREG(363)=FTFL01(63,I)
      GPREG(364)=FTFL01(64,I)
      GPREG(365)=FTFL01(65,I)
      GPREG(366)=FTFL01(66,I)
      GPREG(367)=FTFL01(67,I)
      GPREG(368)=FTFL01(68,I)
      GPREG(369)=FTFL01(69,I)
      GPREG(370)=FTFL01(70,I)
      GPREG(371)=FTFL01(71,I)
      GPREG(372)=FTFL01(72,I)
      GPREG(373)=FTFL01(73,I)
      GPREG(374)=FTFL01(74,I)
      GPREG(375)=FTFL01(75,I)
      GPREG(376)=FTFL01(76,I)
      GPREG(377)=FTFL01(77,I)
      GPREG(378)=FTFL01(78,I)
      GPREG(379)=FTFL01(79,I)
      GPREG(380)=FTFL01(80,I)
      GPREG(381)=FTFL01(81,I)
      GPREG(382)=FTFL01(82,I)
      GPREG(383)=FTFL01(83,I)
      GPREG(384)=FTFL01(84,I)
      GPREG(385)=FTFL01(85,I)
      GPREG(386)=FTFL01(86,I)
      GPREG(387)=FTFL01(87,I)
      GPREG(388)=FTFL01(88,I)
      GPREG(389)=FTFL01(89,I)
      GPREG(390)=FTFL01(90,I)
      GPREG(391)=FTFL01(91,I)
      GPREG(392)=FTFL01(92,I)
      GPREG(393)=FTFL01(93,I)
      GPREG(394)=FTFL01(94,I)
      GPREG(395)=FTFL01(95,I)
      GPREG(396)=FTFL01(96,I)
!     NO FUN10 EXISTS, RETURN 0.0D0 AS THE Z CONTRIBUTION
      Z=0.0D0
      RETURN
   END IF
!
   RETURN
END
! SUB ARG1.FOR
!       ROTATIONALLY SYMMETRIC ASPHERIC
FUNCTION ARG1(C,K,X,Y)
!
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   real(real64) X,Y,C,K,RHO,ARG1
!
   RHO=DSQRT((X**2)+(Y**2))
   ARG1=&
   &1.0D0-((K+1.0D0)*(C**2)*(RHO**2))
   RETURN
END
! SUB ARG2.FOR
!       ANAMORPHIC ASPHERIC
FUNCTION ARG2(CX,CY,KX,KY,X,Y)
!
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   real(real64) CX,CY,KX,KY,X,Y &
   &,ARG2,R
!
   R=1.0D0-((KX+1.0D0)*(CX**2)*(X**2))-((KY+1.0D0)*(CY**2)*(Y**2))
   ARG2=R
   RETURN
END
! SUB SAGRET.FOR
SUBROUTINE SAGRET(I,X,Y,Z,SAGERR)

!       THIS DOES SPECIAL SURFACES NOW
!
   use DATLEN
   use DATMAI
   use mod_surface
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SAGRET. THIS IS THE SUBROUTINE WHICH
!       HANDLES THE SAG OF SURFACES IN RAYTRACING. PUT IN TO SUPPORT
!     NON-FLAT OBJECT SURFACES
!
   real(real64) C,K,SAG,DELTA,ARG &
   &,ARG1,X,Y,ARG2,CX,CY,DX,DY,EX,EY,FX,FY,GX,GY &
   &,KX,KY,Z
!
   LOGICAL SAGERR
!
   INTEGER I
!
   SAGCODE=0.0D0
   SAGERR=.FALSE.
!
!     FIX POSSIBLE OVERFLOWS AT OBJECT AND OTHER SURFACES
!
   IF(DABS(X).GT.1.0D35.OR.DABS(Y).GT.1.0D35) THEN
      SAG=0.0D0
      RETURN
   END IF
!
!       FLAT SURFACE (MAYBE ASPHERICS AND SPECIAL SURFACE STUFF)
   IF(surf_curvature(I).EQ.0.0D0 &
   &.AND.surf_toric_flag(I) == 0) THEN
      !PRINT *, "SAGRET CALLING SAGFLT"
      CALL SAGFLT(I,X,Y,SAG)
      !PRINT *, "SAG AFTER SAGFLG ", SAG
      IF(DABS(SAG).LT.1E-15) SAG=0.0D0
      Z=SAG
      !if (ieee_is_nan(Z)) Z = 0
      RETURN
   END IF
!       SPHERICAL, CONIC AND ASPHERIC ROTATIONALLY SYMMETRIC SURFACES
   IF(surf_curvature(I).NE.0.0D0.AND.surf_toric_flag(I) == 0) THEN
      C=surf_curvature(I)
      K=surf_conic(I)
      ARG= ARG1(C,K,X,Y)
      IF(ARG.LT.0.0D0) THEN
         Z=0.0D0
         SAGERR=.TRUE.
         RETURN
      END IF
!                       PROCEED
      !PRINT *, "SAGRET CALLING SAGASP"
      CALL SAGASP(I,X,Y,SAG)
      !PRINT *, "SAG AFTER SAGASP ", SAG
      IF(DABS(SAG).LT.1E-15) SAG=0.0D0
      Z=SAG
      !if (ieee_is_nan(Z)) Z = 0
      RETURN
   END IF
   IF(surf_toric_flag(I) /= 0) THEN
!       SURFACE I IS TOROIDAL AND MAY BE CONIC AND ANAMORPHIC
!       ASPHERIC
      IF(surf_toric_flag(I) == 1) THEN
!       Y-TORIC
         CY=surf_curvature(I)
         KY=surf_conic(I)
         DY=surf_asphere_coeff(I, 4)
         EY=surf_asphere_coeff(I, 6)
         FY=surf_asphere_coeff(I, 8)
         GY=surf_asphere_coeff(I, 10)
         CX=surf_toric_curvature(I)
         KX=surf_anamorphic_conic(I)
         DX=surf_anamorphic_coeff(I, 4)
         EX=surf_anamorphic_coeff(I, 6)
         FX=surf_anamorphic_coeff(I, 8)
         GX=surf_anamorphic_coeff(I, 10)
      END IF
      IF(surf_toric_flag(I) == 2) THEN
!       X-TORIC
         CX=surf_curvature(I)
         KX=surf_conic(I)
         DX=surf_asphere_coeff(I, 4)
         EX=surf_asphere_coeff(I, 6)
         FX=surf_asphere_coeff(I, 8)
         GX=surf_asphere_coeff(I, 10)
         CY=surf_toric_curvature(I)
         KY=surf_anamorphic_conic(I)
         DY=surf_anamorphic_coeff(I, 4)
         EY=surf_anamorphic_coeff(I, 6)
         FY=surf_anamorphic_coeff(I, 8)
         GY=surf_anamorphic_coeff(I, 10)
      END IF
      ARG=ARG2(CX,CY,KX,KY,X,Y)
      IF(ARG.LT.0.0D0) THEN
         SAGERR=.TRUE.
         Z=0.0D0
         RETURN
      END IF
!                       PROCEED
      !PRINT *, "SAGRET CALLING SAGANA"
      CALL SAGANA(CX,CY,KX,KY,X,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
      IF(DABS(SAG).LT.1E-15) SAG=0.0D0
      Z=SAG
      !if (ieee_is_nan(Z)) Z = 0
      RETURN
   END IF
END
SUBROUTINE MAX_CLAP_VAL(I,MAXCLAP)
   use DATLEN
   use mod_surface
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   real(real64) MAXCLAP1,MAXCLAP2,MAXCLAP
   INTEGER I
   IF(surf_clap_type(I) == 0.OR.surf_array_parity(I) /= 0) THEN
      MAXCLAP1=DABS(PXTRAY(5,I))+DABS(PXTRAY(1,I))
      MAXCLAP2=DABS(PXTRAX(5,I))+DABS(PXTRAX(1,I))
      MAXCLAP=MAXCLAP2
      IF(MAXCLAP1.GT.MAXCLAP2) MAXCLAP=MAXCLAP1
      RETURN
   END IF
   IF(surf_clap_type(I) == 1.AND.surf_array_parity(I) == 0) THEN
      MAXCLAP1=DABS(surf_clap_dim(I, 1))+DABS(surf_clap_dim(I, 3))
      MAXCLAP2=DABS(surf_clap_dim(I, 1))+DABS(surf_clap_dim(I, 4))
      MAXCLAP=MAXCLAP2
      IF(MAXCLAP1.GT.MAXCLAP2) MAXCLAP=MAXCLAP1
      RETURN
   END IF
   IF(surf_clap_type(I) == 5.AND.surf_array_parity(I) == 0) THEN
      MAXCLAP1=DABS(surf_clap_dim(I, 1))+DABS(surf_clap_dim(I, 3))
      MAXCLAP2=DABS(surf_clap_dim(I, 1))+DABS(surf_clap_dim(I, 4))
      MAXCLAP=MAXCLAP2
      IF(MAXCLAP1.GT.MAXCLAP2) MAXCLAP=MAXCLAP1
      RETURN
   END IF
   IF(surf_clap_type(I) == 6.AND.surf_array_parity(I) == 0) THEN
      MAXCLAP1=DABS(surf_clap_dim(I, 2))+DABS(surf_clap_dim(I, 3))
      MAXCLAP2=DABS(surf_clap_dim(I, 2))+DABS(surf_clap_dim(I, 4))
      MAXCLAP=MAXCLAP2
      IF(MAXCLAP1.GT.MAXCLAP2) MAXCLAP=MAXCLAP1
      RETURN
   END IF
   IF(surf_clap_type(I) > 1.AND.surf_clap_type(I) <= 4.AND.&
   &surf_array_parity(I) == 0) THEN
      MAXCLAP1=DABS(surf_clap_dim(I, 1))+DABS(surf_clap_dim(I, 3))
      MAXCLAP2=DABS(surf_clap_dim(I, 2))+DABS(surf_clap_dim(I, 4))
      MAXCLAP=MAXCLAP2
      IF(MAXCLAP1.GT.MAXCLAP2) MAXCLAP=MAXCLAP1
      RETURN
   END IF
END
! SUB SSAAGG.FOR
SUBROUTINE SSAAGG
!       THIS DOES SPECIAL SURFACES NOW
!
   use DATLEN
   use DATMAI
   use mod_surface
   use mod_system, only: sys_units, sys_last_surf, sys_wavelength
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SSAAGG. THIS IS THE SUBROUTINE WHICH
!       HANDLES THE SAG CMD LEVEL COMMAND.
!
   real(real64) C,K,SAG,DELTA,ARG,MAXCLAP,DELCLAP,SSAG &
   &,ARG1,X,Y,ARG2,CX,CY,DX,DY,EX,EY,FX,FY,GX,GY &
   &,KX,KY,S_L,S_M,S_N,DDD
!
   real(real64) VALUE
!
   INTEGER ISAG
   real(real64) SAGMIN,SAGMAX,SAGRMS,SAGMEAN,SAG2MEAN,SAGPTOV
   COMMON/SAGSTUFF/SAGMIN,SAGMAX,SAGRMS,SAGMEAN,ISAG,SAG2MEAN &
   &,SAGPTOV

   LOGICAL EXIS90,OPEN90,ISITIN
!
   INTEGER I,J,NPOINT,GOO,NUM5,L,M,N
!
   COMMON/GV/VALUE,NUM5
!
!
   SAGMAX=-1.0D300
   SAGMIN=1.0D300
   SAGRMS=0.0D0
   SAGMEAN=0.0D0
   SAG2MEAN=0.0D0
!
!       RESTORE TO OLD OBJ,REF AND IMAGE SURFACES
   CALL RESSUR
!
!     GET SAG DONE HERE
!
!       DO GET SAG HERE
   IF(WC.EQ.'GET'.AND.WQ.EQ.'SAG') THEN
!
      IF(INT(W1).LT.NEWOBJ.OR.INT(W1).GT.NEWIMG) THEN
         WRITE(OUTLYNE,*)'FOR "GET SAG"'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)&
         &'SURFACE NUMBER BEYOND LEGAL BOUNDS'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!
      I=INT(W1)
!       FLAT SURFACE (MAYBE ASPHERICS AND SPECIAL SURFACE STUFF)
      IF(surf_curvature(I).EQ.0.0D0 &
      &.AND.surf_toric_flag(I) == 0) THEN
         X=W2
         Y=W3
         CALL SAGFLT(I,X,Y,SAG)
         IF(DABS(SAG).LT.1E-15) SAG=0.0D0
         VALUE=SAG
         CALL GETA
         DDD=SAGDEL
         CALL SAGFLT(I,X+DDD,Y,SAG)
         S_L=SAG
         CALL SAGFLT(I,X-DDD,Y,SAG)
         S_L=S_L-SAG
         S_L=-S_L/(2.0D0*DDD)
         CALL SAGFLT(I,X,Y+DDD,SAG)
         S_M=SAG
         CALL SAGFLT(I,X,Y-DDD,SAG)
         S_M=S_M-SAG
         S_M=-S_M/(2.0D0*DDD)
         S_N=DSQRT(1.0D0-(S_L**2)-(S_M**2))
         REG(12)=S_N
         REG(11)=S_M
         REG(10)=S_L
         RETURN
      END IF
!       SPHERICAL, CONIC AND ASPHERIC ROTATIONALLY SYMMETRIC SURFACES
      IF(surf_curvature(I).NE.0.0D0.AND.surf_toric_flag(I) == 0) THEN
         C=surf_curvature(I)
         K=surf_conic(I)
         X=W2
         Y=W3
         ARG= ARG1(C,K,X,Y)
         IF(ARG.LT.0.0D0) THEN
            WRITE(OUTLYNE,*)&
            &'NO REAL SAG VALUE EXISTS FOR THE GIVEN X AND Y VALUES'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            GOO=1
         END IF
!                       PROCEED
         IF(GOO.NE.1) THEN
            CALL SAGASP(I,X,Y,SAG)
            IF(DABS(SAG).LT.1E-15) SAG=0.0D0
            VALUE=SAG
            CALL GETA
            DDD=SAGDEL
            CALL SAGASP(I,X+DDD,Y,SAG)
            S_L=SAG
            CALL SAGASP(I,X-DDD,Y,SAG)
            S_L=S_L-SAG
            S_L=-S_L/(2.0D0*DDD)
            CALL SAGASP(I,X,Y+DDD,SAG)
            S_M=SAG
            CALL SAGASP(I,X,Y-DDD,SAG)
            S_M=S_M-SAG
            S_M=-S_M/(2.0D0*DDD)
            S_N=DSQRT(1.0D0-(S_L**2)-(S_M**2))
            REG(12)=S_N
            REG(11)=S_M
            REG(10)=S_L
         END IF
         GOO=0
         RETURN
      END IF
      IF(surf_toric_flag(I) /= 0) THEN
!       SURFACE I IS TOROIDAL AND MAY BE CONIC AND ANAMORPHIC
!       ASPHERIC
         IF(surf_toric_flag(I) == 1) THEN
!       Y-TORIC
            CY=surf_curvature(I)
            KY=surf_conic(I)
            DY=surf_asphere_coeff(I, 4)
            EY=surf_asphere_coeff(I, 6)
            FY=surf_asphere_coeff(I, 8)
            GY=surf_asphere_coeff(I, 10)
            CX=surf_toric_curvature(I)
            KX=surf_anamorphic_conic(I)
            DX=surf_anamorphic_coeff(I, 4)
            EX=surf_anamorphic_coeff(I, 6)
            FX=surf_anamorphic_coeff(I, 8)
            GX=surf_anamorphic_coeff(I, 10)
         END IF
         IF(surf_toric_flag(I) == 2) THEN
!       X-TORIC
            CX=surf_curvature(I)
            KX=surf_conic(I)
            DX=surf_asphere_coeff(I, 4)
            EX=surf_asphere_coeff(I, 6)
            FX=surf_asphere_coeff(I, 8)
            GX=surf_asphere_coeff(I, 10)
            CY=surf_toric_curvature(I)
            KY=surf_anamorphic_conic(I)
            DY=surf_anamorphic_coeff(I, 4)
            EY=surf_anamorphic_coeff(I, 6)
            FY=surf_anamorphic_coeff(I, 8)
            GY=surf_anamorphic_coeff(I, 10)
         END IF
         NPOINT=INT((W3-W2)/W4)
         X=W2
         Y=W3
         ARG=ARG2(CX,CY,KX,KY,X,Y)
         IF(ARG.LT.0.0D0) THEN
            WRITE(OUTLYNE,*)&
            &'NO REAL SAG VALUE EXISTS BEYOND THE RANGE ALREADY PRINTED'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'SAG TABLE OUTPUT TERMINATED'
            CALL SHOWIT(1)
            CALL MACFAL
            GOO=1
         END IF
!                       PROCEED
         IF(GOO.NE.1) THEN
            CALL SAGANA(CX,CY,KX,KY,X,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
            IF(DABS(SAG).LT.1E-15) SAG=0.0D0
            VALUE=SAG
            CALL GETA
            DDD=SAGDEL
            CALL SAGANA(CX,CY,KX,KY,X+DDD,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
            S_L=SAG
            CALL SAGANA(CX,CY,KX,KY,X-DDD,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
            S_L=S_L-SAG
            S_L=-S_L/(2.0D0*DDD)
            CALL SAGANA(CX,CY,KX,KY,X,Y+DDD,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
            S_M=SAG
            CALL SAGANA(CX,CY,KX,KY,X,Y-DDD,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
            S_M=S_M-SAG
            S_M=-S_M/(2.0D0*DDD)
            S_N=DSQRT(1.0D0-(S_L**2)-(S_M**2))
            REG(12)=S_N
            REG(11)=S_M
            REG(10)=S_L
         END IF
         GOO=0
         RETURN
      END IF
      RETURN
   END IF
!
!     THE SAG COMMAND
!
   IF(SST.EQ.1) THEN
      WRITE(OUTLYNE,*)&
      &'"',WC(1:3),'"',&
      &' TAKES NO STRING INPUT'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(sys_last_surf().EQ.0.0) THEN
      WRITE(OUTLYNE,*)'LENS SYSTEM HAS NO SURFACES'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!       CHECK QUALIFIERS
   IF(WQ.EQ.'X'.OR.WQ.EQ.'Y'.OR.WQ.EQ.'PT'.OR.WQ.EQ.'FILE'&
   &.OR.WQ.EQ.'PTACC') THEN
!       VALID QUALIFIERS, PROCEED
!       DO DEFAULTS FOR SAG Y AND SAG X
      IF(WQ.EQ.'X'.OR.WQ.EQ.'Y') THEN
         IF(DF1.EQ.1) THEN
            WRITE(OUTLYNE,*)&
            &'"SAG X" AND "SAG Y" REQUIRE EXPLICIT NUMERIC WORD #1 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(DF2.EQ.1) W2=0.0D0
         IF(DF3.EQ.1) THEN
            WRITE(OUTLYNE,*)&
            &'"SAG X" AND "SAG Y" REQUIRE EXPLICIT NUMERIC WORD #3 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(DF4.EQ.1) W4=(DABS(W3)-DABS(W2))/10.0D0
      END IF
!       DO DEFAULTS FOR SAG PT
      IF(WQ.EQ.'PT'.OR.WQ.EQ.'PTACC') THEN
         IF(DF1.EQ.1.OR.DF2.EQ.1.OR.DF3.EQ.1) THEN
            IF(WQ.EQ.'PT')WRITE(OUTLYNE,*)&
            &'"SAG PT" REQUIRES EXPLICIT NUMERIC WORD #1, #2 AND #3 INPUT'
            IF(WQ.EQ.'PTACC')WRITE(OUTLYNE,*)&
            &'"SAG PTACC" REQUIRES EXPLICIT NUMERIC WORD #1, #2 AND #3 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(DF4.EQ.1) W4 = 0.0D0
         IF(DF5.EQ.1) W5 = 0.0D0
         IF(S4.EQ.1.OR.S5.EQ.1) THEN
            WRITE(OUTLYNE,*)&
            &'"SAG PT" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
!       DO DEFAULTS FOR SAG FILE
      IF(WQ.EQ.'FILE') THEN
         IF(DF1.EQ.1) THEN
            WRITE(OUTLYNE,*)&
            &'"SAG FILE" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(W1.LT.0.0D0.OR.W1.GT.sys_last_surf()) THEN
            WRITE(OUTLYNE,*)&
            &'SURFACE NUMBER FOR "SAG FILE" BEYOND LEGAL BOUNDS'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(DF2.EQ.1) W2=25.0D0
         IF(W2.LT.4.0D0) THEN
            WRITE(OUTLYNE,*)&
            &'"N" MUST BE AT LEAST 4, FOR "SAG FILE"'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(DF3.EQ.1) W3 = 0.0D0
         SAGCODE=INT(W3)
         IF(INT(W3).NE.0.AND.INT(W3).NE.1.AND.INT(W3).NE.2) THEN
            WRITE(OUTLYNE,*)&
            &'"CODE" MUST BE 0, 1, OR 2, FOR "SAG FILE"'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(S4.EQ.1) THEN
            WRITE(OUTLYNE,*)&
            &'"SAG FILE" TAKES NO NUMERIC WORD #4 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
!
!       NOW DO SAG X, SAG Y AND SAG PT
      IF(WQ.EQ.'X'.OR.WQ.EQ.'Y') THEN
!
         IF(INT(W1).LT.NEWOBJ.OR.INT(W1).GT.NEWIMG) THEN
            WRITE(OUTLYNE,*)'FOR "SAG Y" AND "SAG X"'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)&
            &'SURFACE NUMBER BEYOND LEGAL BOUNDS'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
!
         IF(W3.LE.W2) THEN
            WRITE(OUTLYNE,*)'FOR "SAG Y" AND "SAG X"'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)&
            &'NUMERIC WORD #3 MUST BE GREATER THAN NUMERIC WORD #2'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
!
         IF((W3-W2).LT.W4) THEN
            WRITE(OUTLYNE,*)'FOR "SAG Y" AND "SAG X"'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)&
            &'NUMERIC WORD #4 MUST BE LESS THAN THE DIFFERENCE BETWEEN'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)&
            &'NUMERIC WORDS #3 AND #2'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
!
         I=INT(W1)
!       FLAT SURFACE MAYBE ASPHERICS OR SPECIAL
         IF(surf_curvature(I).EQ.0.0D0 &
         &.AND.surf_toric_flag(I) == 0) THEN
            NPOINT=INT((W3-W2)/W4)
!       PRINT HEADING
            X=0.0D0
            Y=0.0D0
            SAG=0.0D0
            WRITE(OUTLYNE,100) I
            CALL SHOWIT(0)
            IF(sys_units().EQ.1.0) WRITE(OUTLYNE,23)
            IF(sys_units().EQ.2.0) WRITE(OUTLYNE,24)
            IF(sys_units().EQ.3.0) WRITE(OUTLYNE,25)
            IF(sys_units().EQ.4.0) WRITE(OUTLYNE,33)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,11)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,200)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,11)
            CALL SHOWIT(0)
            IF(WQ.EQ.'X') THEN
               Y=W5
               X=W2
            END IF
            IF(WQ.EQ.'Y') THEN
               X=W5
               Y=W2
            END IF
            DELTA=(W3-W2)/DBLE(NPOINT)
            DO J=1,NPOINT+1
               CALL SAGFLT(I,X,Y,SAG)
               IF(DABS(X).LT.1E-15) X=0.0D0
               IF(DABS(Y).LT.1E-15) Y=0.0D0
               IF(DABS(SAG).LT.1E-15) SAG=0.0D0
               SSAG=SAG
               DDD=SAGDEL
               CALL SAGFLT(I,X+DDD,Y,SAG)
               S_L=SAG
               CALL SAGFLT(I,X-DDD,Y,SAG)
               S_L=S_L-SAG
               S_L=-S_L/(2.0D0*DDD)
               CALL SAGFLT(I,X,Y+DDD,SAG)
               S_M=SAG
               CALL SAGFLT(I,X,Y-DDD,SAG)
               S_M=S_M-SAG
               S_M=-S_M/(2.0D0*DDD)
               S_N=DSQRT(1.0D0-(S_L**2)-(S_M**2))
               WRITE(OUTLYNE,300)X,Y,SSAG,S_L,S_M,S_N
               CALL SHOWIT(0)
               IF(WQ.EQ.'X') X=X+DELTA
               IF(WQ.EQ.'Y') Y=Y+DELTA
            END DO
            RETURN
         END IF
!       SPHERICAL, CONIC AND ASPHERIC ROTATIONALLY SYMMETRIC SURFACES
         IF(surf_curvature(I).NE.0.0D0.AND.surf_toric_flag(I) == 0) THEN
            C=surf_curvature(I)
            K=surf_conic(I)
            NPOINT=INT((W3-W2)/W4)
            X=0.0D0
            Y=0.0D0
            SAG=0.0D0
            IF(WQ.EQ.'X') THEN
               Y=W5
               X=W2
            END IF
            IF(WQ.EQ.'Y') THEN
               X=W5
               Y=W2
            END IF
            WRITE(OUTLYNE,100) I
            CALL SHOWIT(0)
            IF(sys_units().EQ.1.0) WRITE(OUTLYNE,23)
            IF(sys_units().EQ.2.0) WRITE(OUTLYNE,24)
            IF(sys_units().EQ.3.0) WRITE(OUTLYNE,25)
            IF(sys_units().EQ.4.0) WRITE(OUTLYNE,33)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,11)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,200)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,11)
            CALL SHOWIT(0)
!       PRINT HEADING
            DELTA=(W3-W2)/DBLE(NPOINT)
            DO J=1,NPOINT+1
               ARG= ARG1(C,K,X,Y)
               IF(ARG.LT.0.0D0) THEN
                  WRITE(OUTLYNE,*)&
                  &'NO REAL SAG VALUE EXISTS BEYOND THE RANGE ALREADY PRINTED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'SAG TABLE OUTPUT TERMINATED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  GOO=1
               END IF
!                       PROCEED
               IF(GOO.NE.1) THEN
                  CALL SAGASP(I,X,Y,SAG)
                  IF(DABS(X).LT.1E-15) X=0.0D0
                  IF(DABS(Y).LT.1E-15) Y=0.0D0
                  IF(DABS(SAG).LT.1E-15) SAG=0.0D0
                  SSAG=SAG
                  DDD=SAGDEL
                  CALL SAGASP(I,X+DDD,Y,SAG)
                  S_L=SAG
                  CALL SAGASP(I,X-DDD,Y,SAG)
                  S_L=S_L-SAG
                  S_L=-S_L/(2.0D0*DDD)
                  CALL SAGASP(I,X,Y+DDD,SAG)
                  S_M=SAG
                  CALL SAGASP(I,X,Y-DDD,SAG)
                  S_M=S_M-SAG
                  S_M=-S_M/(2.0D0*DDD)
                  S_N=DSQRT(1.0D0-(S_L**2)-(S_M**2))
                  WRITE(OUTLYNE,300)X,Y,SSAG,S_L,S_M,S_N
                  CALL SHOWIT(0)
                  IF(WQ.EQ.'X') X=X+DELTA
                  IF(WQ.EQ.'Y') Y=Y+DELTA
               END IF
               GOO=0
            END DO
            RETURN
         END IF
         IF(surf_toric_flag(I) /= 0) THEN
!       SURFACE I IS TOROIDAL AND MAY BE CONIC AND ANAMORPHIC
!       ASPHERIC
            IF(surf_toric_flag(I) == 1) THEN
!       Y-TORIC
               CY=surf_curvature(I)
               KY=surf_conic(I)
               DY=surf_asphere_coeff(I, 4)
               EY=surf_asphere_coeff(I, 6)
               FY=surf_asphere_coeff(I, 8)
               GY=surf_asphere_coeff(I, 10)
               CX=surf_toric_curvature(I)
               KX=surf_anamorphic_conic(I)
               DX=surf_anamorphic_coeff(I, 4)
               EX=surf_anamorphic_coeff(I, 6)
               FX=surf_anamorphic_coeff(I, 8)
               GX=surf_anamorphic_coeff(I, 10)
            END IF
            IF(surf_toric_flag(I) == 2) THEN
!       X-TORIC
               CX=surf_curvature(I)
               KX=surf_conic(I)
               DX=surf_asphere_coeff(I, 4)
               EX=surf_asphere_coeff(I, 6)
               FX=surf_asphere_coeff(I, 8)
               GX=surf_asphere_coeff(I, 10)
               CY=surf_toric_curvature(I)
               KY=surf_anamorphic_conic(I)
               DY=surf_anamorphic_coeff(I, 4)
               EY=surf_anamorphic_coeff(I, 6)
               FY=surf_anamorphic_coeff(I, 8)
               GY=surf_anamorphic_coeff(I, 10)
            END IF
            NPOINT=INT((W3-W2)/W4)
            X=0.0D0
            Y=0.0D0
            SAG=0.0D0
            IF(WQ.EQ.'X') THEN
               Y=W5
               X=W2
            END IF
            IF(WQ.EQ.'Y') THEN
               X=W5
               Y=W2
            END IF
            WRITE(OUTLYNE,100) I
            CALL SHOWIT(0)
            IF(sys_units().EQ.1.0) WRITE(OUTLYNE,23)
            IF(sys_units().EQ.2.0) WRITE(OUTLYNE,24)
            IF(sys_units().EQ.3.0) WRITE(OUTLYNE,25)
            IF(sys_units().EQ.4.0) WRITE(OUTLYNE,33)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,11)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,200)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,11)
            CALL SHOWIT(0)
!       PRINT HEADING
            DELTA=(W3-W2)/DBLE(NPOINT)
            DO J=1,NPOINT+1
               ARG=ARG2(CX,CY,KX,KY,X,Y)
               IF(ARG.LT.0.0D0) THEN
                  WRITE(OUTLYNE,*)&
                  &'NO REAL SAG VALUE EXISTS BEYOND THE RANGE ALREADY PRINTED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'SAG TABLE OUTPUT TERMINATED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  GOO=1
               END IF
!                       PROCEED
               IF(GOO.NE.1) THEN
                  CALL SAGANA(CX,CY,KX,KY,X,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
                  IF(DABS(X).LT.1E-15) X=0.0D0
                  IF(DABS(Y).LT.1E-15) Y=0.0D0
                  IF(DABS(SAG).LT.1E-15) SAG=0.0D0
                  SSAG=SAG
                  DDD=SAGDEL
                  CALL SAGANA(CX,CY,KX,KY,X+DDD,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
                  S_L=SAG
                  CALL SAGANA(CX,CY,KX,KY,X-DDD,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
                  S_L=S_L-SAG
                  S_L=-S_L/(2.0D0*DDD)
                  CALL SAGANA(CX,CY,KX,KY,X,Y+DDD,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
                  S_M=SAG
                  CALL SAGANA(CX,CY,KX,KY,X,Y-DDD,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
                  S_M=S_M-SAG
                  S_M=-S_M/(2.0D0*DDD)
                  S_N=DSQRT(1.0D0-(S_L**2)-(S_M**2))
                  WRITE(OUTLYNE,300)X,Y,SSAG,S_L,S_M,S_N
                  CALL SHOWIT(0)
                  IF(WQ.EQ.'X') X=X+DELTA
                  IF(WQ.EQ.'Y') Y=Y+DELTA
               END IF
               GOO=0
            END DO
            RETURN
         END IF
      END IF
      IF(WQ.EQ.'PT'.OR.WQ.EQ.'PTACC') THEN
!
         IF(INT(W1).LT.NEWOBJ.OR.INT(W1).GT.NEWIMG) THEN
            WRITE(OUTLYNE,*)'FOR "SAG PT"'
            CALL SHOWIT(0)
            WRITE(OUTLYNE,*)&
            &'SURFACE NUMBER BEYOND LEGAL BOUNDS'
            CALL SHOWIT(0)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(0)
            CALL MACFAL
            RETURN
         END IF
!
         I=INT(W1)
!       FLAT SURFACE MAYBE ASPHERICS AND SPECIAL
         IF(surf_curvature(I).EQ.0.0D0 &
         &.AND.surf_toric_flag(I) == 0) THEN
            X=W2
            Y=W3


            CALL SAGFLT(I,X,Y,SAG)
            IF(DABS(SAG).LT.1E-15) SAG=0.0D0
            DDD=SAGDEL
            CALL SAGFLT(I,X+DDD,Y,SAG)
            S_L=SAG
            CALL SAGFLT(I,X-DDD,Y,SAG)
            S_L=S_L-SAG
            S_L=-S_L/(2.0D0*DDD)
            CALL SAGFLT(I,X,Y+DDD,SAG)
            S_M=SAG
            CALL SAGFLT(I,X,Y-DDD,SAG)
            S_M=S_M-SAG
            S_M=-S_M/(2.0D0*DDD)
            S_N=DSQRT(1.0D0-(S_L**2)-(S_M**2))

            IF(WQ.NE.'PTACC') THEN
               WRITE(OUTLYNE,100) I
               CALL SHOWIT(0)
               IF(sys_units().EQ.1.0) WRITE(OUTLYNE,23)
               IF(sys_units().EQ.2.0) WRITE(OUTLYNE,24)
               IF(sys_units().EQ.3.0) WRITE(OUTLYNE,25)
               IF(sys_units().EQ.4.0) WRITE(OUTLYNE,33)
               CALL SHOWIT(0)
               WRITE(OUTLYNE,301)X
               CALL SHOWIT(0)
               WRITE(OUTLYNE,302)Y
               CALL SHOWIT(0)
               WRITE(OUTLYNE,303)SAG
               CALL SHOWIT(0)
               WRITE(OUTLYNE,304)S_L
               CALL SHOWIT(0)
               WRITE(OUTLYNE,305)S_M
               CALL SHOWIT(0)
               WRITE(OUTLYNE,306)S_N
               CALL SHOWIT(0)
            END IF
            REG(13)=X
            REG(14)=Y
            REG(40)=REG(9)
            REG(9)=SAG
            REG(12)=S_N
            REG(11)=S_M
            REG(10)=S_L
            RETURN
         END IF
!       SPHERICAL, CONIC AND ASPHERIC ROTATIONALLY SYMMETRIC SURFACES
         IF(surf_curvature(I).NE.0.0D0.AND.surf_toric_flag(I) == 0) THEN
            C=surf_curvature(I)
            K=surf_conic(I)
            X=W2
            Y=W3

            ARG= ARG1(C,K,X,Y)
            IF(ARG.LT.0.0D0) THEN
               WRITE(OUTLYNE,*)&
               &'NO REAL SAG VALUE EXISTS FOR THE GIVEN X AND Y VALUES'
               CALL SHOWIT(0)
               WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
               CALL SHOWIT(0)
               CALL MACFAL
               GOO=1
            END IF
!                       PROCEED
            IF(GOO.NE.1) THEN
               CALL SAGASP(I,X,Y,SAG)
               IF(DABS(SAG).LT.1E-15) SAG=0.0D0
               DDD=SAGDEL
               CALL SAGASP(I,X+DDD,Y,SAG)
               S_L=SAG
               CALL SAGASP(I,X-DDD,Y,SAG)
               S_L=S_L-SAG
               S_L=-S_L/(2.0D0*DDD)
               CALL SAGASP(I,X,Y+DDD,SAG)
               S_M=SAG
               CALL SAGASP(I,X,Y-DDD,SAG)
               S_M=S_M-SAG
               S_M=-S_M/(2.0D0*DDD)
               S_N=DSQRT(1.0D0-(S_L**2)-(S_M**2))

               IF(WQ.NE.'PTACC') THEN
                  WRITE(OUTLYNE,100) I
                  CALL SHOWIT(0)
                  IF(sys_units().EQ.1.0) WRITE(OUTLYNE,23)
                  IF(sys_units().EQ.2.0) WRITE(OUTLYNE,24)
                  IF(sys_units().EQ.3.0) WRITE(OUTLYNE,25)
                  IF(sys_units().EQ.4.0) WRITE(OUTLYNE,33)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,301)X
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,302)Y
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,303)SAG
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,304)S_L
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,305)S_M
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,306)S_N
                  CALL SHOWIT(0)
               END IF
               REG(13)=X
               REG(14)=Y
               REG(40)=REG(9)
               REG(9)=SAG
               REG(12)=S_N
               REG(11)=S_M
               REG(10)=S_L
            END IF
            GOO=0
            RETURN
         END IF
         IF(surf_toric_flag(I) /= 0) THEN
!       SURFACE I IS TOROIDAL AND MAY BE CONIC AND ANAMORPHIC
!       ASPHERIC
            IF(surf_toric_flag(I) == 1) THEN
!       Y-TORIC
               CY=surf_curvature(I)
               KY=surf_conic(I)
               DY=surf_asphere_coeff(I, 4)
               EY=surf_asphere_coeff(I, 6)
               FY=surf_asphere_coeff(I, 8)
               GY=surf_asphere_coeff(I, 10)
               CX=surf_toric_curvature(I)
               KX=surf_anamorphic_conic(I)
               DX=surf_anamorphic_coeff(I, 4)
               EX=surf_anamorphic_coeff(I, 6)
               FX=surf_anamorphic_coeff(I, 8)
               GX=surf_anamorphic_coeff(I, 10)
            END IF
            IF(surf_toric_flag(I) == 2) THEN
!       X-TORIC
               CX=surf_curvature(I)
               KX=surf_conic(I)
               DX=surf_asphere_coeff(I, 4)
               EX=surf_asphere_coeff(I, 6)
               FX=surf_asphere_coeff(I, 8)
               GX=surf_asphere_coeff(I, 10)
               CY=surf_toric_curvature(I)
               KY=surf_anamorphic_conic(I)
               DY=surf_anamorphic_coeff(I, 4)
               EY=surf_anamorphic_coeff(I, 6)
               FY=surf_anamorphic_coeff(I, 8)
               GY=surf_anamorphic_coeff(I, 10)
            END IF
            NPOINT=INT((W3-W2)/W4)
            X=W2
            Y=W3

!       PRINT HEADING
            ARG=ARG2(CX,CY,KX,KY,X,Y)
            IF(ARG.LT.0.0D0) THEN
               WRITE(OUTLYNE,*)&
               &'NO REAL SAG VALUE EXISTS BEYOND THE RANGE ALREADY PRINTED'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'SAG TABLE OUTPUT TERMINATED'
               CALL SHOWIT(1)
               CALL MACFAL
               GOO=1
            END IF
!                       PROCEED
            IF(GOO.NE.1) THEN
               CALL SAGANA(CX,CY,KX,KY,X,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
               IF(DABS(SAG).LT.1E-15) SAG=0.0D0
               DDD=SAGDEL
               CALL SAGANA(CX,CY,KX,KY,X+DDD,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
               S_L=SAG
               CALL SAGANA(CX,CY,KX,KY,X-DDD,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
               S_L=S_L-SAG
               S_L=-S_L/(2.0D0*DDD)
               CALL SAGANA(CX,CY,KX,KY,X,Y+DDD,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
               S_M=SAG
               CALL SAGANA(CX,CY,KX,KY,X,Y-DDD,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
               S_M=S_M-SAG
               S_M=-S_M/(2.0D0*DDD)
               S_N=DSQRT(1.0D0-(S_L**2)-(S_M**2))

               IF(WQ.EQ.'PTACC') THEN
                  WRITE(OUTLYNE,100) I
                  CALL SHOWIT(0)
                  IF(sys_units().EQ.1.0) WRITE(OUTLYNE,23)
                  IF(sys_units().EQ.2.0) WRITE(OUTLYNE,24)
                  IF(sys_units().EQ.3.0) WRITE(OUTLYNE,25)
                  IF(sys_units().EQ.4.0) WRITE(OUTLYNE,33)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,301)X
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,302)Y
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,303)SAG
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,304)S_L
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,305)S_M
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,306)S_N
                  CALL SHOWIT(0)
               END IF
               REG(13)=X
               REG(14)=Y
               REG(40)=REG(9)
               REG(9)=SAG
               REG(12)=S_N
               REG(11)=S_M
               REG(10)=S_L
            END IF
            GOO=0
            RETURN
         END IF
      END IF
      IF(WQ.EQ.'FILE') THEN
         EXIS90=.FALSE.
         OPEN90=.FALSE.
         INQUIRE(FILE='SAG.DAT',EXIST=EXIS90)
         INQUIRE(FILE='SAG.DAT',OPENED=OPEN90)
         IF(EXIS90) THEN
            IF(OPEN90) THEN
               CALL CLOSE_FILE(90,0)
            ELSE
               OPEN(UNIT=90,FILE='SAG.DAT',STATUS='UNKNOWN')
               CALL CLOSE_FILE(90,0)
            END IF
         ELSE
!     NO FILE TO GET RID OF
         END IF
!     OPEN THE OUTPUT FILE
         OPEN(UNIT=90,BLANK='NULL'&
         &,FORM='FORMATTED',FILE='SAG.DAT'&
         &,STATUS='UNKNOWN')
!
         I=INT(W1)
         N=INT(W2)
!     SET UP DO LOOP PARAMETERS AND LOOP
!     DETERMINE MAXIMUM CLEAR APERTURE
         CALL MAX_CLAP_VAL(I,MAXCLAP)
         DELCLAP=DABS((2.0D0*MAXCLAP)/DBLE(N-1))
         WRITE(90,*) I,N*N
         Y=-MAXCLAP
         DO M=1,N
            X=-MAXCLAP
            DO L=1,N
!       FLAT SURFACE MAYBE ASPHERICS AND SPECIAL
               IF(surf_curvature(I).EQ.0.0D0 &
               &.AND.surf_toric_flag(I) == 0) THEN
                  CALL SAGFLT(I,X,Y,SAG)
                  IF(DABS(SAG).LT.1E-15) SAG=0.0D0

               END IF
!       SPHERICAL, CONIC AND ASPHERIC ROTATIONALLY SYMMETRIC SURFACES
               IF(surf_curvature(I).NE.0.0D0.AND.surf_toric_flag(I) == 0) THEN
                  C=surf_curvature(I)
                  K=surf_conic(I)
                  ARG= ARG1(C,K,X,Y)
                  IF(ARG.LT.0.0D0) THEN
                     WRITE(OUTLYNE,*)&
                     &'NO REAL SAG VALUE EXISTS FOR THE GIVEN X AND Y VALUES'
                     CALL SHOWIT(0)
                     WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                     CALL SHOWIT(0)
                     CALL MACFAL
                     GOO=1
                  END IF
!                       PROCEED
                  IF(GOO.NE.1) THEN
                     CALL SAGASP(I,X,Y,SAG)
                     IF(DABS(SAG).LT.1E-15) SAG=0.0D0
                  END IF
                  GOO=0
               END IF
               IF(surf_toric_flag(I) /= 0) THEN
!       SURFACE I IS TOROIDAL AND MAY BE CONIC AND ANAMORPHIC
!       ASPHERIC
                  IF(surf_toric_flag(I) == 1) THEN
!       Y-TORIC
                     CY=surf_curvature(I)
                     KY=surf_conic(I)
                     DY=surf_asphere_coeff(I, 4)
                     EY=surf_asphere_coeff(I, 6)
                     FY=surf_asphere_coeff(I, 8)
                     GY=surf_asphere_coeff(I, 10)
                     CX=surf_toric_curvature(I)
                     KX=surf_anamorphic_conic(I)
                     DX=surf_anamorphic_coeff(I, 4)
                     EX=surf_anamorphic_coeff(I, 6)
                     FX=surf_anamorphic_coeff(I, 8)
                     GX=surf_anamorphic_coeff(I, 10)
                  END IF
                  IF(surf_toric_flag(I) == 2) THEN
!       X-TORIC
                     CX=surf_curvature(I)
                     KX=surf_conic(I)
                     DX=surf_asphere_coeff(I, 4)
                     EX=surf_asphere_coeff(I, 6)
                     FX=surf_asphere_coeff(I, 8)
                     GX=surf_asphere_coeff(I, 10)
                     CY=surf_toric_curvature(I)
                     KY=surf_anamorphic_conic(I)
                     DY=surf_anamorphic_coeff(I, 4)
                     EY=surf_anamorphic_coeff(I, 6)
                     FY=surf_anamorphic_coeff(I, 8)
                     GY=surf_anamorphic_coeff(I, 10)
                  END IF
                  NPOINT=INT((W3-W2)/W4)
                  ARG=ARG2(CX,CY,KX,KY,X,Y)
                  IF(ARG.LT.0.0D0) THEN
                     WRITE(OUTLYNE,*)&
                     &'NO REAL SAG VALUE EXISTS BEYOND THE RANGE ALREADY PRINTED'
                     CALL SHOWIT(1)
                     WRITE(OUTLYNE,*)'SAG TABLE OUTPUT TERMINATED'
                     CALL SHOWIT(1)
                     CALL MACFAL
                     GOO=1
                     CALL CLOSE_FILE(90,0)
                     RETURN
                  END IF
!                       PROCEED
                  IF(GOO.NE.1) THEN
                     CALL SAGANA(CX,CY,KX,KY,X,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
                     IF(DABS(SAG).LT.1E-15) SAG=0.0D0
                  END IF
                  GOO=0
               END IF
               CALL SAGCACO(I,X,Y,SAG,ISITIN)
               WRITE(90,102)X,Y,SAG
               IF(SAG.GT.SAGMAX) SAGMAX=SAG
               IF(SAG.LT.SAGMIN) SAGMIN=SAG
               SAGMEAN=SAGMEAN+SAG
               SAG2MEAN=SAG2MEAN+(SAG**2)
               ISAG=ISAG+1
102            FORMAT(D23.15,1X,D23.15,1X,D23.15)
               X=X+DELCLAP
            END DO
            Y=Y+DELCLAP
         END DO
         CALL CLOSE_FILE(90,1)
         SAGRMS=DSQRT((SAG2MEAN-((SAGMEAN**2)/DBLE(ISAG)))/(DBLE(ISAG-1)))
         SAGMEAN=SAGMEAN/DBLE(ISAG)
         IF(sys_units().EQ.1.0D0) THEN
            SAGMIN=SAGMIN*25.4D0*1.0D3
            SAGMAX=SAGMAX*25.4D0*1.0D3
            SAGMEAN=SAGMEAN*25.4D0*1.0D3
            SAGRMS=SAGRMS*25.4D0*1.0D3
         END IF
         IF(sys_units().EQ.2.0D0) THEN
            SAGMIN=SAGMIN*1.0D4
            SAGMAX=SAGMAX*1.0D4
            SAGMEAN=SAGMEAN*1.0D4
            SAGRMS=SAGRMS*1.0D4
         END IF
         IF(sys_units().EQ.3.0D0) THEN
            SAGMIN=SAGMIN*1.0D3
            SAGMAX=SAGMAX*1.0D3
            SAGMEAN=SAGMEAN*1.0D3
            SAGRMS=SAGRMS*1.0D3
         END IF
         IF(sys_units().EQ.4.0D0) THEN
            SAGMIN=SAGMIN*1.0D6
            SAGMAX=SAGMAX*1.0D6
            SAGMEAN=SAGMEAN*1.0D6
            SAGRMS=SAGRMS*1.0D6
         END IF
         SAGPTOV=SAGMAX-SAGMIN
         IF(DF5.EQ.1) THEN
            WRITE(OUTLYNE,103) SAGMIN
            CALL SHOWIT(0)
            WRITE(OUTLYNE,104) SAGMAX
            CALL SHOWIT(0)
            WRITE(OUTLYNE,105) SAGMEAN
            CALL SHOWIT(0)
            WRITE(OUTLYNE,106) SAGRMS
            CALL SHOWIT(0)
            WRITE(OUTLYNE,107) SAGPTOV
            CALL SHOWIT(0)
         END IF
         REG(12)=SAGMEAN
         REG(11)=SAGRMS
         REG(10)=SAGPTOV
103      FORMAT('MINIMUM SAG VALUE IN MICRONS = ',G23.15)
104      FORMAT('MAXIMUM SAG VALUE IN MICRONS = ',G23.15)
105      FORMAT('   MEAN SAG VALUE IN MICRONS = ',G23.15)
106      FORMAT('    RMS SAG VALUE IN MICRONS = ',G23.15)
107      FORMAT('    P-V SAG VALUE IN MICRONS = ',G23.15)
         RETURN
      ELSE
!     NOT 'FILE'
      END IF
   ELSE
!       INVALID QUALIFIER USED WITH SAG
      WRITE(OUTLYNE,*)&
      &'INVALID QUALIFIER WORD USED WITH THE "SAG" COMMAND'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
100 FORMAT('SAG DATA FOR SURFACE NUMBER ',I3)
101 FORMAT('ALL UNITS ARE ',A3)
11 FORMAT(1X)
200 FORMAT(4X,'X',12X,'Y',9X,'SURFACE SAG',5X,'L',12X,'M',12X,'N')
300 FORMAT(G12.5,1X,G12.5,1X,G12.5,1X,G12.5,1X,G12.5,1X,G12.5)
301 FORMAT('  X = ',G13.6)
302 FORMAT('  Y = ',G13.6)
303 FORMAT('SAG = ',G13.6)
304 FORMAT('  L = ',G13.6)
305 FORMAT('  M = ',G13.6)
306 FORMAT('  N = ',G13.6)
23 FORMAT('UNITS = INCHES')
24 FORMAT('UNITS = CENTIMETERS')
25 FORMAT('UNITS = MILIMETERS')
33 FORMAT('UNITS = METERS')
END
SUBROUTINE SAGARRAY(I,X,Y)
   use DATLEN
   use DATMAI
   use mod_surface
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   real(real64) X,Y,DX,DY,XWORKING,YWORKING,N_X,N_Y,SGNX,SGNY
   INTEGER I
   DX=surf_array_dx(I)
   DY=surf_array_dy(I)
   IF(surf_array_parity(I).EQ.-1.0D0) THEN
!       ODD
      N_X=DBLE(NINT(X/DX))
      N_Y=DBLE(NINT(Y/DY))
      XWORKING=X-(N_X*DX)
      YWORKING=Y-(N_Y*DY)
      X=XWORKING
      Y=YWORKING
   END IF
   IF(surf_array_parity(I) == 1) THEN
!       EVEN
      IF(X.EQ.0.0D0) THEN
         SGNX=1.0D0
      ELSE
         SGNX=X/DABS(X)
      END IF
      IF(Y.EQ.0.0D0) THEN
         SGNY=1.0D0
      ELSE
         SGNY=Y/DABS(Y)
      END IF
      N_X=(DBLE(INT(X/DX))*2.0D0)+SGNX
      N_Y=(DBLE(INT(Y/DY))*2.0D0)+SGNY
      XWORKING=X-(N_X*DX/2.0D0)
      YWORKING=Y-(N_Y*DY/2.0D0)
      X=XWORKING
      Y=YWORKING
   END IF
   RETURN
END
! SUB SETBLNI.FOR
SUBROUTINE SETBLNI
!
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       SETS CURRENT INSTRUCTION VALUES AND STATUS INDICATORS
!       TO BLANK
!
   CHARACTER COMMWD_JK*8,QUALWD_JK*8,STRING_JK*80,&
   &ANW_JK*140,ANW1_JK*23,ANW2_JK*23,ANW3_JK*23,&
   &ANW4_JK*23,ANW5_JK*23
!
   real(real64) NW1_JK,NW2_JK,NW3_JK,NW4_JK,&
   &NW5_JK
!
   INTEGER STATNW_JK,STATN1_JK,STATN2_JK,STATN3_JK,&
   &STATN4_JK,STATN5_JK,STATCO_JK,STATC2_JK,&
   &STATBL_JK,STBLK2_JK,STATQL_JK,STATST_JK,SI_JK
!
   COMMON/CBLANK_JK/NW1_JK,NW2_JK,NW3_JK,NW4_JK &
   &,NW5_JK,STATNW_JK,STATN1_JK,STATN2_JK,&
   &STATN3_JK,STATN4_JK,STATN5_JK,STATCO_JK,STATC2_JK,&
   &STATBL_JK,STBLK2_JK,STATQL_JK,&
   &STATST_JK,SI_JK
!
   COMMON/CCBLAN_JK/COMMWD_JK,QUALWD_JK,STRING_JK,&
   &ANW_JK,ANW1_JK,ANW2_JK,ANW3_JK,ANW4_JK,&
   &ANW5_JK
!
!
!
1206 COMMWD_JK=BB
   QUALWD_JK=BB
   STRING_JK=AA//AA//AA//AA
   ANW_JK=AA//AA//AA//AA//AA//AA//AA
   ANW1_JK=AA//'   '
   ANW2_JK=AA//'   '
   ANW3_JK=AA//'   '
   ANW4_JK=AA//'   '
   ANW5_JK=AA//'   '
   NW1_JK=0.0D0
   NW2_JK=0.0D0
   NW3_JK=0.0D0
   NW4_JK=0.0D0
   NW5_JK=0.0D0
   STATNW_JK=0
   STATN1_JK=0
   STATN2_JK=0
   STATN3_JK=0
   STATN4_JK=0
   STATN5_JK=0
   STATQL_JK=0
   STATBL_JK=1
   STBLK2_JK=0
   STATCO_JK=0
   STATC2_JK=0
   STATST_JK=0
   SI_JK=0
   RETURN
END
! SUB SETBLN_JK.FOR
SUBROUTINE SETBLN_JK
!
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       SETS CURRENT INSTRUCTION VALUES AND STATUS INDICATORS
!       TO BLANK
!
   CHARACTER COMMWD_JK*8,QUALWD_JK*8,STRING_JK*80,&
   &ANW_JK*140,ANW1_JK*23,ANW2_JK*23,ANW3_JK*23,&
   &ANW4_JK*23,ANW5_JK*23
!
   real(real64) NW1_JK,NW2_JK,NW3_JK,NW4_JK,&
   &NW5_JK
!
   INTEGER STATNW_JK,STATN1_JK,STATN2_JK,STATN3_JK,&
   &STATN4_JK,STATN5_JK,STATCO_JK,STATC2_JK,&
   &STATBL_JK,STBLK2_JK,STATQL_JK,STATST_JK,SI_JK
!
   COMMON/CBLANK_JK/NW1_JK,NW2_JK,NW3_JK,NW4_JK &
   &,NW5_JK,STATNW_JK,STATN1_JK,STATN2_JK,&
   &STATN3_JK,STATN4_JK,STATN5_JK,STATCO_JK,STATC2_JK,&
   &STATBL_JK,STBLK2_JK,STATQL_JK,&
   &STATST_JK,SI_JK
!
   COMMON/CCBLAN_JK/COMMWD_JK,QUALWD_JK,STRING_JK,&
   &ANW_JK,ANW1_JK,ANW2_JK,ANW3_JK,ANW4_JK,&
   &ANW5_JK
!
!
   COMMWD_JK=BB
   QUALWD_JK=BB
   STRING_JK=AA//AA//AA//AA
   ANW_JK=AA//AA//AA//AA//AA//AA//AA
   ANW1_JK=AA//'   '
   ANW2_JK=AA//'   '
   ANW3_JK=AA//'   '
   ANW4_JK=AA//'   '
   ANW5_JK=AA//'   '
   NW1_JK=0.0D0
   NW2_JK=0.0D0
   NW3_JK=0.0D0
   NW4_JK=0.0D0
   NW5_JK=0.0D0
   STATNW_JK=0
   STATN1_JK=0
   STATN2_JK=0
   STATN3_JK=0
   STATN4_JK=0
   STATN5_JK=0
   STATQL_JK=0
   STATBL_JK=1
   STBLK2_JK=0
   STATCO_JK=0
   STATC2_JK=0
   STATST_JK=0
   SI_JK=0
   RETURN
END
!
SUBROUTINE ATODFAST(REMAIN)
!**********************************************************************
!     THIS SUBROUTINE IS USED FOR CONVERTING CHARACTER REPRESENTATIONS
!     OF real(real64) INPUT TO real(real64) VALUES.
!
!     THE INPUT IS THE CHARACTER*140 VARIABLE "JK_ANW"
!
!     THE TOTAL NUMBER OF EXPECTED VALUES STORED IN INPUT
!     IS 5
!
!     RETURNED VALUES ARE:
!
!               JK_INP(1:5) = CHARACTER REPRESENTATIONS
!     OF EACH OF THE 5 VALUES
!
!               NUM(1:5) = real(real64) VALUES
!
!               JK_DF(1:5) = 0 FOR EXPLICIT INPUT VALUE
!                            1 FOR DEFAULT INPUT VALUE
!                            2 FOR NO INPUT VALUE
!
!              JK_FLG1(1:5) = .TRUE. FOR VALID INPUT
!                             .FALSE. FOR INVALID INPUT
!
!              JK_FLG2        = .TRUE. FOR 5 NUMERIC WORDS ONLY
!                             .FALSE. FOR INPUT BEYOND 5 NUMERIC WORDS
!
!     A VALUE IS INVALID IF IT DOES NOT REPRESENT A NUMERIC VALUE
!
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   INTEGER JK_DF(1:5)
!
   real(real64) JK_NUM(1:5)
!
   CHARACTER JK_ANW*140,JK_INP(1:5)*80,REMAIN*140
!
   LOGICAL JK_FLG1(1:5),JK_FLG2,JK_ERROR

   COMMON/FAST1/JK_ANW,JK_INP
   CHARACTER RA(0:6)*140
   COMMON/FAST7/RA
   COMMON/FAST2/JK_NUM,JK_DF
   COMMON/FAST3/JK_FLG1,JK_FLG2
   COMMON/FAST4/JK_ERROR
!
   CALL BREAKO2(REMAIN)
!
   CALL ATONFAS
!
   RETURN
END
!
SUBROUTINE ATONFAS
!
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   real(real64) JK_NUM(1:5)
!
   INTEGER I,JK_DF(1:5)
!
   CHARACTER JK_ANW*140,JK_INP(1:5)*80,B*140
!
   LOGICAL JK_ERROR,JK_FLG2,JK_FLG1(1:5)
!
   COMMON/FAST1/JK_ANW,JK_INP
   CHARACTER RA(0:6)*140
   COMMON/FAST7/RA
!
   COMMON/FAST2/JK_NUM,JK_DF
!
   COMMON/FAST3/JK_FLG1,JK_FLG2
!
   COMMON/FAST4/JK_ERROR
!
   DO 30 I=1,5
      IF(.NOT.JK_FLG1(I)) THEN
!     EARLIER DETERMINATION OF A BAD INPUT VALUE
         JK_NUM(I)=0.0D0
      ELSE
!     VALUE WAS NOT PREVIOUSLY COUNTED AS BAD
         IF(JK_DF(I).EQ.0) THEN
!
!     CONVERT TO real(real64)
            WRITE(B,10) JK_INP(I)(1:80)
10          FORMAT(A80)
            READ(B,20,ERR=99999) JK_NUM(I)
20          FORMAT(D23.15)
            JK_ERROR=.FALSE.
            JK_FLG1(I)=.TRUE.
            GO TO 30
99999       JK_ERROR=.TRUE.
            JK_FLG1(I)=.FALSE.
            JK_NUM(I)=0.0D0
            JK_INP(I)='0.0'
         ELSE
!     DEFAULT, SET TO 0
            JK_NUM(I)=0.0D0
            JK_FLG1(I)=.TRUE.
            JK_INP(I)='0.0'
         END IF
      END IF
30 CONTINUE
   RETURN
END
!
SUBROUTINE BREAKO2(REMAIN)
!
!     THIS ROUTINE TAKES A 140 CHARACTER VARIABLE NAMED "JK_ANW"
!     AND BREAKS IT INTO UP TO 5 CHARACTER VARIABLES WITH THE BREAKS
!     INDICATED BY A BLANK SPACE OR A COMMA.
!
!     THE RETURNED VARIABLES ARE RETURNED IN THE CHARACTER*80 ARRAY
!     JK_INP(1:20). IF A VALUE WAS RETURNED, THE FLAG JK_FLG(I) IS
!     SET TO .TRUE. ELSE IT IS LEFT AS FALSE.
!
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   real(real64) JK_NUM(1:5)
!
   INTEGER I,J,JK_DF(1:5),JL,JK_N
!
   LOGICAL JK_BLANK,JK_FLG1(1:5),JK_FLG2
!
   CHARACTER JK_ANW*140,REMAIN*140,JK_INP(1:5)*80,BLJK*80
!
   COMMON/FAST1/JK_ANW,JK_INP
   CHARACTER RA(0:6)*140
   COMMON/FAST7/RA
!
   COMMON/FAST2/JK_NUM,JK_DF
!
   COMMON/FAST3/JK_FLG1,JK_FLG2

   COMMON/FAST5/JK_BLANK
!
   COMMON/FAST6/JK_N
!
!
!     INITIALIZE ALL VALUES
   BLJK=AA//AA//AA//AA
   JK_FLG2=.TRUE.
   DO I=1,5
!
!     JK_DF(I)=1 MEANS NO JK_ANW OR DEFAULT INPUT
      JK_DF(I)=1
!
      JK_FLG1(I)=.TRUE.
!
      DO J=1,80
         JK_INP(I)(J:J)=' '
      END DO
   END DO
!
!     STRIP LEADING BLANKS IF ANY
   CALL STPLB2
   REMAIN(1:140)=JK_ANW(1:140)
   IF(JK_BLANK) GO TO 201
!
!     IF A NON-NUMERIC CHARACTER WAS FOUND, JUMP TO 201
   IF(REMAIN(1:1).NE.'+'.AND.&
   &REMAIN(1:1).NE.'-'.AND.&
   &REMAIN(1:1).NE.'0'.AND.&
   &REMAIN(1:1).NE.'1'.AND.&
   &REMAIN(1:1).NE.'2'.AND.&
   &REMAIN(1:1).NE.'3'.AND.&
   &REMAIN(1:1).NE.'4'.AND.&
   &REMAIN(1:1).NE.'5'.AND.&
   &REMAIN(1:1).NE.'6'.AND.&
   &REMAIN(1:1).NE.'7'.AND.&
   &REMAIN(1:1).NE.'8'.AND.&
   &REMAIN(1:1).NE.'9'.AND.&
   &REMAIN(1:1).NE.','.AND.&
   &REMAIN(1:1).NE.'.') GO TO 201
!
!
!     CASE OF EMBEDDED BLANK AS IN 2D 3 REPRESENTING 2D+3
   CALL INSRTP2
   REMAIN(1:140)=JK_ANW(1:140)
!
!     CASE OF MISSING D OR E IN 2-3 REPRESENTING 2D-3
   CALL INSRTD2
   REMAIN(1:140)=JK_ANW(1:140)
!
!     BREAK OUT VALUES
!
   RA(0)=JK_ANW(1:140)
   DO 200 J=1,5
!     STRIP LEADING BLANKS IF ANY
      CALL STPLB2
      REMAIN(1:140)=JK_ANW(1:140)
!
!     IF A NON-NUMERIC CHARACTER WAS FOUND, JUMP TO 201
      IF(REMAIN(1:1).NE.'+'.AND.&
      &REMAIN(1:1).NE.'-'.AND.&
      &REMAIN(1:1).NE.'0'.AND.&
      &REMAIN(1:1).NE.'1'.AND.&
      &REMAIN(1:1).NE.'2'.AND.&
      &REMAIN(1:1).NE.'3'.AND.&
      &REMAIN(1:1).NE.'4'.AND.&
      &REMAIN(1:1).NE.'5'.AND.&
      &REMAIN(1:1).NE.'6'.AND.&
      &REMAIN(1:1).NE.'7'.AND.&
      &REMAIN(1:1).NE.'8'.AND.&
      &REMAIN(1:1).NE.'9'.AND.&
      &REMAIN(1:1).NE.','.AND.&
      &REMAIN(1:1).NE.'.') GO TO 201
!
!     IS THE NEXT VALUE A DEFAULT JK_ANW VALUE
      IF(JK_ANW(1:1).EQ.','.OR.JK_ANW(1:1).EQ.' '.OR.JK_BLANK) THEN
!     THE NEXT VALUE IS DEFALUT
         JK_INP(J)(1:80)=BLJK
         JK_INP(J)(1:23)=' 0.0000000000000000D+00'
         JK_DF(J)=1
!     REBUILD JK_ANW WITHOUT COMMA
         JK_ANW(1:140)=JK_ANW(2:140)
         REMAIN(1:140)=JK_ANW(1:140)
         RA(J)=REMAIN(1:140)
         GO TO 200
      ELSE
!     NEXT VALUE IS NOT DEFAULT, BREAK IT OUT
!
         DO I=2,139
            IF(JK_ANW(I:I).EQ.' '.OR.JK_ANW(I:I).EQ.',') THEN
               JK_INP(J)(1:80)=BLJK
               JK_INP(J)(1:80)=JK_ANW(1:I-1)
               JK_DF(J)=0
               REMAIN(1:140)=JK_ANW(I+1:140)
               JK_ANW(1:140)=JK_ANW(I+1:140)
               RA(J)=REMAIN(1:140)
               GO TO 200
            END IF
         END DO
!
      END IF
!
200 CONTINUE
!
201 CONTINUE
!
!     ALL 5 NUMERIC WORDS ARE BROKEN OUT. IS THERE ANYTHING LEFT
!     NON-BLANK IN JK_ANW(REMAIN).IF SO,SET JK_FLG2 TO FALSE AND RETURN
!     ANYTHING, THAT IS, BEYOND ONE TRAILING COMMA
   DO JL=1,140
      IF(REMAIN(JL:JL).NE.' ') THEN
         JK_FLG2=.FALSE.
      END IF
   END DO
!
   DO J=1,5
!     IF A JK_INP VALUE IS MISSING A DECIMAL POINT, ADD ONE
      JK_N=J
      CALL ADDDE2
   END DO
   DO J=1,5
      IF(JK_FLG1(J)) THEN
!     IF A JK_INP HAS A "D" OR "E", CHECK THE EXPONENT SIZE
         JK_N=J
         CALL DESIZ2
!     NUMBER ALREADY NAN
      END IF
   END DO
!
   DO J=1,5
      IF(JK_FLG1(J)) THEN
         IF(JK_INP(J)(1:1).NE.'(') THEN
!     JK_INP(J) DOES NOT START WITH A LEFT PARENTHESIS
            IF(JK_INP(J)(24:24).NE.' ') THEN
!     JK_INP(J) IS TOO LONG
               JK_FLG1(J)=.FALSE.
               RETURN
!     LENGTH OK
            END IF
!     PARENTHESIS IS THERE
         END IF
      END IF
   END DO
!
   RETURN
END
SUBROUTINE STPLB2
!**********************************************************************
!     SRIP LEADING BLANKS ROUTINE
!**********************************************************************
!
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!     STRIPS OFF LEADING BLANKS FROM INPUT*140
!
   CHARACTER JK_ANW*140,BL*140,JK_INP(1:5)*80
!
   LOGICAL JK_BLANK
!
   INTEGER I
!
   COMMON/FAST1/JK_ANW,JK_INP
   CHARACTER RA(0:6)*140
   COMMON/FAST7/RA
!
   COMMON/FAST5/JK_BLANK
!
   JK_BLANK=.FALSE.
   DO I=1,140
      BL(I:I)=' '
   END DO
!
   I=0
1  IF(ICHAR(JK_ANW(1:1)).EQ.32) THEN
      JK_ANW(1:140)=JK_ANW(2:140)
      IF(JK_ANW(1:140).EQ.BL(1:140)) THEN
         JK_BLANK=.TRUE.
         RETURN
!     PROCEED
      END IF
      I=I+1
      IF(I.LT.140) GO TO 1
      IF(I.GE.140) THEN
         JK_BLANK=.TRUE.
         RETURN
      END IF
!     ALL LEADING BLANKS REMOVED
   END IF
   RETURN
END
SUBROUTINE INSRTP2
!**********************************************************************
!     ADD MISSING PLUS SIGN ROUTINE
!**********************************************************************
!
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   CHARACTER JK_ANW*140,JK_INP(1:5)*80
!
   COMMON/FAST1/JK_ANW,JK_INP
   CHARACTER RA(0:6)*140
   COMMON/FAST7/RA
!
   INTEGER I
!
!     CASE OF EMBEDDED BLANK AS IN 2D 3 REPRESENTING 2D+3
!     IF FORMS LIKE 2D 3 OR 3E 5, THE SPACE MUST BE REPLACED
!     WITH A PLUS SIGN
   DO I=1,138
      IF(JK_ANW(I:I).EQ.'D'.OR.JK_ANW(I:I).EQ.'E') THEN
         IF(JK_ANW(I+1:I+1).EQ.' '.AND.JK_ANW(I+2:I+2).NE.' ') THEN
            JK_ANW(I+1:I+1)='+'
         END IF
      END IF
   END DO
   RETURN
END
SUBROUTINE INSRTD2
!**********************************************************************
!     ADD MISSING "D" ROUTINE
!**********************************************************************
!
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   CHARACTER JK_ANW*140,JK_INP(1:5)*80
!
   COMMON/FAST1/JK_ANW,JK_INP
   CHARACTER RA(0:6)*140
   COMMON/FAST7/RA
!
   INTEGER I
!
!     CASE OF MISSING "D" AS IN 2-3 REPRESENTING 2D-3
   DO I=1,138
      IF(JK_ANW(I:I).EQ.'0'.OR.JK_ANW(I:I).EQ.'1'.OR.&
      &JK_ANW(I:I).EQ.'2'.OR.JK_ANW(I:I).EQ.'3'.OR.&
      &JK_ANW(I:I).EQ.'4'.OR.JK_ANW(I:I).EQ.'5'.OR.&
      &JK_ANW(I:I).EQ.'6'.OR.JK_ANW(I:I).EQ.'7'.OR.&
      &JK_ANW(I:I).EQ.'8'.OR.JK_ANW(I:I).EQ.'9'.OR.&
      &JK_ANW(I:I).EQ.'10') THEN
         IF(JK_ANW(I+1:I+1).EQ.'-'.OR.JK_ANW(I+1:I+1).EQ.'+') THEN
            JK_ANW(1:140)=JK_ANW(1:I)//'D'//JK_ANW(I+1:140)
         END IF
      END IF
   END DO
   RETURN
END
SUBROUTINE ADDDE2
!**********************************************************************
!     DECIMAL ADDIN ROUTINE
!**********************************************************************
!
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   CHARACTER JK_INP(1:5)*80,JK_ANW*140,BLNK80*80,AA20*20
!
   LOGICAL DEE,JK_FLG1(1:5),JK_FLG2
!
   INTEGER I,DEEPOS,JK_N
!
   COMMON/FAST1/JK_ANW,JK_INP
   CHARACTER RA(0:6)*140
   COMMON/FAST7/RA
!
   COMMON/FAST3/JK_FLG1,JK_FLG2
!
   COMMON/FAST6/JK_N
!
   AA20='                    '
   BLNK80=AA20//AA20//AA20//AA20
!
   DEE=.FALSE.
!
   DO I=1,80
      IF(JK_INP(JK_N)(I:I).EQ.'.') THEN
!     DECIMAL PRESENT,RETURN
         JK_FLG1(JK_N)=.TRUE.
         RETURN
!     PROCEED
      END IF
!     DECIMAL NOT PRESENT
   END DO
!
   DO I=1,80
      IF(JK_INP(JK_N)(I:I).EQ.'D'.OR.JK_INP(JK_N)(I:I).EQ.'E') THEN
         DEE=.TRUE.
         DEEPOS=I
         GO TO 20
!     KEEP LOOKING
      END IF
   END DO
20 CONTINUE
!
   IF(DEE) THEN
!     EXPONENT THERE
      IF(DEEPOS.NE.1) THEN
         JK_INP(JK_N)(1:80)=JK_INP(JK_N)(1:DEEPOS-1)//&
         &'.0'//JK_INP(JK_N)(DEEPOS:78)
         JK_FLG1(JK_N)=.TRUE.
      ELSE
!     DEEPOS=1
         JK_INP(JK_N)(1:80)=BLNK80
         JK_INP(JK_N)(1:3)='0.0'
         JK_FLG1(JK_N)=.FALSE.
      END IF
      RETURN
!     NO EXPONENT
   END IF
   DO I=1,78
      IF(JK_INP(JK_N)(I:I).EQ.' ') THEN
         IF(I.EQ.1) THEN
            JK_INP(JK_N)(1:80)=BLNK80
            JK_INP(JK_N)(1:3)='0.0'
         ELSE
!     I NO1 1
            JK_INP(JK_N)(1:80)=JK_INP(JK_N)(1:I-1)//'.0'
         END IF
         GO TO 40
      END IF
   END DO
40 CONTINUE
   RETURN
END
SUBROUTINE DESIZ2
!**********************************************************************
!     EXPONENT TOO BIG ROUTINE
!**********************************************************************
!
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   CHARACTER JK_INP(1:5)*80,JK_ANW*140,CHEXP*80,B*140
!
   LOGICAL DEE,JK_FLG1(1:5),JK_FLG2
!
   real(real64) NUMEXP
!
   INTEGER I,DEEPOS,JK_N

   COMMON/FAST1/JK_ANW,JK_INP
   CHARACTER RA(0:6)*140
   COMMON/FAST7/RA
!
   COMMON/FAST3/JK_FLG1,JK_FLG2
!
   COMMON/FAST6/JK_N
!
   DEE=.FALSE.
!
   DO I=1,80
      IF(JK_INP(JK_N)(I:I).EQ.'D'.OR.JK_INP(JK_N)(I:I).EQ.'E') THEN
         DEE=.TRUE.
         DEEPOS=I
         GO TO 20
!     KEEP LOOKING
      END IF
   END DO
20 CONTINUE
!
   IF(DEE) THEN
      CHEXP(1:80)=JK_INP(JK_N)(DEEPOS+1:80)
      DO I=2,78
         IF(CHEXP(I:I).EQ.' ') THEN
            CHEXP(1:80)=CHEXP(1:I-1)//'.0'
            GO TO 500
         END IF
      END DO
500   CONTINUE
      WRITE(B,100) CHEXP
      READ(B,200,ERR=222) NUMEXP
100   FORMAT(A80)
200   FORMAT(D23.15)
      IF(DABS(NUMEXP).GT.300.0D0) THEN
         JK_INP(JK_N)='0.0'
         JK_FLG1(JK_N)=.FALSE.
         RETURN
      ELSE
!     EXPONENT OK
         JK_FLG1(JK_N)=.TRUE.
      END IF
   ELSE
      JK_FLG1(JK_N)=.TRUE.
!     NO D OR E, RETURN
   END IF
   RETURN
222 CONTINUE
!     CASE OF EXPONENT NOT A NUMBER
   JK_INP(JK_N)='0.0'
   JK_FLG1(JK_N)=.FALSE.
   RETURN
END
! SUB PRO0.FOR
SUBROUTINE PRO0
!
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       IF A COLON FOLLOWS ANYTHING EXCEPT A BANK OR A COMMA, ADD A COMMA
!     INFRONT OF THE COLON
!
!                       DEFINE VARIABLES
!
   CHARACTER RA(0:6)*140
!
   COMMON/FAST7/RA
   INTEGER I
!
!
10 CONTINUE
   DO I=2,140
      IF(INPUT(I:I).EQ.':') THEN
         IF(INPUT(I-1:I-1).NE.','.AND.INPUT(I-1:I-1).NE.' ') THEN
!     ADD A COMMA
            INPUT(1:140)=INPUT(1:I-1)//','//INPUT(I:139)
            GO TO 10
!     CONTINUE, A COMMA OR SPACE WAS FOUND
         END IF
!     NOT A COLON, CONTINUE
      END IF
   END DO
!
   RETURN
END
! SUB PRO4.FOR
SUBROUTINE PRO4
!
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       IF A COLON FOLLOWS ANYTHING EXCEPT A BANK OR A COMMA, ADD A COMMA
!     INFRONT OF THE COLON
!
!                       DEFINE VARIABLES
!
!
   CHARACTER RA(0:6)*140
!
   COMMON/FAST7/RA
   INTEGER I,J
!
10 CONTINUE
   DO I=2,80
      IF(WS(I:I).EQ.':') THEN
         IF(WS(I-1:I-1).EQ.',') THEN
!     REMOVE THE COMMA
            IF(I.EQ.2) THEN
               WS(1:80)=WS(2:80)
               GO TO 10
            ELSE
!     I NOT 2
               WS(1:80)=WS(1:I-2)//WS(I:80)
               GO TO 10
            END IF
!     CONTINUE
         END IF
!     NOT A COLON, CONTINUE
      END IF
   END DO
!
   RETURN
END
! SUB SETBLN.FOR
SUBROUTINE SETBLN
!
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       SETS CURRENT INSTRUCTION VALUES AND STATUS INDICATORS
!       TO BLANK
!
   CHARACTER COMMWD(1:20)*8,QUALWD(1:20)*8,STRING(1:20)*80,&
   &ANW(1:20)*140,ANW1(1:20)*23,ANW2(1:20)*23,ANW3(1:20)*23,&
   &ANW4(1:20)*23,ANW5(1:20)*23
!
   real(real64) NW1(1:20),NW2(1:20),NW3(1:20),NW4(1:20),&
   &NW5(1:20)
!
   INTEGER STATNW(1:20),STATN1(1:20),STATN2(1:20),STATN3(1:20),&
   &STATN4(1:20),STATN5(1:20),STATCO(1:20),STATC2(1:20),&
   &STATBL(1:20),STBLK2(1:20),STATQL(1:20),STATST(1:20),SI(1:20),&
   &BI
!
   COMMON/CBLANK/NW1,NW2,NW3,NW4,NW5,STATNW,STATN1,STATN2,&
   &STATN3,STATN4,STATN5,STATCO,STATC2,STATBL,STBLK2,STATQL,&
   &STATST,SI,BI
!
   COMMON/CCBLAN/COMMWD,QUALWD,STRING,ANW,ANW1,ANW2,ANW3,ANW4,&
   &ANW5
!
   CHARACTER RA(0:6)*140
!
   COMMON/FAST7/RA
!
!
1206 COMMWD(BI)=BB
   QUALWD(BI)=BB
   STRING(BI)=AA//AA//AA//AA
   ANW(BI)=AA//AA//AA//AA//AA//AA//AA
   ANW1(BI)=AA//'   '
   ANW2(BI)=AA//'   '
   ANW3(BI)=AA//'   '
   ANW4(BI)=AA//'   '
   ANW5(BI)=AA//'   '
   NW1(BI)=0.0D0
   NW2(BI)=0.0D0
   NW3(BI)=0.0D0
   NW4(BI)=0.0D0
   NW5(BI)=0.0D0
   STATNW(BI)=0
   STATN1(BI)=0
   STATN2(BI)=0
   STATN3(BI)=0
   STATN4(BI)=0
   STATN5(BI)=0
   STATQL(BI)=0
   STATBL(BI)=1
   STBLK2(BI)=0
   STATCO(BI)=0
   STATC2(BI)=0
   STATST(BI)=0
   SI(BI)=0
   RETURN
END
SUBROUTINE ATOD(INPUT,INP,NUM,DF,FLG1,INUM,FLG2,REMAIN)
!**********************************************************************
!     THIS SUBROUTINE IS USED FOR CONVERTING CHARACTER REPRESENTATIONS
!     OF real(real64) INPUT TO real(real64) VALUES.
!
!     THE INPUT IS THE CHARACTER*140 VARIABLE "INPUT"
!
!     THE TOTAL NUMBER OF EXPECTED VALUES STORED IN INPUT
!     IS DESIGNATED BY THE INTEGER VALUE "INUM".
!
!     RETURNED VALUES ARE:
!
!               INP(1:INUM) = CHARACTER REPRESENTATIONS
!     OF EACH OF THE INUM VALUES
!
!               NUM(1:INUM) = real(real64) VALUES
!
!               DF(1:INUM) = 0 FOR EXPLICIT INPUT VALUE
!                            1 FOR DEFAULT INPUT VALUE
!                            2 FOR NO INPUT VALUE
!
!               FLG1(1:INUM) = .TRUE. FOR VALID INPUT
!                             .FALSE. FOR INVALID INPUT
!
!               FLG2        = .TRUE. FOR 5 NUMERIC WORDS ONLY
!                             .FALSE. FOR INPUT BEYOND 5 NUMERIC WORDS
!
!     A VALUE IS INVALID IF IT DOES NOT REPRESENT A NUMERIC VALUE
!
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   INTEGER I,DF(*),INUM
!
   real(real64) NUM(*)
!
   CHARACTER INPUT*140,INP(*)*80,REMAIN*140
!
   LOGICAL FLG1(*),FLG2,ERROR
!
   CHARACTER RA(0:6)*140
!
   COMMON/FAST7/RA
!
   CALL BREAKOUT(INPUT,INP,DF,FLG1,INUM,FLG2,REMAIN)
!
   CALL ATON(INP,FLG1,NUM,ERROR,DF,INUM)
!
   RETURN
END
!
SUBROUTINE BREAKOUT(INPUT,INP,DF,FLG1,INUM,FLG2,REMAIN)
!
!     THIS ROUTINE TAKES A 140 CHARACTER VARIABLE NAMED "INPUT"
!     AND BREAKS IT INTO UP TO INUM CHARACTER VARIABLES WITH THE BREAKS
!     INDICATED BY A BLANK SPACE OR A COMMA.
!
!     THE RETURNED VARIABLES ARE RETURNED IN THE CHARACTER*80 ARRAY
!     INP(1:20). IF A VALUE WAS RETURNED, THE FLAG FLG(I) IS
!     SET TO .TRUE. ELSE IT IS LEFT AS FALSE.
!
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   INTEGER I,J,DF(*),INUM,JL
!
   LOGICAL BLANK,FLG1(*),FLG2
!
   CHARACTER RA(0:6)*140
!
   COMMON/FAST7/RA
!
   CHARACTER INPUT*140,REMAIN*140,INP(*)*80
!
!     INITIALIZE ALL VALUES
   FLG2=.TRUE.
   DO I=1,INUM
!
!     DF(I)=1 MEANS NO INPUT OR DEFAULT INPUT
      DF(I)=1
!
      FLG1(I)=.TRUE.
!
      DO J=1,80
         INP(I)(J:J)=' '
      END DO
   END DO
!
!     STRIP LEADING BLANKS IF ANY
   CALL STPLBL(INPUT,BLANK)
   REMAIN(1:140)=INPUT(1:140)
   IF(BLANK) GO TO 201
!     IF A NON-NUMERIC CHARACTER WAS FOUND, JUMP TO 201
   IF(REMAIN(1:1).NE.'+'.AND.&
   &REMAIN(1:1).NE.'-'.AND.&
   &REMAIN(1:1).NE.'0'.AND.&
   &REMAIN(1:1).NE.'1'.AND.&
   &REMAIN(1:1).NE.'2'.AND.&
   &REMAIN(1:1).NE.'3'.AND.&
   &REMAIN(1:1).NE.'4'.AND.&
   &REMAIN(1:1).NE.'5'.AND.&
   &REMAIN(1:1).NE.'6'.AND.&
   &REMAIN(1:1).NE.'7'.AND.&
   &REMAIN(1:1).NE.'8'.AND.&
   &REMAIN(1:1).NE.'9'.AND.&
   &REMAIN(1:1).NE.','.AND.&
   &REMAIN(1:1).NE.'.') GO TO 201
!
!     CASE OF EMBEDDED BLANK AS IN 2D 3 REPRESENTING 2D+3
   CALL INSRTP(INPUT)
   REMAIN(1:140)=INPUT(1:140)
!
!     CASE OF MISSING D OR E IN 2-3 REPRESENTING 2D-3
   CALL INSRTD(INPUT)
   REMAIN(1:140)=INPUT(1:140)
!
!     BREAK OUT VALUES
!
   RA(0)=INPUT(1:140)
   DO 200 J=1,INUM
!     STRIP LEADING BLANKS IF ANY
      CALL STPLBL(INPUT,BLANK)
      IF(BLANK) GO TO 200
      REMAIN(1:140)=INPUT(1:140)
!
!     IF A NON-NUMERIC CHARACTER WAS FOUND, JUMP TO 201
      IF(REMAIN(1:1).NE.'+'.AND.&
      &REMAIN(1:1).NE.'-'.AND.&
      &REMAIN(1:1).NE.'0'.AND.&
      &REMAIN(1:1).NE.'1'.AND.&
      &REMAIN(1:1).NE.'2'.AND.&
      &REMAIN(1:1).NE.'3'.AND.&
      &REMAIN(1:1).NE.'4'.AND.&
      &REMAIN(1:1).NE.'5'.AND.&
      &REMAIN(1:1).NE.'6'.AND.&
      &REMAIN(1:1).NE.'7'.AND.&
      &REMAIN(1:1).NE.'8'.AND.&
      &REMAIN(1:1).NE.'9'.AND.&
      &REMAIN(1:1).NE.','.AND.&
      &REMAIN(1:1).NE.'.') GO TO 201
!
!     IS THE NEXT VALUE A DEFAULT INPUT VALUE
      IF(INPUT(1:1).EQ.',') THEN
!     THE NEXT VALUE IS DEFALUT
         INP(J)(1:23)='+0.0000000000000000D000'
         DF(J)=1
!     REBUILD INPUT WITHOUT COMMA
         INPUT(1:140)=INPUT(2:140)
         REMAIN(1:140)=INPUT(1:140)
         RA(J)=REMAIN(1:140)
         GO TO 200
      ELSE
!     NEXT VALUE IS NOT DEFAULT, BREAK IT OUT
!
         DO I=2,139
            IF(INPUT(I:I).EQ.' '.OR.INPUT(I:I).EQ.',') THEN
               INP(J)=INPUT(1:I-1)
               DF(J)=0
               REMAIN(1:140)=INPUT(I+1:140)
               RA(J)=REMAIN(1:140)
               INPUT(1:140)=INPUT(I+1:140)
               GO TO 200
            END IF
         END DO
      END IF
!
200 CONTINUE
201 CONTINUE
!
!     ALL 5 NUMERIC WORDS ARE BROKEN OUT. IS THERE ANYTHING LEFT
!     NON-BLANK IN INPUT(REMAIN).IF SO,SET FLG2 TO FALSE AND RETURN
!     ANYTHING, THAT IS, BEYOND ONE TRAILING COMMA
   DO JL=1,140
      IF(REMAIN(JL:JL).NE.' ') THEN
         FLG2=.FALSE.
      END IF
   END DO
!
   DO J=1,INUM
!     IF A INP VALUE IS MISSING A DECIMAL POINT, ADD ONE
      CALL ADDDEC(INP(J),FLG1(J))
   END DO
   DO J=1,INUM
      IF(FLG1(J)) THEN
!     IF A INP HAS A "D" OR "E", CHECK THE EXPONENT SIZE
         CALL DESIZE(INP(J),FLG1(J))
!     NUMBER ALREADY NAN
      END IF
   END DO
!
   DO J=1,INUM
      IF(FLG1(J)) THEN
         IF(INP(J)(1:1).NE.'(') THEN
!     INP(J) DOES NOT START WITH A LEFT PARENTHESIS
            IF(INP(J)(24:24).NE.' ') THEN
!     INP(J) IS TOO LONG
               FLG1(J)=.FALSE.
               RETURN
!     LENGTH OK
            END IF
!     PARENTHESIS IS THERE
         END IF
      END IF
   END DO
!
   RETURN
END

SUBROUTINE STPLBL(INPUT,BLANK)
!**********************************************************************
!     SRIP LEADING BLANKS ROUTINE
!**********************************************************************
!
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!     STRIPS OFF LEADING BLANKS FROM INPUT*140
!
   CHARACTER INPUT*140,BL*140
!
   LOGICAL BLANK
!
   INTEGER I
!
   CHARACTER RA(0:6)*140
!
   COMMON/FAST7/RA
!
   BLANK=.FALSE.
   DO I=1,140
      BL(I:I)=' '
   END DO
!
   I=0
1  IF(ICHAR(INPUT(1:1)).EQ.32) THEN
      INPUT(1:140)=INPUT(2:140)
      IF(INPUT(1:140).EQ.BL(1:140)) THEN
         BLANK=.TRUE.
         RETURN
!     PROCEED
      END IF
      I=I+1
      IF(I.LT.140) GO TO 1
      IF(I.GE.140) THEN
         BLANK=.TRUE.
         RETURN
      END IF
!     ALL LEADING BLANKS REMOVED
   END IF
   RETURN
END

SUBROUTINE STPCOM(INPUT)
!**********************************************************************
!     STRIP LEADING COMMA ROUTINE
!**********************************************************************
!
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!     STRIPS OFF ONE LEADING COMMA FROM INPUT*140
!     STRIP ONE LEADING COMMA IF PRESENT ROUTINE
!     IF THERE IS ONE, ELSE DOES NOTHING
!
   CHARACTER INPUT*140
!
   CHARACTER RA(0:6)*140
!
   COMMON/FAST7/RA
!
   IF(ICHAR(INPUT(1:1)).EQ.44) THEN
      INPUT(1:140)=INPUT(2:140)
      RETURN
!     NO COMMA TO BE REMOVED
   END IF
   RETURN
END
SUBROUTINE INSRTP(INPUT)
!**********************************************************************
!     ADD MISSING PLUS SIGN ROUTINE
!**********************************************************************
!
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   CHARACTER INPUT*140
!
   INTEGER I
!
   CHARACTER RA(0:6)*140
!
   COMMON/FAST7/RA
!
!     CASE OF EMBEDDED BLANK AS IN 2D 3 REPRESENTING 2D+3
!     IF FORMS LIKE 2D 3 OR 3E 5, THE SPACE MUST BE REPLACED
!     WITH A PLUS SIGN
   DO I=1,138
      IF(INPUT(I:I).EQ.'D'.OR.INPUT(I:I).EQ.'E') THEN
         IF(INPUT(I+1:I+1).EQ.' '.AND.INPUT(I+2:I+2).NE.' ') THEN
            INPUT(I+1:I+1)='+'
         END IF
      END IF
   END DO
   RETURN
END
SUBROUTINE INSRTD(INPUT)
!**********************************************************************
!     ADD MISSING "D" ROUTINE
!**********************************************************************
!
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   CHARACTER INPUT*140
!
   INTEGER I
!
   CHARACTER RA(0:6)*140
!
   COMMON/FAST7/RA
!
!     CASE OF MISSING "D" AS IN 2-3 REPRESENTING 2D-3
   DO I=1,138
      IF(INPUT(I:I).EQ.'0'.OR.INPUT(I:I).EQ.'1'.OR.&
      &INPUT(I:I).EQ.'2'.OR.INPUT(I:I).EQ.'3'.OR.&
      &INPUT(I:I).EQ.'4'.OR.INPUT(I:I).EQ.'5'.OR.&
      &INPUT(I:I).EQ.'6'.OR.INPUT(I:I).EQ.'7'.OR.&
      &INPUT(I:I).EQ.'8'.OR.INPUT(I:I).EQ.'9'.OR.&
      &INPUT(I:I).EQ.'10') THEN
         IF(INPUT(I+1:I+1).EQ.'-'.OR.INPUT(I+1:I+1).EQ.'+') THEN
            INPUT(1:140)=INPUT(1:I)//'D'//INPUT(I+1:140)
         END IF
      END IF
   END DO
   RETURN
END
SUBROUTINE DESIZE(INP1,FLG1)
!**********************************************************************
!     EXPONENT TOO BIG ROUTINE
!**********************************************************************
!
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   CHARACTER INP1*80,CHEXP*80,B*140
!
   LOGICAL DEE,FLG1
!
   real(real64) NUMEXP
!
   INTEGER I,DEEPOS
!
   CHARACTER RA(0:6)*140
!
   COMMON/FAST7/RA
!
   DEE=.FALSE.
!
   DO I=1,80
      IF(INP1(I:I).EQ.'D'.OR.INP1(I:I).EQ.'E') THEN
         DEE=.TRUE.
         DEEPOS=I
         GO TO 20
!     KEEP LOOKING
      END IF
   END DO
20 CONTINUE
!
   IF(DEE) THEN
      CHEXP(1:80)=INP1(DEEPOS+1:80)
      DO I=2,78
         IF(CHEXP(I:I).EQ.' ') THEN
            CHEXP(1:80)=CHEXP(1:I-1)//'.0'
            GO TO 500
         END IF
      END DO
500   CONTINUE
      WRITE(B,100) CHEXP
      READ(B,200,ERR=222) NUMEXP
100   FORMAT(A80)
200   FORMAT(D23.15)
      IF(DABS(NUMEXP).GT.300.0D0) THEN
         INP1='0.0'
         FLG1=.FALSE.
         RETURN
      ELSE
!     EXPONENT OK
         FLG1=.TRUE.
      END IF
   ELSE
      FLG1=.TRUE.
!     NO D OR E, RETURN
   END IF
   RETURN
222 CONTINUE
!     CASE OF EXPONENT NOT A NUMBER
   INP1='0.0'
   FLG1=.FALSE.
   RETURN
END
SUBROUTINE ADDDEC(INP1,FLG1)
!**********************************************************************
!     DECIMAL ADDIN ROUTINE
!**********************************************************************
!
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   CHARACTER INP1*80,BLNK80*80,AA20*20
!
   LOGICAL DEE,FLG1
!
   INTEGER I,DEEPOS
!
   CHARACTER RA(0:6)*140
!
   COMMON/FAST7/RA
!
   AA20='                    '
   BLNK80=AA20//AA20//AA20//AA20
!
   DEE=.FALSE.
!
   DO I=1,80
      IF(INP1(I:I).EQ.'.') THEN
!     DECIMAL PRESENT,RETURN
         FLG1=.TRUE.
         RETURN
!     PROCEED
      END IF
!     DECIMAL NOT PRESENT
   END DO
!
   DO I=1,80
      IF(INP1(I:I).EQ.'D'.OR.INP1(I:I).EQ.'E') THEN
         DEE=.TRUE.
         DEEPOS=I
         GO TO 20
!     KEEP LOOKING
      END IF
   END DO
20 CONTINUE
!
   IF(DEE) THEN
!     EXPONENT THERE
      IF(DEEPOS.NE.1) THEN
         INP1(1:80)=INP1(1:DEEPOS-1)//'.0'//INP1(DEEPOS:78)
         FLG1=.TRUE.
      ELSE
!     DEEPOS=1
         INP1(1:80)=BLNK80
         INP1(1:3)='0.0'
         FLG1=.FALSE.
      END IF
      RETURN
!     NO EXPONENT
   END IF
   DO I=1,78
      IF(INP1(I:I).EQ.' ') THEN
         IF(I.EQ.1) THEN
            INP1(1:80)=BLNK80
            INP1(1:3)='0.0'
         ELSE
!     I NO1 1
            INP1(1:80)=INP1(1:I-1)//'.0'
         END IF
         GO TO 40
      END IF
   END DO
40 CONTINUE
   RETURN
END
SUBROUTINE ATON(INP,FLG1,NUM,ERROR,DF,INUM)
!
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   real(real64) NUM(*)
!
   INTEGER I,DF(*),INUM
!
   CHARACTER INP(*)*80,B*140
!
   LOGICAL ERROR,FLG1(*)
!
   CHARACTER RA(0:6)*140
!
   COMMON/FAST7/RA
!
   DO 30 I=1,INUM
      IF(.NOT.FLG1(I)) THEN
!     EARLIER DETERMINATION OF A BAD INPUT VALE
         NUM(I)=0.0D0
      ELSE
!     VALUE WAS NOT PREVIOUSLY COUNTED AS BAD
         IF(DF(I).EQ.0) THEN
!
!     CONVERT TO real(real64)
            WRITE(B,10) INP(I)(1:80)
10          FORMAT(A80)
            READ(B,20,ERR=99999) NUM(I)
20          FORMAT(D23.15)
            ERROR=.FALSE.
            FLG1(I)=.TRUE.
            GO TO 30
99999       ERROR=.TRUE.
            FLG1(I)=.FALSE.
            NUM(I)=0.0D0
            INP(I)='0.0'
         ELSE
!     DEFAULT, SET TO 0
            NUM(I)=0.0D0
            FLG1(I)=.TRUE.
            INP(I)='0.0'
         END IF
      END IF
30 CONTINUE
   RETURN
END
SUBROUTINE UPPER
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   CHARACTER RA(0:6)*140
!
   COMMON/FAST7/RA
   CHARACTER STRUC*140
   COMMON/JKSTRUC/STRUC
   INTEGER I,J
   DO I=1,140
      J=ICHAR(STRUC(I:I))
      IF(J.GE.97.AND.J.LE.122)&
      &STRUC(I:I)=CHAR(J-32)
   END DO
   RETURN
END
SUBROUTINE UPPER_CASE(STRUC)
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   CHARACTER STRUC*140
   INTEGER I,J
   DO I=1,140
      J=ICHAR(STRUC(I:I))
      IF(J.GE.97.AND.J.LE.122)&
      &STRUC(I:I)=CHAR(J-32)
   END DO
   RETURN
END
SUBROUTINE TOUPPER(STRUC,N)
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   INTEGER I,J,N
   CHARACTER STRUC*(N)
   DO I=1,N
      J=ICHAR(STRUC(I:I))
      IF(J.GE.97.AND.J.LE.122)&
      &STRUC(I:I)=CHAR(J-32)
   END DO
   RETURN
END
! SUB PROCES.FOR
SUBROUTINE PROCES
   USE GLOBALS
!
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!               THIS SUBROUTINE PROCESSES THE 140 CHARACTER
!               INPUT LINE INTO UP TO 20 PROGRAM COMMANDS



!                       DEFINE VARIABLES
!
   CHARACTER INSTRC(1:20)*140,OLYNE*140
!
   INTEGER NSTRUC,I,J
!
   COMMON/PRO22/INSTRC,NSTRUC

!
   CHARACTER RA(0:6)*140
   character(len=1024) :: debugTxt
!
   COMMON/FAST7/RA

   !OUTLYNE= 'PROCES SUBROUTINE'
   !CALL SHOWIT(19)

   !PRINT *, "PROCES SUBROUTINE STARTED, INPUT IS " // INPUT
   !call LogTermFOR("PROCES IPT "//trim(INPUT))
   !PRINT *, "INPUT is ", INPUT
   call logger%logText(INPUT)
   !write(debugTxt, *) F1,F5,F6
   !call logger%logText(trim(debugTxt))



   NUMCOM=1
   IF(MULTICOM) NUMCOM=20
!
   LASTCOMWRD=WC
   LASTWASFOB=.FALSE.
   IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
!
!     THIS ALLOWS RTG? TO BE READ AS RTG ?
101 CONTINUE
   DO J=2,140
      IF(INPUT(J:J).EQ.'?'.AND.INPUT(J-1:J-1).NE.' ') THEN
         INPUT(1:140)=INPUT(1:J-1)//' '//INPUT(J:139)
         GO TO 101
      END IF
   END DO
!       PRO0 ADDS A COMMA BETWEEN ANY CHARACTER AND A COLON
!       IF THE CHARACTER IS NOT A BLANK OR A COMMA  2/14/
   !JN - don't want this behaviour so commenting it out and
   ! crossing my fingers it doesn't break anything
   !        CALL PRO0
!
!     TAKE OUT SPACES INFRONT OF A COMMA
   OLYNE(1:140)=INPUT(1:140)
   CALL NOBLANK(OLYNE)
   INPUT(1:140)=OLYNE(1:140)

!
!       PRO2 DOES VIRTUAL CARRIAGE RETURNS
   CALL PRO2

!       PRO3 FINISHES PROCESSING INPUT INTO PROGRAM INSTRUCTIONS
   CALL PRO3
   RETURN
END
! SUB PRO2.FOR
SUBROUTINE PRO2
   USE GLOBALS
!
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS SUBROUTINE PROCESSES THE STRING VARIABLE "INPUT"
!       BY REMOVING UP TO 20 VIRTUAL CARRIAGE RETURNS AND RETURNING
!       UP TO 20 INSTRUCTION LINES IN THE ARRAY INSTRUC. THE NUMBER
!       NON-BLANK INSTRUCTIONS IS PASSED VIA NSTRUC. THE CHARACTER
!     USED FOR REPRESENTING A VIRTUAL CARRIAGE RETURN IS DESIGNATED
!     BY ITS ASCII VALUE. THIS VALUE IS PASSED BY C_VAL, AN INTEGER.
!     THE DEFAULT VALUE FOR C_VAL IS (59), THIS IS THE ASCII VALUE
!     OF THE SEMICOLON (;). THIS IS THE WAY CODE V DOES IT.
!                       DEFINE VARIABLES
!
   CHARACTER INSTRC(1:20)*140,STRUC*140
!
   CHARACTER RA(0:6)*140
!
   COMMON/FAST7/RA
!
   INTEGER CVAL(1:140),VRETCT,NSTRUC,SUM,VRTRCK(1:140)&
   &,VRPOS(1:140),I,JJ,K,J,INSS,C_VAL
!
   COMMON/PRO22/INSTRC,NSTRUC
!
   COMMON/JKSTRUC/STRUC
!
!
!               NOW WE RESOLVE MULTIPLE PROGRAM INSTRUCTIONS ON THE
!               INPUT INSTRUCTION LINE.
!
!               NOW INITIALIZE AGAIN THE VIRTUAL CARRIAGE RETURN COUNT,
!               ITS POSITION TRACKING ARRAY AND THE CVAL ARRAY.
!               THEN REANALYSE 'INPUT', FILLING THE CVAL AND VRPOS
!               ARRAYS.
!               STRIP 20 LEADING BLANKS
   J=1
   DO I=1,20
      IF(INPUT(J:J).EQ.' ') INPUT(1:140)=INPUT(2:140)
   END DO
!
   C_VAL=59
!
65 VRETCT = 0
   DO I=1,140
      VRPOS(I)=0
      CVAL(I)=0
   END DO
   DO I=1,140
      CVAL(I)=ICHAR(INPUT(I:I))
   END DO
   DO I=1,140
      IF(CVAL(I).EQ.C_VAL) THEN
         VRETCT = VRETCT+1
         VRPOS(I)=I
      END IF
   END DO
!               NOW WE KNOW WHERE THE VIRTUAL RETURNS ARE.
!               THE RESOLUTION RULE FOR VIRTUAL CARRIAGE RETURNS IS:
!
!                       1) THE PROGRAM EXPECTS TO SEE ONLY SINGLE
!                          VIRTUAL CARRIAGE RETURNS.
!                       2) THE PROGRAM DOES NOT WANT TO SEE VIRTUAL
!                          CARRIAGE RETURNS NEXT TO ONE ANOTHER.
!                       3) THE PROGRAM FIRST LOOKS FOR THE CASE OF
!                          MULTIPLE ADJACENT VIRTUAL CARRIAGE RETURNS
!                          AND REMOVES ALL BUT ONE OF THEM. IN THIS
!                          PROCESS THE VARIABLE 'INPUT' IS REBUILT
!                          ONCE AGAIN. ALSO VIRTUAL RETURNS IN POSITION
!                          1 OF 'INPUT' ARE REMOVED.
!
!               ARE THERE ANY VIRTUAL CARRIAGE RETURNS ?
!
   IF(VRETCT.NE.0) THEN
!               IS THE FIRST POSITION A VIRTUAL RETURN ?
      IF(VRPOS(1).NE.0) THEN
!               REBUILD 'INPUT'
         INPUT = INPUT(2:140)
         GO TO 65
      END IF
!               ARE THERE ANY ADJACENT VIRTUAL CARRIAGE RETURNS ?
!               IF THERE ARE THEY ARE ONE BY ONE CUT DOWN TO SINGLE
!               NON ADJACENT RETURNS.
      DO 64 I=1,138
         IF(VRPOS(I).NE.0.AND.VRPOS(I+1).NE.0) THEN
            INPUT = INPUT(1:I)//INPUT(I+2:140)
            GO TO 65
         END IF
64    CONTINUE
   END IF
!
!               NOW THERE ARE NO VIRTUAL RETURNS IN THE FIRST POSITION
!               AND THERE ARE NO ADJACENT VIRTUAL RETURNS.
!
!               NOW REMOVE ALL TRAILING VIRTUAL RETURNS.
!               AS BEFORE, REINITIALIZE COUNTING VARIABLES AND ARRAYS.
!               NOW PROCEED TO REMOVE ANY TRAILING VIRTUAL CARRIAGE
!               RETURNS.
!
76 VRETCT = 0
   DO I=1,140
      VRPOS(I)=0
      CVAL(I)=0
   END DO
!
   DO I=1,140
      CVAL(I)=ICHAR(INPUT(I:I))
   END DO
!
   DO I=1,140
      IF(CVAL(I).EQ.C_VAL) THEN
         VRETCT = VRETCT+1
         VRPOS(I)=I
      END IF
   END DO
!
   DO I=1,140
      JJ=I+1
      SUM = 0
      IF(VRPOS(I).NE.0) THEN
!
         DO K=JJ,140
            SUM=SUM +(CVAL(K)-32)
         END DO
!
         IF(SUM.EQ.0) THEN
            INPUT = INPUT(1:(JJ-2))
            GO TO 1000
         ELSE
            SUM = 0
         END IF
      END IF
   END DO
!
1000 CONTINUE
!
!               NOW ALL VITUAL LINE DELETES,CHARACTER DELETES, AND
!               VIRTUAL CARRIAGE RETURNS HAVE BEEN RESOLVED. IF
!               THERE ARE VIRTUAL CARRIAGE RETURNS,THEN THE
!               INPUT INSTRUCTION LINE CONTAINS MULTIPLE PROGRAM
!               COMMANDS WHICH MUST BE RESOLVED, STORED, AND THEN
!               EXECUTED. THE COMMANDS WILL BE INITIALLY BROKEN UP
!               AND STORED IN THE INSTRC ARRAY. INSTRC IS A CHARACTER
!               ARRAY OF 140 ELEMENTS MAXIMUM. THE ACTUAL NUMBER OF
!               INSTRUCTIONS STORED DURING ANY ONE INPUT CYCLE WILL
!               BE TRACKED BY THE INTEGER VARIABLE NSTRUC. THE MAXIMUM
!               ALLOWABLE VALUE FOR NSTRUC IS 20. 20 COMMANDS CAN
!               BE STACKED ON ONE INPUT LINE.
!               EACH ELEMENT OF INSTR ARRAY CAN HAVE 140 CHARACTERS.
!
!               AT THIS POINT INITIALIZE THE INSTRC(I) ARRAY TO HAVE
!               EACH ELEMENT CONTAIN ONLY BLANKS.ALSO INITIALIZE
!               COMMWD(I),QUALWD(I),STRING(I),ANW1(I),ANW2(I),ANW3(I),
!               ANW4(I),AND ANW5(I). THESE WILL BE THE REPOSITORIES
!               OF THE COMMAND,QUALIFIER,STRING,AND ALPHA-NUMERIC
!               REPRESENTATIONS OF THE NUMERIC WORDS.
!
   DO I=1,NUMCOM
      INSTRC(I)=AA//AA//AA//AA//AA//AA//AA
   END DO
!
!               AGAIN  INITALIZE AND ASSIGN VALUES TO COUNTERS AND
!               ARRAYS.
   J = 1
   DO I=1,140
      CVAL(I)=0
      VRTRCK(I)=0
   END DO
   DO I=1,140
      CVAL(I) = ICHAR(INPUT(I:I))
   END DO
   DO I=1,140
      IF(CVAL(I).EQ.C_VAL) THEN
         VRTRCK(J)=I
         J=J+1
      END IF
   END DO
   IF(J.EQ.1) THEN
!                       THERE IS ONLY ONE INSTRUCTION
      NSTRUC=1
      INSTRC(1)=INPUT
      GO TO 1010
!               THERE IS MORE THAN INSTRUCTION
   END IF
   NSTRUC=J
   INSTRC(1) = INPUT(1:(VRTRCK(1)-1))
   IF(NSTRUC.EQ.2) THEN
      INSTRC(2) = INPUT((VRTRCK(1)+1):140)
   END IF
   IF(NSTRUC.EQ.3) THEN
      INSTRC(2) = INPUT((VRTRCK(1)+1):(VRTRCK(2)-1))
      INSTRC(3) = INPUT((VRTRCK(2)+1):140)
   END IF
   IF(NSTRUC.GT.3) THEN
      DO 1004 I=2,(NSTRUC-1)
         INSTRC(I)=INPUT((VRTRCK(I-1)+1):(VRTRCK(I)-1))
1004  CONTINUE
      INSTRC(NSTRUC)=INPUT((VRTRCK(NSTRUC-1)+1):140)
      GO TO 1020
   END IF
1010 CONTINUE
1020 CONTINUE
!
!
!       NEW FEATURE ADDED ON 8/5/88, CHANGE ALL LOWER CASE ALPHA
!       CHARACTERS IN INSTRC(I) TO UPPER CASE
!       UNLESS THE INSTRUCTION STARTS WITH:
!                      M(SPACE)
!     OR               M,
!     OR               C(SPACE)
!     OR               C,
!     OR               LIC(SPACE)
!     OR               LIC,
!     OR               LI(SPACE)
!     OR               LI,
!     OR               FIGURE(SPACE)
!     OR               FIGURE,
!     OR               ONAME
!     OR               SNAME
!       DO THIS BY CALLING UPPER.FOR
   DO INSS=1,NSTRUC
      STRUC=INSTRC(INSS)
      IF(STRUC(1:1).EQ.'M'.OR.STRUC(1:1).EQ.'m'.or.&
      &STRUC(1:1).EQ.'CK'.OR.STRUC(1:1).EQ.'ck') THEN
         IF(STRUC(2:2).EQ.','.OR.STRUC(2:2).EQ.' ') THEN
            IF(STRUC(1:1).EQ.'m') STRUC(1:1)='M'
            IF(STRUC(1:1).EQ.'ck') STRUC(1:1)='CK'
            STRUC(1:140)=STRUC(1:2)//STRUC(3:139)
            GO TO 89
         END IF
      ELSE
      END IF
      IF(STRUC(1:3).EQ.'LIC'.OR.STRUC(1:3).EQ.'lic') THEN
         IF(STRUC(4:4).EQ.','.OR.STRUC(4:4).EQ.' ') THEN
            STRUC(1:3)='LIC'
            STRUC(1:140)=STRUC(1:4)//STRUC(5:139)
            GO TO 89
         END IF
      ELSE
      END IF
      IF(STRUC(1:3).EQ.'MFG'.OR.STRUC(1:3).EQ.'mfg') THEN
         IF(STRUC(4:4).EQ.','.OR.STRUC(4:4).EQ.' ') THEN
            STRUC(1:3)='MFG'
            STRUC(1:140)=STRUC(1:4)//STRUC(5:139)
            GO TO 89
         END IF
      ELSE
      END IF
      IF(STRUC(1:6).EQ.'CATNUM'.OR.STRUC(1:6).EQ.'catnum') THEN
         IF(STRUC(7:7).EQ.','.OR.STRUC(7:7).EQ.' ') THEN
            STRUC(1:6)='CATNUM'
            STRUC(1:140)=STRUC(1:7)//STRUC(8:139)
            GO TO 89
         END IF
      ELSE
      END IF
      IF(STRUC(1:5).EQ.'ONAME'.OR.STRUC(1:5).EQ.'oname') THEN
         IF(STRUC(6:6).EQ.','.OR.STRUC(6:6).EQ.' ') THEN
            STRUC(1:5)='ONAME'
            STRUC(1:140)=STRUC(1:6)//STRUC(7:139)
            GO TO 89
         END IF
      ELSE
      END IF
      IF(STRUC(1:5).EQ.'SNAME'.OR.STRUC(1:5).EQ.'sname') THEN
         IF(STRUC(6:6).EQ.','.OR.STRUC(6:6).EQ.' ') THEN
            STRUC(1:5)='SNAME'
            STRUC(1:140)=STRUC(1:6)//STRUC(7:139)
            GO TO 89
         END IF
      ELSE
      END IF
      IF(STRUC(1:6).EQ.'CV2PRG'.OR.STRUC(1:6).EQ.'cv2prg') THEN
         IF(STRUC(7:7).EQ.','.OR.STRUC(7:7).EQ.' ') THEN
            STRUC(1:6)='CV2PRG'
            STRUC(1:140)=STRUC(1:7)//STRUC(8:139)
            GO TO 89
         END IF
      ELSE
      END IF
      IF(STRUC(1:7).EQ.'ZMX2PRG'.OR.STRUC(1:7).EQ.'zmx2prg') THEN
         IF(STRUC(8:8).EQ.','.OR.STRUC(8:8).EQ.' ') THEN
            STRUC(1:7)='ZMX2PRG'
            STRUC(1:140)=STRUC(1:7)//STRUC(8:139)
            GO TO 89
         END IF
      ELSE
      END IF
      IF(STRUC(1:7).EQ.'PFANCAP'.OR.STRUC(1:7).EQ.'pfancap') THEN
         IF(STRUC(8:8).EQ.','.OR.STRUC(8:8).EQ.' ') THEN
            STRUC(1:7)='PFANCAP'
            STRUC(1:140)=STRUC(1:7)//STRUC(8:139)
            GO TO 89
         END IF
      ELSE
      END IF
      IF(STRUC(1:7).EQ.'PFANLBL'.OR.STRUC(1:7).EQ.'pfanlbl') THEN
         IF(STRUC(8:8).EQ.','.OR.STRUC(8:8).EQ.' ') THEN
            STRUC(1:7)='PFANLBL'
            STRUC(1:140)=STRUC(1:7)//STRUC(8:139)
            GO TO 89
         END IF
      ELSE
      END IF
      IF(STRUC(1:2).EQ.'LI'.OR.STRUC(1:2).EQ.'li') THEN
         IF(STRUC(3:3).EQ.','.OR.STRUC(3:3).EQ.' ') THEN
            STRUC(1:2)='LI'
            STRUC(1:140)=STRUC(1:3)//STRUC(4:139)
            GO TO 89
         END IF
      ELSE
      END IF
      IF(STRUC(1:3).EQ.'LBL'.OR.STRUC(1:3).EQ.'lbl') THEN
         IF(STRUC(4:4).EQ.','.OR.STRUC(4:4).EQ.' ') THEN
            IF(STRUC(1:3).EQ.'lbl') STRUC(1:3)='LBL'
            STRUC(1:140)=STRUC(1:4)//STRUC(5:139)
            GO TO 89
         END IF
      ELSE
      END IF
      IF(STRUC(1:5).EQ.'LABEL'.OR.STRUC(1:5).EQ.'label') THEN
         IF(STRUC(6:6).EQ.','.OR.STRUC(6:6).EQ.' ') THEN
            IF(STRUC(1:5).EQ.'label') STRUC(1:5)='LABEL'
            STRUC(1:140)=STRUC(1:6)//STRUC(7:139)
            GO TO 89
         END IF
      ELSE
      END IF
      IF(STRUC(1:6).EQ.'FIGURE'.OR.STRUC(1:6).EQ.'figure') THEN
         IF(STRUC(7:7).EQ.','.OR.STRUC(7:7).EQ.' ') THEN
            IF(STRUC(1:6).EQ.'figure') STRUC(1:6)='FIGURE'
            STRUC(1:140)=STRUC(1:7)//STRUC(8:139)
            GO TO 89
         END IF
      ELSE
      END IF
      CALL UPPER
89    INSTRC(INSS)=STRUC
   END DO
!       NEW FEATURE COMPLETE
   RETURN
END
! SUB PRO3.FOR
SUBROUTINE PRO3
   use glass_manager
   USE GLOBALS
!
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS ROUTINE PROCESSES UP TO NUMCOM INSTRUCTIONS
!       INTO PROGRAM COMMANDS
!
!                       DEFINE VARIABLES
!
   CHARACTER INSTRC(1:20)*140,COMMWD(1:20)*8,&
   &QUALWD(1:20)*8,STRING(1:20)*80,CH*140,&
   &ANW1(1:20)*23,ANW2(1:20)*23,ANW3(1:20)*23,&
   &ANW4(1:20)*23,ANW5(1:20)*23,ANW(1:20)*140,&
   &DM1*140,DM2*140,BLJK*80 &
   &,JK_INP(1:5)*80
!
   CHARACTER RA(0:6)*140,REMAIN*140,AHOLD1*23,AHOLD2*23 &
   &,AHOLD3*23,AHOLD4*23,AHOLD5*23,AHOLDWQ*6
!
   COMMON/FAST7/RA
!
   LOGICAL ALLB,COLREP,JJK_FLG1(1:5),JJK_FLG2
!
   INTEGER IQ,NSTRUC,COMTST,JK_DF(1:5),JK_I,&
   &BLNK,BSUM,BVAL,STATQL(1:20),STATST(1:20),STATN1(1:20)&
   &,STATN2(1:20),STATN3(1:20),STATN4(1:20),STATN5(1:20)&
   &,QBVAL,STATCO(1:20),STATBL(1:20),STATNW(1:20)&
   &,STATC2(1:20),STBLK2(1:20),IJK &
   &,I,K,J,L,KKI,KI,M,K1,JKII,HOLDSQ
!
   INTEGER QBVL1,DFSTA1(1:20),DFSTA2(1:20)&
   &,DFSTA3(1:20),DFSTA4(1:20),DFSTA5(1:20)&
   &,SI(1:20),&
   &QBVL,BI,IP
!
   real(real64) NW1(1:20),NW2(1:20),NW3(1:20),NW4(1:20)&
   &,NW5(1:20)&
   &,JK_NUM(1:5),HOLD1,HOLD2,HOLD3,HOLD4,HOLD5
!
   COMMON/CBLANK/NW1,NW2,NW3,NW4,NW5,STATNW,STATN1,STATN2,&
   &STATN3,STATN4,STATN5,STATCO,STATC2,STATBL,STBLK2,STATQL,&
   &STATST,SI,BI
!
   COMMON/CCBLAN/COMMWD,QUALWD,STRING,ANW,ANW1,ANW2,ANW3,ANW4,&
   &ANW5
!
   COMMON/PRO22/INSTRC,NSTRUC
!
!
   DM1=AA//AA//AA//AA//AA//AA//AA
   DM2=AA//AA//AA//AA//AA//AA//AA
!               AT THIS POINT INITIALIZE
!               COMMWD(I),QUALWD(I),STRING(I),ANW1(I),ANW2(I),ANW3(I),
!               ANW4(I),AND ANW5(I). THESE WILL BE THE REPOSITORIES
!               OF THE COMMAND,QUALIFIER,STRING,AND ALPHA-NUMERIC
!               REPRESENTATIONS OF THE NUMERIC WORDS.
!
!
   DO 1036 I=1,NUMCOM
      COMMWD(I)=BB
      QUALWD(I)=BB
      STRING(I)=AA//AA//AA//AA
      ANW(I)=AA//AA//AA//AA//AA//AA//AA
      ANW1(I)=AA//'   '
      ANW2(I)=AA//'   '
      ANW3(I)=AA//'   '
      ANW4(I)=AA//'   '
      ANW5(I)=AA//'   '
      NW1(I)=0.0D0
      NW2(I)=0.0D0
      NW3(I)=0.0D0
      NW4(I)=0.0D0
      NW5(I)=0.0D0
1036 CONTINUE
!
!               NOW  EACH ENTRY IN THE INSTRC ARRAY MUST BE BROKEN UP
!               INTO A COMMAND WORD, QUALIFIER WORD, NUNERIC WORDS,
!               OR ALPHANUMERIC STRING AS APPROPRIATE.
!
!               FOR EACH ENTRY IN THE INSTRC ARRAY THERE WILL
!               BE A CORRESPONDING SET OF ENTRIES IN STATUS
!               ARRAYS WHICH WILL TRACK THE PRESENCE OF ABSCENCE
!               OF A QUALIFIER WORD, FIVE NUMERIC WORDS AND AN
!               ALPHA-NUMERIC STRING. THE STATUS ARRAY FOR THE
!               PRESENCE OR ABSCENCE OF A QUALIFIER WORD IS
!               STATQL(I) WHERE I IS THE INDEX OF THE INSTRUCTION
!               ORIGINALLY STORED IN THE INSTRC(I) ARRAY.
!               IF THERE IS A QUALIFIER WORD FOR THE COMMAND
!               STORED IN INSTRC(I), THEN STATQL(I) WILL BE SET
!               TO 1. IF THERE IS NO QUALIFIER WORD, STATQL(I)
!               WILL BE SET TO 0(ZERO).
!               SIMILAR ARRAYS TO TRACK THE STATUS OF AN
!               ALPHA-NUMERIC STRING OR THE 5 NUMERIC WORDS ARE
!               STATST(I),STATN1(I),STATN2(I),STATN3(I),STATN4(I)
!               AND STATN5(I).
!               THESE STATUS ARRAYS WILL FOLLOW THE INSTRC(I)
!               INSTRUCTIONS TO THE COMMAND EXECUTION SECTION
!               OF THIS PROGRAM.
!
!               THE FIRST EIGHT NON-BLANK CONSECUTIVE ALPHA-
!               NUMERIC CHARACTERS (NOT COUNTING LEADING BLANKS
!               WHICH ARE DISCARDED), ARE CONSIDERED BY THIS
!               PROGRAM TO BE THE COMMAND WORD OF AN INSTRUCTION
!               LINE. THE FIRST CHARACTER OF A COMMAND WORD IS THE
!               FIRST NON-BLANK CHARACTER IN AN INSTRC ARRAY
!               ENTRY. THE LAST CHARACTER IN A COMMAND WORD IS
!               EITHER THE EIGTH CONSECUTIVE NON-BLANK CHARACTER
!               IN AN INSTRC ARRAY ENTRY OR IT IS THE LAST NON-BLANK
!               CONSECUTIVE CHARACTER IN AN INSTRC ARRAY ENTRY.
!               WHEN THE COMMAND PROCESSOR ENCOUNTERS ONE OR MORE BLANK
!               CHARACTERS WHICH ARE NOT LEADING BLANKS, IT ASSUMES
!               THAT COMMAND WORD ENTRY HAS TERMINATED. THE INSTRUCTION
!               PROCESSOR THEN LOOKS FOR QUALIFIER AND OTHER INPUT.
!
!               THE COMMAND WORD ENTRY FROM ARRAY INSTRC(I) IS THEN
!               STORED IN THE COMMAND WORD ARRAY COMMWD(I).
!
!               NOW BREAK OUT THE COMMAND WORDS FROM THE INSTRC ARRAY.
!
!                       THE VARIABLE NSTRUC IS THE COUNT OF THE
!                       NUMBER OF PENDING ISTRUCTIONS STORED IN
!                       THE INSTRC ARRAY. MAX VALUE IS NUMCOM.
!
!
!               HERE SETUP STATUS ARRAYS AND INITIALIZE TO 2
!               WHICH MEANS NOT YET ASSIGNED.
!
   DO 1025 I=1,NUMCOM

!
!       0 MEANS NO, 1 MEANS YES, 2 MEANS NOT YES ASSIGNED STATUS
!
!               EVERYTHING IN AN INSTRUCTION IS BLANK
      STATBL(I)=2
!               QUALIFIER IS BLANK
      STATQL(I)=2
!               STRING IS BLANK
      STATST(I)=2
!               ALL NWS ARE ZERO
      STATNW(I)=2
!               NW1 TO NW5 ARE ZERO
      STATN1(I)=2
      STATN2(I)=2
      STATN3(I)=2
      STATN4(I)=2
      STATN5(I)=2
!               FIRST COMMA PRESENT
      STATCO(I)=2
!               COMMA AFTER QUAL PRESENT
      STATC2(I)=2
!               ALL BLANK EXCEPT COMMAND WORD
      STBLK2(I)=2
!               THE NUMERIC WORDS HAVE THE "DEFAULT" VALUE
!       YES=1,NO=0,NOT ASSIGNED=2
      DFSTA1(I)=2
      DFSTA2(I)=2
      DFSTA3(I)=2
      DFSTA4(I)=2
      DFSTA5(I)=2
!
!       IS THE INTERROGATOR ? PRESENT. YES=1,NO=0,NOT
!       ASSIGNED =0 STATUS INDICATOR IS SI(I)
      SI(I)=0

1025 CONTINUE
!
!       PROCESS EACH INSTRC(I) INSTRUCTION NOW (MAIN LOOP)
!
   DO 1015 I=1,NSTRUC
      rawCommands(I) = INSTRC(I)



!
!               CHECK FOR AND REMOVE FROM EACH INSTRC(I) ANY
!               AND ALL LEADING BLANKS. (ASCII CHARACTER 32)
!
      K = 1
1016  COMTST = ICHAR(INSTRC(I)(1:1))
      IF(COMTST.EQ.32) THEN
         INSTRC(I) = (INSTRC(I)(2:140))
!       A BLANK WAS REMOVED
         K = K + 1
         IF(K.GT.50) THEN
!       COMMAND WORD WAS ALL BLANK. IN THIS CASE WE SET THE ENTIRE
!       LINE TO BLANK AS WITHOUT A COMMAND WORD WE CAN NOT HAVE
!       QUALIFIER,STRING OR NUMERIC INPUT.
!
!       HERE TEST FOR NSTRUC=1. IF SO SET FLAG 50 = 1
!       ONLY IF FLAG 50 = 1 AND THE INSTRUCTION LINE WAS
!       INTENTIONALLY SET TO BLANK WILL THIS ALLOW
!       SUBROUTINE BLANK TO PRINT THE PROGRAM NAME IN RESPONSE
!       TO A SINGLE CARRIAGE RETURN.
!
            IF(NSTRUC.EQ.1) F50=1
            IF(NSTRUC.NE.1) F50=0
!
!       IF INFACT A BLANK COMMAND WORD WAS INTENDED TO BE ENTERED
!       BY EITHER JUST PRESSING THE RETURN KEY OR BY ENTERING A
!       BLANK SPACE FOLLOWED BY A VIRTUAL CARRIAGE RETURN (;) OR A
!       REAL CARRIAGE RETURN, THE INITIALIZATION PROCESS FOR
!       ARRAY INSTRC(I) LEAVES EACH SPACE FILLED WITH A BLANK THUS
!       THAT ENTRY IN THE INSTRC ARRAY WILL BE BLANK IN ALL 140
!       CHARACTERS. WHEN ONLY ONE BLANK IS STORED IN THE FIRST
!       POSITIONS OF AN INSTRC(I) LINE, ALL 140 CHARACTERS WILL
!       LATER TEST AS BLANK.
!
!       IF 140 CHARACTERS TEST BLANK,THEN CERTAINLY THE FIRST
!       EIGHT WILL.
!
!       CALL SETBLN WHERE ALL VALUES SET TO BLANK INSTRUCTION
!       THEN SKIP TO NEXT STACKED INSTRUCTION.
!
            BI=I
            CALL SETBLN
            GO TO 1015
         ELSE
            GO TO 1016
         END IF
      END IF
!       THERE WERE NO LEADING BLANKS
!
!     CHECK FOR SPECIAL NUMEIC INPUT  2/26/96
      IF(IN.EQ.5) THEN
         IF(INSTRC(I)(1:1).EQ.'+'.OR.INSTRC(I)(1:1).EQ.'-') THEN
            IF(INSTRC(I)(2:2).NE.' ') THEN
!     SPECIAL INPUT OF X, PROCESS AND PROCEED
               INSTRC(I)(1:140)='SET X,'//INSTRC(I)(1:34)
            END IF
         END IF
         IF(INSTRC(I)(1:1).EQ.'1'.OR.INSTRC(I)(1:1).EQ.'2'.OR.&
         &INSTRC(I)(1:1).EQ.'3'.OR.INSTRC(I)(1:1).EQ.'4'.OR.&
         &INSTRC(I)(1:1).EQ.'5'.OR.INSTRC(I)(1:1).EQ.'6'.OR.&
         &INSTRC(I)(1:1).EQ.'7'.OR.INSTRC(I)(1:1).EQ.'8'.OR.&
         &INSTRC(I)(1:1).EQ.'0'.OR.INSTRC(I)(1:1).EQ.'.'.OR.&
         &INSTRC(I)(1:1).EQ.'9') THEN
!     SPECIAL INPUT OF X
            INSTRC(I)(1:140)='SET X,'//INSTRC(I)(1:34)
         END IF
      END IF
!     NOW PROCEED WITH NORMAL PROCESSING
!
!       CONTINUE PROCESSING.
!
!               NOW MAKE EACH COMMAND WORD TERMINATE IF A BLANK
!               OR COMMA
!               OCCURS WITHIN THE FIRST EIGHT CHARACTERS OF EACH
!               VIRTUAL COMMAND LINE.
!               STRIP OFF THE COMMAND WORD AND FILE IT IN THE
!               COMMWD(I) ARRAY AND STORE THE REMAINDER OF THE
!               COMMAND BACK IN THE INSTRC(I) ARRAY.
!
      DO 1022 J=2,9
         BLNK = ICHAR(INSTRC(I)(J:J))
         IF(BLNK.EQ.32.OR.BLNK.EQ.44) THEN
            COMMWD(I)=(INSTRC(I)(1:(J-1)))
            INSTRC(I)=(INSTRC(I)(J:140))
            GO TO 1023
         END IF
1022  CONTINUE
      COMMWD(I)=(INSTRC(I)(1:8))
      INSTRC(I)=(INSTRC(I)(9:140))
1023  CONTINUE
!
!               NOW THE COMMAND WORD AND THE REMAINDER WITHOUT
!               THE COMMAND WORD ARE STORED IN COMMWD(I) AND
!               INSTRC(I).
!
!               NOW WE TAKE THE REMAINING PIECE OF EACH ORIGINAL
!               INSTRUCTION,EXAMINE IT, AND TURN IT INTO
!               QUALIFIERS, NUMERIC WORDS, OR A STRING EXPRESSION.
!
!               IF THE FIRST NON-BLANK CHARACTER ENCOUNTERED IN
!               THE REMAINING INTRUCTION IS A COMMA (ASCII 44)
!               THEN THE PROGRAM EXPECTS EITHER A NUMERIC WORD
!               OR AN ALPHANUMERIC STRING. IF THE NEXT INPUT IS
!               AN ALPHANUMERIC STRING THEN THE NEXT NON-BLANK
!               CHARACTER AFTER THE COMMA MUST BE A COLON
!               (ASCII 58).
!               IF THE NEXT NON-BLANK CHARACTER AFTER THE COMMAND WORD
!               IS NOT A COMMA OR A COMMA FOLLOWED BY A COLON
!               THEN THE PROGRAM WILL INTERPRET THE NEXT NON-BLANK
!               CHARACTERS TO BE A QUALIFIER WORD UNLESS THE FIRST
!               CHARACTER OF THE NEXT NON-BLANK CHARCATER IS EITHER
!               A (,),+,-,1,2,3,4,5,6,7,8,9,0,OR,. IF THESE CHARACTERS
!               ARE FIRST SEEN THEN THE NEXT NON-BLANK INPUT IS
!               ONLY NUMERIC. IN THIS CASE A DISCOVERED LEADING BLANK
!               IS REPLACED WITH A COMMA AND THE PROCESSING CONTINUES.
!               IF THE FIRST NON-BLANK CHARACTER AFTER THE BLANK IS
!               A (:) THEN THE NEXT DATA IS STRING DATA AND THE
!               ONE OR MORE BLANKS WITHOUT A COMMA ARE USED
!               TO SEPARATE THE COMMAND WORD FROM THE QUALIFIER.
!               ONE OR MORE BLANKS WITH OR WITHOUT A COMMA CAN SEPARATE
!               THE SECOND THRU FIFTH NUMERIC WORDS FROM EACH OTHER AND
!               FROM THE FIRST NUMERIC WORD. BLANKS CAN ALWAYS
!               OCCUR WITHIN ALPHANUMERIC STRINGS AND ARE THEREIN
!               TREATED AS PART OF THE STRING. COLONS CAN
!               OCCUR WITHIN ALPHANUMERIC STRINGS. ONLY THE FIRST
!               OCCURENCE OF A COLON STARTING AN ALPHANUMERIC
!               STRING IS LOOKED FOR AND TRACKED.
!
!               THE FIRST TEST IS TO EXAMINE EACH INSTRC(I) ENTRY
!               AND DETERMINE IF IT IS ALL BLANK. IF IT IS,THEN ONLY
!               A COMMAND WORD MAKES UP THE CURRENT PROGRAM
!               INSTRUCTION.
!
!       BEGIN CHECKING FOR ALL BLANKS EXCEPT FOR THE COMMAND WORD.
!
      BSUM = 0
      BVAL = 0
      DO 1027 K=2,140
         BVAL =(ICHAR(INSTRC(I)(K:K))-32)
         BSUM = BSUM + BVAL
1027  CONTINUE
!               IF BSUM IS EQUAL TO ZERO THEN ALL OF INSTRC(I) IS
!               BLANK. REMEMBER, BY NOW THE COMMAND WORDS HAVE BEEN
!               REMOVED FROM INSTRC(I). IF THE COMMAND WORD WAS
!               INTERPRETED AS BLANK THEN THE REST OF THAT
!               INSTRUCTION LINE WAS SET TO BLANK AS WELL.
!               THAT OCCURED EARLIER IN THE CODE.
!
      IF(BSUM.EQ.0) THEN
!
         STATQL(I)=0
         STATST(I)=0
         SI(I)=0
         STATNW(I)=0
         STATN1(I)=0
         STATN2(I)=0
         STATN3(I)=0
         STATN4(I)=0
         STATN5(I)=0
         STATCO(I)=0
         STATC2(I)=0
         STBLK2(I)=1
         STATBL(I)=0
         DFSTA1(I)=1
         DFSTA2(I)=1
         DFSTA3(I)=1
         DFSTA4(I)=1
         DFSTA5(I)=1
!
         GO TO 1015
!       GO TO THE NEXT INSTRUCTION
!
      ELSE
!
!       REMAINDER INSTC(I) IS NOT ALL BLANK, CONT. PROCESSING.
!
!       HERE IS WHERE THE SEARCH FOR SPECIFIC NON-BLANK INPUT BEGINS.
!
!       FIRST LOOK FOR THE FIRST OCCURENCE OF A COMMA OR BLANK IN EACH
!       INSTRC(I). DISCARD ANY OTHER CHARACTERS WHICH OCCUR BEFORE THE
!       FIRST COMMA(ASCII 44) OR BLANK(ASCII 32). REFILE EACH INSTRC(I)
!       SO THAT IT STARTS WITH A COMMA OR BLANK. WE ALREADY KNOW THAT
!       THE INSTRC(I)S WHICH WE ARE LOOKING AT ARE NOT ALL BLANK.
!
1032     QBVAL = 0
         L=1
         QBVAL = ICHAR(INSTRC(I)(L:L))
         IF(QBVAL.NE.32.AND.QBVAL.NE.44) THEN
            INSTRC(I)= (INSTRC(I)((L+1):140))
            GO TO 1032
         ELSE
            GO TO 1033
         END IF
      END IF
1033  CONTINUE
!
!       NOW EACH ORIGINAL INSTRC(I) HAS BEEN BROKEN UP INTO
!       A COMMAND WORD COMMWD(I) AND A REMAINDER INSTRC(I).
!       IF THE REMAINDER WAS BLANK THEN ALL THE STATUS ARRAYS
!       ARE SET TO ZERO.
!       IF THE COMMAND WORD WAS BLANK THEN IT IS STORED AS 140
!       CHARCTERS OF BLANK.
!
!       NOW EXAMINE EACH REMAINING INSTRC(I) INSTRUCTION (THEY
!       NOW ALL START WITH EITHER A BLANK OR A COMMA)
!       AND DETERMINE THEIR CONTENTS IN TERMS OF
!       QUALIFIERS,STRINGS,OR NUMERIC WORDS.
!
!       CHECK FOR THE PRESENCE OF A QUALIFIER WORD.
!       IF THERE IS A QUALIFIER WORD THEN THE FIRST NON-BLANK
!       CHARACTER IN INSTRC(I) WILL NOT BE A COMMA(ASCII 44) OR
!       ONE OF THE SPECIAL NUMERIC WORD CHARACTERS +,-,1,2,3,4,5
!       6,7,8,9,0,),(,OR,.
!
!       IF THE FIRST NON-BLANK CHARACTER IS A COMMA
!       THEN THERE IS NO QUALIFIER WORD.
!       ACCORDING TO THE RULES SET DOWN, QUALIFIER WORDS
!       ARE SEPARATED FROM COMMAND WORDS BY ONE OR MORE
!       BLANKS (ASCII 32) AND NEVER BY A COMMA. FURTHERMORE,
!       QUALIFIER WORDS NEVER BEGIN WITH A COLON (:, ASCII
!       58 OR COMMAS ASCII 44).
!       BREAK OFF THE QUALIFIER IN A SIMILAR MANNER TO THE
!       COMMAND WORD SEPARATION DONE EARLIER AND FILE THE REMAINER
!       OF THE INSTRC(I) BACK INTO THE INSTRC(I) ARRAY. IF NO
!       QUALIFIER IS FOUND, SET STATQL(I)=0 AND PROCEED TO
!       LOOK FOR AN ALPHA-NUMERIC STRING OR NUMERIC WORDS.
!
!******************************************************************
!
!       SUPPOSE THAT INSTRC(I) STARTS WITH A BLANK BUT THEN
!       HAS SEVERAL MORE BLANKS BEFORE ENCOUNTERING A COMMA
!       OR ANOTHER NON-BLANK CHARACTER. ALSO SUPPOSE THAT
!       BY NOW INSTRC(I) IS ALL BLANK.
!
!       TEST FOR ALL BLANKS
!
      BSUM=0
      BVAL=0
      DO 7020 KKI=1,140
         BVAL=(ICHAR(INSTRC(I)(KKI:KKI))-32)
         BSUM=BSUM+BVAL
7020  CONTINUE
      IF(BSUM.EQ.0) THEN
!       THE REST OF INSTRC(I) = BLANK
!       SET STATUS INDICATORS ACCORDINGLY AND GO TO NEXT
!       INSTRUCTION.
!
         STATQL(I)=0
         STATST(I)=0
         SI(I)=0
         STATNW(I)=0
         STATN1(I)=0
         STATN2(I)=0
         STATN3(I)=0
         STATN4(I)=0
         STATN5(I)=0
         STATCO(I)=0
         STATC2(I)=0
         STBLK2(I)=1
         STATBL(I)=0
         DFSTA1(I)=1
         DFSTA2(I)=1
         DFSTA3(I)=1
         DFSTA4(I)=1
         DFSTA5(I)=1
         GO TO 1015
!       CONTINUE TO PROCESS
      END IF
!       THE ENTIRE INSTRUCTION WAS NOT BLANK. REMOVE LEADING
!       BLANKS UNTIL A NON-BLANK IS FOUND.
!
7029  KI=1
      QBVAL=ICHAR(INSTRC(I)(KI:KI))
      QBVL=ICHAR(INSTRC(I)((KI+1):(KI+1)))
      IF(QBVAL.EQ.44) GO TO 7030
      IF(QBVAL.NE.32.AND.QBVAL.NE.44) THEN
!
         OUTLYNE='SERIOUS ERROR RESOLVING EXTRA BLANKS, SEE'
         CALL SHOWIT(1)
         OUTLYNE='STATEMENT LABLE 7029/7030 IN SUBROUTINE "PROCESS"'
         CALL SHOWIT(1)
         RETURN
      END IF
      IF(QBVAL.EQ.32.AND.QBVL.EQ.44.OR.QBVAL.EQ.32.AND.&
      &QBVL.EQ.32) THEN
         INSTRC(I)=(INSTRC(I)((KI+1):140))
         GO TO 7029
      ELSE
!       DON'T REMOVE THE BLANK. GET OUT AND CONTINUE
         GO TO 7030
      END IF
7030  CONTINUE
!
!*******************************************************************
!
!       HERE WE MUST HANDLE THE CASE OF A COMMA FOLLOWED BY
!       MORE BLANKS BEFORE MORE NON-BLANK CHARACTERS.
!       LOOK NOW IF THE FIRST CHARACTER IS A COMMA. IF THE
!       ANSWER IS YES THE REMOVE ANY BLANKS WHICH FOLLOW IT
!       UP TO BUT NOT INCLUDING THE NEXT NON-BLANK
!       CHARACTER
!
7010  QBVAL=ICHAR(INSTRC(I)(1:1))
      IF(QBVAL.EQ.44) THEN
         QBVL1=ICHAR(INSTRC(I)(2:2))
         IF(QBVL1.EQ.32) THEN
            DM1(1:1)=(INSTRC(I)(1:1))
            DM2(3:140)=(INSTRC(I)(3:140))
            INSTRC(I)=DM1(1:1)//DM2(3:140)
            GO TO 7010
         ELSE
!       DON'T REMOVE ANYTHING, PROCEED WITH PROCESSING
            GO TO 7011
         END IF
      ELSE
!       DON'T REMOVE ANYTHING, PROCEED WITH PROCESSING
         GO TO 7011
      END IF
7011  CONTINUE
!
!               THE ABOVE SMALL LOOP REMOVES UNWANTED BLANKS
!               OF THE FORM:
!
!       COMMAND WORD,    NUMERIC WORD
!
!*******************************************************************
!
!       HERE TEST FOR A BLANK. IF YES THERE IS A QUALIFIER
!       UNLESS THE NEXT NON-BLANK CHARACTER IS A (,+,-,1,2,3
!       4,5,6,7,8,9,.,:,),OR ?).
!       IF NOT THEN THERE IS NO QUALIFIER BUT THERE MAY BE
!       NUMERIC OR STRING DATA
!
      QBVAL=ICHAR(INSTRC(I)(1:1))
      IF(QBVAL.NE.44) THEN
!
!       REMOVE LEADING BLANKS AND CHECK FOR SPECIAL
!       CHARACTERS.
!
!               START THE QUALIFIER SEARCH HERE.
!
         K=1
         QBVAL=ICHAR(INSTRC(I)(1:1))
!       FIRST REMOVE THE LEADING BLANKS. (WILL HANDLE 20 OF THEM)
1050     IF(K.GT.50) GO TO 1052
         IF(QBVAL.EQ.32) THEN
            INSTRC(I)=(INSTRC(I)(2:140))
!       REFORM QBVAL
            QBVAL=ICHAR(INSTRC(I)(1:1))
!               ADD 1 TO K
            K=K+1
            GO TO 1050
         ELSE
            GO TO 1053
         END IF
!       IF 20 REMOVALS STILL ONLY SEE BLANKS THEN ALL IS BLANK
1052     INSTRC(I)=AA//AA//AA//AA//AA//AA//AA
!               INSTRC(I) SET TO BLANKS AS EIGTHY BLANKS
!               WERE FOUND BEFORE A QUALIFIER.
         QUALWD(I)=BB
         STATQL(I)=0
         STATBL(I)=0
         STBLK2(I)=1
!       ASSUME NO NUMERIC OR STRING VALUES PRESENT EITHER.
         STATST(I)=0
         SI(I)=0
         STATNW(I)=0
         STATCO(I)=0
         STATC2(I)=0
         STATN1(I)=0
         STATN2(I)=0
         STATN3(I)=0
         STATN4(I)=0
         STATN5(I)=0
         DFSTA1(I)=1
         DFSTA2(I)=1
         DFSTA3(I)=1
         DFSTA4(I)=1
         DFSTA5(I)=1
         GO TO 1015
1053     CONTINUE
!
!       ALL LEADING BLANKS ARE REMOVED
!
!       NEW SPECIAL FEATURE ADDED ON 3/11/89 TO ALLOW 13 CHARACTER
!       GLASS NAMES IF THE COMMAND WORD IS:
         COLREP=.FALSE.
!               SCHOTT
!               SCH2000
!               PFANLBL
!               PFANCAP
!               OHARA
!               HOYA
!               HIKARI
!               CORNIN
!               CHANCE
!               GLCAT
!               MATL
!               RUSSIAN
!               RADHARD
!               USER
!               C
!               M
!               LI
!               LIC
!               FIGURE
!               RE
!               SYS OR SYSTEM
!               LFORMAT
!               LBL OR LABEL
!               STWORD
!               SURFCOAT
!               TITLE
!               DWGNO
!               SURFMATL
!               SURFQUAL
!               CONAME
!               EDIT
!               MAC_EDIT
!               LENSLOC
!               LENSSAVE
!               LSAVE
!               LENSREST
!               LOADPROF
!
!
         IF(gdb%isNameInCatalog(COMMWD(I))) COLREP=.TRUE.
         IF(COMMWD(I).EQ.'SURFCOAT')COLREP=.TRUE.
         IF(COMMWD(I).EQ.'TITLE')   COLREP=.TRUE.
         IF(COMMWD(I).EQ.'DWGNO')   COLREP=.TRUE.
         IF(COMMWD(I).EQ.'SURFMATL')COLREP=.TRUE.
         IF(COMMWD(I).EQ.'SURFQUAL')COLREP=.TRUE.
         IF(COMMWD(I).EQ.'CONAME')  COLREP=.TRUE.
         IF(COMMWD(I).EQ.'PFANLBL') COLREP=.TRUE.
         IF(COMMWD(I).EQ.'PFANCAP') COLREP=.TRUE.
         IF(COMMWD(I).EQ.'GLAK')     COLREP=.TRUE.
         IF(COMMWD(I).EQ.'GLCAT')   COLREP=.TRUE.
         IF(COMMWD(I).EQ.'USER')    COLREP=.TRUE.
         IF(COMMWD(I).EQ.'CK')       COLREP=.TRUE.
         IF(COMMWD(I).EQ.'M')       COLREP=.TRUE.
         IF(COMMWD(I).EQ.'LI')      COLREP=.TRUE.
         IF(COMMWD(I).EQ.'PNOTE')   COLREP=.TRUE.
         IF(COMMWD(I).EQ.'LENS')    COLREP=.TRUE.
         IF(COMMWD(I).EQ.'MFG')     COLREP=.TRUE.
         IF(COMMWD(I).EQ.'CATNAME') COLREP=.TRUE.
         IF(COMMWD(I).EQ.'ONAME   ')COLREP=.TRUE.
         IF(COMMWD(I).EQ.'SNAME   ')COLREP=.TRUE.
         IF(COMMWD(I).EQ.'PSFLI')   COLREP=.TRUE.
         IF(COMMWD(I).EQ.'PSFTAG')  COLREP=.TRUE.
         IF(COMMWD(I).EQ.'LIC')     COLREP=.TRUE.
         IF(COMMWD(I).EQ.'CV2PRG')  COLREP=.TRUE.
         IF(COMMWD(I).EQ.'ZMX2PRG') COLREP=.TRUE.
         IF(COMMWD(I).EQ.'INI')     COLREP=.TRUE.
         IF(COMMWD(I).EQ.'LTYPE')   COLREP=.TRUE.
         IF(COMMWD(I).EQ.'FIGURE')  COLREP=.TRUE.
         IF(COMMWD(I).EQ.'RE')      COLREP=.TRUE.
         IF(COMMWD(I).EQ.'SYS')     COLREP=.TRUE.
         IF(COMMWD(I).EQ.'SYSTEM')  COLREP=.TRUE.
         IF(COMMWD(I).EQ.'WSYS')    COLREP=.TRUE.
         IF(COMMWD(I).EQ.'WSYSTEM') COLREP=.TRUE.
         IF(COMMWD(I).EQ.'LFORMAT') COLREP=.TRUE.
         IF(COMMWD(I).EQ.'LBL')     COLREP=.TRUE.
         IF(COMMWD(I).EQ.'LABEL')   COLREP=.TRUE.
         IF(COMMWD(I).EQ.'STWORD')  COLREP=.TRUE.
         IF(COMMWD(I).EQ.'EDIT')    COLREP=.TRUE.
         IF(COMMWD(I).EQ.'SETAX')   COLREP=.TRUE.
         IF(COMMWD(I).EQ.'PROMPT')  COLREP=.TRUE.
         IF(COMMWD(I).EQ.'PREAD')   COLREP=.TRUE.
         IF(COMMWD(I).EQ.'FORMAT')  COLREP=.TRUE.
         IF(COMMWD(I).EQ.'OTOBMP')  COLREP=.TRUE.
         IF(COMMWD(I).EQ.'ITOBMP')  COLREP=.TRUE.
         IF(COMMWD(I).EQ.'BMPREADR')COLREP=.TRUE.
         IF(COMMWD(I).EQ.'MAC_EDIT')COLREP=.TRUE.
         IF(COMMWD(I).EQ.'CAPFNIN') COLREP=.TRUE.
         IF(COMMWD(I).EQ.'CAPFNOUT')COLREP=.TRUE.
         IF(COMMWD(I).EQ.'LENSLOC') COLREP=.TRUE.
         IF(COMMWD(I).EQ.'LENSSAVE')COLREP=.TRUE.
         IF(COMMWD(I).EQ.'LSAVE')   COLREP=.TRUE.
         IF(COMMWD(I).EQ.'LENSREST')COLREP=.TRUE.
         IF(COMMWD(I).EQ.'LOADPROF')COLREP=.TRUE.
!
!       IF COMMWD(I) IS ONE OF THESE, THEN THE REMAINDER OF INSTR
!       IS A STRING
         IF(COLREP) THEN
            COLREP=.FALSE.
!
            QUALWD(I)='        '
            STATQL(I)=0
            STRING(I)=INSTRC(I)
            STATST(I)=1
!     STRIP LEADING COMMA
            DO IJK=1,9
               IF(STRING(I)(1:1).EQ.',') THEN
                  BLJK=STRING(I)(2:80)
                  STRING(I)=AA//AA//AA//AA
                  STRING(I)=BLJK
               ELSE
               END IF
            END DO
!     STRIP LEADING SPACES UNLESS COMMAND WORD IS SPECIAL
            IF(COMMWD(I).NE.'M'.AND.COMMWD(I).NE.'CK') THEN
               DO IJK=1,9
                  IF(STRING(I)(1:1).EQ.' ') THEN
                     BLJK=STRING(I)(2:80)
                     STRING(I)=AA//AA//AA//AA
                     STRING(I)=BLJK
                  ELSE
                     GO TO 8763
                  END IF
               END DO
            ELSE
            END IF
8763        CONTINUE
!     STRIP LEADING COLON
            DO IJK=1,1
               IF(STRING(I)(1:1).EQ.':') THEN
                  BLJK=STRING(I)(2:80)
                  STRING(I)=AA//AA//AA//AA
                  STRING(I)=BLJK
               ELSE
                  GO TO 8764
               END IF
            END DO
8764        CONTINUE
            IF(STRING(I)(1:1).EQ.'?') THEN
               STATST(I)=0
               SI(I)=1
            ELSE
               STATST(I)=1
               SI(I)=0
            END IF
            NW1(I)=0.0
            NW2(I)=0.0
            NW3(I)=0.0
            NW4(I)=0.0
            NW5(I)=0.0
            STATQL(I)=0
            STATBL(I)=0
            STBLK2(I)=0
            STATNW(I)=0
            STATN1(I)=0
            STATN2(I)=0
            STATN3(I)=0
            STATN4(I)=0
            STATN5(I)=0
            STATCO(I)=0
            STATC2(I)=0
            STATN1(I)=0
            STATN2(I)=0
            STATN3(I)=0
            STATN4(I)=0
            STATN5(I)=0
            DFSTA1(I)=1
            DFSTA2(I)=1
            DFSTA3(I)=1
            DFSTA4(I)=1
            DFSTA5(I)=1
            GO TO 1015
!       CONTINUE PROCESSING INSTRC
         END IF
!       QUALIFIER WORDS CANNOT BEGIN WITH A ),(,0,1,2,3,4
!       5,6,7,8,9,.,+,-,:,OR ? IF THESE
!       ARE ENCOUNTERED THEN
!       A COMMA IS INSERTED AHEAD OF THE NEXT NON-BLANK CHARACTER
!       AND THE DATA IS PROCESSED AS NUMERIC OR
!       STRING DATA. THIS ALLOWS FOR THE COMMA
!       BETWEEN COMMAND AND NUMERIC OR STRING
!       AND THE COMMA BETWEEN QUAL AND NUMERIC OR
!       STRING TO BE OPTIONAL.
!
!       CHECK FOR SPECIAL CHARACTERS BEGINS HERE.
         QBVAL = 0
         QBVAL=ICHAR(INSTRC(I)(1:1))
         IF(QBVAL.EQ.48.OR.QBVAL.EQ.49.OR.&
         &QBVAL.EQ.50.OR.QBVAL.EQ.51.OR.QBVAL.EQ.&
         &52.OR.QBVAL.EQ.53.OR.QBVAL.EQ.54.OR.&
         &QBVAL.EQ.55.OR.QBVAL.EQ.56.OR.QBVAL.EQ.&
         &57.OR.QBVAL.EQ.46.OR.QBVAL.EQ.43.OR.&
         &QBVAL.EQ.45.OR.QBVAL.EQ.58.OR.QBVAL &
         &.EQ.63.OR.QBVAL.EQ.40.OR.QBVAL.EQ.41) THEN
!       REFORM INSTRC(I) WITH LEADING COMMA.
            CH= (INSTRC(I)(1:139))
            INSTRC(I)=','//CH
            QUALWD(I)=BB
            STATQL(I)=0
            STATCO(I)=1
            STATC2(I)=0
            STATBL(I)=0
            STBLK2(I)=0
         ELSE
!       NO SPECIAL CHARACTERS OCCURRED, THERE MUST BE A QUALIFIER
!       SO PROCESS OUT THE QUALIFIER.
!
!
!               THE QUALIFIER WORD IS ALSO SHORTENED TO A
!               MAXIMUM OF 8 CHARACTERS (BUT THE QUALIFIER
!               ALSO ENDS IF A BLANK OR COMMA IS ENCOUNTERED.
!               THUS FIRST EITHER
!               LOOK IN INSTRC(I) FOR THE FIRST COMMA,THE FIRST
!               BLANK OR THE FIRST EIGHT NON-BLANK/NON-COMMA
!               CHARACTERS, THEN FILE IN THE QUALIFIER
!               WORD QUALWD(I) ARRAY.
!
            QBVAL = 0
            DO 1043 M=2,9
               QBVAL = ICHAR(INSTRC(I)(M:M))
               IF(QBVAL.EQ.32.OR.QBVAL.EQ.44) THEN
!               THE END OF THE QUALIFIER WAS FOUND BEFORE EIGHT
!               CHARACTERS WERE SEARCHED. MAKE NEW QUALWD(I).
                  QUALWD(I)=(INSTRC(I)(1:(M-1)))
!               REMAINDER IS REFILED INTO INSTRC(I) FOR FUTURE SEARCH
                  INSTRC(I)=(INSTRC(I)(M:140))
                  GO TO 1044
               END IF
1043        CONTINUE
            QUALWD(I)=(INSTRC(I)(1:8))
            INSTRC(I)=(INSTRC(I)(9:140))
1044        CONTINUE
            STATQL(I)=1
            STATCO(I)=0
            STBLK2(I)=0
            STATBL(I)=0
         END IF
!
      ELSE
!
!               THERE IS NO QUAL BUT THERE IS NUMERIC
!               OR STRING DATA SO RESOLVE IT.
!
         QUALWD(I)=BB
         STATQL(I)=0
         STATCO(I)=1
         STATC2(I)=0
         STATBL(I)=0
         STBLK2(I)=0
!
!       NEW SPECIAL FEATURE ADDED ON 3/11/89 TO ALLOW 13 CHARACTER
!       GLASS NAMES IF THE COMMAND WORD IS:
         COLREP=.FALSE.
!               SCHOTT
!               SCH2000
!               PFANLBL
!               PFANCAP
!               OHARA
!               HOYA
!               HIKARI
!               CORNIN
!               CHANCE
!               GLCAT
!               MATL
!               RUSSIAN
!               RADHARD
!               USER
!               C
!               M
!               LI
!               LIC
!               FIGURE
!               RE
!               SYS OR SYSTEM
!               LFORMAT
!               LBL OR LABEL
!               STWORD
!               SURFCOAT
!               TITLE
!               DWGNO
!               SURFMATL
!               SURFQUAL
!               CONAME
!               EDIT
!               MAC_EDIT
!               LENSLOC
!               LENSSAVE
!               LSAVE
!               LENSREST
!               LOADPROF
!
!
         IF(COMMWD(I).EQ.'SURFCOAT')COLREP=.TRUE.
         IF(COMMWD(I).EQ.'TITLE')   COLREP=.TRUE.
         IF(COMMWD(I).EQ.'DWGNO')   COLREP=.TRUE.
         IF(COMMWD(I).EQ.'SURFMATL')COLREP=.TRUE.
         IF(COMMWD(I).EQ.'SURFQUAL')COLREP=.TRUE.
         IF(COMMWD(I).EQ.'CONAME')  COLREP=.TRUE.
         IF(COMMWD(I).EQ.'SCHOTT')  COLREP=.TRUE.
         IF(COMMWD(I).EQ.'SCH2000') COLREP=.TRUE.
         IF(COMMWD(I).EQ.'PFANLBL') COLREP=.TRUE.
         IF(COMMWD(I).EQ.'PFANCAP') COLREP=.TRUE.
         IF(COMMWD(I).EQ.'GLAK')     COLREP=.TRUE.
         IF(COMMWD(I).EQ.'OHARA')   COLREP=.TRUE.
         IF(COMMWD(I).EQ.'HOYA')    COLREP=.TRUE.
         IF(COMMWD(I).EQ.'HIKARI')  COLREP=.TRUE.
         IF(COMMWD(I).EQ.'CORNIN')  COLREP=.TRUE.
         IF(COMMWD(I).EQ.'CHANCE')  COLREP=.TRUE.
         IF(COMMWD(I).EQ.'GLCAT')   COLREP=.TRUE.
         IF(COMMWD(I).EQ.'RUSSIAN') COLREP=.TRUE.
         IF(COMMWD(I).EQ.'RADHARD') COLREP=.TRUE.
         IF(COMMWD(I).EQ.'USER')    COLREP=.TRUE.
         IF(COMMWD(I).EQ.'CK')       COLREP=.TRUE.
         IF(COMMWD(I).EQ.'M')       COLREP=.TRUE.
         IF(COMMWD(I).EQ.'LI')      COLREP=.TRUE.
         IF(COMMWD(I).EQ.'PNOTE')   COLREP=.TRUE.
         IF(COMMWD(I).EQ.'LENS')    COLREP=.TRUE.
         IF(COMMWD(I).EQ.'MFG')     COLREP=.TRUE.
         IF(COMMWD(I).EQ.'CATNUM')  COLREP=.TRUE.
         IF(COMMWD(I).EQ.'ONAME   ')COLREP=.TRUE.
         IF(COMMWD(I).EQ.'SNAME   ')COLREP=.TRUE.
         IF(COMMWD(I).EQ.'PSFLI')   COLREP=.TRUE.
         IF(COMMWD(I).EQ.'PSFTAG')  COLREP=.TRUE.
         IF(COMMWD(I).EQ.'LIC')     COLREP=.TRUE.
         IF(COMMWD(I).EQ.'CV2PRG')  COLREP=.TRUE.
         IF(COMMWD(I).EQ.'ZMX2PRG') COLREP=.TRUE.
         IF(COMMWD(I).EQ.'INI')     COLREP=.TRUE.
         IF(COMMWD(I).EQ.'LTYPE')   COLREP=.TRUE.
         IF(COMMWD(I).EQ.'FIGURE')  COLREP=.TRUE.
         IF(COMMWD(I).EQ.'RE')      COLREP=.TRUE.
         IF(COMMWD(I).EQ.'SYS')     COLREP=.TRUE.
         IF(COMMWD(I).EQ.'SYSTEM')  COLREP=.TRUE.
         IF(COMMWD(I).EQ.'WSYS')    COLREP=.TRUE.
         IF(COMMWD(I).EQ.'WSYSTEM') COLREP=.TRUE.
         IF(COMMWD(I).EQ.'LFORMAT') COLREP=.TRUE.
         IF(COMMWD(I).EQ.'LBL')     COLREP=.TRUE.
         IF(COMMWD(I).EQ.'LABEL')   COLREP=.TRUE.
         IF(COMMWD(I).EQ.'STWORD')  COLREP=.TRUE.
         IF(COMMWD(I).EQ.'EDIT')    COLREP=.TRUE.
         IF(COMMWD(I).EQ.'SETAX')   COLREP=.TRUE.
         IF(COMMWD(I).EQ.'PROMPT')  COLREP=.TRUE.
         IF(COMMWD(I).EQ.'PREAD')   COLREP=.TRUE.
         IF(COMMWD(I).EQ.'FORMAT')  COLREP=.TRUE.
         IF(COMMWD(I).EQ.'OTOBMP')  COLREP=.TRUE.
         IF(COMMWD(I).EQ.'ITOBMP')  COLREP=.TRUE.
         IF(COMMWD(I).EQ.'BMPREADR')COLREP=.TRUE.
         IF(COMMWD(I).EQ.'CAPFNIN') COLREP=.TRUE.
         IF(COMMWD(I).EQ.'CAPFNOUT')COLREP=.TRUE.
         IF(COMMWD(I).EQ.'LENSLOC') COLREP=.TRUE.
         IF(COMMWD(I).EQ.'LENSSAVE')COLREP=.TRUE.
         IF(COMMWD(I).EQ.'LENSREST')COLREP=.TRUE.
         IF(COMMWD(I).EQ.'LSAVE')   COLREP=.TRUE.
         IF(COMMWD(I).EQ.'LOADPROF')COLREP=.TRUE.

!
!       IF COMMWD(I) IS ONE OF THESE, THEN THE REMAINDER OF INSTR
!       IS A STRING
         IF(COLREP) THEN
            COLREP=.FALSE.
!
            QUALWD(I)='        '
            STATQL(I)=0
            STRING(I)=INSTRC(I)
            STATST(I)=1
!     STRIP LEADING COMMA
            DO IJK=1,9
               IF(STRING(I)(1:1).EQ.',') THEN
                  BLJK=STRING(I)(2:80)
                  STRING(I)=AA//AA//AA//AA
                  STRING(I)=BLJK
               ELSE
               END IF
            END DO
!     STRIP LEADING SPACES UNLESS COMMAND WORD IS SPECIAL
            IF(COMMWD(I).NE.'M'.AND.COMMWD(I).NE.'CK') THEN
               DO IJK=1,9
                  IF(STRING(I)(1:1).EQ.' ') THEN
                     BLJK=STRING(I)(2:80)
                     STRING(I)=AA//AA//AA//AA
                     STRING(I)=BLJK
                  ELSE
                     GO TO 9763
                  END IF
               END DO
            ELSE
            END IF
9763        CONTINUE
!     STRIP LEADING COLON
            DO IJK=1,1
               IF(STRING(I)(1:1).EQ.':') THEN
                  BLJK=STRING(I)(2:80)
                  STRING(I)=AA//AA//AA//AA
                  STRING(I)=BLJK
               ELSE
                  GO TO 9764
               END IF
            END DO
9764        CONTINUE
            IF(STRING(I)(1:1).EQ.'?') THEN
               STATST(I)=0
               SI(I)=1
            ELSE
               STATST(I)=1
               SI(I)=0
            END IF
            NW1(I)=0.0
            NW2(I)=0.0
            NW3(I)=0.0
            NW4(I)=0.0
            NW5(I)=0.0
            STATQL(I)=0
            STATBL(I)=0
            STBLK2(I)=0
            STATNW(I)=0
            STATN1(I)=0
            STATN2(I)=0
            STATN3(I)=0
            STATN4(I)=0
            STATN5(I)=0
            STATCO(I)=0
            STATC2(I)=0
            STATN1(I)=0
            STATN2(I)=0
            STATN3(I)=0
            STATN4(I)=0
            STATN5(I)=0
            DFSTA1(I)=1
            DFSTA2(I)=1
            DFSTA3(I)=1
            DFSTA4(I)=1
            DFSTA5(I)=1
            GO TO 1015
!       CONTINUE PROCESSING INSTRC
         END IF
!
      END IF
!               NOW RESOLVE THE REMAINDER
!
!       IF THERE WAS A QUALIFIER AND IF IT WAS TYPED IN AS MORE
!       THAN EIGTH CHARACTERS THEN THE REMAINDER IS IN INSTRC(I)
!       CHECK FOR AND REMOVE ALL LEADING NON-BLANK AND NON-COMMA
!       CHARACTERS.
!
9010  QBVAL=ICHAR(INSTRC(I)(1:1))
      IF(QBVAL.NE.32.AND.QBVAL.NE.44) THEN
         INSTRC(I)=INSTRC(I)(2:140)
         GO TO 9010
!       ALL LEADING NON-BLANK AND NON-COMMA CHARACTERS HAVE
!       BEEN REMOVED, PROCEED.
      END IF
!
!       IS THE INSTRC(I) ALL BLANK?
!
      BSUM=0
      BVAL=0
      DO 9011 KI=1,140
         BVAL=ICHAR(INSTRC(I)(KI:KI))
         BSUM=BSUM+BVAL
9011  CONTINUE
      IF(BSUM.EQ.0) THEN
!       THE REMAINDER OF INSTRC(I) IS BLANK. SET
!       DEFAULT INDICATORS AND PROCEED.
         STATST(I)=0
         SI(I)=0
         STATNW(I)=0
         STATCO(I)=0
         STATC2(I)=0
         STATN1(I)=0
         STATN2(I)=0
         STATN3(I)=0
         STATN4(I)=0
         STATN5(I)=0
         DFSTA1(I)=1
         DFSTA2(I)=1
         DFSTA3(I)=1
         DFSTA4(I)=1
         DFSTA5(I)=1
         GO TO 1015
!       INSTRC(I) IS NOT ALL BLANK AND MUST CONTAIN
!       EITHER STRING OR NUMERIC DATA.
      END IF
!
!       NOW AS WITH THE CASE OF THE COMMAND WORD WITH
!       NO QUALIFIER, WE NEED TO LOOK FOR THE CASE
!       OF EXTRA BLANKS BEFORE AND AFTER A COMMA.
!       REMOVE ALL LEADING BLANKS.
!
      KI=1
9012  QBVAL=ICHAR(INSTRC(I)(1:1))
      KI=KI+1
      IF(QBVAL.EQ.32) THEN
         INSTRC(I)=INSTRC(I)(2:140)
         IF(KI.LE.50) GO TO 9012
         IF(KI.GT.50) THEN
!       THE REST OF THE LINE WAS BLANK
            STATST(I)=0
            SI(I)=0
            STATNW(I)=0
            STATCO(I)=0
            STATC2(I)=0
            STATN1(I)=0
            STATN2(I)=0
            STATN3(I)=0
            STATN4(I)=0
            STATN5(I)=0
            DFSTA1(I)=1
            DFSTA2(I)=1
            DFSTA3(I)=1
            DFSTA4(I)=1
            DFSTA5(I)=1
            GO TO 1015
         END IF
!       ALL LEADING BLANKS REMOVED.
      END IF
!
!       IS THE NEXT CHARACTER A COMMA? IF SO IS
!       IT FOLLOWED BY A BLANK? IF YES REMOVE BLANK.
!
      KI=1
9013  QBVAL=ICHAR(INSTRC(I)(1:1))
      QBVL=ICHAR(INSTRC(I)(2:2))
      IF(QBVAL.EQ.44.AND.QBVL.EQ.32) THEN
!       REMOVE THE BLANK AND TEST AGAIN
         DM1(1:1)=INSTRC(I)(1:1)
         DM2(3:140)=INSTRC(I)(3:140)
         INSTRC(I)=DM1(1:1)//DM2(3:140)
         KI=KI+1
         IF(KI.GT.50) THEN
!
!       THE REST OF THE INSTRC(I) IS BLANK.
!       SET STATUS AND CONTINUE.
            STATST(I)=0
            SI(I)=0
            STATNW(I)=0
            STATCO(I)=0
            STATC2(I)=0
            STATN1(I)=0
            STATN2(I)=0
            STATN3(I)=0
            STATN4(I)=0
            STATN5(I)=0
            DFSTA1(I)=1
            DFSTA2(I)=1
            DFSTA3(I)=1
            DFSTA4(I)=1
            DFSTA5(I)=1
            GO TO 1015
!       PROCEED
         END IF
         GO TO 9013
!       THERE ARE NO BLANKS FOLLOWING THE COMMA
!       PROCEED
      END IF
!       NOW BREAK OUT STRINGS AND NUMERIC WORDS
!
!
!       ALL LEADING BLANKS WERE REMOVED. THEN IF THERE
!       WAS A COMMA, ALL TRAILING BLANKS WERE REMOVED.
!       IF THERE NOW NO COMMA IN POSITION 1, PUT ONE
!       THERE AND THEN PROCEED.
!
      QBVAL=ICHAR(INSTRC(I)(1:1))
      IF(QBVAL.NE.44) THEN
         CH=(INSTRC(I)(1:139))
         INSTRC(I)=','//CH
!       THERE WAS A LEADING COMMA
      END IF
!
!       NEXT CHARCATER IS A COMMA NOT FOLLOWED BY BLANKS
!
!
!               CORRECTLY SET THE COMMA STATUS INDICATOR
!               STATC2(I).
!
      IF(STATCO(I).EQ.1) STATC2(I)=0
      IF(STATCO(I).EQ.0) STATC2(I)=1
!
!       A COMMA WAS THE NEXT NON-BLANK CHARACTER ENCOUNTERED. ANY NON-
!       BLANK CHARACTERS WHICH FOLLOW ARE EITHER STRING OR NUMERIC
!       INPUT. IF THE NEXT NON-BLANK CHARACTER FOUND IS A COLON
!       THEN THE REMAINING INPUT IN INSTRC(I) IS AN ALPHANUMERIC STRING
!
!       NOW REMOVE THE COMMA IN THE LEADING PLACE OF INSTRC(I).
!
      INSTRC(I)=(INSTRC(I)(2:140))
!
!       NOW REMOVE ANY LEADING BLANKS AND CHECK IF THE
!       FIRST NON-BLANK CHARACTER ENCOUNTERED IS A
!       COLON OR THE SPECIAL INTERROGATOR CHARACTER ?.
!       IF A COLON IS FOUND THEN THERE IS STRING
!       DATA AND NOT NUMERIC. IF THE SPECIAL INTERROGATOR
!       CHARACTER ? THEN THE (?) BECOMES THE STRING.
!       THERE ARE TWWO WAYS TO MAKE ? THE STRING.
!       THE TRADITIONAL WAY IS TO TYPE :?, THE SHORT CUT WAY
!       IS TO JUST TYPE ? AFTER A COMMAND WORD OR COMMAND WORD/
!       QUALIFIER WORD PAIR.
!
!       FIRST CHECK FOR ALL BLANKS
!
      QBVAL = 0
      K1=0
1017  QBVAL = ICHAR(INSTRC(I)(1:1))
      K1=K1+1
      IF(QBVAL.EQ.32) THEN
         INSTRC(I)=(INSTRC(I)(2:140))
         IF(K1.EQ.140) THEN
!       NO STRING JUST BLANK NUMERIC INPUT
            STATST(I)=0
            SI(I)=0
            STATNW(I)=0
            STATN1(I)=0
            STATN2(I)=0
            STATN3(I)=0
            STATN4(I)=0
            STATN5(I)=0
            DFSTA1(I)=1
            DFSTA2(I)=1
            DFSTA3(I)=1
            DFSTA4(I)=1
            DFSTA5(I)=1
            GO TO 1015
         END IF
         GO TO 1017
      END IF
!       TEST FOR COLON ASCII(58) OR QUESTION MARK
!       ASCII(63).
!
!     ADDED 1/22/95 SPECIAL STRING INPUT AFTER A QUALIFIER
!
!     IF CW IS WRITE AND THE REST OF THE INSTRUCTION IS NOT BLANK,
!     ADD A COLON
      IF(COMMWD(I).EQ.'WRITE'.OR.COMMWD(I).EQ.'ASTO'.OR.COMMWD(I).EQ.&
      &'ROWHD'.OR.COMMWD(I).EQ.'COLHD'.OR.COMMWD(I).EQ.'ONAME'.OR.&
      &COMMWD(I).EQ.'LI'.OR.COMMWD(I).EQ.'LIC'.OR.COMMWD(I).EQ.'FIGURE'&
      &.OR.COMMWD(I).EQ.'M'.OR.COMMWD(I).EQ.'CK'.OR.COMMWD(I).EQ.'PSFLI'&
      &.OR.COMMWD(I).EQ.'ROWHD2'.OR.COMMWD(I).EQ.'COLHD2'.OR.COMMWD(I)&
      &.EQ.'PSFTAG'.OR.COMMWD(I).EQ.'GLASSP'.OR.COMMWD(I).EQ.'LO'.OR.&
      &COMMWD(I).EQ.'QSUB'.AND.QUALWD(I).EQ.'DV'.OR.COMMWD(I).EQ.&
      &'SNAME'.OR.COMMWD(I).EQ.'RENAME'.OR.COMMWD(I).EQ.'DUP'&
      &.OR.COMMWD(I).EQ.'PLOT'.AND.QUALWD(I).EQ.'NAME'&
      &.OR.COMMWD(I).EQ.'INI'.OR.COMMWD(I).EQ.'LTYPE'.OR.&
      &COMMWD(I).EQ.'AWRTSUM'.AND.QUALWD(I).NE.'        '.OR.&
      &COMMWD(I).EQ.'BWRTSUM'.AND.QUALWD(I).NE.'        '.OR.&
      &COMMWD(I).EQ.'OP_DESC'.AND.QUALWD(I).NE.'        '&
      &.OR.COMMWD(I).EQ.'STREAK'.AND.QUALWD(I).EQ.'PLOT'.OR.COMMWD(I)&
      &.EQ.'STREAK'.AND.QUALWD(I).EQ.'WRITE'.OR.COMMWD(I).EQ.'PLOT'.AND.&
      &QUALWD(I).EQ.'VIGSTAT'.OR.COMMWD(I).EQ.'SSUB'.AND.QUALWD(I)&
      &.EQ.'DV'.OR.COMMWD(I).EQ.'LBL'.OR.COMMWD(I).EQ.'LABEL'&
      &.OR.COMMWD(I).EQ.'CV2PRG'.OR.COMMWD(I).EQ.'ZMX2PRG'&
      &.OR.COMMWD(I).EQ.'DXF'.AND.QUALWD(I).EQ.'LAYER'.OR.COMMWD(I).EQ.&
      &'MFG'.OR.COMMWD(I).EQ.'CATNUM'.OR.COMMWD(I).EQ.'OTOBMP'.OR.&
      &COMMWD(I).EQ.'ITOBMP'.OR.COMMWD(I).EQ.'BMPREADR'.OR.COMMWD(I).EQ.&
      &'CAPFNIN'.OR.COMMWD(I).EQ.'CAPFNOUT'.OR.COMMWD(I).EQ.'LENS'.OR.&
      &COMMWD(I).EQ.'PNOTE') THEN
         DO IQ=1,8
!     STRIP OFF UP TO 8 LEADING COMMAS
            IF(INSTRC(I)(1:1).EQ.',') INSTRC(I)(1:139)=INSTRC(I)(2:139)
         END DO
         ALLB=.TRUE.
         DO IQ=1,80
            IF(INSTRC(I)(IQ:IQ).NE.' ') ALLB=.FALSE.
         END DO
         IF(COMMWD(I).EQ.'OUTPUT'.AND.QUALWD(I).EQ.'T        '.OR.&
         &COMMWD(I).EQ.'OUT'.AND.QUALWD(I).EQ.'T       ') THEN
            DO IQ=1,8
!     STRIP OFF UP TO 8 LEADING COMMAS
               IF(INSTRC(I)(1:1).EQ.',') INSTRC(I)(1:139)=INSTRC(I)(2:139)
            END DO
            ALLB=.TRUE.
            DO IQ=1,80
               IF(INSTRC(I)(IQ:IQ).NE.' ') ALLB=.FALSE.
            END DO
            IF(INSTRC(I)(1:1).EQ.'?') THEN
               SI(I)=1
               STATST(I)=0
            ELSE
               IF(.NOT.ALLB) INSTRC(I)(1:140)=':'//INSTRC(I)(1:139)
               SI(I)=0
               STATST(I)=1
            END IF
         END IF
         IF(INSTRC(I)(1:1).EQ.'?') THEN
            SI(I)=1
            STATST(I)=0
         ELSE
            IF(.NOT.ALLB) INSTRC(I)(1:140)=':'//INSTRC(I)(1:139)
            SI(I)=0
            STATST(I)=1
         END IF
      END IF
!
      QBVAL = 0
      QBVAL=ICHAR(INSTRC(I)(1:1))
      IF(QBVAL.EQ.58) THEN
!       THERE IS A COLON, THERE IS STRING DATA
         STRING(I)=(INSTRC(I)(2:140))
         STATNW(I)=0
         STATN1(I)=0
         STATN2(I)=0
         STATN3(I)=0
         STATN4(I)=0
         STATN5(I)=0
         DFSTA1(I)=1
         DFSTA2(I)=1
         DFSTA3(I)=1
         DFSTA4(I)=1
         DFSTA5(I)=1
         GO TO 1015
      ELSE
         IF(QBVAL.EQ.63) THEN
!       THE INTERROGATOR IS PRESENT.
            STRING(I)=(INSTRC(I)(1:1))
            SI(I)=1
            STATST(I)=0
            STATNW(I)=0
            STATN1(I)=0
            STATN2(I)=0
            STATN3(I)=0
            STATN4(I)=0
            STATN5(I)=0
            DFSTA1(I)=1
            DFSTA2(I)=1
            DFSTA3(I)=1
            DFSTA4(I)=1
            DFSTA5(I)=1
            GO TO 1015
         END IF
!
!       THERE IS NUMERIC DATA, RESOLVE IT.
!
!       NOW BREAK OUT THE ONE TO FIVE ALPHANUMERIC
!       REPRESENTATIONS OF THE NUMERIC WORDS, THEN
!       TRANSLATE THEM INTO NUMERIC real(real64)
!       VALUES. AS OF 1/9/93 AT 5:14PM, NUMERIC WORDS CAN HAVE
!       NESTED COMMAS.
!
         STATST(I)=0
         STATNW(I)=1
         ANW(I)=INSTRC(I)
         CALL ATOD(ANW(I),JK_INP,JK_NUM,JK_DF,JJK_FLG1,5,JJK_FLG2,REMAIN)
!

         IF(.NOT.JJK_FLG2) THEN
!     STRING FOLLOWS 5TH NUMERIC WORD
            STATST(I)=1
            STRING(I)(1:80)=REMAIN(1:80)
         END IF
!
         DO JK_I = 1,5
            IF(.NOT.JJK_FLG1(JK_I)) THEN
               DO JKII=JK_I,5
                  IF(JKII.EQ.1) THEN
                     DFSTA1(I)=1
                     NW1(I)=0.0D0
                     STATN1(I)=0
                  END IF
                  IF(JKII.EQ.2) THEN
                     DFSTA2(I)=1
                     NW2(I)=0.0D0
                     STATN2(I)=0
                  END IF
                  IF(JKII.EQ.3) THEN
                     DFSTA3(I)=1
                     NW3(I)=0.0D0
                     STATN3(I)=0
                  END IF
                  IF(JKII.EQ.4) THEN
                     DFSTA4(I)=1
                     NW4(I)=0.0D0
                     STATN4(I)=0
                  END IF
                  IF(JKII.EQ.5) THEN
                     DFSTA5(I)=1
                     NW5(I)=0.0D0
                     STATN5(I)=0
                  END IF
               END DO
            END IF
         END DO
!
         IF(JK_DF(1).EQ.1) THEN
            DFSTA1(I)=1
            NW1(I)=0.0D0
            STATN1(I)=0
         ELSE
            DFSTA1(I)=0
            NW1(I)=JK_NUM(1)
            STATN1(I)=1
         END IF
         IF(JK_DF(2).EQ.1) THEN
            DFSTA2(I)=1
            NW2(I)=0.0D0
            STATN2(I)=0
         ELSE
            DFSTA2(I)=0
            NW2(I)=JK_NUM(2)
            STATN2(I)=1
         END IF
         IF(JK_DF(3).EQ.1) THEN
            DFSTA3(I)=1
            NW3(I)=0.0D0
            STATN3(I)=0
         ELSE
            DFSTA3(I)=0
            NW3(I)=JK_NUM(3)
            STATN3(I)=1
         END IF
         IF(JK_DF(4).EQ.1) THEN
            DFSTA4(I)=1
            NW4(I)=0.0D0
            STATN4(I)=0
         ELSE
            DFSTA4(I)=0
            NW4(I)=JK_NUM(4)
            STATN4(I)=1
         END IF
         IF(JK_DF(5).EQ.1) THEN
            DFSTA5(I)=1
            NW5(I)=0.0D0
            STATN5(I)=0
         ELSE
            DFSTA5(I)=0
            NW5(I)=JK_NUM(5)
            STATN5(I)=1
         END IF
!

      END IF
!
1015 CONTINUE
!
   DO 1999 I=1,NSTRUC
      ! JN:  I need to know what the current command is
      ! when users input ; and I don't see a rcurrent
      ! variable for this.  So add one here
      currentCommand = ADJUSTL(rawCommands(I))
!
!       CALL THE CONTROL SUBROUTINE TO BRANCH OFF TO
!       SPECIFIC PROGRAM CMDS.
!
!
      IF(COMMWD(I)(1:8).EQ.'WRITE   '.AND.&
      &QUALWD(I)(1:8).EQ.'        ') THEN
         QUALWD(I)(1:8)='X       '
         STATQL(I)=1
      END IF
      WC=COMMWD(I)
      WQ=QUALWD(I)
      WS=STRING(I)

      !PRINT *, "PRO3 INPUT IS ", INPUT
      !WRITE(OUTLYNE, *) "WC = ", WC, "WQ = ",WQ, "WS = ", WS
      !CALL SHOWIT(19)

      W1=NW1(I)
      W2=NW2(I)
      W3=NW3(I)
      W4=NW4(I)
      W5=NW5(I)
      !WRITE(OUTLYNE, *) "W1= ", W1, "W2= ", W2, "W3= ", NW3(I)
      !CALL SHOWIT(19)
      SB1=STATBL(I)
      SB2=STBLK2(I)
      SC1=STATCO(I)
      SC2=STATC2(I)
      SQ=STATQL(I)
      SST=STATST(I)
      STI=SI(I)
      S1=STATN1(I)
      S2=STATN2(I)
      S3=STATN3(I)
      S4=STATN4(I)
      S5=STATN5(I)
      IF(S1.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 &
      &.OR.S5.EQ.1) THEN
         STATNW(I)=1
      ELSE
         STATNW(I)=0
      END IF
      SN=STATNW(I)
      DF1=DFSTA1(I)
      DF2=DFSTA2(I)
      DF3=DFSTA3(I)
      DF4=DFSTA4(I)
      DF5=DFSTA5(I)
      IF(DF1.EQ.2) DF1=1
      IF(DF2.EQ.2) DF2=1
      IF(DF3.EQ.2) DF3=1
      IF(DF4.EQ.2) DF4=1
      IF(DF5.EQ.2) DF5=1
      !WRITE(OUTLYNE, *) "DF1= ", DF1, "DF2= ", DF2
      !CALL SHOWIT(19)
!       CORRECTION MADE ON 2/10/88
!       FOR ANY NUMERIC WORD WITH A DEFAULT VALUE, THE
!       CORRESPONDING S1 TO S5 SHOULD BE SET TO 0
!       IF ALL 5 WORDS ARE DEFAULT, SN IS ALSO 0
      IF(DF1.EQ.1) S1=0
      IF(DF2.EQ.1) S2=0
      IF(DF3.EQ.1) S3=0
      IF(DF4.EQ.1) S4=0
      IF(DF5.EQ.1) S5=0
      IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1.AND.DF4.EQ.1 &
      &.AND.DF5.EQ.1) SN=0
!
!       THIS ERROR WAS FOUND ON 2/10/88 WHEN INSTALLING THE
!       CFGCHG.FOR SUBROUTINE
!
!     (F47/MREA macro execution path removed with binary macro system)
      IF(.FALSE.) THEN
      ELSE
!     REMOVES , INFRONT OF A : USED IN A STRING 2/14/94
         CALL PRO4
!
!     HEXAGON/ACCOS-V CONFIGS INPUT
!
!     6/23/05 ADDED A SAVE AND RELOAD AND PATCH CODE TO PROCESS HEXAGON CONFIGS
!     COMMANDS
         IF(F10.EQ.1.AND.F13.EQ.0.OR.F11.EQ.1.AND.F13.EQ.0) THEN
!     CHECK FOR A HEXAGON COMMAND AND THEN REPLACE IT WITH AN ODP COMMAND SEQUENCE
!     CHECK FOR UPDATE LENS TYPES OF HEXAGON COMMANDS AND ISSUE THE CORRECT ODP COMMANDS
!
!     CV
!
            IF(WC.EQ.'CV'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
               HOLD1=W1
               HOLD2=W2
               HOLD3=W3
               HOLD4=W4
               HOLD5=W5
               WRITE(AHOLD1,666) HOLD1
               WRITE(AHOLD2,666) HOLD2
               WRITE(AHOLD3,666) HOLD3
               WRITE(AHOLD4,666) HOLD4
               WRITE(AHOLD5,666) HOLD5
666            FORMAT(D23.15)
               SAVE_KDP(31)=SAVEINPT(31)
               INPUT='U L'
               CALL PROCES
               INPUT='CHG '//AHOLD1
               CALL PROCES
               INPUT='CV '//AHOLD2
               CALL PROCES
               INPUT='EOS'
               CALL PROCES
               REST_KDP(31)=RESTINPT(31)
               GO TO 1999
            END IF
!
!     RD
!
            IF(WC.EQ.'RD'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
               HOLD1=W1
               HOLD2=W2
               HOLD3=W3
               HOLD4=W4
               HOLD5=W5
               WRITE(AHOLD1,666) HOLD1
               WRITE(AHOLD2,666) HOLD2
               WRITE(AHOLD3,666) HOLD3
               WRITE(AHOLD4,666) HOLD4
               WRITE(AHOLD5,666) HOLD5
               SAVE_KDP(31)=SAVEINPT(31)
               INPUT='U L'
               CALL PROCES
               INPUT='CHG '//AHOLD1
               CALL PROCES
               INPUT='RD '//AHOLD2
               CALL PROCES
               INPUT='EOS'
               CALL PROCES
               REST_KDP(31)=RESTINPT(31)
               GO TO 1999
            END IF
!
!     CVR
!
            IF(WC.EQ.'CVR'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
               HOLD1=W1
               HOLD2=W2
               HOLD3=W3
               HOLD4=W4
               HOLD5=W5
               WRITE(AHOLD1,666) HOLD1
               WRITE(AHOLD2,666) HOLD2
               WRITE(AHOLD3,666) HOLD3
               WRITE(AHOLD4,666) HOLD4
               WRITE(AHOLD5,666) HOLD5
               SAVE_KDP(31)=SAVEINPT(31)
               INPUT='U L'
               CALL PROCES
               INPUT='CHG '//AHOLD1
               CALL PROCES
               INPUT='CVTOR '//AHOLD2
               CALL PROCES
               INPUT='EOS'
               CALL PROCES
               REST_KDP(31)=RESTINPT(31)
               GO TO 1999
            END IF
!
!     RDR
!
            IF(WC.EQ.'RDR'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
               HOLD1=W1
               HOLD2=W2
               HOLD3=W3
               HOLD4=W4
               HOLD5=W5
               WRITE(AHOLD1,666) HOLD1
               WRITE(AHOLD2,666) HOLD2
               WRITE(AHOLD3,666) HOLD3
               WRITE(AHOLD4,666) HOLD4
               WRITE(AHOLD5,666) HOLD5
               SAVE_KDP(31)=SAVEINPT(31)
               INPUT='U L'
               CALL PROCES
               INPUT='CHG '//AHOLD1
               CALL PROCES
               INPUT='RDTOR '//AHOLD2
               CALL PROCES
               INPUT='EOS'
               CALL PROCES
               REST_KDP(31)=RESTINPT(31)
               GO TO 1999
            END IF
!
!     CC
!
            IF(WC.EQ.'CC'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
               HOLD1=W1
               HOLD2=W2
               HOLD3=W3
               HOLD4=W4
               HOLD5=W5
               WRITE(AHOLD1,666) HOLD1
               WRITE(AHOLD2,666) HOLD2
               WRITE(AHOLD3,666) HOLD3
               WRITE(AHOLD4,666) HOLD4
               WRITE(AHOLD5,666) HOLD5
               SAVE_KDP(31)=SAVEINPT(31)
               INPUT='U L'
               CALL PROCES
               INPUT='CHG '//AHOLD1
               CALL PROCES
               INPUT='CC '//AHOLD2
               CALL PROCES
               INPUT='EOS'
               CALL PROCES
               REST_KDP(31)=RESTINPT(31)
               GO TO 1999
            END IF
!
!     XD
!
            IF(WC.EQ.'XD'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
               HOLD1=W1
               HOLD2=W2
               HOLD3=W3
               HOLD4=W4
               HOLD5=W5
               WRITE(AHOLD1,666) HOLD1
               WRITE(AHOLD2,666) HOLD2
               WRITE(AHOLD3,666) HOLD3
               WRITE(AHOLD4,666) HOLD4
               WRITE(AHOLD5,666) HOLD5
               SAVE_KDP(31)=SAVEINPT(31)
               INPUT='U L'
               CALL PROCES
               INPUT='CHG '//AHOLD1
               CALL PROCES
               INPUT='XD '//AHOLD2
               CALL PROCES
               INPUT='EOS'
               CALL PROCES
               REST_KDP(31)=RESTINPT(31)
               GO TO 1999
            END IF
!
!     YD
!
            IF(WC.EQ.'YD'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
               HOLD1=W1
               HOLD2=W2
               HOLD3=W3
               HOLD4=W4
               HOLD5=W5
               WRITE(AHOLD1,666) HOLD1
               WRITE(AHOLD2,666) HOLD2
               WRITE(AHOLD3,666) HOLD3
               WRITE(AHOLD4,666) HOLD4
               WRITE(AHOLD5,666) HOLD5
               SAVE_KDP(31)=SAVEINPT(31)
               INPUT='U L'
               CALL PROCES
               INPUT='CHG '//AHOLD1
               CALL PROCES
               INPUT='YD '//AHOLD2
               CALL PROCES
               INPUT='EOS'
               CALL PROCES
               REST_KDP(31)=RESTINPT(31)
               GO TO 1999
            END IF
!
!     ALPHA
!
            IF(WC.EQ.'ALPHA'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
               HOLD1=W1
               HOLD2=W2
               HOLD3=W3
               HOLD4=W4
               HOLD5=W5
               WRITE(AHOLD1,666) HOLD1
               WRITE(AHOLD2,666) HOLD2
               WRITE(AHOLD3,666) HOLD3
               WRITE(AHOLD4,666) HOLD4
               WRITE(AHOLD5,666) HOLD5
               SAVE_KDP(31)=SAVEINPT(31)
               INPUT='U L'
               CALL PROCES
               INPUT='CHG '//AHOLD1
               CALL PROCES
               INPUT='ALPHA '//AHOLD2
               CALL PROCES
               INPUT='EOS'
               CALL PROCES
               REST_KDP(31)=RESTINPT(31)
               GO TO 1999
            END IF
!
!     BETA
!
            IF(WC.EQ.'BETA'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
               HOLD1=W1
               HOLD2=W2
               HOLD3=W3
               HOLD4=W4
               HOLD5=W5
               WRITE(AHOLD1,666) HOLD1
               WRITE(AHOLD2,666) HOLD2
               WRITE(AHOLD3,666) HOLD3
               WRITE(AHOLD4,666) HOLD4
               WRITE(AHOLD5,666) HOLD5
               SAVE_KDP(31)=SAVEINPT(31)
               INPUT='U L'
               CALL PROCES
               INPUT='CHG '//AHOLD1
               CALL PROCES
               INPUT='BETA '//AHOLD2
               CALL PROCES
               INPUT='EOS'
               CALL PROCES
               REST_KDP(31)=RESTINPT(31)
               GO TO 1999
            END IF
!
!     GAMMA
!
            IF(WC.EQ.'GAMMA'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
               HOLD1=W1
               HOLD2=W2
               HOLD3=W3
               HOLD4=W4
               HOLD5=W5
               WRITE(AHOLD1,666) HOLD1
               WRITE(AHOLD2,666) HOLD2
               WRITE(AHOLD3,666) HOLD3
               WRITE(AHOLD4,666) HOLD4
               WRITE(AHOLD5,666) HOLD5
               SAVE_KDP(31)=SAVEINPT(31)
               INPUT='U L'
               CALL PROCES
               INPUT='CHG '//AHOLD1
               CALL PROCES
               INPUT='GAMMA '//AHOLD2
               CALL PROCES
               INPUT='EOS'
               CALL PROCES
               REST_KDP(31)=RESTINPT(31)
               GO TO 1999
            END IF
!
!     AD
!
            IF(WC.EQ.'AD'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
               HOLD1=W1
               HOLD2=W2
               HOLD3=W3
               HOLD4=W4
               HOLD5=W5
               WRITE(AHOLD1,666) HOLD1
               WRITE(AHOLD2,666) HOLD2
               WRITE(AHOLD3,666) HOLD3
               WRITE(AHOLD4,666) HOLD4
               WRITE(AHOLD5,666) HOLD5
               SAVE_KDP(31)=SAVEINPT(31)
               INPUT='U L'
               CALL PROCES
               INPUT='CHG '//AHOLD1
               CALL PROCES
               INPUT='AD '//AHOLD2
               CALL PROCES
               INPUT='EOS'
               CALL PROCES
               REST_KDP(31)=RESTINPT(31)
               GO TO 1999
            END IF
!
!     AE
!
            IF(WC.EQ.'AE'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
               HOLD1=W1
               HOLD2=W2
               HOLD3=W3
               HOLD4=W4
               HOLD5=W5
               WRITE(AHOLD1,666) HOLD1
               WRITE(AHOLD2,666) HOLD2
               WRITE(AHOLD3,666) HOLD3
               WRITE(AHOLD4,666) HOLD4
               WRITE(AHOLD5,666) HOLD5
               SAVE_KDP(31)=SAVEINPT(31)
               INPUT='U L'
               CALL PROCES
               INPUT='CHG '//AHOLD1
               CALL PROCES
               INPUT='AE '//AHOLD2
               CALL PROCES
               INPUT='EOS'
               CALL PROCES
               REST_KDP(31)=RESTINPT(31)
               GO TO 1999
            END IF
!
!     AF
!
            IF(WC.EQ.'AF'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
               HOLD1=W1
               HOLD2=W2
               HOLD3=W3
               HOLD4=W4
               HOLD5=W5
               WRITE(AHOLD1,666) HOLD1
               WRITE(AHOLD2,666) HOLD2
               WRITE(AHOLD3,666) HOLD3
               WRITE(AHOLD4,666) HOLD4
               WRITE(AHOLD5,666) HOLD5
               SAVE_KDP(31)=SAVEINPT(31)
               INPUT='U L'
               CALL PROCES
               INPUT='CHG '//AHOLD1
               CALL PROCES
               INPUT='AF '//AHOLD2
               CALL PROCES
               INPUT='EOS'
               CALL PROCES
               REST_KDP(31)=RESTINPT(31)
               GO TO 1999
            END IF
!
!     AG
!
            IF(WC.EQ.'AG'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
               HOLD1=W1
               HOLD2=W2
               HOLD3=W3
               HOLD4=W4
               HOLD5=W5
               WRITE(AHOLD1,666) HOLD1
               WRITE(AHOLD2,666) HOLD2
               WRITE(AHOLD3,666) HOLD3
               WRITE(AHOLD4,666) HOLD4
               WRITE(AHOLD5,666) HOLD5
               SAVE_KDP(31)=SAVEINPT(31)
               INPUT='U L'
               CALL PROCES
               INPUT='CHG '//AHOLD1
               CALL PROCES
               INPUT='CV '//AHOLD2
               CALL PROCES
               INPUT='EOS'
               CALL PROCES
               REST_KDP(31)=RESTINPT(31)
               GO TO 1999
            END IF
!
!     RN
!
            IF(WC.EQ.'RN'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
               GO TO 1999
            END IF
!
!     DF
!
            IF(WC.EQ.'DF'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
               GO TO 1999
            END IF
!
!     TH
!
            IF(WC.EQ.'TH'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
               HOLD1=W1
               HOLD2=W2
               HOLD3=W3
               HOLD4=W4
               HOLD5=W5
               WRITE(AHOLD1,666) HOLD1
               WRITE(AHOLD2,666) HOLD2
               WRITE(AHOLD3,666) HOLD3
               WRITE(AHOLD4,666) HOLD4
               WRITE(AHOLD5,666) HOLD5
               SAVE_KDP(31)=SAVEINPT(31)
               INPUT='U L'
               CALL PROCES
               INPUT='CHG '//AHOLD1
               CALL PROCES
               INPUT='TH '//AHOLD2
               CALL PROCES
               INPUT='EOS'
               CALL PROCES
               REST_KDP(31)=RESTINPT(31)
               GO TO 1999
            END IF
!
!     TEMP
!
            IF(WC.EQ.'TEMP'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
               GO TO 1999
            END IF
!
!     GLALPHA
!
            IF(WC.EQ.'GLALPHA'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
               GO TO 1999
            END IF
!
!     SCY
!
            IF(WC.EQ.'SCY'.AND.DF1.EQ.0) THEN
               HOLD1=W1
               HOLD2=W2
               HOLD3=W3
               HOLD4=W4
               HOLD5=W5
               AHOLDWQ=WQ(1:6)
               HOLDSQ=SQ
               WRITE(AHOLD1,666) HOLD1
               WRITE(AHOLD2,666) HOLD2
               WRITE(AHOLD3,666) HOLD3
               WRITE(AHOLD4,666) HOLD4
               WRITE(AHOLD5,666) HOLD5
               SAVE_KDP(31)=SAVEINPT(31)
               INPUT='U L'
               CALL PROCES
               INPUT='CHG '//AHOLD1
               CALL PROCES
               IF(HOLDSQ.EQ.0) THEN
                  INPUT='SCY '//AHOLD2
               ELSE
                  INPUT='SCY '//AHOLDWQ//' '//AHOLD2
               END IF
               CALL PROCES
               INPUT='EOS'
               CALL PROCES
               REST_KDP(31)=RESTINPT(31)
               GO TO 1999
            END IF
!
!     SCX
!
            IF(WC.EQ.'SCX'.AND.DF1.EQ.0) THEN
               HOLD1=W1
               HOLD2=W2
               HOLD3=W3
               HOLD4=W4
               HOLD5=W5
               AHOLDWQ=WQ(1:6)
               HOLDSQ=SQ
               WRITE(AHOLD1,666) HOLD1
               WRITE(AHOLD2,666) HOLD2
               WRITE(AHOLD3,666) HOLD3
               WRITE(AHOLD4,666) HOLD4
               WRITE(AHOLD5,666) HOLD5
               SAVE_KDP(31)=SAVEINPT(31)
               INPUT='U L'
               CALL PROCES
               INPUT='CHG '//AHOLD1
               CALL PROCES
               IF(HOLDSQ.EQ.0) THEN
                  INPUT='SCX '//AHOLD2
               ELSE
                  INPUT='SCX '//AHOLDWQ//' '//AHOLD2
               END IF
               CALL PROCES
               INPUT='EOS'
               CALL PROCES
               REST_KDP(31)=RESTINPT(31)
               GO TO 1999
            END IF
!
!
!
         END IF
         CALL CONTRO
      END IF
!
1999 CONTINUE
   RETURN
!
END
