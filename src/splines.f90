! General-purpose cubic-spline routines (used by LDM plotting, PLOTCAD,
! WAVSPOT, glass manager). Relocated verbatim from OPTIM3.f90 when the
! legacy optimizer was removed.
SUBROUTINE SPLINT(XA,YA,Y2A,N,X,Y)
!     THIS IS A MODIFIED VERSION FROM NUMERICAL RECIPIES
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   real(real64) XA,YA,Y2A,X,Y,H,A,B
   INTEGER KLO,KHI,N,K
   DIMENSION XA(N),YA(N),Y2A(N)
   KLO=1
   KHI=N
1  IF (KHI-KLO.GT.1) THEN
      K=(KHI+KLO)/2
      IF(XA(K).GT.X)THEN
         KHI=K
      ELSE
         KLO=K
      ENDIF
      GOTO 1
   ENDIF
   H=XA(KHI)-XA(KLO)
   IF (H.EQ.0.0D0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'SERIOUS ERROR IN CALL TO SPLINE INTERPOLATION'//'\n'//&
      & 'REPORT THIS IMMEDIATELY', 1)
      RETURN
   END IF
   A=(XA(KHI)-X)/H
   B=(X-XA(KLO))/H
   Y=A*YA(KLO)+B*YA(KHI)+((A**3-A)*Y2A(KLO)+(B**3-B)*Y2A(KHI))*(H**2)/6.0D0
   RETURN
END
SUBROUTINE SPLINE(X,Y,N,YP1,YPN,Y2)
!     THIS IS A MODIFIED VERSION FROM NUMERICAL RECIPIES
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   real(real64) X,Y,Y2,U,SIG,P,YPN,QN,UN,YP1
   INTEGER NMAX,I,N,K
   PARAMETER (NMAX=250)
   DIMENSION X(N),Y(N),Y2(N),U(NMAX)
   PRINT *, "N is ", N
   PRINT *, "Y(N)) is ", Y(N)
   IF (YP1.GT..99D30) THEN
      Y2(1)=0.0D0
      U(1)=0.0D0
   ELSE
      Y2(1)=-0.5D0
      U(1)=(3.0D0/(X(2)-X(1)))*((Y(2)-Y(1))/(X(2)-X(1))-YP1)
   ENDIF
   DO 11 I=2,N-1
      SIG=(X(I)-X(I-1))/(X(I+1)-X(I-1))
      P=SIG*Y2(I-1)+2.0D0
      Y2(I)=(SIG-1.0D0)/P
      U(I)=(6.0D0*((Y(I+1)-Y(I))/(X(I+1)-X(I))-(Y(I)-Y(I-1))/(X(I)-X(I-1)))/(X(I+1)-X(I-1))-SIG*U(I-1))/P
11 CONTINUE
   IF (YPN.GT..99D30) THEN
      QN=0.0D0
      UN=0.0D0
   ELSE
      QN=0.5D0
      UN=(3.0D0/(X(N)-X(N-1)))*(YPN-(Y(N)-Y(N-1))/(X(N)-X(N-1)))
   ENDIF
   Y2(N)=(UN-QN*U(N-1))/(QN*Y2(N-1)+1.0D0)
   DO 12 K=N-1,1,-1
      Y2(K)=Y2(K)*Y2(K+1)+U(K)
12 CONTINUE
   RETURN
END
