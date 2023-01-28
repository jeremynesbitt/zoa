C      THIS IS THE THIRTEENTH FILE OF RAYTRACING ROUTINES
C
        SUBROUTINE CNEWDEL(MF1,MF2,D11,D12,D21,D22,DELFAIL
     1  ,CDDELX,CDDELY)
        IMPLICIT NONE
        LOGICAL DELFAIL,SAGERR
        REAL*8 MF1,MF2,D11,D12,D21,D22,ZSAG,CDDELX,CDDELY
        INTEGER IISURF
        INTEGER SPDCD1,SPDCD2
        COMMON/SPRA2/SPDCD1,SPDCD2
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATLEN.INC'
C
        IF(D11.EQ.0.0D0.AND.D12.EQ.0.0D0.AND.D21.EQ.0.0D0
     1  .AND.D22.EQ.0.0D0) THEN
C       NO SOLUTION EXISTS TO AIM REF RAY TO AIMTOL
                IF(MSG) THEN
      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',NEWOBJ
      CALL SHOWIT(1)
C     WRITE(OUTLYNE,*)'SPECIFIED OBJECT POINT DOES NOT EXIST'
      CALL SHOWIT(1)
                        END IF
                STOPP=1
                RAYCOD(1)=16
                RAYCOD(2)=NEWOBJ
                SPDCD1=RAYCOD(1)
                SPDCD2=RAYCOD(2)
                REFEXT=.FALSE.
                   CALL MACFAL
        DELFAIL=.TRUE.
        RETURN
                        END IF
        IF(D11.NE.0.0D0.AND.
     1  D22.NE.0.0D0) THEN
        CDDELX=MF1/D11
        CDDELY=MF2/D22
      XSTRT=XSTRT+(0.1*CDDELX)
      YSTRT=YSTRT+(0.1*CDDELY)
C     COMPUTE THE APROPRIATE ZSTRT VALUE
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
               IF(SAGERR) THEN
                IF(MSG) THEN
      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',NEWOBJ
      CALL SHOWIT(1)
C     WRITE(OUTLYNE,*)'SPECIFIED OBJECT POINT DOES NOT EXIST'
      CALL SHOWIT(1)
                        END IF
                STOPP=1
                RAYCOD(1)=16
                RAYCOD(2)=NEWOBJ
                SPDCD1=RAYCOD(1)
                SPDCD2=RAYCOD(2)
                REFEXT=.FALSE.
                   CALL MACFAL
                DELFAIL=.TRUE.
                   RETURN
                   END IF
      ZSTRT=ZSAG
                   RETURN
                        END IF
        IF(D11.NE.0.0D0.AND.
     1  D12.NE.0.0D0) THEN
        CDDELX=MF1/D11
        CDDELY=MF1/D12
      XSTRT=XSTRT+(0.1*CDDELX)
      YSTRT=YSTRT+(0.1*CDDELY)
C     COMPUTE THE APROPRIATE ZSTRT VALUE
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
               IF(SAGERR) THEN
                IF(MSG) THEN
      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',NEWOBJ
      CALL SHOWIT(1)
C     WRITE(OUTLYNE,*)'SPECIFIED OBJECT POINT DOES NOT EXIST'
      CALL SHOWIT(1)
                        END IF
                STOPP=1
                RAYCOD(1)=16
                RAYCOD(2)=NEWOBJ
                SPDCD1=RAYCOD(1)
                SPDCD2=RAYCOD(2)
                REFEXT=.FALSE.
                   CALL MACFAL
                DELFAIL=.TRUE.
                   RETURN
                   END IF
      ZSTRT=ZSAG
                   RETURN
                        END IF
        IF(D21.NE.0.0D0.AND.
     1  D12.NE.0.0D0) THEN
        CDDELX=MF2/D21
        CDDELY=MF1/D12
      XSTRT=XSTRT+(0.1*CDDELX)
      YSTRT=YSTRT+(0.1*CDDELY)
C     COMPUTE THE APROPRIATE ZSTRT VALUE
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
               IF(SAGERR) THEN
                IF(MSG) THEN
      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',NEWOBJ
      CALL SHOWIT(1)
C     WRITE(OUTLYNE,*)'SPECIFIED OBJECT POINT DOES NOT EXIST'
      CALL SHOWIT(1)
                        END IF
                STOPP=1
                RAYCOD(1)=16
                RAYCOD(2)=NEWOBJ
                SPDCD1=RAYCOD(1)
                SPDCD2=RAYCOD(2)
                REFEXT=.FALSE.
                   CALL MACFAL
                DELFAIL=.TRUE.
                   RETURN
                   END IF
      ZSTRT=ZSAG
                   RETURN
                        END IF
        IF(D21.NE.0.0D0.AND.
     1  D22.NE.0.0D0) THEN
        CDDELX=MF2/D21
        CDDELY=MF2/D22
      XSTRT=XSTRT+(0.1*CDDELX)
      YSTRT=YSTRT+(0.1*CDDELY)
C     COMPUTE THE APROPRIATE ZSTRT VALUE
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
               IF(SAGERR) THEN
                IF(MSG) THEN
      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',NEWOBJ
      CALL SHOWIT(1)
C     WRITE(OUTLYNE,*)'SPECIFIED OBJECT POINT DOES NOT EXIST'
      CALL SHOWIT(1)
                        END IF
                STOPP=1
                RAYCOD(1)=16
                RAYCOD(2)=NEWOBJ
                SPDCD1=RAYCOD(1)
                SPDCD2=RAYCOD(2)
                REFEXT=.FALSE.
                   CALL MACFAL
                DELFAIL=.TRUE.
                   RETURN
                   END IF
      ZSTRT=ZSAG
                   RETURN
                        END IF
        IF(D11.NE.0.0D0.AND.
     1  D21.NE.0.0D0) THEN
        CDDELX=MF1/D11
        CDDELY=MF2/D21
      XSTRT=XSTRT+(0.1*CDDELX)
      YSTRT=YSTRT+(0.1*CDDELY)
C     COMPUTE THE APROPRIATE ZSTRT VALUE
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
               IF(SAGERR) THEN
                IF(MSG) THEN
      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',NEWOBJ
      CALL SHOWIT(1)
C     WRITE(OUTLYNE,*)'SPECIFIED OBJECT POINT DOES NOT EXIST'
      CALL SHOWIT(1)
                        END IF
                STOPP=1
                RAYCOD(1)=16
                RAYCOD(2)=NEWOBJ
                SPDCD1=RAYCOD(1)
                SPDCD2=RAYCOD(2)
                REFEXT=.FALSE.
                   CALL MACFAL
                DELFAIL=.TRUE.
                   RETURN
                   END IF
      ZSTRT=ZSAG
                   RETURN
                        END IF
        IF(D12.NE.0.0D0.AND.
     1  D22.NE.0.0D0) THEN
        CDDELX=MF1/D12
        CDDELY=MF2/D22
      XSTRT=XSTRT+(0.1*CDDELX)
      YSTRT=YSTRT+(0.1*CDDELY)
C     COMPUTE THE APROPRIATE ZSTRT VALUE
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
               IF(SAGERR) THEN
                IF(MSG) THEN
      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',NEWOBJ
      CALL SHOWIT(1)
C     WRITE(OUTLYNE,*)'SPECIFIED OBJECT POINT DOES NOT EXIST'
      CALL SHOWIT(1)
                        END IF
                STOPP=1
                RAYCOD(1)=16
                RAYCOD(2)=NEWOBJ
                SPDCD1=RAYCOD(1)
                SPDCD2=RAYCOD(2)
                REFEXT=.FALSE.
                   CALL MACFAL
                DELFAIL=.TRUE.
                   RETURN
                   END IF
      ZSTRT=ZSAG
                   RETURN
                        END IF
C       SPECIAL SOLUTION 1
        IF(D11.EQ.0.0D0.AND.D12.EQ.0.0D0.AND.D21.EQ.0.0D0) THEN
        CDDELX=0.0D0
        CDDELY=MF2/D22
      XSTRT=XSTRT+(0.1*CDDELX)
      YSTRT=YSTRT+(0.1*CDDELY)
C     COMPUTE THE APROPRIATE ZSTRT VALUE
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
               IF(SAGERR) THEN
                IF(MSG) THEN
      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',NEWOBJ
      CALL SHOWIT(1)
C     WRITE(OUTLYNE,*)'SPECIFIED OBJECT POINT DOES NOT EXIST'
      CALL SHOWIT(1)
                        END IF
                STOPP=1
                RAYCOD(1)=16
                RAYCOD(2)=NEWOBJ
                SPDCD1=RAYCOD(1)
                SPDCD2=RAYCOD(2)
                REFEXT=.FALSE.
                   CALL MACFAL
                DELFAIL=.TRUE.
                   RETURN
                   END IF
      ZSTRT=ZSAG
                   RETURN
                        END IF
C       SPECIAL SOLUTION 2
        IF(D12.EQ.0.0D0.AND.D21.EQ.0.0D0.AND.D22.EQ.0.0D0) THEN
        CDDELX=MF1/D11
        CDDELY=0.0D0
      XSTRT=XSTRT+(0.1*CDDELX)
      YSTRT=YSTRT+(0.1*CDDELY)
C     COMPUTE THE APROPRIATE ZSTRT VALUE
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
               IF(SAGERR) THEN
                IF(MSG) THEN
      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',NEWOBJ
      CALL SHOWIT(1)
C     WRITE(OUTLYNE,*)'SPECIFIED OBJECT POINT DOES NOT EXIST'
      CALL SHOWIT(1)
                        END IF
                STOPP=1
                RAYCOD(1)=16
                RAYCOD(2)=NEWOBJ
                SPDCD1=RAYCOD(1)
                SPDCD2=RAYCOD(2)
                REFEXT=.FALSE.
                   CALL MACFAL
                DELFAIL=.TRUE.
                   RETURN
                   END IF
      ZSTRT=ZSAG
                   RETURN
                        END IF
C       SPECIAL SOLUTION 3
        IF(D11.EQ.0.0D0.AND.D21.EQ.0.0D0.AND.D22.EQ.0.0D0) THEN
        CDDELX=MF1/D12
        CDDELY=0.0D0
      XSTRT=XSTRT+(0.1*CDDELX)
      YSTRT=YSTRT+(0.1*CDDELY)
C     COMPUTE THE APROPRIATE ZSTRT VALUE
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
               IF(SAGERR) THEN
                IF(MSG) THEN
      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',NEWOBJ
      CALL SHOWIT(1)
C     WRITE(OUTLYNE,*)'SPECIFIED OBJECT POINT DOES NOT EXIST'
      CALL SHOWIT(1)
                        END IF
                STOPP=1
                RAYCOD(1)=16
                RAYCOD(2)=NEWOBJ
                SPDCD1=RAYCOD(1)
                SPDCD2=RAYCOD(2)
                REFEXT=.FALSE.
                   CALL MACFAL
                DELFAIL=.TRUE.
                   RETURN
                   END IF
      ZSTRT=ZSAG
                   RETURN
                        END IF
C       SPECIAL SOLUTION 4
        IF(D11.EQ.0.0D0.AND.D12.EQ.0.0D0.AND.D22.EQ.0.0D0) THEN
        CDDELX=0.0D0
        CDDELY=MF2/D21
      XSTRT=XSTRT+(0.1*CDDELX)
      YSTRT=YSTRT+(0.1*CDDELY)
C     COMPUTE THE APROPRIATE ZSTRT VALUE
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
               IF(SAGERR) THEN
                IF(MSG) THEN
      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',NEWOBJ
      CALL SHOWIT(1)
C     WRITE(OUTLYNE,*)'SPECIFIED OBJECT POINT DOES NOT EXIST'
      CALL SHOWIT(1)
                        END IF
                STOPP=1
                RAYCOD(1)=16
                RAYCOD(2)=NEWOBJ
                SPDCD1=RAYCOD(1)
                SPDCD2=RAYCOD(2)
                REFEXT=.FALSE.
                   CALL MACFAL
                DELFAIL=.TRUE.
                   RETURN
                   END IF
      ZSTRT=ZSAG
                   RETURN
                        END IF
                        END
C
        SUBROUTINE NEWDEL(MF1,MF2,D11,D12,D21,D22,DELFAIL)

        USE GLOBALS

        IMPLICIT NONE
        REAL*8 MF1,MF2,D11,D12,D21,D22,DDELX,DDELY,XC1,YC1,ZC1
        LOGICAL DELFAIL
        INTEGER SPDCD1,SPDCD2
        COMMON/SPRA2/SPDCD1,SPDCD2
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATLEN.INC'
C
        !call logger%logTextWithNum("NEWDEL Start Y1AIM=",Y1AIM)


        IF(D11.EQ.0.0D0.AND.D12.EQ.0.0D0.AND.D21.EQ.0.0D0
     1  .AND.D22.EQ.0.0D0) THEN
C       NO SOLUTION EXISTS TO AIM REF RAY TO AIMTOL
                IF(MSG) THEN
      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',NEWOBJ
      CALL SHOWIT(1)
C     WRITE(OUTLYNE,*)'SPECIFIED OBJECT POINT DOES NOT EXIST'
      CALL SHOWIT(1)
                        END IF
                STOPP=1
                RAYCOD(1)=16
                RAYCOD(2)=NEWOBJ
                SPDCD1=RAYCOD(1)
                SPDCD2=RAYCOD(2)
                REFEXT=.FALSE.
                   CALL MACFAL
                   DELFAIL=.TRUE.
                        RETURN
                        ELSE
C       PROCEED
                        END IF
        IF(D11.NE.0.0D0.AND.
     1  D22.NE.0.0D0) THEN
C       SPECIAL SOLUTION, NON-ZERO DERIVATIVE PRODUCTS
C       THE SOLUTIONS FOR DDELY
C       AND DDELX ARE INDEPENDENT OF ONE ANOTHER
        DDELX=MF1/D11
        DDELY=MF2/D22
      Y1AIM=YAIMOL+DDELY
      X1AIM=XAIMOL+DDELX
      Z1AIM=ZAIMOL
      XC=X1AIM
      YC=Y1AIM
      ZC=Z1AIM
      XC1=XC
      YC1=YC
      ZC1=ZC
      IF(ALENS(1,1).NE.0.0D0)
     1CALL GETZEE1
        X1AIM=XC
        Y1AIM=YC
        Z1AIM=ZC
C     CALL BAKONE TO CONVERT TO NEWOBJ COORDINATES
                        XAIMOL=XC1
                        YAIMOL=YC1
                        ZAIMOL=ZC1
                R_TX=X1AIM
                R_TY=Y1AIM
                R_TZ=Z1AIM
        CALL BAKONE
                X1AIM=R_TX
                Y1AIM=R_TY
                Z1AIM=R_TZ
C       NOW THE COORDINATES ARE IN THE NEWOBJ COORD SYSTEM
       !call logger%logTextWithNum("NEWDEL End 394 Y1AIM=",Y1AIM)
                        RETURN
                        END IF
        IF(D11.NE.0.0D0.AND.
     1  D12.NE.0.0D0) THEN
C       SPECIAL SOLUTION, NON-ZERO DERIVATIVE PRODUCTS
C       THE SOLUTIONS FOR DDELY
C       AND DDELX ARE INDEPENDENT OF ONE ANOTHER
        DDELX=MF1/D11
        DDELY=MF1/D12
      Y1AIM=YAIMOL+DDELY
      X1AIM=XAIMOL+DDELX
      Z1AIM=ZAIMOL
      XC=X1AIM
      YC=Y1AIM
      ZC=Z1AIM
      XC1=XC
      YC1=YC
      ZC1=ZC
      IF(ALENS(1,1).NE.0.0D0)
     1CALL GETZEE1
        X1AIM=XC
        Y1AIM=YC
        Z1AIM=ZC
C     CALL BAKONE TO CONVERT TO NEWOBJ COORDINATES
                        XAIMOL=XC1
                        YAIMOL=YC1
                        ZAIMOL=ZC1
                R_TX=X1AIM
                R_TY=Y1AIM
                R_TZ=Z1AIM
        CALL BAKONE
                X1AIM=R_TX
                Y1AIM=R_TY
                Z1AIM=R_TZ
C       NOW THE COORDINATES ARE IN THE NEWOBJ COORD SYSTEM
        call logger%logTextWithNum("NEWDEL End 430 Y1AIM=",Y1AIM)
                        RETURN
                        END IF
        IF(D21.NE.0.0D0.AND.
     1  D12.NE.0.0D0) THEN
C       SPECIAL SOLUTION, NON-ZERO DERIVATIVE PRODUCTS
C       THE SOLUTIONS FOR DDELY
C       AND DDELX ARE INDEPENDENT OF ONE ANOTHER
        DDELX=MF2/D21
        DDELY=MF1/D12
      Y1AIM=YAIMOL+DDELY
      X1AIM=XAIMOL+DDELX
      Z1AIM=ZAIMOL
      XC=X1AIM
      YC=Y1AIM
      ZC=Z1AIM
      XC1=XC
      YC1=YC
      ZC1=ZC
      IF(ALENS(1,1).NE.0.0D0)
     1CALL GETZEE1
        X1AIM=XC
        Y1AIM=YC
        Z1AIM=ZC
C     CALL BAKONE TO CONVERT TO NEWOBJ COORDINATES
                        XAIMOL=XC1
                        YAIMOL=YC1
                        ZAIMOL=ZC1
                R_TX=X1AIM
                R_TY=Y1AIM
                R_TZ=Z1AIM
        CALL BAKONE
                X1AIM=R_TX
                Y1AIM=R_TY
                Z1AIM=R_TZ
C       NOW THE COORDINATES ARE IN THE NEWOBJ COORD SYSTEM
        !call logger%logTextWithNum("NEWDEL End 466 Y1AIM=",Y1AIM)
                        RETURN
                        END IF
        IF(D21.NE.0.0D0.AND.
     1  D22.NE.0.0D0) THEN
C       SPECIAL SOLUTION, NON-ZERO DERIVATIVE PRODUCTS
C       THE SOLUTIONS FOR DDELY
C       AND DDELX ARE INDEPENDENT OF ONE ANOTHER
        DDELX=MF2/D21
        DDELY=MF2/D22
      Y1AIM=YAIMOL+DDELY
      X1AIM=XAIMOL+DDELX
      Z1AIM=ZAIMOL
      XC=X1AIM
      YC=Y1AIM
      ZC=Z1AIM
      XC1=XC
      YC1=YC
      ZC1=ZC
      IF(ALENS(1,1).NE.0.0D0)
     1CALL GETZEE1
        X1AIM=XC
        Y1AIM=YC
        Z1AIM=ZC
C     CALL BAKONE TO CONVERT TO NEWOBJ COORDINATES
                        XAIMOL=XC1
                        YAIMOL=YC1
                        ZAIMOL=ZC1
                R_TX=X1AIM
                R_TY=Y1AIM
                R_TZ=Z1AIM
        CALL BAKONE
                X1AIM=R_TX
                Y1AIM=R_TY
                Z1AIM=R_TZ
C       NOW THE COORDINATES ARE IN THE NEWOBJ COORD SYSTEM
        !call logger%logTextWithNum("NEWDEL End 502 Y1AIM=",Y1AIM)
                        RETURN
                        END IF
        IF(D11.NE.0.0D0.AND.
     1  D21.NE.0.0D0) THEN
C       SPECIAL SOLUTION, NON-ZERO DERIVATIVE PRODUCTS
C       THE SOLUTIONS FOR DDELY
C       AND DDELX ARE INDEPENDENT OF ONE ANOTHER
        DDELX=MF1/D11
        DDELY=MF2/D21
      Y1AIM=YAIMOL+DDELY
      X1AIM=XAIMOL+DDELX
      Z1AIM=ZAIMOL
      XC=X1AIM
      YC=Y1AIM
      ZC=Z1AIM
      XC1=XC
      YC1=YC
      ZC1=ZC
      IF(ALENS(1,1).NE.0.0D0)
     1CALL GETZEE1
        X1AIM=XC
        Y1AIM=YC
        Z1AIM=ZC
C     CALL BAKONE TO CONVERT TO NEWOBJ COORDINATES
                        XAIMOL=XC1
                        YAIMOL=YC1
                        ZAIMOL=ZC1
                R_TX=X1AIM
                R_TY=Y1AIM
                R_TZ=Z1AIM
        CALL BAKONE
                X1AIM=R_TX
                Y1AIM=R_TY
                Z1AIM=R_TZ
C       NOW THE COORDINATES ARE IN THE NEWOBJ COORD SYSTEM
        !call logger%logTextWithNum("NEWDEL End 538 Y1AIM=",Y1AIM)
                        RETURN
                        END IF
        IF(D12.NE.0.0D0.AND.
     1  D22.NE.0.0D0) THEN
C       SPECIAL SOLUTION, NON-ZERO DERIVATIVE PRODUCTS
C       THE SOLUTIONS FOR DDELY
C       AND DDELX ARE INDEPENDENT OF ONE ANOTHER
        DDELX=MF1/D12
        DDELY=MF2/D22
      Y1AIM=YAIMOL+DDELY
      X1AIM=XAIMOL+DDELX
      Z1AIM=ZAIMOL
      XC=X1AIM
      YC=Y1AIM
      ZC=Z1AIM
      XC1=XC
      YC1=YC
      ZC1=ZC
      IF(ALENS(1,1).NE.0.0D0)
     1CALL GETZEE1
        X1AIM=XC
        Y1AIM=YC
        Z1AIM=ZC
C     CALL BAKONE TO CONVERT TO NEWOBJ COORDINATES
                        XAIMOL=XC1
                        YAIMOL=YC1
                        ZAIMOL=ZC1
                R_TX=X1AIM
                R_TY=Y1AIM
                R_TZ=Z1AIM
        CALL BAKONE
                X1AIM=R_TX
                Y1AIM=R_TY
                Z1AIM=R_TZ
C       NOW THE COORDINATES ARE IN THE NEWOBJ COORD SYSTEM
        !call logger%logTextWithNum("NEWDEL End 574 Y1AIM=",Y1AIM)
                        RETURN
                        END IF

C       SPECIAL SOLUTION 1
        IF(D11.EQ.0.0D0.AND.D12.EQ.0.0D0.AND.D21.EQ.0.0D0) THEN
        DDELX=0.0D0
        DDELY=MF2/D22
      Y1AIM=YAIMOL+DDELY
      X1AIM=XAIMOL+DDELX
      Z1AIM=ZAIMOL
      XC=X1AIM
      YC=Y1AIM
      ZC=Z1AIM
      XC1=XC
      YC1=YC
      ZC1=ZC
      IF(ALENS(1,1).NE.0.0D0)
     1CALL GETZEE1
        X1AIM=XC
        Y1AIM=YC
        Z1AIM=ZC
C     CALL BAKONE TO CONVERT TO NEWOBJ COORDINATES
                        XAIMOL=XC1
                        YAIMOL=YC1
                        ZAIMOL=ZC1
                R_TX=X1AIM
                R_TY=Y1AIM
                R_TZ=Z1AIM
        CALL BAKONE
                X1AIM=R_TX
                Y1AIM=R_TY
                Z1AIM=R_TZ
C       NOW THE COORDINATES ARE IN THE NEWOBJ COORD SYSTEM
        !call logger%logTextWithNum("NEWDEL End 608 Y1AIM=",Y1AIM)
                        RETURN
                        END IF
C       SPECIAL SOLUTION 2
        IF(D12.EQ.0.0D0.AND.D21.EQ.0.0D0.AND.D22.EQ.0.0D0) THEN
        DDELX=MF1/D11
        DDELY=0.0D0
      Y1AIM=YAIMOL+DDELY
      X1AIM=XAIMOL+DDELX
      Z1AIM=ZAIMOL
      XC=X1AIM
      YC=Y1AIM
      ZC=Z1AIM
      XC1=XC
      YC1=YC
      ZC1=ZC
      IF(ALENS(1,1).NE.0.0D0)
     1CALL GETZEE1
        X1AIM=XC
        Y1AIM=YC
        Z1AIM=ZC
C     CALL BAKONE TO CONVERT TO NEWOBJ COORDINATES
                        XAIMOL=XC1
                        YAIMOL=YC1
                        ZAIMOL=ZC1
                R_TX=X1AIM
                R_TY=Y1AIM
                R_TZ=Z1AIM
        CALL BAKONE
                X1AIM=R_TX
                Y1AIM=R_TY
                Z1AIM=R_TZ
C       NOW THE COORDINATES ARE IN THE NEWOBJ COORD SYSTEM
        !call logger%logTextWithNum("NEWDEL End 641 Y1AIM=",Y1AIM)
                        RETURN
                        END IF
C       SPECIAL SOLUTION 3
        IF(D11.EQ.0.0D0.AND.D21.EQ.0.0D0.AND.D22.EQ.0.0D0) THEN
        DDELX=MF1/D12
        DDELY=0.0D0
      Y1AIM=YAIMOL+DDELY
      X1AIM=XAIMOL+DDELX
      Z1AIM=ZAIMOL
      XC=X1AIM
      YC=Y1AIM
      ZC=Z1AIM
      XC1=XC
      YC1=YC
      ZC1=ZC
      IF(ALENS(1,1).NE.0.0D0)
     1CALL GETZEE1
        X1AIM=XC
        Y1AIM=YC
        Z1AIM=ZC
C     CALL BAKONE TO CONVERT TO NEWOBJ COORDINATES
                        XAIMOL=XC1
                        YAIMOL=YC1
                        ZAIMOL=ZC1
                R_TX=X1AIM
                R_TY=Y1AIM
                R_TZ=Z1AIM
        CALL BAKONE
                X1AIM=R_TX
                Y1AIM=R_TY
                Z1AIM=R_TZ
C       NOW THE COORDINATES ARE IN THE NEWOBJ COORD SYSTEM
        !call logger%logTextWithNum("NEWDEL End 674 Y1AIM=",Y1AIM)
                        RETURN
                        END IF
C       SPECIAL SOLUTION 4
        IF(D11.EQ.0.0D0.AND.D12.EQ.0.0D0.AND.D21.EQ.0.0D0) THEN
        DDELX=0.0D0
        DDELY=MF2/D22
      Y1AIM=YAIMOL+DDELY
      X1AIM=XAIMOL+DDELX
      Z1AIM=ZAIMOL
      XC=X1AIM
      YC=Y1AIM
      ZC=Z1AIM
      XC1=XC
      YC1=YC
      ZC1=ZC
      IF(ALENS(1,1).NE.0.0D0)
     1CALL GETZEE1
        X1AIM=XC
        Y1AIM=YC
        Z1AIM=ZC
C     CALL BAKONE TO CONVERT TO NEWOBJ COORDINATES
                        XAIMOL=XC1
                        YAIMOL=YC1
                        ZAIMOL=ZC1
                R_TX=X1AIM
                R_TY=Y1AIM
                R_TZ=Z1AIM
        CALL BAKONE
                X1AIM=R_TX
                Y1AIM=R_TY
                Z1AIM=R_TZ
C       NOW THE COORDINATES ARE IN THE NEWOBJ COORD SYSTEM
        !call logger%logTextWithNum("NEWDEL End 707 Y1AIM=",Y1AIM)
                        RETURN
                        END IF
                        END
      SUBROUTINE RAYDERIV(X1LAST,Y1LAST,X1ONE,Y1ONE
     1,RXONE,RYONE,RXLAST,RYLAST,D11,D12,D21,D22)
C
      IMPLICIT NONE
C
      REAL*8 X1LAST,Y1LAST,X1ONE,Y1ONE,RXONE,RYONE
     1,RXLAST,RYLAST,D11,D12,D21,D22
C
        INCLUDE 'DATMAI.INC'
C

C       DERIVATIVE 1 (D11) IS:
        IF((X1LAST-X1ONE).EQ.0.0D0)THEN
                D11=0.0D0
                !PRINT *, "0 in RAYDERIV!"
                        ELSE
C
                D11=(RXLAST-RXONE)/(X1LAST-X1ONE)
C
                        END IF
C
C       DERIVATIVE 2 (D12) IS:
        IF((Y1LAST-Y1ONE).EQ.0.0D0) THEN
                D12=0.0D0
                !PRINT *, "0 in RAYDERIV!"
                        ELSE
C
                D12=(RXLAST-RXONE)/(Y1LAST-Y1ONE)
C
                        END IF
C
C       DERIVATIVE 3 (D21) IS:
        IF((X1LAST-X1ONE).EQ.0.0D0) THEN
                D21=0.0D0
                !PRINT *, "0 in RAYDERIV!"
                        ELSE
C
                D21=(RYLAST-RYONE)/(X1LAST-X1ONE)
C
                        END IF
C
C       DERIVATIVE 4 (D22) IS:
        IF((Y1LAST-Y1ONE).EQ.0.0D0) THEN
                D22=0.0D0
                !PRINT *, "0 in RAYDERIV!"
                        ELSE
C
                D22=(RYLAST-RYONE)/(Y1LAST-Y1ONE)
                        END IF
C
                       RETURN
                       END
      SUBROUTINE CRAYDERIV(X1LAST,Y1LAST,X1ONE,Y1ONE
     1,RXONE,RYONE,RXLAST,RYLAST,D11,D12,D21,D22)
C
      IMPLICIT NONE
C
      REAL*8 X1LAST,Y1LAST,X1ONE,Y1ONE,RXONE,RYONE
     1,RXLAST,RYLAST,D11,D12,D21,D22
C
        INCLUDE 'DATMAI.INC'
C
C       DERIVATIVE 1 (D11) IS:
      PRINT *, "CRAYDERIV ROUTINE STARTED!"
        IF((X1LAST-X1ONE).EQ.0.0D0)THEN
                D11=0.0D0
                        ELSE
C
                D11=(RXLAST-RXONE)/(X1LAST-X1ONE)
C
                        END IF
C
C       DERIVATIVE 2 (D12) IS:
        IF((Y1LAST-Y1ONE).EQ.0.0D0) THEN
                D12=0.0D0
                        ELSE
C
                D12=(RXLAST-RXONE)/(Y1LAST-Y1ONE)
C
                        END IF
C
C       DERIVATIVE 3 (D21) IS:
        IF((X1LAST-X1ONE).EQ.0.0D0) THEN
                D21=0.0D0
                        ELSE
C
                D21=(RYLAST-RYONE)/(X1LAST-X1ONE)
C
                        END IF
C
C       DERIVATIVE 4 (D22) IS:
        IF((Y1LAST-Y1ONE).EQ.0.0D0) THEN
                D22=0.0D0
                        ELSE
C
                D22=(RYLAST-RYONE)/(Y1LAST-Y1ONE)
                        END IF
C
                       RETURN
                       END
C      THIS IS THE EIGHTH FILE OF RAYTRACING ROUTINES
