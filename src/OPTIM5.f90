!       FIFTH SET OF OPTIMIZATION ROUTINES

! SUB CALCPRE.FOR
SUBROUTINE CALCPRE
!
!     CALCULATES AND LOADS THE VALUE OF OPERAND NUMBER PREDEFI
!
   use DATCFG
   use DATSPD
   use DATSUB
   use DATLEN
   use mod_system
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
   COMMON/SOLU/X
!
   CHARACTER WQLOCAL*8
!
   REAL*8 W2LOCAL,CLEAR
!
   COMMON/LOCALWQ/WQLOCAL,W2LOCAL
!
   REAL*8 X(1:96),NWN1,NWN2,NWN3,NWN4,JPX,TRAYY,TRAYX ,JPY,JPUX,JPUY,JPCX,JPCY,JPUCX,JPUCY,VXLO,VXHI,VYLO,VYHI ,V1,V2,VALVAL,OPDWT,GREYOP,RPOINT,FPOINT,RUN_OPT_MAC
!
   COMMON/WTOPD/OPDWT,GREYOP
!
   INTEGER IAUTO,ISFI,ITYP,SF,CW,IV,PREDEFI,I,NF,IIA,JIA,SF1 ,K,II,WVNUMOP,ERROR,ISURF,CLRTYP
!
   COMMON/AUTOI/IAUTO
!
   REAL*8 WV,VALUE,CONSUM,DELLELL,DELLELL1,DELLELL2
   INTEGER NUM5,ORI
   COMMON/GV/VALUE,NUM5
!
   COMMON/PRCOM/WV,ITYP
!
   REAL*8 OLDREG9,W1A,TEMPR1,TEMPR2,TEMPSUM,TEMPDIF ,INDEX,EDGTHK,VNUM,PARTL,V,DISP,INTV,W1B,XCJ,YCJ,ZCJ,DIAX,DIAY ,ENDIST,EXDIST,AAA
!
   COMMON/NSIN/NF,INDEX,DISP,VNUM,PARTL
!
   LOGICAL ERRFOB,OPMAP,XIS,REFERR,GERROR1,GERROR2,OPDERROR
!
   COMMON/OPOPMP/OPMAP
!
   REAL*8 INV,COSARG,OPVALUE,DUMMY,EFLY,EFLX
!
   COMMON/PREPRE/PREDEFI
!
   LOGICAL FUN ,OLDLDIF,OLDLDIF2,ERRR,ERROP
!
   EXTERNAL EDGTHK
!
   COMMON/OPDFIT1/ERROP
   COMMON/OPDFIT2/WVNUMOP
!
   INTEGER ONUM,CFNUM
!
!
   REG(9)=0.0D0
!
!     THESE ARE DEFINED IN DATSPD.FOR
!     THESE NEXT 6 VARIABLES TRACK STATUS OF FIELD,RAY,CFG,SPDEXT
!     CPFNEXT AND ZERNIKE FIT
!     FOR PREDEFINED OPERANDS TO MINIMIZE EXCESSIVE RAY TRACING
!     MAKE SURE ALL PARAXIAL AND RELATED VALUES ARE UP TO DATE
!
   I=PREDEFI
!
!     I IS THE NUMBER OF THE OPERAND IN THE MERIT FUNCTION
!
!
   ONUM=INT(OPERND(I,17))
!     ONUM IS THE IDENTIFYING INTEGER FOR OPERAND I
!
!     TEST FOR A BAD OPERAND NUMBER
   IF(ONUM.LT.1.OR.ONUM.GT.515.0D0.AND.ONUM.LT.801.OR.ONUM .GT.801) THEN
!     PUT IN A BAD NUMBER
      REG(9)=-666.666
      GO TO 777
   END IF
!
   CFNUM=INT(OPERND(I,16))
!     CFNUM IS THE CONFIG IN WHICH THE OPERAND MUST BE EVALUATED
!     SAVE ORIGINAL VALUE OF ACC
   OLDREG9=REG(9)
!     IF NECESSARY, CHGANGE CONFIGS
!     IF DESIRED CONFIG IS NOT THE CURRENT CONFIG
   IF(CFNUM.NE.F12) THEN
      IAUTO=CFNUM
      CALL CFGCHG2
   END IF
!
   SELECT CASE (ONUM)
!
    CASE(480)
!     CROSS TRACK SPECTRAL CTSX
      ERROR=0
      V1=OPERND(I,8)
      V2=OPERND(I,9)
      CACOCH=0
      CALL CTS(VALVAL,1,V1,V2,ERROR)
      IF(ERROR.EQ.1) THEN
         REG(9)=0.0D0
         CALL MACFAL
         RETURN
      END IF
      REG(9)=VALVAL
      GO TO 777
!
    CASE(481)
!     CROSS TRACK SPECTRAL CTSY
      ERROR=0
      V1=OPERND(I,8)
      V2=OPERND(I,9)
      CACOCH=0
      CALL CTS(VALVAL,2,V1,V2,ERROR)
      IF(ERROR.EQ.1) THEN
         REG(9)=0.0D0
         CALL MACFAL
         RETURN
      END IF
      REG(9)=VALVAL
      GO TO 777
!
    CASE(482)
!     SPATIAL COREGISTRATION ERROR SCEX
      ERROR=0
      V1=OPERND(I,8)
      CACOCH=0
      CALL SCE(VALVAL,1,V1,ERROR)
      IF(ERROR.EQ.1) THEN
         REG(9)=0.0D0
         CALL MACFAL
         RETURN
      END IF
      REG(9)=VALVAL
      GO TO 777
!
    CASE(483)
!     SPATIAL COREGISTRATION ERROR SCEX
      ERROR=0
      V1=OPERND(I,8)
      CACOCH=0
      CALL SCE(VALVAL,2,V1,ERROR)
      IF(ERROR.EQ.1) THEN
         REG(9)=0.0D0
         CALL MACFAL
         RETURN
      END IF
      REG(9)=VALVAL
      GO TO 777
!
    CASE(512)
!     OPERAND #512 "MACOPT"
      REG(9)=RUN_OPT_MAC(INT(OPERND(I,8)))
      GO TO 777
!
    CASE(801)
!     OPERAND #801 "ACT"
      ISURF=INT(OPERND(I,8))
      WQLOCAL='ACT     '
      W2LOCAL=OPERND(I,9)
      CALL DEFGRIDS(7,ISURF,GERROR1,GERROR2)
      IF(GERROR1.OR.GERROR2) THEN
         VALUE=0.0D0
         CALL MACFAL
         RETURN
      END IF
      REG(9)=VALUE
      GO TO 777
!
   END SELECT

!
!     REAL RAY BASED OPERANDS
   IF(ONUM.GE.1.AND.ONUM.LE.69) THEN
!     CALCULATE VALUE AND PLACE IN ACC
!
!     TEST IF RAY NEEDS TO BE TRACED
!
      IF(ONUM.EQ.16.OR.ONUM.EQ.17) THEN
         FPOINT=OPERND(I,8)
         RPOINT=OPERND(I,9)
      ELSE
         FPOINT=OPERND(I,9)
         RPOINT=OPERND(I,10)
      END IF
      IF(REFEXT.AND.INT(FPOINT).EQ.OLDF.AND.CFNUM.EQ.OLDCFG) THEN
!     DON'T NEED TO TRACE THE FOB AGAIN, IT ALREDY EXITS
      ELSE
!     TRACE FOB AND RAY THEN GET VALUE
!     TRACE FIELD DESIGNAMTED BY NW4 INT(FPOINT))
!     TRACE RAY   DESIGNAMTED BY NW5 INT(RPOINT)
!     FOR REFERENCE RAY STUFF, RAY 0 0 IS ALWAYS TRACED
!     DO THE FOB
         SAVE_KDP(1)=SAVEINPT(1)
         OLDLDIF2=LDIF2
         OLDLDIF=LDIF
         IF(OPDIF) THEN
            LDIF2=.TRUE.
            LDIF=.TRUE.
         ELSE
            LDIF2=.FALSE.
            LDIF=.FALSE.
         END IF
         WC='FOB     '
         WQ='        '
         SQ=0
         SST=0
         STI=0
         W1=FIELDY(INT(FPOINT))
         W2=FIELDX(INT(FPOINT))
         W3=FIELDZ(INT(FPOINT))
         W4=FIELDW(INT(FPOINT))
         W5=0.0D0
         DF1=0
         DF2=0
         DF3=0
         DF4=0
         DF5=1
         S1=1
         S2=1
         S3=1
         S4=1
         S5=0
         SN=1
!     SET MSG TO FALSE
         MSG=.FALSE.
         BADOPS=.FALSE.
         CALL FFOB2
         REST_KDP(1)=RESTINPT(1)
         IF(BADOPS) THEN
            IF(F28.EQ.1) BAAD=1
            IF(F28.EQ.1) REG(9)=0.0D0
            IF(F31.EQ.1) CALL MACFAL
            RETURN
         END IF
      END IF
! NOW THE RAY 0 0
!       SAVE_KDP(1)=SAVEINPT(1)
!       WC='RAY     '
!       WQ='        '
!       SQ=0
!       SST=0
!       DF1=1
!       DF2=1
!       DF3=0
!       DF4=1
!       DF5=1
!       S1=0
!       S2=0
!       S3=1
!       S4=0
!       S5=0
!       SN=0
!     SET MSG TO FALSE
!       MSG=.FALSE.
!     BADOPS=.FALSE.
!     W3=FIELDW(INT(FPOINT))
!     NOCOAT=.TRUE.
!       GRASET=.FALSE.
!       DXFSET=.FALSE.
!       CALL RRAY2
!     CACOCH=0
!       REST_KDP(1)=RESTINPT(1)
!     IF(BADOPS) THEN
!     IF(F28.EQ.1) BAAD=1
!     IF(F28.EQ.1) REG(9)=0.0D0
!     IF(F31.EQ.1) CALL MACFAL
!                       RETURN
!                     END IF
! NOW THE RAY
      IF(RAYEXT.AND.INT(RPOINT).EQ.OLDR.AND.CFNUM.EQ.OLDCFG) THEN
!     DON'T NEED TO TRACE THE RAY AGAIN, IT ALREDY EXITS
      ELSE
         SAVE_KDP(1)=SAVEINPT(1)
         WC='RAY     '
         WQ='        '
         SQ=0
         SST=0
         DF1=0
         DF2=0
         DF3=0
         DF4=1
         DF5=1
         S1=1
         S2=1
         S3=1
         S4=0
         S5=0
         SN=1
         IF(OPERND(I,17).GE.43.0D0.AND.OPERND(I,17).LE.58.0D0) THEN
            W1=0.0D0
            W2=0.0D0
            W3=FIELDW(INT(FPOINT))
         ELSE
!     ADDING VIGNETTING OPTIONS TO TRANSVERS ABERRATIONS, THEIR DERIVATIVES
!     AND ASSOCIATED OPD AND OPDW VALUES.
            IF(OPERND(I,17).GE.7.0D0.AND.OPERND(I,17).LE.12.0D0.OR.OPERND(I,17).GE.16.0D0.AND.OPERND(I,17).LE.17.0D0.OR.OPERND(I,17).GE.35.0D0.AND.OPERND(I,17).LE.42.0D0) THEN
               IF(LVIG) CALL VIGCAL(100,VXLO,VXHI,1)
               IF(LVIG) CALL VIGCAL(100,VYLO,VYHI,2)
            ELSE
               VXLO=-1.0D0
               VXHI=1.0D0
               VYLO=-1.0D0
               VYHI=1.0D0
            END IF
            IF(RAYY(INT(RPOINT)).GE.0.0D0)TRAYY=RAYY(INT(RPOINT))*DABS(VYHI)
            IF(RAYY(INT(RPOINT)).LT.0.0D0)TRAYY=RAYY(INT(RPOINT))*DABS(VYLO)
            IF(RAYX(INT(RPOINT)).GE.0.0D0)TRAYX=RAYX(INT(RPOINT))*DABS(VXHI)
            IF(RAYX(INT(RPOINT)).LT.0.0D0)TRAYX=RAYX(INT(RPOINT))*DABS(VXLO)
            W1=TRAYY
            W2=TRAYX
            W3=RAYW(INT(RPOINT))
         END IF
         W4=0.0D0
         W5=0.0D0
!     SET MSG TO FALSE
         MSG=.FALSE.
         BADOPS=.FALSE.
         NOCOAT=.TRUE.
         GRASET=.FALSE.
         DXFSET=.FALSE.
         CALL RRAY2
         CACOCH=0
         REST_KDP(1)=RESTINPT(1)
         IF(BADOPS) THEN
            IF(F28.EQ.1) BAAD=1
            IF(F28.EQ.1) REG(9)=0.0D0
            IF(F31.EQ.1) CALL MACFAL
            RETURN
         END IF
         LDIF2=OLDLDIF2
         LDIF=OLDLDIF
      END IF
      IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
         IF(F28.EQ.1) REG(9)=0.0D0
         IF(F28.EQ.1) BAAD=1
         IF(F31.EQ.1) CALL MACFAL
         RETURN
      END IF
      OLDF=INT(FPOINT)
      OLDW=0
      OLDR=INT(RPOINT)
      OLDCFG=CFNUM
!     GET DATA FOR SURFACE DESIGNATED BY NW3 INT(OPERND(I,8))
      IF(OPERND(I,17).EQ.1.0D0) THEN
         REG(9)=RAYRAY(1,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.2.0D0) THEN
         REG(9)=RAYRAY(2,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.3.0D0) THEN
         REG(9)=RAYRAY(3,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.4.0D0) THEN
         REG(9)=RAYRAY(4,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.5.0D0) THEN
         REG(9)=RAYRAY(5,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.6.0D0) THEN
         REG(9)=RAYRAY(6,INT(OPERND(I,8)))
         GO TO 777
      END IF
!
      IF(OPERND(I,17).EQ.7.0D0) THEN
!     DX
         REG(9)=(RAYRAY(1,INT(OPERND(I,8)))-REFRY(1,INT(OPERND(I,8))))
         GO TO 777
      END IF
!
      IF(OPERND(I,17).EQ.8.0D0) THEN
!     DY
         REG(9)=(RAYRAY(2,INT(OPERND(I,8)))-REFRY(2,INT(OPERND(I,8))))
         GO TO 777
      END IF
!
      IF(OPERND(I,17).EQ.9.0D0) THEN
         REG(9)=DABS(DSQRT((RAYRAY(1,INT(OPERND(I,8)))**2)+(RAYRAY(2,INT(OPERND(I,8)))**2))-DSQRT((REFRY(1,INT(OPERND(I,8)))**2)+(REFRY(2,INT(OPERND(I,8)))**2)))
         GO TO 777
      END IF
!
      IF(OPERND(I,17).EQ.10.0D0) THEN
         REG(9)=RAYRAY(11,INT(OPERND(I,8)))-REFRY(11,INT(OPERND(I,8)))
         IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
         IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
         IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
         GO TO 777
      END IF
!
      IF(OPERND(I,17).EQ.11.0D0) THEN
         REG(9)=RAYRAY(12,INT(OPERND(I,8)))-REFRY(12,INT(OPERND(I,8)))
         IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
         IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
         IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
         GO TO 777
      END IF
!
      IF(OPERND(I,17).EQ.12.0D0) THEN
         COSARG=((RAYRAY(4,INT(OPERND(I,8)))*REFRY(4,INT(OPERND(I,8))))+(RAYRAY(5,INT(OPERND(I,8)))*REFRY(5,INT(OPERND(I,8))))+(RAYRAY(6,INT(OPERND(I,8)))*REFRY(6,INT(OPERND(I,8)))))
         IF(COSARG.LT.0.0D0) COSARG=-COSARG
         IF(COSARG.GT.1.0D0) COSARG=1.0D0
         REG(9)=DACOS(COSARG)
         IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
         IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
         IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
         GO TO 777
      END IF
!
      IF(OPERND(I,17).EQ.13.0D0) THEN
         REG(9)=RAYRAY(11,INT(OPERND(I,8)))
         IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
         IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
         IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.14.0D0) THEN
         REG(9)=RAYRAY(12,INT(OPERND(I,8)))
         IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
         IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
         IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.15.0D0) THEN
         REG(9)=RAYRAY(7,INT(OPERND(I,8)))
         GO TO 777
      END IF
!
      IF(OPERND(I,17).EQ.16.0D0) THEN
         CALL GETOPD(OPVALUE,DUMMY,OPDERROR)
         IF(OPDERROR) OPVALUE=0.0D0
         REG(9)=OPVALUE
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.17.0D0) THEN
         CALL GETOPD(DUMMY,OPVALUE,OPDERROR)
         IF(OPDERROR) OPVALUE=0.0D0
         REG(9)=OPVALUE
         GO TO 777
      END IF
!
      IF(OPERND(I,17).EQ.18.0D0) THEN
         REG(9)=RAYRAY(19,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.19.0D0) THEN
         REG(9)=RAYRAY(20,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.20.0D0) THEN
         REG(9)=RAYRAY(21,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.21.0D0) THEN
         REG(9)=RAYRAY(8,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.22.0D0) THEN
         REG(9)=RAYRAY(9,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.23.0D0) THEN
         REG(9)=RAYRAY(10,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.24.0D0) THEN
         REG(9)=RAYRAY(13,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.25.0D0) THEN
         REG(9)=RAYRAY(14,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.26.0D0) THEN
         REG(9)=RAYRAY(15,INT(OPERND(I,8)))
         GO TO 777
      END IF
!
      IF(OPERND(I,17).EQ.27.0D0) THEN
         IF(OPDIF) THEN
            REG(9)=(RFDIFF(1,INT(OPERND(I,8)))-REFRY(1,INT(OPERND(I,8))))/RFDELX
         ELSE
            REG(9)=0.0D0
         END IF
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.28.0D0) THEN
         IF(OPDIF) THEN
            REG(9)=(RFDIFF(7,INT(OPERND(I,8)))-REFRY(1,INT(OPERND(I,8))))/RFDELY
         ELSE
            REG(9)=0.0D0
         END IF
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.29.0D0) THEN
         IF(OPDIF) THEN
            REG(9)=(RFDIFF(2,INT(OPERND(I,8)))-REFRY(2,INT(OPERND(I,8))))/RFDELX
         ELSE
            REG(9)=0.0D0
         END IF
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.30.0D0) THEN
         IF(OPDIF) THEN
            REG(9)=(RFDIFF(8,INT(OPERND(I,8)))-REFRY(2,INT(OPERND(I,8))))/RFDELY
         ELSE
            REG(9)=0.0D0
         END IF
         GO TO 777
      END IF
!
      IF(OPERND(I,17).EQ.31.0D0) THEN
         IF(OPDIF) THEN
            IF(DABS(RFDIFF(4,INT(OPERND(I,8)))).GE.(1.0D35*DABS(RFDIFF(6,INT(OPERND(I,8)))))) THEN
               IF((RFDIFF(4,INT(OPERND(I,8)))).GE.0.0D0) V1=PII/2.0D0
               IF((RFDIFF(4,INT(OPERND(I,8)))).LT.0.0D0) V1=(3.0D0*PII)/2.0D0
            ELSE
               IF(DABS(RFDIFF(4,INT(OPERND(I,8)))).EQ.0.0D0.AND.DABS(RFDIFF(6,INT(OPERND(I,8)))).EQ.0.0D0) THEN
                  V1=0.0D0
               ELSE
                  V1=DATAN2(RFDIFF(4,INT(OPERND(I,8))),RFDIFF(6,INT(OPERND(I,8))))
               END IF
               IF((V1).LT.0.0D0) V1=V1+(TWOPII)
            END IF
            REG(9)=(V1-REFRY(11,INT(OPERND(I,8))))
            IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
            IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
            IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
            REG(9)=REG(9)/RFDELX
         ELSE
            REG(9)=0.0D0
         END IF
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.32.0D0) THEN
         IF(OPDIF) THEN
            IF(DABS(RFDIFF(10,INT(OPERND(I,8)))).GE.(1.0D35*DABS(RFDIFF(12,INT(OPERND(I,8)))))) THEN
               IF((RFDIFF(10,INT(OPERND(I,8)))).GE.0.0D0) V1=PII/2.0D0
               IF((RFDIFF(10,INT(OPERND(I,8)))).LT.0.0D0) V1=(3.0D0*PII)/2.0D0
            ELSE
               IF(DABS(RFDIFF(10,INT(OPERND(I,8)))).EQ.0.0D0.AND.DABS(RFDIFF(12,INT(OPERND(I,8)))).EQ.0.0D0) THEN
                  V1=0.0D0
               ELSE
                  V1=DATAN2(RFDIFF(10,INT(OPERND(I,8))),RFDIFF(12,INT(OPERND(I,8))))
               END IF
               IF((V1).LT.0.0D0) V1=V1+(TWOPII)
            END IF
            REG(9)=(V1-REFRY(11,INT(OPERND(I,8))))
            IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
            IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
            IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
            REG(9)=REG(9)/RFDELY
         ELSE
            REG(9)=0.0D0
         END IF
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.33.0D0) THEN
         IF(OPDIF) THEN
            IF(DABS(RFDIFF(5,INT(OPERND(I,8)))).GE.(1.0D35*DABS(RFDIFF(6,INT(OPERND(I,8)))))) THEN
               IF((RFDIFF(5,INT(OPERND(I,8)))).GE.0.0D0) V1=PII/2.0D0
               IF((RFDIFF(5,INT(OPERND(I,8)))).LT.0.0D0) V1=(3.0D0*PII)/2.0D0
            ELSE
               IF(DABS(RFDIFF(5,INT(OPERND(I,8)))).EQ.0.0D0.AND.DABS(RFDIFF(6,INT(OPERND(I,8)))).EQ.0.0D0) THEN
                  V1=0.0D0
               ELSE
                  V1=DATAN2(RFDIFF(5,INT(OPERND(I,8))),RFDIFF(6,INT(OPERND(I,8))))
               END IF
               IF((V1).LT.0.0D0) V1=V1+(TWOPII)
            END IF
            REG(9)=(V1-REFRY(12,INT(OPERND(I,8))))
            IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
            IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
            IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
            REG(9)=REG(9)/RFDELX
         ELSE
            REG(9)=0.0D0
         END IF
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.34.0D0) THEN
         IF(OPDIF) THEN
            IF(DABS(RFDIFF(11,INT(OPERND(I,8)))).GE.(1.0D35*DABS(RFDIFF(12,INT(OPERND(I,8)))))) THEN
               IF((RFDIFF(11,INT(OPERND(I,8)))).GE.0.0D0) V1=PII/2.0D0
               IF((RFDIFF(11,INT(OPERND(I,8)))).LT.0.0D0) V1=(3.0D0*PII)/2.0D0
            ELSE
               IF(DABS(RFDIFF(11,INT(OPERND(I,8)))).EQ.0.0D0.AND.DABS(RFDIFF(12,INT(OPERND(I,8)))).EQ.0.0D0) THEN
                  V1=0.0D0
               ELSE
                  V1=DATAN2(RFDIFF(11,INT(OPERND(I,8))),RFDIFF(12,INT(OPERND(I,8))))
               END IF
               IF((V1).LT.0.0D0) V1=V1+(TWOPII)
            END IF
            REG(9)=(V1-REFRY(12,INT(OPERND(I,8))))
            IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
            IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
            IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
            REG(9)=REG(9)/RFDELY
         ELSE
            REG(9)=0.0D0
         END IF
         GO TO 777
      END IF
!
      IF(OPERND(I,17).EQ.35.0D0) THEN
         IF(OPDIF) THEN
            REG(9)=(DIFF(1,INT(OPERND(I,8)))-RAYRAY(1,INT(OPERND(I,8))))/DELX
         ELSE
            REG(9)=0.0D0
         END IF
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.36.0D0) THEN
         IF(OPDIF) THEN
            REG(9)=(DIFF(7,INT(OPERND(I,8)))-RAYRAY(1,INT(OPERND(I,8))))/DELY
         ELSE
            REG(9)=0.0D0
         END IF
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.37.0D0) THEN
         IF(OPDIF) THEN
            REG(9)=(DIFF(2,INT(OPERND(I,8)))-RAYRAY(2,INT(OPERND(I,8))))/DELX
         ELSE
            REG(9)=0.0D0
         END IF
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.38.0D0) THEN
         IF(OPDIF) THEN
            REG(9)=(DIFF(8,INT(OPERND(I,8)))-RAYRAY(2,INT(OPERND(I,8))))/DELY
         ELSE
            REG(9)=0.0D0
         END IF
         GO TO 777
      END IF
!
      IF(OPERND(I,17).EQ.39.0D0) THEN
         IF(OPDIF) THEN
            IF(DABS(DIFF(4,INT(OPERND(I,8)))).GE.(1.0D35*DABS(DIFF(6,INT(OPERND(I,8)))))) THEN
               IF((DIFF(4,INT(OPERND(I,8)))).GE.0.0D0) V1=PII/2.0D0
               IF((DIFF(4,INT(OPERND(I,8)))).LT.0.0D0) V1=(3.0D0*PII)/2.0D0
            ELSE
               IF(DABS(DIFF(4,INT(OPERND(I,8)))).EQ.0.0D0.AND.DABS(DIFF(6,INT(OPERND(I,8)))).EQ.0.0D0) THEN
                  V1=0.0D0
               ELSE
                  V1=DATAN2(DIFF(4,INT(OPERND(I,8))),DIFF(6,INT(OPERND(I,8))))
               END IF
               IF((V1).LT.0.0D0) V1=V1+(TWOPII)
            END IF
            REG(9)=(V1-RAYRAY(11,INT(OPERND(I,8))))
            IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
            IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
            IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
            REG(9)=REG(9)/DELX
         ELSE
            REG(9)=0.0D0
         END IF
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.40.0D0) THEN
         IF(OPDIF) THEN
            IF(DABS(DIFF(10,INT(OPERND(I,8)))).GE.(1.0D35*DABS(DIFF(12,INT(OPERND(I,8)))))) THEN
               IF((DIFF(10,INT(OPERND(I,8)))).GE.0.0D0) V1=PII/2.0D0
               IF((DIFF(10,INT(OPERND(I,8)))).LT.0.0D0) V1=(3.0D0*PII)/2.0D0
            ELSE
               IF(DABS(DIFF(10,INT(OPERND(I,8)))).EQ.0.0D0.AND.DABS(DIFF(12,INT(OPERND(I,8)))).EQ.0.0D0) THEN
                  V1=0.0D0
               ELSE
                  V1=DATAN2(DIFF(10,INT(OPERND(I,8))),DIFF(12,INT(OPERND(I,8))))
               END IF
               IF((V1).LT.0.0D0) V1=V1+(TWOPII)
            END IF
            REG(9)=(V1-RAYRAY(11,INT(OPERND(I,8))))
            IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
            IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
            IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
            REG(9)=REG(9)/DELY
         ELSE
            REG(9)=0.0D0
         END IF
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.41.0D0) THEN
         IF(OPDIF) THEN
            IF(DABS(DIFF(5,INT(OPERND(I,8)))).GE.(1.0D35*DABS(DIFF(6,INT(OPERND(I,8)))))) THEN
               IF((DIFF(5,INT(OPERND(I,8)))).GE.0.0D0) V1=PII/2.0D0
               IF((DIFF(5,INT(OPERND(I,8)))).LT.0.0D0) V1=(3.0D0*PII)/2.0D0
            ELSE
               IF(DABS(DIFF(5,INT(OPERND(I,8)))).EQ.0.0D0.AND.DABS(DIFF(6,INT(OPERND(I,8)))).EQ.0.0D0) THEN
                  V1=0.0D0
               ELSE
                  V1=DATAN2(DIFF(5,INT(OPERND(I,8))),DIFF(6,INT(OPERND(I,8))))
               END IF
               IF((V1).LT.0.0D0) V1=V1+(TWOPII)
            END IF
            REG(9)=(V1-RAYRAY(12,INT(OPERND(I,8))))
            IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
            IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
            IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
            REG(9)=REG(9)/DELX
         ELSE
            REG(9)=0.0D0
         END IF
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.42.0D0) THEN
         IF(OPDIF) THEN
            IF(DABS(DIFF(11,INT(OPERND(I,8)))).GE.(1.0D35*DABS(DIFF(12,INT(OPERND(I,8)))))) THEN
               IF((DIFF(11,INT(OPERND(I,8)))).GE.0.0D0) V1=PII/2.0D0
               IF((DIFF(11,INT(OPERND(I,8)))).LT.0.0D0) V1=(3.0D0*PII)/2.0D0
            ELSE
               IF(DABS(DIFF(11,INT(OPERND(I,8)))).EQ.0.0D0.AND.DABS(DIFF(12,INT(OPERND(I,8)))).EQ.0.0D0) THEN
                  V1=0.0D0
               ELSE
                  V1=DATAN2(DIFF(11,INT(OPERND(I,8))),DIFF(12,INT(OPERND(I,8))))
               END IF
               IF((V1).LT.0.0D0) V1=V1+(TWOPII)
            END IF
            REG(9)=(V1-RAYRAY(12,INT(OPERND(I,8))))
            IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
            IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
            IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
            REG(9)=REG(9)/DELY
         ELSE
            REG(9)=0.0D0
         END IF
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.43.0D0) THEN
         REG(9)=REFRY(1,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.44.0D0) THEN
         REG(9)=REFRY(2,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.45.0D0) THEN
         REG(9)=REFRY(3,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.46.0D0) THEN
         REG(9)=REFRY(4,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.47.0D0) THEN
         REG(9)=REFRY(5,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.48.0D0) THEN
         REG(9)=REFRY(6,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.49.0D0) THEN
         REG(9)=REFRY(19,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.50.0D0) THEN
         REG(9)=REFRY(20,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.51.0D0) THEN
         REG(9)=REFRY(21,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.52.0D0) THEN
         REG(9)=REFRY(9,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.53.0D0) THEN
         REG(9)=REFRY(10,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.54.0D0) THEN
         REG(9)=REFRY(11,INT(OPERND(I,8)))
         IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
         IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
         IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.55.0D0) THEN
         REG(9)=REFRY(12,INT(OPERND(I,8)))
         IF(REG(9).LT.-PII) REG(9)=REG(9)+(TWOPII)
         IF(REG(9).GT.PII) REG(9)=REG(9)-(TWOPII)
         IF(REG(9).EQ.TWOPII) REG(9)=0.0D0
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.56.0D0) THEN
         REG(9)=REFRY(13,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.57.0D0) THEN
         REG(9)=REFRY(14,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.58.0D0) THEN
         REG(9)=REFRY(15,INT(OPERND(I,8)))
         GO TO 777
      END IF
!
      IF(OPERND(I,17).GE.59.0D0.AND.OPERND(I,17).LE.67.0D0) THEN
         IF(.NOT.GLOBE) THEN
            IF(F28.EQ.1) REG(9)=0.0D0
            IF(F28.EQ.1) BAAD=1
            IF(F31.EQ.1) CALL MACFAL
            RETURN
         END IF
      END IF
      IF(OPERND(I,17).EQ.59.0D0) THEN
         REG(9)=GLRAY(1,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.60.0D0) THEN
         REG(9)=GLRAY(2,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.61.0D0) THEN
         REG(9)=GLRAY(3,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.62.0D0) THEN
         REG(9)=GLRAY(4,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.63.0D0) THEN
         REG(9)=GLRAY(5,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.64.0D0) THEN
         REG(9)=GLRAY(6,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.65.0D0) THEN
         REG(9)=GLRAY(7,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.66.0D0) THEN
         REG(9)=GLRAY(8,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.67.0D0) THEN
         REG(9)=GLRAY(9,INT(OPERND(I,8)))
         GO TO 777
      END IF
!
      IF(OPERND(I,17).EQ.68.0D0) THEN
         REG(9)=REFRY(8,INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.69.0D0) THEN
         REG(9)=REFRY(7,INT(OPERND(I,8)))
         GO TO 777
      END IF
   END IF
!
!     CLEARX OPERAND
   IF(ONUM.EQ.514) THEN
!     CALCULATE VALUE AND PLACE IN ACC
      CLFOB1=DABS(DBLE(INT(OPERND(I,8))))
      CLRAY1=DBLE(NINT(DABS(DABS(OPERND(I,8))-DBLE(INT(DABS(OPERND(I,8)))))*10000.0D0))
      CLFOB2=DABS(DBLE(INT(OPERND(I,9))))
      CLRAY2=DBLE(NINT(DABS(DABS(OPERND(I,9))-DBLE(INT(DABS(OPERND(I,9)))))*10000.0D0))
      WRITE(OUTLYNE,*) OPERND(I,10)
      CALL SHOWIT(1)
      CLSRF1=DABS(DBLE(INT(OPERND(I,10))))
      AAA=DABS(OPERND(I,10))
      CLSRF2=AAA-CLSRF1
      CLSRF2=DBLE(NINT(CLSRF2*1000.0D0))
!
      CLRTYP=1
      CALL CLEARANCE(CLEAR,CLRTYP)
      REG(9)=CLEAR
      GO TO 777
   END IF
!
!     CLEARY OPERAND
   IF(ONUM.EQ.515) THEN
!     CALCULATE VALUE AND PLACE IN ACC
      CLFOB1=DABS(DBLE(INT(OPERND(I,8))))
      CLRAY1=DBLE(NINT(DABS(DABS(OPERND(I,8))-DBLE(INT(DABS(OPERND(I,8)))))*10000.0D0))
      CLFOB2=DABS(DBLE(INT(OPERND(I,9))))
      CLRAY2=DBLE(NINT(DABS(DABS(OPERND(I,9))-DBLE(INT(DABS(OPERND(I,9)))))*10000.0D0))
      CLSRF1=DABS(DBLE(INT(OPERND(I,10))))
      AAA=DABS(OPERND(I,10))
      CLSRF2=AAA-CLSRF1
      CLSRF2=DBLE(NINT(CLSRF2*1000.0D0))
!
      CLRTYP=2
      CALL CLEARANCE(CLEAR,CLRTYP)
      REG(9)=CLEAR
      GO TO 777
   END IF
!     CLEARY OPERAND, TRACE TWO RAYS AND CALCULATE THE OPERAND VALUE
   IF(ONUM.EQ.515) THEN
!     CALCULATE VALUE AND PLACE IN ACC
!
      REG(9)=2702.0D0
      GO TO 777
   END IF
   IF(OPERND(I,17).GE.468.0D0.AND.OPERND(I,17).LE.471.0D0) THEN
!     SYMX,SYMY,ASYMX OR ASYMY. JUST TRACE THE RAYS AND CALC THE VALUES
!     OPERND(I,8) IS FRACTIONAL RAY HEIGHT
!     OPERND(I,9) IS FIELD NUMBER
!     OPERND(I,10) IS WAVELENGTH NUMBER FOR RAY, NOT REF RAY
!     DO THE FOB
      SAVE_KDP(1)=SAVEINPT(1)
      OLDLDIF2=LDIF2
      OLDLDIF=LDIF
      IF(OPDIF) THEN
         LDIF2=.TRUE.
         LDIF=.TRUE.
      ELSE
         LDIF2=.FALSE.
         LDIF=.FALSE.
      END IF
      WC='FOB     '
      WQ='        '
      SQ=0
      SST=0
      STI=0
      W1=FIELDY(INT(OPERND(I,9)))
      W2=FIELDX(INT(OPERND(I,9)))
      W3=FIELDZ(INT(OPERND(I,9)))
      W4=FIELDW(INT(OPERND(I,9)))
      W5=0.0D0
      DF1=0
      DF2=0
      DF3=0
      DF4=0
      DF5=1
      S1=1
      S2=1
      S3=1
      S4=1
      S5=0
      SN=1
!     SET MSG TO FALSE
      MSG=.FALSE.
      BADOPS=.FALSE.
      CALL FFOB2
      REST_KDP(1)=RESTINPT(1)
      IF(BADOPS) THEN
         IF(F28.EQ.1) BAAD=1
         IF(F28.EQ.1) REG(9)=0.0D0
         IF(F31.EQ.1) CALL MACFAL
         RETURN
      END IF
! NOW THE RAY 0 0
      SAVE_KDP(1)=SAVEINPT(1)
      WC='RAY     '
      WQ='        '
      SQ=0
      SST=0
      DF1=1
      DF2=1
      DF3=1
      DF4=1
      DF5=1
      S1=0
      S2=0
      S3=0
      S4=0
      S5=0
      SN=0
!     SET MSG TO FALSE
      MSG=.FALSE.
      BADOPS=.FALSE.
      NOCOAT=.TRUE.
      GRASET=.FALSE.
      DXFSET=.FALSE.
      CALL RRAY2
      CACOCH=0
      REST_KDP(1)=RESTINPT(1)
      IF(BADOPS) THEN
         IF(F28.EQ.1) BAAD=1
         IF(F28.EQ.1) REG(9)=0.0D0
         IF(F31.EQ.1) CALL MACFAL
         RETURN
      END IF
!     NOW THE RAY PAIR
      IF(OPERND(I,17).EQ.468.0D0.OR.OPERND(I,17).EQ.470.0D0) THEN
!     XZ-PLANE
! NOW THE RAY #1
         VXLO=-1.0D0
         VXHI=1.0D0
         IF(LVIG) CALL VIGCAL(100,VXLO,VXHI,1)
         SAVE_KDP(1)=SAVEINPT(1)
         WC='RAY     '
         WQ='        '
         SQ=0
         SST=0
         DF1=0
         DF2=0
         DF3=0
         DF4=1
         DF5=1
         S1=1
         S2=1
         S3=1
         S4=0
         S5=0
         SN=1
         W1=0.0D0
         W2=OPERND(I,8)*DABS(VXHI)
         W3=OPERND(I,10)
         W4=0.0D0
         W5=0.0D0
!     SET MSG TO FALSE
         MSG=.FALSE.
         BADOPS=.FALSE.
         NOCOAT=.TRUE.
         GRASET=.FALSE.
         DXFSET=.FALSE.
         CALL RRAY2
         CACOCH=0
         REST_KDP(1)=RESTINPT(1)
         IF(BADOPS) THEN
            IF(F28.EQ.1) BAAD=1
            IF(F28.EQ.1) REG(9)=0.0D0
            IF(F31.EQ.1) CALL MACFAL
            RETURN
         END IF
         LDIF2=OLDLDIF2
         LDIF=OLDLDIF
         IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
            IF(F28.EQ.1) BAAD=1
            IF(F28.EQ.1) REG(9)=0.0D0
            IF(F31.EQ.1) CALL MACFAL
            RETURN
         END IF
         IF(sys_mode().LE.2.0D0)VALUE=RAYRAY(1,NEWIMG)-REFRY(1,NEWIMG)
         IF(sys_mode().GE.3.0D0) THEN
            VALUE=RAYRAY(11,NEWIMG)-REFRY(11,NEWIMG)
            IF(VALUE.LT.-PII) VALUE=VALUE+(TWOPII)
            IF(VALUE.GT.PII) VALUE=VALUE-(TWOPII)
            IF(VALUE.EQ.TWOPII) VALUE=0.0D0
         END IF
         REG(9)=VALUE
! NOW THE RAY #2
         SAVE_KDP(1)=SAVEINPT(1)
         WC='RAY     '
         WQ='        '
         SQ=0
         SST=0
         DF1=0
         DF2=0
         DF3=0
         DF4=1
         DF5=1
         S1=1
         S2=1
         S3=1
         S4=0
         S5=0
         SN=1
         W1=0.0D0
         W2=OPERND(I,8)*DABS(VXLO)
         W3=OPERND(I,10)
         W4=0.0D0
         W5=0.0D0
!     SET MSG TO FALSE
         MSG=.FALSE.
         BADOPS=.FALSE.
         NOCOAT=.TRUE.
         GRASET=.FALSE.
         DXFSET=.FALSE.
         CALL RRAY2
         CACOCH=0
         REST_KDP(1)=RESTINPT(1)
         IF(BADOPS) THEN
            IF(F28.EQ.1) BAAD=1
            IF(F28.EQ.1) REG(9)=0.0D0
            IF(F31.EQ.1) CALL MACFAL
            RETURN
         END IF
         LDIF2=OLDLDIF2
         LDIF=OLDLDIF
         IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
            IF(F28.EQ.1) REG(9)=0.0D0
            IF(F28.EQ.1) BAAD=1
            IF(F31.EQ.1) CALL MACFAL
            RETURN
         END IF
         IF(sys_mode().LE.2.0D0)VALUE=RAYRAY(1,NEWIMG)-REFRY(1,NEWIMG)
         IF(sys_mode().GE.3.0D0) THEN
            VALUE=RAYRAY(11,NEWIMG)-REFRY(11,NEWIMG)
            IF(VALUE.LT.-PII) VALUE=VALUE+(TWOPII)
            IF(VALUE.GT.PII) VALUE=VALUE-(TWOPII)
            IF(VALUE.EQ.TWOPII) VALUE=0.0D0
         END IF
         IF(OPERND(I,17).EQ.468.0D0)REG(9)=(REG(9)-VALUE)/2.0D0
         IF(OPERND(I,17).EQ.470.0D0)REG(9)=(REG(9)+VALUE)/2.0D0
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.469.0D0.OR.OPERND(I,17).EQ.471.0D0) THEN
!     YZ-PLANE
! NOW THE RAY #1
         VYLO=-1.0D0
         VYHI=1.0D0
         IF(LVIG) CALL VIGCAL(100,VYLO,VYHI,2)
         SAVE_KDP(1)=SAVEINPT(1)
         WC='RAY     '
         WQ='        '
         SQ=0
         SST=0
         DF1=0
         DF2=0
         DF3=0
         DF4=1
         DF5=1
         S1=1
         S2=1
         S3=1
         S4=0
         S5=0
         SN=1
         W1=OPERND(I,8)*DABS(VYHI)
         W2=0.0D0
         W3=OPERND(I,10)
         W4=0.0D0
         W5=0.0D0
!     SET MSG TO FALSE
         MSG=.FALSE.
         BADOPS=.FALSE.
         NOCOAT=.TRUE.
         GRASET=.FALSE.
         DXFSET=.FALSE.
         CALL RRAY2
         CACOCH=0
         REST_KDP(1)=RESTINPT(1)
         IF(BADOPS) THEN
            IF(F28.EQ.1) BAAD=1
            IF(F28.EQ.1) REG(9)=0.0D0
            IF(F31.EQ.1) CALL MACFAL
            RETURN
         END IF
         LDIF2=OLDLDIF2
         LDIF=OLDLDIF
         IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
            IF(F28.EQ.1) REG(9)=0.0D0
            IF(F28.EQ.1) BAAD=1
            IF(F31.EQ.1) CALL MACFAL
            RETURN
         END IF
         IF(sys_mode().LE.2.0D0)VALUE=RAYRAY(2,NEWIMG)-REFRY(2,NEWIMG)
         IF(sys_mode().GE.3.0D0) THEN
            VALUE=RAYRAY(12,NEWIMG)-REFRY(12,NEWIMG)
            IF(VALUE.LT.-PII) VALUE=VALUE+(TWOPII)
            IF(VALUE.GT.PII) VALUE=VALUE-(TWOPII)
            IF(VALUE.EQ.TWOPII) VALUE=0.0D0
         END IF
         REG(9)=VALUE
! NOW THE RAY #2
         SAVE_KDP(1)=SAVEINPT(1)
         WC='RAY     '
         WQ='        '
         SQ=0
         SST=0
         DF1=0
         DF2=0
         DF3=0
         DF4=1
         DF5=1
         S1=1
         S2=1
         S3=1
         S4=0
         S5=0
         SN=1
         W1=OPERND(I,8)*DABS(VYLO)
         W2=0.0D0
         W3=OPERND(I,10)
         W4=0.0D0
         W5=0.0D0
!     SET MSG TO FALSE
         MSG=.FALSE.
         BADOPS=.FALSE.
         NOCOAT=.TRUE.
         GRASET=.FALSE.
         DXFSET=.FALSE.
         CALL RRAY2
         CACOCH=0
         REST_KDP(1)=RESTINPT(1)
         IF(BADOPS) THEN
            IF(F28.EQ.1) BAAD=1
            IF(F28.EQ.1) REG(9)=0.0D0
            IF(F31.EQ.1) CALL MACFAL
            RETURN
         END IF
         LDIF2=OLDLDIF2
         LDIF=OLDLDIF
         IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
            IF(F28.EQ.1) REG(9)=0.0D0
            IF(F28.EQ.1) BAAD=1
            IF(F31.EQ.1) CALL MACFAL
            RETURN
         END IF
         IF(sys_mode().LE.2.0D0)VALUE=RAYRAY(2,NEWIMG)-REFRY(2,NEWIMG)
         IF(sys_mode().GE.3.0D0) THEN
            VALUE=RAYRAY(12,NEWIMG)-REFRY(12,NEWIMG)
            IF(VALUE.LT.-PII) VALUE=VALUE+(TWOPII)
            IF(VALUE.GT.PII) VALUE=VALUE-(TWOPII)
            IF(VALUE.EQ.TWOPII) VALUE=0.0D0
         END IF
         IF(OPERND(I,17).EQ.469.0D0)REG(9)=(REG(9)-VALUE)/2.0D0
         IF(OPERND(I,17).EQ.471.0D0)REG(9)=(REG(9)+VALUE)/2.0D0
         GO TO 777
      END IF
   END IF
!     REAL RAY CHROMATICS
   IF(OPERND(I,17).GE.472.0D0.AND.OPERND(I,17).LE.479.0D0.OR.OPERND(I,17).EQ.510.0D0) THEN
      ERRR=.FALSE.
      IF(OPERND(I,17).NE.510.0D0) THEN
         IF(OPERND(I,17).EQ.472.0D0) CACOCH=0
         IF(OPERND(I,17).EQ.473.0D0) CACOCH=0
         IF(OPERND(I,17).EQ.474.0D0) CACOCH=0
         IF(OPERND(I,17).EQ.475.0D0) CACOCH=0
         IF(OPERND(I,17).EQ.476.0D0) CACOCH=0
         IF(OPERND(I,17).EQ.477.0D0) CACOCH=0
         IF(OPERND(I,17).EQ.478.0D0) CACOCH=0
         IF(OPERND(I,17).EQ.479.0D0) CACOCH=0
         IF(OPERND(I,17).EQ.472.0D0) CALL REALCOLR(1,ERRR)
         IF(OPERND(I,17).EQ.473.0D0) CALL REALCOLR(2,ERRR)
         IF(OPERND(I,17).EQ.474.0D0) CALL REALCOLR(3,ERRR)
         IF(OPERND(I,17).EQ.475.0D0) CALL REALCOLR(4,ERRR)
         IF(OPERND(I,17).EQ.476.0D0) CALL REALCOLR(5,ERRR)
         IF(OPERND(I,17).EQ.477.0D0) CALL REALCOLR(6,ERRR)
         IF(OPERND(I,17).EQ.478.0D0) CALL REALCOLR(7,ERRR)
         IF(OPERND(I,17).EQ.479.0D0) CALL REALCOLR(8,ERRR)
         IF(ERRR) THEN
            REG(9)=0.0D0
            CALL MACFAL
            RETURN
         END IF
      ELSE
!     CALCULATE CONRADY D-d OPERAND
!     CALC D-d
!     DO THE FOB
         SAVE_KDP(1)=SAVEINPT(1)
         OLDLDIF2=LDIF2
         OLDLDIF=LDIF
         IF(OPDIF) THEN
            LDIF2=.TRUE.
            LDIF=.TRUE.
         ELSE
            LDIF2=.FALSE.
            LDIF=.FALSE.
         END IF
         WC='FOB     '
         WQ='        '
         SQ=0
         SST=0
         STI=0
         W1=0.0D0
         W2=0.0D0
         W3=0.0D0
         W4=sys_wl_ref()
         W5=0.0D0
         DF1=0
         DF2=0
         DF3=0
         DF4=0
         DF5=1
         S1=1
         S2=1
         S3=1
         S4=1
         S5=0
         SN=1
!     SET MSG TO FALSE
         MSG=.FALSE.
         BADOPS=.FALSE.
         CALL FFOB2
         REST_KDP(1)=RESTINPT(1)
         IF(BADOPS) THEN
            IF(F28.EQ.1) BAAD=1
            IF(F28.EQ.1) REG(9)=0.0D0
            IF(F31.EQ.1) CALL MACFAL
            RETURN
         END IF
! NOW THE RAY 0.7 0
         SAVE_KDP(1)=SAVEINPT(1)
         WC='RAY     '
         WQ='        '
         SQ=0
         SST=0
         DF1=0
         DF2=1
         DF3=0
         DF4=1
         DF5=1
         S1=1
         S2=0
         S3=1
         S4=0
         S5=0
         SN=1
         W1=0.7D0
         W3=sys_wl_ref()
!     SET MSG TO FALSE
         MSG=.FALSE.
         BADOPS=.FALSE.
         NOCOAT=.TRUE.
         GRASET=.FALSE.
         DXFSET=.FALSE.
         CALL RRAY2
         CACOCH=0
         REST_KDP(1)=RESTINPT(1)
         IF(BADOPS) THEN
            IF(F28.EQ.1) BAAD=1
            IF(F28.EQ.1) REG(9)=0.0D0
            IF(F31.EQ.1) CALL MACFAL
            RETURN
         END IF
         CONSUM=0.0D0
         DO K=1,INT(sys_last_surf())
            DELLELL1=0.0D0
            DELLELL2=0.0D0
            DELLELL= 0.0D0
            IF(sys_wl_pri1().EQ.1.0D0)  DELLELL1=surf_refractive_index(K, 1)
            IF(sys_wl_pri1().EQ.2.0D0)  DELLELL1=surf_refractive_index(K, 2)
            IF(sys_wl_pri1().EQ.3.0D0)  DELLELL1=surf_refractive_index(K, 3)
            IF(sys_wl_pri1().EQ.4.0D0)  DELLELL1=surf_refractive_index(K, 4)
            IF(sys_wl_pri1().EQ.5.0D0)  DELLELL1=surf_refractive_index(K, 5)
            IF(sys_wl_pri1().EQ.6.0D0)  DELLELL1=surf_refractive_index(K, 6)
            IF(sys_wl_pri1().EQ.7.0D0)  DELLELL1=surf_refractive_index(K, 7)
            IF(sys_wl_pri1().EQ.8.0D0)  DELLELL1=surf_refractive_index(K, 8)
            IF(sys_wl_pri1().EQ.9.0D0)  DELLELL1=surf_refractive_index(K, 9)
            IF(sys_wl_pri1().EQ.10.0D0) DELLELL1=surf_refractive_index(K, 10)
            IF(sys_wl_pri2().EQ.1.0D0)  DELLELL2=surf_refractive_index(K, 1)
            IF(sys_wl_pri2().EQ.2.0D0)  DELLELL2=surf_refractive_index(K, 2)
            IF(sys_wl_pri2().EQ.3.0D0)  DELLELL2=surf_refractive_index(K, 3)
            IF(sys_wl_pri2().EQ.4.0D0)  DELLELL2=surf_refractive_index(K, 4)
            IF(sys_wl_pri2().EQ.5.0D0)  DELLELL2=surf_refractive_index(K, 5)
            IF(sys_wl_pri2().EQ.6.0D0)  DELLELL2=surf_refractive_index(K, 6)
            IF(sys_wl_pri2().EQ.7.0D0)  DELLELL2=surf_refractive_index(K, 7)
            IF(sys_wl_pri2().EQ.8.0D0)  DELLELL2=surf_refractive_index(K, 8)
            IF(sys_wl_pri2().EQ.9.0D0)  DELLELL2=surf_refractive_index(K, 9)
            IF(sys_wl_pri2().EQ.10.0D0) DELLELL2=surf_refractive_index(K, 10)
            DELLELL=(DELLELL1-DELLELL2)
            IF(DELLELL.NE.0.0D0)CONSUM=CONSUM+((RAYRAY(8,K)-surf_thickness(K-1))*DELLELL)
         END DO
         VALUE=CONSUM
      END IF
      REG(9)=VALUE
      GO TO 777
   END IF
   IF(OPERND(I,17).GE.70.0D0.AND.OPERND(I,17).LE.206.OR.OPERND(I,17).GE.485.0D0.AND.OPERND(I,17).LE.509.0D0.OR.OPERND(I,17).EQ.511.0D0) THEN
!     LENS DATABASE STUFF
      IF(OPERND(I,17).EQ.485.0D0) THEN
!     PIVX
         REG(9)=surf_pivot_x(INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.486.0D0) THEN
!     PIVY
         REG(9)=surf_pivot_y(INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.487.0D0) THEN
!     PIVZ
         REG(9)=surf_pivot_z(INT(OPERND(I,8)))
         GO TO 777
      END IF
      IF(OPERND(I,17).GE.94.0D0.AND.OPERND(I,17).LE.105.0D0) THEN
!     GLOBAL VERTEX DATA, IF NOT GLOBAL THEN OPERANDS NOT CALC. ELSE
!     CALC VALUE AND PROCEED
         SAVE_KDP(31)=SAVEINPT(31)
         WRITE(INPUT,*)'GLOBAL,',OPERND(I,9)
         CALL PROCES
         REST_KDP(31)=RESTINPT(31)
         IF(.NOT.GLOBE) THEN
            IF(F28.EQ.1) REG(9)=0.0D0
            IF(F28.EQ.1) BAAD=1
            IF(F31.EQ.1) CALL MACFAL
            RETURN
         END IF
!     CALCULATIONS FOR VERTEX STUFF GO HERE
         IF(OPERND(I,17).EQ.94.0D0) THEN
            REG(9)=VERTEX(1,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.95.0D0) THEN
            REG(9)=VERTEX(2,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.96.0D0) THEN
            REG(9)=VERTEX(3,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.97.0D0) THEN
            REG(9)=VERTEX(4,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.98.0D0) THEN
            REG(9)=VERTEX(5,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.99.0D0) THEN
            REG(9)=VERTEX(6,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.100.0D0) THEN
            REG(9)=VERTEX(7,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.101.0D0) THEN
            REG(9)=VERTEX(8,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.102.0D0) THEN
            REG(9)=VERTEX(9,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.103.0D0) THEN
            REG(9)=VERTEX(10,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.104.0D0) THEN
            REG(9)=VERTEX(11,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.105.0D0) THEN
            REG(9)=VERTEX(12,INT(OPERND(I,8)))
            GO TO 777
         END IF
      END IF
      IF(OPERND(I,17).GE.70.0D0.AND.OPERND(I,17).LE.93.0D0.OR.OPERND(I,17).GE.106.AND.OPERND(I,17).LE.206.0D0.OR.OPERND(I,17).GE.488.0D0.AND.OPERND(I,17).LE.509.0D0.OR.OPERND(I,17).EQ.511.0D0) THEN
!     JUST GET VALUE AND PROCEED.
!     CALCULATIONS FOR NON-VERTEX LENS DATABASE STUFF GOES HERE


         IF(OPERND(I,17).EQ.91.0D0) THEN
!     INDEX
            REG(9)=surf_fict_n(INT(OPERND(I,8)))
            GO TO 777
         END IF
!     N1 TO N10
         IF(OPERND(I,17).EQ.488.0D0) THEN
            REG(9)=ALENS(46,(INT(OPERND(I,8))))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.489.0D0) THEN
            REG(9)=ALENS(47,(INT(OPERND(I,8))))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.490.0D0) THEN
            REG(9)=ALENS(48,(INT(OPERND(I,8))))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.491.0D0) THEN
            REG(9)=ALENS(49,(INT(OPERND(I,8))))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.492.0D0) THEN
            REG(9)=ALENS(50,(INT(OPERND(I,8))))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.493.0D0) THEN
            REG(9)=ALENS(71,(INT(OPERND(I,8))))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.494.0D0) THEN
            REG(9)=ALENS(72,(INT(OPERND(I,8))))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.495.0D0) THEN
            REG(9)=ALENS(73,(INT(OPERND(I,8))))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.496.0D0) THEN
            REG(9)=ALENS(74,(INT(OPERND(I,8))))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.497.0D0) THEN
            REG(9)=ALENS(75,(INT(OPERND(I,8))))
            GO TO 777
         END IF
!     VALUE IS 70 TO 90, 92, 93,106 TO 206
!     LENGTH
         IF(OPERND(I,17).EQ.106.0D0) THEN
            V1=0.0D0
            DO II=INT(OPERND(I,8)),(INT(OPERND(I,9))-1)
               V1=V1+surf_thickness(II)
            END DO
            REG(9)=V1
            GO TO 777
         END IF
!     MLENGTH
         IF(OPERND(I,17).EQ.107.0D0) THEN
            V1=0.0D0
            DO II=INT(OPERND(I,8)),(INT(OPERND(I,9))-1)
               IF(INT(sys_wl_ref()).GE.1.AND.INT(sys_wl_ref()).LE.5) THEN
                  CW=INT(sys_wl_ref())+45
               END IF
               IF(INT(sys_wl_ref()).GE.6.AND.INT(sys_wl_ref()).LE.10) THEN
                  CW=INT(sys_wl_ref())+65
               END IF
               V1=V1+(surf_thickness(II)*ALENS(CW,II))
            END DO
            REG(9)=V1
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.70.0D0) THEN
!     RD
            IF(surf_curvature(INT(OPERND(I,8))).EQ.0.0D0) REG(9)=1.0D300
            IF(surf_curvature(INT(OPERND(I,8))).NE.0.0D0) REG(9)=1.0D0 /surf_curvature(INT(OPERND(I,8)))
            GO TO 777
         END IF
!     CV
         IF(OPERND(I,17).EQ.71.0D0) THEN
            REG(9)=surf_curvature(INT(OPERND(I,8)))
            GO TO 777
         END IF
!     TH
         IF(OPERND(I,17).EQ.72.0D0) THEN
            REG(9)=surf_thickness(INT(OPERND(I,8)))
            GO TO 777
         END IF
!     CC
         IF(OPERND(I,17).EQ.73.0D0) THEN
            REG(9)=surf_conic(INT(OPERND(I,8)))
            GO TO 777
         END IF
!     AC
         IF(OPERND(I,17).EQ.74.0D0) THEN
            REG(9)=surf_asphere_coeff(INT(OPERND(I,8)), 2)
            GO TO 777
         END IF
!     AD
         IF(OPERND(I,17).EQ.75.0D0) THEN
            REG(9)=surf_asphere_coeff(INT(OPERND(I,8)), 4)
            GO TO 777
         END IF
!     AE
         IF(OPERND(I,17).EQ.76.0D0) THEN
            REG(9)=surf_asphere_coeff(INT(OPERND(I,8)), 6)
            GO TO 777
         END IF
!     AF
         IF(OPERND(I,17).EQ.77.0D0) THEN
            REG(9)=surf_asphere_coeff(INT(OPERND(I,8)), 8)
            GO TO 777
         END IF
!     AG
         IF(OPERND(I,17).EQ.78.0D0) THEN
            REG(9)=surf_asphere_coeff(INT(OPERND(I,8)), 10)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.79.0D0) THEN
!     RDTOR
            IF(surf_toric_curvature(INT(OPERND(I,8))).EQ.0.0D0) REG(9)=1.0D300
            IF(surf_toric_curvature(INT(OPERND(I,8))).NE.0.0D0) REG(9)=1.0D0 /surf_toric_curvature(INT(OPERND(I,8)))
            GO TO 777
         END IF
!     CVTOR
         IF(OPERND(I,17).EQ.80.0D0) THEN
            REG(9)=surf_toric_curvature(INT(OPERND(I,8)))
            GO TO 777
         END IF
!     CCTOR
         IF(OPERND(I,17).EQ.81.0D0) THEN
            REG(9)=surf_anamorphic_conic(INT(OPERND(I,8)))
            GO TO 777
         END IF
!     ADTOR
         IF(OPERND(I,17).EQ.82.0D0) THEN
            REG(9)=surf_anamorphic_conic(INT(OPERND(I,8)))
            GO TO 777
         END IF
!     AETOR
         IF(OPERND(I,17).EQ.83.0D0) THEN
            REG(9)=surf_anamorphic_conic(INT(OPERND(I,8)))
            GO TO 777
         END IF
!     AFTOR
         IF(OPERND(I,17).EQ.84.0D0) THEN
            REG(9)=surf_anamorphic_conic(INT(OPERND(I,8)))
            GO TO 777
         END IF
!     AGTOR
         IF(OPERND(I,17).EQ.85.0D0) THEN
            REG(9)=surf_anamorphic_conic(INT(OPERND(I,8)))
            GO TO 777
         END IF
!     ALPHA
         IF(OPERND(I,17).EQ.86.0D0) THEN
            REG(9)=surf_alpha_deg(INT(OPERND(I,8)))
            GO TO 777
         END IF
!     BETA
         IF(OPERND(I,17).EQ.87.0D0) THEN
            REG(9)=surf_beta_deg(INT(OPERND(I,8)))
            GO TO 777
         END IF
!     GAMMA
         IF(OPERND(I,17).EQ.88.0D0) THEN
            REG(9)=surf_gamma_deg(INT(OPERND(I,8)))
            GO TO 777
         END IF
!     ABBE
         IF(OPERND(I,17).EQ.498.0D0) THEN
            NF=INT(OPERND(I,8))
            CALL SINDEX
            REG(9)=VNUM
            GO TO 777
         END IF
!     DPART
         IF(OPERND(I,17).EQ.499.0D0) THEN
            NF=INT(OPERND(I,8))
            REG(9)=surf_fict_w(NF)
            GO TO 777
         END IF
!     CLPX
         IF(OPERND(I,17).EQ.500.0D0) THEN
            NF=INT(OPERND(I,8))
            IF(surf_clap_type(NF).EQ.0.0D0.OR.surf_multi_clap_flag(NF).NE.0.0D0)REG(9)=0.0D0
            IF(surf_multi_clap_flag(NF).EQ.0.0D0) THEN
               IF(surf_clap_type(NF).EQ.1.0D0)REG(9)=surf_clap_dim(NF, 1)
               IF(surf_clap_type(NF).EQ.2.0D0)REG(9)=surf_clap_dim(NF, 2)
               IF(surf_clap_type(NF).EQ.3.0D0)REG(9)=surf_clap_dim(NF, 2)
               IF(surf_clap_type(NF).EQ.4.0D0)REG(9)=surf_clap_dim(NF, 2)
               IF(surf_clap_type(NF).EQ.5.0D0)REG(9)=surf_clap_dim(NF, 1)
               IF(surf_clap_type(NF).EQ.6.0D0)REG(9)=surf_clap_dim(NF, 2)
            END IF
            GO TO 777
         END IF
!     CLPY
         IF(OPERND(I,17).EQ.501.0D0) THEN
            NF=INT(OPERND(I,8))
            IF(surf_clap_type(NF).EQ.0.0D0.OR.surf_multi_clap_flag(NF).NE.0.0D0)REG(9)=0.0D0
            IF(surf_multi_clap_flag(NF).EQ.0.0D0) THEN
               IF(surf_clap_type(NF).EQ.1.0D0)REG(9)=surf_clap_dim(NF, 1)
               IF(surf_clap_type(NF).EQ.2.0D0)REG(9)=surf_clap_dim(NF, 1)
               IF(surf_clap_type(NF).EQ.3.0D0)REG(9)=surf_clap_dim(NF, 1)
               IF(surf_clap_type(NF).EQ.4.0D0)REG(9)=surf_clap_dim(NF, 1)
               IF(surf_clap_type(NF).EQ.5.0D0)REG(9)=surf_clap_dim(NF, 1)
               IF(surf_clap_type(NF).EQ.6.0D0)REG(9)=surf_clap_dim(NF, 2)
            END IF
            GO TO 777
         END IF
!     GDX
         IF(OPERND(I,17).EQ.502.0D0) THEN
            NF=INT(OPERND(I,8))
            REG(9)=surf_global_dx(NF)
            GO TO 777
         END IF
!     GDY
         IF(OPERND(I,17).EQ.503.0D0) THEN
            NF=INT(OPERND(I,8))
            REG(9)=surf_global_dy(NF)
            GO TO 777
         END IF
!     GDZ
         IF(OPERND(I,17).EQ.504.0D0) THEN
            NF=INT(OPERND(I,8))
            REG(9)=surf_global_dz(NF)
            GO TO 777
         END IF
!     GALPHA
         IF(OPERND(I,17).EQ.505.0D0) THEN
            NF=INT(OPERND(I,8))
            REG(9)=surf_global_alpha(NF)
            GO TO 777
         END IF
!     GBETA
         IF(OPERND(I,17).EQ.506.0D0) THEN
            NF=INT(OPERND(I,8))
            REG(9)=surf_global_beta(NF)
            GO TO 777
         END IF
!     GGAMMA
         IF(OPERND(I,17).EQ.507.0D0) THEN
            NF=INT(OPERND(I,8))
            REG(9)=surf_global_gamma(NF)
            GO TO 777
         END IF
!     GRS
         IF(OPERND(I,17).EQ.508.0D0) THEN
            NF=INT(OPERND(I,8))
            REG(9)=surf_grating_spacing(NF)
            GO TO 777
         END IF
!     WEIGHT
         IF(OPERND(I,17).EQ.509.0D0) THEN
            SAVE_KDP(1)=SAVEINPT(1)
            WC='WEIGHT  '
            WQ='ACC     '
            SQ=1
            W1=OPERND(I,8)
            W2=OPERND(I,9)
            W3=0.0D0
            W4=0.0D0
            W5=0.0D0
            DF1=0
            DF2=0
            DF3=1
            DF4=1
            DF5=1
            S1=1
            S2=1
            S3=0
            S4=0
            S5=0
            SN=1
            CALL WEIGHT
            REST_KDP(1)=RESTINPT(1)
            GO TO 777
         END IF
!     COST
         IF(OPERND(I,17).EQ.511.0D0) THEN
            SAVE_KDP(1)=SAVEINPT(1)
            WC='COST    '
            WQ='ACC     '
            SQ=1
            W1=OPERND(I,8)
            W2=OPERND(I,9)
            W3=0.0D0
            W4=0.0D0
            W5=0.0D0
            DF1=0
            DF2=0
            DF3=1
            DF4=1
            DF5=1
            S1=1
            S2=1
            S3=0
            S4=0
            S5=0
            SN=1
            CALL COST
            REST_KDP(1)=RESTINPT(1)
            GO TO 777
         END IF
!     VNUM
         IF(OPERND(I,17).EQ.89.0D0) THEN
            NF=INT(OPERND(I,8))
            REG(9)=surf_fict_v(NF)
            GO TO 777
         END IF
!     PARTL
         IF(OPERND(I,17).EQ.90.0D0) THEN
            NF=INT(OPERND(I,8))
            CALL SINDEX
            REG(9)=PARTL
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.92.0D0) THEN
!     XD
            REG(9)=surf_focus_dx(INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.467.0D0) THEN
!     ZD
            REG(9)=surf_focus_dz(INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.93.0D0) THEN
!     YD
            REG(9)=surf_focus_dy(INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.108.0D0) THEN
!     ET,ETY
            W1A=OPERND(I,8)
            REG(9)=EDGTHK(INT(W1A),1)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.109.0D0) THEN
!     ETX
            W1A=OPERND(I,8)
            REG(9)=EDGTHK(INT(W1A),2)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.110.0D0) THEN
!     SHAPEFAC
            W1A=OPERND(I,8)
            IF(DABS(surf_curvature(INT(W1A))).LT.1D-30) THEN
               IF(surf_curvature(INT(W1A)).GT.0.0D0) TEMPR1=1.0D30
               IF(surf_curvature(INT(W1A)).LT.0.0D0) TEMPR1=-1.0D30
            ELSE
               TEMPR1=1.0D0/surf_curvature(INT(W1A))
            END IF
            IF(DABS(surf_curvature(INT(W1A)+1)).LT.1D-30) THEN
               IF(surf_curvature(INT(W1A)+1).GT.0.0D0) TEMPR2=1.0D30
               IF(surf_curvature(INT(W1A)+1).LT.0.0D0) TEMPR2=-1.0D30
            ELSE
               TEMPR2=1.0D0/surf_curvature(INT(W1A)+1)
            END IF
            TEMPSUM=TEMPR2+TEMPR1
            TEMPDIF=TEMPR2-TEMPR1
            IF(DABS(TEMPDIF).EQ.0.0D0) THEN
               IF(TEMPSUM.EQ.0.0D0) REG(9)=0.0D0
               IF(TEMPSUM.GT.0.0D0.AND.TEMPDIF.GT.0.0D0) REG(9)=1.0D30
               IF(TEMPSUM.LT.0.0D0.AND.TEMPDIF.LT.0.0D0) REG(9)=1.0D30
               IF(TEMPSUM.GT.0.0D0.AND.TEMPDIF.LT.0.0D0) REG(9)=-1.0D30
               IF(TEMPSUM.LT.0.0D0.AND.TEMPDIF.GT.0.0D0) REG(9)=-1.0D30
            ELSE
               REG(9)=(TEMPR2+TEMPR1)/(TEMPR2-TEMPR1)
            END IF
            GO TO 777
         END IF
         IF(OPERND(I,17).GE.111.0D0.AND.OPERND(I,17).LE.206.0D0) THEN
!     SPSRF COEFS C1 TO C96
            REG(9)=FTFL01((INT(OPERND(I,17))-110),INT(OPERND(I,8)))
            GO TO 777
         END IF
      END IF
   END IF
!
   IF(OPERND(I,17).GE.207.0D0.AND.OPERND(I,17).LE.236) THEN
!
!     PARAXIAL OPERANDS
!
      IF(OPERND(I,17).EQ.207.0D0.OR.OPERND(I,17).EQ.208.0D0.OR.OPERND(I,17).EQ.209.0D0.OR.OPERND(I,17).EQ.210.0D0) THEN
!       YZ AND XZ PLANE EFL CALCULATION
!
         IF(OPERND(I,8).GT.0.0D0) THEN
            IIA=INT(OPERND(I,8))-1
         ELSE
            IIA=INT(OPERND(I,8))
         END IF
         JIA=INT(OPERND(I,9))
         EFLY=-(((PXTRAY(2,IIA)*PXTRAY(5,IIA+1))-(PXTRAY(1,IIA+1)*PXTRAY(6,IIA )))/((PXTRAY(2,IIA)*PXTRAY(6,JIA))-(PXTRAY(6,IIA)*PXTRAY(2,JIA))))
         EFLX=-(((PXTRAX(2,IIA)*PXTRAX(5,IIA+1))-(PXTRAX(1,IIA+1)*PXTRAX(6,IIA )))/((PXTRAX(2,IIA)*PXTRAX(6,JIA))-(PXTRAX(6,IIA)*PXTRAX(2,JIA))))
         IF(OPERND(I,17).EQ.209.0D0) THEN
            REG(9)=EFLX
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.210.0D0) THEN
            REG(9)=EFLY
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.207.0D0) THEN
            IF(EFLY.NE.0.0D0) THEN
               REG(9)=1.0D0/EFLY
            ELSE
               REG(9)=1.0D300
            END IF
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.208.0D0) THEN
            IF(EFLX.NE.0.0D0) THEN
               REG(9)=1.0D0/EFLX
            ELSE
               REG(9)=0.0D0
            END IF
            GO TO 777
         END IF
      END IF
!
      IF(OPERND(I,9).EQ.sys_wl_ref()) THEN
!     PARAXIAL VALUES AT THE CONTROL WAVELENGTH
         IF(OPERND(I,17).EQ.211.0D0) THEN
!     PY
            REG(9)=PXTRAY(1,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.212.0D0) THEN
!     PX
            REG(9)=PXTRAX(1,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.213.0D0) THEN
!     PCY
            REG(9)=PXTRAY(5,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.214.0D0) THEN
!     PCX
            REG(9)=PXTRAX(5,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.215.0D0) THEN
!     PUY
            REG(9)=PXTRAY(2,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.216.0D0) THEN
!     PUX
            REG(9)=PXTRAX(2,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.217.0D0) THEN
!     PUCY
            REG(9)=PXTRAY(6,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.218.0D0) THEN
!     PUCX
            REG(9)=PXTRAX(6,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.219.0D0) THEN
!     PIY
            REG(9)=PXTRAY(3,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.220.0D0) THEN
!     PIX
            REG(9)=PXTRAX(3,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.221.0D0) THEN
!     PICY
            REG(9)=PXTRAY(7,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.222.0D0) THEN
!     PICX
            REG(9)=PXTRAX(7,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.223.0D0) THEN
!     PIYP
            REG(9)=PXTRAY(4,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.224.0D0) THEN
!     PIXP
            REG(9)=PXTRAX(4,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.225.0D0) THEN
!     PICYP
            REG(9)=PXTRAY(8,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.226.0D0) THEN
!     PICXP
            REG(9)=PXTRAX(8,INT(OPERND(I,8)))
            GO TO 777
         END IF
      ELSE
!     NOT AT CONTROL WAVELENGTH
         WV=OPERND(I,9)
         ITYP=1
         CALL PRCOL
         WV=OPERND(I,9)
         ITYP=2
         CALL PRCOL
         IF(OPERND(I,17).EQ.211.0D0) THEN
!     PY
            REG(9)=COLY(1,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.212.0D0) THEN
!     PX
            REG(9)=COLX(1,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.213.0D0) THEN
!     PCY
            REG(9)=COLY(5,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.214.0D0) THEN
!     PCX
            REG(9)=COLX(5,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.215.0D0) THEN
!     PUY
            REG(9)=COLY(2,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.216.0D0) THEN
!     PUX
            REG(9)=COLX(2,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.217.0D0) THEN
!     PUCY
            REG(9)=COLY(6,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.218.0D0) THEN
!     PUCX
            REG(9)=COLX(6,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.219.0D0) THEN
!     PIY
            REG(9)=COLY(3,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.220.0D0) THEN
!     PIX
            REG(9)=COLX(3,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.221.0D0) THEN
!     PICY
            REG(9)=COLY(7,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.222.0D0) THEN
!     PICX
            REG(9)=COLX(7,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.223.0D0) THEN
!     PIYP
            REG(9)=COLY(4,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.224.0D0) THEN
!     PIXP
            REG(9)=COLX(4,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.225.0D0) THEN
!     PICYP
            REG(9)=COLY(8,INT(OPERND(I,8)))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.226.0D0) THEN
!     PICXP
            REG(9)=COLX(8,INT(OPERND(I,8)))
            GO TO 777
         END IF
      END IF
!
      IF(OPERND(I,17).GE.227.0D0.AND.OPERND(I,17).LE.234.0D0) THEN
!     CHROMATIC ABERRATIONS
         CALL PRTRB
         SF=INT(sys_last_surf())
         IF(INT(OPERND(I,18)).EQ.0) SF1=OPERND(I,8)
         IF(INT(sys_wl_ref()).GE.1.AND.INT(sys_wl_ref()).LE.5) THEN
            CW=INT(sys_wl_ref())+45
         END IF
         IF(INT(sys_wl_ref()).GE.6.AND.INT(sys_wl_ref()).LE.10) THEN
            CW=INT(sys_wl_ref())+65
         END IF
         INTV=1.0D0
!       CALCULATE INTV
         IF(OPERND(I,17).EQ.228.0D0.OR.OPERND(I,17).EQ.230.0D0.OR.OPERND(I,17).EQ.232.0D0.OR.OPERND(I,17).EQ.234.0D0) THEN
!     PACX,PLCX,SACX OR SLCX
            INTV=((PXTRAX(5,SF)*ALENS(CW,(SF-1))*PXTRAX(2,(SF-1)))-(PXTRAX(1,SF)*ALENS(CW,(SF-1))*PXTRAX(6,(SF-1))))
         END IF
         IF(OPERND(I,17).EQ.227.0D0.OR.OPERND(I,17).EQ.229.0D0.OR.OPERND(I,17).EQ.231.0D0.OR.OPERND(I,17).EQ.233.0D0) THEN
!     PACY,PLCY,SACY OR SLCY
            INTV=((PXTRAY(5,SF)*ALENS(CW,(SF-1))*PXTRAY(2,(SF-1)))-(PXTRAY(1,SF)*ALENS(CW,(SF-1))*PXTRAY(6,(SF-1))))
         END IF
         IF(INTV.EQ.0.0D0) THEN

            IF(F28.EQ.1) REG(9)=0.0D0
            IF(F28.EQ.1) BAAD=1
            IF(F31.EQ.1) CALL MACFAL
            RETURN
         END IF
         IF(INT(OPERND(I,18)).EQ.0) THEN
!     SINGLE SURFACE NOT IMAGE SURFACE
!     DO EACH CALC NOW
            IF(sys_mode().LE.2.0D0) THEN
               IF(OPERND(I,17).EQ.227.0D0) THEN
                  REG(9)=COLORY(1,SF1)/INTV
                  GO TO 777
               END IF
               IF(OPERND(I,17).EQ.229.0D0) THEN
                  REG(9)=COLORY(2,SF1)/INTV
                  GO TO 777
               END IF
               IF(OPERND(I,17).EQ.231.0D0) THEN
                  REG(9)=COLORY(3,SF1)/INTV
                  GO TO 777
               END IF
               IF(OPERND(I,17).EQ.233.0D0) THEN
                  REG(9)=COLORY(4,SF1)/INTV
                  GO TO 777
               END IF
               IF(OPERND(I,17).EQ.228.0D0) THEN
                  REG(9)=COLORX(1,SF1)/INTV
                  GO TO 777
               END IF
               IF(OPERND(I,17).EQ.230.0D0) THEN
                  REG(9)=COLORX(2,SF1)/INTV
                  GO TO 777
               END IF
               IF(OPERND(I,17).EQ.232.0D0) THEN
                  REG(9)=COLORX(3,SF1)/INTV
                  GO TO 777
               END IF
               IF(OPERND(I,17).EQ.234.0D0) THEN
                  REG(9)=COLORX(4,SF1)/INTV
                  GO TO 777
               END IF
            ELSE
!     AFOCAL
               IF(OPERND(I,17).EQ.227.0D0) THEN
                  REG(9)=COLORY(5,SF1)/INTV
                  GO TO 777
               END IF
               IF(OPERND(I,17).EQ.229.0D0) THEN
                  REG(9)=COLORY(6,SF1)/INTV
                  GO TO 777
               END IF
               IF(OPERND(I,17).EQ.231.0D0) THEN
                  REG(9)=COLORY(7,SF1)/INTV
                  GO TO 777
               END IF
               IF(OPERND(I,17).EQ.233.0D0) THEN
                  REG(9)=COLORY(8,SF1)/INTV
                  GO TO 777
               END IF
               IF(OPERND(I,17).EQ.228.0D0) THEN
                  REG(9)=COLORX(5,SF1)/INTV
                  GO TO 777
               END IF
               IF(OPERND(I,17).EQ.230.0D0) THEN
                  REG(9)=COLORX(6,SF1)/INTV
                  GO TO 777
               END IF
               IF(OPERND(I,17).EQ.232.0D0) THEN
                  REG(9)=COLORX(7,SF1)/INTV
                  GO TO 777
               END IF
               IF(OPERND(I,17).EQ.234.0D0) THEN
                  REG(9)=COLORX(8,SF1)/INTV
                  GO TO 777
               END IF
            END IF
         ELSE
!     FINAL SURFACE
            IF(sys_mode().LE.2.0D0) THEN
               IF(OPERND(I,17).EQ.227.0D0) THEN
                  V=0.0D0
                  DO IV=0,INT(sys_last_surf())
                     V=V+COLORY(1,IV)
                  END DO
                  REG(9)=V/INTV
                  GO TO 777
               END IF
               IF(OPERND(I,17).EQ.229.0D0) THEN
                  V=0.0D0
                  DO IV=0,INT(sys_last_surf())
                     V=V+COLORY(2,IV)
                  END DO
                  REG(9)=V/INTV
                  GO TO 777
               END IF
               IF(OPERND(I,17).EQ.231.0D0) THEN
                  V=0.0D0
                  DO IV=0,INT(sys_last_surf())
                     V=V+COLORY(3,IV)
                  END DO
                  REG(9)=V/INTV
                  GO TO 777
               END IF
               IF(OPERND(I,17).EQ.233.0D0) THEN
                  V=0.0D0
                  DO IV=0,INT(sys_last_surf())
                     V=V+COLORY(4,IV)
                  END DO
                  REG(9)=V/INTV
                  GO TO 777
               END IF
               IF(OPERND(I,17).EQ.228.0D0) THEN
                  V=0.0D0
                  DO IV=0,INT(sys_last_surf())
                     V=V+COLORX(1,IV)
                  END DO
                  REG(9)=V/INTV
                  GO TO 777
               END IF
               IF(OPERND(I,17).EQ.230.0D0) THEN
                  V=0.0D0
                  DO IV=0,INT(sys_last_surf())
                     V=V+COLORX(2,IV)
                  END DO
                  REG(9)=V/INTV
                  GO TO 777
               END IF
               IF(OPERND(I,17).EQ.232.0D0) THEN
                  V=0.0D0
                  DO IV=0,INT(sys_last_surf())
                     V=V+COLORX(3,IV)
                  END DO
                  REG(9)=V/INTV
                  GO TO 777
               END IF
               IF(OPERND(I,17).EQ.234.0D0) THEN
                  V=0.0D0
                  DO IV=0,INT(sys_last_surf())
                     V=V+COLORX(4,IV)
                  END DO
                  REG(9)=V/INTV
                  GO TO 777
               END IF
            ELSE
               IF(OPERND(I,17).EQ.227.0D0) THEN
                  V=0.0D0
                  DO IV=0,INT(sys_last_surf())
                     V=V+COLORY(5,IV)
                  END DO
                  REG(9)=V/INTV
                  GO TO 777
               END IF
               IF(OPERND(I,17).EQ.229.0D0) THEN
                  V=0.0D0
                  DO IV=0,INT(sys_last_surf())
                     V=V+COLORY(6,IV)
                  END DO
                  REG(9)=V/INTV
                  GO TO 777
               END IF
               IF(OPERND(I,17).EQ.231.0D0) THEN
                  V=0.0D0
                  DO IV=0,INT(sys_last_surf())
                     V=V+COLORY(7,IV)
                  END DO
                  REG(9)=V/INTV
                  GO TO 777
               END IF
               IF(OPERND(I,17).EQ.233.0D0) THEN
                  V=0.0D0
                  DO IV=0,INT(sys_last_surf())
                     V=V+COLORY(8,IV)
                  END DO
                  REG(9)=V/INTV
                  GO TO 777
               END IF
               IF(OPERND(I,17).EQ.228.0D0) THEN
                  V=0.0D0
                  DO IV=0,INT(sys_last_surf())
                     V=V+COLORX(5,IV)
                  END DO
                  REG(9)=V/INTV
                  GO TO 777
               END IF
               IF(OPERND(I,17).EQ.230.0D0) THEN
                  V=0.0D0
                  DO IV=0,INT(sys_last_surf())
                     V=V+COLORX(6,IV)
                  END DO
                  REG(9)=V/INTV
                  GO TO 777
               END IF
               IF(OPERND(I,17).EQ.232.0D0) THEN
                  V=0.0D0
                  DO IV=0,INT(sys_last_surf())
                     V=V+COLORX(7,IV)
                  END DO
                  REG(9)=V/INTV
                  GO TO 777
               END IF
               IF(OPERND(I,17).EQ.234.0D0) THEN
                  V=0.0D0
                  DO IV=0,INT(sys_last_surf())
                     V=V+COLORX(8,IV)
                  END DO
                  REG(9)=V/INTV
                  GO TO 777
               END IF
            END IF
         END IF
      END IF
!
      IF(OPERND(I,17).EQ.235.0D0) THEN
!       GET IMDISX
         W1A=OPERND(I,8)
         W1B=OPERND(I,8)
         IF(W1B.NE.0.0D0) W1B=W1B-1.0D0
         IF(PXTRAX(2,INT(W1B)).NE.0.0D0) THEN
            REG(9)=((-PXTRAX(1,INT(W1A)))/(PXTRAX(2,INT(W1B))))
         ELSE
            REG(9)=1.0D300
         END IF
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.236.0D0) THEN
!       GET IMDISY
         W1A=OPERND(I,8)
         W1B=OPERND(I,8)
         IF(W1B.NE.0.0D0) W1B=W1B-1.0D0
         IF(PXTRAY(2,INT(W1B)).NE.0.0D0) THEN
            REG(9)=((-PXTRAY(1,INT(W1A)))/(PXTRAY(2,INT(W1B))))
         ELSE
            REG(9)=1.0D300
         END IF
         GO TO 777
      END IF
   END IF
!
   IF(OPERND(I,17).GE.237.0D0.AND.OPERND(I,17).LE.244.0D0.OR.OPERND(I,17).GE.460.0D0.AND.OPERND(I,17).LE.461.0D0.OR.OPERND(I,17).GE.464.0D0.AND.OPERND(I,17).LE.465.0D0 .OR.OPERND(I,17).EQ.513.0D0) THEN
!     SPOT DIAGRAM BASED OPERANDS
!     CALCULATE VALUE AND PLACE IN ACC
!     TEST IF SPOT NEEDS TO BE TRACED
!
      IF(OLDSPD.AND.INT(OPERND(I,8)).EQ.OLDF.AND.OLDW.EQ.INT(OPERND(I,9)).AND.CFNUM.EQ.OLDCFG) THEN
!     DON'T NEED TO TRACE THE SPOT AGAIN, IT ALREDY EXITS
      ELSE
!
!     TRACE FOB AND SPOT THEN GET VALUE
!     TRACE FIELD DESIGNAMTED BY NW3 INT(OPERND(I,8)))
!     DO THE FOB
         SAVE_KDP(1)=SAVEINPT(1)
         OLDLDIF2=LDIF2
         OLDLDIF=LDIF
         IF(OPDIF) THEN
            LDIF2=.TRUE.
            LDIF=.TRUE.
         ELSE
            LDIF2=.FALSE.
            LDIF=.FALSE.
         END IF
         WC='FOB     '
         WQ='        '
         SQ=0
         SST=0
         STI=0
         W1=FIELDY(INT(OPERND(I,8)))
         W2=FIELDX(INT(OPERND(I,8)))
         W3=FIELDZ(INT(OPERND(I,8)))
         W4=FIELDW(INT(OPERND(I,8)))
         W5=0.0D0
         DF1=0
         DF2=0
         DF3=0
         DF4=0
         DF5=1
         S1=1
         S2=1
         S3=1
         S4=1
         S5=0
         SN=1
!     SET MSG TO FALSE
         MSG=.FALSE.
         BADOPS=.FALSE.
         CALL FFOB2
         REST_KDP(1)=RESTINPT(1)
         IF(BADOPS) THEN
            IF(F28.EQ.1) BAAD=1
            IF(F28.EQ.1) REG(9)=0.0D0
            IF(F31.EQ.1) CALL MACFAL
            RETURN
         END IF
! NOW THE SPOT
         IF(OPERND(I,17).NE.464.0D0.AND.OPERND(I,17).NE.465.0D0.AND.OPERND(I,17).NE.460.0D0.AND.OPERND(I,17).NE.461.0D0) THEN
!     NOT GOTF OR RED S
            SAVE_KDP(1)=SAVEINPT(1)
!     SET MSG TO FALSE
            MSG=.FALSE.
            BADOPS=.FALSE.
            IF(INT(OPERND(I,11)).EQ.1) THEN
               OLDIF=LDIF
               LDIF=.FALSE.
               CALL SPOT1(2)
               LDIF=OLDIF
            ELSE
               OLDSP(1) =sys_wl_weight(1)
               OLDSP(2) =sys_wl_weight(2)
               OLDSP(3) =sys_wl_weight(3)
               OLDSP(4) =sys_wl_weight(4)
               OLDSP(5) =sys_wl_weight(5)
               OLDSP(6) =sys_wl_weight(6)
               OLDSP(7) =sys_wl_weight(7)
               OLDSP(8) =sys_wl_weight(8)
               OLDSP(9) =sys_wl_weight(9)
               OLDSP(10)=sys_wl_weight(10)
               SYSTEM(31:35)=0.0D0
               SYSTEM(76:80)=0.0D0
               call set_sys_wl_weight(1,1.0D0)
               call set_sys_wl_weight(2,1.0D0)
               call set_sys_wl_weight(3,1.0D0)
               call set_sys_wl_weight(4,1.0D0)
               call set_sys_wl_weight(5,1.0D0)
               call set_sys_wl_weight(6,1.0D0)
               call set_sys_wl_weight(7,1.0D0)
               call set_sys_wl_weight(8,1.0D0)
               call set_sys_wl_weight(9,1.0D0)
               call set_sys_wl_weight(10,1.0D0)
               OLDIF=LDIF
               LDIF=.FALSE.
               CALL SPOT1(2)
               LDIF=OLDIF
               call set_sys_wl_weight(1,OLDSP(1))
               call set_sys_wl_weight(2,OLDSP(2))
               call set_sys_wl_weight(3,OLDSP(3))
               call set_sys_wl_weight(4,OLDSP(4))
               call set_sys_wl_weight(5,OLDSP(5))
               call set_sys_wl_weight(6,OLDSP(6))
               call set_sys_wl_weight(7,OLDSP(7))
               call set_sys_wl_weight(8,OLDSP(8))
               call set_sys_wl_weight(9,OLDSP(9))
               call set_sys_wl_weight(10,OLDSP(10))
            END IF
            REST_KDP(1)=RESTINPT(1)
         ELSE
!     GOTFS AND REDS
            OLDIF=LDIF
            LDIF=.FALSE.
            CALL SPOT1(2)
            LDIF=OLDIF
         END IF
         IF(BADOPS) THEN
            IF(F28.EQ.1) BAAD=1
            IF(F28.EQ.1) REG(9)=0.0D0
            IF(F31.EQ.1) CALL MACFAL
            RETURN
         END IF
         LDIF2=OLDLDIF2
         LDIF=OLDLDIF
         IF(.NOT.REFEXT.OR..NOT.SPDEXT) THEN
            IF(F28.EQ.1) REG(9)=0.0D0
            IF(F28.EQ.1) BAAD=1
            IF(F31.EQ.1) CALL MACFAL
            RETURN
         END IF
         OLDF=INT(OPERND(I,8))
         OLDW=INT(OPERND(I,9))
         OLDSPD=SPDEXT
         OLDCFG=CFNUM
      END IF
      IF(OPERND(I,17).EQ.237.0D0) THEN
         REG(9)=CENTX
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.238.0D0) THEN
         REG(9)=CENTY
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.239.0D0) THEN
         REG(9)=RMSX
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.240.0D0) THEN
         REG(9)=RMSY
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.513.0D0) THEN
         IF(RMSX.EQ.0.0D0) THEN
            ERROR=1
            REG(9)=0.0D0
         ELSE
            ERROR=0
            REG(9)=RMSY/RMSX
         END IF
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.241.0D0) THEN
         REG(9)=RMS
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.242.0D0) THEN
         REG(9)=RSSX
         GO TO 777
      END IF
!
      IF(OPERND(I,17).EQ.243.0D0) THEN
         REG(9)=RSSY
         GO TO 777
      END IF
!
      IF(OPERND(I,17).EQ.244.0D0) THEN
         REG(9)=RSS
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.464.0D0) THEN
!     RED
         SAVE_KDP(5)=SAVEINPT(5)
         W1=OPERND(I,9)
         DF1=0
         S1=1
         DF2=1
         DF3=1
         DF4=1
         DF5=1
         S2=0
         S3=0
         S4=0
         S5=0
         SQ=1
         WC='REDK'
         WQ='ACC     '
         STI=0
         SST=0
         SN=1
         CALL SPRED
!       CALL ROUTINE THAT CALCULATES THE RED VALUE
         REST_KDP(5)=RESTINPT(5)
         OLDF=INT(OPERND(I,8))
         OLDW=0
         OLDSPD=SPDEXT
         OLDCFG=CFNUM
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.465.0D0) THEN
! REDCEN
!       CALL ROUTINE THAT CALCULATES THE RED CENT
         SAVE_KDP(5)=SAVEINPT(5)
         W1=OPERND(I,9)
         DF1=0
         S1=1
         DF2=1
         DF3=1
         DF4=1
         DF5=1
         S2=0
         S3=0
         S4=0
         S5=0
         SQ=1
         WC='REDK'
         WQ='CACC    '
         STI=0
         SST=0
         SN=1
         CALL SPRED
         REST_KDP(5)=RESTINPT(5)
         OLDF=INT(OPERND(I,8))
         OLDW=0
         OLDSPD=SPDEXT
         OLDCFG=CFNUM
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.460.0D0) THEN
!     GOTF MOD
         SAVE_KDP(5)=SAVEINPT(5)
!       CALL ROUTINE THAT CALCULATES THE GOTF MOD
         WC='GOTF'
         W1=OPERND(I,9)
         WQ='ACC'
         DF1=0
         S1=1
         W2=OPERND(I,10)
         DF2=0
         DF3=1
         DF4=1
         DF5=1
         S2=1
         S3=0
         S4=0
         S5=0
         SQ=1
         STI=0
         SST=0
         SN=1
         CALL GOTF
         REST_KDP(5)=RESTINPT(5)
         OLDF=INT(OPERND(I,8))
         OLDW=0
         OLDSPD=SPDEXT
         OLDCFG=CFNUM
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.461.0D0) THEN
! GOTF P
         SAVE_KDP(5)=SAVEINPT(5)
!       CALL ROUTINE THAT CALCULATES THE GOTF PHASE
         WC='GOTF'
         W1=OPERND(I,9)
         WQ='ACC'
         DF1=0
         S1=1
         W2=OPERND(I,10)
         DF2=0
         DF3=1
         DF4=1
         DF5=1
         S2=1
         S3=0
         S4=0
         S5=0
         SQ=1
         STI=0
         SST=0
         SN=1
         CALL GOTF
         REST_KDP(5)=RESTINPT(5)
         OLDF=INT(OPERND(I,8))
         OLDW=0
         OLDSPD=SPDEXT
         OLDCFG=CFNUM
         REG(9)=REG(10)
         GO TO 777
      END IF
   END IF
!
   IF(OPERND(I,17).EQ.484.0D0) THEN
!     GREYS OPERAND GREYSPOT DIAGRAM BASED OPERANDS
!     CALCULATE VALUE AND PLACE IN ACC
!
!     TRACE FOB AND SPOT THEN GET VALUE
!     TRACE FIELD DESIGNAMTED BY NW3 INT(OPERND(I,8)))
!     DO THE FOB
      SAVE_KDP(1)=SAVEINPT(1)
      OLDLDIF2=LDIF2
      OLDLDIF=LDIF
      IF(OPDIF) THEN
         LDIF2=.TRUE.
         LDIF=.TRUE.
      ELSE
         LDIF2=.FALSE.
         LDIF=.FALSE.
      END IF
      WC='FOB     '
      WQ='        '
      SQ=0
      SST=0
      STI=0
      W1=FIELDY(INT(OPERND(I,8)))
      W2=FIELDX(INT(OPERND(I,8)))
      W3=FIELDZ(INT(OPERND(I,8)))
!     USE WAVELENGTH FROM GREYS INPUT, NOT FROM FIELD POS INPUT
      W4=INT(OPERND(I,10))
      W5=0.0D0
      DF1=0
      DF2=0
      DF3=0
      DF4=0
      DF5=1
      S1=1
      S2=1
      S3=1
      S4=1
      S5=0
      SN=1
!     SET MSG TO FALSE
      MSG=.FALSE.
      BADOPS=.FALSE.
      CALL FFOB2
      REST_KDP(1)=RESTINPT(1)
      IF(BADOPS) THEN
         IF(F28.EQ.1) BAAD=1
         IF(F28.EQ.1) REG(9)=0.0D0
         IF(F31.EQ.1) CALL MACFAL
         RETURN
      END IF
! NOW THE GREYSPOT
      SAVE_KDP(1)=SAVEINPT(1)
      OLDLAMM=INT(OPERND(I,10))
!     OPD WEIGHT IS JUST OPERND(I,9)
      OPDWT=OPERND(I,9)
      OLDLDIF2=LDIF2
      OLDLDIF=LDIF
      IF(OPDIF) THEN
         LDIF2=.TRUE.
         LDIF=.TRUE.
      ELSE
         LDIF2=.FALSE.
         LDIF=.FALSE.
      END IF
!     SET MSG TO FALSE
      MSG=.FALSE.
      BADOPS=.FALSE.
      CALL GSPOT
      REST_KDP(1)=RESTINPT(1)
      IF(BADOPS) THEN
         IF(F28.EQ.1) BAAD=1
         IF(F28.EQ.1) REG(9)=0.0D0
         IF(F31.EQ.1) CALL MACFAL
         RETURN
      END IF
      LDIF2=OLDLDIF2
      LDIF=OLDLDIF
      IF(.NOT.REFEXT.OR..NOT.GSPDEXT) THEN
         IF(F28.EQ.1) REG(9)=0.0D0
         IF(F28.EQ.1) BAAD=1
         IF(F31.EQ.1) CALL MACFAL
         RETURN
      END IF
      OLDF=INT(OPERND(I,8))
      OLDW=0
      OLDGSPD=GSPDEXT
      OLDCFG=CFNUM
      REG(9)=GREYOP
      GO TO 777
   END IF
!
   IF(OPERND(I,17).GE.245.0D0.AND.OPERND(I,17).LE.246.0D0.OR.OPERND(I,17).GE.462.0D0.AND.OPERND(I,17).LE.463.0D0) THEN
!     CAPFN BASED OPERANDS
!     CALCULATE VALUE AND PLACE IN ACC
!
!     TEST IF SPOT NEEDS TO BE TRACED
!
      IF(OLDCPFN.AND.INT(OPERND(I,8)).EQ.OLDF.AND.CFNUM.EQ.OLDCFG) THEN
!     DON'T NEED TO TRACE THE CAPFN AGAIN, IT ALREDY EXITS
      ELSE
!     TRACE FOB AND SPOT THEN GET VALUE
!     TRACE FIELD DESIGNAMTED BY NW3 INT(OPERND(I,8)))
!     DO THE FOB
         SAVE_KDP(1)=SAVEINPT(1)
         OLDLDIF2=LDIF2
         OLDLDIF=LDIF
         IF(OPDIF) THEN
            LDIF2=.TRUE.
            LDIF=.TRUE.
         ELSE
            LDIF2=.FALSE.
            LDIF=.FALSE.
         END IF
         WC='FOB     '
         WQ='        '
         SQ=0
         SST=0
         STI=0
         W1=FIELDY(INT(OPERND(I,8)))
         W2=FIELDX(INT(OPERND(I,8)))
         W3=FIELDZ(INT(OPERND(I,8)))
         W4=FIELDW(INT(OPERND(I,8)))
         W5=0.0D0
         DF1=0
         DF2=0
         DF3=0
         DF4=0
         DF5=1
         S1=1
         S2=1
         S3=1
         S4=1
         S5=0
         SN=1
!     SET MSG TO FALSE
         MSG=.FALSE.
         BADOPS=.FALSE.
         CALL FFOB2
         REST_KDP(1)=RESTINPT(1)
         IF(BADOPS) THEN
            IF(F28.EQ.1) BAAD=1
            IF(F28.EQ.1) REG(9)=0.0D0
            IF(F31.EQ.1) CALL MACFAL
            RETURN
         END IF
! NOW THE SPOT
         SAVE_KDP(1)=SAVEINPT(1)
!     SET MSG TO FALSE
         MSG=.FALSE.
         BADOPS=.FALSE.
         NRDFACTOR=1.0D0
         IF(OPERND(I,17).EQ.245.0D0) THEN
            IF(INT(OPERND(I,11)).EQ.1) THEN
               OLDIF=LDIF
               LDIF=.FALSE.
               CALL COMPAP(REFERR,2)
               LDIF=OLDIF
            ELSE
               OLDSP(1) =sys_wl_weight(1)
               OLDSP(2) =sys_wl_weight(2)
               OLDSP(3) =sys_wl_weight(3)
               OLDSP(4) =sys_wl_weight(4)
               OLDSP(5) =sys_wl_weight(5)
               OLDSP(6) =sys_wl_weight(6)
               OLDSP(7) =sys_wl_weight(7)
               OLDSP(8) =sys_wl_weight(8)
               OLDSP(9) =sys_wl_weight(9)
               OLDSP(10)=sys_wl_weight(10)
               SYSTEM(31:35)=0.0D0
               SYSTEM(76:80)=0.0D0
               call set_sys_wl_weight(1,1.0D0)
               call set_sys_wl_weight(2,1.0D0)
               call set_sys_wl_weight(3,1.0D0)
               call set_sys_wl_weight(4,1.0D0)
               call set_sys_wl_weight(5,1.0D0)
               call set_sys_wl_weight(6,1.0D0)
               call set_sys_wl_weight(7,1.0D0)
               call set_sys_wl_weight(8,1.0D0)
               call set_sys_wl_weight(9,1.0D0)
               call set_sys_wl_weight(10,1.0D0)
               OLDIF=LDIF
               LDIF=.FALSE.
               CALL COMPAP(REFERR,2)
               LDIF=OLDIF
               call set_sys_wl_weight(1,OLDSP(1))
               call set_sys_wl_weight(2,OLDSP(2))
               call set_sys_wl_weight(3,OLDSP(3))
               call set_sys_wl_weight(4,OLDSP(4))
               call set_sys_wl_weight(5,OLDSP(5))
               call set_sys_wl_weight(6,OLDSP(6))
               call set_sys_wl_weight(7,OLDSP(7))
               call set_sys_wl_weight(8,OLDSP(8))
               call set_sys_wl_weight(9,OLDSP(9))
               call set_sys_wl_weight(10,OLDSP(10))
            END IF
         ELSE
            OLDIF=LDIF
            LDIF=.FALSE.
            CALL COMPAP(REFERR,2)
            LDIF=OLDIF
         END IF
         REST_KDP(1)=RESTINPT(1)
         IF(BADOPS) THEN
            IF(F28.EQ.1) BAAD=1
            IF(F28.EQ.1) REG(9)=0.0D0
            IF(F31.EQ.1) CALL MACFAL
            RETURN
         END IF
         LDIF2=OLDLDIF2
         LDIF=OLDLDIF
         IF(.NOT.REFEXT.OR..NOT.CPFNEXT.OR.REFERR) THEN
            IF(F28.EQ.1) REG(9)=0.0D0
            IF(F28.EQ.1) BAAD=1
            IF(F31.EQ.1) CALL MACFAL
            RETURN
         END IF
         OLDF=INT(OPERND(I,8))
         OLDW=0
         OLDCPFN=CPFNEXT
         OLDCFG=CFNUM
      END IF
      IF(OPERND(I,17).EQ.245.0D0) THEN
         REG(9)=RMSOPD
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.246.0D0) THEN
!     DO A FIT UNLESS ONE ALREADY EXISTS THEN GET OP VAL
         ERROP=.FALSE.
         OPMAP=.FALSE.

         WVNUMOP=INT(OPERND(I,10))
         CALL OPDLOD2
         ERRR=ERROP
         IF(ERRR) THEN
            IF(F28.EQ.1) REG(9)=0.0D0
            IF(F28.EQ.1) BAAD=1
            IF(F31.EQ.1) CALL MACFAL
            RETURN
         END IF
!     FIT EXITS NOW EVAL THE OP
         IF(.NOT.OPMAP) ERRR=.TRUE.
         IF(OPMAP) ERRR=.FALSE.
         IF(.NOT.OPMAP) OLDZRNFT=.FALSE.
         IF(OPMAP) OLDZRNFT=.TRUE.
         IF(ERRR) THEN
            IF(F28.EQ.1) REG(9)=0.0D0
            IF(F28.EQ.1) BAAD=1
            IF(F31.EQ.1) CALL MACFAL
            RETURN
         END IF
         REG(9)=X(INT(OPERND(I,9)))
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.462.0D0) THEN
!     DOTF MOD
         SAVE_KDP(5)=SAVEINPT(5)
!       CALL ROUTINE THAT CALCULATES THE DOTF MOD
         WC='DOTF'
         W1=OPERND(I,9)
         DF1=0
         S1=1
         DF2=1
         DF3=1
         DF4=1
         DF5=1
         S2=0
         S3=0
         S4=0
         S5=0
         SQ=1
         IF(OPERND(I,10).EQ.90.0D0) WQ='YACC    '
         IF(OPERND(I,10).EQ.0.0D0)  WQ='XACC    '
         STI=0
         SST=0
         SN=1
         CALL DOTF
         REST_KDP(5)=RESTINPT(5)
         OLDF=INT(OPERND(I,8))
         OLDW=0
         OLDCSPD=CPFNEXT
         OLDCFG=CFNUM
         GO TO 777
      END IF
      IF(OPERND(I,17).EQ.463.0D0) THEN
! DOTF P
         SAVE_KDP(5)=SAVEINPT(5)
!       CALL ROUTINE THAT CALCULATES THE DOTF PHASE
         WC='DOTF'
         W1=OPERND(I,9)
         DF1=0
         S1=1
         DF2=1
         DF3=1
         DF4=1
         DF5=1
         S2=0
         S3=0
         S4=0
         S5=0
         SQ=1
         IF(OPERND(I,10).EQ.90.0D0) WQ='YACC    '
         IF(OPERND(I,10).EQ.0.0D0)  WQ='XACC    '
         STI=0
         SST=0
         SN=1
         CALL DOTF
         REST_KDP(5)=RESTINPT(5)
         OLDF=INT(OPERND(I,8))
         OLDW=0
         OLDCSPD=CPFNEXT
         OLDCFG=CFNUM
         REG(9)=REG(10)
         GO TO 777
      END IF
   END IF
!
!     SPECIAL OPERANDS
   IF(OPERND(I,17).GE.247.0D0.AND.OPERND(I,17).LE.296.0D0.OR.OPERND(I,17).GE.452.0D0.AND.OPERND(I,17).LE.465.0D0) THEN
!     CALCULATE VALUE AND PLACE IN ACC
      CALL PRTRB
      CALL PRTRC
      CALL PRTRD
!
!     FIRST DO OPERANDS WHICH ARE PARAXIAL
!     THESE ARE NUMBER 279 TO 284
      IF(OPERND(I,17).GE.279.0D0.AND.OPERND(I,17).LE.284.0D0) THEN
!     PARAXIAL
!
!       PUPDIAX
         IF(OPERND(I,17).EQ.279.0D0) THEN
            W1A=OPERND(I,8)
            W1B=OPERND(I,8)
            IF(W1B.NE.0.0D0) W1B=W1B-1.0D0
            V1=((-PXTRAX(5,INT(W1A)))/(PXTRAX(6,INT(W1B))))
            REG(9)=2.0D0*(V1*PXTRAX(2,INT(W1B)))+PXTRAX(1,INT(W1A))
            GO TO 777
         END IF
!       PUPDIAY
         IF(OPERND(I,17).EQ.280.0D0) THEN
            W1A=OPERND(I,8)
            W1B=OPERND(I,8)
            IF(W1B.NE.0.0D0) W1B=W1B-1.0D0
            V1=((-PXTRAY(5,INT(W1A)))/(PXTRAY(6,INT(W1B))))
            REG(9)=2.0D0*(V1*PXTRAY(2,INT(W1B)))+PXTRAY(1,INT(W1A))
            GO TO 777
         END IF
!
         IF(OPERND(I,17).EQ.281.0D0) THEN
!     PUPDISX
            W1A=OPERND(I,8)
            W1B=OPERND(I,8)
            IF(W1B.NE.0.0D0) W1B=W1B-1.0D0
            REG(9)=((-PXTRAX(5,INT(W1A)))/(PXTRAX(6,INT(W1B))))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.282.0D0) THEN
!     PUPDISY
            W1A=OPERND(I,8)
            W1B=OPERND(I,8)
            IF(W1B.NE.0.0D0) W1B=W1B-1.0D0
            REG(9)=((-PXTRAY(5,INT(W1A)))/(PXTRAY(6,INT(W1B))))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.283.0D0) THEN
!     CHFIMX
            W1A=OPERND(I,8)
            W1B=OPERND(I,8)
            IF(W1B.NE.0.0D0) W1B=W1B-1.0D0
            V1=((-PXTRAX(1,INT(W1A)))/(PXTRAX(2,INT(W1B))))
            REG(9)=(V1*PXTRAX(6,INT(W1B)))+PXTRAX(5,INT(W1A))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.284.0D0) THEN
!     CHFIMY
            W1A=OPERND(I,8)
            W1B=OPERND(I,8)
            IF(W1B.NE.0.0D0) W1B=W1B-1.0D0
            V1=((-PXTRAY(1,INT(W1A)))/(PXTRAY(2,INT(W1B))))
            REG(9)=(V1*PXTRAY(6,INT(W1B)))+PXTRAY(5,INT(W1A))
            GO TO 777
         END IF
!
      END IF
!     NEXT DO OPERANDS WHICH NOT PARAXIAL
      IF(OPERND(I,17).GE.247.0D0.AND.OPERND(I,17).LE.278.0D0.OR.OPERND(I,17).GE.285.0D0.AND.OPERND(I,17).LE.296.0D0.OR.OPERND(I,17).GE.452.0D0.AND.OPERND(I,17).LE.465.0D0) THEN
!     NON-PARAXIAL STUFF
!     TEST IF FOB AND RAY O O NEED TO BE TRACED
!
         IF(OPERND(I,17).GE.247.0D0.AND.OPERND(I,17).LE.278.0D0.AND.REFEXT.AND.RAYEXT.AND.INT(OPERND(I,8)).EQ.OLDF.AND.CFNUM.EQ.OLDCFG.OR.OPERND(I,17).GE.293.0D0.AND.OPERND(I,17).LE.296.0D0.AND.REFEXT.AND.RAYEXT.AND.INT(OPERND(I,8)).EQ.OLDF.AND.CFNUM.EQ.OLDCFG.OR.OPERND(I,17).GE.452.0D0.AND.OPERND(I,17).LE.459.0D0.AND.REFEXT.AND.RAYEXT.AND.INT(OPERND(I,8)).EQ.OLDF.AND.CFNUM.EQ.OLDCFG.OR.OPERND(I,17).EQ.466.0D0.AND.REFEXT.AND.RAYEXT.AND.INT(OPERND(I,8)).EQ.OLDF.AND.CFNUM.EQ.OLDCFG.OR.OPERND(I,17).GE.285.0D0.AND.OPERND(I,17).LE.292.0D0.AND.REFEXT.AND.RAYEXT.AND.INT(OPERND(I,9)).EQ.OLDF.AND.CFNUM.EQ.OLDCFG) THEN
!
!     DON'T NEED TO TRACE THE RAY AGAIN, IT ALREDY EXITS
         ELSE
!     TRACE FOB AND A RAY 0 0 1 THEN GET VALUE
!     TRACE FIELD DESIGNAMTED BY NW3 INT(OPERND(I,8)))
!     OR NW4 INT(OPERND(I,9))
!     TRACE RAY 0 0
!     FOR REFERENCE RAY STUFF, RAY 0 0 IS ALWAYS TRACED
!     DO THE FOB
            SAVE_KDP(1)=SAVEINPT(1)
            OLDLDIF2=LDIF2
            OLDLDIF=LDIF
            IF(OPDIF) THEN
               LDIF2=.TRUE.
               LDIF=.TRUE.
            ELSE
               LDIF2=.FALSE.
               LDIF=.FALSE.
            END IF
            WC='FOB     '
            WQ='        '
            SQ=0
            SST=0
            STI=0
            IF(OPERND(I,17).GE.247.0D0.AND.OPERND(I,17).LE.278.0D0.OR.OPERND(I,17).GE.293.AND.OPERND(I,17).LE.296.0D0.OR.OPERND(I,17).EQ.466.0D0) THEN
               W1=FIELDY(INT(OPERND(I,8)))
               W2=FIELDX(INT(OPERND(I,8)))
               W3=FIELDZ(INT(OPERND(I,8)))
               W4=FIELDW(INT(OPERND(I,8)))
            END IF
            IF(OPERND(I,17).GE.285.0D0.AND.OPERND(I,17).LE.292.0D0.OR.OPERND(I,17).GE.452.0D0.AND.OPERND(I,17).LE.465.0D0) THEN
               W1=FIELDY(INT(OPERND(I,9)))
               W2=FIELDX(INT(OPERND(I,9)))
               W3=FIELDZ(INT(OPERND(I,9)))
               W4=FIELDW(INT(OPERND(I,9)))
            END IF
            W5=0.0D0
            DF1=0
            DF2=0
            DF3=0
            DF4=0
            DF5=1
            S1=1
            S2=1
            S3=1
            S4=1
            S5=0
            SN=1
!     SET MSG TO FALSE
            MSG=.FALSE.
            BADOPS=.FALSE.
            CALL FFOB2
            REST_KDP(1)=RESTINPT(1)
            IF(BADOPS) THEN
               IF(F28.EQ.1) BAAD=1
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
! NOW THE RAY 0 0
            SAVE_KDP(1)=SAVEINPT(1)
            WC='RAY     '
            WQ='        '
            SQ=0
            SST=0
            DF1=1
            DF2=1
            DF3=1
            DF4=1
            DF5=1
            S1=0
            S2=0
            S3=0
            S4=0
            S5=0
            SN=0
!     SET MSG TO FALSE
            MSG=.FALSE.
            BADOPS=.FALSE.
            NOCOAT=.TRUE.
            GRASET=.FALSE.
            DXFSET=.FALSE.
            CALL RRAY2
            CACOCH=0
            REST_KDP(1)=RESTINPT(1)
            IF(BADOPS) THEN
               IF(F28.EQ.1) BAAD=1
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            LDIF2=OLDLDIF2
            LDIF=OLDLDIF
            IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
!     NOW CALCULATE PUPIL STUFF
!     NOW CALL AUXFOB TO CALCULATE DIFFERENTIAL RAY BASED FFL,BFL AND MAGS
            CALL LASTRAY(1)
            CALL AUXFOB(ERRFOB)
            CALL LASTRAY(2)
!
            IF(OPERND(I,17).GE.247.0D0.AND.OPERND(I,17).LE.278.0D0.OR.OPERND(I,17).GE.293.AND.OPERND(I,17).LE.296.0D0.OR.OPERND(I,17).EQ.466.0D0) THEN
               OLDF=INT(OPERND(I,8))
               OLDW=0
            END IF
            IF(OPERND(I,17).GE.285.0D0.AND.OPERND(I,17).LE.292.0D0.OR.OPERND(I,17).GE.452.0D0.AND.OPERND(I,17).LE.465.0D0) THEN
               OLDF=INT(OPERND(I,9))
               OLDW=0
               OLDW=0
            END IF
            OLDCFG=CFNUM
         END IF
!
!     NOW CALC THE OPS
         IF(OPERND(I,17).EQ.247.0D0) THEN
            REG(9)=RMAGX
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.248.0D0) THEN
            REG(9)=RMAGY
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.249.0D0) THEN
            REG(9)=MAGXOR
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.250.0D0) THEN
            REG(9)=MAGYOR
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.251.0D0) THEN
            REG(9)=RFFLX
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.252.0D0) THEN
            REG(9)=RFFLY
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.253.0D0) THEN
            REG(9)=RBFLX
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.254.0D0) THEN
            REG(9)=RBFLY
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.255.0D0) THEN
            REG(9)=RFFNX
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.256.0D0) THEN
            REG(9)=RFFNY
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.257.0D0) THEN
            REG(9)=RBFNX
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.258.0D0) THEN
            REG(9)=RBFNY
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.259.0D0) THEN
            REG(9)=REFLX
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.260.0D0) THEN
            REG(9)=REFLY
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.261.0D0) THEN
            REG(9)=ENDIAX
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.262.0D0) THEN
            REG(9)=ENDIAY
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.263.0D0) THEN
            REG(9)=EXDIAX
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.264.0D0) THEN
            REG(9)=EXDIAY
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.265.0D0) THEN
            REG(9)=ENPUX
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.266.0D0) THEN
            REG(9)=ENPUY
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.267.0D0) THEN
            REG(9)=ENPUZ
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.268.0D0) THEN
            REG(9)=EXPUX
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.269.0D0) THEN
            REG(9)=EXPUY
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.270.0D0) THEN
            REG(9)=EXPUZ
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.271.0D0) THEN
!     FNUMX
            ERRR=.FALSE.
            MSG=.FALSE.
            CACOCH=0
            CALL FNUMX(V1,ERRR)
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            REG(9)=V1
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.272.0D0) THEN
!     FNUMY
            ERRR=.FALSE.
            MSG=.FALSE.
            CACOCH=0
            CALL FNUMY(V1,ERRR)
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            REG(9)=V1
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.273.0D0) THEN
!     OBFNUMX
            ERRR=.FALSE.
            MSG=.FALSE.
            CACOCH=0
            CALL OBFNUMX(V1,ERRR)
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            REG(9)=V1
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.274.0D0) THEN
!     OBFNUMY
            ERRR=.FALSE.
            MSG=.FALSE.
            CACOCH=0
            CALL OBFNUMY(V1,ERRR)
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            REG(9)=V1
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.275.0D0) THEN
!     ENPDIAX
            ERRR=.FALSE.
            MSG=.FALSE.
            CACOCH=0
            CALL ENPDIAX(V1,ERRR)
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            REG(9)=V1
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.276.0D0) THEN
!     ENPDIAY
            ERRR=.FALSE.
            MSG=.FALSE.
            CACOCH=0
            CALL ENPDIAY(V1,ERRR)
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            REG(9)=V1
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.277.0D0) THEN
!     EXPDIAX
            ERRR=.FALSE.
            MSG=.FALSE.
            CACOCH=0
            CALL EXPDIAX(V1,ERRR)
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            REG(9)=V1
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.278.0D0) THEN
!     EXPDIAY
            ERRR=.FALSE.
            MSG=.FALSE.
            CACOCH=0
            CALL EXPDIAY(V1,ERRR)
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            REG(9)=V1
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.285.0D0) THEN
!     GPX
            ERRR=.FALSE.
            NWN1=FIELDY(INT(OPERND(I,9)))
            NWN2=FIELDX(INT(OPERND(I,9)))
            NWN3=FIELDZ(INT(OPERND(I,9)))
            NWN4=FIELDW(INT(OPERND(I,9)))
            CALL GNPR2(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,0)
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            REG(9)=VALUE
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.286.0D0) THEN
!     GPY
            ERRR=.FALSE.
            NWN1=FIELDY(INT(OPERND(I,9)))
            NWN2=FIELDX(INT(OPERND(I,9)))
            NWN3=FIELDZ(INT(OPERND(I,9)))
            NWN4=FIELDW(INT(OPERND(I,9)))
            CALL GNPR1(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,0)
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            REG(9)=VALUE
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.287.0D0) THEN
!     GPUX
            ERRR=.FALSE.
            NWN1=FIELDY(INT(OPERND(I,9)))
            NWN2=FIELDX(INT(OPERND(I,9)))
            NWN3=FIELDZ(INT(OPERND(I,9)))
            NWN4=FIELDW(INT(OPERND(I,9)))
            CALL GNPR4(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,0)
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            REG(9)=VALUE
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.288.0D0) THEN
!     GPUY
            ERRR=.FALSE.
            NWN1=FIELDY(INT(OPERND(I,9)))
            NWN2=FIELDX(INT(OPERND(I,9)))
            NWN3=FIELDZ(INT(OPERND(I,9)))
            NWN4=FIELDW(INT(OPERND(I,9)))
            CALL GNPR3(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,0)
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            REG(9)=VALUE
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.289.0D0) THEN
!     GPCX
            ERRR=.FALSE.
            NWN1=FIELDY(INT(OPERND(I,9)))
            NWN2=FIELDX(INT(OPERND(I,9)))
            NWN3=FIELDZ(INT(OPERND(I,9)))
            NWN4=FIELDW(INT(OPERND(I,9)))
            CALL GNPR6(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,0)
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            REG(9)=VALUE
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.290.0D0) THEN
!     GPCY
            ERRR=.FALSE.
            NWN1=FIELDY(INT(OPERND(I,9)))
            NWN2=FIELDX(INT(OPERND(I,9)))
            NWN3=FIELDZ(INT(OPERND(I,9)))
            NWN4=FIELDW(INT(OPERND(I,9)))
            CALL GNPR5(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,0)
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            REG(9)=VALUE
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.291.0D0) THEN
!     GPUCX
            ERRR=.FALSE.
            NWN1=FIELDY(INT(OPERND(I,9)))
            NWN2=FIELDX(INT(OPERND(I,9)))
            NWN3=FIELDZ(INT(OPERND(I,9)))
            NWN4=FIELDW(INT(OPERND(I,9)))
            CALL GNPR8(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,0)
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            REG(9)=VALUE
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.292.0D0) THEN
!     GPUCY
            ERRR=.FALSE.
            NWN1=FIELDY(INT(OPERND(I,9)))
            NWN2=FIELDX(INT(OPERND(I,9)))
            NWN3=FIELDZ(INT(OPERND(I,9)))
            NWN4=FIELDW(INT(OPERND(I,9)))
            CALL GNPR7(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,0)
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            REG(9)=VALUE
            GO TO 777
         END IF
!     NOW THE GAUSSIAN BEAM OPERANDS
         IF(OPERND(I,17).EQ.452.0D0) THEN
!     GBRADX
            OLDOBJ=NEWOBJ
            OLDIMG=NEWIMG
            OLDREF=NEWREF
            NEWREF=1
            NEWIMG=sys_last_surf()
            NEWOBJ=0
            ERRR=.FALSE.
            NWN1=FIELDY(INT(OPERND(I,9)))
            NWN2=FIELDX(INT(OPERND(I,9)))
            NWN3=FIELDZ(INT(OPERND(I,9)))
            NWN4=FIELDW(INT(OPERND(I,9)))
            CALL GNPR2(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
            JPX=VALUE
            CALL GNPR6(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
            JPCX=VALUE
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            REG(9)=DSQRT((JPX**2)+(JPCX**2))
            NEWREF=OLDREF
            NEWIMG=OLDIMG
            NEWOBJ=OLDOBJ
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.453.0D0) THEN
!     GBRADY
            OLDOBJ=NEWOBJ
            OLDIMG=NEWIMG
            OLDREF=NEWREF
            NEWREF=1
            NEWIMG=sys_last_surf()
            NEWOBJ=0
            ERRR=.FALSE.
            NWN1=FIELDY(INT(OPERND(I,9)))
            NWN2=FIELDX(INT(OPERND(I,9)))
            NWN3=FIELDZ(INT(OPERND(I,9)))
            NWN4=FIELDW(INT(OPERND(I,9)))
            CALL GNPR1(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
            JPY=VALUE
            CALL GNPR5(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
            JPCY=VALUE
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            REG(9)=DSQRT((JPY**2)+(JPCY**2))
            NEWREF=OLDREF
            NEWIMG=OLDIMG
            NEWOBJ=OLDOBJ
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.454.0D0) THEN
!     GBDISX
            OLDOBJ=NEWOBJ
            OLDIMG=NEWIMG
            OLDREF=NEWREF
            NEWREF=1
            NEWIMG=sys_last_surf()
            NEWOBJ=0
            ERRR=.FALSE.
            NWN1=FIELDY(INT(OPERND(I,9)))
            NWN2=FIELDX(INT(OPERND(I,9)))
            NWN3=FIELDZ(INT(OPERND(I,9)))
            NWN4=FIELDW(INT(OPERND(I,9)))
            CALL GNPR2(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
            JPX=VALUE
            CALL GNPR4(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
            JPUX=VALUE
            CALL GNPR6(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
            JPCX=VALUE
            CALL GNPR8(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
            JPUCX=VALUE
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            IF(((JPUX**2)+(JPUCX**2)).EQ.0.0D0) THEN
               NEWREF=OLDREF
               NEWIMG=OLDIMG
               NEWOBJ=OLDOBJ
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            ELSE
               REG(9)=-((JPX*JPUX)+(JPCX*JPUCX))/((JPUX**2)+(JPUCX**2))
            END IF
            NEWREF=OLDREF
            NEWIMG=OLDIMG
            NEWOBJ=OLDOBJ
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.455.0D0) THEN
!     GBDISY
            OLDOBJ=NEWOBJ
            OLDIMG=NEWIMG
            OLDREF=NEWREF
            NEWREF=1
            NEWIMG=sys_last_surf()
            NEWOBJ=0
            ERRR=.FALSE.
            NWN1=FIELDY(INT(OPERND(I,9)))
            NWN2=FIELDX(INT(OPERND(I,9)))
            NWN3=FIELDZ(INT(OPERND(I,9)))
            NWN4=FIELDW(INT(OPERND(I,9)))
            CALL GNPR1(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
            JPY=VALUE
            CALL GNPR3(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
            JPUY=VALUE
            CALL GNPR5(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
            JPCY=VALUE
            CALL GNPR7(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
            JPUCY=VALUE
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            IF(((JPUY**2)+(JPUCY**2)).EQ.0.0D0) THEN
               NEWREF=OLDREF
               NEWIMG=OLDIMG
               NEWOBJ=OLDOBJ
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            ELSE
               REG(9)=-((JPY*JPUY)+(JPCY*JPUCY))/((JPUY**2)+(JPUCY**2))
            END IF
            NEWREF=OLDREF
            NEWIMG=OLDIMG
            NEWOBJ=OLDOBJ
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.456.0D0) THEN
!     GBRCVX
            OLDOBJ=NEWOBJ
            OLDIMG=NEWIMG
            OLDREF=NEWREF
            NEWREF=1
            NEWIMG=sys_last_surf()
            NEWOBJ=0
            ERRR=.FALSE.
            NWN1=FIELDY(INT(OPERND(I,9)))
            NWN2=FIELDX(INT(OPERND(I,9)))
            NWN3=FIELDZ(INT(OPERND(I,9)))
            NWN4=FIELDW(INT(OPERND(I,9)))
            CALL GNPR2(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
            JPX=VALUE
            CALL GNPR4(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
            JPUX=VALUE
            CALL GNPR6(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
            JPCX=VALUE
            CALL GNPR8(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
            JPUCX=VALUE
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            IF(((JPX**2)+(JPCX**2)).EQ.0.0D0) THEN
               NEWREF=OLDREF
               NEWIMG=OLDIMG
               NEWOBJ=OLDOBJ
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            V1=((JPX*JPUX)+(JPCX*JPUCX))/DSQRT((JPX**2)+(JPCX**2))
            IF((V1).EQ.0.0D0) THEN
               NEWREF=OLDREF
               NEWIMG=OLDIMG
               NEWOBJ=OLDOBJ
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            REG(9)=-DSQRT((JPX**2)+(JPCX**2))/V1
            NEWREF=OLDREF
            NEWIMG=OLDIMG
            NEWOBJ=OLDOBJ
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.457.0D0) THEN
!     GBRCVY
            OLDOBJ=NEWOBJ
            OLDIMG=NEWIMG
            OLDREF=NEWREF
            NEWREF=1
            NEWIMG=sys_last_surf()
            NEWOBJ=0
            ERRR=.FALSE.
            NWN1=FIELDY(INT(OPERND(I,9)))
            NWN2=FIELDX(INT(OPERND(I,9)))
            NWN3=FIELDZ(INT(OPERND(I,9)))
            NWN4=FIELDW(INT(OPERND(I,9)))
            CALL GNPR1(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
            JPY=VALUE
            CALL GNPR3(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
            JPUY=VALUE
            CALL GNPR5(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
            JPCY=VALUE
            CALL GNPR7(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
            JPUCY=VALUE
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            IF(((JPY**2)+(JPCY**2)).EQ.0.0D0) THEN
               NEWREF=OLDREF
               NEWIMG=OLDIMG
               NEWOBJ=OLDOBJ
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            V1=((JPY*JPUY)+(JPCY*JPUCY))/DSQRT((JPY**2)+(JPCY**2))
            IF((V1).EQ.0.0D0) THEN
               NEWREF=OLDREF
               NEWIMG=OLDIMG
               NEWOBJ=OLDOBJ
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            REG(9)=-DSQRT((JPY**2)+(JPCY**2))/V1
            NEWREF=OLDREF
            NEWIMG=OLDIMG
            NEWOBJ=OLDOBJ
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.458.0D0) THEN
!     GWAISTX
            OLDOBJ=NEWOBJ
            OLDIMG=NEWIMG
            OLDREF=NEWREF
            NEWREF=1
            NEWIMG=sys_last_surf()
            NEWOBJ=0
            ERRR=.FALSE.
            NWN1=FIELDY(INT(OPERND(I,9)))
            NWN2=FIELDX(INT(OPERND(I,9)))
            NWN3=FIELDZ(INT(OPERND(I,9)))
            NWN4=FIELDW(INT(OPERND(I,9)))
            CALL GNPR2(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
            JPX=VALUE
            CALL GNPR4(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
            JPUX=VALUE
            CALL GNPR6(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
            JPCX=VALUE
            CALL GNPR8(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
            JPUCX=VALUE
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            IF(((JPUX**2)+(JPUCX**2)).EQ.0.0D0) THEN
               NEWREF=OLDREF
               NEWIMG=OLDIMG
               NEWOBJ=OLDOBJ
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            V1=-((JPX*JPUX)+(JPCX*JPUCX))/((JPUX**2)+(JPUCX**2))
            REG(9)=DSQRT(((JPX+(JPUX*V1))**2)+((JPCX+(JPUCX*V1))**2))
            NEWREF=OLDREF
            NEWIMG=OLDIMG
            NEWOBJ=OLDOBJ
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.459.0D0) THEN
!     GWAISTY
            OLDOBJ=NEWOBJ
            OLDIMG=NEWIMG
            OLDREF=NEWREF
            NEWREF=1
            NEWIMG=sys_last_surf()
            NEWOBJ=0
            ERRR=.FALSE.
            NWN1=FIELDY(INT(OPERND(I,9)))
            NWN2=FIELDX(INT(OPERND(I,9)))
            NWN3=FIELDZ(INT(OPERND(I,9)))
            NWN4=FIELDW(INT(OPERND(I,9)))
            CALL GNPR1(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
            JPY=VALUE
            CALL GNPR3(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
            JPUY=VALUE
            CALL GNPR5(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
            JPCY=VALUE
            CALL GNPR7(INT(OPERND(I,8)),NWN1,NWN2,NWN3,NWN4,ERRR,1)
            JPUCY=VALUE
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            IF(((JPUY**2)+(JPUCY**2)).EQ.0.0D0) THEN
               NEWREF=OLDREF
               NEWIMG=OLDIMG
               NEWOBJ=OLDOBJ
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            V1=-((JPY*JPUY)+(JPCY*JPUCY))/((JPUY**2)+(JPUCY**2))
            REG(9)=DSQRT(((JPY+(JPUY*V1))**2)+((JPCY+(JPUCY*V1))**2))
            NEWREF=OLDREF
            NEWIMG=OLDIMG
            NEWOBJ=OLDOBJ
            GO TO 777
         END IF
!
         IF(OPERND(I,17).EQ.293.0D0) THEN
!     DISTORTION
            ERRR=.FALSE.
            CALL DISTOP(INT(OPERND(I,8)),ERRR)
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            REG(9)=VALUE
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.466.0D0) THEN
!     DISTORTION
            ERRR=.FALSE.
            CALL FDISTOP(INT(OPERND(I,8)),ERRR)
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            REG(9)=VALUE
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.294.0D0) THEN
!     XFOC
            ERRR=.FALSE.
            ORI=0
            CALL FLDOP(ORI,INT(OPERND(I,8)),ERRR)
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            REG(9)=VALUE
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.295.0D0) THEN
!     YFOC
            ERRR=.FALSE.
            ORI=1
            CALL FLDOP(ORI,INT(OPERND(I,8)),ERRR)
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            REG(9)=VALUE
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.296.0D0) THEN
!     AST
            ERRR=.FALSE.
            ORI=2
            CALL FLDOP(ORI,INT(OPERND(I,8)),ERRR)
            IF(ERRR) THEN
               IF(F28.EQ.1) REG(9)=0.0D0
               IF(F28.EQ.1) BAAD=1
               IF(F31.EQ.1) CALL MACFAL
               RETURN
            END IF
            REG(9)=VALUE
            GO TO 777
         END IF
!
      END IF
!
   END IF
!
   IF(OPERND(I,17).GE.297.0D0.AND.OPERND(I,17).LE.446) THEN
!
!     3,5,7 OPERANDS
      CALL PRTRB
      CALL PRTRC
      CALL PRTRD
      XIS=.FALSE.
      IF(OPERND(I,17).EQ.298.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.300.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.302.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.304.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.306.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.308.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.310.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.312.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.314.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.316.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.318.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.320.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.322.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.324.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.326.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.328.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.330.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.332.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.334.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.336.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.338.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.340.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.342.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.344.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.346.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.348.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.350.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.352.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.354.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.356.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.358.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.360.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.362.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.364.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.366.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.368.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.370.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.372.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.374.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.376.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.378.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.380.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.382.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.384.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.386.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.388.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.390.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.392.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.394.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.396.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.398.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.400.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.402.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.404.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.406.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.408.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.410.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.412.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.414.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.416.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.418.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.420.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.422.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.424.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.426.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.428.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.430.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.432.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.434.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.436.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.438.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.440.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.442.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.444.0D0) XIS=.TRUE.
      IF(OPERND(I,17).EQ.446.0D0) XIS=.TRUE.
      SF=INT(sys_last_surf())
      ISFI=INT(OPERND(I,8))
      IF(INT(sys_wl_ref()).GE.1.AND.INT(sys_wl_ref()).LE.5) THEN
         CW=INT(sys_wl_ref())+45
      END IF
      IF(INT(sys_wl_ref()).GE.6.AND.INT(sys_wl_ref()).LE.10) THEN
         CW=INT(sys_wl_ref())+65
      END IF
      INV=1.0D0
      IF(sys_mode().EQ.1.0D0) THEN
!       MODE IS FOCAL
         IF(.NOT.XIS)INV=-2.0*ALENS(CW,(SF-1))*PXTRAY(2,(SF-1))
         IF(XIS)INV=-2.0*ALENS(CW,(SF-1))*PXTRAX(2,(SF-1))
      END IF
      IF(sys_mode().EQ.3.0D0) THEN
!       MODE IS AFOCAL
         IF(.NOT.XIS)INV= 2.0*ALENS(CW,(SF-1))*PXTRAY(1,SF)
         IF(XIS)INV= 2.0*ALENS(CW,(SF-1))*PXTRAX(1,SF)
      END IF
      IF(INV.EQ.0.0D0) THEN
         IF(F28.EQ.1) REG(9)=0.0D0
         IF(F28.EQ.1) BAAD=1
         IF(F31.EQ.1) CALL MACFAL
         RETURN
      END IF
!
!     PROCEED WITH CALCULATION
      IF(INT(OPERND(I,8)).LT.INT(sys_last_surf())) THEN
!     SINGLE SURFACE STUFF
         IF(OPERND(I,17).EQ.297.0D0) THEN
            REG(9)=MAB3(1,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.298.0D0) THEN
            REG(9)=XMAB3(1,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.299.0D0) THEN
            REG(9)=3.0D0*MAB3(2,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.300.0D0) THEN
            REG(9)=3.0D0*XMAB3(2,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.301.0D0) THEN
            REG(9)=MAB3(3,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.302.0D0) THEN
            REG(9)=XMAB3(3,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.303.0D0) THEN
            REG(9)=MAB3(4,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.304.0D0) THEN
            REG(9)=XMAB3(4,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.305.0D0) THEN
            REG(9)=MAB3(5,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.306.0D0) THEN
            REG(9)=XMAB3(5,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.307.0D0) THEN
            REG(9)=MAB57(1,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.308.0D0) THEN
            REG(9)=XMAB57(1,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.309.0D0) THEN
            REG(9)=MAB57(2,ISFI)+MAB57(3,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.310.0D0) THEN
            REG(9)=XMAB57(2,ISFI)+XMAB57(3,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.311.0D0) THEN
            REG(9)=MAB57(10,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.312.0D0) THEN
            REG(9)=XMAB57(10,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.313.0D0) THEN
            REG(9)=MAB57(12,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.314.0D0) THEN
            REG(9)=XMAB57(12,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.315.0D0) THEN
            REG(9)=MAB57(11,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.316.0D0) THEN
            REG(9)=XMAB57(11,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.317.0D0) THEN
            REG(9)=MAB57(4,ISFI)+MAB57(5,ISFI)+MAB57(6,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.318.0D0) THEN
            REG(9)=XMAB57(4,ISFI)+XMAB57(5,ISFI)+XMAB57(6,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.319.0D0) THEN
            REG(9)=MAB57(5,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.320.0D0) THEN
            REG(9)=XMAB57(5,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.321.0D0) THEN
            REG(9)=MAB57(7,ISFI)+MAB57(8,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.322.0D0) THEN
            REG(9)=XMAB57(7,ISFI)+XMAB57(8,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.323.0D0) THEN
            REG(9)=MAB57(10,ISFI)+(5.0*MAB57(11,ISFI))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.324.0D0) THEN
            REG(9)=XMAB57(10,ISFI)+(5.0*XMAB57(11,ISFI))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.325.0D0) THEN
            REG(9)=MAB57(10,ISFI)+MAB57(11,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.326.0D0) THEN
            REG(9)=XMAB57(10,ISFI)+XMAB57(11,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.327.0D0) THEN
            REG(9)=MAB57(14,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.328.0D0) THEN
            REG(9)=XMAB57(14,ISFI)
            GO TO 777
         END IF
!       PRIMARY CHROMATIC ABERRATION DIFFERENCES
         IF(OPERND(I,17).EQ.329.0D0) THEN
            REG(9)=PDF3(1,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.330.0D0) THEN
            REG(9)=XPDF3(1,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.331.0D0) THEN
            REG(9)=3.0D0*PDF3(2,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.332.0D0) THEN
            REG(9)=3.0D0*XPDF3(2,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.333.0D0) THEN
            REG(9)=PDF3(3,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.334.0D0) THEN
            REG(9)=XPDF3(3,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.335.0D0) THEN
            REG(9)=PDF3(4,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.336.0D0) THEN
            REG(9)=XPDF3(4,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.337.0D0) THEN
            REG(9)=PDF3(5,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.338.0D0) THEN
            REG(9)=XPDF3(5,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.339.0D0) THEN
            REG(9)=PDF57(1,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.340.0D0) THEN
            REG(9)=XPDF57(1,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.341.0D0) THEN
            REG(9)=PDF57(2,ISFI)+PDF57(3,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.342.0D0) THEN
            REG(9)=XPDF57(2,ISFI)+XPDF57(3,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.343.0D0) THEN
            REG(9)=PDF57(10,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.344.0D0) THEN
            REG(9)=XPDF57(10,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.345.0D0) THEN
            REG(9)=PDF57(12,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.346.0D0) THEN
            REG(9)=XPDF57(12,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.347.0D0) THEN
            REG(9)=PDF57(11,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.348.0D0) THEN
            REG(9)=XPDF57(11,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.349.0D0) THEN
            REG(9)=PDF57(4,ISFI)+PDF57(5,ISFI)+PDF57(6,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.350.0D0) THEN
            REG(9)=XPDF57(4,ISFI)+XPDF57(5,ISFI)+XPDF57(6,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.351.0D0) THEN
            REG(9)=PDF57(5,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.352.0D0) THEN
            REG(9)=XPDF57(5,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.353.0D0) THEN
            REG(9)=PDF57(7,ISFI)+PDF57(8,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.354.0D0) THEN
            REG(9)=XPDF57(7,ISFI)+XPDF57(8,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.355.0D0) THEN
            REG(9)=PDF57(10,ISFI)+(5.0*PDF57(11,ISFI))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.356.0D0) THEN
            REG(9)=XPDF57(10,ISFI)+(5.0*XPDF57(11,ISFI))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.357.0D0) THEN
            REG(9)=PDF57(10,ISFI)+PDF57(11,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.358.0D0) THEN
            REG(9)=XPDF57(10,ISFI)+XPDF57(11,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.359.0D0) THEN
            REG(9)=PDF57(14,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.360.0D0) THEN
            REG(9)=XPDF57(14,ISFI)
            GO TO 777
         END IF
!       SECONDARY CHROMATIC ABERRATION DIFFERENCES
         IF(OPERND(I,17).EQ.361.0D0) THEN
            REG(9)=SDF3(1,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.362.0D0) THEN
            REG(9)=XSDF3(1,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.363.0D0) THEN
            REG(9)=3.0D0*SDF3(2,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.364.0D0) THEN
            REG(9)=3.0D0*XSDF3(2,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.365.0D0) THEN
            REG(9)=SDF3(3,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.366.0D0) THEN
            REG(9)=XSDF3(3,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.367.0D0) THEN
            REG(9)=SDF3(4,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.368.0D0) THEN
            REG(9)=XSDF3(4,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.369.0D0) THEN
            REG(9)=SDF3(5,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.370.0D0) THEN
            REG(9)=XSDF3(5,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.371.0D0) THEN
            REG(9)=SDF57(1,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.372.0D0) THEN
            REG(9)=XSDF57(1,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.373.0D0) THEN
            REG(9)=SDF57(2,ISFI)+SDF57(3,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.374.0D0) THEN
            REG(9)=XSDF57(2,ISFI)+XSDF57(3,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.375.0D0) THEN
            REG(9)=SDF57(10,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.376.0D0) THEN
            REG(9)=XSDF57(10,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.377.0D0) THEN
            REG(9)=SDF57(12,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.378.0D0) THEN
            REG(9)=XSDF57(12,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.379.0D0) THEN
            REG(9)=SDF57(11,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.380.0D0) THEN
            REG(9)=XSDF57(11,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.381.0D0) THEN
            REG(9)=SDF57(4,ISFI)+SDF57(5,ISFI)+SDF57(6,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.382.0D0) THEN
            REG(9)=XSDF57(4,ISFI)+XSDF57(5,ISFI)+XSDF57(6,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.383.0D0) THEN
            REG(9)=SDF57(5,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.384.0D0) THEN
            REG(9)=XSDF57(5,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.385.0D0) THEN
            REG(9)=SDF57(7,ISFI)+SDF57(8,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.386.0D0) THEN
            REG(9)=XSDF57(7,ISFI)+XSDF57(8,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.387.0D0) THEN
            REG(9)=SDF57(10,ISFI)+(5.0*SDF57(11,ISFI))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.388.0D0) THEN
            REG(9)=XSDF57(10,ISFI)+(5.0*XSDF57(11,ISFI))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.389.0D0) THEN
            REG(9)=SDF57(10,ISFI)+SDF57(11,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.390.0D0) THEN
            REG(9)=XSDF57(10,ISFI)+XSDF57(11,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.391.0D0) THEN
            REG(9)=SDF57(14,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.392.0D0) THEN
            REG(9)=XSDF57(14,ISFI)
            GO TO 777
         END IF
!       5TH AND 7TH ORDER INTRINSIC SURFACE ABERRATIONS
         IF(OPERND(I,17).EQ.393.0D0) THEN
            REG(9)=SAB57(1,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.394.0D0) THEN
            REG(9)=XSAB57(1,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.395.0D0) THEN
            REG(9)=SAB57(2,ISFI)+SAB57(3,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.396.0D0) THEN
            REG(9)=XSAB57(2,ISFI)+XSAB57(3,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.397.0D0) THEN
            REG(9)=SAB57(10,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.398.0D0) THEN
            REG(9)=XSAB57(10,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.399.0D0) THEN
            REG(9)=SAB57(12,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.400.0D0) THEN
            REG(9)=XSAB57(12,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.401.0D0) THEN
            REG(9)=SAB57(11,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.402.0D0) THEN
            REG(9)=XSAB57(11,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.403.0D0) THEN
            REG(9)=SAB57(4,ISFI)+SAB57(5,ISFI)+SAB57(6,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.404.0D0) THEN
            REG(9)=XSAB57(4,ISFI)+XSAB57(5,ISFI)+XSAB57(6,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.405.0D0) THEN
            REG(9)=SAB57(5,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.406.0D0) THEN
            REG(9)=XSAB57(5,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.407.0D0) THEN
            REG(9)=SAB57(7,ISFI)+SAB57(8,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.408.0D0) THEN
            REG(9)=XSAB57(7,ISFI)+XSAB57(8,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.409.0D0) THEN
            REG(9)=SAB57(10,ISFI)+(5.0*SAB57(11,ISFI))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.410.0D0) THEN
            REG(9)=XSAB57(10,ISFI)+(5.0*XSAB57(11,ISFI))
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.411.0D0) THEN
            REG(9)=SAB57(10,ISFI)+SAB57(11,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.412.0D0) THEN
            REG(9)=XSAB57(10,ISFI)+XSAB57(11,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.413.0D0) THEN
            REG(9)=SAB57(14,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.414.0D0) THEN
            REG(9)=XSAB57(14,ISFI)
            GO TO 777
         END IF
!       3RD ORDER PUPIL ABERRATIONS
         IF(OPERND(I,17).EQ.415.0D0) THEN
            REG(9)=MAB3(6,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.416.0D0) THEN
            REG(9)=XMAB3(6,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.417.0D0) THEN
            REG(9)=3.0D0*MAB3(7,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.418.0D0) THEN
            REG(9)=3.0D0*XMAB3(7,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.419.0D0) THEN
            REG(9)=MAB3(8,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.420.0D0) THEN
            REG(9)=XMAB3(8,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.421.0D0) THEN
            REG(9)=MAB3(9,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.422.0D0) THEN
            REG(9)=XMAB3(9,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.423.0D0) THEN
            REG(9)=MAB3(10,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.424.0D0) THEN
            REG(9)=XMAB3(10,ISFI)
            GO TO 777
         END IF
!       3RD ORDER PUPIL ABERRATION PRIMARY CHROMATIC DIFFERENCES
         IF(OPERND(I,17).EQ.425.0D0) THEN
            REG(9)=PDF3(6,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.426.0D0) THEN
            REG(9)=XPDF3(6,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.427.0D0) THEN
            REG(9)=3.0D0*PDF3(7,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.428.0D0) THEN
            REG(9)=3.0D0*XPDF3(7,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.429.0D0) THEN
            REG(9)=PDF3(8,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.430.0D0) THEN
            REG(9)=XPDF3(8,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.431.0D0) THEN
            REG(9)=PDF3(9,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.432.0D0) THEN
            REG(9)=XPDF3(9,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.433.0D0) THEN
            REG(9)=PDF3(10,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.434.0D0) THEN
            REG(9)=XPDF3(10,ISFI)
            GO TO 777
         END IF
!       3RD ORDER PUPIL ABERRATION SECONDARY CHROMATIC DIFFERENCES
         IF(OPERND(I,17).EQ.435.0D0) THEN
            REG(9)=SDF3(6,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.436.0D0) THEN
            REG(9)=XSDF3(6,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.437.0D0) THEN
            REG(9)=3.0D0*SDF3(7,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.438.0D0) THEN
            REG(9)=3.0D0*XSDF3(7,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.439.0D0) THEN
            REG(9)=SDF3(8,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.440.0D0) THEN
            REG(9)=XSDF3(8,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.441.0D0) THEN
            REG(9)=SDF3(9,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.442.0D0) THEN
            REG(9)=XSDF3(9,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.443.0D0) THEN
            REG(9)=SDF3(10,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.444.0D0) THEN
            REG(9)=XSDF3(10,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.445.0D0) THEN
            REG(9)=MAB3(11,ISFI)
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.446.0D0) THEN
            REG(9)=XMAB3(11,ISFI)
            GO TO 777
         END IF
      ELSE
         V=0.0D0
!     FINAL SURFACE SUM
         IF(OPERND(I,17).EQ.297.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+MAB3(1,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.298.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XMAB3(1,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.299.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+(3.0D0*MAB3(2,IV))
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.300.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+(3.0D0*XMAB3(2,IV))
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.301.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+MAB3(3,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.302.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XMAB3(3,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.303.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+MAB3(4,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.304.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XMAB3(4,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.305.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+MAB3(5,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.306.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XMAB3(5,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.307.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+MAB57(1,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.308.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XMAB57(1,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.309.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+MAB57(2,IV)+MAB57(3,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.310.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XMAB57(2,IV)+XMAB57(3,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.311.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+MAB57(10,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.312.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XMAB57(10,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.313.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+MAB57(12,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.314.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XMAB57(12,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.315.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+MAB57(11,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.316.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XMAB57(11,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.317.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+MAB57(4,IV)+MAB57(5,IV)+MAB57(6,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.318.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XMAB57(4,IV)+XMAB57(5,IV)+XMAB57(6,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.319.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+MAB57(5,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.320.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XMAB57(5,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.321.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+MAB57(7,IV)+MAB57(8,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.322.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XMAB57(7,IV)+XMAB57(8,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.323.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+MAB57(10,IV)+(5.0*MAB57(11,IV))
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.324.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XMAB57(10,IV)+(5.0*XMAB57(11,IV))
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.325.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+MAB57(10,IV)+MAB57(11,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.326.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XMAB57(10,IV)+XMAB57(11,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.327.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+MAB57(14,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.328.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XMAB57(14,IV)
            END DO
            GO TO 777
         END IF
!       PRIMARY CHROMATIC ABERRATION DIFFERENCES
         IF(OPERND(I,17).EQ.329.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+PDF3(1,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.330.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XPDF3(1,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.331.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+(3.0D0*PDF3(2,IV))
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.332.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+(3.0D0*XPDF3(2,IV))
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.333.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+PDF3(3,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.334.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XPDF3(3,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.335.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+PDF3(4,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.336.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XPDF3(4,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.337.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+PDF3(5,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.338.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XPDF3(5,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.339.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+PDF57(1,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.340.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XPDF57(1,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.341.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+PDF57(2,IV)+PDF57(3,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.342.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XPDF57(2,IV)+XPDF57(3,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.343.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+PDF57(10,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.344.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XPDF57(10,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.345.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+PDF57(12,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.346.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XPDF57(12,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.347.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+PDF57(11,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.348.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XPDF57(11,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.349.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+PDF57(4,IV)+PDF57(5,IV)+PDF57(6,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.350.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XPDF57(4,IV)+XPDF57(5,IV)+XPDF57(6,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.351.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+PDF57(5,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.352.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XPDF57(5,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.353.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+PDF57(7,IV)+PDF57(8,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.354.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XPDF57(7,IV)+XPDF57(8,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.355.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+PDF57(10,IV)+(5.0*PDF57(11,IV))
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.356.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XPDF57(10,IV)+(5.0*XPDF57(11,IV))
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.357.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+PDF57(10,IV)+PDF57(11,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.358.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XPDF57(10,IV)+XPDF57(11,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.359.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+PDF57(14,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.360.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XPDF57(14,IV)
            END DO
            GO TO 777
         END IF
!       SECONDARY CHROMATIC ABERRATION DIFFERENCES
         IF(OPERND(I,17).EQ.361.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SDF3(1,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.362.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSDF3(1,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.363.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+(3.0D0*SDF3(2,IV))
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.364.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+(3.0D0*XSDF3(2,IV))
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.365.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SDF3(3,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.366.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSDF3(3,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.367.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SDF3(4,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.368.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSDF3(4,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.369.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SDF3(5,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.370.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSDF3(5,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.371.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SDF57(1,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.372.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSDF57(1,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.373.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SDF57(2,IV)+SDF57(3,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.374.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSDF57(2,IV)+XSDF57(3,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.375.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SDF57(10,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.376.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSDF57(10,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.377.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SDF57(12,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.378.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSDF57(12,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.379.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SDF57(11,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.380.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSDF57(11,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.381.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SDF57(4,IV)+SDF57(5,IV)+SDF57(6,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.382.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSDF57(4,IV)+XSDF57(5,IV)+XSDF57(6,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.383.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SDF57(5,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.384.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSDF57(5,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.385.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SDF57(7,IV)+SDF57(8,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.386.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSDF57(7,IV)+XSDF57(8,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.387.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SDF57(10,IV)+(5.0*SDF57(11,IV))
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.388.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSDF57(10,IV)+(5.0*XSDF57(11,IV))
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.389.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SDF57(10,IV)+SDF57(11,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.390.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSDF57(10,IV)+XSDF57(11,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.391.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SDF57(14,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.392.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSDF57(14,IV)
            END DO
            GO TO 777
         END IF
!       5TH AND 7TH ORDER INTRINSIC SURFACE ABERRATIONS
         IF(OPERND(I,17).EQ.393.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SAB57(1,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.394.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSAB57(1,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.395.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SAB57(2,IV)+SAB57(3,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.396.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSAB57(2,IV)+XSAB57(3,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.397.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SAB57(10,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.398.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSAB57(10,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.399.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SAB57(12,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.400.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSAB57(12,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.401.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SAB57(11,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.402.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSAB57(11,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.403.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SAB57(4,IV)+SAB57(5,IV)+SAB57(6,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.404.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSAB57(4,IV)+XSAB57(5,IV)+XSAB57(6,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.405.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SAB57(5,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.406.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSAB57(5,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.407.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SAB57(7,IV)+SAB57(8,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.408.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSAB57(7,IV)+XSAB57(8,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.409.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SAB57(10,IV)+(5.0*SAB57(11,IV))
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.410.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSAB57(10,IV)+(5.0*XSAB57(11,IV))
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.411.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SAB57(10,IV)+SAB57(11,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.412.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSAB57(10,IV)+XSAB57(11,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.413.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SAB57(14,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.414.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSAB57(14,IV)
            END DO
            GO TO 777
         END IF
!       3RD ORDER PUPIL ABERRATIONS
         IF(OPERND(I,17).EQ.415.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+MAB3(6,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.416.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XMAB3(6,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.417.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+(3.0D0*MAB3(7,IV))
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.418.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+(3.0D0*XMAB3(7,IV))
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.419.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+MAB3(8,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.420.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XMAB3(8,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.421.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+MAB3(9,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.422.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XMAB3(9,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.423.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+MAB3(10,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.424.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XMAB3(10,IV)
            END DO
            GO TO 777
         END IF
!       3RD ORDER PUPIL ABERRATION PRIMARY CHROMATIC DIFFERENCES
         IF(OPERND(I,17).EQ.425.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+PDF3(6,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.426.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XPDF3(6,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.427.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+(3.0D0*PDF3(7,IV))
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.428.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+(3.0D0*XPDF3(7,IV))
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.429.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+PDF3(8,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.430.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XPDF3(8,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.431.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+PDF3(9,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.432.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XPDF3(9,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.433.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+PDF3(10,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.434.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XPDF3(10,IV)
            END DO
            GO TO 777
         END IF
!       3RD ORDER PUPIL ABERRATION SECONDARY CHROMATIC DIFFERENCES
         IF(OPERND(I,17).EQ.435.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SDF3(6,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.436.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSDF3(6,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.437.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+(3.0D0*SDF3(7,IV))
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.438.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+(3.0D0*XSDF3(7,IV))
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.439.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SDF3(8,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.440.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSDF3(8,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.441.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SDF3(9,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.442.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSDF3(9,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.443.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+SDF3(10,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.444.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XSDF3(10,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.445.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+MAB3(11,IV)
            END DO
            GO TO 777
         END IF
         IF(OPERND(I,17).EQ.446.0D0) THEN
            DO IV=0,INT(sys_last_surf())
               V=V+XMAB3(11,IV)
            END DO
            GO TO 777
         END IF
!
      END IF
!
   END IF
   IF(OPERND(I,17).EQ.447.0D0) THEN
!     AH
      REG(9)=surf_asphere_coeff(INT(OPERND(I,8)), 12)
      GO TO 777
   END IF
   IF(OPERND(I,17).EQ.448.0D0) THEN
!     AI
      REG(9)=surf_asphere_coeff(INT(OPERND(I,8)), 14)
      GO TO 777
   END IF
   IF(OPERND(I,17).EQ.449.0D0) THEN
!     AJ
      REG(9)=surf_asphere_coeff(INT(OPERND(I,8)), 16)
      GO TO 777
   END IF
   IF(OPERND(I,17).EQ.450.0D0) THEN
!     AK
      REG(9)=surf_asphere_coeff(INT(OPERND(I,8)), 18)
      GO TO 777
   END IF
   IF(OPERND(I,17).EQ.451.0D0) THEN
!     AL
      REG(9)=surf_asphere_coeff(INT(OPERND(I,8)), 20)
      GO TO 777
   END IF

777 CONTINUE
   IF(OPERND(I,17).GE.297.0D0.AND.OPERND(I,17).LE.444.0D0) THEN
      IF(INT(OPERND(I,8)).LT.INT(sys_last_surf())) THEN
         REG(9)=REG(9)/INV
      ELSE
         REG(9)=V/INV
      END IF
   END IF
   IF(OPERND(I,17).EQ.445.0D0.OR.OPERND(I,17).EQ.446.0D0) THEN
      IF(INT(OPERND(I,8)).LT.INT(sys_last_surf())) THEN
      ELSE
         REG(9)=V
      END IF
   END IF
!     NOW LOAD THE CONTENTS OF THE ACCUMULATOR INTO THE OPERAND # I
!     LOAD VALUE FROM ACC
   IF(OPERND(I,15).EQ.0.0D0) THEN
!     SET ORIGINAL VALUES AND CHANGE HLD TO COR WITH
!     ORIG VALUE AS TARGET
!     SET ORIGINAL VALUE
      OPERND(I,3)=REG(9)
      IF(OPERND(I,13).EQ.10.0D0) THEN
!     HLD FOUND TO CONVERT
         OPERND(I,2)=OPERND(I,3)
         OPERND(I,13)=1.0D0
      END IF
      OPERND(I,15)=1.0D0
   END IF
!     SET PREVIOUS VALUE TO OLD CURRENT VALUE
   OPERND(I,5)=OPERND(I,4)
!     LOAD NEW CURRENT VALUE
   OPERND(I,4)=REG(9)
!     CALCULATE NEW CHANGE VALUE
   OPERND(I,6)=OPERND(I,4)-OPERND(I,5)
!
!     NOW CALCULATE THE SQUARE ROOT OF THE CONTRIBUTION TO THE MERIT FUMCTION
!     WHICH IS THE THE:
!     CURRENT OPERAND VALUE-TARGET VALUE FOR THE OPERAND
!     MULTIPLIED BY THE SQUARE ROOT OF THE OPERAND WEIGHT
!
   IF(OPERND(I,13).EQ.0.0D0)OPERND(I,14)=0.0D0
!
   IF(OPERND(I,13).EQ.1.0D0) THEN
      OPERND(I,14)=(OPERND(I,4)-OPERND(I,2))
      IF(OPERND(I,14).EQ.0.0D0) OPERND(I,4)=OPERND(I,2)
      OPERND(I,14)=((OPERND(I,7)))*(OPERND(I,14))
   END IF
!
   IF(OPERND(I,13).EQ.-2.0D0) THEN
      IF(OPERND(I,4).LT.OPERND(I,2))THEN
         OPERND(I,14)=(OPERND(I,4)-OPERND(I,2))
         IF(OPERND(I,14).EQ.0.0D0) OPERND(I,4)=OPERND(I,2)
         OPERND(I,14)=((OPERND(I,7)))*(OPERND(I,14))
      END IF
      IF(OPERND(I,4).GE.OPERND(I,2))OPERND(I,14)=0.0D0
   END IF
!
   IF(OPERND(I,13).EQ.2.0D0) THEN
      IF(OPERND(I,4).GT.OPERND(I,2)) THEN
         OPERND(I,14)=(OPERND(I,4)-OPERND(I,2))
         IF(OPERND(I,14).EQ.0.0D0) OPERND(I,4)=OPERND(I,2)
         OPERND(I,14)=((OPERND(I,7)))*(OPERND(I,14))
      END IF
      IF(OPERND(I,4).LE.OPERND(I,2))OPERND(I,14)=0.0D0
   END IF
!
!     RESTORE ORIGINAL ACCUMULATOR VALUE
   REG(9)=OLDREG9
   OLDLDIF2=.TRUE.
   OLDLDIF=.TRUE.
   LDIF2=.TRUE.
   LDIF=.TRUE.
   RETURN
END
FUNCTION RUN_OPT_MAC(I)
   use DATMAI
   IMPLICIT NONE
   INTEGER I,OLDOUT1
   REAL*8 RUN_OPT_MAC
   COMMON/OLDOUTT/OLDOUT1
   OLDOUT1=OUT
   OUT=98
   SAVE_KDP(1)=SAVEINPT(1)
   INPUT='MACROOPT'
   CALL PROCES
   REST_KDP(1)=RESTINPT(1)
   OUT=OLDOUT1
   RUN_OPT_MAC=GPREG(I)
   RETURN
END
