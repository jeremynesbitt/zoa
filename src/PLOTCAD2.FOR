C       SECOND FILE OF PLOT/CAD ROUTINES

C ALL INTERACTER/WINTERACTER CALLS HAVE BEEN MOVED TO ISS.FOR
C

C       SECOND FILE OF PLOT/CAD ROUTINES

C ALL INTERACTER/WINTERACTER CALLS HAVE BEEN MOVED TO ISS.FOR
C
C SUB PLTRST.FOR
        SUBROUTINE PLTRST
C
        IMPLICIT NONE
C
C       THIS ROUTINE RESETS PLOT PARAMETERS TO STARTING VALES
C       AFTER PLOT DEV, PLOT SCREEN OR PLOT PRINTER
C
      INTEGER I,J,K,FANWAV
C
      INTEGER JK_WAV(1:10)
C
        REAL*8 VIEROT
C
      INTEGER VIEXOF,VIEYOF
C
        COMMON/OFFVIE/VIEXOF,VIEYOF,VIEROT
C
      COMMON/WAVER/JK_WAV
C
      COMMON/FANNER/FANWAV
C
      LOGICAL FANEXT,RIM
C
      COMMON/RINSHT/RIM
C
      COMMON/FANEXI/FANEXT
C
      CHARACTER FANNAM*8,FANQAL*8,DRWNAM*11
C
      COMMON/DRWTAG/DRWNAM
C
      COMMON/PASFAN/FANNAM,FANQAL
C
      INTEGER XTENT,YTENT,CLIP
C
      COMMON/USEFAN/XTENT,YTENT,CLIP
C
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATSPD.INC'
        INCLUDE 'DATHGR.INC'
C
C     USER-DEFINED FAN PLOTTING SCALE FACTOR
C     DEFAULT MEANS PROGRAM CALCULATES AUTOSCALE
C       RESET 3D LINE VALUES
        LX1=0.0D0
        LY1=0.0D0
        LZ1=0.0D0
        LX2=0.0D0
        LY2=0.0D0
        LZ2=0.0D0
C       NORAYPLOT
        NORAYPLOT=.TRUE.
C
                        FSSIFLG=.FALSE.
                        FSSI=0.0D0
                        FANAXX=5000
                        FANAXY=3500
                        XTENT=2500
                        YTENT=2000
                        CLIP=0
C
      CALL MY_DEL_NEUT
C     RESET PLOTNAME TO NEUTRAL.DAT
      DRWNAM='NEUTRAL.DAT'
        FIXUP=.FALSE.
        POLDX=0.0D0
        POLDY=0.0D0
        POLDZ=0.0D0
        PCURX=0.0D0
        PCURY=0.0D0
        PCURZ=0.0D0
        PNEWX=0.0D0
        PNEWY=0.0D0
        PNEWZ=0.0D0
C     DEFAULT PEN COLOR IS BLACK
                COLDEF=15
C
C     DEFAULT PLOT ORIGIN IS IN THE LOWER LEFT CORNER AT Y=0, X=0
C     (BOTH INTEGER)
        ORX=0
        ORY=0
C
C     DEFAULT PEN POSITIONS ARE 0 AND 0 BUT ARE NEVER USED
C     SINCE PENSTA BY DEFAULT IS 0 (BOTH INTEGER)
        XPEN=0
        YPEN=0
C
C     PLOTTING LOOK VECTOR
        DRALOK=.FALSE.
      LOKFLG=.FALSE.
C
C     PLOTTING VIG MESSAGE
        PLTVIG=.FALSE.
      VIGFLG=.FALSE.
C
C     PLOTTING VIEW ANGLES
        DRAVUE=.FALSE.
      VUEFLG=.FALSE.
C
C     DEFAULT OLD X-PEN POSITION IS 0 (INTEGER)
        XPENOL=0
C
C     DEFAULT OLD Y-PEN POSITION IS 0 (INTEGER)
        YPENOL=0
C
C     THE DEFAULT PEN STATUS IS 3, PEN STATUS NOT SET TO A USEFUL VALUE
        PENSTA=3
C
C     DEFAULT LINE TYPE IS 0, A SOLID LINE
        LNTYPE=0
      OLLNTP=0
C
C     DEFAULT SYMBOL TYPE IS 8, AN ASTERIX
        SYMB=8
C
C     DEFAULT CHARACTER SIZE FOR SYMBOL IS 1
        SYMSIZ=1
C
C     DEFAULT CHARACTER SIZE FOR LABLE IS 1
        LABSIZ=1
C
C     DEFAULT CHARACTER SIZE FOR NOTES IS 1
        NTSIZ=1
C
C     DEFAULT ANGLE FOR SYMBOL PLOTTING IS 0 DEGREES (INTEGER)
        SYMANG=0
C
C     DEFAULT ANGLE FOR LABLE PLOTTING IS 0 DEGREES (INTEGER)
        LABANG=0
C
C     DEFAULT ANGLE FOR NOTE PLOTTING IS 0 DEGREES (INTEGER)
        NTANG=0
C
C     DEFAULT Y-DIRECTION COSINE OF PLOT LOOK IS 0.0
        LOOKY=0.0D0
C
C     DEFAULT X-DIRECTION COSINE OF PLOT LOOK IS -1.0
        LOOKX=-1.0D0
C
C     DEFAULT Z-DIRECTION COSINE OF PLOT LOOK IS 0.0
        LOOKZ=0.0D0
C
C     SET XSHIFT,YSHIFT AND ZSHIFT
      PXSHFT=0
      PYSHFT=0
      PGAMMA=0
      LORIENT=.FALSE.
C
C     DEFAULT "ALPHA" FOR PLOT VIEW IS 0.0 DEGREES
        VIEALF=0.0D0
C
C     DEFAULT "PHI" FOR PLOT VIEW IS 270.0 DEGREES
        VIEPHI=270.0D0
C
C     BY DEFAULT, GRAPHICS IS SET "ON"
        GRASET=.TRUE.
C
C     DUMMY SURFACES PLOTTED AS DASHED LINES
        DASHH=.TRUE.
C
C     NOW THE FLAGS TO TRACK IF THINGS HAVE BEEN DONE ONCE
C
C     FLAG TO SEE IF "LB" HAS BEEN PLOTTED
C     BY DEFAULT SET TO FALSE
                LBLFLG=.FALSE.
                PLTLBL=.FALSE.
C
C     FLAG TO SEE IF "LI" HAS BEEN PLOTTED
C     BY DEFAULT SET TO FALSE
                LIFLG=.FALSE.
                PLTLLI=.FALSE.
C
C     FLAG TO SEE IF "AXIS" SHOULD BE PLOTTED
C     BY DEFAULT SET TO FALSE
                PLTAXS=.FALSE.
C
C     FLAG TO SEE IF "AXIS" HAS BEEN PLOTTED
C     BY DEFAULT SET TO FALSE
                AXFLG=.FALSE.
C
C     MAX AND MIN DEVICE INDEPENDENT COORDINATES
                XXMAX=10010
                XXMIN=-10
                YYMAX=7010
                YYMIN=-10
      XPEN=0
      YPEN=0
      XPENOL=0
      YPENOL=0
      PENSTA=3
C
C     FLAG TO SEE IF "LABLE" HAS BEEN PLOTTED
C     BY DEFAULT SET TO FALSE
                LBLFLG=.FALSE.
C
C     RIGHT,CENTER AND LEFT PLOT JUSTIFICATION FLAG FOR PLOTS
C     OF THE LENS DATA
C
C     RCL=1 MEANS LEFT JUSTIFY
C     RCL=2 MEANS CENTER JUSTIFY(DEFAULT VALUE)
C     RCL=3 MEANS RIGHT JUSTIFY
                RCL=-2
                JUSOFF=5000.0D0
C
                ROTSET=.FALSE.
                XROT=0.0D0
                YROT=0.0D0
                ZROT=0.0D0
C
C
C     PLOT SCALE STUFF
C
C     BY DEFAULT, AUTOSCALE SHOULD BE DONE
        AUTSL=.TRUE.
C
C     PLOT SCALE SHOULD OR SHOUD NOT BE DRAWN
        DRASCL=.FALSE.
C
C     PLOT SCALE IS NOT EXPLICITLY SET
        PLSC=.FALSE.
C
C     PLOT SCALE HAS NOT BEEN DRAWN YET
        SCFLG=.FALSE.
C
C     FLAG TO SEE IF AN AUTO-SCALE FACTOR HAS BEEN CALCULATED
C     BY DEFAULT SET TO FALSE
                ASCFLG=.FALSE.
C
      SCFAY=1.0D0
      SCFAX=1.0D0
      SCFAYP=1.0D0
      SCFAXP=1.0D0
C
C     PLOT SIZE STUFF
C
C     PLOT SIZE SHOULD OR SHOUD NOT BE DRAWN
        DRASZZ=.FALSE.
C
C     PLOT SIZE IS NOT EXPLICITLY SET
        PLSZ=.FALSE.
C
C     PLOT SIZE HAS NOT BEEN DRAWN YET
        SZFLG=.FALSE.
C
                RIM=.FALSE.
C
      IF(.NOT.FANEXT) THEN
C     RESET
C     FAN PLOTTING DEFAULT VALUES
                YFOB1=0.0D0
                YFOB2=0.0D0
                YFOB3=0.0D0
                XFOB1=0.0D0
                XFOB2=0.0D0
                XFOB3=0.0D0
                FANOFF=0.0D0
C     FAN WAVELENGTHS
C     RESTORE THE DEFAULT WAVELENGTH SETTINGS
C     RESTORE THE DEFAULT WAVELENGTH SETTINGS
                FANWV1=.FALSE.
                FANWV2=.FALSE.
                FANWV3=.FALSE.
                FANWV4=.FALSE.
                FANWV5=.FALSE.
                FANWV6=.FALSE.
                FANWV7=.FALSE.
                FANWV8=.FALSE.
                FANWV9=.FALSE.
                FANWV10=.FALSE.
      JK_WAV(1:10)=0
      I=0
      IF(SYSTEM(31).GT.0.0D0) THEN
      FANWV1=.TRUE.
      I=I+1
      IF(I.GT.10) GO TO 900
      JK_WAV(I)=1
      END IF
      IF(SYSTEM(32).GT.0.0D0) THEN
      FANWV2=.TRUE.
      I=I+1
      IF(I.GT.10) GO TO 900
      JK_WAV(I)=2
      END IF
      IF(SYSTEM(33).GT.0.0D0) THEN
      FANWV3=.TRUE.
      I=I+1
      IF(I.GT.10) GO TO 900
      JK_WAV(I)=3
      END IF
      IF(SYSTEM(34).GT.0.0D0) THEN
      FANWV4=.TRUE.
      I=I+1
      IF(I.GT.10) GO TO 900
      JK_WAV(I)=4
      END IF
      IF(SYSTEM(35).GT.0.0D0) THEN
      FANWV5=.TRUE.
      I=I+1
      IF(I.GT.10) GO TO 900
      JK_WAV(I)=5
      END IF
      IF(SYSTEM(76).GT.0.0D0) THEN
      FANWV6=.TRUE.
      I=I+1
      IF(I.GT.10) GO TO 900
      JK_WAV(I)=6
      END IF
      IF(SYSTEM(77).GT.0.0D0) THEN
      FANWV7=.TRUE.
      I=I+1
      IF(I.GT.10) GO TO 900
      JK_WAV(I)=7
      END IF
      IF(SYSTEM(78).GT.0.0D0) THEN
      FANWV8=.TRUE.
      I=I+1
      IF(I.GT.10) GO TO 900
      JK_WAV(I)=8
      END IF
      IF(SYSTEM(79).GT.0.0D0) THEN
      FANWV9=.TRUE.
      I=I+1
      IF(I.GT.10) GO TO 900
      JK_WAV(I)=9
      END IF
      IF(SYSTEM(80).GT.0.0D0) THEN
      FANWV10=.TRUE.
      I=I+1
      IF(I.GT.10) GO TO 900
      JK_WAV(I)=10
      END IF
 900  CONTINUE
C
                FANNUM=1
                FANNM1=3
                FANNM2=3
                FANTYP=5
                QALTYP=0
                SSIFLG=.TRUE.
                REFWV=INT(SYSTEM(11))
C     FAN PLOTTING DEFAULT VALUES HAVE BEEN SET
                FANNOB=0
                FANNRF=INT(SYSTEM(25))
                FANNIM=INT(SYSTEM(20))
                MAXFAN=20
                STEPJP=125.0D0
                      ELSE
C     FANEXT=TRUE
                        END IF
C     ADDED SO AS TO PLOT THE LAST FAN PRINTED USING
C     A CMD LEVEL FANS GENERATION COMMAND
      IF(FANEXT) THEN
                FANNUM=1
                FANNM1=3
                FANNM2=3
                YFOB1=LFOB(1)
                XFOB1=LFOB(2)
                FANWV1=.FALSE.
                FANWV2=.FALSE.
                FANWV3=.FALSE.
                FANWV4=.FALSE.
                FANWV5=.FALSE.
                FANWV6=.FALSE.
                FANWV7=.FALSE.
                FANWV8=.FALSE.
                FANWV9=.FALSE.
                FANWV10=.FALSE.
                IF(FANWAV.EQ.1) FANWV1=.TRUE.
                IF(FANWAV.EQ.2) FANWV2=.TRUE.
                IF(FANWAV.EQ.3) FANWV3=.TRUE.
                IF(FANWAV.EQ.4) FANWV4=.TRUE.
                IF(FANWAV.EQ.5) FANWV5=.TRUE.
                IF(FANWAV.EQ.6) FANWV6=.TRUE.
                IF(FANWAV.EQ.7) FANWV7=.TRUE.
                IF(FANWAV.EQ.8) FANWV8=.TRUE.
                IF(FANWAV.EQ.9) FANWV9=.TRUE.
                IF(FANWAV.EQ.10) FANWV10=.TRUE.
                IF(FANWAV.EQ.1) JK_WAV(1)=1
                IF(FANWAV.EQ.2) JK_WAV(1)=2
                IF(FANWAV.EQ.3) JK_WAV(1)=3
                IF(FANWAV.EQ.4) JK_WAV(1)=4
                IF(FANWAV.EQ.5) JK_WAV(1)=5
                IF(FANWAV.EQ.6) JK_WAV(1)=6
                IF(FANWAV.EQ.7) JK_WAV(1)=7
                IF(FANWAV.EQ.8) JK_WAV(1)=8
                IF(FANWAV.EQ.9) JK_WAV(1)=9
                IF(FANWAV.EQ.10) JK_WAV(1)=10
                        JK_WAV(2:10)=0
                        FANOFF=0.0D0
                        SSI=0.0D0
                        REFWV=INT(LFOB(4))
                 IF(FANNAM(1:4).EQ.'YFAN') FANTYP=1
                 IF(FANNAM(1:4).EQ.'XFAN') FANTYP=2
                 IF(FANNAM(1:4).EQ.'NFAN') FANTYP=3
                 IF(FANNAM(1:4).EQ.'PFAN') FANTYP=4
                 IF(FANNAM(1:5).EQ.'XYFAN') FANTYP=5
                 IF(FANNAM(1:5).EQ.'YXFAN') FANTYP=6
                 IF(FANQAL(1:4).EQ.'    ') QALTYP=0
                 IF(FANQAL(1:3).EQ.'OPD')  QALTYP=1
                 IF(FANQAL(1:2).EQ.'CD')   QALTYP=2
                 IF(FANQAL(1:2).EQ.'LA')   QALTYP=3
                FANNOB=0
                FANNRF=INT(SYSTEM(25))
                FANNIM=INT(SYSTEM(20))
      IF(LFOB(5).EQ.0.0D0) LFOB(5)=DBLE(NEWOBJ)
      IF(LFOB(6).EQ.0.0D0) LFOB(6)=DBLE(NEWREF)
      IF(LFOB(7).EQ.0.0D0) LFOB(7)=DBLE(NEWIMG)
                FANNOB=INT(LFOB(5))
                FANNRF=INT(LFOB(6))
                FANNIM=INT(LFOB(7))
                MAXFAN=20
                STEPJP=125.0D0
                SSIFLG=.TRUE.
C     FAN PLOTTING DEFAULT VALUES HAVE BEEN SET
                MAXFAN=20
                STEPJP=125.0D0

                        ELSE
C     NO FAN WAS DONE
                        FANEXT=.FALSE.
                        END IF

                        RETURN
                        END
C SUB PLTRED.FOR
        SUBROUTINE PLTRED
C
        IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO PLOT RED AT THE CMD LEVEL
C
      CHARACTER BL20*20,NNTT1*80,CRANGE*8,LABX*40,LABY*40
     1,TMY*8,DTY*10,UNN*9,UNN1*9,BLNOTE*80,B*80
C
      REAL GDTARP(1:101),GDTAR(1:101),AGDTARP(1:101),AGDTAR(1:101)
     1,DELX1,RDELA,LLIM,ULIM
C
      REAL*8 RANGE,MTHETA,SPDELX,SPDELY,DELZ
C
      INTEGER NX,NY,IV,ORTAG,I,ENNL,ENN,NT1ANG,NT1SIZ,DFLAG,MYJK,COLPAS
C
      COMMON/REDPASS/ENNL,GDTARP,GDTAR,MTHETA,SPDELX,SPDELY,DELZ
C
      LOGICAL DEXTRED,DEXTREDSQ
C
      COMMON/DREDEXT/DEXTRED
C
      COMMON/DREDEXTSQ/DEXTREDSQ
C
      LOGICAL EXTRED,EXTKED,CENTRED,SUMMS
C
      LOGICAL EXTREDSQ,SQUARE,ROUND
C
      COMMON/REDEXT/EXTRED,EXTKED
C
      COMMON/REDEXTSQ/EXTREDSQ,CENTRED,SUMMS
C
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATHGR.INC'
      SQUARE=.FALSE.
      ROUND=.FALSE.
      IF(EXTRED.OR.DEXTRED) THEN
          ROUND=.TRUE.
          SQUARE=.FALSE.
          END IF
      IF(EXTREDSQ.OR.DEXTREDSQ) THEN
          ROUND=.FALSE.
          SQUARE=.TRUE.
          END IF
C
      MYJK=0
C
        IF(STI.EQ.1) THEN
        OUTLYNE=
     1  '"PLTRED" PLOTS THE EXISTING ENERGY DISTRIBUTION'
        CALL SHOWIT(1)
                        RETURN
                        ELSE
                        END IF
        IF(SST.EQ.1.OR.SQ.EQ.1) THEN
        OUTLYNE=
     1  '"PLTRED" TAKES NO QUALIFIER OR STRING INPUT'
        CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
        CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        ELSE
                        END IF
        IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
        OUTLYNE=
     1  '"PLTRED" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        ELSE
                        END IF
        IF(S2.EQ.1.AND.W2.LE.0.0D0) THEN
        OUTLYNE=
     1  'THE EXTENT OF THE PLOT, NUMERIC WORD 2, MUST BE'
      CALL SHOWIT(1)
        OUTLYNE=
     1  'GREATER THAN ZERO'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        ELSE
                        END IF
C
      IF(WC.EQ.'PLTRED'.AND..NOT.EXTRED.AND..NOT.EXTREDSQ
     1.AND..NOT.DEXTRED.AND..NOT.DEXTREDSQ) THEN
        OUTLYNE=
     1  'ENERGY DISTRIBUTION VALUES DO NOT EXIST'
      CALL SHOWIT(1)
        OUTLYNE='NO ACTION TAKEN'
      CALL SHOWIT(1)
                        CALL MACFAL
                       RETURN
                       END IF
C
      IF(WC.EQ.'PLTESED'.AND..NOT.EXTKED) THEN
        OUTLYNE=
     1  'EXPANDING SLIT ENERGY DISTRIBUTION VALUES DO NOT EXIST'
      CALL SHOWIT(1)
        OUTLYNE='NO ACTION TAKEN'
      CALL SHOWIT(1)
                        CALL MACFAL
                       RETURN
                       END IF
      IF(ENNL.LT.10) THEN
        OUTLYNE=
     1  'LESS THAN 10 ENERGY VALUES IN TABLE'
      CALL SHOWIT(1)
        OUTLYNE='NO ACTION TAKEN'
      CALL SHOWIT(1)
                        CALL MACFAL
                       RETURN
                       END IF
      IF(DF1.EQ.1.OR.DF1.EQ.0.AND.W1.EQ.0.0D0) DFLAG=0
      IF(DF1.EQ.0.AND.W1.NE.0.0D0) DFLAG=1
C     GENERATE GRAPHIC
C     DO A PLOT NEW
                        DEVTYP=1
                        LOOKY=0.0D0
                        LOOKX=-1.0D0
                        LOOKZ=0.0D0
                        CALL PLTDEV
                        GRASET=.TRUE.
                        PLEXIS=.TRUE.
C     SET LETTER SIZE AND ANGLE
      BL20='                    '
      BLNOTE=BL20//BL20//BL20//BL20
                        NT1SIZ=1
                        NT1ANG=0
      CALL MY_SETCHARASPECT(1.5,1.5)
      CALL PLOTBOX
C
      COLPAS=COLLBL
      CALL MY_COLTYP(COLPAS)
C     DO THE PLOTTING OF THE LENS IDENTIFIER
C     AT X=200, Y=500
                        NT1SIZ=1
                        NT1ANG=0
      IF(STMPT) CALL MYTIME(TMY)
      IF(STMPD) CALL MYDATE(DTY)
        IF(.NOT.STMPT.AND..NOT.STMPD) NNTT1=LI
        IF(STMPT.AND.STMPD) NNTT1=TMY//' '//DTY//LI
        IF(STMPT.AND..NOT.STMPD) NNTT1=TMY//' '//LI
        IF(.NOT.STMPT.AND.STMPD) NNTT1=DTY//LI
                IF(NNTT1.NE.BLNOTE) THEN
        CALL MY_JUSTSTRING(200,500,NNTT1(1:80),NT1ANG,NT1SIZ,3,3)
                        ELSE
C     LI BLANK, NOT ACTION
                        END IF
C     DO THE PLOTTING OF THE RSPH STATUS
C     AT X=200, Y=250
                        NT1SIZ=1
                        NT1ANG=0
      IF(.NOT.SUMMS) THEN
      IF(CENTRED)
     1NNTT1(1:80)='IMAGE POINT LOCATED AT SPOT CENTROID'
      IF(.NOT.CENTRED) THEN
      IF(SPDELX.EQ.0.0D0.AND.SPDELY.EQ.0.0D0)
     1NNTT1(1:80)='IMAGE POINT LOCATED ON THE CHIEF RAY'
      IF(SPDELX.EQ.0.0D0.AND.SPDELY.EQ.0.0D0)
     1NX=36
      IF(SPDELX.NE.0.0D0.OR.SPDELY.NE.0.0D0)
     1NNTT1(1:80)='IMAGE POINT LOCATED ON THE CHIEF RAY WITH OFFSETS'
      IF(SPDELX.NE.0.0D0.OR.SPDELY.NE.0.0D0)
     1NX=49
                   END IF
        CALL MY_JUSTSTRING(200,250,NNTT1(1:NX),NT1ANG,NT1SIZ,3,3)
                       END IF
C
C     TARGET ORIEMNTATION LEGEND
      IF(EXTKED) THEN
C
      CALL MY_PLOT(200,6725,0,0,0,10000,0,7000)
      CALL MY_PLOT(1200,6725,1,0,0,10000,0,7000)
C
      WRITE(B,101) MTHETA
      READ(B,200) CRANGE
        ORTAG=0
101   FORMAT(F8.3)
200   FORMAT(A8)
      IF(MTHETA.EQ.0.0D0.OR.MTHETA.EQ.180.0D0.OR.MTHETA.EQ.360.0D0)
     1 THEN
      NNTT1=
     1'VERTICAL OBJECT ORIENTATION'
      ORTAG=1
                       END IF
      IF(MTHETA.EQ.90.0D0.OR.MTHETA.EQ.270.0D0) THEN
      NNTT1=
     1'HORIZONTAL OBJECT ORIENTATION'
      ORTAG=1
                       END IF
      IF(ORTAG.EQ.0) NNTT1='OBJECT ORIENTATION ='
C
      CALL MY_JUSTSTRING(1400,6700,NNTT1(1:20),NT1ANG,NT1SIZ,3,3)

      IF(ORTAG.EQ.0) THEN
      NNTT1=' '//CRANGE//' '//'DEGREES'
      CALL MY_JUSTSTRING(2700,6700,NNTT1(1:17),NT1ANG,NT1SIZ,3,3)
               END IF
      ORTAG=0
                       ELSE
C     WAS A RED OR REDSQ, DON'T DO THE ORIENTATION
                       END IF
C
C
C       LOWER AND UPPER LIMITS OF RED ARE 0 AND 100.0%
C       LOWER AND UPPER LIMITS OF RANGE ARE 0 AND GDTARP(ENN+1)
                        LLIM=0.0
        IF(DF2.EQ.1) ULIM=GDTARP(ENNL+1)
        IF(DF2.EQ.0) ULIM=REAL(W2)
C

        RDELA=ULIM
C
      IF(ROUND) THEN
      IF(SYSTEM(30).GE.3.0D0) THEN
        LABX='SEMI-DIAMETER (mrad)                    '
        NX=20
                   ELSE
      IF(SYSTEM(6).EQ.1.0D0)
     1  LABX='SEMI-DIAMETER (in)                      '
        IF(SYSTEM(6).EQ.2.0D0)
     1  LABX='SEMI-DIAMETER (cm)                      '
        IF(SYSTEM(6).EQ.3.0D0)
     1  LABX='SEMI-DIAMETER (mm)                      '
        IF(SYSTEM(6).EQ.4.0D0)
     1  LABX='SEMI-DIAMETER (meter)                   '
      IF(SYSTEM(6).EQ.1.0D0)
     1  NX=18
        IF(SYSTEM(6).EQ.2.0D0)
     1  NX=18
        IF(SYSTEM(6).EQ.3.0D0)
     1  NX=18
        IF(SYSTEM(6).EQ.4.0D0)
     1  NX=21
                   END IF
                   END IF
      IF(SQUARE) THEN
      IF(SYSTEM(30).GE.3.0D0) THEN
        LABX='SQUARE SIDE LENGTH (mrad)               '
        NX=25
                   ELSE
        IF(SYSTEM(6).EQ.1.0D0)
     1  LABX='SQUARE SIDE LENGTH (in)                 '
        IF(SYSTEM(6).EQ.2.0D0)
     1  LABX='SQUARE SIDE LENGTH (cm)                 '
        IF(SYSTEM(6).EQ.3.0D0)
     1  LABX='SQUARE SIDE LENGTH (mm)                 '
        IF(SYSTEM(6).EQ.4.0D0)
     1  LABX='SQUARE SIDE LENGTH (meter)              '
        IF(SYSTEM(6).EQ.1.0D0)
     1  NX=23
        IF(SYSTEM(6).EQ.2.0D0)
     1  NX=23
        IF(SYSTEM(6).EQ.3.0D0)
     1  NX=23
        IF(SYSTEM(6).EQ.4.0D0)
     1  NX=26
                   END IF
                   END IF

      IF(.NOT.SUMMS) THEN
        IF(EXTRED)
     1  LABY='% ENERGY (RED)                          '
        IF(EXTKED)
     1  LABY='% ENERGY (ESED)                         '
        IF(EXTREDSQ)
     1  LABY='% ENERGY (REDSQ)                        '
        IF(DEXTRED)
     1  LABY='% ENERGY (DRED)                         '
        IF(DEXTREDSQ)
     1  LABY='% ENERGY (DREDSQ)                       '
        IF(EXTRED)
     1  NY=14
        IF(EXTKED)
     1  NY=15
        IF(EXTREDSQ)
     1  NY=16
        IF(DEXTRED)
     1  NY=15
        IF(DEXTREDSQ)
     1  NY=17
                   END IF
      IF(SUMMS) THEN
        IF(EXTRED)
     1  LABY='% ENERGY (SUMMED RED)                   '
        IF(EXTKED)
     1  LABY='% ENERGY (SUMMED ESED)                  '
        IF(EXTREDSQ)
     1  LABY='% ENERGY (SUMMED REDSQ)                 '
        IF(EXTRED)
     1  NY=21
        IF(EXTKED)
     1  NY=22
        IF(EXTREDSQ)
     1  NY=23
                   END IF
C
C     PLOT THE AXES AND TICS
      CALL PLOTAXES4
C     PLOT THE HORIZONTAL AXIS NAME
      CALL PLOTHNAME(LABX,NX)
C     PLOT THE VERTICAL AXIS NAME
      CALL PLOTVNAME(LABY,NY)

C     PLOT THE VALUES FOR THE TIC MARKS
      CALL PLOTVVAL(0.0,10.0)
      DELX1=(ULIM-LLIM)/5.0
      CALL PLOTHVAL(LLIM,DELX1)

C
                       DO I=1,101
        AGDTARP(I)=GDTARP(I)
        AGDTAR(I)=GDTAR(I)
                       END DO
      ENN=ENNL
      CALL PLOTFUNC2(AGDTARP,AGDTAR,ENN+1,LLIM,ULIM,0.0,100.0,150)
        IF(.NOT.PLEXIS) PLEXIS=.TRUE.
C
      COLPAS=COLLBL
      CALL MY_COLTYP(COLPAS)
C     FIELD OF VIEW DATA
      IF(SYSTEM(30).EQ.1.0D0.OR.SYSTEM(30).EQ.2.0D0) THEN
C     FOCAL OR UFOCAL
      IF(SYSTEM(6).EQ.1.0D0) UNN='in(s)    '
      IF(SYSTEM(6).EQ.2.0D0) UNN='cm(s)    '
      IF(SYSTEM(6).EQ.3.0D0) UNN='mm(s)    '
      IF(SYSTEM(6).EQ.4.0D0) UNN='meter(s) '
                        END IF
C
      IF(.NOT.SUMMS) THEN
      IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D10) THEN
C     SCX FANG
      RANGE=SYSTEM(23)*LFOB(2)
      UNN1='DEGREE(S)'
                        ELSE
      RANGE=SYSTEM(16)*LFOB(2)
      UNN1(1:9)=UNN(1:9)
C     SCX
                        END IF
      WRITE(B,101) RANGE
      READ(B,200) CRANGE
      NNTT1='OBJECT POINT-X'
C
      CALL MY_JUSTSTRING(1400,6500,NNTT1(1:14),NT1ANG,NT1SIZ,3,3)
      NNTT1=' = '//CRANGE//' '//UNN1
C
      CALL MY_JUSTSTRING(2700,6500,NNTT1(1:21),NT1ANG,NT1SIZ,3,3)
C
C
      IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D10) THEN
C     SCY FANG
      RANGE=SYSTEM(21)*LFOB(1)
      UNN1='DEGREE(S)'
                        ELSE
      RANGE=SYSTEM(14)*LFOB(1)
      UNN1(1:9)=UNN(1:9)
C     SCY
                        END IF
      WRITE(B,101) RANGE
      READ(B,200) CRANGE
      NNTT1='OBJECT POINT-Y'
C
      CALL MY_JUSTSTRING(1400,6300,NNTT1(1:14),NT1ANG,NT1SIZ,3,3)
      NNTT1=' = '//CRANGE//' '//UNN1
C
      CALL MY_JUSTSTRING(2700,6300,NNTT1(1:21),NT1ANG,NT1SIZ,3,3)
                       END IF
C
      IF(.NOT.SUMMS) THEN
      IF(SPDELX.NE.0.0D0) THEN
      RANGE=SPDELX
      UNN1(1:9)=UNN(1:9)
      WRITE(B,101) RANGE
      READ(B,200) CRANGE
      NNTT1='OFFSET  IN   X'
C
      CALL MY_JUSTSTRING(1400,5900,NNTT1(1:14),NT1ANG,NT1SIZ,3,3)
      NNTT1=' = '//CRANGE//' '//UNN1
C
      CALL MY_JUSTSTRING(2700,5900,NNTT1(1:21),NT1ANG,NT1SIZ,3,3)
                   END IF
      IF(SPDELY.NE.0.0D0) THEN
      RANGE=SPDELY
      UNN1(1:9)=UNN(1:9)
      WRITE(B,101) RANGE
      READ(B,200) CRANGE
      NNTT1='OFFSET  IN   Y'
C
      CALL MY_JUSTSTRING(1400,5700,NNTT1(1:14),NT1ANG,NT1SIZ,3,3)
      NNTT1=' = '//CRANGE//' '//UNN1
C
      CALL MY_JUSTSTRING(2700,5700,NNTT1(1:21),NT1ANG,NT1SIZ,3,3)
                   END IF
                   END IF
      IF(.NOT.SUMMS) THEN
      IF(SYSTEM(30).LE.2.0D0) THEN
C     FOCAL OR UFOCAL, PRINT THE DEFOCUS
      IF(DELZ.NE.0.0D0) THEN
      RANGE=DELZ
      UNN1(1:9)=UNN(1:9)
      WRITE(B,101) RANGE
      READ(B,200) CRANGE
      NNTT1='DE-FOCUS IN  Z'
C
      CALL MY_JUSTSTRING(1400,6100,NNTT1(1:14),NT1ANG,NT1SIZ,3,3)
      NNTT1=' = '//CRANGE//' '//UNN1
C
      CALL MY_JUSTSTRING(2700,6100,NNTT1(1:21),NT1ANG,NT1SIZ,3,3)
                   END IF
                   END IF
                   END IF
C
      IF(DFLAG.EQ.0) THEN
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT='DRAW'
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
                        END IF
                        RETURN
                        END

C SUB THREEDEESET
        SUBROUTINE THREEDEESET
        IMPLICIT NONE
        INCLUDE 'DATMAI.INC'
        IF(STI.EQ.1.OR.SN.EQ.0.AND.SQ.EQ.0.AND.SST.EQ.0) THEN
        IF(WC(1:2).EQ.'X1') THEN
        WRITE(OUTLYNE,*) 'X1=',LX1
        CALL SHOWIT(1)
                        END IF
        IF(WC(1:2).EQ.'Y1') THEN
        WRITE(OUTLYNE,*) 'Y1=',LY1
        CALL SHOWIT(1)
                        END IF
        IF(WC(1:2).EQ.'Z1') THEN
        WRITE(OUTLYNE,*) 'Z1=',LZ1
        CALL SHOWIT(1)
                        END IF
        IF(WC(1:2).EQ.'X2') THEN
        WRITE(OUTLYNE,*) 'X2=',LX2
        CALL SHOWIT(1)
                        END IF
        IF(WC(1:2).EQ.'Y2') THEN
        WRITE(OUTLYNE,*) 'Y2=',LY2
        CALL SHOWIT(1)
                        END IF
        IF(WC(1:2).EQ.'Z2') THEN
        WRITE(OUTLYNE,*) 'Z2=',LZ2
        CALL SHOWIT(1)
                        END IF
                        END IF
        IF(SQ.EQ.1.OR.SST.EQ.1) THEN
        OUTLYNE=
     1  WC(1:2)//' TAKES NO QUALIFIER OR STRING INPUT'
        CALL SHOWIT(1)
        CALL MACFAL
        RETURN
        END IF
        IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
        OUTLYNE=
     1  WC(1:2)//' ONLY TAKES NUMERIC WORD #1 INPUT'
        CALL SHOWIT(1)
        CALL MACFAL
        RETURN
        END IF
        IF(WC.EQ.'X1') LX1=W1
        IF(WC.EQ.'Y1') LY1=W1
        IF(WC.EQ.'Z1') LZ1=W1
        IF(WC.EQ.'X2') LX2=W1
        IF(WC.EQ.'Y2') LY2=W1
        IF(WC.EQ.'Z2') LZ2=W1
        RETURN
        END
C SUB PLTLINE.FOR
        SUBROUTINE PLTLINE
        USE GLOBALS
C
        IMPLICIT NONE
C
C       THIS ROUTINE DOES THE 3D PLOT LINE COMMAND AT THE CMD LEVEL
C
      REAL*8 X,Y,Z,XN,YN,ZN,ROT1X,ROT1Z,ROT2Y,
     1ROT2Z,AX,AY,AZ,AALF,APHI,YMAXI,YMINI,XMAXI,XMINI
     2,XNEW,YNEW,LKG,VIEPH,VIEAL
C
      LOGICAL GGO
C
      INTEGER IX,IY,I,IPST,COLPAS,STOPAT
C
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATLEN.INC'
C
C     LOOK.VIEW TRANSFORMS

      ROT1X(AX,AZ,APHI)=((AX*DCOS(APHI))-(AZ*DSIN(APHI)))
      ROT1Z(AX,AZ,APHI)=((AX*DSIN(APHI))+(AZ*DCOS(APHI)))
C
      ROT2Z(AZ,AY,AALF)=((AZ*DCOS(AALF))+(AY*DSIN(AALF)))
      ROT2Y(AZ,AY,AALF)=((-AZ*DSIN(AALF))+(AY*DCOS(AALF)))
C
                        X=0.0D0
                        Y=0.0D0
C
                VIEPH=(PII/180.0D0)*VIEPHI
                VIEAL=(PII/180.0D0)*VIEALF
C     MAKE SURE YOU HAVE VERTEX DATA WITHOUT OFFSETS AND SET TO
C     THE FIRST SURFACE WITHOUT AN INFINITE THICKNESS
                        GLSURF=-99
                        DO I=NEWIMG,0,-1
      IF(DABS(ALENS(3,I)).LE.1.0D10) GLSURF=I
                        END DO
      IF(GLSURF.EQ.-99) THEN
                        GLOBE=.FALSE.
      OUTLYNE='ALL SURFACES WERE OF INFINITE THICKNESS'
      CALL SHOWIT(1)
      OUTLYNE='NO OPTICAL SYSTEM PLOT COULD BE MADE'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
      IF(MSG) THEN
      OUTLYNE='GENERATING LINE PLOTTING DATA...'
      CALL SHOWIT(1)
                        END IF
C       CHECK SYNTAX
        IF(SST.EQ.1.OR.SN.EQ.1) THEN
        OUTLYNE=
     1  '"PLOT LINE" TAKES NO ADDITIONAL INPUT'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(STI.EQ.1) THEN
                 WRITE(OUTLYNE,800)
      CALL SHOWIT(1)
 800    FORMAT('QUERRY (?) HAS NO MEANING WITH "PLOT LINE"')
                        RETURN
                        ELSE
                        END IF
C       ALL INPUT IS OK, KEEP GOING
C       NOW WE HAVE THE REAL WORLD COORDINATES OF THE RAY TO BE PLOTTED
C       THEY ARE STORED IN THE ARRAY GLBRAY PASSED IN A COMMON BLOCK
C       VIA THE "INCLUDE 'DATLEN.INC'" STATEMENT.
C
C     HERE IS THE LOOP WHICH PLOTS THE LINE USING CALLS TO
C     PENMV1A(IX,IY,IPST)
c
                        DO I=1,2
        IF(I.EQ.1) THEN
      X=LX1
      Y=LY1
      Z=LZ1
                END IF
        IF(I.EQ.2) THEN
      X=LX2
      Y=LY2
      Z=LZ2
                END IF
      X=X-XROT
      Y=Y-YROT
      Z=Z-ZROT
      XN=ROT1X(X,Z,VIEPH)
      YN=Y
      ZN=ROT1Z(X,Z,VIEPH)
      X=XN
      Y=YN
      Z=ZN
C
      ZN=ROT2Z(Z,Y,VIEAL)
      YN=ROT2Y(Z,Y,VIEAL)
      XN=X
      X=XN
      Y=YN
      Z=ZN
C
C     RIGHT NOW,COORDINATES ARE IN WORLD COORDINATES
C     CONVERT THEM TO DEVICE INDEPENDENT GRAPHICS COORDINATES IN
C     TWO STEPS.
C
C     THE WORLD X PLOTS TO THE PLOTTER X
C     THE WORLD Y PLOTS TO THE PLOTTER Y
C
C     STEP 1: CONVERT USING AN APPROPRIATE SCALE FACTOR
C               CALCULATING AN APPROPRIATE FACTOR IF NECESSARY
C
      Y=(Y/SCFAY)*1000.0D0
C
      X=(X/SCFAX)*1000.0D0
C
C     STEP 2: APPLY THE SCREEN Y-OFFSET AND SCREEN GAMMA ROTATION
C
C     APPLY THE PLOT LOOK/VIEW Y-OFFSET VALUE
C
      IF(LORIENT) CALL ORSHIFT
      Y=Y+3500.0D0+DBLE(PYSHFT)
      X=X+DBLE(PXSHFT)
C
      X=X+JUSOFF
C
C     IF THERE IS A NON-ZERO GAMMA SPECIFIED IN PLOT LOOK OR PLOT VIEW THEN
C     ROTATE IN GAMMA ON THE SCREEN ABOUT THE CENTER OF THE SCREEN
C     WHICH HAS COORDINATES X=5000.0D0,Y=3500.0D0
C
C     FIRST SHIFT THE COORDINATE ORIGIN TO THE CETER OF THE DISPLAY
C
                X=X-5000.0D0
                Y=Y-3500.0D0
C     THE SCREEN COORDINATE IN REAL*8 IN THE SHIFTED COORDINATE
C     FRAME IS NOW X AND Y

      IF(DBLE(PGAMMA).NE.0.0D0) THEN
      LKG=(PII/180.0D0)*DBLE(PGAMMA)
                XNEW=((X*DCOS(LKG))-(Y*DSIN(LKG)))
                YNEW=((X*DSIN(LKG))+(Y*DCOS(LKG)))
                X=XNEW
                Y=YNEW
                        ELSE
                        END IF

C     THE ROTATION IS DONE, NOW SHIFT THE ORIGIN BACK TO THE BOTTOM
C     LEFT HAND CORNER
                        X=X+5000.0D0
                        Y=Y+3500.0D0
C
      IF(I.EQ.1) THEN
C     PUT INSTRUCTIONS IN P1ARAY TO LIFT PEN, GO TO STARTING POINT
      IPST=0
                IX=INT(X)
                IY=INT(Y)
C
      P1ARAY(I,1,1)=IX
      P1ARAY(I,2,1)=IY
      P1ARAY(I,3,1)=0
        IF(.NOT.PLEXIS) PLEXIS=.TRUE.
                ELSE
                END IF
      IF(I.GT.1) THEN
C     PUT INSTRUCTIONS IN P1ARAY TO DROP PEN AND DRAW
      IPST=1
      IX=INT(X)
      IY=INT(Y)
C     MOVE TO NEW POSITION WITH LOWERED PEN
      P1ARAY(I,1,1)=IX
      P1ARAY(I,2,1)=IY
      IPST=1
      P1ARAY(I,3,1)=IPST
        IF(.NOT.PLEXIS) PLEXIS=.TRUE.
                END IF
C     IF FINISHED, LIFT PEN
      IF(I.EQ.STPSUR) IPST=0
                        END DO
C
C     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE P1ARAY ARRAY
C
      COLPAS=COLLBL
      CALL MY_COLTYP(COLPAS)
        FIXUP=.FALSE.
                        DO I=1,2
      IF(I.EQ.1)
     1CALL PENMV1A(P1ARAY(I,1,1),P1ARAY(I,2,1),P1ARAY(I,3,1))
      IF(I.GT.1) THEN
                     GGO=.TRUE.
C     TEST IF LAST POINT IS NEW POINT, IF SO DON'T MOVE
      IF(P1ARAY(I,1,1).EQ.P1ARAY(I-1,1,1).AND.P1ARAY(I,2,1).EQ.
     1P1ARAY(I-1,2,1)) GGO=.FALSE.
      IF(P1ARAY(I,3,1).NE.0.AND.GGO)
     1CALL PENMV1A(P1ARAY(I,1,1),P1ARAY(I,2,1),P1ARAY(I,3,1))
                       END IF
                        END DO
      COLPAS=COLLBL
      CALL MY_COLTYP(COLPAS)
C     HERE WE DO THE PLOT AXIS DRAWING
                        IF(.NOT.VIGFLG.AND.PLTVIG) THEN
                        CALL VIGSHO
                        VIGFLG=.TRUE.
                        ELSE
                        END IF
                        RETURN
                        END

C SUB PLTPEN.FOR
        SUBROUTINE PLTPEN
C
        IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOT PEN COMMAND AT THE CMD LEVEL
C
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATLEN.INC'
      INTEGER COLPAS
C
C       PLOT PEN
C       CHECK SYNTAX
        IF(S4.EQ.1.OR.S5.EQ.1) THEN
        OUTLYNE=
     1  '"PLOT PEN" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(SST.EQ.1) THEN
        OUTLYNE=
     1  '"PLOT PEN" TAKES NO STRING INPUT'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(STI.EQ.0) THEN
        IF(DF1.EQ.1.OR.DF2.EQ.1.OR.DF3.EQ.1) THEN
        OUTLYNE=
     1  '"PLOT PEN" REQUIRES EXPLICIT NUMERIC WORD #1, #2 AND #3 INPUT'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
C
        IF(INT(W3).LT.1.OR.INT(W3).GT.3) THEN
        OUTLYNE=
     1  'NUMERIC WORD #3 BEYOND LEGAL RANGE FOR "PLOT PEN"'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
                        END IF
C       STI
        IF(STI.EQ.1) THEN
        WRITE(OUTLYNE,100)
      CALL SHOWIT(1)
        WRITE(OUTLYNE,200) XPEN
      CALL SHOWIT(1)
        WRITE(OUTLYNE,300) YPEN
      CALL SHOWIT(1)
        WRITE(OUTLYNE,400) PENSTA
      CALL SHOWIT(1)
                        RETURN
                        END IF
C       NW1 IS THE X COORDINATE
C       NW2 IS THE Y COORDINATE
C       NW3 IS THE PEN STATUS INDICATOR:
C
        XPENOL=XPEN
        YPENOL=YPEN
        XPEN=INT(W1)
        YPEN=INT(W2)
        PENSTA=INT(W3)
C
C               1=NO CHANGE IN PEN STATUS
C               2=LOWER PEN BEFORE MOVE
C               3=LIFT PEN BEFORE MOVE
C
C       VARAIBLE IS PENSTA THAT IS PASSED TO THE PLOT COMMAND
C       IN PENMV.FOR
C       NOW MOVE THE PEN
                COLPAS=COLPEN
      CALL MY_COLTYP(COLPAS)
                CALL PENMV
        IF(.NOT.PLEXIS) PLEXIS=.TRUE.
C       PEN POSITIONS ARE STORED IN XPEN AND YPEN
100     FORMAT('CURRENT "PLOT PEN" PARAMETERS ARE:')
200     FORMAT('     X-PEN POSITION = ',I5)
300     FORMAT('     Y-PEN POSITION = ',I5)
400     FORMAT('         PEN STATUS = ',I5)
                        RETURN
                        END
C SUB VERTLINE.FOR
        SUBROUTINE VERTLINE
        USE GLOBALS
C
        IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOT LINE CONNECTING SURFACE VERTICES
C
      REAL*8 X,Y,Z,XN,YN,ZN,ROT1X,ROT1Z,ROT2Y,
     1ROT2Z,AX,AY,AZ,AALF,APHI,YMAXI,YMINI,XMAXI,XMINI
     2,XNEW,YNEW,LKG,VIEPH,VIEAL
C
      LOGICAL GGO
C
      INTEGER IX,IY,I,IPST,STOPAT,COLPAS
C
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATLEN.INC'
C
C     LOOK.VIEW TRANSFORMS

      ROT1X(AX,AZ,APHI)=((AX*DCOS(APHI))-(AZ*DSIN(APHI)))
      ROT1Z(AX,AZ,APHI)=((AX*DSIN(APHI))+(AZ*DCOS(APHI)))
C
      ROT2Z(AZ,AY,AALF)=((AZ*DCOS(AALF))+(AY*DSIN(AALF)))
      ROT2Y(AZ,AY,AALF)=((-AZ*DSIN(AALF))+(AY*DCOS(AALF)))
C
C     LNTYPE=0
      LNTYPE=0
                        X=0.0D0
                        Y=0.0D0
C
                VIEPH=(PII/180.0D0)*VIEPHI
                VIEAL=(PII/180.0D0)*VIEALF
C     MAKE SURE YOU HAVE VERTEX DATA WITHOUT OFFSETS AND SET TO
C     THE FIRST SURFACE WITHOUT AN INFINITE THICKNESS
                        GLSURF=-99
                        DO I=NEWIMG,0,-1
      IF(DABS(ALENS(3,I)).LE.1.0D10) GLSURF=I
                        END DO
      IF(GLSURF.EQ.-99) THEN
                        GLOBE=.FALSE.
      OUTLYNE='ALL SURFACES WERE OF INFINITE THICKNESS'
      CALL SHOWIT(1)
      OUTLYNE='NO OPTICAL SYSTEM PLOT COULD BE MADE'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
                        GLOBE=.TRUE.
                        OFFX=0.0D0
                        OFFY=0.0D0
                        OFFZ=0.0D0
                        OFFA=0.0D0
                        OFFB=0.0D0
                        OFFC=0.0D0
                        CALL GLVERT
                        GLOBE=.FALSE.
      IF(MSG) THEN
      OUTLYNE='GENERATING VERTEX LINE PLOTTING DATA...'
      CALL SHOWIT(1)
                        END IF
C       CHECK SYNTAX
        IF(SST.EQ.1) THEN
        OUTLYNE=
     1  '"PLOT VERTLINE" TAKES NO STRING INPUT'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
        OUTLYNE=
     1  '"PLOT VERTLINE" ONLY TAKES NUMERIC WORDS #1 AND #2 INPUT'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(STI.EQ.1) THEN
                 WRITE(OUTLYNE,800)
      CALL SHOWIT(1)
 800    FORMAT('QUERRY (?) HAS NO MEANING WITH "PLOT VERTLINE"')
                        RETURN
                        ELSE
                        END IF
        IF(DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM(20)+W1
        IF(DF2.EQ.0.AND.W2.LT.0.0D0) W2=SYSTEM(20)+W2
C       DEFAULT VALUES
        IF(DF1.EQ.1) THEN
        IF(DABS(ALENS(3,0)).GT.1.0D10) THEN
                        W1=DBLE(1)
                        ELSE
                        W1=DBLE(0)
                        END IF
                        ELSE
C       DF1 NOT 1, W1 EXPLICITLY ENTERED
                        END IF
        IF(DF2.EQ.1) THEN
        IF(DABS(ALENS(3,(NEWIMG-1))).GT.1.0D10) THEN
                        W2=DBLE(NEWIMG-1)
                        ELSE
                        W2=DBLE(NEWIMG)
                        END IF
                        ELSE
C       DF2 NOT 1, W2 EXPLICITLY ENTERED
                        END IF
               STASUR=INT(W1)
               STPSUR=INT(W2)
      IF(STASUR.LT.NEWOBJ) STASUR=NEWOBJ
      IF(STPSUR.LT.NEWOBJ) STPSUR=NEWOBJ
C
        IF(INT(W1).LT.0) THEN
C       INVALID NUMERIC WORD #1
        OUTLYNE=
     1  'SURFACE NUMBER (NUMERIC WORD #1) IS BEYOND LEGAL RANGE'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(INT(W2).GT.NEWIMG) THEN
C       INVALID NUMERIC WORD #2
        OUTLYNE=
     1  'SURFACE NUMBER (NUMERIC WORD #2) IS BEYOND LEGAL RANGE'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(INT(W2).LE.INT(W1)) THEN
C       W2 LESS THAN OR EQUAL TO W1
        OUTLYNE=
     1  'NUMERIC WORD #2 MUST BE GREATER THAN NUMERIC WORD #1'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
C       ALL INPUT IS OK, KEEP GOING
C       NOW WE HAVE THE REAL WORLD COORDINATES OF THE VERTEX LINE TO BE PLOTTED
C       THEY ARE STORED IN THE ARRAY VERTEX PASSED IN A COMMON BLOCK
C       VIA THE "INCLUDE 'DATLEN.INC'" STATEMENT.
C
C     HERE IS WHERE REAL WORK BEGINS. FIRST PROJECT THE 3D WORLD COORDINATES
C     OF THE VERTICIES ONTO THE 2D PLANE INDICATED BY THE PLOT LOOK/ PLOT VIEW
C     DATA. THE DIRECTION COSINES OF THE LOOK VECTOR ARE LOOKX, LOOKY AND
C     LOOKZ. THE TRANSFORMATIONS ARE:
C
      CALL ROT1
C
C     AFTER THIS TRANSFORMATION, THE WORLD X COORDINATE IS PLOTTED IN THE
C     HORIZONTAL X-AXIS OF THE DISPLAY AND THE Y WORLD COORDINATE IS PLOTTED
C     IN THE VERTICAL Y-AXIS OF THE DISPLAY AFTER A CONVERSION TO
C     DEVICE INDEPENDENT COORDINATES ARE MADE.
C
      CALL PLTSC1(XMINI,XMAXI,YMINI,YMAXI)
C
C     HERE IS THE LOOP WHICH PLOTS VERTEX LINE USING CALLS TO
C     PENMV1A(IX,IY,IPST)
c
               IF(STPSUR.LT.STASUR) STPSUR=STASUR
                        DO I=STASUR,STPSUR
      X=VERTEX(1,I)
      Y=VERTEX(2,I)
      Z=VERTEX(3,I)
      X=X-XROT
      Y=Y-YROT
      Z=Z-ZROT
      XN=ROT1X(X,Z,VIEPH)
      YN=Y
      ZN=ROT1Z(X,Z,VIEPH)
      X=XN
      Y=YN
      Z=ZN
C
      ZN=ROT2Z(Z,Y,VIEAL)
      YN=ROT2Y(Z,Y,VIEAL)
      XN=X
      X=XN
      Y=YN
      Z=ZN
C
C     RIGHT NOW,COORDINATES ARE IN WORLD COORDINATES
C     CONVERT THEM TO DEVICE INDEPENDENT GRAPHICS COORDINATES IN
C     TWO STEPS.
C
C     THE WORLD X PLOTS TO THE PLOTTER X
C     THE WORLD Y PLOTS TO THE PLOTTER Y
C
C     STEP 1: CONVERT USING AN APPROPRIATE SCALE FACTOR
C               CALCULATING AN APPROPRIATE FACTOR IF NECESSARY
C
      Y=(Y/SCFAY)*1000.0D0
C
      X=(X/SCFAX)*1000.0D0
C
C     STEP 2: APPLY THE SCREEN Y-OFFSET AND SCREEN GAMMA ROTATION
C
C     APPLY THE PLOT LOOK/VIEW Y-OFFSET VALUE
C
      IF(LORIENT) CALL ORSHIFT
      Y=Y+3500.0D0+DBLE(PYSHFT)
      X=X+DBLE(PXSHFT)
C
C     IF THERE ARE X-OFFSETS (JUSTIFICATION) TO APPLY, DO THEM HERE.
C
      IF(RCL.EQ.1.OR.RCL.EQ.-1) THEN
      JUSOFF=500.0D0-((XMINI/SCFAX)*1000.0D0)
                        RCL=-1
                        ELSE
                        END IF
      IF(RCL.EQ.2.OR.RCL.EQ.-2) THEN
                        RCL=-2
                        JUSOFF=5000.0D0
                        ELSE
                        END IF
      IF(RCL.EQ.3.OR.RCL.EQ.-3) THEN
      JUSOFF=9500.0D0-((XMAXI/SCFAX)*1000.0D0)
                        RCL=-3
                        ELSE
                        END IF
C
      X=X+JUSOFF
C
C     IF THERE IS A NON-ZERO GAMMA SPECIFIED IN PLOT LOOK OR PLOT VIEW THEN
C     ROTATE IN GAMMA ON THE SCREEN ABOUT THE CENTER OF THE SCREEN
C     WHICH HAS COORDINATES X=5000.0D0,Y=3500.0D0
C
C     FIRST SHIFT THE COORDINATE ORIGIN TO THE CETER OF THE DISPLAY
C
                X=X-5000.0D0
                Y=Y-3500.0D0
C     THE SCREEN COORDINATE IN REAL*8 IN THE SHIFTED COORDINATE
C     FRAME IS NOW X AND Y

      IF(DBLE(PGAMMA).NE.0.0D0) THEN
      LKG=(PII/180.0D0)*DBLE(PGAMMA)
                XNEW=((X*DCOS(LKG))-(Y*DSIN(LKG)))
                YNEW=((X*DSIN(LKG))+(Y*DCOS(LKG)))
                X=XNEW
                Y=YNEW
                        ELSE
                        END IF

C     THE ROTATION IS DONE, NOW SHIFT THE ORIGIN BACK TO THE BOTTOM
C     LEFT HAND CORNER
                        X=X+5000.0D0
                        Y=Y+3500.0D0
C
      IF(I.EQ.STASUR) THEN
C     PUT INSTRUCTIONS IN P1ARAY TO LIFT PEN, GO TO STARTING POINT
      IPST=0
                IX=INT(X)
                IY=INT(Y)
C
      P1ARAY(I,1,1)=IX
      P1ARAY(I,2,1)=IY
      P1ARAY(I,3,1)=0
        IF(.NOT.PLEXIS) PLEXIS=.TRUE.
                ELSE
                END IF
      IF(I.GT.STASUR) THEN
C     PUT INSTRUCTIONS IN P1ARAY TO DROP PEN AND DRAW
      IPST=1
      IX=INT(X)
      IY=INT(Y)
C     IF RAY IS VIRTUAL AND NOVIRT IS TRUE, LIFT PEN ANYWAY
      IF(NOVIRT.AND.GLVIRT(I).AND.I.NE.STPSUR.AND.DF2.EQ.1) THEN
C     LEAVE PEN ALONE AT LAST POSITION WITH PEN UP
      IPST=0
      IF(I.NE.0) P1ARAY(I,1,1)=P1ARAY(I-1,1,1)
      IF(I.NE.0) P1ARAY(I,2,1)=P1ARAY(I-1,2,1)
                   ELSE
C     MOVE TO NEW POSITION WITH LOWERED PEN
      P1ARAY(I,1,1)=IX
      P1ARAY(I,2,1)=IY
      IPST=1
                   END IF
      P1ARAY(I,3,1)=IPST
        IF(.NOT.PLEXIS) PLEXIS=.TRUE.
                END IF
C     IF FINISHED, LIFT PEN
      IF(I.EQ.STPSUR) IPST=0
                        END DO
C
C     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE P1ARAY ARRAY
C
      COLPAS=COLRAY
      CALL MY_COLTYP(COLPAS)
        FIXUP=.FALSE.
                        DO I=STASUR,STPSUR
      IF(I.EQ.STASUR)
     1CALL PENMV1A(P1ARAY(I,1,1),P1ARAY(I,2,1),P1ARAY(I,3,1))
      IF(I.GT.STASUR) THEN
                     GGO=.TRUE.
C     TEST IF LAST POINT IS NEW POINT, IF SO DON'T MOVE
      IF(P1ARAY(I,1,1).EQ.P1ARAY(I-1,1,1).AND.P1ARAY(I,2,1).EQ.
     1P1ARAY(I-1,2,1)) GGO=.FALSE.
      IF(P1ARAY(I,3,1).NE.0.AND.GGO)
     1CALL PENMV1A(P1ARAY(I,1,1),P1ARAY(I,2,1),P1ARAY(I,3,1))
                       END IF
                        END DO
      COLPAS=COLLBL
      CALL MY_COLTYP(COLPAS)
C     HERE WE DO THE PLOT LI AND PLOT AXIS DRAWING
                        IF(.NOT.VIGFLG.AND.PLTVIG) THEN
                        CALL VIGSHO
                        VIGFLG=.TRUE.
                        ELSE
                        END IF
                        RETURN
                        END
C SUB VIE.FOR
        SUBROUTINE VIE(CACOCHVIE)
C

        IMPLICIT NONE

C
C       THIS PROGRAM CONTROLS THE "VIE" AND "VIECO" COMMANDS
C
      REAL*8 VIEROT,VHI,VLO,XVHI,XVLO,YVHI,YVLO
C
      INTEGER DFLAG,VIEXOF,VIEYOF
C
        COMMON/OFFVIE/VIEXOF,VIEYOF,VIEROT
C
      INTEGER COLPAS,VDF1,VDF2,VDF3,VS1,VS2
     1,VS3,I,J,CACOCHVIE
C
      CHARACTER VIEWQ*8
C
      REAL*8 VIEW1,VIEW2,VIEW3,MDX,MDY,SFI,GAMGAM
C
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATMAC.INC'
C       JN ADD HACK to figure out plotting
!        INCLUDE 'DATHGR.INC'
C
        PRINT *, "VIE ROUTINE STARTING..."


        IF(DEVTYP.EQ.0.AND.VIEOVERLAY) VIEOVERLAY=.FALSE.
C       VIG OVERLAY ADDED 4/16/2004
        IF(.NOT.VIEOVERLAY) THEN
                        PLEXIS=.FALSE.
                        DEVTYP=0
                        GRASET=.FALSE.
                        END IF
C
C     W1 IS FOR SF
C     W2 IS FOR I
C     W3 IS FOR J
C
        IF(DF2.EQ.1) THEN
        IF(DABS(ALENS(3,NEWOBJ)).GT.1.0D10) THEN
                        W2=DBLE(NEWOBJ+1)
                        DF2=0
                        S2=1
                        ELSE
                        W2=DBLE(NEWOBJ)
                        DF2=0
                        S2=1
                        END IF
                        ELSE
C       DF2 NOT 1, W2 EXPLICITLY ENTERED
                        END IF
        IF(DF3.EQ.1) THEN
        IF(DABS(ALENS(3,(NEWIMG-1))).GT.1.0D10) THEN
                        W3=DBLE(NEWIMG-1)
                        DF3=0
                        S3=1
                        ELSE
                        W3=DBLE(NEWIMG)
                        DF3=0
                        S3=1
                        END IF
                        ELSE
C       DF3 NOT 1, W3 EXPLICITLY ENTERED
                        END IF
C
C     QUALIFIER WORDS:
C         XZ
C         YZ
C         XY
C         ORTHO
C
      !PRINT *, "W1 is for SF ", W1
      !PRINT *, "W2 is for i ", W2
      !PRINT *, "W3 is for j ", W3

      IF(SQ.EQ.0) THEN
                      SQ=1
                      WQ='YZ      '
                      END IF
        IF(WQ.NE.'XZ'.AND.WQ.NE.'YZ'.AND.WQ.NE.'XY'.AND.
     1WQ.NE.'ORTHO') THEN
      OUTLYNE= 'FOR "VIE" AND "VIECO"'
      CALL SHOWIT(1)
        OUTLYNE=
     1'THE ONLY VALID QUALIFIER WORDS ARE:'
      CALL SHOWIT(1)
        OUTLYNE='"YZ     - FOR YZ-VIEWS'
      CALL SHOWIT(1)
        OUTLYNE='"XZ     - FOR XZ-VIEWS'
      CALL SHOWIT(1)
        OUTLYNE='"XY     - FOR XY-VIEWS'
      CALL SHOWIT(1)
        OUTLYNE='"ORTHO" - FOR ORTHOGRAPHIC'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(STI.EQ.1) THEN
      OUTLYNE= '"VIE" AND "VIECO" PERFOM AUTOMATED LENS PLOTTING'
      CALL SHOWIT(1)
                        RETURN
                        END IF
        IF(DF1.EQ.0.AND.W1.LE.0.0D0) THEN
      OUTLYNE= 'THE "SF" VALUE MUST BE GREATER THAN 0.0'
      CALL SHOWIT(1)
      OUTLYNE= 'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                      CALL MACFAL
                        RETURN
                        END IF
        IF(DF2.EQ.0.AND.W2.LT.0.0D0) THEN
      OUTLYNE=
     1'THE FIRST SURFACE # CAN NOT BE LESS THAN 0'
      CALL SHOWIT(1)
      OUTLYNE= 'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                      CALL MACFAL
                        RETURN
                        END IF
        IF(DF2.EQ.0.AND.W2.GT.SYSTEM(20)) THEN
      OUTLYNE=
     1'THE FIRST SURFACE CAN NOT BE BEYOND THE IMAGE SURFACE'
      CALL SHOWIT(1)
      OUTLYNE= 'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                      CALL MACFAL
                        RETURN
                        END IF
        IF(DF3.EQ.0.AND.W3.LT.0.0D0) THEN
      OUTLYNE=
     1'THE LAST SURFACE # CAN NOT BE LESS THAN 0'
      CALL SHOWIT(1)
      OUTLYNE= 'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                      CALL MACFAL
                        RETURN
                        END IF
        IF(DF3.EQ.0.AND.W3.GT.SYSTEM(20)) THEN
      OUTLYNE=
     1'THE LAST SURFACE CAN NOT BE BEYOND THE IMAGE SURFACE'
      CALL SHOWIT(1)
      OUTLYNE= 'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                      CALL MACFAL
                        RETURN
                        END IF
        IF(DF2.EQ.0.AND.DF3.EQ.0.AND.W2.GE.W3) THEN
      OUTLYNE=
     1'THE FIRST SURFACE MUST BE IN FRONT OF THE SECOND SURFACE'
      CALL SHOWIT(1)
      OUTLYNE= 'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                      CALL MACFAL
                        RETURN
                        END IF
      IF(DF4.EQ.1.OR.DF4.EQ.0.AND.W4.EQ.0.0D0) DFLAG=0
      IF(DF4.EQ.0.AND.W4.NE.0.0D0) DFLAG=1

         !PRINT *, "LINE 2147 W1 = ", W1
        VIEWQ=WQ
        VIEW1=W1
        VIEW2=W2
        VIEW3=W3
          VDF1=DF1
          VDF2=DF2
          VDF3=DF3
          VS1=S1
          VS2=S2
          VS3=S3
C
C     DO A PLOT NEW
C
                        DEVTYP=1
      IF(WC.EQ.'VIE') OUTLYNE='"VIE" IS PROCESSING LENS DATA'
      IF(WC.EQ.'VIECO') OUTLYNE='"VIECO" IS PROCESSING LENS DATA'
      CALL SHOWIT(1)
      OUTLYNE='PLEASE WAIT...'
      CALL SHOWIT(1)

      !PRINT *, "SCALE FACTOR AT START OF VIE IS ", SCFAY
C
C******************************************
C
        IF(.NOT.VIEOVERLAY) THEN
C       NOT A VIE OVERLAY
                        !PRINT *, "VIEOVERLAY IS FALSE"
                        CALL PLTDEV1
                        GRASET=.TRUE.
C     DO A FRAME
                        CALL PLOTBOX
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='PLOT LI'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
                END IF
C
C******************************************
C
C     OFFSETS AND PLOT ROTATION
      IF(VIEXOF.NE.0) THEN
              SAVE_KDP(1)=SAVEINPT(1)
C             XSHIFT
              W1=DBLE(VIEXOF)
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              SN=1
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              WQ='XSHIFT'
              SQ=1
              STI=0
              SST=0
              CALL XYGAMA
              REST_KDP(1)=RESTINPT(1)
              END IF
      IF(VIEYOF.NE.0) THEN
              SAVE_KDP(1)=SAVEINPT(1)
C             XSHIFT
              W1=DBLE(VIEYOF)
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              SN=1
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              WQ='YSHIFT'
              SQ=1
              STI=0
              SST=0
              CALL XYGAMA
              REST_KDP(1)=RESTINPT(1)
              END IF
      IF(VIEROT.NE.0.0D0) THEN
              SAVE_KDP(1)=SAVEINPT(1)
C             XSHIFT
              W1=VIEROT
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              SN=1
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              WQ='GAMMA'
              SQ=1
              STI=0
              SST=0
              CALL XYGAMA
              REST_KDP(1)=RESTINPT(1)
              END IF
C
C     SET UP THE SCALE FACTOR
      !PRINT *, "BEFORE SETTING SCALE FACTOR IN VIE, IT IS ", SCFAY
      !PRINT *, "VDF1 Equals ", VDF1
      !PRINT *, "SYSTEM(6) Equals ", SYSTEM(6)
              IF(VDF1.EQ.0) THEN
                        AUTSL=.FALSE.
                        SCFAYP=1.0D0/VIEW1
                        SCFAXP=1.0D0/VIEW1
                        PSIZYP=VIEW1
                        PSIZXP=VIEW1
      IF(SYSTEM(6).EQ.1.0D0) SCFAY=SCFAYP
      IF(SYSTEM(6).EQ.1.0D0) SCFAX=SCFAXP
      IF(SYSTEM(6).EQ.2.0D0) SCFAY=SCFAYP*2.54D0
      IF(SYSTEM(6).EQ.2.0D0) SCFAX=SCFAXP*2.54D0
      IF(SYSTEM(6).EQ.3.0D0) SCFAY=SCFAYP*25.4D0
      IF(SYSTEM(6).EQ.3.0D0) SCFAX=SCFAXP*25.4D0
      IF(SYSTEM(6).EQ.4.0D0) SCFAY=SCFAYP*0.0254
      IF(SYSTEM(6).EQ.4.0D0) SCFAX=SCFAXP*0.0254
      PSIZY=1.0D0/SCFAY
      PSIZX=1.0D0/SCFAX
                        PLSZ=.TRUE.
                        PLSC=.FALSE.
                        ELSE
                        END IF
      !PRINT *, "AFTER SCALE FACTOR DONE, IT IS ", SCFAY
C     SCALE FACTOR DONE
C
C     NOW SET LOOK VECTOR
              SAVE_KDP(1)=SAVEINPT(1)
              IF(VIEWQ.EQ.'YZ') THEN
              INPUT='PLOT LOOK -1 0 0'
              CALL PROCES
                      END IF
              IF(VIEWQ.EQ.'XZ') THEN
              INPUT='PLOT LOOK 0 1 0'
              CALL PROCES
                      END IF
              IF(VIEWQ.EQ.'XY') THEN
              INPUT='PLOT LOOK 0 0 -1'
              CALL PROCES
                      END IF
              IF(VIEWQ.EQ.'ORTHO') THEN
              IF(DF5.EQ.1) INPUT='PLOT VIEW 26.2 232.2'
              CALL PROCES
                      END IF
              REST_KDP(1)=RESTINPT(1)
        IF(.NOT.VIEOVERLAY) THEN
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='PLOT AXIS'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='PLOT YESLOOK'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
                END IF
              !PRINT *, "LINE 2311, SCFAY IS ", SCFAY

C
C  GUT RAY
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='FOB'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=0.0D0
              WW2=0.0D0
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        !PRINT *, "LINE 2329, SCFAY IS ", SCFAY
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              !PRINT *, "LINE 2332, SCFAY IS ", SCFAY
              REST_KDP(1)=RESTINPT(1)
C

              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              PRINT *, "LINE 2338, SCFAY IS ", SCFAY
              REST_KDP(1)=RESTINPT(1)
C
      IF(VIEWQ.EQ.'YZ'.OR.VIEWQ.EQ.'ORTHO'.OR.VIEWQ.EQ.'XY') THEN
C     PLOT PROFY
              SAVE_KDP(1)=SAVEINPT(1)
C             PLOT PROFY
              W1=VIEW2
              W2=VIEW3
              W3=90.0D0
              S1=VS2
              S2=VS3
              S3=1
              S4=0
              S5=0
              SN=1
              DF1=VDF2
              DF2=VDF3
              DF3=0
              DF4=1
              DF5=1
              WQ='PROF'
              SQ=1
              STI=0
              SST=0
              !PRINT *, "CALL PLTPRO1 PROFY"
              !PRINT *, "LINE 2362, SCFAY IS ", SCFAY
              PRINT *, "Before PLTPRO1 , W1,W2, ", W1, ",", W2
              PRINT *, "Before PLTPRO1 , S1,S2, ", S1, ",", S2
              PRINT *, "Before PLTPRO1 , DF1,DF2, ", DF1, ",", DF2


              CALL PLTPRO1
              !PRINT *, "LINE 2364, SCFAY IS ", SCFAY
              REST_KDP(1)=RESTINPT(1)
              END IF
      IF(VIEWQ.EQ.'YZ'.OR.VIEWQ.EQ.'ORTHO'.OR.VIEWQ.EQ.'XY') THEN
C     PLOT EDGEY
              SAVE_KDP(1)=SAVEINPT(1)
C             PLOT EDGEY
              W1=VIEW2
              W2=VIEW3
              S1=VS2
              S2=VS3
              S3=0
              S4=0
              S5=0
              SN=1
              DF1=VDF2
              DF2=VDF3
              DF3=1
              DF4=1
              DF5=1
              WQ='EDGEY'
              SQ=1
              STI=0
              SST=0
              CALL PLTEDG
              REST_KDP(1)=RESTINPT(1)
              END IF
C
      IF(VIEWQ.EQ.'XZ'.OR.VIEWQ.EQ.'ORTHO'.OR.VIEWQ.EQ.'XY') THEN
C     PLOT PROFX
              SAVE_KDP(1)=SAVEINPT(1)
C             PLOT PROFX
              W1=VIEW2
              W2=VIEW3
             W3=0.0D0
              S1=VS2
              S2=VS3
              S3=1
              S4=0
              S5=0
              SN=1
              DF1=VDF2
              DF2=VDF3
              DF3=0
              DF4=1
              DF5=1
              WQ='PROF'
              SQ=1
              STI=0
              SST=0
              PRINT *, "CALL PLTPRO1 PROFX"
              CALL PLTPRO1
              REST_KDP(1)=RESTINPT(1)
              END IF
      IF(VIEWQ.EQ.'XZ'.OR.VIEWQ.EQ.'ORTHO'.OR.VIEWQ.EQ.'XY') THEN
C     PLOT EDGEX
              SAVE_KDP(1)=SAVEINPT(1)
C             PLOT EDGEX
              W1=VIEW2
              W2=VIEW3
              S1=VS2
              S2=VS3
              S3=0
              S4=0
              S5=0
              SN=1
              DF1=VDF2
              DF2=VDF3
              DF3=1
              DF4=1
              DF5=1
              WQ='EDGEX'
              SQ=1
              STI=0
              SST=0
              CALL PLTEDG
              REST_KDP(1)=RESTINPT(1)
              END IF
C
      IF(VIEWQ.EQ.'XY'.OR.VIEWQ.EQ.'ORTHO') THEN
C     PLOT CLAP
              SAVE_KDP(1)=SAVEINPT(1)
C             PLOT CLAP
              W1=VIEW2
              W2=VIEW3
              S1=VS2
              S2=VS3
              S3=0
              S4=0
              S5=0
              SN=1
              DF1=VDF2
              DF2=VDF3
              DF3=1
              DF4=1
              DF5=1
              WQ='CLAP'
              SQ=1
              STI=0
              SST=0
                        SFI=1.0D0
                        DO I=INT(W1),INT(W2)
                   IF(ALENS(127,I).NE.0.0D0) THEN
                       DO J=1,INT(ALENS(127,I))
                   MDX=MULTCLAP(J,1,I)
                   MDY=MULTCLAP(J,2,I)
                   GAMGAM=MULTCLAP(J,2,I)
                   CALL PLTCLP(1,I,SFI,MDX,MDY,GAMGAM)
                       END DO
                       ELSE
                   CALL PLTCLP(1,I,SFI,0.0D0,0.0D0,0.0D0)
                       END IF
                   IF(ALENS(127,I).NE.0.0D0) THEN
                       DO J=1,INT(ALENS(127,I))
                   MDX=MULTCLAP(J,1,I)
                   MDY=MULTCLAP(J,2,I)
                   GAMGAM=MULTCLAP(J,3,I)
                   CALL PLTCLP(2,I,SFI,MDX,MDY,GAMGAM)
                       END DO
                       ELSE
                   CALL PLTCLP(2,I,SFI,0.0D0,0.0D0,0.0D0)
                       END IF
                   IF(ALENS(128,I).NE.0.0D0) THEN
                       DO J=1,INT(ALENS(128,I))
                   MDX=MULTCOBS(J,1,I)
                   MDY=MULTCOBS(J,2,I)
                   GAMGAM=MULTCOBS(J,3,I)
                   CALL PLTCOB(I,MDX,MDY,GAMGAM)
                       END DO
                       ELSE
                   CALL PLTCOB(I,0.0D0,0.0D0,0.0D0)
                       END IF
                        END DO
              REST_KDP(1)=RESTINPT(1)
              END IF
C
C     OTHER RAYS
C
C     Y FIELDS OF VIEW ARE DONE WHEN VIEW IS YZ, XY OR ORTHO
      IF(VIEWQ.EQ.'YZ'.OR.VIEWQ.EQ.'ORTHO'.OR.VIEWQ.EQ.'XY') THEN
C     DO YZ AND XZ PLANE RAYS
C
C     FIRST FOB 0
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='FOB'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
      IF(.NOT.VIGOFF) THEN
      CALL VIGCAL(10,VHI,VLO,2)
      YVHI=VHI
      YVLO=VLO
                   ELSE
      YVHI=1.0D0
      YVLO=-1.0D0
                   END IF
      IF(.NOT.VIGOFF) THEN
      CALL VIGCAL(10,VHI,VLO,1)
      XVHI=VHI
      XVLO=VLO
                   ELSE
      XVHI=1.0D0
      XVLO=-1.0D0
                   END IF
              SAVE_KDP(1)=SAVEINPT(1)
C     GUT RAY A SECOND TIME
              WW1=0.0D0
              WW2=0.0D0
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C     FIRST MARGINAL RAYS
C     PLUS Y-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=YVHI
              WW2=0.0D0
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C     MINUS Y-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=YVLO
              WW2=0.0D0
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
      IF(VIEWQ.NE.'YZ') THEN
C     PLUS X-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=0.0D0
              WW2=XVHI
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C     MINUS X-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=0.0D0
              WW2=XVLO
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
               END IF
C     TOP Y FOV
C     FIRST FOB 1
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='FOB 1'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
      IF(.NOT.VIGOFF) THEN
      CALL VIGCAL(10,VHI,VLO,2)
      YVHI=VHI
      YVLO=VLO
                   ELSE
      YVHI=1.0D0
      YVLO=-1.0D0
                   END IF
      IF(.NOT.VIGOFF) THEN
      CALL VIGCAL(10,VHI,VLO,1)
      XVHI=VHI
      XVLO=VLO
                   ELSE
      XVHI=1.0D0
      XVLO=-1.0D0
                   END IF
C
C     FIRST MARGINAL RAYS
C     GUT RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=0.0D0
              WW2=0.0D0
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C
C     MARGINAL RAYS
C     PLUS Y-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=YVHI
              WW2=0.0D0
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C     MINUS Y-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=YVLO
              WW2=0.0D0
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
      IF(VIEWQ.NE.'YZ') THEN
C     PLUS X-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=0.0D0
              WW2=XVHI
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C     MINUS X-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=0.0D0
              WW2=XVLO
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
               END IF
C     BOTTOM Y FOV
C     FOB -1
              SAVE_KDP(1)=SAVEINPT(1)
      IF(VSYM) INPUT='FOB -1'
      IF(.NOT.VSYM) INPUT='FOB 0.7'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
      IF(.NOT.VIGOFF) THEN
      CALL VIGCAL(10,VHI,VLO,2)
      YVHI=VHI
      YVLO=VLO
                   ELSE
      YVHI=1.0D0
      YVLO=-1.0D0
                   END IF
      IF(.NOT.VIGOFF) THEN
      CALL VIGCAL(10,VHI,VLO,1)
      XVHI=VHI
      XVLO=VLO
                   ELSE
      XVHI=1.0D0
      XVLO=-1.0D0
                   END IF
C
C     FIRST MARGINAL RAYS
C     GUT RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=0.0D0
              WW2=0.0D0
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C
C     MARGINAL RAYS
C     PLUS Y-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=YVHI
              WW2=0.0D0
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C     MINUS Y-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=YVLO
              WW2=0.0D0
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
      IF(VIEWQ.NE.'YZ') THEN
C     PLUS X-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=0.0D0
              WW2=XVHI
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C     MINUS X-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=0.0D0
              WW2=XVLO
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
               END IF
              ELSE
C     NOT YZ PLANE FIELDS OF VIEW
              END IF
      IF(VIEWQ.EQ.'XZ'.OR.VIEWQ.EQ.'ORTHO'.OR.VIEWQ.EQ.'XY') THEN
C     DO XZ PLANE FIELDS
C
C     FIRST FOB 0
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='FOB'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
      IF(.NOT.VIGOFF) THEN
      CALL VIGCAL(10,VHI,VLO,2)
      YVHI=VHI
      YVLO=VLO
                   ELSE
      YVHI=1.0D0
      YVLO=-1.0D0
                   END IF
      IF(.NOT.VIGOFF) THEN
      CALL VIGCAL(10,VHI,VLO,1)
      XVHI=VHI
      XVLO=VLO
                   ELSE
      XVHI=1.0D0
      XVLO=-1.0D0
                   END IF
C     FIRST MARGINAL RAYS
      IF(VIEWQ.NE.'XZ') THEN
C     PLUS Y-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=YVHI
              WW2=0.0D0
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C     MINUS Y-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=YVLO
              WW2=0.0D0
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
               END IF
C     PLUS X-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=0.0D0
              WW2=XVHI
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C     MINUS X-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=0.0D0
              WW2=XVLO
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C     TOP X FOV
C     FIRST FOB 0 1
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='FOB 0 1'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
      IF(.NOT.VIGOFF) THEN
      CALL VIGCAL(10,VHI,VLO,1)
      XVHI=VHI
      XVLO=VLO
                   ELSE
      XVHI=1.0D0
      XVLO=-1.0D0
                   END IF
      IF(.NOT.VIGOFF) THEN
      CALL VIGCAL(10,VHI,VLO,2)
      YVHI=VHI
      YVLO=VLO
                   ELSE
      YVHI=1.0D0
      YVLO=-1.0D0
                   END IF
C
C     FIRST MARGINAL RAYS
C     GUT RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=0.0D0
              WW2=0.0D0
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C     FIRST MARGINAL RAYS
      IF(VIEWQ.NE.'XZ') THEN
C     PLUS Y-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=YVHI
              WW2=0.0D0
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C     MINUS Y-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=YVLO
              WW2=0.0D0
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
               END IF
C     PLUS X-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=0.0D0
              WW2=XVHI
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C     MINUS X-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=0.0D0
              WW2=XVLO
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C     BOTTOM X FOV
C     FOB 0 -1
              SAVE_KDP(1)=SAVEINPT(1)
      IF(VSYM) INPUT='FOB 0 -1'
      IF(.NOT.VSYM) INPUT='FOB 0 0.7'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
      IF(.NOT.VIGOFF) THEN
      CALL VIGCAL(10,VHI,VLO,1)
      XVHI=VHI
      XVLO=VLO
                   ELSE
      XVHI=1.0D0
      XVLO=-1.0D0
                   END IF
      IF(.NOT.VIGOFF) THEN
      CALL VIGCAL(10,VHI,VLO,2)
      YVHI=VHI
      YVLO=VLO
                   ELSE
      YVHI=1.0D0
      YVLO=-1.0D0
                   END IF
C
C     FIRST MARGINAL RAYS
C     GUT RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=0.0D0
              WW2=0.0D0
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C
C     FIRST MARGINAL RAYS
      IF(VIEWQ.NE.'XZ') THEN
C     PLUS Y-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=YVHI
              WW2=0.0D0
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)

C     MINUS Y-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=YVLO
              WW2=0.0D0
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
               END IF
C     PLUS X-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=0.0D0
              WW2=XVHI
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
C     MINUS X-RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=0.0D0
              WW2=XVLO
              WW3=SYSTEM(11)
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
              ELSE
C     NOT YZ PLANE FIELDS
              END IF
C     NOW CLAPS,PROFS AND EDGES

      ! JN 5/22/22 - Comment out this section as it is giving strange
      ! results with the DoubleGauss.seq test system.  cannot figure out
      ! why so punt for now
!
!       IF(VIEWQ.EQ.'YZ'.OR.VIEWQ.EQ.'ORTHO'.OR.VIEWQ.EQ.'XY') THEN
! C     PLOT PROFY
!               SAVE_KDP(1)=SAVEINPT(1)
! C             PLOT PROFY
!               W1=VIEW2
!               W2=VIEW3
!                W3=90.0D0
!               S1=VS2
!               S2=VS3
!               S3=1
!               S4=0
!               S5=0
!               SN=1
!               DF1=VDF2
!               DF2=VDF3
!               DF3=0
!               DF4=1
!               DF5=1
!               WQ='PROF'
!               SQ=1
!               STI=0
!               SST=0
!               CALL PLTPRO1
!               REST_KDP(1)=RESTINPT(1)
!               END IF
!       IF(VIEWQ.EQ.'YZ'.OR.VIEWQ.EQ.'ORTHO'.OR.VIEWQ.EQ.'XY') THEN
! C     PLOT EDGEY
!               SAVE_KDP(1)=SAVEINPT(1)
! C             PLOT EDGEY
!               W1=VIEW2
!               W2=VIEW3
!               S1=VS2
!               S2=VS3
!               S3=0
!               S4=0
!               S5=0
!               SN=1
!               DF1=VDF2
!               DF2=VDF3
!               DF3=1
!               DF4=1
!               DF5=1
!               WQ='EDGEY'
!               SQ=1
!               STI=0
!               SST=0
!               CALL PLTEDG
!               REST_KDP(1)=RESTINPT(1)
!               END IF
! C
!       IF(VIEWQ.EQ.'XZ'.OR.VIEWQ.EQ.'ORTHO'.OR.VIEWQ.EQ.'XY') THEN
! C     PLOT PROFX
!               SAVE_KDP(1)=SAVEINPT(1)
! C             PLOT PROFX
!               W1=VIEW2
!               W2=VIEW3
!                W3=0.0D0
!               S1=VS2
!               S2=VS3
!               S3=1
!               S4=0
!               S5=0
!               SN=1
!               DF1=VDF2
!               DF2=VDF3
!               DF3=0
!               DF4=1
!               DF5=1
!               WQ='PROF'
!               SQ=1
!               STI=0
!               SST=0
!               CALL PLTPRO1
!               REST_KDP(1)=RESTINPT(1)
!               END IF
!       IF(VIEWQ.EQ.'XZ'.OR.VIEWQ.EQ.'ORTHO'.OR.VIEWQ.EQ.'XY') THEN
! C     PLOT EDGEX
!               SAVE_KDP(1)=SAVEINPT(1)
! C             PLOT EDGEX
!               W1=VIEW2
!               W2=VIEW3
!               S1=VS2
!               S2=VS3
!               S3=0
!               S4=0
!               S5=0
!               SN=1
!               DF1=VDF2
!               DF2=VDF3
!               DF3=1
!               DF4=1
!               DF5=1
!               WQ='EDGEX'
!               SQ=1
!               STI=0
!               SST=0
!               CALL PLTEDG
!               REST_KDP(1)=RESTINPT(1)
!               END IF
! C
!       IF(VIEWQ.EQ.'XY'.OR.VIEWQ.EQ.'ORTHO') THEN
! C     PLOT CLAP
!               SAVE_KDP(1)=SAVEINPT(1)
! C             PLOT CLAP
!               W1=VIEW2
!               W2=VIEW3
!               S1=VS2
!               S2=VS3
!               S3=0
!               S4=0
!               S5=0
!               SN=1
!               DF1=VDF2
!               DF2=VDF3
!               DF3=1
!               DF4=1
!               DF5=1
!               WQ='CLAP'
!               SQ=1
!               STI=0
!               SST=0
!                         SFI=1.0D0
!                         DO I=INT(W1),INT(W2)
!       IF(ALENS(127,I).NE.0.0D0) THEN
!                        DO J=1,INT(ALENS(127,I))
!                    MDX=MULTCLAP(J,1,I)
!                    MDY=MULTCLAP(J,2,I)
!                    GAMGAM=MULTCLAP(J,3,I)
!                    CALL PLTCLP(1,I,SFI,MDX,MDY,GAMGAM)
!                        END DO
!                        ELSE
!                    CALL PLTCLP(1,I,SFI,0.0D0,0.0D0,0.0D0)
!                        END IF
!       IF(ALENS(127,I).NE.0.0D0) THEN
!                        DO J=1,INT(ALENS(127,I))
!                    MDX=MULTCLAP(J,1,I)
!                    MDY=MULTCLAP(J,2,I)
!                    GAMGAM=MULTCLAP(J,3,I)
!                    CALL PLTCLP(2,I,SFI,MDX,MDY,GAMGAM)
!                        END DO
!                        ELSE
!                    CALL PLTCLP(2,I,SFI,0.0D0,0.0D0,0.0D0)
!                        END IF
!                    IF(ALENS(128,I).NE.0.0D0) THEN
!                        DO J=1,INT(ALENS(128,I))
!                    MDX=MULTCOBS(J,1,I)
!                    MDY=MULTCOBS(J,2,I)
!                    GAMGAM=MULTCOBS(J,3,I)
!                    CALL PLTCOB(I,MDX,MDY,GAMGAM)
!                        END DO
!                        ELSE
!                    CALL PLTCOB(I,0.0D0,0.0D0,0.0D0)
!                        END IF
!                         END DO
!               REST_KDP(1)=RESTINPT(1)
!               END IF
!
C     PLOT SCALE FACTOR
C     DO A FRAME
      COLPAS=COLLBL
      CALL MY_COLTYP(COLPAS)
          CALL DOSZ


      IF(DFLAG.EQ.0) THEN
              SAVE_KDP(1)=SAVEINPT(1)

      INPUT='DRAW'
      CALL PROCES
              REST_KDP(1)=RESTINPT(1)
               END IF
                        RETURN
                        END
C SUB INSIDEIT.FOR

        FUNCTION INSIDEIT(I,XONE,YONE,XTWO,YTWO)
C
        IMPLICIT NONE
C
        EXTERNAL INSID1,INSID2
C
        INTEGER I,CAFLG,COFLG,N,ITIIT,III
C
        REAL*8 X,Y,XONE,YONE,XTWO,YTWO,ANGLE,
     1  XR,YR,LS,RS,XRD,YRD,X1,X2,X3,X4,Y1,Y2,Y3,Y4,
     2  X5,X6,X7,X8,Y5,Y6,Y7,Y8,XC1,XC2,XC3,XC4,YC1,YC2,
     3  A22,YC3,YC4,RAD2,MAXSID,CS1,CS2,CS3,CS4
C
        LOGICAL INSIDEIT,INS,INSID1,INSID2,INITIT(1:2)
C
        INTEGER CAERAS,COERAS
C
        COMMON/CACO/CAERAS,COERAS,LS
C
C     VARIABLES FOR SPOT TRACING
C
      LOGICAL TCLPRF,SPDTRA
C
      COMMON/SPRA1/SPDTRA
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATMAI.INC'
C
C       I IS THE CURRENT SURFACE NUMBER
C
C       THIS ROUTINE SETS THE FLAGS
C       CAERAS AND COERAS ARE
C       FLAGS SET IF THE NEXT CURRENT SURFACE HAS A COBS OR
C       CLAP ERASE.
C
        IF(ALENS(16,I).EQ.0.0D0) THEN
                       COFLG=0
C
C       NO CLAPS OR COBS, JUST RETURN
        INSIDEIT=.FALSE.
                        RETURN
                        ELSE
                        COFLG=INT(ALENS(16,I))
C       THERE IS COBS, PROCEED CHECKING
                        END IF
C
                   INSIDEIT=.FALSE.
C
                        DO ITIIT=1,2
                   INITIT(ITIIT)=.FALSE.
      IF(ITIIT.EQ.1) THEN
               X=XONE
               Y=YONE
               ELSE
               X=XTWO
               Y=YTWO
               END IF
C
C******************************************************************
C       COFLG NON-ZERO, RESOLVE ALL COBS BLOCKAGES NOW
C******************************************************************
C
C       COFLG=1 CIRCULAR COBS, DOES IT STOP THE RAY
        IF(COFLG.EQ.1) THEN
C
                        LS=0.0D0
C       CIRCULAR COBS EQUATION IS:
C
C       (X)**2 + (Y)**2 = R**2
C
C       SUB X AND Y IN THE LEFT SIDE. IF THE LEFT SIDE IS
C       LESS THAN OR EQUAL TO THE RIGHT SIDE, THE RAY IS BLOCKED
C       ELSE IT IS NOT BLOCKED.
C
C       IF NON-ZERO COBS DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE DECENTERED
C       OBSCURATION. REMEMBER.
C
        XR=X-ALENS(20,I)
        YR=Y-ALENS(19,I)
C
        LS=DSQRT((XR**2)+(YR**2))
C
        RS=DSQRT(ALENS(17,I)**2)-AIMTOL
        IF(REAL(LS).LT.REAL(RS)) THEN
C       RAY IS BLOCKED BY COBS ON SURFACE I
                        LS=10.0D0
                        INITIT(ITIIT)=.TRUE.
                        ELSE
                        LS=0.0D0
                        INITIT(ITIIT)=.FALSE.
                        END IF
C       COFLG NOT 1
                        END IF
C
C       COFLG=2 RECTANGULAR OBSCURATION, DOES IT STOP THE RAY
        IF(COFLG.EQ.2) THEN
C
                        LS=0.0D0
                        INITIT(ITIIT)=.FALSE.
C
C       IN THE COORDINATE SYSTEM OF THE RECTANGLE, THE CORNER
C       COORDINATES ARE:
                        X1=-ALENS(18,I)-AIMTOL
                        Y1=ALENS(17,I)+AIMTOL
                        X2=-ALENS(18,I)-AIMTOL
                        Y2=-ALENS(17,I)-AIMTOL
                        X3=ALENS(18,I)+AIMTOL
                        Y3=-ALENS(17,I)-AIMTOL
                        X4=ALENS(18,I)+AIMTOL
                        Y4=ALENS(17,I)+AIMTOL
C
        XRD=X
        YRD=Y

C       IF NON-ZERO COBS DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
C       OBSCURATION. REMEMBER.
C
        XRD=XRD-ALENS(20,I)
        YRD=YRD-ALENS(19,I)
C
C       IF A NON-ZERO COBS TILT EXISTS, IT MUST BE CONSIDERED
C       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
C       OBSCURATION. REMEMBER. GAMMA ROTATION OF A CLAP OR
C       COBS HAS THE SAME SIGN AS IN A SURFACE ROTATION

      A22=ALENS(22,I)*PII/180.0D0
        XR=(XRD*DCOS(A22))+(YRD*DSIN(A22))
        YR=(YRD*DCOS(A22))-(XRD*DSIN(A22))
C
C
C
C       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
C       IF YES, SET LS=10.0D0
C       IF NO SET LS=0.0D0
                        XT(1)=X1
                        YT(1)=Y1
                        XT(2)=X2
                        YT(2)=Y2
                        XT(3)=X3
                        YT(3)=Y3
                        XT(4)=X4
                        YT(4)=Y4
                        NP=4
                        X0=XR
                        Y0=YR
        INS=INSID2()
        IF(INS) THEN
C       RAY  BLOCKED
                        LS=10.0D0
                        INITIT(ITIIT)=.TRUE.
                        ELSE
                        LS=0.0D0
                        INITIT(ITIIT)=.FALSE.
C       RAY NOT BLOCKED
                        END IF
C
C       COFLG NOT 2
                        END IF
C
C       COFLG=3 ELLIPTICAL COBS, DOES IT STOP THE RAY
        IF(COFLG.EQ.3) THEN
C
                        LS=0.0D0
C       ELLIPTICAL COBS EQUATION IS:
C
C       (X)**2/A**2  + (Y)**2/B**2 = 1
C
C       SUB X AND Y IN THE LEFT SIDE. IF THE LEFT SIDE IS
C       LESS THAN 1.0D0, THE RAY IS BLOCKED
C       ELSE IT IS NOT BLOCKED.
C
        XRD=X
        YRD=Y

C       IF NON-ZERO COBS DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
C       OBSCURATION. REMEMBER.
C
        XRD=XRD-ALENS(20,I)
        YRD=YRD-ALENS(19,I)
C
C       IF A NON-ZERO COBS TILT EXISTS, IT MUST BE CONSIDERED
C       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
C       OBSCURATION. REMEMBER. GAMMA ROTATION OF A CLAP OR
C       COBS HAS THE SAME SIGN AS IN A SURFACE ROTATION

      A22=ALENS(22,I)*PII/180.0D0
        XR=(XRD*DCOS(A22))+(YRD*DSIN(A22))
        YR=(YRD*DCOS(A22))-(XRD*DSIN(A22))
C
C
        LS=((XR**2)/(ALENS(18,I)**2))+
     1     ((YR**2)/(ALENS(17,I)**2))
C
        IF(REAL(LS).LT.(1.0-(AIMTOL**2))) THEN
C       RAY IS BLOCKED BY COBS ON SURFACE I
                        LS=10.0D0
                        INITIT(ITIIT)=.TRUE.
                        ELSE
                        LS=0.0D0
                        INITIT(ITIIT)=.FALSE.
                        END IF
C       COFLG NOT 3
                        END IF
C
C
C       COFLG=4 RACETRACK COBS, DOES IT STOP THE RAY
        IF(COFLG.EQ.4) THEN
C
                        LS=0.0D0
C
C       IF THE RADIUS OF THE CORNERS IS LESS THAN THE LARGER
C       OF THE TWO SIDE DIMENSIONS, AN 8 SIDED POLYGON NEEDS TO BE
C       CHECKED AS WELL AS THE FOUR CIRCLES. IF NOT, THE BASE RECTANGE
C       AND THE FOUR CIRCLES NEED TO BE CHECKED.
C
        IF(ALENS(17,I).LE.ALENS(18,I)) THEN
C       ALENS(18,I) = MAXSID
                MAXSID=ALENS(18,I)
                        ELSE
                MAXSID=ALENS(17,I)
                        END IF
        IF(ALENS(21,I).LT.MAXSID) THEN
C       SETUP THE 8 SIDED BOX
                        N=8
                        X1=-ALENS(18,I)+ALENS(21,I)+AIMTOL
                        Y1=ALENS(17,I)-AIMTOL
                        X2=-ALENS(18,I)+AIMTOL
                        Y2=ALENS(17,I)-ALENS(21,I)-AIMTOL
                        X3=-ALENS(18,I)+AIMTOL
                        Y3=-ALENS(17,I)+ALENS(21,I)+AIMTOL
                        X4=-ALENS(18,I)+ALENS(21,I)+AIMTOL
                        Y4=-ALENS(17,I)+AIMTOL
                        X5=ALENS(18,I)-ALENS(21,I)-AIMTOL
                        Y5=-ALENS(17,I)+AIMTOL
                        X6=ALENS(18,I)-AIMTOL
                        Y6=-ALENS(17,I)+ALENS(21,I)+AIMTOL
                        X7=ALENS(18,I)-AIMTOL
                        Y7=ALENS(17,I)-ALENS(21,I)-AIMTOL
                        X8=ALENS(18,I)-ALENS(21,I)-AIMTOL
                        Y8=ALENS(17,I)-AIMTOL
                        ELSE
C       SET UP THE FOUR SIDED BOX
                        N=4
                        X1=-ALENS(18,I)-AIMTOL
                        Y1=ALENS(17,I)+AIMTOL
                        X2=-ALENS(18,I)-AIMTOL
                        Y2=-ALENS(17,I)-AIMTOL
                        X3=ALENS(18,I)+AIMTOL
                        Y3=-ALENS(17,I)-AIMTOL
                        X4=ALENS(18,I)+AIMTOL
                        Y4=ALENS(17,I)+AIMTOL
                        END IF
C
        XRD=X
        YRD=Y
C
C       IF NON-ZERO COBS DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
C       OBSCURATION. REMEMBER.
C
        XRD=XRD-ALENS(20,I)
        YRD=YRD-ALENS(19,I)
C
C       IF A NON-ZERO COBS TILT EXISTS, IT MUST BE CONSIDERED
C       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
C       OBSCURATION. REMEMBER. GAMMA ROTATION OF A CLAP OR
C       COBS HAS THE SAME SIGN AS IN A SURFACE ROTATION

      A22=ALENS(22,I)*PII/180.0D0
        XR=(XRD*DCOS(A22))+(YRD*DSIN(A22))
        YR=(YRD*DCOS(A22))-(XRD*DSIN(A22))
C
C       ARE THE POINTS XR AND YR ON THE POLYGON OR OUTSIDE
C       IF YES, SET LS=10.0D0
C       IF NO SET LS=0.0D0
        IF(N.EQ.4) THEN
                        XT(1)=X1
                        YT(1)=Y1
                        XT(2)=X2
                        YT(2)=Y2
                        XT(3)=X3
                        YT(3)=Y3
                        XT(4)=X4
                        YT(4)=Y4
                        ELSE
                        XT(1)=X1
                        YT(1)=Y1
                        XT(2)=X2
                        YT(2)=Y2
                        XT(3)=X3
                        YT(3)=Y3
                        XT(4)=X4
                        YT(4)=Y4
                        XT(5)=X5
                        YT(5)=Y5
                        XT(6)=X6
                        YT(6)=Y6
                        XT(7)=X7
                        YT(7)=Y7
                        XT(8)=X8
                        YT(8)=Y8
                        END IF
                        NP=N
                        X0=XR
                        Y0=YR
        INS=INSID2()
        IF(INS) THEN
C       RAY BLOCKED
                        LS=10.0D0
                        INITIT(ITIIT)=.TRUE.
                        ELSE
                        LS=0.0D0
                        INITIT(ITIIT)=.FALSE.
C       RAY NOT BLOCKED
                        END IF
C NOW IS THE POINT OUTSIDE OR ON ANY OF THE FOUR CIRCLES
C       CENTER OF THE FIRST CIRCLE IS AT
                XC1=-ALENS(18,I)+ALENS(21,I)
                YC1= ALENS(17,I)-ALENS(21,I)
        CS1=DSQRT(((XR-XC1)**2)+((YR-YC1)**2))
C       CENTER OF THE SECOND CIRCLE IS AT
                XC2= -ALENS(18,I)+ALENS(21,I)
                YC2= -ALENS(17,I)+ALENS(21,I)
        CS2=DSQRT(((XR-XC2)**2)+((YR-YC2)**2))
C       CENTER OF THE THIRD CIRCLE IS AT
                XC3= ALENS(18,I)-ALENS(21,I)
                YC3=-ALENS(17,I)+ALENS(21,I)
        CS3=DSQRT(((XR-XC3)**2)+((YR-YC3)**2))
C       CENTER OF THE FIRST CIRCLE IS AT
                XC4=ALENS(18,I)-ALENS(21,I)
                YC4=ALENS(17,I)-ALENS(21,I)
        CS4=DSQRT(((XR-XC4)**2)+((YR-YC4)**2))
C
        RAD2=DSQRT(ALENS(21,I)**2)-AIMTOL
C
C     RAY BLOCKED BY THE BOX AND CIRCLES
      IF(INS.OR.
     1REAL(CS1).LT.REAL(RAD2).OR.REAL(CS2).LT.REAL(RAD2)
     1.OR.REAL(CS3).LT.REAL(RAD2).OR.REAL(CS4).LT.REAL(RAD2))
     1                  THEN
C       RAY IS BLOCKED BY ONE OF THE CIRCLES
                        LS=10.0D0
                        INITIT(ITIIT)=.TRUE.
                        ELSE
                        LS=0.0D0
                        INITIT(ITIIT)=.FALSE.
                        END IF
C       COFLG NOT 4
                        END IF
C
C       COFLG=5 POLY CLAP, DOES IT STOP THE RAY
        IF(COFLG.EQ.5) THEN
                        LS=0.0D0
C
C       IN THE COORDINATE SYSTEM OF THE POLYGON, THE CORNER
C       NUMBER OF POINTS IS ALENS(18,I), CENTER TO CORNER DISTANCE
C       IS ALENS(17,I). POINTS GO COUNTER CLOCKWISE LOOKING
C       TOWARD THE +Z DIRECTION
C       COORDINATES ARE:
                        ANGLE=0.0D0
                        DO III=1,INT(ALENS(18,I))
        XT(III)=ALENS(17,I)*DCOS(ANGLE+(PII/2.0D0))
        YT(III)=ALENS(17,I)*DSIN(ANGLE+(PII/2.0D0))
        ANGLE=ANGLE+((TWOPII)/ALENS(18,I))
                        END DO
        XRD=X
        YRD=Y
C
C       IF A NON-ZERO COBS TILT EXISTS, IT MUST BE CONSIDERED
C       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED

      A22=ALENS(22,I)*PII/180.0D0
        XR=(XRD*DCOS(A22))+(YRD*DSIN(A22))
        YR=(YRD*DCOS(A22))-(XRD*DSIN(A22))
C
C       IF NON-ZERO CLAP ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
C       CLEAR APERTURE ERASE. REMEMBER.
C
        XR=XR-ALENS(20,I)
        YR=YR-ALENS(19,I)
C
C       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
                        X0=XR
                        Y0=YR
                        NP=INT(ALENS(18,I))
        INS=INSID2()
        IF(INS) THEN
C       RAY BLOCKED
                        INITIT(ITIIT)=.TRUE.
                        LS=10.0D0
                        ELSE
C       RAY NOT BLOCKED
                        INITIT(ITIIT)=.FALSE.
                        LS=0.0D0
                        END IF
C
                        ELSE
C       COFLG NOT 5
                        END IF
                        END DO
          INSIDEIT=.FALSE.
          IF(INITIT(1).AND.INITIT(2))INSIDEIT=.TRUE.
C
                        RETURN
                        END
C SUB PLTPRAE.FOR
        SUBROUTINE PLTPRAE(K)
        USE GLOBALS
C
        IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOT PRAY COMMAND AT THE CMD LEVEL
C
      REAL*8 X,Y,Z,XN,YN,ZN,ROT1X,ROT1Z,ROT2Y,
     1ROT2Z,AX,AY,AZ,AALF,APHI,YMAXI,YMINI,XMAXI,XMINI
     2,XNEW,YNEW,LKG,VIEPH,VIEAL
C
      LOGICAL GGO
C
      INTEGER IX,IY,I,IPST,COLPAS,STOPAT,K
C
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATLEN.INC'
C
C     LOOK.VIEW TRANSFORMS

      ROT1X(AX,AZ,APHI)=((AX*DCOS(APHI))-(AZ*DSIN(APHI)))
      ROT1Z(AX,AZ,APHI)=((AX*DSIN(APHI))+(AZ*DCOS(APHI)))
C
      ROT2Z(AZ,AY,AALF)=((AZ*DCOS(AALF))+(AY*DSIN(AALF)))
      ROT2Y(AZ,AY,AALF)=((-AZ*DSIN(AALF))+(AY*DCOS(AALF)))
C     LNTYPE=0
      LNTYPE=0
C
                        X=0.0D0
                        Y=0.0D0
C
C     DOES A RAY EXIST
      IF(.NOT.GPRAYEXT) THEN
      IF(MSG) THEN
      OUTLYNE='ERROR'
      CALL SHOWIT(1)
      OUTLYNE='NO GENERALIZED PARAXIAL RAYS EXISTS'
      CALL SHOWIT(1)
      OUTLYNE='NO GENERALIZED PARAXIAL RAYS EXISTS TO BE PLOTTED'
      CALL SHOWIT(1)
                        END IF
      IF(F34.NE.1) CALL MACFAL
                        RETURN
                        END IF
      IF(RAYCOD(1).EQ.0)  STOPAT=NEWIMG
      IF(RAYCOD(1).EQ.1)  STOPAT=RAYCOD(2)-1
      IF(RAYCOD(1).EQ.2)  STOPAT=RAYCOD(2)-1
      IF(RAYCOD(1).EQ.3)  STOPAT=0
      IF(RAYCOD(1).EQ.4)  STOPAT=RAYCOD(2)
      IF(RAYCOD(1).EQ.5)  STOPAT=RAYCOD(2)
      IF(RAYCOD(1).EQ.6)  STOPAT=RAYCOD(2)
      IF(RAYCOD(1).EQ.7)  STOPAT=RAYCOD(2)
      IF(RAYCOD(1).EQ.8)  STOPAT=0
      IF(RAYCOD(1).EQ.9)  STOPAT=RAYCOD(2)
      IF(RAYCOD(1).EQ.10) STOPAT=0
      IF(RAYCOD(1).EQ.11) STOPAT=0
      IF(RAYCOD(1).EQ.12) STOPAT=0
      IF(RAYCOD(1).EQ.13) STOPAT=RAYCOD(2)-1
      IF(RAYCOD(1).EQ.14) STOPAT=RAYCOD(2)
      IF(RAYCOD(1).EQ.15) STOPAT=0
      IF(RAYCOD(1).EQ.16) STOPAT=0
      IF(RAYCOD(1).EQ.17) STOPAT=0
      IF(RAYCOD(1).EQ.18) STOPAT=RAYCOD(2)
                VIEPH=(PII/180.0D0)*VIEPHI
                VIEAL=(PII/180.0D0)*VIEALF
C     MAKE SURE YOU HAVE VERTEX DATA WITHOUT OFFSETS AND SET TO
C     THE FIRST SURFACE WITHOUT AN INFINITE THICKNESS
                        GLSURF=-99
                        DO I=NEWIMG,0,-1
      IF(DABS(ALENS(3,I)).LE.1.0D10) GLSURF=I
                        END DO
      IF(GLSURF.EQ.-99) THEN
                        GLOBE=.FALSE.
      OUTLYNE='ALL SURFACES WERE OF INFINITE THICKNESS'
      CALL SHOWIT(1)
      OUTLYNE='NO OPTICAL SYSTEM PLOT COULD BE MADE'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
                        GLOBE=.TRUE.
                        OFFX=0.0D0
                        OFFY=0.0D0
                        OFFZ=0.0D0
                        OFFA=0.0D0
                        OFFB=0.0D0
                        OFFC=0.0D0
                        CALL GLVERT
                        CALL PGLPRY
                        GLOBE=.FALSE.
      IF(MSG) THEN
      OUTLYNE='GENERATING PARAXIAL RAY PLOTTING DATA...'
      CALL SHOWIT(1)
                        END IF
C       CHECK SYNTAX
        IF(SST.EQ.1) THEN
        OUTLYNE=
     1  '"PLOT PRAY" TAKES NO STRING INPUT'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
        OUTLYNE=
     1  '"PLOT "'//WQ//' ONLY TAKES NUMERIC WORDS #1 AND #2 INPUT'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(STI.EQ.1) THEN
                 WRITE(OUTLYNE,800) WQ
      CALL SHOWIT(1)
 800    FORMAT('QUERRY (?) HAS NO MEANING WITH "PLOT"',A8)
                        RETURN
                        ELSE
                        END IF
        IF(DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM(20)+W1
        IF(DF2.EQ.0.AND.W2.LT.0.0D0) W2=SYSTEM(20)+W2
C       DEFAULT VALUES
        IF(DF1.EQ.1) THEN
        IF(DABS(ALENS(3,0)).GT.1.0D10) THEN
                        W1=DBLE(1)
                        ELSE
                        W1=DBLE(0)
                        END IF
                        ELSE
C       DF1 NOT 1, W1 EXPLICITLY ENTERED
                        END IF
        IF(DF2.EQ.1) THEN
        IF(DABS(ALENS(3,(NEWIMG-1))).GT.1.0D10) THEN
                        W2=DBLE(NEWIMG-1)
                        ELSE
                        W2=DBLE(NEWIMG)
                        END IF
                        ELSE
C       DF2 NOT 1, W2 EXPLICITLY ENTERED
                        END IF
               STASUR=INT(W1)
               STPSUR=INT(W2)
      IF(STASUR.LT.NEWOBJ) STASUR=NEWOBJ
      IF(STPSUR.LT.NEWOBJ) STPSUR=NEWOBJ
C
        IF(INT(W1).LT.0) THEN
C       INVALID NUMERIC WORD #1
        OUTLYNE=
     1  'SURFACE NUMBER (NUMERIC WORD #1) IS BEYOND LEGAL RANGE'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(INT(W2).GT.NEWIMG) THEN
C       INVALID NUMERIC WORD #2
        OUTLYNE=
     1  'SURFACE NUMBER (NUMERIC WORD #2) IS BEYOND LEGAL RANGE'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(INT(W2).LE.INT(W1)) THEN
C       W2 LESS THAN OR EQUAL TO W1
        OUTLYNE=
     1  'NUMERIC WORD #2 MUST BE GREATER THAN NUMERIC WORD #1'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
C       ALL INPUT IS OK, KEEP GOING
C       NOW WE HAVE THE REAL WORLD COORDINATES OF THE RAY TO BE PLOTTED
C       THEY ARE STORED IN THE ARRAY GLBRAY PASSED IN A COMMON BLOCK
C       VIA THE "INCLUDE 'DATLEN.INC'" STATEMENT.
C
C     HERE IS WHERE REAL WORK BEGINS. FIRST PROJECT THE 3D WORLD COORDINATES
C     OF THE VERTICIES ONTO THE 2D PLANE INDICATED BY THE PLOT LOOK/ PLOT VIEW
C     DATA. THE DIRECTION COSINES OF THE LOOK VECTOR ARE LOOKX, LOOKY AND
C     LOOKZ. THE TRANSFORMATIONS ARE:
C
      CALL ROT1
C
C     AFTER THIS TRANSFORMATION, THE WORLD X COORDINATE IS PLOTTED IN THE
C     HORIZONTAL X-AXIS OF THE DISPLAY AND THE Y WORLD COORDINATE IS PLOTTED
C     IN THE VERTICAL Y-AXIS OF THE DISPLAY AFTER A CONVERSION TO
C     DEVICE INDEPENDENT COORDINATES ARE MADE.
C
      CALL PLTSC1(XMINI,XMAXI,YMINI,YMAXI)
C
C     HERE IS THE LOOP WHICH PLOTS THE RAYS USING CALLS TO
C     PENMV1A(IX,IY,IPST)
c
      IF(STPSUR.GE.STOPAT) STPSUR=STOPAT
      IF(STASUR.LT.NEWOBJ) STASUR=NEWOBJ
      IF(STPSUR.LT.NEWOBJ) STPSUR=NEWOBJ
               IF(STPSUR.LT.STASUR) STPSUR=STASUR
                        DO I=STASUR,STPSUR
        IF(K.EQ.1) THEN
      X=PGLPRAY(1,I)+REFRY(1,I)
      Y=PGLPRAY(2,I)+REFRY(2,I)
      Z=PGLPRAY(3,I)+REFRY(3,I)
                END IF
        IF(K.EQ.2) THEN
      X=-PGLPRAY(1,I)+REFRY(1,I)
      Y=-PGLPRAY(2,I)+REFRY(2,I)
      Z=PGLPRAY(3,I)+REFRY(3,I)
                END IF
        IF(K.EQ.3) THEN
      X=PGLPRAY(4,I)+REFRY(1,I)
      Y=PGLPRAY(5,I)+REFRY(2,I)
      Z=PGLPRAY(6,I)+REFRY(3,I)
                END IF
        IF(K.EQ.4) THEN
      X=-PGLPRAY(4,I)+REFRY(1,I)
      Y=-PGLPRAY(5,I)+REFRY(2,I)
      Z=PGLPRAY(6,I)+REFRY(3,I)
                END IF
        IF(K.EQ.5) THEN
      X=PGLPRAY(7,I)+REFRY(1,I)
      Y=PGLPRAY(8,I)+REFRY(2,I)
      Z=PGLPRAY(9,I)+REFRY(3,I)
                END IF
        IF(K.EQ.6) THEN
      X=-PGLPRAY(7,I)+REFRY(1,I)
      Y=-PGLPRAY(8,I)+REFRY(2,I)
      Z=PGLPRAY(9,I)+REFRY(3,I)
                END IF
        IF(K.EQ.7) THEN
      X=PGLPRAY(10,I)+REFRY(1,I)
      Y=PGLPRAY(11,I)+REFRY(2,I)
      Z=PGLPRAY(12,I)+REFRY(3,I)
                END IF
        IF(K.EQ.8) THEN
      X=-PGLPRAY(10,I)+REFRY(1,I)
      Y=-PGLPRAY(11,I)+REFRY(2,I)
      Z=PGLPRAY(12,I)+REFRY(3,I)
                END IF
      X=X-XROT
      Y=Y-YROT
      Z=Z-ZROT
      XN=ROT1X(X,Z,VIEPH)
      YN=Y
      ZN=ROT1Z(X,Z,VIEPH)
      X=XN
      Y=YN
      Z=ZN
C
      ZN=ROT2Z(Z,Y,VIEAL)
      YN=ROT2Y(Z,Y,VIEAL)
      XN=X
      X=XN
      Y=YN
      Z=ZN
C
C     RIGHT NOW,COORDINATES ARE IN WORLD COORDINATES
C     CONVERT THEM TO DEVICE INDEPENDENT GRAPHICS COORDINATES IN
C     TWO STEPS.
C
C     THE WORLD X PLOTS TO THE PLOTTER X
C     THE WORLD Y PLOTS TO THE PLOTTER Y
C
C     STEP 1: CONVERT USING AN APPROPRIATE SCALE FACTOR
C               CALCULATING AN APPROPRIATE FACTOR IF NECESSARY
C
      PRINT *, "SCFAY LINE 4033 IS ", SCFAY
      Y=(Y/SCFAY)*1000.0D0
C
      X=(X/SCFAX)*1000.0D0
C
C     STEP 2: APPLY THE SCREEN Y-OFFSET AND SCREEN GAMMA ROTATION
C
C     APPLY THE PLOT LOOK/VIEW Y-OFFSET VALUE
C
      IF(LORIENT) CALL ORSHIFT
      Y=Y+3500.0D0+DBLE(PYSHFT)
      X=X+DBLE(PXSHFT)
C
C     IF THERE ARE X-OFFSETS (JUSTIFICATION) TO APPLY, DO THEM HERE.
C
      IF(RCL.EQ.1.OR.RCL.EQ.-1) THEN
      JUSOFF=500.0D0-((XMINI/SCFAX)*1000.0D0)
                        RCL=-1
                        ELSE
                        END IF
      IF(RCL.EQ.2.OR.RCL.EQ.-2) THEN
                        RCL=-2
                        JUSOFF=5000.0D0
                        ELSE
                        END IF
      IF(RCL.EQ.3.OR.RCL.EQ.-3) THEN
      JUSOFF=9500.0D0-((XMAXI/SCFAX)*1000.0D0)
                        RCL=-3
                        ELSE
                        END IF
C
      X=X+JUSOFF
C
C     IF THERE IS A NON-ZERO GAMMA SPECIFIED IN PLOT LOOK OR PLOT VIEW THEN
C     ROTATE IN GAMMA ON THE SCREEN ABOUT THE CENTER OF THE SCREEN
C     WHICH HAS COORDINATES X=5000.0D0,Y=3500.0D0
C
C     FIRST SHIFT THE COORDINATE ORIGIN TO THE CETER OF THE DISPLAY
C
                X=X-5000.0D0
                Y=Y-3500.0D0
C     THE SCREEN COORDINATE IN REAL*8 IN THE SHIFTED COORDINATE
C     FRAME IS NOW X AND Y

      IF(DBLE(PGAMMA).NE.0.0D0) THEN
      LKG=(PII/180.0D0)*DBLE(PGAMMA)
                XNEW=((X*DCOS(LKG))-(Y*DSIN(LKG)))
                YNEW=((X*DSIN(LKG))+(Y*DCOS(LKG)))
                X=XNEW
                Y=YNEW
                        ELSE
                        END IF

C     THE ROTATION IS DONE, NOW SHIFT THE ORIGIN BACK TO THE BOTTOM
C     LEFT HAND CORNER
                        X=X+5000.0D0
                        Y=Y+3500.0D0
      IF(I.EQ.STASUR) THEN
C     PUT INSTRUCTIONS IN P1ARAY TO LIFT PEN, GO TO STARTING POINT
      IPST=0
                IX=INT(X)
                IY=INT(Y)
C
      P1ARAY(I,1,1)=IX
      P1ARAY(I,2,1)=IY
      P1ARAY(I,3,1)=0
        IF(.NOT.PLEXIS) PLEXIS=.TRUE.
                ELSE
                END IF
      IF(I.GT.STASUR) THEN
C     PUT INSTRUCTIONS IN P1ARAY TO DROP PEN AND DRAW
      IPST=1
      IX=INT(X)
      IY=INT(Y)
C     IF RAY IS VIRTUAL AND NOVIRT IS TRUE, LIFT PEN ANYWAY
      IF(NOVIRT.AND.GLVIRT(I).AND.I.NE.STPSUR.AND.DF3.EQ.1) THEN
C     LEAVE PEN ALONE AT LAST POSITION WITH PEN UP
      IPST=0
      IF(I.NE.0) P1ARAY(I,1,1)=P1ARAY(I-1,1,1)
      IF(I.NE.0) P1ARAY(I,2,1)=P1ARAY(I-1,2,1)
                   ELSE
C     MOVE TO NEW POSITION WITH LOWERED PEN
      P1ARAY(I,1,1)=IX
      P1ARAY(I,2,1)=IY
      IPST=1
                   END IF
      P1ARAY(I,3,1)=IPST
        IF(.NOT.PLEXIS) PLEXIS=.TRUE.
                END IF
C     IF FINISHED, LIFT PEN
      IF(I.EQ.STPSUR) IPST=0
                        END DO
C
C     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE P1ARAY ARRAY
C
      COLPAS=3
      CALL MY_COLTYP(COLPAS)
        FIXUP=.FALSE.
                        DO I=STASUR,STPSUR
      IF(I.EQ.STASUR)
     1CALL PENMV1A(P1ARAY(I,1,1),P1ARAY(I,2,1),P1ARAY(I,3,1))
      IF(I.GT.STASUR) THEN
                     GGO=.TRUE.
C     TEST IF LAST POINT IS NEW POINT, IF SO DON'T MOVE
      IF(P1ARAY(I,1,1).EQ.P1ARAY(I-1,1,1).AND.P1ARAY(I,2,1).EQ.
     1P1ARAY(I-1,2,1)) GGO=.FALSE.
      IF(P1ARAY(I,3,1).NE.0.AND.GGO)
     1CALL PENMV1A(P1ARAY(I,1,1),P1ARAY(I,2,1),P1ARAY(I,3,1))
                       END IF
                        END DO
      COLPAS=COLLBL
      CALL MY_COLTYP(COLPAS)
      NORAYPLOT=.FALSE.
                        RETURN
                        END

C SUB PLTRAYS.FOR
        SUBROUTINE PLTRAYS(CACOCHVIE)
C
        IMPLICIT NONE
C
C       THIS PROGRAM CONTROLS THE "PLOT RAYS"
C
      REAL*8 VIEROT,VHI,VLO,XVHI,XVLO,YVHI,YVLO
C
      INTEGER DFLAG,VIEXOF,VIEYOF
C
        COMMON/OFFVIE/VIEXOF,VIEYOF,VIEROT
C
      INTEGER COLPAS,VDF1,VDF2,VDF3,VS1,VS2
     1,VS3,I,J,CACOCHVIE
C
      CHARACTER VIEWQ*8
C
      REAL*8 RAYS1,RAYS2,RAYS3,RAYS4,RAYS5,MDX,MDY,SFI,GAMGAM
C
      REAL*8 VIEW2,VIEW3,DELTAX,DELTAY
C
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATMAC.INC'
C
C
        IF(DEVTYP.EQ.0) THEN
        WRITE(OUTLYNE,*) 'NO PLOT NEW HAS BEEN ISSUED'
        CALL SHOWIT(1)
        WRITE(OUTLYNE,*) '"PLOT RAYS" CAN NOT PROCEED'
        CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(.NOT.REFEXT) THEN
        WRITE(OUTLYNE,*) 'NO REREFENCE RAY EXISTS'
        CALL SHOWIT(1)
        WRITE(OUTLYNE,*) '"PLOT RAYS" CAN NOT PROCEED'
        CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
C
C     W1 IS FOR I
C     W2 IS FOR J
C     W3 IS NUMBER OF RAYS
C     W4 IS THE WAVELENGTH NUMBER
C     W5 IS ORIENTATION FLAG, 0 FOR YZ, 1 FOR XZ
C
        IF(DF1.EQ.1) THEN
        IF(DABS(ALENS(3,NEWOBJ)).GT.1.0D10) THEN
                        W1=DBLE(NEWOBJ+1)
                        DF1=0
                        S1=1
                        ELSE
                        W1=DBLE(NEWOBJ)
                        DF1=0
                        S1=1
                        END IF
                        ELSE
C       DF1 NOT 1, W1 EXPLICITLY ENTERED
                        END IF
        IF(DF2.EQ.1) THEN
        IF(DABS(ALENS(3,(NEWIMG-1))).GT.1.0D10) THEN
                        W2=DBLE(NEWIMG-1)
                        DF2=0
                        S2=1
                        ELSE
                        W2=DBLE(NEWIMG)
                        DF2=0
                        S2=1
                        END IF
                        ELSE
C       DF2 NOT 1, W2 EXPLICITLY ENTERED
                        END IF
        IF(DF3.EQ.1) THEN
                        W3=11.0D0
                        DF3=0
                        S3=1
                        ELSE
C       DF3 NOT 1, W3 EXPLICITLY ENTERED
                        END IF
                        W3=INT(W3)
        IF(W3.LE.3.0D0) W3=11.0D0
        IF(DF4.EQ.1) THEN
                        W4=SYSTEM(11)
                        DF4=0
                        S4=1
                        ELSE
C       DF4 NOT 1, W4 EXPLICITLY ENTERED
                        END IF
                        W4=INT(W4)
        IF(W4.LT.1.0D0.OR.W4.GT.10.0D0) W4=SYSTEM(11)
C
        IF(STI.EQ.1) THEN
      OUTLYNE= '"PLOT RAYS" PERFOMS AUTOMATED RAY FAN PLOTTING'
      CALL SHOWIT(1)
                        RETURN
                        END IF
        IF(DF1.EQ.0.AND.W1.LT.0.0D0) THEN
      OUTLYNE=
     1'THE FIRST SURFACE # CAN NOT BE LESS THAN 0'
      CALL SHOWIT(1)
      OUTLYNE= 'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                      CALL MACFAL
                        RETURN
                        END IF
        IF(DF1.EQ.0.AND.W1.GT.SYSTEM(20)) THEN
      OUTLYNE=
     1'THE FIRST SURFACE CAN NOT BE BEYOND THE IMAGE SURFACE'
      CALL SHOWIT(1)
      OUTLYNE= 'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                      CALL MACFAL
                        RETURN
                        END IF
        IF(DF2.EQ.0.AND.W2.LT.0.0D0) THEN
      OUTLYNE=
     1'THE LAST SURFACE # CAN NOT BE LESS THAN 0'
      CALL SHOWIT(1)
      OUTLYNE= 'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                      CALL MACFAL
                        RETURN
                        END IF
        IF(DF2.EQ.0.AND.W2.GT.SYSTEM(20)) THEN
      OUTLYNE=
     1'THE LAST SURFACE CAN NOT BE BEYOND THE IMAGE SURFACE'
      CALL SHOWIT(1)
      OUTLYNE= 'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                      CALL MACFAL
                        RETURN
                        END IF
        IF(DF1.EQ.0.AND.DF2.EQ.0.AND.W1.GE.W2) THEN
      OUTLYNE=
     1'THE FIRST SURFACE MUST BE IN FRONT OF THE SECOND SURFACE'
      CALL SHOWIT(1)
      OUTLYNE= 'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                      CALL MACFAL
                        RETURN
                        END IF
        IF(INT(W5).NE.0.AND.INT(W5).NE.1.AND.INT(W5).NE.2) THEN
      OUTLYNE=
     1'FAN TYPE MUST BE 0 - YZ, 1 - XZ'
      CALL SHOWIT(1)
      OUTLYNE= 'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                      CALL MACFAL
                        RETURN
                        END IF
C       SET DRAW FLAG TO 0 OR NO.
      DFLAG=0
C
        RAYS1=W1
        RAYS2=W2
        RAYS3=W3
        RAYS4=W4
        RAYS5=W5
        VIEW2=RAYS1
        VIEW3=RAYS2
        VDF2=0
        VDF3=0
        VS2=1
        VS3=1
C
      IF(.NOT.VIGOFF) THEN
      CALL VIGCAL(10,VHI,VLO,2)
      YVHI=VHI
      YVLO=VLO
                   ELSE
      YVHI=1.0D0
      YVLO=-1.0D0
                   END IF
      IF(.NOT.VIGOFF) THEN
      CALL VIGCAL(10,VHI,VLO,1)
      XVHI=VHI
      XVLO=VLO
                   ELSE
      XVHI=1.0D0
      XVLO=-1.0D0
                   END IF
C     NOW TRACE THE RAYS
C
      IF(RAYS5.EQ.0.0D0) THEN
C     YZ-PLANE
C
      DELTAY=(YVHI-YVLO)/(RAYS3-1.0D0)
              WW1=YVHI
              DO I=1,INT(RAYS3)
              SAVE_KDP(1)=SAVEINPT(1)
              WW2=0.0D0
              WW3=RAYS4
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
              CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
        IF(I.EQ.INT(RAYS3)) THEN
              WW1=YVLO
              ELSE
              WW1=WW1-DELTAY
              END IF
                        END DO
              END IF
      IF(RAYS5.EQ.1.0D0) THEN
C     XZ-PLANE
C
      DELTAX=(XVHI-XVLO)/(RAYS3-1.0D0)
              WW2=XVHI
              DO I=1,INT(RAYS3)
              SAVE_KDP(1)=SAVEINPT(1)
              WW1=0.0D0
              WW3=RAYS4
              WVN=WW3
              MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
              GRASET=.TRUE.
              CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
        IF(I.EQ.INT(RAYS3)) THEN
              WW2=XVLO
              ELSE
              WW2=WW2-DELTAX
              END IF
                        END DO
              END IF
      IF(RAYS5.EQ.2.0D0) THEN
C     XZ-PLANE
C
      DELTAY=(YVHI-YVLO)/(RAYS3-1.0D0)
      DELTAX=(XVHI-XVLO)/(RAYS3-1.0D0)
              WW2=XVHI
              DO J=1,INT(RAYS3)
C       X LOOP
              WW1=YVHI
              DO I=1,INT(RAYS3)
C       Y LOOP
              SAVE_KDP(1)=SAVEINPT(1)
              WW3=RAYS4
              WVN=WW3
              MSG=.FALSE.
              WW4=1.0D0
              NOCOAT=.TRUE.
              GRASET=.TRUE.
              CACOCH=1
              CALL RAYTRA
              REST_KDP(1)=RESTINPT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
        IF(I.EQ.INT(RAYS3)) THEN
              WW1=YVLO
              ELSE
              WW1=WW1-DELTAY
              END IF
                        END DO
        IF(I.EQ.INT(RAYS3)) THEN
              WW2=XVLO
              ELSE
              WW2=WW2-DELTAX
              END IF
                        END DO
              END IF
                        RETURN
                        END
