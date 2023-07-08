C SUB FOBA.FOR
        SUBROUTINE FOBA
        USE GLOBALS
C
        IMPLICIT NONE
C
C       THIS IS SUBROUTINE FOBA.FOR. THIS SUBROUTINE IMPLEMENTS
C       THE CMD LEVEL COMMAND FOBA.
C
      CHARACTER LUNI*3
C
      REAL*8 XPFOB,YPFOB
      COMMON/PFOB/XPFOB,YPFOB,LUNI
C
      REAL*8 YAYA,NWW1,NWW2,NWW3
C
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATSPD.INC'
C
C
        FOBRUN=.TRUE.
C
      DLLX=0.0D0
      DLLY=0.0D0
      DLLZ=0.0D0
C
C     IS ANGLE INPUT NODE ON OR NOT?
        IF(STI.EQ.1) THEN
        OUTLYNE='"FOBA" DEFINES AN OBJECT POSITION BY ANGULAR INPUT'
      CALL SHOWIT(1)
        OUTLYNE='IN DEGREES'
      CALL SHOWIT(1)
        OUTLYNE='INPUTS ARE Y-ANGLE, X-ANGLE AND WAVELENGTH NUMBER'
      CALL SHOWIT(1)
                        RETURN
                        END IF
        IF(SST.EQ.1) THEN
        OUTLYNE='"FOBA" TAKES NO STRING INPUT'
      CALL SHOWIT(1)
                           CALL MACFAL
                        RETURN
                        END IF
C       CHECK FOR NATURE OF QUALIFIER WORD
        IF(SQ.EQ.1) THEN
        IF(WQ.NE.'NULL'.AND.WQ.NE.'P'.AND.WQ.NE.
     1  'PIC'.AND.WQ.NE.'PICNH'.AND.WQ.NE.'PFS'
     2  .AND.WQ.NE.'PFSNH') THEN
        OUTLYNE='INVALID QUALIFIER USED WITH "FOBA"'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
C       NO QUALIFIER,PROCEED
                        END IF
        IF(S4.EQ.1.OR.S5.EQ.1) THEN
        OUTLYNE='"FOBA" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
      CALL SHOWIT(1)
                           CALL MACFAL
                        RETURN
                        END IF
C       CHECK FOR LEGAL WAVELENGTH BOUNDS
      IF(DF1.EQ.1) W1=0.0D0
      IF(DF2.EQ.1) W2=0.0D0
      IF(DF3.EQ.1) W3=SYSTEM(11)
        IF(INT(W3).NE.1.AND.
     1INT(W3).NE.2.AND.
     1INT(W3).NE.3.AND.
     1INT(W3).NE.4.AND.
     1INT(W3).NE.5.AND.
     1INT(W3).NE.6.AND.
     1INT(W3).NE.7.AND.
     1INT(W3).NE.8.AND.
     1INT(W3).NE.9.AND.
     1INT(W3).NE.10) THEN
        OUTLYNE='WAVELENGTH NUMBER BEYOND LEGAL BOUNDS'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
      IF(SYSTEM(18).EQ.1.0D0.AND.SYSTEM(19).EQ.1.0D0) THEN
      IF(DABS(W1).GT.180.0D0) THEN
        OUTLYNE='Y-INPUT ANGLE MAY NOT EXCEED +/- 180 DEGREES'
      CALL SHOWIT(1)
        OUTLYNE='WHEN REF. OBJ. HT. IS SET WITH SCY FANG/SCX FANG'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                           END IF
      IF(DABS(W2).GT.180.0D0) THEN
        OUTLYNE='X-INPUT ANGLE MAY NOT EXCEED +/- 180 DEGREES'
      CALL SHOWIT(1)
        OUTLYNE='WHEN REF. OBJ. HT. IS SET WITH SCY FANG/SCX FANG'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                           END IF
                           END IF
      IF(SYSTEM(18).EQ.0.0D0.AND.SYSTEM(19).EQ.0.0D0) THEN
      IF(DABS(W1).GE.90.0D0) THEN
        OUTLYNE='Y-INPUT ANGLE MUST BE LESS THAN 90 DEGREES'
      CALL SHOWIT(1)
        OUTLYNE='WHEN REF. OBJ. HT. IS SET WITH SCY/SCX'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                           END IF
      IF(DABS(W2).GE.90.0D0) THEN
        OUTLYNE='X-INPUT ANGLE MUST BE LESS THAN 90 DEGREES'
      CALL SHOWIT(1)
        OUTLYNE='WHEN REF. OBJ. HT. IS SET WITH SCY/SCX'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                           END IF
                           END IF
      NWW1=W1
      NWW2=W2
      NWW3=W3
C     SAVE INPUT
      LFOBA(1)=W1
      LFOBA(2)=W2
      LFOBA(3)=W3
      IF(SYSTEM(18).EQ.1.0D0.AND.SYSTEM(19).EQ.1.0D0) THEN
C     ANGLE INPUT MODE
      IF(SYSTEM(21).NE.0.0D0) THEN
      W1=W1/SYSTEM(21)
                       ELSE
      W1=0.0D0
                       END IF
      IF(SYSTEM(23).NE.0.0D0) THEN
      W2=W2/SYSTEM(23)
                       ELSE
      W2=0.0D0
                       END IF
                        END IF
      IF(SYSTEM(18).EQ.0.0D0.AND.SYSTEM(19).EQ.0.0D0) THEN
C     LINEAR INPUT MODE
      IF(W1.NE.0.0D0.AND.ALENS(3,NEWOBJ).NE.0.0D0) THEN
      YAYA=DTAN(W1*PII/180.0D0)*ALENS(3,NEWOBJ)
      W1=-YAYA/SYSTEM(14)
                       ELSE
      W1=0.0D0
                       END IF
      IF(W2.NE.0.0D0.AND.ALENS(3,NEWOBJ).NE.0.0D0) THEN
      YAYA=DTAN(W2*PII/180.0D0)*ALENS(3,NEWOBJ)
      W2=-YAYA/SYSTEM(16)
                       ELSE
      W2=0.0D0
                       END IF
                       END IF
      W4=W3
      W3=0.0D0
      WC='FOB'
      WS='        '
      STI=0
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
      LFOB(1)=W1
      LFOB(2)=W2
      LFOB(3)=W3
      LFOB(4)=W4
      LFOB(5)=0.0D0
      LFOB(6)=SYSTEM(25)
      LFOB(7)=SYSTEM(20)
                       CALL FFOB
      WC='FOBA'
      SST=0
      S1=1
      S2=1
      S3=1
      S4=0
      S5=0
      DF1=0
      DF2=0
      DF3=0
      DF4=1
      DF5=1
      W1=NWW1
      W2=NWW2
      W3=NWW3
      W4=0.0D0
      W5=0.0D0
                       RETURN
                        END
C SUB FFOBH.FOR
        SUBROUTINE FFOBH
        USE GLOBALS
C
        IMPLICIT NONE
C
C       THIS IS SUBROUTINE FFOBH.FOR. THIS SUBROUTINE IMPLEMENTS
C       THE CMD LEVEL COMMAND FOBH.
C       NOTE: EVERY TIME A REFERENCE RAY IS TRACED, THE
C       FOB,0 0 0 (CW) NW5, RAY 0 0 1 IS TRACED FIRST SO THAT TILT
C       AUTOS ARE SET CORRECTLY
C
C     W5 NOT USED WHEN TEL IS ON
C
      CHARACTER LUNI*3
C
      REAL*8 XPFOB,YPFOB
      COMMON/PFOB/XPFOB,YPFOB,LUNI
C
        INTEGER IISURF,FFT,FFS,I,J,ICNT,ICNTEST
C
         REAL*8 FT,FS,X00,Y00,Z0
     1  ,LAMBDA,ANGLE1,XK,YK,XLN,XMN,XNN,XFOBB0,YFOBB0,ZFOBB0
     2  ,ANGJK1,ANGJK2,XXANG,YYANG,FACTER,XA,YA,ZA,XJIM,YJIM,ZJIM
     3,ZSAG,ANGFACX,ANGFACY,ANGLE2,SCLFACY,SCLFACX,AWW1,AWW2
     4,NWW1,NWW2,NWW3
C
      COMMON/OHFOB/XFOBB0,YFOBB0,ZFOBB0
C
        LOGICAL FANEXT,ZEEERR,SAGERR
        COMMON/ERRZEE/ZEEERR
C
        COMMON/FANEXI/FANEXT
C
C       FOBB0=TRUE FOR ON AXIS AND FALSE FOR OFF AXIS
        LOGICAL FOBB0,FOBB0X,FOBB0Y
C
        COMMON/FFOOBB/FOBB0,FOBB0X,FOBB0Y
C
      COMMON/FBDIFF/FS,FT,FFS,FFT
C
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATSPD.INC'
        FOBRUN=.TRUE.
C
      DLLX=0.0D0
      DLLY=0.0D0
      DLLZ=0.0D0
      ANGFACY=0.0D0
      SCLFACY=1.0D0
      ANGFACX=0.0D0
      SCLFACX=1.0D0
C     ALWAYS USE OBJECT HEIGHTS FOR "FOBH"
      AWW1=W1/SCLFACY
      AWW2=W2/SCLFACX
C     CONVERT W1, W2 AND W3 FROM REAL LENS UNITS TO
      NWW1=W1
      NWW2=W2
      NWW3=W3
      W1=W1/SYSTEM(14)
      W2=W2/SYSTEM(16)
      W3=W3/ALENS(3,NEWOBJ)
C     FRACTIONALS WHICH CAN BE PROCESSED
C
C       CHECK FOR STRING INPUT
        IF(STI.EQ.1) THEN
      OUTLYNE=
     1'"FOB" TYPE COMMANDS DEFINE AN OBJECT POSITION FOR RAY TRACING'
      CALL SHOWIT(1)
      IF(REFEXT) THEN
        OUTLYNE='THE LAST OBJECT POSITION DATA INPUT WAS:'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,11) LFOB(1)
      CALL SHOWIT(1)
      WRITE(OUTLYNE,12) LFOB(2)
      CALL SHOWIT(1)
      WRITE(OUTLYNE,13) LFOB(3)
      CALL SHOWIT(1)
      WRITE(OUTLYNE,14) INT(LFOB(4))
      CALL SHOWIT(1)
      WRITE(OUTLYNE,15) INT(LFOB(5))
      CALL SHOWIT(1)
      WRITE(OUTLYNE,16) INT(LFOB(6))
      CALL SHOWIT(1)
      WRITE(OUTLYNE,17) INT(LFOB(7))
      CALL SHOWIT(1)
                   END IF
 11   FORMAT('               Y-INPUT VALUE = ',1PD23.15)
 12   FORMAT('               X-INPUT VALUE = ',1PD23.15)
 13   FORMAT('               Z-INPUT VALUE = ',1PD23.15)
 14   FORMAT('REF. WAVELENGTH NUMBER INPUT VALUE = ',I2)
 15   FORMAT('   OBJ. SURFACE NUMBER INPUT VALUE = ',I3)
 16   FORMAT('   REF. SURFACE NUMBER INPUT VALUE = ',I3)
 17   FORMAT('   IMG. SURFACE NUMBER INPUT VALUE = ',I3)
                        RETURN
                        END IF
C       CHECK FOR STRING INPUT
        IF(SST.EQ.1) THEN
        OUTLYNE='"FOBH" TAKES NO STRING INPUT'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
C     OBJECT ANGLES NOT INPUT
      ANGIN=.FALSE.
C
      IF(ALENS(9,NEWOBJ).EQ.0.OR.ALENS(127,NEWOBJ).NE.0.0D0) THEN
C     THE STARTING RAY COORDINATES ON THE OBJECT SURAFCE
C     ARE NOT DETERMINED BY THE CLAP ON THE OBJECT SURFACE
C     USING SCY AND SCX, FOB REPRESENTS FRACTIONS OF IMAGE HEIGHT
C     AND DEPTH.
      IF(SYSTEM(16).NE.0.0D0) XSTRT=W2*PXTRAX(5,NEWOBJ)
      IF(SYSTEM(16).EQ.0.0D0) XSTRT=0.0D0
      IF(SYSTEM(14).NE.0.0D0) YSTRT=W1*PXTRAY(5,NEWOBJ)
      IF(SYSTEM(14).EQ.0.0D0) YSTRT=0.0D0
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
      IF(SAGERR) CALL HANDLESAGRETFAILURE(MSG, NEWOBJ)
        ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
                        ELSE
C     USE Y HEIGHT OF CLAP WITH OFFSETS
      IF(ALENS(9,NEWOBJ).EQ.1.0D0.AND.ALENS(127,NEWOBJ).EQ.0.0D0) THEN
      IF(ALENS(10,NEWOBJ).LE.ALENS(11,NEWOBJ)) THEN
      XSTRT=W2*(ALENS(10,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(10,NEWOBJ)+ALENS(12,NEWOBJ))
                           ELSE
      XSTRT=W2*(ALENS(11,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(11,NEWOBJ)+ALENS(12,NEWOBJ))
                           END IF
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
      IF(SAGERR) CALL HANDLESAGRETFAILURE(MSG, NEWOBJ)
      ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
                        END IF
      IF(ALENS(9,NEWOBJ).EQ.5.0D0.AND.ALENS(127,NEWOBJ).EQ.0.0D0) THEN
      XSTRT=W2*(ALENS(10,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(10,NEWOBJ)+ALENS(12,NEWOBJ))
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)

      ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
                        END IF
      IF(ALENS(9,NEWOBJ).GT.1.0D0.AND.ALENS(9,NEWOBJ).LT.5.0D0.AND.
     1ALENS(127,NEWOBJ).EQ.0.0D0) THEN
      XSTRT=W2*(ALENS(11,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(10,NEWOBJ)+ALENS(12,NEWOBJ))
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
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
      ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
                        END IF
                        END IF
C     NOW THE INITIAL AIMING POINT AT NEWOBJ+1
        X1AIM=PXTRAX(5,(NEWOBJ+1))
        X1AIM=(X1AIM*W2)-ALENS(31,NEWOBJ+1)+SYSTEM(81)
        Y1AIM=PXTRAY(5,(NEWOBJ+1))
        Y1AIM=(Y1AIM*W1)-ALENS(30,NEWOBJ+1)+SYSTEM(82)
      IISURF=NEWOBJ+1
      CALL SAGRET(IISURF,X1AIM,Y1AIM,ZSAG,SAGERR)
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
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
        Z1AIM=SYSTEM(89)+ZSAG
      TRYX=X1AIM
      TRYY=Y1AIM
      TRYZ=Z1AIM
      XC=TRYX
      YC=TRYY
      ZC=TRYZ
      ZEEERR=.FALSE.
      PRINT *, "TRYY 451 IS ", TRYY
      IF(ALENS(1,NEWOBJ+1).NE.0.0D0)
     1CALL GETZEE1
C     STARTING INTERSECTION POINT ON SURF 1 IS
      TRYX=XC
      TRYY=YC
      TRYZ=ZC
C
                        FANEXT=.FALSE.
C
C       INITIALIZE RAYCOD
                RAYCOD(1)=0
                RAYCOD(2)=-1
C
C       SET RAYEXT TO FALSE AND FAIL TO TRUE
                        RAYEXT=.FALSE.
                        POLEXT=.FALSE.
                        FAIL=.TRUE.
C       SET REFEXT AND STOPP
                        STOPP=0
                        REFEXT=.TRUE.
                        SPDEXT=.FALSE.
                        GSPDEXT=.FALSE.
                        CPFNEXT=.FALSE.
                        CALL DELPSF
        IF(SYSTEM(63).EQ.1.AND.S5.EQ.1) THEN
        OUTLYNE='"FOB" TAKES NO NUMERIC WORD #5 INPUT'
      CALL SHOWIT(1)
        OUTLYNE='WHEN TELECENTRIC RAY AIMING IS "ON"'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
C
C       SET DEFAULT NUMERICS
        IF(DF1.EQ.1) W1=0.0D0
        IF(DF2.EQ.1) W2=0.0D0
        IF(DF3.EQ.1) W3=0.0D0
C       THE CONTROL WAVELENGTH
        IF(DF4.EQ.1) W4=SYSTEM(11)
        IF(DF4.EQ.1) WVN=SYSTEM(11)
        IF(DF5.EQ.0) THEN
      OUTLYNE='"FOBH" TAKES NO NUMERIC WORD #5 INPUT'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
C
C       THIS MAKES FOB EQUAL FOB 0 0 0 (CW) 0
C
C       CHECK FOR NATURE OF QUALIFIER WORD
        IF(SQ.EQ.1) THEN
        IF(WQ.NE.'NULL'.AND.WQ.NE.'P'.AND.WQ.NE.
     1  'PIC'.AND.WQ.NE.'PICNH'.AND.WQ.NE.'PFS'
     2  .AND.WQ.NE.'PFSNH') THEN
        OUTLYNE='INVALID QUALIFIER USED WITH "FOBH"'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
C       NO QUALIFIER,PROCEED
                        END IF
        IF(WQ.EQ.'NULL') THEN
                        NULL=.TRUE.
                        ELSE
                        NULL=.FALSE.
                        END IF
C       CHECK FOR LEGAL WAVELENGTH BOUNDS
        IF(INT(W4).NE.1.AND.
     1INT(W4).NE.2.AND.
     1INT(W4).NE.3.AND.
     1INT(W4).NE.4.AND.
     1INT(W4).NE.5.AND.
     1INT(W4).NE.6.AND.
     1INT(W4).NE.7.AND.
     1INT(W4).NE.8.AND.
     1INT(W4).NE.9.AND.
     1INT(W4).NE.10) THEN
        OUTLYNE='WAVELENGTH NUMBER BEYOND LEGAL BOUNDS'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
      IF(W4.GE.1.0D0.AND.W4.LE.5.0D0) THEN
      IF(SYSTEM(INT(W4)).EQ.0.0D0) THEN
                IF(MSG) THEN
      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',NEWOBJ
      CALL SHOWIT(1)
        OUTLYNE=
     1'REFERENCE RAY CAN NOT BE TRACED AT ZERO WAVELENGTH'
      CALL SHOWIT(1)
                        END IF
                STOPP=1
                RAYCOD(1)=11
                RAYCOD(2)=NEWOBJ
                REFEXT=.FALSE.
                IF(.NOT.NULL) CALL MACFAL
                        RETURN
                        END IF
                        END IF
      IF(W4.GE.6.0D0.AND.W4.LE.10.0D0) THEN
      IF(SYSTEM(65+INT(W4)).EQ.0.0D0) THEN
                IF(MSG) THEN
      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',NEWOBJ
      CALL SHOWIT(1)
        OUTLYNE=
     1'REFERENCE RAY CAN NOT BE TRACED AT ZERO WAVELENGTH'
      CALL SHOWIT(1)
                        END IF
                STOPP=1
                RAYCOD(1)=11
                RAYCOD(2)=NEWOBJ
                REFEXT=.FALSE.
                IF(.NOT.NULL) CALL MACFAL
                      RETURN
                      END IF
                      END IF
C
C
C       NOW THE DEFAULTS HAVE BEEN SET
C       NOW CALL THE SUBROUTINE WHICH STARTS THE REFERENCE
C       RAY RAY TRACE. THIS IS REFRAY.
C
                WWQ=WQ
                WW1=W1
                WW2=W2
                WW3=W3
                WW4=W4
                WVN=WW4
                WW5=W5
      IF(WW4.GE.1.0D0.AND.WW4.LE.5.0D0)
     1LAMBDA=SYSTEM(INT(WW4))
      IF(WW4.GE.6.0D0.AND.WW4.LE.10.0D0)
     1LAMBDA=SYSTEM(65+INT(WW4))
        Y00=NWW1
        X00=NWW2
        Z0=NWW3
C
        IF(WW1.EQ.0.0D0.AND.WW2.EQ.0.0D0.AND.WW3.EQ.0.0D0) THEN
                        FOB0=1
                        FOBB0=.TRUE.
                        ELSE
                        FOB0=0
                        FOBB0=.FALSE.
                        END IF
        IF(WW1.EQ.0.0D0) THEN
                        FOBB0Y=.TRUE.
                        ELSE
                        FOBB0Y=.FALSE.
                        END IF
        IF(WW2.EQ.0.0D0) THEN
                        FOBB0X=.TRUE.
                        ELSE
                        FOBB0X=.FALSE.
                        END IF
C
C       THE CALL TO REFRAY HAS INPUTS:
C               QUALIFIER
C               WW1
C               WW2
C               WW3
C               WW4
C               WW5
C               NEWOBJ
C               NEWREF
C               NEWIMG
C               FOB0 (SET TO 1 IF THE REQUESTED RAY IS THE "GUT" RAY
C                       ELSE SET TO 0 TO TRIGGER A SECOND RAY TO BE
C                       TRACED FIRST.)
C
        IF(FOB0.EQ.0) THEN
C       TRACE THE FOB 0 0 0 RAY 0 0 FIRST THEN TRACE THE REQUESTED
C       RAY
        FOBYES=.TRUE.
        CHLFOB=WWQ
        LFOB(1)=0.0D0
        LFOB(2)=0.0D0
        LFOB(3)=0.0D0
        LFOB(4)=W4
        LFOB(5)=DBLE(NEWOBJ)
        LFOB(6)=DBLE(NEWREF)
        LFOB(7)=DBLE(NEWIMG)
                WW1=0.0D0
                WW2=0.0D0
                WW3=0.0D0
                WW4=LFOB(4)
                WVN=WW4
        TRYX=0.0D0
        TRYY=0.0D0
        TRYZ=0.0D0
        CALL REFRAY(OBJLEVEL)
        CALL IPLANE_TILT
        CALL CLPCEN
      IF(STOPP.EQ.1) THEN
                        REFEXT=.FALSE.
                        CPFNEXT=.FALSE.
                        CALL DELPSF
                        SPDEXT=.FALSE.
                        GSPDEXT=.FALSE.
                        IF(.NOT.NULL) FOBYES=.FALSE.
                IF(MSG) THEN
      CALL RAYDOC
      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',RAYCOD(2)
      CALL SHOWIT(1)
        OUTLYNE=
     1  'CHIEF RAY FROM CURRENT OBJECT POINT COULD NOT BE TRACED'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*) '2',RAYCOD(1),RAYCOD(2)
        CALL SHOWIT(1)
                        END IF
                STOPP=1
                REFEXT=.FALSE.
        IF(.NOT.NULL) FOBYES=.FALSE.
        IF(.NOT.NULL.AND.F34.EQ.0.AND.F28.EQ.0.AND.F35.EQ.0.AND.
     1F31.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
                        IF(.NOT.NULL) FOBYES=.FALSE.
C       RAY WAS TRACED
        XFOBB0=REFRY(1,NEWOBJ+1)
        YFOBB0=REFRY(2,NEWOBJ+1)
        ZFOBB0=REFRY(3,NEWOBJ+1)
        XLN=REFRY(4,NEWOBJ)
        XMN=REFRY(5,NEWOBJ)
        XNN=REFRY(6,NEWOBJ)
                        END IF
                        ELSE
C       FOB0 WAS 1 SO JUST TRACE THE REQUESTED RAY.
C       THE REQUESTED RAY IS THE GUT RAY, JUST TRACE IT AND RETURN
        FOBYES=.TRUE.
        CHLFOB=WWQ
        LFOB(1)=0.0D0
        LFOB(2)=0.0D0
        LFOB(3)=0.0D0
        LFOB(4)=W4
        LFOB(5)=DBLE(NEWOBJ)
        LFOB(6)=DBLE(NEWREF)
        LFOB(7)=DBLE(NEWIMG)
                WW1=0.0D0
                WW2=0.0D0
                WW3=0.0D0
                WW4=LFOB(4)
                WVN=WW4
        TRYX=0.0D0
        TRYY=0.0D0
        TRYZ=0.0D0
        CALL REFRAY(OBJLEVEL)
        CALL IPLANE_TILT
        CALL CLPCEN
      IF(STOPP.EQ.1) THEN
                        REFEXT=.FALSE.
                        CPFNEXT=.FALSE.
                        CALL DELPSF
                        SPDEXT=.FALSE.
                        GSPDEXT=.FALSE.
                        IF(.NOT.NULL) FOBYES=.FALSE.
                IF(MSG) THEN
      CALL RAYDOC
      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',RAYCOD(2)
      CALL SHOWIT(1)
        OUTLYNE=
     1  'CHIEF RAY FROM CURRENT OBJECT POINT COULD NOT BE TRACED'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*) '3',RAYCOD(1),RAYCOD(2)
        CALL SHOWIT(1)
                        END IF
C       NO REF RAY TRACED
                        REFEXT=.FALSE.
        IF(.NOT.NULL.AND.F34.EQ.0.AND.F28.EQ.0.AND.F35.EQ.0.AND.
     1F31.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
C       GUT RAY WAS TRACED.
C       DIRECTION COSINES OF THE Z-AXIS ARE:
        XLN=0.0D0
        XMN=0.0D0
        XNN=1.0D0
                        END IF
                        DO J=0,INT(SYSTEM(20))
      IF(ALENS(34,J).EQ.18.0D0) LDIF2=.FALSE.
      IF(ALENS(34,J).EQ.18.0D0) LDIF=.FALSE.
                        END DO
        IF(LDIF2) THEN
C       TRACE DIFFERENTIAL RAY WITHRESPECT TO AN OBJECT POINT
C       SHIFT
C       SAVE GUT RAY DATA
                        GUTRAY(1:50,0:499)=REFRY(1:50,0:499)
C       SAVE REFRY DATA
                        SAVE=REFRY
        SREFDIFEXT=.FALSE.
        DIFRAYTRACE=.TRUE.
        CALL FRFDIF
        DIFRAYTRACE=.FALSE.
        IF(STOPP.EQ.1) THEN
      STOPP=0
                        REFRY=SAVE
                        IF(WQ.NE.'NULL'.AND.F34.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
                        END IF
C       RESTORE REFRY DATA
                        REFRY=SAVE
C       NO DIFERENTIAL TRACE
                        END IF
C       RESET FOB0 TO 0 AND SET THE REFRAY EXISTENCE FLAG TO TRUE
                        FOB0=0
                        REFEXT=.TRUE.
        ANGLE1=REFRY(11,NEWOBJ)*180.0D0/PII
        ANGLE2=REFRY(12,NEWOBJ)*180.0D0/PII
        YK=REFRY(2,NEWIMG)
        XK=REFRY(1,NEWIMG)
        IF(WQ.EQ.'P') THEN
        WRITE(OUTLYNE,250)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,300) LAMBDA
      CALL SHOWIT(0)
      IF(SYSTEM(18).EQ.0.0D0) THEN
      IF(SYSTEM(6).EQ.1.0D0) LUNI='IN '
      IF(SYSTEM(6).EQ.2.0D0) LUNI='CM '
      IF(SYSTEM(6).EQ.3.0D0) LUNI='MM '
      IF(SYSTEM(6).EQ.4.0D0) LUNI='M  '
        WRITE(OUTLYNE,3302) REFRY(1,NEWOBJ),LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,3303) REFRY(2,NEWOBJ),LUNI
      CALL SHOWIT(0)
      XPFOB=REFRY(1,NEWOBJ)
      YPFOB=REFRY(2,NEWOBJ)
                   ELSE
      LUNI='DEG'
        ANGLE1=REFRY(11,NEWOBJ)*180.0D0/PII
        ANGLE2=REFRY(12,NEWOBJ)*180.0D0/PII
        WRITE(OUTLYNE,302) ANGLE1,LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,303) ANGLE2,LUNI
      CALL SHOWIT(0)
      XPFOB=ANGLE1
      YPFOB=ANGLE2
                   END IF
        WRITE(OUTLYNE,350)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,301) Y00,X00,Z0
      CALL SHOWIT(0)
                        END IF
        IF(WQ.EQ.'PIC') THEN
        WRITE(OUTLYNE,250)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,300) LAMBDA
      CALL SHOWIT(0)
      IF(SYSTEM(18).EQ.0.0D0) THEN
      IF(SYSTEM(6).EQ.1.0D0) LUNI='IN '
      IF(SYSTEM(6).EQ.2.0D0) LUNI='CM '
      IF(SYSTEM(6).EQ.3.0D0) LUNI='MM '
      IF(SYSTEM(6).EQ.4.0D0) LUNI='M  '
        WRITE(OUTLYNE,3302) REFRY(1,NEWOBJ),LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,3303) REFRY(2,NEWOBJ),LUNI
      CALL SHOWIT(0)
      XPFOB=REFRY(1,NEWOBJ)
      YPFOB=REFRY(2,NEWOBJ)
                   ELSE
      LUNI='DEG'
        ANGLE1=REFRY(11,NEWOBJ)*180.0D0/PII
        ANGLE2=REFRY(12,NEWOBJ)*180.0D0/PII
        WRITE(OUTLYNE,302) ANGLE1,LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,303) ANGLE2,LUNI
      CALL SHOWIT(0)
      XPFOB=ANGLE1
      YPFOB=ANGLE2
                   END IF
        WRITE(OUTLYNE,400)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,401) Y00,X00,Z0,YK,XK
      CALL SHOWIT(0)
                        END IF
        IF(WQ.EQ.'PICNH') THEN
        WRITE(OUTLYNE,401) Y00,X00,Z0,YK,XK
      CALL SHOWIT(0)
                        END IF
C       REFERENCE RAY WAS TRACED, NOW TRACE THE DIFFERENTIAL RAY
C       DATA IF FOB HAS QUALIFIERS PFS OR PFSNH.
        IF(WQ.EQ.'PFS'.OR.WQ.EQ.'PFSNH') THEN
C       SAVE REFRY DATA
                        SAVE=REFRY
        STOPP=0
        SREFDIFEXT=.FALSE.
        DIFRAYTRACE=.TRUE.
        CALL FOBDIF
        DIFRAYTRACE=.FALSE.
        IF(REFEXT) STOPP=0
        IF(STOPP.EQ.1) THEN
        STOPP=0
C       RESTORE REFRY DATA
                        REFRY=SAVE
                        IF(WQ.NE.'NULL'.AND.F34.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
                        END IF
C       RESTORE REFRY DATA
                        REFRY=SAVE
        IF(WQ.EQ.'PFS') THEN
C       OUTPUT A PRINTED HEADER WITH APPROPRIATE VALUES
        WRITE(OUTLYNE,250)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,300) LAMBDA
      CALL SHOWIT(0)
      IF(SYSTEM(18).EQ.0.0D0) THEN
      IF(SYSTEM(6).EQ.1.0D0) LUNI='IN '
      IF(SYSTEM(6).EQ.2.0D0) LUNI='CM '
      IF(SYSTEM(6).EQ.3.0D0) LUNI='MM '
      IF(SYSTEM(6).EQ.4.0D0) LUNI='M  '
        WRITE(OUTLYNE,3302) REFRY(1,NEWOBJ),LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,3303) REFRY(2,NEWOBJ),LUNI
      CALL SHOWIT(0)
      XPFOB=REFRY(1,NEWOBJ)
      YPFOB=REFRY(2,NEWOBJ)
                   ELSE
      LUNI='DEG'
        ANGLE1=REFRY(11,NEWOBJ)*180.0D0/PII
        ANGLE2=REFRY(12,NEWOBJ)*180.0D0/PII
        WRITE(OUTLYNE,302) ANGLE1,LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,303) ANGLE2,LUNI
      CALL SHOWIT(0)
      XPFOB=ANGLE1
      YPFOB=ANGLE2
                   END IF

        WRITE(OUTLYNE,101)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,401) Y00,X00,Z0,FT,FS
      CALL SHOWIT(0)
                        END IF
        IF(WQ.EQ.'PFSNH') THEN
C       OUTPUT APPROPRIATE VALUES
        WRITE(OUTLYNE,401) Y00,X00,Z0,FT,FS
      CALL SHOWIT(0)
                        END IF
C       PROCEED
                        END IF
C       THE TRACE FOR FOB0=1 IS COMPLETED
                        RETURN
                        END IF
C       THE FOB0 WAS 0, A 000 GUT RAY WAS TRACED, NOW TRACE THE
C       REQUESTED RAY WHICH WAS NOT 0 0 0.
C       NOW TRACE THE REQUESTED CHIEF RAY IF IT WAS NOT 0 0 0
C
        FOBYES=.TRUE.
        CHLFOB=WWQ
                WW1=W1
                WW2=W2
                WW3=W3
                WW4=W4
        LFOB(1)=NWW1
        LFOB(2)=NWW2
        LFOB(3)=NWW3
        LFOB(4)=WW4
        LFOB(5)=DBLE(NEWOBJ)
        LFOB(6)=DBLE(NEWREF)
        LFOB(7)=DBLE(NEWIMG)
                WWQ=WQ
                WVN=WW4
                WW5=W5
C
C     OBJECT ANGLES NOT INPUT
      ANGIN=.FALSE.
C
      IF(ALENS(9,NEWOBJ).EQ.0.OR.ALENS(127,NEWOBJ).NE.0.0D0) THEN
C     THE STARTING RAY COORDINATES ON THE OBJECT SURAFCE
C     ARE NOT DETERMINED BY THE CLAP ON THE OBJECT SURFACE
C     USING SCY AND SCX, FOB REPRESENTS FRACTIONS OF IMAGE HEIGHT
C     AND DEPTH.
      IF(SYSTEM(16).NE.0.0D0) XSTRT=W2*PXTRAX(5,NEWOBJ)
      IF(SYSTEM(16).EQ.0.0D0) XSTRT=0.0D0
      IF(SYSTEM(14).NE.0.0D0) YSTRT=W1*PXTRAY(5,NEWOBJ)
      IF(SYSTEM(14).EQ.0.0D0) YSTRT=0.0D0
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
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
        ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
                        ELSE
C     USE Y HEIGHT OF CLAP WITH OFFSETS
      IF(ALENS(9,NEWOBJ).EQ.1.0D0.AND.ALENS(127,NEWOBJ).EQ.0.0D0)THEN
      IF(ALENS(10,NEWOBJ).LE.ALENS(11,NEWOBJ)) THEN
      XSTRT=W2*(ALENS(10,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(10,NEWOBJ)+ALENS(12,NEWOBJ))
                           ELSE
      XSTRT=W2*(ALENS(11,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(11,NEWOBJ)+ALENS(12,NEWOBJ))
                           END IF
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
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
      ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
                        END IF
      IF(ALENS(9,NEWOBJ).EQ.5.0D0.AND.ALENS(127,NEWOBJ).EQ.0.0D0)THEN
      XSTRT=W2*(ALENS(10,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(10,NEWOBJ)+ALENS(12,NEWOBJ))
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
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
      ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
                        END IF
      IF(ALENS(9,NEWOBJ).GT.1.0D0.AND.ALENS(9,NEWOBJ).LT.5.0D0.AND.
     1ALENS(127,NEWOBJ).EQ.0.0D0) THEN
      XSTRT=W2*(ALENS(11,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(10,NEWOBJ)+ALENS(12,NEWOBJ))
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
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
      ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
                        END IF
                        END IF
C     NOW THE INITIAL AIMING POINT AT NEWOBJ+1
C     NOT ANGIN
        X1AIM=PXTRAX(5,(NEWOBJ+1))
        X1AIM=(X1AIM*W2)-ALENS(31,NEWOBJ+1)+SYSTEM(81)
        Y1AIM=PXTRAY(5,(NEWOBJ+1))
        Y1AIM=(Y1AIM*W1)-ALENS(30,NEWOBJ+1)+SYSTEM(82)
      IISURF=NEWOBJ+1
      CALL SAGRET(IISURF,X1AIM,Y1AIM,ZSAG,SAGERR)
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
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
        Z1AIM=SYSTEM(89)+ZSAG
      TRYX=X1AIM
      TRYY=Y1AIM
      TRYZ=Z1AIM
      XJIM=TRYX
      YJIM=TRYY
      ZJIM=TRYZ
      XC=TRYX
      YC=TRYY
      ZC=TRYZ
      ZEEERR=.FALSE.
      IF(ALENS(1,NEWOBJ+1).NE.0.0D0)
     1CALL GETZEE1
C     STARTING INTERSECTION POINT ON SURF 1 IS
      TRYX=XC
      TRYY=YC
      TRYZ=ZC
        FACTER=-(DABS(SERLIM)+DABS(SERINC))
        ICNTEST=INT(DABS(2.0D0*SERLIM)/DABS(SERINC))
        ICNT=0
 992  CONTINUE
      CALL REFRAY(OBJLEVEL)
        CALL IPLANE_TILT
        CALL CLPCEN
      IF(STOPP.EQ.1.AND.ICNT.LE.ICNTEST) THEN
               STOPP=0
               REFEXT=.TRUE.
C       INITIALIZE RAYCOD
                RAYCOD(1)=0
                RAYCOD(2)=-1
        FACTER=FACTER+DABS(SERINC)
      XA=XJIM
      YA=YJIM
      ZA=ZJIM
      TRYX=XA
      TRYY=YA
      TRYZ=ZA+FACTER
      XC=TRYX
      YC=TRYY
      ZC=TRYZ
      ZEEERR=.FALSE.
      IF(ALENS(1,NEWOBJ+1).NE.0.0D0)
     1CALL GETZEE1
      IF(ZEEERR) THEN
      XC=TRYX
      YC=TRYY
      ZC=TRYZ
          END IF
        TRYX=XC
        TRYY=YC
        TRYZ=ZC
               ICNT=ICNT+1
               GO TO 992
                       END IF
        FACTER=-(DABS(SERLIM)+DABS(SERINC))
        ICNTEST=INT(DABS(2.0D0*SERLIM)/DABS(SERINC))
        ICNT=0
      IF(STOPP.EQ.1) THEN
 993  CONTINUE
      IF(ICNT.NE.0) THEN
      CALL REFRAY(OBJLEVEL)
        CALL IPLANE_TILT
        CALL CLPCEN
      END IF
      IF(STOPP.EQ.1.AND.ICNT.LE.ICNTEST) THEN
               STOPP=0
               REFEXT=.TRUE.
C       INITIALIZE RAYCOD
                RAYCOD(1)=0
                RAYCOD(2)=-1
        FACTER=FACTER+DABS(SERINC)
      XA=0.0D0
      YA=0.0D0
      ZA=FACTER
      TRYX=XA
      TRYY=YA
      TRYZ=ZA
      XC=TRYX
      YC=TRYY
      ZC=TRYZ
      ZEEERR=.FALSE.
      IF(ALENS(1,NEWOBJ+1).NE.0.0D0)
     1CALL GETZEE1
      IF(ZEEERR) THEN
      XC=TRYX
      YC=TRYY
      ZC=TRYZ
          END IF
        TRYX=XC
        TRYY=YC
        TRYZ=ZC
               ICNT=ICNT+1
               GO TO 993
                       END IF
                       END IF
      IF(STOPP.EQ.1.AND.ICNT.GT.ICNTEST) THEN
                        REFEXT=.FALSE.
                        CPFNEXT=.FALSE.
                        CALL DELPSF
                        SPDEXT=.FALSE.
                        GSPDEXT=.FALSE.
                        IF(.NOT.NULL) FOBYES=.FALSE.
                IF(MSG) THEN
      CALL RAYDOC
      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',RAYCOD(2)
      CALL SHOWIT(1)
        OUTLYNE=
     1  'CHIEF RAY FROM CURRENT OBJECT POINT COULD NOT BE TRACED'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*) '4',RAYCOD(1),RAYCOD(2)
        CALL SHOWIT(1)
                        END IF
C       NO REF RAY TRACED
                        REFEXT=.FALSE.
        IF(.NOT.NULL.AND.F34.EQ.0.AND.F28.EQ.0.AND.F35.EQ.0.AND.
     1F31.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
                        END IF
                        STOPP=0
                        REFEXT=.TRUE.
                        FOBYES=.TRUE.
                        DO J=0,INT(SYSTEM(20))
      IF(ALENS(34,J).EQ.18.0D0) LDIF2=.FALSE.
      IF(ALENS(34,J).EQ.18.0D0) LDIF=.FALSE.
                        END DO
                IF(LDIF2) THEN
C       TRACE DIFFERENTIAL RAY WITHRESPECT TO AN OBJECT POINT
C       SHIFT
C       SAVE GUT RAY DATA
                        GUTRAY(1:50,0:499)=REFRY(1:50,0:499)
C       SAVE REFRY DATA
                        SAVE=REFRY
        SREFDIFEXT=.FALSE.
        DIFRAYTRACE=.TRUE.
        CALL FRFDIF
        DIFRAYTRACE=.FALSE.
        IF(STOPP.EQ.1) THEN
      STOPP=0
C       RESTORE REFRY DATA
                        REFRY=SAVE
                        IF(WQ.NE.'NULL'.AND.F34.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
                        END IF
C       RESTORE REFRY DATA
                        REFRY=SAVE
C       NO DIF TRACE
                        END IF
C
                        FOB0=0
C
C       THE RETURN WILL SEND BACK VIA THE COMMON/REFR COMMON
C       THE FULL RAY DATA FOR THE FULLY AIMMED AND TRACED REFERENCE
C       RAY. THEN ANY APPROPRIATE FOB RELATED PRINTOUT WILL BE DONE.
        YK=REFRY(2,NEWIMG)
        XK=REFRY(1,NEWIMG)
        ANGLE1=REFRY(11,NEWOBJ)*180.0D0/PII
        ANGLE2=REFRY(12,NEWOBJ)*180.0D0/PII
        IF(WQ.EQ.'P') THEN
        WRITE(OUTLYNE,250)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,300) LAMBDA
      CALL SHOWIT(0)
      IF(SYSTEM(18).EQ.0.0D0) THEN
      IF(SYSTEM(6).EQ.1.0D0) LUNI='IN '
      IF(SYSTEM(6).EQ.2.0D0) LUNI='CM '
      IF(SYSTEM(6).EQ.3.0D0) LUNI='MM '
      IF(SYSTEM(6).EQ.4.0D0) LUNI='M  '
        WRITE(OUTLYNE,3302) REFRY(1,NEWOBJ),LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,3303) REFRY(2,NEWOBJ),LUNI
      CALL SHOWIT(0)
      XPFOB=REFRY(1,NEWOBJ)
      YPFOB=REFRY(2,NEWOBJ)
                   ELSE
      LUNI='DEG'
        ANGLE1=REFRY(11,NEWOBJ)*180.0D0/PII
        ANGLE2=REFRY(12,NEWOBJ)*180.0D0/PII
        WRITE(OUTLYNE,302) ANGLE1,LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,303) ANGLE2,LUNI
      CALL SHOWIT(0)
      XPFOB=ANGLE1
      YPFOB=ANGLE2
                   END IF
        WRITE(OUTLYNE,350)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,301) Y00,X00,Z0
      CALL SHOWIT(0)
                        END IF
        IF(WQ.EQ.'PIC') THEN
        WRITE(OUTLYNE,250)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,300) LAMBDA
      CALL SHOWIT(0)
      IF(SYSTEM(18).EQ.0.0D0) THEN
      IF(SYSTEM(6).EQ.1.0D0) LUNI='IN '
      IF(SYSTEM(6).EQ.2.0D0) LUNI='CM '
      IF(SYSTEM(6).EQ.3.0D0) LUNI='MM '
      IF(SYSTEM(6).EQ.4.0D0) LUNI='M  '
        WRITE(OUTLYNE,3302) REFRY(1,NEWOBJ),LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,3303) REFRY(2,NEWOBJ),LUNI
      CALL SHOWIT(0)
      XPFOB=REFRY(1,NEWOBJ)
      YPFOB=REFRY(2,NEWOBJ)
                   ELSE
      LUNI='DEG'
        ANGLE1=REFRY(11,NEWOBJ)*180.0D0/PII
        ANGLE2=REFRY(12,NEWOBJ)*180.0D0/PII
        WRITE(OUTLYNE,302) ANGLE1,LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,303) ANGLE2,LUNI
      CALL SHOWIT(0)
      XPFOB=ANGLE1
      YPFOB=ANGLE2
                   END IF

        WRITE(OUTLYNE,400)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,401) Y00,X00,Z0,YK,XK
      CALL SHOWIT(0)
                        END IF
        IF(WQ.EQ.'PICNH') THEN
        WRITE(OUTLYNE,401) Y00,X00,Z0,YK,XK
      CALL SHOWIT(0)
                        END IF
C       REFERENCE RAY WAS TRACED, NOW TRACE THE DIFFERENTIAL RAY
C       DATA IF FOB HAS QUALIFIERS PFS OR PFSNH.
        IF(WQ.EQ.'PFS'.OR.WQ.EQ.'PFSNH') THEN
C       SAVE REFRY DATA
                        SAVE=REFRY
        STOPP=0
        SREFDIFEXT=.FALSE.
        DIFRAYTRACE=.TRUE.
        CALL FOBDIF
        DIFRAYTRACE=.FALSE.
        IF(REFEXT) STOPP=0
        IF(STOPP.EQ.1) THEN
      STOPP=0
C       RESTORE REFRY DATA
                        REFRY=SAVE
                        IF(WQ.NE.'NULL'.AND.F34.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
                        END IF
C       RESTORE REFRY DATA
                        REFRY=SAVE
        IF(WQ.EQ.'PFS') THEN
C       OUTPUT A PRINTED HEADER WITH APPROPRIATE VALUES
        WRITE(OUTLYNE,250)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,300) LAMBDA
      CALL SHOWIT(0)
      IF(SYSTEM(18).EQ.0.0D0) THEN
      IF(SYSTEM(6).EQ.1.0D0) LUNI='IN '
      IF(SYSTEM(6).EQ.2.0D0) LUNI='CM '
      IF(SYSTEM(6).EQ.3.0D0) LUNI='MM '
      IF(SYSTEM(6).EQ.4.0D0) LUNI='M  '
        WRITE(OUTLYNE,3302) REFRY(1,NEWOBJ),LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,3303) REFRY(2,NEWOBJ),LUNI
      CALL SHOWIT(0)
      XPFOB=REFRY(1,NEWOBJ)
      YPFOB=REFRY(2,NEWOBJ)
                   ELSE
      LUNI='DEG'
        ANGLE1=REFRY(11,NEWOBJ)*180.0D0/PII
        ANGLE2=REFRY(12,NEWOBJ)*180.0D0/PII
        WRITE(OUTLYNE,302) ANGLE1,LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,303) ANGLE2,LUNI
      CALL SHOWIT(0)
      XPFOB=ANGLE1
      YPFOB=ANGLE2
                   END IF
        WRITE(OUTLYNE,101)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,401) Y00,X00,Z0,FT,FS
      CALL SHOWIT(0)
                        END IF
        IF(WQ.EQ.'PFSNH') THEN
C       OUTPUT APPROPRIATE VALUES
        WRITE(OUTLYNE,401) Y00,X00,Z0,FT,FS
      CALL SHOWIT(0)
                        END IF
C       PROCEED
                        END IF
C
                        STOPP=0
                        REFEXT=.TRUE.
                        RETURN
 250    FORMAT('REFERENCE RAY TRACED')
 300    FORMAT('WAVELENGTH = ',G12.4,' MICRONS')
 302    FORMAT('X-FIELD ANGLE = ',G15.7,1X,A3)
 303    FORMAT('Y-FIELD ANGLE = ',G15.7,1X,A3)
 3302   FORMAT('X-OBJECT HEIGHT = ',G15.7,1X,A3)
 3303   FORMAT('Y-OBJECT HEIGHT = ',G15.7,1X,A3)
 401    FORMAT(G10.2,2X,G10.2,2X,G10.2,2X,G14.6,2X,G14.6)
 350    FORMAT(2X,'Y-POS.    ',2X,'X-POS.    ',2X,'Z-POS.    ')
 400    FORMAT(2X,'Y-POS.    ',2X,'X-POS.    ',2X,'Z-POS.',
     1  2X,'Y-IMAGE HT. ',4X,'X-IMAGE HT. ')
 101    FORMAT(2X,'Y-POS.    ',2X,'X-POS.    ',2X,'Z-POS.    ',
     1  2X,'T-FOCUS POS.',4X,'S-FOCUS POS.')
 301    FORMAT(G10.2,2X,G10.2,2X,G10.2)
                        END
C SUB FFOB2.FOR
        SUBROUTINE FFOB2
        USE GLOBALS
C
        IMPLICIT NONE
C
C       THIS IS SUBROUTINE FFOB2.FOR. THIS SUBROUTINE IMPLEMENTS
C       THE CMD LEVEL COMMAND FOB FROM CALCPRE.
C
        INTEGER IISURF,FFT,FFS,I,J,ICNT,ICNTEST
C
      CHARACTER LUNI*3
C
      REAL*8 XPFOB,YPFOB
      COMMON/PFOB/XPFOB,YPFOB,LUNI
C
         REAL*8 FT,FS,X00,Y00,Z0
     1  ,LAMBDA,ANGLE1,XK,YK,XLN,XMN,XNN,XFOBB0,YFOBB0,ZFOBB0
     2  ,ANGJK1,ANGJK2,XXANG,YYANG,FACTER,XA,YA,ZA,XJIM,YJIM,ZJIM
     3,ZSAG,ANGLE2,SCLFACY,SCLFACX,AWW1,AWW2
C
      REAL*8 XRAYER,YRAYER,ZRAYER
      COMMON/RAYERPASS/XRAYER,YRAYER,ZRAYER
C
      COMMON/OHFOB/XFOBB0,YFOBB0,ZFOBB0
C
        LOGICAL FANEXT,ZEEERR,SAGERR
        COMMON/ERRZEE/ZEEERR
C
        COMMON/FANEXI/FANEXT
C
C       FOBB0=TRUE FOR ON AXIS AND FALSE FOR OFF AXIS
        LOGICAL FOBB0,FOBB0X,FOBB0Y
C
        COMMON/FFOOBB/FOBB0,FOBB0X,FOBB0Y
C
      COMMON/FBDIFF/FS,FT,FFS,FFT
C
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATSPD.INC'
        FOBRUN=.TRUE.
      DLLX=0.0D0
      DLLY=0.0D0
      DLLZ=0.0D0
C     SET UP ANGLE FACTERS
      IF(SYSTEM(21).EQ.0.0D0) THEN
      SCLFACY=1.0D0
                   ELSE
      SCLFACY=DABS(1.0D0/SYSTEM(21))
                   END IF
      IF(SYSTEM(23).EQ.0.0D0) THEN
      SCLFACX=1.0D0
                   ELSE
      SCLFACX=DABS(1.0D0/SYSTEM(23))
                   END IF
      AWW1=W1/SCLFACY
      AWW2=W2/SCLFACX
C
      IF(SYSTEM(18).EQ.1.AND.NEWOBJ.EQ.0.OR.
     1SYSTEM(19).EQ.1.AND.NEWOBJ.EQ.0) THEN
      IF(DF3.EQ.0.AND.W3.NE.0.0D0) THEN
                   W3=0.0D0
                   DF3=1
                   S3=0
                   END IF
C     OBJECT ANGLES INPUT
      ANGIN=.TRUE.
C     REF OBJ HT INPUT AS ANGLE
C     IN THIS CASE, FOB INDICATES FRACTIONS OF FIELD ANGLES
C     BUT ONLY IF THE NEWOBJ SURFACE IS SURFACE 0
C
C     THIS NEEDS MODIFICATION FOR FRACTIONS OF FIELD ANGLES
C
      ANGJK1=AWW1
      ANGJK2=AWW2
      IF(ANGJK1.GT.89.9999D0.AND.
     1ANGJK1.LT.90.0001D0.OR.
     2ANGJK1.LT.-89.9999D0.AND.
     3ANGJK1.GT.-90.0001D0.OR.
     4ANGJK1.GT.-270.0001D0.AND.
     5ANGJK1.LT.-269.9999D0.OR.
     6ANGJK1.GT.269.9999D0.AND.
     7ANGJK1.LT.270.0001D0) THEN
      IF(DF2.EQ.0) THEN
      OUTLYNE='THE Y-FIELD ANGLE IS 90.0 OR -90.0 DEGREES'
      CALL SHOWIT(1)
      OUTLYNE='NUMERIC WORD #2 OF THE "FOB" COMMAND IS NOT USED HERE'
      CALL SHOWIT(1)
                   W2=0.0D0
                   DF2=1
                   S2=0
                   END IF
                   END IF
      IF(ANGJK1.GT.89.9999D0.AND.
     1ANGJK1.LT.90.0001D0)  ANGJK1=89.9999D0

      IF(ANGJK1.LT.-89.9999D0.AND.
     1ANGJK1.GT.-90.0001D0) ANGJK1=-89.9999D0

      IF(ANGJK1.GT.-270.0001D0.AND.
     1ANGJK1.LT.-269.9999D0) ANGJK1=89.9999D0

      IF(ANGJK1.GT.269.9999D0.AND.
     1ANGJK1.LT.270.0001D0) ANGJK1=-89.9999D0

      IF(ANGJK2.GT.89.9999D0.AND.
     1ANGJK2.LT.90.0001D0)  ANGJK2=89.9999D0

      IF(ANGJK2.LT.-89.9999D0.AND.
     1ANGJK2.GT.-90.0001D0) ANGJK2=-89.9999D0

      IF(ANGJK2.GT.-270.0001D0.AND.
     1ANGJK2.LT.-269.9999D0) ANGJK2=89.9999D0

      IF(ANGJK2.GT.269.9999D0.AND.
     1ANGJK2.LT.270.0001D0) ANGJK2=-89.9999D0

C     ANGLES IN RADIANS ARE:
C
      YYANG=ANGJK1*PII/180.0D0
      XXANG=ANGJK2*PII/180.0D0
C
      IF(DABS(ANGJK1).GT.90.0D0) YSTRT=DTAN(YYANG)*ALENS(3,NEWOBJ)
      IF(DABS(ANGJK1).LE.90.0D0) YSTRT=-DTAN(YYANG)*ALENS(3,NEWOBJ)
C
      IF(DABS(ANGJK2).GT.90.0D0) XSTRT=DTAN(XXANG)*ALENS(3,NEWOBJ)
      IF(DABS(ANGJK2).LE.90.0D0) XSTRT=-DTAN(XXANG)*ALENS(3,NEWOBJ)

      ZSTRT=2.0D0*ALENS(3,NEWOBJ)

      IF(ANGJK1.GT.90.0D0.AND.ANGJK1.LT.270.0D0.OR.
     1ANGJK1.LT.-90.0D0.AND.ANGJK1.GT.-270.0D0) THEN
C
      IF(ANGJK2.GT.90.0D0.AND.ANGJK2.LT.270.0D0.OR.
     1ANGJK2.LT.-90.0D0.AND.ANGJK2.GT.-270.0D0) THEN
                       ZSTRT=0.0D0
                       END IF
                       END IF
      IF(ANGJK2.GT.90.0D0.AND.ANGJK2.LT.270.0D0.OR.
     1ANGJK2.LT.-90.0D0.AND.ANGJK2.GT.-270.0D0) THEN
C
      IF(ANGJK1.GT.90.0D0.AND.ANGJK1.LT.270.0D0.OR.
     1ANGJK1.LT.-90.0D0.AND.ANGJK1.GT.-270.0D0) THEN
                       ZSTRT=0.0D0
                       END IF
                       END IF
      IF(ANGJK1.GE.0.0D0.AND.ANGJK1.LT.90.0D0.OR.
     1ANGJK1.LE.0.0D0.AND.ANGJK1.GT.-90.0D0.OR.
     1ANGJK1.GT.270.0D0.AND.ANGJK1.LE.0.0D0.OR.
     1ANGJK1.LT.-270.0D0.AND.ANGJK1.GE.0.0D0) THEN
C
      IF(ANGJK2.GE.0.0D0.AND.ANGJK2.LT.90.0D0.OR.
     1ANGJK2.LE.0.0D0.AND.ANGJK2.GT.-90.0D0.OR.
     1ANGJK2.GT.270.0D0.AND.ANGJK2.LE.0.0D0.OR.
     1ANGJK2.LT.-270.0D0.AND.ANGJK2.GE.0.0D0) THEN
                       ZSTRT=0.0D0
                       END IF
                       END IF
      IF(ANGJK2.GE.0.0D0.AND.ANGJK2.LT.90.0D0.OR.
     1ANGJK2.LE.0.0D0.AND.ANGJK2.GT.-90.0D0.OR.
     1ANGJK2.GT.270.0D0.AND.ANGJK2.LE.0.0D0.OR.
     1ANGJK2.LT.-270.0D0.AND.ANGJK2.GE.0.0D0) THEN
C
      IF(ANGJK1.GE.0.0D0.AND.ANGJK1.LT.90.0D0.OR.
     1ANGJK1.LE.0.0D0.AND.ANGJK1.GT.-90.0D0.OR.
     1ANGJK1.GT.270.0D0.AND.ANGJK1.LE.0.0D0.OR.
     1ANGJK1.LT.-270.0D0.AND.ANGJK1.GE.0.0D0) THEN
                       ZSTRT=0.0D0
                       END IF
                       END IF
      IF(SYSTEM(18).EQ.1.0D0.AND.SYSTEM(21).EQ.0.0D0) YSTRT=0.0D0
      IF(SYSTEM(19).EQ.1.0D0.AND.SYSTEM(23).EQ.0.0D0) XSTRT=0.0D0
      IF(XSTRT.EQ.0.0D0.AND.YSTRT.EQ.0.0D0) ZSTRT=0.0D0
                       ELSE
C     OBJECT ANGLES NOT INPUT
      ANGIN=.FALSE.
C
      IF(ALENS(9,NEWOBJ).EQ.0.OR.ALENS(127,NEWOBJ).NE.0.0D0) THEN
C     THE STARTING RAY COORDINATES ON THE OBJECT SURAFCE
C     ARE NOT DETERMINED BY THE CLAP ON THE OBJECT SURFACE
C     USING SCY AND SCX, FOB REPRESENTS FRACTIONS OF IMAGE HEIGHT
C     AND DEPTH.
      IF(SYSTEM(16).NE.0.0D0) XSTRT=W2*PXTRAX(5,NEWOBJ)
      IF(SYSTEM(16).EQ.0.0D0) XSTRT=0.0D0
      IF(SYSTEM(14).NE.0.0D0) YSTRT=W1*PXTRAY(5,NEWOBJ)
      IF(SYSTEM(14).EQ.0.0D0) YSTRT=0.0D0
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
               IF(SAGERR) THEN
                STOPP=1
                RAYCOD(1)=16
                RAYCOD(2)=NEWOBJ
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
        ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
                        ELSE
C     USE Y HEIGHT OF CLAP WITH OFFSETS
      IF(ALENS(9,NEWOBJ).EQ.1.0D0.AND.ALENS(127,NEWOBJ).EQ.0.0D0) THEN
      IF(ALENS(10,NEWOBJ).LE.ALENS(11,NEWOBJ)) THEN
      XSTRT=W2*(ALENS(10,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(10,NEWOBJ)+ALENS(12,NEWOBJ))
                           ELSE
      XSTRT=W2*(ALENS(11,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(11,NEWOBJ)+ALENS(12,NEWOBJ))
                           END IF
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
               IF(SAGERR) THEN
                STOPP=1
                RAYCOD(1)=16
                RAYCOD(2)=NEWOBJ
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
      ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
                        END IF
      IF(ALENS(9,NEWOBJ).EQ.5.0D0.AND.ALENS(127,NEWOBJ).EQ.0.0D0) THEN
      XSTRT=W2*(ALENS(10,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(10,NEWOBJ)+ALENS(12,NEWOBJ))
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
               IF(SAGERR) THEN
                STOPP=1
                RAYCOD(1)=16
                RAYCOD(2)=NEWOBJ
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
      ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
                        END IF
      IF(ALENS(9,NEWOBJ).GT.1.0D0.AND.ALENS(9,NEWOBJ).LT.5.0D0.AND.
     1ALENS(127,NEWOBJ).EQ.0.0D0) THEN
      XSTRT=W2*(ALENS(11,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(10,NEWOBJ)+ALENS(12,NEWOBJ))
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
               IF(SAGERR) THEN
                STOPP=1
                RAYCOD(1)=16
                RAYCOD(2)=NEWOBJ
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
      ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
                        END IF
                        END IF
                        END IF
C     NOW THE INITIAL AIMING POINT AT NEWOBJ+1
      IF(ANGIN) THEN
        X1AIM=PXTRAX(5,(NEWOBJ+1))
        X1AIM=(X1AIM*W2)-ALENS(31,NEWOBJ+1)+SYSTEM(81)
        Y1AIM=PXTRAY(5,(NEWOBJ+1))
        Y1AIM=(Y1AIM*W1)-ALENS(30,NEWOBJ+1)+SYSTEM(82)
        Z1AIM=SYSTEM(89)
                       ELSE
C     NOT ANGIN
        X1AIM=PXTRAX(5,(NEWOBJ+1))
        X1AIM=(X1AIM*W2)-ALENS(31,NEWOBJ+1)+SYSTEM(81)
        Y1AIM=PXTRAY(5,(NEWOBJ+1))
        Y1AIM=(Y1AIM*W1)-ALENS(30,NEWOBJ+1)+SYSTEM(82)
      IISURF=NEWOBJ+1
      CALL SAGRET(IISURF,X1AIM,Y1AIM,ZSAG,SAGERR)
               IF(SAGERR) THEN
                STOPP=1
                RAYCOD(1)=16
                RAYCOD(2)=NEWOBJ
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
        Z1AIM=SYSTEM(89)+ZSAG
                       END IF
      TRYX=X1AIM
      TRYY=Y1AIM
      TRYZ=Z1AIM
      XC=TRYX
      YC=TRYY
      ZC=TRYZ
      ZEEERR=.FALSE.
      IF(ALENS(1,NEWOBJ+1).NE.0.0D0)
     1CALL GETZEE1
C     STARTING INTERSECTION POINT ON SURF 1 IS
      TRYX=XC
      TRYY=YC
      TRYZ=ZC
C
                        FANEXT=.FALSE.
C
C       INITIALIZE RAYCOD
                RAYCOD(1)=0
                RAYCOD(2)=-1
C
C       SET RAYEXT TO FALSE AND FAIL TO TRUE
                        RAYEXT=.FALSE.
                        POLEXT=.FALSE.
                        FAIL=.TRUE.
C       SET REFEXT AND STOPP
                        STOPP=0
                        REFEXT=.TRUE.
                        SPDEXT=.FALSE.
                        GSPDEXT=.FALSE.
                        CPFNEXT=.FALSE.
                        CALL DELPSF
        IF(SYSTEM(63).EQ.1.AND.S5.EQ.1) THEN
        OUTLYNE='"FOB" TAKES NO NUMERIC WORD #5 INPUT'
      CALL SHOWIT(1)
        OUTLYNE='WHEN TELECENTRIC RAY AIMING IS "ON"'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
C
C       SET DEFAULT NUMERICS
        IF(DF1.EQ.1) W1=0.0D0
        IF(DF2.EQ.1) W2=0.0D0
        IF(DF3.EQ.1) W3=0.0D0
C       THE CONTROL WAVELENGTH
        IF(DF4.EQ.1) W4=SYSTEM(11)
        IF(DF4.EQ.1) WVN=SYSTEM(11)
        IF(DF5.EQ.1) THEN
      IF(WC.EQ.'FOB') THEN
                FT=0.0D0
                FS=0.0D0
                   END IF
                        END IF
      IF(DF5.EQ.0) THEN
      IF(GLANAM(INT(SYSTEM(20))-1,2).EQ.'PERFECT      ') THEN
      OUTLYNE='OBJECT, REFERENCE AND IMAGE SURFACES MAY NOT BE'
      CALL SHOWIT(1)
        OUTLYNE='REASSIGNED WHEN THE "PERFECT" SURFACE IS BEING USED'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
      IF(GLANAM(INT(SYSTEM(20))-1,2).EQ.'IDEAL        ') THEN
      OUTLYNE='OBJECT, REFERENCE AND IMAGE SURFACES MAY NOT BE'
      CALL SHOWIT(1)
        OUTLYNE='REASSIGNED WHEN THE "IDEAL" SURFACE IS BEING USED'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
                        END IF
C
C       THIS MAKES FOB EQUAL FOB 0 0 0 (CW) 0
C
C       CHECK FOR NATURE OF QUALIFIER WORD
        IF(SQ.EQ.1) THEN
        IF(WQ.NE.'NULL'.AND.WQ.NE.'P'.AND.WQ.NE.
     1  'PIC'.AND.WQ.NE.'PICNH'.AND.WQ.NE.'PFS'
     2  .AND.WQ.NE.'PFSNH') THEN
        OUTLYNE='INVALID QUALIFIER USED WITH "FOB"'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
C       NO QUALIFIER,PROCEED
                        END IF
C       CHECK FOR LEGAL WAVELENGTH BOUNDS
        IF(INT(W4).NE.1.AND.
     1INT(W4).NE.2.AND.
     1INT(W4).NE.3.AND.
     1INT(W4).NE.4.AND.
     1INT(W4).NE.5.AND.
     1INT(W4).NE.6.AND.
     1INT(W4).NE.7.AND.
     1INT(W4).NE.8.AND.
     1INT(W4).NE.9.AND.
     1INT(W4).NE.10) THEN
        OUTLYNE='WAVELENGTH NUMBER BEYOND LEGAL BOUNDS'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
      IF(W4.GE.1.0D0.AND.W4.LE.5.0D0) THEN
      IF(SYSTEM(INT(W4)).EQ.0.0D0) THEN
                STOPP=1
                RAYCOD(1)=11
                RAYCOD(2)=NEWOBJ
                REFEXT=.FALSE.
                IF(.NOT.NULL) CALL MACFAL
                        RETURN
                        END IF
                        END IF
      IF(W4.GE.6.0D0.AND.W4.LE.10.0D0) THEN
      IF(SYSTEM(65+INT(W4)).EQ.0.0D0) THEN
                STOPP=1
                RAYCOD(1)=11
                RAYCOD(2)=NEWOBJ
                REFEXT=.FALSE.
                IF(.NOT.NULL) CALL MACFAL
                      RETURN
                      END IF
                      END IF
C
C       PROCESS W5 IF IT IS NOT DEFAULT
C
        IF(DF5.NE.1) THEN
      IF(WC.EQ.'FOB') THEN
C       W5 NOT DEFAULT
               W5=DABS(W5)
C
        NEWOBJ=INT(W5/1000000.0D0)
        NEWREF=INT(DMOD(W5,1000000.0D0)/1000.0D0)
        NEWIMG=INT(DMOD(W5,1000.0D0))
      IF(NEWREF.LE.NEWOBJ.OR.NEWIMG.LE.NEWOBJ.OR.NEWIMG.LE.
     1NEWREF) THEN
        OUTLYNE='INVALID SURFACE ORDER IN SURFACE RE-ASSIGNMENT'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
C
                      DO I=0,NEWREF
      IF(ALENS(25,I).EQ.2.0D0.OR.ALENS(25,I).EQ.3.0D0) THEN
        OUTLYNE='A "TILT AUTO" OR "TILT AUTOM" IS NOT ALLOWED'
      CALL SHOWIT(1)
        OUTLYNE='BEFORE OR ON THE NEW REFERENCE SURFACE'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                NEWOBJ=0
                NEWREF=INT(SYSTEM(25))
                NEWIMG=INT(SYSTEM(20))
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
                        END DO
C
        IF(NEWREF.LT.0.OR.NEWREF.GT.INT(SYSTEM(20)))THEN
        OUTLYNE='NEW REFERENCE SURFACE NUMBER BEYOND LEGAL BOUNDS'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                NEWOBJ=0
                NEWREF=INT(SYSTEM(25))
                NEWIMG=INT(SYSTEM(20))
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
        IF(NEWIMG.LT.0.OR.NEWIMG.GT.INT(SYSTEM(20))) THEN
        OUTLYNE='NEW IMAGE SURFACE NUMBER BEYOND LEGAL BOUNDS'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                NEWOBJ=0
                NEWREF=INT(SYSTEM(25))
                NEWIMG=INT(SYSTEM(20))
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
        IF(NEWOBJ.LT.0.OR.NEWOBJ.GT.INT(SYSTEM(20)))THEN
        OUTLYNE='NEW OBJECT SURFACE NUMBER BEYOND LEGAL BOUNDS'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                NEWOBJ=0
                NEWREF=INT(SYSTEM(25))
                NEWIMG=INT(SYSTEM(20))
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
        IF(NEWOBJ.GE.NEWREF.OR.NEWOBJ.GE.NEWIMG.OR.
     1  NEWREF.GE.NEWIMG)THEN
        OUTLYNE=
     1  'NEW OBJECT/REFERENCE/IMAGE SURFACES ILLEGALLY ORDERED'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                NEWOBJ=0
                NEWREF=INT(SYSTEM(25))
                NEWIMG=INT(SYSTEM(20))
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
                        END IF
      IF(WC.EQ.'IFOB') THEN
                       WW5=W5
                       END IF
C
C       IF W5 WAS DEFAULT, IT WAS SET TO 0.0 ABOVE
                        END IF
C
        IF(WQ.EQ.'NULL') THEN
                        NULL=.TRUE.
                        ELSE
                        NULL=.FALSE.
                        END IF
C
C       NOW THE DEFAULTS HAVE BEEN SET
C       NOW CALL THE SUBROUTINE WHICH STARTS THE REFERENCE
C       RAY RAY TRACE. THIS IS REFRAY.
C
                WWQ=WQ
                WW1=W1
                WW2=W2
                WW3=W3
                WW4=W4
                WVN=WW4
                WW5=W5
      IF(WW4.GE.1.0D0.AND.WW4.LE.5.0D0)
     1LAMBDA=SYSTEM(INT(WW4))
      IF(WW4.GE.6.0D0.AND.WW4.LE.10.0D0)
     1LAMBDA=SYSTEM(65+INT(WW4))
        Y00=WW1
        X00=WW2
        Z0=WW3
C
        IF(WW1.EQ.0.0D0.AND.WW2.EQ.0.0D0.AND.WW3.EQ.0.0D0) THEN
                        FOB0=1
                        FOBB0=.TRUE.
                        ELSE
                        FOB0=0
                        FOBB0=.FALSE.
                        END IF
        IF(WW1.EQ.0.0D0) THEN
                        FOBB0Y=.TRUE.
                        ELSE
                        FOBB0Y=.FALSE.
                        END IF
        IF(WW2.EQ.0.0D0) THEN
                        FOBB0X=.TRUE.
                        ELSE
                        FOBB0X=.FALSE.
                        END IF
C
C       THE CALL TO REFRAY HAS INPUTS:
C               QUALIFIER
C               WW1
C               WW2
C               WW3
C               WW4
C               WW5
C               NEWOBJ
C               NEWREF
C               NEWIMG
C               FOB0 (SET TO 1 IF THE REQUESTED RAY IS THE "GUT" RAY
C                       ELSE SET TO 0 TO TRIGGER A SECOND RAY TO BE
C                       TRACED FIRST.)
C
        IF(FOB0.EQ.0) THEN
C       TRACE THE FOB 0 0 0 RAY 0 0 FIRST THEN TRACE THE REQUESTED
C       RAY
        FOBYES=.TRUE.
        CHLFOB=WWQ
        LFOB(1)=0.0D0
        LFOB(2)=0.0D0
        LFOB(3)=0.0D0
        LFOB(4)=W4
        LFOB(5)=DBLE(NEWOBJ)
        LFOB(6)=DBLE(NEWREF)
        LFOB(7)=DBLE(NEWIMG)
                WW1=0.0D0
                WW2=0.0D0
                WW3=0.0D0
                WW4=LFOB(4)
                WVN=WW4
        TRYX=0.0D0
        TRYY=0.0D0
        TRYZ=0.0D0
        CALL REFRAY(OBJLEVEL)
        CALL IPLANE_TILT
        CALL CLPCEN
      IF(STOPP.EQ.1) THEN
                        REFEXT=.FALSE.
                        CPFNEXT=.FALSE.
                        CALL DELPSF
                        SPDEXT=.FALSE.
                        GSPDEXT=.FALSE.
                        IF(.NOT.NULL) FOBYES=.FALSE.
                STOPP=1
                REFEXT=.FALSE.
        IF(.NOT.NULL) FOBYES=.FALSE.
        IF(.NOT.NULL.AND.F34.EQ.0.AND.F28.EQ.0.AND.F35.EQ.0.AND.
     1F31.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
                        IF(.NOT.NULL) FOBYES=.FALSE.
      IF(SYSTEM(98).NE.0.0D0.OR.SYSTEM(99).NE.0.0D0) THEN
                XRAYER=REFRY(1,NEWOBJ)
                YRAYER=REFRY(2,NEWOBJ)
                ZRAYER=REFRY(3,NEWOBJ)
                        END IF
C       RAY WAS TRACED
        XFOBB0=REFRY(1,NEWOBJ+1)
        YFOBB0=REFRY(2,NEWOBJ+1)
        ZFOBB0=REFRY(3,NEWOBJ+1)
        XLN=REFRY(4,NEWOBJ)
        XMN=REFRY(5,NEWOBJ)
        XNN=REFRY(6,NEWOBJ)
                        END IF
                        ELSE
C       FOB0 WAS 1 SO JUST TRACE THE REQUESTED RAY.
C       THE REQUESTED RAY IS THE GUT RAY, JUST TRACE IT AND RETURN
        FOBYES=.TRUE.
        CHLFOB=WWQ
        LFOB(1)=0.0D0
        LFOB(2)=0.0D0
        LFOB(3)=0.0D0
        LFOB(4)=W4
        LFOB(5)=DBLE(NEWOBJ)
        LFOB(6)=DBLE(NEWREF)
        LFOB(7)=DBLE(NEWIMG)
                WW1=0.0D0
                WW2=0.0D0
                WW3=0.0D0
                WW4=LFOB(4)
                WVN=WW4
        TRYX=0.0D0
        TRYY=0.0D0
        TRYZ=0.0D0
        CALL REFRAY(OBJLEVEL)
        CALL IPLANE_TILT
        CALL CLPCEN
      IF(STOPP.EQ.1) THEN
                        REFEXT=.FALSE.
                        CPFNEXT=.FALSE.
                        CALL DELPSF
                        SPDEXT=.FALSE.
                        GSPDEXT=.FALSE.
                        IF(.NOT.NULL) FOBYES=.FALSE.
C       NO REF RAY TRACED
                        REFEXT=.FALSE.
        IF(.NOT.NULL.AND.F34.EQ.0.AND.F28.EQ.0.AND.F35.EQ.0.AND.
     1F31.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
      IF(SYSTEM(98).NE.0.0D0.OR.SYSTEM(99).NE.0.0D0) THEN
                XRAYER=REFRY(1,NEWOBJ)
                YRAYER=REFRY(2,NEWOBJ)
                ZRAYER=REFRY(3,NEWOBJ)
                        END IF
C       GUT RAY WAS TRACED.
C       DIRECTION COSINES OF THE Z-AXIS ARE:
        XLN=0.0D0
        XMN=0.0D0
        XNN=1.0D0
                        END IF
                        DO J=0,INT(SYSTEM(20))
      IF(ALENS(34,J).EQ.18.0D0) LDIF2=.FALSE.
      IF(ALENS(34,J).EQ.18.0D0) LDIF=.FALSE.
                        END DO
        IF(LDIF2) THEN
C       TRACE DIFFERENTIAL RAY WITHRESPECT TO AN OBJECT POINT
C       SHIFT
C       SAVE GUT RAY DATA
                        GUTRAY(1:50,0:499)=REFRY(1:50,0:499)
C       SAVE REFRY DATA
                        SAVE=REFRY
        SREFDIFEXT=.FALSE.
        DIFRAYTRACE=.TRUE.
        CALL FRFDIF
        DIFRAYTRACE=.FALSE.
        IF(STOPP.EQ.1) THEN
      STOPP=0
                        REFRY=SAVE
                        IF(WQ.NE.'NULL'.AND.F34.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
                        END IF
C       RESTORE REFRY DATA
                        REFRY=SAVE
C       NO DIFERENTIAL TRACE
                        END IF
C       RESET FOB0 TO 0 AND SET THE REFRAY EXISTENCE FLAG TO TRUE
                        FOB0=0
                        REFEXT=.TRUE.
        ANGLE1=REFRY(11,NEWOBJ)*180.0D0/PII
        ANGLE2=REFRY(12,NEWOBJ)*180.0D0/PII
        YK=REFRY(2,NEWIMG)
        XK=REFRY(1,NEWIMG)
C       REFERENCE RAY WAS TRACED, NOW TRACE THE DIFFERENTIAL RAY
C       DATA IF FOB HAS QUALIFIERS PFS OR PFSNH.
        IF(WQ.EQ.'PFS'.OR.WQ.EQ.'PFSNH') THEN
C       SAVE REFRY DATA
                        SAVE=REFRY
        STOPP=0
        SREFDIFEXT=.FALSE.
        DIFRAYTRACE=.TRUE.
        CALL FOBDIF
        DIFRAYTRACE=.FALSE.
        IF(REFEXT) STOPP=0
        IF(STOPP.EQ.1) THEN
        STOPP=0
C       RESTORE REFRY DATA
                        REFRY=SAVE
                        IF(WQ.NE.'NULL'.AND.F34.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
                        END IF
C       RESTORE REFRY DATA
                        REFRY=SAVE
C       PROCEED
                        END IF
C       THE TRACE FOR FOB0=1 IS COMPLETED
                        RETURN
                        END IF
C       THE FOB0 WAS 0, A 000 GUT RAY WAS TRACED, NOW TRACE THE
C       REQUESTED RAY WHICH WAS NOT 0 0 0.
C       NOW TRACE THE REQUESTED CHIEF RAY IF IT WAS NOT 0 0 0
C
        FOBYES=.TRUE.
        CHLFOB=WWQ
                WW1=W1
                WW2=W2
                WW3=W3
                WW4=W4
        LFOB(1)=WW1
        LFOB(2)=WW2
        LFOB(3)=WW3
        LFOB(4)=WW4
        LFOB(5)=DBLE(NEWOBJ)
        LFOB(6)=DBLE(NEWREF)
        LFOB(7)=DBLE(NEWIMG)
                WWQ=WQ
                WVN=WW4
                WW5=W5
C
      IF(SYSTEM(18).EQ.1.AND.NEWOBJ.EQ.0.OR.
     1SYSTEM(19).EQ.1.AND.NEWOBJ.EQ.0) THEN
C     OBJECT ANGLES INPUT
      ANGIN=.TRUE.
C     REF OBJ HT INPUT AS ANGLE
C     IN THIS CASE, FOB INDICATES FRACTIONS OF FIELD ANGLES
C     BUT ONLY IF THE NEWOBJ SURFACE IS SURFACE 0
C
C     THIS NEEDS MODIFICATION FOR FRACTIONS OF FIELD ANGLES
C
      ANGJK1=AWW1
      ANGJK2=AWW2

      IF(ANGJK1.GT.89.9999D0.AND.
     1ANGJK1.LT.90.0001D0)  ANGJK1=89.9999D0

      IF(ANGJK1.LT.-89.9999D0.AND.
     1ANGJK1.GT.-90.0001D0) ANGJK1=-89.9999D0

      IF(ANGJK1.GT.-270.0001D0.AND.
     1ANGJK1.LT.-269.9999D0) ANGJK1=89.9999D0

      IF(ANGJK1.GT.269.9999D0.AND.
     1ANGJK1.LT.270.0001D0) ANGJK1=-89.9999D0

      IF(ANGJK2.GT.89.9999D0.AND.
     1ANGJK2.LT.90.0001D0)  ANGJK2=89.9999D0

      IF(ANGJK2.LT.-89.9999D0.AND.
     1ANGJK2.GT.-90.0001D0) ANGJK2=-89.9999D0

      IF(ANGJK2.GT.-270.0001D0.AND.
     1ANGJK2.LT.-269.9999D0) ANGJK2=89.9999D0

      IF((W2*SYSTEM(23)).GT.269.9999D0.AND.
     1ANGJK2.LT.270.0001D0) ANGJK2=-89.9999D0

C     ANGLES IN RADIANS ARE:
C
      YYANG=ANGJK1*PII/180.0D0
      XXANG=ANGJK2*PII/180.0D0
C
      IF(DABS(ANGJK1).GT.90.0D0) YSTRT=DTAN(YYANG)*ALENS(3,NEWOBJ)
      IF(DABS(ANGJK1).LE.90.0D0) YSTRT=-DTAN(YYANG)*ALENS(3,NEWOBJ)
C
      IF(DABS(ANGJK2).GT.90.0D0) XSTRT=DTAN(XXANG)*ALENS(3,NEWOBJ)
      IF(DABS(ANGJK2).LE.90.0D0) XSTRT=-DTAN(XXANG)*ALENS(3,NEWOBJ)

      ZSTRT=2.0D0*ALENS(3,NEWOBJ)

      IF(ANGJK1.GT.90.0D0.AND.ANGJK1.LT.270.0D0.OR.
     1ANGJK1.LT.-90.0D0.AND.ANGJK1.GT.-270.0D0) THEN
C
      IF(ANGJK2.GT.90.0D0.AND.ANGJK2.LT.270.0D0.OR.
     1ANGJK2.LT.-90.0D0.AND.ANGJK2.GT.-270.0D0) THEN
                       ZSTRT=0.0D0
                       END IF
                       END IF
      IF(ANGJK2.GT.90.0D0.AND.ANGJK2.LT.270.0D0.OR.
     1ANGJK2.LT.-90.0D0.AND.ANGJK2.GT.-270.0D0) THEN
C
      IF(ANGJK1.GT.90.0D0.AND.ANGJK1.LT.270.0D0.OR.
     1ANGJK1.LT.-90.0D0.AND.ANGJK1.GT.-270.0D0) THEN
                       ZSTRT=0.0D0
                       END IF
                       END IF
      IF(ANGJK1.GE.0.0D0.AND.ANGJK1.LT.90.0D0.OR.
     1ANGJK1.LE.0.0D0.AND.ANGJK1.GT.-90.0D0.OR.
     1ANGJK1.GT.270.0D0.AND.ANGJK1.LE.0.0D0.OR.
     1ANGJK1.LT.-270.0D0.AND.ANGJK1.GE.0.0D0) THEN
C
      IF(ANGJK2.GE.0.0D0.AND.ANGJK2.LT.90.0D0.OR.
     1ANGJK2.LE.0.0D0.AND.ANGJK2.GT.-90.0D0.OR.
     1ANGJK2.GT.270.0D0.AND.ANGJK2.LE.0.0D0.OR.
     1ANGJK2.LT.-270.0D0.AND.ANGJK2.GE.0.0D0) THEN
                       ZSTRT=0.0D0
                       END IF
                       END IF
      IF(ANGJK2.GE.0.0D0.AND.ANGJK2.LT.90.0D0.OR.
     1ANGJK2.LE.0.0D0.AND.ANGJK2.GT.-90.0D0.OR.
     1ANGJK2.GT.270.0D0.AND.ANGJK2.LE.0.0D0.OR.
     1ANGJK2.LT.-270.0D0.AND.ANGJK2.GE.0.0D0) THEN
C
      IF(ANGJK1.GE.0.0D0.AND.ANGJK1.LT.90.0D0.OR.
     1ANGJK1.LE.0.0D0.AND.ANGJK1.GT.-90.0D0.OR.
     1ANGJK1.GT.270.0D0.AND.ANGJK1.LE.0.0D0.OR.
     1ANGJK1.LT.-270.0D0.AND.ANGJK1.GE.0.0D0) THEN
                       ZSTRT=0.0D0
                       END IF
                       END IF
      IF(SYSTEM(18).EQ.1.0D0.AND.SYSTEM(21).EQ.0.0D0) YSTRT=0.0D0
      IF(SYSTEM(19).EQ.1.0D0.AND.SYSTEM(23).EQ.0.0D0) XSTRT=0.0D0
      IF(XSTRT.EQ.0.0D0.AND.YSTRT.EQ.0.0D0) ZSTRT=0.0D0
                       ELSE
C     OBJECT ANGLES NOT INPUT
      ANGIN=.FALSE.
C
      IF(ALENS(9,NEWOBJ).EQ.0.OR.ALENS(127,NEWOBJ).NE.0.0D0) THEN
C     THE STARTING RAY COORDINATES ON THE OBJECT SURAFCE
C     ARE NOT DETERMINED BY THE CLAP ON THE OBJECT SURFACE
C     USING SCY AND SCX, FOB REPRESENTS FRACTIONS OF IMAGE HEIGHT
C     AND DEPTH.
      IF(SYSTEM(16).NE.0.0D0) XSTRT=W2*PXTRAX(5,NEWOBJ)
      IF(SYSTEM(16).EQ.0.0D0) XSTRT=0.0D0
      IF(SYSTEM(14).NE.0.0D0) YSTRT=W1*PXTRAY(5,NEWOBJ)
      IF(SYSTEM(14).EQ.0.0D0) YSTRT=0.0D0
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
               IF(SAGERR) THEN
                STOPP=1
                RAYCOD(1)=16
                RAYCOD(2)=NEWOBJ
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
        ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
                        ELSE
C     USE Y HEIGHT OF CLAP WITH OFFSETS
      IF(ALENS(9,NEWOBJ).EQ.1.0D0.AND.ALENS(127,NEWOBJ).EQ.0.0D0) THEN
      IF(ALENS(10,NEWOBJ).LE.ALENS(11,NEWOBJ)) THEN
      XSTRT=W2*(ALENS(10,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(10,NEWOBJ)+ALENS(12,NEWOBJ))
                           ELSE
      XSTRT=W2*(ALENS(11,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(11,NEWOBJ)+ALENS(12,NEWOBJ))
                           END IF
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
               IF(SAGERR) THEN
                STOPP=1
                RAYCOD(1)=16
                RAYCOD(2)=NEWOBJ
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
      ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
                        END IF
      IF(ALENS(9,NEWOBJ).EQ.5.0D0.AND.ALENS(127,NEWOBJ).EQ.0.0D0) THEN
      XSTRT=W2*(ALENS(10,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(10,NEWOBJ)+ALENS(12,NEWOBJ))
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
               IF(SAGERR) THEN
                STOPP=1
                RAYCOD(1)=16
                RAYCOD(2)=NEWOBJ
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
      ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
                        END IF
      IF(ALENS(9,NEWOBJ).GT.1.0D0.AND.ALENS(9,NEWOBJ).LT.5.0D0.AND.
     1ALENS(127,NEWOBJ).EQ.0.0D0) THEN
      XSTRT=W2*(ALENS(11,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(10,NEWOBJ)+ALENS(12,NEWOBJ))
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
               IF(SAGERR) THEN
                STOPP=1
                RAYCOD(1)=16
                RAYCOD(2)=NEWOBJ
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
      ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
                        END IF
                        END IF
                        END IF
C     NOW THE INITIAL AIMING POINT AT NEWOBJ+1
      IF(ANGIN) THEN
        X1AIM=PXTRAX(5,(NEWOBJ+1))
        X1AIM=(X1AIM*W2)-ALENS(31,NEWOBJ+1)+SYSTEM(81)
        Y1AIM=PXTRAY(5,(NEWOBJ+1))
        Y1AIM=(Y1AIM*W1)-ALENS(30,NEWOBJ+1)+SYSTEM(82)
        Z1AIM=SYSTEM(89)
                       ELSE
C     NOT ANGIN
        X1AIM=PXTRAX(5,(NEWOBJ+1))
        X1AIM=(X1AIM*W2)-ALENS(31,NEWOBJ+1)+SYSTEM(81)
        Y1AIM=PXTRAY(5,(NEWOBJ+1))
        Y1AIM=(Y1AIM*W1)-ALENS(30,NEWOBJ+1)+SYSTEM(82)
      IISURF=NEWOBJ+1
      CALL SAGRET(IISURF,X1AIM,Y1AIM,ZSAG,SAGERR)
               IF(SAGERR) THEN
                STOPP=1
                RAYCOD(1)=16
                RAYCOD(2)=NEWOBJ
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
        Z1AIM=SYSTEM(89)+ZSAG
                       END IF
      TRYX=X1AIM
      TRYY=Y1AIM
      TRYZ=Z1AIM
      XJIM=TRYX
      YJIM=TRYY
      ZJIM=TRYZ
      XC=TRYX
      YC=TRYY
      ZC=TRYZ
      ZEEERR=.FALSE.
      IF(ALENS(1,NEWOBJ+1).NE.0.0D0)
     1CALL GETZEE1
C     STARTING INTERSECTION POINT ON SURF 1 IS
      TRYX=XC
      TRYY=YC
      TRYZ=ZC
        FACTER=-(DABS(SERLIM)+DABS(SERINC))
        ICNTEST=INT(DABS(2.0D0*SERLIM)/DABS(SERINC))
        ICNT=0
 992  CONTINUE
      CALL REFRAY(OBJLEVEL)
        CALL IPLANE_TILT
        CALL CLPCEN
      IF(STOPP.EQ.1.AND.ICNT.LE.ICNTEST) THEN
               STOPP=0
               REFEXT=.TRUE.
C       INITIALIZE RAYCOD
                RAYCOD(1)=0
                RAYCOD(2)=-1
        FACTER=FACTER+DABS(SERINC)
      XA=XJIM
      YA=YJIM
      ZA=ZJIM
      TRYX=XA
      TRYY=YA
      TRYZ=ZA+FACTER
      XC=TRYX
      YC=TRYY
      ZC=TRYZ
      ZEEERR=.FALSE.
      IF(ALENS(1,NEWOBJ+1).NE.0.0D0)
     1CALL GETZEE1
      IF(ZEEERR) THEN
      XC=TRYX
      YC=TRYY
      ZC=TRYZ
          END IF
        TRYX=XC
        TRYY=YC
        TRYZ=ZC
               ICNT=ICNT+1
               GO TO 992
                       END IF
        FACTER=-(DABS(SERLIM)+DABS(SERINC))
        ICNTEST=INT(DABS(2.0D0*SERLIM)/DABS(SERINC))
        ICNT=0
      IF(STOPP.EQ.1) THEN
 993  CONTINUE
      IF(ICNT.NE.0) THEN
      CALL REFRAY(OBJLEVEL)
      CALL IPLANE_TILT
        CALL CLPCEN
      END IF
      IF(STOPP.EQ.1.AND.ICNT.LE.ICNTEST) THEN
               STOPP=0
               REFEXT=.TRUE.
C       INITIALIZE RAYCOD
                RAYCOD(1)=0
                RAYCOD(2)=-1
        FACTER=FACTER+DABS(SERINC)
      XA=0.0D0
      YA=0.0D0
      ZA=FACTER
      TRYX=XA
      TRYY=YA
      TRYZ=ZA
      XC=TRYX
      YC=TRYY
      ZC=TRYZ
      ZEEERR=.FALSE.
      IF(ALENS(1,NEWOBJ+1).NE.0.0D0)
     1CALL GETZEE1
      IF(ZEEERR) THEN
      XC=TRYX
      YC=TRYY
      ZC=TRYZ
          END IF
        TRYX=XC
        TRYY=YC
        TRYZ=ZC
               ICNT=ICNT+1
               GO TO 993
                       END IF
                       END IF
      IF(STOPP.EQ.1.AND.ICNT.GT.ICNTEST) THEN
                        REFEXT=.FALSE.
                        CPFNEXT=.FALSE.
                        CALL DELPSF
                        SPDEXT=.FALSE.
                        GSPDEXT=.FALSE.
                        IF(.NOT.NULL) FOBYES=.FALSE.
C       NO REF RAY TRACED
                        REFEXT=.FALSE.
        IF(.NOT.NULL.AND.F34.EQ.0.AND.F28.EQ.0.AND.F35.EQ.0.AND.
     1F31.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
                        END IF
                        STOPP=0
                        REFEXT=.TRUE.
                        FOBYES=.TRUE.
                        DO J=0,INT(SYSTEM(20))
      IF(ALENS(34,J).EQ.18.0D0) LDIF2=.FALSE.
      IF(ALENS(34,J).EQ.18.0D0) LDIF=.FALSE.
                        END DO
                IF(LDIF2) THEN
C       TRACE DIFFERENTIAL RAY WITHRESPECT TO AN OBJECT POINT
C       SHIFT
C       SAVE GUT RAY DATA
                        GUTRAY(1:50,0:499)=REFRY(1:50,0:499)
C       SAVE REFRY DATA
                        SAVE=REFRY
        SREFDIFEXT=.FALSE.
        DIFRAYTRACE=.TRUE.
        CALL FRFDIF
        DIFRAYTRACE=.FALSE.
        IF(STOPP.EQ.1) THEN
      STOPP=0
C       RESTORE REFRY DATA
                        REFRY=SAVE
                        IF(WQ.NE.'NULL'.AND.F34.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
                        END IF
C       RESTORE REFRY DATA
                        REFRY=SAVE
C       NO DIF TRACE
                        END IF
C
                        FOB0=0
C
C       THE RETURN WILL SEND BACK VIA THE COMMON/REFR COMMON
C       THE FULL RAY DATA FOR THE FULLY AIMMED AND TRACED REFERENCE
C       RAY. THEN ANY APPROPRIATE FOB RELATED PRINTOUT WILL BE DONE.
        YK=REFRY(2,NEWIMG)
        XK=REFRY(1,NEWIMG)
        ANGLE1=REFRY(11,NEWOBJ)*180.0D0/PII
        ANGLE2=REFRY(12,NEWOBJ)*180.0D0/PII
C       REFERENCE RAY WAS TRACED, NOW TRACE THE DIFFERENTIAL RAY
C       DATA IF FOB HAS QUALIFIERS PFS OR PFSNH.
C
                        STOPP=0
                        REFEXT=.TRUE.
                        RETURN
 250    FORMAT('REFERENCE RAY TRACED')
 300    FORMAT('WAVELENGTH = ',G12.4,' MICRONS')
 302    FORMAT('X-FIELD ANGLE = ',G15.7,1X,A3)
 303    FORMAT('Y-FIELD ANGLE = ',G15.7,1X,A3)
 3302   FORMAT('X-OBJECT HEIGHT = ',G15.7,1X,A3)
 3303   FORMAT('Y-OBJECT HEIGHT = ',G15.7,1X,A3)
 401    FORMAT(G10.2,2X,G10.2,2X,G10.2,2X,G14.6,2X,G14.6)
 350    FORMAT(2X,'Y-FOB     ',2X,'X-FOB     ',2X,'DELTA-Z   ')
 400    FORMAT(2X,'Y-FOB     ',2X,'X-FOB     ',2X,'DELTA-Z',
     1  2X,'Y-IMAGE HT. ',4X,'X-IMAGE HT. ')
 101    FORMAT(2X,'Y-FOB     ',2X,'X-FOB     ',2X,'DELTA-Z   ',
     1  2X,'T-FOCUS POS.',4X,'S-FOCUS POS.')
 301    FORMAT(G10.2,2X,G10.2,2X,G10.2)
                        END
C SUB FASTFFOB.FOR
        SUBROUTINE FASTFFOB(WPAS)
        USE GLOBALS
C
        IMPLICIT NONE
C
        INTEGER IISURF,FFT,FFS,I,J,ICNT,ICNTEST
C
      CHARACTER LUNI*3
C
      REAL*8 XPFOB,YPFOB,WPAS
      COMMON/PFOB/XPFOB,YPFOB,LUNI
C
         REAL*8 FT,FS,X00,Y00,Z0
     1  ,LAMBDA,ANGLE1,XK,YK,XLN,XMN,XNN,XFOBB0,YFOBB0,ZFOBB0
     2  ,ANGJK1,ANGJK2,XXANG,YYANG,FACTER,XA,YA,ZA,XJIM,YJIM,ZJIM
     3,ZSAG,ANGLE2,SCLFACY,SCLFACX,AWW1,AWW2
C
      REAL*8 XRAYER,YRAYER,ZRAYER
      COMMON/RAYERPASS/XRAYER,YRAYER,ZRAYER
C
      COMMON/OHFOB/XFOBB0,YFOBB0,ZFOBB0
C
        LOGICAL FANEXT,ZEEERR,SAGERR
        COMMON/ERRZEE/ZEEERR
C
        COMMON/FANEXI/FANEXT
C
C       FOBB0=TRUE FOR ON AXIS AND FALSE FOR OFF AXIS
        LOGICAL FOBB0,FOBB0X,FOBB0Y
C
        COMMON/FFOOBB/FOBB0,FOBB0X,FOBB0Y
C
      COMMON/FBDIFF/FS,FT,FFS,FFT
C
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATSPD.INC'
        FOBRUN=.TRUE.
C
      DLLX=0.0D0
      DLLY=0.0D0
      DLLZ=0.0D0
C     SET UP ANGLE FACTERS
      SCLFACY=1.0D0
      SCLFACX=1.0D0
      AWW1=W1/SCLFACY
      AWW2=W2/SCLFACX
C
C     REF OBJ HT INPUT AS ANGLE
C     IN THIS CASE, FOB INDICATES FRACTIONS OF FIELD ANGLES
C     BUT ONLY IF THE NEWOBJ SURFACE IS SURFACE 0
C
C     THIS NEEDS MODIFICATION FOR FRACTIONS OF FIELD ANGLES
C
      ANGJK1=AWW1
      ANGJK2=AWW2

      ZSTRT=2.0D0*ALENS(3,NEWOBJ)
C     OBJECT ANGLES NOT INPUT
      ANGIN=.FALSE.
C
C     THE STARTING RAY COORDINATES ON THE OBJECT SURAFCE
C     ARE NOT DETERMINED BY THE CLAP ON THE OBJECT SURFACE
C     USING SCY AND SCX, FOB REPRESENTS FRACTIONS OF IMAGE HEIGHT
C     AND DEPTH.
      IF(SYSTEM(16).NE.0.0D0) XSTRT=W2*PXTRAX(5,NEWOBJ)
      IF(SYSTEM(16).EQ.0.0D0) XSTRT=0.0D0
      IF(SYSTEM(14).NE.0.0D0) YSTRT=W1*PXTRAY(5,NEWOBJ)
      IF(SYSTEM(14).EQ.0.0D0) YSTRT=0.0D0
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
               IF(SAGERR) THEN
                STOPP=1
                RAYCOD(1)=16
                RAYCOD(2)=NEWOBJ
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
        ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
C
C     NOT ANGIN
        X1AIM=PXTRAX(5,(NEWOBJ+1))
        X1AIM=(X1AIM*W2)-ALENS(31,NEWOBJ+1)+SYSTEM(81)
        Y1AIM=PXTRAY(5,(NEWOBJ+1))
        Y1AIM=(Y1AIM*W1)-ALENS(30,NEWOBJ+1)+SYSTEM(82)
      IISURF=NEWOBJ+1
      CALL SAGRET(IISURF,X1AIM,Y1AIM,ZSAG,SAGERR)
               IF(SAGERR) THEN
                STOPP=1
                RAYCOD(1)=16
                RAYCOD(2)=NEWOBJ
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
        Z1AIM=SYSTEM(89)+ZSAG
      TRYX=X1AIM
      TRYY=Y1AIM
      TRYZ=Z1AIM
      XC=TRYX
      YC=TRYY
      ZC=TRYZ
      ZEEERR=.FALSE.
      IF(ALENS(1,NEWOBJ+1).NE.0.0D0)
     1CALL GETZEE1
C     STARTING INTERSECTION POINT ON SURF 1 IS
      TRYX=XC
      TRYY=YC
      TRYZ=ZC
C
                        FANEXT=.FALSE.
C
C       INITIALIZE RAYCOD
                RAYCOD(1)=0
                RAYCOD(2)=-1
C
C       SET RAYEXT TO FALSE AND FAIL TO TRUE
                        RAYEXT=.FALSE.
                        POLEXT=.FALSE.
                        FAIL=.TRUE.
C       SET REFEXT AND STOPP
                        STOPP=0
                        REFEXT=.TRUE.
                        SPDEXT=.FALSE.
                        GSPDEXT=.FALSE.
                        CPFNEXT=.FALSE.
C
                FT=0.0D0
                FS=0.0D0
C
C       THIS MAKES FOB EQUAL FOB 0 0 0 (CW) 0
C
                        NULL=.FALSE.
C
C       NOW THE DEFAULTS HAVE BEEN SET
C       NOW CALL THE SUBROUTINE WHICH STARTS THE REFERENCE
C       RAY RAY TRACE. THIS IS REFRAY.
C
                WWQ=WQ
                WW1=W1
                WW2=W2
                WW3=W3
                WW4=W4
                WVN=WW4
                WW5=W5
      IF(WW4.GE.1.0D0.AND.WW4.LE.5.0D0)
     1LAMBDA=SYSTEM(INT(WW4))
      IF(WW4.GE.6.0D0.AND.WW4.LE.10.0D0)
     1LAMBDA=SYSTEM(65+INT(WW4))
        Y00=WW1
        X00=WW2
        Z0=WW3
C
        IF(WW1.EQ.0.0D0.AND.WW2.EQ.0.0D0.AND.WW3.EQ.0.0D0) THEN
                        FOB0=1
                        FOBB0=.TRUE.
                        ELSE
                        FOB0=0
                        FOBB0=.FALSE.
                        END IF
        IF(WW1.EQ.0.0D0) THEN
                        FOBB0Y=.TRUE.
                        ELSE
                        FOBB0Y=.FALSE.
                        END IF
        IF(WW2.EQ.0.0D0) THEN
                        FOBB0X=.TRUE.
                        ELSE
                        FOBB0X=.FALSE.
                        END IF
C
C       THE CALL TO REFRAY HAS INPUTS:
C               QUALIFIER
C               WW1
C               WW2
C               WW3
C               WW4
C               WW5
C               NEWOBJ
C               NEWREF
C               NEWIMG
C               FOB0 (SET TO 1 IF THE REQUESTED RAY IS THE "GUT" RAY
C                       ELSE SET TO 0 TO TRIGGER A SECOND RAY TO BE
C                       TRACED FIRST.)
C
        IF(FOB0.EQ.0) THEN
C       TRACE THE FOB 0 0 0 RAY 0 0 FIRST THEN TRACE THE REQUESTED
C       RAY
        FOBYES=.TRUE.
        CHLFOB=WWQ
        LFOB(1)=0.0D0
        LFOB(2)=0.0D0
        LFOB(3)=0.0D0
        LFOB(4)=W4
        LFOB(5)=DBLE(NEWOBJ)
        LFOB(6)=DBLE(NEWREF)
        LFOB(7)=DBLE(NEWIMG)
                WW1=0.0D0
                WW2=0.0D0
                WW3=0.0D0
                WW4=LFOB(4)
                WVN=WW4
        TRYX=0.0D0
        TRYY=0.0D0
        TRYZ=0.0D0
        CALL REFRAY(OBJLEVEL)
      CALL IPLANE_TILT
        CALL CLPCEN
      IF(STOPP.EQ.1) THEN
                        REFEXT=.FALSE.
                        CPFNEXT=.FALSE.
                        SPDEXT=.FALSE.
                        GSPDEXT=.FALSE.
                        IF(.NOT.NULL) FOBYES=.FALSE.
                STOPP=1
                REFEXT=.FALSE.
        IF(.NOT.NULL) FOBYES=.FALSE.
        IF(.NOT.NULL.AND.F34.EQ.0.AND.F28.EQ.0.AND.F35.EQ.0.AND.
     1F31.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
                        IF(.NOT.NULL) FOBYES=.FALSE.
      IF(SYSTEM(98).NE.0.0D0.OR.SYSTEM(99).NE.0.0D0) THEN
                XRAYER=REFRY(1,NEWOBJ)
                YRAYER=REFRY(2,NEWOBJ)
                ZRAYER=REFRY(3,NEWOBJ)
                        END IF
C       RAY WAS TRACED
        XFOBB0=REFRY(1,NEWOBJ+1)
        YFOBB0=REFRY(2,NEWOBJ+1)
        ZFOBB0=REFRY(3,NEWOBJ+1)
        XLN=REFRY(4,NEWOBJ)
        XMN=REFRY(5,NEWOBJ)
        XNN=REFRY(6,NEWOBJ)
                        END IF
                        ELSE
C       FOB0 WAS 1 SO JUST TRACE THE REQUESTED RAY.
C       THE REQUESTED RAY IS THE GUT RAY, JUST TRACE IT AND RETURN
        FOBYES=.TRUE.
        CHLFOB=WWQ
        LFOB(1)=0.0D0
        LFOB(2)=0.0D0
        LFOB(3)=0.0D0
        LFOB(4)=W4
        LFOB(5)=DBLE(NEWOBJ)
        LFOB(6)=DBLE(NEWREF)
        LFOB(7)=DBLE(NEWIMG)
                WW1=0.0D0
                WW2=0.0D0
                WW3=0.0D0
                WW4=LFOB(4)
                WVN=WW4
        TRYX=0.0D0
        TRYY=0.0D0
        TRYZ=0.0D0
        CALL REFRAY(OBJLEVEL)
      CALL IPLANE_TILT
        CALL CLPCEN
      IF(STOPP.EQ.1) THEN
                        REFEXT=.FALSE.
                        CPFNEXT=.FALSE.
                        SPDEXT=.FALSE.
                        GSPDEXT=.FALSE.
                        IF(.NOT.NULL) FOBYES=.FALSE.
C       NO REF RAY TRACED
                        REFEXT=.FALSE.
        IF(.NOT.NULL.AND.F34.EQ.0.AND.F28.EQ.0.AND.F35.EQ.0.AND.
     1F31.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
      IF(SYSTEM(98).NE.0.0D0.OR.SYSTEM(99).NE.0.0D0) THEN
                XRAYER=REFRY(1,NEWOBJ)
                YRAYER=REFRY(2,NEWOBJ)
                ZRAYER=REFRY(3,NEWOBJ)
                        END IF
C       GUT RAY WAS TRACED.
C       DIRECTION COSINES OF THE Z-AXIS ARE:
        XLN=0.0D0
        XMN=0.0D0
        XNN=1.0D0
                        END IF
C       RESET FOB0 TO 0 AND SET THE REFRAY EXISTENCE FLAG TO TRUE
                        FOB0=0
                        REFEXT=.TRUE.
        ANGLE1=REFRY(11,NEWOBJ)*180.0D0/PII
        ANGLE2=REFRY(12,NEWOBJ)*180.0D0/PII
        YK=REFRY(2,NEWIMG)
        XK=REFRY(1,NEWIMG)
C       THE TRACE FOR FOB0=1 IS COMPLETED
                        RETURN
                        END IF
C       THE FOB0 WAS 0, A 000 GUT RAY WAS TRACED, NOW TRACE THE
C       REQUESTED RAY WHICH WAS NOT 0 0 0.
C       NOW TRACE THE REQUESTED CHIEF RAY IF IT WAS NOT 0 0 0
C
        FOBYES=.TRUE.
        CHLFOB=WWQ
                WW1=W1
                WW2=W2
                WW3=W3
                WW4=W4
        LFOB(1)=WW1
        LFOB(2)=WW2
        LFOB(3)=WW3
        LFOB(4)=WW4
        LFOB(5)=DBLE(NEWOBJ)
        LFOB(6)=DBLE(NEWREF)
        LFOB(7)=DBLE(NEWIMG)
                WWQ=WQ
                WVN=WW4
                WW5=W5
C
C     OBJECT ANGLES NOT INPUT
      ANGIN=.FALSE.
C
      IF(SYSTEM(16).NE.0.0D0) XSTRT=W2*PXTRAX(5,NEWOBJ)
      IF(SYSTEM(16).EQ.0.0D0) XSTRT=0.0D0
      IF(SYSTEM(14).NE.0.0D0) YSTRT=W1*PXTRAY(5,NEWOBJ)
      IF(SYSTEM(14).EQ.0.0D0) YSTRT=0.0D0
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
               IF(SAGERR) THEN
                STOPP=1
                RAYCOD(1)=16
                RAYCOD(2)=NEWOBJ
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
        ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
C     NOW THE INITIAL AIMING POINT AT NEWOBJ+1
      IF(ANGIN) THEN
        X1AIM=PXTRAX(5,(NEWOBJ+1))
        X1AIM=(X1AIM*W2)-ALENS(31,NEWOBJ+1)+SYSTEM(81)
        Y1AIM=PXTRAY(5,(NEWOBJ+1))
        Y1AIM=(Y1AIM*W1)-ALENS(30,NEWOBJ+1)+SYSTEM(82)
        Z1AIM=SYSTEM(89)
                       ELSE
C     NOT ANGIN
        X1AIM=PXTRAX(5,(NEWOBJ+1))
        X1AIM=(X1AIM*W2)-ALENS(31,NEWOBJ+1)+SYSTEM(81)
        Y1AIM=PXTRAY(5,(NEWOBJ+1))
        Y1AIM=(Y1AIM*W1)-ALENS(30,NEWOBJ+1)+SYSTEM(82)
      IISURF=NEWOBJ+1
      CALL SAGRET(IISURF,X1AIM,Y1AIM,ZSAG,SAGERR)
               IF(SAGERR) THEN
                STOPP=1
                RAYCOD(1)=16
                RAYCOD(2)=NEWOBJ
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
        Z1AIM=SYSTEM(89)+ZSAG
                       END IF
      TRYX=X1AIM
      TRYY=Y1AIM
      TRYZ=Z1AIM
      XJIM=TRYX
      YJIM=TRYY
      ZJIM=TRYZ
      XC=TRYX
      YC=TRYY
      ZC=TRYZ
      ZEEERR=.FALSE.
      IF(ALENS(1,NEWOBJ+1).NE.0.0D0)
     1CALL GETZEE1
C     STARTING INTERSECTION POINT ON SURF 1 IS
      TRYX=XC
      TRYY=YC
      TRYZ=ZC
        FACTER=-(DABS(SERLIM)+DABS(SERINC))
        ICNTEST=INT(DABS(2.0D0*SERLIM)/DABS(SERINC))
        ICNT=0
 992  CONTINUE
      CALL REFRAY(WPAS)
      CALL IPLANE_TILT
        CALL CLPCEN
      IF(STOPP.EQ.1.AND.ICNT.LE.ICNTEST) THEN
               STOPP=0
               REFEXT=.TRUE.
C       INITIALIZE RAYCOD
                RAYCOD(1)=0
                RAYCOD(2)=-1
        FACTER=FACTER+DABS(SERINC)
      XA=XJIM
      YA=YJIM
      ZA=ZJIM
      TRYX=XA
      TRYY=YA
      TRYZ=ZA+FACTER
      XC=TRYX
      YC=TRYY
      ZC=TRYZ
      ZEEERR=.FALSE.
      IF(ALENS(1,NEWOBJ+1).NE.0.0D0)
     1CALL GETZEE1
      IF(ZEEERR) THEN
      XC=TRYX
      YC=TRYY
      ZC=TRYZ
          END IF
        TRYX=XC
        TRYY=YC
        TRYZ=ZC
               ICNT=ICNT+1
               GO TO 992
                       END IF
        FACTER=-(DABS(SERLIM)+DABS(SERINC))
        ICNTEST=INT(DABS(2.0D0*SERLIM)/DABS(SERINC))
        ICNT=0
      IF(STOPP.EQ.1) THEN
 993  CONTINUE
      IF(ICNT.NE.0) THEN
      CALL REFRAY(WPAS)
      CALL IPLANE_TILT
        CALL CLPCEN
      END IF
      IF(STOPP.EQ.1.AND.ICNT.LE.ICNTEST) THEN
               STOPP=0
               REFEXT=.TRUE.
C       INITIALIZE RAYCOD
                RAYCOD(1)=0
                RAYCOD(2)=-1
        FACTER=FACTER+DABS(SERINC)
      XA=0.0D0
      YA=0.0D0
      ZA=FACTER
      TRYX=XA
      TRYY=YA
      TRYZ=ZA
      XC=TRYX
      YC=TRYY
      ZC=TRYZ
      ZEEERR=.FALSE.
      IF(ALENS(1,NEWOBJ+1).NE.0.0D0)
     1CALL GETZEE1
      IF(ZEEERR) THEN
      XC=TRYX
      YC=TRYY
      ZC=TRYZ
          END IF
        TRYX=XC
        TRYY=YC
        TRYZ=ZC
               ICNT=ICNT+1
               GO TO 993
                       END IF
                       END IF
      IF(STOPP.EQ.1.AND.ICNT.GT.ICNTEST) THEN
                        REFEXT=.FALSE.
                        CPFNEXT=.FALSE.
                        SPDEXT=.FALSE.
                        GSPDEXT=.FALSE.
                        IF(.NOT.NULL) FOBYES=.FALSE.
C       NO REF RAY TRACED
                        REFEXT=.FALSE.
        IF(.NOT.NULL.AND.F34.EQ.0.AND.F28.EQ.0.AND.F35.EQ.0.AND.
     1F31.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
                        END IF
                        STOPP=0
                        REFEXT=.TRUE.
                        FOBYES=.TRUE.
                        FOB0=0
C
C       THE RETURN WILL SEND BACK VIA THE COMMON/REFR COMMON
C       THE FULL RAY DATA FOR THE FULLY AIMMED AND TRACED REFERENCE
C       RAY. THEN ANY APPROPRIATE FOB RELATED PRINTOUT WILL BE DONE.
        YK=REFRY(2,NEWIMG)
        XK=REFRY(1,NEWIMG)
        ANGLE1=REFRY(11,NEWOBJ)*180.0D0/PII
        ANGLE2=REFRY(12,NEWOBJ)*180.0D0/PII
C
                        STOPP=0
                        REFEXT=.TRUE.
                        RETURN
                        END
C SUB SLOWFFOB.FOR
        SUBROUTINE SLOWFFOB(WPAS)
        USE GLOBALS
C
        IMPLICIT NONE
C
C       SLOWFFOB IS USED BY IMTRACE3 FOR IMAGE CREATION
C
C     W5 NOT USED WHEN TEL IS ON
C
        INTEGER IISURF,FFT,FFS,I,J,ICNT,ICNTEST
C
      CHARACTER LUNI*3
C
      REAL*8 XPFOB,YPFOB
      COMMON/PFOB/XPFOB,YPFOB,LUNI
C
         REAL*8 FT,FS,X00,Y00,Z0,WPAS
     1  ,LAMBDA,ANGLE1,XK,YK,XLN,XMN,XNN,XFOBB0,YFOBB0,ZFOBB0
     2  ,ANGJK1,ANGJK2,XXANG,YYANG,FACTER,XA,YA,ZA,XJIM,YJIM,ZJIM
     3,ZSAG,ANGLE2,SCLFACY,SCLFACX,AWW1,AWW2
C
      REAL*8 XRAYER,YRAYER,ZRAYER
      COMMON/RAYERPASS/XRAYER,YRAYER,ZRAYER
C
      COMMON/OHFOB/XFOBB0,YFOBB0,ZFOBB0
C
        LOGICAL FANEXT,ZEEERR,SAGERR
        COMMON/ERRZEE/ZEEERR
C
        COMMON/FANEXI/FANEXT
C
C       FOBB0=TRUE FOR ON AXIS AND FALSE FOR OFF AXIS
        LOGICAL FOBB0,FOBB0X,FOBB0Y
C
        COMMON/FFOOBB/FOBB0,FOBB0X,FOBB0Y
C
      COMMON/FBDIFF/FS,FT,FFS,FFT
C
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATSPD.INC'
        FOBRUN=.TRUE.
C
      DLLX=0.0D0
      DLLY=0.0D0
      DLLZ=0.0D0
C     SET UP ANGLE FACTERS
      IF(SYSTEM(21).EQ.0.0D0) THEN
      SCLFACY=1.0D0
                   ELSE
      SCLFACY=DABS(1.0D0/SYSTEM(21))
                   END IF
      IF(SYSTEM(23).EQ.0.0D0) THEN
      SCLFACX=1.0D0
                   ELSE
      SCLFACX=DABS(1.0D0/SYSTEM(23))
                   END IF
      AWW1=W1/SCLFACY
      AWW2=W2/SCLFACX
      IF(SYSTEM(18).EQ.1.AND.NEWOBJ.EQ.0.OR.
     1SYSTEM(19).EQ.1.AND.NEWOBJ.EQ.0) THEN
      IF(DF3.EQ.0.AND.W3.NE.0.0D0) THEN
                   W3=0.0D0
                   DF3=1
                   S3=0
                   END IF
C     OBJECT ANGLES INPUT
      ANGIN=.TRUE.
C     REF OBJ HT INPUT AS ANGLE
C     IN THIS CASE, FOB INDICATES FRACTIONS OF FIELD ANGLES
C     BUT ONLY IF THE NEWOBJ SURFACE IS SURFACE 0
C
C     THIS NEEDS MODIFICATION FOR FRACTIONS OF FIELD ANGLES
C
      ANGJK1=AWW1
      ANGJK2=AWW2
      IF(ANGJK1.GT.89.9999D0.AND.
     1ANGJK1.LT.90.0001D0.OR.
     2ANGJK1.LT.-89.9999D0.AND.
     3ANGJK1.GT.-90.0001D0.OR.
     4ANGJK1.GT.-270.0001D0.AND.
     5ANGJK1.LT.-269.9999D0.OR.
     6ANGJK1.GT.269.9999D0.AND.
     7ANGJK1.LT.270.0001D0) THEN
      IF(DF2.EQ.0) THEN
      OUTLYNE='THE Y-FIELD ANGLE IS 90.0 OR -90.0 DEGREES'
      CALL SHOWIT(1)
      OUTLYNE='NUMERIC WORD #2 OF THE "FOB" COMMAND IS NOT USED HERE'
      CALL SHOWIT(1)
                   W2=0.0D0
                   DF2=1
                   S2=0
                   END IF
                   END IF
      IF(ANGJK1.GT.89.9999D0.AND.
     1ANGJK1.LT.90.0001D0)  ANGJK1=89.9999D0

      IF(ANGJK1.LT.-89.9999D0.AND.
     1ANGJK1.GT.-90.0001D0) ANGJK1=-89.9999D0

      IF(ANGJK1.GT.-270.0001D0.AND.
     1ANGJK1.LT.-269.9999D0) ANGJK1=89.9999D0

      IF(ANGJK1.GT.269.9999D0.AND.
     1ANGJK1.LT.270.0001D0) ANGJK1=-89.9999D0

      IF(ANGJK2.GT.89.9999D0.AND.
     1ANGJK2.LT.90.0001D0)  ANGJK2=89.9999D0

      IF(ANGJK2.LT.-89.9999D0.AND.
     1ANGJK2.GT.-90.0001D0) ANGJK2=-89.9999D0

      IF(ANGJK2.GT.-270.0001D0.AND.
     1ANGJK2.LT.-269.9999D0) ANGJK2=89.9999D0

      IF(ANGJK2.GT.269.9999D0.AND.
     1ANGJK2.LT.270.0001D0) ANGJK2=-89.9999D0

C     ANGLES IN RADIANS ARE:
C
      YYANG=ANGJK1*PII/180.0D0
      XXANG=ANGJK2*PII/180.0D0
C
      IF(DABS(ANGJK1).GT.90.0D0) YSTRT=DTAN(YYANG)*ALENS(3,NEWOBJ)
      IF(DABS(ANGJK1).LE.90.0D0) YSTRT=-DTAN(YYANG)*ALENS(3,NEWOBJ)
C
      IF(DABS(ANGJK2).GT.90.0D0) XSTRT=DTAN(XXANG)*ALENS(3,NEWOBJ)
      IF(DABS(ANGJK2).LE.90.0D0) XSTRT=-DTAN(XXANG)*ALENS(3,NEWOBJ)

      ZSTRT=2.0D0*ALENS(3,NEWOBJ)

      IF(ANGJK1.GT.90.0D0.AND.ANGJK1.LT.270.0D0.OR.
     1ANGJK1.LT.-90.0D0.AND.ANGJK1.GT.-270.0D0) THEN
C
      IF(ANGJK2.GT.90.0D0.AND.ANGJK2.LT.270.0D0.OR.
     1ANGJK2.LT.-90.0D0.AND.ANGJK2.GT.-270.0D0) THEN
                       ZSTRT=0.0D0
                       END IF
                       END IF
      IF(ANGJK2.GT.90.0D0.AND.ANGJK2.LT.270.0D0.OR.
     1ANGJK2.LT.-90.0D0.AND.ANGJK2.GT.-270.0D0) THEN
C
      IF(ANGJK1.GT.90.0D0.AND.ANGJK1.LT.270.0D0.OR.
     1ANGJK1.LT.-90.0D0.AND.ANGJK1.GT.-270.0D0) THEN
                       ZSTRT=0.0D0
                       END IF
                       END IF
      IF(ANGJK1.GE.0.0D0.AND.ANGJK1.LT.90.0D0.OR.
     1ANGJK1.LE.0.0D0.AND.ANGJK1.GT.-90.0D0.OR.
     1ANGJK1.GT.270.0D0.AND.ANGJK1.LE.0.0D0.OR.
     1ANGJK1.LT.-270.0D0.AND.ANGJK1.GE.0.0D0) THEN
C
      IF(ANGJK2.GE.0.0D0.AND.ANGJK2.LT.90.0D0.OR.
     1ANGJK2.LE.0.0D0.AND.ANGJK2.GT.-90.0D0.OR.
     1ANGJK2.GT.270.0D0.AND.ANGJK2.LE.0.0D0.OR.
     1ANGJK2.LT.-270.0D0.AND.ANGJK2.GE.0.0D0) THEN
                       ZSTRT=0.0D0
                       END IF
                       END IF
      IF(ANGJK2.GE.0.0D0.AND.ANGJK2.LT.90.0D0.OR.
     1ANGJK2.LE.0.0D0.AND.ANGJK2.GT.-90.0D0.OR.
     1ANGJK2.GT.270.0D0.AND.ANGJK2.LE.0.0D0.OR.
     1ANGJK2.LT.-270.0D0.AND.ANGJK2.GE.0.0D0) THEN
C
      IF(ANGJK1.GE.0.0D0.AND.ANGJK1.LT.90.0D0.OR.
     1ANGJK1.LE.0.0D0.AND.ANGJK1.GT.-90.0D0.OR.
     1ANGJK1.GT.270.0D0.AND.ANGJK1.LE.0.0D0.OR.
     1ANGJK1.LT.-270.0D0.AND.ANGJK1.GE.0.0D0) THEN
                       ZSTRT=0.0D0
                       END IF
                       END IF
      IF(SYSTEM(18).EQ.1.0D0.AND.SYSTEM(21).EQ.0.0D0) YSTRT=0.0D0
      IF(SYSTEM(19).EQ.1.0D0.AND.SYSTEM(23).EQ.0.0D0) XSTRT=0.0D0
      IF(XSTRT.EQ.0.0D0.AND.YSTRT.EQ.0.0D0) ZSTRT=0.0D0
                       ELSE
C     OBJECT ANGLES NOT INPUT
      ANGIN=.FALSE.
C
      IF(ALENS(9,NEWOBJ).EQ.0.OR.ALENS(127,NEWOBJ).NE.0.0D0) THEN
C     THE STARTING RAY COORDINATES ON THE OBJECT SURAFCE
C     ARE NOT DETERMINED BY THE CLAP ON THE OBJECT SURFACE
C     USING SCY AND SCX, FOB REPRESENTS FRACTIONS OF IMAGE HEIGHT
C     AND DEPTH.
      IF(SYSTEM(16).NE.0.0D0) XSTRT=W2*PXTRAX(5,NEWOBJ)
      IF(SYSTEM(16).EQ.0.0D0) XSTRT=0.0D0
      IF(SYSTEM(14).NE.0.0D0) YSTRT=W1*PXTRAY(5,NEWOBJ)
      IF(SYSTEM(14).EQ.0.0D0) YSTRT=0.0D0
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
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
        ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
                        ELSE
C     USE Y HEIGHT OF CLAP WITH OFFSETS
      IF(ALENS(9,NEWOBJ).EQ.1.0D0.AND.ALENS(127,NEWOBJ).EQ.0.0D0) THEN
      IF(ALENS(10,NEWOBJ).LE.ALENS(11,NEWOBJ)) THEN
      XSTRT=W2*(ALENS(10,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(10,NEWOBJ)+ALENS(12,NEWOBJ))
                           ELSE
      XSTRT=W2*(ALENS(11,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(11,NEWOBJ)+ALENS(12,NEWOBJ))
                           END IF
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
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
      ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
                        END IF
      IF(ALENS(9,NEWOBJ).EQ.5.0D0.AND.ALENS(127,NEWOBJ).EQ.0.0D0) THEN
      XSTRT=W2*(ALENS(10,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(10,NEWOBJ)+ALENS(12,NEWOBJ))
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
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
      ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
                        END IF
      IF(ALENS(9,NEWOBJ).GT.1.0D0.AND.ALENS(9,NEWOBJ).LT.5.0D0.AND.
     1ALENS(127,NEWOBJ).EQ.0.0D0) THEN
      XSTRT=W2*(ALENS(11,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(10,NEWOBJ)+ALENS(12,NEWOBJ))
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
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
      ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
                        END IF
                        END IF
                        END IF
C     NOW THE INITIAL AIMING POINT AT NEWOBJ+1
      IF(ANGIN) THEN
        X1AIM=PXTRAX(5,(NEWOBJ+1))
        X1AIM=(X1AIM*W2)-ALENS(31,NEWOBJ+1)+SYSTEM(81)
        Y1AIM=PXTRAY(5,(NEWOBJ+1))
        Y1AIM=(Y1AIM*W1)-ALENS(30,NEWOBJ+1)+SYSTEM(82)
        Z1AIM=SYSTEM(89)
                       ELSE
C     NOT ANGIN
        X1AIM=PXTRAX(5,(NEWOBJ+1))
        X1AIM=(X1AIM*W2)-ALENS(31,NEWOBJ+1)+SYSTEM(81)
        Y1AIM=PXTRAY(5,(NEWOBJ+1))
        Y1AIM=(Y1AIM*W1)-ALENS(30,NEWOBJ+1)+SYSTEM(82)
      IISURF=NEWOBJ+1
      CALL SAGRET(IISURF,X1AIM,Y1AIM,ZSAG,SAGERR)
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
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
        Z1AIM=SYSTEM(89)+ZSAG
                       END IF
      TRYX=X1AIM
      TRYY=Y1AIM
      TRYZ=Z1AIM
      XC=TRYX
      YC=TRYY
      ZC=TRYZ
      ZEEERR=.FALSE.
      IF(ALENS(1,NEWOBJ+1).NE.0.0D0)
     1CALL GETZEE1
C     STARTING INTERSECTION POINT ON SURF 1 IS
      TRYX=XC
      TRYY=YC
      TRYZ=ZC
C
                        FANEXT=.FALSE.
C
C       INITIALIZE RAYCOD
                RAYCOD(1)=0
                RAYCOD(2)=-1
C
C       SET RAYEXT TO FALSE AND FAIL TO TRUE
                        RAYEXT=.FALSE.
                        POLEXT=.FALSE.
                        FAIL=.TRUE.
C       SET REFEXT AND STOPP
                        STOPP=0
                        REFEXT=.TRUE.
                        SPDEXT=.FALSE.
                        GSPDEXT=.FALSE.
                        CPFNEXT=.FALSE.
                        CALL DELPSF
C
C       SET DEFAULT NUMERICS
        IF(DF1.EQ.1) W1=0.0D0
        IF(DF2.EQ.1) W2=0.0D0
        IF(DF3.EQ.1) W3=0.0D0
C       THE CONTROL WAVELENGTH
        IF(DF4.EQ.1) W4=SYSTEM(11)
        IF(DF4.EQ.1) WVN=SYSTEM(11)
        IF(DF5.EQ.1) THEN
      IF(WC.EQ.'FOB') THEN
                FT=0.0D0
                FS=0.0D0
                   END IF
                        END IF
C
C       THIS MAKES FOB EQUAL FOB 0 0 0 (CW) 0
C
C       CHECK FOR LEGAL WAVELENGTH BOUNDS
C
C       PROCESS W5 IF IT IS NOT DEFAULT
C
C
        IF(WQ.EQ.'NULL') THEN
                        NULL=.TRUE.
                        ELSE
                        NULL=.FALSE.
                        END IF
C
C       NOW THE DEFAULTS HAVE BEEN SET
C       NOW CALL THE SUBROUTINE WHICH STARTS THE REFERENCE
C       RAY RAY TRACE. THIS IS REFRAY.
C
                WWQ=WQ
                WW1=W1
                WW2=W2
                WW3=W3
                WW4=W4
                WVN=WW4
                WW5=W5
      IF(WW4.GE.1.0D0.AND.WW4.LE.5.0D0)
     1LAMBDA=SYSTEM(INT(WW4))
      IF(WW4.GE.6.0D0.AND.WW4.LE.10.0D0)
     1LAMBDA=SYSTEM(65+INT(WW4))
        Y00=WW1
        X00=WW2
        Z0=WW3
C
        IF(WW1.EQ.0.0D0.AND.WW2.EQ.0.0D0.AND.WW3.EQ.0.0D0) THEN
                        FOB0=1
                        FOBB0=.TRUE.
                        ELSE
                        FOB0=0
                        FOBB0=.FALSE.
                        END IF
        IF(WW1.EQ.0.0D0) THEN
                        FOBB0Y=.TRUE.
                        ELSE
                        FOBB0Y=.FALSE.
                        END IF
        IF(WW2.EQ.0.0D0) THEN
                        FOBB0X=.TRUE.
                        ELSE
                        FOBB0X=.FALSE.
                        END IF
C
C       THE CALL TO REFRAY HAS INPUTS:
C               QUALIFIER
C               WW1
C               WW2
C               WW3
C               WW4
C               WW5
C               NEWOBJ
C               NEWREF
C               NEWIMG
C               FOB0 (SET TO 1 IF THE REQUESTED RAY IS THE "GUT" RAY
C                       ELSE SET TO 0 TO TRIGGER A SECOND RAY TO BE
C                       TRACED FIRST.)
C
        IF(FOB0.EQ.0) THEN
C       TRACE THE FOB 0 0 0 RAY 0 0 FIRST THEN TRACE THE REQUESTED
C       RAY
        FOBYES=.TRUE.
        CHLFOB=WWQ
        LFOB(1)=0.0D0
        LFOB(2)=0.0D0
        LFOB(3)=0.0D0
        LFOB(4)=W4
        LFOB(5)=DBLE(NEWOBJ)
        LFOB(6)=DBLE(NEWREF)
        LFOB(7)=DBLE(NEWIMG)
                WW1=0.0D0
                WW2=0.0D0
                WW3=0.0D0
                WW4=LFOB(4)
                WVN=WW4
        TRYX=0.0D0
        TRYY=0.0D0
        TRYZ=0.0D0

        CALL REFRAY(WPAS)
        CALL IPLANE_TILT
        CALL CLPCEN
      IF(STOPP.EQ.1) THEN
                        REFEXT=.FALSE.
                        CPFNEXT=.FALSE.
                        CALL DELPSF
                        SPDEXT=.FALSE.
                        GSPDEXT=.FALSE.
                        IF(.NOT.NULL) FOBYES=.FALSE.
                IF(MSG) THEN
              WRITE(OUTLYNE,*)  "WPAS in REFRAY FAILURE IS ", WPAS
              CALL SHOWIT(19)
      CALL RAYDOC
      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',RAYCOD(2)
      CALL SHOWIT(1)
        OUTLYNE=
     1  'CHIEF RAY FROM CURRENT OBJECT POINT COULD NOT BE TRACED'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*) '5',RAYCOD(1),RAYCOD(2)
        CALL SHOWIT(1)
                        END IF
                STOPP=1
                REFEXT=.FALSE.
        IF(.NOT.NULL) FOBYES=.FALSE.
        IF(.NOT.NULL.AND.F34.EQ.0.AND.F28.EQ.0.AND.F35.EQ.0.AND.
     1F31.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
                        IF(.NOT.NULL) FOBYES=.FALSE.
      IF(SYSTEM(98).NE.0.0D0.OR.SYSTEM(99).NE.0.0D0) THEN
                XRAYER=REFRY(1,NEWOBJ)
                YRAYER=REFRY(2,NEWOBJ)
                ZRAYER=REFRY(3,NEWOBJ)
                        END IF
C       RAY WAS TRACED
        XFOBB0=REFRY(1,NEWOBJ+1)
        YFOBB0=REFRY(2,NEWOBJ+1)
        ZFOBB0=REFRY(3,NEWOBJ+1)
        XLN=REFRY(4,NEWOBJ)
        XMN=REFRY(5,NEWOBJ)
        XNN=REFRY(6,NEWOBJ)
                        END IF
                        ELSE
C       FOB0 WAS 1 SO JUST TRACE THE REQUESTED RAY.
C       THE REQUESTED RAY IS THE GUT RAY, JUST TRACE IT AND RETURN
        FOBYES=.TRUE.
        CHLFOB=WWQ
        LFOB(1)=0.0D0
        LFOB(2)=0.0D0
        LFOB(3)=0.0D0
        LFOB(4)=W4
        LFOB(5)=DBLE(NEWOBJ)
        LFOB(6)=DBLE(NEWREF)
        LFOB(7)=DBLE(NEWIMG)
                WW1=0.0D0
                WW2=0.0D0
                WW3=0.0D0
                WW4=LFOB(4)
                WVN=WW4
        TRYX=0.0D0
        TRYY=0.0D0
        TRYZ=0.0D0
        CALL REFRAY(WPAS)
        CALL IPLANE_TILT
        CALL CLPCEN
      IF(STOPP.EQ.1) THEN
                        REFEXT=.FALSE.
                        CPFNEXT=.FALSE.
                        CALL DELPSF
                        SPDEXT=.FALSE.
                        GSPDEXT=.FALSE.
                        IF(.NOT.NULL) FOBYES=.FALSE.
                IF(MSG) THEN
      CALL RAYDOC
      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',RAYCOD(2)
      CALL SHOWIT(1)
        OUTLYNE=
     1  'CHIEF RAY FROM CURRENT OBJECT POINT COULD NOT BE TRACED'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*) '5',RAYCOD(1),RAYCOD(2)
        CALL SHOWIT(1)
                        END IF
C       NO REF RAY TRACED
                        REFEXT=.FALSE.
        IF(.NOT.NULL.AND.F34.EQ.0.AND.F28.EQ.0.AND.F35.EQ.0.AND.
     1F31.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
      IF(SYSTEM(98).NE.0.0D0.OR.SYSTEM(99).NE.0.0D0) THEN
                XRAYER=REFRY(1,NEWOBJ)
                YRAYER=REFRY(2,NEWOBJ)
                ZRAYER=REFRY(3,NEWOBJ)
                        END IF
C       GUT RAY WAS TRACED.
C       DIRECTION COSINES OF THE Z-AXIS ARE:
        XLN=0.0D0
        XMN=0.0D0
        XNN=1.0D0
                        END IF
                        DO J=0,INT(SYSTEM(20))
      IF(ALENS(34,J).EQ.18.0D0) LDIF2=.FALSE.
      IF(ALENS(34,J).EQ.18.0D0) LDIF=.FALSE.
                        END DO
        IF(LDIF2) THEN
C       TRACE DIFFERENTIAL RAY WITHRESPECT TO AN OBJECT POINT
C       SHIFT
C       SAVE GUT RAY DATA
                        GUTRAY(1:50,0:499)=REFRY(1:50,0:499)
C       SAVE REFRY DATA
                        SAVE=REFRY
        SREFDIFEXT=.FALSE.
        DIFRAYTRACE=.TRUE.
        CALL FRFDIF
        DIFRAYTRACE=.FALSE.
        IF(STOPP.EQ.1) THEN
      STOPP=0
                        REFRY=SAVE
                        IF(WQ.NE.'NULL'.AND.F34.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
                        END IF
C       RESTORE REFRY DATA
                        REFRY=SAVE
C       NO DIFERENTIAL TRACE
                        END IF
C       RESET FOB0 TO 0 AND SET THE REFRAY EXISTENCE FLAG TO TRUE
                        FOB0=0
                        REFEXT=.TRUE.
        ANGLE1=REFRY(11,NEWOBJ)*180.0D0/PII
        ANGLE2=REFRY(12,NEWOBJ)*180.0D0/PII
        YK=REFRY(2,NEWIMG)
        XK=REFRY(1,NEWIMG)
        IF(WQ.EQ.'P') THEN
        WRITE(OUTLYNE,250)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,300) LAMBDA
      CALL SHOWIT(0)
      IF(SYSTEM(18).EQ.0.0D0) THEN
      IF(SYSTEM(6).EQ.1.0D0) LUNI='IN '
      IF(SYSTEM(6).EQ.2.0D0) LUNI='CM '
      IF(SYSTEM(6).EQ.3.0D0) LUNI='MM '
      IF(SYSTEM(6).EQ.4.0D0) LUNI='M  '
        WRITE(OUTLYNE,3302) REFRY(1,NEWOBJ),LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,3303) REFRY(2,NEWOBJ),LUNI
      CALL SHOWIT(0)
      XPFOB=REFRY(1,NEWOBJ)
      YPFOB=REFRY(2,NEWOBJ)
                   ELSE
      LUNI='DEG'
        ANGLE1=REFRY(11,NEWOBJ)*180.0D0/PII
        ANGLE2=REFRY(12,NEWOBJ)*180.0D0/PII
        WRITE(OUTLYNE,302) ANGLE1,LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,303) ANGLE2,LUNI
      CALL SHOWIT(0)
      XPFOB=ANGLE1
      YPFOB=ANGLE2
                   END IF
        WRITE(OUTLYNE,350)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,301) Y00,X00,Z0
      CALL SHOWIT(0)
                        END IF
        IF(WQ.EQ.'PIC') THEN
        WRITE(OUTLYNE,250)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,300) LAMBDA
      CALL SHOWIT(0)
      IF(SYSTEM(18).EQ.0.0D0) THEN
      IF(SYSTEM(6).EQ.1.0D0) LUNI='IN '
      IF(SYSTEM(6).EQ.2.0D0) LUNI='CM '
      IF(SYSTEM(6).EQ.3.0D0) LUNI='MM '
      IF(SYSTEM(6).EQ.4.0D0) LUNI='M  '
        WRITE(OUTLYNE,3302) REFRY(1,NEWOBJ),LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,3303) REFRY(2,NEWOBJ),LUNI
      CALL SHOWIT(0)
      XPFOB=REFRY(1,NEWOBJ)
      YPFOB=REFRY(2,NEWOBJ)
                   ELSE
      LUNI='DEG'
        ANGLE1=REFRY(11,NEWOBJ)*180.0D0/PII
        ANGLE2=REFRY(12,NEWOBJ)*180.0D0/PII
        WRITE(OUTLYNE,302) ANGLE1,LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,303) ANGLE2,LUNI
      CALL SHOWIT(0)
      XPFOB=ANGLE1
      YPFOB=ANGLE2
                   END IF
        WRITE(OUTLYNE,400)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,401) Y00,X00,Z0,YK,XK
      CALL SHOWIT(0)
                        END IF
        IF(WQ.EQ.'PICNH') THEN
        WRITE(OUTLYNE,401) Y00,X00,Z0,YK,XK
      CALL SHOWIT(0)
                        END IF
C       REFERENCE RAY WAS TRACED, NOW TRACE THE DIFFERENTIAL RAY
C       DATA IF FOB HAS QUALIFIERS PFS OR PFSNH.
        IF(WQ.EQ.'PFS'.OR.WQ.EQ.'PFSNH') THEN
C       SAVE REFRY DATA
                        SAVE=REFRY
        STOPP=0
        SREFDIFEXT=.FALSE.
        DIFRAYTRACE=.TRUE.
        CALL FOBDIF
        DIFRAYTRACE=.FALSE.
        IF(REFEXT) STOPP=0
        IF(STOPP.EQ.1) THEN
        STOPP=0
C       RESTORE REFRY DATA
                        REFRY=SAVE
                        IF(WQ.NE.'NULL'.AND.F34.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
                        END IF
C       RESTORE REFRY DATA
                        REFRY=SAVE
        IF(WQ.EQ.'PFS') THEN
C       OUTPUT A PRINTED HEADER WITH APPROPRIATE VALUES
        WRITE(OUTLYNE,250)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,300) LAMBDA
      CALL SHOWIT(0)
      IF(SYSTEM(18).EQ.0.0D0) THEN
      IF(SYSTEM(6).EQ.1.0D0) LUNI='IN '
      IF(SYSTEM(6).EQ.2.0D0) LUNI='CM '
      IF(SYSTEM(6).EQ.3.0D0) LUNI='MM '
      IF(SYSTEM(6).EQ.4.0D0) LUNI='M  '
        WRITE(OUTLYNE,3302) REFRY(1,NEWOBJ),LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,3303) REFRY(2,NEWOBJ),LUNI
      CALL SHOWIT(0)
      XPFOB=REFRY(1,NEWOBJ)
      YPFOB=REFRY(2,NEWOBJ)
                   ELSE
      LUNI='DEG'
        ANGLE1=REFRY(11,NEWOBJ)*180.0D0/PII
        ANGLE2=REFRY(12,NEWOBJ)*180.0D0/PII
        WRITE(OUTLYNE,302) ANGLE1,LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,303) ANGLE2,LUNI
      CALL SHOWIT(0)
      XPFOB=ANGLE1
      YPFOB=ANGLE2
                   END IF
        WRITE(OUTLYNE,101)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,401) Y00,X00,Z0,FT,FS
      CALL SHOWIT(0)
                        END IF
        IF(WQ.EQ.'PFSNH') THEN
C       OUTPUT APPROPRIATE VALUES
        WRITE(OUTLYNE,401) Y00,X00,Z0,FT,FS
      CALL SHOWIT(0)
                        END IF
C       PROCEED
                        END IF
C       THE TRACE FOR FOB0=1 IS COMPLETED
                        RETURN
                        END IF
C       THE FOB0 WAS 0, A 000 GUT RAY WAS TRACED, NOW TRACE THE
C       REQUESTED RAY WHICH WAS NOT 0 0 0.
C       NOW TRACE THE REQUESTED CHIEF RAY IF IT WAS NOT 0 0 0
C
        FOBYES=.TRUE.
        CHLFOB=WWQ
                WW1=W1
                WW2=W2
                WW3=W3
                WW4=W4
        LFOB(1)=WW1
        LFOB(2)=WW2
        LFOB(3)=WW3
        LFOB(4)=WW4
        LFOB(5)=DBLE(NEWOBJ)
        LFOB(6)=DBLE(NEWREF)
        LFOB(7)=DBLE(NEWIMG)
                WWQ=WQ
                WVN=WW4
                WW5=W5
C
      IF(SYSTEM(18).EQ.1.AND.NEWOBJ.EQ.0.OR.
     1SYSTEM(19).EQ.1.AND.NEWOBJ.EQ.0) THEN
C     OBJECT ANGLES INPUT
      ANGIN=.TRUE.
C     REF OBJ HT INPUT AS ANGLE
C     IN THIS CASE, FOB INDICATES FRACTIONS OF FIELD ANGLES
C     BUT ONLY IF THE NEWOBJ SURFACE IS SURFACE 0
C
C     THIS NEEDS MODIFICATION FOR FRACTIONS OF FIELD ANGLES
C
      ANGJK1=AWW1
      ANGJK2=AWW2

      IF(ANGJK1.GT.89.9999D0.AND.
     1ANGJK1.LT.90.0001D0)  ANGJK1=89.9999D0

      IF(ANGJK1.LT.-89.9999D0.AND.
     1ANGJK1.GT.-90.0001D0) ANGJK1=-89.9999D0

      IF(ANGJK1.GT.-270.0001D0.AND.
     1ANGJK1.LT.-269.9999D0) ANGJK1=89.9999D0

      IF(ANGJK1.GT.269.9999D0.AND.
     1ANGJK1.LT.270.0001D0) ANGJK1=-89.9999D0

      IF(ANGJK2.GT.89.9999D0.AND.
     1ANGJK2.LT.90.0001D0)  ANGJK2=89.9999D0

      IF(ANGJK2.LT.-89.9999D0.AND.
     1ANGJK2.GT.-90.0001D0) ANGJK2=-89.9999D0

      IF(ANGJK2.GT.-270.0001D0.AND.
     1ANGJK2.LT.-269.9999D0) ANGJK2=89.9999D0

      IF((W2*SYSTEM(23)).GT.269.9999D0.AND.
     1ANGJK2.LT.270.0001D0) ANGJK2=-89.9999D0

C     ANGLES IN RADIANS ARE:
C
      YYANG=ANGJK1*PII/180.0D0
      XXANG=ANGJK2*PII/180.0D0
C
      IF(DABS(ANGJK1).GT.90.0D0) YSTRT=DTAN(YYANG)*ALENS(3,NEWOBJ)
      IF(DABS(ANGJK1).LE.90.0D0) YSTRT=-DTAN(YYANG)*ALENS(3,NEWOBJ)
C
      IF(DABS(ANGJK2).GT.90.0D0) XSTRT=DTAN(XXANG)*ALENS(3,NEWOBJ)
      IF(DABS(ANGJK2).LE.90.0D0) XSTRT=-DTAN(XXANG)*ALENS(3,NEWOBJ)

      ZSTRT=2.0D0*ALENS(3,NEWOBJ)

      IF(ANGJK1.GT.90.0D0.AND.ANGJK1.LT.270.0D0.OR.
     1ANGJK1.LT.-90.0D0.AND.ANGJK1.GT.-270.0D0) THEN
C
      IF(ANGJK2.GT.90.0D0.AND.ANGJK2.LT.270.0D0.OR.
     1ANGJK2.LT.-90.0D0.AND.ANGJK2.GT.-270.0D0) THEN
                       ZSTRT=0.0D0
                       END IF
                       END IF
      IF(ANGJK2.GT.90.0D0.AND.ANGJK2.LT.270.0D0.OR.
     1ANGJK2.LT.-90.0D0.AND.ANGJK2.GT.-270.0D0) THEN
C
      IF(ANGJK1.GT.90.0D0.AND.ANGJK1.LT.270.0D0.OR.
     1ANGJK1.LT.-90.0D0.AND.ANGJK1.GT.-270.0D0) THEN
                       ZSTRT=0.0D0
                       END IF
                       END IF
      IF(ANGJK1.GE.0.0D0.AND.ANGJK1.LT.90.0D0.OR.
     1ANGJK1.LE.0.0D0.AND.ANGJK1.GT.-90.0D0.OR.
     1ANGJK1.GT.270.0D0.AND.ANGJK1.LE.0.0D0.OR.
     1ANGJK1.LT.-270.0D0.AND.ANGJK1.GE.0.0D0) THEN
C
      IF(ANGJK2.GE.0.0D0.AND.ANGJK2.LT.90.0D0.OR.
     1ANGJK2.LE.0.0D0.AND.ANGJK2.GT.-90.0D0.OR.
     1ANGJK2.GT.270.0D0.AND.ANGJK2.LE.0.0D0.OR.
     1ANGJK2.LT.-270.0D0.AND.ANGJK2.GE.0.0D0) THEN
                       ZSTRT=0.0D0
                       END IF
                       END IF
      IF(ANGJK2.GE.0.0D0.AND.ANGJK2.LT.90.0D0.OR.
     1ANGJK2.LE.0.0D0.AND.ANGJK2.GT.-90.0D0.OR.
     1ANGJK2.GT.270.0D0.AND.ANGJK2.LE.0.0D0.OR.
     1ANGJK2.LT.-270.0D0.AND.ANGJK2.GE.0.0D0) THEN
C
      IF(ANGJK1.GE.0.0D0.AND.ANGJK1.LT.90.0D0.OR.
     1ANGJK1.LE.0.0D0.AND.ANGJK1.GT.-90.0D0.OR.
     1ANGJK1.GT.270.0D0.AND.ANGJK1.LE.0.0D0.OR.
     1ANGJK1.LT.-270.0D0.AND.ANGJK1.GE.0.0D0) THEN
                       ZSTRT=0.0D0
                       END IF
                       END IF
      IF(SYSTEM(18).EQ.1.0D0.AND.SYSTEM(21).EQ.0.0D0) YSTRT=0.0D0
      IF(SYSTEM(19).EQ.1.0D0.AND.SYSTEM(23).EQ.0.0D0) XSTRT=0.0D0
      IF(XSTRT.EQ.0.0D0.AND.YSTRT.EQ.0.0D0) ZSTRT=0.0D0
                       ELSE
C     OBJECT ANGLES NOT INPUT
      ANGIN=.FALSE.
C
      IF(ALENS(9,NEWOBJ).EQ.0.OR.ALENS(127,NEWOBJ).NE.0.0D0) THEN
C     THE STARTING RAY COORDINATES ON THE OBJECT SURAFCE
C     ARE NOT DETERMINED BY THE CLAP ON THE OBJECT SURFACE
C     USING SCY AND SCX, FOB REPRESENTS FRACTIONS OF IMAGE HEIGHT
C     AND DEPTH.
      IF(SYSTEM(16).NE.0.0D0) XSTRT=W2*PXTRAX(5,NEWOBJ)
      IF(SYSTEM(16).EQ.0.0D0) XSTRT=0.0D0
      IF(SYSTEM(14).NE.0.0D0) YSTRT=W1*PXTRAY(5,NEWOBJ)
      IF(SYSTEM(14).EQ.0.0D0) YSTRT=0.0D0
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
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
        ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
                        ELSE
C     USE Y HEIGHT OF CLAP WITH OFFSETS
      IF(ALENS(9,NEWOBJ).EQ.1.0D0.AND.ALENS(127,NEWOBJ).EQ.0.0D0) THEN
      IF(ALENS(10,NEWOBJ).LE.ALENS(11,NEWOBJ)) THEN
      XSTRT=W2*(ALENS(10,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(10,NEWOBJ)+ALENS(12,NEWOBJ))
                           ELSE
      XSTRT=W2*(ALENS(11,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(11,NEWOBJ)+ALENS(12,NEWOBJ))
                           END IF
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
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
      ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
                        END IF
      IF(ALENS(9,NEWOBJ).EQ.5.0D0.AND.ALENS(127,NEWOBJ).EQ.0.0D0) THEN
      XSTRT=W2*(ALENS(10,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(10,NEWOBJ)+ALENS(12,NEWOBJ))
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
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
      ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
                        END IF
      IF(ALENS(9,NEWOBJ).GT.1.0D0.AND.ALENS(9,NEWOBJ).LT.5.0D0.AND.
     1ALENS(127,NEWOBJ).EQ.0.0D0) THEN
      XSTRT=W2*(ALENS(11,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(10,NEWOBJ)+ALENS(12,NEWOBJ))
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
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
      ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
                        END IF
                        END IF
                        END IF
C     NOW THE INITIAL AIMING POINT AT NEWOBJ+1
      IF(ANGIN) THEN
        X1AIM=PXTRAX(5,(NEWOBJ+1))
        X1AIM=(X1AIM*W2)-ALENS(31,NEWOBJ+1)+SYSTEM(81)
        Y1AIM=PXTRAY(5,(NEWOBJ+1))
        Y1AIM=(Y1AIM*W1)-ALENS(30,NEWOBJ+1)+SYSTEM(82)
        Z1AIM=SYSTEM(89)
                       ELSE
C     NOT ANGIN
        X1AIM=PXTRAX(5,(NEWOBJ+1))
        X1AIM=(X1AIM*W2)-ALENS(31,NEWOBJ+1)+SYSTEM(81)
        Y1AIM=PXTRAY(5,(NEWOBJ+1))
        Y1AIM=(Y1AIM*W1)-ALENS(30,NEWOBJ+1)+SYSTEM(82)
      IISURF=NEWOBJ+1
      CALL SAGRET(IISURF,X1AIM,Y1AIM,ZSAG,SAGERR)
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
                REFEXT=.FALSE.
                   CALL MACFAL
                   RETURN
                   END IF
        Z1AIM=SYSTEM(89)+ZSAG
                       END IF
      TRYX=X1AIM
      TRYY=Y1AIM
      TRYZ=Z1AIM
      XJIM=TRYX
      YJIM=TRYY
      ZJIM=TRYZ
      XC=TRYX
      YC=TRYY
      ZC=TRYZ
      ZEEERR=.FALSE.
      IF(ALENS(1,NEWOBJ+1).NE.0.0D0)
     1CALL GETZEE1
C     STARTING INTERSECTION POINT ON SURF 1 IS
      TRYX=XC
      TRYY=YC
      TRYZ=ZC
        FACTER=-(DABS(SERLIM)+DABS(SERINC))
        ICNTEST=INT(DABS(2.0D0*SERLIM)/DABS(SERINC))
        ICNT=0
 992  CONTINUE
      CALL REFRAY(WPAS)
      CALL IPLANE_TILT
        CALL CLPCEN
      IF(STOPP.EQ.1.AND.ICNT.LE.ICNTEST) THEN
               STOPP=0
               REFEXT=.TRUE.
C       INITIALIZE RAYCOD
                RAYCOD(1)=0
                RAYCOD(2)=-1
        FACTER=FACTER+DABS(SERINC)
      XA=XJIM
      YA=YJIM
      ZA=ZJIM
      TRYX=XA
      TRYY=YA
      TRYZ=ZA+FACTER
      XC=TRYX
      YC=TRYY
      ZC=TRYZ
      ZEEERR=.FALSE.
      IF(ALENS(1,NEWOBJ+1).NE.0.0D0)
     1CALL GETZEE1
      IF(ZEEERR) THEN
      XC=TRYX
      YC=TRYY
      ZC=TRYZ
          END IF
        TRYX=XC
        TRYY=YC
        TRYZ=ZC
               ICNT=ICNT+1
               GO TO 992
                       END IF
        FACTER=-(DABS(SERLIM)+DABS(SERINC))
        ICNTEST=INT(DABS(2.0D0*SERLIM)/DABS(SERINC))
        ICNT=0
      IF(STOPP.EQ.1) THEN
 993  CONTINUE
      IF(ICNT.NE.0) THEN
      CALL REFRAY(WPAS)
      CALL IPLANE_TILT
        CALL CLPCEN
      END IF
      IF(STOPP.EQ.1.AND.ICNT.LE.ICNTEST) THEN
               STOPP=0
               REFEXT=.TRUE.
C       INITIALIZE RAYCOD
                RAYCOD(1)=0
                RAYCOD(2)=-1
        FACTER=FACTER+DABS(SERINC)
      XA=0.0D0
      YA=0.0D0
      ZA=FACTER
      TRYX=XA
      TRYY=YA
      TRYZ=ZA
      XC=TRYX
      YC=TRYY
      ZC=TRYZ
      ZEEERR=.FALSE.
      IF(ALENS(1,NEWOBJ+1).NE.0.0D0)
     1CALL GETZEE1
      IF(ZEEERR) THEN
      XC=TRYX
      YC=TRYY
      ZC=TRYZ
          END IF
        TRYX=XC
        TRYY=YC
        TRYZ=ZC
               ICNT=ICNT+1
               GO TO 993
                       END IF
                       END IF
      IF(STOPP.EQ.1.AND.ICNT.GT.ICNTEST) THEN
                        REFEXT=.FALSE.
                        CPFNEXT=.FALSE.
                        CALL DELPSF
                        SPDEXT=.FALSE.
                        GSPDEXT=.FALSE.
                        IF(.NOT.NULL) FOBYES=.FALSE.
                IF(MSG) THEN
      CALL RAYDOC
      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',RAYCOD(2)
      CALL SHOWIT(1)
        OUTLYNE=
     1  'CHIEF RAY FROM CURRENT OBJECT POINT COULD NOT BE TRACED'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*) '6',RAYCOD(1),RAYCOD(2)
        CALL SHOWIT(1)
                        END IF
C       NO REF RAY TRACED
                        REFEXT=.FALSE.
        IF(.NOT.NULL.AND.F34.EQ.0.AND.F28.EQ.0.AND.F35.EQ.0.AND.
     1F31.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
                        END IF
                        STOPP=0
                        REFEXT=.TRUE.
                        FOBYES=.TRUE.
                        DO J=0,INT(SYSTEM(20))
      IF(ALENS(34,J).EQ.18.0D0) LDIF2=.FALSE.
      IF(ALENS(34,J).EQ.18.0D0) LDIF=.FALSE.
                        END DO
                IF(LDIF2) THEN
C       TRACE DIFFERENTIAL RAY WITHRESPECT TO AN OBJECT POINT
C       SHIFT
C       SAVE GUT RAY DATA
                        GUTRAY(1:50,0:499)=REFRY(1:50,0:499)
C       SAVE REFRY DATA
                        SAVE=REFRY
        SREFDIFEXT=.FALSE.
        DIFRAYTRACE=.TRUE.
        CALL FRFDIF
        DIFRAYTRACE=.FALSE.
        IF(STOPP.EQ.1) THEN
      STOPP=0
C       RESTORE REFRY DATA
                        REFRY=SAVE
                        IF(WQ.NE.'NULL'.AND.F34.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
                        END IF
C       RESTORE REFRY DATA
                        REFRY=SAVE
C       NO DIF TRACE
                        END IF
C
                        FOB0=0
C
C       THE RETURN WILL SEND BACK VIA THE COMMON/REFR COMMON
C       THE FULL RAY DATA FOR THE FULLY AIMMED AND TRACED REFERENCE
C       RAY. THEN ANY APPROPRIATE FOB RELATED PRINTOUT WILL BE DONE.
        YK=REFRY(2,NEWIMG)
        XK=REFRY(1,NEWIMG)
        ANGLE1=REFRY(11,NEWOBJ)*180.0D0/PII
        ANGLE2=REFRY(12,NEWOBJ)*180.0D0/PII
        IF(WQ.EQ.'P') THEN
        WRITE(OUTLYNE,250)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,300) LAMBDA
      CALL SHOWIT(0)
      IF(SYSTEM(18).EQ.0.0D0) THEN
      IF(SYSTEM(6).EQ.1.0D0) LUNI='IN '
      IF(SYSTEM(6).EQ.2.0D0) LUNI='CM '
      IF(SYSTEM(6).EQ.3.0D0) LUNI='MM '
      IF(SYSTEM(6).EQ.4.0D0) LUNI='M  '
        WRITE(OUTLYNE,3302) REFRY(1,NEWOBJ),LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,3303) REFRY(2,NEWOBJ),LUNI
      CALL SHOWIT(0)
      XPFOB=REFRY(1,NEWOBJ)
      YPFOB=REFRY(2,NEWOBJ)
                   ELSE
      LUNI='DEG'
        ANGLE1=REFRY(11,NEWOBJ)*180.0D0/PII
        ANGLE2=REFRY(12,NEWOBJ)*180.0D0/PII
        WRITE(OUTLYNE,302) ANGLE1,LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,303) ANGLE2,LUNI
      CALL SHOWIT(0)
      XPFOB=ANGLE1
      YPFOB=ANGLE2
                   END IF
        WRITE(OUTLYNE,350)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,301) Y00,X00,Z0
      CALL SHOWIT(0)
                        END IF
        IF(WQ.EQ.'PIC') THEN
        WRITE(OUTLYNE,250)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,300) LAMBDA
      CALL SHOWIT(0)
      IF(SYSTEM(18).EQ.0.0D0) THEN
      IF(SYSTEM(6).EQ.1.0D0) LUNI='IN '
      IF(SYSTEM(6).EQ.2.0D0) LUNI='CM '
      IF(SYSTEM(6).EQ.3.0D0) LUNI='MM '
      IF(SYSTEM(6).EQ.4.0D0) LUNI='M  '
        WRITE(OUTLYNE,3302) REFRY(1,NEWOBJ),LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,3303) REFRY(2,NEWOBJ),LUNI
      CALL SHOWIT(0)
      XPFOB=REFRY(1,NEWOBJ)
      YPFOB=REFRY(2,NEWOBJ)
                   ELSE
      LUNI='DEG'
        ANGLE1=REFRY(11,NEWOBJ)*180.0D0/PII
        ANGLE2=REFRY(12,NEWOBJ)*180.0D0/PII
        WRITE(OUTLYNE,302) ANGLE1,LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,303) ANGLE2,LUNI
      CALL SHOWIT(0)
      XPFOB=ANGLE1
      YPFOB=ANGLE2
                   END IF
        WRITE(OUTLYNE,400)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,401) Y00,X00,Z0,YK,XK
      CALL SHOWIT(0)
                        END IF
        IF(WQ.EQ.'PICNH') THEN
        WRITE(OUTLYNE,401) Y00,X00,Z0,YK,XK
      CALL SHOWIT(0)
                        END IF
C       REFERENCE RAY WAS TRACED, NOW TRACE THE DIFFERENTIAL RAY
C       DATA IF FOB HAS QUALIFIERS PFS OR PFSNH.
        IF(WQ.EQ.'PFS'.OR.WQ.EQ.'PFSNH') THEN
C       SAVE REFRY DATA
                        SAVE=REFRY
        STOPP=0
        SREFDIFEXT=.FALSE.
        DIFRAYTRACE=.TRUE.
        CALL FOBDIF
        DIFRAYTRACE=.FALSE.
        IF(REFEXT) STOPP=0
        IF(STOPP.EQ.1) THEN
      STOPP=0
C       RESTORE REFRY DATA
                        REFRY=SAVE
                        IF(WQ.NE.'NULL'.AND.F34.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
                        END IF
C       RESTORE REFRY DATA
                        REFRY(I,J)=SAVE(I,J)
        IF(WQ.EQ.'PFS') THEN
C       OUTPUT A PRINTED HEADER WITH APPROPRIATE VALUES
        WRITE(OUTLYNE,250)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,300) LAMBDA
      CALL SHOWIT(0)
      IF(SYSTEM(18).EQ.0.0D0) THEN
      IF(SYSTEM(6).EQ.1.0D0) LUNI='IN '
      IF(SYSTEM(6).EQ.2.0D0) LUNI='CM '
      IF(SYSTEM(6).EQ.3.0D0) LUNI='MM '
      IF(SYSTEM(6).EQ.4.0D0) LUNI='M  '
        WRITE(OUTLYNE,3302) REFRY(1,NEWOBJ),LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,3303) REFRY(2,NEWOBJ),LUNI
      CALL SHOWIT(0)
      XPFOB=REFRY(1,NEWOBJ)
      YPFOB=REFRY(2,NEWOBJ)
                   ELSE
      LUNI='DEG'
        ANGLE1=REFRY(11,NEWOBJ)*180.0D0/PII
        ANGLE2=REFRY(12,NEWOBJ)*180.0D0/PII
        WRITE(OUTLYNE,302) ANGLE1,LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,303) ANGLE2,LUNI
      CALL SHOWIT(0)
      XPFOB=ANGLE1
      YPFOB=ANGLE2
                   END IF
        WRITE(OUTLYNE,101)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,401) Y00,X00,Z0,FT,FS
      CALL SHOWIT(0)
                        END IF
        IF(WQ.EQ.'PFSNH') THEN
C       OUTPUT APPROPRIATE VALUES
        WRITE(OUTLYNE,401) Y00,X00,Z0,FT,FS
      CALL SHOWIT(0)
                        END IF
C       PROCEED
                        END IF
C
                        STOPP=0
                        REFEXT=.TRUE.
                        RETURN
 250    FORMAT('REFERENCE RAY TRACED')
 300    FORMAT('WAVELENGTH = ',G12.4,' MICRONS')
 302    FORMAT('X-FIELD ANGLE = ',G15.7,1X,A3)
 303    FORMAT('Y-FIELD ANGLE = ',G15.7,1X,A3)
 3302   FORMAT('X-OBJECT HEIGHT = ',G15.7,1X,A3)
 3303   FORMAT('Y-OBJECT HEIGHT = ',G15.7,1X,A3)
 401    FORMAT(G10.2,2X,G10.2,2X,G10.2,2X,G14.6,2X,G14.6)
 350    FORMAT(2X,'Y-FOB     ',2X,'X-FOB     ',2X,'DELTA-Z   ')
 400    FORMAT(2X,'Y-FOB     ',2X,'X-FOB     ',2X,'DELTA-Z',
     1  2X,'Y-IMAGE HT. ',4X,'X-IMAGE HT. ')
 101    FORMAT(2X,'Y-FOB     ',2X,'X-FOB     ',2X,'DELTA-Z   ',
     1  2X,'T-FOCUS POS.',4X,'S-FOCUS POS.')
 301    FORMAT(G10.2,2X,G10.2,2X,G10.2)
                        END
C SUB MFFOBS
        SUBROUTINE MFFOBS
        USE GLOBALS
C
        IMPLICIT NONE
C
C       THIS IS SUBROUTINE MFFOBS.FOR. THIS SUBROUTINE IMPLEMENTS
C       THE CMD LEVEL COMMAND "MFOBS".
C
        INTEGER I,J,N
        REAL*8 XSTEP,YSTEP,SW1,SW2
C
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATLEN.INC'
C       SET DEFAULT NUMERICS
        IF(STI.NE.1) THEN
        IF(DF1.EQ.1) W1=0.0D0
        IF(DF2.EQ.1) W2=0.0D0
        IF(DF3.EQ.1) W3=SYSTEM(11)
        IF(DF3.EQ.1) WW3=W3
      IF(DF4.EQ.1) W4=1.0D0
      IF(W4.LE.1.0D0) W4=1.0D0
        IF(DF1.EQ.1) FW1=0.0D0
        IF(DF2.EQ.1) FW2=0.0D0
        IF(DF3.EQ.1) FW3=SYSTEM(11)
        IF(DF3.EQ.1) WW3=W3
      IF(DF4.EQ.1) FW4=1.0D0
      IF(FW4.LE.1.0D0) FW4=1.0D0
        END IF
        IF(STI.EQ.1) THEN
        WRITE(OUTLYNE,*)'MFOBS',FW1,FW2,FW3
        CALL SHOWIT(1)
        WRITE(OUTLYNE,*)FW4
        CALL SHOWIT(1)
                        RETURN
                        END IF

        MFOBSSET=.FALSE.
C
C       CHECK FOR STRING INPUT
        IF(SST.EQ.1.OR.SQ.EQ.1) THEN
        OUTLYNE='"MFOBS" TAKES NO STRING OR QUALIFIER INPUT'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
        IF(DF5.EQ.0) MSG=.TRUE.
                        RETURN
                        END IF
C
C       "MFOBS" COMMAND
C       CHECK FOR CORRECT WAVELENGTH BOUNDS
        IF(INT(W3).NE.1.AND.
     1INT(W3).NE.2.AND.
     1INT(W3).NE.3.AND.
     1INT(W3).NE.4.AND.
     1INT(W3).NE.5.AND.
     1INT(W3).NE.6.AND.
     1INT(W3).NE.7.AND.
     1INT(W3).NE.8.AND.
     1INT(W3).NE.9.AND.
     1INT(W3).NE.10) THEN
        OUTLYNE='WAVELENGTH NUMBER BEYOND LEGAL BOUNDS'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*) W3
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
C
C       CHECK IF REFEXT=.FALSE. IF SO STOP AND PRINT  MESSAGE
        FW1=W1
        FW2=W2
        FW3=W3
        FW4=W4
        FW5=W5
        MFOBSSET=.TRUE.
C
                        RETURN
                        END
C SUB FFOB.FOR
        SUBROUTINE FFOB
          !use ieee_arithmetic, only: ieee_is_nan
        USE GLOBALS

C
        IMPLICIT NONE
C
C       THIS IS SUBROUTINE FFOB.FOR. THIS SUBROUTINE IMPLEMENTS
C       THE CMD LEVEL COMMAND FOB.
C       NOTE: EVERY TIME A REFERENCE RAY IS TRACED, THE
C       FOB,0 0 0 (CW) NW5, RAY 0 0 1 IS TRACED FIRST SO THAT TILT
C       AUTOS ARE SET CORRECTLY
C
C     W5 NOT USED WHEN TEL IS ON
C
        INTEGER IISURF,FFT,FFS,I,J,ICNT,ICNTEST
C
      CHARACTER LUNI*3
C
      REAL*8 XPFOB,YPFOB
      COMMON/PFOB/XPFOB,YPFOB,LUNI
C
         REAL*8 FT,FS,X00,Y00,Z0
     1  ,LAMBDA,ANGLE1,XK,YK,XLN,XMN,XNN,XFOBB0,YFOBB0,ZFOBB0
     2  ,ANGJK1,ANGJK2,XXANG,YYANG,FACTER,XA,YA,ZA,XJIM,YJIM,ZJIM
     3,ZSAG,ANGLE2,SCLFACY,SCLFACX,AWW1,AWW2
C
      REAL*8 XRAYER,YRAYER,ZRAYER
      COMMON/RAYERPASS/XRAYER,YRAYER,ZRAYER
C
      COMMON/OHFOB/XFOBB0,YFOBB0,ZFOBB0
C
        LOGICAL FANEXT,ZEEERR,SAGERR, DEBUGZEE
        COMMON/ERRZEE/ZEEERR
        COMMON DEBUGZEE
C
        COMMON/FANEXI/FANEXT
C
C       FOBB0=TRUE FOR ON AXIS AND FALSE FOR OFF AXIS
        LOGICAL FOBB0,FOBB0X,FOBB0Y, boolResult
C
        COMMON/FFOOBB/FOBB0,FOBB0X,FOBB0Y
C
      COMMON/FBDIFF/FS,FT,FFS,FFT
C
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATSPD.INC'
        FOBRUN=.TRUE.
        DEBUGZEE=.FALSE.
C
      DLLX=0.0D0
      DLLY=0.0D0
      DLLZ=0.0D0
C     SET UP ANGLE FACTERS
      IF(SYSTEM(21).EQ.0.0D0) THEN
      SCLFACY=1.0D0
                   ELSE
      SCLFACY=DABS(1.0D0/SYSTEM(21))
                   END IF
      IF(SYSTEM(23).EQ.0.0D0) THEN
      SCLFACX=1.0D0
                   ELSE
      SCLFACX=DABS(1.0D0/SYSTEM(23))
                   END IF
      AWW1=W1/SCLFACY
      AWW2=W2/SCLFACX

      !call logger%logText("STARTING FFOB ROUTINE")
      CALL SHOWIT(19)
      !call megaDebugLogger('FFOB')
      !PRINT *, "YSTRT FOBBS BEGINNING IS ", YSTRT
C
C       CHECK FOR STRING INPUT
        IF(STI.EQ.1) THEN
      OUTLYNE=
     1'"FOB" TYPE COMMANDS DEFINE AN OBJECT POSITION FOR RAY TRACING'
      CALL SHOWIT(1)
      IF(REFEXT) THEN
        OUTLYNE='THE LAST OBJECT POSITION DATA INPUT WAS:'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,11) LFOB(1)
      CALL SHOWIT(1)
      WRITE(OUTLYNE,12) LFOB(2)
      CALL SHOWIT(1)
      WRITE(OUTLYNE,13) LFOB(3)
      CALL SHOWIT(1)
      WRITE(OUTLYNE,14) INT(LFOB(4))
      CALL SHOWIT(1)
      WRITE(OUTLYNE,15) INT(LFOB(5))
      CALL SHOWIT(1)
      WRITE(OUTLYNE,16) INT(LFOB(6))
      CALL SHOWIT(1)
      WRITE(OUTLYNE,17) INT(LFOB(7))
      CALL SHOWIT(1)
                   END IF
 11   FORMAT('               Y-INPUT VALUE = ',1PD23.15)
 12   FORMAT('               X-INPUT VALUE = ',1PD23.15)
 13   FORMAT('               Z-INPUT VALUE = ',1PD23.15)
 14   FORMAT('REF. WAVELENGTH NUMBER INPUT VALUE = ',I2)
 15   FORMAT('   OBJ. SURFACE NUMBER INPUT VALUE = ',I3)
 16   FORMAT('   REF. SURFACE NUMBER INPUT VALUE = ',I3)
 17   FORMAT('   IMG. SURFACE NUMBER INPUT VALUE = ',I3)
                        RETURN
                        END IF
C       CHECK FOR STRING INPUT
        IF(SST.EQ.1) THEN
        OUTLYNE='"FOB" TAKES NO STRING INPUT'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
      IF(SYSTEM(18).EQ.1.AND.NEWOBJ.EQ.0.OR.
     1SYSTEM(19).EQ.1.AND.NEWOBJ.EQ.0) THEN

      !call logger%logText("FFOB Object Angle Loop ")

      IF(DF3.EQ.0.AND.W3.NE.0.0D0) THEN
                   W3=0.0D0
                   DF3=1
                   S3=0
                   END IF

C     OBJECT ANGLES INPUT
      ANGIN=.TRUE.
C     REF OBJ HT INPUT AS ANGLE
C     IN THIS CASE, FOB INDICATES FRACTIONS OF FIELD ANGLES
C     BUT ONLY IF THE NEWOBJ SURFACE IS SURFACE 0
C
C     THIS NEEDS MODIFICATION FOR FRACTIONS OF FIELD ANGLES
C
      ANGJK1=AWW1
      ANGJK2=AWW2
      call ISYFIELDANGLE90DEGREES(ANGJK1, boolResult)
      IF(boolResult) THEN
      IF(DF2.EQ.0) THEN
      OUTLYNE='THE Y-FIELD ANGLE IS 90.0 OR -90.0 DEGREES'
      CALL SHOWIT(1)
      OUTLYNE='NUMERIC WORD #2 OF THE "FOB" COMMAND IS NOT USED HERE'
      CALL SHOWIT(1)
                   W2=0.0D0
                   DF2=1
                   S2=0
                   END IF
                   END IF
      call ADJUSTANGLEIFCLOSETO90(ANGJK1)
      call ADJUSTANGLEIFCLOSETO90(ANGJK2)

C     ANGLES IN RADIANS ARE:
C
      YYANG=ANGJK1*PII/180.0D0
      XXANG=ANGJK2*PII/180.0D0
C
      IF(DABS(ANGJK1).GT.90.0D0) YSTRT=DTAN(YYANG)*ALENS(3,NEWOBJ)
      IF(DABS(ANGJK1).LE.90.0D0) YSTRT=-DTAN(YYANG)*ALENS(3,NEWOBJ)
      !PRINT *, "YSTRT FOBBS 4516 =", YSTRT
C
      IF(DABS(ANGJK2).GT.90.0D0) XSTRT=DTAN(XXANG)*ALENS(3,NEWOBJ)
      IF(DABS(ANGJK2).LE.90.0D0) XSTRT=-DTAN(XXANG)*ALENS(3,NEWOBJ)

      ZSTRT=2.0D0*ALENS(3,NEWOBJ)

      IF(ANGJK1.GT.90.0D0.AND.ANGJK1.LT.270.0D0.OR.
     1ANGJK1.LT.-90.0D0.AND.ANGJK1.GT.-270.0D0) THEN
C
      IF(ANGJK2.GT.90.0D0.AND.ANGJK2.LT.270.0D0.OR.
     1ANGJK2.LT.-90.0D0.AND.ANGJK2.GT.-270.0D0) THEN
                       ZSTRT=0.0D0
                       END IF
                       END IF
      IF(ANGJK2.GT.90.0D0.AND.ANGJK2.LT.270.0D0.OR.
     1ANGJK2.LT.-90.0D0.AND.ANGJK2.GT.-270.0D0) THEN
C
      IF(ANGJK1.GT.90.0D0.AND.ANGJK1.LT.270.0D0.OR.
     1ANGJK1.LT.-90.0D0.AND.ANGJK1.GT.-270.0D0) THEN
                       ZSTRT=0.0D0
                       END IF
                       END IF
      IF(ANGJK1.GE.0.0D0.AND.ANGJK1.LT.90.0D0.OR.
     1ANGJK1.LE.0.0D0.AND.ANGJK1.GT.-90.0D0.OR.
     1ANGJK1.GT.270.0D0.AND.ANGJK1.LE.0.0D0.OR.
     1ANGJK1.LT.-270.0D0.AND.ANGJK1.GE.0.0D0) THEN
C
      IF(ANGJK2.GE.0.0D0.AND.ANGJK2.LT.90.0D0.OR.
     1ANGJK2.LE.0.0D0.AND.ANGJK2.GT.-90.0D0.OR.
     1ANGJK2.GT.270.0D0.AND.ANGJK2.LE.0.0D0.OR.
     1ANGJK2.LT.-270.0D0.AND.ANGJK2.GE.0.0D0) THEN
                       ZSTRT=0.0D0
                       END IF
                       END IF
      IF(ANGJK2.GE.0.0D0.AND.ANGJK2.LT.90.0D0.OR.
     1ANGJK2.LE.0.0D0.AND.ANGJK2.GT.-90.0D0.OR.
     1ANGJK2.GT.270.0D0.AND.ANGJK2.LE.0.0D0.OR.
     1ANGJK2.LT.-270.0D0.AND.ANGJK2.GE.0.0D0) THEN
C
      IF(ANGJK1.GE.0.0D0.AND.ANGJK1.LT.90.0D0.OR.
     1ANGJK1.LE.0.0D0.AND.ANGJK1.GT.-90.0D0.OR.
     1ANGJK1.GT.270.0D0.AND.ANGJK1.LE.0.0D0.OR.
     1ANGJK1.LT.-270.0D0.AND.ANGJK1.GE.0.0D0) THEN
                       ZSTRT=0.0D0
                       END IF
                       END IF
      IF(SYSTEM(18).EQ.1.0D0.AND.SYSTEM(21).EQ.0.0D0) YSTRT=0.0D0
      IF(SYSTEM(19).EQ.1.0D0.AND.SYSTEM(23).EQ.0.0D0) XSTRT=0.0D0
      IF(XSTRT.EQ.0.0D0.AND.YSTRT.EQ.0.0D0) ZSTRT=0.0D0
                       ELSE

C     OBJECT ANGLES NOT INPUT
      ANGIN=.FALSE.
      !call logger%logText("FFOB Object Height Loop ")
C
      IF(ALENS(9,NEWOBJ).EQ.0.OR.ALENS(127,NEWOBJ).NE.0.0D0) THEN
C     THE STARTING RAY COORDINATES ON THE OBJECT SURAFCE
C     ARE NOT DETERMINED BY THE CLAP ON THE OBJECT SURFACE
C     USING SCY AND SCX, FOB REPRESENTS FRACTIONS OF IMAGE HEIGHT
C     AND DEPTH.
      IF(SYSTEM(16).NE.0.0D0) XSTRT=W2*PXTRAX(5,NEWOBJ)
      IF(SYSTEM(16).EQ.0.0D0) XSTRT=0.0D0
      IF(SYSTEM(14).NE.0.0D0) YSTRT=W1*PXTRAY(5,NEWOBJ)
      IF(SYSTEM(14).EQ.0.0D0) YSTRT=0.0D0
      !PRINT *, "YSTRT =", YSTRT
      !PRINT *, "XSTRT =", YSTRT
      !PRINT *, "SYSTEM(14) ",SYSTEM(14),"SYSTEM(16) ",SYSTEM(16)
      !PRINT *, "PXTRAY(5,NEWOBJ) ", PXTRAY(5,NEWOBJ)
      !PRINT *, "NEWOBJ ", NEWOBJ
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
      IF(SAGERR) CALL HANDLESAGRETFAILURE(MSG, NEWOBJ)

      ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
      !call logger%logTextWithReal("Z start = ",ZSTRT)

      ELSE
      !call logger%logText("USE Y HEIGHT OF CLAP WITH OFFSETS")
C     USE Y HEIGHT OF CLAP WITH OFFSETS
      IF(ALENS(9,NEWOBJ).EQ.1.0D0.AND.ALENS(127,NEWOBJ).EQ.0.0D0) THEN
      IF(ALENS(10,NEWOBJ).LE.ALENS(11,NEWOBJ)) THEN
         XSTRT=W2*(ALENS(10,NEWOBJ)+ALENS(13,NEWOBJ))
         YSTRT=W1*(ALENS(10,NEWOBJ)+ALENS(12,NEWOBJ))
      ELSE
         XSTRT=W2*(ALENS(11,NEWOBJ)+ALENS(13,NEWOBJ))
         YSTRT=W1*(ALENS(11,NEWOBJ)+ALENS(12,NEWOBJ))
      END IF
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
      IF(SAGERR) CALL HANDLESAGRETFAILURE(MSG, NEWOBJ)

                   !if (ieee_is_nan(ZSAG)) ZSAG = 0
      ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
      END IF

      IF(ALENS(9,NEWOBJ).EQ.5.0D0.AND.ALENS(127,NEWOBJ).EQ.0.0D0) THEN
      XSTRT=W2*(ALENS(10,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(10,NEWOBJ)+ALENS(12,NEWOBJ))
      !PRINT *, "YSTRT FOBBS 4632 =", YSTRT
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
      IF(SAGERR) CALL HANDLESAGRETFAILURE(MSG, NEWOBJ)
      !if (ieee_is_nan(ZSAG)) ZSAG = 0
      ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
                        END IF
      IF(ALENS(9,NEWOBJ).GT.1.0D0.AND.ALENS(9,NEWOBJ).LT.5.0D0.AND.
     1ALENS(127,NEWOBJ).EQ.0.0D0) THEN
      XSTRT=W2*(ALENS(11,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(10,NEWOBJ)+ALENS(12,NEWOBJ))
      !PRINT *, "YSTRT FOBBS 4656 =", YSTRT
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
      IF(SAGERR) CALL HANDLESAGRETFAILURE(MSG, NEWOBJ)
      !if (ieee_is_nan(ZSAG)) ZSAG = 0
      ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
                        END IF
                        END IF
                        END IF
C     NOW THE INITIAL AIMING POINT AT NEWOBJ+1
      IF(ANGIN) THEN
        X1AIM=PXTRAX(5,(NEWOBJ+1))
        X1AIM=(X1AIM*W2)-ALENS(31,NEWOBJ+1)+SYSTEM(81)
        Y1AIM=PXTRAY(5,(NEWOBJ+1))
        Y1AIM=(Y1AIM*W1)-ALENS(30,NEWOBJ+1)+SYSTEM(82)
        Z1AIM=SYSTEM(89)
                       ELSE
C     NOT ANGIN
        X1AIM=PXTRAX(5,(NEWOBJ+1))
        X1AIM=(X1AIM*W2)-ALENS(31,NEWOBJ+1)+SYSTEM(81)
        Y1AIM=PXTRAY(5,(NEWOBJ+1))
        Y1AIM=(Y1AIM*W1)-ALENS(30,NEWOBJ+1)+SYSTEM(82)
        !call logger%logTextWithNum("Surf = ", IISURF)
        !call logger%logTextWithReal("X1 Aim = ", X1AIM)
        !call logger%logTextWithReal("Y1 Aim = ", X1AIM)

      IISURF=NEWOBJ+1

      CALL SAGRET(IISURF,X1AIM,Y1AIM,ZSAG,SAGERR)
      IF(SAGERR) CALL HANDLESAGRETFAILURE(MSG, NEWOBJ)

      Z1AIM=SYSTEM(89)+ZSAG
                       END IF
      !PRINT *, "TRYY 4710 IS ", TRYY
      TRYX=X1AIM
      TRYY=Y1AIM
      TRYZ=Z1AIM
      XC=TRYX
      YC=TRYY
      ZC=TRYZ
      ZEEERR=.FALSE.
      !PRINT *, "TRYY 4710 IS ", TRYY
      IF(ALENS(1,NEWOBJ+1).NE.0.0D0)
     1CALL GETZEE1
C     STARTING INTERSECTION POINT ON SURF 1 IS
      TRYX=XC
      TRYY=YC
      TRYZ=ZC
      !PRINT *, "TRYY 4716 IS ", TRYY
C
      FANEXT=.FALSE.
C
C       INITIALIZE RAYCOD
                RAYCOD(1)=0
                RAYCOD(2)=-1
C
C       SET RAYEXT TO FALSE AND FAIL TO TRUE
        RAYEXT=.FALSE.
        POLEXT=.FALSE.
        FAIL=.TRUE.
C       SET REFEXT AND STOPP
                        STOPP=0
                        REFEXT=.TRUE.
                        SPDEXT=.FALSE.
                        GSPDEXT=.FALSE.
                        CPFNEXT=.FALSE.
        CALL DELPSF
        ! JN why is this here?  why can't you check validity earlier?
        IF(SYSTEM(63).EQ.1.AND.S5.EQ.1) THEN
        OUTLYNE='"FOB" TAKES NO NUMERIC WORD #5 INPUT'
      CALL SHOWIT(1)
        OUTLYNE='WHEN TELECENTRIC RAY AIMING IS "ON"'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
C
C       SET DEFAULT NUMERICS
        IF(DF1.EQ.1) W1=0.0D0
        IF(DF2.EQ.1) W2=0.0D0
        IF(DF3.EQ.1) W3=0.0D0
C       THE CONTROL WAVELENGTH
        IF(DF4.EQ.1) W4=SYSTEM(11)
        IF(DF4.EQ.1) WVN=SYSTEM(11)
        IF(DF5.EQ.1) THEN
           IF(WC.EQ.'FOB') THEN
              FT=0.0D0
              FS=0.0D0
            END IF
        END IF
      ! JN:  Same as above.  Can this be checked before now?
      IF(DF5.EQ.0) THEN
      IF(GLANAM(INT(SYSTEM(20))-1,2).EQ.'PERFECT      ') THEN
      OUTLYNE='OBJECT, REFERENCE AND IMAGE SURFACES MAY NOT BE'
      CALL SHOWIT(1)
        OUTLYNE='REASSIGNED WHEN THE "PERFECT" SURFACE IS BEING USED'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
      IF(GLANAM(INT(SYSTEM(20))-1,2).EQ.'IDEAL        ') THEN
      OUTLYNE='OBJECT, REFERENCE AND IMAGE SURFACES MAY NOT BE'
      CALL SHOWIT(1)
        OUTLYNE='REASSIGNED WHEN THE "IDEAL" SURFACE IS BEING USED'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
                        END IF
C
C       THIS MAKES FOB EQUAL FOB 0 0 0 (CW) 0
C
C       CHECK FOR NATURE OF QUALIFIER WORD
        IF(SQ.EQ.1) THEN
        IF(WQ.NE.'NULL'.AND.WQ.NE.'P'.AND.WQ.NE.
     1  'PIC'.AND.WQ.NE.'PICNH'.AND.WQ.NE.'PFS'
     2  .AND.WQ.NE.'PFSNH') THEN
        OUTLYNE='INVALID QUALIFIER USED WITH "FOB"'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
C       NO QUALIFIER,PROCEED
                        END IF
C       CHECK FOR LEGAL WAVELENGTH BOUNDS
      CALL CHECKWAVELENGTHBOUNDS()

      CALL CHECK_OBJ_REF_IMG_SURFACES()


C       NOW THE DEFAULTS HAVE BEEN SET
C       NOW CALL THE SUBROUTINE WHICH STARTS THE REFERENCE
C       RAY RAY TRACE. THIS IS REFRAY.
C
                WWQ=WQ
                WW1=W1
                WW2=W2
                WW3=W3
                WW4=W4
                WVN=WW4
                WW5=W5
      IF(WW4.GE.1.0D0.AND.WW4.LE.5.0D0)
     1LAMBDA=SYSTEM(INT(WW4))
      IF(WW4.GE.6.0D0.AND.WW4.LE.10.0D0)
     1LAMBDA=SYSTEM(65+INT(WW4))
        Y00=WW1
        X00=WW2
        Z0=WW3
C
        IF(WW1.EQ.0.0D0.AND.WW2.EQ.0.0D0.AND.WW3.EQ.0.0D0) THEN
                        FOB0=1
                        FOBB0=.TRUE.
                        ELSE
                        FOB0=0
                        FOBB0=.FALSE.
                        END IF
        IF(WW1.EQ.0.0D0) THEN
                        FOBB0Y=.TRUE.
                        ELSE
                        FOBB0Y=.FALSE.
                        END IF
        IF(WW2.EQ.0.0D0) THEN
                        FOBB0X=.TRUE.
                        ELSE
                        FOBB0X=.FALSE.
                        END IF
C
C       THE CALL TO REFRAY HAS INPUTS:
C               QUALIFIER
C               WW1
C               WW2
C               WW3
C               WW4
C               WW5
C               NEWOBJ
C               NEWREF
C               NEWIMG
C               FOB0 (SET TO 1 IF THE REQUESTED RAY IS THE "GUT" RAY
C                       ELSE SET TO 0 TO TRIGGER A SECOND RAY TO BE
C                       TRACED FIRST.)
C
        IF(FOB0.EQ.0) THEN
C       TRACE THE FOB 0 0 0 RAY 0 0 FIRST THEN TRACE THE REQUESTED
C       RAY
        FOBYES=.TRUE.
        CHLFOB=WWQ
        LFOB(1)=0.0D0
        LFOB(2)=0.0D0
        LFOB(3)=0.0D0
        LFOB(4)=W4
        LFOB(5)=DBLE(NEWOBJ)
        LFOB(6)=DBLE(NEWREF)
        LFOB(7)=DBLE(NEWIMG)
                WW1=0.0D0
                WW2=0.0D0
                WW3=0.0D0
                WW4=LFOB(4)
                WVN=WW4
        TRYX=0.0D0
        TRYY=0.0D0
        TRYZ=0.0D0
      !PRINT *, "STOP IN 5027 is ", STOPP
        CALL REFRAY(OBJLEVEL)
        CALL IPLANE_TILT
        CALL CLPCEN
      !PRINT *, "STOP IN 5030 is ", STOPP
      IF(STOPP.EQ.1) THEN
          REFEXT=.FALSE.
          CPFNEXT=.FALSE.
          CALL DELPSF
          SPDEXT=.FALSE.
          GSPDEXT=.FALSE.
          IF(.NOT.NULL) FOBYES=.FALSE.
          IF(MSG) THEN
              CALL RAYDOC
              WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',
     1        RAYCOD(2)
              CALL SHOWIT(1)
              PRINT *, OUTLYNE
              OUTLYNE=
     1        'CHIEF RAY FROM CURRENT OBJECT POINT COULD NOT BE TRACED'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) '7',RAYCOD(1),RAYCOD(2)
              CALL SHOWIT(1)
              PRINT *, OUTLYNE
          END IF
                STOPP=1
                REFEXT=.FALSE.
        IF(.NOT.NULL) FOBYES=.FALSE.
        IF(.NOT.NULL.AND.F34.EQ.0.AND.F28.EQ.0.AND.F35.EQ.0.AND.
     1F31.EQ.0) CALL MACFAL
        RETURN
      ELSE
                        !PRINT *, "STOPP IN 5059 is ", STOPP
                        STOPP=0
                        REFEXT=.TRUE.
                        IF(.NOT.NULL) FOBYES=.FALSE.
      IF(SYSTEM(98).NE.0.0D0.OR.SYSTEM(99).NE.0.0D0) THEN
                XRAYER=REFRY(1,NEWOBJ)
                YRAYER=REFRY(2,NEWOBJ)
                ZRAYER=REFRY(3,NEWOBJ)
                        END IF
C       RAY WAS TRACED
        XFOBB0=REFRY(1,NEWOBJ+1)
        YFOBB0=REFRY(2,NEWOBJ+1)
        ZFOBB0=REFRY(3,NEWOBJ+1)
        XLN=REFRY(4,NEWOBJ)
        XMN=REFRY(5,NEWOBJ)
        XNN=REFRY(6,NEWOBJ)
                        END IF
                        ELSE
C       FOB0 WAS 1 SO JUST TRACE THE REQUESTED RAY.
C       THE REQUESTED RAY IS THE GUT RAY, JUST TRACE IT AND RETURN
        FOBYES=.TRUE.
        CHLFOB=WWQ
        LFOB(1)=0.0D0
        LFOB(2)=0.0D0
        LFOB(3)=0.0D0
        LFOB(4)=W4
        LFOB(5)=DBLE(NEWOBJ)
        LFOB(6)=DBLE(NEWREF)
        LFOB(7)=DBLE(NEWIMG)
                WW1=0.0D0
                WW2=0.0D0
                WW3=0.0D0
                WW4=LFOB(4)
                WVN=WW4
        TRYX=0.0D0
        TRYY=0.0D0
        TRYZ=0.0D0
        CALL REFRAY(OBJLEVEL)
        CALL IPLANE_TILT
        CALL CLPCEN
      IF(STOPP.EQ.1) THEN
                        REFEXT=.FALSE.
                        CPFNEXT=.FALSE.
                        CALL DELPSF
                        SPDEXT=.FALSE.
                        GSPDEXT=.FALSE.
                        IF(.NOT.NULL) FOBYES=.FALSE.
                IF(MSG) THEN
      CALL RAYDOC
      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',RAYCOD(2)
      CALL SHOWIT(1)
        OUTLYNE=
     1  'CHIEF RAY FROM CURRENT OBJECT POINT COULD NOT BE TRACED'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*) '8',RAYCOD(1),RAYCOD(2)
        CALL SHOWIT(1)
                        END IF
C       NO REF RAY TRACED
                        REFEXT=.FALSE.
        IF(.NOT.NULL.AND.F34.EQ.0.AND.F28.EQ.0.AND.F35.EQ.0.AND.
     1F31.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
      IF(SYSTEM(98).NE.0.0D0.OR.SYSTEM(99).NE.0.0D0) THEN
                XRAYER=REFRY(1,NEWOBJ)
                YRAYER=REFRY(2,NEWOBJ)
                ZRAYER=REFRY(3,NEWOBJ)
                        END IF
C       GUT RAY WAS TRACED.
C       DIRECTION COSINES OF THE Z-AXIS ARE:
        XLN=0.0D0
        XMN=0.0D0
        XNN=1.0D0
                        END IF
                        DO J=0,INT(SYSTEM(20))
      IF(ALENS(34,J).EQ.18.0D0) LDIF2=.FALSE.
      IF(ALENS(34,J).EQ.18.0D0) LDIF=.FALSE.
                        END DO
        IF(LDIF2) THEN
C       TRACE DIFFERENTIAL RAY WITHRESPECT TO AN OBJECT POINT
C       SHIFT
C       SAVE GUT RAY DATA
                        GUTRAY(1:50,0:499)=REFRY(1:50,0:499)
C       SAVE REFRY DATA
                        SAVE=REFRY
        SREFDIFEXT=.FALSE.
        DIFRAYTRACE=.TRUE.
        CALL FRFDIF
        DIFRAYTRACE=.FALSE.
        IF(STOPP.EQ.1) THEN
      STOPP=0
                        REFRY=SAVE
                        IF(WQ.NE.'NULL'.AND.F34.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
                        END IF
C       RESTORE REFRY DATA
                        REFRY=SAVE
C       NO DIFERENTIAL TRACE
                        END IF
C       RESET FOB0 TO 0 AND SET THE REFRAY EXISTENCE FLAG TO TRUE
                        FOB0=0
                        REFEXT=.TRUE.
        ANGLE1=REFRY(11,NEWOBJ)*180.0D0/PII
        ANGLE2=REFRY(12,NEWOBJ)*180.0D0/PII
        YK=REFRY(2,NEWIMG)
        XK=REFRY(1,NEWIMG)
        IF(WQ.EQ.'P') THEN
        WRITE(OUTLYNE,250)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,300) LAMBDA
      CALL SHOWIT(0)
      IF(SYSTEM(18).EQ.0.0D0) THEN
      IF(SYSTEM(6).EQ.1.0D0) LUNI='IN '
      IF(SYSTEM(6).EQ.2.0D0) LUNI='CM '
      IF(SYSTEM(6).EQ.3.0D0) LUNI='MM '
      IF(SYSTEM(6).EQ.4.0D0) LUNI='M  '
        WRITE(OUTLYNE,3302) REFRY(1,NEWOBJ),LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,3303) REFRY(2,NEWOBJ),LUNI
      CALL SHOWIT(0)
      XPFOB=REFRY(1,NEWOBJ)
      YPFOB=REFRY(2,NEWOBJ)
                   ELSE
      LUNI='DEG'
        ANGLE1=REFRY(11,NEWOBJ)*180.0D0/PII
        ANGLE2=REFRY(12,NEWOBJ)*180.0D0/PII
        WRITE(OUTLYNE,302) ANGLE1,LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,303) ANGLE2,LUNI
      CALL SHOWIT(0)
      XPFOB=ANGLE1
      YPFOB=ANGLE2
                   END IF
        WRITE(OUTLYNE,350)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,301) Y00,X00,Z0
      CALL SHOWIT(0)
                        END IF
        IF(WQ.EQ.'PIC') THEN
        WRITE(OUTLYNE,250)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,300) LAMBDA
      CALL SHOWIT(0)
      IF(SYSTEM(18).EQ.0.0D0) THEN
      IF(SYSTEM(6).EQ.1.0D0) LUNI='IN '
      IF(SYSTEM(6).EQ.2.0D0) LUNI='CM '
      IF(SYSTEM(6).EQ.3.0D0) LUNI='MM '
      IF(SYSTEM(6).EQ.4.0D0) LUNI='M  '
        WRITE(OUTLYNE,3302) REFRY(1,NEWOBJ),LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,3303) REFRY(2,NEWOBJ),LUNI
      CALL SHOWIT(0)
      XPFOB=REFRY(1,NEWOBJ)
      YPFOB=REFRY(2,NEWOBJ)
                   ELSE
      LUNI='DEG'
        ANGLE1=REFRY(11,NEWOBJ)*180.0D0/PII
        ANGLE2=REFRY(12,NEWOBJ)*180.0D0/PII
        WRITE(OUTLYNE,302) ANGLE1,LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,303) ANGLE2,LUNI
      CALL SHOWIT(0)
      XPFOB=ANGLE1
      YPFOB=ANGLE2
                   END IF
        WRITE(OUTLYNE,400)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,401) Y00,X00,Z0,YK,XK
      CALL SHOWIT(0)
                        END IF
        IF(WQ.EQ.'PICNH') THEN
        WRITE(OUTLYNE,401) Y00,X00,Z0,YK,XK
      CALL SHOWIT(0)
                        END IF
C       REFERENCE RAY WAS TRACED, NOW TRACE THE DIFFERENTIAL RAY
C       DATA IF FOB HAS QUALIFIERS PFS OR PFSNH.
        IF(WQ.EQ.'PFS'.OR.WQ.EQ.'PFSNH') THEN
C       SAVE REFRY DATA
                        SAVE=REFRY
        STOPP=0
        SREFDIFEXT=.FALSE.
        DIFRAYTRACE=.TRUE.
        CALL FOBDIF
        DIFRAYTRACE=.FALSE.
        IF(REFEXT) STOPP=0
        IF(STOPP.EQ.1) THEN
        STOPP=0
C       RESTORE REFRY DATA
                        REFRY=SAVE
                        IF(WQ.NE.'NULL'.AND.F34.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
                        END IF
C       RESTORE REFRY DATA
                        REFRY=SAVE
        IF(WQ.EQ.'PFS') THEN
C       OUTPUT A PRINTED HEADER WITH APPROPRIATE VALUES
        WRITE(OUTLYNE,250)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,300) LAMBDA
      CALL SHOWIT(0)
      IF(SYSTEM(18).EQ.0.0D0) THEN
      IF(SYSTEM(6).EQ.1.0D0) LUNI='IN '
      IF(SYSTEM(6).EQ.2.0D0) LUNI='CM '
      IF(SYSTEM(6).EQ.3.0D0) LUNI='MM '
      IF(SYSTEM(6).EQ.4.0D0) LUNI='M  '
        WRITE(OUTLYNE,3302) REFRY(1,NEWOBJ),LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,3303) REFRY(2,NEWOBJ),LUNI
      CALL SHOWIT(0)
      XPFOB=REFRY(1,NEWOBJ)
      YPFOB=REFRY(2,NEWOBJ)
                   ELSE
      LUNI='DEG'
        ANGLE1=REFRY(11,NEWOBJ)*180.0D0/PII
        ANGLE2=REFRY(12,NEWOBJ)*180.0D0/PII
        WRITE(OUTLYNE,302) ANGLE1,LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,303) ANGLE2,LUNI
      CALL SHOWIT(0)
      XPFOB=ANGLE1
      YPFOB=ANGLE2
                   END IF
        WRITE(OUTLYNE,101)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,401) Y00,X00,Z0,FT,FS
      CALL SHOWIT(0)
                        END IF
        IF(WQ.EQ.'PFSNH') THEN
C       OUTPUT APPROPRIATE VALUES
        WRITE(OUTLYNE,401) Y00,X00,Z0,FT,FS
      CALL SHOWIT(0)
                        END IF
C       PROCEED
                        END IF
C       THE TRACE FOR FOB0=1 IS COMPLETED
                        RETURN
                        END IF
C       THE FOB0 WAS 0, A 000 GUT RAY WAS TRACED, NOW TRACE THE
C       REQUESTED RAY WHICH WAS NOT 0 0 0.
C       NOW TRACE THE REQUESTED CHIEF RAY IF IT WAS NOT 0 0 0
C
        FOBYES=.TRUE.
        CHLFOB=WWQ
                WW1=W1
                WW2=W2
                WW3=W3
                WW4=W4
        LFOB(1)=WW1
        LFOB(2)=WW2
        LFOB(3)=WW3
        LFOB(4)=WW4
        LFOB(5)=DBLE(NEWOBJ)
        LFOB(6)=DBLE(NEWREF)
        LFOB(7)=DBLE(NEWIMG)
                WWQ=WQ
                WVN=WW4
                WW5=W5
C
      IF(SYSTEM(18).EQ.1.AND.NEWOBJ.EQ.0.OR.
     1SYSTEM(19).EQ.1.AND.NEWOBJ.EQ.0) THEN
C     OBJECT ANGLES INPUT
      ANGIN=.TRUE.
C     REF OBJ HT INPUT AS ANGLE
C     IN THIS CASE, FOB INDICATES FRACTIONS OF FIELD ANGLES
C     BUT ONLY IF THE NEWOBJ SURFACE IS SURFACE 0
C
C     THIS NEEDS MODIFICATION FOR FRACTIONS OF FIELD ANGLES
C
      ANGJK1=AWW1
      ANGJK2=AWW2

      IF(ANGJK1.GT.89.9999D0.AND.
     1ANGJK1.LT.90.0001D0)  ANGJK1=89.9999D0

      IF(ANGJK1.LT.-89.9999D0.AND.
     1ANGJK1.GT.-90.0001D0) ANGJK1=-89.9999D0

      IF(ANGJK1.GT.-270.0001D0.AND.
     1ANGJK1.LT.-269.9999D0) ANGJK1=89.9999D0

      IF(ANGJK1.GT.269.9999D0.AND.
     1ANGJK1.LT.270.0001D0) ANGJK1=-89.9999D0

      IF(ANGJK2.GT.89.9999D0.AND.
     1ANGJK2.LT.90.0001D0)  ANGJK2=89.9999D0

      IF(ANGJK2.LT.-89.9999D0.AND.
     1ANGJK2.GT.-90.0001D0) ANGJK2=-89.9999D0

      IF(ANGJK2.GT.-270.0001D0.AND.
     1ANGJK2.LT.-269.9999D0) ANGJK2=89.9999D0

      IF((W2*SYSTEM(23)).GT.269.9999D0.AND.
     1ANGJK2.LT.270.0001D0) ANGJK2=-89.9999D0

C     ANGLES IN RADIANS ARE:
C
      YYANG=ANGJK1*PII/180.0D0
      XXANG=ANGJK2*PII/180.0D0
C
      IF(DABS(ANGJK1).GT.90.0D0) YSTRT=DTAN(YYANG)*ALENS(3,NEWOBJ)
      IF(DABS(ANGJK1).LE.90.0D0) YSTRT=-DTAN(YYANG)*ALENS(3,NEWOBJ)
      !PRINT *, "YSTRT FOBBS 5381 =", YSTRT
C
      IF(DABS(ANGJK2).GT.90.0D0) XSTRT=DTAN(XXANG)*ALENS(3,NEWOBJ)
      IF(DABS(ANGJK2).LE.90.0D0) XSTRT=-DTAN(XXANG)*ALENS(3,NEWOBJ)

      ZSTRT=2.0D0*ALENS(3,NEWOBJ)

      IF(ANGJK1.GT.90.0D0.AND.ANGJK1.LT.270.0D0.OR.
     1ANGJK1.LT.-90.0D0.AND.ANGJK1.GT.-270.0D0) THEN
C
      IF(ANGJK2.GT.90.0D0.AND.ANGJK2.LT.270.0D0.OR.
     1ANGJK2.LT.-90.0D0.AND.ANGJK2.GT.-270.0D0) THEN
                       ZSTRT=0.0D0
                       END IF
                       END IF
      IF(ANGJK2.GT.90.0D0.AND.ANGJK2.LT.270.0D0.OR.
     1ANGJK2.LT.-90.0D0.AND.ANGJK2.GT.-270.0D0) THEN
C
      IF(ANGJK1.GT.90.0D0.AND.ANGJK1.LT.270.0D0.OR.
     1ANGJK1.LT.-90.0D0.AND.ANGJK1.GT.-270.0D0) THEN
                       ZSTRT=0.0D0
                       END IF
                       END IF
      IF(ANGJK1.GE.0.0D0.AND.ANGJK1.LT.90.0D0.OR.
     1ANGJK1.LE.0.0D0.AND.ANGJK1.GT.-90.0D0.OR.
     1ANGJK1.GT.270.0D0.AND.ANGJK1.LE.0.0D0.OR.
     1ANGJK1.LT.-270.0D0.AND.ANGJK1.GE.0.0D0) THEN
C
      IF(ANGJK2.GE.0.0D0.AND.ANGJK2.LT.90.0D0.OR.
     1ANGJK2.LE.0.0D0.AND.ANGJK2.GT.-90.0D0.OR.
     1ANGJK2.GT.270.0D0.AND.ANGJK2.LE.0.0D0.OR.
     1ANGJK2.LT.-270.0D0.AND.ANGJK2.GE.0.0D0) THEN
                       ZSTRT=0.0D0
                       END IF
                       END IF
      IF(ANGJK2.GE.0.0D0.AND.ANGJK2.LT.90.0D0.OR.
     1ANGJK2.LE.0.0D0.AND.ANGJK2.GT.-90.0D0.OR.
     1ANGJK2.GT.270.0D0.AND.ANGJK2.LE.0.0D0.OR.
     1ANGJK2.LT.-270.0D0.AND.ANGJK2.GE.0.0D0) THEN
C
      IF(ANGJK1.GE.0.0D0.AND.ANGJK1.LT.90.0D0.OR.
     1ANGJK1.LE.0.0D0.AND.ANGJK1.GT.-90.0D0.OR.
     1ANGJK1.GT.270.0D0.AND.ANGJK1.LE.0.0D0.OR.
     1ANGJK1.LT.-270.0D0.AND.ANGJK1.GE.0.0D0) THEN
                       ZSTRT=0.0D0
                       END IF
                       END IF
      IF(SYSTEM(18).EQ.1.0D0.AND.SYSTEM(21).EQ.0.0D0) YSTRT=0.0D0
      IF(SYSTEM(19).EQ.1.0D0.AND.SYSTEM(23).EQ.0.0D0) XSTRT=0.0D0
      IF(XSTRT.EQ.0.0D0.AND.YSTRT.EQ.0.0D0) ZSTRT=0.0D0
                       ELSE
C     OBJECT ANGLES NOT INPUT
      ANGIN=.FALSE.
C
      IF(ALENS(9,NEWOBJ).EQ.0.OR.ALENS(127,NEWOBJ).NE.0.0D0) THEN
C     THE STARTING RAY COORDINATES ON THE OBJECT SURAFCE
C     ARE NOT DETERMINED BY THE CLAP ON THE OBJECT SURFACE
C     USING SCY AND SCX, FOB REPRESENTS FRACTIONS OF IMAGE HEIGHT
C     AND DEPTH.
      IF(SYSTEM(16).NE.0.0D0) XSTRT=W2*PXTRAX(5,NEWOBJ)
      IF(SYSTEM(16).EQ.0.0D0) XSTRT=0.0D0
      IF(SYSTEM(14).NE.0.0D0) YSTRT=W1*PXTRAY(5,NEWOBJ)
      IF(SYSTEM(14).EQ.0.0D0) YSTRT=0.0D0
      !PRINT *, "YSTRT FOBBS 5444 =", YSTRT
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
      IF(SAGERR) CALL HANDLESAGRETFAILURE(MSG, NEWOBJ)

           ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG

        !PRINT *, "ALENS(3,NEWOBJ) ",ALENS(3,NEWOBJ),"SAG ",ZSAG
        !PRINT *, "ZSTRT FOBBS 5465 ", ZSTRT
                        ELSE
C     USE Y HEIGHT OF CLAP WITH OFFSETS
      IF(ALENS(9,NEWOBJ).EQ.1.0D0.AND.ALENS(127,NEWOBJ).EQ.0.0D0) THEN
      IF(ALENS(10,NEWOBJ).LE.ALENS(11,NEWOBJ)) THEN
      XSTRT=W2*(ALENS(10,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(10,NEWOBJ)+ALENS(12,NEWOBJ))
      !PRINT *, "YSTRT FOBBS 5468 =", YSTRT
                           ELSE
      XSTRT=W2*(ALENS(11,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(11,NEWOBJ)+ALENS(12,NEWOBJ))
      !PRINT *, "YSTRT FOBBS 5472 =", YSTRT
                           END IF
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
      IF(SAGERR) CALL HANDLESAGRETFAILURE(MSG, NEWOBJ)
      !if (ieee_is_nan(ZSAG)) ZSAG = 0
      ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
      !PRINT *, "ZSTRT FOBBS 5496 ", ZSTRT
                        END IF
      IF(ALENS(9,NEWOBJ).EQ.5.0D0.AND.ALENS(127,NEWOBJ).EQ.0.0D0) THEN
      XSTRT=W2*(ALENS(10,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(10,NEWOBJ)+ALENS(12,NEWOBJ))
      !PRINT *, "YSTRT FOBBS 5495 =", YSTRT
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
      IF(SAGERR) CALL HANDLESAGRETFAILURE(MSG, NEWOBJ)
      !if (ieee_is_nan(ZSAG)) ZSAG = 0
      ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
      !PRINT *, "ZSTRT FOBBS 5519 ", ZSTRT
                        END IF
      IF(ALENS(9,NEWOBJ).GT.1.0D0.AND.ALENS(9,NEWOBJ).LT.5.0D0.AND.
     1ALENS(127,NEWOBJ).EQ.0.0D0) THEN
      XSTRT=W2*(ALENS(11,NEWOBJ)+ALENS(13,NEWOBJ))
      YSTRT=W1*(ALENS(10,NEWOBJ)+ALENS(12,NEWOBJ))
      !PRINT *, "YSTRT FOBBS 5518 =", YSTRT
      IISURF=NEWOBJ
      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
      IF(SAGERR) CALL HANDLESAGRETFAILURE(MSG, NEWOBJ)
      !if (ieee_is_nan(ZSAG)) ZSAG = 0
      ZSTRT=(W3*ALENS(3,NEWOBJ))+ZSAG
      !PRINT *, "ZSTRT FOBBS 5543 ", ZSTRT
                        END IF
                        END IF
                        END IF
C     NOW THE INITIAL AIMING POINT AT NEWOBJ+1
      IF(ANGIN) THEN
        X1AIM=PXTRAX(5,(NEWOBJ+1))
        X1AIM=(X1AIM*W2)-ALENS(31,NEWOBJ+1)+SYSTEM(81)
        Y1AIM=PXTRAY(5,(NEWOBJ+1))
        Y1AIM=(Y1AIM*W1)-ALENS(30,NEWOBJ+1)+SYSTEM(82)
        Z1AIM=SYSTEM(89)
                       ELSE
C     NOT ANGIN
        X1AIM=PXTRAX(5,(NEWOBJ+1))
        X1AIM=(X1AIM*W2)-ALENS(31,NEWOBJ+1)+SYSTEM(81)
        Y1AIM=PXTRAY(5,(NEWOBJ+1))
        Y1AIM=(Y1AIM*W1)-ALENS(30,NEWOBJ+1)+SYSTEM(82)
      IISURF=NEWOBJ+1
      CALL SAGRET(IISURF,X1AIM,Y1AIM,ZSAG,SAGERR)
      IF(SAGERR) CALL HANDLESAGRETFAILURE(MSG, NEWOBJ)
        Z1AIM=SYSTEM(89)+ZSAG
                       END IF
      TRYX=X1AIM
      TRYY=Y1AIM
      TRYZ=Z1AIM
      !PRINT *, "TRYY 5558 IS ", TRYY
      XJIM=TRYX
      YJIM=TRYY
      ZJIM=TRYZ
      XC=TRYX
      YC=TRYY
      ZC=TRYZ
      ZEEERR=.FALSE.
      IF(ALENS(1,NEWOBJ+1).NE.0.0D0)
     1CALL GETZEE1
C     STARTING INTERSECTION POINT ON SURF 1 IS
      TRYX=XC
      TRYY=YC
      TRYZ=ZC
      !PRINT *, "TRYY 5572 IS ", TRYY
        FACTER=-(DABS(SERLIM)+DABS(SERINC))
        ICNTEST=INT(DABS(2.0D0*SERLIM)/DABS(SERINC))
        ICNT=0
 992  CONTINUE
      CALL REFRAY(OBJLEVEL)
      CALL IPLANE_TILT
        CALL CLPCEN
      IF(STOPP.EQ.1.AND.ICNT.LE.ICNTEST) THEN
        IF(ICNT.EQ.0) THEN
          !call logger%logText("REF RAY Failed!")
          !call logger%logTextWithNum("ICNTEST is ",ICNTEST)
        END IF

               STOPP=0
               REFEXT=.TRUE.
C       INITIALIZE RAYCOD
                RAYCOD(1)=0
                RAYCOD(2)=-1
        FACTER=FACTER+DABS(SERINC)
      XA=XJIM
      YA=YJIM
      ZA=ZJIM
      TRYX=XA
      TRYY=YA
      !PRINT *, "TRYY 5593 IS ", TRYY
      TRYZ=ZA+FACTER
      XC=TRYX
      YC=TRYY
      ZC=TRYZ
      ZEEERR=.FALSE.
      !PRINT *, "FOBBS 5600 YC IS ", YC
      IF(ALENS(1,NEWOBJ+1).NE.0.0D0) CALL GETZEE1
      !PRINT *, "FOBBS 5602 POST GETZEE1 YC IS ", YC
      IF(ZEEERR) THEN
        !call logger%logText("GetZee1 Failed after RefRay failed!")
      XC=TRYX
      YC=TRYY
      ZC=TRYZ
          END IF
        TRYX=XC
        TRYY=YC
        TRYZ=ZC
        !PRINT *, "TRYY 5609 IS ", TRYY
               ICNT=ICNT+1
               GO TO 992
                       END IF
        FACTER=-(DABS(SERLIM)+DABS(SERINC))
        ICNTEST=INT(DABS(2.0D0*SERLIM)/DABS(SERINC))
        !call logger%logTextWithNum("ICNTEST 5322 = ", ICNTEST)
        !PRINT *, "ICNTEST IS ", ICNTEST
        !PRINT *, "STOPP IS ", STOPP
        ICNT=0
      IF(STOPP.EQ.1) THEN

 993  CONTINUE

C
C       THE CALL TO REFRAY HAS INPUTS:
C               QUALIFIER
C               WW1
C               WW2
C               WW3
C               WW4
C               WW5
C               NEWOBJ
C               NEWREF
C               NEWIMG
C               FOB0 (SET TO 1 IF THE REQUESTED RAY IS THE "GUT" RAY
C                       ELSE SET TO 0 TO TRIGGER A SECOND RAY TO BE
C                       TRACED FIRST.)
C


      IF(ICNT.NE.0) THEN
        IF (ICNT.EQ.1) THEN
        PRINT *, "STOPP in 5616 is ", STOPP
1235    FORMAT(A12, G15.7, G15.7, G15.7, G15.7, G15.7, I3, I3, I3)
        PRINT 1235, "B4 REFRAY",WW1,WW2,WW3,WW4,WW5,NEWOBJ,NEWREF,
     1  NEWIMG
        PRINT *, "B4 REFRAY TRYY is ", TRYY
      END IF
        CALL REFRAY(OBJLEVEL)
        !PRINT *, "STOPP in 5618 is ", STOPP
        CALL IPLANE_TILT
        CALL CLPCEN

      END IF
      IF(STOPP.EQ.1.AND.ICNT.LE.ICNTEST) THEN
         STOPP=0
         REFEXT=.TRUE.
C       INITIALIZE RAYCOD
          RAYCOD(1)=0
          RAYCOD(2)=-1
          FACTER=FACTER+DABS(SERINC)
          XA=0.0D0
          YA=0.0D0
          ! JN MOD
          ZA=ZA + FACTER
          TRYX=XA
          TRYY=YA
          TRYZ=ZA
          XC=TRYX
          YC=TRYY
          ZC=TRYZ
          !PRINT *, "TRYY 5661 IS ", TRYY
1234      FORMAT(I4,G15.7,G15.7,G15.7)
          !PRINT 1234, ICNT, XC,YC,ZC


          ZEEERR=.FALSE.
          IF (ICNT.EQ.1) THEN
            PRINT *, "YC FOBS 5674 BEFORE GETZEE1 is ", YC
            PRINT *, "YSTRT FOBS 5674 BEFORE GETZEE1 is ", YSTRT
            PRINT *, "ZSTRT FOBS 5674 BEFORE GETZEE1 is ", ZSTRT

            DEBUGZEE = .TRUE.
          END IF
          IF(ALENS(1,NEWOBJ+1).NE.0.0D0) CALL GETZEE1
          IF (ICNT.EQ.1) THEN

          DEBUGZEE = .FALSE.

          PRINT *, "YC FOBBS 5680 AFTER GETZEE1 is ", YC
        END IF

            IF(ZEEERR) THEN
               PRINT *, "ZEEERR Occured!"
               XC=TRYX
               YC=TRYY
               ZC=TRYZ
            END IF
            TRYX=XC
            TRYY=YC
            TRYZ=ZC
            !PRINT *, "TRYY 5684 is ", TRYY
            ICNT=ICNT+1
            !call logger%logTextWithNum("ICNT 5409 = ",ICNT)
            GO TO 993
          END IF
       END IF
      IF(STOPP.EQ.1.AND.ICNT.GT.ICNTEST) THEN
        !call logger%logText("FOBBS Fail STOPP ICNTEST")
                        REFEXT=.FALSE.
                        CPFNEXT=.FALSE.
                        CALL DELPSF
                        SPDEXT=.FALSE.
                        GSPDEXT=.FALSE.
                        IF(.NOT.NULL) FOBYES=.FALSE.
                IF(MSG) THEN
            WRITE(OUTLYNE,*)  "OBJ in REFRAY FAILURE IS ", OBJLEVEL
            CALL SHOWIT(19)
      CALL RAYDOC
      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',RAYCOD(2)
      CALL SHOWIT(1)
        OUTLYNE=
     1  'CHIEF RAY FROM CURRENT OBJECT POINT COULD NOT BE TRACED'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*) '9',RAYCOD(1),RAYCOD(2)
        CALL SHOWIT(1)
                        END IF
C       NO REF RAY TRACED
                        REFEXT=.FALSE.
        IF(.NOT.NULL.AND.F34.EQ.0.AND.F28.EQ.0.AND.F35.EQ.0.AND.
     1F31.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
                        END IF
                        STOPP=0
                        REFEXT=.TRUE.
                        FOBYES=.TRUE.
                        DO J=0,INT(SYSTEM(20))
      IF(ALENS(34,J).EQ.18.0D0) LDIF2=.FALSE.
      IF(ALENS(34,J).EQ.18.0D0) LDIF=.FALSE.
                        END DO
                IF(LDIF2) THEN
C       TRACE DIFFERENTIAL RAY WITHRESPECT TO AN OBJECT POINT
C       SHIFT
C       SAVE GUT RAY DATA
                        GUTRAY(1:50,0:499)=REFRY(1:50,0:499)
C       SAVE REFRY DATA
                        SAVE=REFRY
        SREFDIFEXT=.FALSE.
        DIFRAYTRACE=.TRUE.
        CALL FRFDIF
        DIFRAYTRACE=.FALSE.
        IF(STOPP.EQ.1) THEN
      STOPP=0
C       RESTORE REFRY DATA
                        REFRY=SAVE
                        IF(WQ.NE.'NULL'.AND.F34.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
                        END IF
C       RESTORE REFRY DATA
                        REFRY=SAVE
C       NO DIF TRACE
                        END IF
C
                        FOB0=0
C
C       THE RETURN WILL SEND BACK VIA THE COMMON/REFR COMMON
C       THE FULL RAY DATA FOR THE FULLY AIMMED AND TRACED REFERENCE
C       RAY. THEN ANY APPROPRIATE FOB RELATED PRINTOUT WILL BE DONE.
        YK=REFRY(2,NEWIMG)
        XK=REFRY(1,NEWIMG)
        ANGLE1=REFRY(11,NEWOBJ)*180.0D0/PII
        ANGLE2=REFRY(12,NEWOBJ)*180.0D0/PII
        IF(WQ.EQ.'P') THEN
        WRITE(OUTLYNE,250)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,300) LAMBDA
      CALL SHOWIT(0)
      IF(SYSTEM(18).EQ.0.0D0) THEN
      IF(SYSTEM(6).EQ.1.0D0) LUNI='IN '
      IF(SYSTEM(6).EQ.2.0D0) LUNI='CM '
      IF(SYSTEM(6).EQ.3.0D0) LUNI='MM '
      IF(SYSTEM(6).EQ.4.0D0) LUNI='M  '
        WRITE(OUTLYNE,3302) REFRY(1,NEWOBJ),LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,3303) REFRY(2,NEWOBJ),LUNI
      CALL SHOWIT(0)
      XPFOB=REFRY(1,NEWOBJ)
      YPFOB=REFRY(2,NEWOBJ)
                   ELSE
      LUNI='DEG'
        ANGLE1=REFRY(11,NEWOBJ)*180.0D0/PII
        ANGLE2=REFRY(12,NEWOBJ)*180.0D0/PII
        WRITE(OUTLYNE,302) ANGLE1,LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,303) ANGLE2,LUNI
      CALL SHOWIT(0)
      XPFOB=ANGLE1
      YPFOB=ANGLE2
                   END IF
        WRITE(OUTLYNE,350)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,301) Y00,X00,Z0
      CALL SHOWIT(0)
                        END IF
        IF(WQ.EQ.'PIC') THEN
        WRITE(OUTLYNE,250)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,300) LAMBDA
      CALL SHOWIT(0)
      IF(SYSTEM(18).EQ.0.0D0) THEN
      IF(SYSTEM(6).EQ.1.0D0) LUNI='IN '
      IF(SYSTEM(6).EQ.2.0D0) LUNI='CM '
      IF(SYSTEM(6).EQ.3.0D0) LUNI='MM '
      IF(SYSTEM(6).EQ.4.0D0) LUNI='M  '
        WRITE(OUTLYNE,3302) REFRY(1,NEWOBJ),LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,3303) REFRY(2,NEWOBJ),LUNI
      CALL SHOWIT(0)
      XPFOB=REFRY(1,NEWOBJ)
      YPFOB=REFRY(2,NEWOBJ)
                   ELSE
      LUNI='DEG'
        ANGLE1=REFRY(11,NEWOBJ)*180.0D0/PII
        ANGLE2=REFRY(12,NEWOBJ)*180.0D0/PII
        WRITE(OUTLYNE,302) ANGLE1,LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,303) ANGLE2,LUNI
      CALL SHOWIT(0)
      XPFOB=ANGLE1
      YPFOB=ANGLE2
                   END IF
        WRITE(OUTLYNE,400)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,401) Y00,X00,Z0,YK,XK
      CALL SHOWIT(0)
                        END IF
        IF(WQ.EQ.'PICNH') THEN
        WRITE(OUTLYNE,401) Y00,X00,Z0,YK,XK
      CALL SHOWIT(0)
                        END IF
C       REFERENCE RAY WAS TRACED, NOW TRACE THE DIFFERENTIAL RAY
C       DATA IF FOB HAS QUALIFIERS PFS OR PFSNH.
        IF(WQ.EQ.'PFS'.OR.WQ.EQ.'PFSNH') THEN
C       SAVE REFRY DATA
                        SAVE=REFRY
        STOPP=0
        SREFDIFEXT=.FALSE.
        DIFRAYTRACE=.TRUE.
        CALL FOBDIF
        DIFRAYTRACE=.FALSE.
        IF(REFEXT) STOPP=0
        IF(STOPP.EQ.1) THEN
      STOPP=0
C       RESTORE REFRY DATA
                        REFRY=SAVE
                        IF(WQ.NE.'NULL'.AND.F34.EQ.0) CALL MACFAL
                        RETURN
                        ELSE
                        STOPP=0
                        REFEXT=.TRUE.
                        END IF
C       RESTORE REFRY DATA
                        REFRY(I,J)=SAVE(I,J)
        IF(WQ.EQ.'PFS') THEN
C       OUTPUT A PRINTED HEADER WITH APPROPRIATE VALUES
        WRITE(OUTLYNE,250)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,300) LAMBDA
      CALL SHOWIT(0)
      IF(SYSTEM(18).EQ.0.0D0) THEN
      IF(SYSTEM(6).EQ.1.0D0) LUNI='IN '
      IF(SYSTEM(6).EQ.2.0D0) LUNI='CM '
      IF(SYSTEM(6).EQ.3.0D0) LUNI='MM '
      IF(SYSTEM(6).EQ.4.0D0) LUNI='M  '
        WRITE(OUTLYNE,3302) REFRY(1,NEWOBJ),LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,3303) REFRY(2,NEWOBJ),LUNI
      CALL SHOWIT(0)
      XPFOB=REFRY(1,NEWOBJ)
      YPFOB=REFRY(2,NEWOBJ)
                   ELSE
      LUNI='DEG'
        ANGLE1=REFRY(11,NEWOBJ)*180.0D0/PII
        ANGLE2=REFRY(12,NEWOBJ)*180.0D0/PII
        WRITE(OUTLYNE,302) ANGLE1,LUNI
      CALL SHOWIT(0)
        WRITE(OUTLYNE,303) ANGLE2,LUNI
      CALL SHOWIT(0)
      XPFOB=ANGLE1
      YPFOB=ANGLE2
                   END IF
        WRITE(OUTLYNE,101)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,401) Y00,X00,Z0,FT,FS
      CALL SHOWIT(0)
                        END IF
        IF(WQ.EQ.'PFSNH') THEN
C       OUTPUT APPROPRIATE VALUES
        WRITE(OUTLYNE,401) Y00,X00,Z0,FT,FS
      CALL SHOWIT(0)
                        END IF
C       PROCEED
                        END IF
C
                        STOPP=0
                        REFEXT=.TRUE.
                        RETURN
 250    FORMAT('REFERENCE RAY TRACED')
 300    FORMAT('WAVELENGTH = ',G12.4,' MICRONS')
 302    FORMAT('X-FIELD ANGLE = ',G15.7,1X,A3)
 303    FORMAT('Y-FIELD ANGLE = ',G15.7,1X,A3)
 3302   FORMAT('X-OBJECT HEIGHT = ',G15.7,1X,A3)
 3303   FORMAT('Y-OBJECT HEIGHT = ',G15.7,1X,A3)
 401    FORMAT(G10.2,2X,G10.2,2X,G10.2,2X,G14.6,2X,G14.6)
 350    FORMAT(2X,'Y-FOB     ',2X,'X-FOB     ',2X,'DELTA-Z   ')
 400    FORMAT(2X,'Y-FOB     ',2X,'X-FOB     ',2X,'DELTA-Z',
     1  2X,'Y-IMAGE HT. ',4X,'X-IMAGE HT. ')
 101    FORMAT(2X,'Y-FOB     ',2X,'X-FOB     ',2X,'DELTA-Z   ',
     1  2X,'T-FOCUS POS.',4X,'S-FOCUS POS.')
 301    FORMAT(G10.2,2X,G10.2,2X,G10.2)
                        END


      subroutine ISYFIELDANGLE90DEGREES(ANGJK, boolResult)
        use GLOBALS

        real*8, intent(in) :: ANGJK
        logical, intent(inout) :: boolresult
        character(len=140) :: tmpTxt

        boolResult = .FALSE.
        WRITE(tmpTxt, *) "Checking Angle Input ", ANGJK

        !call logger%logText(trim(tmpTxt))

      IF(ANGJK.GT.89.9999D0.AND.
     1ANGJK1.LT.90.0001D0.OR.
     2ANGJK1.LT.-89.9999D0.AND.
     3ANGJK1.GT.-90.0001D0.OR.
     4ANGJK1.GT.-270.0001D0.AND.
     5ANGJK1.LT.-269.9999D0.OR.
     6ANGJK1.GT.269.9999D0.AND.
     7ANGJK1.LT.270.0001D0) THEN
        boolResult = .TRUE.
      end if

      end subroutine

      subroutine ADJUSTANGLEIFCLOSETO90(ANG)
        use GLOBALS
        real*8, intent(inout) :: ANG
        character(len=140) :: tmpTxt

        !write(tmpTxt, *) "Checking if Angle is close to n90  ", ANG
        !call logger%logText(tmpTxt)
      IF(ANGJK.GT.89.9999D0.AND.
     1 ANGJK.LT.90.0001D0)  ANGJK1=89.9999D0

      IF(ANGJK.LT.-89.9999D0.AND.
     1 ANGJK.GT.-90.0001D0) ANGJK=-89.9999D0

      IF(ANGJK.GT.-270.0001D0.AND.
     1 ANGJK.LT.-269.9999D0) ANGJK=89.9999D0

      IF(ANGJK.GT.269.9999D0.AND.
     1 ANGJK.LT.270.0001D0) ANGJK=-89.9999D0

      !write(tmpTxt, *) "Updated Angle  ", ANG
      !call logger%logText(tmpTxt)

      end subroutine

      subroutine HANDLESAGRETFAILURE(MSG, NEWOBJ)
        integer, intent(in) :: NEWOBJ
        logical, intent(in) :: MSG

        INTEGER STOPP
        COMMON/RAYSTP/STOPP
        LOGICAL REFEXT,NULL,FAIL,RAYEXT,GPRAYEXT
        COMMON/RFEXIT/REFEXT,NULL
        COMMON/RAYC/RAYCOD
        INTEGER RAYCOD(1:2),REFITR,MRAYS
        CHARACTER*140 OUTLYNE
        CHARACTER*256 OUTLYNE_LONG
        COMMON/REPLWRIT/OUTLYNE,OUTLYNE_LONG


          IF(MSG) THEN
          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',NEWOBJ
          CALL SHOWIT(1)
          END IF

        STOPP=1
        RAYCOD(1)=16
        RAYCOD(2)=NEWOBJ
        REFEXT=.FALSE.
        CALL MACFAL
        RETURN

      end subroutine

      subroutine CHECKWAVELENGTHBOUNDS()
        use GLOBALS
        implicit none

        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATSPD.INC'

        IF(INT(W4).NE.1.AND.
     1INT(W4).NE.2.AND.
     1INT(W4).NE.3.AND.
     1INT(W4).NE.4.AND.
     1INT(W4).NE.5.AND.
     1INT(W4).NE.6.AND.
     1INT(W4).NE.7.AND.
     1INT(W4).NE.8.AND.
     1INT(W4).NE.9.AND.
     1INT(W4).NE.10) THEN
        OUTLYNE='WAVELENGTH NUMBER BEYOND LEGAL BOUNDS'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
      IF(W4.GE.1.0D0.AND.W4.LE.5.0D0) THEN
      IF(SYSTEM(INT(W4)).EQ.0.0D0) THEN
                IF(MSG) THEN
      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',NEWOBJ
      CALL SHOWIT(1)
        OUTLYNE=
     1'REFERENCE RAY CAN NOT BE TRACED AT ZERO WAVELENGTH'
      CALL SHOWIT(1)
                        END IF
          !PRINT *, "STOPP=1 4827"
          STOPP=1
          RAYCOD(1)=11
          RAYCOD(2)=NEWOBJ
          REFEXT=.FALSE.
          IF(.NOT.NULL) CALL MACFAL
          RETURN
      END IF
                        END IF
      IF(W4.GE.6.0D0.AND.W4.LE.10.0D0) THEN
      IF(SYSTEM(65+INT(W4)).EQ.0.0D0) THEN
                IF(MSG) THEN
      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',NEWOBJ
      CALL SHOWIT(1)
        OUTLYNE=
     1'REFERENCE RAY CAN NOT BE TRACED AT ZERO WAVELENGTH'
      CALL SHOWIT(1)
                        END IF
                STOPP=1
                RAYCOD(1)=11
                RAYCOD(2)=NEWOBJ
                REFEXT=.FALSE.
                IF(.NOT.NULL) CALL MACFAL
                      RETURN
                      END IF
                      END IF

      end subroutine

      SUBROUTINE CHECK_OBJ_REF_IMG_SURFACES()

        use GLOBALS
        implicit none

        integer :: I

        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATSPD.INC'



C
C       PROCESS W5 IF IT IS NOT DEFAULT
C
        IF(DF5.NE.1) THEN
      IF(WC.EQ.'FOB') THEN
C       W5 NOT DEFAULT
               W5=DABS(W5)
C
        NEWOBJ=INT(W5/1000000.0D0)
        NEWREF=INT(DMOD(W5,1000000.0D0)/1000.0D0)
        NEWIMG=INT(DMOD(W5,1000.0D0))
      IF(NEWREF.LE.NEWOBJ.OR.NEWIMG.LE.NEWOBJ.OR.NEWIMG.LE.
     1NEWREF) THEN
        OUTLYNE='INVALID SURFACE ORDER IN SURFACE RE-ASSIGNMENT'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
C
                      DO I=0,NEWREF
      IF(ALENS(25,I).EQ.2.0D0.OR.ALENS(25,I).EQ.3.0D0) THEN
        OUTLYNE='A "TILT AUTO" OR "TILT AUTOM" IS NOT ALLOWED'
      CALL SHOWIT(1)
        OUTLYNE='BEFORE OR ON THE NEW REFERENCE SURFACE'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                NEWOBJ=0
                NEWREF=INT(SYSTEM(25))
                NEWIMG=INT(SYSTEM(20))
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
                        END DO
C
        IF(NEWREF.LT.0.OR.NEWREF.GT.INT(SYSTEM(20)))THEN
        OUTLYNE='NEW REFERENCE SURFACE NUMBER BEYOND LEGAL BOUNDS'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                NEWOBJ=0
                NEWREF=INT(SYSTEM(25))
                NEWIMG=INT(SYSTEM(20))
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
        IF(NEWIMG.LT.0.OR.NEWIMG.GT.INT(SYSTEM(20))) THEN
        OUTLYNE='NEW IMAGE SURFACE NUMBER BEYOND LEGAL BOUNDS'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                NEWOBJ=0
                NEWREF=INT(SYSTEM(25))
                NEWIMG=INT(SYSTEM(20))
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
        IF(NEWOBJ.LT.0.OR.NEWOBJ.GT.INT(SYSTEM(20)))THEN
        OUTLYNE='NEW OBJECT SURFACE NUMBER BEYOND LEGAL BOUNDS'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                NEWOBJ=0
                NEWREF=INT(SYSTEM(25))
                NEWIMG=INT(SYSTEM(20))
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
        IF(NEWOBJ.GE.NEWREF.OR.NEWOBJ.GE.NEWIMG.OR.
     1  NEWREF.GE.NEWIMG)THEN
        OUTLYNE=
     1  'NEW OBJECT/REFERENCE/IMAGE SURFACES ILLEGALLY ORDERED'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                NEWOBJ=0
                NEWREF=INT(SYSTEM(25))
                NEWIMG=INT(SYSTEM(20))
                        REFEXT=.FALSE.
                        CALL MACFAL
                        RETURN
                        END IF
                        END IF
      IF(WC.EQ.'IFOB') THEN
                       WW5=W5
                       END IF
C
C       IF W5 WAS DEFAULT, IT WAS SET TO 0.0 ABOVE
                        END IF
C
        IF(WQ.EQ.'NULL') THEN
                        NULL=.TRUE.
                        ELSE
                        NULL=.FALSE.
                        END IF
C
      END SUBROUTINE
