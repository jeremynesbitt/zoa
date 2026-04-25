!     SIXTH FILE OF CAPFN/SPOT ROUTINES

SUBROUTINE FOURN(DATA,NPOINTS,NDAT)
!
   IMPLICIT NONE
!
!     NDAT COMES IN AS 2*NPOINTS*NPOINTS
!
   INTEGER NTOT,NDIM,NN(2),ISIGN,NPREV,IP1,IP2,IP3,N &
   &,IFP1,NREM,IBIT,IDIM,I1,I2,I3,I2REV,I3REV,NDAT &
   &,IFP2,K1,K2,NPOINTS
!
   REAL*8 TWOPI,WR,WI,WPR,WPI,WTEMP,THETA
!
   REAL*8 DATA,TEMPI,TEMPR
!
   DIMENSION DATA(NDAT)
!
!     SET NN ARRAY
   NN(1)=NPOINTS
   NN(2)=NPOINTS
!
!     SET A FORWARD TRANSFER
   ISIGN=+1
   NDIM=2
!
   NTOT=1
   DO 11 IDIM=1,NDIM
      NTOT=NTOT*NN(IDIM)
11 CONTINUE
   NPREV=1
   DO 18 IDIM=1,NDIM
      N=NN(IDIM)
      NREM=NTOT/(N*NPREV)
      IP1=2*NPREV
      IP2=IP1*N
      IP3=IP2*NREM
      I2REV=1
      DO 14 I2=1,IP2,IP1
         IF(I2.LT.I2REV)THEN
            DO 13 I1=I2,I2+IP1-2,2
               DO 12 I3=I1,IP3,IP2
                  I3REV=I2REV+I3-I2
                  TEMPR=DATA(I3)
                  TEMPI=DATA(I3+1)
                  DATA(I3)=DATA(I3REV)
                  DATA(I3+1)=DATA(I3REV+1)
                  DATA(I3REV)=TEMPR
                  DATA(I3REV+1)=TEMPI
12             CONTINUE
13          CONTINUE
         ENDIF
         IBIT=IP2/2
1        IF ((IBIT.GE.IP1).AND.(I2REV.GT.IBIT)) THEN
            I2REV=I2REV-IBIT
            IBIT=IBIT/2
            GO TO 1
         ENDIF
         I2REV=I2REV+IBIT
14    CONTINUE
      IFP1=IP1
2     IF(IFP1.LT.IP2)THEN
         IFP2=2*IFP1
         TWOPI= (2.0D0*3.14159265358979323846D0)
         THETA=ISIGN*TWOPI/(IFP2/IP1)
         WPR=-2.D0*DSIN(0.5D0*THETA)**2
         WPI= DSIN(THETA)
         WR=1.0D0
         WI=0.0D0
         DO 17 I3=1,IFP1,IP1
            DO 16 I1=I3,I3+IP1-2,2
               DO 15 I2=I1,IP3,IFP2
                  K1=I2
                  K2=K1+IFP1
                  TEMPR=(WR)*DATA(K2)-(WI)*DATA(K2+1)
                  TEMPI=(WR)*DATA(K2+1)+(WI)*DATA(K2)
                  DATA(K2)=DATA(K1)-TEMPR
                  DATA(K2+1)=DATA(K1+1)-TEMPI
                  DATA(K1)=DATA(K1)+TEMPR
                  DATA(K1+1)=DATA(K1+1)+TEMPI
15             CONTINUE
16          CONTINUE
            WTEMP=WR
            WR=WR*WPR-WI*WPI+WR
            WI=WI*WPR+WTEMP*WPI+WI
17       CONTINUE
         IFP1=IFP2
         GO TO 2
      ENDIF
      NPREV=N*NPREV
18 CONTINUE
   RETURN
END

SUBROUTINE FFT2(PCOUNT,XPLT,YPLT,FPLT,DFLAG)
   use DATHGR
   use DATSPD
   use DATLEN
   use DATMAI
   IMPLICIT NONE
   REAL PEXTENT,PSPACING,SPACER,PEAK
   INTEGER ROT,I,J,ALLOERR,PCOUNT,COLPAS,DFLAG
!
   REAL FPLT,XPLT,YPLT,FTF,CNTX,CNTY,FACTORL
!
   DIMENSION FTF(:,:),FPLT(PCOUNT,PCOUNT),XPLT(PCOUNT),YPLT(PCOUNT)
   ALLOCATABLE :: FTF
!
   COMMON/PSFPLOTER/PEXTENT,PSPACING,CNTX,CNTY

   DEALLOCATE(FTF,STAT=ALLOERR)
   ALLOCATE(FTF(PCOUNT,PCOUNT),STAT=ALLOERR)
!     FIX THE ARRAY SO IT PLOTS LIKE IN CODE-V
!     WE NEED TO REFLECT IN THE Y-DIRECTION
   ROT=0
!
   DO I=1,PCOUNT
      DO J=1,PCOUNT
         FTF(I,J)=FPLT(PCOUNT+1-I,J)
      END DO
   END DO
   DO J=1,PCOUNT
      DO I=1,PCOUNT
         FPLT(I,J)=FTF(I,J)
      END DO
   END DO
!     WE NEED TO REFLECT IN THE Y-DIRECTION
   DO I=1,PCOUNT
      DO J=1,PCOUNT
         FTF(I,J)=FPLT(I,PCOUNT+1-J)
      END DO
   END DO
   DO J=1,PCOUNT
      DO I=1,PCOUNT
         FPLT(I,J)=FTF(I,J)
      END DO
   END DO
   IF(ROT.EQ.90) THEN
!     ROTATE THE F ARRAY BY 90 DEGREES (CLOCKWISE LOOKING DOWN
!     FROM THE PLUS INTENSITY Z AXIS TOWARD THE MINUS INTENSITY
!     Z AXIS. STORE THE TEMPORARY FUNCTION IN ARRAY FTF
      DO J=1,PCOUNT
         DO I=1,PCOUNT
            FTF(I,J)=FPLT(PCOUNT+1-J,I)
         END DO
      END DO
      DO J=1,PCOUNT
         DO I=1,PCOUNT
            FPLT(I,J)=FTF(I,J)
         END DO
      END DO
   END IF
   PEAK=-1.0E10
   DO I=1,PCOUNT
      DO J=1,PCOUNT
         IF(FPLT(I,J).GE.PEAK) PEAK=FPLT(I,J)
      END DO
   END DO
   IF(PEAK.EQ.0.0) PEAK=1.0
   DO I=1,PCOUNT
      DO J=1,PCOUNT
         FPLT(I,J)=(FPLT(I,J)/PEAK)*1.9
      END DO
   END DO

   DO I=1,PCOUNT
      XPLT(I)=((XPLT(I)/.95)*1500.0)+1500.0
      YPLT(I)=((YPLT(I)/.95)*1500.0)+1500.0
   END DO
   IF(PSFLIN.EQ.1) FACTORL=2000.0
   IF(PSFLIN.EQ.0) FACTORL=1500.0
   DO I=1,PCOUNT
      DO J=1,PCOUNT
         FPLT(I,J)=(FPLT(I,J)/1.9)*FACTORL
      END DO
   END DO
!
!     NOW FUNCTION GOES FROM 0 TO 1800 AND XPLT AND YPLT GO FROM
!     0 TO +2000 EACH
!
!     THE PLOT WILL BE SEEN IN ORTHOGRAPHIC PROJECTION, XPLT ACROSS THE SCREEN,
!     YPLT INTO THE SCREEN AT 45 DEG EL AND AZ AND FPLT UP ON THE SCREEN

!
   DEALLOCATE(FTF,STAT=ALLOERR)
   RETURN
END
! SUB DOPSF.FOR was here.  now in PSF.f90

! SUB DOPUPIL.FOR

SUBROUTINE DOPUPIL(KKK)
   USE GLOBALS
!
   use DATSP1
   use DATSPD
   use DATLEN
   use DATMAI
   use mod_system, only: sys_mode, sys_units, sys_wavelength, sys_wl_weight
   IMPLICIT NONE
!
!     THIS IS SUBROUTINE DOPUPIL.FOR.
!     CALLED BY CMDER FOR COMMAND PUPIL IN FOE
!     THIS CREATES A PUPIL FUNCTION
!
   CHARACTER*9 UNN,UNN1
!
   REAL*8 VALUE,VALVAL,SHTVALUE,GRNX,GRNY,XRANGE,YRANGE &
   &,WVNUM,DIAM,OUTGRIDEXTENT,EXEX,EXEY,V1,&
   &OUTGRIDSPACING,FACTER,EFFER,SEFFER,CEFFER,CVALUE &
   &,XPL1,YPL1,SPACER1,OLDGRI,NEWGRI
!
   INTEGER IT,IA,IB,IQ,IJK,WK,IER,PCOUNT,HOLN,KKK
!
   REAL*8 FAF,FAFF,FSUM,IIII,IV,PSFXCENT,PSFYCENT &
   &,FACTER1,SPTT,SPTOT,FTOT &
   &,SPACER,PEAKER,CRAYX,CRAYY,X2JK,Y2JK,F2JK,IWLIJK(1:10)&
   &,C
!
   COMMON/PEPITO/IWLIJK,IJK
!
   REAL SPACING,EXTENT,CNTX,CNTY,PEXTENT,PSPACING &
   &,PEAKADJ
!
   LOGICAL EXIS51,OPEN51,PSFERR,NOCOBSPSF
!
   COMMON/PSFCOBS/NOCOBSPSF
!
   LOGICAL ERR
!
!
   INTEGER IREAL,IIMAG,INDEX,SHTNM,IX,IY,II,I,J,NDAT,NDIM,&
   &MM,III,JJJ,IIX,IIY,ALLOERR,HI,MMM,IIIX,IIIY,DFLAG
!
   LOGICAL ERRR,ERRFOB
!
   COMMON/PSFPLOTER/PEXTENT,PSPACING,CNTX,CNTY
!
   DEALLOCATE(FOEGRID1,STAT=ALLOERR)
   GPREG(101:110)=0.0D0
!
   PSFWV(1)=sys_wavelength(1)
   PSFWV(2)=sys_wavelength(2)
   PSFWV(3)=sys_wavelength(3)
   PSFWV(4)=sys_wavelength(4)
   PSFWV(5)=sys_wavelength(5)
   PSFWV(6)=sys_wavelength(6)
   PSFWV(7)=sys_wavelength(7)
   PSFWV(8)=sys_wavelength(8)
   PSFWV(9)=sys_wavelength(9)
   PSFWV(10)=sys_wavelength(10)
!
!     M IS THE DIMENSION OF THE REQUESTED GRID OVER THE PUPIL
!     THE TRANSFORM IS TWICE THIS BIG
   HI=0
   MM=TGR
   MMM=MM-1
   NDAT=MM*MM*2
   NDIM=2
!
!     HERE IS WHERE WE BUILD THE PSF
   IF(S1.EQ.1) DFLAG=1
   IF(S1.EQ.0) DFLAG=0
!
   IF(.NOT.CPFNEXT) THEN
      OUTLYNE='NO COMPLEX APERTURE FUNCTION EXISTS'
      CALL SHOWIT(1)
      OUTLYNE='NO PSF CALCULATION IS POSSIBLE'
      CALL SHOWIT(1)
      CALL MACFAL
      CALL DELPSF
      RETURN
   END IF
!     CALC SPTOT
   SPTOT=0.0D0
   SPTOT=SPTOT+(sys_wl_weight(1))
   SPTOT=SPTOT+(sys_wl_weight(2))
   SPTOT=SPTOT+(sys_wl_weight(3))
   SPTOT=SPTOT+(sys_wl_weight(4))
   SPTOT=SPTOT+(sys_wl_weight(5))
   SPTOT=SPTOT+(sys_wl_weight(6))
   SPTOT=SPTOT+(sys_wl_weight(7))
   SPTOT=SPTOT+(sys_wl_weight(8))
   SPTOT=SPTOT+(sys_wl_weight(9))
   SPTOT=SPTOT+(sys_wl_weight(10))
!
!     NOW COMPUTE THE SHORTEST WAVELENGTH IN CURENT LENS UNITS
!     CALC THE SHRTWAVE
   SHRTWAVE=0.0D0
   IF(sys_wl_weight(1).NE.0.0D0.AND.sys_wavelength(1).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(1)
      GO TO 314
   END IF
   IF(sys_wl_weight(2).NE.0.0D0.AND.sys_wavelength(2).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(2)
      GO TO 314
   END IF
   IF(sys_wl_weight(3).NE.0.0D0.AND.sys_wavelength(3).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(3)
      GO TO 314
   END IF
   IF(sys_wl_weight(4).NE.0.0D0.AND.sys_wavelength(4).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(4)
      GO TO 314
   END IF
   IF(sys_wl_weight(5).NE.0.0D0.AND.sys_wavelength(5).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(5)
      GO TO 314
   END IF
   IF(sys_wl_weight(6).NE.0.0D0.AND.sys_wavelength(6).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(6)
      GO TO 314
   END IF
   IF(sys_wl_weight(7).NE.0.0D0.AND.sys_wavelength(7).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(7)
      GO TO 314
   END IF
   IF(sys_wl_weight(8).NE.0.0D0.AND.sys_wavelength(8).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(8)
      GO TO 314
   END IF
   IF(sys_wl_weight(9).NE.0.0D0.AND.sys_wavelength(9).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(9)
      GO TO 314
   END IF
   IF(sys_wl_weight(10).NE.0.0D0.AND.sys_wavelength(10).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(10)
      GO TO 314
   END IF
314 CONTINUE
   IF(SHRTWAVE.EQ.0.0D0) THEN
      OUTLYNE='WAVELENGTHS ARE ALL ZERO'
      CALL SHOWIT(1)
      OUTLYNE='NO "PSF" CAN BE CALCULATED'
      CALL SHOWIT(1)
      CALL MACFAL
      CALL DELPSF
      RETURN
   END IF
!
   IF(sys_wl_weight(1).NE.0.0D0.AND.sys_wavelength(1).LE.SHRTWAVE &
   &.AND.sys_wavelength(1).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(1)
      SHTNM=1
   END IF
   IF(sys_wl_weight(2).NE.0.0D0.AND.sys_wavelength(2).LE.SHRTWAVE &
   &.AND.sys_wavelength(2).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(2)
      SHTNM=2
   END IF
   IF(sys_wl_weight(3).NE.0.0D0.AND.sys_wavelength(3).LE.SHRTWAVE &
   &.AND.sys_wavelength(3).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(3)
      SHTNM=3
   END IF
   IF(sys_wl_weight(4).NE.0.0D0.AND.sys_wavelength(4).LE.SHRTWAVE &
   &.AND.sys_wavelength(4).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(4)
      SHTNM=4
   END IF
   IF(sys_wl_weight(5).NE.0.0D0.AND.sys_wavelength(5).LE.SHRTWAVE &
   &.AND.sys_wavelength(5).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(5)
      SHTNM=5
   END IF
   IF(sys_wl_weight(6).NE.0.0D0.AND.sys_wavelength(6).LE.SHRTWAVE &
   &.AND.sys_wavelength(6).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(6)
      SHTNM=6
   END IF
   IF(sys_wl_weight(7).NE.0.0D0.AND.sys_wavelength(7).LE.SHRTWAVE &
   &.AND.sys_wavelength(7).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(7)
      SHTNM=7
   END IF
   IF(sys_wl_weight(8).NE.0.0D0.AND.sys_wavelength(8).LE.SHRTWAVE &
   &.AND.sys_wavelength(8).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(8)
      SHTNM=8
   END IF
   IF(sys_wl_weight(9).NE.0.0D0.AND.sys_wavelength(9).LE.SHRTWAVE &
   &.AND.sys_wavelength(9).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(9)
      SHTNM=9
   END IF
   IF(sys_wl_weight(10).NE.0.0D0.AND.sys_wavelength(10).LE.SHRTWAVE &
   &.AND.sys_wavelength(10).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(10)
      SHTNM=10
   END IF
!     SHRTWAVE IS IN MICRONS, CHANGE TO LENS UNITS
   VALUE=SHRTWAVE
   IF(sys_units().EQ.1.0D0) SHTVALUE=((VALUE*1.0D-3)/25.4D0)
   IF(sys_units().EQ.2.0D0) SHTVALUE=(VALUE*1.0D-4)
   IF(sys_units().EQ.3.0D0) SHTVALUE=(VALUE*1.0D-3)
   IF(sys_units().EQ.4.0D0) SHTVALUE=(VALUE*1.0D-6)
!
!     WE NOW HAVE THE WAVELENGTH AND WAVELENGTH NUMBER OF THE
!     SHORTEST WAVELENGTH FOR WHICH THE SPECTRAL WEIGHTING FACTOR
!     IS NON-ZERO. THESE ARE SHTVALUE AND SHTNM
!
!     TGR ALWAYS RULES AND IS NOT CHANGED.
!
!     NOW COMPUTE THE PUPIL GRID SPACING AND EXTENT
!     THIS SETS THE GRID TO BE USED FOR THE PSF
!     IF THE MODE IS FOCAL OR UFOCAL
!     WE WANT THE ENTRANCE PUPIL DIAMETERS IN THE X AND Y DIRECTIONS
!     ENTRANCE PUPIL DIAMETERS IN THE X AND Y DIRECTION AT THE SHORT WAVELENGTH
!     THE LARGER OF THE TWO ENTRANCE PUPIL DIAMETERS WILL ACT AS THE
!     DIMENSION FOR COMPUTING THE PUPIL GRID SPACING AND EXTENT. THIS
!     IS CONSISTENT WITH THE WAY THE GRID IS SET IN THE CAPFN RAYTRACE
!     GRID IN COMPAP.FOR. THESE ARE THE UNVIGNETTED ENTRANCE PUPIL
!     VALUES. VIGNETTING IS ACCOUNTED FOR IN THE PSF RAY TRACE.
!
   IF(sys_mode().LE.2.0D0) THEN
      IF(REFEXT) THEN
         ERR=.FALSE.
         MSG=.FALSE.
         CALL FNUMX(VALVAL,ERR)
         IF(.NOT.ERR) GRNX=VALVAL
         ERR=.FALSE.
         MSG=.FALSE.
         CALL FNUMY(VALVAL,ERR)
         IF(.NOT.ERR) GRNY=VALVAL
      ELSE
         GRNX=RBFNX
         GRNY=RBFNY
      END IF
   ELSE
!     AFOCAL
      IF(REFEXT) THEN
         ERR=.FALSE.
         MSG=.FALSE.
         CALL EXPDIAX(VALVAL,ERR)
         IF(.NOT.ERR) EXEX=VALVAL
         ERR=.FALSE.
         MSG=.FALSE.
         CALL EXPDIAY(VALVAL,ERR)
         IF(.NOT.ERR) EXEY=VALVAL
      ELSE
         EXEX=EXDIAX
         EXEY=EXDIAY
      END IF
   END IF
!
   IF(sys_mode().LE.2.0D0) THEN
!     FOCAL
      DIAM=GRNX
      IF(GRNY.LT.GRNX) DIAM=GRNY
      IF(DIAM.EQ.0.0D0) THEN
         OUTLYNE='F/NUMBER WAS ZERO, NO PSF CALCULATION POSSIBLE'
         CALL SHOWIT(1)
         CALL MACFAL
         CALL DELPSF
         RETURN
      END IF
   ELSE
!     AFOCAL
      DIAM=EXEX
      IF(EXEY.GT.EXEX) DIAM=EXEY
      IF(DIAM.EQ.0.0D0) THEN
         OUTLYNE=&
         &'EXIT PUPIL DIAMETER WAS ZERO, NO PSF CALCULATION POSSIBLE'
         CALL SHOWIT(1)
         CALL MACFAL
         CALL DELPSF
         RETURN
      ELSE
!     MAKE THE SYSTEM HAVE A 100 UNIT FOCAL LENGTH
         DIAM=1.0/DIAM
      END IF
   END IF

   IF(GRIFLG.EQ.1)&
   &OUTGRIDEXTENT=DIAM*(NNRD-1.0D0)*SHTVALUE
   IF(GRIFLG.EQ.0)&
   &OUTGRIDEXTENT=DIAM*(DBLE(NRD)-1.0D0)*SHTVALUE
   OUTGRIDSPACING=OUTGRIDEXTENT/DBLE(TGR-1)
   GRI=OUTGRIDSPACING
!
!     NOW PROCESS THE CAPFN DATA INTO PSF DATA
!
   DEALLOCATE(FOEGRID1,STAT=ALLOERR)
   ALLOCATE(FOEGRID1(MM,MM,1:10,1:2),STAT=ALLOERR)
!
   IWIW=IW**2
   I=2
!
   III=MM
   JJJ=MM
   FOEGRID1(1:MM,1:MM,1:10,1:2)=0.0D0
   DO J=1,NUMCOL
!     DOING A COLOR NOW
      IX=0
      IY=1
      II=0

!     WE WANT TO DO IWIW READS AND LOADS OF THE A ARRAY
10    II=II+1
      IX=IX+1
      IF(IX.GT.IW) THEN
         IY=IY+1
         IX=1
      END IF
!     LOAD DSPOT(*) WITH DSPOTT(*,ID)
      ID=I-1
      CALL SPOTIT(4)
      SPTT=(DSPOT(17))
      I=I+1
!     LOAD AN ARRAY ELEMENT WITH REAL AND IMAGINARY CAPFN
!     FOR A COLOR
!     COL,ROW INDEXED AND ZERO PADDED, THAT IS WHY THE IIX AND IIY
!     ARE NEEDED

      IIIX=((MM/2)-((NRD)/2))
      IIIY=((MM/2)-((NRD)/2))
      IF(IIIX.LT.0) IIIX=0
      IF(IIIY.LT.0) IIIY=0
      IIX=IX+IIIX
      IIY=IY+IIIY
      IF(DSPOT(12).NE.0.0D0) THEN
      END IF
      IF(KKK.EQ.1) THEN
!       SAVE AIB
!     REAL PART
         DSPOT(12)=DSPOT(12)*W3
         DSPOT(37)=DSPOT(12)*RAYRAY(9,NEWIMG)
         FOEGRID1(IIX,IIY,J,1)=&
         &DSQRT(DABS(DSPOT(12)))*DCOS(DSPOT(4))
!     IMAGINARY PART
         FOEGRID1(IIX,IIY,J,2)=&
         &-DSQRT(DABS(DSPOT(12)))*DSIN(DSPOT(4))
         WVNUM=DSPOT(16)
      ELSE
!       SAVE AS REITHETA
!     MODULUS
         DSPOT(12)=DSPOT(12)*W3
         DSPOT(37)=DSPOT(12)*RAYRAY(9,NEWIMG)
         FOEGRID1(IIX,IIY,J,1)=DSQRT(DABS(DSPOT(12)))
!     PHASE IN WAVES
         FOEGRID1(IIX,IIY,J,2)=DSPOT(4)
      END IF
!
      IF(II.LT.IWIW) GO TO 10
!     FELL THROUGH, FINISHED READING IWIW POINTS
      IF(sys_mode().LE.2.0D0) THEN
!       FOCAL
         EXTENT=SNGL(OUTGRIDEXTENT)
         SPACING=SNGL(OUTGRIDSPACING)
      ELSE
!       AFOCAL
         EXTENT=SNGL(OUTGRIDEXTENT)
         SPACING=SNGL(OUTGRIDSPACING)
      END IF
      PEXTENT=EXTENT*REAL(PGR)/REAL(TGR-1)
      PSPACING=SPACING
!     THE ARRAYS FOR A COLOR ARE WRITTEN, NOW SET UP THE X AND Y ARRAYS
   END DO
   RETURN
END
! SUB NRDCALC.FOR

SUBROUTINE NRDCALC(ERRORR)
!
   use DATSP1
   use DATSPD
   use DATLEN
   use DATMAI
   use mod_system, only: sys_mode, sys_units, sys_wavelength, sys_wl_weight
   IMPLICIT NONE
!
!     THIS CALCULATED A NON-INTEGER NRD WHEN GRI IS SPECIFIED
!
   REAL*8 SHTVALUE,VALUE,FNX,FNY,FNN,VALVAL &
   &,TNRD
!
   LOGICAL ERRORR,ERRR,ERR
!
!
   INTEGER SHTNM
!
!     NOW COMPUTE THE SHORTEST WAVELENGTH IN CURENT LENS UNITS
!     CALC THE SHRTWAVE
   SHRTWAVE=0.0D0
   IF(sys_wl_weight(1).NE.0.0D0.AND.sys_wavelength(1).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(1)
      GO TO 314
   END IF
   IF(sys_wl_weight(2).NE.0.0D0.AND.sys_wavelength(2).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(2)
      GO TO 314
   END IF
   IF(sys_wl_weight(3).NE.0.0D0.AND.sys_wavelength(3).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(3)
      GO TO 314
   END IF
   IF(sys_wl_weight(4).NE.0.0D0.AND.sys_wavelength(4).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(4)
      GO TO 314
   END IF
   IF(sys_wl_weight(5).NE.0.0D0.AND.sys_wavelength(5).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(5)
      GO TO 314
   END IF
   IF(sys_wl_weight(6).NE.0.0D0.AND.sys_wavelength(6).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(6)
      GO TO 314
   END IF
   IF(sys_wl_weight(7).NE.0.0D0.AND.sys_wavelength(7).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(7)
      GO TO 314
   END IF
   IF(sys_wl_weight(8).NE.0.0D0.AND.sys_wavelength(8).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(8)
      GO TO 314
   END IF
   IF(sys_wl_weight(9).NE.0.0D0.AND.sys_wavelength(9).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(9)
      GO TO 314
   END IF
   IF(sys_wl_weight(10).NE.0.0D0.AND.sys_wavelength(10).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(10)
      GO TO 314
   END IF
314 CONTINUE
   IF(SHRTWAVE.EQ.0.0D0) THEN
      OUTLYNE='WAVELENGTHS ARE ALL ZERO'
      CALL SHOWIT(1)
      OUTLYNE='NO "PSF" CAN BE CALCULATED'
      CALL SHOWIT(1)
      ERRORR=.TRUE.
      CALL MACFAL
      CALL DELPSF
      RETURN
   END IF
!
   IF(sys_wl_weight(1).NE.0.0D0.AND.sys_wavelength(1).LE.SHRTWAVE &
   &.AND.sys_wavelength(1).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(1)
      SHTNM=1
   END IF
   IF(sys_wl_weight(2).NE.0.0D0.AND.sys_wavelength(2).LE.SHRTWAVE &
   &.AND.sys_wavelength(2).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(2)
      SHTNM=2
   END IF
   IF(sys_wl_weight(3).NE.0.0D0.AND.sys_wavelength(3).LE.SHRTWAVE &
   &.AND.sys_wavelength(3).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(3)
      SHTNM=3
   END IF
   IF(sys_wl_weight(4).NE.0.0D0.AND.sys_wavelength(4).LE.SHRTWAVE &
   &.AND.sys_wavelength(4).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(4)
      SHTNM=4
   END IF
   IF(sys_wl_weight(5).NE.0.0D0.AND.sys_wavelength(5).LE.SHRTWAVE &
   &.AND.sys_wavelength(5).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(5)
      SHTNM=5
   END IF
   IF(sys_wl_weight(6).NE.0.0D0.AND.sys_wavelength(6).LE.SHRTWAVE &
   &.AND.sys_wavelength(6).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(6)
      SHTNM=6
   END IF
   IF(sys_wl_weight(7).NE.0.0D0.AND.sys_wavelength(7).LE.SHRTWAVE &
   &.AND.sys_wavelength(7).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(7)
      SHTNM=7
   END IF
   IF(sys_wl_weight(8).NE.0.0D0.AND.sys_wavelength(8).LE.SHRTWAVE &
   &.AND.sys_wavelength(8).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(8)
      SHTNM=8
   END IF
   IF(sys_wl_weight(9).NE.0.0D0.AND.sys_wavelength(9).LE.SHRTWAVE &
   &.AND.sys_wavelength(9).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(9)
      SHTNM=9
   END IF
   IF(sys_wl_weight(10).NE.0.0D0.AND.sys_wavelength(10).LE.SHRTWAVE &
   &.AND.sys_wavelength(10).NE.0.0D0) THEN
      SHRTWAVE=sys_wavelength(10)
      SHTNM=10
   END IF
!     SHRTWAVE IS IN MICRONS, CHANGE TO LENS UNITS
   VALUE=SHRTWAVE
   IF(sys_units().EQ.1.0D0) SHTVALUE=((VALUE*1.0D-3)/25.4D0)
   IF(sys_units().EQ.2.0D0) SHTVALUE=(VALUE*1.0D-4)
   IF(sys_units().EQ.3.0D0) SHTVALUE=(VALUE*1.0D-3)
   IF(sys_units().EQ.4.0D0) SHTVALUE=(VALUE*1.0D-6)
!
!     WE NOW HAVE THE WAVELENGTH AND WAVELENGTH NUMBER OF THE
!     SHORTEST WAVELENGTH FOR WHICH THE SPECTRAL WEIGHTING FACTOR
!     IS NON-ZERO. THESE ARE SHTVALUE AND SHTNM
!
!     TGR ALWAYS RULES AND IS NOT CHANGED.
!
!     NOW COMPUTE THE NRD
!
   IF(sys_mode().LE.2.0D0) THEN
      IF(REFEXT) THEN
         ERR=.FALSE.
         MSG=.FALSE.
         CALL FNUMX(VALVAL,ERR)
         IF(.NOT.ERR) FNX=VALVAL
         ERR=.FALSE.
         MSG=.FALSE.
         CALL FNUMY(VALVAL,ERR)
         IF(.NOT.ERR) FNY=VALVAL
      ELSE
         FNX=RBFNX
         FNY=RBFNY
      END IF
      FNN=FNX
      IF(FNY.LT.FNX) FNN=FNY
   ELSE
!     AFOCAL
      IF(REFEXT) THEN
         ERR=.FALSE.
         MSG=.FALSE.
         CALL EXPDIAX(VALVAL,ERR)
         IF(.NOT.ERR) FNX=VALVAL
         ERR=.FALSE.
         MSG=.FALSE.
         CALL EXPDIAY(VALVAL,ERR)
         IF(.NOT.ERR) FNY=VALVAL
      ELSE
         FNX=EXDIAX
         FNY=EXDIAY
      END IF
      FNN=FNX
      IF(FNY.GT.FNX) FNN=FNY
      FNN=1.0D0/FNN
   END IF
   FNN=DABS(FNN)
!
   NNRD=DABS(((GRI*DBLE(TGR-1))/(FNN*SHTVALUE))+1.0D0)
   IF(NNRD.GT.DBLE(TGR)) THEN
      NNRD=DABS(DBLE(NRD))
      OUTLYNE='THE NEW "NRD" FOR THE GIVEN "GRI" WAS UNREALISTIC'
      CALL SHOWIT(1)
      OUTLYNE='NO NEW "NRD" VALUE WAS COMPUTED OR WILL BE USED'
      CALL SHOWIT(1)
      OUTLYNE='"GRI" NO LONGER DETERMINES THE "NRD" VALUE'
      CALL SHOWIT(1)
   END IF
   TNRD=DINT(NNRD)
   IF(MOD(TNRD,2.0D0).NE.0.0D0) TNRD=TNRD-1.0D0
   IF(INT(TNRD).NE.0) THEN
      NRD=INT(DABS(TNRD))
      NRDFACTOR=DBLE(NRD-1)/NNRD
   END IF
   IF(INT(TNRD).EQ.0) THEN
      NNRD=DABS(DBLE(NRD))
      OUTLYNE='THE NEW "NRD" FOR THE GIVEN "GRI" WAS UNREALISTIC'
      CALL SHOWIT(1)
      OUTLYNE='NO NEW "NRD" VALUE WAS COMPUTED OR WILL BE USED'
      CALL SHOWIT(1)
      OUTLYNE='"GRI" NO LONGER DETERMINES THE "NRD" VALUE'
      CALL SHOWIT(1)
   END IF
   ERRORR=.FALSE.
   GRIFLG=0
   NRDFLG=1
   RETURN
END
