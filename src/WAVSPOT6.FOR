C     SIXTH FILE OF CAPFN/SPOT ROUTINES

      SUBROUTINE FOURN(DATA,NPOINTS,NDAT)
C
      IMPLICIT NONE
C
C     NDAT COMES IN AS 2*NPOINTS*NPOINTS
C
      INTEGER NTOT,NDIM,NN(2),ISIGN,NPREV,IP1,IP2,IP3,N
     1,IFP1,NREM,IBIT,IDIM,I1,I2,I3,I2REV,I3REV,NDAT
     2,IFP2,K1,K2,NPOINTS
C
      REAL*8 TWOPI,WR,WI,WPR,WPI,WTEMP,THETA
C
      REAL*8 DATA,TEMPI,TEMPR
C
      DIMENSION DATA(NDAT)
C
C     SET NN ARRAY
                       NN(1)=NPOINTS
                       NN(2)=NPOINTS
C
C     SET A FORWARD TRANSFER
      ISIGN=+1
      NDIM=2
C
      NTOT=1
      DO 11 IDIM=1,NDIM
      NTOT=NTOT*NN(IDIM)
11    CONTINUE
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
12            CONTINUE
13          CONTINUE
          ENDIF
          IBIT=IP2/2
1         IF ((IBIT.GE.IP1).AND.(I2REV.GT.IBIT)) THEN
            I2REV=I2REV-IBIT
            IBIT=IBIT/2
          GO TO 1
          ENDIF
          I2REV=I2REV+IBIT
14      CONTINUE
        IFP1=IP1
2       IF(IFP1.LT.IP2)THEN
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
15            CONTINUE
16          CONTINUE
            WTEMP=WR
            WR=WR*WPR-WI*WPI+WR
            WI=WI*WPR+WTEMP*WPI+WI
17        CONTINUE
          IFP1=IFP2
        GO TO 2
        ENDIF
        NPREV=N*NPREV
18    CONTINUE
      RETURN
      END

      SUBROUTINE FFT2(PCOUNT,XPLT,YPLT,FPLT,DFLAG)
      IMPLICIT NONE
      REAL PEXTENT,PSPACING,SPACER,PEAK
      INTEGER ROT,I,J,ALLOERR,PCOUNT,COLPAS,DFLAG
C
      REAL FPLT,XPLT,YPLT,FTF,CNTX,CNTY,FACTORL
C
      DIMENSION FTF(:,:),FPLT(PCOUNT,PCOUNT),XPLT(PCOUNT),YPLT(PCOUNT)
      ALLOCATABLE :: FTF
C
      COMMON/PSFPLOTER/PEXTENT,PSPACING,CNTX,CNTY

      INCLUDE 'DATMAI.INC'
      INCLUDE 'DATLEN.INC'
      INCLUDE 'DATSPD.INC'
      INCLUDE 'DATHGR.INC'
      DEALLOCATE(FTF,STAT=ALLOERR)
      ALLOCATE(FTF(PCOUNT,PCOUNT),STAT=ALLOERR)
C     FIX THE ARRAY SO IT PLOTS LIKE IN CODE-V
C     WE NEED TO REFLECT IN THE Y-DIRECTION
      ROT=0
C
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
C     WE NEED TO REFLECT IN THE Y-DIRECTION
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
C     ROTATE THE F ARRAY BY 90 DEGREES (CLOCKWISE LOOKING DOWN
C     FROM THE PLUS INTENSITY Z AXIS TOWARD THE MINUS INTENSITY
C     Z AXIS. STORE THE TEMPORARY FUNCTION IN ARRAY FTF
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
C
C     NOW FUNCTION GOES FROM 0 TO 1800 AND XPLT AND YPLT GO FROM
C     0 TO +2000 EACH
C
C     THE PLOT WILL BE SEEN IN ORTHOGRAPHIC PROJECTION, XPLT ACROSS THE SCREEN,
C     YPLT INTO THE SCREEN AT 45 DEG EL AND AZ AND FPLT UP ON THE SCREEN

C
      DEALLOCATE(FTF,STAT=ALLOERR)
                       RETURN
                       END
C SUB DOPSF.FOR
! To be replaced by PSF.f90
        SUBROUTINE DOPSFOLD
        USE GLOBALS
        use global_widgets, only: curr_psf
        use data_registers
C
        IMPLICIT NONE
C
C     THIS IS SUBROUTINE DOPSF.FOR.
C     CALLED BY CMDER FOR COMMAND PSF
C     THIS DOES DIFFRACTION PSF CALCULATIONS
C
        CHARACTER*9 UNN,UNN1
C
      REAL*8 VALUE,VALVAL,SHTVALUE,GRNX,GRNY,XRANGE,YRANGE
     1,WVNUM,DIAM,OUTGRIDEXTENT,EXEX,EXEY,V1,APEAK,
     2OUTGRIDSPACING,FACTER,EFFER,SEFFER,CEFFER,CVALUE
     3,XPL1,YPL1,CENTERX,CENTERY,SPACER1,OLDGRI,NEWGRI
C
      INTEGER IT,IA,IB,IQ,IJK,WK,IER,PCOUNT,HOLN
C
      INTEGER BINNREC,UNIT_FLAG,MODE_FLAG,IREC
C
      REAL*8 SCALE_FACTOR
C
      REAL*8 FAF,FAFF,FSUM,IIII,IV,PSFXCENT,PSFYCENT
     1,FACTER1,SPTT,SPTOT,FTOT,PUP,DATA
     2,SPACER,PEAKER,CRAYX,CRAYY,X2JK,Y2JK,F2JK,IWLIJK(1:10)
     3,C,F,FHOLDF,FHOLDF1
C
      COMMON/PEPITO/IWLIJK,IJK
C
      REAL SPACING,EXTENT,XPLT,CNTX,CNTY,PEXTENT,PSPACING
     1,YPLT,FPLT,PEAKADJ
      DIMENSION XPLT(:),YPLT(:),FPLT(:,:)
      ALLOCATABLE :: XPLT,YPLT,FPLT
C
      DIMENSION F(:,:),DATA(:),PUP(:,:,:)
     1,FHOLDF(:,:),FHOLDF1(:,:)
     2,CENTERX(:),CENTERY(:)
      ALLOCATABLE :: F,DATA,PUP,FHOLDF,FHOLDF1
     1,CENTERX,CENTERY
C
      LOGICAL EXIS51,OPEN51,PSFERR,NOCOBSPSF
C
      LOGICAL EXIS106,OPEN106
C
      COMMON/PSFCOBS/NOCOBSPSF
C
        LOGICAL ERR
C
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATSPD.INC'
        INCLUDE 'DATSP1.INC'
C
      INTEGER IREAL,IIMAG,INDEX,SHTNM,IX,IY,II,I,J,NDAT,NDIM,
     1MM,III,JJJ,IIX,IIY,ALLOERR,HI,MMM,IIIX,IIIY,DFLAG
C
      LOGICAL ERRR,ERRFOB
C
      COMMON/PSFPLOTER/PEXTENT,PSPACING,CNTX,CNTY
C
      DEALLOCATE(F,PUP,FHOLDF,FHOLDF1,FIMG,STAT=ALLOERR)
      GPREG(101:110)=0.0D0
C
      PSFWV(1)=SYSTEM(1)
      PSFWV(2)=SYSTEM(2)
      PSFWV(3)=SYSTEM(3)
      PSFWV(4)=SYSTEM(4)
      PSFWV(5)=SYSTEM(5)
      PSFWV(6)=SYSTEM(71)
      PSFWV(7)=SYSTEM(72)
      PSFWV(8)=SYSTEM(73)
      PSFWV(9)=SYSTEM(74)
      PSFWV(10)=SYSTEM(75)
C
C     M IS THE DIMENSION OF THE REQUESTED GRID OVER THE PUPIL
C     THE TRANSFORM IS TWICE THIS BIG
      HI=0
      MM=TGR
      MMM=MM-1
      NDAT=MM*MM*2
      NDIM=2
C
C     HERE IS WHERE WE BUILD THE PSF
               IF(S1.EQ.1) DFLAG=1
               IF(S1.EQ.0) DFLAG=0
C
      IF(.NOT.CPFNEXT) THEN
               OUTLYNE='NO COMPLEX APERTURE FUNCTION EXISTS'
               CALL SHOWIT(1)
               OUTLYNE='NO PSF CALCULATION IS POSSIBLE'
               CALL SHOWIT(1)
               CALL MACFAL
               CALL DELPSF
               RETURN
               END IF
C     CALC SPTOT
                   SPTOT=0.0D0
          SPTOT=SPTOT+(SYSTEM(31))
          SPTOT=SPTOT+(SYSTEM(32))
          SPTOT=SPTOT+(SYSTEM(33))
          SPTOT=SPTOT+(SYSTEM(34))
          SPTOT=SPTOT+(SYSTEM(35))
          SPTOT=SPTOT+(SYSTEM(76))
          SPTOT=SPTOT+(SYSTEM(77))
          SPTOT=SPTOT+(SYSTEM(78))
          SPTOT=SPTOT+(SYSTEM(79))
          SPTOT=SPTOT+(SYSTEM(80))
C
C     NOW COMPUTE THE SHORTEST WAVELENGTH IN CURENT LENS UNITS
C     CALC THE SHRTWAVE
        SHRTWAVE=0.0D0
      IF(SYSTEM(31).NE.0.0D0.AND.SYSTEM(1).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(1)
                      GO TO 314
                      END IF
      IF(SYSTEM(32).NE.0.0D0.AND.SYSTEM(2).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(2)
                      GO TO 314
                      END IF
      IF(SYSTEM(33).NE.0.0D0.AND.SYSTEM(3).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(3)
                      GO TO 314
                      END IF
      IF(SYSTEM(34).NE.0.0D0.AND.SYSTEM(4).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(4)
                      GO TO 314
                      END IF
      IF(SYSTEM(35).NE.0.0D0.AND.SYSTEM(5).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(5)
                      GO TO 314
                      END IF
      IF(SYSTEM(76).NE.0.0D0.AND.SYSTEM(71).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(71)
                      GO TO 314
                      END IF
      IF(SYSTEM(77).NE.0.0D0.AND.SYSTEM(72).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(72)
                      GO TO 314
                      END IF
      IF(SYSTEM(78).NE.0.0D0.AND.SYSTEM(73).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(73)
                      GO TO 314
                      END IF
      IF(SYSTEM(79).NE.0.0D0.AND.SYSTEM(74).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(74)
                      GO TO 314
                      END IF
      IF(SYSTEM(80).NE.0.0D0.AND.SYSTEM(75).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(75)
                      GO TO 314
                      END IF
 314                  CONTINUE
      IF(SHRTWAVE.EQ.0.0D0) THEN
        OUTLYNE='WAVELENGTHS ARE ALL ZERO'
      CALL SHOWIT(1)
        OUTLYNE='NO "PSF" CAN BE CALCULATED'
      CALL SHOWIT(1)
                        CALL MACFAL
               CALL DELPSF
                        RETURN
                        END IF
C
      IF(SYSTEM(31).NE.0.0D0.AND.SYSTEM(1).LE.SHRTWAVE
     1.AND.SYSTEM(1).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(1)
      SHTNM=1
      END IF
      IF(SYSTEM(32).NE.0.0D0.AND.SYSTEM(2).LE.SHRTWAVE
     1.AND.SYSTEM(2).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(2)
      SHTNM=2
      END IF
      IF(SYSTEM(33).NE.0.0D0.AND.SYSTEM(3).LE.SHRTWAVE
     1.AND.SYSTEM(3).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(3)
      SHTNM=3
      END IF
      IF(SYSTEM(34).NE.0.0D0.AND.SYSTEM(4).LE.SHRTWAVE
     1.AND.SYSTEM(4).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(4)
      SHTNM=4
      END IF
      IF(SYSTEM(35).NE.0.0D0.AND.SYSTEM(5).LE.SHRTWAVE
     1.AND.SYSTEM(5).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(5)
      SHTNM=5
      END IF
      IF(SYSTEM(76).NE.0.0D0.AND.SYSTEM(71).LE.SHRTWAVE
     1.AND.SYSTEM(71).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(71)
      SHTNM=6
      END IF
      IF(SYSTEM(77).NE.0.0D0.AND.SYSTEM(72).LE.SHRTWAVE
     1.AND.SYSTEM(72).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(72)
      SHTNM=7
      END IF
      IF(SYSTEM(78).NE.0.0D0.AND.SYSTEM(73).LE.SHRTWAVE
     1.AND.SYSTEM(73).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(73)
      SHTNM=8
      END IF
      IF(SYSTEM(79).NE.0.0D0.AND.SYSTEM(74).LE.SHRTWAVE
     1.AND.SYSTEM(74).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(74)
      SHTNM=9
      END IF
      IF(SYSTEM(80).NE.0.0D0.AND.SYSTEM(75).LE.SHRTWAVE
     1.AND.SYSTEM(75).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(75)
      SHTNM=10
      END IF
C     SHRTWAVE IS IN MICRONS, CHANGE TO LENS UNITS
      VALUE=SHRTWAVE
      IF(SYSTEM(6).EQ.1.0D0) SHTVALUE=((VALUE*1.0D-3)/25.4D0)
      IF(SYSTEM(6).EQ.2.0D0) SHTVALUE=(VALUE*1.0D-4)
      IF(SYSTEM(6).EQ.3.0D0) SHTVALUE=(VALUE*1.0D-3)
      IF(SYSTEM(6).EQ.4.0D0) SHTVALUE=(VALUE*1.0D-6)
C
C     WE NOW HAVE THE WAVELENGTH AND WAVELENGTH NUMBER OF THE
C     SHORTEST WAVELENGTH FOR WHICH THE SPECTRAL WEIGHTING FACTOR
C     IS NON-ZERO. THESE ARE SHTVALUE AND SHTNM
C
C     TGR ALWAYS RULES AND IS NOT CHANGED.
C
C     NOW COMPUTE THE PUPIL GRID SPACING AND EXTENT
C     THIS SETS THE GRID TO BE USED FOR THE PSF
C     IF THE MODE IS FOCAL OR UFOCAL
C     WE WANT THE ENTRANCE PUPIL DIAMETERS IN THE X AND Y DIRECTIONS
C     ENTRANCE PUPIL DIAMETERS IN THE X AND Y DIRECTION AT THE SHORT WAVELENGTH
C     THE LARGER OF THE TWO ENTRANCE PUPIL DIAMETERS WILL ACT AS THE
C     DIMENSION FOR COMPUTING THE PUPIL GRID SPACING AND EXTENT. THIS
C     IS CONSISTENT WITH THE WAY THE GRID IS SET IN THE CAPFN RAYTRACE
C     GRID IN COMPAP.FOR. THESE ARE THE UNVIGNETTED ENTRANCE PUPIL
C     VALUES. VIGNETTING IS ACCOUNTED FOR IN THE PSF RAY TRACE.
C
      IF(SYSTEM(30).LE.2.0D0) THEN
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
C     AFOCAL
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
C
               IF(SYSTEM(30).LE.2.0D0) THEN
C     FOCAL
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
C     AFOCAL
      DIAM=EXEX
      IF(EXEY.GT.EXEX) DIAM=EXEY
      IF(DIAM.EQ.0.0D0) THEN
      OUTLYNE=
     1'EXIT PUPIL DIAMETER WAS ZERO, NO PSF CALCULATION POSSIBLE'
      CALL SHOWIT(1)
                   CALL MACFAL
               CALL DELPSF
                   RETURN
                   ELSE
C     MAKE THE SYSTEM HAVE A 100 UNIT FOCAL LENGTH
      DIAM=1.0/DIAM
                   END IF
                   END IF

      IF(GRIFLG.EQ.1)
     1OUTGRIDEXTENT=DIAM*(NNRD-1.0D0)*SHTVALUE
      IF(GRIFLG.EQ.0)
     1OUTGRIDEXTENT=DIAM*(DBLE(NRD)-1.0D0)*SHTVALUE
      OUTGRIDSPACING=OUTGRIDEXTENT/DBLE(TGR-1)
      GRI=OUTGRIDSPACING
C
C     NOW PROCESS THE CAPFN DATA INTO PSF DATA
C
      DEALLOCATE(F,FHOLDF,PUP,FIMG,STAT=ALLOERR)
      ALLOCATE(PUP(MM,MM,NDIM),STAT=ALLOERR)
      ALLOCATE(F(MMM+1,MMM+1),FHOLDF(MMM+1,MMM+1),FIMG(MMM+1,MMM+1)
     1,STAT=ALLOERR)
C
                        IWIW=IW**2
                        I=2
C
               III=MMM
               JJJ=MMM
      FHOLDF(1:III,1:JJJ)=0.0D0
                        DO J=1,NUMCOL
               III=MM
               JJJ=MM
               PUP(1:III,1:JJJ,1:2)=0.0D0
               III=MMM
               JJJ=MMM
      F(1:III,1:JJJ)=0.0D0
C     DOING A COLOR NOW
                        IX=0
                        IY=1
                        II=0

C     WE WANT TO DO IWIW READS AND LOADS OF THE A ARRAY
 10                     II=II+1
                        IX=IX+1
          IF(IX.GT.IW) THEN
                        IY=IY+1
                        IX=1
                        END IF
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
      ID=I-1
      CALL SPOTIT(4)
                        SPTT=(DSPOT(17))
                         I=I+1
C     LOAD AN ARRAY ELEMENT WITH REAL AND IMAGINARY CAPFN
C     FOR A COLOR
C     COL,ROW INDEXED AND ZERO PADDED, THAT IS WHY THE IIX AND IIY
C     ARE NEEDED

                       IIIX=((MM/2)-((NRD)/2))
                       IIIY=((MM/2)-((NRD)/2))
      IF(IIIX.LT.0) IIIX=0
      IF(IIIY.LT.0) IIIY=0
                       IIX=IX+IIIX
                       IIY=IY+IIIY
        IF(DSPOT(12).NE.0.0D0) THEN
                        END IF
C     REAL PART
      DSPOT(12)=DSPOT(12)*W3
        DSPOT(37)=DSPOT(12)*RAYRAY(9,NEWIMG)
      PUP(IIX,IIY,1)=
     1DSQRT(DABS(DSPOT(12)))*DCOS(DSPOT(4))
C     IMAGINARY PART
      PUP(IIX,IIY,2)=
     1-DSQRT(DABS(DSPOT(12)))*DSIN(DSPOT(4))
      WVNUM=DSPOT(16)
C
                   IF(II.LT.IWIW) GO TO 10
C     FELL THROUGH, FINISHED READING IWIW POINTS
C     NOW SET UP THE CALL TO THE FFT ROUTINE AND DO THE FFT
C
C     NOW DO THE PSF
C     CHANGE EVERY OTHER SIGN SO WE DON'T NEED TO INVERT THE ANSWER
                 DO III=1,MM
                 DO JJJ=1,MM
      IT=0
      IF(
     1(((INT(DBLE(III+JJJ)/2.0D0))*2)-(III+JJJ)).NE.0
     2) IT=1
      IF(IT.NE.0) PUP(III,JJJ,1)=-PUP(III,JJJ,1)
      IF(IT.NE.0) PUP(III,JJJ,2)=-PUP(III,JJJ,2)
                   END DO
                   END DO
C     NOW ORDER THE PUP ARRAY INTO THE DATA ARRAY
      DEALLOCATE(DATA,STAT=ALLOERR)
      ALLOCATE(DATA(NDAT),STAT=ALLOERR)
               DATA(1:NDAT)=0.0D0
      DO III = 1, MM
        DO JJJ = 1, MM
        INDEX = ( (III-1) * MM ) + JJJ
        IREAL = 2*INDEX-1
        IIMAG = 2*INDEX
      DATA(IREAL) = PUP(III,JJJ,1)
      DATA(IIMAG) = PUP(III,JJJ,2)
               END DO
               END DO
C     NOW DO THE FFT
      CALL FOURN(DATA,MM,NDAT)
C     NOW DO THE SIMPLE REORDER WITHOUT INVERSION
      DO  III = 1, MM
      DO  JJJ = 1, MM
      INDEX = (JJJ-1)*MM+III
      IREAL=INDEX*2-1
      IIMAG=INDEX*2
      PUP(III,JJJ,1)  = DATA(IREAL)
      PUP(III,JJJ,2)  = DATA(IIMAG)
               END DO
               END DO
      DEALLOCATE(DATA,STAT=ALLOERR)
        IF(SYSTEM(30).LE.2.0D0) THEN
C       FOCAL
      EXTENT=SNGL(OUTGRIDEXTENT)
      SPACING=SNGL(OUTGRIDSPACING)
                ELSE
C       AFOCAL
      EXTENT=SNGL(OUTGRIDEXTENT)
      SPACING=SNGL(OUTGRIDSPACING)
                END IF
      PEXTENT=EXTENT*REAL(PGR)/REAL(TGR-1)
      PSPACING=SPACING
C     FILL UP THE REAL INTENSITY PSF ARRAY F
                   DO III=2,MM
                   DO JJJ=2,MM
       F(III-1,JJJ-1)=
     2 ((PUP(III,JJJ,1)**2)+(PUP(III,JJJ,2)**2))
                   END DO
                   END DO
C
      DEALLOCATE(FHOLDF1,STAT=ALLOERR)
      ALLOCATE(FHOLDF1(MMM+1,MMM+1),STAT=ALLOERR)
                   DO JJJ=1,MMM
                   DO III=1,MMM
                   FHOLDF1(III,JJJ)=F(MMM+1-JJJ,III)
                   END DO
                   END DO
                   DO JJJ=1,MMM
                   DO III=1,MMM
                   F(III,JJJ)=FHOLDF1(III,JJJ)
                   END DO
                   END DO
      DEALLOCATE(FHOLDF1,STAT=ALLOERR)
C     REFLEXT IN Y
      ALLOCATE(FHOLDF1(MMM+1,MMM+1),STAT=ALLOERR)
                   DO JJJ=1,MMM
                   DO III=1,MMM
                   FHOLDF1(III,JJJ)=F(III,MMM+1-JJJ)
                   END DO
                   END DO
                   DO JJJ=1,MMM
                   DO III=1,MMM
                   F(III,JJJ)=FHOLDF1(III,JJJ)
                   END DO
                   END DO
      DEALLOCATE(FHOLDF1,STAT=ALLOERR)
C
C     IF WAVELENGTH IS SHORTEST, STORE THE PSF IN THE ARRAY
C     FHOLDF AFTER MULTIPLYING THE INTENSITY BY THE FRACTIONAL
C     SPECTRAL WEIGHT.
      IF(SHTNM.EQ.INT(WVNUM).AND.HI.EQ.0) THEN
      HI=HI+1
C     AT SHORT WAVELENGTH FIRST TIME
C
C     NOW SCALE THE PSF AT CURRENT LAMBDA TO A PEAK OF 1.0
      PEAKER=-1.0D300
               DO III=1,MMM
               DO JJJ=1,MMM
      IF(F(III,JJJ).GT.PEAKER) PEAKER=F(III,JJJ)
                       END DO
                       END DO
      GPREG(INT(WVNUM)+100)=PEAKER
      IF(PEAKER.EQ.0.0D0) PEAKER=1.0D0
C     MULT BY SPECTRAL WEIGHT AND STORE IN FHOLDF
               FACTER1=SPTT/SPTOT
               DO III=1,MMM
               DO JJJ=1,MMM
      FHOLDF(III,JJJ)=FACTER1*F(III,JJJ)
                       END DO
                       END DO
                       ELSE
C     NOT AT SHORTEST WAVELENGTH, INTERPOLATION NEEDED
C
C
C     NOW SCALE THE PSF AT CURRENT LAMBDA TO A PEAK OF 1.0
      PEAKER=-1.0D300
               DO III=1,MMM
               DO JJJ=1,MMM
      IF(F(III,JJJ).GT.PEAKER) PEAKER=F(III,JJJ)
                       END DO
                       END DO
      PEAKADJ=(PSFWV(INT(WVNUM))/SHRTWAVE)**2

      IF(PEAKER.EQ.0.0D0) PEAKER=1.0D0
      GPREG(INT(WVNUM)+100)=PEAKER*PEAKADJ
C
                   FACTER1=(SPTT/SPTOT)*PEAKADJ
C     NRD CHANGES IN COMPAP FOR PSF BUT NOT DOR CAPFN OR DOTF
C       NO PSF INTERPOLATION NECESSARY
C
C     NOW STORE/ADD WITH SPECTRAL WEIGHTING FACTORS INTO FHOLDF
               DO III=1,MMM
               DO JJJ=1,MMM
      FHOLDF(III,JJJ)=FHOLDF(III,JJJ)+(FACTER1*F(III,JJJ))
                       END DO
                       END DO
C
                      END IF
C     THE ARRAYS FOR A COLOR ARE WRITTEN, NOW SET UP THE X AND Y ARRAYS
                      END DO
C     NOW COPY BACK TO F
      DEALLOCATE(FIMG,STAT=ALLOERR)
      ALLOCATE(FIMG(MMM+1,MMM+1),STAT=ALLOERR)
        FIMG(1:MMM+1,1:MMM+1)=0.0D0
        PEAKER=-1.0D300
               DO III=1,MMM
               DO JJJ=1,MMM
      IF(FHOLDF(III,JJJ).GE.PEAKER) PEAKER=FHOLDF(III,JJJ)
      F(III,JJJ)=FHOLDF(III,JJJ)
                       END DO
                       END DO
        FIMG(1:MMM,1:MMM)=F(1:MMM,1:MMM)/PEAKER
        WRITE(OUTLYNE,*) 'PSF PEAK VALUE = ',PEAKER
        CALL SHOWIT(0)
        ! Store PSF data 
        if (allocated(curr_psf)) deallocate(curr_psf)
        allocate(curr_psf(MMM,MMM))
        curr_psf = FIMG

        ! New way
        allocate(currImg%img, mold=FIMG)
        currImg%img = FIMG
        currImg%N = MMM

        APEAK=PEAKER
        IF(SYSTEM(30).LE.2.0D0) THEN
      XCENTOFF=NINT(REFRY(1,INT(SYSTEM(20)))/GRI)
      YCENTOFF=NINT(REFRY(2,INT(SYSTEM(20)))/GRI)
                        ELSE
      V1=REFRY(4,INT(SYSTEM(20))/REFRY(6,INT(SYSTEM(20))))
      XCENTOFF=NINT(DATAN(V1)/GRI)
      V1=REFRY(5,INT(SYSTEM(20))/REFRY(6,INT(SYSTEM(20))))
      YCENTOFF=NINT(DATAN(V1)/GRI)
                        END IF
      WRITE(OUTLYNE,*)'CHIEF RAY X-OFFSET BY: ',XCENTOFF,' GRI UNITS'
        CALL SHOWIT(0)
      WRITE(OUTLYNE,*)'CHIEF RAY Y-OFFSET BY: ',YCENTOFF,' GRI UNITS'
        CALL SHOWIT(0)
      MMMIMG=MMM
      GRIIMG=GRI
      PGRIMG=PGR
      DEALLOCATE(FHOLDF,STAT=ALLOERR)
C
C     F IS THE FULL INTENSITY PSF FILE ON THE TGRxTGR GRID AFTER ALL
C     COLORS HAVE BEEN ADDED IN AND ON THE ORIGINAL GRI GRID SPACING
C     WHICH WAS SET BY THE VALUES OF NRD AND TGR
      CENTERVAL=F(((TGR/2)-1),((TGR/2)-1))
C
                       PEAKER=-1.0D300
                       DO JJJ=1,MMM
                       DO III=1,MMM
            IF(F(III,JJJ).GT.PEAKER) PEAKER=F(III,JJJ)
                       END DO
                       END DO
      SCALE_FACTOR=PEAKER
                       DO JJJ=1,MMM
                       DO III=1,MMM
      IF(PEAKER.NE.0.0D0) F(III,JJJ)=DNINT((F(III,JJJ)/PEAKER)
     1*32767.0D0)
      IF(PEAKER.EQ.0.0D0) F(III,JJJ)=DNINT(F(III,JJJ))
                       END DO
                       END DO
      IF(PSFLIN.EQ.0) THEN
                   DO III=1,MMM
                   DO JJJ=1,MMM
C     LOG REPRESENTATION
      IF(F(III,JJJ).LT.(10.0D0**(-PSFLOG)))
     1F(III,JJJ)=(10.0D0**(-PSFLOG))
      F(III,JJJ)=(DBLE(PSFLOG)+DLOG10(F(III,JJJ)))/DBLE(PSFLOG)
                   END DO
                   END DO
                   END IF

      DEALLOCATE(CENTERX,CENTERY,STAT=ALLOERR)
      ALLOCATE(CENTERX(PGR),CENTERY(PGR),STAT=ALLOERR)
                   CENTERX(1:PGR)=0.0D0
                   CENTERY(1:PGR)=0.0D0
C     CALCULATE THE TRUE PSF X-CENTROID POSITION
      PSFXCENT=0.0D0
      IIII=0.0D0
      IV=0.0D0
      FTOT=0.0D0
                   DO III=1,PGR
                   IIII=IIII+1.0D0
                   IV=IV+IIII
      FSUM=0.0D0
                   DO JJJ=1,PGR
                   FSUM=FSUM+
     1F((((TGR-1)-PGR)/2)+III,(((TGR-1)-PGR)/2)+JJJ)
                   END DO
                   FTOT=FTOT+FSUM
               CENTERX(III)=FSUM
      PSFXCENT=PSFXCENT+(IIII*FSUM)
                   END DO
      PSFXCENT=-((PSFXCENT/FTOT)-(IV/DBLE(PGR)))
C     CALCULATE THE TRUE PSF Y-CENTROID POSITION
      PSFYCENT=0.0D0
      IIII=0.0D0
      IV=0.0D0
      FTOT=0.0D0
                   DO JJJ=1,PGR
                   IIII=IIII+1.0D0
                   IV=IV+IIII
      FSUM=0.0D0
                   DO III=1,PGR
                   FSUM=FSUM+
     1F((((TGR-1)-PGR)/2)+III,(((TGR-1)-PGR)/2)+JJJ)
                   END DO
                   FTOT=FTOT+FSUM
               CENTERY(JJJ)=FSUM
      PSFYCENT=PSFYCENT+(IIII*FSUM)
                   END DO
      PSFYCENT=-((PSFYCENT/FTOT)-(IV/DBLE(PGR)))
C
        REG(40)=REG(9)
        REG(10)=0.0D0
        REG(9)=0.0D0
                   REG(10)=PSFYCENT*DBLE(SPACING)
                   REG(9)=PSFXCENT*DBLE(SPACING)
C
                   CNTY=SNGL(PSFYCENT)*SPACING
                   CNTX=SNGL(PSFXCENT)*SPACING
      IF(ABS(CNTY).LT.1.0E-8) CNTY=0.0
      IF(ABS(CNTX).LT.1.0E-8) CNTX=0.0
                   CRAYX=(REFRY(1,NEWIMG))
                   CRAYY=(REFRY(2,NEWIMG))
C
      PSFEXT=.TRUE.
      CPFNEXT=.TRUE.

                      RETURN
                      END
C SUB DOPUPIL.FOR

        SUBROUTINE DOPUPIL(KKK)
        USE GLOBALS
C
        IMPLICIT NONE
C
C     THIS IS SUBROUTINE DOPUPIL.FOR.
C     CALLED BY CMDER FOR COMMAND PUPIL IN FOE
C     THIS CREATES A PUPIL FUNCTION
C
        CHARACTER*9 UNN,UNN1
C
      REAL*8 VALUE,VALVAL,SHTVALUE,GRNX,GRNY,XRANGE,YRANGE
     1,WVNUM,DIAM,OUTGRIDEXTENT,EXEX,EXEY,V1,
     2OUTGRIDSPACING,FACTER,EFFER,SEFFER,CEFFER,CVALUE
     3,XPL1,YPL1,SPACER1,OLDGRI,NEWGRI
C
      INTEGER IT,IA,IB,IQ,IJK,WK,IER,PCOUNT,HOLN,KKK
C
      REAL*8 FAF,FAFF,FSUM,IIII,IV,PSFXCENT,PSFYCENT
     1,FACTER1,SPTT,SPTOT,FTOT
     2,SPACER,PEAKER,CRAYX,CRAYY,X2JK,Y2JK,F2JK,IWLIJK(1:10)
     3,C
C
      COMMON/PEPITO/IWLIJK,IJK
C
      REAL SPACING,EXTENT,CNTX,CNTY,PEXTENT,PSPACING
     1,PEAKADJ
C
      LOGICAL EXIS51,OPEN51,PSFERR,NOCOBSPSF
C
      COMMON/PSFCOBS/NOCOBSPSF
C
        LOGICAL ERR
C
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATSPD.INC'
        INCLUDE 'DATSP1.INC'
C
      INTEGER IREAL,IIMAG,INDEX,SHTNM,IX,IY,II,I,J,NDAT,NDIM,
     1MM,III,JJJ,IIX,IIY,ALLOERR,HI,MMM,IIIX,IIIY,DFLAG
C
      LOGICAL ERRR,ERRFOB
C
      COMMON/PSFPLOTER/PEXTENT,PSPACING,CNTX,CNTY
C
      DEALLOCATE(FOEGRID1,STAT=ALLOERR)
      GPREG(101:110)=0.0D0
C
      PSFWV(1)=SYSTEM(1)
      PSFWV(2)=SYSTEM(2)
      PSFWV(3)=SYSTEM(3)
      PSFWV(4)=SYSTEM(4)
      PSFWV(5)=SYSTEM(5)
      PSFWV(6)=SYSTEM(71)
      PSFWV(7)=SYSTEM(72)
      PSFWV(8)=SYSTEM(73)
      PSFWV(9)=SYSTEM(74)
      PSFWV(10)=SYSTEM(75)
C
C     M IS THE DIMENSION OF THE REQUESTED GRID OVER THE PUPIL
C     THE TRANSFORM IS TWICE THIS BIG
      HI=0
      MM=TGR
      MMM=MM-1
      NDAT=MM*MM*2
      NDIM=2
C
C     HERE IS WHERE WE BUILD THE PSF
               IF(S1.EQ.1) DFLAG=1
               IF(S1.EQ.0) DFLAG=0
C
      IF(.NOT.CPFNEXT) THEN
               OUTLYNE='NO COMPLEX APERTURE FUNCTION EXISTS'
               CALL SHOWIT(1)
               OUTLYNE='NO PSF CALCULATION IS POSSIBLE'
               CALL SHOWIT(1)
               CALL MACFAL
               CALL DELPSF
               RETURN
               END IF
C     CALC SPTOT
                   SPTOT=0.0D0
          SPTOT=SPTOT+(SYSTEM(31))
          SPTOT=SPTOT+(SYSTEM(32))
          SPTOT=SPTOT+(SYSTEM(33))
          SPTOT=SPTOT+(SYSTEM(34))
          SPTOT=SPTOT+(SYSTEM(35))
          SPTOT=SPTOT+(SYSTEM(76))
          SPTOT=SPTOT+(SYSTEM(77))
          SPTOT=SPTOT+(SYSTEM(78))
          SPTOT=SPTOT+(SYSTEM(79))
          SPTOT=SPTOT+(SYSTEM(80))
C
C     NOW COMPUTE THE SHORTEST WAVELENGTH IN CURENT LENS UNITS
C     CALC THE SHRTWAVE
        SHRTWAVE=0.0D0
      IF(SYSTEM(31).NE.0.0D0.AND.SYSTEM(1).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(1)
                      GO TO 314
                      END IF
      IF(SYSTEM(32).NE.0.0D0.AND.SYSTEM(2).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(2)
                      GO TO 314
                      END IF
      IF(SYSTEM(33).NE.0.0D0.AND.SYSTEM(3).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(3)
                      GO TO 314
                      END IF
      IF(SYSTEM(34).NE.0.0D0.AND.SYSTEM(4).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(4)
                      GO TO 314
                      END IF
      IF(SYSTEM(35).NE.0.0D0.AND.SYSTEM(5).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(5)
                      GO TO 314
                      END IF
      IF(SYSTEM(76).NE.0.0D0.AND.SYSTEM(71).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(71)
                      GO TO 314
                      END IF
      IF(SYSTEM(77).NE.0.0D0.AND.SYSTEM(72).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(72)
                      GO TO 314
                      END IF
      IF(SYSTEM(78).NE.0.0D0.AND.SYSTEM(73).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(73)
                      GO TO 314
                      END IF
      IF(SYSTEM(79).NE.0.0D0.AND.SYSTEM(74).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(74)
                      GO TO 314
                      END IF
      IF(SYSTEM(80).NE.0.0D0.AND.SYSTEM(75).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(75)
                      GO TO 314
                      END IF
 314                  CONTINUE
      IF(SHRTWAVE.EQ.0.0D0) THEN
        OUTLYNE='WAVELENGTHS ARE ALL ZERO'
      CALL SHOWIT(1)
        OUTLYNE='NO "PSF" CAN BE CALCULATED'
      CALL SHOWIT(1)
                        CALL MACFAL
               CALL DELPSF
                        RETURN
                        END IF
C
      IF(SYSTEM(31).NE.0.0D0.AND.SYSTEM(1).LE.SHRTWAVE
     1.AND.SYSTEM(1).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(1)
      SHTNM=1
      END IF
      IF(SYSTEM(32).NE.0.0D0.AND.SYSTEM(2).LE.SHRTWAVE
     1.AND.SYSTEM(2).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(2)
      SHTNM=2
      END IF
      IF(SYSTEM(33).NE.0.0D0.AND.SYSTEM(3).LE.SHRTWAVE
     1.AND.SYSTEM(3).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(3)
      SHTNM=3
      END IF
      IF(SYSTEM(34).NE.0.0D0.AND.SYSTEM(4).LE.SHRTWAVE
     1.AND.SYSTEM(4).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(4)
      SHTNM=4
      END IF
      IF(SYSTEM(35).NE.0.0D0.AND.SYSTEM(5).LE.SHRTWAVE
     1.AND.SYSTEM(5).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(5)
      SHTNM=5
      END IF
      IF(SYSTEM(76).NE.0.0D0.AND.SYSTEM(71).LE.SHRTWAVE
     1.AND.SYSTEM(71).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(71)
      SHTNM=6
      END IF
      IF(SYSTEM(77).NE.0.0D0.AND.SYSTEM(72).LE.SHRTWAVE
     1.AND.SYSTEM(72).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(72)
      SHTNM=7
      END IF
      IF(SYSTEM(78).NE.0.0D0.AND.SYSTEM(73).LE.SHRTWAVE
     1.AND.SYSTEM(73).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(73)
      SHTNM=8
      END IF
      IF(SYSTEM(79).NE.0.0D0.AND.SYSTEM(74).LE.SHRTWAVE
     1.AND.SYSTEM(74).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(74)
      SHTNM=9
      END IF
      IF(SYSTEM(80).NE.0.0D0.AND.SYSTEM(75).LE.SHRTWAVE
     1.AND.SYSTEM(75).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(75)
      SHTNM=10
      END IF
C     SHRTWAVE IS IN MICRONS, CHANGE TO LENS UNITS
      VALUE=SHRTWAVE
      IF(SYSTEM(6).EQ.1.0D0) SHTVALUE=((VALUE*1.0D-3)/25.4D0)
      IF(SYSTEM(6).EQ.2.0D0) SHTVALUE=(VALUE*1.0D-4)
      IF(SYSTEM(6).EQ.3.0D0) SHTVALUE=(VALUE*1.0D-3)
      IF(SYSTEM(6).EQ.4.0D0) SHTVALUE=(VALUE*1.0D-6)
C
C     WE NOW HAVE THE WAVELENGTH AND WAVELENGTH NUMBER OF THE
C     SHORTEST WAVELENGTH FOR WHICH THE SPECTRAL WEIGHTING FACTOR
C     IS NON-ZERO. THESE ARE SHTVALUE AND SHTNM
C
C     TGR ALWAYS RULES AND IS NOT CHANGED.
C
C     NOW COMPUTE THE PUPIL GRID SPACING AND EXTENT
C     THIS SETS THE GRID TO BE USED FOR THE PSF
C     IF THE MODE IS FOCAL OR UFOCAL
C     WE WANT THE ENTRANCE PUPIL DIAMETERS IN THE X AND Y DIRECTIONS
C     ENTRANCE PUPIL DIAMETERS IN THE X AND Y DIRECTION AT THE SHORT WAVELENGTH
C     THE LARGER OF THE TWO ENTRANCE PUPIL DIAMETERS WILL ACT AS THE
C     DIMENSION FOR COMPUTING THE PUPIL GRID SPACING AND EXTENT. THIS
C     IS CONSISTENT WITH THE WAY THE GRID IS SET IN THE CAPFN RAYTRACE
C     GRID IN COMPAP.FOR. THESE ARE THE UNVIGNETTED ENTRANCE PUPIL
C     VALUES. VIGNETTING IS ACCOUNTED FOR IN THE PSF RAY TRACE.
C
      IF(SYSTEM(30).LE.2.0D0) THEN
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
C     AFOCAL
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
C
               IF(SYSTEM(30).LE.2.0D0) THEN
C     FOCAL
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
C     AFOCAL
      DIAM=EXEX
      IF(EXEY.GT.EXEX) DIAM=EXEY
      IF(DIAM.EQ.0.0D0) THEN
      OUTLYNE=
     1'EXIT PUPIL DIAMETER WAS ZERO, NO PSF CALCULATION POSSIBLE'
      CALL SHOWIT(1)
                   CALL MACFAL
               CALL DELPSF
                   RETURN
                   ELSE
C     MAKE THE SYSTEM HAVE A 100 UNIT FOCAL LENGTH
      DIAM=1.0/DIAM
                   END IF
                   END IF

      IF(GRIFLG.EQ.1)
     1OUTGRIDEXTENT=DIAM*(NNRD-1.0D0)*SHTVALUE
      IF(GRIFLG.EQ.0)
     1OUTGRIDEXTENT=DIAM*(DBLE(NRD)-1.0D0)*SHTVALUE
      OUTGRIDSPACING=OUTGRIDEXTENT/DBLE(TGR-1)
      GRI=OUTGRIDSPACING
C
C     NOW PROCESS THE CAPFN DATA INTO PSF DATA
C
      DEALLOCATE(FOEGRID1,STAT=ALLOERR)
      ALLOCATE(FOEGRID1(MM,MM,1:10,1:2),STAT=ALLOERR)
C
                        IWIW=IW**2
                        I=2
C
               III=MM
               JJJ=MM
               FOEGRID1(1:MM,1:MM,1:10,1:2)=0.0D0
                        DO J=1,NUMCOL
C     DOING A COLOR NOW
                        IX=0
                        IY=1
                        II=0

C     WE WANT TO DO IWIW READS AND LOADS OF THE A ARRAY
 10                     II=II+1
                        IX=IX+1
          IF(IX.GT.IW) THEN
                        IY=IY+1
                        IX=1
                        END IF
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
      ID=I-1
      CALL SPOTIT(4)
                        SPTT=(DSPOT(17))
                         I=I+1
C     LOAD AN ARRAY ELEMENT WITH REAL AND IMAGINARY CAPFN
C     FOR A COLOR
C     COL,ROW INDEXED AND ZERO PADDED, THAT IS WHY THE IIX AND IIY
C     ARE NEEDED

                       IIIX=((MM/2)-((NRD)/2))
                       IIIY=((MM/2)-((NRD)/2))
      IF(IIIX.LT.0) IIIX=0
      IF(IIIY.LT.0) IIIY=0
                       IIX=IX+IIIX
                       IIY=IY+IIIY
        IF(DSPOT(12).NE.0.0D0) THEN
                        END IF
        IF(KKK.EQ.1) THEN
C       SAVE AIB
C     REAL PART
      DSPOT(12)=DSPOT(12)*W3
        DSPOT(37)=DSPOT(12)*RAYRAY(9,NEWIMG)
      FOEGRID1(IIX,IIY,J,1)=
     1DSQRT(DABS(DSPOT(12)))*DCOS(DSPOT(4))
C     IMAGINARY PART
      FOEGRID1(IIX,IIY,J,2)=
     1-DSQRT(DABS(DSPOT(12)))*DSIN(DSPOT(4))
      WVNUM=DSPOT(16)
                        ELSE
C       SAVE AS REITHETA
C     MODULUS
      DSPOT(12)=DSPOT(12)*W3
        DSPOT(37)=DSPOT(12)*RAYRAY(9,NEWIMG)
      FOEGRID1(IIX,IIY,J,1)=DSQRT(DABS(DSPOT(12)))
C     PHASE IN WAVES
      FOEGRID1(IIX,IIY,J,2)=DSPOT(4)
                        END IF
C
                   IF(II.LT.IWIW) GO TO 10
C     FELL THROUGH, FINISHED READING IWIW POINTS
        IF(SYSTEM(30).LE.2.0D0) THEN
C       FOCAL
      EXTENT=SNGL(OUTGRIDEXTENT)
      SPACING=SNGL(OUTGRIDSPACING)
                ELSE
C       AFOCAL
      EXTENT=SNGL(OUTGRIDEXTENT)
      SPACING=SNGL(OUTGRIDSPACING)
                END IF
      PEXTENT=EXTENT*REAL(PGR)/REAL(TGR-1)
      PSPACING=SPACING
C     THE ARRAYS FOR A COLOR ARE WRITTEN, NOW SET UP THE X AND Y ARRAYS
                      END DO
                      RETURN
                      END
C SUB NRDCALC.FOR

        SUBROUTINE NRDCALC(ERRORR)
C
        IMPLICIT NONE
C
C     THIS CALCULATED A NON-INTEGER NRD WHEN GRI IS SPECIFIED
C
      REAL*8 SHTVALUE,VALUE,FNX,FNY,FNN,VALVAL
     1,TNRD
C
      LOGICAL ERRORR,ERRR,ERR
C
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATSPD.INC'
        INCLUDE 'DATSP1.INC'
C
      INTEGER SHTNM
C
C     NOW COMPUTE THE SHORTEST WAVELENGTH IN CURENT LENS UNITS
C     CALC THE SHRTWAVE
        SHRTWAVE=0.0D0
      IF(SYSTEM(31).NE.0.0D0.AND.SYSTEM(1).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(1)
                      GO TO 314
                      END IF
      IF(SYSTEM(32).NE.0.0D0.AND.SYSTEM(2).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(2)
                      GO TO 314
                      END IF
      IF(SYSTEM(33).NE.0.0D0.AND.SYSTEM(3).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(3)
                      GO TO 314
                      END IF
      IF(SYSTEM(34).NE.0.0D0.AND.SYSTEM(4).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(4)
                      GO TO 314
                      END IF
      IF(SYSTEM(35).NE.0.0D0.AND.SYSTEM(5).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(5)
                      GO TO 314
                      END IF
      IF(SYSTEM(76).NE.0.0D0.AND.SYSTEM(71).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(71)
                      GO TO 314
                      END IF
      IF(SYSTEM(77).NE.0.0D0.AND.SYSTEM(72).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(72)
                      GO TO 314
                      END IF
      IF(SYSTEM(78).NE.0.0D0.AND.SYSTEM(73).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(73)
                      GO TO 314
                      END IF
      IF(SYSTEM(79).NE.0.0D0.AND.SYSTEM(74).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(74)
                      GO TO 314
                      END IF
      IF(SYSTEM(80).NE.0.0D0.AND.SYSTEM(75).NE.0.0D0) THEN
                      SHRTWAVE=SYSTEM(75)
                      GO TO 314
                      END IF
 314                  CONTINUE
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
C
      IF(SYSTEM(31).NE.0.0D0.AND.SYSTEM(1).LE.SHRTWAVE
     1.AND.SYSTEM(1).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(1)
      SHTNM=1
      END IF
      IF(SYSTEM(32).NE.0.0D0.AND.SYSTEM(2).LE.SHRTWAVE
     1.AND.SYSTEM(2).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(2)
      SHTNM=2
      END IF
      IF(SYSTEM(33).NE.0.0D0.AND.SYSTEM(3).LE.SHRTWAVE
     1.AND.SYSTEM(3).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(3)
      SHTNM=3
      END IF
      IF(SYSTEM(34).NE.0.0D0.AND.SYSTEM(4).LE.SHRTWAVE
     1.AND.SYSTEM(4).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(4)
      SHTNM=4
      END IF
      IF(SYSTEM(35).NE.0.0D0.AND.SYSTEM(5).LE.SHRTWAVE
     1.AND.SYSTEM(5).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(5)
      SHTNM=5
      END IF
      IF(SYSTEM(76).NE.0.0D0.AND.SYSTEM(71).LE.SHRTWAVE
     1.AND.SYSTEM(71).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(71)
      SHTNM=6
      END IF
      IF(SYSTEM(77).NE.0.0D0.AND.SYSTEM(72).LE.SHRTWAVE
     1.AND.SYSTEM(72).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(72)
      SHTNM=7
      END IF
      IF(SYSTEM(78).NE.0.0D0.AND.SYSTEM(73).LE.SHRTWAVE
     1.AND.SYSTEM(73).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(73)
      SHTNM=8
      END IF
      IF(SYSTEM(79).NE.0.0D0.AND.SYSTEM(74).LE.SHRTWAVE
     1.AND.SYSTEM(74).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(74)
      SHTNM=9
      END IF
      IF(SYSTEM(80).NE.0.0D0.AND.SYSTEM(75).LE.SHRTWAVE
     1.AND.SYSTEM(75).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(75)
      SHTNM=10
      END IF
C     SHRTWAVE IS IN MICRONS, CHANGE TO LENS UNITS
      VALUE=SHRTWAVE
      IF(SYSTEM(6).EQ.1.0D0) SHTVALUE=((VALUE*1.0D-3)/25.4D0)
      IF(SYSTEM(6).EQ.2.0D0) SHTVALUE=(VALUE*1.0D-4)
      IF(SYSTEM(6).EQ.3.0D0) SHTVALUE=(VALUE*1.0D-3)
      IF(SYSTEM(6).EQ.4.0D0) SHTVALUE=(VALUE*1.0D-6)
C
C     WE NOW HAVE THE WAVELENGTH AND WAVELENGTH NUMBER OF THE
C     SHORTEST WAVELENGTH FOR WHICH THE SPECTRAL WEIGHTING FACTOR
C     IS NON-ZERO. THESE ARE SHTVALUE AND SHTNM
C
C     TGR ALWAYS RULES AND IS NOT CHANGED.
C
C     NOW COMPUTE THE NRD
C
      IF(SYSTEM(30).LE.2.0D0) THEN
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
C     AFOCAL
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
C
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
