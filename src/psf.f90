module mod_psf
    implicit none

    contains



! SUB DOPSF.FOR
    ! See VIE_new, where it takes plot setting manager (psm) as input
    ! and takes the settings as needed
    ! put output in curr_psf for retrieval?

    SUBROUTINE DOPSF()
        USE GLOBALS
        use global_widgets, only: curr_psf
        use data_registers
        use type_utils
!
!
!     THIS IS SUBROUTINE DOPSF.FOR.
!     CALLED BY CMDER FOR COMMAND PSF
!     THIS DOES DIFFRACTION PSF CALCULATIONS
!
        CHARACTER*9 UNN,UNN1
!
      REAL*8 VALUE,VALVAL,SHTVALUE,GRNX,GRNY,XRANGE,YRANGE &
      ,WVNUM,DIAM,OUTGRIDEXTENT,EXEX,EXEY,V1,APEAK, &
      OUTGRIDSPACING,FACTER,EFFER,SEFFER,CEFFER,CVALUE &
      ,XPL1,YPL1,CENTERX,CENTERY,SPACER1,OLDGRI,NEWGRI
!
      INTEGER IT,IA,IB,IQ,IJK,WK,IER,PCOUNT,HOLN
!
      INTEGER BINNREC,UNIT_FLAG,MODE_FLAG,IREC
!
      REAL*8 SCALE_FACTOR
!
      REAL*8 FAF,FAFF,FSUM,IIII,IV,PSFXCENT,PSFYCENT &
      ,FACTER1,SPTT,SPTOT,FTOT,PUP,DATA &
      ,SPACER,PEAKER,CRAYX,CRAYY,X2JK,Y2JK,F2JK,IWLIJK(1:10) &
      ,C,F,FHOLDF,FHOLDF1
!
      COMMON/PEPITO/IWLIJK,IJK
!
      REAL SPACING,EXTENT,XPLT,CNTX,CNTY,PEXTENT,PSPACING &
      ,YPLT,FPLT,PEAKADJ
      DIMENSION XPLT(:),YPLT(:),FPLT(:,:)
      ALLOCATABLE :: XPLT,YPLT,FPLT
!
      DIMENSION F(:,:),DATA(:),PUP(:,:,:) &
      ,FHOLDF(:,:),FHOLDF1(:,:) &
      ,CENTERX(:),CENTERY(:)
      ALLOCATABLE :: F,DATA,PUP,FHOLDF,FHOLDF1 &
      ,CENTERX,CENTERY
!
      LOGICAL EXIS51,OPEN51,PSFERR,NOCOBSPSF
!
      LOGICAL EXIS106,OPEN106
!
      COMMON/PSFCOBS/NOCOBSPSF
!
        LOGICAL ERR
!
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATSPD.INC'
        INCLUDE 'DATSP1.INC'

!
      INTEGER IREAL,IIMAG,INDEX,SHTNM,IX,IY,II,I,J,NDAT,NDIM, &
      MM,III,JJJ,IIX,IIY,ALLOERR,HI,MMM,IIIX,IIIY,DFLAG
!
      LOGICAL ERRR,ERRFOB

      real(long) :: fNum, maxNA
!
      COMMON/PSFPLOTER/PEXTENT,PSPACING,CNTX,CNTY
!
      call LogTermDebug("doPSF beginning NRD is "//int2str(NRD))
      print *, "NRD at beginning of DOPSF is ", INT(NRD)

      DEALLOCATE(F,PUP,FHOLDF,FHOLDF1,FIMG,STAT=ALLOERR)
      GPREG(101:110)=0.0D0
!
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
!
!     NOW COMPUTE THE SHORTEST WAVELENGTH IN CURENT LENS UNITS
!     CALC THE SHRTWAVE
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
!
      IF(SYSTEM(31).NE.0.0D0.AND.SYSTEM(1).LE.SHRTWAVE &
      .AND.SYSTEM(1).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(1)
      SHTNM=1
      END IF
      IF(SYSTEM(32).NE.0.0D0.AND.SYSTEM(2).LE.SHRTWAVE &
      .AND.SYSTEM(2).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(2)
      SHTNM=2
      END IF
      IF(SYSTEM(33).NE.0.0D0.AND.SYSTEM(3).LE.SHRTWAVE &
      .AND.SYSTEM(3).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(3)
      SHTNM=3
      END IF
      IF(SYSTEM(34).NE.0.0D0.AND.SYSTEM(4).LE.SHRTWAVE &
      .AND.SYSTEM(4).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(4)
      SHTNM=4
      END IF
      IF(SYSTEM(35).NE.0.0D0.AND.SYSTEM(5).LE.SHRTWAVE &
      .AND.SYSTEM(5).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(5)
      SHTNM=5
      END IF
      IF(SYSTEM(76).NE.0.0D0.AND.SYSTEM(71).LE.SHRTWAVE &
      .AND.SYSTEM(71).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(71)
      SHTNM=6
      END IF
      IF(SYSTEM(77).NE.0.0D0.AND.SYSTEM(72).LE.SHRTWAVE &
      .AND.SYSTEM(72).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(72)
      SHTNM=7
      END IF
      IF(SYSTEM(78).NE.0.0D0.AND.SYSTEM(73).LE.SHRTWAVE &
      .AND.SYSTEM(73).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(73)
      SHTNM=8
      END IF
      IF(SYSTEM(79).NE.0.0D0.AND.SYSTEM(74).LE.SHRTWAVE &
      .AND.SYSTEM(74).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(74)
      SHTNM=9
      END IF
      IF(SYSTEM(80).NE.0.0D0.AND.SYSTEM(75).LE.SHRTWAVE &
      .AND.SYSTEM(75).NE.0.0D0) THEN
      SHRTWAVE=SYSTEM(75)
      SHTNM=10
      END IF
!     SHRTWAVE IS IN MICRONS, CHANGE TO LENS UNITS
      VALUE=SHRTWAVE
      IF(SYSTEM(6).EQ.1.0D0) SHTVALUE=((VALUE*1.0D-3)/25.4D0)
      IF(SYSTEM(6).EQ.2.0D0) SHTVALUE=(VALUE*1.0D-4)
      IF(SYSTEM(6).EQ.3.0D0) SHTVALUE=(VALUE*1.0D-3)
      IF(SYSTEM(6).EQ.4.0D0) SHTVALUE=(VALUE*1.0D-6)
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
               IF(SYSTEM(30).LE.2.0D0) THEN
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
      OUTLYNE= &
      'EXIT PUPIL DIAMETER WAS ZERO, NO PSF CALCULATION POSSIBLE'
      CALL SHOWIT(1)
                   CALL MACFAL
               CALL DELPSF
                   RETURN
                   ELSE
!     MAKE THE SYSTEM HAVE A 100 UNIT FOCAL LENGTH
      DIAM=1.0/DIAM
                   END IF
                   END IF

      IF(GRIFLG.EQ.1) &
      OUTGRIDEXTENT=DIAM*(NNRD-1.0D0)*SHTVALUE
      IF(GRIFLG.EQ.0) &
      OUTGRIDEXTENT=DIAM*(DBLE(NRD)-1.0D0)*SHTVALUE
      OUTGRIDSPACING=OUTGRIDEXTENT/DBLE(TGR-1)
      GRI=OUTGRIDSPACING
!
!     NOW PROCESS THE CAPFN DATA INTO PSF DATA
!
      DEALLOCATE(F,FHOLDF,PUP,FIMG,STAT=ALLOERR)
      ALLOCATE(PUP(MM,MM,NDIM),STAT=ALLOERR)
      ALLOCATE(F(MMM+1,MMM+1),FHOLDF(MMM+1,MMM+1),FIMG(MMM+1,MMM+1) &
      ,STAT=ALLOERR)
!
                        IWIW=IW**2
                        I=2
!
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
!     DOING A COLOR NOW
                        IX=0
                        IY=1
                        II=0

!     WE WANT TO DO IWIW READS AND LOADS OF THE A ARRAY
 10                     II=II+1
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
!     REAL PART
      DSPOT(12)=DSPOT(12)*W3
        DSPOT(37)=DSPOT(12)*RAYRAY(9,NEWIMG)
      PUP(IIX,IIY,1)= &
      DSQRT(DABS(DSPOT(12)))*DCOS(DSPOT(4))
!     IMAGINARY PART
      PUP(IIX,IIY,2)= &
      -DSQRT(DABS(DSPOT(12)))*DSIN(DSPOT(4))
      WVNUM=DSPOT(16)
!
                   IF(II.LT.IWIW) GO TO 10
!     FELL THROUGH, FINISHED READING IWIW POINTS
!     NOW SET UP THE CALL TO THE FFT ROUTINE AND DO THE FFT
!
!     NOW DO THE PSF
!     CHANGE EVERY OTHER SIGN SO WE DON'T NEED TO INVERT THE ANSWER
                 DO III=1,MM
                 DO JJJ=1,MM
      IT=0
      IF( &
      (((INT(DBLE(III+JJJ)/2.0D0))*2)-(III+JJJ)).NE.0 &
      ) IT=1
      IF(IT.NE.0) PUP(III,JJJ,1)=-PUP(III,JJJ,1)
      IF(IT.NE.0) PUP(III,JJJ,2)=-PUP(III,JJJ,2)
                   END DO
                   END DO
!     NOW ORDER THE PUP ARRAY INTO THE DATA ARRAY
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
!     NOW DO THE FFT
      CALL FOURN(DATA,MM,NDAT)
!     NOW DO THE SIMPLE REORDER WITHOUT INVERSION
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
!     FILL UP THE REAL INTENSITY PSF ARRAY F
                   DO III=2,MM
                   DO JJJ=2,MM
       F(III-1,JJJ-1)= &
       ((PUP(III,JJJ,1)**2)+(PUP(III,JJJ,2)**2))
                   END DO
                   END DO
!
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
!     REFLEXT IN Y
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
!
!     IF WAVELENGTH IS SHORTEST, STORE THE PSF IN THE ARRAY
!     FHOLDF AFTER MULTIPLYING THE INTENSITY BY THE FRACTIONAL
!     SPECTRAL WEIGHT.
      IF(SHTNM.EQ.INT(WVNUM).AND.HI.EQ.0) THEN
      HI=HI+1
!     AT SHORT WAVELENGTH FIRST TIME
!
!     NOW SCALE THE PSF AT CURRENT LAMBDA TO A PEAK OF 1.0
      PEAKER=-1.0D300
               DO III=1,MMM
               DO JJJ=1,MMM
      IF(F(III,JJJ).GT.PEAKER) PEAKER=F(III,JJJ)
                       END DO
                       END DO
      GPREG(INT(WVNUM)+100)=PEAKER
      IF(PEAKER.EQ.0.0D0) PEAKER=1.0D0
!     MULT BY SPECTRAL WEIGHT AND STORE IN FHOLDF
               FACTER1=SPTT/SPTOT
               DO III=1,MMM
               DO JJJ=1,MMM
      FHOLDF(III,JJJ)=FACTER1*F(III,JJJ)
                       END DO
                       END DO
                       ELSE
!     NOT AT SHORTEST WAVELENGTH, INTERPOLATION NEEDED
!
!
!     NOW SCALE THE PSF AT CURRENT LAMBDA TO A PEAK OF 1.0
      PEAKER=-1.0D300
               DO III=1,MMM
               DO JJJ=1,MMM
      IF(F(III,JJJ).GT.PEAKER) PEAKER=F(III,JJJ)
                       END DO
                       END DO
      PEAKADJ=(PSFWV(INT(WVNUM))/SHRTWAVE)**2

      IF(PEAKER.EQ.0.0D0) PEAKER=1.0D0
      GPREG(INT(WVNUM)+100)=PEAKER*PEAKADJ
!
                   FACTER1=(SPTT/SPTOT)*PEAKADJ
!     NRD CHANGES IN COMPAP FOR PSF BUT NOT DOR CAPFN OR DOTF
!       NO PSF INTERPOLATION NECESSARY
!
!     NOW STORE/ADD WITH SPECTRAL WEIGHTING FACTORS INTO FHOLDF
               DO III=1,MMM
               DO JJJ=1,MMM
      FHOLDF(III,JJJ)=FHOLDF(III,JJJ)+(FACTER1*F(III,JJJ))
                       END DO
                       END DO
!
                      END IF
!     THE ARRAYS FOR A COLOR ARE WRITTEN, NOW SET UP THE X AND Y ARRAYS
                      END DO
!     NOW COPY BACK TO F
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
        if(ALLOCATED(currImg%img)) deallocate(currImg%img)
        allocate(currImg%img, mold=FIMG)
        currImg%img = FIMG
        currImg%N   = MMM+1     
        ! Temp data
        fnum = 0.7029729
        maxNA = 1/(2*fnum)
        currImg%pS = 2.0*0.248/(maxNA*REAL(TGR)/REAL(16)) 
        print *, "********************************!!!!!!!!!!*************************************"
        print *, "TGR IS ", TGR
        print *, "NRD IS ", INT(NRD)
        print *, "pixel size psf90 is ", currImg%pS
        print *, "TRG/NRD IS ", REAL(TGR)/REAL(NRD)
  

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
!
!     F IS THE FULL INTENSITY PSF FILE ON THE TGRxTGR GRID AFTER ALL
!     COLORS HAVE BEEN ADDED IN AND ON THE ORIGINAL GRI GRID SPACING
!     WHICH WAS SET BY THE VALUES OF NRD AND TGR
      CENTERVAL=F(((TGR/2)-1),((TGR/2)-1))
!
                       PEAKER=-1.0D300
                       DO JJJ=1,MMM
                       DO III=1,MMM
            IF(F(III,JJJ).GT.PEAKER) PEAKER=F(III,JJJ)
                       END DO
                       END DO
      SCALE_FACTOR=PEAKER
                       DO JJJ=1,MMM
                       DO III=1,MMM
      IF(PEAKER.NE.0.0D0) F(III,JJJ)=DNINT((F(III,JJJ)/PEAKER) &
      *32767.0D0)
      IF(PEAKER.EQ.0.0D0) F(III,JJJ)=DNINT(F(III,JJJ))
                       END DO
                       END DO
      IF(PSFLIN.EQ.0) THEN
                   DO III=1,MMM
                   DO JJJ=1,MMM
!     LOG REPRESENTATION
      IF(F(III,JJJ).LT.(10.0D0**(-PSFLOG))) &
      F(III,JJJ)=(10.0D0**(-PSFLOG))
      F(III,JJJ)=(DBLE(PSFLOG)+DLOG10(F(III,JJJ)))/DBLE(PSFLOG)
                   END DO
                   END DO
                   END IF

      DEALLOCATE(CENTERX,CENTERY,STAT=ALLOERR)
      ALLOCATE(CENTERX(PGR),CENTERY(PGR),STAT=ALLOERR)
                   CENTERX(1:PGR)=0.0D0
                   CENTERY(1:PGR)=0.0D0
!     CALCULATE THE TRUE PSF X-CENTROID POSITION
      PSFXCENT=0.0D0
      IIII=0.0D0
      IV=0.0D0
      FTOT=0.0D0
                   DO III=1,PGR
                   IIII=IIII+1.0D0
                   IV=IV+IIII
      FSUM=0.0D0
                   DO JJJ=1,PGR
                   FSUM=FSUM+ &
      F((((TGR-1)-PGR)/2)+III,(((TGR-1)-PGR)/2)+JJJ)
                   END DO
                   FTOT=FTOT+FSUM
               CENTERX(III)=FSUM
      PSFXCENT=PSFXCENT+(IIII*FSUM)
                   END DO
      PSFXCENT=-((PSFXCENT/FTOT)-(IV/DBLE(PGR)))
!     CALCULATE THE TRUE PSF Y-CENTROID POSITION
      PSFYCENT=0.0D0
      IIII=0.0D0
      IV=0.0D0
      FTOT=0.0D0
                   DO JJJ=1,PGR
                   IIII=IIII+1.0D0
                   IV=IV+IIII
      FSUM=0.0D0
                   DO III=1,PGR
                   FSUM=FSUM+ &
      F((((TGR-1)-PGR)/2)+III,(((TGR-1)-PGR)/2)+JJJ)
                   END DO
                   FTOT=FTOT+FSUM
               CENTERY(JJJ)=FSUM
      PSFYCENT=PSFYCENT+(IIII*FSUM)
                   END DO
      PSFYCENT=-((PSFYCENT/FTOT)-(IV/DBLE(PGR)))
!
        REG(40)=REG(9)
        REG(10)=0.0D0
        REG(9)=0.0D0
                   REG(10)=PSFYCENT*DBLE(SPACING)
                   REG(9)=PSFXCENT*DBLE(SPACING)
!
                   CNTY=SNGL(PSFYCENT)*SPACING
                   CNTX=SNGL(PSFXCENT)*SPACING
      IF(ABS(CNTY).LT.1.0E-8) CNTY=0.0
      IF(ABS(CNTX).LT.1.0E-8) CNTX=0.0
                   CRAYX=(REFRY(1,NEWIMG))
                   CRAYY=(REFRY(2,NEWIMG))
!
      PSFEXT=.TRUE.
      CPFNEXT=.TRUE.

!
                      RETURN
                      END

            !           C SUB PSFPLT.FOR
            !           SUBROUTINE PSFPLT
            !   C
            !           IMPLICIT NONE
            !   C
            !   C       THIS IS SUBROUTINE PSFPLT.FOR. THIS SUBROUTINE CONTROLS
            !   C     PSF PLOT OUTPUT
            !   C
            !           INCLUDE 'DATMAI.INC'
            !           INCLUDE 'DATLEN.INC'
            !           INCLUDE 'DATSPD.INC'
            !   C
            !   C       CHECK FOR STRING INPUT
            !           IF(SST.EQ.1.OR.SN.EQ.1) THEN
            !           OUTLYNE='"PSFPLOT" TAKES NO STRING OR NUMERIC INPUT'
            !         CALL SHOWIT(1)
            !           OUTLYNE='RE-ENTER COMMAND'
            !         CALL SHOWIT(1)
            !                           CALL MACFAL
            !                           RETURN
            !                           END IF
            !           IF(STI.EQ.0) THEN
            !           IF(WQ.NE.'ON'.AND.WQ.NE.'OFF'.AND.WQ.NE.'HCONLY') THEN
            !           OUTLYNE='"ON", "YES","OFF", "NO" AND "HCONLY"'
            !         CALL SHOWIT(1)
            !           OUTLYNE=' ARE THE ONLY VALID QUALIFIERS'
            !         CALL SHOWIT(1)
            !           OUTLYNE='USED WITH "PSFPLOT"'
            !         CALL SHOWIT(1)
            !           OUTLYNE='RE-ENTER COMMAND'
            !         CALL SHOWIT(1)
            !                           CALL MACFAL
            !                           RETURN
            !                           END IF
            !   C     STI=1
            !                           END IF
            !         IF(STI.EQ.1) THEN
            !           IF(.NOT.PSFPLOT) WRITE(OUTLYNE,10)
            !           IF(PSFPLOT) WRITE(OUTLYNE,11)
            !           IF(PSFHC) WRITE(OUTLYNE,12)
            !         CALL SHOWIT(0)
            !    10   FORMAT('PSF PLOT OUTPUT IS CURRENTLY TURNED "OFF"')
            !    11   FORMAT('PSF PLOT OUTPUT IS CURRENTLY TURNED "ON"')
            !    12   FORMAT('PSF PLOT OUTPUT IS "HARD COPY ONLY"')
            !         CALL SHOWIT(0)
            !                           RETURN
            !                           END IF
            !         IF(WQ.EQ.'ON') THEN
            !                           PSFPLOT=.TRUE.
            !                           PSFHC=.FALSE.
            !                           RETURN
            !                           END IF
            !         IF(WQ.EQ.'OFF') THEN
            !                           PSFPLOT=.FALSE.
            !                           PSFHC=.FALSE.
            !                           RETURN
            !                           END IF
            !         IF(WQ.EQ.'HCONLY') THEN
            !                           PSFPLOT=.TRUE.
            !                           PSFHC=.TRUE.
            !                           RETURN
            !                           END IF
            !                           RETURN
            !                           END
            !   C SUB PSFROT.FOR
            !           SUBROUTINE PSFROT
            !   C
            !           IMPLICIT NONE
            !   C
            !   C       THIS IS SUBROUTINE PSFROT.FOR. THIS SUBROUTINE CONTROLS
            !   C     PSF PLOT ROTATION OUTPUT
            !   C
            !           INCLUDE 'DATMAI.INC'
            !           INCLUDE 'DATLEN.INC'
            !           INCLUDE 'DATSPD.INC'
            !   C
            !   C       CHECK FOR STRING INPUT
            !           IF(SST.EQ.1.OR.SN.EQ.1) THEN
            !           OUTLYNE='"PSFROT" TAKES NO STRING OR NUMERIC INPUT'
            !         CALL SHOWIT(1)
            !           OUTLYNE='RE-ENTER COMMAND'
            !         CALL SHOWIT(1)
            !                           CALL MACFAL
            !                           RETURN
            !                           END IF
            !           IF(STI.EQ.0) THEN
            !           IF(WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
            !           OUTLYNE='"ON", "YES","OFF" AND "NO"'
            !         CALL SHOWIT(1)
            !           OUTLYNE=' ARE THE ONLY VALID QUALIFIERS'
            !         CALL SHOWIT(1)
            !           OUTLYNE='USED WITH "PSFROT"'
            !         CALL SHOWIT(1)
            !           OUTLYNE='RE-ENTER COMMAND'
            !         CALL SHOWIT(1)
            !                           CALL MACFAL
            !                           RETURN
            !                           END IF
            !   C     STI=1
            !                           END IF
            !         IF(STI.EQ.1) THEN
            !           IF(.NOT.ROTPSF) WRITE(OUTLYNE,10)
            !           IF(ROTPSF) WRITE(OUTLYNE,11)
            !         CALL SHOWIT(0)
            !    10   FORMAT('PSF PLOT ROTATION IS CURRENTLY TURNED "OFF"')
            !    11   FORMAT('PSF PLOT ROTATION IS CURRENTLY TURNED "ON"')
            !         CALL SHOWIT(0)
            !                           RETURN
            !                           END IF
            !         IF(WQ.EQ.'ON') THEN
            !                           ROTPSF=.TRUE.
            !                           RETURN
            !                           END IF
            !         IF(WQ.EQ.'OFF') THEN
            !                           ROTPSF=.FALSE.
            !                           RETURN
            !                           END IF
            !                           RETURN
            !                           END


end module