
        SUBROUTINE INITKDP
        USE GLOBALS
        USE NSSMOD
        use zoa_file_handler
        use zoa_logger
C
        IMPLICIT NONE
C
C       THIS IS CALLED BY THE UI at program initialization
C
C       READ THE COMMENTS, THEY TELL YOU WHAT EVERYTHING
C       DOES (SOMETIMES).
C
C       THE COMPLETE LIST OF VALID PROGRAM COMMANDS IS GIVEN
C       WHERE THE ARRAY WCC IS DEFINED IN NAMES.FOR
C
        character (len=255) :: cwd, homedir
        REAL*8 VIEROT
        INTEGER :: macID
C
      INTEGER OLDOUTOLD,FOTLIM,VIEXOF,VIEYOF,N3,OIN,OLD_JUL,NOW_JUL
     1,JULDAY1,BIG_GUY,IREND
C
      EXTERNAL JULDAY1
C
        COMMON/LIMMER/FOTLIM
C
        COMMON/OFFVIE/VIEXOF,VIEYOF,VIEROT
C
        REAL*8 F1X,F1Y,F2X,F2Y,F3X,F3Y
C
        COMMON/FANFOB/F1X,F1Y,F2X,F2Y,F3X,F3Y
C
        LOGICAL FANEXT,EXIS49,EXIS22
C
        COMMON/FANEXI/FANEXT
C
        LOGICAL EXIS7,EXISFD,EXISRA,OPENIN,EXIS68,OPEN68
C
      LOGICAL ATTOP,ATBOT,EXIS89,EXIS96,OPEN96,OPEN44,EXIS44
C
      COMMON/TOPBOT/ATTOP,ATBOT
C
        CHARACTER MCNAM1*8,FILNAM*10,DOSKEY(1:100)*140,
     1  DDDAT*10,TTTIM*8,MYED*8
     2  ,DMNAM*10,STRUC*140,AI4*4,COMLINE*140
C
      COMMON/JKSTRUC/STRUC
C
      INTEGER N,DOSCNT(1:100),KL,PGPOS,OLDIN,ALLOERR
C
      COMMON/DOSKEE/DOSKEY
C
      COMMON/DOCKNT/DOSCNT
C
        COMMON/STRNGR/DDDAT,TTTIM,MCNAM1,DMNAM
C
        CHARACTER SPL*140,AI*3,KKDP*3
     1  ,BLANK*80,BL140*140
     3  ,DRWNAM*11
C
      COMMON/DRWTAG/DRWNAM
C
        INTEGER J,I,K,L,M, IDOS
     4  ,FLG(0:20),IH,MAXPAG
     5  ,LINBOT,LINCUR,KLI
C
        LOGICAL DEG,RAD,TANG
C
        COMMON/ANGMOD/DEG,RAD,TANG
C
        LOGICAL OPEN11,OPEN12,OPEN13,EXISMC, EXISTFP
C
        COMMON/FFL/FLG
C
        COMMON/STAT13/LINCUR,LINBOT,MAXPAG
C
        CHARACTER HCOL(0:9)*12,HCOL2(0:9)*12,YN*1,
     1  HROW(0:100)*12,HROW2(0:100)*12,WCOLD*8
C
        COMMON/HDTAB/HCOL,HCOL2,HROW,HROW2
C
        INTEGER NF
C
      COMMON/NEFER/NF
C
      COMMON/COMNAM/FILNAM
C
      LOGICAL LBLHT
C
      COMMON/JKA/LBLHT
C
      INTEGER PIXAR1,PIXAR2,II
C
      CHARACTER*80 DATA
C
      COMMON/TOYSTORY/PIXAR1,PIXAR2

      REAL*8 DILUPPER,DILLOWER
      COMMON/DILLIM/DILUPPER,DILLOWER
      LOGICAL GETTER,DERVAL
      COMMON/RETTEG/GETTER,DERVAL
C
      LOGICAL ROTSAGFL,BFLAG
C
      COMMON/SAGFLROT/ROTSAGFL
C
      LOGICAL NOCOBSPSF
      COMMON/PSFCOBS/NOCOBSPSF
C
      REAL MSEED
      COMMON/SEEDER/MSEED
      !character(len=256) :: basePath
      character(len=1) :: fileSep
C
        LOGICAL EXIS48,OPEN48
        LOGICAL TABEXIST
        COMMON/EXISTAB/TABEXIST
C
       LOGICAL GROPEN(1:10)
       COMMON/OPENGR/GROPEN
C
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATCFG.INC'
        INCLUDE 'DATMAC.INC'
        INCLUDE 'DATSUB.INC'
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATSPD.INC'
        INCLUDE 'DATSP1.INC'
        INCLUDE 'DATHGR.INC'
C
C       MAX SAVE/REST = 36 8/24/2006
C
        CALL MY_NDPEXC
        PRINT *, "Got to Line 140"

        basePath = getZoaPath()
        fileSep = getFileSep()

        logger = zoaLogger(basePath)
        call logger%logText("File Sep is "//fileSep)


        PRINT *, "File Path is ", trim(basePath)

        !EXISTFP=.FALSE.
        !INQUIRE(FILE='tstFilePath.txt',EXIST=EXISTFP)


        call logger%logText('Got to line 140')
        !write(1,*) 'File Path Test Result is ', EXISTFP
        !INQUIRE(FILE='Info.plist',EXIST=EXISTFP)
        !write(1,*) 'File Path Test Result is ', EXISTFP
        !Call GETARG(0,basePath)
        !write(1,*) 'Base Path is ', basePath


        !PRINT *, "Desired file path is ", getZoaPath()
      !  close(1)
C
C     NO NSS SURFACES
      SYSTEM(102)=0.0D0
C

 991    FORMAT(A79)
 992    FORMAT(A79)
 223    FORMAT(A79)
 224    FORMAT(A79)
 115    FORMAT(A78)
 100    FORMAT(A140)
 105    FORMAT(A80)

 9877   OUTLYNE='AN INPUT FILE ERROR WAS DETECTED'


C     MRAYS=200
      MRAYS=200
C
C       USER PLOT RANGES AND OFFSETS
        UIXSTART=2000
        UIXSTOP=8000
        UIYSTART=1400
        UIYSTOP=5600
        UUDX=5.0
        UUDY=10.0
C
C       MULTICOM SET TO .FALSE. IS THE DEFAULT
        MULTICOM=.FALSE.
C
C       RAY HISTORY MAXRAYS
        RHIST_MAXRAYS=1000
        SHORTHIST=.FALSE.
        LONGHIST=.FALSE.
C
C       DIFFERENTIAL RAY AIMING ON
        AIMRFDIF=.TRUE.
        AIMRYDIF=.TRUE.
C
C       FRAME DRAWING FLAG
        FRAME=.FALSE.
C
        GUIERROR=.FALSE.
        NUMCOLORS=1
        OBJLEVEL=1.0D0
        SYSTEM(11)=1.0D0
        SYSTEM(7)=2.0D0
        SYSTEM(8)=3.0D0
C
C       SET ITERROR TO FALSE
        ITERROR=.FALSE.
C
C       INITIALIZE NRECL TO 4 FOR NON-VAX (NUMBER OF BYTES)
C       REAL*8 = 2*4=8 BYTES
C               NRECL=4
C       INITIALIZE NRECL TO 1 FOR CVF AND VAX (NUMBER OF 4 BYTE WORDS)
C       REAL*8 = 2*1=2 WORDS
                NRECL=1
C
C       DIMENSIONS FOR IMAGING ARRAYS 9/2002
        OBJNX=0
        OBJNY=0
C       NAME OF BMP READER PROGRAM
        BMPREADR='BMP'
        APODGAUSS=.FALSE.
        APODDBLOSS=0.0D0
C
C
                AA='                    '
                BB='        '
                CNULL=AA//AA//AA//AA
C
         MFG='                 '
C        MFG='EDMUND SCIENTIFIC'
C        MFG='MELLES GRIOT'
C        MFG='NEWPORT'
C        MFG='SPINDLER % HOYER'
C        MFG='ROLYN'
C
        GROPEN(1:10)=.FALSE.
C
        NEUTFILE=.TRUE.
C
        THMINLIM=-1.0D20
        THMAXLIM=1.0D20
        RDNEGLIM=-1.0D-20
        RDPOSLIM=1.0D-20
C
        MACFAILURE=.TRUE.
C
        TABEXIST=.FALSE.
C
C       SET MAKEAUTO TO MONOCHROMATIC MODE
        CHROMATIC=.FALSE.
C
C       SET NEXISTN TO FALSE
        NEXISTN=.FALSE.
C
      RAYCLEAR=.TRUE.
C     RAY CLEAR IS ONLY FALSE DURING CONFIGS, SPSRF TYPE 13 RAY TRACING
C
C       SET NODRAW AND NOWMF TO DEFAULTS
        NODRAW=.FALSE.
        NOWMF=.TRUE.
C
      HALTING=.FALSE.
C
C     INITIALIZE THE NIM/MAX REGISTERS
      MIN_REG(1:100)=1.0D300
      MAX_REG(1:100)=-1.0D300
      MAXNEUTRAL=50000
      COHERENCE_FACTOR=0.0D0
C     BFLAG=.FALSE.
C     CALL FORCE_BATCH(BFLAG)
C
      CHRSHIFTEXT=.FALSE.
C
      SCFAY=1.0D0
      SCFAX=1.0D0
      SCFAYP=1.0D0
      SCFAXP=1.0D0
C     ASSIGN 3D LINE DEFAULT VALUES
        LX1=0.0D0
        LY1=0.0D0
        LZ1=0.0D0
        LX2=0.0D0
        LY2=0.0D0
        LZ2=0.0D0
C
       CALL MY_GETCL
      DLEICA(0)=.TRUE.
      DLEICA(1:10)=.TRUE.
      GLEICA(1:10)=.TRUE.
C
c     SET FOR FAR OBJECTS IN DOTF/GOTF
      NEAR_FAR=1
C
      CMDLINE=(CMDLINE(2:127)//' ')
      COMLINE(1:127)=CMDLINE(1:127)
      CALL UPPER_CASE(COMLINE)
      CMDLINE(1:127)=COMLINE(1:127)
C     IF(BFLAG) CMDLINE(1:5)='BATCH'
C WINTER
C      IF(CMDLINE(1:5).NE.'BATCH')CALL SETTEXTSCREEN
C
        CALL MY_NDPEXC
C
      OPTM_INIT=1
C
C     LIN/LOG PSF SCALING
      PSFLIN=1
      PSFLOG=2



C     SET USING OLD REFERENCE RAY DATA TO FALSE
C
      USEOLREF=.FALSE.
      SAVEREF=.FALSE.
C
         NOCOBSPSF=.FALSE.
C     SET PROGRAM RANDOM NUMBER GENERATOR
      SN=0
      SQ=0
      SST=0
      STI=0
      MSEED=1618033.
      CALL RANDSET
C
C     ADDED 1/12/99
      CARTMAN=.TRUE.
C
      ROTSAGFL=.FALSE.
C



C     NO STREHL RATIO EXISTS YET

      RSTREHL_EXIST=.FALSE.

C
C
C     NEXT TWO VARIABLE TRACK STATUS FOR CAPFNIN COMMAND
C
      ALLO=.FALSE.
      ALLONUM=0
C
      OUT=98
      SN=0
      STI=0
      SST=0
      SQ=0
      SST=1
      WS='LENSES  '
      CALL LENSLOC
C     INPUT='OUT TP'
      CALL PROCES
      OUT=6
C
      TFTMIN=-0.005D0
      TFTMAX=0.005D0
      TFDELT=0.001D0
      TFDIRECTION= 2
      DILLOWER=1.0D0
      DILUPPER=1000.0D0
      GETTER=.FALSE.
C     THIS IS USED WHEN GETTING A DERIVATIVE VALUE
      GRSPT=0
      GOOY=.FALSE.
      MTFGRIDS=.FALSE.
      PLTLLI=.FALSE.
      PLTLBL=.FALSE.
      LBLSURF=-99
C


C     SET MACRO OVERWRITEING NOW ALWAYS ON
               ORITEM=.TRUE.
C
C     SET VIGOFF TO .FALSE.
               VIGOFF=.FALSE.
C     SET VSYM TO .TRUE.
               VSYM=.TRUE.
C     SET APPEND TO FALSE FOR EDITTEXT.DAT, CARDTEXT.DAT AND PUNCH.DAT
      call clear_file(trim(basePath)//'SENSIOUT.DAT')
      call clear_file(trim(basePath)//'ISENSOUT.DAT')
      call clear_file(trim(basePath)//'MONTEOUT.DAT')

C
          APPEND=.FALSE.
          ALLSET=.FALSE.
C
C     SET TIME AND DATE STAMPING OF THE LI TO "OFF"
      STMPT=.FALSE.
      STMPD=.FALSE.
C
      XPEN=0
      XPENOL=0
      YPEN=0
      YPENOL=0
      PENSTA=3
C
      RADUNI=1
C
C     SET DINCR DEFAULTS
C
        DINC1=1.0D-5
        DINC2=1.0D-5
        DINC3=1.0D-5
        DINC4=1.0D-7
        DINC5A=1.0D-11
        DINC6A=1.0D-15
        DINC7A=1.0D-19
        DINC8A=1.0D-23
        DINC9A=1.0D-27
        DINC10A=1.0D-31
        DINC11A=1.0D-35
        DINC12A=1.0D-39
        DINC13A=1.0D-42
        DINC14A=1.0D-7
        DINC5B=1.0D-7
        DINC6B=1.0D-7
        DINC7B=1.0D-7
        DINC8B=1.0D-7
        DINC9B=1.0D-7
        DINC10B=1.0D-7
        DINC11B=1.0D-7
        DINC12B=1.0D-7
        DINC13B=1.0D-7
        DINC14B=1.0D-7
        DINC15=1.0D-5
        DINC16=1.0D-5
        DINC17=1.0D-7
        DINC18A=1.0D-11
        DINC19A=1.0D-15
        DINC20A=1.0D-19
        DINC21A=1.0D-23
        DINC18B=1.0D-7
        DINC19B=1.0D-7
        DINC20B=1.0D-7
        DINC21B=1.0D-7
        DINC22=1.0D-7
        DINC23=1.0D-7
        DINC24=1.0D-7
        DINC25=1.0D-7
        DINC26=1.0D-7
        DINC27=1.0D-6
        DINC28=1.0D-10
        DINC29=1.0D-7
        DINC30=0.1D-3
        NSSDINC(1:120)=1.0D-7
C
C     SET DELTT DEFAULTS
C
      DELTT1=0.1D0
      DELTT2=0.1D0
      DELTT1A=4.0D0
      DELTT2A=4.0D0
      DELTT3=0.001D0
      DELTT4=1.0D-6
      DELTT5A=1.0D-10
      DELTT6A=1.0D-14
      DELTT7A=1.0D-18
      DELTT8A=1.0D-22
      DELTT9A=1.0D-26
      DELTT10A=1.0D-30
      DELTT11A=1.0D-34
      DELTT12A=1.0D-36
      DELTT13A=1.0D-40
      DELTT14A=1.0D-6
      DELTT5B=1.0D-6
      DELTT6B=1.0D-6
      DELTT7B=1.0D-6
      DELTT8B=1.0D-6
      DELTT9B=1.0D-6
      DELTT10B=1.0D-6
      DELTT11B=1.0D-6
      DELTT12B=1.0D-6
      DELTT13B=1.0D-6
      DELTT14B=1.0D-6
      DELTT15=0.025D0
      DELTT16=0.025D0
      DELTT15A=1.0D0
      DELTT16A=1.0D0
      DELTT17=1.0D-6
      DELTT18A=1.0D-10
      DELTT19A=1.0D-14
      DELTT20A=1.0D-18
      DELTT21A=1.0D-22
      DELTT18B=1.0D-6
      DELTT19B=1.0D-6
      DELTT20B=1.0D-6
      DELTT21B=1.0D-6
      DELTT22=0.057D0
      DELTT23=0.057D0
      DELTT24=0.057D0
      DELTT25=0.001D0
      DELTT26=0.001D0
      DELTT27=1.0D-5
      DELTT28=1.0D-10
      DELTT29=0.001D0
C
C
C
C     FOOTPRINT GRID SEMI-EXTENT = 8
               FOTLIM=8
C
C     SET DIFFRACTION SPACE TO "I"
          SPACEBALL=2
C
!      OPEN (UNIT=98,BLANK='NULL'
!     1,FILE=basePath//'NUL')
C
C     DEFAULTS FOR VIE PLOTS
              VIEXOF=0.0D0
              VIEYOF=0.0D0
              VIEROT=0.0D0
C
C     SET DEFAULTS FOR AUTO FAN PLOTS
C
                      F1Y=1.0D0
                      F2Y=0.7D0
                      F3Y=0.0D0
                      F1X=0.0D0
                      F2X=0.0D0
                      F3X=0.0D0
C
C     TELLS PLOTTING FANS THAT A FAN DOES NOT EXIST
C               FANEXT=.FALSE.
C
C     PSF TAGGER DEFAULT
      PSFTAG='     PSF.DAT'
C
C     PSF LI DEFAULT
      PSFLI=
     1'THIS IS AN ASCII REPRESENTATION OF A POINT SPREAD FUNCTION'
C
          CAPDEF=16
C     CAPFN GRID DEFAULT IN OPTIMIZATION
          OPNRD=16
C     CAPFN GRID DEFAULT IN TOLERANCING
          TOLNRD=16
C     CAPFN GRID DEFAULT IN PSF CALCULATIONS
          NRD=16
          NRDFLG=1
C     FFT GRID DEFAULT IN PSF CALCULATIONS
          TGR=64
          PGR=TGR-1
          TGRFLG=1
          GRIFLG=0
C     PSF WRITE AND PLOT ON, PLOT ROTATION FALSE
      PSFPLOT=.TRUE.
      PSFWRITE=.TRUE.
      PSFBIN=.FALSE.
      ROTPSF=.FALSE.
C     STREAKED PSFS WRITE AND PLOT ON
      STKPLT=.TRUE.
      STKWRT=.TRUE.
C
C     PIXEL DEFAULT VALUES
      PIXAR1=3
      PIXAR2=3
C
C     DEFAULT VALUES FOR SPOT DIAGRAMS
          NRECT=10
          RINGTOT=5
          RNUMBR=2000
C     RING GRID IS DEFAULT
          SPDTYPE=2
C
          RINGRAD(0)=0.0D0
          RINGRAD(1)=0.2D0
          RINGRAD(2)=0.4D0
          RINGRAD(3)=0.707D0
          RINGRAD(4)=0.866D0
          RINGRAD(5)=1.0D0
          RINGPNT(0)=1
          RINGPNT(1)=80
          RINGPNT(3)=100
          RINGPNT(4)=160
          RINGPNT(4)=200
          RINGPNT(5)=320
          RINGANG(0)=0.0D0
          RINGANG(1)=0.0D0
          RINGANG(2)=0.0D0
          RINGANG(3)=0.0D0
          RINGANG(4)=0.0D0
          RINGANG(5)=0.0D0
C     DEFAULT VALUES FOR SPOT DIAGRAMS FOR OPTIM
          OPNRECT=10
          OPRINGTOT=4
          OPRNUMBR=200
C     RING GRID
          OPSPDTYPE=2
          OPRINGRAD(0)=0.0D0
          OPRINGRAD(1)=0.4D0
          OPRINGRAD(2)=0.7D0
          OPRINGRAD(3)=0.866D0
          OPRINGRAD(4)=1.0D0
          OPRINGPNT(0)=1
          OPRINGPNT(1)=8
          OPRINGPNT(2)=8
          OPRINGPNT(3)=8
          OPRINGPNT(4)=8
          OPRINGANG(0)=0.0D0
          OPRINGANG(1)=0.0D0
          OPRINGANG(2)=0.0D0
          OPRINGANG(3)=0.0D0
          OPRINGANG(4)=0.0D0
C
C     SET DEFAULT FORMAT FOR "WRITE" TO G23.15
                WFORM='(G23.15)'
C
C     SET STATS TO FULL FOR SPOT DIAGRAMS
                STATSP=.TRUE.
C
C       RE-INITIALIZE OPERATING CONDITIONS
                WCOLD=WC
                WC='OPCON'
                CALL PM
                WC=WCOLD
C
C     INITIALIZE NON-LENS GLASS WAVELENGTH
                        GLSWV(1)=0.58756D0
                        GLSWV(2)=0.48613D0
                        GLSWV(3)=0.65627D0
                        GLSWV(4)=0.43584D0
                        GLSWV(5)=0.70652D0
C
C       INITIALIZE CHANGE VECTOR LENGTH
                LCVLCV=0.0D0
C
C       SET OPTMES TO TRUE, DON'T BLOCK MESSAGES AS IN OPTIM.
                OPTMES=.TRUE.
C
C     SET THE GLASS LIBRARY DIRECTORY NAME

C
C     INITIALIZE ALL COLORS TO THEIR DEFAULTS
                COLDEF=15
                COLRAY=15
                COLCLP=15
                COLCOB=9
                COLEDG=15
                COLPRO=15
                COLAXS=15
                COLBAC=0
C
                COLR1=15
                COLR2=12
                COLR3=2
                COLR4=3
                COLR5=4
                COLR6=5
                COLR7=6
                COLR8=7
                COLR9=8
                COLR10=9
C
                COLFRM=15
                COLLBL=15
                COLSPE=15
                COLPEN=15
                COLFILL = 1
C
                N=11
C
        BLANK=AA//AA//AA//AA
        BL140=AA//AA//AA//AA//AA//AA//AA
C
C     INITIALIZE FIGURE TITLE
      FIGTITLE=BLANK
C
C     INITIALIZE DOSKEY AND DOSCNT
                        DOSKEY(1:100)=BL140
                        DOSCNT(1:100)=0
C
C     SET AUTOMATIC EXIT PUPIL LOCATION CALCULATION TO ON
      EXPAUT=.TRUE.
C
C     SET DEFAULT RSPH TO CHIEF
      REFLOC=1
C
C     INITIALIZE ALTERNAMTE MACRO FUNCTION NAMES
              FNAMED(1)='FUN01'
              FNAMED(2)='FUN02'
              FNAMED(3)='FUN03'
              FNAMED(4)='FUN04'
              FNAMED(5)='FUN05'
              FNAMED(6)='FUN06'
              FNAMED(7)='FUN07'
              FNAMED(8)='FUN08'
              FNAMED(9)='FUN09'
              FNAMED(10)='FUN10'
                  DRWNAM='NEUTRAL.DAT'
 ! Set directories.  Base path and File separator should have been
 ! set based on os
              LIBLEN=trim(basePath)//'LIBLEN'//fileSep
              LIBMAC=trim(basePath)//'LIBMAC'//fileSep
              LIBTRA=trim(basePath)//'LIBTRA'//fileSep
              LIBSPO=trim(basePath)//'LIBSPO'//fileSep
              LIBAUT=basePath//'LIBAUT'//fileSep
              LIBPLO=basePath//'LIBPLO'//fileSep
              NSSDIR=trim(basePath)//'NSSDIR'//fileSep
              LIBGLA=trim(basePath)//'LIBGLA'//fileSep
              CURLENS=trim(basePath)//'CURLENS'//fileSep


        PRINT *, "LIBMAC IS ", trim(LIBMAC)
        PRINT *, "LIBGLA IS ", trim(LIBGLA)
C
C       SET DEFAULTS FOR "GLOBAL" RAYTRACE OPTION
        GLOBE=.FALSE.
        GLSURF=1
        OFFX=0.0D0
        OFFY=0.0D0
        OFFZ=0.0D0
        OFFA=0.0D0
        OFFB=0.0D0
        OFFC=0.0D0
C
C       CALCULATION OF GLOBAL RAY PLOTTING DATA IS "OFF"
        PLTRAY=.FALSE.
C
C       SET FLAG FOR FUNCTIONS TO FALSE
        FUNEXT(1:10)=.FALSE.
C
C       THE UNIT NUMBERS USED FOR FILE ACCESS ARE:
C       5,6,7,8,9,10,11,12,13,16,17,18,19
C       20,21,22,23,24,25,26,27,28,29
C       30,31,32,33,34,35,36,37,38,39
C       40,41,42 USED BY WINTERACTER
C       43
C       44 IS FOR INPUT FROM FILE BATCH.DAT
C       45 AND 46 ARE FOR THE GRIDAPD AND GRID OPD FILES CREATED BY
C       47,49
C       48 FOR NSS RAY HISTORY FILE AND ALSO FOR RAYHIST.DAT FOR SEQUENTIAL RAY HISTORIES
C       THE CAPGRID COMMAND (4/28/99)
C       50,51,52,53,54,55,56,57,58,59
C       60,61,62,63,64,65,66,69,
C       67 IS USED FOR USER DEFINED CLEAR APERTURES (9/27/2004)
C       68 FOR MACNEW.DAT
C       70,71,72,73,75,76,77,78,79
C       80 AND 81 ARE 2 OUTPUT FILES DURING GUI OPERATIONS
C       80 IS APPEND AND HOLDS ALL TEXT OUTPUT SINCE SESSION START
C       81 IS REPLACE AND HOLDS LAST OUTPUT
C       82 USED FOR MEM FILE IN NSS
C       83 USED FOR DELETING THE ENTERPRISE PROFESSIONAL FILE
C       84 USED FOR LENO.CSV
C       85,86,87,88 USED FOR FSMEDIT OPERATION
C       89 USED FOR LENSTEXT.DAT
C       90 USED FOR SAGFILE PLOTTING
C       91 USED FOR USER READABLE NSS RAY HISTORY CODE
C       92 USED FOR NSSRAY DATA FILE FOR NSS RAY PLOTTING PLTNSSRY.DAT
C       96 FOR 'QUIET.DAT' INPUT FILE DURING GUI INTERFACE OPERATIONS
C       97 FOR THE GENERAL FILENAME SPECIFIED FOR OUTPUT AND FOR NSS DATABASE STORAGE
C       OR INPUT
C       98 FOR THE NUL FILE
C       99 FOR FILES IN FINDGLASS OPERATION
C       32 FOR THE SPOT DIAGRAM STORAGE TO DISK
C       100 USED FOR NSSSPOT DIAGRAM FILE
C       101 USED FOR THE INTEN.DAT FILE
C       102 USED FOR THE SCENE.DAT FILE
C       103 USED FOR THE RHFOOT.DAT FILE
C       104 USED FOR MTRACEI OUTPUT FILE
C       105 USED FOR MTRACEI BINARY IRRADIANCE FILE
C       106 USED FOR PSFBIN 1/18/2005
C       107 USED FOR PROFIT.DAT 7/20/2005
C       108 USED FOR PROFIT BINARY LIBRAY FILE 7/20/2005
C       109 USED FOR PROFIT BINARY OUTPUT FILES 7/20/2005
C       110 FONTSAVE TO SAVE FONT SIZE
C
C     SAVE AND RELOAD INPUT LEVELS USED 1 - 14
C
C       STANDARD FOR WARNING MESSAGES IS:
C       FIRST QUALIFIER
C       THEN  STRING
C       THEN  NUMERIC INPUT
C
C       SET MAXIMUMS FOR PROGRAM, PASSED AS VARAIBLES
C       IN THE DAT***.FOR ROUTINES IN INCLUDES
                        PII=3.14159265358979323846D0
                        TWOPII=2.0D0*PII
                        HALFPII=PII/2.0D0
                        RPII=SNGL(PII)
                        RTWOPII=SNGL(TWOPII)
                        RHALFPII=SNGL(HALFPII)
C       MAX NUMBER OF CONFIGS
C       MAXIMUM NUMBER OF SURFACES = MAXSUR
C       MAXIMUM NUMBER OF MACRO LINES
C       MAXIMUM NUMBER OF MACROS
C
      MAXLIN=1024
      MAXSUR=499
      MAXMAC=999
      MAXCFG=75

      !  write(1,*) 'Got to line 800'



C
      ALLOCATE(
     1NUMHITS(0:MAXSUR),STAT=ALLOERR)
        NUMHITS(0:MAXSUR)=1
C     REMEMBER TO SET THE ABOUT BOX AND THE PROGRAM NAME MESSAGE CORRECTLY IN GUICODE.FOR AND HARDWAR3.FOR
C
C       MAXIMUM NUMBER OF FITTED DATA PAIRS
                        MAXSPS=2000
C       MAXIMUM NUMBER OF VARIABLES IN VARIABLE SUBFILE SET IN PM
C       MAXIMUM NUMBER OF TOLERANCE VARIABLES SET IN PM
C       MAXIMUM NUMBER OF TOLERANCE COMPENSATORS
                        MAXCMP=10
C       MAXIMUM NUMBER OF TOLERANCE OPERANDS
                        MAXTOP=5
C       MAXIMUM NUMBER OF FOCUS CRITERIA OPERANDS
                        MAXFOCRIT=5
C       MAX GRID SIZE FOR SPOT DIAGRAMS IS MAXGRD x MAXGRD
                        MAXGRD=128
C       MAX SYSTEM ARRAY SIZE (MUST BE A MULTIPLE OF 5)
                        SSIZ=150
C       MAX LENS ARRAY SIZE (MUST BE A MULTIPLE OF 5)
                        LSIZ=160
C       MAX PIKUP ARRAY SIZE (MUST BE A MULTIPLE OF 5)
                        PSIZ=45
C       MAX REAL RAY ARRAY SIZE
                        RAYSIZ=50
C       MAX REAL RAY DIFF ARRAY SIZE
                        DIFSIZ=18
C       SET LDIF AND LDIF2 TO TRUE
                        LDIF=.TRUE.
                        LDIF2=.TRUE.
                        OPDIF=.TRUE.
C       SET LVIG TO TRUE
                        LVIG=.TRUE.
C       SET NOVIRT TO TRUE
                        NOVIRT=.TRUE.
C       SET ANAAIM TO .TRUE. (ONLY FALSE IN CAPFN TRACING)
C
                        ANAAIM=.TRUE.
C     PFIND DEFAULT VALUES
              PFDELM=0.6D0
              PFDELA=0.01D0
              MAXFAIL=2
              PFNUM=10
C
C       RE-INITIALIZE OPERATING CONDITIONS
                WCOLD=WC
                WC='OPCON'
                CALL PM
                WC=WCOLD
C
C
C       SET HEADIN TO FALSE FOR ACCOS LIKE PRINOUT OF SINGLE
C       LINES OF DATA SUCH AS RTG,4
                        HEADIN=.FALSE.
C       VARIABLES INITIALIZATION
                        VBCNT=0
C       MERIT (OPERAND) INITIALIZATION
                        OPCNT=0
                        TOPCNT=0
                        FCCNT=0
                        CMPCNT=0
                        FMTEXT=.FALSE.
C       TOLERANCE VARIABLES INITIALIZATION
                        TVBCNT=0
C       FMT INITIALIZATION
                        FMTFMT=0.0D0
                        FMTFLG=.FALSE.
C     DERIVATIVE MATRIX INITIALIZATION
                        DEREXT=.FALSE.
        ITERROR=.FALSE.
      CALL ITER(0,0,ITERROR)
                        SOLEXT=.FALSE.
                        CFCH=.FALSE.
                        DERSIZ=0
C
C       SET DEFAULT ANGMOD TO DEGREES FOR ALL ANGULAR
C       RAYTRACE OUTPUT
                        DEG=.TRUE.
                        RAD=.FALSE.
                        TANG=.FALSE.
C
C       THIS PROGRAM CONSISTS OF A CMD OR MAIN LEVEL AND
C       A NUMBER OF SUB- LEVELS.
C
C       FLAG F1=1 REPRESENTS THE CMD LEVEL OF THE PROGRAM
C       FLAG F1=0 REPRESENTS NOT THE CMD LEVEL.
C
C       FLAG F2=1 REPRESENTS NEW MACRO CREATION
C       FLAG F2=0 REPRESENTS NOT NEW MACRO CREATION
C
C       (OTHER FLAG STATUS COMMENTS WILL GO HERE AS THE
C       PROGRAM IS DEVELOPED)
C
C       INITIALIZE THE RAY-DIRECTION COSINE TRACKING FLAGS
C       TO +Z DIRECTION, OTHERWISE THE RAYTRACE GIVES INCORRECT
C       RESULTS.
C
C       START THE PROGRAM ALWAYS AT CMD LEVEL. SET FLAG F1
C
                                F1=1
C
C       AND FLAG F2 (MACRO CREATION) IS SET TO 0
                                F2=0
C
C       FLAG F3 IS INITIALLY SET TO 0. WHEN SET TO 1
C       THE MACRO EDIT OR MEDIT LEVEL OF THE PROGRAM
C       IS IN EFFECT. ONLY THE FL COMMAND CAN RESET
C       FLAG F3 TO ZERO.
                        F3=0
C
C       FLAG F4 WHEN SET MEANS THAT A MACRO IS BEING EXECUTED.
C       THE INITIAL DEFAULT SETTING FOR F4 IS F4 = 0
                        F4=0
C
C       THE SPECIAL TRACE FLAG TF(NEST) TRACKS STATUS
C       OF TRACE ON OR OFF IN MACRO EXECUTION AT VARIOUS
C       NESTING LEVELS.
C       TF(NEST)=1 IS TRACE ON, TF(NEST)=0 IS TRACE OFF
C       DEFAULT IS TF(NEST)=0
C
                TF(0:20)=0
C
C       FLAG F5 TRACKS LENS INPUT MODE WHICH IS INITIALIZED
C       BY THE 'LENS' COMMAND AND TERMINATED WITH EOS (END OF SUBFILE).
C       THE DEFAULT VALUE OF F5 IS 0. DURING LENS INPUT F5=1 AND F1=0.
C
                F5=0
C
C       FLAG F6 TRACKS LENS UPDATES INITIALIZED BY THE 'UPDATE'
C       COMMAND WITH THE 'LENS' QUALIFIER WORD. THE DEFAULT VALUE OF
C       F6=0. DURING LENS UPDATING F6=0 AND F1=0.
C
                F6=0
C
C       FLAG F7 TRACKS SPSRF (SPECIAL SURFACE) INPUT MODE
C       INITIATED BY THE COMMAND (SPSRF). DEFAULT IS F7=0
C
                F7=0
C
C       FLAG F8 TRACKS UPDATE SPSRF (SPECIAL SURFACE)
C       UPDATE MODE BY THE COMMAND (UPDATE SPSRF) DEFAULT VALUE
C       IS F8=0
C
                F8=0
C
C       FLAG F9 TRACKS THE SPFIT LEVEL ENTERED BY (SPFIT)
C       DEFAULT VALUE IS F9=0
C
                F9=0
C
C       FLAG F10 TRACKS THE CONFIGS INPUT LEVEL ENTERED BY COMMAND
C       (CONFIGS) ITS DEFAULT VALUE IS 0
C
                F10=0
C
C       FLAG F11 TRACKS THE CONFIGS UPDATE LEVEL ENTERED BY COMMAND
C       (UPDATE CONFIGS) ITS DEFAULT VALUE IS 0
C
                F11=0
C
C
C       FLAG F12 TRACKS THE CURRENT CONFIGURATION NUMBER
C       THE STARTING AND DEFAULT VALUE IS 1
C
                F12=1
C
C       FLAGS F13 AND F14 TRACK THE USAGE OF UPDATE LENS
C       AND UPDATE SPSRF FROM WITHIN CONFIGS OR UPDATE
C       CONFIGS AND ARE USED BY QUERRY TO PRINT CORRECT
C       RESPONSE TO A ? AT THE CONFIGS/UPDATE CONFIGS LEVEL
C       DEFAULT VALUES ARE BOTH 0
C
                F13=0
                F14=0
C
C       FLAG F15 IS SET TO 1 WHEN THE PROCESS OF ACTIVATING
C       A CONFIGURATION IS BEING PERFORMED. OTHERWISE
C       ITS DEFAULT VALUE IS 0
C
                F15=0
C
C       FLAG F16 TRACKS WHETHER OR NOT A MACRO OF THE SAME NAME AS
C       AN EXISTING MACRO IS TRYING TO BE CREATED BY THE "MACRO"
C       COMMAND. THE DEFAULT VALUE ID F16=0. IF A MACRO OF THE SAME
C       NAME DOES EXIST, F16 IS SET TO 1
C
                F16=0
C
C       FLAG F17 TRACKS THE SPECT OR SPECTRAL ANALYSIS
C       LEVEL OF THE PROGRAM. IF F17=1 THEN F1 IS = 1 NOT 0.
C       SPECT HAS AVAILABLE, MANY CMD LEVEL COMMANDS.
C       AND WE ARE AT THE SPECT SUB-LEVEL OF THE PROGRAM.
C       THE DEFAULT IS F17=0
C
                        F17=0
C
C       FLAG F18 TRACKS THE TABLE INPUT LEVEL WITHIN THE SPECT
C       LEVEL OF THE PROGRAM. IF F18=1 AND F17=1 THEN
C       AND WE ARE AT THE TABLE SUB-LEVEL OF SPECT.
C       THE DEFAULT IS F18=0
C
                        F18=0
C
C       FLAG F19 TRACKS THE EXISTENCE OF TABLE DATA.
C       THE DEFAULT IS F19=0 FOR NO DATA.
C
                        F19=0
C
C       FLAG F20 IS AVAILABLE FOR USE
C
C       FLAG F21 IS USED TO TELL THE PROGRAM WHEN
C       REFRACTIVE INDICES SHOULD BE MADE NEGATIVE.
C       EACH TIME A REFECTIVE SURFACE IS ENCOUNTERED, THE
C       VALUE OF FLAG F21 IS REVERSED. THE DEFAULT VALUE IS
C               F21=0
                        F21=0
C
C       AT LENS EOS IN SUBROUTINE LNSEOS, ALL RESOLUTION OF
C       SIGN OF REFRACTIVE INDICES IS HANDLED AS ONE OF THE
C       FIRST DUTIES IN THAT SUBROUTINE.
C
C       F22
C       IF F22=0, THE GLASS CATALOG IS NOT REFERENCED. IF
C       F22=1, A CATALOG LOOKUP IS PERFORMED.
C       F22 IS ALWAYS SET TO 1 DURING LENS INPUT. AFTER
C       LENS INPUT OR LENS UPDATE OR CONFIG CHANGE, F22=0
C       THE STARTING DEFAULT IS F22=0
C       DURING LENS UPDATE, THE FOLLOWING OPERATIONS
C       SET F22 TO 1
C                       ANY CHANGE OF MATERIAL
C                       ANY SUFACE DELETION OR INSERTION
C                       ANY CHANGE TO THE REFRACTIVE INDICES
C                       ANY CHANGE TO THE WAVELENGTHS
C       LIB GET ALSO SETS F22=1
C
                        F22=0
C
C       F23 TRACKS IF TYPE OR SURF WAS ENTERED IN THE
C       SPFIT ROUTINES. IF TYPE IS SET, F23=1,
C       IF SURF IS SET, F23=2
C       DEFAULT IS NOT F23=0
C
                        F23=0
C
C       F24 TRACKS THE PRESENTS OF DATA TO BE FITTED, THAT IS
C       DATA ACCUMULATED WITH THE PROER COEFFS AND FUNCTIONAL FORM
C       NO DATA IS F24=0, DATA = F24=1
C
                        F24=0
C
C       F25 IS SET TO 1 IF FITTED DATA EXISTS TO BE EVALUATED.
C       IF A FIT IS NOT PERFORMED TO THE POINT THAT COEFS ARE
C       AVAILABLE FOR EVALUATION, F24=0 (DEFAULT)
C
                        F25=0
C
C       FLAG F26 IS 1 WHEN A MACRO FUNCTION IS RUNNING
                        F26=0
C
C       FLAG F27 TRACKS IF THE PROGRAM IS IN THE MERIT LEVEL
C       AND THE UPDATE MERIT (U M) LEVEL
C       IF F27=0, NOT
C       IF F27=1, MERIT CREATION LEVEL
C       IF F27=2, UPDATE MERIT LEVEL
                        F27=0
C
C       FLAG F28 TRACKS THE PROGRAM OPERATION DURING OPTIMIZATION
C       CALCULATIONS
C       IF F28=0, NOT
C       IF F28=1, OPTIMIZATION CALCULATIONS IN PROGRESS
                        F28=0
C
C       F35 IS LIKE F28 BUT ONLY USED WITH INTERNAL CALLS TO ITER
C       FLAG F35 TRACKS THE PROGRAM OPERATION DURING OPTIMIZATION
C       CALCULATIONS
C       IF F35=0, NOT
C       IF F35=1, OPTIMIZATION CALCULATIONS IN PROGRESS
                        F35=0
C
C       FLAG F29 TRACKS THE VARIABLES LEVEL
C       AND THE UPDATE VARIABLES LEVEL
C     IF F29=0 ; NOT
C     IF F29=1 ; VARIABLES
C     IF F29=2 ; UPDATE VARIABLES
                        F29=0
C
C       FLAG F30 NOT USED
                        F30=0
C
C       FLAG F31 TRACKS THE PROGRAM OPERATION DURING TOLERANCE
C       CALCULATIONS
C       IF F31=0, NOT
C       IF F31=1, TOLERANCING CALCULATIONS IN PROGRESS
                        F31=0
C       FLAG F32 TRACKS THE LENSDRAWING INPUT LEVEL
C
C       FLAG 46 IS USED TO TRACK A BLANK MACRO
C
C       FLAG F47 IS USED TO FACILITATE CALLING SUBROUTINE
C       MREA DURING A REPLACEMENT OF A MACRO LINE DURING
C       THE MEDIT PROCESS.
C
C       FLAGS F45,F48 AND F49 ARE USED IN ITF,ILF AND IMF PROCEED
C       PROCEEDURES. THE MAKE ITF,IMF AND ILF WORK CORRECTLY.
C       INITIAL VALUES ARE ZERO. FLAG F33 IS USED FOR IPF (PLOT FILE
C       INITIALIZATION)
                        F33=0
                        F45=0
                        F48=0
                        F49=0
C
C     F34=1 IF VIE PLOTTING IS BEING DONE
                        F34=0
C
C       FLAG F50 TRACKS STATUS TO DETERMINE IF A MESSAGE
C       RESPONSE IS REQUIRED FOR BLANK LINE INPUT.
C       DEFAULT IF F50=0
                        F50=0
C
C     FLAG F51 TRACKS TOLERANCE VARIABLE INPUT AND UPDATE (0,1 OR 2)
C     FLAG F52 TRACKS TOLERANCE COMPENSATOR INPUT AND UPDATE (0,1 OR 2)
C     FLAG F53 TRACKS TOLERANCE OPERAND INPUT AND UPDATE (0,1 OR 2)
C     FLAG F54 TRACKS TOLERANCE FOCRIT INPUT AND UPDATE (0,1 OR 2)
C     FLAG F55 TRACKS OPERATION OF THE TOLERANCE ANALYSIS
C       F55=0 MEANS NONE BEING DONE
C       F55=1 MEANS SENSI
C       F55=2 MEANS INVERSE SENSI
C       F55=3 MEANS MONTE-CARLO
                       F51=0
                       F52=0
                       F53=0
                       F54=0
                       F55=0
C
C     FLAGS 51 TO 100 ADDED ON 9/5/92 FOR FUTURE EXPANSION AND SET TO
C     ZERO HERE
C     F56 IS 0 IF DEFORMABLE SURFACE DATA HAS NOT BEEN READ FROM DISK
C     AND ID 1 IF IT ALREADY HAS BEEN
                        F56=0
C
C
C     FLAG F57 IS 0 IF NO AUTOFUNC FUNCTION IS TO BE EXECUTED, IT IS 1
C     IF THERE IS AN UNRESOLVED AUTOFUNC FUNCTION
                        F57=0
C
C
C
C
C     FLAG F58 IS 1 IF FANS ARE BEING GENERATED, ELSE F58=0
C
                        F58=0
C
C
                        F59=0
                        F60=0
                        F61=0
                        F62=0
                        F63=0
                        F64=0
                        F65=0
                        F66=0
                        F67=0
                        F68=0
                        F69=0
                        F70=0
                        F71=0
                        F72=0
                        F73=0
                        F74=0
                        F75=0
                        F76=0
                        F77=0
                        F78=0
                        F79=0
                        F80=0
                        F81=0
                        F82=0
                        F83=0
                        F84=0
                        F85=0
                        F86=0
                        F87=0
                        F88=0
                        F89=0
                        F90=0
                        F91=0
                        F92=0
                        F93=0
                        F94=0
                        F95=0
                        F96=0
                        F97=0
                        F98=0
                        F99=0
                        F100=0
C     PIVAXIS MODE ALWAYS STARTS AS "VERTEX" BECAUSE SYSTEM(101) ALWAYS
C     STARTS AS 0
C
C       THE PROGRAM USER FLAGS FLG(0:20) ARE INITIALIZED HERE
                        FLG(0)=0
                        FLG(1:20)=-1
C
C       INITIALIZE BOTTOM LINE OF FILE UNIT 13
                        LINBOT=0
C       INITIALIZE CURRENT LINE IN FILE UNIT 13
                        LINCUR=0
C
C       SET REFEXT TO NO INDICATING THAT NO REFERENCE RAY DATA EXISTS
C       AND NULL IN FOB IS .FALSE.
C
                FOBYES=.FALSE.
                REFEXT=.FALSE.
                SPDEXT=.FALSE.
                GSPDEXT=.FALSE.
                CPFNEXT=.FALSE.
                CALL DELPSF
                NULL=.FALSE.
                RAYEXT=.FALSE.
                POLEXT=.FALSE.
                FAIL=.TRUE.

      OUTLYNE = "GOT TO LINE 1218 IN INITKDP!"
      !write(1,*) 'Got to line 1227'
      CALL SHOWIT(19)
C
C***********************************************************************
C
C       INITIALIZE TIMER
                CALL SETTIM
C
C***********************************************************************
C       INITIALIZE MACRO FUNCTIONS
        call does_mac_dat_exist(EXISMC)
        IF(EXISMC) THEN
C       LOAD FUNCTIONS
        macID = open_mac_dat()

C
        DO I=1,MAXMAC
        READ(macID,REC=I,ERR=1012) MCDIR1(I),MCDIR2(1,I),MCDIR2(2,I),
     1  MCDIR2(3,I),MCDIR3(I)
C
        KLI=0
        IF(MCDIR1(I).EQ.'FUN01   ') KLI=1
        IF(MCDIR1(I).EQ.'FUN02   ') KLI=2
        IF(MCDIR1(I).EQ.'FUN03   ') KLI=3
        IF(MCDIR1(I).EQ.'FUN04   ') KLI=4
        IF(MCDIR1(I).EQ.'FUN05   ') KLI=5
        IF(MCDIR1(I).EQ.'FUN06   ') KLI=6
        IF(MCDIR1(I).EQ.'FUN07   ') KLI=7
        IF(MCDIR1(I).EQ.'FUN08   ') KLI=8
        IF(MCDIR1(I).EQ.'FUN09   ') KLI=9
        IF(MCDIR1(I).EQ.'FUN10   ') KLI=10
C
        IF(KLI.NE.0) THEN
        FUNEXT(KLI)=.TRUE.
        FCDIR1(KLI)=MCDIR1(I)
        FCDIR2(1,KLI)=MCDIR2(1,I)
        FCDIR2(2,KLI)=MCDIR2(2,I)
        FCDIR2(3,KLI)=MCDIR2(3,I)
C       LOAD MACRO FUNCTIONS INTO THE FUNCTION STORAGE
C
C       NOW READ THE BODY OF THE MACRO STARTING AT RECORD 1 FROM FILE
C       FILNAM AFTER DETERMINING FILNAM
                        NF=I
C
                CALL MACFIL
        CALL READMACROFILE(trim(LIBMAC)//FILNAM)

        DO K=1,(FCDIR2(2,KLI))
                L=K-1
        FUNCW(KLI,L)=MACCW(L)
        FUNQW(KLI,L)=MACQW(L)
        FUNSTR(KLI,L)=MACSTR(L)
           DO M = 1, 5
               FUNNW(KLI,M,L)=MACNW(M,L)
           END DO
           DO M = 1, 20
              FUNSTA(KLI,M,L)=MACSTA(M,L)
          END DO

        END DO
C       FUNCTION LOADED
C       CHECK FOR NEXT MACRO NAME
                        END IF
 1012   CONTINUE
        END DO
C
C       CLOSE UNIT 20 TO I/O
C
        CALL CLOSE_FILE(macID,1)
C       PROCEDD WITHOUT ATTEMPED FUNCTION LOADING
                        END IF

C                        PRINT *, "Got to line 1301"
C
C***********************************************************************
C
C                       PLOT INITIALIZATION
C     IS PERFORMED BT PLTRST.FOR WHEN ENTERING THE GRAPHIC MODE
C     VIA THE PLOT NEW COMMAND
C
C       SET PPLI TO BLANK
        PPLI(1:80)=BLANK(1:80)
C
C**********************************************************************
C               INPUT IS ALWAYS AN 140 CHARACTER,CHARACTER
C               VARIABLE.
C
C               THE STATEMENTS WHICH FOLLOW UP TO BUT NOT
C               INCLUDING STATEMENT 1 ARE EXECUTED ONLY ONCE
C               AT PROGRAM STARTUP. THEY INITIALIZE PROGRAM VARIABLES.
C
C               HERE IS WHERE ALL POSSIBLE VALID PROGRAM COMMAND WORDS
C               ARE INITIALIZED AND STORED IN ARRAY WCC WHICH IS PASSED
C               VIA COMWDS COMMON. THIS IS DONE IN SUBROUTINE 'NAMES'
C
                        PRINT *, "ABOUT to Call NAMES"
                        !write(1,*) 'Got to line 1385'
                        CALL NAMES
                        !write(1,*) 'Got to line 1387'
C
                PRINT *, "POST NAMES"
                IN  = 5
                OUT = 6
                ECH=1
C       INITIALIZE MEMORY REGISTERS TO ZERO (REG)
C
                REG(1:50)=0.0D0
C       INITIALIZE THE GENERAL PURPOSE CHARACTER REGISTERS TO ZERO
                AGPREG(1:100000)=' '
                GPREG(1:100000)=0.0D0
C
C       BEFORE THE MAIN READ STATEMENT, CHECK IF
C       A DEFAULTS.DAT FILE EXISTS. IF SO, READ AND PROCESS THE
C       INSTRUCTIONS THERE.
C
        OPEN(UNIT=16,ACCESS='SEQUENTIAL',
     1  BLANK='NULL',FORM='FORMATTED',
     1  FILE=trim(basePath)//'DEFAULTS.DAT',STATUS='UNKNOWN')
                DO I=1,99999
        READ(UNIT=16,FMT=100,END=9887,ERR=9877) INPUT(1:140)
        MULTICOM=.TRUE.
        !write(1,*) 'Got to line 1410'
        CALL PROCES
        MULTICOM=.FALSE.
                END DO
 9887   CALL CLOSE_FILE(16,1)
      !write(1,*) 'Got to line 1415'
      INPUT='FIELDS RESET'
        MULTICOM=.TRUE.
      CALL PROCES
        MULTICOM=.FALSE.
      INPUT='RAYS RESET'
        MULTICOM=.TRUE.
      !write(1,*) 'Got to line 1422'
      CALL PROCES
        MULTICOM=.FALSE.
C
      !PRINT *, "Got to line 1361"
      !write(1,*) 'Got to line 1393'

C     INITIALIZE OPERAND VALUES AND DESCRIPTORS
                       I=MAXOPT
                       OPERND(1:I,1)=0.0D0
                       OPERDESC(1:I)(1:80)=' '
                  OPERND(1:I,1:20)=0.0D0
C
C       SET THE COUNTER TO THE TOP OF THE MERIT ARRAY STRUCTURE.
                        OPCNT=0
                        FCCNT=0
                        TOPCNT=0
                        FMTEXT=.FALSE.
C
                        CORMOD=1
C       INITIALIZE THE CURRENT CONFIGURATION NUMBER TO 1
                        CURFIG=1
C
C               IN AND OUT ARE THE DEFAULT READ/WRITE UNIT
C               NUMBERS.
C*******************************************************************
C       LOAD INITIAL LENS DATA
        EXIS89=.FALSE.
      OLDIN=IN
          IN=89
        OPTMES=.FALSE.
      EXIS89=.FALSE.
        INQUIRE(FILE=trim(CURLENS)//'LENSTEXT.DAT',EXIST=EXIS89)
                IF(EXIS89) THEN
C       LENSTEXT EXISTS, READ IT
C
C               THIS IS THE LENSTEXT.DAT INPUT SECTION
C               THAT IS USED TO RESTORE THE LAST CURRENT LENS
C               FROM UNIT 89
C
         OPEN(UNIT=89,ACCESS='SEQUENTIAL',BLANK='NULL'
     1  ,FORM='FORMATTED',FILE=trim(CURLENS)//'LENSTEXT.DAT'
     2  ,STATUS='UNKNOWN')
        REWIND(UNIT=89)
        !PRINT *, "Got to line 1400"
 3141   READ(UNIT=89,FMT=100,END=8888,ERR=8887) INPUT(1:140)
        PRINT *, "READ CURLENS INPUT IS ", INPUT(1:140)
        IF(INPUT(1:3).EQ.'OUT') GO TO 3141
      IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
      IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
      IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
      IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
      IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
      IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
      IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
        IF(INPUT(1:8).EQ.'FLDSRAYS') THEN
                        IREND=500
                IF(INPUT(10:13).EQ.' 500') IREND=500
                IF(INPUT(10:13).EQ.'1000') IREND=1000
                IF(INPUT(10:13).EQ.'1500') IREND=1500
                IF(INPUT(10:13).EQ.'2000') IREND=2000
                IF(INPUT(10:13).EQ.'2500') IREND=2500
                IF(INPUT(10:13).EQ.'3000') IREND=3000
                IF(INPUT(10:13).EQ.'3500') IREND=3500
                IF(INPUT(10:13).EQ.'4000') IREND=4000
                IF(INPUT(10:13).EQ.'4500') IREND=4500
                IF(INPUT(10:13).EQ.'5000') IREND=5000
                DO I=1,200
        READ(89,*) AI,FIELDY(I),FIELDX(I),
     1FIELDZ(I),N3
        FIELDW(I)=DBLE(N3)
        IF(FIELDW(I).EQ.0.0D0) THEN
                FIELDW(I)=SYSTEM(11)
                END IF
                END DO
                DO I=1,IREND
        READ(89,*,ERR=8887,END=8887) AI4,RAYY(I),RAYX(I),N3
                RAYW(I)=DBLE(N3)
        IF(RAYW(I).EQ.0.0D0) THEN
        IF(I.GE.1.AND.I.LE.41) RAYW(I)=SYSTEM(11)
        IF(I.GE.42.AND.I.LE.82) RAYW(I)=SYSTEM(7)
        IF(I.GE.83.AND.I.LE.123) RAYW(I)=SYSTEM(8)
                END IF
                END DO
                ELSE
      IF(INPUT(1:1).EQ.'.'.OR.
     1INPUT(1:1).EQ.'1'.OR.
     1INPUT(1:1).EQ.'2'.OR.
     1INPUT(1:1).EQ.'3'.OR.
     1INPUT(1:1).EQ.'4'.OR.
     1INPUT(1:1).EQ.'5'.OR.
     1INPUT(1:1).EQ.'6'.OR.
     1INPUT(1:1).EQ.'7'.OR.
     1INPUT(1:1).EQ.'8'.OR.
     1INPUT(1:1).EQ.'9'.OR.
     1INPUT(1:1).EQ.'0'
     1.OR.INPUT(1:8).EQ.'FLDSRAYS') THEN
                ELSE
                CALL PROCES
                END IF
                END IF
                        GO TO 3141
C     LENSTEXT.DAT HAS BEEN LOADED
 8888   BACKSPACE(UNIT=89)
        REWIND (UNIT=89)
        CALL CLOSE_FILE(89,1)
                       GO TO 8889
 8887   BACKSPACE(UNIT=89)
        BACKSPACE(UNIT=89)
                        GO TO 3141
 8889                   CONTINUE
                        ELSE
C     NO LENSTEXT EXISTS,OR AN ERROR EXITSED
                        END IF
          IN=OLDIN
        OPTMES=.TRUE.
        !PRINT *, "Got to line 1470"
C
C       SET UP DEFAULT MERIT FUNCTION CONDITIONS
C       DFGRID=1 (HEX) OR =2 FOR (RECT-THE DEFAULT)
                DFGRID=1
                DFDEL=0.385
                DFSEC=8
                DFRIN=2
                DFWT1=1.0D0
                DFWT2=1.0D0
                DFWT3=1.0D0
                DEFAULT_FOB(1,1:25)=0.0D0
                DEFAULT_FOB(2,1:25)=0.0D0
                DEFAULT_FOB(3,1:25)=1.0D0
                DEFAULT_FOB(4,1:25)=SYSTEM(11)
                DEFAULT_FOB(1,1)=0.0D0
                DEFAULT_FOB(2,1)=0.0D0
                DEFAULT_FOB(3,1)=1.0D0
                DEFAULT_FOB(4,1)=SYSTEM(11)
                DEFAULT_FOB(1,2)=0.7D0
                DEFAULT_FOB(2,2)=0.0D0
                DEFAULT_FOB(3,2)=1.0D0
                DEFAULT_FOB(4,2)=SYSTEM(11)
                DEFAULT_FOB(1,3)=1.0D0
                DEFAULT_FOB(2,3)=0.0D0
                DEFAULT_FOB(3,3)=1.0D0
                DEFAULT_FOB(4,3)=SYSTEM(11)
                DFPNUMB=3
                DFWAVENUMB=INT(SYSTEM(11))
                DFTYPENUMB=1
                DF_CFG=1
                IN=5

                   EXIS7=.FALSE.
        INQUIRE(FILE=trim(basePath)//'PRINTER.TXT',EXIST=EXIS7)
        IF(EXIS7) call clear_file(trim(basePath)//'PRINTER.TXT')

                   EXIS7=.FALSE.

C
C*******************************************************************
C
C               THIS IS THE PROGRAM MAIN READ STATEMENT.
C******************************************************************
C       THE NEXT FEW STATEMENTS CONTROL THE INPUT LINE
C       AND THE PROGRAM CURSOR. THEY ARE THE ONLY NON-
C       ANSI FORTRAN 77 STATEMENTS IN THE PROGRAN.
C
C     SET DOSKEY KL TO 0
                        KL=0
C

        OLDOUTOLD=OUT
 1              CONTINUE
        OLDOUTOLD=OUT
C
C     ALL KEYBOARD AND LATER WINFILE INPUT IS DONE WITH A CALL TO
C     USERINPT



      IF(CMDLINE(1:5).EQ.'BATCH') THEN
        EXIS44=.FALSE.
        OPEN44=.FALSE.
        INQUIRE(FILE=basePath//'BATCH.DAT',EXIST=EXIS44)
        INQUIRE(FILE=basePath//'BATCH.DAT',OPENED=OPEN44)
      IF(EXIS44) THEN
      IF(OPEN44) CALL CLOSE_FILE(44,1)
C     OPEN AND PROCESS CONTENTS
      OPEN(UNIT=44,ACCESS='SEQUENTIAL',FILE=basePath//'BATCH.DAT',
     1FORM='FORMATTED',STATUS='UNKNOWN')
      REWIND(UNIT=44)

      PRINT *, "Got to line 1595"
C     READ AND PROCESS INSTRUCTIONS
        DO I=1,99999
        READ(UNIT=44,FMT=100,END=9743,ERR=9744) INPUT(1:140)
        IF(INPUT(1:3).NE.'OUT') THEN
      IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
      IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
      IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
      IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
      IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
      IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
      IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
        IF(INPUT(1:8).EQ.'FLDSRAYS') THEN
                        IREND=500
                IF(INPUT(10:13).EQ.' 500') IREND=500
                IF(INPUT(10:13).EQ.'1000') IREND=1000
                IF(INPUT(10:13).EQ.'1500') IREND=1500
                IF(INPUT(10:13).EQ.'2000') IREND=2000
                IF(INPUT(10:13).EQ.'2500') IREND=2500
                IF(INPUT(10:13).EQ.'3000') IREND=3000
                IF(INPUT(10:13).EQ.'3500') IREND=3500
                IF(INPUT(10:13).EQ.'4000') IREND=4000
                IF(INPUT(10:13).EQ.'4500') IREND=4500
                IF(INPUT(10:13).EQ.'5000') IREND=5000
                DO J=1,200
        READ(44,*) AI,FIELDY(I),FIELDX(I),
     1FIELDZ(I),N3
        FIELDW(I)=DBLE(N3)
        IF(FIELDW(I).EQ.0.0D0) THEN
                FIELDW(I)=SYSTEM(11)
                END IF
                END DO
                DO J=1,IREND
        READ(44,*,ERR=9678,END=9678) AI4,RAYY(I),RAYX(I),N3
                RAYW(I)=DBLE(N3)
        IF(RAYW(I).EQ.0.0D0) THEN
        IF(I.GE.1.AND.I.LE.41) RAYW(I)=SYSTEM(11)
        IF(I.GE.42.AND.I.LE.82) RAYW(I)=SYSTEM(7)
        IF(I.GE.83.AND.I.LE.123) RAYW(I)=SYSTEM(8)
                END IF
                END DO
                ELSE

                END IF
        IF(F16.EQ.1.AND.INPUT(1:3).NE.'EOM') GO TO 9677
        IF(F16.EQ.1.AND.INPUT(1:3).EQ.'EOM') THEN
                        F16=0
                        GO TO 9677
                        END IF
 9677   CONTINUE
      IF(INPUT(1:1).EQ.'.'.OR.
     1INPUT(1:1).EQ.'1'.OR.
     1INPUT(1:1).EQ.'2'.OR.
     1INPUT(1:1).EQ.'3'.OR.
     1INPUT(1:1).EQ.'4'.OR.
     1INPUT(1:1).EQ.'5'.OR.
     1INPUT(1:1).EQ.'6'.OR.
     1INPUT(1:1).EQ.'7'.OR.
     1INPUT(1:1).EQ.'8'.OR.
     1INPUT(1:1).EQ.'9'.OR.
     1INPUT(1:1).EQ.'0'
     1.OR.INPUT(1:8).EQ.'FLDSRAYS') THEN
                ELSE
                CALL PROCES
                END IF
C     AFTER PROCESS, CLOSE AND KEEP BOTH 80 AND 81
                        GO TO 9679
 9678                   BACKSPACE(UNIT=44)
                        BACKSPACE(UNIT=44)
 9679                   CONTINUE
                END IF
                        END DO
      CALL CLOSE_FILE(44,1)
C     WHEN FINISHED, EXITT
      SQ=0
      SST=0
      SN=0
      STI=0
                   CALL EXITT(0)
                   ELSE
 9744              CONTINUE
      OUTLYNE='ERROR PROCESSING FILE "BATCH.DAT"'
      CALL SHOWIT(1)
      OUTLYNE='BATCH JOB TERMINATED'
      CALL SHOWIT(1)
      SQ=0
      SST=0
      SN=0
      STI=0
      CALL EXITT(1)
 9743              CONTINUE
      OUTLYNE='END OF FILE "BATCH.DAT" ENCOUNTERED'
      CALL SHOWIT(1)
      OUTLYNE='BATCH JOB TERMINATED'
      CALL SHOWIT(1)
      SQ=0
      SST=0
      SN=0
      STI=0
      CALL EXITT(1)
                     END IF
                       ELSE
                       END IF

      IF(IN.EQ.5.AND.CMDLINE(1:5).NE.'BATCH') THEN
      HALTING=.FALSE.
C WINTER
C      CALL USERINPT
C      GO TO 1
      END IF
C

      call logger%logText('Got to line 1783 - end of INITKDP')

      RETURN

        END
