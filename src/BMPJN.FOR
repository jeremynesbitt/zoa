        SUBROUTINE READIMAGEARRAY(II,WRD1)

        USE GLOBALS
        IMPLICIT NONE
        INTEGER I,J,K,L,INFO(3),LENGTH,NX,NY,II
        INTEGER IR,IG,IB,ALLOERR,IERROR
        REAL*8 WRD1
        INTEGER ABMPDATA24
        DIMENSION ABMPDATA24(:)
        LOGICAL advance
        CHARACTER*80 BMPFILE
C        character*12 fin,fou
        character*54 header
        INTEGER im, jm
C        character*1  s
C        integer rgb,im,jm,i,j,k,t
C         open(unit=1,file=fin,status='old',recl=1)
C         open(unit=2,file=fou,form='unformatted')
C         read(1,'(a54,$)')header(1:54)
C         call Bits(4,header(19:22),im) !Width
C         call Bits(4,header(23:26),jm) !Height

        COMMON/FILEBMP/BMPFILE
        INCLUDE 'DATMAI.INC'
        ALLOCATABLE :: ABMPDATA24
        PRINT *, "READ IMAGE ARRAY.."
        open(unit=1,file=BMPFILE,status='old',recl=54,
     1  FORM='UNFORMATTED', ACCESS='DIRECT')
         READ(UNIT=1,REC=1) HEADER(1:54)
         call bit42num(header(19:22),im) !Width
         call bit42num(header(23:26),jm) !Height
C        read(1,'(a54,$)')header(1:54)
        PRINT *, header(1:54)
        PRINT *, "IM JM", im, " ", jm
        close(1)
        PRINT *, "II = ", II
C        II=1 ! JN HACK

C        CALL IGrFileInfo(TRIM(BMPFILE),INFO,3)
        IF(II.EQ.1) THEN
        PRINT *, " FAILING HERE?"
        OBJNX = im
C        OBJNX=INFO(2)
C        NX=INFO(2)
        NX = im
        OBJNY = jm
        NY = jm
C        OBJNY=INFO(3)
C        NY=INFO(3)
        PRINT *, "DEALLOCATE?"
        DEALLOCATE (IOBJECTX,IOBJECTY,
     1  IOBJECTV,STAT=ALLOERR)
        ALLOCATE (IOBJECTX(NX,NY),IOBJECTY(NX,NY),
     1  IOBJECTV(NX,NY,3),STAT=ALLOERR)
        IOBJECTX(1:NX,1:NY)=0.0D0
        IOBJECTY(1:NX,1:NY)=0.0D0
        IOBJECTV(1:NX,1:NY,1:3)=0.0D0
        ODELX=WRD1/DBLE(REAL(NX-1))
        ODELY=ODELX
C       LOAD ARRAYS
        PRINT *, "LOAD ARRAYS"
        OXODD=.FALSE.
        IF((DBLE(NX)/2.0D0)-DBLE(NX/2).NE.0.0D0) OXODD=.TRUE.
        OYODD=.FALSE.
        IF((DBLE(NY)/2.0D0)-DBLE(NY/2).NE.0.0D0) OYODD=.TRUE.
C
        IF(OXODD) THEN
                    DO I=0,NX-1
                    DO J=1,NY
        IOBJECTX(I+1,J)=-(ODELX*DBLE((NX-1)/2))+(DBLE(I)*ODELX)
                    END DO
                    END DO
                        END IF
        IF(.NOT.OXODD) THEN
                    DO I=1,NX
                    DO J=1,NY
        IOBJECTX(I,J)=-(ODELX*(DBLE(NX+1)/2.0D0))+(DBLE(I)*ODELX)
                    END DO
                    END DO
                    END IF
        IF(OYODD) THEN
                    DO I=1,NX
                    DO J=0,NY-1
        IOBJECTY(I,J+1)=-(ODELY*DBLE((NY-1)/2))+(DBLE(J)*ODELY)
                    END DO
                    END DO
                        END IF
        IF(.NOT.OYODD) THEN
                    DO I=1,NX
                    DO J=1,NY
        IOBJECTY(I,J)=-(ODELY*(DBLE(NY+1)/2.0D0))+(DBLE(J)*ODELY)
                    END DO
                    END DO
                    END IF
                        END IF
        IF(II.EQ.2) THEN
        IMGNX=INFO(2)
        NX=INFO(2)
        IMGNY=INFO(3)
        NY=INFO(3)
        DEALLOCATE (IIMAGEX,IIMAGEY,
     1  IIMAGEV,STAT=ALLOERR)
        ALLOCATE (IIMAGEX(NX,NY),IIMAGEY(NX,NY),
     1  IIMAGEV(NX,NY,3,4),STAT=ALLOERR)
        IIMAGEX(1:NX,1:NY)=0.0D0
        IIMAGEY(1:NX,1:NY)=0.0D0
        IIMAGEV(1:NX,1:NY,1:3,1:4)=0.0D0
        IDELX=W1/DBLE(NX-1)
        IDELY=IDELX
C       LOAD ARRAYS
        IXODD=.FALSE.
        IF((DBLE(NX)/2.0D0)-DBLE(NX/2).NE.0.0D0) IXODD=.TRUE.
        IYODD=.FALSE.
        IF((DBLE(NY)/2.0D0)-DBLE(NY/2).NE.0.0D0) IYODD=.TRUE.
C
        IF(IXODD) THEN
                    DO I=0,NX-1
                    DO J=1,NY
        IIMAGEX(I+1,J)=-(IDELX*DBLE((NX-1)/2))+(DBLE(I)*IDELX)
                    END DO
                    END DO
                        END IF
        IF(.NOT.IXODD) THEN
                    DO I=1,NX
                    DO J=1,NY
        IIMAGEX(I,J)=-(IDELX*(DBLE(NX+1)/2.0D0))+(DBLE(I)*IDELX)
                    END DO
                    END DO
                    END IF
        IF(IYODD) THEN
                    DO I=1,NX
                    DO J=0,NY-1
        IIMAGEY(I,J+1)=-(IDELY*DBLE((NY-1)/2))+(DBLE(J)*IDELY)
                    END DO
                    END DO
                        END IF
        IF(.NOT.IYODD) THEN
                    DO I=1,NX
                    DO J=1,NY
        IIMAGEY(I,J)=-(IDELY*(DBLE(NY+1)/2.0D0))+(DBLE(J)*IDELY)
                    END DO
                    END DO
                    END IF
                        END IF
C
        DEALLOCATE (ABMPDATA24,STAT=ALLOERR)
        ALLOCATE (ABMPDATA24(INFO(2)*INFO(3)),STAT=ALLOERR)
        IF(II.EQ.1) THEN
C       READ OBJECT
        IF(NUMCOLORS.EQ.1) THEN
        L=1
        IERROR=0
C        CALL IGrLoadImageData(TRIM(BMPFILE),ABMPDATA24)
C        IERROR=INFOERROR(2)
        IF(IERROR.NE.0) THEN
        WRITE(OUTLYNE,*) 'ERROR OPENING',TRIM(BMPFILE)
        CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
                DO K=1,OBJNY
                DO J=1,OBJNX
C       CALL WRGBsplit(ABMPDATA24(L),IR,IG,IB)
        IOBJECTV(J,K,1)=DBLE(IG)
        L=L+1
                END DO
                END DO
                        END IF
        IF(NUMCOLORS.EQ.3) THEN
        L=1
        IERROR=0
C       CALL IGrLoadImageData(TRIM(BMPFILE),ABMPDATA24)
C       IERROR=INFOERROR(2)
        IF(IERROR.NE.0) THEN
        WRITE(OUTLYNE,*) 'ERROR OPENING ',TRIM(BMPFILE)
        CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
                DO K=1,OBJNY
                DO J=1,OBJNX
C        CALL WRGBsplit(ABMPDATA24(L),IR,IG,IB)
        IOBJECTV(J,K,3)=DBLE(IR)
        IOBJECTV(J,K,1)=DBLE(IG)
        IOBJECTV(J,K,2)=DBLE(IB)
        L=L+1
                END DO
                END DO
                        END IF

                        END IF
        IF(II.EQ.2) THEN
C       READ IMAGE
        IF(NUMCOLORS.EQ.1) THEN
        L=1
        IERROR=0
C        CALL IGrLoadImageData(TRIM(BMPFILE),ABMPDATA24)
C        IERROR=INFOERROR(2)
        IF(IERROR.NE.0) THEN
C       IERROR=INFOERROR(1)
        WRITE(OUTLYNE,*) 'ERROR OPENING ',TRIM(BMPFILE)
        CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
                DO K=1,IMGNY
                DO J=1,IMGNX
C        CALL WRGBsplit(ABMPDATA24(L),IR,IG,IB)
        IIMAGEV(J,K,1,1)=DBLE(IG)
        L=L+1
                END DO
                END DO
                        END IF
        IF(NUMCOLORS.EQ.3) THEN
        L=1
        IERROR=0
C        CALL IGrLoadImageData(TRIM(BMPFILE),ABMPDATA24)
C        IERROR=INFOERROR(2)
        IF(IERROR.NE.0) THEN
        WRITE(OUTLYNE,*) 'ERROR OPENING ',TRIM(BMPFILE)
        CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
                DO K=1,IMGNY
                DO J=1,IMGNX
C        CALL WRGBsplit(ABMPDATA24(L),IR,IG,IB)
        IIMAGEV(J,K,3,1)=DBLE(IR)
        IIMAGEV(J,K,1,1)=DBLE(IG)
        IIMAGEV(J,K,2,1)=DBLE(IB)
        L=L+1
                END DO
                END DO
                        END IF

                        END IF
                        RETURN
                        END
        SUBROUTINE PLOTIMAGEARRAY(I,LENBMP,TRIMMER)

        USE GLOBALS
        !USE csv_file
        IMPLICIT NONE
        INTEGER I,J,K,L
        INTEGER LENBMP, lun
        INTEGER*1 INTERM
        INTEGER*4 BMPDATA24(1:LENBMP)
        REAL*8 PEAKER,TRIMMER
        CHARACTER*80 BMPFILE
        character*1 rgb(3,OBJNX,OBJNY) !      RGB pixel data array
        INCLUDE 'DATMAI.INC'
        BMPFILE=TRIM('PLOTBMP.BMP')
        PRINT *, "PLOTIMAGEARRAY ROUTINE..."
        IF(I.EQ.1) THEN
C       PLOT OBJECT
        IF(NUMCOLORS.EQ.1) THEN
        PEAKER=-1.0D300
                DO K=1,OBJNY
                DO J=1,OBJNX
        IF(IOBJECTV(J,K,1).GT.PEAKER) PEAKER=IOBJECTV(J,K,1)
                END DO
                END DO
        IF(PEAKER.LE.255.0D0) PEAKER=1.0D0
        IF(PEAKER.GT.255.0D0) PEAKER=PEAKER/255.0D0
        L=1
                DO K=1,OBJNY
                DO J=1,OBJNX
        INTERM=INT((IOBJECTV(J,K,1)/PEAKER))
        BMPDATA24(L)=INTERM+(256*INTERM)+(256*256*INTERM)
        L=L+1
                END DO
                END DO
        CALL MY_SYSTEM2('DEL PLOTBMP.BMP',15)
C        CALL IGrSaveImageData(TRIM(BMPFILE),BMPDATA24,OBJNX,OBJNY)
        CALL MY_SYSTEM2(TRIM(BMPREADR)//' PLOTBMP.BMP',80)
                        END IF
        IF(NUMCOLORS.EQ.3) THEN
        L=1
        PEAKER=-1.0D300
                DO K=1,OBJNY
                DO J=1,OBJNX
                DO L=1,3
        IF(IOBJECTV(J,K,L).GT.PEAKER) PEAKER=IOBJECTV(J,K,L)
                END DO
                END DO
                END DO
        IF(PEAKER.LE.255.0D0) PEAKER=1.0D0
        IF(PEAKER.GT.255.0D0) PEAKER=PEAKER/255.0D0
                DO K=1,OBJNY
                DO J=1,OBJNX
        BMPDATA24(L)=INT(((IOBJECTV(J,K,3))/PEAKER))+
     1  (256*INT(((IOBJECTV(J,K,1)))/PEAKER))+
     2  (256*256*INT(((IOBJECTV(J,K,2)))/PEAKER))

        L=L+1
                END DO
                END DO
        CALL MY_SYSTEM2('DEL PLOTBMP.BMP',15)
C        CALL IGrSaveImageData(TRIM(BMPFILE),BMPDATA24,OBJNX,OBJNY)
        CALL MY_SYSTEM2(TRIM(BMPREADR)//' PLOTBMP.BMP',80)
                        END IF

                        END IF
        IF(I.EQ.2) THEN
C       PLOT IMAGE
        IF(NUMCOLORS.EQ.1) THEN
        L=1
                DO K=1+INT(TRIMMER),IMGNY-INT(TRIMMER)
                DO J=1+INT(TRIMMER),IMGNX-INT(TRIMMER)
        INTERM=INT((IIMAGEV(J,K,1,1)))
        BMPDATA24(L)=INTERM+(256*INTERM)+(256*256*INTERM)
        L=L+1
                END DO
                END DO
        CALL MY_SYSTEM2('DEL PLOTBMP.BMP',15)
        PRINT *, "BMPDATA24 ", BMPDATA24
c        CALL IGrSaveImageData(TRIM(BMPFILE),BMPDATA24,
C     1  IMGNX-(2*INT(TRIMMER))
C     1 ,IMGNY-(2*INT(TRIMMER)))
        CALL MY_SYSTEM2(TRIM(BMPREADR)//' PLOTBMP.BMP',80)
                        END IF
        IF(NUMCOLORS.EQ.3) THEN
        L=1
101     format(1x, *(g0, ", "))
        OPEN(unit = 10, access = "sequential", action = "write",
     1  status = "replace", file = "data.csv", form = "formatted")
                DO K=1+INT(TRIMMER),IMGNY-INT(TRIMMER)
                DO J=1+INT(TRIMMER),IMGNX-INT(TRIMMER)
        BMPDATA24(L)=INT(((IIMAGEV(J,K,3,1))))+
     1  (256*INT(((IIMAGEV(J,K,1,1)))))+
     2  (256*256*INT(((IIMAGEV(J,K,2,1)))))
        rgb(1,J,K) = CHAR(INT(255*IIMAGEV(J,K,1,1)))
        rgb(2,J,K) = CHAR(INT(255*IIMAGEV(J,K,2,1)))
        rgb(3,J,K) = CHAR(INT(255*IIMAGEV(J,K,3,1)))
        WRITE(10, 101) BMPDATA24(L)
C        write(1, '(1x, F, 3(",", F))') a
C       CALL csv_write(lun=10, value=BMPDATA24(L), advance=.false. )
        L=L+1
                END DO
                END DO
C       call csv_write(lun=10, value=BMPDATA24(L), advance=.true. )
C        write(1, '(1x, F, 3(",", F))') BMPDATA24
        CLOSE(10)
        OPEN(unit = 10, access = "sequential", action = "write",
     1  status = "replace", file = "data2d.csv", form = "formatted")
        DO J=1,OBJNX
           WRITE(10, 101) IOBJECTV(J,:,1)
        END DO
        CLOSE(10)
        CALL MY_SYSTEM2('DEL PLOTBMP.BMP',15)
        PRINT *, "TRIMMER ", TRIMMER
        PRINT *, " J, K", J, " ", K
        call pixout(rgb,3)

C        CALL IGrSaveImageData(TRIM(BMPFILE),BMPDATA24,
C     1  IMGNX-(2*INT(TRIMMER))
C     1 ,IMGNY-(2*INT(TRIMMER)))
        CALL MY_SYSTEM2(TRIM(BMPREADR)//' PLOTBMP.BMP',80)
                        END IF

                        END IF
                        RETURN
                        END
        SUBROUTINE WRITEIMAGEARRAY(I,LENBMP,BMPFILE)

        USE GLOBALS
        IMPLICIT NONE
        INTEGER I,J,K,L
        INTEGER LENBMP
        INTEGER*1 INTERM
        INTEGER*4 BMPDATA24(1:LENBMP)
        REAL*8 PEAKER
        CHARACTER*80 BMPFILE
        INCLUDE 'DATMAI.INC'
        PRINT *, "WRITEIMAGEARRAY ROUTINE.."
        IF(I.EQ.1) THEN
C       PLOT OBJECT
        IF(NUMCOLORS.EQ.1) THEN
        PEAKER=-1.0D300
                DO K=1,OBJNY
                DO J=1,OBJNX
        IF(IOBJECTV(J,K,1).GT.PEAKER) PEAKER=IOBJECTV(J,K,1)
                END DO
                END DO
        IF(PEAKER.LE.255.0D0) PEAKER=1.0D0
        IF(PEAKER.GT.255.0D0) PEAKER=PEAKER/255.0D0
        L=1
                DO K=1,OBJNY
                DO J=1,OBJNX
        INTERM=INT((IOBJECTV(J,K,1)/PEAKER))
        BMPDATA24(L)=INTERM+(256*INTERM)+(256*256*INTERM)
        L=L+1
                END DO
                END DO
C        CALL IGrSaveImageData(TRIM(BMPFILE),BMPDATA24,OBJNX,OBJNY)
C       CALL MY_SYSTEM2(TRIM(BMPREADR)//' PLOTBMP.BMP',80)
                        END IF
        IF(NUMCOLORS.EQ.3) THEN
        L=1
        PEAKER=-1.0D300
                DO K=1,OBJNY
                DO J=1,OBJNX
                DO L=1,3
        IF(IOBJECTV(J,K,L).GT.PEAKER) PEAKER=IOBJECTV(J,K,L)
                END DO
                END DO
                END DO
        IF(PEAKER.LE.255.0D0) PEAKER=1.0D0
        IF(PEAKER.GT.255.0D0) PEAKER=PEAKER/255.0D0
                DO K=1,OBJNY
                DO J=1,OBJNX
        BMPDATA24(L)=INT(((IOBJECTV(J,K,3))/PEAKER))+
     1  (256*INT(((IOBJECTV(J,K,1)))/PEAKER))+
     2  (256*256*INT(((IOBJECTV(J,K,2)))/PEAKER))
        L=L+1
                END DO
                END DO
C        CALL IGrSaveImageData(TRIM(BMPFILE),BMPDATA24,OBJNX,OBJNY)
C       CALL MY_SYSTEM2(TRIM(BMPREADR)//' PLOTBMP.BMP',80)
                        END IF

                        END IF
        IF(I.EQ.2) THEN
C       PLOT IMAGE
        IF(NUMCOLORS.EQ.1) THEN
        L=1
                DO K=1,IMGNY
                DO J=1,IMGNX
        INTERM=INT((IIMAGEV(J,K,1,1)))
        BMPDATA24(L)=INTERM+(256*INTERM)+(256*256*INTERM)
        L=L+1
                END DO
                END DO
C        CALL IGrSaveImageData(TRIM(BMPFILE),BMPDATA24,IMGNX,IMGNY)
C       CALL MY_SYSTEM2(TRIM(BMPREADR)//' PLOTBMP.BMP',80)
                        END IF
        IF(NUMCOLORS.EQ.3) THEN
        L=1
                DO K=1,IMGNY
                DO J=1,IMGNX
        BMPDATA24(L)=INT(((IIMAGEV(J,K,3,1))))+
     1  (256*INT(((IIMAGEV(J,K,1,1)))))+
     2  (256*256*INT(((IIMAGEV(J,K,2,1)))))
        L=L+1
                END DO
                END DO
C        CALL IGrSaveImageData(TRIM(BMPFILE),BMPDATA24,IMGNX,IMGNY)
C       CALL MY_SYSTEM2(TRIM(BMPREADR)//' PLOTBMP.BMP',80)
                        END IF

                        END IF
                        RETURN
                        END

       SUBROUTINE bit42num(s,b)
         integer n      ! length of bit
         integer b      ! bit value
         character*4 s  ! string of bmp bit
         integer i,b1,b2
         n = 4
         b2 = 0
         do i = 1,n
            b1 = ichar(s(i:i))
            b1 = b1*256**(i-1)
            b2 = b2 + b1
         enddo
         b = b2
         return
      END SUBROUTINE


* --------------------------------------
* convert integer values to 4 8-bit characters
* --------------------------------------

       subroutine num2bit4(inum,byt4)
       implicit none
       integer inum
       character*4 byt4
       integer itmp1, itmp2
       itmp1 = inum
       itmp2 = itmp1 / 256**3
       byt4(4:4) = char(itmp2)
       itmp1 =-itmp2 * 256**3 +itmp1
       itmp2 = itmp1 / 256**2
       byt4(3:3) = char(itmp2)
       itmp1 =-itmp2 * 256**2 +itmp1
       itmp2 = itmp1 / 256
       byt4(2:2) = char(itmp2)
       itmp1 =-itmp2 * 256    +itmp1
       byt4(1:1) = char(itmp1)
       return
       end subroutine num2bit4

* --------------------------------------
* convert integer values to 2 8-bit characters
* --------------------------------------

       subroutine num2bit2(inum,byt2)
       implicit none
       integer inum
       character*2 byt2
       integer itmp1, itmp2
       itmp1 = inum
       itmp2 = itmp1 / 256
       byt2(2:2) = char(itmp2)
       itmp1 =-itmp2 * 256 + itmp1
       byt2(1:1) = char(itmp1)
       return
       end subroutine num2bit2


       subroutine pixout(rgb,nframe)
       implicit none
* interface arg.
       integer ihpixf, jvpixf
       parameter(ihpixf = 128, jvpixf = 128) ! pixel size, eacg must be multiple of 4, if BMP is chosen as output format.
       character*1 rgb(3,ihpixf,jvpixf)      ! RGB data array
       integer nframe
* local
       character*12 fnameout
       integer i, j, k
       integer itmp, icnt
       character*14 frmtstr
       character*54 headmsw
       character*4  byt4
       character*2  byt2
* choices
       integer ipixout
       parameter(ipixout = 3) ! 1 / 2 / other= PPM6, PPM3, BMP(24bit)

       fnameout = "tstBMP"

       if (ipixout .EQ. 1) then

* PPM P6

         write(fnameout,'(''smpl'',i3.3,''.ppm'')') nframe ! name of PPM file
         open(unit=2,file=fnameout,status='unknown')
         write(*,*) 'Now writing PPM (P6) file : ', fnameout
* header
         write(2,'(''P6'', 2(1x,i4),'' 255 '',$)')         ! some compiler may not accept this line.
     &     ihpixf, jvpixf
* image data
         itmp = ihpixf * jvpixf * 3
         write(frmtstr,'(''('',i8.8,''A,$)'')') itmp     ! make output "format"
         write(2,fmt=frmtstr)                              ! some compiler may not accept this line.
     &     (((rgb(k,i,j),k=1,3),i=1,ihpixf),j=jvpixf,1,-1) ! here, j (vertical address) runs from top to bottom.
         close(2)

       else if (ipixout .EQ. 2) then

* PPM P3 ! rather "safer" choice for many Fortran compiler(s).

         write(fnameout,'(''smpl'',i3.3,''.ppm'')') nframe ! name of PPM file
         open(unit=2,file=fnameout,status='unknown')
         write(*,*) 'Now writing PPM (P3) file : ', fnameout
* header
         write(2,'(A)') 'P3'
         write(2,'(2(1x,i4),'' 255 '')')  ihpixf, jvpixf
         icnt = 0
* image data
         do j = jvpixf, 1, -1                              ! here, j (vertical address) runs from top to bottom.
         do i = 1, ihpixf, 1
         do k = 1, 3
           itmp = ichar(rgb(k,i,j))
           icnt = icnt + 4
           if (icnt .LT. 60) then
             write(2,fmt='(1x,i3,$)') itmp                 ! "$" is not standard.
           else
             write(2,fmt='(1x,i3)') itmp
             icnt = 0
           endif
         enddo
         enddo
         enddo
         write(2,'(A)') ' '
         close(2)

       else

* BMP (24bit depth)... this part works only when width is multiple of 4.

         itmp = mod(ihpixf, 4)
         if (itmp .NE. 0) then
           write(*,*) 'width must be multiple of 4'
           stop
         endif

         write(fnameout,'(''smpl'',i3.3,''.bmp'')') nframe ! name of BMP file
         open(unit=2,file=fnameout,status='unknown')
         write(*,*) 'Now writing BMP(24bit) file : ', fnameout
* header 1 (file header ; 1--14 byte)
         headmsw( 1: 2) = 'BM'             ! declaring this is BMP file
         itmp = 54 + ihpixf * jvpixf * 3 ! total file size = header + data
         call num2bit4(itmp,byt4)
         headmsw( 3: 6) = byt4(1:4)
         itmp = 0                        ! may be 0
         call num2bit2(itmp,byt2)
         headmsw( 7: 8) = byt2(1:2)
         itmp = 0                        ! may be 0
         call num2bit2(itmp,byt2)
         headmsw( 9:10) = byt2(1:2)
         itmp = 54                       ! must be 54 : total length of header
         call num2bit4(itmp,byt4)
         headmsw(11:14) = byt4(1:4)
* header 2 (bit-map header ; 13--54 byte)
         itmp = 40                       ! must be 40 : length of bit-map header
         call num2bit4(itmp,byt4)
         headmsw(15:18) = byt4(1:4)
         itmp = ihpixf                   ! width
         call num2bit4(itmp,byt4)
         headmsw(19:22) = byt4(1:4)
         itmp = jvpixf                   ! height
         call num2bit4(itmp,byt4)
         headmsw(23:26) = byt4(1:4)
         itmp = 1                        ! must be 1
         call num2bit2(itmp,byt2)
         headmsw(27:28) = byt2(1:2)
         itmp = 24                       ! must be 24 : color depth in bit.
         call num2bit2(itmp,byt2)
         headmsw(29:30) = byt2(1:2)
         itmp = 0                        ! may be 0 : compression method index
         call num2bit4(itmp,byt4)
         headmsw(31:34) = byt4(1:4)
         itmp = 0                        ! may be 0 : file size if compressed
         call num2bit4(itmp,byt4)
         headmsw(35:38) = byt4(1:4)
         itmp = 0                        ! arbit. : pixel per meter, horizontal
         call num2bit4(itmp,byt4)
         headmsw(39:42) = byt4(1:4)
         itmp = 0                        ! arbit. : pixel per meter, vertical
         call num2bit4(itmp,byt4)
         headmsw(43:46) = byt4(1:4)
         itmp = 0                        ! may be 0 here : num. of color used
         call num2bit4(itmp,byt4)
         headmsw(47:50) = byt4(1:4)
         itmp = 0                        ! may be 0 here : num. of important color
         call num2bit4(itmp,byt4)
         headmsw(51:54) = byt4(1:4)

* writing header part
         write(2,'(a54,$)') headmsw(1:54)
* image data
         itmp = ihpixf * jvpixf * 3
         write(frmtstr,'(''('',i8.8,''A,$)'')') itmp
         write(2,fmt=frmtstr)
     &     (((rgb(k,i,j),k=3,1,-1),i=1,ihpixf),j=1,jvpixf) ! writing in BGR order, not RGB.
         close(2)

       endif

       return
       end subroutine pixout
