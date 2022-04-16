        SUBROUTINE READIMAGEARRAY(II,WRD1)
        USE WINTERACTER
        USE GLOBALS
        IMPLICIT NONE
        INTEGER I,J,K,L,INFO(3),LENGTH,NX,NY,II
        INTEGER IR,IG,IB,ALLOERR,IERROR
        REAL*8 WRD1
        INTEGER ABMPDATA24
        DIMENSION ABMPDATA24(:)
        CHARACTER*80 BMPFILE
        COMMON/FILEBMP/BMPFILE
        INCLUDE 'DATMAI.INC'
        ALLOCATABLE :: ABMPDATA24
        CALL IGrFileInfo(TRIM(BMPFILE),INFO,3)
        IF(II.EQ.1) THEN
        OBJNX=INFO(2)
        NX=INFO(2)
        OBJNY=INFO(3)
        NY=INFO(3)
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
        CALL IGrLoadImageData(TRIM(BMPFILE),ABMPDATA24)
        IERROR=INFOERROR(2)
        IF(IERROR.NE.0) THEN
        WRITE(OUTLYNE,*) 'ERROR OPENING',TRIM(BMPFILE)
        CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
                DO K=1,OBJNY
                DO J=1,OBJNX
        CALL WRGBsplit(ABMPDATA24(L),IR,IG,IB)
        IOBJECTV(J,K,1)=DBLE(IG)
        L=L+1
                END DO
                END DO
                        END IF
        IF(NUMCOLORS.EQ.3) THEN
        L=1
        IERROR=0
        CALL IGrLoadImageData(TRIM(BMPFILE),ABMPDATA24)
        IERROR=INFOERROR(2)
        IF(IERROR.NE.0) THEN
        WRITE(OUTLYNE,*) 'ERROR OPENING ',TRIM(BMPFILE)
        CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
                DO K=1,OBJNY
                DO J=1,OBJNX
        CALL WRGBsplit(ABMPDATA24(L),IR,IG,IB)
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
        CALL IGrLoadImageData(TRIM(BMPFILE),ABMPDATA24)
        IERROR=INFOERROR(2)
        IF(IERROR.NE.0) THEN
        IERROR=INFOERROR(1)
        WRITE(OUTLYNE,*) 'ERROR OPENING ',TRIM(BMPFILE)
        CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
                DO K=1,IMGNY
                DO J=1,IMGNX
        CALL WRGBsplit(ABMPDATA24(L),IR,IG,IB)
        IIMAGEV(J,K,1,1)=DBLE(IG)
        L=L+1
                END DO
                END DO
                        END IF
        IF(NUMCOLORS.EQ.3) THEN
        L=1
        IERROR=0
        CALL IGrLoadImageData(TRIM(BMPFILE),ABMPDATA24)
        IERROR=INFOERROR(2)
        IF(IERROR.NE.0) THEN
        WRITE(OUTLYNE,*) 'ERROR OPENING ',TRIM(BMPFILE)
        CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
                DO K=1,IMGNY
                DO J=1,IMGNX
        CALL WRGBsplit(ABMPDATA24(L),IR,IG,IB)
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
        USE WINTERACTER
        USE GLOBALS
        IMPLICIT NONE
        INTEGER I,J,K,L
        INTEGER LENBMP
        INTEGER*1 INTERM
        INTEGER*4 BMPDATA24(1:LENBMP)
        REAL*8 PEAKER,TRIMMER
        CHARACTER*80 BMPFILE
        INCLUDE 'DATMAI.INC'
        BMPFILE=TRIM('PLOTBMP.BMP')
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
        CALL IGrSaveImageData(TRIM(BMPFILE),BMPDATA24,OBJNX,OBJNY)
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
        CALL IGrSaveImageData(TRIM(BMPFILE),BMPDATA24,OBJNX,OBJNY)
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
        CALL IGrSaveImageData(TRIM(BMPFILE),BMPDATA24,
     1  IMGNX-(2*INT(TRIMMER))
     1 ,IMGNY-(2*INT(TRIMMER)))
        CALL MY_SYSTEM2(TRIM(BMPREADR)//' PLOTBMP.BMP',80)
                        END IF
        IF(NUMCOLORS.EQ.3) THEN
        L=1
                DO K=1+INT(TRIMMER),IMGNY-INT(TRIMMER)
                DO J=1+INT(TRIMMER),IMGNX-INT(TRIMMER)
        BMPDATA24(L)=INT(((IIMAGEV(J,K,3,1))))+
     1  (256*INT(((IIMAGEV(J,K,1,1)))))+
     2  (256*256*INT(((IIMAGEV(J,K,2,1)))))
        L=L+1
                END DO
                END DO
        CALL MY_SYSTEM2('DEL PLOTBMP.BMP',15)
        CALL IGrSaveImageData(TRIM(BMPFILE),BMPDATA24,
     1  IMGNX-(2*INT(TRIMMER))
     1 ,IMGNY-(2*INT(TRIMMER)))
        CALL MY_SYSTEM2(TRIM(BMPREADR)//' PLOTBMP.BMP',80)
                        END IF

                        END IF
                        RETURN
                        END
        SUBROUTINE WRITEIMAGEARRAY(I,LENBMP,BMPFILE)
        USE WINTERACTER
        USE GLOBALS
        IMPLICIT NONE
        INTEGER I,J,K,L
        INTEGER LENBMP
        INTEGER*1 INTERM
        INTEGER*4 BMPDATA24(1:LENBMP)
        REAL*8 PEAKER
        CHARACTER*80 BMPFILE
        INCLUDE 'DATMAI.INC'
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
        CALL IGrSaveImageData(TRIM(BMPFILE),BMPDATA24,OBJNX,OBJNY)
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
        CALL IGrSaveImageData(TRIM(BMPFILE),BMPDATA24,OBJNX,OBJNY)
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
        CALL IGrSaveImageData(TRIM(BMPFILE),BMPDATA24,IMGNX,IMGNY)
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
        CALL IGrSaveImageData(TRIM(BMPFILE),BMPDATA24,IMGNX,IMGNY)
C       CALL MY_SYSTEM2(TRIM(BMPREADR)//' PLOTBMP.BMP',80)
                        END IF

                        END IF
                        RETURN
                        END
