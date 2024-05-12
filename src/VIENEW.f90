module kdp_plot_gen
    use iso_fortran_env, only: real64
    ! These values are hard coded in many places.  This is an initial attempt
    ! to get that under control so I can eventually change them
    integer :: kdp_width = 10000 !10010
    integer :: kdp_height = 7000 !7010

    real(kind=real64) :: width_mid = 5000.0 !5000.0
    real(kind=real64) :: height_mid = 3500.0 ! 3500.0




end module

! Vie new new strawman
! error checking
! figure out scale factor
! scale xy size (10 units of canvas).  cairo will get this info when they read the data.  
! not sure if this is a problem or not.
! 

! SUB VIE.FOR
        SUBROUTINE VIE_NEW(CACOCHVIE)
          use global_widgets
          use handlers, only:  updateterminallog
          use mod_plotopticalsystem
          use ISO_FORTRAN_ENV, only: real64
          use DATLEN
          use DATMAC
!

        IMPLICIT NONE


!
!       THIS PROGRAM CONTROLS THE "VIE" AND "VIECO" COMMANDS
!
      REAL(real64) :: VIEROT,XVHI,XVLO,YVHI,YVLO, relAngle

      integer :: numRays, ii, jj, numFields

      INTEGER DFLAG,VIEXOF,VIEYOF
!
        COMMON/OFFVIE/VIEXOF,VIEYOF,VIEROT
!
      INTEGER COLPAS,VDF1,VDF2,VDF3,VS1,VS2 &
      ,VS3,I,J,CACOCHVIE
!
      CHARACTER VIEWQ*8
!
      REAL(real64) ::  VIEW1,VIEW2,VIEW3,MDX,MDY,SFI,GAMGAM
!
        INCLUDE 'DATMAI.INC'
        !INCLUDE 'DATLEN.INC'
        !INCLUDE 'DATMAC.INC'

!
        PRINT *, "VIE ROUTINE STARTING..."


        IF(DEVTYP.EQ.0.AND.VIEOVERLAY) VIEOVERLAY=.FALSE.
!       VIG OVERLAY ADDED 4/16/2004
        IF(.NOT.VIEOVERLAY) THEN
                        PLEXIS=.FALSE.
                        DEVTYP=0
                        GRASET=.FALSE.
                        END IF
!
!     W1 IS FOR SF
!     W2 IS FOR I
!     W3 IS FOR J
!
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
!       DF2 NOT 1, W2 EXPLICITLY ENTERED
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
!       DF3 NOT 1, W3 EXPLICITLY ENTERED
                        END IF
!
!     QUALIFIER WORDS:
!         XZ
!         YZ
!         XY
!         ORTHO
!
      !PRINT *, "W1 is for SF ", W1
      !PRINT *, "W2 is for i ", W2
      !PRINT *, "W3 is for j ", W3

      IF(SQ.EQ.0) THEN
                      SQ=1
                      WQ='YZ      '
                      END IF
        IF(WQ.NE.'XZ'.AND.WQ.NE.'YZ'.AND.WQ.NE.'XY'.AND. &
      WQ.NE.'ORTHO') THEN
      OUTLYNE= 'FOR "VIE" AND "VIECO"'
      CALL SHOWIT(1)
        OUTLYNE= &
      'THE ONLY VALID QUALIFIER WORDS ARE:'
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
      OUTLYNE= &
      'THE FIRST SURFACE # CAN NOT BE LESS THAN 0'
      CALL SHOWIT(1)
      OUTLYNE= 'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                      CALL MACFAL
                        RETURN
                        END IF
        IF(DF2.EQ.0.AND.W2.GT.SYSTEM(20)) THEN
      OUTLYNE= &
      'THE FIRST SURFACE CAN NOT BE BEYOND THE IMAGE SURFACE'
      CALL SHOWIT(1)
      OUTLYNE= 'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                      CALL MACFAL
                        RETURN
                        END IF
        IF(DF3.EQ.0.AND.W3.LT.0.0D0) THEN
      OUTLYNE= &
      'THE LAST SURFACE # CAN NOT BE LESS THAN 0'
      CALL SHOWIT(1)
      OUTLYNE= 'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                      CALL MACFAL
                        RETURN
                        END IF
        IF(DF3.EQ.0.AND.W3.GT.SYSTEM(20)) THEN
      OUTLYNE= &
      'THE LAST SURFACE CAN NOT BE BEYOND THE IMAGE SURFACE'
      CALL SHOWIT(1)
      OUTLYNE= 'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                      CALL MACFAL
                        RETURN
                        END IF
        IF(DF2.EQ.0.AND.DF3.EQ.0.AND.W2.GE.W3) THEN
      OUTLYNE= &
      'THE FIRST SURFACE MUST BE IN FRONT OF THE SECOND SURFACE'
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
!
!     DO A PLOT NEW
!
                        DEVTYP=1
      IF(WC.EQ.'VIE') OUTLYNE='"VIE" IS PROCESSING LENS DATA'
      IF(WC.EQ.'VIECO') OUTLYNE='"VIECO" IS PROCESSING LENS DATA'
      CALL SHOWIT(1)
      OUTLYNE='PLEASE WAIT...'
      CALL SHOWIT(1)

      !PRINT *, "SCALE FACTOR AT START OF VIE IS ", SCFAY
!
!******************************************
!
        IF(.NOT.VIEOVERLAY) THEN
!       NOT A VIE OVERLAY
                        !PRINT *, "VIEOVERLAY IS FALSE"
                        CALL PLTDEV1
                        GRASET=.TRUE.
!     DO A FRAME
                        CALL PLOTBOX
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='PLOT LI'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
                END IF
!
!******************************************
!
!     OFFSETS AND PLOT ROTATION
      IF(VIEXOF.NE.0) THEN
              SAVE_KDP(1)=SAVEINPT(1)
!             XSHIFT
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
!             XSHIFT
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
!             XSHIFT
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
!
!     SET UP THE SCALE FACTOR
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
!     SCALE FACTOR DONE
!
!     NOW SET LOOK VECTOR
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

!
!  GUT RAY
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='FOB'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
!
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
!

              SAVE_KDP(1)=SAVEINPT(1)
              ! Test comment out- this ray should get plotted later
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              !PRINT *, "LINE 2338, SCFAY IS ", SCFAY
              REST_KDP(1)=RESTINPT(1)
!
      
      !IF(VIEWQ.EQ.'YZ'.OR.VIEWQ.EQ.'ORTHO'.OR.VIEWQ.EQ.'XY') THEN
      IF(VIEWQ.EQ.'YZ') THEN
!     PLOT PROFY
              SAVE_KDP(1)=SAVEINPT(1)
              CALL VIEPROF(90.0D0, VIEW2, VIEW3, VS2, VS3, VDF2, VDF3)
              CALL VIE_EDGE('Y', VIEW2, VIEW3, VS2, VS3, VDF2, VDF3)
              REST_KDP(1)=RESTINPT(1)
              END IF
!
      !IF(VIEWQ.EQ.'XZ'.OR.VIEWQ.EQ.'ORTHO'.OR.VIEWQ.EQ.'XY') THEN
      IF(VIEWQ.EQ.'XZ') THEN
!     PLOT PROFX
              SAVE_KDP(1)=SAVEINPT(1)
              CALL VIEPROF(0.0D0, VIEW2, VIEW3, VS2, VS3, VDF2, VDF3)
              CALL VIE_EDGE('X', VIEW2, VIEW3, VS2, VS3, VDF2, VDF3)
              REST_KDP(1)=RESTINPT(1)
              END IF

      IF(VIEWQ.EQ.'XY'.OR.VIEWQ.EQ.'ORTHO') THEN
!     PLOT CLAP
              SAVE_KDP(1)=SAVEINPT(1)
!             PLOT CLAP
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

! Compute relative angles

      IF(.NOT.VIGOFF) THEN
       ! For any YZ Calls
       CALL VIGCAL(10,YVHI,YVLO,2)

      ELSE
        ! TODO:  Not sure this is the logic I want when VIGOFF
        YVHI=1.0D0
        YVLO=-1.0D0
      END IF
      ! For any XZ related calls
      IF(.NOT.VIGOFF) THEN
        CALL VIGCAL(10,XVHI,XVLO,1)
      ELSE
        XVHI=1.0D0
        XVLO=-1.0D0
      END IF


       numRays = ld_settings%num_field_rays
       numFields = sysConfig%numFields

!
!     OTHER RAYS
!
!     Y FIELDS OF VIEW ARE DONE WHEN VIEW IS YZ, XY OR ORTHO
      IF(VIEWQ.EQ.'YZ'.OR.VIEWQ.EQ.'ORTHO'.OR.VIEWQ.EQ.'XY') THEN

     !   CALL VIE_RSI(xF,yF,xA,yA, rW, VIEW2, VIEW3, VDF2, VDF3, VS2, VS3, CACOCHVIE, RAYEXT)

       do jj = 1, numFields

          SAVE_KDP(1)=SAVEINPT(1)
          COLRAY = sysConfig%fieldColorCodes(jj)
          WRITE(INPUT, *) "FOB ", sysConfig%relativeFields(2,jj)
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)

    do ii = 0, numRays-1
      relAngle = YVLO + ii*(YVHI-YVLO)/(numRays-1)
      SAVE_KDP(1)=SAVEINPT(1)
      call VIE_TRACERAY(0.0D0, relAngle, sysConfig%refWavelengthIndex, &
      & VIEW2, VIEW3, VDF2, VDF3, VS2, VS3, CACOCHVIE)
      REST_KDP(1)=RESTINPT(1)
  end do ! Angles
 end do  !  Fields

       END IF ! YZ plotting



 ! X Fields (if necessesary)
 IF(VIEWQ.EQ.'XZ'.OR.VIEWQ.EQ.'ORTHO'.OR.VIEWQ.EQ.'XY') THEN

       PRINT *, "Plotting X for VIEW ", VIEWQ

       do jj = 1, numFields

              SAVE_KDP(1)=SAVEINPT(1)
              COLRAY = sysConfig%fieldColorCodes(jj)
              WRITE(INPUT, *) "FOB ", sysConfig%relativeFields(2,jj) &
              & , ' ' , sysConfig%relativeFields(1,jj)
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)

        do ii = 0, numRays-1
          ! Note relAngle is X now not Y.
          relAngle = XVLO + ii*(XVHI-XVLO)/(numRays-1)
          SAVE_KDP(1)=SAVEINPT(1)
      call VIE_TRACERAY(relAngle, 0.0D0, sysConfig%refWavelengthIndex, &
      & VIEW2, VIEW3, VDF2, VDF3, VS2, VS3, CACOCHVIE)
      REST_KDP(1)=RESTINPT(1)
         end do ! Angles
        end do  !  Fields

END IF ! X plotting


!     PLOT SCALE FACTOR
!     DO A FRAME
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

subroutine VIEPROF(angProf, VIEW2, VIEW3, VS2, VS3, VDF2, VDF3)
  use ISO_FORTRAN_ENV, only: real64
  implicit none
  INTEGER VDF2,VDF3,VS2,VS3
  REAL(real64) ::  VIEW2,VIEW3
  REAL(real64) :: angProf


  include "DATMAI.INC"


!             PLOT PROF common vars
              W1=VIEW2
              W2=VIEW3
              W3=angProf
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


              CALL PLTPRO1

end subroutine

subroutine VIE_EDGE(xory, VIEW2, VIEW3, VS2, VS3, VDF2, VDF3)
  use ISO_FORTRAN_ENV, only: real64
  implicit none
  INTEGER VDF2,VDF3,VS2,VS3
  REAL(real64) ::  VIEW2,VIEW3
  character(len=1) :: xory

  include "DATMAI.INC"

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
              WQ='EDGE'//xory
              SQ=1
              STI=0
              SST=0
              CALL PLTEDG


end subroutine

subroutine VIE_TRACERAY(xA, yA, rW, VIEW2, VIEW3, VDF2, VDF3, VS2, VS3, CACOCHVIE)
  use ISO_FORTRAN_ENV, only: real64
  use DATLEN
  implicit none
  INTEGER VDF2,VDF3,VS2,VS3
  REAL(real64) ::  xA, yA, VIEW2,VIEW3
  integer :: rW, CACOCHVIE

  include "DATMAI.INC"
  !include "DATLEN.INC"

      WW1 = yA
      WW2=  xA
      WW3=  rW
      WVN=WW3
      MSG=.FALSE.
      WW4=1.0D0
      NOCOAT=.TRUE.
      GRASET=.TRUE.
        IF(CACOCHVIE.EQ.1) CACOCH=1
              CALL RAYTRA
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)


end subroutine

! SUB PLTRAE.FOR
SUBROUTINE PLTRAE
  USE GLOBALS
  use global_widgets, only: sysConfig
  use kdp_plot_gen
  use DATLEN
!
  IMPLICIT NONE
!
!       THIS ROUTINE DOES THE PLOT RAY COMMAND AT THE CMD LEVEL
!
REAL*8 X,Y,Z,XN,YN,ZN,ROT1X,ROT1Z,ROT2Y, &
ROT2Z,AX,AY,AZ,AALF,APHI,YMAXI,YMINI,XMAXI,XMINI &
,XNEW,YNEW,LKG,VIEPH,VIEAL
!
LOGICAL GGO
!
INTEGER IX,IY,I,IPST,COLPAS,STOPAT,J
!
  INCLUDE 'DATMAI.INC'
  !INCLUDE 'DATLEN.INC'
!
!     LOOK.VIEW TRANSFORMS

ROT1X(AX,AZ,APHI)=((AX*DCOS(APHI))-(AZ*DSIN(APHI)))
ROT1Z(AX,AZ,APHI)=((AX*DSIN(APHI))+(AZ*DCOS(APHI)))
!
ROT2Z(AZ,AY,AALF)=((AZ*DCOS(AALF))+(AY*DSIN(AALF)))
ROT2Y(AZ,AY,AALF)=((-AZ*DSIN(AALF))+(AY*DCOS(AALF)))
!     LNTYPE=0
LNTYPE=0
!
                  X=0.0D0
                  Y=0.0D0
!
!     DOES A RAY EXIST
IF(.NOT.REFEXT.AND..NOT.RAYEXT) THEN
IF(MSG) THEN
OUTLYNE='ERROR'
CALL SHOWIT(1)
OUTLYNE='NO REFERENCE RAY EXISTS'
CALL SHOWIT(1)
OUTLYNE='NO RAY EXISTS TO BE PLOTTED'
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
!     MAKE SURE YOU HAVE VERTEX DATA WITHOUT OFFSETS AND SET TO
!     THE FIRST SURFACE WITHOUT AN INFINITE THICKNESS
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
OUTLYNE='GENERATING RAY PLOTTING DATA...'
CALL SHOWIT(1)
                  END IF
!       CHECK SYNTAX
  IF(SST.EQ.1) THEN
  OUTLYNE= &
  '"PLOT RAY" TAKES NO STRING INPUT'
CALL SHOWIT(1)
  OUTLYNE='RE-ENTER COMMAND'
CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
                  END IF
  IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
  OUTLYNE= &
  '"PLOT RAY" ONLY TAKES NUMERIC WORDS #1 AND #2 INPUT'
CALL SHOWIT(1)
  OUTLYNE='RE-ENTER COMMAND'
CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
                  END IF
  IF(STI.EQ.1) THEN
           WRITE(OUTLYNE,800)
CALL SHOWIT(1)
800    FORMAT('QUERRY (?) HAS NO MEANING WITH "PLOT RAY"')
                  RETURN
                  ELSE
                  END IF
  IF(DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM(20)+W1
  IF(DF2.EQ.0.AND.W2.LT.0.0D0) W2=SYSTEM(20)+W2
!       DEFAULT VALUES
  IF(DF1.EQ.1) THEN
  IF(DABS(ALENS(3,0)).GT.1.0D10) THEN
                  W1=DBLE(1)
                  ELSE
                  W1=DBLE(0)
                  END IF
                  ELSE
!       DF1 NOT 1, W1 EXPLICITLY ENTERED
                  END IF
  IF(DF2.EQ.1) THEN
  IF(DABS(ALENS(3,(NEWIMG-1))).GT.1.0D10) THEN
                  W2=DBLE(NEWIMG-1)
                  ELSE
                  W2=DBLE(NEWIMG)
                  END IF
                  ELSE
!       DF2 NOT 1, W2 EXPLICITLY ENTERED
                  END IF
         STASUR=INT(W1)
         STPSUR=INT(W2)
IF(STASUR.LT.NEWOBJ) STASUR=NEWOBJ
IF(STPSUR.LT.NEWOBJ) STPSUR=NEWOBJ
!
  IF(INT(W1).LT.0) THEN
!       INVALID NUMERIC WORD #1
  OUTLYNE= &
  'SURFACE NUMBER (NUMERIC WORD #1) IS BEYOND LEGAL RANGE'
CALL SHOWIT(1)
  OUTLYNE='RE-ENTER COMMAND'
CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
                  END IF
  IF(INT(W2).GT.NEWIMG) THEN
!       INVALID NUMERIC WORD #2
  OUTLYNE= &
  'SURFACE NUMBER (NUMERIC WORD #2) IS BEYOND LEGAL RANGE'
CALL SHOWIT(1)
  OUTLYNE='RE-ENTER COMMAND'
CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
                  END IF
  IF(INT(W2).LE.INT(W1)) THEN
!       W2 LESS THAN OR EQUAL TO W1
  OUTLYNE= &
  'NUMERIC WORD #2 MUST BE GREATER THAN NUMERIC WORD #1'
CALL SHOWIT(1)
  OUTLYNE='RE-ENTER COMMAND'
CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
                  END IF
!       ALL INPUT IS OK, KEEP GOING
!       NOW WE HAVE THE REAL WORLD COORDINATES OF THE RAY TO BE PLOTTED
!       THEY ARE STORED IN THE ARRAY GLBRAY PASSED IN A COMMON BLOCK
!       VIA THE "INCLUDE 'DATLEN.INC'" STATEMENT.
!
!     HERE IS WHERE REAL WORK BEGINS. FIRST PROJECT THE 3D WORLD COORDINATES
!     OF THE VERTICIES ONTO THE 2D PLANE INDICATED BY THE PLOT LOOK/ PLOT VIEW
!     DATA. THE DIRECTION COSINES OF THE LOOK VECTOR ARE LOOKX, LOOKY AND
!     LOOKZ. THE TRANSFORMATIONS ARE:

! This is probably not the best solution, but the original
! Code did not plot collimated rays from the first surface.
! To fix this propogate back from the first surface to the leftmos
! surface
! JN 10/6/23

if (sysConfig%isObjectAfInf()) THEN
   GLPRAY(3,0) = MINVAL(GLPRAY(3,1:STPSUR))-2.0
   GLPRAY(2,0) = GLPRAY(2,1) - &
   (GLPRAY(3,1)-GLPRAY(3,0))*TAN(ACOS(GLPRAY(9,1)))

   PRINT *, "GLPRAY min Z is ", GLPRAY(3,0)
end if
!GLPRAY(2,2)+GLPRAY(3,1)*GLPRAY(9,1)
!
CALL ROT1
!
!     AFTER THIS TRANSFORMATION, THE WORLD X COORDINATE IS PLOTTED IN THE
!     HORIZONTAL X-AXIS OF THE DISPLAY AND THE Y WORLD COORDINATE IS PLOTTED
!     IN THE VERTICAL Y-AXIS OF THE DISPLAY AFTER A CONVERSION TO
!     DEVICE INDEPENDENT COORDINATES ARE MADE.
!
CALL PLTSC1(XMINI,XMAXI,YMINI,YMAXI)
!
!     HERE IS THE LOOP WHICH PLOTS THE RAYS USING CALLS TO
!     PENMV1A(IX,IY,IPST)
!
IF(STPSUR.GE.STOPAT) STPSUR=STOPAT
IF(STASUR.LT.NEWOBJ) STASUR=NEWOBJ
IF(STPSUR.LT.NEWOBJ) STPSUR=NEWOBJ
!     HERE IS WHERE THE CODE GOES TO PLOT THE RAYS INSIDE THE NSS SURFACE
         IF(STPSUR.LT.STASUR) STPSUR=STASUR
                  DO I=STASUR,STPSUR
IF(I.GE.1) THEN

IF(NUMHITS(I-1).GT.1) THEN
!     THERE ARE NSS RAY POINTS TO PLOT BEFORE PLOTTING TO THE RAY COORDINATE
!     AT SURFACE I
                          DO J=2,NUMHITS(I-1)
!
!     PLOT TO THE RAY COORDINATE AT SURFACE I
X=GLOBAL_MULTIRAY_DATA(1,I-1,J)
Y=GLOBAL_MULTIRAY_DATA(2,I-1,J)
Z=GLOBAL_MULTIRAY_DATA(3,I-1,J)

X=X-XROT
Y=Y-YROT
Z=Z-ZROT
XN=ROT1X(X,Z,VIEPH)
YN=Y
ZN=ROT1Z(X,Z,VIEPH)
X=XN
Y=YN
Z=ZN
!
ZN=ROT2Z(Z,Y,VIEAL)
YN=ROT2Y(Z,Y,VIEAL)
XN=X
X=XN
Y=YN
Z=ZN
!
!     RIGHT NOW,COORDINATES ARE IN WORLD COORDINATES
!     CONVERT THEM TO DEVICE INDEPENDENT GRAPHICS COORDINATES IN
!     TWO STEPS.
!
!     THE WORLD X PLOTS TO THE PLOTTER X
!     THE WORLD Y PLOTS TO THE PLOTTER Y
!
!     STEP 1: CONVERT USING AN APPROPRIATE SCALE FACTOR
!               CALCULATING AN APPROPRIATE FACTOR IF NECESSARY
!
Y=(Y/SCFAY)*1000.0D0
!
X=(X/SCFAX)*1000.0D0
!
!     STEP 2: APPLY THE SCREEN Y-OFFSET AND SCREEN GAMMA ROTATION
!
!     APPLY THE PLOT LOOK/VIEW Y-OFFSET VALUE
!
IF(LORIENT) CALL ORSHIFT
!Y=Y+3500.0D0+DBLE(PYSHFT)
Y=Y+height_mid+DBLE(PYSHFT)

X=X+DBLE(PXSHFT)
!
!     IF THERE ARE X-OFFSETS (JUSTIFICATION) TO APPLY, DO THEM HERE.
!
IF(RCL.EQ.1.OR.RCL.EQ.-1) THEN
JUSOFF=500.0D0-((XMINI/SCFAX)*1000.0D0)
                  RCL=-1
                  END IF
IF(RCL.EQ.2.OR.RCL.EQ.-2) THEN
                  RCL=-2
                  !JUSOFF=5000.0D0
                  JUSOFF=width_mid
                  END IF
IF(RCL.EQ.3.OR.RCL.EQ.-3) THEN
JUSOFF=9500.0D0-((XMAXI/SCFAX)*1000.0D0)
                  RCL=-3
                  END IF
!
X=X+JUSOFF
!
!     IF THERE IS A NON-ZERO GAMMA SPECIFIED IN PLOT LOOK OR PLOT VIEW THEN
!     ROTATE IN GAMMA ON THE SCREEN ABOUT THE CENTER OF THE SCREEN
!     WHICH HAS COORDINATES X=5000.0D0,Y=3500.0D0
!
!     FIRST SHIFT THE COORDINATE ORIGIN TO THE CETER OF THE DISPLAY
!
          !X=X-5000.0D0
          X=X-width_mid
          !Y=Y-3500.0D0
          Y=Y-height_mid

!     THE SCREEN COORDINATE IN REAL*8 IN THE SHIFTED COORDINATE
!     FRAME IS NOW X AND Y

IF(DBLE(PGAMMA).NE.0.0D0) THEN
LKG=(PII/180.0D0)*DBLE(PGAMMA)
          XNEW=((X*DCOS(LKG))-(Y*DSIN(LKG)))
          YNEW=((X*DSIN(LKG))+(Y*DCOS(LKG)))
          X=XNEW
          Y=YNEW
                  END IF

!     THE ROTATION IS DONE, NOW SHIFT THE ORIGIN BACK TO THE BOTTOM
!     LEFT HAND CORNER
                  !X=X+5000.0D0
                  X=X+width_mid

                  !Y=Y+3500.0D0
                  Y=Y+height_mid
!     MOVE TO NEW POSITION WITH LOWERED PEN
  IX=INT(X)
  IY=INT(Y)
P1ARAY(I-1,1,J+1)=IX
P1ARAY(I-1,2,J+1)=IY
P1ARAY(I-1,3,J+1)=1
  IF(.NOT.PLEXIS) PLEXIS=.TRUE.
                  END DO
                  END IF
                  END IF
!     PLOT TO THE RAY COORDINATE AT SURFACE I
X=GLPRAY(1,I)
Y=GLPRAY(2,I)
Z=GLPRAY(3,I)
X=X-XROT
Y=Y-YROT
Z=Z-ZROT
XN=ROT1X(X,Z,VIEPH)
YN=Y
ZN=ROT1Z(X,Z,VIEPH)
X=XN
Y=YN
Z=ZN
!
ZN=ROT2Z(Z,Y,VIEAL)
YN=ROT2Y(Z,Y,VIEAL)
XN=X
X=XN
Y=YN
Z=ZN
!
!     RIGHT NOW,COORDINATES ARE IN WORLD COORDINATES
!     CONVERT THEM TO DEVICE INDEPENDENT GRAPHICS COORDINATES IN
!     TWO STEPS.
!
!     THE WORLD X PLOTS TO THE PLOTTER X
!     THE WORLD Y PLOTS TO THE PLOTTER Y
!
!     STEP 1: CONVERT USING AN APPROPRIATE SCALE FACTOR
!               CALCULATING AN APPROPRIATE FACTOR IF NECESSARY
!
Y=(Y/SCFAY)*1000.0D0
!
X=(X/SCFAX)*1000.0D0
!
!     STEP 2: APPLY THE SCREEN Y-OFFSET AND SCREEN GAMMA ROTATION
!
!     APPLY THE PLOT LOOK/VIEW Y-OFFSET VALUE
!
IF(LORIENT) CALL ORSHIFT
!Y=Y+3500.0D0+DBLE(PYSHFT)
Y=Y+height_mid+DBLE(PYSHFT)
X=X+DBLE(PXSHFT)
!
!     IF THERE ARE X-OFFSETS (JUSTIFICATION) TO APPLY, DO THEM HERE.
!
IF(RCL.EQ.1.OR.RCL.EQ.-1) THEN
JUSOFF=500.0D0-((XMINI/SCFAX)*1000.0D0)
                  RCL=-1
                  ELSE
                  END IF
IF(RCL.EQ.2.OR.RCL.EQ.-2) THEN
                  RCL=-2
                  !JUSOFF=5000.0D0
                  JUSOFF=width_mid
                  ELSE
                  END IF
IF(RCL.EQ.3.OR.RCL.EQ.-3) THEN
JUSOFF=9500.0D0-((XMAXI/SCFAX)*1000.0D0)
                  RCL=-3
                  ELSE
                  END IF
!
X=X+JUSOFF
!
!     IF THERE IS A NON-ZERO GAMMA SPECIFIED IN PLOT LOOK OR PLOT VIEW THEN
!     ROTATE IN GAMMA ON THE SCREEN ABOUT THE CENTER OF THE SCREEN
!     WHICH HAS COORDINATES X=5000.0D0,Y=3500.0D0
!
!     FIRST SHIFT THE COORDINATE ORIGIN TO THE CETER OF THE DISPLAY
!
          !X=X-5000.0D0
          !Y=Y-3500.0D0
          X=X-width_mid
          Y=Y-height_mid          
!     THE SCREEN COORDINATE IN REAL*8 IN THE SHIFTED COORDINATE
!     FRAME IS NOW X AND Y

IF(DBLE(PGAMMA).NE.0.0D0) THEN
LKG=(PII/180.0D0)*DBLE(PGAMMA)
          XNEW=((X*DCOS(LKG))-(Y*DSIN(LKG)))
          YNEW=((X*DSIN(LKG))+(Y*DCOS(LKG)))
          X=XNEW
          Y=YNEW
                  ELSE
                  END IF

!     THE ROTATION IS DONE, NOW SHIFT THE ORIGIN BACK TO THE BOTTOM
!     LEFT HAND CORNER
                  !X=X+5000.0D0
                  !Y=Y+3500.0D0
                  X=X+width_mid
                  Y=Y+height_mid                  
IF(I.EQ.STASUR) THEN
!     PUT INSTRUCTIONS IN P1ARAY TO LIFT PEN, GO TO STARTING POINT
IPST=0
          IX=INT(X)
          IY=INT(Y)
!
P1ARAY(I,1,1)=IX
P1ARAY(I,2,1)=IY
P1ARAY(I,3,1)=0
  IF(.NOT.PLEXIS) PLEXIS=.TRUE.
          ELSE
          END IF
IF(I.GT.STASUR) THEN
!     PUT INSTRUCTIONS IN P1ARAY TO DROP PEN AND DRAW
IPST=1
IX=INT(X)
IY=INT(Y)
!     IF RAY IS VIRTUAL AND NOVIRT IS TRUE, LIFT PEN ANYWAY
IF(NOVIRT.AND.GLVIRT(I).AND.I.NE.STPSUR.AND.DF3.EQ.1) THEN
!     LEAVE PEN ALONE AT LAST POSITION WITH PEN UP
IPST=0
IF(I.NE.0) P1ARAY(I,1,1)=P1ARAY(I-1,1,1)
IF(I.NE.0) P1ARAY(I,2,1)=P1ARAY(I-1,2,1)
             ELSE
!     MOVE TO NEW POSITION WITH LOWERED PEN
P1ARAY(I,1,1)=IX
P1ARAY(I,2,1)=IY
IPST=1
             END IF
P1ARAY(I,3,1)=IPST
  IF(.NOT.PLEXIS) PLEXIS=.TRUE.
          END IF
!     IF FINISHED, LIFT PEN
IF(I.EQ.STPSUR) IPST=0
                  END DO
!
!     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE P1ARAY ARRAY
!

COLPAS=COLRAY
CALL MY_COLTYP(COLPAS)
  FIXUP=.FALSE.
                  DO I=STASUR,STPSUR
                  IF(I.GE.1) THEN
                    IF(NUMHITS(I-1).GT.1) THEN
!                       PLOT NSS RAY DATA FIRST
                  DO J=2,NUMHITS(I-1)
CALL PENMV1A( &
P1ARAY(I-1,1,J+1),P1ARAY(I-1,2,J+1),P1ARAY(I-1,3,J+1))
                  END DO
                  END IF
                  END IF
IF(I.EQ.STASUR) &
CALL PENMV1A(P1ARAY(I,1,1),P1ARAY(I,2,1),P1ARAY(I,3,1))
IF(I.GT.STASUR) THEN
               GGO=.TRUE.
!     TEST IF LAST POINT IS NEW POINT, IF SO DON'T MOVE
IF(P1ARAY(I,1,1).EQ.P1ARAY(I-1,1,1).AND.P1ARAY(I,2,1).EQ. &
P1ARAY(I-1,2,1)) GGO=.FALSE.
IF(P1ARAY(I,3,1).NE.0.AND.GGO) &
CALL PENMV1A(P1ARAY(I,1,1),P1ARAY(I,2,1),P1ARAY(I,3,1))
                 END IF
                  END DO
! Add a Pen up move here to force drawing of last ray in correct c
CALL PENMV1A(0,0,0)
COLPAS=COLLBL
CALL MY_COLTYP(COLPAS)
NORAYPLOT=.FALSE.
!     HERE WE DO THE PLOT AXIS DRAWING
                  IF(.NOT.VIGFLG.AND.PLTVIG) THEN
                  CALL VIGSHO
                  VIGFLG=.TRUE.
                  ELSE
                  END IF
                  RETURN
                  END

! SUB VIERAY.FOR
      SUBROUTINE VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
      implicit none
      REAL*8 VIEW2,VIEW3
      INTEGER VDF2,VDF3,VS2,VS3
      INCLUDE 'DATMAI.INC'
      !INCLUDE 'DATLEN.INC'
!             PLOT THE RAY
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
              WQ='RAY'
              SQ=1
              STI=0
              SST=0
              CALL PLTRAE
              RETURN
              END

! SUB PLTDEV1.FOR
        SUBROUTINE PLTDEV1
        USE GLOBALS
        USE NSSMOD
        use DATLEN, only: DEVTYP, GLOBE, GRASET, SCFAX, SCFAY

        IMPLICIT NONE
!
!       THIS ROUTINE DOES THE "PLOT NEW" COMMAND AT THE CMD
!       AND SPECT PROGRAM LEVELS
!
!
      LOGICAL USCALEX,USCALEY
      COMMON/SCALEU/USCALEX,USCALEY
      REAL YL,YU
      COMMON/YLYU/YL,YU
      REAL XL,XU
      COMMON/XLXU/XL,XU
!
        INCLUDE 'DATMAI.INC'
        !INCLUDE 'DATLEN.INC'
        INCLUDE 'DATHGR.INC'
!
!       THIS ROUTINE SETS GRASET AND DEVTYP
      OUTLYNE = "PLTDEV1 ROUTINE"
      CALL SHOWIT(19)
      GLOBE=.FALSE.
      CALL PLTRST1
      XL=0.0
      XU=0.0
      YL=0.0
      YU=0.0
                        DEVTYP=1
                        GRASET=.TRUE.
                       USCALEX=.FALSE.
                       USCALEY=.FALSE.
                        SCFAX=0.0D0
                        SCFAY=0.0D0
                        NSSSCFA=0.0D0
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
                        CALL PSTART
                        RETURN
                        END              

        SUBROUTINE PSTART
          use kdp_plot_gen
          use DATLEN, only: COLBAC, DEVTYP, PLEXIS, PPLI, LI
!
        IMPLICIT NONE
!
!       THIS PROGRAM CONTROLS THE ALL PLOTTING INITIALIZATION
!
        CHARACTER BLANK*80
!
      INTEGER LLX,LLY,URX,URY,COLBACC
      COMMON/VIEWER/LLX,LLY,URX,URY
!
        INCLUDE 'DATMAI.INC'
        !INCLUDE 'DATLEN.INC'
        INCLUDE 'DATHGR.INC'
!
        BLANK=AA//AA//AA//AA
!
        PLEXIS=.FALSE.
        IF(DEVTYP.EQ.1) THEN
!       PROCEED
                        PPLI(1:60)=LI(1:60)
!   WINTER
!        CALL IGRLINEWIDTH(1,1,.1)
                        CALL MY_INIPLT
!
      LLX=-10
      LLY=-10
      URX=kdp_width
      URY=kdp_height
!
!
!     LIGHT BLUE (DEFAULT) BACKGROUND
!     DOES IT NEED TO CHANGE ?
!     WHITE BACKGROUND         COLBAC=0
!     LIGHT YELLOW BACKGROUND  COLBAC=1
!     LIGHT MAGENTA BACKGROUND COLBAC=2
!     LIGHT RED BACKGROUND     COLBAC=3
!     LIGHT CYAN BACKGROUND    COLBAC=4
!     LIGHT GREEN BACKGROUND   COLBAC=5
!     LIGHT BLUE BACKGROUND    COLBAC=6
!     DARK GREY BACKGROUND     COLBAC=7
!     LIGHT GREY BACKGROUND    COLBAC=8
!     DARK YELLOW BACKGROUND   COLBAC=9
!     DARK MAGENTA BACKGROUND  COLBAC=10
!     DARK RED BACKGROUND      COLBAC=11
!     DARK CYAN BACKGROUND     COLBAC=12
!     DARK GREEN BACKGROUND    COLBAC=13
!     DARK BLUE BACKGROUND     COLBAC=14
!     BLACK BACKGROUND         COLBAC=15
      COLBACC=COLBAC
!     RESET THE BACKGROUND COLOR AND THE FONT STYLE
      CALL MY_SETPAL(COLBACC)
      CALL MY_SETFONT(1,0)
                        RETURN
                        ELSE
!       PROCEED
                        END IF
!       NOT A VALID DEVICE TYPE
      OUTLYNE= &
      '"PLOT NEW" MUST BE ISSUED BEFORE PLOTTING CAN PROCEED'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END
      SUBROUTINE MY_INIPLT
      USE GLOBALS
      USE zoa_ui
      IMPLICIT NONE
      INTEGER ALLOERR
      INTEGER I1,I2,I3,I4,I5,I6,I7,I8
      CHARACTER STRINGER*1,NEUTLINE*42
      INCLUDE 'DATHGR.INC'
      INCLUDE 'DATMAI.INC'
      CALL CLOSE_FILE(28,0)
      DEALLOCATE(NEUTARRAY,STAT=ALLOERR)
!
      CALL MY_DEL_NEUT
      ALLOCATE(NEUTARRAY(1:MAXNEUTRAL),STAT=ALLOERR)
      PRINT *, "MY_INIPLT CALLED"
      PRINT *, "WQ = ", WQ
!
!     NOW OPEN NEW FILE NEUTRAL.DAT FOR OUTPUT, IT IS CURRENTLY EMPTY
!
      OPEN(UNIT=28 &
      ,FILE=trim(basePath)//'NEUTRAL.DAT' &
      ,FORM='UNFORMATTED',ACCESS='DIRECT' &
      ,RECL=(NRECL*42),STATUS='REPLACE')
!      call OPEN_NEUTRAL_DAT(fNeut)
      I1=0
      I2=0
      I3=0
      I4=0
      I5=0
      I6=0
      I7=0
      I8=0
      IF(I1.GT.99999) I1=99999
      IF(I2.GT.99999) I2=99999
      IF(I3.GT.99999) I3=99999
      IF(I4.GT.99999) I4=99999
      IF(I5.GT.99999) I5=99999
      IF(I6.GT.99999) I6=99999
      IF(I7.GT.99999) I7=99999
      IF(I8.GT.99999) I8=99999
      IF(I1.LT.-9999) I1=-9999
      IF(I2.LT.-9999) I2=-9999
      IF(I3.LT.-9999) I3=-9999
      IF(I4.LT.-9999) I4=-9999
      IF(I5.LT.-9999) I5=-9999
      IF(I6.LT.-9999) I6=-9999
      IF(I7.LT.-9999) I7=-9999
      IF(I8.LT.-9999) I8=-9999
      NEUTTOTAL=1
      STRINGER='A'
      WRITE(NEUTLINE,1000) STRINGER &
      ,I1,I2,I3,I4,I5,I6,I7,I8
      IF(NEUTTOTAL+1.GE.MAXNEUTRAL/2) &
      CALL RESIZE_NEUT
      NEUTARRAY(NEUTTOTAL+1)=NEUTLINE
 1000 FORMAT(A1,I5,I5,I5,I5,I5,I5,I5,I5)
                        RETURN
                        END

      SUBROUTINE PLOTBOX
        use kdp_plot_gen
        USE GLOBALS
        use DATLEN, only: COLFRM
        implicit none
!     DOES THE BIG FRAME AROUND PLOTS
      INCLUDE 'DATHGR.INC'
      !INCLUDE 'DATLEN.INC'
      
      INTEGER COLPAS

      PRINT *, "RUNNING PLOTBOX ROUTINE"
                IF(FRAME) THEN
!     LIFT PEN, MOVE TO FRAME START
      COLPAS=COLFRM
      CALL MY_COLTYP(COLPAS)
        CALL MY_PLOT(0,0,0,0,-10,kdp_width,-10,kdp_height)
!     DROP PEN, DRAW BOX
        CALL MY_PLOT(10000,0,1,0,-10,kdp_width,-10,kdp_height)
        CALL MY_PLOT(10000,7000,1,0,-10,kdp_width,-10,kdp_height)
        CALL MY_PLOT(0,7000,1,0,-10,kdp_width,-10,kdp_height)
        CALL MY_PLOT(0,0,1,0,-10,kdp_width,-10,kdp_height)
                       END IF
                       RETURN
    END                        

! SUB PLTRST1.FOR
    SUBROUTINE PLTRST1
      use kdp_plot_gen
      use DATLEN

      !
      IMPLICIT NONE
!
!       THIS ROUTINE RESETS PLOT PARAMETERS TO STARTING VALES
!       AFTER PLOT DEV, PLOT SCREEN OR PLOT PRINTER
!
    INTEGER I,FANWAV
!
    INTEGER JK_WAV(1:10)
!
      REAL*8 VIEROT
!
    INTEGER VIEXOF,VIEYOF
!
      COMMON/OFFVIE/VIEXOF,VIEYOF,VIEROT
!
    COMMON/WAVER/JK_WAV
!
    COMMON/FANNER/FANWAV
!
    LOGICAL FANEXT,RIM
!
    COMMON/RINSHT/RIM
!
    COMMON/FANEXI/FANEXT
!
    CHARACTER FANNAM*8,FANQAL*8,DRWNAM*11
!
    COMMON/DRWTAG/DRWNAM
!
    COMMON/PASFAN/FANNAM,FANQAL
!
    INTEGER XTENT,YTENT,CLIP
!
    COMMON/USEFAN/XTENT,YTENT,CLIP
!
      INCLUDE 'DATMAI.INC'
      !INCLUDE 'DATLEN.INC'
      INCLUDE 'DATSPD.INC'
      INCLUDE 'DATHGR.INC'
!
!     USER-DEFINED FAN PLOTTING SCALE FACTOR
!     DEFAULT MEANS PROGRAM CALCULATES AUTOSCALE
!       RESET 3D LINE VALUES
      LX1=0.0D0
      LY1=0.0D0
      LZ1=0.0D0
      LX2=0.0D0
      LY2=0.0D0
      LZ2=0.0D0
!       NORAYPLOT
      NORAYPLOT=.TRUE.
!
                      FSSIFLG=.FALSE.
                      FSSI=0.0D0
                      FANAXX=INT(width_mid)!5000
                      FANAXY=INT(height_mid)!3500
                      XTENT=2500
                      YTENT=2000
                      CLIP=0
!
    CALL MY_DEL_NEUT
!     RESET PLOTNAME TO NEUTRAL.DAT
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
!     DEFAULT PEN COLOR IS BLACK
              COLDEF=15
!
!     DEFAULT PLOT ORIGIN IS IN THE LOWER LEFT CORNER AT Y=0, X=0
!     (BOTH INTEGER)
      ORX=0
      ORY=0
!
!     DEFAULT PEN POSITIONS ARE 0 AND 0 BUT ARE NEVER USED
!     SINCE PENSTA BY DEFAULT IS 0 (BOTH INTEGER)
      XPEN=0
      YPEN=0
!
!     PLOTTING LOOK VECTOR
!         DRALOK=.FALSE.
!      LOKFLG=.FALSE.
!
!     PLOTTING VIG MESSAGE
      PLTVIG=.FALSE.
    VIGFLG=.FALSE.
!
!     PLOTTING VIEW ANGLES
      DRAVUE=.FALSE.
    VUEFLG=.FALSE.
!
!     DEFAULT OLD X-PEN POSITION IS 0 (INTEGER)
      XPENOL=0
!
!     DEFAULT OLD Y-PEN POSITION IS 0 (INTEGER)
      YPENOL=0
!
!     THE DEFAULT PEN STATUS IS 3, PEN STATUS NOT SET TO A USEFUL VALUE
      PENSTA=3
!
!     DEFAULT LINE TYPE IS 0, A SOLID LINE
      LNTYPE=0
    OLLNTP=0
!
!     DEFAULT SYMBOL TYPE IS 8, AN ASTERIX
      SYMB=8
!
!     DEFAULT CHARACTER SIZE FOR SYMBOL IS 1
      SYMSIZ=1
!
!     DEFAULT CHARACTER SIZE FOR LABLE IS 1
      LABSIZ=1
!
!     DEFAULT CHARACTER SIZE FOR NOTES IS 1
      NTSIZ=1
!
!     DEFAULT ANGLE FOR SYMBOL PLOTTING IS 0 DEGREES (INTEGER)
      SYMANG=0
!
!     DEFAULT ANGLE FOR LABLE PLOTTING IS 0 DEGREES (INTEGER)
      LABANG=0
!
!     DEFAULT ANGLE FOR NOTE PLOTTING IS 0 DEGREES (INTEGER)
      NTANG=0
!
!     DEFAULT Y-DIRECTION COSINE OF PLOT LOOK IS 0.0
!        LOOKY=0.0D0
!
!     DEFAULT X-DIRECTION COSINE OF PLOT LOOK IS -1.0
!        LOOKX=-1.0D0
!
!     DEFAULT Z-DIRECTION COSINE OF PLOT LOOK IS 0.0
!        LOOKZ=0.0D0
!
!     SET XSHIFT,YSHIFT AND ZSHIFT
    PXSHFT=0
    PYSHFT=0
    PGAMMA=0
    LORIENT=.FALSE.
!
!     DEFAULT "ALPHA" FOR PLOT VIEW IS 0.0 DEGREES
!       VIEALF=0.0D0
!
!     DEFAULT "PHI" FOR PLOT VIEW IS 270.0 DEGREES
!       VIEPHI=270.0D0
!
!     BY DEFAULT, GRAPHICS IS SET "ON"
      GRASET=.TRUE.
!
!     DON'T REST DUMMY SURFACES PLOTTED AS DASHED LINES. LEAVE AS SET
!
!     NOW THE FLAGS TO TRACK IF THINGS HAVE BEEN DONE ONCE
!
!     FLAG TO SEE IF "LB" HAS BEEN PLOTTED
!     BY DEFAULT SET TO FALSE
              LBLFLG=.FALSE.
              PLTLBL=.FALSE.
!
!     FLAG TO SEE IF "LI" HAS BEEN PLOTTED
!     BY DEFAULT SET TO FALSE
              LIFLG=.FALSE.
              PLTLLI=.FALSE.
!
!     FLAG TO SEE IF "AXIS" SHOULD BE PLOTTED
!     BY DEFAULT SET TO FALSE
              PLTAXS=.FALSE.
!
!     FLAG TO SEE IF "AXIS" HAS BEEN PLOTTED
!     BY DEFAULT SET TO FALSE
              AXFLG=.FALSE.
!
!     MAX AND MIN DEVICE INDEPENDENT COORDINATES
              XXMAX=kdp_width
              XXMIN=-10
              YYMAX=kdp_height
              YYMIN=-10
    XPEN=0
    YPEN=0
    XPENOL=0
    YPENOL=0
    PENSTA=3
!
!     FLAG TO SEE IF "LABLE" HAS BEEN PLOTTED
!     BY DEFAULT SET TO FALSE
              LBLFLG=.FALSE.
!
!     RIGHT,CENTER AND LEFT PLOT JUSTIFICATION FLAG FOR PLOTS
!     OF THE LENS DATA
!
!     RCL=1 MEANS LEFT JUSTIFY
!     RCL=2 MEANS CENTER JUSTIFY(DEFAULT VALUE)
!     RCL=3 MEANS RIGHT JUSTIFY
              RCL=-2
              JUSOFF=width_mid
!
              ROTSET=.FALSE.
              XROT=0.0D0
              YROT=0.0D0
              ZROT=0.0D0
!
!
!     PLOT SCALE STUFF
!
!     BY DEFAULT, AUTOSCALE SHOULD BE DONE
      AUTSL=.TRUE.
!
!     PLOT SCALE SHOULD OR SHOUD NOT BE DRAWN
      DRASCL=.FALSE.
!
!     PLOT SCALE IS NOT EXPLICITLY SET
      PLSC=.FALSE.
!
!     PLOT SCALE HAS NOT BEEN DRAWN YET
      SCFLG=.FALSE.
!
!     FLAG TO SEE IF AN AUTO-SCALE FACTOR HAS BEEN CALCULATED
!     BY DEFAULT SET TO FALSE
              ASCFLG=.FALSE.
!
    SCFAY=1.0D0
    SCFAX=1.0D0
    SCFAYP=1.0D0
    SCFAXP=1.0D0
!
!     PLOT SIZE STUFF
!
!     PLOT SIZE SHOULD OR SHOUD NOT BE DRAWN
      DRASZZ=.FALSE.
!
!     PLOT SIZE IS NOT EXPLICITLY SET
      PLSZ=.FALSE.
!
!     PLOT SIZE HAS NOT BEEN DRAWN YET
      SZFLG=.FALSE.
!
              RIM=.FALSE.
!
    IF(.NOT.FANEXT) THEN
!     RESET
!     FAN PLOTTING DEFAULT VALUES
              YFOB1=0.0D0
              YFOB2=0.0D0
              YFOB3=0.0D0
              XFOB1=0.0D0
              XFOB2=0.0D0
              XFOB3=0.0D0
              FANOFF=0.0D0
!     FAN WAVELENGTHS
!     RESTORE THE DEFAULT WAVELENGTH SETTINGS
!     RESTORE THE DEFAULT WAVELENGTH SETTINGS
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
!
              FANNUM=1
              FANNM1=3
              FANNM2=3
              FANTYP=5
              QALTYP=0
              SSIFLG=.TRUE.
              REFWV=INT(SYSTEM(11))
!     FAN PLOTTING DEFAULT VALUES HAVE BEEN SET
              FANNOB=0
              FANNRF=INT(SYSTEM(25))
              FANNIM=INT(SYSTEM(20))
              MAXFAN=20
              STEPJP=125.0D0
                    ELSE
!     FANEXT=TRUE
                      END IF
!     ADDED SO AS TO PLOT THE LAST FAN PRINTED USING
!     A CMD LEVEL FANS GENERATION COMMAND
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
!     FAN PLOTTING DEFAULT VALUES HAVE BEEN SET
              MAXFAN=20
              STEPJP=125.0D0

                      ELSE
!     NO FAN WAS DONE
                      FANEXT=.FALSE.
                      END IF

                      RETURN
                      END
                      
! SUB PLTPRO1.FOR
        SUBROUTINE PLTPRO1
        use kdp_plot_gen
        use type_utils, only: int2str
        USE GLOBALS
        use DATLEN
!        USE testHDF5
        !use h5fortran

!
        IMPLICIT NONE

       !type(hdf5_file) :: h5f

!
!       THIS ROUTINE DOES THE PLOT PROF COMMAND AT THE CMD LEVEL
!
      REAL*8 X,Y,Z,XN,YN,ZN,ROT1X,ROT1Z,ROT2Y,XX1,XX2,YY1, &
      ROT2Z,AX,AY,AZ,AALF,APHI,YMAXI,YMINI,XMAXI,XMINI &
      ,XNEW,YNEW,LKG,VIEPH,VIEAL,Z1,YY2,AX1,AX2,AY1,AY2,ZDELZ &
      ,X00,Y00,Z0,LX0,LY0,LZ0,ACALL1,ACALL2,XM,YM,ZCORR,ZDELZ1 &
      ,X1,X2,Y1,Y2,SLOPE,DELXX,DELYY,MX0,MY0,MZ0,NX0,NY0,NZ0
!
      LOGICAL ALT,VERT,INSIT,INSIDEIT,YESONE1,YESONE2,YESONE3,YESONE4 &
      ,YESONE5,YESONE6,SECPLT(0:499)
!
      COMMON/YESSIR/YESONE1,YESONE2,YESONE3,YESONE4,YESONE5,YESONE6
!
      EXTERNAL INSIDEIT
!
      INTEGER M1,M2,M3,M4,CAFLG,COFLG,J,IK,III,NO,CLRR,ALLOERR &
      ,COLPAS,KKK
!
      REAL*8 XMIN,YMIN,XMAX,YMAX,ZA,ZB,ZM,FRACRAD, &
      XMINO,YMINO,XMAXO,YMAXO,DRAPRO,THETA &
      ,YMIN2,XMIN2,YMAX2,XMAX2
!
      INTEGER IX,IY,I,II,IPST
!
      INTEGER STARTPOINT,STOPPOINT
      DIMENSION STARTPOINT(:,:,:),STOPPOINT(:,:,:)
      ALLOCATABLE :: STARTPOINT,STOPPOINT
!
        LOGICAL NOPLOT
!
!
        INCLUDE 'DATMAI.INC'
        !INCLUDE 'DATLEN.INC'
!
      REAL*8 PRO

      DIMENSION PRO(:,:,:)
      ALLOCATABLE :: PRO



!     LOOK.VIEW TRANSFORMS

      ROT1X(AX,AZ,APHI)=((AX*DCOS(APHI))-(AZ*DSIN(APHI)))
      ROT1Z(AX,AZ,APHI)=((AX*DSIN(APHI))+(AZ*DCOS(APHI)))
!
      ROT2Z(AZ,AY,AALF)=((AZ*DCOS(AALF))+(AY*DSIN(AALF)))
      ROT2Y(AZ,AY,AALF)=((-AZ*DSIN(AALF))+(AY*DCOS(AALF)))
      DEALLOCATE (PRO,STARTPOINT,STOPPOINT,STAT=ALLOERR)
      ALLOCATE (STARTPOINT(1:4,1:3,0:INT(SYSTEM(20))),STAT=ALLOERR)
      ALLOCATE (STOPPOINT(1:4,1:3,0:INT(SYSTEM(20))),STAT=ALLOERR)
        LNTYPE=0
                        I=INT(SYSTEM(20))
        STARTPOINT(1:4,1:3,0:I)=0.0D0
        STOPPOINT(1:4,1:3,0:I)=0.0D0

      !PRINT *, "PLTPRO1 ROUTINE STARTING..."

!
      IF(MSG) THEN
      OUTLYNE='GENERATING SURFACE PROFILE PLOTTING DATA...'
      CALL SHOWIT(1)
                        END IF
                           DO KKK=1,2
!     IF KKK=1, REGULAR SURFACE PROFILE
!     IF KKK=2, DRAW MIRROR BACKING IF ALENS(110, ) NOT ZERO
!     BY ADDING AN APPROPRIATE Z VALUE TO THE SURFACE LOCATION
!
      M1=360
      M2=4
      M3=0
      M4=INT(SYSTEM(20))
      DEALLOCATE (PRO,STAT=ALLOERR)
      ALLOCATE (PRO(M1,M2,M3:M4),STAT=ALLOERR)

      PRO(1:360,1:4,0:M4)=0.0D0

!
                X=0.0D0
                Y=0.0D0
!
                VIEPH=(PII/180.0D0)*VIEPHI
                VIEAL=(PII/180.0D0)*VIEALF
!
!     MAKE SURE YOU HAVE VERTEX DATA WITHOUT OFFSETS AND SET TO
!     THE FIRST SURFACE WITHOUT AN INFINITE THICKNESS
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
      DEALLOCATE(STARTPOINT,STOPPOINT,PRO,STAT=ALLOERR)
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
!
!       CHECK SYNTAX
        IF(SST.EQ.1) THEN
        OUTLYNE= &
        '"PLOT PROF" TAKES NO STRING INPUT'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
      DEALLOCATE(STARTPOINT,STOPPOINT,PRO,STAT=ALLOERR)
                        RETURN
                        END IF
        IF(S4.EQ.1) THEN
        OUTLYNE= &
        '"PLOT PROF" ONLY TAKES NUMERIC WORDS #1, #2,#3 AND #5 INPUT'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
      DEALLOCATE(STARTPOINT,STOPPOINT,PRO,STAT=ALLOERR)
                        RETURN
                        END IF
                        NOPLOT=.FALSE.
                        IF(DF5.EQ.0) NOPLOT=.TRUE.
        IF(STI.EQ.1) THEN
                 WRITE(OUTLYNE,801)
      CALL SHOWIT(1)
 801    FORMAT('QUERRY (?) HAS NO MEANING WITH "PLOT PROF"')
      DEALLOCATE(STARTPOINT,STOPPOINT,PRO,STAT=ALLOERR)
                        RETURN
                        END IF
        IF(DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM(20)+W1
        IF(DF2.EQ.0.AND.W2.LT.0.0D0) W2=SYSTEM(20)+W2
        IF(DF3.EQ.1) W3=0.0D0
      IF(W3.LT.0.0D0.OR.W3.GT.360.0D0) THEN
      OUTLYNE='THE ANGLE "THETA", NUMERIC WORD #3 MUST BE IN THE'
      CALL SHOWIT(1)
      OUTLYNE='RANGE 0.0 TO 360 DEGREES'
      CALL SHOWIT(1)
                   CALL MACFAL
      DEALLOCATE(STARTPOINT,STOPPOINT,PRO,STAT=ALLOERR)
                   RETURN
                   END IF
!     CONVERT THETA TO RADIAN MEASURE
      IF(W3.GE.89.95D0.AND.W3.LE.90.05D0) W3=90.001D0
      IF(W3.GE.269.95D0.AND.W3.LE.270.05D0) W3=270.001D0
        THETA=W3*PII/180.0D0
!       DEFAULT VALUES
        IF(DF1.EQ.1) THEN
        IF(DABS(ALENS(3,0)).GT.1.0D10) THEN
                        W1=1.0D0
                        ELSE
                        W1=0.0D0
                        END IF
                        ELSE
!       DF1 NOT 1, W1 EXPLICITLY ENTERED
                        END IF
                STASUR=INT(W1)
        IF(DF2.EQ.1) THEN
                        W2=SYSTEM(20)
                        ELSE
!       DF2 NOT 1, W2 EXPLICITLY ENTERED
                        END IF
                STPSUR=INT(W2)
        IF(INT(W1).LT.0) THEN
!       INVALID NUMERIC WORD #1
        OUTLYNE= &
        'SURFACE NUMBER (NUMERIC WORD #1) IS BEYOND LEGAL RANGE'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
      DEALLOCATE(STARTPOINT,STOPPOINT,PRO,STAT=ALLOERR)
                        RETURN
                        END IF
        IF(INT(W2).GT.NEWIMG) THEN
!       INVALID NUMERIC WORD #2
        OUTLYNE= &
        'SURFACE NUMBER (NUMERIC WORD #2) IS BEYOND LEGAL RANGE'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
      DEALLOCATE(STARTPOINT,STOPPOINT,PRO,STAT=ALLOERR)
                        RETURN
                        END IF
        IF(INT(W2).LT.INT(W1)) THEN
!       W2 LESS THAN OR EQUAL TO W1
        OUTLYNE= &
        'NUMERIC WORD #2 MAY NOT BE LESS THAN NUMERIC WORD #1'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
      DEALLOCATE(STARTPOINT,STOPPOINT,PRO,STAT=ALLOERR)
                        RETURN
                        END IF
!
!       ALL INPUT IS OK, KEEP GOING
!     THE ARRAY CONTAINING SURFACE PROFILE DATA ARE:
!     SURPRO(N,Q,I) WHERE
!     Q=1:3(X,Y AND Z GLOBAL COORDINATES,I=0:MAXSUR, AND N = TOTAL NUMBER OF
!     DATA POINTS IN THE PROFILE=360
!     INTEGER SURA,SURB THE STARTING AND ENDING SURFACES OF THE
!     REQUESTED RANGE OF SURFACE PROFILES TO BE PLOTTED
!
!     PRO(1:360,1:3,0:MAXSUR)
!
!     THE FIRST DIMENSION IS FOR THE DATA POINT NUMBER
!     THE SECOND DIMENSION IS FOR THE X,Y AND Z COORDINATES OR THE POINT
!
!     WE NEED TO LOAD THE ARRAY BEFORE PLOTTING
!
!     THE PROCEDURE IS:
!     1. DETERMINE THE MAXIMUM AND MINIMUM LOCAL X AND Y
!               COORDINATES FOR A SURFACE FOR WHICH THE PROFILE
!               IS TO BE CALCULATED.
!
!     CYCLE THROUGH ALL THE SURFACES BUT SKIP SURFACES WITH MULTIPLE
!     APERTURE DEFS ON IT
!


                DO II=STASUR,STPSUR
      YESONE1=.FALSE.
      YESONE2=.FALSE.
      YESONE3=.FALSE.
      YESONE4=.FALSE.
      YESONE5=.FALSE.
      YESONE6=.FALSE.
                X1=0.0D0
                X2=0.0D0
                Y1=0.0D0
                Y2=0.0D0
                AX1=0.0D0
                AX2=0.0D0
                AY1=0.0D0
                AY2=0.0D0
                XX1=0.0D0
                XX2=0.0D0
                YY1=0.0D0
                YY2=0.0D0
                XMIN=0.0D0
                YMIN=0.0D0
                XMAX=0.0D0
                YMAX=0.0D0
                XMIN2=0.0D0
                YMIN2=0.0D0
                XMAX2=0.0D0
                YMAX2=0.0D0
                XMINO=0.0D0
                YMINO=0.0D0
                XMAXO=0.0D0
                YMAXO=0.0D0
                CAFLG=0
                COFLG=0
                III=II
!
!     NOW FOR THE II SURFACE WE CALCULATE ALL THE END POINTS
!
      ZDELZ=0.0D0
      CALL CAOJK(YMIN,XMIN,YMAX,XMAX, &
      YMINO,XMINO,YMAXO,XMAXO,CAFLG, &
      COFLG,III, &
      YMIN2,XMIN2,YMAX2,XMAX2,THETA,ZDELZ)
!
!     NOW WE HAVE THE END POINTS, CYCLE THROUGH ALL FOUR PAIRS
!
!     2. USE APPROPRIATE CALLS TO THE SAGPLT.FOR ROUTINE
!               TO CALCULATE THE SAG AND MAKE
!               CERTAIN THE SIGN IS CORRECT FOR A LOCAL Z COORDINATE
!
                        X1=XMIN
                        Y1=YMIN
                        X2=XMAX
                        Y2=YMAX
                        AX1=XMIN2
                        AY1=YMIN2
                        AX2=XMAX2
                        AY2=YMAX2
                        XX1=XMINO
                        YY1=YMINO
                        XX2=XMAXO
                        YY2=YMAXO
!     SET UP THE LINE SLOPE AND THE INITIAL DELXX VALUE
             IF((X2-X1).NE.0.0D0)SLOPE=(Y2-Y1)/(X2-X1)
                        VERT=.FALSE.
             IF((X2-X1).EQ.0.0D0) VERT=.TRUE.
!
                DELXX=(X2-X1)/(360.0D0-1.0D0)
                DELYY=(Y2-Y1)/(360.0D0-1.0D0)
!     NOW WE HAVE THE END POINTS, CYCLE THROUGH ALL THE PAIRS AND LOAD
!     ALL THE ARRAYS WITH SURFACE LOCAL X,Y AND Z DATA
!
                              DO J=1,360
                IF(J.EQ.1) THEN
                        X=X1
                        ELSE
                        X=X+DELXX
                        END IF
      IF(.NOT.VERT) Y=(SLOPE*(X-X1))+Y1
      IF(VERT) THEN
      IF(J.EQ.1) THEN
      Y=Y1
      ELSE
      Y=Y+DELYY
      END IF
                ELSE
                END IF
!
!     CALLS TO SAGPLT GO HERE
                III=II
      ACALL1=X
      ACALL2=Y
      ALT=.FALSE.
      IF(ALENS(34,III).NE.18.0D0) THEN
      ALT=.FALSE.
      IF(X.LT.AX1) THEN
      ACALL1=AX1
      ALT=.TRUE.
      END IF
      IF(X.GT.AX2) THEN
      ACALL1=AX2
      ALT=.TRUE.
      END IF
      IF(Y.LT.AY1) THEN
      ACALL2=AY1
      ALT=.TRUE.
      END IF
      IF(Y.GT.AY2) THEN
      ACALL2=AY2
      ALT=.TRUE.
      END IF
      END IF
      IF(ALENS(9,III).EQ.1.0D0) THEN
!     CIRCULAR CLEAR APERTURE, MAY BE TYPE 18 SPECIAL SURFACE
      ALT=.FALSE.
      IF(X.LT.AX1) THEN
      ACALL1=AX1
      ALT=.TRUE.
      END IF
      IF(X.GT.AX2) THEN
      ACALL1=AX2
      ALT=.TRUE.
      END IF
      IF(Y.LT.AY1) THEN
      ACALL2=AY1
      ALT=.TRUE.
      END IF
      IF(Y.GT.AY2) THEN
      ACALL2=AY2
      ALT=.TRUE.
      END IF
      END IF
      CALL SAGPLT(III,ACALL1,ACALL2,Z,NO)
      IF(ALENS(9,III).EQ.5.0D0.OR.ALENS(9,III).EQ.6.0D0) THEN
      ZDELZ=0.0D0
      ZDELZ1=0.0D0
                   ELSE
      IF(ALENS(9,III).NE.1.0D0.OR.ALENS(9,III).EQ.1.0D0.AND. &
      ALENS(12,III).NE.0.0D0.OR.ALENS(9,III).EQ.1.0D0.AND. &
      ALENS(13,III).NE.0.0D0) THEN
               FRACRAD=0.0D0
               ELSE
      IF(ALENS(10,III).NE.ALENS(11,III)) THEN
      FRACRAD=((DSQRT((X**2)+(Y**2))-ALENS(11,III))/ &
      (ALENS(10,III)-ALENS(11,III)))
      IF(FRACRAD.LE.0.0D0) FRACRAD=0.0D0
               ELSE
      FRACRAD=0.0D0
               END IF
               END IF
               END IF
      ZDELZ1=ZDELZ*FRACRAD
      Z=Z+ZDELZ1
         IF(NO.EQ.1) DRAPRO=0.0D0
         IF(NO.NE.1) DRAPRO=1.0D0
!
      IF(ALENS(34,II).NE.18.0D0) THEN
!     ASSIGN ARRAY VALUES
                PRO(J,1,II)=X
                PRO(J,2,II)=Y
      IF(J.GT.1) THEN
                XM=PRO(J-1,1,II)
                YM=PRO(J-1,2,II)
               ELSE
                XM=PRO(J,1,II)
                YM=PRO(J,2,II)
               END IF
!     DO WE HAVE TO DO A FIX FOR AN OBSCURATION
!
! IF XM,YM AND X,Y ARE INSIDE OBS, DRAPRO=0.0D0
!                  OTHERWISE
!                  DRAPRO=1.0D0

        INSIT=.FALSE.
        INSIT=INSIDEIT(II,X,Y,XM,YM)
        IF(INSIT) DRAPRO=0.0D0
        IF(.NOT.INSIT)DRAPRO=1.0D0
                PRO(J,3,II)=Z
                PRO(J,4,II)=DRAPRO
                   ELSE
!*****************************************************************************
! TYPE 18 SPECIAL SURFACE
      ZA=FTFL01(1,II)
      ZB=FTFL01(2,II)
                PRO(J,1,II)=X
                PRO(J,2,II)=Y
          III=II
         CALL SAGPLT(III,X,Y,Z,NO)
                PRO(J,3,II)=Z
                PRO(J,4,II)=1.0D0
!
!     DO WE HAVE TO DO A FIX FOR A SURFACE LIMIT
!
      IF(J.GT.1) THEN
!     REMEMBER THE PREVIUOS Z VALUE
                ZM=PRO(J-1,3,II)
                XM=PRO(J-1,1,II)
                YM=PRO(J-1,2,II)
               END IF
      IF(J.GT.1) THEN
!     DOES THE PREVIOUS VALUE AND THE CURRENT VALUE STRADEL THE ZA AND ZB
!     VALUES
      IF(DABS(ZM).GT.DABS(ZA).AND.DABS(Z).LT.DABS(ZB).AND..NOT.YESONE5) &
      THEN
                   PRO(J-1,1,II)=XM
                   PRO(J-1,2,II)=YM
                   PRO(J-1,3,II)=ZA
                   ZM=ZA
                   PRO(J-1,4,II)=1.0D0
                   PRO(J,1,II)=X
                   PRO(J,2,II)=Y
                   PRO(J,3,II)=ZB
                   Z=ZB
                   PRO(J,4,II)=1.0D0
                   YESONE5=.TRUE.
                           END IF
      IF(DABS(ZM).LT.DABS(ZB).AND.DABS(Z).GT.DABS(ZA).AND..NOT.YESONE6) &
       THEN
                   PRO(J-1,1,II)=XM
                   PRO(J-1,2,II)=YM
                   PRO(J-1,3,II)=ZB
                   ZM=ZB
                   PRO(J-1,4,II)=1.0D0
                   PRO(J,1,II)=X
                   PRO(J,2,II)=Y
                   PRO(J,3,II)=ZA
                   Z=ZA
                   PRO(J,4,II)=1.0D0
                   YESONE6=.TRUE.
                           END IF
!     DOES THE PREVIOUS VALUE AND THE CURRENT VALUE STRADLE THE OUTER
!     BOUNDARY VALUE AND THE CHECK WAS NOT YET MADE, THEN CHECK
      IF(DABS(ZM).GT.DABS(ZA).AND.DABS(Z).LT.DABS(ZA).AND..NOT.YESONE3) &
       THEN
!     WE JUMPED OVER THE INSIDE BOUBDARY ON THE WAY IN
!     SET THE CURRENT VALUE TO THE INNER BOUNDARY LIMIT AND SET TH
!     DRAW FLAG TO 1
                PRO(J,3,II)=ZA
                Z=ZA
                PRO(J,4,II)=1.0D0
                YESONE3=.TRUE.
               END IF
      IF(DABS(ZM).LT.DABS(ZA).AND.DABS(Z).GT.DABS(ZA).AND..NOT.YESONE2) &
       THEN
!     WE JUMPED OVER THE INSIDE BOUBDARY ON THE WAY OUT
!     SET THE CURRENT VALUE TO THE INNER BOUNDARY LIMIT AND SET TH
!     DRAW FLAG TO 1
                PRO(J,3,II)=ZA
                Z=ZA
                PRO(J,4,II)=1.0D0
                YESONE4=.TRUE.
               END IF
!     DOES THE PREVIOUS VALUE AND THE CURRENT VALUE STRADLE THE INNER
!     BOUNDARY VALUE AND THE CHECK WAS NOT YET MADE, THEN CHECK
      IF(DABS(ZM).GT.DABS(ZB).AND.DABS(Z).LT.DABS(ZB).AND..NOT.YESONE1) &
       THEN
!     WE JUMPED OVER THE INSIDE BOUBDARY ON THE WAY IN
!     SET THE CURRENT VALUE TO THE INNER BOUNDARY LIMIT AND SET TH
!     DRAW FLAG TO 1
                PRO(J,3,II)=ZB
                Z=ZB
                PRO(J,4,II)=1.0D0
                YESONE1=.TRUE.
               END IF
      IF(DABS(ZM).LT.DABS(ZB).AND.DABS(Z).GT.DABS(ZB).AND..NOT.YESONE2) &
       THEN
!     WE JUMPED OVER THE INSIDE BOUBDARY ON THE WAY OUT
!     SET THE CURRENT VALUE TO THE INNER BOUNDARY LIMIT AND SET TH
!     DRAW FLAG TO 1
                PRO(J,3,II)=ZB
                Z=ZB
                PRO(J,4,II)=1.0D0
                YESONE2=.TRUE.
               END IF
               END IF
      IF(DABS(Z).LT.DABS(ZB)) PRO(J,4,II)=0.0D0
      IF(DABS(Z).GT.DABS(ZA)) PRO(J,4,II)=0.0D0
      IF(DABS(Z).GE.DABS(ZB).AND.DABS(Z).LE.DABS(ZA)) PRO(J,4,II)=1.0D0
!         IF(NO.EQ.1) DRAPRO=0.0D0
!         IF(NO.NE.1) DRAPRO=1.0D0
               IF(J.EQ.1) PRO(J,4,II)=1.0D0
               IF(J.EQ.360) PRO(J,4,II)=1.0D0
!*****************************************************************************
                   END IF
!
!               CYCLE THROUGH THE NEXT DATA PAIR
                        END DO
!
!               CYCLE THROUGH THE NEXT SURFACE

                        END DO
!
!     3. THE ARRAYS NOW HAVE LOCAL X,Y AND Z VALUES STORED IN THEM
!     CONVERT THE LOCAL X ANY Y PROFILES TO GLOBAL NUMBERS
!     GLOBAL VERTEX DATA IS
                        DO II=STASUR,STPSUR
                        DO I=1,360
        X00=VERTEX(1,II)
        Y00=VERTEX(2,II)
        Z0=VERTEX(3,II)
        LX0=VERTEX(4,II)
        MX0=VERTEX(5,II)
        NX0=VERTEX(6,II)
        LY0=VERTEX(7,II)
        MY0=VERTEX(8,II)
        NY0=VERTEX(9,II)
        LZ0=VERTEX(10,II)
        MZ0=VERTEX(11,II)
        NZ0=VERTEX(12,II)
                X=PRO(I,1,II)
                Y=PRO(I,2,II)
                SECPLT(II)=.FALSE.
        IF(KKK.EQ.2) THEN
        IF(ALENS(110,II).NE.0.0D0) THEN
        ZCORR=DABS(ALENS(110,II))
        IF(ALENS(46,II).LT.0.0D0) PRO(I,3,II)=PRO(I,3,II)+ZCORR
        IF(ALENS(46,II).GT.0.0D0) PRO(I,3,II)=PRO(I,3,II)-ZCORR
                       SECPLT(II)=.TRUE.
                           ELSE
                       SECPLT(II)=.FALSE.
                           END IF
                           END IF
        Z=PRO(I,3,II)
!
        X1=X00+((LX0*(X))+(LY0*(Y)) &
        +(LZ0*(Z)))
        Y1=Y00+((MX0*(X))+(MY0*(Y)) &
        +(MZ0*(Z)))
        Z1=Z0+((NX0*(X))+(NY0*(Y)) &
        +(NZ0*(Z)))
                PRO(I,1,II)=X1
                PRO(I,2,II)=Y1
                PRO(I,3,II)=Z1
                        END DO
                        END DO
!
!     4. NOW DETERMINE THE BEST PLACE FOR THE ROTATION POINT FOR
!               PLOT LOOK/VIEW
!
                CALL ROT2(PRO,M1,M2,M3,M4)
!
!     5.  CONVERT THE GLOBAL X ANY Y PROFILES
!               USING THE LOOK/VIEW VALUES
                        DO II=STASUR,STPSUR
                        DO I=1,360
                X=PRO(I,1,II)
                Y=PRO(I,2,II)
                Z=PRO(I,3,II)
      X=X-XROT
      Y=Y-YROT
      Z=Z-ZROT
      XN=ROT1X(X,Z,VIEPH)
      YN=Y
      ZN=ROT1Z(X,Z,VIEPH)
      X=XN
      Y=YN
      Z=ZN
!
      ZN=ROT2Z(Z,Y,VIEAL)
      YN=ROT2Y(Z,Y,VIEAL)
      XN=X
      PRO(I,1,II)=XN
      PRO(I,2,II)=YN
      PRO(I,3,II)=ZN
                        END DO
                        END DO
!
!     THE ARRAYS NOW HAVE GLOBAL SURFACE PROFILE DATA IN THEM
!
!     6.IF NEEDED, DETERMINE SCALE FACTORS AND PLOT RANGE
!
      !PRINT *, "ABOUT TO CALL PLTSC2 FROM PLTPRO1..."
      !PRINT *, "BEFORE CALLING, SCALE FACTOR IS ", SCFAY
      CALL PLTSC2(XMINI,XMAXI,YMINI,YMAXI,PRO,M1,M2,M3,M4)
!
!     7.CYCLE THROUGH THE THE ARRAYS, APPLY SCALE FACTORS
!
!     RIGHT NOW,COORDINATES ARE IN WORLD COORDINATES
!     CONVERT THEM TO DEVICE INDEPENDENT GRAPHICS COORDINATES IN
!     TWO STEPS.
!
!     THE WORLD X PLOTS TO THE PLOTTER X
!     THE WORLD Y PLOTS TO THE PLOTTER Y
!
!     STEP 1: CONVERT USING AN APPROPRIATE SCALE FACTOR
!               CALCULATING AN APPROPRIATE FACTOR IF NECESSARY
!
                        DO II=STASUR,STPSUR
                        DO I=1,360
                  PRO(I,1,II)=(PRO(I,1,II)/SCFAX)*1*1000.0D0
                  PRO(I,2,II)=(PRO(I,2,II)/SCFAY)*1*1000.0D0
                        END DO
                        END DO
!
!     8. APPLY THE PLOT XSHIFT AND YSHIFT VALUES
                        DO I=1,360
                        DO II=STASUR,STPSUR
      IF(LORIENT) CALL ORSHIFT
      PRO(I,1,II)=PRO(I,1,II)+DBLE(PXSHFT)
      !PRO(I,2,II)=PRO(I,2,II)+3500.0D0+DBLE(PYSHFT)
      PRO(I,2,II)=PRO(I,2,II)+height_mid+DBLE(PYSHFT)
                        END DO
                        END DO
!
!     9. SET THE PLOT JUSTIFICATION IF NEEDED
!     IF THERE ARE X-OFFSETS (JUSTIFICATION) TO APPLY, DO THEM HERE.
!
!     NOW
      call LogTermFOR("RCL is "//int2str(RCL))
      IF(RCL.EQ.1.OR.RCL.EQ.-1) THEN
      JUSOFF=500.0D0-((XMINI/SCFAX)*1000.0D0)
                        RCL=-1
                        ELSE
                        END IF
      IF(RCL.EQ.2.OR.RCL.EQ.-2) THEN
                        RCL=-2
                        !JUSOFF=5000.0D0
                        JUSOFF=width_mid
                        ELSE
                        END IF
      IF(RCL.EQ.3.OR.RCL.EQ.-3) THEN
      !JUSOFF=9500.0D0-((XMAXI/SCFAX)*1000.0D0)
      JUSOFF=kdp_width-500.0-((XMAXI/SCFAX)*1000.0D0)
                        RCL=-3
                        ELSE
                        END IF
!
                        DO I=1,360
                        DO II=STASUR,STPSUR
      PRO(I,1,II)=PRO(I,1,II)+JUSOFF
                        END DO
                        END DO
!     9. PLOT GAMMA
!     IF THERE IS A NON-ZERO GAMMA SPECIFIED IN PLOT LOOK OR PLOT VIEW THEN
!     ROTATE IN GAMMA ON THE SCREEN ABOUT THE CENTER OF THE SCREEN
!     WHICH HAS COORDINATES X=5000.0D0,Y=3500.0D0
!
!     FIRST SHIFT THE COORDINATE ORIGIN TO THE CETER OF THE DISPLAY
!
                        DO I=1,360
                        DO II=STASUR,STPSUR
      PRO(I,1,II)=PRO(I,1,II)-width_mid! 5000.0D0
      PRO(I,2,II)=PRO(I,2,II)-height_mid !3500.0D0
                        END DO
                        END DO
!     THE SCREEN COORDINATE IN REAL*8 IN THE SHIFTED COORDINATE
!     FRAME IS NOW X AND Y

      IF(DBLE(PGAMMA).NE.0.0D0) THEN
      LKG=(PII/180.0D0)*DBLE(PGAMMA)

                        DO I=1,360
                        DO II=STASUR,STPSUR
                X=PRO(I,1,II)
                Y=PRO(I,2,II)
                XNEW=((X*DCOS(LKG))-(Y*DSIN(LKG)))
                YNEW=((X*DSIN(LKG))+(Y*DCOS(LKG)))
                PRO(I,1,II)=XNEW
                PRO(I,2,II)=YNEW
                        END DO
                        END DO
                        ELSE
                        END IF
!     THE ROTATION IS DONE, NOW SHIFT THE ORIGIN BACK TO THE BOTTOM
!     LEFT HAND CORNER
                        DO I=1,360
                        DO II=STASUR,STPSUR
      PRO(I,1,II)=PRO(I,1,II)+width_mid !5000.0D0
      PRO(I,2,II)=PRO(I,2,II)+height_mid !3500.0D0
                        END DO
                        END DO
!
                        DO I=STASUR,STPSUR
      IF(ALENS(127,I).NE.0.0D0) GO TO 51
!     NOW DRAW THE X PROFILE OF THE CLAP AT SURFACE I
!     WITH THE PEN UP, GO TO THE STARTING PLOT POSITION
        IF(.NOT.PLEXIS) PLEXIS=.TRUE.
      IF(WQ.EQ.'PROF') THEN
!     FIRST CLAP PROFILE
                DO J=1,360
!     PUT INSTRUCTIONS IN P1ARAY TO DROP PEN AND DRAW
      DRAPRO=PRO(J,4,I)
      IF(J.EQ.1) IPST=0
      IF(J.GT.1) THEN
        IF(DRAPRO.EQ.1.0D0.AND.PRO((J-1),4,I).EQ.0.0D0) IPST=0
        IF(DRAPRO.EQ.1.0D0.AND.PRO((J-1),4,I).EQ.1.0D0) IPST=1
        IF(DRAPRO.EQ.0.0D0) IPST=0
      END IF
      IF(J.GT.0) THEN
       IF(PRO(J,1,I).GT.1.0D6) PRO(J,1,I)=1.0D6
       IF(PRO(J,2,I).GT.1.0D6) PRO(J,2,I)=1.0D6
       IF(PRO(J,1,I).LT.-1.0D6) PRO(J,1,I)=-1.0D6
       IF(PRO(J,2,I).LT.-1.0D6) PRO(J,2,I)=-1.0D6
       IX=INT(PRO(J,1,I))
       IY=INT(PRO(J,2,I))
       P1ARAY(J,1,1)=IX
       P1ARAY(J,2,1)=IY
       P1ARAY(J,3,1)=IPST
      END IF

        IF(.NOT.PLEXIS) PLEXIS=.TRUE.
                END DO
!     FINISHED WITH THAT PROFILE, LIFT PEN
      IPST=0
!     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE P1ARAY ARRAY
!
!     LINE TYPE SETTING
      COLPAS=COLPRO
      CALL MY_COLTYP(COLPAS)
      OLLNTP=LNTYPE
      LNTYPE=0
!     DASH, SOLID OR INVISIBLE
                        CLRR=0
      IF(DUMMMY(I).AND.ALENS(9,I).EQ.0.0D0) &
      CLRR=-1
      IF(DUMMMY(I).AND.ALENS(9,I).NE.0.0D0) THEN
      IF(DASHH) LNTYPE=2
                        ELSE
!     LEAVE LINE ALONE
                        END IF
!
                        IF(CLRR.NE.-1) THEN
        FIXUP=.FALSE.
                        DO IK=1,360
      IF(IK.EQ.1) THEN
        IF(P1ARAY(IK,3,1).NE.0) P1ARAY(IK,3,1)=0
                        END IF
      IF(IK.GT.1.AND.IK.LE.360) THEN
        IF(P1ARAY(IK-1,1,1).LE.0.OR.P1ARAY(IK-1,2,1).LE.0 &
        .OR.P1ARAY(IK,1,1).LE.0.OR.P1ARAY(IK,2,1).LE.0) P1ARAY(IK,3,1)=0
                        END IF
                        END DO
!
                        DO IK=1,360

      IF(KKK.EQ.2.AND.SECPLT(I).OR.KKK.EQ.1) THEN
                        IF(KKK.EQ.1) THEN
                        IF(IK.EQ.1) THEN
        STARTPOINT(1,1,I)=P1ARAY(1,1,1)
        STARTPOINT(1,2,I)=P1ARAY(1,2,1)
        STARTPOINT(1,3,I)=P1ARAY(1,3,1)
                        END IF
                        IF(IK.EQ.360) THEN
        STARTPOINT(4,1,I)=P1ARAY(360,1,1)
        STARTPOINT(4,2,I)=P1ARAY(360,2,1)
        STARTPOINT(4,3,I)=P1ARAY(360,3,1)
                        END IF
                        END IF
                        IF(KKK.EQ.2.AND.SECPLT(I)) THEN
                        IF(IK.EQ.1) THEN
        STOPPOINT(1,1,I)=P1ARAY(1,1,1)
        STOPPOINT(1,2,I)=P1ARAY(1,2,1)
        STOPPOINT(1,3,I)=P1ARAY(1,3,1)
                        END IF
                        IF(IK.EQ.360) THEN
        STOPPOINT(4,1,I)=P1ARAY(360,1,1)
        STOPPOINT(4,2,I)=P1ARAY(360,2,1)
        STOPPOINT(4,3,I)=P1ARAY(360,3,1)
                        END IF
                        END IF
      IF(IK.EQ.1) THEN
        IF(P1ARAY(IK,3,1).NE.0) P1ARAY(IK,3,1)=0
                        END IF
      IF(IK.GT.1.AND.IK.LE.360) THEN
        IF(P1ARAY(IK-1,1,1).LE.0.OR.P1ARAY(IK-1,2,1).LE.0 &
        .OR.P1ARAY(IK,1,1).LE.0.OR.P1ARAY(IK,2,1).LE.0) P1ARAY(IK,3,1)=0
                        END IF
      IF(.NOT.NOPLOT.OR.NOPLOT.AND.ALENS(9,I).NE.0.0D0) &
      CALL PENMV1(P1ARAY(IK,1,1),P1ARAY(IK,2,1),P1ARAY(IK,3,1))
                        END IF
                        END DO
!     NOW DO THE EDGES AT THE OBSCURATIONS
                        DO IK=3,359
      IF(KKK.EQ.1) THEN
      IF(P1ARAY(IK-1,3,1).EQ.0.AND.P1ARAY(IK,3,1).EQ.1) THEN
      STARTPOINT(2,1,I)=P1ARAY(IK,1,1)
      STARTPOINT(2,2,I)=P1ARAY(IK,2,1)
      STARTPOINT(2,3,I)=P1ARAY(IK,3,1)
                       GO TO 20
                           END IF
                           END IF
                        END DO
!     NOW DO THE EDGES AT THE OBSCURATIONS
 20                     DO IK=3,359
      IF(KKK.EQ.1) THEN
      IF(P1ARAY(IK-1,3,1).EQ.1.AND.P1ARAY(IK,3,1).EQ.0) THEN
      STARTPOINT(3,1,I)=P1ARAY(IK,1,1)
      STARTPOINT(3,2,I)=P1ARAY(IK,2,1)
      STARTPOINT(3,3,I)=P1ARAY(IK,3,1)
                       GO TO 30
                           END IF
                           END IF
                        END DO
 30                     DO IK=3,359
      IF(KKK.EQ.2.AND.SECPLT(I)) THEN
      IF(P1ARAY(IK-1,3,1).EQ.0.AND.P1ARAY(IK,3,1).EQ.1) THEN
      STOPPOINT(2,1,I)=P1ARAY(IK,1,1)
      STOPPOINT(2,2,I)=P1ARAY(IK,2,1)
      STOPPOINT(2,3,I)=P1ARAY(IK,3,1)
                       GO TO 40
                           END IF
                           END IF
                        END DO
 40                     DO IK=3,359
      IF(KKK.EQ.2.AND.SECPLT(I)) THEN
      IF(P1ARAY(IK-1,3,1).EQ.1.AND.P1ARAY(IK,3,1).EQ.0) THEN
      STOPPOINT(3,1,I)=P1ARAY(IK,1,1)
      STOPPOINT(3,2,I)=P1ARAY(IK,2,1)
      STOPPOINT(3,3,I)=P1ARAY(IK,3,1)
                       GO TO 50
                           END IF
                           END IF
                        END DO
 50                     CONTINUE
                        ELSE
                        CLRR=0
                        END IF
                        ELSE
                        END IF
!     DO NEXT SURFACE
! TEMP - testing how to fill a surface
!          IF (I.EQ.5) THEN
!       CALL MY_COLTYP_ALPHA(COLFILL, 50)
!       PRINT *, "COLFILL IS ", COLFILL
!       CALL FILLSURF(STARTPOINT(1,1,I),STARTPOINT(1,2,I),1)
!       CALL MY_COLTYP(COLPAS)
!          END IF

 51                     CONTINUE
                        END DO
!     DO SECOND PASS FOR MIRROR BACKING
      !PRINT *, PRO
      if (STASUR.GT.2.AND.STPSUR.LT.8) then
        !call h5write('ppro_sub.h5','/p', PRO)
        !call hdf5_close()
      else
        !call h5write('ppro.h5','/p', PRO)
        !call hdf5_close()
      end if
      ! open(17,file='values.dat',action='write')
      ! !write(17,'(2I5)') n,m
      ! do xx=1,SIZE(P1ARAY,1)
      ! do yy=1,SIZE(P1ARAY,2)
      ! do zz=0,SIZE(P1ARAY,3)-1
      ! write(17, *) xx, ",", yy, ",", zz, ",", PRO(xx,yy,zz)
      ! end do
      ! end do
      ! end do
      ! close(17)
      !*write(17,*) !skip a line not required enddo close(17)
      !PRINT *, PRO
      DEALLOCATE(PRO,STAT=ALLOERR)
                        END DO
        FIXUP=.FALSE.
                        DO I=0,INT(SYSTEM(20))
      IF(.NOT.NOPLOT.OR.NOPLOT.AND.ALENS(9,I).NE.0.0D0) THEN
        IF(ALENS(110,I).NE.0.0D0) THEN
          !PRINT *, "LINE 5755 PLTPRO1 Executed!"
      CALL PENMV1(STARTPOINT(1,1,I),STARTPOINT(1,2,I),0)
      CALL PENMV1(STOPPOINT(1,1,I),STOPPOINT(1,2,I),1)
      CALL PENMV1(STARTPOINT(2,1,I),STARTPOINT(2,2,I),0)
      CALL PENMV1(STOPPOINT(2,1,I),STOPPOINT(2,2,I),1)
      CALL PENMV1(STARTPOINT(3,1,I),STARTPOINT(3,2,I),0)
      CALL PENMV1(STOPPOINT(3,1,I),STOPPOINT(3,2,I),1)
      CALL PENMV1(STARTPOINT(4,1,I),STARTPOINT(4,2,I),0)
      CALL PENMV1(STOPPOINT(4,1,I),STOPPOINT(4,2,I),1)
                        END IF
                        END IF
                        END DO
!
!     HERE WE DO THE PLOT LI AND PLOT AXIS DRAWING
                        IF(.NOT.VIGFLG.AND.PLTVIG) THEN
                        CALL VIGSHO
                        VIGFLG=.TRUE.
                        ELSE
                        END IF
      LNTYPE=OLLNTP

      !if (STASUR.GT.2.AND.STPSUR.LT.8) then
       !call h5write('startpoint_sub.h5','/f', STARTPOINT)
       !call hdf5_close()
       !call h5write('stoppoint_sub.h', '/l', STOPPOINT)
       !call hdf5_close()
      !else
       !call h5write('startpoint.h5','/f', STARTPOINT)
       !call hdf5_close()
       !call h5write('stoppoint.h', '/l', STOPPOINT)
       !call hdf5_close()

      !end if

      !call h5write('PROTEST.h5','/x', PRO)
      !call h5write('p1aray.h5','/p1', P1ARAY)
      !call h5write('pro4.h5','/pro', PRO)

      !call h5write('PRO.h5','/pro', PRO)

      !call tstwrite
      ! call h5f%open('startpoint.h5', action='w')
      ! call h5f%write('/f', STARTPOINT)
      ! call h5f%close()
       ! call h5f%open('stoppoint.h5', action='w')
       ! call h5f%write('/f', STOPPOINT)
       ! call h5f%close()
      !PRINT *, "PRO Size is ", SIZE(PRO,1), " ", SIZE(PRO,2)
      !PRINT *, "PRO Sisze is ", SIZE(PRO,3), " ", SIZE(PRO)
      !call h5write('PRO.h5', '/x', P1ARAY)


      ! open(17,file='values.dat',action='write')
      ! !write(17,'(2I5)') n,m
      ! do xx=1,SIZE(P1ARAY,1)
      ! do yy=1,SIZE(P1ARAY,2)
      ! do zz=1,SIZE(P1ARAY,3)
      ! write(17, *) xx, ",", yy, ",", zz, ",", P1ARAY(xx,yy,zz)
      ! end do
      ! end do
      ! end do
      !*write(17,*) !skip a line not required enddo close(17)



      DEALLOCATE(STARTPOINT,STOPPOINT,PRO,STAT=ALLOERR)
      DEALLOCATE (PRO,STARTPOINT,STOPPOINT,STAT=ALLOERR)
                        LNTYPE=0
                        RETURN
                        END
! SUB PLTEDG.FOR
        SUBROUTINE PLTEDG
        USE GLOBALS
        USE DATLEN
        use kdp_plot_gen
!
        IMPLICIT NONE
!
!       THIS ROUTINE DOES THE PLOT EDGEX/EDGEY COMMAND AT THE CMD LEVEL
!
      REAL*8 X,Y,Z,XN,YN,ZN,ROT1X,ROT1Z,ROT2Y,XX1,XX2,YY1, &
      ROT2Z,AX,AY,AZ,AALF,APHI,YMAXI,YMINI,XMAXI,XMINI &
      ,XNEW,YNEW,LKG,VIEPH,VIEAL,Z1,YY2 &
      ,X00,Y00,Z0,LX0,LY0,LZ0 &
      ,X1,X2,Y1,Y2,MX0,MY0,MZ0,NX0,NY0,NZ0
!
      INTEGER M1,M2,M3,CAFLG,COFLG,IK,III,NO,ALLOERR
!
      REAL*8 XLFT,YLFT,XRHT,YRHT,XTOP,YTOP,XBOT,YBOT &
      ,XLFTO,YLFTO,XRHTO,YRHTO,XTOPO,YTOPO,XBOTO,YBOTO &
      ,YLFT2,XLFT2,YRHT2,XRHT2,XTOP2,YTOP2,XBOT2,YBOT2,ZDELZ
!
      INTEGER COLPAS,IX,IY,I,II,IPST,J,SKIPNEXT
!
        LOGICAL NOPLOT
!
        INCLUDE 'DATMAI.INC'
        !INCLUDE 'DATLEN.INC'
!
      REAL EDGE
      DIMENSION EDGE(:,:,:)
      ALLOCATABLE :: EDGE
!
!     LOOK.VIEW TRANSFORMS

      ROT1X(AX,AZ,APHI)=((AX*DCOS(APHI))-(AZ*DSIN(APHI)))
      ROT1Z(AX,AZ,APHI)=((AX*DSIN(APHI))+(AZ*DCOS(APHI)))
!
      ROT2Z(AZ,AY,AALF)=((AZ*DCOS(AALF))+(AY*DSIN(AALF)))
      ROT2Y(AZ,AY,AALF)=((-AZ*DSIN(AALF))+(AY*DCOS(AALF)))
      LNTYPE=0
!
      M1=4
      M2=0
      M3=INT(SYSTEM(20))
      DEALLOCATE (EDGE,STAT=ALLOERR)
      ALLOCATE (EDGE(M1,M1,M2:M3),STAT=ALLOERR)
                X=0.0D0
                Y=0.0D0
          EDGE(1:4,1:4,0:M3)=0.0
!
                VIEPH=(PII/180.0D0)*VIEPHI
                VIEAL=(PII/180.0D0)*VIEALF
!

      !PRINT *, "PLTEDG ROUTINE Starting.."
!     MAKE SURE YOU HAVE VERTEX DATA WITHOUT OFFSETS AND SET TO
!     THE FIRST SURFACE WITHOUT AN INFINITE THICKNESS
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
      DEALLOCATE(EDGE,STAT=ALLOERR)
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
      OUTLYNE='GENERATING SURFACE EDGE DATA...'
      CALL SHOWIT(1)
                        END IF
!
!       CHECK SYNTAX
        IF(SST.EQ.1) THEN
        IF(WQ.EQ.'EDGEX')OUTLYNE= &
        '"PLOT EDGEX" TAKES NO STRING INPUT'
        IF(WQ.EQ.'EDGEY')OUTLYNE= &
        '"PLOT EDGEY" TAKES NO STRING INPUT'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
      DEALLOCATE(EDGE,STAT=ALLOERR)
                        RETURN
                        END IF
        IF(S3.EQ.1.OR.S4.EQ.1) THEN
        IF(WQ.EQ.'EDGEX')OUTLYNE= &
        '"PLOT EDGEX" ONLY TAKES NUMERIC WORDS #1, #2 AND #5 INPUT'
        IF(WQ.EQ.'EDGEY')OUTLYNE= &
        '"PLOT EDGEY" ONLY TAKES NUMERIC WORDS #1, #2 AND #5 INPUT'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
      DEALLOCATE(EDGE,STAT=ALLOERR)
                        RETURN
                        END IF
                        NOPLOT=.FALSE.
                        IF(DF5.EQ.0) NOPLOT=.TRUE.
        IF(STI.EQ.1) THEN
                 IF(WQ.EQ.'EDGEX')WRITE(OUTLYNE,800)
                 IF(WQ.EQ.'EDGEY')WRITE(OUTLYNE,801)
      CALL SHOWIT(1)
 800    FORMAT('QUERRY (?) HAS NO MEANING WITH "PLOT EDGEX"')
 801    FORMAT('QUERRY (?) HAS NO MEANING WITH "PLOT EDGEY"')
      DEALLOCATE(EDGE,STAT=ALLOERR)
                        RETURN
                        ELSE
                        END IF
        IF(DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM(20)+W1
        IF(DF2.EQ.0.AND.W2.LT.0.0D0) W2=SYSTEM(20)+W2
!       DEFAULT VALUES
        IF(DF1.EQ.1) THEN
        IF(DABS(ALENS(3,0)).GT.1.0D10) THEN
                        W1=DBLE(1)
                        ELSE
                        W1=DBLE(0)
                        END IF
                        ELSE
!       DF1 NOT 1, W1 EXPLICITLY ENTERED
                        END IF
                STASUR=INT(W1)
        IF(DF2.EQ.1) THEN
                        W2=DBLE(NEWIMG)
                        ELSE
!       DF2 NOT 1, W2 EXPLICITLY ENTERED
                        END IF
                STPSUR=INT(W2)
        IF(INT(W1).LT.0) THEN
!       INVALID NUMERIC WORD #1
        OUTLYNE= &
        'SURFACE NUMBER (NUMERIC WORD #1) IS BEYOND LEGAL RANGE'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
      DEALLOCATE(EDGE,STAT=ALLOERR)
                        RETURN
                        END IF
        IF(INT(W2).GT.NEWIMG) THEN
!       INVALID NUMERIC WORD #2
        OUTLYNE= &
        'SURFACE NUMBER (NUMERIC WORD #2) IS BEYOND LEGAL RANGE'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
      DEALLOCATE(EDGE,STAT=ALLOERR)
                        RETURN
                        END IF
        IF(INT(W2).LE.INT(W1)) THEN
!       W2 LESS THAN OR EQUAL TO W1
        OUTLYNE= &
        'NUMERIC WORD #2 MUST BE GREATER THAN NUMERIC WORD #1'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
      DEALLOCATE(EDGE,STAT=ALLOERR)
                        RETURN
                        END IF
!
!       ALL INPUT IS OK, KEEP GOING
!     THE ARRAY CONTAINING SURFACE EDGE DATA ARE:
!
!     EDGE(1:4,1:3,0:MAXSUR)
!
!     THE FIRST DIMENSION IS FOR THE DATA POINT NUMBER
!     FOR YEDGE
!               1=BOT   (CLAP)
!               2=TOP   (CLAP)
!               3=BOT   (COBS)
!               4=TOP   (COBS)
!     FOR YEDGE
!               1=LFT   (CLAP)
!               2=RHT   (CLAP)
!               3=LFT   (COBS)
!               4=RHT   (COBS)
!     THE SECOND DIMENSION IS FOR THE X,Y AND Z COORDINATES OR THE POINT
!
!     WE NEED TO LOAD THE ARRAY BEFORE PLOTTING
!
!     THE PROCEDURE IS:
!     1. DETERMINE THE MAXIMUM AND MINIMUM LOCAL X AND Y
!               COORDINATES FOR A SURFACE FOR WHICH THE EDGE
!               IS TO BE CALCULATED.
!
!     CYCLE THROUGH ALL THE SURFACES
!
                DO II=STASUR,STPSUR
                X1=0.0D0
                X2=0.0D0
                Y1=0.0D0
                Y2=0.0D0
                XX1=0.0D0
                XX2=0.0D0
                YY1=0.0D0
                YY2=0.0D0
                XLFT=0.0D0
                YLFT=0.0D0
                XRHT=0.0D0
                YRHT=0.0D0
                XBOT=0.0D0
                YBOT=0.0D0
                XTOP=0.0D0
                YTOP=0.0D0
                XLFTO=0.0D0
                YLFTO=0.0D0
                XRHTO=0.0D0
                YRHTO=0.0D0
                XBOTO=0.0D0
                YBOTO=0.0D0
                XTOPO=0.0D0
                YTOPO=0.0D0
                CAFLG=0
                COFLG=0
                III=II
!
!     NOW FOR THE II SURFACE WE CALCULATE ALL THE END POINTS
!
      CALL CAO(YLFT,XLFT,YRHT,XRHT,XTOP,YTOP,XBOT,YBOT, &
      YLFTO,XLFTO,YRHTO,XRHTO,XTOPO,YTOPO,XBOTO,YBOTO,CAFLG, &
      COFLG,III &
      ,YLFT2,XLFT2,YRHT2,XRHT2,XTOP2,YTOP2,XBOT2,YBOT2,ZDELZ)
!
!
      IF(WQ.EQ.'EDGEY') THEN
                        EDGE(1,1,II)=REAL(XBOT,4)
                        EDGE(1,2,II)=reAL(YBOT,4)
                        EDGE(2,1,II)=REAL(XTOP,4)
                        EDGE(2,2,II)=REAL(YTOP,4)
                        EDGE(3,1,II)=reAL(XBOTO,4)
                        EDGE(3,2,II)=REAL(YBOTO,4)
                        EDGE(4,1,II)=REAL(XTOPO,4)
                        EDGE(4,2,II)=REAL(YTOPO,4)
                        ELSE
                        END IF
!
                IF(WQ.EQ.'EDGEX') THEN
                        EDGE(1,1,II)=REAL(XLFT,4)
                        EDGE(1,2,II)=REAL(YLFT,4)
                        EDGE(2,1,II)=REAL(XRHT,4)
                        EDGE(2,2,II)=REAL(YRHT,4)
                        EDGE(3,1,II)=REAL(XLFTO,4)
                        EDGE(3,2,II)=REAL(YLFTO,4)
                        EDGE(4,1,II)=REAL(XRHTO,4)
                        EDGE(4,2,II)=REAL(YRHTO,4)
                        ELSE
                        END IF
!
!     NOW WE HAVE THE END POINTS, CYCLE THROUGH ALL FOUR PAIRS
!
!     2. USE APPROPRIATE CALLS TO THE SAGPLT.FOR ROUTINE
!               TO CALCULATE THE SAG AND MAKE
!               CETRAIN THE SIGN IS CORRECT FOR A LOCAL Z COORDINATE
!
                        IF(WQ.EQ.'EDGEX') THEN
                        X1=XLFT2
                        Y1=YLFT2
                        X2=XRHT2
                        Y2=YRHT2
                        XX1=XLFTO
                        YY1=YLFTO
                        XX2=XRHTO
                        YY2=YRHTO
                        ELSE
                        END IF
                        IF(WQ.EQ.'EDGEY') THEN
                        X1=XBOT2
                        Y1=YBOT2
                        X2=XTOP2
                        Y2=YTOP2
                        XX1=XBOTO
                        YY1=YBOTO
                        XX2=XTOPO
                        YY2=YTOPO
                        ELSE
                        END IF
                CALL SAGPLT(III,X1,Y1,Z,NO)
                EDGE(1,3,II)=REAL(Z+ZDELZ,4)
                CALL SAGPLT(III,X2,Y2,Z,NO)
                EDGE(2,3,II)=REAL(Z+ZDELZ,4)
                CALL SAGPLT(III,XX1,YY1,Z,NO)
                EDGE(3,3,II)=REAL(Z+ZDELZ,4)
                CALL SAGPLT(III,XX2,YY2,Z,NO)
                EDGE(4,3,II)=REAL(Z+ZDELZ,4)
!
!               CYCLE THROUGH THE NEXT SURFACE
                        END DO
!
!     3. THE ARRAYS NOW HAVE LOCAL X,Y AND Z VALUES STORED IN THEM
!     CONVERT THE LOCAL X ANY Y EDGES TO GLOBAL NUMBERS
!     GLOBAL VERTEX DATA IS
                        DO II=STASUR,STPSUR
        X00=VERTEX(1,II)
        Y00=VERTEX(2,II)
        Z0=VERTEX(3,II)
        LX0=VERTEX(4,II)
        MX0=VERTEX(5,II)
        NX0=VERTEX(6,II)
        LY0=VERTEX(7,II)
        MY0=VERTEX(8,II)
        NY0=VERTEX(9,II)
        LZ0=VERTEX(10,II)
        MZ0=VERTEX(11,II)
        NZ0=VERTEX(12,II)
                X=EDGE(1,1,II)
                Y=EDGE(1,2,II)
                Z=EDGE(1,3,II)
!
        X1=X00+((LX0*(X))+(LY0*(Y)) &
        +(LZ0*(Z)))
        Y1=Y00+((MX0*(X))+(MY0*(Y)) &
        +(MZ0*(Z)))
        Z1=Z0+((NX0*(X))+(NY0*(Y)) &
        +(NZ0*(Z)))
                EDGE(1,1,II)=REAL(X1,4)
                EDGE(1,2,II)=REAL(Y1,4)
                EDGE(1,3,II)=reAL(Z1,4)
                X=REAL(EDGE(2,1,II),8)
                Y=REAL(EDGE(2,2,II),8)
                Z=REAL(EDGE(2,3,II),8)
!
        X1=X00+((LX0*(X))+(LY0*(Y)) &
        +(LZ0*(Z)))
        Y1=Y00+((MX0*(X))+(MY0*(Y)) &
        +(MZ0*(Z)))
        Z1=Z0+((NX0*(X))+(NY0*(Y)) &
        +(NZ0*(Z)))
                EDGE(2,1,II)=REAL(X1,4)
                EDGE(2,2,II)=ReAL(Y1,4)
                EDGE(2,3,II)=REAL(Z1,4)
                X=REAL(EDGE(3,1,II),8)
                Y=REAL(EDGE(3,2,II),8)
                Z=REAL(EDGE(3,3,II),8)
!
        X1=X00+((LX0*(X))+(LY0*(Y)) &
        +(LZ0*(Z)))
        Y1=Y00+((MX0*(X))+(MY0*(Y)) &
        +(MZ0*(Z)))
        Z1=Z0+((NX0*(X))+(NY0*(Y)) &
        +(NZ0*(Z)))
                EDGE(3,1,II)=REAL(X1,4)
                EDGE(3,2,II)=REAL(Y1,4)
                EDGE(3,3,II)=REAL(Z1,4)
                X=REAL(EDGE(4,1,II),8)
                Y=REAL(EDGE(4,2,II),8)
                Z=REAL(EDGE(4,3,II),8)
!
        X1=X00+((LX0*(X))+(LY0*(Y)) &
        +(LZ0*(Z)))
        Y1=Y00+((MX0*(X))+(MY0*(Y)) &
        +(MZ0*(Z)))
        Z1=Z0+((NX0*(X))+(NY0*(Y)) &
        +(NZ0*(Z)))
                EDGE(4,1,II)=REAL(X1,4)
                EDGE(4,2,II)=REAL(Y1,4)
                EDGE(4,3,II)=REAL(Z1,4)
                        END DO
!
!     4. NOW DETERMINE THE BEST PLACE FOR THE ROTATION POINT FOR
!               PLOT LOOK/VIEW
!
                CALL ROT1
!
!     5.  CONVERT THE GLOBAL X ANY Y EDGES
!               USING THE LOOK/VIEW VALUES
                        DO II=STASUR,STPSUR
                X=REAL(EDGE(1,1,II),8)
                Y=REAL(EDGE(1,2,II),8)
                Z=REAL(EDGE(1,3,II),8)
      X=X-XROT
      Y=Y-YROT
      Z=Z-ZROT
      XN=ROT1X(X,Z,VIEPH)
      YN=Y
      ZN=ROT1Z(X,Z,VIEPH)
      X=XN
      Y=YN
      Z=ZN
!
      ZN=ROT2Z(Z,Y,VIEAL)
      YN=ROT2Y(Z,Y,VIEAL)
      XN=X
      EDGE(1,1,II)=REAL(XN,4)
      EDGE(1,2,II)=REAL(YN,4)
      EDGE(1,3,II)=REAL(ZN,4)
                X=EDGE(2,1,II)
                Y=EDGE(2,2,II)
                Z=EDGE(2,3,II)
      X=X-XROT
      Y=Y-YROT
      Z=Z-ZROT
      XN=ROT1X(X,Z,VIEPH)
      YN=Y
      ZN=ROT1Z(X,Z,VIEPH)
      X=XN
      Y=YN
      Z=ZN
!
      ZN=ROT2Z(Z,Y,VIEAL)
      YN=ROT2Y(Z,Y,VIEAL)
      XN=X
      EDGE(2,1,II)=REAL(XN,4)
      EDGE(2,2,II)=REAL(YN,4)
      EDGE(2,3,II)=REAL(ZN,4)
                X=REAL(EDGE(3,1,II),8)
                Y=REAL(EDGE(3,2,II),8)
                Z=REAL(EDGE(3,3,II),8)
      X=X-XROT
      Y=Y-YROT
      Z=Z-ZROT
      XN=ROT1X(X,Z,VIEPH)
      YN=Y
      ZN=ROT1Z(X,Z,VIEPH)
      X=XN
      Y=YN
      Z=ZN
!
      ZN=ROT2Z(Z,Y,VIEAL)
      YN=ROT2Y(Z,Y,VIEAL)
      XN=X
      EDGE(3,1,II)=REAL(XN,4)
      EDGE(3,2,II)=REAL(YN,4)
      EDGE(3,3,II)=REAL(ZN,4)
                X=REAL(EDGE(4,1,II),8)
                Y=REAL(EDGE(4,2,II),8)
                Z=REAL(EDGE(4,3,II),8)
      X=X-XROT
      Y=Y-YROT
      Z=Z-ZROT
      XN=ROT1X(X,Z,VIEPH)
      YN=Y
      ZN=ROT1Z(X,Z,VIEPH)
      X=XN
      Y=YN
      Z=ZN
!
      ZN=ROT2Z(Z,Y,VIEAL)
      YN=ROT2Y(Z,Y,VIEAL)
      XN=X
      EDGE(4,1,II)=REAL(XN,4)
      EDGE(4,2,II)=REAL(YN,4)
      EDGE(4,3,II)=REAL(ZN,4)
                        END DO
!
!     THE ARRAYS NOW HAVE GLOBAL SURFACE EDGE DATA IN THEM
!
!     6.IF NEEDED, DETERMINE SCALE FACTORS AND PLOT RANGE
!
                CALL PLTSC1(XMINI,XMAXI,YMINI,YMAXI)
!
!     7.CYCLE THROUGH THE THE ARRAYS, APPLY SCALE FACTORS
!
!     RIGHT NOW,COORDINATES ARE IN WORLD COORDINATES
!     CONVERT THEM TO DEVICE INDEPENDENT GRAPHICS COORDINATES IN
!     TWO STEPS.
!
!     THE WORLD X PLOTS TO THE PLOTTER X
!     THE WORLD Y PLOTS TO THE PLOTTER Y
!
!     STEP 1: CONVERT USING AN APPROPRIATE SCALE FACTOR
!               CALCULATING AN APPROPRIATE FACTOR IF NECESSARY
!
                        DO II=STASUR,STPSUR
                        DO I=1,4
                  EDGE(I,1,II)=(EDGE(I,1,II)/ReAL(SCFAX,4))*1000.0
                  EDGE(I,2,II)=(EDGE(I,2,II)/REAL(SCFAY,4))*1000.0
                        END DO
                        END DO
!
!     8. APPLY THE PLOT XSHIFT AND YSHIFT VALUES
                        DO I=1,4
                        DO II=STASUR,STPSUR
      IF(LORIENT) CALL ORSHIFT
      EDGE(I,1,II)=EDGE(I,1,II)+REAL(DBLE(PXSHFT),4)
      EDGE(I,2,II)=EDGE(I,2,II)+REAL(height_mid,4)+REAL(DBLE(PYSHFT),4)
                        END DO
                        END DO
!
!     9. SET THE PLOT JUSTIFICATION IF NEEDED
!     IF THERE ARE X-OFFSETS (JUSTIFICATION) TO APPLY, DO THEM HERE.
!
!     NOW
      IF(RCL.EQ.1.OR.RCL.EQ.-1) THEN
      JUSOFF=500.0D0-((XMINI/SCFAX)*1000.0D0)
                        RCL=-1
                        ELSE
                        END IF
      IF(RCL.EQ.2.OR.RCL.EQ.-2) THEN
                        RCL=-2
                        JUSOFF=width_mid
                        ELSE
                        END IF
      IF(RCL.EQ.3.OR.RCL.EQ.-3) THEN
      JUSOFF=9500.0D0-((XMAXI/REAL(SCFAX,4))*1000.0D0)
                        RCL=-3
                        ELSE
                        END IF
!
                        DO I=1,4
                        DO II=STASUR,STPSUR
      EDGE(I,1,II)=EDGE(I,1,II)+REAL(JUSOFF,4)
                        END DO
                        END DO
!     9. PLOT GAMMA
!     IF THERE IS A NON-ZERO GAMMA SPECIFIED IN PLOT LOOK OR PLOT VIEW THEN
!     ROTATE IN GAMMA ON THE SCREEN ABOUT THE CENTER OF THE SCREEN
!     WHICH HAS COORDINATES X=5000.0D0,Y=3500.0D0
!
!     FIRST SHIFT THE COORDINATE ORIGIN TO THE CETER OF THE DISPLAY
!
                        DO I=1,4
                        DO II=STASUR,STPSUR
      EDGE(I,1,II)=EDGE(I,1,II)-REAL(width_mid,4)
      EDGE(I,2,II)=EDGE(I,2,II)-REAL(height_mid,4)
                        END DO
                        END DO
!     THE SCREEN COORDINATE IN REAL*8 IN THE SHIFTED COORDINATE
!     FRAME IS NOW X AND Y

      IF(DBLE(PGAMMA).NE.0.0D0) THEN
      LKG=(PII/180.0D0)*DBLE(PGAMMA)

                        DO I=1,4
                        DO II=STASUR,STPSUR
                X=EDGE(I,1,II)
                Y=EDGE(I,2,II)
                XNEW=((X*DCOS(LKG))-(Y*DSIN(LKG)))
                YNEW=((X*DSIN(LKG))+(Y*DCOS(LKG)))
                EDGE(I,1,II)=REAL(XNEW,4)
                EDGE(I,2,II)=REAL(YNEW,4)
                        END DO
                        END DO
                        ELSE
                        END IF
!     THE ROTATION IS DONE, NOW SHIFT THE ORIGIN BACK TO THE BOTTOM
!     LEFT HAND CORNER
                        DO I=1,4
                        DO II=STASUR,STPSUR
      EDGE(I,1,II)=EDGE(I,1,II)+REAL(width_mid,4)
      EDGE(I,2,II)=EDGE(I,2,II)+REAL(height_mid,4)
                        END DO
                        END DO
!
        IF(.NOT.PLEXIS) PLEXIS=.TRUE.
!
      IF(WQ.EQ.'EDGEX') THEN
!     FIRST DO THE EDGES OF THE CLAPS
!     DRAW THE X EDGES AND RETURN
                        DO J=1,4
      IF(J.EQ.1.OR.J.EQ.2) THEN
                        DO I=STASUR,STPSUR
      IF(I.EQ.0) THEN
                        IPST=0
                        ELSE
!                       NOT OBJECT
!
!     NOW DRAW THE X EDGES OF THE CLAP AT SURFACE I
!     WITH THE PEN UP, GO TO THE STARTING PLOT POSITION
!
!     IF THE CURRENT SURFACE IS PRECEEDED BY AIR OR REFL
!     AND ALL THE REFRACTIVE INDICES ARE 1 OR -1, DON'T
!     DRAW THE EDGE. IF THE CURRENT SURFACE MATERIAL IS AIR, DON'T
!     DRAW THE EDGE.
!     IN ALL OTHER CASES, DRAW THE EDGE.
      IF(GLANAM(I-1,2).EQ.'AIR          '.OR. &
      GLANAM(I-1,2).EQ.'PERFECT      '.OR. &
      GLANAM(I-1,2).EQ.'IDEAL        '.OR. &
      GLANAM(I-1,2).EQ.'REFLTIR      '.OR. &
      GLANAM(I-1,2).EQ.'REFLTIRO     '.OR. &
      GLANAM(I-1,2).EQ.'REFL         '.AND. &
      DABS(ALENS(46,I-1)).EQ.1.0D0.AND. &
      DABS(ALENS(47,I-1)).EQ.1.0D0.AND. &
      DABS(ALENS(48,I-1)).EQ.1.0D0.AND. &
      DABS(ALENS(49,I-1)).EQ.1.0D0.AND. &
      DABS(ALENS(50,I-1)).EQ.1.0D0) THEN
                        IPST=0
                        ELSE
                        IPST=1
      IF(I.EQ.STASUR.AND.GLANAM(I,2).EQ.'AIR') IPST=0
                        END IF
                        END IF
      IF(I.GT.STASUR) THEN
        IF (GLANAM(I-1,1).EQ.'MYGLASS') THEN
                        IPST=1
                        END IF
      END IF
!
      IF(EDGE(J,1,I).GT.1.0D6) EDGE(J,1,I)=1.0D6
      IF(EDGE(J,2,I).GT.1.0D6) EDGE(J,2,I)=1.0D6
      IF(EDGE(J,1,I).LT.-1.0D6) EDGE(J,1,I)=-1.0D6
      IF(EDGE(J,2,I).LT.-1.0D6) EDGE(J,2,I)=-1.0D6
      IX=INT(EDGE(J,1,I))
      IY=INT(EDGE(J,2,I))
      P1ARAY(I,1,1)=IX
      P1ARAY(I,2,1)=IY
      P1ARAY(I,3,1)=IPST
        IF(.NOT.PLEXIS) PLEXIS=.TRUE.
                        END DO
!     FINISHED WITH THAT EDGE, LIFT PEN
      IPST=0
!     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE P1ARAY ARRAY
!
      COLPAS=COLEDG
      CALL MY_COLTYP(COLPAS)
        FIXUP=.FALSE.
                        DO IK=STASUR,STPSUR
        IF(IK.GT.STASUR) THEN

      IF(ALENS(127,IK).NE.0.0D0.OR.IK.NE.STASUR.AND. &
      ALENS(127,IK-1).NE.0.0D0) THEN
                        ELSE
        IF(IK.EQ.0) P1ARAY(IK,3,1)=0
        IF(IK.GT.0) THEN
        IF(P1ARAY(IK-1,1,1).LE.0.OR.P1ARAY(IK-1,2,1).LE.0 &
        .OR.P1ARAY(IK,1,1).LE.0.OR.P1ARAY(IK,2,1).LE.0) P1ARAY(IK,3,1)=0
                        END IF
      END IF
      IF(.NOT.NOPLOT.OR.NOPLOT.AND.ALENS(9,IK).NE.0.0D0) &
      CALL PENMV1(P1ARAY(IK,1,1),P1ARAY(IK,2,1),P1ARAY(IK,3,1))
                        END IF
                        END DO
!     NOW DO THE EDGES OF THE COBS
                        ELSE
!     J NOT 1 OR 2
                        END IF
!
      IF(J.EQ.3.OR.J.EQ.4) THEN

!     DRAW THE X EDGES AND RETURN
                        DO I=STASUR,STPSUR
!
!     NOW DRAW THE X EDGES OF THE COBS AT SURFACE I
!     WITH THE PEN UP, GO TO THE STARTING PLOT POSITION
!
      IF(I.EQ.0) THEN
                        IPST=0
                        ELSE
!                       NOT OBJECT
!
!     NOW DRAW THE X EDGES OF THE CLAP AT SURFACE I
!     WITH THE PEN UP, GO TO THE STARTING PLOT POSITION
!
!     IF THE CURRENT SURFACE IS PRECEEDED BY AIR OR REFL
!     AND ALL THE REFRACTIVE INDICES ARE 1 OR -1, DON'T
!     DRAW THE EDGE. IN ALL OTHER CASES, DRAW THE EDGE.
      IF(GLANAM(I-1,2).EQ.'AIR          '.OR. &
      GLANAM(I-1,2).EQ.'PERFECT      '.OR. &
      GLANAM(I-1,2).EQ.'IDEAL        '.OR. &
      GLANAM(I-1,2).EQ.'REFLTIRO     '.OR. &
      GLANAM(I-1,2).EQ.'REFLTIR      '.OR. &
      GLANAM(I-1,2).EQ.'REFL         '.AND. &
      DABS(ALENS(46,I-1)).EQ.1.0D0.AND. &
      DABS(ALENS(47,I-1)).EQ.1.0D0.AND. &
      DABS(ALENS(48,I-1)).EQ.1.0D0.AND. &
      DABS(ALENS(49,I-1)).EQ.1.0D0.AND. &
      DABS(ALENS(50,I-1)).EQ.1.0D0) THEN
                        IPST=0
                        ELSE
      IF(ALENS(16,I).NE.0.0D0.AND.ALENS(16,I-1).NE.0.0D0) THEN
                        IPST=1
      IF(I.EQ.STASUR.AND.GLANAM(I,2).EQ.'AIR') IPST=0
                        ELSE
                        IPST=0
                        END IF
                        END IF
                        END IF
        IF(I.GT.STASUR) THEN
           IF(GLANAM(I-1,1).EQ.'MYGLASS') THEN
              IPST=1
           END IF
        END IF
      IF(EDGE(J,1,I).GT.1.0D6) EDGE(J,1,I)=1.0D6
      IF(EDGE(J,2,I).GT.1.0D6) EDGE(J,2,I)=1.0D6
      IF(EDGE(J,1,I).LT.-1.0D6) EDGE(J,1,I)=-1.0D6
      IF(EDGE(J,2,I).LT.-1.0D6) EDGE(J,2,I)=-1.0D6
      IX=INT(EDGE(J,1,I))
      IY=INT(EDGE(J,2,I))
      P1ARAY(I,1,1)=IX
      P1ARAY(I,2,1)=IY
      P1ARAY(I,3,1)=IPST
        IF(.NOT.PLEXIS) PLEXIS=.TRUE.
                END DO
!     FINISHED WITH THAT EDGE, LIFT PEN
      IPST=0
!     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE P1ARAY ARRAY
!
      COLPAS=COLEDG
      CALL MY_COLTYP(COLPAS)
        FIXUP=.FALSE.
                        DO IK=STASUR,STPSUR
      IF(IK.GT.STASUR) THEN
      IF(ALENS(127,IK).NE.0.0D0.OR.IK.NE.STASUR.AND. &
      ALENS(127,IK-1).NE.0.0D0) THEN
                        ELSE
        IF(IK.EQ.0) P1ARAY(IK,3,1)=0
        IF(IK.GT.0) THEN
        IF(P1ARAY(IK-1,1,1).LE.0.OR.P1ARAY(IK-1,2,1).LE.0 &
        .OR.P1ARAY(IK,1,1).LE.0.OR.P1ARAY(IK,2,1).LE.0) P1ARAY(IK,3,1)=0
                        END IF
      END IF
      IF(.NOT.NOPLOT.OR.NOPLOT.AND.ALENS(9,IK).NE.0.0D0) &
      CALL PENMV1(P1ARAY(IK,1,1),P1ARAY(IK,2,1),P1ARAY(IK,3,1))
                        END IF
                        END DO
                        ELSE
!     J NOT 3 OR 4
                        END IF
                        END DO
                        ELSE
!     NOT EDGEX
                        END IF
!
      IF(WQ.EQ.'EDGEY') THEN
!     FIRST DO THE EDGES OF THE CLAPS
!     DRAW THE Y EDGES AND RETURN
                        DO J=1,4
      IF(J.EQ.1.OR.J.EQ.2) THEN
                        DO I=STASUR,STPSUR
      IF(I.EQ.0) THEN
                        IPST=0
                        ELSE
!                       NOT OBJECT
!
!     NOW DRAW THE Y EDGES OF THE CLAP AT SURFACE I
!     WITH THE PEN UP, GO TO THE STARTING PLOT POSITION
!
!     IF THE CURRENT SURFACE IS PRECEEDED BY AIR OR REFL
!     AND ALL THE REFRACTIVE INDICES ARE 1 OR -1, DON'T
!     DRAW THE EDGE. IN ALL OTHER CASES, DRAW THE EDGE.
      IF(GLANAM(I-1,2).EQ.'AIR          '.OR. &
      GLANAM(I-1,2).EQ.'PERFECT      '.OR. &
      GLANAM(I-1,2).EQ.'IDEAL        '.OR. &
      GLANAM(I-1,2).EQ.'REFLTIRO     '.OR. &
      GLANAM(I-1,2).EQ.'REFLTIR      '.OR. &
      GLANAM(I-1,2).EQ.'REFL         '.AND. &
      DABS(ALENS(46,I-1)).EQ.1.0D0.AND. &
      DABS(ALENS(47,I-1)).EQ.1.0D0.AND. &
      DABS(ALENS(48,I-1)).EQ.1.0D0.AND. &
      DABS(ALENS(49,I-1)).EQ.1.0D0.AND. &
      DABS(ALENS(50,I-1)).EQ.1.0D0) THEN
                        IPST=0
                        ELSE
                        IPST=1
      IF(I.EQ.STASUR.AND.GLANAM(I,2).EQ.'AIR') IPST=0
                        END IF
                        END IF
        IF(I.GT.1) THEN
          IF(GLANAM(I-1,1).EQ.'MYGLASS') THEN
            IPST=1
          END IF
        END IF
!
      IF(EDGE(J,1,I).GT.1.0D6) EDGE(J,1,I)=1.0D6
      IF(EDGE(J,2,I).GT.1.0D6) EDGE(J,2,I)=1.0D6
      IF(EDGE(J,1,I).LT.-1.0D6) EDGE(J,1,I)=-1.0D6
      IF(EDGE(J,2,I).LT.-1.0D6) EDGE(J,2,I)=-1.0D6
      IX=INT(EDGE(J,1,I))
      IY=INT(EDGE(J,2,I))
      P1ARAY(I,1,1)=IX
      P1ARAY(I,2,1)=IY
      P1ARAY(I,3,1)=IPST
        IF(.NOT.PLEXIS) PLEXIS=.TRUE.
                        END DO
!     FINISHED WITH THAT EDGE, LIFT PEN
      IPST=0
!     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE P1ARAY ARRAY
!
      COLPAS=COLEDG
      CALL MY_COLTYP(COLPAS)
        FIXUP=.FALSE.

                        DO IK=STASUR,STPSUR
        ! Original code did not compile with checkflags
  !      IF(ALENS(127,IK).NE.0.0D0.OR.IK.NE.STASUR.AND.
  !   1ALENS(127,IK-1).NE.0.0D0) THEN
        SKIPNEXT = 0
        IF(ALENS(127,IK).NE.0.0D0) SKIPNEXT = 1
        IF((IK-1).GT.-1) THEN
          IF(IK.NE.STASUR.AND.ALENS(127,IK-1).NE.0.0D0) THEN
            SKIPNEXT=1
          END IF
        END IF
          IF(SKIPNEXT.EQ.1) THEN
                        ELSE
        IF(IK.EQ.0) P1ARAY(IK,3,1)=0
        IF(IK.GT.0) THEN
        IF(P1ARAY(IK-1,1,1).LE.0.OR.P1ARAY(IK-1,2,1).LE.0 &
        .OR.P1ARAY(IK,1,1).LE.0.OR.P1ARAY(IK,2,1).LE.0) P1ARAY(IK,3,1)=0
                        END IF
      IF(.NOT.NOPLOT.OR.NOPLOT.AND.ALENS(9,IK).NE.0.0D0) &
      CALL PENMV1(P1ARAY(IK,1,1),P1ARAY(IK,2,1),P1ARAY(IK,3,1))
                        END IF
                        END DO
!     NOW DO THE EDGES OF THE COBS
                        ELSE
!     J NOT 1 OR 2
                        END IF
!
      IF(J.EQ.3.OR.J.EQ.4) THEN

!     DRAW THE Y EDGES AND RETURN
                        DO I=STASUR,STPSUR
!
!     NOW DRAW THE Y EDGES OF THE COBS AT SURFACE I
!     WITH THE PEN UP, GO TO THE STARTING PLOT POSITION
!
      IF(I.EQ.0) THEN
                        IPST=0
                        ELSE
!                       NOT OBJECT
!
!     NOW DRAW THE X EDGES OF THE CLAP AT SURFACE I
!     WITH THE PEN UP, GO TO THE STARTING PLOT POSITION
!
!     IF THE CURRENT SURFACE IS PRECEEDED BY AIR OR REFL
!     AND ALL THE REFRACTIVE INDICES ARE 1 OR -1, DON'T
!     DRAW THE EDGE. IN ALL OTHER CASES, DRAW THE EDGE.
      IF(GLANAM(I-1,2).EQ.'AIR          '.OR. &
      GLANAM(I-1,2).EQ.'PERFECT      '.OR. &
      GLANAM(I-1,2).EQ.'IDEAL        '.OR. &
      GLANAM(I-1,2).EQ.'REFLTIR      '.OR. &
      GLANAM(I-1,2).EQ.'REFLTIRO     '.OR. &
      GLANAM(I-1,2).EQ.'REFL         '.AND. &
      DABS(ALENS(46,I-1)).EQ.1.0D0.AND. &
      DABS(ALENS(47,I-1)).EQ.1.0D0.AND. &
      DABS(ALENS(48,I-1)).EQ.1.0D0.AND. &
      DABS(ALENS(49,I-1)).EQ.1.0D0.AND. &
      DABS(ALENS(50,I-1)).EQ.1.0D0) THEN
                        IPST=0
                        ELSE
      IF(ALENS(16,I).NE.0.0D0.AND.ALENS(16,I-1).NE.0.0D0) THEN
                        IPST=1
      IF(I.EQ.STASUR.AND.GLANAM(I,2).EQ.'AIR') IPST=0
                        ELSE
                        IPST=0
                        END IF
                        END IF
                        END IF
        IF(I.GT.0) THEN

        IF(GLANAM(I-1,1).EQ.'MYGLASS') THEN
                        IPST=1
                        END IF
        END IF
      IF(EDGE(J,1,I).GT.1.0D6) EDGE(J,1,I)=1.0D6
      IF(EDGE(J,2,I).GT.1.0D6) EDGE(J,2,I)=1.0D6
      IF(EDGE(J,1,I).LT.-1.0D6) EDGE(J,1,I)=-1.0D6
      IF(EDGE(J,2,I).LT.-1.0D6) EDGE(J,2,I)=-1.0D6
      IX=INT(EDGE(J,1,I))
      IY=INT(EDGE(J,2,I))
      P1ARAY(I,1,1)=IX
      P1ARAY(I,2,1)=IY
      P1ARAY(I,3,1)=IPST
        IF(.NOT.PLEXIS) PLEXIS=.TRUE.
                END DO
!     FINISHED WITH THAT EDGE, LIFT PEN
      IPST=0
!     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE P1ARAY ARRAY
!
      COLPAS=COLEDG
      CALL MY_COLTYP(COLPAS)
        FIXUP=.FALSE.
                        DO IK=STASUR,STPSUR
        SKIPNEXT = 0
        IF(ALENS(127,IK).NE.0.0D0) SKIPNEXT = 1
        IF((IK-1).GT.-1) THEN
          IF(IK.NE.STASUR.AND.ALENS(127,IK-1).NE.0.0D0) THEN
            SKIPNEXT=1
          END IF
        END IF
          IF(SKIPNEXT.EQ.1) THEN
!      IF(ALENS(127,IK).NE.0.0D0.OR.IK.NE.STASUR.AND.
!     1ALENS(127,IK-1).NE.0.0D0) THEN
                        ELSE
!
        IF(IK.EQ.0) P1ARAY(IK,3,1)=0
        IF(IK.GT.0) THEN
        IF(P1ARAY(IK-1,1,1).LE.0.OR.P1ARAY(IK-1,2,1).LE.0 &
        .OR.P1ARAY(IK,1,1).LE.0.OR.P1ARAY(IK,2,1).LE.0) P1ARAY(IK,3,1)=0
                        END IF
!
      IF(.NOT.NOPLOT.OR.NOPLOT.AND.ALENS(9,IK).NE.0.0D0) &
      CALL PENMV1(P1ARAY(IK,1,1),P1ARAY(IK,2,1),P1ARAY(IK,3,1))
                        END IF
                        END DO
                        ELSE
!     J NOT 3 OR 4
                        END IF
                        END DO
                        ELSE
!     NOT EDGEY
                        END IF
!
!
!     HERE WE DO THE PLOT LI AND PLOT AXIS DRAWING
                        IF(.NOT.VIGFLG.AND.PLTVIG) THEN
                        CALL VIGSHO
                        VIGFLG=.TRUE.
                        ELSE
                        END IF
      DEALLOCATE(EDGE,STAT=ALLOERR)
                        RETURN
                        END