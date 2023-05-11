! SUB VIE.FOR
        SUBROUTINE VIE_NEW(CACOCHVIE)
          use global_widgets
          use handlers, only:  updateterminallog
          use mod_plotopticalsystem
!

        IMPLICIT NONE

!
!       THIS PROGRAM CONTROLS THE "VIE" AND "VIECO" COMMANDS
!
      REAL*8 VIEROT,VHI,VLO,XVHI,XVLO,YVHI,YVLO, relAngle

      integer :: numRays, ii, jj, numFields

      real, dimension(3) :: fFields
!
      INTEGER DFLAG,VIEXOF,VIEYOF
!
        COMMON/OFFVIE/VIEXOF,VIEYOF,VIEROT
!
      INTEGER COLPAS,VDF1,VDF2,VDF3,VS1,VS2 &
      ,VS3,I,J,CACOCHVIE
!
      CHARACTER VIEWQ*8
!
      REAL*8 VIEW1,VIEW2,VIEW3,MDX,MDY,SFI,GAMGAM
!
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATMAC.INC'
!       JN ADD HACK to figure out plotting
!        INCLUDE 'DATHGR.INC'
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
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              PRINT *, "LINE 2338, SCFAY IS ", SCFAY
              REST_KDP(1)=RESTINPT(1)
!
      IF(VIEWQ.EQ.'YZ'.OR.VIEWQ.EQ.'ORTHO'.OR.VIEWQ.EQ.'XY') THEN
!     PLOT PROFY
              SAVE_KDP(1)=SAVEINPT(1)
!             PLOT PROFY
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
!     PLOT EDGEY
              SAVE_KDP(1)=SAVEINPT(1)
!             PLOT EDGEY
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
!
      IF(VIEWQ.EQ.'XZ'.OR.VIEWQ.EQ.'ORTHO'.OR.VIEWQ.EQ.'XY') THEN
!     PLOT PROFX
              SAVE_KDP(1)=SAVEINPT(1)
!             PLOT PROFX
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
!     PLOT EDGEX
              SAVE_KDP(1)=SAVEINPT(1)
!             PLOT EDGEX
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
!
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
!
!     OTHER RAYS
!
!     Y FIELDS OF VIEW ARE DONE WHEN VIEW IS YZ, XY OR ORTHO
      IF(VIEWQ.EQ.'YZ'.OR.VIEWQ.EQ.'ORTHO'.OR.VIEWQ.EQ.'XY') THEN

numRays = ld_settings%num_field_rays ! TODO:  Abstract and have selectable
numFields = sysConfig%numFields !3 ! TODO:  Abstract and selectable
!fFields =  sysConfig%relativeFields
![0.0, 0.7, 1.0] ! TODO:  Abstract and selectable
PRINT *, "NUM FIELDS IS ", numFields
do jj = 1, numFields


              !CALL MY_COLTYP(jj+2)


              SAVE_KDP(1)=SAVEINPT(1)
              COLRAY = sysConfig%fieldColorCodes(jj)
              WRITE(INPUT, *) "FOB ", sysConfig%relativeFields(2,jj)
              !INPUT='FOB'
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
! !     GUT RAY A SECOND TIME
!               WW1=0.0D0
!               WW2=0.0D0
!               WW3=SYSTEM(11)
!               WVN=WW3
!               MSG=.FALSE.
!       WW4=1.0D0
!       NOCOAT=.TRUE.
!               GRASET=.TRUE.
        ! IF(CACOCHVIE.EQ.1) CACOCH=1
        !       CALL RAYTRA
        !       REST_KDP(1)=RESTINPT(1)
        !       SAVE_KDP(1)=SAVEINPT(1)
        !       IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
        !       REST_KDP(1)=RESTINPT(1)


do ii = 0, numRays-1
  !relAngle = YVHI + ii*(YVLO-YVHI)/(numRays-1)
  relAngle = YVLO + ii*(YVHI-YVLO)/(numRays-1)
  PRINT *, "relAngle is ", relAngle
              SAVE_KDP(1)=SAVEINPT(1)
              WW1 = relAngle
              !WW1=YVHI
              !PRINT *, "YVHI  is ", YVHI
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
!
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
  end do ! Angles

 end do  !  Fields


!! UNCOMMENT OUT AFTER RAY COLOR DEBUGGING!!!

      IF(VIEWQ.NE.'YZ') THEN
!     PLUS X-RAY
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
!
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
!     MINUS X-RAY
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
!
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
      END IF
      END IF ! YZ plotting



      IF(VIEWQ.EQ.'XZ'.OR.VIEWQ.EQ.'ORTHO'.OR.VIEWQ.EQ.'XY') THEN
!     DO XZ PLANE FIELDS
!
!     FIRST FOB 0
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
!     FIRST MARGINAL RAYS
      IF(VIEWQ.NE.'XZ') THEN
!     PLUS Y-RAY
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
!
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
!     MINUS Y-RAY
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
!
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
               END IF
!     PLUS X-RAY
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
!
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
!     MINUS X-RAY
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
!
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
!     TOP X FOV
!     FIRST FOB 0 1
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
!
!     FIRST MARGINAL RAYS
!     GUT RAY
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
!
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
!     FIRST MARGINAL RAYS
      IF(VIEWQ.NE.'XZ') THEN
!     PLUS Y-RAY
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
!
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
!     MINUS Y-RAY
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
!
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
               END IF
!     PLUS X-RAY
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
!
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
!     MINUS X-RAY
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
!
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
!     BOTTOM X FOV
!     FOB 0 -1
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
!
!     FIRST MARGINAL RAYS
!     GUT RAY
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
!
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
!
!     FIRST MARGINAL RAYS
      IF(VIEWQ.NE.'XZ') THEN
              call updateTerminalLog("Not XY Marginal Ray being plotted", "black")
              !PRINT *, "Not XY Marginal ray being plotted"
!     PLUS Y-RAY
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
!
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)

!     MINUS Y-RAY
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
!
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
               END IF
!     PLUS X-RAY
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
!
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
!     MINUS X-RAY
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
!
              SAVE_KDP(1)=SAVEINPT(1)
              IF(RAYEXT) CALL VIERAY(VIEW2,VIEW3,VDF2,VDF3,VS2,VS3)
              REST_KDP(1)=RESTINPT(1)
              ELSE
!     NOT YZ PLANE FIELDS
              END IF
!     NOW CLAPS,PROFS AND EDGES

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
