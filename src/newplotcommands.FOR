

      SUBROUTINE PLTFILL
      !use GLOBALS
      REAL*8 VIEROT,VHI,VLO,XVHI,XVLO,YVHI,YVLO

      INTEGER DFLAG,VIEXOF,VIEYOF

        COMMON/OFFVIE/VIEXOF,VIEYOF,VIEROT

      INTEGER COLPAS,VDF1,VDF2,VDF3,VS1,VS2,VS3,I,J,CACOCHVIE

      CHARACTER VIEWQ*8

      REAL*8 VIEW1,VIEW2,VIEW3,MDX,MDY,SFI,GAMGAM

      !LOGICAL GRASET
      !LOGICAL SAVE_KDP(1:200)

      !COMMON/KDPSAVE/SAVE_KDP
      !COMMON INPUT

        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATMAC.INC'


        PRINT *, "WAS ABLE TO ADD COMMAND!"



        SCFAY = .045
        SFFAX = SCFAY



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
          DEVTYP=1

                        CALL PLTDEV1
                        GRASET=.TRUE.
!     DO A FRAME
                        CALL PLOTBOX
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='PLOT LI'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)

          VIEW1 = .045
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


              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='PLOT LOOK -1 0 0'
              CALL PROCES
      SAVE_KDP(1)=SAVEINPT(1)
      INPUT='PLOT AXIS'
      CALL PROCES
      REST_KDP(1)=RESTINPT(1)
      SAVE_KDP(1)=SAVEINPT(1)
      INPUT='PLOT YESLOOK'
      CALL PROCES
      REST_KDP(1)=RESTINPT(1)

              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='FOB'
              CALL PROCES
              PRINT *, "Scale after FOB is ", SCFAY
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

              SAVE_KDP(1)=SAVEINPT(1)
!            PLOT PROFY
              W1=4D0
              W2=5D0
              W3=90.0D0
              S1=VS2
              S2=VS3
              S3=1
              S4=0
              S5=0
              SN=1
              DF1=0
              DF2=0
              DF3=0
              DF4=1
              DF5=1
              WQ='PROF'
              SQ=1
              STI=0
              SST=0
              !PRINT *, "CALL PLTPRO1 PROFY"
              !PRINT *, "LINE 2362, SCFAY IS ", SCFAY

              PRINT *, "S1 , S2 = ", VS2, ",", VS3

              PRINT *, "Before PLTPRO1, Scale is ", SCFAY
              CALL PLTPRO1
              PRINT *, "After PLTPRO1, Scale is ", SCFAY

C     PLOT EDGEY
              SAVE_KDP(1)=SAVEINPT(1)
C             PLOT EDGEY
              !W1=8
              !W2=VIEW3
              S1=VS2
              S2=VS3
              S3=0
              S4=0
              S5=0
              SN=1
              !DF1=VDF2
              !DF2=VDF3
              DF3=1
              DF4=1
              DF5=1
              WQ='EDGEY'
              SQ=1
              STI=0
              SST=0
              CALL PLTEDG
              REST_KDP(1)=RESTINPT(1)

              PRINT *, "Scale is ", SCFAY

       END
