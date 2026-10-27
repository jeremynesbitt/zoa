!       NINTH FILE FOR LENS DATABASE MANAGER FILES

SUBROUTINE RETRES
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_last_surf, sys_set_tilt_ret
   use mod_lens_data_manager, only: ldm
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   INTEGER I,RETSURF,SINB
   real(real64) D11,D12,D13
   real(real64) D21,D22,D23
   real(real64) D31,D32,D33,BETA,ALPHA,GAMMA,COSB
!
!
!
   DO I=0,INT(sys_last_surf())
      IF(surf_tilt_flag(I).EQ.6.0D0) THEN
!     THERE IS A TILT RETURN TO RESOLVE WITH A RETURN TO SURFACE
         RETSURF=INT(surf_ret_surf_num(I))
!     SET ALL THE TILT AND DECENTERS ON THIS SURFACE TO ZERO
         call ldm%clearFocusAndTiltDegData(I)
         call ldm%clearTiltAnglesAndDecentValues(I)
         call set_surf_decenter_z(I, 0.0D0)
!
!     HERE IS WHERE THE MAGIC GOES.
!     1. SET THE GLOBAL SURFACE TO SURFACE I
!     2. GET THE LOCATION OF SURFACES I AND RETSURF AND THEIR ORIENTATIONS
!        IN THE COORDINATE SYSTEM OF SURFACE I.
!     3. APPLY X,Y,Z DECENTERS TO PLACE SURFACE I'S VERTEX WHERE SURFACE
!        RETSURS'S VERTEX IS LOCATED.
!     4. DETERMINE THE ALPHA, BETA AND GAMMA TO APPLY TO THE ITH SURFACE
!     5. APPLY TILTS OF ALPHA,BETA AND GAMMA TO SURFACE I SO ITS AXES
!        ARE ALLIGNED WITH THE RETSURF'S AXES
!     5. SET surf_tilt_return_flag(I) TO 1.0D0
         SAVE_KDP(10)=SAVEINPT(10)
         WC='GLOBAL'
         SQ=0
         S1=1
         S2=0
         S3=0
         S4=0
         S5=0
         DF1=0
         DF2=1
         DF3=1
         DF4=1
         DF5=1
         SN=1
         SST=0
         STI=0
         W1=DBLE(I)
         CALL GLOBAL
         REST_KDP(10)=RESTINPT(10)
!     NOW GET ALL THE GOODIES
         D11=VERTEX(4,RETSURF)
         D12=VERTEX(5,RETSURF)
         D13=VERTEX(6,RETSURF)
         D21=VERTEX(7,RETSURF)
         D22=VERTEX(8,RETSURF)
         D23=VERTEX(9,RETSURF)
         D31=VERTEX(10,RETSURF)
         D32=VERTEX(11,RETSURF)
         D33=VERTEX(12,RETSURF)

!     NOW CALCULATE ALPHA,BETA AND GAMMA AND ASSIGN THEM TO SURFACE I
!     CALCULATE BETA
         BETA=DASIN(-D31)
         COSB=DCOS(BETA)
         IF(COSB.NE.0.0D0) THEN
!     COSINE OF BETA IS NOT ZERO
            IF((D32/COSB).EQ.0.0D0.AND.(D33/COSB).NE.0.0D0) ALPHA=0.0D0
            IF((D32/COSB).EQ.0.0D0.AND.(D33/COSB).EQ.0.0D0) ALPHA=0.0D0
            IF((D32/COSB).NE.0.0D0.AND.(D33/COSB).EQ.0.0D0) ALPHA=PII/2.0D0
            IF((D32/COSB).NE.0.0D0.AND.(D33/COSB).NE.0.0D0)ALPHA=DATAN2((D32/COSB),(D33/COSB))
            IF((D21/COSB).EQ.0.0D0.AND.(D11/COSB).NE.0.0D0) GAMMA=0.0D0
            IF((D21/COSB).EQ.0.0D0.AND.(D11/COSB).EQ.0.0D0) GAMMA=0.0D0
            IF((D21/COSB).NE.0.0D0.AND.(D11/COSB).EQ.0.0D0) GAMMA=PII/2.0D0
            IF((D21/COSB).NE.0.0D0.AND.(D11/COSB).NE.0.0D0)GAMMA=DATAN2((-D21/COSB),(D11/COSB))
         END IF
         IF(COSB.EQ.0.0D0) THEN
!     COSINE OF BETA IS ZERO
            IF(D31.EQ.-1.0D0) SINB=1
            IF(D31.EQ.1.0D0) SINB=-1
            IF(SINB.EQ.1) BETA=PII/2.0D0
            IF(SINB.EQ.-1) BETA=-PII/2.0D0
            GAMMA=0.0D0
            IF(SINB.EQ.1) THEN
               IF(D12.EQ.0.0D0.AND.D13.NE.0.0D0)ALPHA=0.0D0
               IF(D12.EQ.0.0D0.AND.D13.EQ.0.0D0)ALPHA=0.0D0
               IF(D12.NE.0.0D0.AND.D13.EQ.0.0D0)ALPHA=PII/2.0D0
               IF(D12.NE.0.0D0.AND.D13.NE.0.0D0)ALPHA=DATAN2((D12),(D13))
            END IF
            IF(SINB.EQ.-1) THEN
               IF(D12.EQ.0.0D0.AND.D13.NE.0.0D0)ALPHA=0.0D0
               IF(D12.EQ.0.0D0.AND.D13.EQ.0.0D0)ALPHA=0.0D0
               IF(D12.NE.0.0D0.AND.D13.EQ.0.0D0)ALPHA=PII/2.0D0
               IF(D12.NE.0.0D0.AND.D13.NE.0.0D0)ALPHA=DATAN2((-D12),(-D13))
            END IF
         END IF
         ALPHA=ALPHA*(180.0D0/PII)
         BETA=BETA*(180.0D0/PII)
         GAMMA=GAMMA*(180.0D0/PII)
         call set_surf_alpha(I, ALPHA)
         call set_surf_beta(I, BETA)
         call set_surf_gamma(I, GAMMA)
         call set_surf_alpha_deg(I, ALPHA)
         call set_surf_beta_deg(I, BETA)
         call set_surf_gamma_deg(I, GAMMA)
!
!     NOW APPLY THE DECENTERS
         call set_surf_decenter_x(I, VERTEX(1,RETSURF))
         call set_surf_decenter_y(I, VERTEX(2,RETSURF))
         call set_surf_decenter_z(I, VERTEX(3,RETSURF))
         call set_surf_focus_dx(I, VERTEX(1,RETSURF))
         call set_surf_focus_dy(I, VERTEX(2,RETSURF))
         call set_surf_focus_dz(I, VERTEX(3,RETSURF))
         IF(surf_tilt_flag(I).EQ.6.0D0) THEN
            call set_surf_decenter_flag(I, 0)
            IF(surf_decenter_y(I).NE.0.0D0.OR.surf_decenter_x(I).NE.0.0D0.OR.surf_decenter_z(I).NE.0.0D0)call set_surf_decenter_flag(I, 1)
         END IF
!     SET TILT RET RES FLAG TO ON AND SET THE TILT TYPE TO TILT
         call set_surf_tilt_return_flag(I, 1)
         call set_surf_tilt_flag(I, 1)
      END IF
   END DO
!     SET TILT RET FLAG TO RESOLVED FOR THE LENS
   call sys_set_tilt_ret(1.0D0)
!     TURN OFF GLOBAL MODE
   IF(RAYCLEAR) THEN
      FOBYES=.FALSE.
      REFEXT=.FALSE.
      SPDEXT=.FALSE.
      GSPDEXT=.FALSE.
      CPFNEXT=.FALSE.
      CALL DELPSF
      RAYEXT=.FALSE.
      POLEXT=.FALSE.
   END IF
   GLOBE=.FALSE.
   GLSURF=0
   OFFX=0.0D0
   OFFY=0.0D0
   OFFZ=0.0D0
   OFFA=0.0D0
   OFFB=0.0D0
   OFFC=0.0D0
   RETURN
END
! SUB REMPIK.FOR
SUBROUTINE REMPIK
!
   use DATCFG
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS SUBROUTINE IS CALLED FROM SUBROUTINE CFPIKD
!       AND REMOVES THE ENTRY DESIGNATED BY THE VALUE OF
!       PIK
!
!       THE COMMAND IS: REMPIK,PIK
!
   INTEGER PIK,J,PIKLIN
!
   COMMON/DPIKER2/PIKLIN
!
!
   PIK=PIKLIN
!
!       DELETE THE LINE
   DO 20 J=PIK,(CFGCNT(F12)-1)
      HOLDER=CONFG(F12,(J+1))
      CONFG(F12,J)=HOLDER
20 CONTINUE
!       ONE LINE DELETED
   CFGCNT(F12)=CFGCNT(F12)-1
!
!       CLEAN UP CONFIGS
   CALL CFGCLN
   RETURN
!
END
! SUB QUERRY.FOR
SUBROUTINE QUERRYY
!
   use DATCFG
   use DATSUB
   use DATLEN
   use mod_surface
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       HANDLES RESPONSE TO '?' TYPED IN AS THE COMMAND WORD.
!
   INTEGER IMAC,IMACEN,MACLOC,CNTR,MMIJ,I
!
   COMMON/CNTER/CNTR
!
   CHARACTER MACNAM*8
!
   COMMON/MACID/MACNAM
!
   COMMON/MACID2/MACLOC,IMACEN
!
   COMMON/IMAC/IMAC
!
   COMMON/MIJ/MMIJ
!
!
   IF(IN.EQ.5) THEN
      IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
         CALL REPORT_ERROR_AND_FAIL('"?" TAKES NO EXPLICIT INPUT'//'\n'//'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!
      IF(F1.EQ.1.AND.F17.EQ.1.AND.F18.EQ.0) THEN
         OUTLYNE= 'READY FOR SPECT INPUT'
         CALL SHOWIT(1)
         RETURN
      ELSE
      END IF
      IF(F1.EQ.1.AND.F17.EQ.1.AND.F18.EQ.1) THEN
         WRITE(OUTLYNE,130) CNTR
130      FORMAT('READY FOR SPECT-TABLE INPUT AT TABLE ITEM ',I5)
         CALL SHOWIT(1)
         RETURN
      END IF
      IF(F1.EQ.1) THEN
         OUTLYNE= 'READY FOR CMD LEVEL INPUT'
         CALL SHOWIT(1)
      ELSE
!               F1 MUST BE = 0, NOT AT CMD LEVEL
!*******************************************************************
         IF(F2.EQ.1) THEN
            WRITE(OUTLYNE,131) MACNAM,IMAC
131         FORMAT('READY FOR MACRO ',A8,' INPUT AT LINE ',I3)
            CALL SHOWIT(1)
         END IF
!     F3=1 (macro editing mode) removed with binary macro system
         IF(F5.EQ.1) THEN
            WRITE(OUTLYNE,133) SURF
133         FORMAT('READY FOR LENS INPUT AT SURFACE NUMBER ',I3)
            CALL SHOWIT(1)
         END IF
         IF(F6.EQ.1) THEN
            WRITE(OUTLYNE,134) SURF
134         FORMAT('READY FOR LENS UPDATE AT SURFACE NUMBER ',I3)
            CALL SHOWIT(1)
         END IF
         IF(F27.EQ.1) THEN
            OUTLYNE= 'READY FOR MERIT INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,135) (OPCNT+1)
            CALL SHOWIT(1)
135         FORMAT('NEXT OPERAND NUMBER WILL BE = ',I3)
         END IF
         IF(F27.EQ.2) THEN
            OUTLYNE=  'READY FOR MERIT UPDATE'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,136) (OPCNT+1)
136         FORMAT('NEXT OPERAND NUMBER WILL BE = ',I3)
            CALL SHOWIT(1)
         END IF
         IF(F29.EQ.1) THEN
            OUTLYNE= 'READY FOR VARIABLE INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,137) (VBCNT+1)
137         FORMAT('NEXT VARIABLE NUMBER WILL BE = ',I3)
            CALL SHOWIT(1)
         END IF
         IF(F29.EQ.2) THEN
            OUTLYNE=  'READY FOR VARIABLE UPDATE'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,137) (VBCNT+1)
            CALL SHOWIT(1)
         END IF
         IF(F51.EQ.1) THEN
            OUTLYNE= 'READY FOR TVAR INPUT'
            CALL SHOWIT(1)
         END IF
         IF(F51.EQ.2) THEN
            OUTLYNE=  'READY FOR TVAR UPDATE'
            CALL SHOWIT(1)
         END IF
         IF(F52.EQ.1) THEN
            OUTLYNE= 'READY FOR COMPVAR INPUT'
            CALL SHOWIT(1)
         END IF
         IF(F52.EQ.2) THEN
            OUTLYNE=  'READY FOR COMPVAR UPDATE'
            CALL SHOWIT(1)
         END IF
         IF(F53.EQ.1) THEN
            OUTLYNE= 'READY FOR TOPER INPUT'
            CALL SHOWIT(1)
         END IF
         IF(F53.EQ.2) THEN
            OUTLYNE=  'READY FOR TOPER UPDATE'
            CALL SHOWIT(1)
         END IF
         IF(F54.EQ.1) THEN
            OUTLYNE= 'READY FOR FOCRIT INPUT'
            CALL SHOWIT(1)
         END IF
         IF(F54.EQ.2) THEN
            OUTLYNE=  'READY FOR FOCRIT UPDATE'
            CALL SHOWIT(1)
         END IF
         IF(F7.EQ.1) THEN
            WRITE(OUTLYNE,140) SURF
140         FORMAT('READY FOR SPSRF INPUT AT SURFACE NUMBER ',I3)
            CALL SHOWIT(1)
         END IF
         IF(F8.EQ.1) THEN
            WRITE(OUTLYNE,141) SURF
141         FORMAT('READY FOR SPSRF UPDATE AT SURFACE NUMBER ',I3)
            CALL SHOWIT(1)
         END IF
         IF(F9.EQ.1) THEN
            OUTLYNE= 'READY FOR SPFIT INPUT'
            CALL SHOWIT(1)
         END IF
         IF(F10.EQ.1) THEN
            IF(F13.EQ.0.AND.F14.EQ.0) THEN
               OUTLYNE= 'READY FOR CMD LEVEL INPUT'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,110) F12
               CALL SHOWIT(1)
110            FORMAT('FROM CONFIGS LEVEL AT CFG NUMBER ',I3)
               I=200-CFGCNT(F12)+1
               WRITE(OUTLYNE,111) I
               CALL SHOWIT(1)
111            FORMAT(I3,' :ENTRIES AVALIABLE FOR STORAGE')
            END IF
            IF(F13.EQ.1) THEN
               OUTLYNE= 'READY FOR UPDATE LENS DATA'
               CALL SHOWIT(1)
112            FORMAT('FROM CONFIGS LEVEL AT CFG NUMBER ',I3)
               WRITE(OUTLYNE,112) F12
               CALL SHOWIT(1)
               I=200-CFGCNT(F12)+1
               WRITE(OUTLYNE,113) I
               CALL SHOWIT(1)
113            FORMAT(I3,' :ENTRIES AVALIABLE FOR STORAGE')
!       NOT UPDATE LENS COMMANDS
            END IF
            IF(F14.EQ.1) THEN
               OUTLYNE= 'READY FOR UPDATE SPSRF DATA'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,114) F12
               CALL SHOWIT(1)
114            FORMAT('FROM CONFIGS LEVEL AT CFG NUMBER ',I3)
               I=200-CFGCNT(F12)+1
               WRITE(OUTLYNE,115) I
               CALL SHOWIT(1)
115            FORMAT(I3,' :ENTRIES AVALIABLE FOR STORAGE')
            ELSE
!       NOT AT UPDATE SPSRFIN CONFIGS
            END IF
         END IF
         IF(F11.EQ.1) THEN
            IF(F13.EQ.0.AND.F14.EQ.0) THEN
               OUTLYNE= 'READY FOR CMD LEVEL INPUT'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,116) F12
               CALL SHOWIT(1)
116            FORMAT('FROM CONFIGS UPDATE LEVEL AT CFG NUMBER ',I3)
               I=200-CFGCNT(F12)+1
               WRITE(OUTLYNE,117) I
               CALL SHOWIT(1)
117            FORMAT(I3,' :ENTRIES AVALIABLE FOR STORAGE')
            ELSE
!       NOT AT CMD LEVEL IN CONFIGS
            END IF
            IF(F13.EQ.1) THEN
               OUTLYNE= 'READY FOR UPDATE LENS DATA'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,118) F12
               CALL SHOWIT(1)
118            FORMAT('FROM CONFIGS UPDATE LEVEL AT CFG NUMBER ',I3)
               I=200-CFGCNT(F12)+1
               WRITE(OUTLYNE,119) I
               CALL SHOWIT(1)
119            FORMAT(I3,' :ENTRIES AVALIABLE FOR STORAGE')
            ELSE
!       NOT UPDATE LENS COMMANDS
            END IF
            IF(F14.EQ.1) THEN
               OUTLYNE= 'READY FOR UPDATE SPSRF DATA'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,120) F12
               CALL SHOWIT(1)
120            FORMAT('FROM CONFIGS UPDATE LEVEL AT CFG NUMBER ',I3)
               I=200-CFGCNT(F12)+1
               WRITE(OUTLYNE,121) I
               CALL SHOWIT(1)
121            FORMAT(I3,' :ENTRIES AVALIABLE FOR STORAGE')
            ELSE
!       NOT AT UPDATE SPSRFIN CONFIGS
            END IF
         END IF
      END IF
      RETURN
   ELSE
!     NO RESPONSE TO ? PRINTED IN IN NOT 5
   END IF
END
! SUB PTORIC.FOR
SUBROUTINE PTORIC
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_last_surf
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PTORIC WHICH IMPLEMENTS THE TORIC,TR,TC,
!       RTORIC OR CTORIC COMMANDS AT THE CMD LEVEL
!       THE ONLY VALID QUALIFIERS ARE (BLANK) AND "ALL","OB" AND "OBJ"
!       THE NUMERIC ENTRY W1, IS THE SURFACE DESIGNATOR
!       FOR PRINTOUT OF ONE SURFACE'S DATA ONLY.
!       TR,TORIC,AND RTORIC PRODUCE RADIUS OUTPUT.
!       TC AND CTORIC PRODUCE CURVATURE OUTPUT.
!
   CHARACTER TYPE*10
!
   INTEGER RADIUS,TORCNT,I
!
   real(real64) RADVAL,CURVAL
!
!
!               THIS IS A CMD LEVEL COMMAND ONLY
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'TORIC OUTPUT COMMANDS TAKE NO NUMERIC WORD #2 THROUGH #5'//'\n'//&
      & 'OR STRING INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(SQ.EQ.1.AND.S1.EQ.1) THEN
      S1=0
      DF1=0
      W1=0.0D0
      CALL REPORT_ERROR_AND_FAIL(&
      & 'TORIC OUTPUT COMMANDS REQUIRE EITHER QUALIFIER OR'//'\n'//&
      & 'NUMERIC WORD #1 INPUT BUT NOT BOTH'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!
!       WHAT IF NO SURFACES EXIST
   IF(sys_last_surf().EQ.0.0D0) THEN
      CALL REPORT_ERROR_AND_FAIL('NO TORICS EXIST'//'\n'//'LENS SYSTEM HAS NO SURFACES', 1)
      RETURN
   END IF
!       W1 DESIGNATES THE SURFACE FOR WHICH THE TORIC
!       DATA IS TO BE OUTPUT.
!       IF THE QUALIFIER "ALL" IS USED, THEN THE TILT/DEC DATA FOR
!       THE ENTIRE LENS IS PRINTED. IF THE QUALIFIER IS "OB" OR "OBJ"
!       THEN DATA FOR THE OBJECT SURFACE IS PRINTED.
!
   IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
!
!       THIS IS SAME AS SURFACE ZERO
      S1=1
      DF1=0
      SQ=0
      WQ='        '
      W1=0.0D0
   ELSE
!       WQ NOT OB OR OBJ
   END IF
   IF(SQ.EQ.1.AND.WQ.NE.'ALL') THEN
!
!       WE HAVE INVALID QUALIFIER
      CALL REPORT_ERROR_AND_FAIL('INVALID QUALIFIER INPUT'//'\n'//'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(SQ.EQ.0) THEN
!       HANDEL AN INDIVIDUAL SURFACE INCLUDING "OB" AND "OBJ"
      IF(W1.LT.0.0D0) W1=sys_last_surf()+W1
      IF(DF1.EQ.1) W1=DBLE(INT(sys_last_surf()))
      SURF=INT(W1)
      IF(SURF.GT.INT(sys_last_surf()).OR.SURF.LT.0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'SURFACE NUMBER BEYOND LEGAL RANGE'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
! SET RADIUS OR CURVATURE MODE AS APPROPRIATE
!
      IF(WC.EQ.'TR'.OR.WC.EQ.'TORIC'.OR.WC.EQ.'RTORIC') THEN
         RADIUS=1
         IF(surf_toric_curvature(SURF).EQ.0.0D0) THEN
            RADVAL=0.0D0
         ELSE
            RADVAL=1.0D0/(surf_toric_curvature(SURF))
         END IF
      ELSE
!       CURVATURE MODE
         RADIUS=0
         CURVAL=surf_toric_curvature(SURF)
      END IF
!       SET TORIC TYPE
      IF(surf_toric_flag(SURF).EQ.0.0D0) THEN
!       NO TORIC DATA, WRITE MESSAGE AND RETURN
         WRITE(OUTLYNE,110) SURF
         CALL SHOWIT(0)
         RETURN
      END IF
!       THERE IS A TORIC
      IF(surf_toric_flag(SURF).EQ.1.0D0) TYPE='Y-TORIC   '
      IF(surf_toric_flag(SURF).EQ.2.0D0) TYPE='X-TORIC   '
      IF(surf_anamorphic_flag(SURF).NE.0.0D0.AND.surf_anamorphic_conic(SURF).NE.0.0D0.OR.surf_anamorphic_flag(SURF).NE.0.0D0.OR.surf_anamorphic_conic(SURF).NE.0.0D0) THEN
!
!       THE TORIC CURVATURE HAS A CONIC OR ASPHERIC TERM
!       CHANGE TYPE NAME.
         IF(surf_toric_flag(SURF).EQ.1.0D0) TYPE='Y-ANAMORPH'
         IF(surf_toric_flag(SURF).EQ.2.0D0) TYPE='X-ANAMORPH'
      ELSE
!       SURFACES ARE JUST PLANE OLD TORICS.
      END IF
!
      IF(RADIUS.EQ.1) THEN
         IF(HEADIN) WRITE(OUTLYNE,2000)
         IF(HEADIN) CALL SHOWIT(0)
         WRITE(OUTLYNE,200) SURF,TYPE,RADVAL
         CALL SHOWIT(0)
         RETURN
      ELSE
!               CURVATURE PRINT OUT
         IF(HEADIN) WRITE(OUTLYNE,2001)
         IF(HEADIN) CALL SHOWIT(0)
         WRITE(OUTLYNE,200) SURF,TYPE,CURVAL
         CALL SHOWIT(0)
      END IF
      RETURN
   ELSE
!*********************************************************************
!       SQ=1 AND ONLY ALLOWED WQ BY NOW IS "ALL"
!       HANDEL THE WHOLE LENS
      TORCNT=0
!
      DO 16 I=0,INT(sys_last_surf())
         IF(surf_toric_flag(I).NE.0.0D0) THEN
            TORCNT=TORCNT+1
         ELSE
         END IF
16    CONTINUE
      IF(TORCNT.EQ.0) THEN
         WRITE(OUTLYNE,100)
         CALL SHOWIT(0)
         RETURN
      ELSE
!       PROCEED WITH PRINTOUT
      END IF
      IF(WC.EQ.'TR'.OR.WC.EQ.'TORIC'.OR.WC.EQ.'RTORIC') THEN
         RADIUS=1
         WRITE(OUTLYNE,1000)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,1500)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,2000)
         CALL SHOWIT(0)
      ELSE
         RADIUS=0
         WRITE(OUTLYNE,1001)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,1500)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,2001)
         CALL SHOWIT(0)
      END IF
!
      DO 15 I=0,INT(sys_last_surf())
!
!       SET TORIC TYPE
!
         IF(surf_toric_flag(I).EQ.0.0D0) THEN
!       NOT A TORIC OR ANAMORPH. JUMP TO 15 AND
!       GO TO THE NEXT SURFACE NUMBER
!       WITHOUT PRINTING DATA
            GO TO 15
         ELSE
!       PROCEED
         END IF
         IF(surf_toric_flag(I).EQ.1.0D0) TYPE='Y-TORIC   '
         IF(surf_toric_flag(I).EQ.2.0D0) TYPE='X-TORIC   '
         IF(surf_anamorphic_flag(I).NE.0.0D0.AND.surf_anamorphic_conic(I).NE.0.0D0.OR.surf_anamorphic_flag(I).NE.0.0D0.OR.surf_anamorphic_conic(I).NE.0.0D0) THEN
!
!       THE TORIC CURVATURE HAS A CONIC OR ASPHERIC TERM
!       CHANGE TYPE NAME.
            IF(surf_toric_flag(I).EQ.1.0D0) TYPE='Y-ANAMORPH'
            IF(surf_toric_flag(I).EQ.2.0D0) TYPE='X-ANAMORPH'
         ELSE
!       SURFACES ARE JUST PLANE OLD TORICS.
         END IF
!
         IF(RADIUS.EQ.1) THEN
            IF(surf_toric_curvature(I).EQ.0.0D0) THEN
               RADVAL=0.0D0
            ELSE
               RADVAL=1.0D0/(surf_toric_curvature(I))
            END IF
            WRITE(OUTLYNE,200) I,TYPE,RADVAL
            CALL SHOWIT(0)
            GO TO 15
         ELSE
!               CURVATURE PRINT OUT
            CURVAL=surf_toric_curvature(I)
            WRITE(OUTLYNE,200) I,TYPE,CURVAL
            CALL SHOWIT(0)
         END IF
!
15    CONTINUE
   END IF
!
200 FORMAT(I3,1X,A10,2X,G13.6)
!
110 FORMAT('SURF',1X,I3,1X,':NO TORIC OR ANAMORPHIC DATA')
100 FORMAT('NO TORIC OR ANAMORPHIC DATA')
1000 FORMAT('TORIC/ANAMORPHIC DATA (RADIUS MODE)')
1001 FORMAT('TORIC/ANAMORPHIC DATA (CURVATURE MODE)')
2000 FORMAT('SURF',2X,'TYPE',8X,'RADIUS')
2001 FORMAT('SURF',2X,'TYPE',8X,'CURVATURE')
1500 FORMAT(1X)
   RETURN
END
