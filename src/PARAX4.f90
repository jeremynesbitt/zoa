!       FOURTH SET OF PARAXIAL ROUTINES GO HERE

! SUB FADJ.FOR
SUBROUTINE FADJ
!
   use DATLEN
   use DATMAI
   use mod_surface
   use mod_system, only: sys_fno_flag_x, sys_fno_flag_y, sys_fno_hold_x, sys_fno_hold_y, &
      & sys_fno_val_set, sys_fno_val_x, sys_fno_val_y, sys_last_surf, &
      & sys_na_set, sys_naox, sys_naoy, sys_sax, sys_say, sys_telecentric, &
      & sys_set_fno_flag_x, sys_set_fno_flag_y, sys_set_fno_hold_x, sys_set_fno_hold_y, &
      & sys_set_sax, sys_set_say
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE FADJ. THIS IS THE SUBROUTINE
!       WHICH PERFORMS THE FNBY/X ADJUSTMENT
!
   INTEGER ITYPEP
!
   COMMON/PTYPER/ITYPEP
!
!
   IF(ITYPEP.EQ.1) THEN
!       FIRST CHECK FOR PUY=0 AT IMAGE PLANE
      IF(DABS(PXTRAY(2,(INT(sys_last_surf())))).LE.1.0D-15) THEN
         OUTLYNE='FINAL "PUY" VALUE IS ZERO, F-NUBER HAS NO MEANING'
         CALL SHOWIT(1)
         OUTLYNE='"FNBY" ADJUSTMENT NOT PERFORMED AND WILL BE REMOVED'
         CALL SHOWIT(1)
         call sys_set_fno_flag_y(0.0D0)
         call sys_set_fno_hold_y(0.0D0)
         RETURN
      ELSE
!       PUY(FINAL) NOT ZERO, PROCEED
      END IF
!       NOW, THE CURRENT FNUMBER IS JUST 1.0/(2.0*PUY(FINAL))
!       THE DESIRED F-NUMBER IS STORED IN SYSTEM(46)
!       IF ALL SOLVED ARE SHUT OFF, THE NEW F-NUMBER
!       IS REALIZED BY SCALING THE SAY VALUE BY:
!
!       CURRENT F-NUMBER/NEW F-NUMBER
!
      call sys_set_say(sys_say()*(-(1.0D0/(2.0D0*&
      &PXTRAY(2,(INT(sys_last_surf())))))/sys_fno_hold_y()))
!
!       NOW WE PERFORM A PARAXIAL RAY TRACE WITHOUT AND SOLVES
!
!       TELECENTRIC STUFF, 11/12/2000
      IF(sys_telecentric().EQ.1.0D0) THEN
         IF(sys_na_set().EQ.0.0D0.AND.sys_fno_val_set().EQ.0.0D0) THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & 'WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'//'\n'//&
            & 'TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'//'\n'//&
            & 'VALUES'//'\n'//&
            & 'PARAXIAL TRACE STOPPED', 1)
            RETURN
         ELSE
            IF(sys_na_set().EQ.1.0D0) THEN
               call sys_set_say(surf_thickness(0)*sys_naoy())
               call sys_set_sax(surf_thickness(0)*sys_naox())
            END IF
            IF(sys_fno_val_set().EQ.1.0D0) THEN
               call sys_set_say(surf_thickness(0)/(2.0D0*sys_fno_val_y()))
               call sys_set_sax(surf_thickness(0)/(2.0D0*sys_fno_val_x()))
            END IF
         END IF
      END IF
!
      CALL TR
!
!       NOW THE PARAXIAL SOLVE DATA ARRAYS ARE LOADED WITH THE CORRECT
!       SOLVELESS DATA. NOW RE-SET ALL SOLVE TARGET VALUES
!
!       WHEN THE RETURN OCCURS, THE RETURN WILL GO TO LNSEOS IN
!       THE CASE OF HLDS OR TO SFNBY.FOR OR SER.FOR. LNSEOS.FOR
!       WILL AGAIN PERFORM THE TRACE FIXING THE EXIT PUPIL DISTACES AND
!       CALCULATING CHROMATIC AND OTHER VALUES.
!       A CALL TO PRTRA1 AND PRTRA2 IS PLACED IN SER.FOR AND SFNB.FOR
!       TO PERFORM A SIMILAR FUNCTION.
!       SVYSET RESETS ALL YZ-PLANE PARAXIAL SOLVES USING CURRENT YZ-PLANE
!       PARAXIAL DATA.
      CALL SVSET
      RETURN
   ELSE
!       ITYPEP NOT 1
   END IF
   IF(ITYPEP.EQ.2) THEN
!
!       FIRST CHECK FOR PUX=0 AT IMAGE PLANE
      IF(DABS(PXTRAX(2,(INT(sys_last_surf())))).LE.1.0D-15) THEN
         OUTLYNE='FINAL "PUX" VALUE IS ZERO, F-NUBER HAS NO MEANING'
         CALL SHOWIT(1)
         OUTLYNE='"FNBX" ADJUSTMENT NOT PERFORMED AND WILL BE REMOVED'
         CALL SHOWIT(1)
         call sys_set_fno_flag_x(0.0D0)
         call sys_set_fno_hold_x(0.0D0)
         RETURN
      ELSE
!       PUX(FINAL) NOT ZERO, PROCEED
      END IF
!       NOW, THE CURRENT FNUMBER IS JUST 1.0/(2.0*PUX(FINAL))
!       THE DESIRED F-NUMBER IS STORED IN SYSTEM(47)
!       IF ALL SOLVED ARE SHUT OFF, THE NEW F-NUMBER
!       IS REALIZED BY SCALING THE SAX VALUE BY:
!
!       CURRENT F-NUMBER/NEW F-NUMBER
!
      call sys_set_sax(sys_sax()*(-(1.0D0/(2.0D0*&
      &PXTRAX(2,(INT(sys_last_surf())))))/sys_fno_hold_x()))
!
!       NOW WE PERFORM A PARAXIAL RAY TRACE WITHOUT AND SOLVES
!
!       TELECENTRIC STUFF, 11/12/2000
      IF(sys_telecentric().EQ.1.0D0) THEN
         IF(sys_na_set().EQ.0.0D0.AND.sys_fno_val_set().EQ.0.0D0) THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & 'WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'//'\n'//&
            & 'TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'//'\n'//&
            & 'VALUES'//'\n'//&
            & 'PARAXIAL TRACE STOPPED', 1)
            RETURN
         ELSE
            IF(sys_na_set().EQ.1.0D0) THEN
               call sys_set_say(surf_thickness(0)*sys_naoy())
               call sys_set_sax(surf_thickness(0)*sys_naox())
            END IF
            IF(sys_fno_val_set().EQ.1.0D0) THEN
               call sys_set_say(surf_thickness(0)/(2.0D0*sys_fno_val_y()))
               call sys_set_sax(surf_thickness(0)/(2.0D0*sys_fno_val_x()))
            END IF
         END IF
      END IF
!
      CALL TR
!
!       NOW THE PARAXIAL SOLVE DATA ARRAYS ARE LOADED WITH THE CORRECT
!       SOLVELESS DATA. NOW RE-SET ALL SOLVE TARGET VALUES
!
!       WHEN THE RETURN OCCURS, THE RETURN WILL GO TO LNSEOS IN
!       THE CASE OF HLDS OR TO SFNB.FOR OR SER.FOR. LNSEOS.FOR
!       WILL AGAIN PERFORM THE TRACE FIXING THE EXIT PUPIL DISTACES AND
!       CALCULATING CHROMATIC AND OTHER VALUES.
!       A CALL TO PRTRA1 AND PRTRA2 IS PLACED IN SER.FOR AND SFNB.FOR
!       TO PERFORM A SIMILAR FUNCTION.
!       SVXSET RESETS ALL XZ-PLANE PARAXIAL SOLVES USING CURRENT XZ-PLANE
!       PARAXIAL DATA.
      CALL SVSET
      RETURN
   ELSE
!       ITYPEP NOT 2
   END IF
   RETURN
END
! SUB G357.FOR
SUBROUTINE G357
!
   use DATCFG
   use DATLEN
   use DATMAI
   use mod_system, only: sys_last_surf, sys_mode, sys_wl_ref
   use mod_lens_data_manager, only: ldm
   IMPLICIT NONE
!
!       SUBROUTINE GET SERVES TO GET THE 3RD, 5TH AND 7TH
!       ORDER ABERRATIONS, THEIR TOTALS AND THEIR CHROMATIC
!       DIFFERENCES. IT IS CALLED BY GET.FOR AND RETURNS THE
!       REAL*8
!
   REAL*8 VALUE,INV,V,INTV,W1A,W1B,V1
!
   LOGICAL NEG
!
   INTEGER SF,I,NUM5
!
   COMMON/GV/VALUE,NUM5
!
   COMMON/LGV/NEG
!
!
   CALL PRTRB
   CALL PRTRC
   CALL PRTRD
!
   W1A=W1
   W1B=W1
   IF(NEG) THEN
      IF(W1.NE.0.0D0) THEN
         W1A=W1
         W1B=W1-1.0D0
      END IF
   END IF
!       GET IMDISX AND IMDISY

   IF(WQ.EQ.'IMDISY') THEN
!       VALUE IS DISTACE TO FOCUS
      IF(PXTRAY(2,INT(W1B)).NE.0.0D0) THEN
         VALUE=((-PXTRAY(1,INT(W1A)))/(PXTRAY(2,INT(W1B))))
      ELSE
         VALUE=1.0D300
      END IF
      RETURN
   END IF
   IF(WQ.EQ.'IMDISX') THEN
!       VALUE IS DISTACE TO FOCUS
      IF(PXTRAX(2,INT(W1B)).NE.0.0D0) THEN
         VALUE=((-PXTRAX(1,INT(W1A)))/(PXTRAX(2,INT(W1B))))
      ELSE
         VALUE=1.0D300
      END IF
      RETURN
   END IF
!
!       GET CHFIM
   IF(WQ.EQ.'CHFIMY') THEN
!       V1 IS DISTACE TO FOCUS, VALUE IS GAUSSIAN IMAGE HEIGHT THERE.
      V1=((-PXTRAY(1,INT(W1A)))/(PXTRAY(2,INT(W1B))))
      VALUE=(V1*PXTRAY(6,INT(W1B)))+PXTRAY(5,INT(W1A))
      RETURN
   END IF
   IF(WQ.EQ.'CHFIMX') THEN
!       V1 IS DISTACE TO FOCUS, VALUE IS GAUSSIAN IMAGE HEIGHT THERE.
      V1=((-PXTRAX(1,INT(W1A)))/(PXTRAX(2,INT(W1B))))
      VALUE=(V1*PXTRAX(6,INT(W1B)))+PXTRAX(5,INT(W1A))
      RETURN
   END IF
!
!
!       GET PUPDIS(X OR Y)
   IF(WQ.EQ.'PUPDISY') THEN
!       VALUE IS DISTANCE TO PUPIL
      VALUE=((-PXTRAY(5,INT(W1A)))/(PXTRAY(6,INT(W1B))))
      RETURN
   END IF
   IF(WQ.EQ.'PUPDISX') THEN
!       VALUE IS DISTANCE TO PUPIL
      VALUE=((-PXTRAX(5,INT(W1A)))/(PXTRAX(6,INT(W1B))))
      RETURN
   END IF
!
!       GET PUPDIA(X OR Y)
   IF(WQ.EQ.'PUPDIAY') THEN
!       V1 IS DISTANCE TO PUPIL, VALUE IS MARG RAY HEIGHT THERE.
      V1=((-PXTRAY(5,INT(W1A)))/(PXTRAY(6,INT(W1B))))
      VALUE=2.0D0*(V1*PXTRAY(2,INT(W1B)))+PXTRAY(1,INT(W1A))
      RETURN
   END IF
   IF(WQ.EQ.'PUPDIAX') THEN
!       V1 IS DISTANCE TO PUPIL, VALUE IS MARG RAY HEIGHT THERE.
      V1=((-PXTRAX(5,INT(W1A)))/(PXTRAX(6,INT(W1B))))
      VALUE=2.0D0*(V1*PXTRAX(2,INT(W1B)))+PXTRAX(1,INT(W1A))
      RETURN
   END IF
!
   VALUE=0.0D0
   SF=INT(sys_last_surf())
   INTV=1.0D0
   IF(sys_mode().EQ.1.0D0.OR.sys_mode().EQ.3.0D0) THEN
!       CALCULATE INTV
      IF(WQ.EQ.'PACX'.OR.WQ.EQ.'PLCX'.OR.WQ.EQ.'SACX'&
      &.OR.WQ.EQ.'SLCX') THEN
         INTV=((PXTRAX(5,SF)*ldm%getSurfIndex(SF-1, INT(sys_wl_ref()))*PXTRAX(2,(SF-1)))-&
         &(PXTRAX(1,SF)*ldm%getSurfIndex(SF-1, INT(sys_wl_ref()))*PXTRAX(6,(SF-1))))
      END IF
      IF(WQ.EQ.'PACY'.OR.WQ.EQ.'PLCY'.OR.WQ.EQ.'SACY'&
      &.OR.WQ.EQ.'SLCY') THEN
         INTV=((PXTRAY(5,SF)*ldm%getSurfIndex(SF-1, INT(sys_wl_ref()))*PXTRAY(2,(SF-1)))-&
         &(PXTRAY(1,SF)*ldm%getSurfIndex(SF-1, INT(sys_wl_ref()))*PXTRAY(6,(SF-1))))
      END IF
   END IF
!
   INV=1.0D0
   IF(sys_mode().EQ.1.0D0) THEN
!       MODE IS FOCAL
      IF(WQ(1:1).NE.'X')&
      &INV=-2.0*ldm%getSurfIndex(SF-1, INT(sys_wl_ref()))*PXTRAY(2,(SF-1))
      IF(WQ(1:1).EQ.'X')&
      &INV=-2.0*ldm%getSurfIndex(SF-1, INT(sys_wl_ref()))*PXTRAX(2,(SF-1))
   END IF
   IF(sys_mode().EQ.3.0D0) THEN
!       MODE IS AFOCAL
      IF(WQ(1:1).NE.'X')&
      &INV= 2.0*ldm%getSurfIndex(SF-1, INT(sys_wl_ref()))*PXTRAY(1,SF)
      IF(WQ(1:1).EQ.'X')&
      &INV= 2.0*ldm%getSurfIndex(SF-1, INT(sys_wl_ref()))*PXTRAX(1,SF)
   END IF
   IF(INV.EQ.0.0D0) THEN
      WRITE(OUTLYNE,*)&
      &'THE LENS (MODE) IS NOT CONSISTENT WITH PARAXIAL VALUES'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)&
      &'ABERRATIONS ARE NOT CALCULABLE'
      CALL SHOWIT(1)
      IF(sys_mode().EQ.1.0D0)&
      &WRITE(OUTLYNE,*)'CHANGE FROM "MODE FOCAL" TO "MODE AFOCAL"'
      IF(sys_mode().EQ.3.0D0)&
      &WRITE(OUTLYNE,*)'CHANGE FROM "MODE AFOCAL" TO "MODE FOCAL"'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'THEN RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(DF1.EQ.0) THEN
!       A SPECIFIC SURFACE HAS BEEN REQUESTED
!       3RD, 5TH AND 7TH ORDER ABERRATIONS
      IF(WQ.EQ.'SA3')       VALUE=MAB3(1,INT(W1))
      IF(WQ.EQ.'XSA3')      VALUE=XMAB3(1,INT(W1))
      IF(WQ.EQ.'CMA3')      VALUE=3.0D0*MAB3(2,INT(W1))
      IF(WQ.EQ.'XCMA3')     VALUE=3.0D0*XMAB3(2,INT(W1))
      IF(WQ.EQ.'AST3')      VALUE=MAB3(3,INT(W1))
      IF(WQ.EQ.'XAST3')     VALUE=XMAB3(3,INT(W1))
      IF(WQ.EQ.'DIS3')      VALUE=MAB3(4,INT(W1))
      IF(WQ.EQ.'XDIS3')     VALUE=XMAB3(4,INT(W1))
      IF(WQ.EQ.'PTZ3')      VALUE=MAB3(5,INT(W1))
      IF(WQ.EQ.'XPTZ3')     VALUE=XMAB3(5,INT(W1))
      IF(WQ.EQ.'PTZCV')     VALUE=MAB3(11,INT(W1))
      IF(WQ.EQ.'XPTZCV')    VALUE=XMAB3(11,INT(W1))
      IF(WQ.EQ.'SA5')       VALUE=MAB57(1,INT(W1))
      IF(WQ.EQ.'XSA5')      VALUE=XMAB57(1,INT(W1))
      IF(WQ.EQ.'CMA5')      VALUE=MAB57(2,INT(W1))+MAB57(3,INT(W1))
      IF(WQ.EQ.'XCMA5')     VALUE=XMAB57(2,INT(W1))+XMAB57(3,INT(W1))
      IF(WQ.EQ.'AST5')      VALUE=MAB57(10,INT(W1))
      IF(WQ.EQ.'XAST5')     VALUE=XMAB57(10,INT(W1))
      IF(WQ.EQ.'DIS5')      VALUE=MAB57(12,INT(W1))
      IF(WQ.EQ.'XDIS5')     VALUE=XMAB57(12,INT(W1))
      IF(WQ.EQ.'PTZ5')      VALUE=MAB57(11,INT(W1))
      IF(WQ.EQ.'XPTZ5')     VALUE=XMAB57(11,INT(W1))
      IF(WQ.EQ.'TOBSA')     VALUE=MAB57(4,INT(W1))+&
      &MAB57(5,INT(W1))+MAB57(6,INT(W1))
      IF(WQ.EQ.'XTOBSA')    VALUE=XMAB57(4,INT(W1))+&
      &XMAB57(5,INT(W1))+XMAB57(6,INT(W1))
      IF(WQ.EQ.'SOBSA')     VALUE=MAB57(5,INT(W1))
      IF(WQ.EQ.'XSOBSA')    VALUE=XMAB57(5,INT(W1))
      IF(WQ.EQ.'ELCMA')     VALUE=MAB57(7,INT(W1))+&
      &MAB57(8,INT(W1))
      IF(WQ.EQ.'XELCMA')    VALUE=XMAB57(7,INT(W1))+&
      &XMAB57(8,INT(W1))
      IF(WQ.EQ.'TAS')       VALUE=MAB57(10,INT(W1))+&
      &(5.0*MAB57(11,INT(W1)))
      IF(WQ.EQ.'XTAS')      VALUE=XMAB57(10,INT(W1))+&
      &(5.0*XMAB57(11,INT(W1)))
      IF(WQ.EQ.'SAS')       VALUE=MAB57(10,INT(W1))+&
      &MAB57(11,INT(W1))
      IF(WQ.EQ.'XSAS')      VALUE=XMAB57(10,INT(W1))+&
      &XMAB57(11,INT(W1))
      IF(WQ.EQ.'SA7')       VALUE=MAB57(14,INT(W1))
      IF(WQ.EQ.'XSA7')      VALUE=XMAB57(14,INT(W1))
!       PRIMARY CHROMATIC ABERRATION DIFFERENCES
      IF(WQ.EQ.'SA3P')       VALUE=PDF3(1,INT(W1))
      IF(WQ.EQ.'XSA3P')      VALUE=XPDF3(1,INT(W1))
      IF(WQ.EQ.'CMA3P')      VALUE=3.0D0*PDF3(2,INT(W1))
      IF(WQ.EQ.'XCMA3P')     VALUE=3.0D0*XPDF3(2,INT(W1))
      IF(WQ.EQ.'AST3P')      VALUE=PDF3(3,INT(W1))
      IF(WQ.EQ.'XAST3P')     VALUE=XPDF3(3,INT(W1))
      IF(WQ.EQ.'DIS3P')      VALUE=PDF3(4,INT(W1))
      IF(WQ.EQ.'XDIS3P')     VALUE=XPDF3(4,INT(W1))
      IF(WQ.EQ.'PTZ3P')      VALUE=PDF3(5,INT(W1))
      IF(WQ.EQ.'XPTZ3P')     VALUE=XPDF3(5,INT(W1))
      IF(WQ.EQ.'SA5P')       VALUE=PDF57(1,INT(W1))
      IF(WQ.EQ.'XSA5P')      VALUE=XPDF57(1,INT(W1))
      IF(WQ.EQ.'CMA5P')      VALUE=PDF57(2,INT(W1))+PDF57(3,INT(W1))
      IF(WQ.EQ.'XCMA5P')     VALUE=XPDF57(2,INT(W1))+XPDF57(3,INT(W1))
      IF(WQ.EQ.'AST5P')      VALUE=PDF57(10,INT(W1))
      IF(WQ.EQ.'XAST5P')     VALUE=XPDF57(10,INT(W1))
      IF(WQ.EQ.'DIS5P')      VALUE=PDF57(12,INT(W1))
      IF(WQ.EQ.'XDIS5P')     VALUE=XPDF57(12,INT(W1))
      IF(WQ.EQ.'PTZ5P')      VALUE=PDF57(11,INT(W1))
      IF(WQ.EQ.'XPTZ5P')     VALUE=XPDF57(11,INT(W1))
      IF(WQ.EQ.'TOBSAP')     VALUE=PDF57(4,INT(W1))+&
      &PDF57(5,INT(W1))+PDF57(6,INT(W1))
      IF(WQ.EQ.'XTOBSAP')    VALUE=XPDF57(4,INT(W1))+&
      &XPDF57(5,INT(W1))+XPDF57(6,INT(W1))
      IF(WQ.EQ.'SOBSAP')     VALUE=PDF57(5,INT(W1))
      IF(WQ.EQ.'XSOBSAP')    VALUE=XPDF57(5,INT(W1))
      IF(WQ.EQ.'ELCMAP')     VALUE=PDF57(7,INT(W1))+&
      &PDF57(8,INT(W1))
      IF(WQ.EQ.'XELCMAP')    VALUE=XPDF57(7,INT(W1))+&
      &XPDF57(8,INT(W1))
      IF(WQ.EQ.'TASP')       VALUE=PDF57(10,INT(W1))+&
      &(5.0*PDF57(11,INT(W1)))
      IF(WQ.EQ.'XTASP')      VALUE=XPDF57(10,INT(W1))+&
      &(5.0*XPDF57(11,INT(W1)))
      IF(WQ.EQ.'SASP')       VALUE=PDF57(10,INT(W1))+&
      &PDF57(11,INT(W1))
      IF(WQ.EQ.'XSASP')      VALUE=XPDF57(10,INT(W1))+&
      &XPDF57(11,INT(W1))
      IF(WQ.EQ.'SA7P')       VALUE=PDF57(14,INT(W1))
      IF(WQ.EQ.'XSA7P')      VALUE=XPDF57(14,INT(W1))
!       SECONDARY CHROMATIC ABERRATION DIFFERENCES
      IF(WQ.EQ.'SA3S')       VALUE=SDF3(1,INT(W1))
      IF(WQ.EQ.'XSA3S')      VALUE=XSDF3(1,INT(W1))
      IF(WQ.EQ.'CMA3S')      VALUE=3.0D0*SDF3(2,INT(W1))
      IF(WQ.EQ.'XCMA3S')     VALUE=3.0D0*XSDF3(2,INT(W1))
      IF(WQ.EQ.'AST3S')      VALUE=SDF3(3,INT(W1))
      IF(WQ.EQ.'XAST3S')     VALUE=XSDF3(3,INT(W1))
      IF(WQ.EQ.'DIS3S')      VALUE=SDF3(4,INT(W1))
      IF(WQ.EQ.'XDIS3S')     VALUE=XSDF3(4,INT(W1))
      IF(WQ.EQ.'PTZ3S')      VALUE=SDF3(5,INT(W1))
      IF(WQ.EQ.'XPTZ3S')     VALUE=XSDF3(5,INT(W1))
      IF(WQ.EQ.'SA5S')       VALUE=SDF57(1,INT(W1))
      IF(WQ.EQ.'XSA5S')      VALUE=XSDF57(1,INT(W1))
      IF(WQ.EQ.'CMA5S')      VALUE=SDF57(2,INT(W1))+SDF57(3,INT(W1))
      IF(WQ.EQ.'XCMA5S')     VALUE=XSDF57(2,INT(W1))+XSDF57(3,INT(W1))
      IF(WQ.EQ.'AST5S')      VALUE=SDF57(10,INT(W1))
      IF(WQ.EQ.'XAST5S')     VALUE=XSDF57(10,INT(W1))
      IF(WQ.EQ.'DIS5S')      VALUE=SDF57(12,INT(W1))
      IF(WQ.EQ.'XDIS5S')     VALUE=XSDF57(12,INT(W1))
      IF(WQ.EQ.'PTZ5S')      VALUE=SDF57(11,INT(W1))
      IF(WQ.EQ.'XPTZ5S')     VALUE=XSDF57(11,INT(W1))
      IF(WQ.EQ.'TOBSAS')     VALUE=SDF57(4,INT(W1))+&
      &SDF57(5,INT(W1))+SDF57(6,INT(W1))
      IF(WQ.EQ.'XTOBSAS')    VALUE=XSDF57(4,INT(W1))+&
      &XSDF57(5,INT(W1))+XSDF57(6,INT(W1))
      IF(WQ.EQ.'SOBSAS')     VALUE=SDF57(5,INT(W1))
      IF(WQ.EQ.'XSOBSAS')    VALUE=XSDF57(5,INT(W1))
      IF(WQ.EQ.'ELCMAS')     VALUE=SDF57(7,INT(W1))+&
      &SDF57(8,INT(W1))
      IF(WQ.EQ.'XELCMAS')    VALUE=XSDF57(7,INT(W1))+&
      &XSDF57(8,INT(W1))
      IF(WQ.EQ.'TASS')       VALUE=SDF57(10,INT(W1))+&
      &(5.0*SDF57(11,INT(W1)))
      IF(WQ.EQ.'XTASS')      VALUE=XSDF57(10,INT(W1))+&
      &(5.0*XSDF57(11,INT(W1)))
      IF(WQ.EQ.'SASS')       VALUE=SDF57(10,INT(W1))+&
      &SDF57(11,INT(W1))
      IF(WQ.EQ.'XSASS')      VALUE=XSDF57(10,INT(W1))+&
      &XSDF57(11,INT(W1))
      IF(WQ.EQ.'SA7S')       VALUE=SDF57(14,INT(W1))
      IF(WQ.EQ.'XSA7S')      VALUE=XSDF57(14,INT(W1))
!       5TH AND 7TH ORDER INTRINSIC SURFACE ABERRATIONS
      IF(WQ.EQ.'SA5I')       VALUE=SAB57(1,INT(W1))
      IF(WQ.EQ.'XSA5I')      VALUE=XSAB57(1,INT(W1))
      IF(WQ.EQ.'CMA5I')      VALUE=SAB57(2,INT(W1))+SAB57(3,INT(W1))
      IF(WQ.EQ.'XCMA5I')     VALUE=XSAB57(2,INT(W1))+XSAB57(3,INT(W1))
      IF(WQ.EQ.'AST5I')      VALUE=SAB57(10,INT(W1))
      IF(WQ.EQ.'XAST5I')     VALUE=XSAB57(10,INT(W1))
      IF(WQ.EQ.'DIS5I')      VALUE=SAB57(12,INT(W1))
      IF(WQ.EQ.'XDIS5I')     VALUE=XSAB57(12,INT(W1))
      IF(WQ.EQ.'PTZ5I')      VALUE=SAB57(11,INT(W1))
      IF(WQ.EQ.'XPTZ5I')     VALUE=XSAB57(11,INT(W1))
      IF(WQ.EQ.'TOBSAI')     VALUE=SAB57(4,INT(W1))+&
      &SAB57(5,INT(W1))+SAB57(6,INT(W1))
      IF(WQ.EQ.'XTOBSAI')    VALUE=XSAB57(4,INT(W1))+&
      &XSAB57(5,INT(W1))+XSAB57(6,INT(W1))
      IF(WQ.EQ.'SOBSAI')     VALUE=SAB57(5,INT(W1))
      IF(WQ.EQ.'XSOBSAI')    VALUE=XSAB57(5,INT(W1))
      IF(WQ.EQ.'ELCMAI')     VALUE=SAB57(7,INT(W1))+&
      &SAB57(8,INT(W1))
      IF(WQ.EQ.'XELCMAI')    VALUE=XSAB57(7,INT(W1))+&
      &XSAB57(8,INT(W1))
      IF(WQ.EQ.'TASI')       VALUE=SAB57(10,INT(W1))+&
      &(5.0*SAB57(11,INT(W1)))
      IF(WQ.EQ.'XTASI')      VALUE=XSAB57(10,INT(W1))+&
      &(5.0*XSAB57(11,INT(W1)))
      IF(WQ.EQ.'SASI')       VALUE=SAB57(10,INT(W1))+&
      &SAB57(11,INT(W1))
      IF(WQ.EQ.'XSASI')      VALUE=XSAB57(10,INT(W1))+&
      &XSAB57(11,INT(W1))
      IF(WQ.EQ.'SA7I')       VALUE=SAB57(14,INT(W1))
      IF(WQ.EQ.'XSA7I')      VALUE=XSAB57(14,INT(W1))
!       3RD ORDER PUPIL ABERRATIONS
      IF(WQ.EQ.'PSA3')       VALUE=MAB3(6,INT(W1))
      IF(WQ.EQ.'XPSA3')      VALUE=XMAB3(6,INT(W1))
      IF(WQ.EQ.'PCMA3')      VALUE=3.0D0*MAB3(7,INT(W1))
      IF(WQ.EQ.'XPCMA3')     VALUE=3.0D0*XMAB3(7,INT(W1))
      IF(WQ.EQ.'PAST3')      VALUE=MAB3(8,INT(W1))
      IF(WQ.EQ.'XPAST3')     VALUE=XMAB3(8,INT(W1))
      IF(WQ.EQ.'PDIS3')      VALUE=MAB3(9,INT(W1))
      IF(WQ.EQ.'XPDIS3')     VALUE=XMAB3(9,INT(W1))
      IF(WQ.EQ.'PPTZ3')      VALUE=MAB3(10,INT(W1))
      IF(WQ.EQ.'XPPTZ3')     VALUE=XMAB3(10,INT(W1))
!       3RD ORDER PUPIL ABERRATION PRIMARY CHROMATIC DIFFERENCES
      IF(WQ.EQ.'PSA3P')       VALUE=PDF3(6,INT(W1))
      IF(WQ.EQ.'XPSA3P')      VALUE=XPDF3(6,INT(W1))
      IF(WQ.EQ.'PCMA3P')      VALUE=3.0D0*PDF3(7,INT(W1))
      IF(WQ.EQ.'XPCMA3P')     VALUE=3.0D0*XPDF3(7,INT(W1))
      IF(WQ.EQ.'PAST3P')      VALUE=PDF3(8,INT(W1))
      IF(WQ.EQ.'XPAST3P')     VALUE=XPDF3(8,INT(W1))
      IF(WQ.EQ.'PDIS3P')      VALUE=PDF3(9,INT(W1))
      IF(WQ.EQ.'XPDIS3P')     VALUE=XPDF3(9,INT(W1))
      IF(WQ.EQ.'PPTZ3P')      VALUE=PDF3(10,INT(W1))
      IF(WQ.EQ.'XPPTZ3P')     VALUE=XPDF3(10,INT(W1))
!       3RD ORDER PUPIL ABERRATION SECONDARY CHROMATIC DIFFERENCES
      IF(WQ.EQ.'PSA3S')       VALUE=SDF3(6,INT(W1))
      IF(WQ.EQ.'XPSA3S')      VALUE=XSDF3(6,INT(W1))
      IF(WQ.EQ.'PCMA3S')      VALUE=3.0D0*SDF3(7,INT(W1))
      IF(WQ.EQ.'XPCMA3S')     VALUE=3.0D0*XSDF3(7,INT(W1))
      IF(WQ.EQ.'PAST3S')      VALUE=SDF3(8,INT(W1))
      IF(WQ.EQ.'XPAST3S')     VALUE=XSDF3(8,INT(W1))
      IF(WQ.EQ.'PDIS3S')      VALUE=SDF3(9,INT(W1))
      IF(WQ.EQ.'XPDIS3S')     VALUE=XSDF3(9,INT(W1))
      IF(WQ.EQ.'PPTZ3S')      VALUE=SDF3(10,INT(W1))
      IF(WQ.EQ.'XPPTZ3S')     VALUE=XSDF3(10,INT(W1))
      IF(WQ.EQ.'PTZCV'.OR.WQ.EQ.'XPTZCV') THEN
         VALUE=VALUE
      ELSE
         VALUE=VALUE/INV
      END IF

      IF(sys_mode().LE.2.0D0) THEN
         IF(WQ.EQ.'PACY') THEN
            VALUE=COLORY(1,INT(W1))/INTV
         END IF
         IF(WQ.EQ.'PLCY') THEN
            VALUE=COLORY(2,INT(W1))/INTV
         END IF
         IF(WQ.EQ.'SACY') THEN
            VALUE=COLORY(3,INT(W1))/INTV
         END IF
         IF(WQ.EQ.'SLCY') THEN
            VALUE=COLORY(4,INT(W1))/INTV
         END IF
         IF(WQ.EQ.'PACX') THEN
            VALUE=COLORX(1,INT(W1))/INTV
         END IF
         IF(WQ.EQ.'PLCX') THEN
            VALUE=COLORX(2,INT(W1))/INTV
         END IF
         IF(WQ.EQ.'SACX') THEN
            VALUE=COLORX(3,INT(W1))/INTV
         END IF
         IF(WQ.EQ.'SLCX') THEN
            VALUE=COLORX(4,INT(W1))/INTV
         END IF
      ELSE
         IF(WQ.EQ.'PACY') THEN
            VALUE=COLORY(5,INT(W1))/INTV
         END IF
         IF(WQ.EQ.'PLCY') THEN
            VALUE=COLORY(6,INT(W1))/INTV
         END IF
         IF(WQ.EQ.'SACY') THEN
            VALUE=COLORY(7,INT(W1))/INTV
         END IF
         IF(WQ.EQ.'SLCY') THEN
            VALUE=COLORY(8,INT(W1))/INTV
         END IF
         IF(WQ.EQ.'PACX') THEN
            VALUE=COLORX(5,INT(W1))/INTV
         END IF
         IF(WQ.EQ.'PLCX') THEN
            VALUE=COLORX(6,INT(W1))/INTV
         END IF
         IF(WQ.EQ.'SACX') THEN
            VALUE=COLORX(7,INT(W1))/INTV
         END IF
         IF(WQ.EQ.'SLCX') THEN
            VALUE=COLORX(8,INT(W1))/INTV
         END IF
      END IF
   ELSE
!       DO THE TOTALS FOR ALL SURFACES
!       3RD, 5TH AND 7TH ORDER ABERRATIONS
      V=0.0D0
      IF(WQ.EQ.'SA3') THEN
         DO I=0,INT(sys_last_surf())
            V=V+MAB3(1,I)
         END DO
      END IF
      IF(WQ.EQ.'XSA3') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XMAB3(1,I)
         END DO
      END IF
      IF(WQ.EQ.'CMA3') THEN
         DO I=0,INT(sys_last_surf())
            !V=V+(3.0D0*MAB3(2,I))
            V=V+(MAB3(2,I))
         END DO
      END IF
      IF(WQ.EQ.'XCMA3') THEN
         DO I=0,INT(sys_last_surf())
            V=V+(3.0D0*XMAB3(2,I))
         END DO
      END IF
      IF(WQ.EQ.'AST3') THEN
         DO I=0,INT(sys_last_surf())
            V=V+MAB3(3,I)
         END DO
      END IF
      IF(WQ.EQ.'XAST3') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XMAB3(3,I)
         END DO
      END IF
      IF(WQ.EQ.'DIS3') THEN
         DO I=0,INT(sys_last_surf())
            V=V+MAB3(4,I)
         END DO
      END IF
      IF(WQ.EQ.'XDIS3') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XMAB3(4,I)
         END DO
      END IF
      IF(WQ.EQ.'PTZ3') THEN
         DO I=0,INT(sys_last_surf())
            V=V+MAB3(5,I)
         END DO
      END IF
      IF(WQ.EQ.'XPTZ3') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XMAB3(5,I)
         END DO
      END IF
      IF(WQ.EQ.'PTZCV') THEN
         DO I=0,INT(sys_last_surf())
            V=V+MAB3(11,I)
         END DO
         RETURN
      END IF
      IF(WQ.EQ.'XPTZCV') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XMAB3(11,I)
         END DO
         RETURN
      END IF
      IF(WQ.EQ.'SA5') THEN
         DO I=0,INT(sys_last_surf())
            V=V+MAB57(1,I)
         END DO
      END IF
      IF(WQ.EQ.'XSA5') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XMAB57(1,I)
         END DO
      END IF
      IF(WQ.EQ.'CMA5') THEN
         DO I=0,INT(sys_last_surf())
            V=V+MAB57(2,I)+MAB57(3,I)
         END DO
      END IF
      IF(WQ.EQ.'XCMA5') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XMAB57(2,I)+XMAB57(3,I)
         END DO
      END IF
      IF(WQ.EQ.'AST5') THEN
         DO I=0,INT(sys_last_surf())
            V=V+MAB57(10,I)
         END DO
      END IF
      IF(WQ.EQ.'XAST5') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XMAB57(10,I)
         END DO
      END IF
      IF(WQ.EQ.'DIS5') THEN
         DO I=0,INT(sys_last_surf())
            V=V+MAB57(12,I)
         END DO
      END IF
      IF(WQ.EQ.'XDIS5') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XMAB57(12,I)
         END DO
      END IF
      IF(WQ.EQ.'PTZ5') THEN
         DO I=0,INT(sys_last_surf())
            V=V+MAB57(11,I)
         END DO
      END IF
      IF(WQ.EQ.'XPTZ5') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XMAB57(11,I)
         END DO
      END IF
      IF(WQ.EQ.'TOBSA') THEN
         DO I=0,INT(sys_last_surf())
            V=V+MAB57(4,I)+&
            &MAB57(5,I)+MAB57(6,I)
         END DO
      END IF
      IF(WQ.EQ.'XTOBSA') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XMAB57(4,I)+&
            &XMAB57(5,I)+XMAB57(6,I)
         END DO
      END IF
      IF(WQ.EQ.'SOBSA') THEN
         DO I=0,INT(sys_last_surf())
            V=V+MAB57(5,I)
         END DO
      END IF
      IF(WQ.EQ.'XSOBSA') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XMAB57(5,I)
         END DO
      END IF
      IF(WQ.EQ.'ELCMA') THEN
         DO I=0,INT(sys_last_surf())
            V=V+MAB57(7,I)+&
            &MAB57(8,I)
         END DO
      END IF
      IF(WQ.EQ.'XELCMA') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XMAB57(7,I)+&
            &XMAB57(8,I)
         END DO
      END IF
      IF(WQ.EQ.'TAS') THEN
         DO I=0,INT(sys_last_surf())
            V=V+MAB57(10,I)+&
            &(5.0*MAB57(11,I))
         END DO
      END IF
      IF(WQ.EQ.'XTAS') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XMAB57(10,I)+&
            &(5.0*XMAB57(11,I))
         END DO
      END IF
      IF(WQ.EQ.'SAS') THEN
         DO I=0,INT(sys_last_surf())
            V=V+MAB57(10,I)+&
            &MAB57(11,I)
         END DO
      END IF
      IF(WQ.EQ.'XSAS') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XMAB57(10,I)+&
            &XMAB57(11,I)
         END DO
      END IF
      IF(WQ.EQ.'SA7') THEN
         DO I=0,INT(sys_last_surf())
            V=V+MAB57(14,I)
         END DO
      END IF
      IF(WQ.EQ.'XSA7') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XMAB57(14,I)
         END DO
      END IF
!       PRIMARY CHROMATIC ABERRATION DIFFERENCES
      IF(WQ.EQ.'SA3P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+PDF3(1,I)
         END DO
      END IF
      IF(WQ.EQ.'XSA3P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XPDF3(1,I)
         END DO
      END IF
      IF(WQ.EQ.'CMA3P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+(3.0D0*PDF3(2,I))
         END DO
      END IF
      IF(WQ.EQ.'XCMA3P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+(3.0D0*XPDF3(2,I))
         END DO
      END IF
      IF(WQ.EQ.'AST3P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+PDF3(3,I)
         END DO
      END IF
      IF(WQ.EQ.'XAST3P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XPDF3(3,I)
         END DO
      END IF
      IF(WQ.EQ.'DIS3P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+PDF3(4,I)
         END DO
      END IF
      IF(WQ.EQ.'XDIS3P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XPDF3(4,I)
         END DO
      END IF
      IF(WQ.EQ.'PTZ3P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+PDF3(5,I)
         END DO
      END IF
      IF(WQ.EQ.'XPTZ3P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XPDF3(5,I)
         END DO
      END IF
      IF(WQ.EQ.'SA5P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+PDF57(1,I)
         END DO
      END IF
      IF(WQ.EQ.'XSA5P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XPDF57(1,I)
         END DO
      END IF
      IF(WQ.EQ.'CMA5P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+PDF57(2,I)+PDF57(3,I)
         END DO
      END IF
      IF(WQ.EQ.'XCMA5P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XPDF57(2,I)+XPDF57(3,I)
         END DO
      END IF
      IF(WQ.EQ.'AST5P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+PDF57(10,I)
         END DO
      END IF
      IF(WQ.EQ.'XAST5P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XPDF57(10,I)
         END DO
      END IF
      IF(WQ.EQ.'DIS5P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+PDF57(12,I)
         END DO
      END IF
      IF(WQ.EQ.'XDIS5P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XPDF57(12,I)
         END DO
      END IF
      IF(WQ.EQ.'PTZ5P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+PDF57(11,I)
         END DO
      END IF
      IF(WQ.EQ.'XPTZ5P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XPDF57(11,I)
         END DO
      END IF
      IF(WQ.EQ.'TOBSAP') THEN
         DO I=0,INT(sys_last_surf())
            V=V+PDF57(4,I)+&
            &PDF57(5,I)+PDF57(6,I)
         END DO
      END IF
      IF(WQ.EQ.'XTOBSAP') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XPDF57(4,I)+&
            &XPDF57(5,I)+XPDF57(6,I)
         END DO
      END IF
      IF(WQ.EQ.'SOBSAP') THEN
         DO I=0,INT(sys_last_surf())
            V=V+PDF57(5,I)
         END DO
      END IF
      IF(WQ.EQ.'XSOBSAP') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XPDF57(5,I)
         END DO
      END IF
      IF(WQ.EQ.'ELCMAP') THEN
         DO I=0,INT(sys_last_surf())
            V=V+PDF57(7,I)+&
            &PDF57(8,I)
         END DO
      END IF
      IF(WQ.EQ.'XELCMAP') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XPDF57(7,I)+&
            &XPDF57(8,I)
         END DO
      END IF
      IF(WQ.EQ.'TASP') THEN
         DO I=0,INT(sys_last_surf())
            V=V+PDF57(10,I)+&
            &(5.0*PDF57(11,I))
         END DO
      END IF
      IF(WQ.EQ.'XTASP') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XPDF57(10,I)+&
            &(5.0*XPDF57(11,I))
         END DO
      END IF
      IF(WQ.EQ.'SASP') THEN
         DO I=0,INT(sys_last_surf())
            V=V+PDF57(10,I)+&
            &PDF57(11,I)
         END DO
      END IF
      IF(WQ.EQ.'XSASP') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XPDF57(10,I)+&
            &XPDF57(11,I)
         END DO
      END IF
      IF(WQ.EQ.'SA7P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+PDF57(14,I)
         END DO
      END IF
      IF(WQ.EQ.'XSA7P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XPDF57(14,I)
         END DO
      END IF
!       SECONDARY CHROMATIC ABERRATION DIFFERENCES
      IF(WQ.EQ.'SA3S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SDF3(1,I)
         END DO
      END IF
      IF(WQ.EQ.'XSA3S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSDF3(1,I)
         END DO
      END IF
      IF(WQ.EQ.'CMA3S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+(3.0D0*SDF3(2,I))
         END DO
      END IF
      IF(WQ.EQ.'XCMA3S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+(3.0D0*XSDF3(2,I))
         END DO
      END IF
      IF(WQ.EQ.'AST3S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SDF3(3,I)
         END DO
      END IF
      IF(WQ.EQ.'XAST3S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSDF3(3,I)
         END DO
      END IF
      IF(WQ.EQ.'DIS3S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SDF3(4,I)
         END DO
      END IF
      IF(WQ.EQ.'XDIS3S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSDF3(4,I)
         END DO
      END IF
      IF(WQ.EQ.'PTZ3S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SDF3(5,I)
         END DO
      END IF
      IF(WQ.EQ.'XPTZ3S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSDF3(5,I)
         END DO
      END IF
      IF(WQ.EQ.'SA5S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SDF57(1,I)
         END DO
      END IF
      IF(WQ.EQ.'XSA5S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSDF57(1,I)
         END DO
      END IF
      IF(WQ.EQ.'CMA5S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SDF57(2,I)+SDF57(3,I)
         END DO
      END IF
      IF(WQ.EQ.'XCMA5S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSDF57(2,I)+XSDF57(3,I)
         END DO
      END IF
      IF(WQ.EQ.'AST5S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SDF57(10,I)
         END DO
      END IF
      IF(WQ.EQ.'XAST5S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSDF57(10,I)
         END DO
      END IF
      IF(WQ.EQ.'DIS5S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SDF57(12,I)
         END DO
      END IF
      IF(WQ.EQ.'XDIS5S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSDF57(12,I)
         END DO
      END IF
      IF(WQ.EQ.'PTZ5S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SDF57(11,I)
         END DO
      END IF
      IF(WQ.EQ.'XPTZ5S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSDF57(11,I)
         END DO
      END IF
      IF(WQ.EQ.'TOBSAS') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SDF57(4,I)+&
            &SDF57(5,I)+SDF57(6,I)
         END DO
      END IF
      IF(WQ.EQ.'XTOBSAS') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSDF57(4,I)+&
            &XSDF57(5,I)+XSDF57(6,I)
         END DO
      END IF
      IF(WQ.EQ.'SOBSAS') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SDF57(5,I)
         END DO
      END IF
      IF(WQ.EQ.'XSOBSAS') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSDF57(5,I)
         END DO
      END IF
      IF(WQ.EQ.'ELCMAS') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SDF57(7,I)+&
            &SDF57(8,I)
         END DO
      END IF
      IF(WQ.EQ.'XELCMAS') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSDF57(7,I)+&
            &XSDF57(8,I)
         END DO
      END IF
      IF(WQ.EQ.'TASS') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SDF57(10,I)+&
            &(5.0*SDF57(11,I))
         END DO
      END IF
      IF(WQ.EQ.'XTASS') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSDF57(10,I)+&
            &(5.0*XSDF57(11,I))
         END DO
      END IF
      IF(WQ.EQ.'SASS') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SDF57(10,I)+&
            &SDF57(11,I)
         END DO
      END IF
      IF(WQ.EQ.'XSASS') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSDF57(10,I)+&
            &XSDF57(11,I)
         END DO
      END IF
      IF(WQ.EQ.'SA7S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SDF57(14,I)
         END DO
      END IF
      IF(WQ.EQ.'XSA7S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSDF57(14,I)
         END DO
      END IF
!       5TH AND 7TH ORDER INTRINSIC SURFACE ABERRATIONS
      IF(WQ.EQ.'SA5I') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SAB57(1,I)
         END DO
      END IF
      IF(WQ.EQ.'XSA5I') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSAB57(1,I)
         END DO
      END IF
      IF(WQ.EQ.'CMA5I') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SAB57(2,I)+SAB57(3,I)
         END DO
      END IF
      IF(WQ.EQ.'XCMA5I') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSAB57(2,I)+XSAB57(3,I)
         END DO
      END IF
      IF(WQ.EQ.'AST5I') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SAB57(10,I)
         END DO
      END IF
      IF(WQ.EQ.'XAST5I') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSAB57(10,I)
         END DO
      END IF
      IF(WQ.EQ.'DIS5I') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SAB57(12,I)
         END DO
      END IF
      IF(WQ.EQ.'XDIS5I') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSAB57(12,I)
         END DO
      END IF
      IF(WQ.EQ.'PTZ5I') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SAB57(11,I)
         END DO
      END IF
      IF(WQ.EQ.'XPTZ5I') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSAB57(11,I)
         END DO
      END IF
      IF(WQ.EQ.'TOBSAI') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SAB57(4,I)+&
            &SAB57(5,I)+SAB57(6,I)
         END DO
      END IF
      IF(WQ.EQ.'XTOBSAI') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSAB57(4,I)+&
            &XSAB57(5,I)+XSAB57(6,I)
         END DO
      END IF
      IF(WQ.EQ.'SOBSAI') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SAB57(5,I)
         END DO
      END IF
      IF(WQ.EQ.'XSOBSAI') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSAB57(5,I)
         END DO
      END IF
      IF(WQ.EQ.'ELCMAI') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SAB57(7,I)+&
            &SAB57(8,I)
         END DO
      END IF
      IF(WQ.EQ.'XELCMAI') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSAB57(7,I)+&
            &XSAB57(8,I)
         END DO
      END IF
      IF(WQ.EQ.'TASI') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SAB57(10,I)+&
            &(5.0*SAB57(11,I))
         END DO
      END IF
      IF(WQ.EQ.'XTASI') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSAB57(10,I)+&
            &(5.0*XSAB57(11,I))
         END DO
      END IF
      IF(WQ.EQ.'SASI') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SAB57(10,I)+&
            &SAB57(11,I)
         END DO
      END IF
      IF(WQ.EQ.'XSASI') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSAB57(10,I)+&
            &XSAB57(11,I)
         END DO
      END IF
      IF(WQ.EQ.'SA7I') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SAB57(14,I)
         END DO
      END IF
      IF(WQ.EQ.'XSA7I') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSAB57(14,I)
         END DO
      END IF
!       3RD ORDER PUPIL ABERRATIONS
      IF(WQ.EQ.'PSA3') THEN
         DO I=0,INT(sys_last_surf())
            V=V+MAB3(6,I)
         END DO
      END IF
      IF(WQ.EQ.'XPSA3') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XMAB3(6,I)
         END DO
      END IF
      IF(WQ.EQ.'PCMA3') THEN
         DO I=0,INT(sys_last_surf())
            V=V+(3.0D0*MAB3(7,I))
         END DO
      END IF
      IF(WQ.EQ.'XPCMA3') THEN
         DO I=0,INT(sys_last_surf())
            V=V+(3.0D0*XMAB3(7,I))
         END DO
      END IF
      IF(WQ.EQ.'PAST3') THEN
         DO I=0,INT(sys_last_surf())
            V=V+MAB3(8,I)
         END DO
      END IF
      IF(WQ.EQ.'XPAST3') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XMAB3(8,I)
         END DO
      END IF
      IF(WQ.EQ.'PDIS3') THEN
         DO I=0,INT(sys_last_surf())
            V=V+MAB3(9,I)
         END DO
      END IF
      IF(WQ.EQ.'XPDIS3') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XMAB3(9,I)
         END DO
      END IF
      IF(WQ.EQ.'PPTZ3') THEN
         DO I=0,INT(sys_last_surf())
            V=V+MAB3(10,I)
         END DO
      END IF
      IF(WQ.EQ.'XPPTZ3') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XMAB3(10,I)
         END DO
      END IF
!       3RD ORDER PUPIL ABERRATION PRIMARY CHROMATIC DIFFERENCES
      IF(WQ.EQ.'PSA3P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+PDF3(6,I)
         END DO
      END IF
      IF(WQ.EQ.'XPSA3P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XPDF3(6,I)
         END DO
      END IF
      IF(WQ.EQ.'PCMA3P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+(3.0D0*PDF3(7,I))
         END DO
      END IF
      IF(WQ.EQ.'XPCMA3P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+(3.0D0*XPDF3(7,I))
         END DO
      END IF
      IF(WQ.EQ.'PAST3P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+PDF3(8,I)
         END DO
      END IF
      IF(WQ.EQ.'XPAST3P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XPDF3(8,I)
         END DO
      END IF
      IF(WQ.EQ.'PDIS3P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+PDF3(9,I)
         END DO
      END IF
      IF(WQ.EQ.'XPDIS3P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XPDF3(9,I)
         END DO
      END IF
      IF(WQ.EQ.'PPTZ3P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+PDF3(10,I)
         END DO
      END IF
      IF(WQ.EQ.'XPPTZ3P') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XPDF3(10,I)
         END DO
      END IF
!       3RD ORDER PUPIL ABERRATION SECONDARY CHROMATIC DIFFERENCES
      IF(WQ.EQ.'PSA3S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SDF3(6,I)
         END DO
      END IF
      IF(WQ.EQ.'XPSA3S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSDF3(6,I)
         END DO
      END IF
      IF(WQ.EQ.'PCMA3S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+(3.0D0*SDF3(7,I))
         END DO
      END IF
      IF(WQ.EQ.'XPCMA3S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+(3.0D0*XSDF3(7,I))
         END DO
      END IF
      IF(WQ.EQ.'PAST3S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SDF3(8,I)
         END DO
      END IF
      IF(WQ.EQ.'XPAST3S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSDF3(8,I)
         END DO
      END IF
      IF(WQ.EQ.'PDIS3S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SDF3(9,I)
         END DO
      END IF
      IF(WQ.EQ.'XPDIS3S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSDF3(9,I)
         END DO
      END IF
      IF(WQ.EQ.'PPTZ3S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+SDF3(10,I)
         END DO
      END IF
      IF(WQ.EQ.'XPPTZ3S') THEN
         DO I=0,INT(sys_last_surf())
            V=V+XSDF3(10,I)
         END DO
      END IF
      IF(WQ.EQ.'PTZCV'.OR.WQ.EQ.'XPTZCV') THEN
         VALUE=V
      ELSE
         VALUE=V/INV
      END IF
      IF(sys_mode().LE.2.0D0) THEN
         IF(WQ.EQ.'PACY') THEN
            DO I=0,INT(sys_last_surf())
               V=V+COLORY(1,I)
            END DO
            VALUE=V/INTV
         END IF
         IF(WQ.EQ.'PLCY') THEN
            DO I=0,INT(sys_last_surf())
               V=V+COLORY(2,I)
            END DO
            VALUE=V/INTV
         END IF
         IF(WQ.EQ.'SACY') THEN
            DO I=0,INT(sys_last_surf())
               V=V+COLORY(3,I)
            END DO
            VALUE=V/INTV
         END IF
         IF(WQ.EQ.'SLCY') THEN
            DO I=0,INT(sys_last_surf())
               V=V+COLORY(4,I)
            END DO
            VALUE=V/INTV
         END IF
         IF(WQ.EQ.'PACX') THEN
            DO I=0,INT(sys_last_surf())
               V=V+COLORX(1,I)
            END DO
            VALUE=V/INTV
         END IF
         IF(WQ.EQ.'PLCX') THEN
            DO I=0,INT(sys_last_surf())
               V=V+COLORX(2,I)
            END DO
            VALUE=V/INTV
         END IF
         IF(WQ.EQ.'SACX') THEN
            DO I=0,INT(sys_last_surf())
               V=V+COLORX(3,I)
            END DO
            VALUE=V/INTV
         END IF
         IF(WQ.EQ.'SLCX') THEN
            DO I=0,INT(sys_last_surf())
               V=V+COLORX(4,I)
            END DO
            VALUE=V/INTV
         END IF
      ELSE
         IF(WQ.EQ.'PACY') THEN
            DO I=0,INT(sys_last_surf())
               V=V+COLORY(5,I)
            END DO
            VALUE=V/INTV
         END IF
         IF(WQ.EQ.'PLCY') THEN
            DO I=0,INT(sys_last_surf())
               V=V+COLORY(6,I)
            END DO
            VALUE=V/INTV
         END IF
         IF(WQ.EQ.'SACY') THEN
            DO I=0,INT(sys_last_surf())
               V=V+COLORY(7,I)
            END DO
            VALUE=V/INTV
         END IF
         IF(WQ.EQ.'SLCY') THEN
            DO I=0,INT(sys_last_surf())
               V=V+COLORY(8,I)
            END DO
            VALUE=V/INTV
         END IF
         IF(WQ.EQ.'PACX') THEN
            DO I=0,INT(sys_last_surf())
               V=V+COLORX(5,I)
            END DO
            VALUE=V/INTV
         END IF
         IF(WQ.EQ.'PLCX') THEN
            DO I=0,INT(sys_last_surf())
               V=V+COLORX(6,I)
            END DO
            VALUE=V/INTV
         END IF
         IF(WQ.EQ.'SACX') THEN
            DO I=0,INT(sys_last_surf())
               V=V+COLORX(7,I)
            END DO
            VALUE=V/INTV
         END IF
         IF(WQ.EQ.'SLCX') THEN
            DO I=0,INT(sys_last_surf())
               V=V+COLORX(8,I)
            END DO
            VALUE=V/INTV
         END IF
      END IF
      RETURN
   END IF
END
