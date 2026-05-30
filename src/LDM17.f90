!       SENENTEENTH FILE FOR LENS DATABASE MANAGER FILES

! SUB LENOUT.FOR
SUBROUTINE LENOUT
   USE GLOBALS
!
   use DATCFG
   use DATSUB
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_last_surf
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       IT IS WITHIN THIS SUBROUTINE THAT (LENO) COMMANDS
!       ARE HANDELED. LENO IS LENS OUTPUT IN A FORM
!       WHICH COULD BE THE READ BACK INTO THE PROGRAM VIA AN
!       ASCII FILE AND AFTER READING IN, THIS LENS WOULD
!       BECOME THE NEW CURRENT LENS. LENO REVERSE
!
   INTEGER I,SSTOPI
!
   LOGICAL CAL,RDOUT,ISKDP,ISAC,ISCV,ISREVERSE,NOOPT
!
   COMMON/LENOTYPE/ISKDP,ISAC,ISCV,ISREVERSE
!
   CHARACTER AI*3,AI4*4
!
   ISKDP=.TRUE.
   ISAC=.FALSE.
   ISCV=.FALSE.
   ISREVERSE=.FALSE.

   PRINT *, "LENOUT ROUTINE WQ = ", WQ

   IF(WQ.EQ.'AC') ISKDP=.FALSE.
   IF(WQ.EQ.'HEX') ISKDP=.FALSE.
   IF(WQ.EQ.'CV') ISKDP=.FALSE.
   IF(WQ.EQ.'REVERSE') ISKDP=.FALSE.
   IF(WQ.EQ.'AC') ISAC=.TRUE.
   IF(WQ.EQ.'HEX') ISAC=.TRUE.
   IF(WQ.EQ.'CV') ISCV=.TRUE.
   IF(WQ.EQ.'REVERSE') ISREVERSE=.TRUE.

!
!*************************************************************
!       THE LENO COMMAND HAS THE FORMS:
!                               LENO
!                               LENO REVERSE
!                               LENO AC
!                               LENO CV
!*************************************************************
!
!               SIMPLE LENO USES SUBROUTINES
!                       LENHD
!                       LENSF
!                       LENED
!                       PIKSLV
!
!
!       LENO TAKES NO STRING OF NUMERIC INPUT
   IF(SST.EQ.1.OR.SN.EQ.1) THEN
      WRITE(OUTLYNE,*)'"LENO (QUALIFIER)" TAKES NO STRING OR NUMERIC INPUT'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(WQ.EQ.'RD') THEN
      WQ='        '
      SQ=0
      RDOUT=.TRUE.
   ELSE
      RDOUT=.FALSE.
   END IF
   IF(WQ.EQ.'NOOPT') THEN
      WQ='        '
      SQ=0
      NOOPT=.TRUE.
   ELSE
      NOOPT=.FALSE.
   END IF
   IF(ISKDP.OR.ISAC) THEN
!       MUST BE SIMPLE LENO OR LENO AC
      IF(ISKDP) CALL LENHD
      IF(ISAC) CALL LENHDAC
      IF(ISKDP) THEN
         DO I=0,INT(sys_last_surf())
            CALL LENSF(I,RDOUT)
         END DO
      END IF
      IF(ISAC) THEN
         DO I=0,INT(sys_last_surf())
            CALL LENSFAC(I)
         END DO
      END IF
!
!
      IF(ISAC) CALL LENEDAC
      IF(ISKDP) CALL LENED
!       SHOULD WE CALL PIKSLV AT ALL?
      IF(ISKDP) THEN
!     PLANE LENO ONLY
         CAL=.FALSE.
         DO I=0,INT(sys_last_surf())
            IF(surf_special_type(I).NE.0.0D0.OR.surf_solve_flag(I).NE.0.0D0) CAL=.TRUE.
         END DO
         IF(CAL) THEN
!       YES GO TO THE U L CYCLE
            WRITE(OUTLYNE,10)
            CALL SHOWIT(10)
10          FORMAT('U        L')
            DO I=0,INT(sys_last_surf())
               IF(surf_special_type(I).NE.0.0D0.OR.surf_solve_flag(I).NE.0.0D0) THEN
!       SOLVES AND/OR PIKUPS EXITS, CALL PIKSLV
                  WRITE(OUTLYNE,11) DBLE(I)
                  CALL SHOWIT(10)
11                FORMAT('CHG     ,',G23.15)
                  CALL PIKSLV(I)
               END IF
            END DO
            WRITE(OUTLYNE,15)
            CALL SHOWIT(10)
15          FORMAT('EOS')
         END IF
      END IF
      IF(OUT.NE.6.AND.OUT.NE.7) THEN
         IF(.NOT.NOOPT) THEN
!     TOL AND OPIMIZATION DUMPING IF NEEDED
            IF(ISKDP) THEN
100            FORMAT(A4,1X,D15.8,1X,D15.8,1X,D15.8,1X,I2)
               IF(RAYY(501).EQ.0.0D0.AND.RAYX(501).EQ.0.0D0)  SSTOPI=500
               IF(RAYY(501).NE.0.0D0.OR.RAYX(501).NE.0.0D0)   SSTOPI=1000
               IF(RAYY(1001).NE.0.0D0.OR.RAYX(1001).NE.0.0D0) SSTOPI=1500
               IF(RAYY(1501).NE.0.0D0.OR.RAYX(1501).NE.0.0D0) SSTOPI=2000
               IF(RAYY(2001).NE.0.0D0.OR.RAYX(2001).NE.0.0D0) SSTOPI=2500
               IF(RAYY(2501).NE.0.0D0.OR.RAYX(2501).NE.0.0D0) SSTOPI=3000
               IF(RAYY(3001).NE.0.0D0.OR.RAYX(3001).NE.0.0D0) SSTOPI=3500
               IF(RAYY(3501).NE.0.0D0.OR.RAYX(3501).NE.0.0D0) SSTOPI=4000
               IF(RAYY(4001).NE.0.0D0.OR.RAYX(4001).NE.0.0D0) SSTOPI=4500
               IF(RAYY(4501).NE.0.0D0.OR.RAYX(4501).NE.0.0D0) SSTOPI=5000
               WRITE(OUTLYNE,*) 'FLDSRAYS ',DBLE(SSTOPI)
               CALL SHOWIT(10)
               DO I=1,200
                  CALL ITOAAA(I,AI4)
                  WRITE(UNIT=OUTLYNE,FMT=100) AI4,FIELDY(I),FIELDX(I),FIELDZ(I),INT(FIELDW(I))
                  CALL SHOWIT(11)
               END DO
102            FORMAT(A4,1X,D15.8,1X,D15.8,1X,I2)
               IF(RAYY(501).EQ.0.0D0.AND.RAYX(501).EQ.0.0D0)  SSTOPI=500
               IF(RAYY(501).NE.0.0D0.OR.RAYX(501).NE.0.0D0)   SSTOPI=1000
               IF(RAYY(1001).NE.0.0D0.OR.RAYX(1001).NE.0.0D0) SSTOPI=1500
               IF(RAYY(1501).NE.0.0D0.OR.RAYX(1501).NE.0.0D0) SSTOPI=2000
               IF(RAYY(2001).NE.0.0D0.OR.RAYX(2001).NE.0.0D0) SSTOPI=2500
               IF(RAYY(2501).NE.0.0D0.OR.RAYX(2501).NE.0.0D0) SSTOPI=3000
               IF(RAYY(3001).NE.0.0D0.OR.RAYX(3001).NE.0.0D0) SSTOPI=3500
               IF(RAYY(3501).NE.0.0D0.OR.RAYX(3501).NE.0.0D0) SSTOPI=4000
               IF(RAYY(4001).NE.0.0D0.OR.RAYX(4001).NE.0.0D0) SSTOPI=4500
               IF(RAYY(4501).NE.0.0D0.OR.RAYX(4501).NE.0.0D0) SSTOPI=5000
               DO I=1,SSTOPI
                  CALL ITOAAA(I,AI4)
                  WRITE(UNIT=OUTLYNE,FMT=102) AI4,RAYY(I),RAYX(I),INT(RAYW(I))
                  CALL SHOWIT(11)
               END DO
            END IF
            CALL OPDMP
            CALL TLDMP
         END IF
      END IF
      IF(ISKDP) CALL MULTFLDS
!

!       FINISHED WITH LENO OUTPUT
!
!********************************************************
!
      RETURN
   END IF
   IF(WQ.NE.'REVERSE'.AND.WQ.NE.'CV'.AND.WQ.NE.'AC'.AND.WQ.NE.'RD'.AND.WQ.NE.'NOOPT'.AND.WQ.NE.'HEX') THEN
      WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD USED WITH "LENO".'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(WQ.EQ.'REVERSE') THEN
!       DO A LENO REVERSE. THIS OPERATION REMOVES
!       ALL PIKUPS, SOLVES, TILTS, DECENTRATIONS, AND ALTERNATE
!       CONFIGURATIONS. THE REVERSED DESIGN MAY NOT BE TRACABLE.
      CALL RLENHD
      DO 20 I=INT(sys_last_surf()),0,-1
         CALL RLENSF(I)
20    CONTINUE
!       LENO REVERSE COMPLETED.
      CALL RLENED
      RETURN
   END IF
   IF(WQ.EQ.'CV') THEN
!       DO A LENO CV (CODE-V). THIS OPERATION REMOVES
!       ALL PIKUPS, SOLVES, AND ALTERNATE
!       CONFIGURATIONS.
      PRINT *, "LENO CV Being executed..."
      CALL LENHDCV
      DO I=0,INT(sys_last_surf())
         CALL LENSFCV(I)
      END DO
      OUTLYNE='GO'
      CALL SHOWIT(10)
!       LENO REVERSE COMPLETED.
      RETURN
   END IF
   RETURN
END
! SUB LENOCSV.FOR
SUBROUTINE LENOCSV
   use DATCFG
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_last_surf
   INTEGER I
   real(real64) RD,TH
   CHARACTER*13 GLNAME
   OPEN(UNIT=84,BLANK='NULL',FORM='FORMATTED',FILE='LENO.CSV',STATUS='UNKNOWN')
   CALL CLOSE_FILE(84,0)
   OPEN(UNIT=84,BLANK='NULL',FORM='FORMATTED',FILE='LENO.CSV',STATUS='UNKNOWN')
!     HERE IS WHERE WE OUTPUT THE LENS DATABASE TO A CSV FILE
!     OUTPUT OF SURF #,RADIUS, THICKNESS, GLASS NAME
   WRITE(84,*)'SURF#,RADIUS,THICKNESS,GLASS NAME'
   DO I=0,INT(sys_last_surf())
      IF(surf_curvature(I).EQ.0.0D0) THEN
         RD=0.0D0
      ELSE
         RD=1.0D0/surf_curvature(I)
      END IF
      TH=surf_thickness(I)
      GLNAME=GLANAM(I,2)
      WRITE(84,10) I,RD,TH,GLNAME
10    FORMAT(I3,',',D23.15,',',D23.15,',',A13)
   END DO
   CALL CLOSE_FILE(84,1)
   RETURN
END

! SUB MULTFLDS.FOR
SUBROUTINE MULTFLDS
!
   use DATLEN
   use mod_surface
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       SUBROUTINE MULTFLDS HANDELS LENS MULTIPLE FIELD OF VIEW OUTPUT
!
   INTEGER I
!
!
   IF(CFLDCNT.GT.0) THEN
!     MULTIPLE FLDS DEFINED
!
      WRITE(OUTLYNE,1000) DBLE(CFLDCNT)
      CALL SHOWIT(10)
1000  FORMAT('FLDS MAX ,',G23.15)
      DO I=1,CFLDCNT
         WRITE(OUTLYNE,2000) DBLE(I),CFLDS(1,I),CFLDS(2,I)
         CALL SHOWIT(10)
      END DO
2000  FORMAT('FLDS ,',G23.15,',',G23.15,',',G23.15)
   END IF
   RETURN
END
! SUB LENED.FOR
SUBROUTINE LENED
!
   use DATCFG
   use DATSPD
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_aim_offset_x, sys_aim_offset_y, sys_aim_offset_z, &
      & sys_fliprefx, sys_fliprefy, &
      & sys_last_surf, sys_mode, sys_ray_aiming, sys_screen, sys_screen_d, &
      & sys_screen_excl_angle, sys_screen_h, sys_screen_s, sys_screen_surf, &
      & sys_telecentric, sys_wl_weight
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       SUBROUTINE LENED HANDEL LENS TRAILING INFO OUTPUT
!       DURING A SIMPLE LENO. ACTS ON THE CURRENT LENS.
!       USED AFTER SURFACE DATA OUTPUT
!
   INTEGER SS,I,J
!
!
!       THE TERMINAL RECORD FOR AND LENO IS EOS
!
   WRITE(OUTLYNE,10)
   CALL SHOWIT(10)
!
!**********************************************************
   IF(sys_ray_aiming().EQ.1.0D0) THEN
!     RAY AIMING YES
      WRITE(OUTLYNE,1000)
      CALL SHOWIT(10)
      WRITE(OUTLYNE,4000) sys_aim_offset_x(),sys_aim_offset_y(),sys_aim_offset_z()
      CALL SHOWIT(10)
4000  FORMAT('AIMRAY  OFFSET  ,',G23.15,',',G23.15,',',G23.15)
   ELSE
      IF(sys_telecentric().EQ.1.0D0) THEN
!     TEL YES
         WRITE(OUTLYNE,1002)
         CALL SHOWIT(10)
      ELSE
!     RAY AIMING OFF
         WRITE(OUTLYNE,1001)
         CALL SHOWIT(10)
      END IF
1000  FORMAT('AIMRAY   ON')
1001  FORMAT('AIMRAY   OFF')
1002  FORMAT('TEL      ON')
   END IF
!**********************************************************
!**********************************************************
   IF(sys_fliprefx().EQ.1.0D0) THEN
!     FLIPREFY
      WRITE(OUTLYNE,4001)
      CALL SHOWIT(10)
4001  FORMAT('FLIPREFX ON')
   END IF
!**********************************************************
!**********************************************************
   IF(sys_fliprefy().EQ.1.0D0) THEN
!     FLIPREFY
      WRITE(OUTLYNE,4002)
      CALL SHOWIT(10)
4002  FORMAT('FLIPREFY ON')
   END IF
!**********************************************************
!**********************************************************
   IF(sys_screen().EQ.1.0D0) THEN
!     SCREEN ON
      WRITE(OUTLYNE,1003) sys_screen_surf(),sys_screen_d(),sys_screen_h(),sys_screen_s(),sys_screen_excl_angle()
      CALL SHOWIT(10)
1003  FORMAT('SCREEN   ON      ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
   END IF
!**********************************************************

!       MODE SETTING

   IF(sys_mode().EQ.1) WRITE(OUTLYNE,33)
   IF(sys_mode().EQ.2) WRITE(OUTLYNE,34)
   IF(sys_mode().EQ.3) WRITE(OUTLYNE,35)
   IF(sys_mode().EQ.4) WRITE(OUTLYNE,36)
   CALL SHOWIT(10)
33 FORMAT('MODE     FOCAL')
34 FORMAT('MODE     UFOCAL')
35 FORMAT('MODE     AFOCAL')
36 FORMAT('MODE     UAFOCAL')
!
!       SPTWT

   WRITE(OUTLYNE,3000) sys_wl_weight(1),sys_wl_weight(2),sys_wl_weight(3),sys_wl_weight(4),sys_wl_weight(5)
   CALL SHOWIT(10)
3000 FORMAT('SPTWT   ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
!       SPTWT2

3003 FORMAT('SPTWT2  ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
   WRITE(OUTLYNE,3003) sys_wl_weight(6),sys_wl_weight(7),sys_wl_weight(8),sys_wl_weight(9),sys_wl_weight(10)
   CALL SHOWIT(10)
!     *****************************************************************
!       NOW IS THERE ANY SPSRF DATA
!
   SS=0
   DO 20 I=0,INT(sys_last_surf())
      IF(surf_asi_flag(I).NE.0.0) THEN
!       FOUND SPECIAL SURFACE DATA
         SS=SS+1
      END IF
20 CONTINUE
   IF(SS.NE.0) THEN
!       PRINT SPECIAL SURFACE DATA
      WRITE(OUTLYNE,200)
      CALL SHOWIT(10)
200   FORMAT('SPSRF')
      DO 21  I=0,INT(sys_last_surf())
         IF(surf_asi_flag(I).NE.0.0) THEN
            WRITE(OUTLYNE,210) DBLE(I),surf_asi_flag(I)
            CALL SHOWIT(10)
210         FORMAT('SPECIAL ,',G23.15,',',G23.15)
!
!       NOW WRITE THE COEFFICIENTS
            IF(ABS(surf_asi_flag(I)).GE.1.0.AND.ABS(surf_asi_flag(I)).LE.30.0) THEN
               IF(FTFL01(1,I).NE.0.0D0) WRITE(OUTLYNE,300) DBLE(I),FTFL01(1,I)
               IF(FTFL01(1,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(2,I).NE.0.0D0) WRITE(OUTLYNE,301) DBLE(I),FTFL01(2,I)
               IF(FTFL01(2,I).NE.0.0D0) CALL SHOWIT(10)
               IF(ABS(surf_asi_flag(I)).NE.12.0D0.AND.ABS(surf_asi_flag(I)).NE.13.0D0)THEN
                  IF(FTFL01(3,I).NE.0.0D0) WRITE(OUTLYNE,302) DBLE(I),FTFL01(3,I)
                  IF(FTFL01(3,I).NE.0.0D0) CALL SHOWIT(10)
                  IF(FTFL01(4,I).NE.0.0D0) WRITE(OUTLYNE,303) DBLE(I),FTFL01(4,I)
                  IF(FTFL01(4,I).NE.0.0D0) CALL SHOWIT(10)
                  IF(FTFL01(5,I).NE.0.0D0) WRITE(OUTLYNE,304) DBLE(I),FTFL01(5,I)
                  IF(FTFL01(5,I).NE.0.0D0) CALL SHOWIT(10)
               ELSE
                  WRITE(OUTLYNE,302) DBLE(I),FTFL01(3,I)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,303) DBLE(I),FTFL01(4,I)
                  CALL SHOWIT(10)
                  WRITE(OUTLYNE,304) DBLE(I),FTFL01(5,I)
                  CALL SHOWIT(10)
               END IF

               IF(FTFL01(6,I).NE.0.0D0) WRITE(OUTLYNE,305) DBLE(I),FTFL01(6,I)
               IF(FTFL01(6,I).NE.0.0D0) CALL SHOWIT(10)

               IF(ABS(surf_asi_flag(I)).NE.12.0D0.AND.ABS(surf_asi_flag(I)).NE.13.0D0)THEN
                  IF(FTFL01(7,I).NE.0.0D0) WRITE(OUTLYNE,306) DBLE(I),FTFL01(7,I)
                  IF(FTFL01(7,I).NE.0.0D0) CALL SHOWIT(10)

                  IF(FTFL01(8,I).NE.0.0D0) WRITE(OUTLYNE,307) DBLE(I),FTFL01(8,I)
                  IF(FTFL01(8,I).NE.0.0D0) CALL SHOWIT(10)

                  IF(FTFL01(9,I).NE.0.0D0) WRITE(OUTLYNE,308) DBLE(I),FTFL01(9,I)
                  IF(FTFL01(9,I).NE.0.0D0) CALL SHOWIT(10)
               ELSE
                  WRITE(OUTLYNE,306) DBLE(I),FTFL01(7,I)
                  CALL SHOWIT(10)

                  WRITE(OUTLYNE,307) DBLE(I),FTFL01(8,I)
                  CALL SHOWIT(10)

                  WRITE(OUTLYNE,308) DBLE(I),FTFL01(9,I)
                  CALL SHOWIT(10)
               END IF

               IF(FTFL01(10,I).NE.0.0D0) WRITE(OUTLYNE,309) DBLE(I),FTFL01(10,I)
               IF(FTFL01(10,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(11,I).NE.0.0D0) WRITE(OUTLYNE,310) DBLE(I),FTFL01(11,I)
               IF(FTFL01(11,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(12,I).NE.0.0D0) WRITE(OUTLYNE,311) DBLE(I),FTFL01(12,I)
               IF(FTFL01(12,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(13,I).NE.0.0D0) WRITE(OUTLYNE,312) DBLE(I),FTFL01(13,I)
               IF(FTFL01(13,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(14,I).NE.0.0D0) WRITE(OUTLYNE,313) DBLE(I),FTFL01(14,I)
               IF(FTFL01(14,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(15,I).NE.0.0D0) WRITE(OUTLYNE,314) DBLE(I),FTFL01(15,I)
               IF(FTFL01(15,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(16,I).NE.0.0D0) WRITE(OUTLYNE,315) DBLE(I),FTFL01(16,I)
               IF(FTFL01(16,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(17,I).NE.0.0D0) WRITE(OUTLYNE,316) DBLE(I),FTFL01(17,I)
               IF(FTFL01(17,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(18,I).NE.0.0D0) WRITE(OUTLYNE,317) DBLE(I),FTFL01(18,I)
               IF(FTFL01(18,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(19,I).NE.0.0D0) WRITE(OUTLYNE,318) DBLE(I),FTFL01(19,I)
               IF(FTFL01(19,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(20,I).NE.0.0D0) WRITE(OUTLYNE,319) DBLE(I),FTFL01(20,I)
               IF(FTFL01(20,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(21,I).NE.0.0D0) WRITE(OUTLYNE,320) DBLE(I),FTFL01(21,I)
               IF(FTFL01(21,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(22,I).NE.0.0D0) WRITE(OUTLYNE,321) DBLE(I),FTFL01(22,I)
               IF(FTFL01(22,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(23,I).NE.0.0D0) WRITE(OUTLYNE,322) DBLE(I),FTFL01(23,I)
               IF(FTFL01(23,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(24,I).NE.0.0D0) WRITE(OUTLYNE,323) DBLE(I),FTFL01(24,I)
               IF(FTFL01(24,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(25,I).NE.0.0D0) WRITE(OUTLYNE,324) DBLE(I),FTFL01(25,I)
               IF(FTFL01(25,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(26,I).NE.0.0D0) WRITE(OUTLYNE,325) DBLE(I),FTFL01(26,I)
               IF(FTFL01(26,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(27,I).NE.0.0D0) WRITE(OUTLYNE,326) DBLE(I),FTFL01(27,I)
               IF(FTFL01(27,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(28,I).NE.0.0D0) WRITE(OUTLYNE,327) DBLE(I),FTFL01(28,I)
               IF(FTFL01(28,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(29,I).NE.0.0D0) WRITE(OUTLYNE,328) DBLE(I),FTFL01(29,I)
               IF(FTFL01(29,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(30,I).NE.0.0D0) WRITE(OUTLYNE,329) DBLE(I),FTFL01(30,I)
               IF(FTFL01(30,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(31,I).NE.0.0D0) WRITE(OUTLYNE,330) DBLE(I),FTFL01(31,I)
               IF(FTFL01(31,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(32,I).NE.0.0D0) WRITE(OUTLYNE,331) DBLE(I),FTFL01(32,I)
               IF(FTFL01(32,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(33,I).NE.0.0D0) WRITE(OUTLYNE,332) DBLE(I),FTFL01(33,I)
               IF(FTFL01(33,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(34,I).NE.0.0D0) WRITE(OUTLYNE,333) DBLE(I),FTFL01(34,I)
               IF(FTFL01(34,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(35,I).NE.0.0D0) WRITE(OUTLYNE,334) DBLE(I),FTFL01(35,I)
               IF(FTFL01(35,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(36,I).NE.0.0D0) WRITE(OUTLYNE,335) DBLE(I),FTFL01(36,I)
               IF(FTFL01(36,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(37,I).NE.0.0D0) WRITE(OUTLYNE,336) DBLE(I),FTFL01(37,I)
               IF(FTFL01(37,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(38,I).NE.0.0D0) WRITE(OUTLYNE,337) DBLE(I),FTFL01(38,I)
               IF(FTFL01(38,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(39,I).NE.0.0D0) WRITE(OUTLYNE,338) DBLE(I),FTFL01(39,I)
               IF(FTFL01(39,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(40,I).NE.0.0D0) WRITE(OUTLYNE,339) DBLE(I),FTFL01(40,I)
               IF(FTFL01(40,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(41,I).NE.0.0D0) WRITE(OUTLYNE,340) DBLE(I),FTFL01(41,I)
               IF(FTFL01(41,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(42,I).NE.0.0D0) WRITE(OUTLYNE,341) DBLE(I),FTFL01(42,I)
               IF(FTFL01(42,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(43,I).NE.0.0D0) WRITE(OUTLYNE,342) DBLE(I),FTFL01(43,I)
               IF(FTFL01(43,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(44,I).NE.0.0D0) WRITE(OUTLYNE,343) DBLE(I),FTFL01(44,I)
               IF(FTFL01(44,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(45,I).NE.0.0D0) WRITE(OUTLYNE,344) DBLE(I),FTFL01(45,I)
               IF(FTFL01(45,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(46,I).NE.0.0D0) WRITE(OUTLYNE,345) DBLE(I),FTFL01(46,I)
               IF(FTFL01(46,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(47,I).NE.0.0D0) WRITE(OUTLYNE,346) DBLE(I),FTFL01(47,I)
               IF(FTFL01(47,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(48,I).NE.0.0D0) WRITE(OUTLYNE,347) DBLE(I),FTFL01(48,I)
               IF(FTFL01(48,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(49,I).NE.0.0D0) WRITE(OUTLYNE,348) DBLE(I),FTFL01(49,I)
               IF(FTFL01(49,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(50,I).NE.0.0D0) WRITE(OUTLYNE,349) DBLE(I),FTFL01(50,I)
               IF(FTFL01(50,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(51,I).NE.0.0D0) WRITE(OUTLYNE,350) DBLE(I),FTFL01(51,I)
               IF(FTFL01(51,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(52,I).NE.0.0D0) WRITE(OUTLYNE,351) DBLE(I),FTFL01(52,I)
               IF(FTFL01(52,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(53,I).NE.0.0D0) WRITE(OUTLYNE,352) DBLE(I),FTFL01(53,I)
               IF(FTFL01(53,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(54,I).NE.0.0D0) WRITE(OUTLYNE,353) DBLE(I),FTFL01(54,I)
               IF(FTFL01(54,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(55,I).NE.0.0D0) WRITE(OUTLYNE,354) DBLE(I),FTFL01(55,I)
               IF(FTFL01(55,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(56,I).NE.0.0D0) WRITE(OUTLYNE,355) DBLE(I),FTFL01(56,I)
               IF(FTFL01(56,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(57,I).NE.0.0D0) WRITE(OUTLYNE,356) DBLE(I),FTFL01(57,I)
               IF(FTFL01(57,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(58,I).NE.0.0D0) WRITE(OUTLYNE,357) DBLE(I),FTFL01(58,I)
               IF(FTFL01(58,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(59,I).NE.0.0D0) WRITE(OUTLYNE,358) DBLE(I),FTFL01(59,I)
               IF(FTFL01(59,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(60,I).NE.0.0D0) WRITE(OUTLYNE,359) DBLE(I),FTFL01(60,I)
               IF(FTFL01(60,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(61,I).NE.0.0D0) WRITE(OUTLYNE,360) DBLE(I),FTFL01(61,I)
               IF(FTFL01(61,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(62,I).NE.0.0D0) WRITE(OUTLYNE,361) DBLE(I),FTFL01(62,I)
               IF(FTFL01(62,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(63,I).NE.0.0D0) WRITE(OUTLYNE,362) DBLE(I),FTFL01(63,I)
               IF(FTFL01(63,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(64,I).NE.0.0D0) WRITE(OUTLYNE,363) DBLE(I),FTFL01(64,I)
               IF(FTFL01(64,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(65,I).NE.0.0D0) WRITE(OUTLYNE,364) DBLE(I),FTFL01(65,I)
               IF(FTFL01(65,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(66,I).NE.0.0D0) WRITE(OUTLYNE,365) DBLE(I),FTFL01(66,I)
               IF(FTFL01(66,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(67,I).NE.0.0D0) WRITE(OUTLYNE,366) DBLE(I),FTFL01(67,I)
               IF(FTFL01(67,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(68,I).NE.0.0D0) WRITE(OUTLYNE,367) DBLE(I),FTFL01(68,I)
               IF(FTFL01(68,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(69,I).NE.0.0D0) WRITE(OUTLYNE,368) DBLE(I),FTFL01(69,I)
               IF(FTFL01(69,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(70,I).NE.0.0D0) WRITE(OUTLYNE,369) DBLE(I),FTFL01(70,I)
               IF(FTFL01(70,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(71,I).NE.0.0D0) WRITE(OUTLYNE,370) DBLE(I),FTFL01(71,I)
               IF(FTFL01(71,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(72,I).NE.0.0D0) WRITE(OUTLYNE,371) DBLE(I),FTFL01(72,I)
               IF(FTFL01(72,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(73,I).NE.0.0D0) WRITE(OUTLYNE,372) DBLE(I),FTFL01(73,I)
               IF(FTFL01(73,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(74,I).NE.0.0D0) WRITE(OUTLYNE,373) DBLE(I),FTFL01(74,I)
               IF(FTFL01(74,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(75,I).NE.0.0D0) WRITE(OUTLYNE,374) DBLE(I),FTFL01(75,I)
               IF(FTFL01(75,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(76,I).NE.0.0D0) WRITE(OUTLYNE,375) DBLE(I),FTFL01(76,I)
               IF(FTFL01(76,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(77,I).NE.0.0D0) WRITE(OUTLYNE,376) DBLE(I),FTFL01(77,I)
               IF(FTFL01(77,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(78,I).NE.0.0D0) WRITE(OUTLYNE,377) DBLE(I),FTFL01(78,I)
               IF(FTFL01(78,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(79,I).NE.0.0D0) WRITE(OUTLYNE,378) DBLE(I),FTFL01(79,I)
               IF(FTFL01(79,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(80,I).NE.0.0D0) WRITE(OUTLYNE,379) DBLE(I),FTFL01(80,I)
               IF(FTFL01(80,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(81,I).NE.0.0D0) WRITE(OUTLYNE,380) DBLE(I),FTFL01(81,I)
               IF(FTFL01(81,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(82,I).NE.0.0D0) WRITE(OUTLYNE,381) DBLE(I),FTFL01(82,I)
               IF(FTFL01(82,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(83,I).NE.0.0D0) WRITE(OUTLYNE,382) DBLE(I),FTFL01(83,I)
               IF(FTFL01(83,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(84,I).NE.0.0D0) WRITE(OUTLYNE,383) DBLE(I),FTFL01(84,I)
               IF(FTFL01(84,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(85,I).NE.0.0D0) WRITE(OUTLYNE,384) DBLE(I),FTFL01(85,I)
               IF(FTFL01(85,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(86,I).NE.0.0D0) WRITE(OUTLYNE,385) DBLE(I),FTFL01(86,I)
               IF(FTFL01(86,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(87,I).NE.0.0D0) WRITE(OUTLYNE,386) DBLE(I),FTFL01(87,I)
               IF(FTFL01(87,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(88,I).NE.0.0D0) WRITE(OUTLYNE,387) DBLE(I),FTFL01(88,I)
               IF(FTFL01(88,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(89,I).NE.0.0D0) WRITE(OUTLYNE,388) DBLE(I),FTFL01(89,I)
               IF(FTFL01(89,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(90,I).NE.0.0D0) WRITE(OUTLYNE,389) DBLE(I),FTFL01(90,I)
               IF(FTFL01(90,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(91,I).NE.0.0D0) WRITE(OUTLYNE,390) DBLE(I),FTFL01(91,I)
               IF(FTFL01(91,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(92,I).NE.0.0D0) WRITE(OUTLYNE,391) DBLE(I),FTFL01(92,I)
               IF(FTFL01(92,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(93,I).NE.0.0D0) WRITE(OUTLYNE,392) DBLE(I),FTFL01(93,I)
               IF(FTFL01(93,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(94,I).NE.0.0D0) WRITE(OUTLYNE,393) DBLE(I),FTFL01(94,I)
               IF(FTFL01(94,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(95,I).NE.0.0D0) WRITE(OUTLYNE,394) DBLE(I),FTFL01(95,I)
               IF(FTFL01(95,I).NE.0.0D0) CALL SHOWIT(10)

               IF(FTFL01(96,I).NE.0.0D0) WRITE(OUTLYNE,395) DBLE(I),FTFL01(96,I)
               IF(FTFL01(96,I).NE.0.0D0) CALL SHOWIT(10)
            END IF
         END IF
21    CONTINUE
      WRITE(OUTLYNE,10)
      CALL SHOWIT(10)
   END IF
300 FORMAT('C1      ,',G23.15,',',G23.15)
301 FORMAT('C2      ,',G23.15,',',G23.15)
302 FORMAT('C3      ,',G23.15,',',G23.15)
303 FORMAT('C4      ,',G23.15,',',G23.15)
304 FORMAT('C5      ,',G23.15,',',G23.15)
305 FORMAT('C6      ,',G23.15,',',G23.15)
306 FORMAT('C7      ,',G23.15,',',G23.15)
307 FORMAT('C8      ,',G23.15,',',G23.15)
308 FORMAT('C9      ,',G23.15,',',G23.15)
309 FORMAT('C10     ,',G23.15,',',G23.15)
310 FORMAT('C11     ,',G23.15,',',G23.15)
311 FORMAT('C12     ,',G23.15,',',G23.15)
312 FORMAT('C13     ,',G23.15,',',G23.15)
313 FORMAT('C14     ,',G23.15,',',G23.15)
314 FORMAT('C15     ,',G23.15,',',G23.15)
315 FORMAT('C16     ,',G23.15,',',G23.15)
316 FORMAT('C17     ,',G23.15,',',G23.15)
317 FORMAT('C18     ,',G23.15,',',G23.15)
318 FORMAT('C19     ,',G23.15,',',G23.15)
319 FORMAT('C20     ,',G23.15,',',G23.15)
320 FORMAT('C21     ,',G23.15,',',G23.15)
321 FORMAT('C22     ,',G23.15,',',G23.15)
322 FORMAT('C23     ,',G23.15,',',G23.15)
323 FORMAT('C24     ,',G23.15,',',G23.15)
324 FORMAT('C25     ,',G23.15,',',G23.15)
325 FORMAT('C26     ,',G23.15,',',G23.15)
326 FORMAT('C27     ,',G23.15,',',G23.15)
327 FORMAT('C28     ,',G23.15,',',G23.15)
328 FORMAT('C29     ,',G23.15,',',G23.15)
329 FORMAT('C30     ,',G23.15,',',G23.15)
330 FORMAT('C31     ,',G23.15,',',G23.15)
331 FORMAT('C32     ,',G23.15,',',G23.15)
332 FORMAT('C33     ,',G23.15,',',G23.15)
333 FORMAT('C34     ,',G23.15,',',G23.15)
334 FORMAT('C35     ,',G23.15,',',G23.15)
335 FORMAT('C36     ,',G23.15,',',G23.15)
336 FORMAT('C37     ,',G23.15,',',G23.15)
337 FORMAT('C38     ,',G23.15,',',G23.15)
338 FORMAT('C39     ,',G23.15,',',G23.15)
339 FORMAT('C40     ,',G23.15,',',G23.15)
340 FORMAT('C41     ,',G23.15,',',G23.15)
341 FORMAT('C42     ,',G23.15,',',G23.15)
342 FORMAT('C43     ,',G23.15,',',G23.15)
343 FORMAT('C44     ,',G23.15,',',G23.15)
344 FORMAT('C45     ,',G23.15,',',G23.15)
345 FORMAT('C46     ,',G23.15,',',G23.15)
346 FORMAT('C47     ,',G23.15,',',G23.15)
347 FORMAT('C48     ,',G23.15,',',G23.15)
348 FORMAT('C49     ,',G23.15,',',G23.15)
349 FORMAT('C50     ,',G23.15,',',G23.15)
350 FORMAT('C51     ,',G23.15,',',G23.15)
351 FORMAT('C52     ,',G23.15,',',G23.15)
352 FORMAT('C53     ,',G23.15,',',G23.15)
353 FORMAT('C54     ,',G23.15,',',G23.15)
354 FORMAT('C55     ,',G23.15,',',G23.15)
355 FORMAT('C56     ,',G23.15,',',G23.15)
356 FORMAT('C57     ,',G23.15,',',G23.15)
357 FORMAT('C58     ,',G23.15,',',G23.15)
358 FORMAT('C59     ,',G23.15,',',G23.15)
359 FORMAT('C60     ,',G23.15,',',G23.15)
360 FORMAT('C61     ,',G23.15,',',G23.15)
361 FORMAT('C62     ,',G23.15,',',G23.15)
362 FORMAT('C63     ,',G23.15,',',G23.15)
363 FORMAT('C64     ,',G23.15,',',G23.15)
364 FORMAT('C65     ,',G23.15,',',G23.15)
365 FORMAT('C66     ,',G23.15,',',G23.15)
366 FORMAT('C67     ,',G23.15,',',G23.15)
367 FORMAT('C68     ,',G23.15,',',G23.15)
368 FORMAT('C69     ,',G23.15,',',G23.15)
369 FORMAT('C70     ,',G23.15,',',G23.15)
370 FORMAT('C71     ,',G23.15,',',G23.15)
371 FORMAT('C72     ,',G23.15,',',G23.15)
372 FORMAT('C73     ,',G23.15,',',G23.15)
373 FORMAT('C74     ,',G23.15,',',G23.15)
374 FORMAT('C75     ,',G23.15,',',G23.15)
375 FORMAT('C76     ,',G23.15,',',G23.15)
376 FORMAT('C77     ,',G23.15,',',G23.15)
377 FORMAT('C78     ,',G23.15,',',G23.15)
378 FORMAT('C79     ,',G23.15,',',G23.15)
379 FORMAT('C80     ,',G23.15,',',G23.15)
380 FORMAT('C81     ,',G23.15,',',G23.15)
381 FORMAT('C82     ,',G23.15,',',G23.15)
382 FORMAT('C83     ,',G23.15,',',G23.15)
383 FORMAT('C84     ,',G23.15,',',G23.15)
384 FORMAT('C85     ,',G23.15,',',G23.15)
385 FORMAT('C86     ,',G23.15,',',G23.15)
386 FORMAT('C87     ,',G23.15,',',G23.15)
387 FORMAT('C88     ,',G23.15,',',G23.15)
388 FORMAT('C89     ,',G23.15,',',G23.15)
389 FORMAT('C90     ,',G23.15,',',G23.15)
390 FORMAT('C91     ,',G23.15,',',G23.15)
391 FORMAT('C92     ,',G23.15,',',G23.15)
392 FORMAT('C93     ,',G23.15,',',G23.15)
393 FORMAT('C94     ,',G23.15,',',G23.15)
394 FORMAT('C95     ,',G23.15,',',G23.15)
395 FORMAT('C96     ,',G23.15,',',G23.15)
!**********************************************************
!
!       IF THE LENS FILE HAS ANY CONFIGURATION DATA
!       DEFINED, WRITE IT OUT
!
!       CHECK EACH CONFIGURATION
!
   SS=0
   DO 100 I=2,MAXCFG
      IF(CFGCNT(I).GT.0) THEN
!       THERE IS DATA FOR CFG I
         SS=SS+1
      END IF
100 CONTINUE
   IF(SS.GT.0) THEN
!
!       THERE IS CONFIGS DATA
      WRITE(OUTLYNE,11)
      CALL SHOWIT(10)
11    FORMAT('CONFIGS')
      DO 110 I=2,MAXCFG
         IF(CFGCNT(I).GT.0) THEN
            WRITE(OUTLYNE,13) DBLE(I)
            CALL SHOWIT(10)
13          FORMAT('CFG     ,',G23.15)
!
            DO 120 J=1,CFGCNT(I)
               WRITE(OUTLYNE,12) CONFG(I,J)(1:139)
               CALL SHOWIT(10)
12             FORMAT(A139)
120         CONTINUE
         END IF
!       FINISHED WRITTING CURRENT CFG DATA
!       OR CFG HAD NO DATA
!       CHECK FOR OTHER CFG DATA
110   CONTINUE
!       FINISHED WRITEING CFG DATA, WRITE EOS
      WRITE(OUTLYNE,10)
      CALL SHOWIT(10)
!
!       ALL CFG I DATA WRITEN
10    FORMAT('EOS')
   END IF
!
!       TGR,NRD,PGR,GRI
   IF(TGR.GT.0.0D0.AND.TGRFLG.NE.0) THEN
      WRITE(OUTLYNE,*)'TGR,',TGR
      CALL SHOWIT(10)
   END IF
   IF(NRD.GT.0.0D0.AND.NRDFLG.NE.0) THEN
      WRITE(OUTLYNE,*)'NRD,',NRD
      CALL SHOWIT(10)
   END IF
   IF(PGR.GT.0.0D0) THEN
      WRITE(OUTLYNE,*)'PGR,',PGR
      CALL SHOWIT(10)
   END IF
   IF(GRI.GT.0.0D0.AND.GRIFLG.NE.0) THEN
      WRITE(OUTLYNE,*)'GRI,',GRI
      CALL SHOWIT(10)
   END IF
!
   RETURN
END
! SUB RLENED.FOR
SUBROUTINE RLENED
!
   use DATCFG
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_aim_offset_x, sys_aim_offset_y, sys_aim_offset_z, &
      & sys_fliprefx, sys_fliprefy, sys_last_surf, sys_mode, sys_wl_weight
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       SUBROUTINE LENED HANDEL LENS TRAILING INFO OUTPUT
!       DURING A LENO REVERSE. ACTS ON THE CURRENT LENS.
!       USED AFTER SURFACE DATA OUTPUT
!
   INTEGER SS,I,J
!
!
!       THE TERMINAL RECORD FOR AND LENO IS EOS
!
   WRITE(OUTLYNE,10)
   CALL SHOWIT(10)
10 FORMAT('EOS')
!
!     RAY AIMING STATUS IS ALWAYS ON FOR LENO REVERSE AND
!     TEL IS ALWAYS "OFF"
   WRITE(OUTLYNE,1000)
   CALL SHOWIT(10)
   CALL SHOWIT(10)
   WRITE(OUTLYNE,4000) sys_aim_offset_x(),sys_aim_offset_y(),sys_aim_offset_z()
   CALL SHOWIT(10)
4000 FORMAT('AIMRAY  OFFSET  ,',G23.15,',',G23.15,',',G23.15)
1000 FORMAT('AIMRAY   ON')
!**********************************************************
   IF(sys_fliprefx().EQ.1.0D0) THEN
!     FLIPREFY
      WRITE(OUTLYNE,4001)
      CALL SHOWIT(10)
4001  FORMAT('FLIPREFX ON')
   END IF
!**********************************************************
!**********************************************************
   IF(sys_fliprefy().EQ.1.0D0) THEN
!     FLIPREFY
      WRITE(OUTLYNE,4002)
      CALL SHOWIT(10)
4002  FORMAT('FLIPREFY ON')
   END IF
!**********************************************************

!       MODE SETTING
   IF(sys_mode().EQ.1) WRITE(OUTLYNE,33)
   IF(sys_mode().EQ.2) WRITE(OUTLYNE,34)
   IF(sys_mode().EQ.3) WRITE(OUTLYNE,35)
   IF(sys_mode().EQ.4) WRITE(OUTLYNE,36)
   CALL SHOWIT(10)
33 FORMAT('MODE     FOCAL')
34 FORMAT('MODE     UFOCAL')
35 FORMAT('MODE     AFOCAL')
36 FORMAT('MODE     UAFOCAL')
!       SPTWT
   WRITE(OUTLYNE,3000) sys_wl_weight(1),sys_wl_weight(2),sys_wl_weight(3),sys_wl_weight(4),sys_wl_weight(5)
   CALL SHOWIT(10)
3000 FORMAT('SPTWT   ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
!       SPTWT2
3003 FORMAT('SPTWT2  ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
   WRITE(OUTLYNE,3003) sys_wl_weight(6),sys_wl_weight(7),sys_wl_weight(8),sys_wl_weight(9),sys_wl_weight(10)
   CALL SHOWIT(10)
!
!       IF THE LENS FILE HAS ANY CONFIGURATION DATA
!       IGNORE IT
!
!       IS THERE ANY SPSRF DATA
!
   SS=0
   DO 20 I=0,INT(sys_last_surf())
      IF(surf_asi_flag(I).NE.0.0) THEN
!       FOUND SPECIAL SURFACE DATA
         SS=SS+1
      END IF
20 CONTINUE
   IF(SS.NE.0) THEN
!       PRINT SPECIAL SURFACE DATA
      WRITE(OUTLYNE,200)
      CALL SHOWIT(10)
200   FORMAT('SPSRF')
      DO 21  I=INT(sys_last_surf()),0
         J=INT(sys_last_surf())-I
         IF(surf_asi_flag(I).NE.0.0) THEN
            WRITE(OUTLYNE,210) DBLE(J),surf_asi_flag(I)
            CALL SHOWIT(10)
210         FORMAT('SPECIAL ,',G23.15,',',G23.15)
         END IF
!
!       NOW WRITE THE COEFFICIENTS
!
         IF(ABS(surf_asi_flag(I)).GE.1.0.AND.ABS(surf_asi_flag(I)).LE.30.0) THEN
            IF(FTFL01(1,I).NE.0.0D0) WRITE(OUTLYNE,300) DBLE(I),FTFL01(1,I)
            IF(FTFL01(1,I).NE.0.0D0) CALL SHOWIT(10)
            IF(FTFL01(2,I).NE.0.0D0) WRITE(OUTLYNE,301) DBLE(I),FTFL01(2,I)
            IF(FTFL01(2,I).NE.0.0D0) CALL SHOWIT(10)
            IF(ABS(surf_asi_flag(I)).NE.12.0D0.AND.ABS(surf_asi_flag(I)).NE.13.0D0)THEN
               IF(FTFL01(3,I).NE.0.0D0) WRITE(OUTLYNE,302) DBLE(I),FTFL01(3,I)
               IF(FTFL01(3,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(4,I).NE.0.0D0) WRITE(OUTLYNE,303) DBLE(I),FTFL01(4,I)
               IF(FTFL01(4,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(5,I).NE.0.0D0) WRITE(OUTLYNE,304) DBLE(I),FTFL01(5,I)
               IF(FTFL01(5,I).NE.0.0D0) CALL SHOWIT(10)
            ELSE
               WRITE(OUTLYNE,302) DBLE(I),FTFL01(3,I)
               CALL SHOWIT(10)
               WRITE(OUTLYNE,303) DBLE(I),FTFL01(4,I)
               CALL SHOWIT(10)
               WRITE(OUTLYNE,304) DBLE(I),FTFL01(5,I)
               CALL SHOWIT(10)
            END IF
            IF(FTFL01(6,I).NE.0.0D0) WRITE(OUTLYNE,305) DBLE(I),FTFL01(6,I)
            IF(FTFL01(6,I).NE.0.0D0) CALL SHOWIT(10)
            IF(ABS(surf_asi_flag(I)).NE.12.0D0.AND.ABS(surf_asi_flag(I)).NE.13.0D0)THEN
               IF(FTFL01(7,I).NE.0.0D0) WRITE(OUTLYNE,306) DBLE(I),FTFL01(7,I)
               IF(FTFL01(7,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(8,I).NE.0.0D0) WRITE(OUTLYNE,307) DBLE(I),FTFL01(8,I)
               IF(FTFL01(8,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(9,I).NE.0.0D0) WRITE(OUTLYNE,308) DBLE(I),FTFL01(9,I)
               IF(FTFL01(9,I).NE.0.0D0) CALL SHOWIT(10)
            ELSE
               WRITE(OUTLYNE,306) DBLE(I),FTFL01(7,I)
               CALL SHOWIT(10)
               WRITE(OUTLYNE,307) DBLE(I),FTFL01(8,I)
               CALL SHOWIT(10)
               WRITE(OUTLYNE,308) DBLE(I),FTFL01(9,I)
               CALL SHOWIT(10)
               IF(FTFL01(10,I).NE.0.0D0) WRITE(OUTLYNE,309) DBLE(I),FTFL01(10,I)
               IF(FTFL01(10,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(11,I).NE.0.0D0) WRITE(OUTLYNE,310) DBLE(I),FTFL01(11,I)
               IF(FTFL01(11,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(12,I).NE.0.0D0) WRITE(OUTLYNE,311) DBLE(I),FTFL01(12,I)
               IF(FTFL01(12,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(13,I).NE.0.0D0) WRITE(OUTLYNE,312) DBLE(I),FTFL01(13,I)
               IF(FTFL01(13,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(14,I).NE.0.0D0) WRITE(OUTLYNE,313) DBLE(I),FTFL01(14,I)
               IF(FTFL01(14,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(15,I).NE.0.0D0) WRITE(OUTLYNE,314) DBLE(I),FTFL01(15,I)
               IF(FTFL01(15,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(16,I).NE.0.0D0) WRITE(OUTLYNE,315) DBLE(I),FTFL01(16,I)
               IF(FTFL01(16,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(17,I).NE.0.0D0) WRITE(OUTLYNE,316) DBLE(I),FTFL01(17,I)
               IF(FTFL01(17,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(18,I).NE.0.0D0) WRITE(OUTLYNE,317) DBLE(I),FTFL01(18,I)
               IF(FTFL01(18,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(19,I).NE.0.0D0) WRITE(OUTLYNE,318) DBLE(I),FTFL01(19,I)
               IF(FTFL01(19,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(20,I).NE.0.0D0) WRITE(OUTLYNE,319) DBLE(I),FTFL01(20,I)
               IF(FTFL01(20,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(21,I).NE.0.0D0) WRITE(OUTLYNE,320) DBLE(I),FTFL01(21,I)
               IF(FTFL01(21,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(22,I).NE.0.0D0) WRITE(OUTLYNE,321) DBLE(I),FTFL01(22,I)
               IF(FTFL01(22,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(23,I).NE.0.0D0) WRITE(OUTLYNE,322) DBLE(I),FTFL01(23,I)
               IF(FTFL01(23,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(24,I).NE.0.0D0) WRITE(OUTLYNE,323) DBLE(I),FTFL01(24,I)
               IF(FTFL01(24,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(25,I).NE.0.0D0) WRITE(OUTLYNE,324) DBLE(I),FTFL01(25,I)
               IF(FTFL01(25,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(26,I).NE.0.0D0) WRITE(OUTLYNE,325) DBLE(I),FTFL01(26,I)
               IF(FTFL01(26,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(27,I).NE.0.0D0) WRITE(OUTLYNE,326) DBLE(I),FTFL01(27,I)
               IF(FTFL01(27,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(28,I).NE.0.0D0) WRITE(OUTLYNE,327) DBLE(I),FTFL01(28,I)
               IF(FTFL01(28,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(29,I).NE.0.0D0) WRITE(OUTLYNE,328) DBLE(I),FTFL01(29,I)
               IF(FTFL01(29,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(30,I).NE.0.0D0) WRITE(OUTLYNE,329) DBLE(I),FTFL01(30,I)
               IF(FTFL01(30,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(31,I).NE.0.0D0) WRITE(OUTLYNE,330) DBLE(I),FTFL01(31,I)
               IF(FTFL01(31,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(32,I).NE.0.0D0) WRITE(OUTLYNE,331) DBLE(I),FTFL01(32,I)
               IF(FTFL01(32,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(33,I).NE.0.0D0) WRITE(OUTLYNE,332) DBLE(I),FTFL01(33,I)
               IF(FTFL01(33,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(34,I).NE.0.0D0) WRITE(OUTLYNE,333) DBLE(I),FTFL01(34,I)
               IF(FTFL01(34,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(35,I).NE.0.0D0) WRITE(OUTLYNE,334) DBLE(I),FTFL01(35,I)
               IF(FTFL01(35,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(36,I).NE.0.0D0) WRITE(OUTLYNE,335) DBLE(I),FTFL01(36,I)
               IF(FTFL01(36,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(37,I).NE.0.0D0) WRITE(OUTLYNE,336) DBLE(I),FTFL01(37,I)
               IF(FTFL01(37,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(38,I).NE.0.0D0) WRITE(OUTLYNE,337) DBLE(I),FTFL01(38,I)
               IF(FTFL01(38,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(39,I).NE.0.0D0) WRITE(OUTLYNE,338) DBLE(I),FTFL01(39,I)
               IF(FTFL01(39,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(40,I).NE.0.0D0) WRITE(OUTLYNE,339) DBLE(I),FTFL01(40,I)
               IF(FTFL01(40,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(41,I).NE.0.0D0) WRITE(OUTLYNE,340) DBLE(I),FTFL01(41,I)
               IF(FTFL01(41,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(42,I).NE.0.0D0) WRITE(OUTLYNE,341) DBLE(I),FTFL01(42,I)
               IF(FTFL01(42,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(43,I).NE.0.0D0) WRITE(OUTLYNE,342) DBLE(I),FTFL01(43,I)
               IF(FTFL01(43,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(44,I).NE.0.0D0) WRITE(OUTLYNE,343) DBLE(I),FTFL01(44,I)
               IF(FTFL01(44,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(45,I).NE.0.0D0) WRITE(OUTLYNE,344) DBLE(I),FTFL01(45,I)
               IF(FTFL01(45,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(46,I).NE.0.0D0) WRITE(OUTLYNE,345) DBLE(I),FTFL01(46,I)
               IF(FTFL01(46,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(47,I).NE.0.0D0) WRITE(OUTLYNE,346) DBLE(I),FTFL01(47,I)
               IF(FTFL01(47,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(48,I).NE.0.0D0) WRITE(OUTLYNE,347) DBLE(I),FTFL01(48,I)
               IF(FTFL01(48,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(49,I).NE.0.0D0) WRITE(OUTLYNE,348) DBLE(I),FTFL01(49,I)
               IF(FTFL01(49,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(50,I).NE.0.0D0) WRITE(OUTLYNE,349) DBLE(I),FTFL01(50,I)
               IF(FTFL01(50,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(51,I).NE.0.0D0) WRITE(OUTLYNE,350) DBLE(I),FTFL01(51,I)
               IF(FTFL01(51,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(52,I).NE.0.0D0) WRITE(OUTLYNE,351) DBLE(I),FTFL01(52,I)
               IF(FTFL01(52,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(53,I).NE.0.0D0) WRITE(OUTLYNE,352) DBLE(I),FTFL01(53,I)
               IF(FTFL01(53,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(54,I).NE.0.0D0) WRITE(OUTLYNE,353) DBLE(I),FTFL01(54,I)
               IF(FTFL01(54,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(55,I).NE.0.0D0) WRITE(OUTLYNE,354) DBLE(I),FTFL01(55,I)
               IF(FTFL01(55,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(56,I).NE.0.0D0) WRITE(OUTLYNE,355) DBLE(I),FTFL01(56,I)
               IF(FTFL01(56,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(57,I).NE.0.0D0) WRITE(OUTLYNE,356) DBLE(I),FTFL01(57,I)
               IF(FTFL01(57,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(58,I).NE.0.0D0) WRITE(OUTLYNE,357) DBLE(I),FTFL01(58,I)
               IF(FTFL01(58,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(59,I).NE.0.0D0) WRITE(OUTLYNE,358) DBLE(I),FTFL01(59,I)
               IF(FTFL01(59,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(60,I).NE.0.0D0) WRITE(OUTLYNE,359) DBLE(I),FTFL01(60,I)
               IF(FTFL01(60,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(61,I).NE.0.0D0) WRITE(OUTLYNE,360) DBLE(I),FTFL01(61,I)
               IF(FTFL01(61,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(62,I).NE.0.0D0) WRITE(OUTLYNE,361) DBLE(I),FTFL01(62,I)
               IF(FTFL01(62,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(63,I).NE.0.0D0) WRITE(OUTLYNE,362) DBLE(I),FTFL01(63,I)
               IF(FTFL01(63,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(64,I).NE.0.0D0) WRITE(OUTLYNE,363) DBLE(I),FTFL01(64,I)
               IF(FTFL01(64,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(65,I).NE.0.0D0) WRITE(OUTLYNE,364) DBLE(I),FTFL01(65,I)
               IF(FTFL01(65,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(66,I).NE.0.0D0) WRITE(OUTLYNE,365) DBLE(I),FTFL01(66,I)
               IF(FTFL01(66,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(67,I).NE.0.0D0) WRITE(OUTLYNE,366) DBLE(I),FTFL01(67,I)
               IF(FTFL01(67,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(68,I).NE.0.0D0) WRITE(OUTLYNE,367) DBLE(I),FTFL01(68,I)
               IF(FTFL01(68,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(69,I).NE.0.0D0) WRITE(OUTLYNE,368) DBLE(I),FTFL01(69,I)
               IF(FTFL01(69,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(70,I).NE.0.0D0) WRITE(OUTLYNE,369) DBLE(I),FTFL01(70,I)
               IF(FTFL01(70,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(71,I).NE.0.0D0) WRITE(OUTLYNE,370) DBLE(I),FTFL01(71,I)
               IF(FTFL01(71,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(72,I).NE.0.0D0) WRITE(OUTLYNE,371) DBLE(I),FTFL01(72,I)
               IF(FTFL01(72,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(73,I).NE.0.0D0) WRITE(OUTLYNE,372) DBLE(I),FTFL01(73,I)
               IF(FTFL01(73,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(74,I).NE.0.0D0) WRITE(OUTLYNE,373) DBLE(I),FTFL01(74,I)
               IF(FTFL01(74,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(75,I).NE.0.0D0) WRITE(OUTLYNE,374) DBLE(I),FTFL01(75,I)
               IF(FTFL01(75,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(76,I).NE.0.0D0) WRITE(OUTLYNE,375) DBLE(I),FTFL01(76,I)
               IF(FTFL01(76,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(77,I).NE.0.0D0) WRITE(OUTLYNE,376) DBLE(I),FTFL01(77,I)
               IF(FTFL01(77,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(78,I).NE.0.0D0) WRITE(OUTLYNE,377) DBLE(I),FTFL01(78,I)
               IF(FTFL01(78,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(79,I).NE.0.0D0) WRITE(OUTLYNE,378) DBLE(I),FTFL01(79,I)
               IF(FTFL01(79,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(80,I).NE.0.0D0) WRITE(OUTLYNE,379) DBLE(I),FTFL01(80,I)
               IF(FTFL01(80,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(81,I).NE.0.0D0) WRITE(OUTLYNE,380) DBLE(I),FTFL01(81,I)
               IF(FTFL01(81,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(82,I).NE.0.0D0) WRITE(OUTLYNE,381) DBLE(I),FTFL01(82,I)
               IF(FTFL01(82,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(83,I).NE.0.0D0) WRITE(OUTLYNE,382) DBLE(I),FTFL01(83,I)
               IF(FTFL01(83,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(84,I).NE.0.0D0) WRITE(OUTLYNE,383) DBLE(I),FTFL01(84,I)
               IF(FTFL01(84,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(85,I).NE.0.0D0) WRITE(OUTLYNE,384) DBLE(I),FTFL01(85,I)
               IF(FTFL01(85,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(86,I).NE.0.0D0) WRITE(OUTLYNE,385) DBLE(I),FTFL01(86,I)
               IF(FTFL01(86,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(87,I).NE.0.0D0) WRITE(OUTLYNE,386) DBLE(I),FTFL01(87,I)
               IF(FTFL01(87,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(88,I).NE.0.0D0) WRITE(OUTLYNE,387) DBLE(I),FTFL01(88,I)
               IF(FTFL01(88,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(89,I).NE.0.0D0) WRITE(OUTLYNE,388) DBLE(I),FTFL01(89,I)
               IF(FTFL01(89,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(90,I).NE.0.0D0) WRITE(OUTLYNE,389) DBLE(I),FTFL01(90,I)
               IF(FTFL01(90,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(91,I).NE.0.0D0) WRITE(OUTLYNE,390) DBLE(I),FTFL01(91,I)
               IF(FTFL01(91,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(92,I).NE.0.0D0) WRITE(OUTLYNE,391) DBLE(I),FTFL01(92,I)
               IF(FTFL01(92,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(93,I).NE.0.0D0) WRITE(OUTLYNE,392) DBLE(I),FTFL01(93,I)
               IF(FTFL01(93,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(94,I).NE.0.0D0) WRITE(OUTLYNE,393) DBLE(I),FTFL01(94,I)
               IF(FTFL01(94,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(95,I).NE.0.0D0) WRITE(OUTLYNE,394) DBLE(I),FTFL01(95,I)
               IF(FTFL01(95,I).NE.0.0D0) CALL SHOWIT(10)
               IF(FTFL01(96,I).NE.0.0D0) WRITE(OUTLYNE,395) DBLE(I),FTFL01(96,I)
               IF(FTFL01(96,I).NE.0.0D0) CALL SHOWIT(10)
            END IF
         END IF
21    CONTINUE
      WRITE(OUTLYNE,10)
      CALL SHOWIT(10)
   END IF
300 FORMAT('C1      ,',G23.15,',',G23.15)
301 FORMAT('C2      ,',G23.15,',',G23.15)
302 FORMAT('C3      ,',G23.15,',',G23.15)
303 FORMAT('C4      ,',G23.15,',',G23.15)
304 FORMAT('C5      ,',G23.15,',',G23.15)
305 FORMAT('C6      ,',G23.15,',',G23.15)
306 FORMAT('C7      ,',G23.15,',',G23.15)
307 FORMAT('C8      ,',G23.15,',',G23.15)
308 FORMAT('C9      ,',G23.15,',',G23.15)
309 FORMAT('C10     ,',G23.15,',',G23.15)
310 FORMAT('C11     ,',G23.15,',',G23.15)
311 FORMAT('C12     ,',G23.15,',',G23.15)
312 FORMAT('C13     ,',G23.15,',',G23.15)
313 FORMAT('C14     ,',G23.15,',',G23.15)
314 FORMAT('C15     ,',G23.15,',',G23.15)
315 FORMAT('C16     ,',G23.15,',',G23.15)
316 FORMAT('C17     ,',G23.15,',',G23.15)
317 FORMAT('C18     ,',G23.15,',',G23.15)
318 FORMAT('C19     ,',G23.15,',',G23.15)
319 FORMAT('C20     ,',G23.15,',',G23.15)
320 FORMAT('C21     ,',G23.15,',',G23.15)
321 FORMAT('C22     ,',G23.15,',',G23.15)
322 FORMAT('C23     ,',G23.15,',',G23.15)
323 FORMAT('C24     ,',G23.15,',',G23.15)
324 FORMAT('C25     ,',G23.15,',',G23.15)
325 FORMAT('C26     ,',G23.15,',',G23.15)
326 FORMAT('C27     ,',G23.15,',',G23.15)
327 FORMAT('C28     ,',G23.15,',',G23.15)
328 FORMAT('C29     ,',G23.15,',',G23.15)
329 FORMAT('C30     ,',G23.15,',',G23.15)
330 FORMAT('C31     ,',G23.15,',',G23.15)
331 FORMAT('C32     ,',G23.15,',',G23.15)
332 FORMAT('C33     ,',G23.15,',',G23.15)
333 FORMAT('C34     ,',G23.15,',',G23.15)
334 FORMAT('C35     ,',G23.15,',',G23.15)
335 FORMAT('C36     ,',G23.15,',',G23.15)
336 FORMAT('C37     ,',G23.15,',',G23.15)
337 FORMAT('C38     ,',G23.15,',',G23.15)
338 FORMAT('C39     ,',G23.15,',',G23.15)
339 FORMAT('C40     ,',G23.15,',',G23.15)
340 FORMAT('C41     ,',G23.15,',',G23.15)
341 FORMAT('C42     ,',G23.15,',',G23.15)
342 FORMAT('C43     ,',G23.15,',',G23.15)
343 FORMAT('C44     ,',G23.15,',',G23.15)
344 FORMAT('C45     ,',G23.15,',',G23.15)
345 FORMAT('C46     ,',G23.15,',',G23.15)
346 FORMAT('C47     ,',G23.15,',',G23.15)
347 FORMAT('C48     ,',G23.15,',',G23.15)
348 FORMAT('C49     ,',G23.15,',',G23.15)
349 FORMAT('C50     ,',G23.15,',',G23.15)
350 FORMAT('C51     ,',G23.15,',',G23.15)
351 FORMAT('C52     ,',G23.15,',',G23.15)
352 FORMAT('C53     ,',G23.15,',',G23.15)
353 FORMAT('C54     ,',G23.15,',',G23.15)
354 FORMAT('C55     ,',G23.15,',',G23.15)
355 FORMAT('C56     ,',G23.15,',',G23.15)
356 FORMAT('C57     ,',G23.15,',',G23.15)
357 FORMAT('C58     ,',G23.15,',',G23.15)
358 FORMAT('C59     ,',G23.15,',',G23.15)
359 FORMAT('C60     ,',G23.15,',',G23.15)
360 FORMAT('C61     ,',G23.15,',',G23.15)
361 FORMAT('C62     ,',G23.15,',',G23.15)
362 FORMAT('C63     ,',G23.15,',',G23.15)
363 FORMAT('C64     ,',G23.15,',',G23.15)
364 FORMAT('C65     ,',G23.15,',',G23.15)
365 FORMAT('C66     ,',G23.15,',',G23.15)
366 FORMAT('C67     ,',G23.15,',',G23.15)
367 FORMAT('C68     ,',G23.15,',',G23.15)
368 FORMAT('C69     ,',G23.15,',',G23.15)
369 FORMAT('C70     ,',G23.15,',',G23.15)
370 FORMAT('C71     ,',G23.15,',',G23.15)
371 FORMAT('C72     ,',G23.15,',',G23.15)
372 FORMAT('C73     ,',G23.15,',',G23.15)
373 FORMAT('C74     ,',G23.15,',',G23.15)
374 FORMAT('C75     ,',G23.15,',',G23.15)
375 FORMAT('C76     ,',G23.15,',',G23.15)
376 FORMAT('C77     ,',G23.15,',',G23.15)
377 FORMAT('C78     ,',G23.15,',',G23.15)
378 FORMAT('C79     ,',G23.15,',',G23.15)
379 FORMAT('C80     ,',G23.15,',',G23.15)
380 FORMAT('C81     ,',G23.15,',',G23.15)
381 FORMAT('C82     ,',G23.15,',',G23.15)
382 FORMAT('C83     ,',G23.15,',',G23.15)
383 FORMAT('C84     ,',G23.15,',',G23.15)
384 FORMAT('C85     ,',G23.15,',',G23.15)
385 FORMAT('C86     ,',G23.15,',',G23.15)
386 FORMAT('C87     ,',G23.15,',',G23.15)
387 FORMAT('C88     ,',G23.15,',',G23.15)
388 FORMAT('C89     ,',G23.15,',',G23.15)
389 FORMAT('C90     ,',G23.15,',',G23.15)
390 FORMAT('C91     ,',G23.15,',',G23.15)
391 FORMAT('C92     ,',G23.15,',',G23.15)
392 FORMAT('C93     ,',G23.15,',',G23.15)
393 FORMAT('C94     ,',G23.15,',',G23.15)
394 FORMAT('C95     ,',G23.15,',',G23.15)
395 FORMAT('C96     ,',G23.15,',',G23.15)
!
   RETURN
END
! SUB DOGTAG.FOR
SUBROUTINE DOGTAG
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_last_surf, sys_mode, sys_units, sys_wl_pri1, sys_wl_pri2, sys_wl_ref, sys_wl_sec1
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE DOGTAG WHICH CREATES THE IDTAG FOR A LENS
!       THIS CAN'T BE ISSUED FROM THE KEYBOARD. THE COMMAND ONLY
!       WORKS FROM THE TASK LEVEL WHEN INPUT IS NOT 5
!
   LOGICAL OPEN9
   CHARACTER A3*3,A23*23,NM1*13,NM2*13,NM3*13,NM4*13
   INTEGER OLDOUT,K,L,M,N,I
!
!
   IDTAG(1)(1:75)=CNULL
   IDTAG(2)(1:75)=CNULL
   IDTAG(3)(1:75)=CNULL
   IDTAG(4)(1:75)=CNULL
   IDTAG(5)(1:75)=CNULL
   IDTAG(6)(1:75)=CNULL
   IDTAG(7)(1:75)=CNULL
   IDTAG(8)(1:75)=CNULL
   IDTAG(9)(1:75)=CNULL
   IDTAG(10)(1:75)=CNULL
!     NOW COMPUTE THE NEW IDTAG
!
!       LINE ONE IS THE FIRST 75 CHARACTERS OF THE LI
   IDTAG(1)(1:75)=LI(1:75)
!       LINE TWO IS THE FIRST 75 CHARACTERS OF THE MFG
   IDTAG(2)(1:75)=MFG(1:75)
!       LINE THREE IS THE FIRST 75 CHARACTERS OF THE CATNUM
   IDTAG(3)(1:75)=CATNUM(1:75)
!       LINE FOUR IS THE EFL IN G23.15 FORMAT FOLLOWED BY THE NUMBER OF
!       SURFACES NOT COUNTING 0 AND LAST SURFACE IN I3 FORMAT FOLLOWED BY
!       THE OVERALL LENGTH FROM SURFACE 1 TO SURFACE LAST SURF -1 IN G23.15 FORMAT.
   SAVE_KDP(25)=SAVEINPT(25)
   INPUT='FIRD QUIET'
   CALL PROCES
   WRITE(OUTLYNE,*) 'GET OAL 1',INT(sys_last_surf())-1
   INPUT(1:75)=OUTLYNE(1:75)
   CALL PROCES
   REST_KDP(25)=RESTINPT(25)
   WRITE(IDTAG(4),100) GPREG(1),INT(sys_last_surf()),REG(9)
100 FORMAT(G23.15,I3,G23.15)
!       LINE FIVE IS THE MODE CODE 1=FOCAL,2=UFOCAL,3=AFOCAL, 4=UAFOCAL IN I1 FOLLOWED BY
!       THE UNITS CODE 1=IN, 2= CM, 3= MM, 4=METERS IN I1 FORMAT FOLLOWED BY THE CONTROL
!       WAVELENGTH IN G23.15,FOLLOWED BY THE PRIMARY WAVELENGTH PAIR EACH IN G23.15 FORMAT
   WRITE(IDTAG(5),101) INT(sys_mode()),INT(sys_units()),SYSTEM(INT(sys_wl_ref())+110),SYSTEM(INT(sys_wl_pri1())+110),SYSTEM(INT(sys_wl_pri2())+110)
101 FORMAT(I1,I1,G23.15,G23.15,G23.15)
!       LINE SIX IS SECONDARY WAVELENGTH PAIR WAVELENGTHS IN G23.15 FORMAT (EACH)
   WRITE(IDTAG(6),102) SYSTEM(INT(sys_wl_sec1())+110),SYSTEM(INT(sys_wl_pri2())+110)
102 FORMAT(G23.15,G23.15)
!       LINE SEVEN FULL NAME OF THE FIRST NON-AIR, NON-REFLECTIVE GLASS
!       FOLLOWED BY THE SECOND FULL GLASS NAME EACH IN 2(A13) FORMAT
   K=0
   L=0
   M=0
   N=0
   DO I=0,INT(sys_last_surf())
      IF(GLANAM(I,1).EQ.'SCHOTT'.OR.GLANAM(I,1).EQ.'HOYA'.OR.GLANAM(I,1).EQ.'CORNIN'.OR.GLANAM(I,1).EQ.'CHANCE'.OR.GLANAM(I,1).EQ.'SCH2000'.OR.GLANAM(I,1).EQ.'HIKARI'.OR.GLANAM(I,1).EQ.'GLCAT'.OR.GLANAM(I,1).EQ.'MATL'.OR.GLANAM(I,1).EQ.'RUSSIAN'.OR.GLANAM(I,1).EQ.'GLAK'.OR.GLANAM(I,1).EQ.'RADHARD'.OR.GLANAM(I,1).EQ.'USER'.OR.GLANAM(I,1).EQ.'OHARA') THEN
         IF(GLANAM(I,2).NE.'AIR'.AND.GLANAM(I,2).NE.'REFL'.AND.GLANAM(I,2).NE.'REFLTIRO') THEN
            IF(N.EQ.0) THEN
               IF(M.EQ.0) THEN
                  IF(L.EQ.0) THEN
                     IF(K.EQ.0) THEN
                        K=I
                        GO TO 10
                     END IF
                     L=I
                     GO TO 10
                  END IF
                  M=I
                  GO TO 10
               END IF
               N=I
               GO TO 10
            END IF

         END IF

      END IF
10    CONTINUE
   END DO
   NM1=GLANAM(K,1)
   NM2=GLANAM(K,2)
   NM3=GLANAM(L,1)
   NM4=GLANAM(L,2)
   IF(NM1.EQ.'AIR') NM1='             '
   IF(NM2.EQ.'AIR') NM2='             '
   IF(NM3.EQ.'AIR') NM3='             '
   IF(NM4.EQ.'AIR') NM4='             '
   WRITE(IDTAG(7),103) NM1,NM2,NM3,NM4

103 FORMAT(A13,A13,A13,A13)
!       LINE EIGHT FULL NAME OF THE THIRD NON-AIR, NON-REFLECTIVE GLASS
!       FOLLOWED BY THE FOURTH FULL GLASS NAME EACH IN 2(A13) FORMAT
   NM1=GLANAM(M,1)
   NM2=GLANAM(M,2)
   NM3=GLANAM(N,1)
   NM4=GLANAM(N,2)
   IF(NM1.EQ.'AIR') NM1='             '
   IF(NM2.EQ.'AIR') NM2='             '
   IF(NM3.EQ.'AIR') NM3='             '
   IF(NM4.EQ.'AIR') NM4='             '
   WRITE(IDTAG(8),103) NM1,NM2,NM3,NM4
!
!       WRITE(OUTLYNE,*)'1',IDTAG(1)
!       CALL SHOWIT(1)
!       WRITE(OUTLYNE,*)'2',IDTAG(2)
!       CALL SHOWIT(1)
!       WRITE(OUTLYNE,*)'3',IDTAG(3)
!       CALL SHOWIT(1)
!       WRITE(OUTLYNE,*)'4',IDTAG(4)
!       CALL SHOWIT(1)
!       WRITE(OUTLYNE,*)'5',IDTAG(5)
!       CALL SHOWIT(1)
!       WRITE(OUTLYNE,*)'6',IDTAG(6)
!       CALL SHOWIT(1)
!       WRITE(OUTLYNE,*)'7',IDTAG(7)
!       CALL SHOWIT(1)
!       WRITE(OUTLYNE,*)'8',IDTAG(8)
!       CALL SHOWIT(1)
!       WRITE(OUTLYNE,*)'9',IDTAG(9)
!       CALL SHOWIT(1)
!       WRITE(OUTLYNE,*)'10',IDTAG(10)
!       CALL SHOWIT(1)
   RETURN
END
! SUB RLENHD.FOR
SUBROUTINE RLENHD
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_autofunc, sys_bdx, sys_bdy, sys_last_surf, sys_units, &
      & sys_verbose_optim, sys_wavelength, sys_wl_pri1, sys_wl_pri2, sys_wl_ref, &
      & sys_wl_sec1, sys_wl_sec2, sys_wrx, sys_wry, sys_set_bdx, sys_set_bdy
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       SUBROUTINE LENHD HANDEL LENS HEADER INFO OUTPUT
!       DURING A LENO REVERSE. ACTS ON THE CURRENT LENS
!
   INTEGER I
!
!
!     OVERBOSE
   IF(sys_verbose_optim().EQ.1.0D0)WRITE(OUTLYNE,*)'OVERBOSE ON'
!
!       OUTPUT THE HEADER COMMAND (LENS)
   WRITE(OUTLYNE,10)
   CALL SHOWIT(10)
!
!       NOW THE LI
   IF(LI(1:20).NE.'                    ')WRITE(OUTLYNE,20) LI(1:79)
   IF(LI(1:20).NE.'                    ') CALL SHOWIT(10)
!
!       NOW LIC
!
   DO I=1,LICCNT
      IF(LIC(I)(1:20).NE.'                    ') THEN
         WRITE(OUTLYNE,21) LIC(I)
         CALL SHOWIT(10)
      END IF
   END DO
!
   IF(INNI(1:20).NE.'                    ') WRITE(OUTLYNE,121) INNI
   IF(INNI(1:20).NE.'                    ') CALL SHOWIT(10)
   IF(LLTYPE(1:5).NE.'     ') WRITE(OUTLYNE,1212) LLTYPE(1:5)
   IF(LLTYPE(1:5).NE.'     ') CALL SHOWIT(10)
!
   IF(INT(sys_autofunc()).NE.0) THEN
!       NOW AUTOFUNC
      WRITE(OUTLYNE,50) DBLE(INT(sys_autofunc()))
50    FORMAT('AUTOFUNC,',G23.15)
      CALL SHOWIT(10)
   END IF
!
!       NOW WV
!
   WRITE(OUTLYNE,22)sys_wavelength(1),sys_wavelength(2),sys_wavelength(3),sys_wavelength(4),sys_wavelength(5)
   CALL SHOWIT(10)
!
!       NOW WV2
!
   WRITE(OUTLYNE,221)sys_wavelength(6),sys_wavelength(7),sys_wavelength(8),sys_wavelength(9),sys_wavelength(10)
   CALL SHOWIT(10)
!
6661 FORMAT('WV      ,',G23.15,',',G23.15)
6662 FORMAT('WV2     ,',G23.15,',',G23.15)
6663 FORMAT(G23.15,',',G23.15,',',G23.15)
!
!       NOW UNITS
   IF(sys_units().EQ.1.0) WRITE(OUTLYNE,23)
   IF(sys_units().EQ.2.0) WRITE(OUTLYNE,24)
   IF(sys_units().EQ.3.0) WRITE(OUTLYNE,25)
   IF(sys_units().EQ.4.0) WRITE(OUTLYNE,33)
   CALL SHOWIT(10)
!
!       NOW PRIMARY WAVELENGTH PAIR
   WRITE(OUTLYNE,26) INT(sys_wl_pri1()),INT(sys_wl_pri2())
   CALL SHOWIT(10)
!
!       SECONDARY WAVELENGTH PAIR
   WRITE(OUTLYNE,27) INT(sys_wl_sec1()),INT(sys_wl_sec2())
   CALL SHOWIT(10)
!
!       CONTROL WAVELENGTH
   WRITE(OUTLYNE,28) INT(sys_wl_ref())
   CALL SHOWIT(10)
!
!       WRX
   WRITE(OUTLYNE,2992) sys_wrx()
   CALL SHOWIT(10)
2992 FORMAT('WRX     ,',G23.15)
!
!       WRY
   WRITE(OUTLYNE,2993) sys_wry()
   CALL SHOWIT(10)
2993 FORMAT('WRY     ,',G23.15)
!
!       BDX
   IF(sys_bdx().EQ.0.0D0) call sys_set_bdx(0.001D0)
   WRITE(OUTLYNE,2994) sys_bdx()
   CALL SHOWIT(10)
2994 FORMAT('BDX     ,',G23.15)
!
!
!       BDY
   IF(sys_bdy().EQ.0.0D0) call sys_set_bdy(0.001D0)
   WRITE(OUTLYNE,2995) sys_bdy()
   CALL SHOWIT(10)
2995 FORMAT('BDY     ,',G23.15)
!
!       SAY, THIS IS THE PY VALUE AT SURFACE sys_last_surf()-1
   WRITE(OUTLYNE,29) PXTRAY(1,(INT(sys_last_surf()-1.0D0)))
   CALL SHOWIT(10)
!
!       SAX, THIS IS THE PX VALUE AT SURFACE sys_last_surf()-1
   WRITE(OUTLYNE,30) PXTRAX(1,(INT(sys_last_surf()-1.0D0)))
   CALL SHOWIT(10)
!
!       SCY (SCY FANG IS NOT DIRECTLY PASSED BY LENO)
!       THIS IS THE PCY
   WRITE(OUTLYNE,31) PXTRAY(5,INT(sys_last_surf()))
   CALL SHOWIT(10)
!
   IF(PXTRAY(5,INT(sys_last_surf())).NE.PXTRAX(5,INT(sys_last_surf()))) THEN
!     DO SCX
!       SCX (SCX FANG IS NOT DIRECTLY PASSED BY LENO)
      WRITE(OUTLYNE,32) PXTRAX(5,INT(sys_last_surf()))
      CALL SHOWIT(10)
   END IF
   RETURN
!
10 FORMAT('LENS')
20 FORMAT('LI,',A79)
21 FORMAT('LIC,',A79)
121 FORMAT('INI,',A79)
1212 FORMAT('LTYPE,',A5)
22 FORMAT('WV      ,',G23.15,',',G23.15,',',G23.15,',',G23.15 ,',',G23.15)
221 FORMAT('WV2     ,',G23.15,',',G23.15,',',G23.15,',',G23.15 ,',',G23.15)
23 FORMAT('UNITS    IN')
24 FORMAT('UNITS    CM')
25 FORMAT('UNITS    MM')
33 FORMAT('UNITS    M ')
26 FORMAT('PCW     ,',I2,',',I2)
27 FORMAT('SCW     ,',I2,',',I2)
28 FORMAT('CW      ,',I2)
29 FORMAT('SAY     ,',G23.15)
30 FORMAT('SAX     ,',G23.15)
31 FORMAT('SCY     ,',G23.15)
32 FORMAT('SCX     ,',G23.15)
END
! SUB LENHDCV.FOR
SUBROUTINE LENHDCV
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_pxim_fang_set, sys_pyim_fang_set, sys_rxim_fang_set, &
      & sys_ryim_fang_set, sys_say, sys_scy, sys_scy_fang, sys_scy_fang_set, &
      & sys_units, sys_wavelength, sys_wl_ref, sys_wl_weight
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       SUBROUTINE LENHD HANDEL LENS HEADER INFO OUTPUT
!       DURING A LENO CV. ACTS ON THE CURRENT LENS
!
   CHARACTER LIO*80
!
   INTEGER I,IV
!
   real(real64) VW(5)
!
!
!       OUTPUT THE HEADER COMMAND (LENS)
!
   WRITE(OUTLYNE,10)
   CALL SHOWIT(10)
   WRITE(OUTLYNE,11)
   CALL SHOWIT(10)
!
!
!       NOW THE LI
   LIO='TIT '//''''//LI(1:73)//''''
!
   IF(LI(1:20).NE.'                    ')WRITE(OUTLYNE,20) LIO(1:79)
   IF(LI(1:20).NE.'                    ') CALL SHOWIT(10)
!
!       NOW WV
!
   IV=0
   IF(sys_wavelength(1).NE.0.0D0) THEN
      IV=IV+1
      VW(IV)=sys_wavelength(1)*1000.0D0
   END IF
   IF(sys_wavelength(2).NE.0.0D0) THEN
      IV=IV+1
      VW(IV)=sys_wavelength(2)*1000.0D0
   END IF
   IF(sys_wavelength(3).NE.0.0D0) THEN
      IV=IV+1
      VW(IV)=sys_wavelength(3)*1000.0D0
   END IF
   IF(sys_wavelength(4).NE.0.0D0) THEN
      IV=IV+1
      VW(IV)=sys_wavelength(4)*1000.0D0
   END IF
   IF(sys_wavelength(5).NE.0.0D0) THEN
      IV=IV+1
      VW(IV)=sys_wavelength(5)*1000.0D0
   END IF
   IF(IV.EQ.1) THEN
      WRITE(OUTLYNE,41) VW(1)
   END IF
   IF(IV.EQ.2) THEN
      WRITE(OUTLYNE,42) VW(1),VW(2)
   END IF
   IF(IV.EQ.3) THEN
      WRITE(OUTLYNE,43) VW(1),VW(2),VW(3)
   END IF
   IF(IV.EQ.4) THEN
      WRITE(OUTLYNE,44) VW(1),VW(2),VW(3),VW(4)
   END IF
   IF(IV.EQ.5) THEN
      WRITE(OUTLYNE,45) VW(1),VW(2),VW(3),VW(4),VW(5)
   END IF
   CALL SHOWIT(10)
41 FORMAT('WL ',G15.7)
42 FORMAT('WL ',G15.7,1X,G15.7)
43 FORMAT('WL ',G15.7,1X,G15.7,1X,G15.7)
44 FORMAT('WL ',G15.7,1X,G15.7,1X,G15.7,1X,G15.7)
45 FORMAT('WL ',G15.7,1X,G15.7,1X,G15.7,1X,G15.7,1X,G15.7)
!
!       NOW SPTWT
!
   IV=0
   IF(sys_wl_weight(1).NE.0.0D0) THEN
      IV=IV+1
      VW(IV)=sys_wl_weight(1)*100.0D0
   END IF
   IF(sys_wl_weight(2).NE.0.0D0) THEN
      IV=IV+1
      VW(IV)=sys_wl_weight(2)*100.0D0
   END IF
   IF(sys_wl_weight(3).NE.0.0D0) THEN
      IV=IV+1
      VW(IV)=sys_wl_weight(3)*100.0D0
   END IF
   IF(sys_wl_weight(4).NE.0.0D0) THEN
      IV=IV+1
      VW(IV)=sys_wl_weight(4)*100.0D0
   END IF
   IF(sys_wl_weight(5).NE.0.0D0) THEN
      IV=IV+1
      VW(IV)=sys_wl_weight(5)*100.0D0
   END IF
   IF(IV.EQ.1) THEN
      WRITE(OUTLYNE,61) VW(1)
   END IF
   IF(IV.EQ.2) THEN
      WRITE(OUTLYNE,62) VW(1),VW(2)
   END IF
   IF(IV.EQ.3) THEN
      WRITE(OUTLYNE,63) VW(1),VW(2),VW(3)
   END IF
   IF(IV.EQ.4) THEN
      WRITE(OUTLYNE,64) VW(1),VW(2),VW(3),VW(4)
   END IF
   IF(IV.EQ.5) THEN
      WRITE(OUTLYNE,65) VW(1),VW(2),VW(3),VW(4),VW(5)
   END IF
   CALL SHOWIT(10)
61 FORMAT('WTW ',G15.7)
62 FORMAT('WTW ',G15.7,1X,G15.7)
63 FORMAT('WTW ',G15.7,1X,G15.7,1X,G15.7)
64 FORMAT('WTW ',G15.7,1X,G15.7,1X,G15.7,1X,G15.7)
65 FORMAT('WTW ',G15.7,1X,G15.7,1X,G15.7,1X,G15.7,1X,G15.7)
!
!       NOW UNITS
   IF(sys_units().EQ.1.0) WRITE(OUTLYNE,23)
   IF(sys_units().EQ.2.0) WRITE(OUTLYNE,24)
   IF(sys_units().EQ.3.0) WRITE(OUTLYNE,25)
   CALL SHOWIT(10)
!
!       CONTROL WAVELENGTH
   WRITE(OUTLYNE,28) INT(sys_wl_ref())
   CALL SHOWIT(10)
!
!       SAY

   WRITE(OUTLYNE,29) DABS(2.0D0*sys_say())
   CALL SHOWIT(10)
!
   IF(sys_pxim_fang_set().EQ.0.0D0.AND.sys_pyim_fang_set().EQ.0.0D0.AND.sys_rxim_fang_set().EQ.0.0D0.AND.sys_ryim_fang_set().EQ.0.0D0) THEN
!     NOT IMAGE SPACE FIELD SPEC
      IF(sys_scy_fang_set().NE.1.0D0) THEN
!       SCY (SCY FANG IS NOT DIRECTLY PASSED BY LENO)
         WRITE(OUTLYNE,31) (0.0D0*sys_scy()),(0.7D0*sys_scy()),sys_scy()
         CALL SHOWIT(10)
      ELSE
!     SCY FANG
         WRITE(OUTLYNE,32) (0.0D0*sys_scy_fang()),(0.7D0*sys_scy_fang()),sys_scy_fang()
         CALL SHOWIT(10)
      END IF
   ELSE
!     IMAGE SPACE SPECIFICATION
!     NOT DONE YET
   END IF
!
10 FORMAT('LENS')
11 FORMAT('RDM')
20 FORMAT(A79)
22 FORMAT('WL      ,',D15.7,',',D15.7,',',D15.7,',',D15.7 ,',',D15.7)
23 FORMAT('DIM I')
24 FORMAT('DIM C')
25 FORMAT('DIM M')
28 FORMAT('REF ',G23.15)
29 FORMAT('EPD ',G23.15)
31 FORMAT('YOB ',G23.15,1X,G23.15,1X,G23.15)
32 FORMAT('YAN ',G23.15,1X,G23.15,1X,G23.15)
!
   RETURN
END
! SUB LENHD.FOR
SUBROUTINE LENHD
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_autofunc, sys_fno_val_set, sys_fno_val_x, sys_fno_val_y, &
      & sys_na_set, sys_naox, sys_naoy, sys_pxim, sys_pxim_fang_set, &
      & sys_pyim, sys_pyim_fang_set, sys_reverse_trace, sys_rxim, sys_rxim_fang_set, &
      & sys_ryim, sys_ryim_fang_set, sys_sax, sys_sax_float, sys_say, sys_say_float, &
      & sys_scx, sys_scx_fang, sys_scx_fang_set, sys_scy, sys_scy_fang, &
      & sys_scy_fang_set, sys_units, sys_verbose_optim, sys_wavelength, &
      & sys_wl_pri1, sys_wl_pri2, sys_wl_ref, sys_wl_sec1, sys_wl_sec2, sys_wrx, sys_wry, &
      & sys_x1_scx, sys_x1_scx_fang, sys_y1_scy, sys_y1_scy_fang, &
      & sys_bdx, sys_bdy, sys_set_bdx, sys_set_bdy
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       SUBROUTINE LENHD HANDEL LENS HEADER INFO OUTPUT
!       DURING A SIMPLE LENO. ACTS ON THE CURRENT LENS
!
   INTEGER I
!
!
!     OVERBOSE
   IF(sys_verbose_optim().EQ.1.0D0)WRITE(OUTLYNE,*)'OVERBOSE ON'
!
!       OUTPUT THE HEADER COMMAND (LENS)
!
   WRITE(OUTLYNE,10)
   CALL SHOWIT(10)
!
!
!       NOW THE LI
!
   IF(LI(1:20).NE.'                    ')WRITE(OUTLYNE,20) LI(1:79)
   IF(LI(1:20).NE.'                    ') CALL SHOWIT(10)
!
!       NOW LIC
!
   DO I=1,LICCNT
      IF(LIC(I)(1:20).NE.'                    ') THEN
         WRITE(OUTLYNE,21) LIC(I)
         CALL SHOWIT(10)
      END IF
   END DO
!
   IF(MFG(1:20).NE.'                    ') THEN
!     MFG
      WRITE(OUTLYNE,8768) MFG
8768  FORMAT('MFG,',A75)
      CALL SHOWIT(10)
!     CATNUM
      DO I=2,80
         IF(LI(I:I).EQ.' ') THEN
            CATNUM=LI(1:I-1)
            GO TO 8767
         END IF
      END DO
8767  CONTINUE
      WRITE(OUTLYNE,8769) CATNUM
8769  FORMAT('CATNUM,',A75)
      CALL SHOWIT(10)
   END IF
   IF(INNI(1:20).NE.'                    ') WRITE(OUTLYNE,121) INNI
   IF(INNI(1:20).NE.'                    ') CALL SHOWIT(10)
   IF(LLTYPE(1:5).NE.'     ') WRITE(OUTLYNE,1212) LLTYPE(1:5)
   IF(LLTYPE(1:5).NE.'     ') CALL SHOWIT(10)
!
   IF(INT(sys_autofunc()).NE.0) THEN
!       NOW AUTOFUNC
!
      WRITE(OUTLYNE,50) DBLE(INT(sys_autofunc()))
50    FORMAT('AUTOFUNC,',G23.15)
      CALL SHOWIT(10)
   END IF
!
!
!       NOW WV
!
   WRITE(OUTLYNE,22)sys_wavelength(1),sys_wavelength(2),sys_wavelength(3),sys_wavelength(4),sys_wavelength(5)
   CALL SHOWIT(10)
!
!       NOW WV2
!
   WRITE(OUTLYNE,221)sys_wavelength(6),sys_wavelength(7),sys_wavelength(8),sys_wavelength(9),sys_wavelength(10)
   CALL SHOWIT(10)
!
!       NOW UNITS
   IF(sys_units().EQ.1.0) WRITE(OUTLYNE,23)
   IF(sys_units().EQ.2.0) WRITE(OUTLYNE,24)
   IF(sys_units().EQ.3.0) WRITE(OUTLYNE,25)
   IF(sys_units().EQ.4.0) WRITE(OUTLYNE,33)
   CALL SHOWIT(10)
!
!       NOW PRIMARY WAVELENGTH PAIR
   WRITE(OUTLYNE,26) INT(sys_wl_pri1()),INT(sys_wl_pri2())
   CALL SHOWIT(10)
!
!       SECONDARY WAVELENGTH PAIR
   WRITE(OUTLYNE,27) INT(sys_wl_sec1()),INT(sys_wl_sec2())
   CALL SHOWIT(10)
!
!       CONTROL WAVELENGTH
   WRITE(OUTLYNE,28) INT(sys_wl_ref())
   CALL SHOWIT(10)
!
!       WRX
   WRITE(OUTLYNE,2992) sys_wrx()
   CALL SHOWIT(10)
2992 FORMAT('WRX     ,',G23.15)
!
!
!       WRY
   WRITE(OUTLYNE,2993) sys_wry()
   CALL SHOWIT(10)
2993 FORMAT('WRY     ,',G23.15)
!
!       BDX
   IF(sys_bdx().EQ.0.0D0) call sys_set_bdx(0.001D0)
   WRITE(OUTLYNE,2994) sys_bdx()
   CALL SHOWIT(10)
2994 FORMAT('BDX     ,',G23.15)
!
!
!       BDY
   IF(sys_bdy().EQ.0.0D0) call sys_set_bdy(0.001D0)
   WRITE(OUTLYNE,2995) sys_bdy()
   CALL SHOWIT(10)
2995 FORMAT('BDY     ,',G23.15)
!
!
!       SAY
   IF(sys_na_set().EQ.0.0D0.AND.sys_fno_val_set().EQ.0.0D0) THEN
      IF(sys_say_float().EQ.0.0D0) WRITE(OUTLYNE,29) sys_say()
      IF(sys_say_float().NE.0.0D0) WRITE(OUTLYNE,292)
      CALL SHOWIT(10)
   ELSE
      IF(sys_na_set().EQ.1.0D0.OR.sys_na_set().EQ.3.0D0)WRITE(OUTLYNE,2929) sys_naoy()
      IF(sys_na_set().EQ.1.0D0.OR.sys_na_set().EQ.3.0D0)CALL SHOWIT(10)
      IF(sys_fno_val_set().EQ.1.0D0.OR.sys_fno_val_set().EQ.3.0D0)WRITE(OUTLYNE,2930) sys_fno_val_y()
      IF(sys_fno_val_set().EQ.1.0D0.OR.sys_fno_val_set().EQ.3.0D0)CALL SHOWIT(10)
   END IF
!
!       SAX
   IF(sys_na_set().EQ.0.0D0.AND.sys_fno_val_set().EQ.0.0D0) THEN
      IF(sys_sax().NE.sys_say()) THEN
         IF(sys_sax_float().EQ.0.0D0) WRITE(OUTLYNE,30) sys_sax()
         IF(sys_sax_float().EQ.0.0D0) CALL SHOWIT(10)
         IF(sys_sax_float().NE.0.0D0) WRITE(OUTLYNE,3022)
         IF(sys_sax_float().NE.0.0D0) CALL SHOWIT(10)
      END IF
   ELSE
      IF(sys_na_set().EQ.2.0D0.OR.sys_na_set().EQ.3.0D0)WRITE(OUTLYNE,3030) sys_naox()
      IF(sys_na_set().EQ.2.0D0.OR.sys_na_set().EQ.3.0D0)CALL SHOWIT(10)
      IF(sys_fno_val_set().EQ.2.0D0.OR.sys_fno_val_set().EQ.3.0D0)WRITE(OUTLYNE,3031) sys_fno_val_x()
      IF(sys_fno_val_set().EQ.2.0D0.OR.sys_fno_val_set().EQ.3.0D0)CALL SHOWIT(10)
   END IF
!
   IF(sys_pxim_fang_set().EQ.0.0D0.AND.sys_pyim_fang_set().EQ.0.0D0.AND.sys_rxim_fang_set().EQ.0.0D0.AND.sys_ryim_fang_set().EQ.0.0D0) THEN
      IF(sys_scy_fang_set().NE.1.0D0) THEN
!       SCY (SCY FANG IS NOT DIRECTLY PASSED BY LENO)
         WRITE(OUTLYNE,31) sys_scy(),sys_y1_scy()
         CALL SHOWIT(10)
      ELSE
!     SCY FANG
         WRITE(OUTLYNE,1319) sys_scy_fang(),sys_y1_scy_fang()
1319     FORMAT('SCY FANG,',G23.15,',',G23.15)
         CALL SHOWIT(10)
      END IF
!
      IF(sys_scx_fang_set().NE.1.0D0) THEN
         IF(sys_scy().NE.sys_scx().OR.sys_y1_scy().NE.sys_x1_scx()) THEN
!     DO SCX
!       SCX (SCX FANG IS NOT DIRECTLY PASSED BY LENO)
            WRITE(OUTLYNE,32) sys_scx(),sys_x1_scx()
            CALL SHOWIT(10)
         END IF
      ELSE
!     PASS SCX FANG
         IF(sys_scy_fang().NE.sys_scx_fang().OR.sys_y1_scy_fang().NE.sys_x1_scx_fang()) THEN
!     DO SCX FANG
1320        FORMAT('SCX FANG,',G23.15,',',G23.15)
            WRITE(OUTLYNE,1320) sys_scx_fang(),sys_x1_scx_fang()
            CALL SHOWIT(10)
         END IF
      END IF
   ELSE
      IF(sys_pxim_fang_set().NE.0.0D0) THEN
!     PXIM OR PXIM FANG
         IF(sys_pxim_fang_set().EQ.-1.0D0) THEN
            WRITE(OUTLYNE,331) sys_pxim()
         ELSE
            WRITE(OUTLYNE,332) sys_pxim()
         END IF
         CALL SHOWIT(10)
      END IF
      IF(sys_pyim_fang_set().NE.0.0D0) THEN
!     PYIM OR PYIM FANG
         IF(sys_pyim_fang_set().EQ.-1.0D0) THEN
            WRITE(OUTLYNE,341) sys_pyim()
         ELSE
            WRITE(OUTLYNE,342) sys_pyim()
         END IF
         CALL SHOWIT(10)
      END IF
      IF(sys_rxim_fang_set().NE.0.0D0) THEN
!     RXIM OR RXIM FANG
         IF(sys_rxim_fang_set().EQ.-1.0D0) THEN
            WRITE(OUTLYNE,351) sys_rxim()
         ELSE
            WRITE(OUTLYNE,352) sys_rxim()
         END IF
         CALL SHOWIT(10)
      END IF
      IF(sys_ryim_fang_set().NE.0.0D0) THEN
!     RYIM OR RYIM FANG
         IF(sys_ryim_fang_set().EQ.-1.0D0) THEN
            WRITE(OUTLYNE,361) sys_ryim()
         ELSE
            WRITE(OUTLYNE,362) sys_ryim()
         END IF
         CALL SHOWIT(10)
      END IF
   END IF
   IF(sys_ryim_fang_set().NE.0.0D0.AND.sys_rxim_fang_set().NE.0.0D0.AND.sys_reverse_trace().NE.0.0D0) THEN
      WRITE(OUTLYNE,363)
363   FORMAT('REVRAY ON')
      CALL SHOWIT(10)
   END IF
!
10 FORMAT('LENS')
20 FORMAT('LI,',A79)
21 FORMAT('LIC,',A79)
121 FORMAT('INI,',A79)
1212 FORMAT('LTYPE,',A5)
!
6661 FORMAT('WV      ,',G23.15,',',G23.15)
6662 FORMAT('WV2     ,',G23.15,',',G23.15)
6663 FORMAT(G23.15,',',G23.15,',',G23.15)
!
22 FORMAT('WV      ,',G23.15,',',G23.15,',',G23.15,',',G23.15 ,',',G23.15)
221 FORMAT('WV2     ,',G23.15,',',G23.15,',',G23.15,',',G23.15 ,',',G23.15)
23 FORMAT('UNITS    IN')
24 FORMAT('UNITS    CM')
25 FORMAT('UNITS    MM')
33 FORMAT('UNITS    M ')
26 FORMAT('PCW     ,',I2,',',I2)
27 FORMAT('SCW     ,',I2,',',I2)
28 FORMAT('CW      ,',I2)
29 FORMAT('SAY     ,',G23.15)
292 FORMAT('SAY      FLOAT    ')
30 FORMAT('SAX     ,',G23.15)
3022 FORMAT('SAX      FLOAT   ,')
2929 FORMAT('NAOY    ,',G23.15)
3030 FORMAT('NAOX    ,',G23.15)
2930 FORMAT('FNOY    ,',G23.15)
3031 FORMAT('FNOX    ,',G23.15)
31 FORMAT('SCY     ,',G23.15,',',G23.15)
32 FORMAT('SCX     ,',G23.15,',',G23.15)
331 FORMAT('PXIM    ',G23.15)
341 FORMAT('PYIM    ',G23.15)
351 FORMAT('RXIM    ',G23.15)
361 FORMAT('RYIM    ',G23.15)
332 FORMAT('PXIM     FANG    ,',G23.15)
342 FORMAT('PYIM     FANG    ,',G23.15)
352 FORMAT('RXIM     FANG    ,',G23.15)
362 FORMAT('RYIM     FANG    ,',G23.15)
   RETURN
END
! SUB PIKSLV.FOR
SUBROUTINE PIKSLV(I)
!
   use DATLEN
   use mod_surface
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       SUBROUTINE PIKSLV HANDELS LENS SURFACE PIKUPS
!       DURING A LENO AND
!       ACTS ON THE CURRENT LENS, ONLY
!       ONE SURFACE AT A TIME.
!       THE VARIABLE I PASSES THE SURFACE
!       NUMBER OF INTEREST
!
   INTEGER I
!
!
!       THE CURRENT SURFACE IS PASSED WITH I
!       A DUMMY SURFACE HAS AIR AS ITS MATERIAL AND HAS
!       AIR AS THE MATERIAL INFRONT OF IT.
!
401 FORMAT ('CK THE FOLLOWING PIKUP/SOLVE DATA REFERS TO SURFACE #',I1)
402 FORMAT ('CK THE FOLLOWING PIKUP/SOLVE DATA REFERS TO SURFACE #',I2)
403 FORMAT ('CK THE FOLLOWING PIKUP/SOLVE DATA REFERS TO SURFACE #',I3)
!     SURFACE LABEL
   IF(I.LT.10) WRITE(OUTLYNE,401) I
   IF(I.GE.10.AND.I.LT.100) WRITE(OUTLYNE,402) I
   IF(I.GE.100) WRITE(OUTLYNE,403) I
   CALL SHOWIT(10)
!       PIKUPS
!
!       RD PIKUP
   IF(PIKUP(1,I,1).EQ.1.0D0) THEN
      WRITE(OUTLYNE,1) PIKUP(2,I,1),PIKUP(3,I,1),PIKUP(4,I,1),PIKUP(6,I,1)
      CALL SHOWIT(10)
1     FORMAT('PIKUP    RD      ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!       CV PIKUP
   IF(PIKUP(1,I,2).EQ.1.0D0) THEN
      WRITE(OUTLYNE,2) PIKUP(2,I,2),PIKUP(3,I,2),PIKUP(4,I,2),PIKUP(6,I,2)
      CALL SHOWIT(10)
2     FORMAT('PIKUP    CV      ,',G23.15,',',G23.15,',',G23.15,',,',G23.15)
   END IF
!
!       TH PIKUP
   IF(PIKUP(1,I,3).EQ.1.0D0) THEN
      WRITE(OUTLYNE,3) PIKUP(2,I,3),PIKUP(3,I,3),PIKUP(4,I,3),PIKUP(6,I,3)
      CALL SHOWIT(10)
3     FORMAT('PIKUP    TH      ,',G23.15,',',G23.15,',',G23.15,',,',G23.15)
   END IF
!
!       THOAL PIKUP
   IF(PIKUP(1,I,32).EQ.1.0D0) THEN
      WRITE(OUTLYNE,901) PIKUP(2,I,32),PIKUP(3,I,32),PIKUP(4,I,32),PIKUP(5,I,32),PIKUP(6,I,32)
      CALL SHOWIT(10)
901   FORMAT('PIKUP    THOAL   ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
   END IF
!       PIVX PIKUP
   IF(PIKUP(1,I,34).EQ.1.0D0) THEN
      WRITE(OUTLYNE,902) PIKUP(2,I,34),PIKUP(3,I,34),PIKUP(4,I,34),PIKUP(6,I,34)
      CALL SHOWIT(10)
902   FORMAT('PIKUP    PIVX    ,',G23.15,',',G23.15,',',G23.15,',,',G23.15)
   END IF
!       PIVX PIKUP
   IF(PIKUP(1,I,35).EQ.1.0D0) THEN
      WRITE(OUTLYNE,904) PIKUP(2,I,35),PIKUP(3,I,35),PIKUP(4,I,35),PIKUP(6,I,35)
      CALL SHOWIT(10)
904   FORMAT('PIKUP    PIVY    ,',G23.15,',',G23.15,',',G23.15,',,',G23.15)
   END IF
!       PIVZ PIKUP
   IF(PIKUP(1,I,36).EQ.1.0D0) THEN
      WRITE(OUTLYNE,906) PIKUP(2,I,36),PIKUP(3,I,36),PIKUP(4,I,36),PIKUP(6,I,36)
      CALL SHOWIT(10)
906   FORMAT('PIKUP    PIVZ    ,',G23.15,',',G23.15,',',G23.15,',,',G23.15)
   END IF
!       GDX PIKUP
   IF(PIKUP(1,I,37).EQ.1.0D0) THEN
      WRITE(OUTLYNE,908) PIKUP(2,I,37),PIKUP(3,I,37),PIKUP(4,I,37),PIKUP(6,I,37)
      CALL SHOWIT(10)
908   FORMAT('PIKUP    GDX    ,',G23.15,',',G23.15,',',G23.15,',,',G23.15)
   END IF
!       GDY PIKUP
   IF(PIKUP(1,I,38).EQ.1.0D0) THEN
      WRITE(OUTLYNE,910) PIKUP(2,I,38),PIKUP(3,I,38),PIKUP(4,I,38),PIKUP(6,I,38)
      CALL SHOWIT(10)
910   FORMAT('PIKUP    GDY    ,',G23.15,',',G23.15,',',G23.15,',,',G23.15)
   END IF
!       GDZ PIKUP
   IF(PIKUP(1,I,39).EQ.1.0D0) THEN
      WRITE(OUTLYNE,912) PIKUP(2,I,39),PIKUP(3,I,39),PIKUP(4,I,39),PIKUP(6,I,39)
      CALL SHOWIT(10)
912   FORMAT('PIKUP    GDZ     ,',G23.15,',',G23.15,',',G23.15,',,',G23.15)
   END IF
!       GALPHA PIKUP
   IF(PIKUP(1,I,40).EQ.1.0D0) THEN
      WRITE(OUTLYNE,920) PIKUP(2,I,40),PIKUP(3,I,40),PIKUP(4,I,40),PIKUP(6,I,40)
      CALL SHOWIT(10)
920   FORMAT('PIKUP    GALPHA  ,',G23.15,',',G23.15,',',G23.15,',,',G23.15)
   END IF
!       GBETA PIKUP
   IF(PIKUP(1,I,41).EQ.1.0D0) THEN
      WRITE(OUTLYNE,916) PIKUP(2,I,41),PIKUP(3,I,41),PIKUP(4,I,41),PIKUP(6,I,41)
      CALL SHOWIT(10)
916   FORMAT('PIKUP    GBETA   ,',G23.15,',',G23.15,',',G23.15,',,',G23.15)
   END IF
!       GGAMMA PIKUP
   IF(PIKUP(1,I,42).EQ.1.0D0) THEN
      WRITE(OUTLYNE,918) PIKUP(2,I,42),PIKUP(3,I,42),PIKUP(4,I,42),PIKUP(6,I,42)
      CALL SHOWIT(10)
918   FORMAT('PIKUP    GGAMMA  ,',G23.15,',',G23.15,',',G23.15,',,',G23.15)
   END IF
!       CC PIKUP
   IF(PIKUP(1,I,4).EQ.1.0D0) THEN
      WRITE(OUTLYNE,4) PIKUP(2,I,4),PIKUP(3,I,4),PIKUP(4,I,4),PIKUP(6,I,4)
      CALL SHOWIT(10)
4     FORMAT('PIKUP    CC      ,',G23.15,',',G23.15,',',G23.15,',,',G23.15)
   END IF
!
!       AD PIKUP
   IF(PIKUP(1,I,5).EQ.1.0D0) THEN
      WRITE(OUTLYNE,5) PIKUP(2,I,5),PIKUP(3,I,5),PIKUP(4,I,5),PIKUP(6,I,5)
      CALL SHOWIT(10)
5     FORMAT('PIKUP    AD      ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       AE PIKUP
   IF(PIKUP(1,I,6).EQ.1.0D0) THEN
      WRITE(OUTLYNE,6) PIKUP(2,I,6),PIKUP(3,I,6),PIKUP(4,I,6),PIKUP(6,I,6)
      CALL SHOWIT(10)
6     FORMAT('PIKUP    AE      ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       AF PIKUP
   IF(PIKUP(1,I,7).EQ.1.0D0) THEN
      WRITE(OUTLYNE,7) PIKUP(2,I,7),PIKUP(3,I,7),PIKUP(4,I,7),PIKUP(6,I,7)
      CALL SHOWIT(10)
7     FORMAT('PIKUP    AF      ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       AG PIKUP
   IF(PIKUP(1,I,8).EQ.1.0D0) THEN
      WRITE(OUTLYNE,8) PIKUP(2,I,8),PIKUP(3,I,8),PIKUP(4,I,8),PIKUP(6,I,8)
      CALL SHOWIT(10)
8     FORMAT('PIKUP    AG      ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       AH PIKUP
   IF(PIKUP(1,I,27).EQ.1.0D0) THEN
      WRITE(OUTLYNE,8) PIKUP(2,I,27),PIKUP(3,I,27),PIKUP(4,I,27),PIKUP(6,I,27)
      CALL SHOWIT(10)
801   FORMAT('PIKUP    AH      ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       AI PIKUP
   IF(PIKUP(1,I,28).EQ.1.0D0) THEN
      WRITE(OUTLYNE,8) PIKUP(2,I,28) ,PIKUP(3,I,28),PIKUP(4,I,28),PIKUP(6,I,28)
      CALL SHOWIT(10)
802   FORMAT('PIKUP    AI      ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       AJ PIKUP
   IF(PIKUP(1,I,29).EQ.1.0D0) THEN
      WRITE(OUTLYNE,8) PIKUP(2,I,29),PIKUP(3,I,29),PIKUP(4,I,29),PIKUP(6,I,29)
      CALL SHOWIT(10)
803   FORMAT('PIKUP    AJ      ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       AK PIKUP
   IF(PIKUP(1,I,30).EQ.1.0D0) THEN
      WRITE(OUTLYNE,8) PIKUP(2,I,30),PIKUP(3,I,30),PIKUP(4,I,30),PIKUP(6,I,30)
      CALL SHOWIT(10)
804   FORMAT('PIKUP    AK      ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       AL PIKUP
   IF(PIKUP(1,I,31).EQ.1.0D0) THEN
      WRITE(OUTLYNE,8) PIKUP(2,I,31),PIKUP(3,I,31),PIKUP(4,I,31),PIKUP(6,I,31)
      CALL SHOWIT(10)
805   FORMAT('PIKUP    AL      ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       CVTOR PIKUP
   IF(PIKUP(1,I,9).EQ.1.0D0) THEN
      WRITE(OUTLYNE,9) PIKUP(2,I,9),PIKUP(3,I,9),PIKUP(4,I,9),PIKUP(6,I,9)
      CALL SHOWIT(10)
9     FORMAT('PIKUP    CVTOR   ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       RDTOR PIKUP
   IF(PIKUP(1,I,10).EQ.1.0D0) THEN
      WRITE(OUTLYNE,10) PIKUP(2,I,10),PIKUP(3,I,10),PIKUP(4,I,10),PIKUP(6,I,10)
      CALL SHOWIT(10)
10    FORMAT('PIKUP    RDTOR   ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       PRO PIKUP
   IF(PIKUP(1,I,11).EQ.1.0D0) THEN
      WRITE(OUTLYNE,11) PIKUP(2,I,11),PIKUP(6,I,11)
      CALL SHOWIT(10)
11    FORMAT('PIKUP    PRO     ,',G23.15,',,,,',G23.15)
   END IF
!
!       NPRO PIKUP
   IF(PIKUP(1,I,12).EQ.1.0D0) THEN
      WRITE(OUTLYNE,12) PIKUP(2,I,12),PIKUP(6,I,12)
      CALL SHOWIT(10)
12    FORMAT('PIKUP    NPRO    ,',G23.15,',,,,',G23.15)
   END IF
!
!       YD PIKUP
   IF(PIKUP(1,I,13).EQ.1.0D0) THEN
      WRITE(OUTLYNE,13) PIKUP(2,I,13),PIKUP(3,I,13),PIKUP(4,I,13),PIKUP(6,I,13)
      CALL SHOWIT(10)
13    FORMAT('PIKUP    YD      ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       XD PIKUP
   IF(PIKUP(1,I,14).EQ.1.0D0) THEN
      WRITE(OUTLYNE,14) PIKUP(2,I,14),PIKUP(3,I,14),PIKUP(4,I,14),PIKUP(6,I,14)
      CALL SHOWIT(10)
14    FORMAT('PIKUP    XD      ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!
!       ZD PIKUP
   IF(PIKUP(1,I,33).EQ.1.0D0) THEN
      WRITE(OUTLYNE,914) PIKUP(2,I,33),PIKUP(3,I,33),PIKUP(4,I,33),PIKUP(6,I,33)
      CALL SHOWIT(10)
914   FORMAT('PIKUP    ZD      ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       ALPHA PIKUP
   IF(PIKUP(1,I,15).EQ.1.0D0) THEN
      WRITE(OUTLYNE,15) PIKUP(2,I,15),PIKUP(3,I,15),PIKUP(4,I,15),PIKUP(6,I,15)
      CALL SHOWIT(10)
15    FORMAT('PIKUP    ALPHA   ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       BETA PIKUP
   IF(PIKUP(1,I,16).EQ.1.0D0) THEN
      WRITE(OUTLYNE,16) PIKUP(2,I,16),PIKUP(3,I,16),PIKUP(4,I,16),PIKUP(6,I,16)
      CALL SHOWIT(10)
16    FORMAT('PIKUP    BETA    ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       GAMMA PIKUP
   IF(PIKUP(1,I,17).EQ.1.0D0) THEN
      WRITE(OUTLYNE,17) PIKUP(2,I,17),PIKUP(3,I,17),PIKUP(4,I,17),PIKUP(6,I,17)
      CALL SHOWIT(10)
17    FORMAT('PIKUP    GAMMA   ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       CLAP PIKUP
   IF(PIKUP(1,I,18).EQ.1.0D0) THEN
      WRITE(OUTLYNE,18) PIKUP(2,I,18),PIKUP(6,I,18)
      CALL SHOWIT(10)
18    FORMAT('PIKUP    CLAP    ,',G23.15,',,,,',G23.15)
   END IF
!
!       COBS PIKUP
   IF(PIKUP(1,I,19).EQ.1.0D0) THEN
      WRITE(OUTLYNE,19) PIKUP(2,I,19),PIKUP(6,I,19)
      CALL SHOWIT(10)
19    FORMAT('PIKUP    COBS    ,',G23.15,',,,,',G23.15)
   END IF
!
!       GLASS PIKUP
   IF(PIKUP(1,I,20).EQ.1.0D0) THEN
      WRITE(OUTLYNE,20) PIKUP(2,I,20),PIKUP(6,I,20)
      CALL SHOWIT(10)
20    FORMAT('PIKUP    GLASS   ,',G23.15,',,,,',G23.15)
   END IF
!
!       CCTOR PIKUP
   IF(PIKUP(1,I,21).EQ.1.0D0) THEN
      WRITE(OUTLYNE,21) PIKUP(2,I,21),PIKUP(3,I,21),PIKUP(4,I,21),PIKUP(6,I,21)
      CALL SHOWIT(10)
21    FORMAT('PIKUP    CCTOR   ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       ADTOR PIKUP
   IF(PIKUP(1,I,22).EQ.1.0D0) THEN
      WRITE(OUTLYNE,22) PIKUP(2,I,22),PIKUP(3,I,22),PIKUP(4,I,22),PIKUP(6,I,22)
      CALL SHOWIT(10)
22    FORMAT('PIKUP    ADTOR   ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       AETOR PIKUP
   IF(PIKUP(1,I,23).EQ.1.0D0) THEN
      WRITE(OUTLYNE,23) PIKUP(2,I,23),PIKUP(3,I,23),PIKUP(4,I,23),PIKUP(6,I,23)
      CALL SHOWIT(10)
23    FORMAT('PIKUP    AETOR   ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       AFTOR PIKUP
   IF(PIKUP(1,I,24).EQ.1.0D0) THEN
      WRITE(OUTLYNE,24) PIKUP(2,I,24),PIKUP(3,I,24),PIKUP(4,I,24),PIKUP(6,I,24)
      CALL SHOWIT(10)
24    FORMAT('PIKUP    AFTOR   ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       AGTOR PIKUP
   IF(PIKUP(1,I,25).EQ.1.0D0) THEN
      WRITE(OUTLYNE,25) PIKUP(2,I,25),PIKUP(3,I,25),PIKUP(4,I,25),PIKUP(6,I,25)
      CALL SHOWIT(10)
25    FORMAT('PIKUP    AGTOR   ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       AC PIKUP
   IF(PIKUP(1,I,26).EQ.1.0D0) THEN
      WRITE(OUTLYNE,26) PIKUP(2,I,26),PIKUP(3,I,26),PIKUP(4,I,26),PIKUP(6,I,26)
      CALL SHOWIT(10)
26    FORMAT('PIKUP    AC      ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       AH PIKUP
   IF(PIKUP(1,I,27).EQ.1.0D0) THEN
      WRITE(OUTLYNE,27) PIKUP(2,I,27),PIKUP(3,I,27),PIKUP(4,I,27),PIKUP(6,I,27)
      CALL SHOWIT(10)
27    FORMAT('PIKUP    AH      ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       AI PIKUP
   IF(PIKUP(1,I,28).EQ.1.0D0) THEN
      WRITE(OUTLYNE,28) PIKUP(2,I,28),PIKUP(3,I,28),PIKUP(4,I,28),PIKUP(6,I,28)
      CALL SHOWIT(10)
28    FORMAT('PIKUP    AI      ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       AJ PIKUP
   IF(PIKUP(1,I,29).EQ.1.0D0) THEN
      WRITE(OUTLYNE,29) PIKUP(2,I,29),PIKUP(3,I,29),PIKUP(4,I,29),PIKUP(6,I,29)
      CALL SHOWIT(10)
29    FORMAT('PIKUP    AJ      ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       AK PIKUP
   IF(PIKUP(1,I,30).EQ.1.0D0) THEN
      WRITE(OUTLYNE,30) PIKUP(2,I,30),PIKUP(3,I,30),PIKUP(4,I,30),PIKUP(6,I,30)
      CALL SHOWIT(10)
30    FORMAT('PIKUP    AK      ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       AL PIKUP
   IF(PIKUP(1,I,31).EQ.1.0D0) THEN
      WRITE(OUTLYNE,31) PIKUP(2,I,31),PIKUP(3,I,31),PIKUP(4,I,31),PIKUP(6,I,31)
      CALL SHOWIT(10)
31    FORMAT('PIKUP    AL      ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       THOAL PIKUP
   IF(PIKUP(1,I,32).EQ.1.0D0) THEN
      WRITE(OUTLYNE,32) PIKUP(2,I,32),PIKUP(3,I,32),PIKUP(4,I,32),PIKUP(5,I,32),PIKUP(6,I,32)
      CALL SHOWIT(10)
32    FORMAT('PIKUP    THOAL   ,',G23.15,',',G23.15,',',G23.15 ,',',G23.15,',',G23.15)
   END IF
!
!       ZD PIKUP
   IF(PIKUP(1,I,33).EQ.1.0D0) THEN
      WRITE(OUTLYNE,33) PIKUP(2,I,33),PIKUP(3,I,33),PIKUP(4,I,33),PIKUP(6,I,33)
      CALL SHOWIT(10)
33    FORMAT('PIKUP    ZD      ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       PIVX PIKUP
   IF(PIKUP(1,I,34).EQ.1.0D0) THEN
      WRITE(OUTLYNE,34) PIKUP(2,I,34),PIKUP(3,I,34),PIKUP(4,I,34),PIKUP(6,I,34)
      CALL SHOWIT(10)
34    FORMAT('PIKUP    PIVX    ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       PIVY PIKUP
   IF(PIKUP(1,I,35).EQ.1.0D0) THEN
      WRITE(OUTLYNE,35) PIKUP(2,I,35),PIKUP(3,I,35),PIKUP(4,I,35),PIKUP(6,I,35)
      CALL SHOWIT(10)
35    FORMAT('PIKUP    PIVY    ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       PIVZ PIKUP
   IF(PIKUP(1,I,36).EQ.1.0D0) THEN
      WRITE(OUTLYNE,36) PIKUP(2,I,36),PIKUP(3,I,36),PIKUP(4,I,36),PIKUP(6,I,36)
      CALL SHOWIT(10)
36    FORMAT('PIKUP    PIVZ    ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       GDX PIKUP
   IF(PIKUP(1,I,37).EQ.1.0D0) THEN
      WRITE(OUTLYNE,37) PIKUP(2,I,37),PIKUP(3,I,37),PIKUP(4,I,37),PIKUP(6,I,37)
      CALL SHOWIT(10)
37    FORMAT('PIKUP    GDX     ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       GDY PIKUP
   IF(PIKUP(1,I,38).EQ.1.0D0) THEN
      WRITE(OUTLYNE,38) PIKUP(2,I,38),PIKUP(3,I,38),PIKUP(4,I,38),PIKUP(6,I,38)
      CALL SHOWIT(10)
38    FORMAT('PIKUP    GDY     ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       GDZ PIKUP
   IF(PIKUP(1,I,39).EQ.1.0D0) THEN
      WRITE(OUTLYNE,39) PIKUP(2,I,39),PIKUP(3,I,39),PIKUP(4,I,39),PIKUP(6,I,39)
      CALL SHOWIT(10)
39    FORMAT('PIKUP    GDZ     ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       GALPHA PIKUP
   IF(PIKUP(1,I,40).EQ.1.0D0) THEN
      WRITE(OUTLYNE,40) PIKUP(2,I,40),PIKUP(3,I,40),PIKUP(4,I,40),PIKUP(6,I,40)
      CALL SHOWIT(10)
40    FORMAT('PIKUP    GALPHA  ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       GBETA PIKUP
   IF(PIKUP(1,I,41).EQ.1.0D0) THEN
      WRITE(OUTLYNE,41) PIKUP(2,I,41),PIKUP(3,I,41),PIKUP(4,I,41),PIKUP(6,I,41)
      CALL SHOWIT(10)
41    FORMAT('PIKUP    GBETA   ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       GGAMMA PIKUP
   IF(PIKUP(1,I,42).EQ.1.0D0) THEN
      WRITE(OUTLYNE,42) PIKUP(2,I,42),PIKUP(3,I,42),PIKUP(4,I,42),PIKUP(6,I,42)
      CALL SHOWIT(10)
42    FORMAT('PIKUP    GGAMMA  ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       GRT PIKUP
   IF(PIKUP(1,I,43).EQ.1.0D0) THEN
      WRITE(OUTLYNE,43) PIKUP(2,I,43),PIKUP(3,I,43),PIKUP(4,I,43),PIKUP(6,I,43)
      CALL SHOWIT(10)
43    FORMAT('PIKUP    GRT     ,',G23.15,',',G23.15,',',G23.15 ,',,',G23.15)
   END IF
!
!       COATING PIKUP
   IF(PIKUP(1,I,44).EQ.1.0D0) THEN
      WRITE(OUTLYNE,44) PIKUP(2,I,44),PIKUP(6,I,44)
      CALL SHOWIT(10)
44    FORMAT('PIKUP    COATING ,',G23.15,',,,,',G23.15)
   END IF
!
!       CURVATURE SOLVES
   IF(surf_toric_flag(I).EQ.0.0D0.OR.surf_toric_flag(I).EQ.1.0D0) THEN
!       SURFACE IS NOT TORIC OR IS Y-TORIC
!       APY SOLVE
      IF(SOLVE(8,I).EQ.1.0D0) THEN
         WRITE(OUTLYNE,202)SOLVE(9,I)
         CALL SHOWIT(10)
202      FORMAT('APY     ,',G23.15)
      END IF
!       PIY SOLVE
      IF(SOLVE(8,I).EQ.2.0D0) THEN
         WRITE(OUTLYNE,203)SOLVE(9,I)
         CALL SHOWIT(10)
203      FORMAT('PIY     ,',G23.15)
      END IF
!       PUY SOLVE
      IF(SOLVE(8,I).EQ.3.0D0) THEN
         WRITE(OUTLYNE,204)SOLVE(9,I)
         CALL SHOWIT(10)
204      FORMAT('PUY     ,',G23.15)
      END IF
!       APCY SOLVE
      IF(SOLVE(8,I).EQ.4.0D0) THEN
         WRITE(OUTLYNE,205)SOLVE(9,I)
         CALL SHOWIT(10)
205      FORMAT('APCY    ,',G23.15)
      END IF
!       PICY SOLVE
      IF(SOLVE(8,I).EQ.5.0D0) THEN
         WRITE(OUTLYNE,206)SOLVE(9,I)
         CALL SHOWIT(10)
206      FORMAT('PICY    ,',G23.15)
      END IF
!       PUCY SOLVE
      IF(SOLVE(8,I).EQ.6.0D0) THEN
         WRITE(OUTLYNE,207)SOLVE(9,I)
         CALL SHOWIT(10)
207      FORMAT('PUCY    ,',G23.15)
      END IF
!       COCY SOLVE
      IF(SOLVE(8,I).EQ.7.0D0) THEN
         WRITE(OUTLYNE,208)SOLVE(9,I)
         CALL SHOWIT(10)
208      FORMAT('COCY    ,',G23.15)
      END IF
   END IF
   IF(surf_toric_flag(I).EQ.2.0D0) THEN
!       SURFACE IS X-TORIC
!       APX SOLVE
      IF(SOLVE(2,I).EQ.8.0D0) THEN
         WRITE(OUTLYNE,209)SOLVE(1,I)
         CALL SHOWIT(10)
209      FORMAT('APX     ,',G23.15)
      END IF
!       PIX SOLVE
      IF(SOLVE(2,I).EQ.9.0D0) THEN
         WRITE(OUTLYNE,210)SOLVE(1,I)
         CALL SHOWIT(10)
210      FORMAT('PIX     ,',G23.15)
      END IF
!       PUX SOLVE
      IF(SOLVE(2,I).EQ.10.0D0) THEN
         WRITE(OUTLYNE,211)SOLVE(1,I)
         CALL SHOWIT(10)
211      FORMAT('PUX     ,',G23.15)
      END IF
!       APCX SOLVE
      IF(SOLVE(2,I).EQ.11.0D0) THEN
         WRITE(OUTLYNE,212)SOLVE(1,I)
         CALL SHOWIT(10)
212      FORMAT('APCX    ,',G23.15)
      END IF
!       PICX SOLVE
      IF(SOLVE(2,I).EQ.12.0D0) THEN
         WRITE(OUTLYNE,213)SOLVE(1,I)
         CALL SHOWIT(10)
213      FORMAT('PICX    ,',G23.15)
      END IF
!       PUCX SOLVE
      IF(SOLVE(2,I).EQ.13.0D0) THEN
         WRITE(OUTLYNE,214)SOLVE(1,I)
         CALL SHOWIT(10)
214      FORMAT('PUCX    ,',G23.15)
      END IF
!       COCX SOLVE
      IF(SOLVE(2,I).EQ.14.0D0) THEN
         WRITE(OUTLYNE,215)SOLVE(1,I)
         CALL SHOWIT(10)
215      FORMAT('COCX    ,',G23.15)
      END IF
   END IF
!
!       THICKNESS SOLVES
!
!       PY SOLVE
   IF(SOLVE(6,I).EQ.1.0D0) THEN
      WRITE(OUTLYNE,302)SOLVE(7,I)
      CALL SHOWIT(10)
302   FORMAT('PY      ,',G23.15)
   END IF
!       PCY SOLVE
   IF(SOLVE(6,I).EQ.2.0D0) THEN
      WRITE(OUTLYNE,303)SOLVE(7,I)
      CALL SHOWIT(10)
303   FORMAT('PCY     ,',G23.15)
   END IF
!       CAY SOLVE
   IF(SOLVE(6,I).EQ.3.0D0) THEN
      WRITE(OUTLYNE,304)SOLVE(7,I)
      CALL SHOWIT(10)
304   FORMAT('CAY     ,',G23.15)
   END IF
   ! Mag SOLVE
   IF(SOLVE(6,I).EQ.7.0D0) THEN
      WRITE(OUTLYNE,308)SOLVE(7,I)
      CALL SHOWIT(10)
308   FORMAT('REDSLV ,',G23.15)
   END IF

!
!       PX SOLVE
   IF(SOLVE(4,I).EQ.4.0D0) THEN
      WRITE(OUTLYNE,305)SOLVE(3,I)
      CALL SHOWIT(10)
305   FORMAT('PX      ,',G23.15)
   END IF
!       PCX SOLVE
   IF(SOLVE(4,I).EQ.5.0D0) THEN
      WRITE(OUTLYNE,306)SOLVE(3,I)
      CALL SHOWIT(10)
306   FORMAT('PCX     ,',G23.15)
   END IF
!       CAX SOLVE
   IF(SOLVE(4,I).EQ.6.0D0) THEN
      WRITE(OUTLYNE,307)SOLVE(3,I)
      CALL SHOWIT(10)
307   FORMAT('CAX     ,',G23.15)
   END IF
!       ALL SOLVES AND PIKUPS HAVE BEEN TAKEN CARE OF.
   RETURN
END
! SUB LENSFCV.FOR
SUBROUTINE LENSFCV(I)
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_astop, sys_ref_surf
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       SUBROUTINE LENSF HANDELS LENS SURFACE INFO OUTPUT
!       DURING A LENO CV AND
!       ACTS ON THE CURRENT LENS, ONLY
!       ONE SURFACE AT A TIME.
!       THE VARIABLE I PASSES THE SURFACE
!       NUMBER OF INTEREST
!
   INTEGER I,J,JA
!
   real(real64) A,B
!
   CHARACTER GL*27,SBL*79
!
!
!       THE CURRENT SURFACE IS PASSED WITH I
!       A DUMMY SURFACE HAS AIR AS ITS MATERIAL AND HAS
!       AIR AS THE MATERIAL INFRONT OF IT.
!
!       WE NOW HAVE LENO CV, PROCEED
   IF(I.EQ.0.AND.surf_thickness(I).GE.0.0D0) WRITE(OUTLYNE,1)
   IF(I.EQ.0.AND.surf_thickness(I).LT.0.0D0) WRITE(OUTLYNE,119)
   IF(surf_toric_flag(I).EQ.0.0D0.OR.surf_toric_flag(I).EQ.1.0D0) THEN
      IF(surf_curvature(I).EQ.0.0D0) A=0.0D0
      IF(surf_curvature(I).NE.0.0D0) A=1.0D0/surf_curvature(I)
   END IF
   IF(surf_toric_flag(I).EQ.2.0D0) THEN
      IF(surf_toric_curvature(I).EQ.0.0D0) A=0.0D0
      IF(surf_toric_curvature(I).NE.0.0D0) A=1.0D0/surf_toric_curvature(I)
   END IF
   B=surf_thickness(I)
   DO,J=13,1,-1
      IF(GLANAM(I,2)(J:J).NE.' ') THEN
         JA=J
         GO TO 666
      END IF
   END DO
666 CONTINUE
   IF(GLANAM(I,1).EQ.'SCHOTT')GL=GLANAM(I,2)(1:JA)//'_SCHOTT'
   IF(GLANAM(I,1).EQ.'SCH2000')GL=GLANAM(I,2)(1:JA)//'_SCH2000'
   IF(GLANAM(I,1).EQ.'OHARA')GL=GLANAM(I,2)(1:JA)//'_OHARA'
   IF(GLANAM(I,1).EQ.'HOYA')GL=GLANAM(I,2)(1:JA)//'_HOYA'
   IF(GLANAM(I,1).EQ.'HIKARI')GL=GLANAM(I,2)(1:JA)//'_HIKARI'
   IF(GLANAM(I,1).EQ.'CORNIN')GL=GLANAM(I,2)(1:JA)//'_CORNFR'
   IF(GLANAM(I,1).EQ.'CHANCE')GL=GLANAM(I,2)(1:JA)//'_CHANCE'
   IF(GLANAM(I,2).EQ.'LAST SURFACE')GL='AIR                        '
   IF(GLANAM(I,1).NE.'SCHOTT'.AND.GLANAM(I,1).NE.'SCH2000'.AND.GLANAM(I,1).NE.'OHARA'.AND.GLANAM(I,1).NE.'HOYA'.AND.GLANAM(I,1).NE.'HIKARI'.AND.GLANAM(I,1).NE.'CORNIN'.AND.GLANAM(I,2).NE.'LAST SURFACE'.AND.GLANAM(I,1).NE.'CHANCE')GL=GLANAM(I,2)(1:JA)
   IF(I.GT.0) WRITE(OUTLYNE,2) A,B,GL
   CALL SHOWIT(10)
!     SURFACE LABLE
   IF(surf_label_flag(I).EQ.1.0D0) THEN
      SBL='SLB '//''''//LBL(I)(1:72)//''''
      WRITE(OUTLYNE,400) SBL(1:79)
      CALL SHOWIT(10)
   END IF
400 FORMAT(A79)
1  FORMAT('S0 0.0 1.0E20 AIR')
119 FORMAT('S0 0.0 -1.0E20 AIR')
2  FORMAT('S',1X,G23.15,1X,G23.15,1X,A27)
!
!               GRATING

   IF(surf_diffraction_flag(I).EQ.1.0D0) THEN
      WRITE(OUTLYNE,1212)
1212  FORMAT('GRT')
      CALL SHOWIT(10)
      WRITE(OUTLYNE,1213) surf_grating_order(I)
1213  FORMAT('GRO',G23.15)
      CALL SHOWIT(10)
      WRITE(OUTLYNE,1214) surf_grating_spacing(I)
1214  FORMAT('GRS',G23.15)
      CALL SHOWIT(10)
      WRITE(OUTLYNE,1215) surf_grating_vx(I)
1215  FORMAT('GRX',G23.15)
      CALL SHOWIT(10)
      WRITE(OUTLYNE,1216) surf_grating_vy(I)
1216  FORMAT('GRY',G23.15)
      CALL SHOWIT(10)
      WRITE(OUTLYNE,1217) surf_grating_vz(I)
1217  FORMAT('GRZ',G23.15)
      CALL SHOWIT(10)
   END IF
!
   IF(surf_conic(I).NE.0.0) THEN
!               CONIC CONSTANT
      IF(surf_conic(I).NE.0.0D0.AND.surf_toric_flag(I).EQ.0.0D0.AND..NOT.surf_is_asphere(I)) THEN
         WRITE(OUTLYNE,3)
3        FORMAT('CONIC')
         CALL SHOWIT(10)
         WRITE(OUTLYNE,11) surf_conic(I)
         CALL SHOWIT(10)
11       FORMAT('K ',G23.15)
      END IF
   END IF
   IF(surf_toric_flag(I).EQ.0.0D0.AND.surf_is_asphere(I)) THEN
      WRITE(OUTLYNE,70)
70    FORMAT('ASP')
      CALL SHOWIT(10)
      IF(surf_conic(I).NE.0.0D0) THEN
         WRITE(OUTLYNE,11) surf_conic(I)
         CALL SHOWIT(10)
      END IF
   END IF
!               ASPHERICS
   IF(surf_is_asphere(I)) THEN
      IF(surf_asphere_coeff(I, 4).NE.0.0D0) THEN
         WRITE(OUTLYNE,5) surf_asphere_coeff(I, 4)
         CALL SHOWIT(10)
5        FORMAT('A ',G23.15)
      END IF
      IF(surf_asphere_coeff(I, 6).NE.0.0D0) THEN
         WRITE(OUTLYNE,6) surf_asphere_coeff(I, 6)
         CALL SHOWIT(10)
6        FORMAT('B ',G23.15)
      END IF
      IF(surf_asphere_coeff(I, 8).NE.0.0D0) THEN
         WRITE(OUTLYNE,7) surf_asphere_coeff(I, 8)
         CALL SHOWIT(10)
7        FORMAT('C ',G23.15)
      END IF
      IF(surf_asphere_coeff(I, 10).NE.0.0D0) THEN
         WRITE(OUTLYNE,8) surf_asphere_coeff(I, 10)
         CALL SHOWIT(10)
8        FORMAT('D ',G23.15)
      END IF
      IF(surf_asphere_coeff(I, 12).NE.0.0D0) THEN
         WRITE(OUTLYNE,990) surf_asphere_coeff(I, 12)
         CALL SHOWIT(10)
990      FORMAT('E ',G23.15)
      END IF
      IF(surf_asphere_coeff(I, 14).NE.0.0D0) THEN
         WRITE(OUTLYNE,991) surf_asphere_coeff(I, 14)
         CALL SHOWIT(10)
991      FORMAT('F ',G23.15)
      END IF
      IF(surf_asphere_coeff(I, 16).NE.0.0D0) THEN
         WRITE(OUTLYNE,992) surf_asphere_coeff(I, 16)
         CALL SHOWIT(10)
992      FORMAT('G ',G23.15)
      END IF
      IF(surf_asphere_coeff(I, 18).NE.0.0D0) THEN
         WRITE(OUTLYNE,993) surf_asphere_coeff(I, 18)
         CALL SHOWIT(10)
993      FORMAT('H ',G23.15)
      END IF
      IF(surf_asphere_coeff(I, 20).NE.0.0D0) THEN
         WRITE(OUTLYNE,994) surf_asphere_coeff(I, 20)
         CALL SHOWIT(10)
994      FORMAT('J ',G23.15)
      END IF
   END IF
!
!       NOW TORICS IF PRESENT
!
   IF(surf_toric_flag(I).NE.0.0D0) THEN
!     TORICS
      IF(surf_conic(I).EQ.0.0D0.AND.surf_anamorphic_conic(I).EQ.0.0D0.AND.surf_asphere_coeff(I, 6).EQ.0.0D0.AND.surf_asphere_coeff(I, 8).EQ.0.0D0.AND.surf_asphere_coeff(I, 10).EQ.0.0D0.AND..NOT.surf_is_asphere(I).AND.surf_anamorphic_coeff(I, 4).EQ.0.0D0.AND.surf_anamorphic_coeff(I, 6).EQ.0.0D0.AND.surf_anamorphic_coeff(I, 8).EQ.0.0D0.AND.surf_anamorphic_coeff(I, 10).EQ.0.0D0) THEN
!     CYLINDER ONLY
         WRITE(OUTLYNE,35)
         CALL SHOWIT(10)
!       TORICS PRESENT
         IF(surf_toric_flag(I).EQ.1.0) THEN
!     YTORIC
            WRITE(OUTLYNE,38) surf_toric_curvature(I)
            CALL SHOWIT(10)
         END IF
         IF(surf_toric_flag(I).EQ.2.0) THEN
            WRITE(OUTLYNE,38) surf_curvature(I)
            CALL SHOWIT(10)
         END IF
      ELSE
!     ANAMORPHIC ASPHERE
         WRITE(OUTLYNE,36)
         CALL SHOWIT(10)
!       TORICS PRESENT
         IF(surf_toric_flag(I).EQ.1.0) THEN
!     YTORIC
            WRITE(OUTLYNE,38) surf_toric_curvature(I)
            CALL SHOWIT(10)
            WRITE(OUTLYNE,61) surf_conic(I)
            CALL SHOWIT(10)
            WRITE(OUTLYNE,60) surf_anamorphic_conic(I)
            CALL SHOWIT(10)
            WRITE(OUTLYNE,62) surf_asphere_coeff(I, 4)
            CALL SHOWIT(10)
            WRITE(OUTLYNE,63) surf_asphere_coeff(I, 6)
            CALL SHOWIT(10)
            WRITE(OUTLYNE,64) surf_asphere_coeff(I, 8)
            CALL SHOWIT(10)
            WRITE(OUTLYNE,65) surf_asphere_coeff(I, 10)
            CALL SHOWIT(10)
            WRITE(OUTLYNE,66) surf_anamorphic_coeff(I, 4)
            CALL SHOWIT(10)
            WRITE(OUTLYNE,67) surf_anamorphic_coeff(I, 6)
            CALL SHOWIT(10)
            WRITE(OUTLYNE,68) surf_anamorphic_coeff(I, 8)
            CALL SHOWIT(10)
            WRITE(OUTLYNE,69) surf_anamorphic_coeff(I, 10)
            CALL SHOWIT(10)
         END IF
         IF(surf_toric_flag(I).EQ.2.0) THEN
            CALL SHOWIT(10)
            WRITE(OUTLYNE,38) surf_curvature(I)
            CALL SHOWIT(10)
            WRITE(OUTLYNE,60) surf_conic(I)
            CALL SHOWIT(10)
            WRITE(OUTLYNE,61) surf_anamorphic_conic(I)
            CALL SHOWIT(10)
            WRITE(OUTLYNE,62) surf_anamorphic_coeff(I, 4)
            CALL SHOWIT(10)
            WRITE(OUTLYNE,63) surf_anamorphic_coeff(I, 6)
            CALL SHOWIT(10)
            WRITE(OUTLYNE,64) surf_anamorphic_coeff(I, 8)
            CALL SHOWIT(10)
            WRITE(OUTLYNE,65) surf_anamorphic_coeff(I, 10)
            CALL SHOWIT(10)
            WRITE(OUTLYNE,66) surf_asphere_coeff(I, 4)
            CALL SHOWIT(10)
            WRITE(OUTLYNE,67) surf_asphere_coeff(I, 6)
            CALL SHOWIT(10)
            WRITE(OUTLYNE,68) surf_asphere_coeff(I, 8)
            CALL SHOWIT(10)
            WRITE(OUTLYNE,69) surf_asphere_coeff(I, 10)
            CALL SHOWIT(10)
         END IF
      END IF
   END IF
35 FORMAT('CYL')
36 FORMAT('AAS')
38 FORMAT('CUX',1X,G23.15)
37 FORMAT('CUY',1X,G23.15)
60 FORMAT('KX',1X,G23.15)
61 FORMAT('KY',1X,G23.15)
62 FORMAT('AR ',1X,G23.15)
63 FORMAT('BR ',1X,G23.15)
64 FORMAT('CR ',1X,G23.15)
65 FORMAT('DR ',1X,G23.15)
66 FORMAT('AP ',1X,G23.15)
67 FORMAT('BP ',1X,G23.15)
68 FORMAT('CP ',1X,G23.15)
69 FORMAT('DP ',1X,G23.15)
!

!
!               CLEAR APERTURE
   IF(surf_clap_type(I).NE.0.0.OR.surf_coat_type(I).NE.0.0) THEN
!     CLAP OR COBS PRESENT
      IF(surf_clap_type(I).EQ.1.0) THEN
!       CIRCULAR CLAP
14       FORMAT('CIR',1x,G23.15)
         WRITE(OUTLYNE,14)surf_clap_dim(I, 1)
         CALL SHOWIT(10)
814      FORMAT('ADY',1x,G23.15)
1814     FORMAT('ADY OBS',1x,G23.15)
         WRITE(OUTLYNE,814)surf_clap_dim(I, 3)
         CALL SHOWIT(10)
914      FORMAT('ADX',1x,G23.15)
1914     FORMAT('ADX OBS',1x,G23.15)
         WRITE(OUTLYNE,914)surf_clap_dim(I, 4)
         CALL SHOWIT(10)
      END IF
      IF(surf_clap_type(I).EQ.3.0) THEN
!       ELLIPTICAL CLAP
16       FORMAT('ELY',1X,G23.15)
816      FORMAT('ELX',1X,G23.15)
         WRITE(OUTLYNE,16)surf_clap_dim(I, 1)
         CALL SHOWIT(10)
         WRITE(OUTLYNE,816)surf_clap_dim(I, 2)
         CALL SHOWIT(10)
         WRITE(OUTLYNE,814)surf_clap_dim(I, 3)
         CALL SHOWIT(10)
         WRITE(OUTLYNE,914)surf_clap_dim(I, 4)
         CALL SHOWIT(10)
         IF(surf_clap_tilt(I).NE.0.0) THEN
            WRITE(OUTLYNE,15) surf_clap_tilt(I)
            CALL SHOWIT(10)
         END IF
15       FORMAT('ARO',1X,G23.15)
1515     FORMAT('ARO OBS',1X,G23.15)
      END IF
      IF(surf_clap_type(I).EQ.2.0) THEN
!       RECTANGULAR CLAP
17       FORMAT('REY',1X,G23.15)
817      FORMAT('REX',1X,G23.15)
         WRITE(OUTLYNE,17)surf_clap_dim(I, 1)
         CALL SHOWIT(10)
         WRITE(OUTLYNE,817)surf_clap_dim(I, 2)
         CALL SHOWIT(10)
         WRITE(OUTLYNE,814)surf_clap_dim(I, 3)
         CALL SHOWIT(10)
         WRITE(OUTLYNE,914)surf_clap_dim(I, 4)
         CALL SHOWIT(10)
         IF(surf_clap_tilt(I).NE.0.0) THEN
            WRITE(OUTLYNE,15) surf_clap_tilt(I)
            CALL SHOWIT(10)
         END IF
      END IF
   END IF
!               OBSCURATIONS
   IF(surf_coat_type(I).NE.0.0) THEN
      IF(surf_coat_type(I).EQ.1.0) THEN
!       CIRCULAR CLAP
714      FORMAT('CIR OBS',1x,G23.15)
         WRITE(OUTLYNE,714)surf_cobs_poly(I, 1)
         CALL SHOWIT(10)
         IF(surf_cobs_poly(I, 3).NE.0.0D0) THEN
            WRITE(OUTLYNE,1814)surf_cobs_poly(I, 3)
            CALL SHOWIT(10)
         END IF
         IF(surf_cobs_poly(I, 4).NE.0.0D0) THEN
            WRITE(OUTLYNE,1914)surf_cobs_poly(I, 4)
            CALL SHOWIT(10)
         END IF
      END IF
      IF(surf_coat_type(I).EQ.3.0) THEN
!       ELLIPTICAL CLAP
916      FORMAT('ELY OBS',1X,G23.15)
1816     FORMAT('ELX OBS',1X,G23.15)
         WRITE(OUTLYNE,916)surf_cobs_poly(I, 1)
         CALL SHOWIT(10)
         WRITE(OUTLYNE,1816)surf_cobs_poly(I, 2)
         CALL SHOWIT(10)
         IF(surf_cobs_poly(I, 3).NE.0.0D0) THEN
            WRITE(OUTLYNE,1814)surf_cobs_poly(I, 3)
            CALL SHOWIT(10)
         END IF
         IF(surf_cobs_poly(I, 4).NE.0.0D0) THEN
            WRITE(OUTLYNE,1914)surf_cobs_poly(I, 4)
            CALL SHOWIT(10)
         END IF
         IF(surf_cobs_poly(I, 6).NE.0.0) THEN
            WRITE(OUTLYNE,1515) surf_cobs_poly(I, 6)
            CALL SHOWIT(10)
         END IF
      END IF
      IF(surf_coat_type(I).EQ.2.0) THEN
!       RECTANGULAR CLAP
917      FORMAT('REY OBS',1X,G23.15)
1817     FORMAT('REX OBS',1X,G23.15)
         WRITE(OUTLYNE,917)surf_cobs_poly(I, 1)
         CALL SHOWIT(10)
         WRITE(OUTLYNE,1817)surf_cobs_poly(I, 2)
         CALL SHOWIT(10)
         IF(surf_cobs_poly(I, 3).NE.0.0D0) THEN
            WRITE(OUTLYNE,1814)surf_cobs_poly(I, 3)
            CALL SHOWIT(10)
         END IF
         IF(surf_cobs_poly(I, 4).NE.0.0D0) THEN
            WRITE(OUTLYNE,1914)surf_cobs_poly(I, 4)
            CALL SHOWIT(10)
         END IF
         IF(surf_cobs_poly(I, 6).NE.0.0) THEN
            WRITE(OUTLYNE,1515) surf_cobs_poly(I, 6)
            CALL SHOWIT(10)
         END IF
      END IF
   END IF
!
!       TILTS AND DECENTRATIONS
   IF(surf_decenter_flag(I).NE.0.0D0) THEN
!       SURFACE DECENTER
      WRITE(OUTLYNE,40) surf_focus_dx(I)
      CALL SHOWIT(10)
      WRITE(OUTLYNE,41) surf_focus_dy(I)
      CALL SHOWIT(10)
      WRITE(OUTLYNE,42) surf_focus_dz(I)
      CALL SHOWIT(10)
   END IF
   IF(surf_tilt_flag(I).NE.0.0D0) THEN
!       TILTS
      IF(surf_tilt_flag(I).EQ.4.0D0) THEN
51       FORMAT('BEN')
         WRITE(OUTLYNE,51)
         CALL SHOWIT(10)
      END IF
      IF(surf_tilt_flag(I).EQ.5.0D0) THEN
52       FORMAT('DAR')
         WRITE(OUTLYNE,52)
         CALL SHOWIT(10)
      END IF
      IF(surf_tilt_flag(I).EQ.7.0D0) THEN
545      FORMAT('REV')
         WRITE(OUTLYNE,545)
         CALL SHOWIT(10)
      END IF
      WRITE(OUTLYNE,43) surf_alpha_deg(I)
      CALL SHOWIT(10)
      WRITE(OUTLYNE,44) surf_beta_deg(I)
      CALL SHOWIT(10)
      WRITE(OUTLYNE,45) surf_gamma_deg(I)
      CALL SHOWIT(10)
      IF(surf_tilt_flag(I).EQ.6.0D0) THEN
53       FORMAT('RET ,',G23.15)
         WRITE(OUTLYNE,53) surf_ret_surf_num(I)
         CALL SHOWIT(10)
      END IF
   END IF
!
40 FORMAT('XDE',1X,G23.15)
41 FORMAT('YDE',1X,G23.15)
42 FORMAT('ZDE',1X,G23.15)
43 FORMAT('ADE',1X,G23.15)
44 FORMAT('BDE',1X,G23.15)
45 FORMAT('CDE',1X,G23.15)
!
!
!       NOW ASTOP AND REF
   IF(I.EQ.INT(sys_astop())) THEN
!       I IS THE ASTOP SURFACE, IS THERE AN ADJUSTMENT ?
      WRITE(OUTLYNE,95)
95    FORMAT('STOP')
   END IF
   IF(I.EQ.INT(sys_ref_surf())) THEN
!       SURFACE SHOULD BE THE REFERENCE SURFACE
      WRITE(OUTLYNE,95)
      CALL SHOWIT(10)
   END IF
!
   RETURN
END
! SUB LENHDAC.FOR
SUBROUTINE LENHDAC
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_sax, sys_say, sys_scx, sys_scx_fang_set, sys_scy, &
      & sys_scy_fang_set, sys_units, sys_wavelength, sys_wl_pri1, sys_wl_pri2, &
      & sys_wl_ref, sys_wl_sec1, sys_wl_sec2, sys_x1_scx, sys_y1_scy
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       SUBROUTINE LENHDAC HANDEL LENS HEADER INFO OUTPUT
!       DURING A LENO AC. ACTS ON THE CURRENT LENS
!
   INTEGER I
!
!
!       OUTPUT THE HEADER COMMAND (LENS)
!

   WRITE(OUTLYNE,10)
   CALL SHOWIT(10)
!
!
!       NOW THE LI
!

   IF(LI(1:20).NE.'                    ')WRITE(OUTLYNE,20) LI(1:79)
   IF(LI(1:20).NE.'                    ') CALL SHOWIT(10)
!
!       NOW LIC
!
   DO I=1,LICCNT
      IF(LIC(I)(1:20).NE.'                    ') THEN
         WRITE(OUTLYNE,21) LIC(I)
         CALL SHOWIT(10)
      END IF
   END DO
!
!       NOW WV
!

   WRITE(OUTLYNE,22)sys_wavelength(1),sys_wavelength(2),sys_wavelength(3),sys_wavelength(4),sys_wavelength(5)
   CALL SHOWIT(10)
!
!
!       NOW UNITS

   IF(sys_units().EQ.1.0) WRITE(OUTLYNE,23)
   IF(sys_units().EQ.2.0) WRITE(OUTLYNE,24)
   IF(sys_units().EQ.3.0) WRITE(OUTLYNE,25)
   IF(sys_units().EQ.4.0) WRITE(OUTLYNE,33)
   CALL SHOWIT(10)
!
!       NOW PRIMARY WAVELENGTH PAIR

   WRITE(OUTLYNE,26) INT(sys_wl_pri1()),INT(sys_wl_pri2())
   CALL SHOWIT(10)
!
!       SECONDARY WAVELENGTH PAIR

   WRITE(OUTLYNE,27) INT(sys_wl_sec1()),INT(sys_wl_sec2())
   CALL SHOWIT(10)
!
!       CONTROL WAVELENGTH

   WRITE(OUTLYNE,28) INT(sys_wl_ref())
   CALL SHOWIT(10)
!
!
!       SAY

   WRITE(OUTLYNE,29) sys_say()
   CALL SHOWIT(10)
!
!       SAX
   IF(sys_sax().NE.sys_say()) THEN
      WRITE(OUTLYNE,29) sys_sax()
      CALL SHOWIT(10)
      WRITE(OUTLYNE,30) sys_sax()
      CALL SHOWIT(10)
   END IF
!
!       SCY (SCY FANG IS NOT DIRECTLY PASSED BY LENO)
   WRITE(OUTLYNE,31) sys_scy(),sys_y1_scy()
   CALL SHOWIT(10)
!
   IF(sys_scy().NE.sys_scx().OR.sys_y1_scy().NE.sys_x1_scx()) THEN
!     DO SCX
!       SCX (SCX FANG IS NOT DIRECTLY PASSED BY LENO)
      WRITE(OUTLYNE,31) sys_scx(),sys_x1_scx()
      CALL SHOWIT(10)
      WRITE(OUTLYNE,32) sys_scx(),sys_x1_scx()
      CALL SHOWIT(10)
   END IF
!
10 FORMAT('LENS')
20 FORMAT('LI,',A79)
21 FORMAT('LIC,',A79)
121 FORMAT('INI,',A79)
1212 FORMAT('LTYPE,',A5)
!
22 FORMAT('WV,',E15.7,',',E15.7,',',E15.7,',',E15.7 ,',',E15.7)
23 FORMAT('UNITS IN')
24 FORMAT('UNITS CM')
25 FORMAT('UNITS MM')
33 FORMAT('UNITS METERS')
26 FORMAT('PCW,',I2,',',I2)
27 FORMAT('SCW,',I2,',',I2)
28 FORMAT('CW,',I2)
29 FORMAT('SAY,',E15.7)
30 FORMAT('SAX,',E15.7)
31 FORMAT('SCY,',E15.7,',',E15.7)
32 FORMAT('SCX,',E15.7,',',E15.7)
   RETURN
END
! SUB LENEDAC.FOR
SUBROUTINE LENEDAC
!
   use DATCFG
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_mode, sys_wl_weight
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       SUBROUTINE LENED HANDEL LENS TRAILING INFO OUTPUT
!       DURING A LENO AC. ACTS ON THE CURRENT LENS.
!       USED AFTER SURFACE DATA OUTPUT
!
   INTEGER SS,I,J
!
!
!       THE TERMINAL RECORD FOR AND LENO IS EOS
!
   WRITE(OUTLYNE,10)
   CALL SHOWIT(10)
10 FORMAT('EOS')
!**********************************************************

!       MODE SETTING
   IF(sys_mode().EQ.1) WRITE(OUTLYNE,33)
   IF(sys_mode().EQ.2) WRITE(OUTLYNE,34)
   IF(sys_mode().EQ.3) WRITE(OUTLYNE,35)
   IF(sys_mode().EQ.4) WRITE(OUTLYNE,36)
   CALL SHOWIT(10)
33 FORMAT('MODE FOCAL')
34 FORMAT('MODE UFOCAL')
35 FORMAT('MODE AFOCAL')
36 FORMAT('MODE UAFOCAL')
!
!       SPTWT

   WRITE(OUTLYNE,3000) sys_wl_weight(1),sys_wl_weight(2),sys_wl_weight(3),sys_wl_weight(4),sys_wl_weight(5)
   CALL SHOWIT(10)
3000 FORMAT('SPTWT,',E15.7,',',E15.7,',',E15.7,',',E15.7,',',E15.7)
   RETURN
END
! SUB LENSFAC.FOR
SUBROUTINE LENSFAC(I)
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_astop, sys_astop_adj, sys_ref_surf
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       SUBROUTINE LENSF HANDELS LENS SURFACE INFO OUTPUT
!       DURING A LENO AC AND
!       ACTS ON THE CURRENT LENS, ONLY
!       ONE SURFACE AT A TIME.
!       THE VARIABLE I PASSES THE SURFACE
!       NUMBER OF INTEREST
!
   INTEGER I,IG,J
!
!
!       THE CURRENT SURFACE IS PASSED WITH I
!       A DUMMY SURFACE HAS AIR AS ITS MATERIAL AND HAS
!       AIR AS THE MATERIAL INFRONT OF IT.
!
!       WE NOW HAVE LENO AC, PROCEED
!
!               CURVATURE

   WRITE(OUTLYNE,10) surf_curvature(I)
   CALL SHOWIT(10)
10 FORMAT('CV,',E15.7)
!
!       ALL DONE WITH CURVATURE
!
   IF(surf_conic(I).NE.0.0) THEN
!               CONIC CONSTANT

      WRITE(OUTLYNE,11) surf_conic(I)
      CALL SHOWIT(10)
11    FORMAT('CC,',E15.7)
   END IF

!               THICKNESS

   WRITE(OUTLYNE,12) surf_thickness((I))
   CALL SHOWIT(10)
12 FORMAT('TH,',E15.7)
!
!               ASPHERICS
   IF(surf_is_asphere(I)) THEN

!     LONG FORM
      WRITE(OUTLYNE,13) surf_asphere_coeff(I, 4),surf_asphere_coeff(I, 6),surf_asphere_coeff(I, 8),surf_asphere_coeff(I, 10)
      CALL SHOWIT(10)
13    FORMAT('ASPH,',E15.7,',',E15.7,',',E15.7,',',E15.7)
   END IF
!               CLEAR APERTURE
   IF(surf_clap_type(I).NE.0.0D0) THEN
      IF(surf_clap_type(I).EQ.1.0) THEN
!       CIRCULAR CLAP

14       FORMAT('CLAP,',E15.7)
         WRITE(OUTLYNE,14)surf_clap_dim(I, 1)
         CALL SHOWIT(10)
      END IF
      IF(surf_clap_type(I).EQ.3.0) THEN
!       ELLIPTICAL CLAP

16       FORMAT('CLAP ELIP,',E15.7,',',E15.7)
         WRITE(OUTLYNE,16)surf_clap_dim(I, 1),surf_clap_dim(I, 2)
         CALL SHOWIT(10)
      END IF
      IF(surf_clap_type(I).EQ.2.0) THEN
!       RECTANGULAR CLAP
17       FORMAT('CLAP RECT,',E15.7,',',E15.7)
         WRITE(OUTLYNE,17)surf_clap_dim(I, 1),surf_clap_dim(I, 2)
         CALL SHOWIT(10)
      END IF
   END IF
!     OBSCURATIONS
   IF(surf_coat_type(I).NE.0.0) THEN
      IF(surf_coat_type(I).EQ.1.0) THEN
!       CIRCULAR COBS
25       FORMAT('COBS,',E15.7)
         WRITE(OUTLYNE,25)surf_cobs_poly(I, 1)
         CALL SHOWIT(10)
      END IF
      IF(surf_coat_type(I).EQ.3.0) THEN
!       ELLIPTICAL COBS
26       FORMAT('COBS ELIP,',E15.7,',',E15.7)
         WRITE(OUTLYNE,26)surf_cobs_poly(I, 1),surf_cobs_poly(I, 2)
         CALL SHOWIT(10)
      END IF
      IF(surf_coat_type(I).EQ.2.0) THEN
!       RECTANGULAR COBS
28       FORMAT('COBS RECT,',E15.7,',',E15.7)
         WRITE(OUTLYNE,28)surf_cobs_poly(I, 1),surf_cobs_poly(I, 2)
         CALL SHOWIT(10)
      END IF
   END IF
!
!
!       NOW TORICS IF PRESENT
!
   IF(surf_toric_flag(I).NE.0.0) THEN
!       TORICS PRESENT
      IF(surf_toric_flag(I).EQ.1.0) THEN

         WRITE(OUTLYNE,36)
         CALL SHOWIT(10)

         WRITE(OUTLYNE,38) surf_toric_curvature(I)
         CALL SHOWIT(10)
      END IF
      IF(surf_toric_flag(I).EQ.2.0) THEN

         WRITE(OUTLYNE,37)
         CALL SHOWIT(10)

         WRITE(OUTLYNE,39) surf_toric_curvature(I)
         CALL SHOWIT(10)
      END IF
   END IF
36 FORMAT('YTORIC')
38 FORMAT('CVX,',E15.7)
37 FORMAT('XTORIC  ')
39 FORMAT('CVY,',E15.7)
!
!
!       TILTS AND DECENTRATIONS
   IF(surf_decenter_flag(I).NE.0.0D0) THEN
!       SURFACE DECENTER
      WRITE(OUTLYNE,106) surf_focus_dy(I),surf_focus_dx(I)
      CALL SHOWIT(10)
106   FORMAT('DEC,',E15.7,',',E15.7)
   END IF
   IF(surf_tilt_flag(I).EQ.1.0D0.AND.surf_tilt_return_flag(I).EQ.0.0D0.OR.surf_tilt_flag(I).EQ.-1.0D0) THEN
!       TILTS
      IF(surf_tilt_flag(I).EQ.1.0D0.AND.surf_tilt_return_flag(I).EQ.0.0D0)WRITE(OUTLYNE,102) surf_alpha_deg(I),surf_beta_deg(I),surf_gamma_deg(I)
      IF(surf_tilt_flag(I).EQ.-1.0D0)WRITE(OUTLYNE,103) surf_alpha_deg(I),surf_beta_deg(I),surf_gamma_deg(I)
      CALL SHOWIT(10)
!
102   FORMAT('TILT,',E15.7,',',E15.7,',',E15.7)
103   FORMAT('RTILT,',E15.7,',',E15.7,',',E15.7)
!
   END IF
!
!       NOW ASTOP AND REF
   IF(I.EQ.INT(sys_ref_surf())) THEN
!       SURFACE SHOULD BE THE REFERENCE SURFACE

      WRITE(OUTLYNE,94)
      CALL SHOWIT(10)
   END IF
   IF(I.EQ.INT(sys_astop())) THEN
!       I IS THE ASTOP SURFACE, IS THERE AN ADJUSTMENT ?

      IF(sys_astop_adj().EQ.0.0)  WRITE(OUTLYNE,95)
      IF(sys_astop_adj().EQ.1.0)  WRITE(OUTLYNE,96)
      IF(sys_astop_adj().EQ.-1.0) WRITE(OUTLYNE,97)
      IF(sys_astop_adj().EQ.2.0)  WRITE(OUTLYNE,98)
      IF(sys_astop_adj().GE.-1.0D0.AND.sys_astop_adj().LE.2.0D0) CALL SHOWIT(10)
94    FORMAT('REFS')
95    FORMAT('ASTOP')
96    FORMAT('ASTOP    EN')
97    FORMAT('ASTOP    EX')
98    FORMAT('ASTOP    ENEX')
   END IF
!
   IF(GLANAM(I,1).EQ.' '.AND.GLANAM(I,2).EQ.'AIR'.OR.GLANAM(I,1).EQ.' '.AND.GLANAM(I,2).EQ.'LAST SURFACE') THEN
      WRITE(OUTLYNE,110)
      CALL SHOWIT(10)
      RETURN
   END IF
   IF(GLANAM(I,1).EQ.' '.AND.GLANAM(I,2).EQ.'REFL') THEN
      WRITE(OUTLYNE,120)
      CALL SHOWIT(10)
      RETURN
   END IF
   GLANAM(I,1)='GLAK'
   DO J=1,6
      IF(GLANAM(I,2)(J:J).EQ.' ') THEN
         IG=J
         GO TO 101
      END IF
   END DO
101 CONTINUE
   IF(GLANAM(I,1)(1:5).EQ.'GLAK') THEN
      IF(IG.EQ.1)WRITE(OUTLYNE,201)GLANAM(I,1)(1:6),GLANAM(I,2)(1:IG),surf_refractive_index(I, 1),surf_refractive_index(I, 2),surf_refractive_index(I, 3),surf_refractive_index(I, 4),surf_refractive_index(I, 5)
      IF(IG.EQ.2)WRITE(OUTLYNE,202)GLANAM(I,1)(1:6),GLANAM(I,2)(1:IG),surf_refractive_index(I, 1),surf_refractive_index(I, 2),surf_refractive_index(I, 3),surf_refractive_index(I, 4),surf_refractive_index(I, 5)
      IF(IG.EQ.3)WRITE(OUTLYNE,203)GLANAM(I,1)(1:6),GLANAM(I,2)(1:IG),surf_refractive_index(I, 1),surf_refractive_index(I, 2),surf_refractive_index(I, 3),surf_refractive_index(I, 4),surf_refractive_index(I, 5)
      IF(IG.EQ.4)WRITE(OUTLYNE,204)GLANAM(I,1)(1:6),GLANAM(I,2)(1:IG),surf_refractive_index(I, 1),surf_refractive_index(I, 2),surf_refractive_index(I, 3),surf_refractive_index(I, 4),surf_refractive_index(I, 5)
      IF(IG.EQ.5)WRITE(OUTLYNE,205)GLANAM(I,1)(1:6),GLANAM(I,2)(1:IG),surf_refractive_index(I, 1),surf_refractive_index(I, 2),surf_refractive_index(I, 3),surf_refractive_index(I, 4),surf_refractive_index(I, 5)
      IF(IG.EQ.6)WRITE(OUTLYNE,206)GLANAM(I,1)(1:6),GLANAM(I,2)(1:IG),surf_refractive_index(I, 1),surf_refractive_index(I, 2),surf_refractive_index(I, 3),surf_refractive_index(I, 4),surf_refractive_index(I, 5)
      CALL SHOWIT(10)
   END IF
201 FORMAT(A6,' ',A1,',',E15.7,',',E15.7,',',E15.7,',',E15.7,',',E15.7)
202 FORMAT(A6,' ',A2,',',E15.7,',',E15.7,',',E15.7,',',E15.7,',',E15.7)
203 FORMAT(A6,' ',A3,',',E15.7,',',E15.7,',',E15.7,',',E15.7,',',E15.7)
204 FORMAT(A6,' ',A4,',',E15.7,',',E15.7,',',E15.7,',',E15.7,',',E15.7)
205 FORMAT(A6,' ',A5,',',E15.7,',',E15.7,',',E15.7,',',E15.7,',',E15.7)
206 FORMAT(A6,' ',A6,',',E15.7,',',E15.7,',',E15.7,',',E15.7,',',E15.7)
110 FORMAT('AIR')
120 FORMAT('REFL')
   RETURN
END
!     LENS SURFACES
! SUB RLENSF.FOR
SUBROUTINE RLENSF(I)
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_astop, sys_astop_adj, sys_ref_surf
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       SUBROUTINE RLENSF HANDELS LENS SURFACE INFO OUTPUT
!       DURING A LENO REVERSE AND
!       ACTS ON THE CURRENT LENS, ONLY
!       ONE SURFACE AT A TIME.
!       THE VARIABLE I PASSES THE SURFACE
!       NUMBER OF INTEREST
!       ALL SOLVES,PIKUPS AND TILTS/DECENTRATIONS ARE IGNORED
!
   INTEGER I,J
   CHARACTER*3 AALL
!
   real(real64) AVAV
!
!
!       THE CURRENT SURFACE IS PASSED WITH I
!       A DUMMY SURFACE HAS AIR AS ITS MATERIAL AND HAS
!       AIR AS THE MATERIAL INFRONT OF IT.
!
!       WE NOW HAVE LENO REVERSE, PROCEED
!     SURFACE LABEL
   IF(surf_label_flag(I).EQ.1.0D0) THEN
      WRITE(OUTLYNE,400) LBL(I)(1:74)
      CALL SHOWIT(10)
400   FORMAT('LBL ,',A74)
   END IF
!     COATING NUMBER
   WRITE(OUTLYNE,300) DBLE(INT(surf_coating_index(I)))
   CALL SHOWIT(10)
300 FORMAT('COATING ,',G23.15)
!               CURVATURE
   WRITE(OUTLYNE,10) -surf_curvature(I)
   CALL SHOWIT(10)
10 FORMAT('CV      ,',G23.15)
   IF(surf_conic(I).NE.0.0) THEN
!               CONIC CONSTANT
      WRITE(OUTLYNE,11) surf_conic(I)
      CALL SHOWIT(10)
11    FORMAT('CC      ,',G23.15)
   END IF
!               THICKNESS
   IF(I.NE.0) WRITE(OUTLYNE,12) surf_thickness((I-1))
   IF(I.NE.0) CALL SHOWIT(10)
   AVAV=0.0D0
   IF(I.EQ.0) WRITE(OUTLYNE,12) AVAV
   IF(I.NE.0) CALL SHOWIT(10)
12 FORMAT('TH      ,',G23.15)
!
!
!       DEFORM
   IF(surf_default_flag(I).EQ.1.0D0) THEN
      IF(surf_mtracei_nx(I).EQ.1.0D0) AALL='F01'
      IF(surf_mtracei_nx(I).EQ.2.0D0) AALL='F02'
      IF(surf_mtracei_nx(I).EQ.3.0D0) AALL='F03'
      IF(surf_mtracei_nx(I).EQ.4.0D0) AALL='F04'
      IF(surf_mtracei_nx(I).EQ.5.0D0) AALL='F05'
      IF(surf_mtracei_nx(I).EQ.6.0D0) AALL='F06'
      IF(surf_mtracei_nx(I).EQ.7.0D0) AALL='F07'
      IF(surf_mtracei_nx(I).EQ.8.0D0) AALL='F08'
      IF(surf_mtracei_nx(I).EQ.9.0D0) AALL='F09'
      IF(surf_mtracei_nx(I).EQ.10.0D0) AALL='F10'
      WRITE(OUTLYNE,1218) AALL,surf_mtracei_ny(I),surf_psfbin_data(I)
      CALL SHOWIT(10)
1218  FORMAT('DEFORM   ',A3,1X,G23.15,',',G23.15)
   END IF
!       LENS ARRAY
   IF(surf_array_parity(I).NE.0.0D0) THEN
      IF(surf_array_parity(I).EQ.-1.0D0)WRITE(OUTLYNE,1219) surf_array_dx(I),surf_array_dy(I)
      IF(surf_array_parity(I).EQ.1.0D0)WRITE(OUTLYNE,1220) surf_array_dx(I),surf_array_dy(I)
      CALL SHOWIT(10)
1219  FORMAT('ARRAY ODD',G23.15,',',G23.15)
1220  FORMAT('ARRAY EVEN',G23.15,',',G23.15)
   END IF
!               GRATING

   IF(surf_diffraction_flag(I).EQ.1.0D0) THEN
      WRITE(OUTLYNE,1212)
1212  FORMAT('GRT')
      CALL SHOWIT(10)
      WRITE(OUTLYNE,1213) surf_grating_order(I)
1213  FORMAT('GRO',G23.15)
      CALL SHOWIT(10)
      WRITE(OUTLYNE,1214) surf_grating_spacing(I)
1214  FORMAT('GRS',G23.15)
      CALL SHOWIT(10)
      WRITE(OUTLYNE,1215) surf_grating_vx(I)
1215  FORMAT('GRX',G23.15)
      CALL SHOWIT(10)
      WRITE(OUTLYNE,1216) surf_grating_vy(I)
1216  FORMAT('GRY',G23.15)
      CALL SHOWIT(10)
      WRITE(OUTLYNE,1217) surf_grating_vz(I)
1217  FORMAT('GRZ',G23.15)
      CALL SHOWIT(10)
   END IF
!
!               ASPHERICS
   IF(surf_is_asphere(I)) THEN
!     LONG FORM
      WRITE(OUTLYNE,13) -surf_asphere_coeff(I, 4),-surf_asphere_coeff(I, 6),-surf_asphere_coeff(I, 8),-surf_asphere_coeff(I, 10),-surf_asphere_coeff(I, 2)
      CALL SHOWIT(10)
13    FORMAT('ASPH    ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,113) -surf_asphere_coeff(I, 12),-surf_asphere_coeff(I, 14),-surf_asphere_coeff(I, 16),-surf_asphere_coeff(I, 18),-surf_asphere_coeff(I, 20)
      CALL SHOWIT(10)
113   FORMAT('ASPH2   ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
   END IF
!               CLEAR APERTURE
   IF(surf_clap_type(I).EQ.1.0) THEN
!       CIRCULAR CLAP
14    FORMAT('CLAP    ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
      IF(surf_clap_dim(I, 2) == 0.0D0) call set_surf_clap_dim(I, 2, surf_clap_dim(I, 1))
      WRITE(OUTLYNE,14)surf_clap_dim(I, 1),surf_clap_dim(I, 3),surf_clap_dim(I, 4),surf_clap_dim(I, 2),surf_clap_dim(I, 5)
      CALL SHOWIT(10)
   END IF
   IF(surf_clap_type(I).EQ.3.0) THEN
!       ELLIPTICAL CLAP
16    FORMAT('CLAP     ELIP    ,',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,16)surf_clap_dim(I, 1),surf_clap_dim(I, 2),surf_clap_dim(I, 3),surf_clap_dim(I, 4)
      CALL SHOWIT(10)
      IF(surf_clap_tilt(I).NE.0.0) THEN
         WRITE(OUTLYNE,15) surf_clap_tilt(I)
         CALL SHOWIT(10)
      END IF
15    FORMAT('CLAP     TILT    ,',G23.15)
   END IF
   IF(surf_clap_type(I).EQ.2.0) THEN
!       RECTANGULAR CLAP
17    FORMAT('CLAP     RECT    ,',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,17)surf_clap_dim(I, 1),surf_clap_dim(I, 2),surf_clap_dim(I, 3),surf_clap_dim(I, 4)
      CALL SHOWIT(10)
      IF(surf_clap_tilt(I).NE.0.0) THEN
         WRITE(OUTLYNE,15) surf_clap_tilt(I)
         CALL SHOWIT(10)
      END IF
   END IF
   IF(surf_clap_type(I).EQ.4.0) THEN
!       RACETRACK CLAP
18    FORMAT('CLAP     RCTK    ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,18)surf_clap_dim(I, 1),surf_clap_dim(I, 2),surf_clap_dim(I, 3),surf_clap_dim(I, 4),surf_clap_dim(I, 5)
      CALL SHOWIT(10)
      IF(surf_clap_tilt(I).NE.0.0) THEN
         WRITE(OUTLYNE,15) surf_clap_tilt(I)
         CALL SHOWIT(10)
      END IF
   END IF
   IF(surf_clap_type(I).EQ.5.0) THEN
!       POLY CLAP
181   FORMAT('CLAP     POLY    ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,181)surf_clap_dim(I, 1),surf_clap_dim(I, 2),surf_clap_dim(I, 3),surf_clap_dim(I, 4),surf_clap_dim(I, 5)
      CALL SHOWIT(10)
      IF(surf_clap_tilt(I).NE.0.0) THEN
         WRITE(OUTLYNE,15) surf_clap_tilt(I)
         CALL SHOWIT(10)
      END IF
   END IF
   IF(surf_clap_type(I).EQ.6.0) THEN
!       POLY ICLAP
182   FORMAT('CLAP     IPOLY   ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,182)surf_clap_dim(I, 1),surf_clap_dim(I, 2),surf_clap_dim(I, 3),surf_clap_dim(I, 4),surf_clap_dim(I, 5)
      CALL SHOWIT(10)
      IF(surf_clap_tilt(I).NE.0.0) THEN
         WRITE(OUTLYNE,15) surf_clap_tilt(I)
         CALL SHOWIT(10)
      END IF
   END IF
!
!     MULTCLAP
   IF(surf_multi_clap_flag(I).NE.0.0D0) THEN
      DO J=1,INT(surf_multi_clap_flag(I))
         WRITE(OUTLYNE,1818) J,MULTCLAP(J,1,I),MULTCLAP(J,2,I),MULTCLAP(J,3,I)
         CALL SHOWIT(10)
1818     FORMAT('MULTCLAP ',I4,',',G23.15,',',G23.15,',',G23.15)
      END DO
   END IF
!
   IF(surf_cobs_ape_type(I).EQ.1.0) THEN
!       CIRCULAR ERASE CLAP
20    FORMAT('CLAP     ERASE   ,',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,20)surf_cobs_ape_data(I, 1),surf_cobs_ape_data(I, 3),surf_cobs_ape_data(I, 4)
      CALL SHOWIT(10)
   END IF
   IF(surf_cobs_ape_type(I).EQ.3.0) THEN
!       ELLIPTICAL ERASE CLAP
21    FORMAT('CLAP     ELIPE   ,',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,21)surf_cobs_ape_data(I, 1),surf_cobs_ape_data(I, 2),surf_cobs_ape_data(I, 3),surf_cobs_ape_data(I, 4)
      CALL SHOWIT(10)
      IF(surf_cobs_ape_data(I, 6).NE.0.0) THEN
         WRITE(OUTLYNE,15) surf_cobs_ape_data(I, 6)
         CALL SHOWIT(10)
      END IF
   END IF
   IF(surf_cobs_ape_type(I).EQ.2.0) THEN
!       RECTANGULAR ERACE CLAP
22    FORMAT('CLAP     RECTE   ,',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,22)surf_cobs_ape_data(I, 1),surf_cobs_ape_data(I, 2),surf_cobs_ape_data(I, 3),surf_cobs_ape_data(I, 4)
      CALL SHOWIT(10)
      IF(surf_cobs_ape_data(I, 6).NE.0.0) THEN
         WRITE(OUTLYNE,15) surf_cobs_ape_data(I, 6)
         CALL SHOWIT(10)
      END IF
   END IF
   IF(surf_cobs_ape_type(I).EQ.4.0) THEN
!       RACETRACK ERASE CLAP
23    FORMAT('CLAP     RCTKE   ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,23)surf_cobs_ape_data(I, 1),surf_cobs_ape_data(I, 2),surf_cobs_ape_data(I, 3),surf_cobs_ape_data(I, 4),surf_cobs_ape_data(I, 5)
      CALL SHOWIT(10)
      IF(surf_cobs_ape_data(I, 6).NE.0.0) THEN
         WRITE(OUTLYNE,15) surf_cobs_ape_data(I, 6)
         CALL SHOWIT(10)
      END IF
   END IF
   IF(surf_cobs_ape_type(I).EQ.5.0) THEN
!       POLY ERASE CLAP
231   FORMAT('CLAP     POLYE   ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,231)surf_cobs_ape_data(I, 1),surf_cobs_ape_data(I, 2),surf_cobs_ape_data(I, 3),surf_cobs_ape_data(I, 4),surf_cobs_ape_data(I, 5)
      CALL SHOWIT(10)
      IF(surf_cobs_ape_data(I, 6).NE.0.0) THEN
         WRITE(OUTLYNE,15) surf_cobs_ape_data(I, 6)
         CALL SHOWIT(10)
      END IF
   END IF
   IF(surf_cobs_ape_type(I).EQ.6.0) THEN
!       IPOLY ERASE CLAP
232   FORMAT('CLAP     IPOLYE  ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,232)surf_cobs_ape_data(I, 1),surf_cobs_ape_data(I, 2),surf_cobs_ape_data(I, 3),surf_cobs_ape_data(I, 4),surf_cobs_ape_data(I, 5)
      CALL SHOWIT(10)
      IF(surf_cobs_ape_data(I, 6).NE.0.0) THEN
         WRITE(OUTLYNE,15) surf_cobs_ape_data(I, 6)
         CALL SHOWIT(10)
      END IF
   END IF
!               OBSCURATIONS
   IF(surf_coat_type(I).NE.0.0) THEN
      IF(surf_coat_type(I).EQ.1.0) THEN
!       CIRCULAR COBS
25       FORMAT('COBS    ,',G23.15,',',G23.15,',',G23.15)
         WRITE(OUTLYNE,25)surf_cobs_poly(I, 1),surf_cobs_poly(I, 3),surf_cobs_poly(I, 4)
         CALL SHOWIT(10)
      END IF
      IF(surf_coat_type(I).EQ.3.0) THEN
!       ELLIPTICAL COBS
26       FORMAT('COBS     ELIP    ,',G23.15,',',G23.15,',',G23.15,',',G23.15)
         WRITE(OUTLYNE,26)surf_cobs_poly(I, 1),surf_cobs_poly(I, 2),surf_cobs_poly(I, 3),surf_cobs_poly(I, 4)
         CALL SHOWIT(10)
         IF(surf_cobs_poly(I, 6).NE.0.0) THEN
            WRITE(OUTLYNE,27) surf_cobs_poly(I, 6)
            CALL SHOWIT(10)
         END IF
27       FORMAT('COBS     TILT    ,',G23.15)
      END IF
      IF(surf_coat_type(I).EQ.2.0D0.AND.surf_spider_flag(I).EQ.0.0D0) THEN
!       RECTANGULAR COBS
28       FORMAT('COBS     RECT    ,',G23.15,',',G23.15,',',G23.15,',',G23.15)
         WRITE(OUTLYNE,28)surf_cobs_poly(I, 1),surf_cobs_poly(I, 2),surf_cobs_poly(I, 3),surf_cobs_poly(I, 4)
         CALL SHOWIT(10)
         IF(surf_cobs_poly(I, 6).NE.0.0) THEN
            WRITE(OUTLYNE,27) surf_cobs_poly(I, 6)
            CALL SHOWIT(10)
         END IF
      END IF
!       NOT RECTANGULAR COBS,PROCEED
   END IF
   IF(surf_coat_type(I).EQ.4.0) THEN
!       RACETRACK COBS
29    FORMAT('COBS     RCTK    ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,29)surf_cobs_poly(I, 1),surf_cobs_poly(I, 2),surf_cobs_poly(I, 3),surf_cobs_poly(I, 4),surf_cobs_poly(I, 5)
      CALL SHOWIT(10)
      IF(surf_cobs_poly(I, 6).NE.0.0) THEN
         WRITE(OUTLYNE,27) surf_cobs_poly(I, 6)
         CALL SHOWIT(10)
      END IF
   END IF
   IF(surf_coat_type(I).EQ.5.0) THEN
!       POLY COBS
291   FORMAT('COBS     POLY    ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,291)surf_cobs_poly(I, 1),surf_cobs_poly(I, 2),surf_cobs_poly(I, 3),surf_cobs_poly(I, 4),surf_cobs_poly(I, 5)
      CALL SHOWIT(10)
      IF(surf_cobs_poly(I, 6).NE.0.0) THEN
         WRITE(OUTLYNE,27) surf_cobs_poly(I, 6)
         CALL SHOWIT(10)
      END IF
   END IF
   IF(surf_coat_type(I).EQ.6.0) THEN
!       IPOLY COBS
292   FORMAT('COBS     IPOLY   ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,292)surf_cobs_poly(I, 1),surf_cobs_poly(I, 2),surf_cobs_poly(I, 3),surf_cobs_poly(I, 4),surf_cobs_poly(I, 5)
      CALL SHOWIT(10)
      IF(surf_cobs_poly(I, 6).NE.0.0) THEN
         WRITE(OUTLYNE,27) surf_cobs_poly(I, 6)
         CALL SHOWIT(10)
      END IF
   END IF
!
!     MULTCOBS
   IF(surf_multi_cobs_flag(I).NE.0.0D0.AND.surf_spider_flag(I).EQ.0.0D0) THEN
      DO J=1,INT(surf_multi_cobs_flag(I))
         WRITE(OUTLYNE,1819) J,MULTCOBS(J,1,I),MULTCOBS(J,2,I),MULTCOBS(J,3,I)
         CALL SHOWIT(10)
1819     FORMAT('MULTCOBS ',I4,',',G23.15,',',G23.15,',',G23.15)
      END DO
   END IF
!
!     SPIDER
   IF(surf_spider_flag(I).NE.0.0D0) THEN
      WRITE(OUTLYNE,1820) surf_spider_arms(I),surf_spider_angle(I),surf_spider_width(I)
      CALL SHOWIT(10)
1820  FORMAT('SPIDER ,',G23.15,',',G23.15,',',G23.15)
   END IF
   IF(surf_cobs_era_type(I).EQ.1.0) THEN
!       CIRCULAR ERASE COBS
31    FORMAT('COBS     ERASE   ,',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,31)surf_cobs_era_data(I, 1),surf_cobs_era_data(I, 3),surf_cobs_era_data(I, 4)
      CALL SHOWIT(10)
   END IF
   IF(surf_cobs_era_type(I).EQ.3.0) THEN
!       ELLIPTICAL ERASE COBS
32    FORMAT('COBS     ELIPE   ,',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,32)surf_cobs_era_data(I, 1),surf_cobs_era_data(I, 2),surf_cobs_era_data(I, 3),surf_cobs_era_data(I, 4)
      CALL SHOWIT(10)
      IF(surf_cobs_era_data(I, 6).NE.0.0) THEN
         WRITE(OUTLYNE,27) surf_cobs_era_data(I, 6)
         CALL SHOWIT(10)
      END IF
   END IF
   IF(surf_cobs_era_type(I).EQ.2.0) THEN
!       RECTANGULAR ERACE COBS
33    FORMAT('COBS     RECTE   ,',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,33)surf_cobs_era_data(I, 1),surf_cobs_era_data(I, 2),surf_cobs_era_data(I, 3),surf_cobs_era_data(I, 4)
      CALL SHOWIT(10)
      IF(surf_cobs_era_data(I, 6).NE.0.0) THEN

         WRITE(OUTLYNE,27) surf_cobs_era_data(I, 6)
         CALL SHOWIT(10)
      END IF
   END IF
   IF(surf_cobs_era_type(I).EQ.4.0) THEN
!       RACETRACK ERASE CLAP
34    FORMAT('COBS     RCTKE   ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,34)surf_cobs_era_data(I, 1),surf_cobs_era_data(I, 2),surf_cobs_era_data(I, 3),surf_cobs_era_data(I, 4),surf_cobs_era_data(I, 5)
      CALL SHOWIT(10)
      IF(surf_cobs_era_data(I, 6).NE.0.0) THEN

         WRITE(OUTLYNE,27) surf_cobs_era_data(I, 6)
         CALL SHOWIT(10)
      END IF
   END IF
   IF(surf_cobs_era_type(I).EQ.5.0) THEN
!       POLY ERASE CLAP
341   FORMAT('COBS     POLYE   ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,341)surf_cobs_era_data(I, 1),surf_cobs_era_data(I, 2),surf_cobs_era_data(I, 3),surf_cobs_era_data(I, 4),surf_cobs_era_data(I, 5)
      CALL SHOWIT(10)
      IF(surf_cobs_era_data(I, 6).NE.0.0) THEN

         WRITE(OUTLYNE,27) surf_cobs_era_data(I, 6)
         CALL SHOWIT(10)
      END IF
   END IF
   IF(surf_cobs_era_type(I).EQ.6.0) THEN
!       IPOLY ERASE CLAP
342   FORMAT('COBS     IPOLYE  ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,342)surf_cobs_era_data(I, 1),surf_cobs_era_data(I, 2),surf_cobs_era_data(I, 3),surf_cobs_era_data(I, 4),surf_cobs_era_data(I, 5)
      CALL SHOWIT(10)
      IF(surf_cobs_era_data(I, 6).NE.0.0) THEN

         WRITE(OUTLYNE,27) surf_cobs_era_data(I, 6)
         CALL SHOWIT(10)
      END IF
   END IF
!
!
!       NOW TORICS IF PRESENT
!
   IF(surf_toric_flag(I).NE.0.0) THEN
!       TORICS PRESENT
      IF(surf_toric_flag(I).EQ.1.0) THEN
         WRITE(OUTLYNE,36)
         CALL SHOWIT(10)
         WRITE(OUTLYNE,38) -surf_toric_curvature(I)
         CALL SHOWIT(10)
      END IF
      IF(surf_toric_flag(I).EQ.2.0) THEN
         WRITE(OUTLYNE,37)
         CALL SHOWIT(10)
         WRITE(OUTLYNE,39) -surf_toric_curvature(I)
         CALL SHOWIT(10)
      END IF
   END IF
36 FORMAT('YTORIC  ')
38 FORMAT('CVTOR   ,',G23.15)
37 FORMAT('XTORIC  ')
39 FORMAT('CVTOR   ,',G23.15)
!
!       ROOF, CORNER CUBE AND RAYERROR SURFACES
   IF(surf_ccr_flag(I).EQ.1.0D0) THEN
      WRITE(OUTLYNE,1121) surf_roof_a(I),surf_roof_angle_err(I)
      CALL SHOWIT(10)
   END IF
   IF(surf_ccr_flag(I).EQ.2.0D0) THEN
      WRITE(OUTLYNE,1122) surf_roof_a(I),surf_roof_angle_err(I),surf_roof_b(I),surf_ccr_angle_err2(I)
      CALL SHOWIT(10)
   END IF
   WRITE(OUTLYNE,1123) surf_ray_error(I)
   CALL SHOWIT(10)
1121 FORMAT('ROO ',G23.15,',',G23.15)
1122 FORMAT('CCR ',G23.15,',',G23.15,',',G23.15,',',G23.15)
1123 FORMAT('RAYERROR',G23.15)
!
!       ANAMORPHIC CONIC AND ASPHERIC TERMS
!
   IF(surf_anamorphic_conic(I).NE.0.0) THEN
      WRITE(OUTLYNE,40) surf_anamorphic_conic(I)
      CALL SHOWIT(10)
40    FORMAT('CCTOR   ,',G23.15)
   END IF
   IF(surf_anamorphic_flag(I).NE.0.0) THEN
!       ANAMORPHIC ASPHERICS
      WRITE(OUTLYNE,41) -surf_anamorphic_coeff(I, 4),-surf_anamorphic_coeff(I, 6),-surf_anamorphic_coeff(I, 8),-surf_anamorphic_coeff(I, 10)
      CALL SHOWIT(10)
41    FORMAT('TASPH   ,',G23.15,',',G23.15,',',G23.15,',',G23.15)
   END IF
!
!       REAL OR PARAXAIL SURFACE
   IF(surf_paraxial_val(I).EQ.1.0D0) THEN
!       PARAXIAL SURFACE
      WRITE(OUTLYNE,4711)
      CALL SHOWIT(10)
4711  FORMAT('PARAX')
   END IF
!
!       SPECIFIC GRAVITY
   WRITE(OUTLYNE,4701) DABS(surf_spgr(I))
   IF(surf_spgr(I).NE.0.0D0)CALL SHOWIT(10)
4701 FORMAT('SPGR,',G23.15)
!
!       MIRROR THICKNESS
   IF(DABS(surf_mirror_thickness(I)).GT.0.0D0) THEN
      IF(GLANAM(I,1).EQ.'             '.AND.GLANAM(I,2).EQ.'REFL         ') THEN
         WRITE(OUTLYNE,4702) DABS(surf_mirror_thickness(I))
         CALL SHOWIT(10)
      END IF
   END IF
4702 FORMAT('THM,',G23.15)
!
!       PRICE
   WRITE(OUTLYNE,4703) DABS(surf_price(I))
   IF(surf_price(I).NE.0.0D0) CALL SHOWIT(10)
4703 FORMAT('PRICE,',G23.15)
!
!       FOOTBLOK
   IF(surf_footblok_flag(I).EQ.1.0D0) THEN
!       SET FOOTBLOK
      WRITE(OUTLYNE,4747)
      CALL SHOWIT(10)
4747  FORMAT('FOOTBLOK ON')
   END IF
!
!       PIVAXIS
   IF(surf_pivot_axis(I).EQ.1.0D0) THEN
      WRITE(OUTLYNE,4748)
      CALL SHOWIT(10)
4748  FORMAT('PIVAXIS NORMAL')
   END IF
!       SURFACE FORCED DUMMY
   IF(surf_dummy_val(I).EQ.1.0D0) THEN
      WRITE(OUTLYNE,1471)
      CALL SHOWIT(10)
1471  FORMAT('NODUM YES')
   END IF
   IF(surf_inr_flag(I).EQ.0.0D0) THEN
!     INR IS EXPLICITLY SET
      WRITE(OUTLYNE,475) surf_inr_value(I)
      CALL SHOWIT(10)
475   FORMAT('INR     ,',G23.15)
   END IF
!       NOW ASTOP AND REF
   IF(I.EQ.INT(sys_ref_surf())) THEN
!       SURFACE SHOULD BE THE REFERENCE SURFACE
      WRITE(OUTLYNE,94)
      CALL SHOWIT(10)
   END IF
   IF(I.EQ.INT(sys_astop())) THEN
!       I IS THE ASTOP SURFACE, IS THERE AN ADJUSTMENT ?
      IF(sys_astop_adj().EQ.0.0)  WRITE(OUTLYNE,95)
      IF(sys_astop_adj().EQ.1.0)  WRITE(OUTLYNE,96)
      IF(sys_astop_adj().EQ.-1.0) WRITE(OUTLYNE,97)
      IF(sys_astop_adj().EQ.2.0)  WRITE(OUTLYNE,98)
      IF(sys_astop_adj().GE.-1.0D0.AND.sys_astop_adj().LE.2.0D0) CALL SHOWIT(10)
94    FORMAT('REFS')
95    FORMAT('ASTOP')
96    FORMAT('ASTOP    EN')
97    FORMAT('ASTOP    EX')
98    FORMAT('ASTOP    ENEX')
   END IF
!
   IF(GLANAM(I,1).EQ.' '.AND.GLANAM(I,2).EQ.'REFL') THEN
      WRITE(OUTLYNE,120)
      CALL SHOWIT(10)
      RETURN
   END IF
   IF(GLANAM(I,1).EQ.' '.AND.GLANAM(I,2).EQ.'REFLTIRO') THEN
      WRITE(OUTLYNE,2120)
      CALL SHOWIT(10)
      RETURN
   END IF
   IF(GLANAM(I,1).EQ.' '.AND.GLANAM(I,2).EQ.'REFLTIR') THEN
      WRITE(OUTLYNE,3120)
      CALL SHOWIT(10)
      RETURN
   END IF
   IF(I.NE.0) THEN
      IF(GLANAM(I-1,1).EQ.' '.AND.GLANAM(I-1,2).EQ.'REFL'.OR.GLANAM(I-1,1).EQ.' '.AND.GLANAM(I-2,2).EQ.'REFLTIRO') THEN
         IF(DABS(surf_refractive_index(I-1, 1)).EQ.1.0D0.AND.DABS(surf_refractive_index(I-1, 2)).EQ.1.0D0.AND.DABS(surf_refractive_index(I-1, 3)).EQ.1.0D0.AND.DABS(surf_refractive_index(I-1, 4)).EQ.1.0D0.AND.DABS(surf_refractive_index(I-1, 5)).EQ.1.0D0.AND.DABS(surf_refractive_index(I-1, 6)).EQ.1.0D0.AND.DABS(surf_refractive_index(I-1, 7)).EQ.1.0D0.AND.DABS(surf_refractive_index(I-1, 8)).EQ.1.0D0.AND.DABS(surf_refractive_index(I-1, 9)).EQ.1.0D0.AND.DABS(surf_refractive_index(I-1, 10)).EQ.1.0D0) THEN
!     USE AIR
            WRITE(OUTLYNE,110)
            CALL SHOWIT(10)
         ELSE
!     HERE IS WHERE THE SEARCH GOES. WE ARE NOT AT I=0
!     WE ALREADY KNOW WHAT SITS AT I-1, IT IS REFL
!     WE CAN SEARCH BACK TO WHEN J=0
            IF(I.GT.1) THEN
               DO J=I-2,0,-1
!     IS IT ANOTHER REFLECTOR ? GO TO 87
                  IF(GLANAM(J,1).EQ.' '.AND.GLANAM(J,2).EQ.'REFLTIRO') GO TO 87
                  IF(GLANAM(J,1).EQ.' '.AND.GLANAM(J,2).EQ.'REFLTIR') GO TO 87
                  IF(GLANAM(J,1).EQ.' '.AND.GLANAM(J,2).EQ.'REFL') GO TO 87
!
!     WE LOOK FOR GLASSES TO USE AT SURFACE J
!     IF FOUND GOOD GLASS, GO TO 9876
!     IS IT "GLASS" ?
                  IF(GLANAM(J,1).EQ.'GLASS') THEN
                     WRITE(OUTLYNE,100)GLANAM(J,1)(1:8),GLANAM(J,2)(1:8),surf_refractive_index(J, 1),surf_refractive_index(J, 2),surf_refractive_index(J, 3),surf_refractive_index(J, 4),surf_refractive_index(J, 5)
                     CALL SHOWIT(10)
                     WRITE(OUTLYNE,1011) surf_refractive_index(J, 6)
                     CALL SHOWIT(10)
                     WRITE(OUTLYNE,1012) surf_refractive_index(J, 7)
                     CALL SHOWIT(10)
                     WRITE(OUTLYNE,1013) surf_refractive_index(J, 8)
                     CALL SHOWIT(10)
                     WRITE(OUTLYNE,1014) surf_refractive_index(J, 9)
                     CALL SHOWIT(10)
                     WRITE(OUTLYNE,1015) surf_refractive_index(J, 10)
                     CALL SHOWIT(10)
                     GO TO 9876
                  END IF
!     IS IT "MODEL" ?
                  IF(GLANAM(J,1).EQ.'MODEL') THEN
                     WRITE(OUTLYNE,7676)GLANAM(J,1)(1:8),GLANAM(J,2)(1:8),surf_fict_n(J),surf_fict_v(J),surf_fict_w(J)
                     CALL SHOWIT(10)
                     GO TO 9876
                  END IF
!     IS IT A CATALOG GLASS ?
                  IF(GLANAM(J,1).NE.'GLASS'.AND.GLANAM(J,1).NE.' '.AND.GLANAM(J,2).NE.'AIR'.AND.GLANAM(J,2).NE.'REFL'.AND.GLANAM(J,2).NE.'LAST SURFACE'.AND.GLANAM(J,1).NE.'MODEL'.AND.GLANAM(I,2).NE.'REFLTIRO'.AND.GLANAM(I,2).NE.'REFLTIR') THEN
                     WRITE(OUTLYNE,101)GLANAM(J,1)(1:8),GLANAM(J,2)
                     CALL SHOWIT(10)
                     GO TO 9876
                  END IF
87                CONTINUE
               END DO
            END IF
!     FELL OUT HERE, USE UNKNOWN GLASS
!     NO GOOD REAL GLASS FOUND, USE UNKNOWN
            WRITE(OUTLYNE,9010)
            CALL SHOWIT(10)
            WRITE(OUTLYNE,9011)
            CALL SHOWIT(10)
            WRITE(OUTLYNE,9012)
            CALL SHOWIT(10)
            WRITE(OUTLYNE,9013)
            CALL SHOWIT(10)
            WRITE(OUTLYNE,9014)
            CALL SHOWIT(10)
            WRITE(OUTLYNE,9015)
            CALL SHOWIT(10)
9010        FORMAT('GLASS UNKNOWN 1.0 1.0 1.0 1.0 1.0')
9011        FORMAT('N6 1.0')
9012        FORMAT('N7 1.0')
9013        FORMAT('N8 1.0')
9014        FORMAT('N9 1.0')
9015        FORMAT('N10 1.0')
         END IF
9876     CONTINUE
         RETURN
      END IF
!
      IF(GLANAM(I-1,1).EQ.' '.AND.GLANAM(I-1,2).EQ.'AIR'.OR.GLANAM(I-1,1).EQ.' '.AND.GLANAM(I-1,2).EQ.'LAST SURFACE') THEN
         WRITE(OUTLYNE,110)
         CALL SHOWIT(10)
         RETURN
      END IF
      IF(GLANAM(I-1,1).EQ.'GLASS') THEN
         WRITE(OUTLYNE,100)GLANAM(I-1,1)(1:8),GLANAM(I-1,2)(1:8),surf_refractive_index(I-1, 1),surf_refractive_index(I-1, 2),surf_refractive_index(I-1, 3),surf_refractive_index(I-1, 4),surf_refractive_index(I-1, 5)
         CALL SHOWIT(10)
6676     FORMAT(A8,' ',A8,',',G23.15,',',G23.15,',',G23.15)
7676     FORMAT(A8,' ',A8,',',G23.15,',',G23.15,',',G23.15)
6677     FORMAT(G23.15,',',G23.15)
      END IF
      IF(GLANAM(I-1,1).EQ.'GLASS') THEN
         WRITE(OUTLYNE,1011) surf_refractive_index(I-1, 6)
         CALL SHOWIT(10)
         WRITE(OUTLYNE,1012) surf_refractive_index(I-1, 7)
         CALL SHOWIT(10)
         WRITE(OUTLYNE,1013) surf_refractive_index(I-1, 8)
         CALL SHOWIT(10)
         WRITE(OUTLYNE,1014) surf_refractive_index(I-1, 9)
         CALL SHOWIT(10)
         WRITE(OUTLYNE,1015) surf_refractive_index(I-1, 10)
         CALL SHOWIT(10)
1011     FORMAT('N6      ,',G23.15)
1012     FORMAT('N7      ,',G23.15)
1013     FORMAT('N8      ,',G23.15)
1014     FORMAT('N9      ,',G23.15)
1015     FORMAT('N10     ,',G23.15)
         RETURN
      END IF
      IF(GLANAM(I-1,1).NE.'GLASS'.AND.GLANAM(I-1,1).NE.' '.AND.GLANAM(I-1,2).NE.'AIR'.AND.GLANAM(I-1,2).NE.'REFL'.AND.GLANAM(I-1,1).NE.'MODEL'.AND.GLANAM(I-1,2).NE.'REFLTIRO'.AND.GLANAM(I-1,2).NE.'REFLTIR')THEN
         WRITE(OUTLYNE,101)GLANAM(I-1,1)(1:8),GLANAM(I-1,2)
         CALL SHOWIT(10)
         RETURN
      END IF
   ELSE
      WRITE(OUTLYNE,110)
      CALL SHOWIT(10)
   END IF
100 FORMAT(A8,' ',A8,',',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
101 FORMAT(A8,' ',A13)
110 FORMAT('AIR')
120 FORMAT('REFL')
2120 FORMAT('REFLTIRO')
3120 FORMAT('REFLTIR')
!
   RETURN
END
! SUB LENSF.FOR
SUBROUTINE LENSF(I,RDOUT)
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_astop, sys_astop_adj, sys_ref_surf
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       SUBROUTINE LENSF HANDELS LENS SURFACE INFO OUTPUT
!       DURING A LENO AND
!       ACTS ON THE CURRENT LENS, ONLY
!       ONE SURFACE AT A TIME.
!       THE VARIABLE I PASSES THE SURFACE
!       NUMBER OF INTEREST
!
   INTEGER I,J
!
   LOGICAL RDOUT
!
   CHARACTER*3 AALL
!
!
!       THE CURRENT SURFACE IS PASSED WITH I
!       A DUMMY SURFACE HAS AIR AS ITS MATERIAL AND HAS
!       AIR AS THE MATERIAL INFRONT OF IT.
!
!       WE NOW HAVE SIMPLE LENO, PROCEED
!
!
401 FORMAT('CK THE FOLLOWING DATA REFERS TO SURFACE #',I1)
402 FORMAT('CK THE FOLLOWING DATA REFERS TO SURFACE #',I2)
403 FORMAT('CK THE FOLLOWING DATA REFERS TO SURFACE #',I3)
!     SURFACE LABEL
   IF(I.LT.10) WRITE(OUTLYNE,401) I
   IF(I.GE.10.AND.I.LT.100) WRITE(OUTLYNE,402) I
   IF(I.GE.100) WRITE(OUTLYNE,403) I
   CALL SHOWIT(10)
!
   IF(surf_label_flag(I).EQ.1.0D0) THEN
      WRITE(OUTLYNE,400) LBL(I)(1:74)
      CALL SHOWIT(10)
400   FORMAT('LBL ,',A74)
   END IF
!     COATING NUMBER
   WRITE(OUTLYNE,300) DBLE(INT(surf_coating_index(I)))
   CALL SHOWIT(10)
300 FORMAT('COATING ,',G23.15)
!               CURVATURE

   IF(.NOT.RDOUT)WRITE(OUTLYNE,10) surf_curvature(I)
   IF(RDOUT) THEN
      IF(surf_curvature(I).NE.0.0D0)WRITE(OUTLYNE,1010) 1.0D0/surf_curvature(I)
      IF(surf_curvature(I).EQ.0.0D0)WRITE(OUTLYNE,10) surf_curvature(I)
   END IF
   CALL SHOWIT(10)
10 FORMAT('CV      ,',G23.15)
1010 FORMAT('RD      ,',G23.15)
!
!       ALL DONE WITH CURVATURE
!
   IF(surf_conic(I).NE.0.0) THEN
!               CONIC CONSTANT

      WRITE(OUTLYNE,11) surf_conic(I)
      CALL SHOWIT(10)
11    FORMAT('CC      ,',G23.15)
   END IF

!               THICKNESS

   WRITE(OUTLYNE,12) surf_thickness((I))
   CALL SHOWIT(10)
12 FORMAT('TH      ,',G23.15)
!
!
!       DEFORM
   IF(surf_default_flag(I).EQ.1.0D0) THEN
      IF(surf_mtracei_nx(I).EQ.1.0D0) AALL='F01'
      IF(surf_mtracei_nx(I).EQ.2.0D0) AALL='F02'
      IF(surf_mtracei_nx(I).EQ.3.0D0) AALL='F03'
      IF(surf_mtracei_nx(I).EQ.4.0D0) AALL='F04'
      IF(surf_mtracei_nx(I).EQ.5.0D0) AALL='F05'
      IF(surf_mtracei_nx(I).EQ.6.0D0) AALL='F06'
      IF(surf_mtracei_nx(I).EQ.7.0D0) AALL='F07'
      IF(surf_mtracei_nx(I).EQ.8.0D0) AALL='F08'
      IF(surf_mtracei_nx(I).EQ.9.0D0) AALL='F09'
      IF(surf_mtracei_nx(I).EQ.10.0D0) AALL='F10'
      WRITE(OUTLYNE,1218) AALL,surf_mtracei_ny(I),surf_psfbin_data(I)
      CALL SHOWIT(10)
1218  FORMAT('DEFORM   ',A3,1X,G23.15,',',G23.15)
   END IF
!       LENS ARRAY
   IF(surf_array_parity(I).NE.0.0D0) THEN
      IF(surf_array_parity(I).EQ.-1.0D0)WRITE(OUTLYNE,1219) surf_array_dx(I),surf_array_dy(I)
      IF(surf_array_parity(I).EQ.1.0D0)WRITE(OUTLYNE,1220) surf_array_dx(I),surf_array_dy(I)
      CALL SHOWIT(10)
1219  FORMAT('ARRAY ODD',G23.15,',',G23.15)
1220  FORMAT('ARRAY EVEN',G23.15,',',G23.15)
   END IF
!               GRATING

   IF(surf_diffraction_flag(I).EQ.1.0D0) THEN
      WRITE(OUTLYNE,1212)
1212  FORMAT('GRT')
      CALL SHOWIT(10)
      WRITE(OUTLYNE,1213) surf_grating_order(I)
1213  FORMAT('GRO',G23.15)
      CALL SHOWIT(10)
      WRITE(OUTLYNE,1214) surf_grating_spacing(I)
1214  FORMAT('GRS',G23.15)
      CALL SHOWIT(10)
      WRITE(OUTLYNE,1215) surf_grating_vx(I)
1215  FORMAT('GRX',G23.15)
      CALL SHOWIT(10)
      WRITE(OUTLYNE,1216) surf_grating_vy(I)
1216  FORMAT('GRY',G23.15)
      CALL SHOWIT(10)
      WRITE(OUTLYNE,1217) surf_grating_vz(I)
1217  FORMAT('GRZ',G23.15)
      CALL SHOWIT(10)
   END IF
!
!               ASPHERICS
   IF(surf_is_asphere(I)) THEN

      WRITE(OUTLYNE,13) surf_asphere_coeff(I, 4),surf_asphere_coeff(I, 6),surf_asphere_coeff(I, 8),surf_asphere_coeff(I, 10),surf_asphere_coeff(I, 2)
      CALL SHOWIT(10)
13    FORMAT('ASPH    ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,113) surf_asphere_coeff(I, 12),surf_asphere_coeff(I, 14),surf_asphere_coeff(I, 16),surf_asphere_coeff(I, 18),surf_asphere_coeff(I, 20)
      CALL SHOWIT(10)
113   FORMAT('ASPH2   ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
   END IF
!               CLEAR APERTURE
   IF(surf_clap_type(I).EQ.1.0) THEN
!       CIRCULAR CLAP

      IF(surf_clap_dim(I, 2) == 0.0D0) call set_surf_clap_dim(I, 2, surf_clap_dim(I, 1))
14    FORMAT('CLAP    ,',G23.15,',',G23.15,',',G23.15,',',G23.15 ,',',G23.15)
      WRITE(OUTLYNE,14)surf_clap_dim(I, 1),surf_clap_dim(I, 3),surf_clap_dim(I, 4),surf_clap_dim(I, 2),surf_clap_dim(I, 5)
      CALL SHOWIT(10)
   END IF
   IF(surf_clap_type(I).EQ.3.0) THEN
!       ELLIPTICAL CLAP

16    FORMAT('CLAP     ELIP    ,',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,16)surf_clap_dim(I, 1),surf_clap_dim(I, 2),surf_clap_dim(I, 3),surf_clap_dim(I, 4)
      CALL SHOWIT(10)
      IF(surf_clap_tilt(I).NE.0.0) THEN
         WRITE(OUTLYNE,15) surf_clap_tilt(I)
         CALL SHOWIT(10)
      END IF
15    FORMAT('CLAP     TILT    ,',G23.15)
   END IF
   IF(surf_clap_type(I).EQ.2.0) THEN
!       RECTANGULAR CLAP
17    FORMAT('CLAP     RECT    ,',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,17)surf_clap_dim(I, 1),surf_clap_dim(I, 2),surf_clap_dim(I, 3),surf_clap_dim(I, 4)
      CALL SHOWIT(10)
      IF(surf_clap_tilt(I).NE.0.0) THEN

         WRITE(OUTLYNE,15) surf_clap_tilt(I)
         CALL SHOWIT(10)
      END IF
   END IF
   IF(surf_clap_type(I).EQ.4.0) THEN
!       RACETRACK CLAP
18    FORMAT('CLAP     RCTK    ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,18)surf_clap_dim(I, 1),surf_clap_dim(I, 2),surf_clap_dim(I, 3),surf_clap_dim(I, 4),surf_clap_dim(I, 5)
      CALL SHOWIT(10)
      IF(surf_clap_tilt(I).NE.0.0) THEN

         WRITE(OUTLYNE,15) surf_clap_tilt(I)
         CALL SHOWIT(10)
      END IF
   END IF
   IF(surf_clap_type(I).EQ.5.0) THEN
!       POLY CLAP
181   FORMAT('CLAP     POLY    ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,181)surf_clap_dim(I, 1),surf_clap_dim(I, 2),surf_clap_dim(I, 3),surf_clap_dim(I, 4),surf_clap_dim(I, 5)
      CALL SHOWIT(10)
      IF(surf_clap_tilt(I).NE.0.0) THEN

         WRITE(OUTLYNE,15) surf_clap_tilt(I)
         CALL SHOWIT(10)
      END IF
   END IF
   IF(surf_clap_type(I).EQ.6.0) THEN
!       IPOLY CLAP
182   FORMAT('CLAP     IPOLY   ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,182)surf_clap_dim(I, 1),surf_clap_dim(I, 2),surf_clap_dim(I, 3),surf_clap_dim(I, 4),surf_clap_dim(I, 5)
      CALL SHOWIT(10)
      IF(surf_clap_tilt(I).NE.0.0) THEN

         WRITE(OUTLYNE,15) surf_clap_tilt(I)
         CALL SHOWIT(10)
      END IF
   END IF
!     MULTCLAP
   IF(surf_multi_clap_flag(I).NE.0.0D0) THEN
      DO J=1,INT(surf_multi_clap_flag(I))
         WRITE(OUTLYNE,1818) J,MULTCLAP(J,1,I),MULTCLAP(J,2,I),MULTCLAP(J,3,I)
         CALL SHOWIT(10)
1818     FORMAT('MULTCLAP ',I4,',',G23.15,',',G23.15,',',G23.15)
      END DO
   END IF
   IF(surf_cobs_ape_type(I).EQ.1.0) THEN
!       CIRCULAR ERASE CLAP
20    FORMAT('CLAP     ERASE   ,',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,20)surf_cobs_ape_data(I, 1),surf_cobs_ape_data(I, 3),surf_cobs_ape_data(I, 4)
      CALL SHOWIT(10)
   END IF
   IF(surf_cobs_ape_type(I).EQ.3.0) THEN
!       ELLIPTICAL ERASE CLAP
21    FORMAT('CLAP     ELIPE   ,',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,21)surf_cobs_ape_data(I, 1),surf_cobs_ape_data(I, 2),surf_cobs_ape_data(I, 3),surf_cobs_ape_data(I, 4)
      CALL SHOWIT(10)
      IF(surf_cobs_ape_data(I, 6).NE.0.0) THEN

         WRITE(OUTLYNE,15) surf_cobs_ape_data(I, 6)
         CALL SHOWIT(10)
      END IF
   END IF
   IF(surf_cobs_ape_type(I).EQ.2.0) THEN
!       RECTANGULAR ERASE CLAP
22    FORMAT('CLAP     RECTE   ,',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,22)surf_cobs_ape_data(I, 1),surf_cobs_ape_data(I, 2),surf_cobs_ape_data(I, 3),surf_cobs_ape_data(I, 4)
      CALL SHOWIT(10)
      IF(surf_cobs_ape_data(I, 6).NE.0.0) THEN

         WRITE(OUTLYNE,15) surf_cobs_ape_data(I, 6)
         CALL SHOWIT(10)
      END IF
   END IF
   IF(surf_cobs_ape_type(I).EQ.4.0) THEN
!       RACETRACK ERASE CLAP
23    FORMAT('CLAP     RCTKE   ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,23)surf_cobs_ape_data(I, 1),surf_cobs_ape_data(I, 2),surf_cobs_ape_data(I, 3),surf_cobs_ape_data(I, 4),surf_cobs_ape_data(I, 5)
      CALL SHOWIT(10)
      IF(surf_cobs_ape_data(I, 6).NE.0.0) THEN

         WRITE(OUTLYNE,15) surf_cobs_ape_data(I, 6)
         CALL SHOWIT(10)
      END IF
   END IF
   IF(surf_cobs_ape_type(I).EQ.5.0) THEN
!       POLY ERASE CLAP
231   FORMAT('CLAP     POLYE   ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,231)surf_cobs_ape_data(I, 1),surf_cobs_ape_data(I, 2),surf_cobs_ape_data(I, 3),surf_cobs_ape_data(I, 4),surf_cobs_ape_data(I, 5)
      CALL SHOWIT(10)
      IF(surf_cobs_ape_data(I, 6).NE.0.0) THEN

         WRITE(OUTLYNE,15) surf_cobs_ape_data(I, 6)
         CALL SHOWIT(10)
      END IF
   END IF
   IF(surf_cobs_ape_type(I).EQ.6.0) THEN
!       IPOLY ERASE CLAP
232   FORMAT('CLAP     IPOLYE  ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,232)surf_cobs_ape_data(I, 1),surf_cobs_ape_data(I, 2),surf_cobs_ape_data(I, 3),surf_cobs_ape_data(I, 4),surf_cobs_ape_data(I, 5)
      CALL SHOWIT(10)
      IF(surf_cobs_ape_data(I, 6).NE.0.0) THEN

         WRITE(OUTLYNE,15) surf_cobs_ape_data(I, 6)
         CALL SHOWIT(10)
      END IF
   END IF
!               OBSCURATIONS
   IF(surf_coat_type(I).EQ.1.0) THEN
!       CIRCULAR COBS
25    FORMAT('COBS    ,',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,25)surf_cobs_poly(I, 1),surf_cobs_poly(I, 3),surf_cobs_poly(I, 4)
      CALL SHOWIT(10)
   END IF
   IF(surf_coat_type(I).EQ.3.0) THEN
!       ELLIPTICAL COBS
26    FORMAT('COBS     ELIP    ,',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,26)surf_cobs_poly(I, 1),surf_cobs_poly(I, 2),surf_cobs_poly(I, 3),surf_cobs_poly(I, 4)
      CALL SHOWIT(10)
      IF(surf_cobs_poly(I, 6).NE.0.0) THEN
         WRITE(OUTLYNE,27) surf_cobs_poly(I, 6)
         CALL SHOWIT(10)
      END IF
27    FORMAT('COBS     TILT    ,',G23.15)
   END IF
   IF(surf_coat_type(I).EQ.2.0D0.AND.surf_spider_flag(I).EQ.0.0D0) THEN
!       RECTANGULAR COBS
28    FORMAT('COBS     RECT    ,',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,28)surf_cobs_poly(I, 1),surf_cobs_poly(I, 2),surf_cobs_poly(I, 3),surf_cobs_poly(I, 4)
      CALL SHOWIT(10)
      IF(surf_cobs_poly(I, 6).NE.0.0) THEN
         WRITE(OUTLYNE,27) surf_cobs_poly(I, 6)
         CALL SHOWIT(10)
      END IF
   END IF
   IF(surf_coat_type(I).EQ.4.0) THEN
!       RACETRACK COBS
29    FORMAT('COBS     RCTK    ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,29)surf_cobs_poly(I, 1),surf_cobs_poly(I, 2),surf_cobs_poly(I, 3),surf_cobs_poly(I, 4),surf_cobs_poly(I, 5)
      CALL SHOWIT(10)
      IF(surf_cobs_poly(I, 6).NE.0.0) THEN

         WRITE(OUTLYNE,27) surf_cobs_poly(I, 6)
         CALL SHOWIT(10)
      END IF
   END IF
   IF(surf_coat_type(I).EQ.5.0) THEN
!       POLY COBS
291   FORMAT('COBS     POLY    ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,291)surf_cobs_poly(I, 1),surf_cobs_poly(I, 2),surf_cobs_poly(I, 3),surf_cobs_poly(I, 4),surf_cobs_poly(I, 5)
      CALL SHOWIT(10)
      IF(surf_cobs_poly(I, 6).NE.0.0) THEN

         WRITE(OUTLYNE,27) surf_cobs_poly(I, 6)
         CALL SHOWIT(10)
      END IF
   END IF
   IF(surf_coat_type(I).EQ.6.0) THEN
!       IPOLY COBS
292   FORMAT('COBS     IPOLY   ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,292)surf_cobs_poly(I, 1),surf_cobs_poly(I, 2),surf_cobs_poly(I, 3),surf_cobs_poly(I, 4),surf_cobs_poly(I, 5)
      CALL SHOWIT(10)
      IF(surf_cobs_poly(I, 6).NE.0.0) THEN

         WRITE(OUTLYNE,27) surf_cobs_poly(I, 6)
         CALL SHOWIT(10)
      END IF
   END IF
!
!     MULTCOBS
   IF(surf_multi_cobs_flag(I).NE.0.0D0.AND.surf_spider_flag(I).EQ.0.0D0) THEN
      DO J=1,INT(surf_multi_cobs_flag(I))
         WRITE(OUTLYNE,1819) J,MULTCOBS(J,1,I),MULTCOBS(J,2,I),MULTCOBS(J,3,I)
         CALL SHOWIT(10)
1819     FORMAT('MULTCOBS ',I4,',',G23.15,',',G23.15,',',G23.15)
      END DO
   END IF
!
!     SPIDER
   IF(surf_spider_flag(I).NE.0.0D0) THEN
      WRITE(OUTLYNE,1820) surf_spider_arms(I),surf_spider_angle(I),surf_spider_width(I)
      CALL SHOWIT(10)
1820  FORMAT('SPIDER ,',G23.15,',',G23.15,',',G23.15)
   END IF
   IF(surf_cobs_era_type(I).EQ.1.0) THEN
!       CIRCULAR ERASE COBS

31    FORMAT('COBS     ERASE   ,',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,31)surf_cobs_era_data(I, 1),surf_cobs_era_data(I, 3),surf_cobs_era_data(I, 4)
      CALL SHOWIT(10)
   END IF
   IF(surf_cobs_era_type(I).EQ.3.0) THEN
!       ELLIPTICAL ERASE COBS

32    FORMAT('COBS     ELIPE   ,',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,32)surf_cobs_era_data(I, 1),surf_cobs_era_data(I, 2),surf_cobs_era_data(I, 3),surf_cobs_era_data(I, 4)
      CALL SHOWIT(10)
      IF(surf_cobs_era_data(I, 6).NE.0.0) THEN

         WRITE(OUTLYNE,27) surf_cobs_era_data(I, 6)
         CALL SHOWIT(10)
      END IF
   END IF
   IF(surf_cobs_era_type(I).EQ.2.0) THEN
!       RECTANGULAR ERACE COBS

33    FORMAT('COBS     RECTE   ,',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,33)surf_cobs_era_data(I, 1),surf_cobs_era_data(I, 2),surf_cobs_era_data(I, 3),surf_cobs_era_data(I, 4)
      CALL SHOWIT(10)
      IF(surf_cobs_era_data(I, 6).NE.0.0) THEN

         WRITE(OUTLYNE,27) surf_cobs_era_data(I, 6)
         CALL SHOWIT(10)
      END IF
   END IF
   IF(surf_cobs_era_type(I).EQ.4.0) THEN
!       RACETRACK ERASE CLAP
34    FORMAT('COBS     RCTKE   ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,34)surf_cobs_era_data(I, 1),surf_cobs_era_data(I, 2),surf_cobs_era_data(I, 3),surf_cobs_era_data(I, 4),surf_cobs_era_data(I, 5)
      CALL SHOWIT(10)
      IF(surf_cobs_era_data(I, 6).NE.0.0) THEN

         WRITE(OUTLYNE,27) surf_cobs_era_data(I, 6)
         CALL SHOWIT(10)
      END IF
   END IF
   IF(surf_cobs_era_type(I).EQ.5.0) THEN
!       POLY ERASE CLAP
341   FORMAT('COBS     POLYE   ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,341)surf_cobs_era_data(I, 1),surf_cobs_era_data(I, 2),surf_cobs_era_data(I, 3),surf_cobs_era_data(I, 4),surf_cobs_era_data(I, 5)
      CALL SHOWIT(10)
      IF(surf_cobs_era_data(I, 6).NE.0.0) THEN

         WRITE(OUTLYNE,27) surf_cobs_era_data(I, 6)
         CALL SHOWIT(10)
      END IF
   END IF
   IF(surf_cobs_era_type(I).EQ.6.0) THEN
!       IPOLY ERASE CLAP
342   FORMAT('COBS     IPOLYE  ,',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
      WRITE(OUTLYNE,342)surf_cobs_era_data(I, 1),surf_cobs_era_data(I, 2),surf_cobs_era_data(I, 3),surf_cobs_era_data(I, 4),surf_cobs_era_data(I, 5)
      CALL SHOWIT(10)
      IF(surf_cobs_era_data(I, 6).NE.0.0) THEN

         WRITE(OUTLYNE,27) surf_cobs_era_data(I, 6)
         CALL SHOWIT(10)
      END IF
   END IF
!
!
!       NOW TORICS IF PRESENT
!
   IF(surf_toric_flag(I).NE.0.0) THEN
!       TORICS PRESENT
      IF(surf_toric_flag(I).EQ.1.0) THEN

         WRITE(OUTLYNE,36)
         CALL SHOWIT(10)

         IF(.NOT.RDOUT) THEN
            WRITE(OUTLYNE,38) surf_toric_curvature(I)
         ELSE
            IF(surf_toric_curvature(I).NE.0.0D0) WRITE(OUTLYNE,39) 1.0D0/surf_toric_curvature(I)
            IF(surf_toric_curvature(I).EQ.0.0D0) WRITE(OUTLYNE,38) surf_toric_curvature(I)
         END IF
         CALL SHOWIT(10)
      END IF
      IF(surf_toric_flag(I).EQ.2.0) THEN

         WRITE(OUTLYNE,37)
         CALL SHOWIT(10)

         WRITE(OUTLYNE,38) surf_toric_curvature(I)
         CALL SHOWIT(10)
      END IF
   END IF
36 FORMAT('YTORIC  ')
38 FORMAT('CVTOR   ,',G23.15)
37 FORMAT('XTORIC  ')
39 FORMAT('RDTOR   ,',G23.15)
!
!       ROOF, CORNER CUBE AND RAYERROR SURFACES
   IF(surf_ccr_flag(I).EQ.1.0D0) THEN
      WRITE(OUTLYNE,1121) surf_roof_a(I),surf_roof_angle_err(I)
      CALL SHOWIT(10)
   END IF
   IF(surf_ccr_flag(I).EQ.2.0D0) THEN
      WRITE(OUTLYNE,1122) surf_roof_a(I),surf_roof_angle_err(I),surf_roof_b(I),surf_ccr_angle_err2(I)
      CALL SHOWIT(10)
   END IF
   WRITE(OUTLYNE,1123) surf_ray_error(I)
   CALL SHOWIT(10)
1121 FORMAT('ROO ',G23.15,',',G23.15)
1122 FORMAT('CCR ',G23.15,',',G23.15,',',G23.15,',',G23.15)
1123 FORMAT('RAYERROR',G23.15)
!
!       ANAMORPHIC CONIC AND ASPHERIC TERMS
!
   IF(surf_anamorphic_conic(I).NE.0.0) THEN

      WRITE(OUTLYNE,40) surf_anamorphic_conic(I)
      CALL SHOWIT(10)
40    FORMAT('CCTOR   ,',G23.15)
   END IF
   IF(surf_anamorphic_flag(I).NE.0.0) THEN
!       ANAMORPHIC ASPHERICS
      WRITE(OUTLYNE,41) surf_anamorphic_coeff(I, 4),surf_anamorphic_coeff(I, 6),surf_anamorphic_coeff(I, 8),surf_anamorphic_coeff(I, 10)
      CALL SHOWIT(10)
41    FORMAT('TASPH   ,',G23.15,',',G23.15,',',G23.15,',',G23.15)
   END IF
!
!       TILTS AND DECENTRATIONS
   IF(surf_decenter_flag(I).NE.0.0D0) THEN
!       SURFACE DECENTER
      WRITE(OUTLYNE,106) surf_focus_dy(I),surf_focus_dx(I),surf_focus_dz(I)
      CALL SHOWIT(10)
106   FORMAT('DEC     ,',G23.15,',',G23.15,',',G23.15)
   END IF
!
   IF(surf_tilt_flag(I).NE.0.0D0) THEN
!       TILTS
      IF(surf_tilt_flag(I).EQ.1.0D0.AND.surf_tilt_return_flag(I).EQ.1.0D0) THEN
         WRITE(OUTLYNE,1007) surf_ret_surf_num(I)
         CALL SHOWIT(10)
         IF(surf_global_dx(I).NE.0.0D0) THEN
            WRITE(OUTLYNE,9001) surf_global_dx(I)
            CALL SHOWIT(10)
         END IF
         IF(surf_global_dy(I).NE.0.0D0) THEN
            WRITE(OUTLYNE,9002) surf_global_dy(I)
            CALL SHOWIT(10)
         END IF
         IF(surf_global_dz(I).NE.0.0D0) THEN
            WRITE(OUTLYNE,9003) surf_global_dz(I)
            CALL SHOWIT(10)
         END IF
         IF(surf_global_alpha(I).NE.0.0D0) THEN
            WRITE(OUTLYNE,9004) surf_global_alpha(I)
            CALL SHOWIT(10)
         END IF
         IF(surf_global_beta(I).NE.0.0D0) THEN
            WRITE(OUTLYNE,9005) surf_global_beta(I)
            CALL SHOWIT(10)
         END IF
         IF(surf_global_gamma(I).NE.0.0D0) THEN
            WRITE(OUTLYNE,9006) surf_global_gamma(I)
            CALL SHOWIT(10)
         END IF
      ELSE
         IF(surf_tilt_flag(I).EQ.1.0D0)WRITE(OUTLYNE,102) surf_alpha_deg(I),surf_beta_deg(I),surf_gamma_deg(I)
         IF(surf_tilt_flag(I).EQ.-1.0D0)WRITE(OUTLYNE,103) surf_alpha_deg(I),surf_beta_deg(I),surf_gamma_deg(I)
         IF(surf_tilt_flag(I).EQ.2.0D0)WRITE(OUTLYNE,104) surf_alpha_deg(I),surf_beta_deg(I),surf_gamma_deg(I)
         IF(surf_tilt_flag(I).EQ.3.0D0)WRITE(OUTLYNE,105) surf_alpha_deg(I),surf_beta_deg(I),surf_gamma_deg(I)
         IF(surf_tilt_flag(I).EQ.4.0D0)WRITE(OUTLYNE,1005) surf_alpha_deg(I),surf_beta_deg(I),surf_gamma_deg(I)
         IF(surf_tilt_flag(I).EQ.5.0D0)WRITE(OUTLYNE,1006) surf_alpha_deg(I),surf_beta_deg(I),surf_gamma_deg(I)
         IF(surf_tilt_flag(I).EQ.7.0D0)WRITE(OUTLYNE,7674) surf_alpha_deg(I),surf_beta_deg(I),surf_gamma_deg(I)
         CALL SHOWIT(10)
      END IF
!
102   FORMAT('TILT    ,',G23.15,',',G23.15,',',G23.15)
103   FORMAT('RTILT   ,',G23.15,',',G23.15,',',G23.15)
104   FORMAT('TILT     AUTO    ,',G23.15,',',G23.15,',',G23.15)
105   FORMAT('TILT     AUTOM   ,',G23.15,',',G23.15,',',G23.15)
1005  FORMAT('TILT     BEN     ,',G23.15,',',G23.15,',',G23.15)
1006  FORMAT('TILT     DAR     ,',G23.15,',',G23.15,',',G23.15)
1007  FORMAT('TILT     RET     ,',G23.15,',',G23.15,',',G23.15 ,',',G23.15)
      IF(surf_tilt_flag(I).EQ.1.0D0.AND.surf_tilt_return_flag(I).EQ.1.0D0) THEN
         WRITE(OUTLYNE,1007) surf_ret_surf_num(I)
         CALL SHOWIT(10)
         IF(surf_global_dx(I).NE.0.0D0) THEN
            WRITE(OUTLYNE,9001) surf_global_dx(I)
            CALL SHOWIT(10)
9001        FORMAT('GDX     ,',G23.15)
         END IF
         IF(surf_global_dy(I).NE.0.0D0) THEN
            WRITE(OUTLYNE,9002) surf_global_dy(I)
            CALL SHOWIT(10)
9002        FORMAT('GDY     ,',G23.15)
         END IF
         IF(surf_global_dz(I).NE.0.0D0) THEN
            WRITE(OUTLYNE,9003) surf_global_dz(I)
            CALL SHOWIT(10)
9003        FORMAT('GDZ     ,',G23.15)
         END IF
         IF(surf_global_alpha(I).NE.0.0D0) THEN
            WRITE(OUTLYNE,9004) surf_global_alpha(I)
            CALL SHOWIT(10)
9004        FORMAT('GALPHA  ,',G23.15)
         END IF
         IF(surf_global_beta(I).NE.0.0D0) THEN
            WRITE(OUTLYNE,9005) surf_global_beta(I)
            CALL SHOWIT(10)
9005        FORMAT('GBETA   ,',G23.15)
         END IF
         IF(surf_global_gamma(I).NE.0.0D0) THEN
            WRITE(OUTLYNE,9006) surf_global_gamma(I)
            CALL SHOWIT(10)
9006        FORMAT('GGAMMA  ,',G23.15)
         END IF
      ELSE
         IF(surf_tilt_flag(I).EQ.1.0D0)WRITE(OUTLYNE,6669) surf_alpha_deg(I),surf_beta_deg(I),surf_gamma_deg(I)
         IF(surf_tilt_flag(I).EQ.-1.0D0)WRITE(OUTLYNE,6670) surf_alpha_deg(I),surf_beta_deg(I),surf_gamma_deg(I)
         IF(surf_tilt_flag(I).EQ.2.0D0)WRITE(OUTLYNE,6671) surf_alpha_deg(I),surf_beta_deg(I),surf_gamma_deg(I)
         IF(surf_tilt_flag(I).EQ.3.0D0)WRITE(OUTLYNE,6672) surf_alpha_deg(I),surf_beta_deg(I),surf_gamma_deg(I)
         IF(surf_tilt_flag(I).EQ.4.0D0)WRITE(OUTLYNE,6673) surf_alpha_deg(I),surf_beta_deg(I),surf_gamma_deg(I)
         IF(surf_tilt_flag(I).EQ.5.0D0)WRITE(OUTLYNE,6674) surf_alpha_deg(I),surf_beta_deg(I),surf_gamma_deg(I)
         IF(surf_tilt_flag(I).EQ.7.0D0)WRITE(OUTLYNE,7674) surf_alpha_deg(I),surf_beta_deg(I),surf_gamma_deg(I)
      END IF
!
6669  FORMAT('TILT    ,',G23.15,',',G23.15,',',G23.15)
6670  FORMAT('RTILT   ,',G23.15,',',G23.15,',',G23.15)
6671  FORMAT('TILT     AUTO    ,',G23.15,',',G23.15,',',G23.15)
6672  FORMAT('TILT     AUTOM   ,',G23.15,',',G23.15,',',G23.15)
6673  FORMAT('TILT     BEN     ,',G23.15,',',G23.15,',',G23.15)
6674  FORMAT('TILT     DAR     ,',G23.15,',',G23.15,',',G23.15)
7674  FORMAT('TILT     REV     ,',G23.15,',',G23.15,',',G23.15)
   END IF
   IF(surf_pivot_flag(I).NE.0) THEN
!       SURFACE PIVOT
      WRITE(OUTLYNE,1061) surf_pivot_x(I),surf_pivot_y(I),surf_pivot_z(I)
      CALL SHOWIT(10)
1061  FORMAT('PIVOT   ,',G23.15,',',G23.15,',',G23.15)
   END IF
!
!
!       REAL OR PARAXAIL SURFACE
   IF(surf_paraxial_val(I).EQ.1.0D0) THEN
!       PARAXIAL SURFACE
      WRITE(OUTLYNE,4711)
      CALL SHOWIT(10)
4711  FORMAT('PARAX')
   END IF
!
!       SPECIFIC GRAVITY
   WRITE(OUTLYNE,4701) DABS(surf_spgr(I))
   IF(surf_spgr(I).NE.0.0D0)CALL SHOWIT(10)
4701 FORMAT('SPGR,',G23.15)
!
!       MIRROR THICKNESS
   IF(DABS(surf_mirror_thickness(I)).GT.0.0D0) THEN
      IF(GLANAM(I,1).EQ.'             '.AND.GLANAM(I,2).EQ.'REFL         ') THEN
         WRITE(OUTLYNE,4702) DABS(surf_mirror_thickness(I))
         CALL SHOWIT(10)
      END IF
   END IF
4702 FORMAT('THM,',G23.15)
!
!       PRICE
   WRITE(OUTLYNE,4703) DABS(surf_price(I))
   IF(surf_price(I).NE.0.0D0) CALL SHOWIT(10)
4703 FORMAT('PRICE,',G23.15)
!
!       FOOTBLOK
   IF(surf_footblok_flag(I).EQ.1.0) THEN
!       SET FOOTBLOK
      WRITE(OUTLYNE,4747)
      CALL SHOWIT(10)
4747  FORMAT('FOOTBLOK ON')
   END IF
!
!       PIVAXIS
   IF(surf_pivot_axis(I).EQ.1.0D0) THEN
      WRITE(OUTLYNE,4748)
      CALL SHOWIT(10)
4748  FORMAT('PIVAXIS NORMAL')
   END IF
!       SURFACE FORCED DUMMY
   IF(surf_dummy_val(I).EQ.1.0D0) THEN
      WRITE(OUTLYNE,1471)
      CALL SHOWIT(10)
1471  FORMAT('NODUM YES')
   END IF
   IF(surf_inr_flag(I).EQ.1.0D0) THEN
!     INR IS EXPLICITLY SET
      WRITE(OUTLYNE,475) surf_inr_value(I)
      CALL SHOWIT(10)
475   FORMAT('INR     ,',G23.15)
   END IF
!       NOW ASTOP AND REF
   IF(I.EQ.INT(sys_ref_surf())) THEN
!       SURFACE SHOULD BE THE REFERENCE SURFACE
      WRITE(OUTLYNE,94)
      CALL SHOWIT(10)
   END IF
   IF(I.EQ.INT(sys_astop())) THEN
!       I IS THE ASTOP SURFACE, IS THERE AN ADJUSTMENT ?
      IF(sys_astop_adj().EQ.0.0)  WRITE(OUTLYNE,95)
      IF(sys_astop_adj().EQ.1.0)  WRITE(OUTLYNE,96)
      IF(sys_astop_adj().EQ.-1.0) WRITE(OUTLYNE,97)
      IF(sys_astop_adj().EQ.2.0)  WRITE(OUTLYNE,98)
      IF(sys_astop_adj().GE.-1.0D0.AND.sys_astop_adj().LE.2.0D0) CALL SHOWIT(10)
94    FORMAT('REFS')
95    FORMAT('ASTOP')
96    FORMAT('ASTOP    EN')
97    FORMAT('ASTOP    EX')
98    FORMAT('ASTOP    ENEX')
   END IF
!
   IF(GLANAM(I,1).EQ.' '.AND.GLANAM(I,2).EQ.'AIR'.OR.GLANAM(I,1).EQ.' '.AND.GLANAM(I,2).EQ.'LAST SURFACE') THEN
      WRITE(OUTLYNE,110)
      CALL SHOWIT(10)
      RETURN
   END IF
!
   IF(GLANAM(I,1).EQ.' '.AND.GLANAM(I,2).EQ.'PERFECT') THEN
      WRITE(OUTLYNE,1201)
      CALL SHOWIT(10)
      RETURN
   END IF
!
   IF(GLANAM(I,1).EQ.' '.AND.GLANAM(I,2).EQ.'IDEAL') THEN
      WRITE(OUTLYNE,11201) surf_ideal_efl(I)
      CALL SHOWIT(10)
      RETURN
   END IF
   IF(GLANAM(I,1).EQ.' '.AND.GLANAM(I,2).EQ.'REFL') THEN
      WRITE(OUTLYNE,120)
      CALL SHOWIT(10)
      RETURN
   END IF
   IF(GLANAM(I,1).EQ.' '.AND.GLANAM(I,2).EQ.'REFLTIRO') THEN
      WRITE(OUTLYNE,2120)
      CALL SHOWIT(10)
      RETURN
   END IF
   IF(GLANAM(I,1).EQ.' '.AND.GLANAM(I,2).EQ.'REFLTIR') THEN
      WRITE(OUTLYNE,3120)
      CALL SHOWIT(10)
      RETURN
   END IF
   IF(GLANAM(I,1).EQ.'GLASS') THEN
      WRITE(OUTLYNE,100)GLANAM(I,1)(1:8),GLANAM(I,2)(1:8),surf_refractive_index(I, 1),surf_refractive_index(I, 2),surf_refractive_index(I, 3),surf_refractive_index(I, 4),surf_refractive_index(I, 5)
      CALL SHOWIT(10)
   END IF
   IF(GLANAM(I,1).EQ.'GLASS') THEN
      WRITE(OUTLYNE,1011) surf_refractive_index(I, 6)
      CALL SHOWIT(10)
      WRITE(OUTLYNE,1012) surf_refractive_index(I, 7)
      CALL SHOWIT(10)
      WRITE(OUTLYNE,1013) surf_refractive_index(I, 8)
      CALL SHOWIT(10)
      WRITE(OUTLYNE,1014) surf_refractive_index(I, 9)
      CALL SHOWIT(10)
      WRITE(OUTLYNE,1015) surf_refractive_index(I, 10)
      CALL SHOWIT(10)
1011  FORMAT('N6      ,',G23.15)
1012  FORMAT('N7      ,',G23.15)
1013  FORMAT('N8      ,',G23.15)
1014  FORMAT('N9      ,',G23.15)
1015  FORMAT('N10     ,',G23.15)
      RETURN
   END IF
!     IS IT "MODEL" ?
   IF(GLANAM(I,1).EQ.'MODEL') THEN
      WRITE(OUTLYNE,7676)GLANAM(I,1)(1:8),GLANAM(I,2)(1:8),surf_fict_n(I),surf_fict_v(I),surf_fict_w(I)
      CALL SHOWIT(10)
   END IF
   IF(GLANAM(I,1).NE.'GLASS'.AND.GLANAM(I,1).NE.' '.AND.GLANAM(I,2).NE.'AIR'.AND.GLANAM(I,2).NE.'REFL'.AND.GLANAM(I,1).NE.'MODEL'.AND.GLANAM(I,2).NE.'PERFECT'.AND.GLANAM(I,2).NE.'REFLTIRO'.AND.GLANAM(I,2).NE.'REFLTIR') THEN
      WRITE(OUTLYNE,101)GLANAM(I,1)(1:8),GLANAM(I,2)
      CALL SHOWIT(10)
      RETURN
   END IF
100 FORMAT(A8,' ',A8,',',G23.15,',',G23.15,',',G23.15,',',G23.15,',',G23.15)
6676 FORMAT(A8,' ',A8,',',G23.15,',',G23.15,',',G23.15)
7676 FORMAT(A8,' ',A8,',',G23.15,',',G23.15,',',G23.15)
6677 FORMAT(G23.15,',',G23.15)
101 FORMAT(A8,' ',A13)
110 FORMAT('AIR')
120 FORMAT('REFL')
2120 FORMAT('REFLTIRO')
3120 FORMAT('REFLTIR')
1201 FORMAT('PERFECT')
11201 FORMAT('IDEAL , ',G23.15)
!
   RETURN
END
