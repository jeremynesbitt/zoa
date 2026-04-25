!       FOURTEENTH FILE FOR LENS DATABASE MANAGER FILES

! SUB SCOATING.FOR
SUBROUTINE SCOATING
!
   use DATLEN
   use mod_system
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SAIR WHICH IMPLEMENTS THE COATING
!       COMMAND AT THE LENS OF UPDATE LENS LEVEL.
!
   INTEGER PIKCNT,I
!
!
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
!               AND FOR NUMERIC INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SQ.EQ.1.OR.SST.EQ.1)THEN
      OUTLYNE='"COATING" TAKES NO QUALIFIER OR STRING INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
!
   ELSE
   END IF
   IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1)THEN
      OUTLYNE='"COATING" TAKES NO NUMERIC WORD #2 TO #5 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
!
   ELSE
   END IF
   IF(S1.EQ.0)THEN
      OUTLYNE='"COATING" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
!
   ELSE
   END IF
   IF(W1.LT.0.0D0.OR.W1.GT.1000.0D0)THEN
      OUTLYNE='"COATING" REQUIRES NUMERIC WORD #1 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='IN THE RANGE 0 TO 1000'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
!
   ELSE
   END IF
!
   call set_surf_coating_index(SURF, NINT(W1))
!
   IF(PIKUP(1,SURF,44).EQ.0.0) THEN
!
!       NO COATING PIKUPS, JUST RETURN
      RETURN
   ELSE
   END IF
!
!       DELETE THE PIKUP
   PIKUP(1:6,SURF,44)=0.0
   call set_surf_special_type(SURF, surf_special_type(SURF)-1)
!
!
!       PRINT MESSAGE
   WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (COATING) DELETED'
   CALL SHOWIT(1)
!
   PIKCNT=0
   DO 10 I=1,PSIZ
      IF(PIKUP(1,SURF,I).NE.0.0) THEN
         PIKCNT=PIKCNT+1
      ELSE
      END IF
10 CONTINUE
!
   IF(PIKCNT.EQ.0.0)call set_surf_special_type(SURF, 0)
!
   CALL MACFAL
   RETURN
END
! SUB SREFL.FOR
SUBROUTINE SREFL
!
   use DATLEN
   use mod_system
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SREFL WHICH IMPLEMENTS THE REFL
!       COMMAND AT THE LENS OF UPDATE LENS LEVEL.
!
!
   INTEGER PIKCNT,I
!
!
!       "REFL" IS A SURFACE TERMINAL COMMAND. IN THE LENS INPUT
!       ROUTINE F5=1, "REFL" CAUSES THE SURFACE COUNTING VARIABLE
!       (SURF) AND sys_last_surf() TO BE INCREMENTED BY 1 AND 1.0
!       AND CAUSES THE NEXT SURFACE DATA TO BE STORED FOR THE NEXT
!       SURFACE. AT F6=1 (UPDATE ROUTINE) THIS INCREMENTING DOES
!       NO OCCUR. THE LAST SURFACE FOR WHICH DATA MAY BE ENTERED
!       IS SURFACE MAXSUR. SURF AND sys_last_surf() ARE NOT INCREMENTED
!       PASSED MAXSUR SO IF AN ATTEMPT IS MADE TO INPUT PASSED
!       SURFACE MAXSUR IN THE LENS INPUT MODE, THE DATA FOR SURFACE
!       MAXSUR WILL BE OVERWRITTEN.
!
!               CHECK FOR STRING,NUMERIC OR QUALIFIER INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SST.EQ.1) THEN
      OUTLYNE='"REFL","REFLTIR" AND "REFLTIRO" TAKE NO EXPLICIT INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
   IF(F5.EQ.1.OR.F6.EQ.1) THEN
      IF(SURF.EQ.0) THEN
         OUTLYNE='OBJECT SURFACE CAN NOT BE A REFLECTOR'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!
      IF(SURF.EQ.0) THEN
         OUTLYNE='REFLECTIVE SURFACE NOT VALID ON OBJECT SURFACE'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!
!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
!
!       DEFAULT INDICES ALWAYS GO TO THE NEGATIVE VALUE OF THE
!       INDICES OF THE PRECEEDING SURFACE.
!

      call set_surf_refractive_index(SURF, 1, -surf_refractive_index((SURF-1), 1))
      call set_surf_refractive_index(SURF, 2, -surf_refractive_index((SURF-1), 2))
      call set_surf_refractive_index(SURF, 3, -surf_refractive_index((SURF-1), 3))
      call set_surf_refractive_index(SURF, 4, -surf_refractive_index((SURF-1), 4))
      call set_surf_refractive_index(SURF, 5, -surf_refractive_index((SURF-1), 5))
      call set_surf_refractive_index(SURF, 6, -surf_refractive_index((SURF-1), 6))
      call set_surf_refractive_index(SURF, 7, -surf_refractive_index((SURF-1), 7))
      call set_surf_refractive_index(SURF, 8, -surf_refractive_index((SURF-1), 8))
      call set_surf_refractive_index(SURF, 9, -surf_refractive_index((SURF-1), 9))
      call set_surf_refractive_index(SURF, 10, -surf_refractive_index((SURF-1), 10))
      GLANAM(SURF,1)='             '
      IF(WC.EQ.'REFL')     GLANAM(SURF,2)='REFL         '
      IF(WC.EQ.'REFLTIRO') GLANAM(SURF,2)='REFLTIRO     '
      IF(WC.EQ.'REFLTIR')  GLANAM(SURF,2)='REFLTIR      '
      IF(WC.EQ.'REFL')call set_surf_reflection_mode(SURF, 0)
      IF(WC.EQ.'REFLTIRO')call set_surf_reflection_mode(SURF, 1)
      IF(WC.EQ.'REFLTIR')call set_surf_reflection_mode(SURF, 2)
!
!       NOW HANDEL EXISTING PIKUPS OF GLASS TYPE, I E DELETE THEM.
!       CHECK FOR CC PIKUPS AND DELETE IF FOUND
!
!
      IF(PIKUP(1,SURF,20).EQ.0.0) THEN
!
!       NO GLASS PIKUPS, JUST RETURN
!
         IF(F5.EQ.1.AND.SURF.LT.MAXSUR) THEN
            SURF=SURF+1
            ALENS(1:LSIZ,SURF)=0.0D0
            call set_sys_last_surf(DBLE(SURF))
         ELSE
!       DON'T INCREMENT SURF AND sys_last_surf()
         END IF
         F22=1
         RETURN
      ELSE
      END IF
!
!       DELETE THE PIKUP
      PIKUP(1:6,SURF,20)=0.0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
!
!
!       PRINT MESSAGE
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GLASS) DELETED'
      CALL SHOWIT(1)
!
!       ARE THERE MORE PIKUPS? IF NOT SET surf_special_type(SURF) TO ZERO.
!
      PIKCNT=0
      DO 10 I=1,PSIZ
         IF(PIKUP(1,SURF,I).NE.0.0) THEN
            PIKCNT=PIKCNT+1
         ELSE
         END IF
10    CONTINUE
!
      IF(PIKCNT.EQ.0.0)call set_surf_special_type(SURF, 0)
!
      IF(F5.EQ.1.AND.SURF.LT.MAXSUR) THEN
         SURF=SURF+1
         ALENS(1:LSIZ,SURF)=0.0D0
         call set_sys_last_surf(DBLE(SURF))
      ELSE
!       DON'T INCREMENT SURF AND sys_last_surf()
      END IF
!
   ELSE
!
      F22=1
      OUTLYNE='"REFL" NOT VALID AT THIS PROGRAM LEVEL'
      CALL SHOWIT(1)
   END IF
   CALL MACFAL
   RETURN
END
! SUB SAIR.FOR
SUBROUTINE SAIR
!
   use DATLEN
   use mod_system
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SAIR WHICH IMPLEMENTS THE AIR
!       COMMAND AT THE LENS OF UPDATE LENS LEVEL.
!
   INTEGER PIKCNT,I
!
!
!       "AIR" IS A SURFACE TERMINAL COMMAND. IN THE LENS INPUT
!       ROUTINE F5=1, "AIR" CAUSES THE SURFACE COUNTING VARIABLE
!       (SURF) AND sys_last_surf() TO BE INCREMENTED BY 1 AND 1.0
!       AND CAUSES THE NEXT SURFACE DATA TO BE STORED FOR THE NEXT
!       SURFACE. AT F6=1 (UPDATE ROUTINE) THIS INCREMENTING DOES
!       NO OCCUR. THE LAST SURFACE FOR WHICH DATA MAY BE ENTERED
!       IS SURFACE MAXSUR. SURF AND sys_last_surf() ARE NOT INCREMENTED
!       PASSED MAXSUR SO IF AN ATTEMPT IS MADE TO INPUT PASSED
!       SURFACE MAXSUR IN THE LENS INPUT MODE, THE DATA FOR SURFACE
!       MAXSUR WILL BE OVERWRITTEN.
!
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
!               AND FOR NUMERIC INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1)THEN
      OUTLYNE='"AIR" TAKES NO EXPLICIT INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
!
   ELSE
   END IF
!
   IF(F5.EQ.1.OR.F6.EQ.1) THEN
!
!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
!
      ALENS(46:50,SURF)=1.0
      ALENS(71:75,SURF)=1.0
      GLANAM(SURF,1)='             '
      GLANAM(SURF,2)='AIR          '
!
!       NOW HANDEL EXISTING PIKUPS OF GLASS TYPE, I E DELETE THEM.
!       CHECK FOR CC PIKUPS AND DELETE IF FOUND
!
!
      IF(PIKUP(1,SURF,20).EQ.0.0) THEN
!
!       NO GLASS PIKUPS, JUST RETURN
!
         F22=1
         IF(F5.EQ.1.AND.SURF.LT.MAXSUR) THEN
            SURF=SURF+1
            ALENS(1:LSIZ,SURF)=0.0D0
            call set_sys_last_surf(DBLE(SURF))
         ELSE
!       DON'T INCREMENT SURF AND sys_last_surf()
         END IF
         RETURN
      ELSE
      END IF
!
!       DELETE THE PIKUP
      PIKUP(1:6,SURF,20)=0.0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
!
!
!       PRINT MESSAGE
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GLASS) DELETED'
      CALL SHOWIT(1)
!
      PIKCNT=0
      DO 10 I=1,PSIZ
         IF(PIKUP(1,SURF,I).NE.0.0) THEN
            PIKCNT=PIKCNT+1
         ELSE
         END IF
10    CONTINUE
!
      IF(PIKCNT.EQ.0.0)call set_surf_special_type(SURF, 0)
      IF(F5.EQ.1.AND.SURF.LT.MAXSUR) THEN
         SURF=SURF+1
         ALENS(1:LSIZ,SURF)=0.0D0
         call set_sys_last_surf(DBLE(SURF))
      ELSE
!       DON'T INCREMENT SURF AND sys_last_surf()
      END IF
!
      F22=1
   ELSE
      OUTLYNE='"AIR" NOT VALID AT THIS PROGRAM LEVEL'
      CALL SHOWIT(1)
   END IF
   CALL MACFAL
   RETURN
END
! SUB GLSCAT.FOR
SUBROUTINE GLSCAT
!
   use DATLEN
   use mod_system
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE GLSCAT WHICH IMPLEMENTS THE SCHOTT
!       SCH2000,GLASS,OHARA,HOYA,CHANCE,CORNIN,PLASTIC,RADHARD,GLCAT
!       HIKARI AND USER COMMAND AT THE LENS OF UPDATE LENS LEVEL.
!
   INTEGER PIKCNT,I
!
!
!       THIS IS A SURFACE TERMINAL COMMAND. IN THE LENS INPUT
!       ROUTINE F5=1, AND CAUSES THE SURFACE COUNTING VARIABLE
!       (SURF) AND sys_last_surf() TO BE INCREMENTED BY 1 AND 1.0
!       AND CAUSES THE NEXT SURFACE DATA TO BE STORED FOR THE NEXT
!       SURFACE. AT F6=1 (UPDATE ROUTINE) THIS INCREMENTING DOES
!       NO OCCUR. THE LAST SURFACE FOR WHICH DATA MAY BE ENTERED
!       IS SURFACE MAXSUR. SURF AND sys_last_surf() ARE NOT INCREMENTED
!       PASSED MAXSUR SO IF AN ATTEMPT IS MADE TO INPUT PASSED
!       SURFACE MAXSUR IN THE LENS INPUT MODE, THE DATA FOR SURFACE
!       MAXSUR WILL BE OVERWRITTEN.
!
!               CHECK FOR STRING INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   !call LogTermFOR("In GLSCAT")

   IF(SN.EQ.1) THEN
      OUTLYNE='"'//WC//'" zAKES NO NUMERIC INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL MACFAL
      RETURN
!
   ELSE
   END IF
!
   ALENS(46:50,SURF)=1.0D0
   ALENS(71:75,SURF)=1.0D0
   GLANAM(SURF,1)=WC(1:8)//'   '
   GLANAM(SURF,2)= WS(1:13)

   F22=1
!
!       NOW HANDEL EXISTING PIKUPS OF GLASS TYPE, I E DELETE THEM.
!       CHECK FOR CC PIKUPS AND DELETE IF FOUND
!
!
   IF(PIKUP(1,SURF,20).EQ.0.0) THEN
!
!       NO GLASS PIKUPS, JUST RETURN
      IF(F5.EQ.1.AND.SURF.LT.MAXSUR) THEN
         SURF=SURF+1
         ALENS(1:LSIZ,SURF)=0.0D0
         call set_sys_last_surf(DBLE(SURF))
      ELSE
!       DON'T INCREMENT SURF AND sys_last_surf()
      END IF
!
      RETURN
   ELSE
   END IF
!
!       DELETE THE PIKUP
   PIKUP(1:6,SURF,20)=0.0
   call set_surf_special_type(SURF, surf_special_type(SURF)-1)
!
!
!       PRINT MESSAGE
   WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GLASS) DELETED'
   CALL SHOWIT(1)
!
   PIKCNT=0
   DO 10 I=1,PSIZ
      IF(PIKUP(1,SURF,I).NE.0.0) THEN
         PIKCNT=PIKCNT+1
      ELSE
      END IF
10 CONTINUE
!
   IF(PIKCNT.EQ.0.0)call set_surf_special_type(SURF, 0)
   IF(F5.EQ.1.AND.SURF.LT.MAXSUR) THEN
      SURF=SURF+1
      ALENS(1:LSIZ,SURF)=0.0D0
      call set_sys_last_surf(DBLE(SURF))
   ELSE
!       DON'T INCREMENT SURF AND sys_last_surf()
   END IF
!
END

SUBROUTINE SINDEXJN(surfIndex, INDEX, VNUM)
!
   use DATLEN
   use mod_system
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SINDEX. THIS IS THE SUBROUTINE WHICH
!       HANDLES THE INDEX,DISPERSION AND V-NUMBER CALCULATIONS
!       FOR THE RTG AND CTG COMMANDS
!
   REAL*8, intent(INOUT) :: INDEX
   REAL*8 DISP,DISA,DISB,PARTL
   REAL*8, INTENT(INOUT) :: VNUM

!
   INTEGER, INTENT(IN) :: surfIndex
!
   COMMON DISP,PARTL
!
!
!       THE CONTROL WAVLENGTH NUMBER IS STORED IN sys_wl_ref()
!
   IF(sys_wl_ref().EQ.1.0) INDEX=surf_refractive_index(surfIndex, 1)
   IF(sys_wl_ref().EQ.2.0) INDEX=surf_refractive_index(surfIndex, 2)
   IF(sys_wl_ref().EQ.3.0) INDEX=surf_refractive_index(surfIndex, 3)
   IF(sys_wl_ref().EQ.4.0) INDEX=surf_refractive_index(surfIndex, 4)
   IF(sys_wl_ref().EQ.5.0) INDEX=surf_refractive_index(surfIndex, 5)
   IF(sys_wl_ref().EQ.6.0) INDEX=surf_refractive_index(surfIndex, 6)
   IF(sys_wl_ref().EQ.7.0) INDEX=surf_refractive_index(surfIndex, 7)
   IF(sys_wl_ref().EQ.8.0) INDEX=surf_refractive_index(surfIndex, 8)
   IF(sys_wl_ref().EQ.9.0) INDEX=surf_refractive_index(surfIndex, 9)
   IF(sys_wl_ref().EQ.10.0) INDEX=surf_refractive_index(surfIndex, 10)

   !PRINT *, "ALENS IS ", ALENS(46:50,surfIndex)
!
!       THE PRIMARY WAVELENGTH PAIR NUMBERS ARE STORED IN
!       sys_wl_pri1() AND sys_wl_pri2()
!
   IF(sys_wl_pri1().EQ.1.0) DISA=surf_refractive_index(surfIndex, 1)
   IF(sys_wl_pri1().EQ.2.0) DISA=surf_refractive_index(surfIndex, 2)
   IF(sys_wl_pri1().EQ.3.0) DISA=surf_refractive_index(surfIndex, 3)
   IF(sys_wl_pri1().EQ.4.0) DISA=surf_refractive_index(surfIndex, 4)
   IF(sys_wl_pri1().EQ.5.0) DISA=surf_refractive_index(surfIndex, 5)
   IF(sys_wl_pri1().EQ.6.0) DISA=surf_refractive_index(surfIndex, 6)
   IF(sys_wl_pri1().EQ.7.0) DISA=surf_refractive_index(surfIndex, 7)
   IF(sys_wl_pri1().EQ.8.0) DISA=surf_refractive_index(surfIndex, 8)
   IF(sys_wl_pri1().EQ.9.0) DISA=surf_refractive_index(surfIndex, 9)
   IF(sys_wl_pri1().EQ.10.0) DISA=surf_refractive_index(surfIndex, 10)
   IF(sys_wl_pri2().EQ.1.0) DISB=surf_refractive_index(surfIndex, 1)
   IF(sys_wl_pri2().EQ.2.0) DISB=surf_refractive_index(surfIndex, 2)
   IF(sys_wl_pri2().EQ.3.0) DISB=surf_refractive_index(surfIndex, 3)
   IF(sys_wl_pri2().EQ.4.0) DISB=surf_refractive_index(surfIndex, 4)
   IF(sys_wl_pri2().EQ.5.0) DISB=surf_refractive_index(surfIndex, 5)
   IF(sys_wl_pri2().EQ.6.0) DISB=surf_refractive_index(surfIndex, 6)
   IF(sys_wl_pri2().EQ.7.0) DISB=surf_refractive_index(surfIndex, 7)
   IF(sys_wl_pri2().EQ.8.0) DISB=surf_refractive_index(surfIndex, 8)
   IF(sys_wl_pri2().EQ.9.0) DISB=surf_refractive_index(surfIndex, 9)
   IF(sys_wl_pri2().EQ.10.0) DISB=surf_refractive_index(surfIndex, 9)
!
   DISP=DABS(DISA)-DABS(DISB)
!       CALC V-NUMBER
!
   IF(DISP.EQ.0.0D0) THEN
      VNUM=0.0D0
      PARTL=0.0D0
   ELSE
      VNUM=(DABS(INDEX)-1.0)/(DISP)
      PARTL=(DABS(INDEX)-DISB)/(DISP)
   END IF
   RETURN
END

! SUB SINDEX.FOR
SUBROUTINE SINDEX
!
   use DATLEN
   use mod_system
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SINDEX. THIS IS THE SUBROUTINE WHICH
!       HANDLES THE INDEX,DISPERSION AND V-NUMBER CALCULATIONS
!       FOR THE RTG AND CTG COMMANDS
!
   REAL*8 INDEX,DISP,VNUM,DISA,DISB,PARTL
!
   INTEGER NF
!
   COMMON/NSIN/NF,INDEX,DISP,VNUM,PARTL
!
!
!       THE CONTROL WAVLENGTH NUMBER IS STORED IN sys_wl_ref()
!
   IF(sys_wl_ref().EQ.1.0) INDEX=surf_refractive_index(NF, 1)
   IF(sys_wl_ref().EQ.2.0) INDEX=surf_refractive_index(NF, 2)
   IF(sys_wl_ref().EQ.3.0) INDEX=surf_refractive_index(NF, 3)
   IF(sys_wl_ref().EQ.4.0) INDEX=surf_refractive_index(NF, 4)
   IF(sys_wl_ref().EQ.5.0) INDEX=surf_refractive_index(NF, 5)
   IF(sys_wl_ref().EQ.6.0) INDEX=surf_refractive_index(NF, 6)
   IF(sys_wl_ref().EQ.7.0) INDEX=surf_refractive_index(NF, 7)
   IF(sys_wl_ref().EQ.8.0) INDEX=surf_refractive_index(NF, 8)
   IF(sys_wl_ref().EQ.9.0) INDEX=surf_refractive_index(NF, 9)
   IF(sys_wl_ref().EQ.10.0) INDEX=surf_refractive_index(NF, 10)
!
!       THE PRIMARY WAVELENGTH PAIR NUMBERS ARE STORED IN
!       sys_wl_pri1() AND sys_wl_pri2()
!
   IF(sys_wl_pri1().EQ.1.0) DISA=surf_refractive_index(NF, 1)
   IF(sys_wl_pri1().EQ.2.0) DISA=surf_refractive_index(NF, 2)
   IF(sys_wl_pri1().EQ.3.0) DISA=surf_refractive_index(NF, 3)
   IF(sys_wl_pri1().EQ.4.0) DISA=surf_refractive_index(NF, 4)
   IF(sys_wl_pri1().EQ.5.0) DISA=surf_refractive_index(NF, 5)
   IF(sys_wl_pri1().EQ.6.0) DISA=surf_refractive_index(NF, 6)
   IF(sys_wl_pri1().EQ.7.0) DISA=surf_refractive_index(NF, 7)
   IF(sys_wl_pri1().EQ.8.0) DISA=surf_refractive_index(NF, 8)
   IF(sys_wl_pri1().EQ.9.0) DISA=surf_refractive_index(NF, 9)
   IF(sys_wl_pri1().EQ.10.0) DISA=surf_refractive_index(NF, 10)
   IF(sys_wl_pri2().EQ.1.0) DISB=surf_refractive_index(NF, 1)
   IF(sys_wl_pri2().EQ.2.0) DISB=surf_refractive_index(NF, 2)
   IF(sys_wl_pri2().EQ.3.0) DISB=surf_refractive_index(NF, 3)
   IF(sys_wl_pri2().EQ.4.0) DISB=surf_refractive_index(NF, 4)
   IF(sys_wl_pri2().EQ.5.0) DISB=surf_refractive_index(NF, 5)
   IF(sys_wl_pri2().EQ.6.0) DISB=surf_refractive_index(NF, 6)
   IF(sys_wl_pri2().EQ.7.0) DISB=surf_refractive_index(NF, 7)
   IF(sys_wl_pri2().EQ.8.0) DISB=surf_refractive_index(NF, 8)
   IF(sys_wl_pri2().EQ.9.0) DISB=surf_refractive_index(NF, 9)
   IF(sys_wl_pri2().EQ.10.0) DISB=surf_refractive_index(NF, 9)
!
   DISP=DABS(DISA)-DABS(DISB)
!       CALC V-NUMBER
!
   IF(DISP.EQ.0.0D0) THEN
      VNUM=0.0D0
      PARTL=0.0D0
   ELSE
      VNUM=(DABS(INDEX)-1.0)/(DISP)
      PARTL=(DABS(INDEX)-DISB)/(DISP)
   END IF
   RETURN
END
! SUB SPERFECT.FOR
SUBROUTINE SPERFECT
!
   use DATLEN
   use mod_system
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SPERFECT WHICH IMPLEMENTS THE PERFECT
!       COMMAND AT THE LENS OF UPDATE LENS LEVEL.
!
   INTEGER PIKCNT,I
!
!
   IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1)THEN
      OUTLYNE='"PERFECT" TAKES NO EXPLICIT INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
!
   ELSE
   END IF
!
   ALENS(46:50,SURF)=1.0
   ALENS(71:75,SURF)=1.0
   GLANAM(SURF,1)='             '
   GLANAM(SURF,2)='PERFECT      '
!
!       NOW HANDEL EXISTING PIKUPS OF GLASS TYPE, I E DELETE THEM.
!       CHECK FOR CC PIKUPS AND DELETE IF FOUND
!
!
   IF(PIKUP(1,SURF,20).EQ.0.0) THEN
!
!       NO GLASS PIKUPS, JUST RETURN
!
      F22=1
      IF(F5.EQ.1.AND.SURF.LT.MAXSUR) THEN
         SURF=SURF+1
         ALENS(1:LSIZ,SURF)=0.0D0
         call set_sys_last_surf(DBLE(SURF))
      ELSE
!       DON'T INCREMENT SURF AND sys_last_surf()
      END IF
      RETURN
   ELSE
   END IF
!
!       DELETE THE PIKUP
   PIKUP(1:6,SURF,20)=0.0
   call set_surf_special_type(SURF, surf_special_type(SURF)-1)
!
!
!       PRINT MESSAGE
   WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GLASS) DELETED'
   CALL SHOWIT(1)
!
   PIKCNT=0
   DO 10 I=1,PSIZ
      IF(PIKUP(1,SURF,I).NE.0.0) THEN
         PIKCNT=PIKCNT+1
      ELSE
      END IF
10 CONTINUE
!
   IF(PIKCNT.EQ.0.0)call set_surf_special_type(SURF, 0)
   IF(F5.EQ.1.AND.SURF.LT.MAXSUR) THEN
      SURF=SURF+1
      ALENS(1:LSIZ,SURF)=0.0D0
      call set_sys_last_surf(DBLE(SURF))
   ELSE
!       DON'T INCREMENT SURF AND sys_last_surf()
   END IF
!
   F22=1
   CALL MACFAL
   RETURN
END
! SUB SIDEAL.FOR
SUBROUTINE SIDEAL
!
   use DATLEN
   use mod_system
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SIDEAL WHICH IMPLEMENTS THE IDEAL
!       COMMAND AT THE LENS OF UPDATE LENS LEVEL.
!
   INTEGER PIKCNT,I
!
!
   IF(SQ.EQ.1.OR.SST.EQ.1)THEN
      OUTLYNE='"IDEAL" TAKES NO QUALIFIER OR STRING INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   ELSE
   END IF
   IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1)THEN
      OUTLYNE='"IDEAL" ONLY TAKES NUMERIC WORD #1 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   ELSE
   END IF
   IF(S1.EQ.0)THEN
      OUTLYNE='"IDEAL" REQUIRES EXPLICIT WORD #1 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   ELSE
   END IF
   IF(W1.EQ.0.0D0)THEN
      OUTLYNE='"IDEAL" REQUIRES NON-ZERO NUMERIC WORD #1 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   ELSE
   END IF
!
   call set_surf_ideal_efl(SURF, W1)
   ALENS(46:50,SURF)=1.0
   ALENS(71:75,SURF)=1.0
   GLANAM(SURF,1)='             '
   GLANAM(SURF,2)='IDEAL        '
!
!       NOW HANDEL EXISTING PIKUPS OF GLASS TYPE, I E DELETE THEM.
!       CHECK FOR CC PIKUPS AND DELETE IF FOUND
!
!
   IF(PIKUP(1,SURF,20).EQ.0.0) THEN
!
!       NO GLASS PIKUPS, JUST RETURN
!
      F22=1
      IF(F5.EQ.1.AND.SURF.LT.MAXSUR) THEN
         SURF=SURF+1
         ALENS(1:LSIZ,SURF)=0.0D0
         call set_sys_last_surf(DBLE(SURF))
      ELSE
!       DON'T INCREMENT SURF AND sys_last_surf()
      END IF
      RETURN
   ELSE
   END IF
!
!       DELETE THE PIKUP
   PIKUP(1:6,SURF,20)=0.0
   call set_surf_special_type(SURF, surf_special_type(SURF)-1)
!
!
!       PRINT MESSAGE
   WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GLASS) DELETED'
   CALL SHOWIT(1)
!
   PIKCNT=0
   DO 10 I=1,PSIZ
      IF(PIKUP(1,SURF,I).NE.0.0) THEN
         PIKCNT=PIKCNT+1
      ELSE
      END IF
10 CONTINUE
!
   IF(PIKCNT.EQ.0.0)call set_surf_special_type(SURF, 0)
   IF(F5.EQ.1.AND.SURF.LT.MAXSUR) THEN
      SURF=SURF+1
      ALENS(1:LSIZ,SURF)=0.0D0
      call set_sys_last_surf(DBLE(SURF))
   ELSE
!       DON'T INCREMENT SURF AND sys_last_surf()
   END IF
!
   F22=1
   CALL MACFAL
   RETURN
END
! SUB LQUERY.FOR
SUBROUTINE LQUERY
!
   use DATCFG
   use DATLEN
   use mod_system
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS SUBROUTINE DISPLAYS THE CURRENT VALUE OF A LENS
!       SYSTEM PARAMETER FROM WITHIN THE LENS INPUT
!       LEVEL IN RESPONSE TO A COMMAND NAME FOLLOWED BY A ?
!
   REAL*8 VALUE1,VALUE2,VALUE3,VALUE4,VALUE5,VALUE6
!
   INTEGER V1,V2,V3,V4,V5,V6,VA1,VA1WS1,VA1WS2,VA1WS3,VA1WS4 ,MAT,REPEAT,I
!
   CHARACTER VAL*80,VALWS1*80,VALWS2*80,VALWS3*80 ,VALWS4*80
!
!
   VALUE1=0.0D0
   VALUE2=0.0D0
   VALUE3=0.0D0
   VALUE4=0.0D0
   VALUE5=0.0D0
   VALUE6=0.0D0
   VAL=AA//AA//AA//AA
   VALWS1=AA//AA//AA//AA
   VALWS2=AA//AA//AA//AA
   VALWS3=AA//AA//AA//AA
   VALWS4=AA//AA//AA//AA
   V1=0
   V2=0
   V3=0
   V4=0
   V5=0
   V6=0
   VA1=0
   VA1WS1=0
   VA1WS2=0
   VA1WS3=0
   VA1WS4=0
   REPEAT=0
!
   MAT=0
   IF(WC.EQ.'AIR') MAT=1
   IF(WC.EQ.'REFL') MAT=1
   IF(WC.EQ.'REFLTIRO') MAT=1
   IF(WC.EQ.'REFLTIR') MAT=1
   IF(WC.EQ.'PERFECT') MAT=1
   IF(WC.EQ.'IDEAL') MAT=1
   IF(WC.EQ.'GLAK') MAT=1
   IF(WC.EQ.'GLASS') MAT=1
   IF(WC.EQ.'MODEL') MAT=1
   IF(WC.EQ.'OHARA') MAT=1
   IF(WC.EQ.'HOYA') MAT=1
   IF(WC.EQ.'HIKARI') MAT=1
   IF(WC.EQ.'SCHOTT') MAT=1
   IF(WC.EQ.'SCH2000') MAT=1
   IF(WC.EQ.'CHANCE') MAT=1
   IF(WC.EQ.'CORNIN') MAT=1
   IF(WC.EQ.'USER') MAT=1
   IF(WC.EQ.'RADHARD') MAT=1
   IF(WC.EQ.'GLCAT') MAT=1
   IF(WC.EQ.'MATL') MAT=1
   IF(WC.EQ.'RUSSIAN') MAT=1
   IF(WC.NE.'IDEAL') THEN
      IF(MAT.EQ.1) THEN
         VA1WS1=1
         VALWS1=GLANAM(SURF,1)//GLANAM(SURF,2)
         VAL='CURRENT SURFACE MATERIAL TYPE IS:'
         VA1=1
         IF(WC.EQ.'MODEL') THEN
            V1=1
            VALUE1=surf_fict_n(SURF)
            V2=1
            VALUE2=surf_fict_v(SURF)
            V3=1
            VALUE3=surf_fict_w(SURF)
         END IF
         GO TO 200
      ELSE
      END IF
      IF(WC.EQ.'N1'.OR.WC.EQ.'N2'.OR.WC.EQ.'N3'.OR.WC.EQ.'N4'.OR.WC.EQ.'N5'.OR.WC.EQ.'N6'.OR.WC.EQ.'N7'.OR.WC.EQ.'N8'.OR.WC.EQ.'N9'.OR.WC.EQ.'N10') THEN
         IF((SURF-1).LT.0) THEN
            OUTLYNE='INDEX CHANGE COMMANDS WORK ON THE PREVIOUS SURFACE IN LENS'
            CALL SHOWIT(1)
            OUTLYNE='INPUT MODE BUT THERE ARE NO SURFACES AS YET'
            OUTLYNE='NO VALUE TO DISPLAY'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(WC.EQ.'N1') THEN
            VA1=1
            VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #1 IS:'
            V1=1
            VALUE1=surf_refractive_index(SURF-1, 1)
            GO TO 200
         ELSE
         END IF
         IF(WC.EQ.'N2') THEN
            VA1=1
            VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #2 IS:'
            V1=1
            VALUE1=surf_refractive_index(SURF-1, 2)
            GO TO 200
         ELSE
         END IF
         IF(WC.EQ.'N3') THEN
            VA1=1
            VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #3 IS:'
            V1=1
            VALUE1=surf_refractive_index(SURF-1, 3)
            GO TO 200
         ELSE
         END IF
         IF(WC.EQ.'N4') THEN
            VA1=1
            VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #4 IS:'
            V1=1
            VALUE1=surf_refractive_index(SURF-1, 4)
            GO TO 200
         ELSE
         END IF
         IF(WC.EQ.'N5') THEN
            VA1=1
            VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #5 IS:'
            V1=1
            VALUE1=surf_refractive_index(SURF-1, 5)
            GO TO 200
         ELSE
         END IF
         IF(WC.EQ.'N6') THEN
            VA1=1
            VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #6 IS:'
            V1=1
            VALUE1=surf_refractive_index(SURF-1, 6)
            GO TO 200
         ELSE
         END IF
         IF(WC.EQ.'N7') THEN
            VA1=1
            VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #7 IS:'
            V1=1
            VALUE1=surf_refractive_index(SURF-1, 7)
            GO TO 200
         ELSE
         END IF
         IF(WC.EQ.'N8') THEN
            VA1=1
            VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #8 IS:'
            V1=1
            VALUE1=surf_refractive_index(SURF-1, 8)
            GO TO 200
         ELSE
         END IF
         IF(WC.EQ.'N9') THEN
            VA1=1
            VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #9 IS:'
            V1=1
            VALUE1=surf_refractive_index(SURF-1, 9)
            GO TO 200
         ELSE
         END IF
         IF(WC.EQ.'N10') THEN
            VA1=1
            VAL='THE REFRACTIVE INDEX FOR WAVELENGTH #10 IS:'
            V1=1
            VALUE1=surf_refractive_index(SURF-1, 10)
            GO TO 200
         ELSE
         END IF
      ELSE
      END IF
   ELSE
!     WC IS 'IDEAL'
      IF(MAT.EQ.1) THEN
         VA1WS1=1
         V1=1
         VALWS1=GLANAM(SURF,1)//GLANAM(SURF,2)
         VAL='CURRENT SURFACE MATERIAL TYPE IS:'
         VALUE1=surf_ideal_efl(SURF)
         VA1=1
      ELSE
      END IF
   END IF
   IF(WC.EQ.'UNITS') THEN
      IF(sys_units().EQ.1.0D0) VALWS1='INCHES'
      IF(sys_units().EQ.2.0D0) VALWS1='CM    '
      IF(sys_units().EQ.3.0D0) VALWS1='MM    '
      IF(sys_units().EQ.4.0D0) VALWS1='METERS'
      VA1WS1=1
      VAL='CURRENT SYSTEM UNITS ARE:'
      VA1=1
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'REAL'.OR.WC.EQ.'PARAX') THEN
      IF(surf_paraxial_val(SURF).EQ.0.0D0) VALWS1='REAL'
      IF(surf_paraxial_val(SURF).EQ.1.0D0) VALWS1='PARAX'
      VA1WS1=1
      VAL='SURFACE TYPE IS:'
      VA1=1
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'INDEX'.OR.WC.EQ.'VNUM'.OR.WC.EQ.'DPART') THEN
      IF(GLANAM(SURF-1,1).NE.'MODEL') THEN
         OUTLYNE='NO "INDEX", "VNUM" OR "DPART" VALUES EXIST'
         CALL SHOWIT(1)
         OUTLYNE='FOR NON-"MODEL" GLASSES'
         CALL SHOWIT(1)
      ELSE
         IF((SURF-1).LT.0) THEN
            OUTLYNE='"INDEX", "VNUM" AND "DPART"'
            CALL SHOWIT(1)
            OUTLYNE='COMMANDS WORK ON THE PREVIOUS SURFACE IN LENS'
            CALL SHOWIT(1)
            OUTLYNE='INPUT MODE BUT THERE ARE NO SURFACES AS YET'
            OUTLYNE='NO VALUE TO DISPLAY'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(WC.EQ.'INDEX') THEN
            VA1=1
            VAL='THE "INDEX" VALUE IS:'
            V1=1
            VALUE1=surf_fict_n(SURF-1)
            GO TO 200
         END IF
         IF(WC.EQ.'VNUM') THEN
            VA1=1
            VAL='THE "VNUM" VALUE IS:'
            V1=1
            VALUE1=surf_fict_v(SURF-1)
            GO TO 200
         END IF
         IF(WC.EQ.'DPART') THEN
            VA1=1
            VAL='THE "DPART" VALUE IS:'
            V1=1
            VALUE1=surf_fict_w(SURF-1)
            GO TO 200
         END IF
      END IF
   END IF
   IF(WC.EQ.'UNITS') THEN
      IF(sys_units().EQ.1.0D0) VALWS1='INCHES'
      IF(sys_units().EQ.2.0D0) VALWS1='CM    '
      IF(sys_units().EQ.3.0D0) VALWS1='MM    '
      IF(sys_units().EQ.4.0D0) VALWS1='METERS'
      VA1WS1=1
      VAL='CURRENT SYSTEM UNITS ARE:'
      VA1=1
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'LBL'.OR.WC.EQ.'LABEL') THEN
      VAL=LBL(SURF)
      IF(surf_label_flag(SURF).EQ.0.0D0)VAL='"THERE IS NO LABEL FOR THE CURRENT SURFACE"'
      VA1=1
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'LI') THEN
      VA1=1
      VAL='THE CURRENT LENS IDENTIFIER IS:'
      VA1WS1=1
      VALWS1=LI
      IF(LI.EQ.AA) VALWS1='"LI IS ALL BLANK"'
      VA1WS1=1
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'MFG') THEN
      VA1=1
      VAL='THE CURRENT MFG IS:'
      VA1WS1=1
      VALWS1=MFG
      IF(MFG.EQ.AA) VALWS1='"MFG IS ALL BLANK"'
      VA1WS1=1
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'CATNUM') THEN
      VA1=1
      VAL='THE CURRENT CATNUM IS:'
      VA1WS1=1
      VALWS1=CATNUM
      IF(CATNUM.EQ.AA) VALWS1='"CATNUM IS ALL BLANK"'
      VA1WS1=1
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'INI') THEN
      VA1=1
      VAL='THE CURRENT DESIGNER IDENTIFIER IS:'
      VA1WS1=1
      VALWS1=INNI
      IF(INNI.EQ.AA) VALWS1='"INI IS ALL BLANK"'
      VA1WS1=1
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'LTYPE') THEN
      VA1=1
      VAL='THE CURRENT LENS TYPE IDENTIFIER IS:'
      VA1WS1=1
      VALWS1=LLTYPE(1:5)
      IF(INNI.EQ.AA) VALWS1='"LTYPE IS ALL BLANK"'
      VA1WS1=1
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'LIC') THEN
      VAL='THE CURRENT LENS IDENTIFIER CONTINUATION IS:'
      VA1=1
      VALWS1=LIC(1)
      IF(LIC(1).EQ.AA) VALWS1='"LIC (LINE 1) IS ALL BLANK"'
      VA1WS1=1
      VALWS2=LIC(2)
      IF(LIC(2).EQ.AA) VALWS2='"LIC (LINE 2) IS ALL BLANK"'
      VA1WS2=1
      VALWS3=LIC(3)
      IF(LIC(3).EQ.AA) VALWS3='"LIC (LINE 3) IS ALL BLANK"'
      VA1WS3=1
      VALWS4=LIC(4)
      IF(LIC(4).EQ.AA) VALWS4='"LIC (LINE 4) IS ALL BLANK"'
      VA1WS4=1
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'REFS') THEN
      VA1=1
      VAL='CURRENT REFERENCE SURFACE NUMBER AND ORIENTATION ANGLE(DEG) ARE:'
      V1=1
      VALUE1=sys_ref_surf()
      V2=1
      VALUE2=sys_ref_orient()
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'SAY'.OR.WC.EQ.'SAX') THEN
      IF(sys_nao_flag().NE.0.0D0) THEN
         WRITE(OUTLYNE,*)'"'//WC(1:3)//'" HAS NOT BEEN EXPLICITLY SET'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'BUT IS HELD WITH AN "NAO(X OR Y)" ASSIGNMENT'
         CALL SHOWIT(1)
      ELSE
      END IF
      IF(sys_fno_flag().NE.0.0D0) THEN
         WRITE(OUTLYNE,*)'"'//WC(1:3)//'" HAS NOT BEEN EXPLICITLY SET'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'BUT IS HELD WITH AN "FNO(X OR Y)" ASSIGNMENT'
         CALL SHOWIT(1)
      ELSE
      END IF
      IF(sys_say_float().NE.0.0D0) THEN
         WRITE(OUTLYNE,*)'"SAY" IS CURRENTLY FLOATING'
         CALL SHOWIT(1)
      ELSE
      END IF
      IF(sys_sax_float().NE.0.0D0) THEN
         WRITE(OUTLYNE,*)'"SAX" IS CURRENTLY FLOATING'
         CALL SHOWIT(1)
      ELSE
      END IF
      VA1=1
      IF(WC.EQ.'SAY') VAL='CURRENT "SAY" VALUE IS:'
      IF(WC.EQ.'SAX') VAL='CURRENT "SAX" VALUE IS:'
      V1=1
      IF(WC.EQ.'SAY') VALUE1=sys_say()
      IF(WC.EQ.'SAX') VALUE1=sys_sax()
      GO TO 200
   ELSE
   END IF
!
   IF(WC.EQ.'WRY'.OR.WC.EQ.'WRX'.OR.WC.EQ.'BDX'.OR.WC.EQ.'BDY') THEN
      VA1=1
      IF(WC.EQ.'WRY') VAL='CURRENT "WRY" VALUE IS:'
      IF(WC.EQ.'WRX') VAL='CURRENT "WRX" VALUE IS:'
      IF(WC.EQ.'BDY') VAL='CURRENT "BDY" VALUE IS:'
      IF(WC.EQ.'BDX') VAL='CURRENT "BDX" VALUE IS:'
      V1=1
      IF(WC.EQ.'WRX') VALUE1=sys_wrx()
      IF(WC.EQ.'WRY') VALUE1=sys_wry()
      IF(WC.EQ.'BDX') VALUE1=sys_bdx()
      IF(WC.EQ.'BDY') VALUE1=sys_bdy()
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'NAOY'.OR.WC.EQ.'NAOX') THEN
      IF(sys_nao_flag().NE.1.0D0) THEN
         OUTLYNE='"'//WC(1:4)//'" HAS NOT BEEN EXPLICITLY SET'
         CALL SHOWIT(1)
         IF(INT(sys_wl_ref()).GE.1.AND.INT(sys_wl_ref()).LE.5) THEN
            call set_sys_nao_y((ALENS(45+INT(sys_wl_ref()),0)*sys_say())/DSQRT((surf_thickness(0)**2)+(sys_say()**2)))
            call set_sys_nao_x((ALENS(45+INT(sys_wl_ref()),0)*sys_sax())/DSQRT((surf_thickness(0)**2)+(sys_sax()**2)))
            call set_sys_say_float(0.0D0)
            call set_sys_sax_float(0.0D0)
         END IF
         IF(INT(sys_wl_ref()).GE.6.AND.INT(sys_wl_ref()).LE.10) THEN
            call set_sys_nao_y((ALENS(70-5+INT(sys_wl_ref()),0)*sys_say())/DSQRT((surf_thickness(0)**2)+(sys_say()**2)))
            call set_sys_nao_x((ALENS(70-5+INT(sys_wl_ref()),0)*sys_sax())/DSQRT((surf_thickness(0)**2)+(sys_sax()**2)))
            call set_sys_say_float(0.0D0)
            call set_sys_sax_float(0.0D0)
         END IF
      ELSE
      END IF
      VA1=1
      IF(WC.EQ.'NAOY') VAL='CURRENT "NAOY" VALUE IS:'
      IF(WC.EQ.'NAOX') VAL='CURRENT "NAOX" VALUE IS:'
      V1=1
      IF(WC.EQ.'NAOY') VALUE1=sys_nao_y()
      IF(WC.EQ.'NAOX') VALUE1=sys_nao_x()
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'FNOY'.OR.WC.EQ.'FNOX') THEN
      IF(sys_fno_flag().NE.1.0D0) THEN
         OUTLYNE='"'//WC(1:4)//'" HAS NOT BEEN EXPLICITLY SET'
         CALL SHOWIT(1)
         call set_sys_fno_y(1.0D0/((2.0D0*sys_say())/surf_thickness(0)))
         call set_sys_fno_x(1.0D0/((2.0D0*sys_sax())/surf_thickness(0)))
         call set_sys_say_float(0.0D0)
         call set_sys_sax_float(0.0D0)
      ELSE
      END IF
      VA1=1
      IF(WC.EQ.'FNOY') VAL='CURRENT "FNOY" VALUE IS:'
      IF(WC.EQ.'FNOX') VAL='CURRENT "FNOX" VALUE IS:'
      V1=1
      IF(WC.EQ.'FNOY') VALUE1=sys_fno_y()
      IF(WC.EQ.'FNOX') VALUE1=sys_fno_x()
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'SCY'.OR.WC.EQ.'SCX') THEN
      VA1=1
      IF(WC.EQ.'SCY'.AND.sys_scy_fang_set().EQ.0.0D0)VAL='CURRENT "SCY" VALUES ARE:'
      IF(WC.EQ.'SCY'.AND.sys_scy_fang_set().EQ.1.0D0)VAL='CURRENT "SCY FANG" VALUES ARE:'
      IF(WC.EQ.'SCX'.AND.sys_scx_fang_set().EQ.0.0D0)VAL='CURRENT "SCX" VALUES ARE:'
      IF(WC.EQ.'SCX'.AND.sys_scx_fang_set().EQ.1.0D0)VAL='CURRENT "SCX FANG" VALUES ARE:'
      V1=1
      V2=1
      IF(WC.EQ.'SCY'.AND.sys_scy_fang_set().EQ.0.0D0) THEN
         VALUE1=sys_scy()
         VALUE2=sys_scy_y1()
      ELSE
      END IF
      IF(WC.EQ.'SCY'.AND.sys_scy_fang_set().EQ.1.0D0) THEN
         VALUE1=sys_fang_y()
         VALUE2=sys_fang_y_y1()
      ELSE
      END IF
      IF(WC.EQ.'SCX'.AND.sys_scx_fang_set().EQ.0.0D0) THEN
         VALUE1=sys_scx()
         VALUE2=sys_scx_x1()
      ELSE
      END IF
      IF(WC.EQ.'SCX'.AND.sys_scx_fang_set().EQ.1.0D0) THEN
         VALUE1=sys_fang_x()
         VALUE2=sys_fang_x_x1()
      ELSE
      END IF
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'PXIM'.OR.WC.EQ.'PYIM') THEN
      VA1=1
      IF(WC.EQ.'PXIM'.AND.sys_pxim_fang_set().EQ.0.0D0)VAL='CURRENT "PXIM" VALUES ARE:'
      IF(WC.EQ.'PXIM'.AND.sys_pxim_fang_set().EQ.1.0D0)VAL='CURRENT "PXIM FANG" VALUES ARE:'
      IF(WC.EQ.'PYIM'.AND.sys_pyim_fang_set().EQ.0.0D0)VAL='CURRENT "PYIM" VALUES ARE:'
      IF(WC.EQ.'PYIM'.AND.sys_pyim_fang_set().EQ.1.0D0)VAL='CURRENT "PYIM FANG" VALUES ARE:'
      V1=1
      IF(WC.EQ.'PXIM') THEN
         VALUE1=sys_pxim()
      ELSE
      END IF
      IF(WC.EQ.'PYIM') THEN
         VALUE1=sys_pyim()
      ELSE
      END IF
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'RXIM'.OR.WC.EQ.'RYIM') THEN
      VA1=1
      IF(WC.EQ.'RXIM'.AND.sys_rxim_fang_set().EQ.0.0D0)VAL='CURRENT "RXIM" VALUES ARE:'
      IF(WC.EQ.'RXIM'.AND.sys_rxim_fang_set().EQ.1.0D0)VAL='CURRENT "RXIM FANG" VALUES ARE:'
      IF(WC.EQ.'RYIM'.AND.sys_ryim_fang_set().EQ.0.0D0)VAL='CURRENT "RYIM" VALUES ARE:'
      IF(WC.EQ.'RXIM'.AND.sys_ryim_fang_set().EQ.1.0D0)VAL='CURRENT "RYIM FANG" VALUES ARE:'
      V1=1
      IF(WC.EQ.'RXIM') THEN
         VALUE1=sys_rxim()
      ELSE
      END IF
      IF(WC.EQ.'RYIM') THEN
         VALUE1=sys_ryim()
      ELSE
      END IF
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'ASTOP') THEN
      VA1=1
      VAL='CURRENT APERTURE STOP SURFACE NUMBER IS:'
      V1=1
      VALUE1=sys_astop()
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'AUTOFUNC') THEN
      VA1=1
      VAL='CURRENT AUTOFUNC FUNCTION NUMBER IS:'
      V1=1
      VALUE1=sys_autofunc()
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'DEFORM') THEN
      IF(surf_default_flag(SURF).EQ.0.0D0) THEN
         VA1=1
         VAL='CURRENT SURFACE IS NOT A DEFORMABLE SURFACE'
      ELSE
         VA1=1
         VAL='CURRENT SURFACE IS A DEFORMABLE SURFACE'
         V1=1
         VALUE1=surf_mtracei_nx(SURF)
         V2=1
         VALUE2=surf_mtracei_ny(SURF)
         V3=1
         VALUE2=surf_psfbin_data(SURF)
      END IF
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'SPTWT') THEN
      VA1=1
      VAL='CURRENT SPECTRAL WEIGHTING FACTORS (1-5) ARE:'
      V1=1
      VALUE1=sys_wl_weight(1)
      V2=1
      VALUE2=sys_wl_weight(2)
      V3=1
      VALUE3=sys_wl_weight(3)
      V4=1
      VALUE4=sys_wl_weight(4)
      V5=1
      VALUE5=sys_wl_weight(5)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'SPTWT2') THEN
      VA1=1
      VAL='CURRENT SPECTRAL WEIGHTING FACTORS (6-10) ARE:'
      V1=1
      VALUE1=sys_wl_weight(6)
      V2=1
      VALUE2=sys_wl_weight(7)
      V3=1
      VALUE3=sys_wl_weight(8)
      V4=1
      VALUE4=sys_wl_weight(9)
      V5=1
      VALUE5=sys_wl_weight(10)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'CW') THEN
      VA1=1
      VAL='CURRENT CONTROL WAVELENGTH NUMBER IS:'
      V1=1
      VALUE1=sys_wl_ref()
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'PCW') THEN
      VA1=1
      VAL='CURRENT PRIMARY WAVELENGTH PAIR NUMBERS ARE:'
      V1=1
      VALUE1=sys_wl_pri1()
      V2=1
      VALUE2=sys_wl_pri2()
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'SCW') THEN
      VA1=1
      VAL='CURRENT SECONDARY WAVELENGTH PAIR NUMBERS ARE:'
      V1=1
      VALUE1=sys_wl_sec1()
      V1=2
      VALUE1=sys_wl_sec2()
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'MODE') THEN
      VA1=1
      IF(sys_mode().EQ.1) VAL='CURRENT MODE IS "FOCAL"'
      IF(sys_mode().EQ.2) VAL='CURRENT MODE IS "UFOCAL"'
      IF(sys_mode().EQ.3) VAL='CURRENT MODE IS "AFOCAL"'
      IF(sys_mode().EQ.4) VAL='CURRENT MODE IS "UAFOCAL"'
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'CV') THEN
      VA1=1
      VAL='CURRENT SURFACE CURVATURE IS:'
      V1=1
      VALUE1=surf_curvature(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'INR') THEN
      VA1=1
      VAL='CURRENT SURFACE "INR" VALUE IS:'
      V1=1
      VALUE1=surf_inr_value(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'CVTOR') THEN
      VA1=1
      VAL='CURRENT SURFACE TORIC CURVATURE IS:'
      V1=1
      VALUE1=surf_toric_curvature(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'TH') THEN
      VA1=1
      VAL='CURRENT SURFACE THICKNESS IS:'
      V1=1
      VALUE1=surf_thickness(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'THM') THEN
      VA1=1
      VAL='CURRENT MIRROR THICKNESS IS:'
      V1=1
      VALUE1=surf_mirror_thickness(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'PRICE') THEN
      VA1=1
      VAL='CURRENT PRICE PER UNIT MASS IS:'
      V1=1
      VALUE1=surf_price(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'AC') THEN
      VA1=1
      VAL='CURRENT 2ND ORDER ASPHERIC SURFACE COEFFICIENT IS:'
      V1=1
      VALUE1=surf_asphere_coeff(SURF, 2)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'AD') THEN
      VA1=1
      VAL='CURRENT 4TH ORDER ASPHERIC SURFACE COEFFICIENT IS:'
      V1=1
      VALUE1=surf_asphere_coeff(SURF, 4)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'AE') THEN
      VA1=1
      VAL='CURRENT 6TH ORDER ASPHERIC SURFACE COEFFICIENT IS:'
      V1=1
      VALUE1=surf_asphere_coeff(SURF, 6)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'AF') THEN
      VA1=1
      VAL='CURRENT 8TH ORDER ASPHERIC SURFACE COEFFICIENT IS:'
      V1=1
      VALUE1=surf_asphere_coeff(SURF, 8)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'AG') THEN
      VA1=1
      VAL='CURRENT 10TH ORDER ASPHERIC SURFACE COEFFICIENT IS:'
      V1=1
      VALUE1=surf_asphere_coeff(SURF, 10)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'AH') THEN
      VA1=1
      VAL='CURRENT 12TH ORDER ASPHERIC SURFACE COEFFICIENT IS:'
      V1=1
      VALUE1=surf_asphere_coeff(SURF, 12)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'AI') THEN
      VA1=1
      VAL='CURRENT 14TH ORDER ASPHERIC SURFACE COEFFICIENT IS:'
      V1=1
      VALUE1=surf_asphere_coeff(SURF, 14)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'AJ') THEN
      VA1=1
      VAL='CURRENT 16TH ORDER ASPHERIC SURFACE COEFFICIENT IS:'
      V1=1
      VALUE1=surf_asphere_coeff(SURF, 16)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'AK') THEN
      VA1=1
      VAL='CURRENT 18TH ORDER ASPHERIC SURFACE COEFFICIENT IS:'
      V1=1
      VALUE1=surf_asphere_coeff(SURF, 18)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'AL') THEN
      VA1=1
      VAL='CURRENT 20TH ORDER ASPHERIC SURFACE COEFFICIENT IS:'
      V1=1
      VALUE1=surf_asphere_coeff(SURF, 20)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'ARRAY'.AND.surf_array_parity(SURF).EQ.0.0D0) THEN
      VA1=1
      VAL='SURFACE IS CURRENTLY NOT AN ARRAY LENS'
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'ARRAY'.AND.surf_array_parity(SURF).EQ.-1.0D0) THEN
      VA1=1
      VAL='CURRENT ODD ARRAY SURFACE VALUES'
      VA1WS1=1
      VALWS1='(DX AND DY) ARE:'
      V1=1
      VALUE1=surf_array_dx(SURF)
      V2=1
      VALUE2=surf_array_dy(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'ARRAY'.AND.surf_array_parity(SURF).EQ.1.0D0) THEN
      VA1=1
      VAL='CURRENT EVEN ARRAY SURFACE VALUES'
      VA1WS1=1
      VALWS1='(DX AND DY) ARE:'
      V1=1
      VALUE1=surf_array_dx(SURF)
      V2=1
      VALUE2=surf_array_dy(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'ASPH') THEN
      VA1=1
      VAL='CURRENT ASPHERIC SURFACE COEFFICIENTS'
      VA1WS1=1
      VALWS1='(AD, AE, AF, AG AND AC) ARE:'
      V1=1
      VALUE1=surf_asphere_coeff(SURF, 4)
      V2=1
      VALUE2=surf_asphere_coeff(SURF, 6)
      V3=1
      VALUE3=surf_asphere_coeff(SURF, 8)
      V4=1
      VALUE4=surf_asphere_coeff(SURF, 10)
      V5=1
      VALUE5=surf_asphere_coeff(SURF, 2)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'ASPH2') THEN
      VA1=1
      VAL='CURRENT ASPHERIC SURFACE COEFFICIENTS'
      VA1WS1=1
      VALWS1='(AH, AI, AJ, AK AND AL) ARE:'
      V1=1
      VALUE1=surf_asphere_coeff(SURF, 12)
      V2=1
      VALUE2=surf_asphere_coeff(SURF, 14)
      V3=1
      VALUE3=surf_asphere_coeff(SURF, 16)
      V4=1
      VALUE4=surf_asphere_coeff(SURF, 18)
      V5=1
      VALUE5=surf_asphere_coeff(SURF, 20)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'TASPH') THEN
      VA1=1
      VAL='CURRENT ANAMORPHIC ASPHERIC SURFACE COEFFICIENTS'
      VA1WS1=1
      VALWS1='(ADTOR, AETOR, AFTOR AND AGTOR) ARE:'
      V1=1
      VALUE1=surf_anamorphic_coeff(SURF, 4)
      V2=1
      VALUE2=surf_anamorphic_coeff(SURF, 6)
      V3=1
      VALUE3=surf_anamorphic_coeff(SURF, 8)
      V4=1
      VALUE4=surf_anamorphic_coeff(SURF, 10)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'ADTOR') THEN
      VA1=1
      VAL='CURRENT 4TH ORDER ANAMORPHIC ASPHERIC SURFACE COEFFICIENT IS:'
      V1=1
      VALUE1=surf_anamorphic_coeff(SURF, 4)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'AETOR') THEN
      VA1=1
      VAL='CURRENT 6TH ORDER ANAMORPHIC ASPHERIC SURFACE COEFFICIENT IS:'
      V1=1
      VALUE1=surf_anamorphic_coeff(SURF, 6)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'AFTOR') THEN
      VA1=1
      VAL='CURRENT 8TH ORDER ANAMORPHIC ASPHERIC SURFACE COEFFICIENT IS:'
      V1=1
      VALUE1=surf_anamorphic_coeff(SURF, 8)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'AGTOR') THEN
      VA1=1
      VAL='CURRENT 10TH ORDER ANAMORPHIC ASPHERIC SURFACE COEFFICIENT IS:'
      V1=1
      VALUE1=surf_anamorphic_coeff(SURF, 10)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'CC') THEN
      VA1=1
      VAL='SURFACE CONIC CONSTANT IS:'
      V1=1
      VALUE1=surf_conic(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'CCTOR') THEN
      VA1=1
      VAL='CURRENT SURFACE ANAMORPHIC CONIC CONSTANT IS:'
      V1=1
      VALUE1=surf_anamorphic_conic(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'GDX') THEN
      VA1=1
      VAL='SURFACE GLOBAL X-DECENTER IS:'
      V1=1
      VALUE1=surf_global_dx(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'GDY') THEN
      VA1=1
      VAL='SURFACE GLOBAL Y-DECENTER IS:'
      V1=1
      VALUE1=surf_global_dy(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'GDZ') THEN
      VA1=1
      VAL='SURFACE GLOBAL Z-DECENTER IS:'
      V1=1
      VALUE1=surf_global_dz(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'GALPHA') THEN
      VA1=1
      VAL='SURFACE GLOBAL ALPHA TILT ANGLE IN DEGREES IS:'
      V1=1
      VALUE1=surf_global_alpha(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'GBETA') THEN
      VA1=1
      VAL='SURFACE GLOBAL BETA TILT ANGLE IN DEGREES IS:'
      V1=1
      VALUE1=surf_global_beta(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'GGAMMA') THEN
      VA1=1
      VAL='SURFACE GLOBAL GAMMA TILT ANGLE IN DEGREES IS:'
      V1=1
      VALUE1=surf_global_gamma(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'ALPHA') THEN
      VA1=1
      VAL='SURFACE ALPHA TILT ANGLE IN DEGREES IS:'
      V1=1
      VALUE1=surf_alpha_deg(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'BETA') THEN
      VA1=1
      VAL='SURFACE BETA TILT ANGLE IN DEGREES IS:'
      V1=1
      VALUE1=surf_beta_deg(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'GAMMA') THEN
      VA1=1
      VAL='SURFACE GAMMA TILT ANGLE IN DEGREES IS:'
      V1=1
      VALUE1=surf_gamma_deg(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'TILT'.OR.WC.EQ.'RTILT') THEN
      IF(surf_tilt_flag(SURF).EQ.0.0D0) THEN
         VA1=1
         VAL='SURFACE HAS NO TILT OR RTILT VALUES'
         GO TO 200
      ELSE
      END IF
      VA1WS1=1
      VALWS1='SURFACE TILTS (ALPHA, BETA AND GAMMA) IN DEGREES ARE:'
      VA1=1
      VAL='SURFACE IS NOT CURRENTLY TILTED'
      IF(surf_tilt_flag(SURF).EQ.1.0D0)VAL='CURRENT SURFACE HAS A "TILT" DEFINED ON IT'
      IF(surf_tilt_flag(SURF).EQ.-1.0D0)VAL='CURRENT SURFACE HAS AN "RTILT" DEFINED ON IT'
      IF(surf_tilt_flag(SURF).EQ.2.0D0)VAL='CURRENT SURFACE HAS A "TILT AUTO" DEFINED ON IT'
      IF(surf_tilt_flag(SURF).EQ.3.0D0)VAL='CURRENT SURFACE HAS A "TILT AUTOM" DEFINED ON IT'
      IF(surf_tilt_flag(SURF).EQ.4.0D0)VAL='CURRENT SURFACE HAS A "TILT BEN" DEFINED ON IT'
      IF(surf_tilt_flag(SURF).EQ.5.0D0)VAL='CURRENT SURFACE HAS A "TILT DAR" DEFINED ON IT'
      IF(surf_tilt_flag(SURF).EQ.7.0D0)VAL='CURRENT SURFACE HAS A "TILT REV" DEFINED ON IT'
      IF(surf_tilt_flag(SURF).EQ.6.0D0.OR.surf_tilt_flag(SURF).EQ.1.0D0.AND.surf_tilt_return_flag(SURF).EQ.1.0D0)VAL='CURRENT SURFACE HAS A "TILT RET" DEFINED ON IT'
      IF(surf_tilt_flag(SURF).EQ.6.0D0.OR.surf_tilt_flag(SURF).EQ.1.0D0 .AND.surf_tilt_return_flag(SURF).EQ.1.0D0) THEN
         V1=1
         VALUE1=surf_ret_surf_num(SURF)
      ELSE
         V1=1
         VALUE1=surf_alpha_deg(SURF)
         V2=1
         VALUE2=surf_beta_deg(SURF)
         V3=1
         VALUE3=surf_gamma_deg(SURF)
      END IF
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'XD') THEN
      VA1=1
      VAL='CURRENT SURFACE X-DECENTRATION IS:'
      V1=1
      VALUE1=surf_focus_dx(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'ZD') THEN
      VA1=1
      VAL='CURRENT SURFACE Z-DECENTRATION IS:'
      V1=1
      VALUE1=surf_focus_dz(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'YD') THEN
      VA1=1
      VAL='CURRENT SURFACE Y-DECENTRATION IS:'
      V1=1
      VALUE1=surf_focus_dy(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'DEC') THEN
      VA1=1
      VAL='CURRENT SURFACE Y, X AND Z DECENTRATIONS ARE:'
      V1=1
      VALUE1=surf_focus_dy(SURF)
      V2=1
      VALUE2=surf_focus_dx(SURF)
      V3=1
      VALUE3=surf_focus_dz(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'PIVOT') THEN
      VA1=1
      VAL='CURRENT SURFACE ALTERNATE PIVOT DEFINITIONS ARE:'
      V1=1
      VALUE1=surf_pivot_x(SURF)
      V2=1
      VALUE2=surf_pivot_y(SURF)
      V3=1
      VALUE3=surf_pivot_z(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'PIVX') THEN
      VA1=1
      VAL='CURRENT SURFACE X-PIVOT POSITION IS:'
      V1=1
      VALUE1=surf_pivot_x(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'PIVY') THEN
      VA1=1
      VAL='CURRENT SURFACE Y-PIVOT POSITION IS:'
      V1=1
      VALUE1=surf_pivot_y(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'PIVZ') THEN
      VA1=1
      VAL='CURRENT SURFACE Z-PIVOT POSITION IS:'
      V1=1
      VALUE1=surf_pivot_z(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'RD') THEN
      VA1=1
      VAL='CURRENT SURFACE RADIUS OF CURVATURE IS:'
      IF(DABS(surf_curvature(SURF)).LT.1D-30) THEN
         V1=1
         VALUE1=0.0D0
      ELSE
         V1=1
         VALUE1=1.0D0/surf_curvature(SURF)
      END IF
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'XTORIC'.OR.WC.EQ.'YTORIC') THEN
      VA1=1
      VAL='THE SURFACE IS CURRENTLY:'
      IF(surf_toric_flag(SURF).EQ.0.0D0)VALWS1='NOT DEFINED AS A TORIC'
      IF(surf_toric_flag(SURF).EQ.1.0D0)VALWS1='DEFINED AS A Y-TORIC'
      IF(surf_toric_flag(SURF).EQ.2.0D0)VALWS1='DEFINED AS AN X-TORIC'
      VA1WS1=1
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'GRT') THEN
      VA1=1
      VAL='THE SURFACE IS CURRENTLY:'
      IF(surf_diffraction_flag(SURF).EQ.0.0D0)VALWS1='NOT A LINEAR DIFFRATION GRATING'
      IF(surf_diffraction_flag(SURF).EQ.1.0D0)VALWS1='A LINEAR DIFFRATION GRATING'
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'GRO') THEN
      IF(surf_diffraction_flag(SURF).EQ.0.0D0) THEN
         VA1=1
         VAL='THE SURFACE NOT A LINEAR DIFFRATION GRATING:'
      ELSE
         V1=1
         VALUE1=surf_grating_order(SURF)
      END IF
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'GRS') THEN
      IF(surf_diffraction_flag(SURF).EQ.0.0D0) THEN
         VA1=1
         VAL='THE SURFACE NOT A LINEAR DIFFRATION GRATING:'
      ELSE
         V1=1
         VALUE1=surf_grating_spacing(SURF)
      END IF
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'GRX') THEN
      IF(surf_diffraction_flag(SURF).EQ.0.0D0) THEN
         VA1=1
         VAL='THE SURFACE NOT A LINEAR DIFFRATION GRATING:'
      ELSE
         V1=1
         VALUE1=surf_grating_vx(SURF)
      END IF
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'GRY') THEN
      IF(surf_diffraction_flag(SURF).EQ.0.0D0) THEN
         VA1=1
         VAL='THE SURFACE NOT A LINEAR DIFFRATION GRATING:'
      ELSE
         V1=1
         VALUE1=surf_grating_vy(SURF)
      END IF
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'GRZ') THEN
      IF(surf_diffraction_flag(SURF).EQ.0.0D0) THEN
         VA1=1
         VAL='THE SURFACE NOT A LINEAR DIFFRATION GRATING:'
      ELSE
         V1=1
         VALUE1=surf_grating_vz(SURF)
      END IF
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'SPGR') THEN
      V1=1
      VALUE1=surf_spgr(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'RDTOR') THEN
      VA1=1
      VAL='CURRENT SURFACE TORIC RADIUS OF CURVATURE IS:'
      IF(DABS(surf_toric_curvature(SURF)).LT.1D-30) THEN
         V1=1
         VALUE1=0.0D0
      ELSE
         V1=1
         VALUE1=1.0D0/surf_toric_curvature(SURF)
      END IF
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'CCR') THEN
      IF(surf_ccr_flag(SURF).EQ.2.0D0) THEN
         VA1=1
         VAL='CORNER CUBE DATA IS:'
         V1=1
         VALUE1=surf_roof_a(SURF)
         V1=2
         VALUE1=surf_roof_angle_err(SURF)
         V1=3
         VALUE1=surf_roof_b(SURF)
         V1=4
         VALUE1=surf_ccr_angle_err2(SURF)
      ELSE
         VA1=1
         VAL='SURFACE IS NOT A CORNER CUBE'
      END IF
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'ROO') THEN
      IF(surf_ccr_flag(SURF).EQ.1.0D0) THEN
         VA1=1
         VAL='ROOF DATA IS:'
         V1=1
         VALUE1=surf_roof_a(SURF)
         V1=2
         VALUE1=surf_roof_angle_err(SURF)
      ELSE
         VA1=1
         VAL='SURFACE IS NOT A ROOF SURFACE'
      END IF
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'RAYERROR') THEN
      VA1=1
      VAL='RANDON SURFACE RAY ERROR IS:'
      V1=1
      VALUE1=surf_ray_error(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'COATING') THEN
      VA1=1
      VAL='CURRENT SURFACE COATING NUMBER IS:'
      V1=1
      VALUE1=surf_coating_index(SURF)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'FOOTBLOK') THEN
      VA1=1
      IF(surf_footblok_flag(SURF).EQ.0.0D0)VAL='CURRENT SURFACE DOES NOT HAVE A "FOOTBLOK" DEFINITION ON IT'
      IF(surf_footblok_flag(SURF).EQ.1.0D0)VAL='CURRENT SURFACE HAS A "FOOTBLOK" DEFINITION ON IT'
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'PIVAXIS') THEN
      VA1=1
      IF(surf_pivot_axis(SURF).EQ.0.0D0)VAL='CURRENT SURFACE HAS "PIVAXIS" SET TO "LOCAL"'
      IF(surf_pivot_axis(SURF).EQ.1.0D0)VAL='CURRENT SURFACE HAS "PIVAXIS" SET TO "NORMAL"'
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'NODUM') THEN
      VA1=1
      IF(surf_dummy_val(SURF).EQ.0.0D0)VAL='SURFACE NOT FORCED TO DUMMY'
      IF(surf_dummy_val(SURF).EQ.1.0D0)VAL='SURFACE FORCED TO DUMMY'
      GO TO 200
   ELSE
   END IF
!       WV
   IF(WC.EQ.'WV') THEN
      VA1=1
      VAL='CURRENT OPTICAL SYSTEM WAVELENGTHS (1-5) ARE:'
      V1=1
      VALUE1=sys_wavelength(1)
      V2=1
      VALUE2=sys_wavelength(2)
      V3=1
      VALUE3=sys_wavelength(3)
      V4=1
      VALUE4=sys_wavelength(4)
      V5=1
      VALUE5=sys_wavelength(5)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'WV2') THEN
      VA1=1
      VAL='CURRENT OPTICAL SYSTEM WAVELENGTHS (6-10) ARE:'
      V1=1
      VALUE1=sys_wavelength(6)
      V2=1
      VALUE2=sys_wavelength(7)
      V3=1
      VALUE3=sys_wavelength(8)
      V4=1
      VALUE4=sys_wavelength(9)
      V5=1
      VALUE5=sys_wavelength(10)
      GO TO 200
   ELSE
   END IF
   IF(WC.EQ.'SPIDER') THEN
      IF(surf_spider_flag(SURF).EQ.0.0D0) THEN
         VA1=1
         VAL='NO SPIDER IS DEFINED ON THIS SURFACE'
      ELSE
         VA1=1
         VAL='CURRENT SPIDER VALUES ARE:'
         V1=1
         VALUE1=surf_spider_arms(SURF)
         V2=1
         VALUE2=surf_spider_angle(SURF)
         V3=1
         VALUE3=surf_spider_width(SURF)
         V4=1
         VALUE4=surf_roof_a(SURF)
         GO TO 200
      END IF
   ELSE
   END IF
   IF(WC.EQ.'MULTCLAP') THEN
      OUTLYNE='"MULTCLAP" QUERRY IS NOT AVAILABLE. USE "CAOB" FROM THE'
      CALL SHOWIT(1)
      OUTLYNE='CMD PROGRAM LEVEL'
      CALL SHOWIT(1)
      CALL MACFAL
      GO TO 200
   END IF
   IF(WC.EQ.'MULTCOBS') THEN
      OUTLYNE='"MULTCOBS" QUERRY IS NOT AVAILABLE. USE "CAOB" FROM THE'
      CALL SHOWIT(1)
      OUTLYNE='CMD PROGRAM LEVEL'
      CALL SHOWIT(1)
      CALL MACFAL
      GO TO 200
   END IF
   IF(WC.EQ.'CLAP') THEN
      IF(ABS(surf_clap_type(SURF)).EQ.0.0D0) THEN
         ALENS(51:57,SURF)=0.0D0
         OUTLYNE='NO "CLEAR APERTURE DATA" ON CURRENT SURFACE"'
         CALL SHOWIT(1)
         CALL MACFAL
         GO TO 200
      ELSE
         VA1=1
         IF(surf_clap_type(SURF).EQ.1.0D0)VAL='CURRENT SURFACE "CLAP" VALUES ARE:'
         IF(surf_clap_type(SURF).EQ.2.0D0)VAL='CURRENT SURFACE "CLAP RECT" VALUES ARE:'
         IF(surf_clap_type(SURF).EQ.3.0D0)VAL='CURRENT SURFACE"CLAP ELIP" VALUES ARE:'
         IF(surf_clap_type(SURF).EQ.4.0D0)VAL='CURRENT SURFACE "CLAP RCTK" VALUES ARE:'
         IF(surf_clap_type(SURF).EQ.5.0D0)VAL='CURRENT SURFACE "CLAP POLY" VALUES ARE:'
         IF(surf_clap_type(SURF).EQ.6.0D0)VAL='CURRENT SURFACE "CLAP IPOLY" VALUES ARE:'
         V1=1
         VALUE1=surf_clap_dim(SURF, 1)
         V2=1
         VALUE2=surf_clap_dim(SURF, 2)
         V3=1
         VALUE3=surf_clap_dim(SURF, 3)
         V4=1
         VALUE4=surf_clap_dim(SURF, 4)
         V5=1
         VALUE5=surf_clap_dim(SURF, 5)
         V6=1
         VALUE6=surf_clap_tilt(SURF)
         GO TO 200
      END IF
   ELSE
!       NOT CLAP
   END IF
   IF(WC.EQ.'CLAP') THEN
      IF(ABS(surf_cobs_ape_type(SURF)).EQ.0.0D0) THEN
         ALENS(51:57,SURF)=0.0D0
         OUTLYNE='NO "CLEAR APERTURE ERASE DATA" ON CURRENT SURFACE"'
         CALL SHOWIT(1)
         CALL MACFAL
         GO TO 200
      ELSE
         VA1=1
         IF(surf_cobs_ape_type(SURF).EQ.1.0D0)VAL='CURRENT SURFACE "CLAP ERASE" VALUES ARE:'
         IF(surf_cobs_ape_type(SURF).EQ.2.0D0)VAL='CURRENT SURFACE "CLAP RECTE" VALUES ARE:'
         IF(surf_cobs_ape_type(SURF).EQ.3.0D0)VAL='CURRENT SURFACE "CLAP ELIPE" VALUES ARE:'
         IF(surf_cobs_ape_type(SURF).EQ.4.0D0)VAL='CURRENT SURFACE "CLAP RCTKE" VALUES ARE:'
         IF(surf_cobs_ape_type(SURF).EQ.5.0D0)VAL='CURRENT SURFACE "CLAP POLYE" VALUES ARE:'
         IF(surf_cobs_ape_type(SURF).EQ.6.0D0)VAL='CURRENT SURFACE "CLAP IPOLYE" VALUES ARE:'
         V1=1
         VALUE1=surf_cobs_ape_data(SURF, 1)
         V2=1
         VALUE2=surf_cobs_ape_data(SURF, 2)
         V3=1
         VALUE3=surf_cobs_ape_data(SURF, 3)
         V4=1
         VALUE4=surf_cobs_ape_data(SURF, 4)
         V5=1
         VALUE5=surf_cobs_ape_data(SURF, 5)
         V6=1
         VALUE6=surf_cobs_ape_data(SURF, 6)
         GO TO 200
      END IF
   ELSE
!       NOT CLAP
   END IF
   IF(WC.EQ.'COBS') THEN
      IF(ABS(surf_coat_type(SURF)).EQ.0.0D0) THEN
         ALENS(61:67,SURF)=0.0D0
         OUTLYNE='NO "OBSCURATION DATA" ON CURRENT SURFACE"'
         CALL SHOWIT(1)
         CALL MACFAL
         GO TO 200
      ELSE
         VA1=1
         IF(surf_coat_type(SURF).EQ.1.0D0)VAL='CURRENT SURFACE "COBS" VALUES ARE:'
         IF(surf_coat_type(SURF).EQ.2.0D0)VAL='CURRENT SURFACE "COBS RECT" VALUES ARE:'
         IF(surf_coat_type(SURF).EQ.3.0D0)VAL='CURRENT SURFACE "COBS ELIP" VALUES ARE:'
         IF(surf_coat_type(SURF).EQ.4.0D0)VAL='CURRENT SURFACE "COBS RCTK" VALUES ARE:'
         IF(surf_coat_type(SURF).EQ.5.0D0)VAL='CURRENT SURFACE "COBS POLY" VALUES ARE:'
         IF(surf_coat_type(SURF).EQ.6.0D0)VAL='CURRENT SURFACE "COBS IPOLY" VALUES ARE:'
         V1=1
         VALUE1=surf_cobs_poly(SURF, 1)
         V2=1
         VALUE2=surf_cobs_poly(SURF, 2)
         V3=1
         VALUE3=surf_cobs_poly(SURF, 3)
         V4=1
         VALUE4=surf_cobs_poly(SURF, 4)
         V5=1
         VALUE5=surf_cobs_poly(SURF, 5)
         V6=1
         VALUE6=surf_cobs_poly(SURF, 6)
         GO TO 200
      END IF
   ELSE
!       NOT COBS
   END IF
   IF(WC.EQ.'COBS') THEN
      IF(ABS(surf_cobs_era_type(SURF)).EQ.0.0D0) THEN
         ALENS(61:67,SURF)=0.0D0
         OUTLYNE='NO "OBSCURATION ERASE DATA" ON CURRENT SURFACE"'
         CALL SHOWIT(1)
         CALL MACFAL
         GO TO 200
      ELSE
         VA1=1
         IF(surf_cobs_era_type(SURF).EQ.1.0D0)VAL='CURRENT SURFACE "COBS ERASE" VALUES ARE:'
         IF(surf_cobs_era_type(SURF).EQ.2.0D0)VAL='CURRENT SURFACE "COBS RECTE" VALUES ARE:'
         IF(surf_cobs_era_type(SURF).EQ.3.0D0)VAL='CURRENT SURFACE "COBS ELIPE" VALUES ARE:'
         IF(surf_cobs_era_type(SURF).EQ.4.0D0)VAL='CURRENT SURFACE "COBS RCTKE" VALUES ARE:'
         IF(surf_cobs_era_type(SURF).EQ.5.0D0)VAL='CURRENT SURFACE "COBS POLYE" VALUES ARE:'
         IF(surf_cobs_era_type(SURF).EQ.6.0D0)VAL='CURRENT SURFACE "COBS IPOLYE" VALUES ARE:'
         V1=1
         VALUE1=surf_cobs_era_data(SURF, 1)
         V2=1
         VALUE2=surf_cobs_era_data(SURF, 2)
         V3=1
         VALUE3=surf_cobs_era_data(SURF, 3)
         V4=1
         VALUE4=surf_cobs_era_data(SURF, 4)
         V5=1
         VALUE5=surf_cobs_era_data(SURF, 5)
         V6=1
         VALUE6=surf_cobs_era_data(SURF, 6)
         GO TO 200
      END IF
   ELSE
!       NOT COBS
   END IF
!       SOLVES
   IF(WC.EQ.'PY'.OR.WC.EQ.'PX'.OR.WC.EQ.'PCY'.OR.WC.EQ.'PCX'.OR.WC.EQ.'CAY'.OR.WC.EQ.'CAX'.OR.WC.EQ.'PUY'.OR.WC.EQ.'PUX'.OR.WC.EQ.'PUCY'.OR.WC.EQ.'PUCX'.OR.WC.EQ.'COCY'.OR.WC.EQ.'COCX'.OR.WC.EQ.'APY'.OR.WC.EQ.'APX'.OR.WC.EQ.'APCY'.OR.WC.EQ.'APCX'.OR.WC.EQ.'PIY'.OR.WC.EQ.'PIX'.OR.WC.EQ.'PICY'.OR.WC.EQ.'PICX') THEN
!
      IF(SOLVE(6,SURF).EQ.1.0D0) THEN
         VA1=1
         VAL='CURRENT SURFACE SOLVE IS A "PY" SOLVE WITH TARGET VALUE:'
         V1=1
         VALUE1=SOLVE(7,SURF)
         GO TO 200
      ELSE
      END IF
      IF(SOLVE(6,SURF).EQ.2.0D0) THEN
         VA1=1
         VAL='CURRENT SURFACE SOLVE IS A "PCY" SOLVE WITH TARGET VALUE:'
         V1=1
         VALUE1=SOLVE(7,SURF)
         GO TO 200
      ELSE
      END IF
      IF(SOLVE(6,SURF).EQ.3.0D0) THEN
         VA1=1
         VAL='CURRENT SURFACE SOLVE IS A "CAY" SOLVE WITH TARGET VALUE:'
         V1=1
         VALUE1=SOLVE(7,SURF)
         GO TO 200
      ELSE
      END IF
!
      IF(SOLVE(4,SURF).EQ.4.0D0) THEN
         VA1=1
         VAL='CURRENT SURFACE SOLVE IS A "PX" SOLVE WITH TARGET VALUE:'
         V1=1
         VALUE1=SOLVE(3,SURF)
         GO TO 200
      ELSE
      END IF
      IF(SOLVE(4,SURF).EQ.5.0D0) THEN
         VA1=1
         VAL='CURRENT SURFACE SOLVE IS A "PCX" SOLVE WITH TARGET VALUE:'
         V1=1
         VALUE1=SOLVE(3,SURF)
         GO TO 200
      ELSE
      END IF
      IF(SOLVE(4,SURF).EQ.6.0D0) THEN
         VA1=1
         VAL='CURRENT SURFACE SOLVE IS A "CAX" SOLVE WITH TARGET VALUE:'
         V1=1
         VALUE1=SOLVE(3,SURF)
         GO TO 200
      ELSE
      END IF
!
      IF(SOLVE(8,SURF).EQ.1.0D0) THEN
         VA1=1
         VAL='CURRENT SURFACE SOLVE IS AN "APY" SOLVE'
         GO TO 200
      ELSE
      END IF
      IF(SOLVE(8,SURF).EQ.2.0D0) THEN
         VA1=1
         VAL='CURRENT SURFACE SOLVE IS A "PIY" SOLVE WITH TARGET VALUE:'
         V1=1
         VALUE1=SOLVE(9,SURF)
         GO TO 200
      ELSE
      END IF
      IF(SOLVE(8,SURF).EQ.3.0D0) THEN
         VA1=1
         VAL='CURRENT SURFACE SOLVE IS A "PUY" SOLVE WITH TARGET VALUE:'
         V1=1
         VALUE1=SOLVE(9,SURF)
         GO TO 200
      ELSE
      END IF
      IF(SOLVE(8,SURF).EQ.4.0D0) THEN
         VA1=1
         VAL='CURRENT SURFACE SOLVE IS AN "APCY" SOLVE'
         GO TO 200
      ELSE
      END IF
      IF(SOLVE(8,SURF).EQ.5.0D0) THEN
         VA1=1
         VAL='CURRENT SURFACE SOLVE IS A "PICY" SOLVE WITH TARGET VALUE:'
         V1=1
         VALUE1=SOLVE(9,SURF)
         GO TO 200
      ELSE
      END IF
      IF(SOLVE(8,SURF).EQ.6.0D0) THEN
         VA1=1
         VAL='CURRENT SURFACE SOLVE IS A "PUCY" SOLVE WITH TARGET VALUE:'
         V1=1
         VALUE1=SOLVE(9,SURF)
         GO TO 200
      ELSE
      END IF
      IF(SOLVE(8,SURF).EQ.7.0D0) THEN
         VA1=1
         VAL='CURRENT SURFACE SOLVE IS A "COCY" SOLVE TO SURFACE #:'
         V1=1
         VALUE1=SOLVE(9,SURF)
         GO TO 200
      ELSE
      END IF
!
      IF(SOLVE(2,SURF).EQ.8.0D0) THEN
         VA1=1
         VAL='CURRENT SURFACE SOLVE IS AN "APX" SOLVE'
         GO TO 200
      ELSE
      END IF
      IF(SOLVE(2,SURF).EQ.9.0D0) THEN
         VA1=1
         VAL='CURRENT SURFACE SOLVE IS A "PIX" SOLVE WITH TARGET VALUE:'
         V1=1
         VALUE1=SOLVE(1,SURF)
         GO TO 200
      ELSE
      END IF
      IF(SOLVE(2,SURF).EQ.10.0D0) THEN
         VA1=1
         VAL='CURRENT SURFACE SOLVE IS A "PUX" SOLVE WITH TARGET VALUE:'
         V1=1
         VALUE1=SOLVE(1,SURF)
         GO TO 200
      ELSE
      END IF
      IF(SOLVE(2,SURF).EQ.11.0D0) THEN
         VA1=1
         VAL='CURRENT SURFACE SOLVE IS AN "APCX" SOLVE'
         GO TO 200
      ELSE
      END IF
      IF(SOLVE(2,SURF).EQ.12.0D0) THEN
         VA1=1
         VAL='CURRENT SURFACE SOLVE IS A "PICX" SOLVE WITH TARGET VALUE:'
         V1=1
         VALUE1=SOLVE(1,SURF)
         GO TO 200
      ELSE
      END IF
      IF(SOLVE(2,SURF).EQ.13.0D0) THEN
         VA1=1
         VAL='CURRENT SURFACE SOLVE IS A "PUCX" SOLVE WITH TARGET VALUE:'
         V1=1
         VALUE1=SOLVE(1,SURF)
         GO TO 200
      ELSE
      END IF
      IF(SOLVE(2,SURF).EQ.14.0D0) THEN
         VA1=1
         VAL='CURRENT SURFACE SOLVE IS A "COCX" SOLVE TO SURFACE #:'
         V1=1
         VALUE1=SOLVE(1,SURF)
         GO TO 200
      ELSE
      END IF
      IF(SOLVE(6,SURF).EQ.0.0D0.AND.SOLVE(4,SURF).EQ.0.0D0 .AND.SOLVE(8,SURF).EQ.0.0D0.AND.SOLVE(2,SURF).EQ.0.0D0)THEN
         VA1=1
         VAL='CURRENT SURFACE HAS NO SOLVES'
         GO TO 200
      ELSE
      END IF
   ELSE
!       NOT A SOLVE
   END IF
300 CONTINUE
   IF(WC.EQ.'PIKUP') THEN
!       PIKUPS HERE
      DO I=1,PSIZ
         IF(PIKUP(1,SURF,I).NE.0.0D0) REPEAT=REPEAT+1
      END DO
      IF(REPEAT.EQ.0) THEN
         VA1=1
         VAL='CURRENT SURFACE HAS NO PIKUPS DEFINED ON IT'
         GO TO 200
      ELSE
!       THERE ARE PIKUPS, TAKE APPROPRIATE ACTION
         DO I=1,PSIZ
            IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
!
               IF(I.EQ.1) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS AN "RD" PIKUP ON IT WITH VALUES:'
                  V1=1
                  V2=1
                  V3=1
                  V4=1
                  V5=1
                  VALUE1=PIKUP(2,SURF,I)
                  VALUE2=PIKUP(3,SURF,I)
                  VALUE3=PIKUP(4,SURF,I)
                  VALUE4=PIKUP(5,SURF,I)
                  VALUE5=PIKUP(6,SURF,I)
                  GO TO 200
               ELSE
!       PROCEED TO NEXT PIKUP TYPE
               END IF
!
               IF(I.EQ.2) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS AN "CV" PIKUP ON IT WITH VALUES:'
                  V1=1
                  V2=1
                  V3=1
                  V4=1
                  V5=1
                  VALUE1=PIKUP(2,SURF,I)
                  VALUE2=PIKUP(3,SURF,I)
                  VALUE3=PIKUP(4,SURF,I)
                  VALUE4=PIKUP(5,SURF,I)
                  VALUE5=PIKUP(6,SURF,I)
                  GO TO 200
               ELSE
!       PROCEED TO NEXT PIKUP TYPE
               END IF
!
               IF(I.EQ.3) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS AN "TH" PIKUP ON IT WITH VALUES:'
                  V1=1
                  V2=1
                  V3=1
                  V4=1
                  V5=1
                  VALUE1=PIKUP(2,SURF,I)
                  VALUE2=PIKUP(3,SURF,I)
                  VALUE3=PIKUP(4,SURF,I)
                  VALUE4=PIKUP(5,SURF,I)
                  VALUE5=PIKUP(6,SURF,I)
                  GO TO 200
               ELSE
!       PROCEED TO NEXT PIKUP TYPE
               END IF
!
               IF(I.EQ.32) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS AN "THOAL" PIKUP ON IT WITH VALUES:'
                  V1=1
                  V2=1
                  V3=1
                  V4=1
                  V5=1
                  VALUE1=PIKUP(2,SURF,I)
                  VALUE2=PIKUP(3,SURF,I)
                  VALUE3=PIKUP(4,SURF,I)
                  VALUE4=PIKUP(5,SURF,I)
                  VALUE5=PIKUP(6,SURF,I)
                  GO TO 200
               ELSE
!       PROCEED TO NEXT PIKUP TYPE
               END IF
!
               IF(I.EQ.4) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS AN "CC" PIKUP ON IT WITH VALUES:'
                  V1=1
                  V2=1
                  V3=1
                  V4=1
                  V5=1
                  VALUE1=PIKUP(2,SURF,I)
                  VALUE2=PIKUP(3,SURF,I)
                  VALUE3=PIKUP(4,SURF,I)
                  VALUE4=PIKUP(5,SURF,I)
                  VALUE5=PIKUP(6,SURF,I)
                  GO TO 200
               ELSE
!       PROCEED TO NEXT PIKUP TYPE
               END IF
!
               IF(I.EQ.5) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS AN "AD" PIKUP ON IT WITH VALUES:'
                  V1=1
                  V2=1
                  V3=1
                  V4=1
                  V5=1
                  VALUE1=PIKUP(2,SURF,I)
                  VALUE2=PIKUP(3,SURF,I)
                  VALUE3=PIKUP(4,SURF,I)
                  VALUE4=PIKUP(5,SURF,I)
                  VALUE5=PIKUP(6,SURF,I)
                  GO TO 200
               ELSE
!       PROCEED TO NEXT PIKUP TYPE
               END IF
!
               IF(I.EQ.6) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS AN "AE" PIKUP ON IT WITH VALUES:'
                  V1=1
                  V2=1
                  V3=1
                  V4=1
                  V5=1
                  VALUE1=PIKUP(2,SURF,I)
                  VALUE2=PIKUP(3,SURF,I)
                  VALUE3=PIKUP(4,SURF,I)
                  VALUE4=PIKUP(5,SURF,I)
                  VALUE5=PIKUP(6,SURF,I)
                  GO TO 200
               ELSE
!       PROCEED TO NEXT PIKUP TYPE
               END IF
!
               IF(I.EQ.7) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS AN "AF" PIKUP ON IT WITH VALUES:'
                  V1=1
                  V2=1
                  V3=1
                  V4=1
                  V5=1
                  VALUE1=PIKUP(2,SURF,I)
                  VALUE2=PIKUP(3,SURF,I)
                  VALUE3=PIKUP(4,SURF,I)
                  VALUE4=PIKUP(5,SURF,I)
                  VALUE5=PIKUP(6,SURF,I)
                  GO TO 200
               ELSE
!       PROCEED TO NEXT PIKUP TYPE
               END IF
!
               IF(I.EQ.8) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS AN "AG" PIKUP ON IT WITH VALUES:'
                  V1=1
                  V2=1
                  V3=1
                  V4=1
                  V5=1
                  VALUE1=PIKUP(2,SURF,I)
                  VALUE2=PIKUP(3,SURF,I)
                  VALUE3=PIKUP(4,SURF,I)
                  VALUE4=PIKUP(5,SURF,I)
                  VALUE5=PIKUP(6,SURF,I)
                  GO TO 200
               ELSE
!       PROCEED TO NEXT PIKUP TYPE
               END IF
!
               IF(I.EQ.9) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS AN "CVTOR" PIKUP ON IT WITH VALUES:'
                  V1=1
                  V2=1
                  V3=1
                  V4=1
                  V5=1
                  VALUE1=PIKUP(2,SURF,I)
                  VALUE2=PIKUP(3,SURF,I)
                  VALUE3=PIKUP(4,SURF,I)
                  VALUE4=PIKUP(5,SURF,I)
                  VALUE5=PIKUP(6,SURF,I)
                  GO TO 200
               ELSE
!       PROCEED TO NEXT PIKUP TYPE
               END IF
!
               IF(I.EQ.10) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS AN "RDTOR" PIKUP ON IT WITH VALUES:'
                  V1=1
                  V2=1
                  V3=1
                  V4=1
                  V5=1
                  VALUE1=PIKUP(2,SURF,I)
                  VALUE2=PIKUP(3,SURF,I)
                  VALUE3=PIKUP(4,SURF,I)
                  VALUE4=PIKUP(5,SURF,I)
                  VALUE5=PIKUP(6,SURF,I)
                  GO TO 200
               ELSE
!       PROCEED TO NEXT PIKUP TYPE
               END IF
!
               IF(I.EQ.11) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS AN "PRO" PIKUP ON IT WITH VALUES:'
                  V1=1
                  V2=1
                  V3=1
                  V4=1
                  V5=1
                  VALUE1=PIKUP(2,SURF,I)
                  VALUE2=PIKUP(3,SURF,I)
                  VALUE3=PIKUP(4,SURF,I)
                  VALUE4=PIKUP(5,SURF,I)
                  VALUE5=PIKUP(6,SURF,I)
                  GO TO 200
               ELSE
!       PROCEED TO NEXT PIKUP TYPE
               END IF
!
               IF(I.EQ.12) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS AN "NPRO" PIKUP ON IT WITH VALUES:'
                  V1=1
                  V2=1
                  V3=1
                  V4=1
                  V5=1
                  VALUE1=PIKUP(2,SURF,I)
                  VALUE2=PIKUP(3,SURF,I)
                  VALUE3=PIKUP(4,SURF,I)
                  VALUE4=PIKUP(5,SURF,I)
                  VALUE5=PIKUP(6,SURF,I)
                  GO TO 200
               ELSE
!       PROCEED TO NEXT PIKUP TYPE
               END IF
!
               IF(I.EQ.13) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS AN "YD" PIKUP ON IT WITH VALUES:'
                  V1=1
                  V2=1
                  V3=1
                  V4=1
                  V5=1
                  VALUE1=PIKUP(2,SURF,I)
                  VALUE2=PIKUP(3,SURF,I)
                  VALUE3=PIKUP(4,SURF,I)
                  VALUE4=PIKUP(5,SURF,I)
                  VALUE5=PIKUP(6,SURF,I)
                  GO TO 200
               ELSE
!       PROCEED TO NEXT PIKUP TYPE
               END IF
!
               IF(I.EQ.14) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS AN "XD" PIKUP ON IT WITH VALUES:'
                  V1=1
                  V2=1
                  V3=1
                  V4=1
                  V5=1
                  VALUE1=PIKUP(2,SURF,I)
                  VALUE2=PIKUP(3,SURF,I)
                  VALUE3=PIKUP(4,SURF,I)
                  VALUE4=PIKUP(5,SURF,I)
                  VALUE5=PIKUP(6,SURF,I)
                  GO TO 200
               ELSE
!       PROCEED TO NEXT PIKUP TYPE
               END IF
!
               IF(I.EQ.15) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS AN "ALPHA" PIKUP ON IT WITH VALUES:'
                  V1=1
                  V2=1
                  V3=1
                  V4=1
                  V5=1
                  VALUE1=PIKUP(2,SURF,I)
                  VALUE2=PIKUP(3,SURF,I)
                  VALUE3=PIKUP(4,SURF,I)
                  VALUE4=PIKUP(5,SURF,I)
                  VALUE5=PIKUP(6,SURF,I)
                  GO TO 200
               ELSE
!       PROCEED TO NEXT PIKUP TYPE
               END IF
!
               IF(I.EQ.16) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS AN "BETA" PIKUP ON IT WITH VALUES:'
                  V1=1
                  V2=1
                  V3=1
                  V4=1
                  V5=1
                  VALUE1=PIKUP(2,SURF,I)
                  VALUE2=PIKUP(3,SURF,I)
                  VALUE3=PIKUP(4,SURF,I)
                  VALUE4=PIKUP(5,SURF,I)
                  VALUE5=PIKUP(6,SURF,I)
                  GO TO 200
               ELSE
!       PROCEED TO NEXT PIKUP TYPE
               END IF
!
               IF(I.EQ.17) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS AN "GAMMA" PIKUP ON IT WITH VALUES:'
                  V1=1
                  V2=1
                  V3=1
                  V4=1
                  V5=1
                  VALUE1=PIKUP(2,SURF,I)
                  VALUE2=PIKUP(3,SURF,I)
                  VALUE3=PIKUP(4,SURF,I)
                  VALUE4=PIKUP(5,SURF,I)
                  VALUE5=PIKUP(6,SURF,I)
                  GO TO 200
               ELSE
!       PROCEED TO NEXT PIKUP TYPE
               END IF
!
               IF(I.EQ.18) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS AN "CLAP" PIKUP ON IT WITH VALUES:'
                  V1=1
                  V2=1
                  V3=1
                  V4=1
                  V5=1
                  VALUE1=PIKUP(2,SURF,I)
                  VALUE2=PIKUP(3,SURF,I)
                  VALUE3=PIKUP(4,SURF,I)
                  VALUE4=PIKUP(5,SURF,I)
                  VALUE5=PIKUP(6,SURF,I)
                  GO TO 200
               ELSE
!       PROCEED TO NEXT PIKUP TYPE
               END IF
!
               IF(I.EQ.19) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS AN "COBS" PIKUP ON IT WITH VALUES:'
                  V1=1
                  V2=1
                  V3=1
                  V4=1
                  V5=1
                  VALUE1=PIKUP(2,SURF,I)
                  VALUE2=PIKUP(3,SURF,I)
                  VALUE3=PIKUP(4,SURF,I)
                  VALUE4=PIKUP(5,SURF,I)
                  VALUE5=PIKUP(6,SURF,I)
                  GO TO 200
               ELSE
!       PROCEED TO NEXT PIKUP TYPE
               END IF
!
               IF(I.EQ.20) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS AN "GLASS" PIKUP ON IT WITH VALUES:'
                  V1=1
                  V2=1
                  V3=1
                  V4=1
                  V5=1
                  VALUE1=PIKUP(2,SURF,I)
                  VALUE2=PIKUP(3,SURF,I)
                  VALUE3=PIKUP(4,SURF,I)
                  VALUE4=PIKUP(5,SURF,I)
                  VALUE5=PIKUP(6,SURF,I)
                  GO TO 200
               ELSE
!       PROCEED TO NEXT PIKUP TYPE
               END IF
!
               IF(I.EQ.21) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS AN "CCTOR" PIKUP ON IT WITH VALUES:'
                  V1=1
                  V2=1
                  V3=1
                  V4=1
                  V5=1
                  VALUE1=PIKUP(2,SURF,I)
                  VALUE2=PIKUP(3,SURF,I)
                  VALUE3=PIKUP(4,SURF,I)
                  VALUE4=PIKUP(5,SURF,I)
                  VALUE5=PIKUP(6,SURF,I)
                  GO TO 200
               ELSE
!       PROCEED TO NEXT PIKUP TYPE
               END IF
!
               IF(I.EQ.22) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS AN "ADTOR" PIKUP ON IT WITH VALUES:'
                  V1=1
                  V2=1
                  V3=1
                  V4=1
                  V5=1
                  VALUE1=PIKUP(2,SURF,I)
                  VALUE2=PIKUP(3,SURF,I)
                  VALUE3=PIKUP(4,SURF,I)
                  VALUE4=PIKUP(5,SURF,I)
                  VALUE5=PIKUP(6,SURF,I)
                  GO TO 200
               ELSE
!       PROCEED TO NEXT PIKUP TYPE
               END IF
!
               IF(I.EQ.23) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS AN "AETOR" PIKUP ON IT WITH VALUES:'
                  V1=1
                  V2=1
                  V3=1
                  V4=1
                  V5=1
                  VALUE1=PIKUP(2,SURF,I)
                  VALUE2=PIKUP(3,SURF,I)
                  VALUE3=PIKUP(4,SURF,I)
                  VALUE4=PIKUP(5,SURF,I)
                  VALUE5=PIKUP(6,SURF,I)
                  GO TO 200
               ELSE
!       PROCEED TO NEXT PIKUP TYPE
               END IF
!
               IF(I.EQ.24) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS AN "AFTOR" PIKUP ON IT WITH VALUES:'
                  V1=1
                  V2=1
                  V3=1
                  V4=1
                  V5=1
                  VALUE1=PIKUP(2,SURF,I)
                  VALUE2=PIKUP(3,SURF,I)
                  VALUE3=PIKUP(4,SURF,I)
                  VALUE4=PIKUP(5,SURF,I)
                  VALUE5=PIKUP(6,SURF,I)
                  GO TO 200
               ELSE
!       PROCEED TO NEXT PIKUP TYPE
               END IF
!
               IF(I.EQ.25) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS AN "AGTOR" PIKUP ON IT WITH VALUES:'
                  V1=1
                  V2=1
                  V3=1
                  V4=1
                  V5=1
                  VALUE1=PIKUP(2,SURF,I)
                  VALUE2=PIKUP(3,SURF,I)
                  VALUE3=PIKUP(4,SURF,I)
                  VALUE4=PIKUP(5,SURF,I)
                  VALUE5=PIKUP(6,SURF,I)
                  GO TO 200
               ELSE
!       PROCEED TO NEXT PIKUP TYPE
               END IF
!
               IF(I.EQ.26) THEN
                  VA1=1
                  VAL='CURRENT SURFACE HAS AN "AC" PIKUP ON IT WITH VALUES:'
                  V1=1
                  V2=1
                  V3=1
                  V4=1
                  V5=1
                  VALUE1=PIKUP(2,SURF,I)
                  VALUE2=PIKUP(3,SURF,I)
                  VALUE3=PIKUP(4,SURF,I)
                  VALUE4=PIKUP(5,SURF,I)
                  VALUE5=PIKUP(6,SURF,I)
                  GO TO 200
               ELSE
!       PROCEED TO NEXT PIKUP TYPE
               END IF
!
               IF(I.GT.26) THEN
!                 DO NOTHING
               ELSE
               END IF
            ELSE
!       PROCEED BY INCREMENTING I BY 1
            END IF
         END DO
      END IF
   ELSE
!       WC NOT PIKUP
   END IF
   RETURN
!
200 CALL LQAS(VALUE1,VALUE2,VALUE3,VALUE4,VALUE5,VAL,V1,V2,V3,V4,V5,VA1,V6,VALUE6,VA1WS1,VA1WS2,VA1WS3,VA1WS4 ,VALWS1,VALWS2,VALWS3,VALWS4,REPEAT)
!
   IF(REPEAT.NE.0) GO TO 300
   RETURN
END
! SUB LQAS.FOR
SUBROUTINE LQAS(VALUE1,VALUE2,VALUE3,VALUE4,VALUE5,VAL,V1,V2,V3,V4,V5,VA1,V6,VALUE6,VA1WS1,VA1WS2,VA1WS3,VA1WS4 ,VALWS1,VALWS2,VALWS3,VALWS4,REPEAT)
!
   use DATMAI
   IMPLICIT NONE
!
   CHARACTER*80 VAL,VALWS1,VALWS2,VALWS3,VALWS4
!
   INTEGER V1,V2,V3,V4,V5,V6,VA1,VA1WS1,VA1WS2,VA1WS3,VA1WS4 ,REPEAT
!
   REAL*8 VALUE1,VALUE2,VALUE3,VALUE4,VALUE5,VALUE6
!
!
   IF(VA1.NE.0) WRITE(OUTLYNE,10) VAL
   IF(VA1.NE.0) CALL SHOWIT(1)
   IF(VA1WS1.NE.0) WRITE(OUTLYNE,10) VALWS1
   IF(VA1WS1.NE.0) CALL SHOWIT(1)
   IF(VA1WS2.NE.0) WRITE(OUTLYNE,10) VALWS2
   IF(VA1WS2.NE.0) CALL SHOWIT(1)
   IF(VA1WS3.NE.0) WRITE(OUTLYNE,10)VALWS3
   IF(VA1WS3.NE.0) CALL SHOWIT(1)
   IF(VA1WS4.NE.0) WRITE(OUTLYNE,10) VALWS4
   IF(VA1WS4.NE.0) CALL SHOWIT(1)
   IF(V1.NE.0)  WRITE(OUTLYNE,20) VALUE1
   IF(V1.NE.0)  CALL SHOWIT(1)
   IF(V2.NE.0)  WRITE(OUTLYNE,21) VALUE2
   IF(V2.NE.0)  CALL SHOWIT(1)
   IF(V3.NE.0)  WRITE(OUTLYNE,22) VALUE3
   IF(V3.NE.0)  CALL SHOWIT(1)
   IF(V4.NE.0)  WRITE(OUTLYNE,23) VALUE4
   IF(V4.NE.0)  CALL SHOWIT(1)
   IF(V5.NE.0)  WRITE(OUTLYNE,24) VALUE5
   IF(V5.NE.0)  CALL SHOWIT(1)
   IF(V6.NE.0)  WRITE(OUTLYNE,25) VALUE6
   IF(V6.NE.0)  CALL SHOWIT(1)
   IF(V6.NE.0)  WRITE(OUTLYNE,26)
   IF(V6.NE.0)  CALL SHOWIT(1)
10 FORMAT(A79)
20 FORMAT('NUMERIC VALUE #1 = ',D23.15)
21 FORMAT('NUMERIC VALUE #2 = ',D23.15)
22 FORMAT('NUMERIC VALUE #3 = ',D23.15)
23 FORMAT('NUMERIC VALUE #4 = ',D23.15)
24 FORMAT('NUMERIC VALUE #5 = ',D23.15)
25 FORMAT('NUMERIC VALUE #6 = ',D23.15)
26 FORMAT('NOTE: VALUE #6 FOR "CLAP" OR "COBS" IS THE "TILT" VALUE')
   IF(REPEAT.NE.0) REPEAT=REPEAT-1
   RETURN
END
! SUB MGADJ.FOR
SUBROUTINE MGADJ(MAG,I,J)
!
   use DATLEN
   use mod_system
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE MGADJ WHICH IMPLEMENTS THE MAGX
!       AND MAGY ADJUSTMEMT.
!
   REAL*8 MAG,OBJ,IMG,EFLX,EFLY,ARG
!
   INTEGER I,J,KK
!
!
   IF(WC.EQ.'MAGX') THEN
      DO KK=I,J-1
         IF(surf_solve_flag(KK).NE.0.0D0) THEN
            OUTLYNE='ALL SOLVES FROM'
            CALL SHOWIT(1)
            OUTLYNE='SURFACE (I) TO (J-1) MUST BE REMOVED'
            CALL SHOWIT(1)
            OUTLYNE='MAGNIFICATION ADJUSTMENT NOT PERFORMED'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END DO
      SAVE_KDP(1)=SAVEINPT(1)
      W1=DBLE(I+1)
      W2=DBLE(J-1)
      S3=0
      S4=0
      S5=0
      DF1=0
      DF2=0
      DF3=1
      DF4=1
      DF5=1
      SN=1
      SST=0
      STI=0
      SQ=1
      WC='FIRD'
      WQ='QUIET'
      CALL FIRD
      REST_KDP(1)=RESTINPT(1)
!     NOW ATTEMPT THE MAG ADJUSTMENT
!     NOW SET DISTANCES
      EFLX=GPREG(2)
      IF(EFLX.EQ.0.0D0) THEN
         OUTLYNE='EFLX IS ZERO'
         CALL SHOWIT(1)
         OUTLYNE='MAGNIFICATION ADJUSTMENT NOT PERFORMED'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      OBJ=((MAG+1.0D0)*EFLX)/MAG
      IF(OBJ.EQ.0.0D0) THEN
         OUTLYNE='OBJECT DISTANCE IS ZERO'
         CALL SHOWIT(1)
         OUTLYNE='MAGNIFICATION ADJUSTMENT NOT PERFORMED'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      ARG=((1.0D0/EFLX)-(1.0D0/OBJ))
      IF(ARG.EQ.0.0D0) THEN
         OUTLYNE='IMAGE DISTANCE DISTANCE IS INFINITE'
         CALL SHOWIT(1)
         OUTLYNE='MAGNIFICATION ADJUSTMENT NOT PERFORMED'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IMG=1.0D0/ARG
      call set_surf_thickness(I, OBJ-GPREG(9))
      call set_surf_thickness(J-1, IMG+GPREG(10))
      F1=0
      F6=1
      F22=0
      LNSTYP=1
      CALL LNSEOS
      WRITE(OUTLYNE,110) F12
      CALL SHOWIT(0)
      WRITE(OUTLYNE,201)I,J-1
      CALL SHOWIT(0)
      WRITE(OUTLYNE,202)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,200)I,J,MAG
      CALL SHOWIT(0)
      RETURN
   ELSE
!       WC NOT MAGX
   END IF
!
   IF(WC.EQ.'MAGY') THEN
      PRINT *, "Check for extra solves"

!       CHECK FOR EXTRA SOLVES
      DO KK=I,J-1
         IF(surf_solve_flag(KK).NE.0.0D0) THEN
            OUTLYNE='ALL SOLVES FROM'
            CALL SHOWIT(1)
            OUTLYNE='SURFACE (I) TO (J-1) MUST BE REMOVED'
            CALL SHOWIT(1)
            OUTLYNE='MAGNIFICATION ADJUSTMENT NOT PERFORMED'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END DO
      SAVE_KDP(1)=SAVEINPT(1)
      W1=DBLE(I+1)
      W2=DBLE(J-1)
      S3=0
      S4=0
      S5=0
      DF1=0
      DF2=0
      DF3=1
      DF4=1
      DF5=1
      SN=1
      SST=0
      STI=0
      SQ=1
      WC='FIRD'
      WQ='QUIET'
      CALL FIRD
      REST_KDP(1)=RESTINPT(1)
!     NOW ATTEMPT THE MAG ADJUSTMENT
!     NOW SET DISTANCES
      EFLY=GPREG(1)
      IF(EFLY.EQ.0.0D0) THEN
         OUTLYNE='EFLY IS ZERO'
         CALL SHOWIT(1)
         OUTLYNE='MAGNIFICATION ADJUSTMENT NOT PERFORMED'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      OBJ=((MAG+1.0D0)*EFLY)/MAG
      IF(OBJ.EQ.0.0D0) THEN
         OUTLYNE='OBJECT DISTANCE IS ZERO'
         CALL SHOWIT(1)
         OUTLYNE='MAGNIFICATION ADJUSTMENT NOT PERFORMED'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      ARG=((1.0D0/EFLY)-(1.0D0/OBJ))
      IF(ARG.EQ.0.0D0) THEN
         OUTLYNE='IMAGE DISTANCE DISTANCE IS INFINITE'
         CALL SHOWIT(1)
         OUTLYNE='MAGNIFICATION ADJUSTMENT NOT PERFORMED'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IMG=1.0D0/ARG
      call set_surf_thickness(I, OBJ-GPREG(7))
      call set_surf_thickness(J-1, IMG+GPREG(8))
      F1=0
      F6=1
      F22=0
      LNSTYP=1
      CALL LNSEOS
      WRITE(OUTLYNE,110) F12
      CALL SHOWIT(0)
      WRITE(OUTLYNE,201)I,J-1
      CALL SHOWIT(0)
      WRITE(OUTLYNE,202)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,400)I,J,MAG
      CALL SHOWIT(0)
      RETURN
   ELSE
!       NOT MAGY
   END IF
110 FORMAT('FOR CURRENT CONFIGURATION # ',I2)
201 FORMAT('AXIAL THICKNESSES OF SURFACES ',I3,' AND ',I3)
202 FORMAT('HAVE BEEN RESET SO THAT THE TRANSVERSE MAGNIFICTION')
200 FORMAT('IN THE XZ-PLANE FROM SURFACE ',I3,' TO SURFACE ',I3, ' = ',G11.3,'X')
400 FORMAT('IN THE YZ-PLANE FROM SURFACE ',I3,' TO SURFACE ',I3, ' = ',G11.3,'X')
   RETURN
END
SUBROUTINE ROO
   use DATLEN
   use mod_system
   use mod_surface
   use DATMAI
   IMPLICIT NONE
   IF(STI.EQ.1) THEN
      OUTLYNE='"ROO" SETS UP A SINGLE SURFACE OPTICAL ROOF'
      CALL SHOWIT(1)
      RETURN
   END IF
   IF(SST.EQ.1.OR.SQ.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      OUTLYNE='"ROO" TAKES NO QUALIFIER, STRING OR NUMERIC WORD #3 TO #5 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(W1.LE.0.0D0.OR.S1.EQ.0) THEN
      OUTLYNE='"ROO" REQUIRES AN EXPLICIT, POSITIVE APEX HEIGHT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(DF2.EQ.1) W2=0.0D0
   IF(DF3.EQ.1) W3=0.0D0
   call set_surf_roof_a(SURF, W1)
   call set_surf_roof_angle_err(SURF, W2)
   call set_surf_ccr_flag(SURF, 1)
   RETURN
END
SUBROUTINE CCR
   use DATLEN
   use mod_system
   use mod_surface
   use DATMAI
   IMPLICIT NONE
   IF(STI.EQ.1) THEN
      OUTLYNE='"CCR" SETS UP A SINGLE SURFACE CORNER CUBE'
      CALL SHOWIT(1)
      RETURN
   END IF
   IF(SST.EQ.1.OR.SQ.EQ.1) THEN
      OUTLYNE='"CCR" TAKES NO QUALIFIER OR STRING INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(S5.EQ.1) THEN
      OUTLYNE='"CCR" TAKES NO NUMERIC WORK #5 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(W1.LE.0.0D0.OR.S1.EQ.0) THEN
      OUTLYNE='"CCR" REQUIRES AN EXPLICIT, POSITIVE APEX HEIGHT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(DF2.EQ.1) W2=0.0D0
   IF(DF3.EQ.1) W3=0.0D0
   IF(DF4.EQ.1) W4=0.0D0
   call set_surf_roof_a(SURF, W1)
   call set_surf_roof_angle_err(SURF, W1)
   call set_surf_roof_b(SURF, W2)
   call set_surf_ccr_angle_err2(SURF, W4)
   call set_surf_ccr_flag(SURF, 2)
   RETURN
END
SUBROUTINE SHOWNSS
!     THIS DISPLAYS ROOF AND CCR DATA IN THE SEQUENTIAL DATABASE.
   use DATLEN
   use mod_system
   use mod_surface
   use DATMAI
   IMPLICIT NONE
   LOGICAL NONSS
   INTEGER I
   IF(STI.EQ.1) THEN
      OUTLYNE='"SHOWNSS" LISTS ALL ROOFS AND CORNER CUBES IN'
      CALL SHOWIT(1)
      OUTLYNE='THE CURRENT LENS CONFIGURATION'
      CALL SHOWIT(1)
      RETURN
   END IF
   IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
      OUTLYNE='"NSSRTG" TAKES NO NUMERIC, QUALIFIER OR STRING INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   NONSS=.TRUE.
   DO I=0,INT(sys_last_surf())
      IF(surf_ccr_flag(I).NE.0D0) NONSS=.FALSE.
   END DO
   IF(NONSS) THEN
      OUTLYNE='NO ROOF OR CCR DATA EXISTS FOR'
      CALL SHOWIT(1)
      OUTLYNE='THE CURRENT LENS CONFIGURATION'
      CALL SHOWIT(1)
      RETURN
   END IF
   OUTLYNE='ROOF/CCR DATA FOR THE CURRENT LENS CONFIGURATION'
   CALL SHOWIT(0)
   OUTLYNE='                 '
   CALL SHOWIT(0)
   DO I=0,INT(sys_last_surf())
      IF(surf_ccr_flag(I).EQ.1.0D0) THEN
         WRITE(OUTLYNE,*)'ROOF/CCR DATA FOR SURFACE NUMBER: ',I
         CALL SHOWIT(0)
!     NSS DATA EXISTS FOR SURFACE I, LIST IT
!
!     ROO
         IF(surf_ccr_flag(I).EQ.1.0D0) THEN
            OUTLYNE='SURFACE IS DEFINED AS "ROO"'
            CALL SHOWIT(0)
            WRITE(OUTLYNE,*) ' ROO APEX DISTANCE = ',surf_roof_a(I)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,*) ' ROO ANGLE ERROR   = ',surf_roof_angle_err(I),' ARCSEC'
            CALL SHOWIT(0)
         END IF
!
!     CCR
         IF(surf_ccr_flag(I).EQ.2.0D0) THEN
            OUTLYNE='SURFACE IS DEFINED AS "CCR"'
            CALL SHOWIT(0)
            WRITE(OUTLYNE,*) '   CCR APEX DISTANCE = ',surf_roof_a(I)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,*) 'CCR ANGLE ERROR #1   = ',surf_roof_angle_err(I),' ARCSEC'
            CALL SHOWIT(0)
            WRITE(OUTLYNE,*) 'CCR ANGLE ERROR #1   = ',surf_roof_b(I),' ARCSEC'
            CALL SHOWIT(0)
            WRITE(OUTLYNE,*) 'CCR ANGLE ERROR #1   = ',surf_ccr_angle_err2(I),' ARCSEC'
            CALL SHOWIT(0)
         END IF
      END IF
   END DO
   RETURN
END
! SUB DUMOUT.FOR
SUBROUTINE DUMOUT
!
   use DATLEN
   use mod_system
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
   INTEGER I
!
!
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SST.EQ.1) THEN
      OUTLYNE='AT THE CMD LEVEL, "DUMOUT" TAKES NO STRING INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.1.AND.S1.EQ.1) THEN
      OUTLYNE='AT THE CMD LEVEL, "DUMOUT" TAKES EITHER QUALIFIER OR'
      CALL SHOWIT(1)
      OUTLYNE='NUMERIC WORD #1 INPUT BUT NOT BOTH'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      OUTLYNE='AT THE CMD LEVEL,'
      CALL SHOWIT(1)
      OUTLYNE='"DUMOUT" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!       WHAT IF NO SURFACES EXIST
   IF(sys_last_surf().EQ.0.0D0) THEN
      OUTLYNE='NO NODUM DATA EXISTS'
      CALL SHOWIT(1)
      OUTLYNE='LENS SYSTEM HAS NO SURFACES'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!       W1 DESIGNATES THE SURFACE FOR WHICH THE NORMAL
!       DATA IS TO BE OUTPUT.
!       IF THE QUALIFIER "ALL" IS USED, THEN THE NORMAL DATA FOR
!       THE ENTIRE LENS IS PRINTED.
!
!       PRINT OUT FOR AN INDIVIDUAL SURFACE
!
   IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
!       THIS IS THE SAME AS NORMAL,0
      SQ=0
      WQ='        '
      S1=1
      W1=0.0D0
      DF1=0
   ELSE
!       NOT "OB" OR "OBJ"
   END IF
!
   IF(SQ.EQ.0) THEN
      IF(W1.LT.0.0D0) W1=sys_last_surf()+W1
      IF(DF1.EQ.1) W1=DBLE(INT(sys_last_surf()))
      I=INT(W1)
      IF(I.GT.INT(sys_last_surf()).OR.I.LT.0) THEN
         OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(HEADIN) WRITE(OUTLYNE,500)
      IF(HEADIN) CALL SHOWIT(0)
      IF(surf_dummy_val(I).EQ.0.0D0) WRITE(OUTLYNE,100)I,'NO/OFF '
      IF(surf_dummy_val(I).EQ.1.0D0) WRITE(OUTLYNE,100)I,'YES/ON '
      CALL SHOWIT(0)
      CALL SHOWIT(0)
      RETURN
   ELSE
!       THERE WAS A QUALIFIER.
      IF(WQ.NE.'ALL') THEN
         OUTLYNE='INVALID QUALIFIER WORD'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!
!       CHECK FOR NO DATA
!
!       PRINT HEADER MESSAGE
      WRITE(OUTLYNE,400)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,401)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,500)
      CALL SHOWIT(0)
      DO I=0,INT(sys_last_surf())
         IF(surf_dummy_val(I).EQ.0.0D0) WRITE(OUTLYNE,100)I,'NO/OFF '
         IF(surf_dummy_val(I).EQ.1.0D0) WRITE(OUTLYNE,100)I,'YES/ON '
         CALL SHOWIT(0)
      END DO
!
      RETURN
   END IF
400 FORMAT('SURFACE (FORCED DUMMY) DATA')
401 FORMAT(1X)
500 FORMAT('SURF',5X,'FORCED DUMMY SETTING')
100 FORMAT(I3,11X,A8)
END
! SUB BLKOUT.FOR
SUBROUTINE BLKOUT
!
   use DATLEN
   use mod_system
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
   INTEGER I
!
!
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SST.EQ.1) THEN
      OUTLYNE='AT THE CMD LEVEL, "FOOTBLOK" TAKES NO STRING INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.1.AND.S1.EQ.1) THEN
      OUTLYNE='AT THE CMD LEVEL, "FOOTBLOK" TAKES EITHER QUALIFIER OR'
      CALL SHOWIT(1)
      OUTLYNE='NUMERIC WORD #1 INPUT BUT NOT BOTH'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      OUTLYNE='AT THE CMD LEVEL,'
      CALL SHOWIT(1)
      OUTLYNE='"FOOTBLOK" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!       WHAT IF NO SURFACES EXIST
   IF(sys_last_surf().EQ.0.0D0) THEN
      OUTLYNE='NO FOOTBLOK DATA EXISTS'
      CALL SHOWIT(1)
      OUTLYNE='LENS SYSTEM HAS NO SURFACES'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!       W1 DESIGNATES THE SURFACE FOR WHICH THE FOOTBLOK
!       DATA IS TO BE OUTPUT.
!       IF THE QUALIFIER "ALL" IS USED, THEN THE FOOTBLOK DATA FOR
!       THE ENTIRE LENS IS PRINTED.
!
!       PRINT OUT FOR AN INDIVIDUAL SURFACE
!
   IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
!       THIS IS THE SAME AS FOOTBLOK,0
      SQ=0
      WQ='        '
      S1=1
      W1=0.0D0
      DF1=0
   ELSE
!       NOT "OB" OR "OBJ"
   END IF
!
   IF(SQ.EQ.0) THEN
      IF(W1.LT.0.0D0) W1=sys_last_surf()+W1
      IF(DF1.EQ.1) W1=DBLE(INT(sys_last_surf()))
      I=INT(W1)
      IF(I.GT.INT(sys_last_surf()).OR.I.LT.0) THEN
         OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(HEADIN) WRITE(OUTLYNE,500)
      CALL SHOWIT(0)
      IF(surf_footblok_flag(I).EQ.0.0D0) WRITE(OUTLYNE,100)I,'OFF     '
      IF(surf_footblok_flag(I).EQ.1.0D0) WRITE(OUTLYNE,100)I,'ON      '
      CALL SHOWIT(0)
      RETURN
   ELSE
!       THERE WAS A QUALIFIER.
      IF(WQ.NE.'ALL') THEN
         OUTLYNE='INVALID QUALIFIER WORD'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!
!       CHECK FOR NO DATA
!
!       PRINT HEADER MESSAGE
      WRITE(OUTLYNE,400)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,401)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,500)
      CALL SHOWIT(0)
      DO I=0,INT(sys_last_surf())
         IF(surf_footblok_flag(I).EQ.0.0D0) WRITE(OUTLYNE,100)I,'OFF     '
         IF(surf_footblok_flag(I).EQ.1.0D0) WRITE(OUTLYNE,100)I,'ON      '
         CALL SHOWIT(0)
      END DO
!
      RETURN
   END IF
400 FORMAT('SURFACE FOOTBLOK DATA')
401 FORMAT(1X)
500 FORMAT('SURF',5X,'FOOTBLOK DEFINITION')
100 FORMAT(I3,11X,A8)
END
! SUB PIVAXOUT.FOR
SUBROUTINE PIVAXOUT
!
   use DATLEN
   use mod_system
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
   INTEGER I
!
!
!      PIVAXIS AT THE COMMAND LEVEL
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SST.EQ.1) THEN
      OUTLYNE='AT THE CMD LEVEL, "PIVAXIS" TAKES NO STRING INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.1.AND.S1.EQ.1) THEN
      OUTLYNE='AT THE CMD LEVEL, "PIVAXIS" TAKES EITHER QUALIFIER OR'
      CALL SHOWIT(1)
      OUTLYNE='NUMERIC WORD #1 INPUT BUT NOT BOTH'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      OUTLYNE='AT THE CMD LEVEL,'
      CALL SHOWIT(1)
      OUTLYNE='"PIVAXIS" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!       WHAT IF NO SURFACES EXIST
   IF(sys_last_surf().EQ.0.0D0) THEN
      OUTLYNE='NO PIVAXIS DATA EXISTS'
      CALL SHOWIT(1)
      OUTLYNE='LENS SYSTEM HAS NO SURFACES'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!       W1 DESIGNATES THE SURFACE FOR WHICH THE PIVAXIS
!       DATA IS TO BE OUTPUT.
!       IF THE QUALIFIER "ALL" IS USED, THEN THE PIVAXIS DATA FOR
!       THE ENTIRE LENS IS PRINTED.
!
!       PRINT OUT FOR AN INDIVIDUAL SURFACE
!
   IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
!       THIS IS THE SAME AS PIVAXIS,0
      SQ=0
      WQ='        '
      S1=1
      W1=0.0D0
      DF1=0
   ELSE
!       NOT "OB" OR "OBJ"
   END IF
!
   IF(SQ.EQ.0) THEN
      IF(W1.LT.0.0D0) W1=sys_last_surf()+W1
      IF(DF1.EQ.1) W1=DBLE(INT(sys_last_surf()))
      I=INT(W1)
      IF(I.GT.INT(sys_last_surf()).OR.I.LT.0) THEN
         OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(HEADIN) WRITE(OUTLYNE,500)
      CALL SHOWIT(0)
      IF(surf_pivot_axis(I).EQ.0.0D0) WRITE(OUTLYNE,100)I,'VERTEX  '
      IF(surf_pivot_axis(I).EQ.1.0D0) WRITE(OUTLYNE,100)I,'NORMAL  '
      CALL SHOWIT(0)
      RETURN
   ELSE
!       THERE WAS A QUALIFIER.
      IF(WQ.NE.'ALL') THEN
         OUTLYNE='INVALID QUALIFIER WORD'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!
!       CHECK FOR NO DATA
!
!       PRINT HEADER MESSAGE
      WRITE(OUTLYNE,400)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,401)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,500)
      CALL SHOWIT(0)
      DO I=0,INT(sys_last_surf())
         IF(surf_pivot_axis(I).EQ.0.0D0) WRITE(OUTLYNE,100)I,'VERTEX  '
         IF(surf_pivot_axis(I).EQ.1.0D0) WRITE(OUTLYNE,100)I,'NORMAL  '
         CALL SHOWIT(0)
      END DO
!
      RETURN
   END IF
400 FORMAT('SURFACE PIVAXIS DATA')
401 FORMAT(1X)
500 FORMAT('SURF',5X,'PIVAXIS DEFINITION')
100 FORMAT(I3,11X,A8)
END
! SUB SURFTYPE.FOR
SUBROUTINE SURFTYPE
!
   use DATLEN
   use mod_system
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
   INTEGER I
!
!     DOES THE SURTYPE COMMAND
!
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SST.EQ.1) THEN
      OUTLYNE='AT THE CMD LEVEL, "SURTYPE" TAKES NO STRING INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.1.AND.S1.EQ.1) THEN
      OUTLYNE='AT THE CMD LEVEL, "SURTYPE" TAKES EITHER QUALIFIER OR'
      CALL SHOWIT(1)
      OUTLYNE='NUMERIC WORD #1 INPUT BUT NOT BOTH'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      OUTLYNE='AT THE CMD LEVEL,'
      CALL SHOWIT(1)
      OUTLYNE='"SURTYPE" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!       WHAT IF NO SURFACES EXIST
   IF(sys_last_surf().EQ.0.0D0) THEN
      OUTLYNE='NO SURFACE TYPE DATA EXISTS'
      CALL SHOWIT(1)
      OUTLYNE='LENS SYSTEM HAS NO SURFACES'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!       W1 DESIGNATES THE SURFACE FOR WHICH THE SURFACE TYPE
!       DATA IS TO BE OUTPUT.
!       IF THE QUALIFIER "ALL" IS USED, THEN THE SURFACE DATA FOR
!       THE ENTIRE LENS IS PRINTED.
!
!       PRINT OUT FOR AN INDIVIDUAL SURFACE
!
   IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
!       THIS IS THE SAME AS SURTYPE,0
      SQ=0
      WQ='        '
      S1=1
      W1=0.0D0
      DF1=0
   ELSE
!       NOT "OB" OR "OBJ"
   END IF
!
   IF(SQ.EQ.0) THEN
      IF(W1.LT.0.0D0) W1=sys_last_surf()+W1
      IF(DF1.EQ.1) W1=DBLE(INT(sys_last_surf()))
      I=INT(W1)
      IF(I.GT.INT(sys_last_surf()).OR.I.LT.0) THEN
         OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(HEADIN) WRITE(OUTLYNE,500)
      CALL SHOWIT(0)
      IF(surf_paraxial_val(I).EQ.0.0D0) WRITE(OUTLYNE,100)I,'REAL    '
      IF(surf_paraxial_val(I).EQ.1.0D0) WRITE(OUTLYNE,100)I,'PARAXIAL'
      CALL SHOWIT(0)
      RETURN
   ELSE
!       THERE WAS A QUALIFIER.
      IF(WQ.NE.'ALL') THEN
         OUTLYNE='INVALID QUALIFIER WORD'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!
!       CHECK FOR NO DATA
!
!       PRINT HEADER MESSAGE
      WRITE(OUTLYNE,400)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,401)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,500)
      CALL SHOWIT(0)
      DO I=0,INT(sys_last_surf())
         IF(surf_paraxial_val(I).EQ.0.0D0) WRITE(OUTLYNE,100)I,'REAL    '
         IF(surf_paraxial_val(I).EQ.1.0D0) WRITE(OUTLYNE,100)I,'PARAXIAL'
         CALL SHOWIT(0)
      END DO
!
      RETURN
   END IF
400 FORMAT('SURFACE TYPE DATA')
401 FORMAT(1X)
500 FORMAT('SURF',5X,'  SURFACE TYPE')
100 FORMAT(I3,11X,A8)
END
! SUB SGLASS.FOR
SUBROUTINE SGLASS
!
   use DATLEN
   use mod_system
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SGLASS WHICH IMPLEMENTS THE GLASS
!       AND MODEL COMMAND AT THE LENS OF UPDATE LENS LEVEL.
!
   INTEGER PIKCNT,I
!
!
!       "GLASS" IS A SURFACE TERMINAL COMMAND. IN THE LENS INPUT
!       ROUTINE F5=1, "GLASS" CAUSES THE SURFACE COUNTING VARIABLE
!       (SURF) AND sys_last_surf() TO BE INCREMENTED BY 1 AND 1.0
!       AND CAUSES THE NEXT SURFACE DATA TO BE STORED FOR THE NEXT
!       SURFACE. AT F6=1 (UPDATE ROUTINE) THIS INCREMENTING DOES
!       NO OCCUR. THE LAST SURFACE FOR WHICH DATA MAY BE ENTERED
!       IS SURFACE MAXSUR. SURF AND sys_last_surf() ARE NOT INCREMENTED
!       PASSED MAXSUR SO IF AN ATTEMPT IS MADE TO INPUT PASSED
!       SURFACE MAXSUR IN THE LENS INPUT MODE, THE DATA FOR SURFACE
!       MAXSUR WILL BE OVERWRITTEN.
!
!       "MODEL" PUTS IN A CODE-V LIKE FICTICIOUS GLASS WHICH HAS THE
!       INDEX RESOLVED IN THE LNSEOS SUBROUTINE
!
!               CHECK FOR STRING INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   SST=0
   IF(SST.EQ.1) THEN
      IF(WC.EQ.'GLASS') OUTLYNE='"GLASS" TAKES NO STRING INPUT'
      IF(WC.EQ.'MODEL') OUTLYNE='"MODEL" TAKES NO STRING INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.0) THEN
      IF(WC.EQ.'GLASS')OUTLYNE='"GLASS" REQUIRES EXPLICIT QUALIFIER INPUT'
      IF(WC.EQ.'MODEL')OUTLYNE='"MODEL" REQUIRES EXPLICIT QUALIFIER INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
!
!       DEFAULT INDICES ALWAYS GO TO 1.0 NOT TO 0.0
!
!
   IF(WC.EQ.'MODEL'.AND.S4.EQ.1.OR.WC.EQ.'MODEL'.AND.S5.EQ.1) THEN
      OUTLYNE='"MODEL" TAKES NO NUMERIC WORD #3 THROUGH #5 INPUT'
      CALL SHOWIT(0)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(0)
      CALL MACFAL
      RETURN
   END IF
   IF(WC.NE.'MODEL') THEN
      IF(DF1.EQ.1) W1=1.0D0
      IF(DF2.EQ.1) W2=1.0D0
      IF(DF3.EQ.1) W3=1.0D0
      IF(DF4.EQ.1) W4=1.0D0
      IF(DF5.EQ.1) W5=1.0D0
   END IF
   IF(WC.EQ.'MODEL') THEN
      IF(DF1.EQ.1) W1=1.0D0
      IF(DF2.EQ.1) W2=0.0D0
      IF(DF3.EQ.1) W3=0.0D0
   END IF
   IF(WC.EQ.'GLASS') THEN
      IF(W1.EQ.0.0D0.OR.W2.EQ.0.0D0.OR.W3.EQ.0.0D0.OR.W4.EQ.0.0D0 .OR.W5.EQ.0.0D0) THEN
         OUTLYNE='"GLASS" TAKES NO ZERO VALUES FOR REFRACTIVE INDICES'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(WC.NE.'MODEL') THEN
      IF(W1.EQ.0.0D0) THEN
         OUTLYNE='"MODEL" TAKES NO ZERO VALUES FOR THE "INDEX" VALUE'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      call set_surf_refractive_index(SURF, 1, W1)
      call set_surf_refractive_index(SURF, 2, W2)
      call set_surf_refractive_index(SURF, 3, W3)
      call set_surf_refractive_index(SURF, 4, W4)
      call set_surf_refractive_index(SURF, 5, W5)
      GLANAM(SURF,1)='GLASS      '
      GLANAM(SURF,2)= WQ//'     '
   END IF
   IF(WC.EQ.'MODEL') THEN
      IF(W1.LE.0.0D0) THEN
         OUTLYNE='"MODEL" REQUIRES A NON-ZERO, POSITIVE INPUT INDEX VALUE'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      call set_surf_fict_n(SURF, W1)
      call set_surf_fict_v(SURF, W2)
      call set_surf_fict_w(SURF, W3)
      GLANAM(SURF,1)='MODEL        '
      GLANAM(SURF,2)= WQ//'     '
   END IF
!
!       NOW HANDEL EXISTING PIKUPS OF GLASS TYPE, I E DELETE THEM.
!       CHECK FOR CC PIKUPS AND DELETE IF FOUND
!
!
   IF(PIKUP(1,SURF,20).EQ.0.0) THEN
!
!       NO GLASS PIKUPS, JUST RETURN
!
      IF(F5.EQ.1.AND.SURF.LT.MAXSUR) THEN
         SURF=SURF+1
         ALENS(1:LSIZ,SURF)=0.0D0
         call set_sys_last_surf(DBLE(SURF))
      ELSE
!       DON'T INCREMENT SURF AND sys_last_surf()
      END IF
      F22=1
      RETURN
   ELSE
   END IF
!
!       DELETE THE PIKUP
   PIKUP(1:6,SURF,20)=0.0
   call set_surf_special_type(SURF, surf_special_type(SURF)-1)
!
!
!       PRINT MESSAGE
   WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GLASS) DELETED'
   CALL SHOWIT(1)
!
   PIKCNT=0
   DO 10 I=1,PSIZ
      IF(PIKUP(1,SURF,I).NE.0.0) THEN
         PIKCNT=PIKCNT+1
      ELSE
      END IF
10 CONTINUE
!
   IF(PIKCNT.EQ.0.0)call set_surf_special_type(SURF, 0)
   IF(F5.EQ.1.AND.SURF.LT.MAXSUR) THEN
      SURF=SURF+1
      ALENS(1:LSIZ,SURF)=0.0D0
      call set_sys_last_surf(DBLE(SURF))
   ELSE
!       DON'T INCREMENT SURF AND sys_last_surf()
   END IF
!
   F22=1
   RETURN
END
SUBROUTINE RAYERROR
   use DATLEN
   use mod_system
   use mod_surface
   use DATMAI
   IMPLICIT NONE
   IF(STI.EQ.1) THEN
      OUTLYNE='"RAYERROR" SETS A RAYDOM RAY/SURFACE ANGLE ERROR'
      CALL SHOWIT(1)
      RETURN
   END IF
   IF(SST.EQ.1.OR.SQ.EQ.1) THEN
      OUTLYNE='"RAYERROR" TAKES NO QUALIFIER OR STRING INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      OUTLYNE='"RAYERROR" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   call set_surf_ray_error(SURF, W1)
   RETURN
END
