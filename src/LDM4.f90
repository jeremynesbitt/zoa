!       FOURTH FILE FOR LENS DATABASE MANAGER FILES

! SUB COATING.FOR
SUBROUTINE COATING
!
   use DATLEN
   use DATMAI
   use mod_surface
   use mod_system, only: sys_last_surf
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE COATING WHICH IMPLEMENTS THE COATING
!       COMMAND AT THE CMD LEVEL
!       THE ONLY VALID QUALIFIERS ARE (BLANK) AND "ALL","OB" AND "OBJ"
!       THE NUMERIC ENTRY W1, IS THE SURFACE DESIGNATOR
!       FOR PRINTOUT OF ONE SURFACE'S DATA ONLY
!
!
   IF(WC.EQ.'COATING') THEN
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
      IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         OUTLYNE=&
         &'"COATING" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
         CALL SHOWIT(1)
         OUTLYNE=&
         &'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETURN
      END IF
      IF(SQ.EQ.1.AND.S1.EQ.1) THEN
         S1=0
         DF1=0
         W1=0.0D0
         OUTLYNE=&
         &'"COATING" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('BUT NOT BOTH'//'\n'//'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!       WHAT IF NO SURFACES EXIST
      IF(sys_last_surf().EQ.0.0D0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'NO SURFACE COATINGS EXIST'//'\n'//&
         & 'LENS SYSTEM HAS NO SURFACES'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!
      IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
!
!       THIS IS SAME AS TAD,0
         S1=1
         DF1=0
         SQ=0
         WQ='        '
         W1=0.0
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
         IF(DF1.EQ.1) W1=DBLE(INT(sys_last_surf()))
         SURF=INT(W1)
         IF(SURF.GT.(INT(sys_last_surf())).OR.SURF.LT.0) THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & 'SURFACE NUMBER BEYOND LEGAL RANGE'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
!
!
         IF(HEADIN) WRITE(OUTLYNE,2000)
         IF(HEADIN) CALL SHOWIT(0)
         IF(surf_coating_index(SURF).NE.0.0D0)&
         &WRITE(OUTLYNE,300) SURF,INT(surf_coating_index(SURF))
         IF(surf_coating_index(SURF).EQ.0.0D0)&
         &WRITE(OUTLYNE,301) SURF
         CALL SHOWIT(0)
      ELSE
!*********************************************************************
!       SQ=1 AND ONLY ALLOWED WQ BY NOW IS "ALL"
!       HANDEL THE WHOLE LENS
!       PRINT HEADING DATA
         WRITE(OUTLYNE,1000)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,1500)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,2000)
         CALL SHOWIT(0)
         DO SURF=0,INT(sys_last_surf())
            IF(surf_coating_index(SURF).NE.0.0D0)&
            &WRITE(OUTLYNE,300) SURF,INT(surf_coating_index(SURF))
            IF(surf_coating_index(SURF).EQ.0.0D0)&
            &WRITE(OUTLYNE,301) SURF
            CALL SHOWIT(0)
         END DO
      END IF
   ELSE
!     NOT RIN
   END IF
!
300 FORMAT(I3,11X,I4)
301 FORMAT(I3,5X,'SURFACE NOT COATED')
1000 FORMAT('SURFACE COATING NUMBER DATA')
2000 FORMAT('SURF',4X,'COATING NUMBER')
1500 FORMAT(1X)
   RETURN
END
! SUB SNDEX.FOR
SUBROUTINE SNDEX
!
   use DATLEN
   use DATMAI
   use mod_surface
   use mod_system, only: sys_last_surf
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SNDEX WHICH IMPLEMENTS THE NDEX, NDEX2
!       COMMAND AT THE CMD LEVEL
!       THE ONLY VALID QUALIFIERS ARE (BLANK) AND "ALL","OB" AND "OBJ"
!       THE NUMERIC ENTRY W1, IS THE SURFACE DESIGNATOR
!       FOR PRINTOUT OF ONE SURFACE'S DATA ONLY
!
   LOGICAL PRINTIT
!
   IF(WC.EQ.'NDEX') THEN
!               THIS IS A CMD LEVEL COMMAND ONLY
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
      IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         OUTLYNE=&
         &'"NDEX" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
         CALL SHOWIT(1)
         OUTLYNE=&
         &'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETURN
      END IF
      IF(SQ.EQ.1.AND.S1.EQ.1) THEN
         S1=0
         DF1=0
         W1=0.0D0
         OUTLYNE=&
         &'"NDEX" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('BUT NOT BOTH'//'\n'//'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!       WHAT IF NO SURFACES EXIST
      IF(sys_last_surf().EQ.0.0D0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'NO REFRACTIVE INDICES EXIST'//'\n'//&
         & 'LENS SYSTEM HAS NO SURFACES'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!
!       W1 DESIGNATES THE SURFACE FOR WHICH THE NDEX
!       DATA IS TO BE OUTPUT.
!       IF THE QUALIFIER "ALL" IS USED, THEN THE NDEX DATA FOR
!       THE ENTIRE LENS IS PRINTED. IF THE QUALIFIER IS "OB" OR "OBJ"
!       THEN DATA FOR THE OBJECT SURFACE IS PRINTED.
!
      IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
!
!       THIS IS SAME AS TAD,0
         S1=1
         DF1=0
         SQ=0
         WQ='        '
         W1=0.0
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
         IF(DF1.EQ.1) W1=DBLE(INT(sys_last_surf()))
         SURF=INT(W1)
         IF(SURF.GT.(INT(sys_last_surf())).OR.SURF.LT.0) THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & 'SURFACE NUMBER BEYOND LEGAL RANGE'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
!
         IF(HEADIN) WRITE(OUTLYNE,2000)
         IF(HEADIN) CALL SHOWIT(0)
         IF(DABS(surf_refractive_index(SURF, 1)).EQ.1.0D0.AND.&
         &DABS(surf_refractive_index(SURF, 2)).EQ.1.0D0.AND.&
         &DABS(surf_refractive_index(SURF, 3)).EQ.1.0D0.AND.&
         &DABS(surf_refractive_index(SURF, 4)).EQ.1.0D0.AND.&
         &DABS(surf_refractive_index(SURF, 5)).EQ.1.0D0) THEN
            WRITE(OUTLYNE,201) SURF
            CALL SHOWIT(0)
201         FORMAT('FOR SURFACE ',I3,', N1 TO N5 HAVE UNIT VALUES')
202         FORMAT('FOR ALL SURFACES, N1 TO N5 HAVE UNIT VALUES')
203         FORMAT('FOR SURFACE ',I3,', N6 TO N10 HAVE UNIT VALUES')
204         FORMAT('FOR ALL SURFACES, N6 TO N10 HAVE UNIT VALUES')
         ELSE
            WRITE(OUTLYNE,300) SURF,GLANAM(SURF,1),GLANAM(SURF,2)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,200) SURF,surf_refractive_index(SURF, 1),surf_refractive_index(SURF, 2),&
            &surf_refractive_index(SURF, 3),surf_refractive_index(SURF, 4),surf_refractive_index(SURF, 5)
            CALL SHOWIT(0)
         END IF
      ELSE
!*********************************************************************
!       SQ=1 AND ONLY ALLOWED WQ BY NOW IS "ALL"
!       HANDEL THE WHOLE LENS
!       PRINT HEADING DATA
         WRITE(OUTLYNE,1000)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,1500)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,2000)
         CALL SHOWIT(0)


         PRINTIT=.FALSE.
         DO SURF=0,INT(sys_last_surf())
            IF(DABS(surf_refractive_index(SURF, 1)).EQ.1.0D0.AND.&
            &DABS(surf_refractive_index(SURF, 2)).EQ.1.0D0.AND.&
            &DABS(surf_refractive_index(SURF, 3)).EQ.1.0D0.AND.&
            &DABS(surf_refractive_index(SURF, 4)).EQ.1.0D0.AND.&
            &DABS(surf_refractive_index(SURF, 5)).EQ.1.0D0) THEN
            ELSE
               PRINTIT=.TRUE.
            END IF
         END DO
         IF(.NOT.PRINTIT) THEN
            WRITE(OUTLYNE,202)
            CALL SHOWIT(0)
            RETURN
         END IF
         DO SURF=0,INT(sys_last_surf())
            IF(DABS(surf_refractive_index(SURF, 1)).EQ.1.0D0.AND.&
            &DABS(surf_refractive_index(SURF, 2)).EQ.1.0D0.AND.&
            &DABS(surf_refractive_index(SURF, 3)).EQ.1.0D0.AND.&
            &DABS(surf_refractive_index(SURF, 4)).EQ.1.0D0.AND.&
            &DABS(surf_refractive_index(SURF, 5)).EQ.1.0D0) THEN
            ELSE
               WRITE(OUTLYNE,300) SURF,GLANAM(SURF,1),GLANAM(SURF,2)
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) SURF,surf_refractive_index(SURF, 1),surf_refractive_index(SURF, 2),&
               &surf_refractive_index(SURF, 3),surf_refractive_index(SURF, 4),surf_refractive_index(SURF, 5)
               CALL SHOWIT(0)
            END IF
         END DO
      END IF
   ELSE
!     NOT NDEX
   END IF
   IF(WC.EQ.'NDEX2') THEN
!               THIS IS A CMD LEVEL COMMAND ONLY
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
      IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         OUTLYNE=&
         &'"NDEX" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(SQ.EQ.1.AND.S1.EQ.1) THEN
         S1=0
         DF1=0
         W1=0.0D0
         OUTLYNE=&
         &'"NDEX2" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('BUT NOT BOTH'//'\n'//'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!       WHAT IF NO SURFACES EXIST
      IF(sys_last_surf().EQ.0.0D0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'NO REFRACTIVE INDICES EXIST'//'\n'//&
         & 'LENS SYSTEM HAS NO SURFACES'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!
!       W1 DESIGNATES THE SURFACE FOR WHICH THE NDEX2
!       DATA IS TO BE OUTPUT.
!       IF THE QUALIFIER "ALL" IS USED, THEN THE NDEX2 DATA FOR
!       THE ENTIRE LENS IS PRINTED. IF THE QUALIFIER IS "OB" OR "OBJ"
!       THEN DATA FOR THE OBJECT SURFACE IS PRINTED.
!
      IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
!
!       THIS IS SAME AS TAD,0
         S1=1
         DF1=0
         SQ=0
         WQ='        '
         W1=0.0
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
         IF(DF1.EQ.1) W1=DBLE(INT(sys_last_surf()))
         SURF=INT(W1)
         IF(SURF.GT.(INT(sys_last_surf())).OR.SURF.LT.0) THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & 'SURFACE NUMBER BEYOND LEGAL RANGE'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(HEADIN) WRITE(OUTLYNE,2002)
         IF(HEADIN) CALL SHOWIT(0)
         IF(DABS(surf_refractive_index(SURF, 6)).EQ.1.0D0.AND.&
         &DABS(surf_refractive_index(SURF, 7)).EQ.1.0D0.AND.&
         &DABS(surf_refractive_index(SURF, 8)).EQ.1.0D0.AND.&
         &DABS(surf_refractive_index(SURF, 9)).EQ.1.0D0.AND.&
         &DABS(surf_refractive_index(SURF, 10)).EQ.1.0D0) THEN
            WRITE(OUTLYNE,203) SURF
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,300) SURF,GLANAM(SURF,1),GLANAM(SURF,2)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,200) SURF,surf_refractive_index(SURF, 6),surf_refractive_index(SURF, 7),&
            &surf_refractive_index(SURF, 8),surf_refractive_index(SURF, 9),surf_refractive_index(SURF, 10)
            CALL SHOWIT(0)
         END IF
      ELSE
!*********************************************************************
!       SQ=1 AND ONLY ALLOWED WQ BY NOW IS "ALL"
!       HANDEL THE WHOLE LENS
!       PRINT HEADING DATA
         WRITE(OUTLYNE,1002)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,1500)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,2002)
         CALL SHOWIT(0)
         PRINTIT=.FALSE.
         DO SURF=0,INT(sys_last_surf())
            IF(DABS(surf_refractive_index(SURF, 6)).EQ.1.0D0.AND.&
            &DABS(surf_refractive_index(SURF, 7)).EQ.1.0D0.AND.&
            &DABS(surf_refractive_index(SURF, 8)).EQ.1.0D0.AND.&
            &DABS(surf_refractive_index(SURF, 9)).EQ.1.0D0.AND.&
            &DABS(surf_refractive_index(SURF, 10)).EQ.1.0D0) THEN
            ELSE
               PRINTIT=.TRUE.
            END IF
         END DO
         IF(.NOT.PRINTIT) THEN
            WRITE(OUTLYNE,204)
            CALL SHOWIT(0)
            RETURN
         END IF
         DO SURF=0,INT(sys_last_surf())
            IF(DABS(surf_refractive_index(SURF, 6)).EQ.1.0D0.AND.&
            &DABS(surf_refractive_index(SURF, 7)).EQ.1.0D0.AND.&
            &DABS(surf_refractive_index(SURF, 8)).EQ.1.0D0.AND.&
            &DABS(surf_refractive_index(SURF, 9)).EQ.1.0D0.AND.&
            &DABS(surf_refractive_index(SURF, 10)).EQ.1.0D0) THEN
            ELSE
               WRITE(OUTLYNE,300) SURF,GLANAM(SURF,1),GLANAM(SURF,2)
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) SURF,surf_refractive_index(SURF, 6),surf_refractive_index(SURF, 7),&
               &surf_refractive_index(SURF, 8),surf_refractive_index(SURF, 9),surf_refractive_index(SURF, 10)
               CALL SHOWIT(0)
            END IF
         END DO
      END IF
   ELSE
!     NOT NDEX2
   END IF
!
1002 FORMAT('REFRACTIVE INDEX DATA (WAVELENGTHS #6 TO #10)')
2002 FORMAT('SURF',4X,'N6',13X,'N7',13X,'N8',13X,'N9',13X,'N10')
200 FORMAT(I3,1X,G14.6,1X,G14.6,1X,G14.6,1X,G14.6,1X,G14.6)
300 FORMAT(I3,1X,A13,1X,A13)
!
1000 FORMAT('REFRACTIVE INDEX DATA (WAVELENGTHS #1 TO #5)')
2000 FORMAT('SURF',4X,'N1',13X,'N2',13X,'N3',13X,'N4',13X,'N5')
1500 FORMAT(1X)
   RETURN
END
! SUB SGRATT.FOR
SUBROUTINE SGRATT
!
   use DATLEN
   use DATMAI
   use mod_surface
   use mod_system, only: sys_last_surf
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SRIN WHICH IMPLEMENTS THE GRT
!       COMMAND AT THE CMD LEVEL
!       THE ONLY VALID QUALIFIERS ARE (BLANK) AND "ALL","OB" AND "OBJ"
!       THE NUMERIC ENTRY W1, IS THE SURFACE DESIGNATOR
!       FOR PRINTOUT OF ONE SURFACE'S DATA ONLY
!
   LOGICAL NOGRT
!
!
!               THIS IS A CMD LEVEL COMMAND ONLY
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      OUTLYNE=&
      &'"GRT" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      RETURN
   END IF
   IF(SQ.EQ.1.AND.S1.EQ.1) THEN
      S1=0
      DF1=0
      W1=0.0D0
      OUTLYNE=&
      &'"GRT" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
      CALL SHOWIT(1)
      CALL REPORT_ERROR_AND_FAIL('BUT NOT BOTH'//'\n'//'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!       WHAT IF NO SURFACES EXIST
   IF(sys_last_surf().EQ.0.0D0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'NO LINEAR GRATINGS EXIST'//'\n'//&
      & 'LENS SYSTEM HAS NO SURFACES'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!
!       W1 DESIGNATES THE SURFACE FOR WHICH THE RIN
!       DATA IS TO BE OUTPUT.
!       IF THE QUALIFIER "ALL" IS USED, THEN THE RIN DATA FOR
!       THE ENTIRE LENS IS PRINTED. IF THE QUALIFIER IS "OB" OR "OBJ"
!       THEN DATA FOR THE OBJECT SURFACE IS PRINTED.
!
   IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
!
!       THIS IS SAME AS TAD,0
      S1=1
      DF1=0
      SQ=0
      WQ='        '
      W1=0.0
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
      IF(DF1.EQ.1) W1=DBLE(INT(sys_last_surf()))
      SURF=INT(W1)
      IF(SURF.GT.(INT(sys_last_surf())).OR.SURF.LT.0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'SURFACE NUMBER BEYOND LEGAL RANGE'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(HEADIN) WRITE(OUTLYNE,2000)
      IF(HEADIN) CALL SHOWIT(0)
      IF(surf_diffraction_flag(SURF).EQ.1.0D0) THEN
         WRITE(OUTLYNE,200) SURF,surf_grating_order(SURF),surf_grating_spacing(SURF),&
         &surf_grating_vx(SURF),surf_grating_vy(SURF),surf_grating_vz(SURF)
         CALL SHOWIT(0)
      ELSE
         WRITE(OUTLYNE,300)
         CALL SHOWIT(0)
      END IF
   ELSE
!*********************************************************************
!       SQ=1 AND ONLY ALLOWED WQ BY NOW IS "ALL"
!       HANDEL THE WHOLE LENS
!       PRINT HEADING DATA
      NOGRT=.TRUE.
      DO SURF=0,INT(sys_last_surf())
!
         IF(surf_diffraction_flag(SURF).EQ.1.0D0) THEN
            NOGRT=.FALSE.
            GO TO 20
         END IF
      END DO
20    CONTINUE
      IF(.NOT.NOGRT) THEN
         WRITE(OUTLYNE,1000)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,1500)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,2000)
         CALL SHOWIT(0)
         DO SURF=0,INT(sys_last_surf())
!
            IF(surf_diffraction_flag(SURF).EQ.1.0D0) THEN
               WRITE(OUTLYNE,200) SURF,surf_grating_order(SURF),surf_grating_spacing(SURF),&
               &surf_grating_vx(SURF),surf_grating_vy(SURF),surf_grating_vz(SURF)
               CALL SHOWIT(0)
            END IF
         END DO
      ELSE
!     NO GRATINGS
         WRITE(OUTLYNE,300)
         CALL SHOWIT(0)
      END IF
   END IF
!
200 FORMAT(I3,1X,G14.6,1X,G14.6,1X,G14.6,1X,G14.6,1X,G14.6)
!
300 FORMAT('NO LINEAR DIFFRATION GRATING DATA FOUND')
1000 FORMAT('LINEAR DIFFRACTION GRATING DATA')
2000 FORMAT('SURF',4X,'GRO',12X,'GRS',12X,'GRX',12X,'GRY',12X,'GRZ')
1500 FORMAT(1X)
   RETURN
END
! SUB SRIN.FOR
SUBROUTINE SRIN
!
   use DATLEN
   use DATMAI
   use mod_surface
   use mod_system, only: sys_last_surf
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SRIN WHICH IMPLEMENTS THE RIN
!       COMMAND AT THE CMD LEVEL
!       THE ONLY VALID QUALIFIERS ARE (BLANK) AND "ALL","OB" AND "OBJ"
!       THE NUMERIC ENTRY W1, IS THE SURFACE DESIGNATOR
!       FOR PRINTOUT OF ONE SURFACE'S DATA ONLY
!
!
   IF(WC.EQ.'RIN') THEN
!               THIS IS A CMD LEVEL COMMAND ONLY
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
      IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         OUTLYNE=&
         &'"RIN" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
         CALL SHOWIT(1)
         OUTLYNE=&
         &'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETURN
      END IF
      IF(SQ.EQ.1.AND.S1.EQ.1) THEN
         S1=0
         DF1=0
         W1=0.0D0
         OUTLYNE=&
         &'"RIN" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('BUT NOT BOTH'//'\n'//'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!       WHAT IF NO SURFACES EXIST
      IF(sys_last_surf().EQ.0.0D0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'NO REFRACTIVE INDICES EXIST'//'\n'//&
         & 'LENS SYSTEM HAS NO SURFACES'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!
!       W1 DESIGNATES THE SURFACE FOR WHICH THE RIN
!       DATA IS TO BE OUTPUT.
!       IF THE QUALIFIER "ALL" IS USED, THEN THE RIN DATA FOR
!       THE ENTIRE LENS IS PRINTED. IF THE QUALIFIER IS "OB" OR "OBJ"
!       THEN DATA FOR THE OBJECT SURFACE IS PRINTED.
!
      IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
!
!       THIS IS SAME AS TAD,0
         S1=1
         DF1=0
         SQ=0
         WQ='        '
         W1=0.0
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
         IF(DF1.EQ.1) W1=DBLE(INT(sys_last_surf()))
         SURF=INT(W1)
         IF(SURF.GT.(INT(sys_last_surf())).OR.SURF.LT.0) THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & 'SURFACE NUMBER BEYOND LEGAL RANGE'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(HEADIN) WRITE(OUTLYNE,2000)
         IF(HEADIN) CALL SHOWIT(0)
         WRITE(OUTLYNE,300) SURF,GLANAM(SURF,1),GLANAM(SURF,2)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,200) SURF,surf_refractive_index(SURF, 1),surf_refractive_index(SURF, 2),&
         &surf_refractive_index(SURF, 3),surf_refractive_index(SURF, 4),surf_refractive_index(SURF, 5)
         CALL SHOWIT(0)
      ELSE
!*********************************************************************
!       SQ=1 AND ONLY ALLOWED WQ BY NOW IS "ALL"
!       HANDEL THE WHOLE LENS
!       PRINT HEADING DATA
         WRITE(OUTLYNE,1000)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,1500)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,2000)
         CALL SHOWIT(0)
         DO SURF=0,INT(sys_last_surf())
            WRITE(OUTLYNE,300) SURF,GLANAM(SURF,1),GLANAM(SURF,2)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,200) SURF,surf_refractive_index(SURF, 1),surf_refractive_index(SURF, 2),&
            &surf_refractive_index(SURF, 3),surf_refractive_index(SURF, 4),surf_refractive_index(SURF, 5)
            CALL SHOWIT(0)
         END DO
      END IF
   ELSE
!     NOT RIN
   END IF
   IF(WC.EQ.'RIN2') THEN
!               THIS IS A CMD LEVEL COMMAND ONLY
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
      IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         OUTLYNE=&
         &'"RIN2" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(SQ.EQ.1.AND.S1.EQ.1) THEN
         S1=0
         DF1=0
         W1=0.0D0
         OUTLYNE=&
         &'"RIN2" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('BUT NOT BOTH'//'\n'//'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!       WHAT IF NO SURFACES EXIST
      IF(sys_last_surf().EQ.0.0D0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'NO REFRACTIVE INDICES EXIST'//'\n'//&
         & 'LENS SYSTEM HAS NO SURFACES'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!
!       W1 DESIGNATES THE SURFACE FOR WHICH THE RIN2
!       DATA IS TO BE OUTPUT.
!       IF THE QUALIFIER "ALL" IS USED, THEN THE RIN2 DATA FOR
!       THE ENTIRE LENS IS PRINTED. IF THE QUALIFIER IS "OB" OR "OBJ"
!       THEN DATA FOR THE OBJECT SURFACE IS PRINTED.
!
      IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
!
!       THIS IS SAME AS TAD,0
         S1=1
         DF1=0
         SQ=0
         WQ='        '
         W1=0.0
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
         IF(DF1.EQ.1) W1=DBLE(INT(sys_last_surf()))
         SURF=INT(W1)
         IF(SURF.GT.(INT(sys_last_surf())).OR.SURF.LT.0) THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & 'SURFACE NUMBER BEYOND LEGAL RANGE'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(HEADIN) WRITE(OUTLYNE,2002)
         IF(HEADIN) CALL SHOWIT(0)
         WRITE(OUTLYNE,300) SURF,GLANAM(SURF,1),GLANAM(SURF,2)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,200) SURF,surf_refractive_index(SURF, 6),surf_refractive_index(SURF, 7),&
         &surf_refractive_index(SURF, 8),surf_refractive_index(SURF, 9),surf_refractive_index(SURF, 10)
         CALL SHOWIT(0)
      ELSE
!*********************************************************************
!       SQ=1 AND ONLY ALLOWED WQ BY NOW IS "ALL"
!       HANDEL THE WHOLE LENS
!       PRINT HEADING DATA
         WRITE(OUTLYNE,1002)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,1500)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,2002)
         CALL SHOWIT(0)
         DO SURF=0,INT(sys_last_surf())
            WRITE(OUTLYNE,300) SURF,GLANAM(SURF,1),GLANAM(SURF,2)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,200) SURF,surf_refractive_index(SURF, 6),surf_refractive_index(SURF, 7),&
            &surf_refractive_index(SURF, 8),surf_refractive_index(SURF, 9),surf_refractive_index(SURF, 10)
            CALL SHOWIT(0)
         END DO
      END IF
   ELSE
!     NOT RIN2
   END IF
!
1002 FORMAT('REFRACTIVE INDEX DATA (WAVELENGTHS #6 TO #10)')
2002 FORMAT('SURF',4X,'N6',13X,'N7',13X,'N8',13X,'N9',13X,'N10')
!
200 FORMAT(I3,1X,G14.6,1X,G14.6,1X,G14.6,1X,G14.6,1X,G14.6)
300 FORMAT(I3,1X,A13,1X,A13)
!
1000 FORMAT('REFRACTIVE INDEX DATA (WAVELENGTHS #1 TO #5)')
2000 FORMAT('SURF',4X,'N1',13X,'N2',13X,'N3',13X,'N4',13X,'N5')
1500 FORMAT(1X)
   RETURN
END
! SUB IMAGEDIR.FOR
SUBROUTINE IMAGEDIR
!
   use DATLEN
   use DATMAI
   use mod_surface
   use mod_system, only: sys_last_surf
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE IMAGEDIR WHICH IMPLEMENTS THE IMAGEDIR
!       COMMAND AT THE CMD LEVEL
!       THE ONLY VALID QUALIFIERS ARE (BLANK) AND "ALL","OB" AND "OBJ"
!       THE NUMERIC ENTRY W1, IS THE SURFACE DESIGNATOR
!       FOR PRINTOUT OF ONE SURFACE'S DATA ONLY
!
!
   IF(WC.EQ.'IMAGEDIR') THEN
!               THIS IS A CMD LEVEL COMMAND ONLY
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
      IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         OUTLYNE=&
         &'"IMAGEDIR" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
         CALL SHOWIT(1)
         OUTLYNE=&
         &'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETURN
      END IF
      IF(SQ.EQ.1.AND.S1.EQ.1) THEN
         S1=0
         DF1=0
         W1=0.0D0
         OUTLYNE=&
         &'"IMAGEDIR" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('BUT NOT BOTH'//'\n'//'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!       WHAT IF NO SURFACES EXIST
      IF(sys_last_surf().EQ.0.0D0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'NO IMAGE ORIENTATION DATA EXISTS'//'\n'//&
         & 'LENS SYSTEM HAS NO SURFACES'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(.NOT.SREFDIFEXT) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'NO IMAGE ORIENTATION DATA EXISTS'//'\n'//&
         & 'NO DIFFERENTIAL RAY DATA EXISTS'//'\n'//&
         & 'ISSUE AN "FOB" COMMAND TO PROCEED', 1)
         RETURN
      END IF
!
!       W1 DESIGNATES THE SURFACE FOR WHICH THE ORIENTATION
!       DATA IS TO BE OUTPUT.
!       IF THE QUALIFIER "ALL" IS USED, THEN THE IMAGE ORIENTATION DATA FOR
!       THE ENTIRE LENS IS PRINTED. IF THE QUALIFIER IS "OB" OR "OBJ"
!       THEN DATA FOR THE OBJECT SURFACE IS PRINTED.
!
      IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
!
!       THIS IS SAME AS TAD,0
         S1=1
         DF1=0
         SQ=0
         WQ='        '
         W1=0.0
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
         IF(DF1.EQ.1) W1=DBLE(INT(sys_last_surf()))
         SURF=INT(W1)
         IF(SURF.GT.(INT(sys_last_surf())).OR.SURF.LT.0) THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & 'SURFACE NUMBER BEYOND LEGAL RANGE'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(HEADIN) WRITE(OUTLYNE,2000)
         IF(HEADIN) CALL SHOWIT(0)
         WRITE(OUTLYNE,300) SURF,POLANGX(SURF),POLANGY(SURF)
         CALL SHOWIT(0)
      ELSE
!*********************************************************************
!       SQ=1 AND ONLY ALLOWED WQ BY NOW IS "ALL"
!       HANDEL THE WHOLE LENS
!       PRINT HEADING DATA
         WRITE(OUTLYNE,1000)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,1500)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,2000)
         CALL SHOWIT(0)
         DO SURF=0,INT(sys_last_surf())
            WRITE(OUTLYNE,300) SURF,POLANGX(SURF),POLANGY(SURF)
            CALL SHOWIT(0)
         END DO
      END IF
   ELSE
!     NOT IMAGEDIR
   END IF
!
1000 FORMAT('IMAGE ORIENTATION DATA')
2000 FORMAT('SURF',5X,'X-VECTOR',16X,'Y-VECTOR')
300 FORMAT(I3,1X,G23.15,1X,G23.15)
!
1500 FORMAT(1X)
   RETURN
END
! SUB SREFS.FOR
SUBROUTINE SREFS
!
   use DATLEN
   use DATMAI
   use mod_surface
   use mod_system, only: sys_astop, sys_ref_surf, &
      & sys_set_astop, sys_set_astop_adj, sys_set_ref_orient, sys_set_ref_surf
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SREFS WHICH IMPLEMENTS THE REFS
!       COMMAND AT THE LENS OF UPDATE LENS LEVEL.
!
!
!               CHECK FOR PRESENCE OF STRING,QUALIFIER,OR NUMERIC
!               INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SQ.EQ.1.OR.SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 &
   &.OR.S5.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"REFS" ONLY TAKES NUMERIC WORD #1 INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(W1.LT.0.0D0.OR.W1.GT.360.0D0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'ORIENTATION ANGLE RANGE TO 0.0 TO 360.0 DEGREES'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!
!       CHECK THAT REFS NOT ASSIGNED TO OBJECT SURFACE
!
   IF(DF1.EQ.1) W1=0.0D0
   IF(SURF.EQ.0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'OBJECT SURFACE CAN NOT BE THE REFERENCE SURFACE'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(surf_multi_clap_flag(SURF) /= 0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'THE CURRENT SURFACE HAS MULTIPLE APERTURES ASSIGNED'//'\n'//&
      & 'AND THEREFORE MAY NOT BE SET AS THE REFERENCE SURFACE'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   call sys_set_ref_surf(DBLE(SURF))
   call sys_set_ref_orient(W1)
   NEWREF=INT(sys_ref_surf())
!     IF THE ASTOP IS ON -99 THEN IF REFSURF IS NOT SURFACE 1
!     SET ASTOP SURF TO BE EQUAL TO REF SURF
   IF(sys_astop().LT.0.0D0) THEN
      IF(NEWREF.NE.1) THEN
!     RE-ASSIGN THE ASTOP TO THE REFSURF
         call sys_set_astop_adj(0.0D0)
         call sys_set_astop(sys_ref_surf())
         IF(F6.EQ.1) THEN
            OUTLYNE='ASTOP AUTOMATICALLY SET TO BE ON THE REFERENCE SURFACE'
            CALL SHOWIT(1)
         END IF
      END IF
   END IF
   RETURN
END
! SUB SREF.FOR
SUBROUTINE SREF
!
   use DATLEN
   use DATMAI
   use mod_surface
   use mod_system, only: sys_last_surf, sys_pxim, sys_pxim_fang_set, sys_pyim, &
      & sys_pyim_fang_set, sys_ref_orient, sys_rxim, sys_rxim_fang_set, sys_ryim, &
      & sys_ryim_fang_set, sys_scx, sys_scx_fang, sys_scy, sys_scy_fang, sys_units
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SREF WHICH IMPLEMENTS THE REF
!       COMMAND AT THE CMD LEVEL.
!
   real(real64) VALUE,SYS12,SYS13
!
   CHARACTER UNI1*7
!
!
!               CHECK FOR PRESENCE OF STRING,QUALIFIER,OR NUMERIC
!               INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL('"REF" TAKES NO EXPLICIT INPUT'//'\n'//'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!
   IF(F1.EQ.1) THEN
!
!               WE ARE AT THE CMD LEVEL
!
!       WHAT IS THERE ARE NO SURFACES
      IF(sys_last_surf().EQ.0.0D0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'NO REFERENCE SURFACE EXISTS'//'\n'//&
         & 'LENS SYSTEM HAS NO SURFACES'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!
!       WRITE STATEMENTS GO HERE
!
!*************************************************************
      IF(ABS(surf_clap_type(NEWREF)).GE.1.0D0.AND.&
      &ABS(surf_clap_type(NEWREF)).LE.6.0D0.AND.surf_multi_clap_flag(NEWREF)&
      &.EQ.0.0D0) THEN
!
!       CLAP IS ON REFERENCE SURFACE
!
!       CIRCULAR CLAP
!
         IF(ABS(surf_clap_type(NEWREF)).EQ.1.0D0) THEN
            IF(surf_clap_dim(NEWREF, 1).LE.surf_clap_dim(NEWREF, 2)) THEN
               SYS12=surf_clap_dim(NEWREF, 1)
               SYS13=surf_clap_dim(NEWREF, 1)
            ELSE
               SYS12=surf_clap_dim(NEWREF, 2)
               SYS13=surf_clap_dim(NEWREF, 2)
            END IF
         ELSE
!       NOT CIRCULAR CLAP
         END IF
!        RECT CLAP
!
         IF(ABS(surf_clap_type(NEWREF)).EQ.2.0D0) THEN
            SYS12=surf_clap_dim(NEWREF, 1)
            SYS13=surf_clap_dim(NEWREF, 2)
         ELSE
!       NOT RECT CLAP
         END IF
!        ELIP CLAP
!
         IF(ABS(surf_clap_type(NEWREF)).EQ.3.0D0) THEN
            SYS12=surf_clap_dim(NEWREF, 1)
            SYS13=surf_clap_dim(NEWREF, 2)
         ELSE
!       NOT ELIP CLAP
         END IF
!        RCTK CLAP
!
         IF(ABS(surf_clap_type(NEWREF)).EQ.4.0D0) THEN
            SYS12=surf_clap_dim(NEWREF, 1)
            SYS13=surf_clap_dim(NEWREF, 2)
         ELSE
!       NOT RCTK CLAP
         END IF
!        POLY CLAP
!
         IF(ABS(surf_clap_type(NEWREF)).EQ.5.0D0) THEN
            SYS12=surf_clap_dim(NEWREF, 1)
            SYS13=surf_clap_dim(NEWREF, 1)
         ELSE
!       NOT POLY CLAP
         END IF
!        IPOLY CLAP
!
         IF(ABS(surf_clap_type(NEWREF)).EQ.6.0D0) THEN
            SYS12=surf_clap_dim(NEWREF, 5)
            SYS13=surf_clap_dim(NEWREF, 5)
         ELSE
!       NOT IPOLY CLAP
         END IF
!
      ELSE
!       NO CLAP ON REF SURF.
         SYS13=PXTRAX(1,NEWREF)
         SYS12=PXTRAY(1,NEWREF)
      END IF
!*************************************************************
      WRITE(OUTLYNE,1000)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,2000) sys_scy(),sys_scy_fang(),SYS12,&
      &NEWOBJ,NEWREF,NEWIMG
!
      CALL SHOWIT(0)
      WRITE(OUTLYNE,3000)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,4000)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,2000) sys_scx(),sys_scx_fang(),SYS13,&
      &NEWOBJ,NEWREF,NEWIMG
      CALL SHOWIT(0)
      IF(sys_ref_orient().NE.0.0D0) THEN
         WRITE(OUTLYNE,3000)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,6000) sys_ref_orient()
         CALL SHOWIT(0)
      ELSE
      END IF
   ELSE
   END IF
   IF(sys_pxim_fang_set().NE.0.0D0) THEN
      IF(sys_pxim_fang_set().LT.0.0D0.AND.sys_units().EQ.1.0D0) UNI1='INCHES '
      IF(sys_pxim_fang_set().LT.0.0D0.AND.sys_units().EQ.2.0D0) UNI1='CM     '
      IF(sys_pxim_fang_set().LT.0.0D0.AND.sys_units().EQ.3.0D0) UNI1='MM     '
      IF(sys_pxim_fang_set().LT.0.0D0.AND.sys_units().EQ.4.0D0) UNI1='METERS '
      IF(sys_pxim_fang_set().GT.0.0D0)                        UNI1='DEGREES'
      WRITE(OUTLYNE,3000)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,5000)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,5004) sys_pxim(),UNI1
      CALL SHOWIT(0)
   END IF
   IF(sys_pyim_fang_set().NE.0.0D0) THEN
      IF(sys_pyim_fang_set().LT.0.0D0.AND.sys_units().EQ.1.0D0) UNI1='INCHES '
      IF(sys_pyim_fang_set().LT.0.0D0.AND.sys_units().EQ.2.0D0) UNI1='CM     '
      IF(sys_pyim_fang_set().LT.0.0D0.AND.sys_units().EQ.3.0D0) UNI1='MM     '
      IF(sys_pyim_fang_set().LT.0.0D0.AND.sys_units().EQ.4.0D0) UNI1='METERS '
      IF(sys_pyim_fang_set().GT.0.0D0)                        UNI1='DEGREES'
      WRITE(OUTLYNE,3000)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,5001)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,5005) sys_pyim(),UNI1
      CALL SHOWIT(0)
   END IF
   IF(sys_rxim_fang_set().NE.0.0D0) THEN
      IF(sys_rxim_fang_set().LT.0.0D0.AND.sys_units().EQ.1.0D0) UNI1='INCHES '
      IF(sys_rxim_fang_set().LT.0.0D0.AND.sys_units().EQ.2.0D0) UNI1='CM     '
      IF(sys_rxim_fang_set().LT.0.0D0.AND.sys_units().EQ.3.0D0) UNI1='MM     '
      IF(sys_rxim_fang_set().LT.0.0D0.AND.sys_units().EQ.4.0D0) UNI1='METERS '
      IF(sys_rxim_fang_set().GT.0.0D0)                        UNI1='DEGREES'
      WRITE(OUTLYNE,3000)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,5002)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,5006) sys_rxim(),UNI1
      CALL SHOWIT(0)
   END IF
   IF(sys_ryim_fang_set().NE.0.0D0) THEN
      IF(sys_ryim_fang_set().LT.0.0D0.AND.sys_units().EQ.1.0D0) UNI1='INCHES '
      IF(sys_ryim_fang_set().LT.0.0D0.AND.sys_units().EQ.2.0D0) UNI1='CM     '
      IF(sys_ryim_fang_set().LT.0.0D0.AND.sys_units().EQ.3.0D0) UNI1='MM     '
      IF(sys_ryim_fang_set().LT.0.0D0.AND.sys_units().EQ.4.0D0) UNI1='METERS '
      IF(sys_ryim_fang_set().GT.0.0D0)                        UNI1='DEGREES'
      WRITE(OUTLYNE,3000)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,5003)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,5007) sys_ryim(),UNI1
      CALL SHOWIT(0)
   END IF
1000 FORMAT('REF OBJ Y-HT',20X,'REF AP Y-HT',9X &
   &,'OBJ SF#  REF SF#  IMG SF#')
2000 FORMAT(G13.6,'(',G11.4,1X,'DG)',4X,G11.4,11X,I3,6X,I3,6X,I3)
3000 FORMAT(1X)
4000 FORMAT('REF OBJ X-HT',20X,'REF AP X-HT',9X &
   &,'OBJ SF#  REF SF#  IMG SF#')
6000 FORMAT('ORIENTATION ANGLE = ',G11.4,' DEGREES')
5000 FORMAT('X-REF OBJ. VIA PARAXIAL IMAGE HT/ANGLE SPEC.')
5001 FORMAT('Y-REF OBJ. VIA PARAXIAL IMAGE HT/ANGLE SPEC.')
5002 FORMAT('X-REF OBJ. VIA REAL HT/ANGLE SPEC.')
5003 FORMAT('Y-REF OBJ. VIA REAL HT/ANGLE SPEC.')
5004 FORMAT('PXIM = ',D23.15,1X,A7)
5005 FORMAT('PYIM = ',D23.15,1X,A7)
5006 FORMAT('RXIM = ',D23.15,1X,A7)
5007 FORMAT('RYIM = ',D23.15,1X,A7)
   RETURN
END

! SUB SPCGLS.FOR
! This sub is not called by anything anymore, but I leave it herae
! as I have not fully transferred all the material data to the new
! Special catalog
! JRN 4/20/24

SUBROUTINE SPCGLS(I,MTYPE)
   use DATLEN
   use DATMAI
   use mod_surface
   use mod_system, only: sys_wavelength
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   INTEGER I,MTYPE,J,N
!
   real(real64) X(1:100),Y(1:100),Y2(1:100),LA,CSI &
   &,YPN,YP1,GSCX(1:20),GSCY(1:20),GPCX(1:17),GPCY(1:17)&
   &,SIL,GPLX(1:13),GPL1Y(1:13),GPL2Y(1:13),VIR3X(1:18),VIR3Y(1:18)&
   &,GPL3Y(1:13),GPL4Y(1:13),IRGX(1:25),IRGY(1:25),ZNSEX(1:56)&
   &,ZNSEY(1:56),ZNSX(1:59),ZNSY(1:59),CLRX(1:29),CLRY(1:29)&
   &,SAPH,FSIL,MGF2O,MGF2E,CAF2,MGO,G9754X(1:43),G9754Y(1:43)&
   &,AMTR1X(1:17),AMTR1Y(1:17),DYN,BAF2,KBR,CSBR,KRS5 &
   &,AMTR3X(1:12),AMTR3Y(1:12),AS2S3,ALONX(1:14),ALONY(1:14)&
   &,GAASX(1:13),GAASY(1:13),CDTEX(1:10),CDTEY(1:10)&
   &,SPINX(1:11),SPINY(1:11),CALALX(1:12),CALALY(1:12)&
   &,H2OX(1:21),H2OY(1:21),SUPX(1:60),SUPY(1:60),HOMOX(1:47),&
   &HOMOY(1:47),ZNSMSX(1:59),ZNSMSY(1:59),YAG
!
   real(real64) SIO2OX(1:29),SIO2OY(1:29),SIO2EX(1:20)&
   &,SIO2EY(1:20),LIF,NACLX(1:60),NACLY(1:60)&
   &,B270X(1:10),B270Y(1:10),DIAMOND &
   &,IRG2X(1:18),IRG2Y(1:18),IRG3X(1:17),IRG3Y(1:17),IRGN6X(1:16)&
   &,IRGN6Y(1:16),IRG7X(1:16),IRG7Y(1:16),IRG9X(1:16),IRG9Y(1:16)&
   &,IRG11X(1:16),IRG11Y(1:16),IRG15X(1:16),IRG15Y(1:16)&
   &,DUMA,DUMB,DUML,LM1,A,B,LA1,LA2,LA3,LA4,LA5
!
!
!     VACUME
   LM1(DUMA,DUMB,DUML)=DUMA*(1.0D0+(DUMB/(DUML**2)))
!
!     SILICA/SIO2 INTERPOLATION FORMULA (HANDBOOK OF OPTICS 11/14/2000)
   FSIL(LA)=DSQRT(1.0D0+&
   &((0.6961663*(LA**2))/((LA**2)-((0.0684043)**2)))+&
   &((0.4079426*(LA**2))/((LA**2)-((0.1162414)**2)))+&
   &((0.8974794*(LA**2))/((LA**2)-((9.896161)**2)))&
   &)
!
!
!     DIAMOND INTERPOLATION FORMULA
   DIAMOND(LA)=&
   &2.37837D0+&
   &((1.18897E-2)*(1.0D0/((LA**2)-0.028D0)))+&
   &((-1.0083E-4)*((1.0D0/((LA**2)-0.028D0))**2))+&
   &((-2.3676E-5)*(LA**2))+&
   &((3.24263E-8)*(LA**4))
!
!     LiF INTERPOLATION FORMULA
   LIF(LA)=&
   &1.38761D0+(0.001796D0/((LA**2)-0.028D0))&
   &+(0.000041D0/(((LA**2)-0.028D0)**2))-(0.0023045D0*(LA**2))&
   &-(0.00000557D0*(LA**4))
!
!     SILICON INTERPOLATION FORMULA
   SIL(LA)=&
   &3.41696D0+(0.138497D0/((LA**2)-0.028D0))&
   &+(0.013924D0/(((LA**2)-0.028D0)**2))-(0.0000209D0*(LA**2))&
   &+(0.000000148D0*(LA**4))
!
!     KBr INTERPOLATION FORMULA
   KBR(LA)=DSQRT(&
   &2.361323D0-(3.11497D-4*(LA**2))-(5.8613D-8*(LA**4))+&
   &(0.007676D0/(LA**2))+(0.0156569D0/((LA**2)-0.0324D0)))
!
!     CsBr INTERPOLATION FORMULA
   CSBR(LA)=DSQRT(&
   &5.640752D0-(3.338D-6*(LA**2))+&
   &(0.0018612D0/(LA**2))+(41110.49D0/((LA**2)-14390.4D0))&
   &+(0.0290764D0/((LA**2)-0.024964D0)))
!
!     MGO INTERPOLATION FORMULA
   MGO(LA)=DSQRT(&
   &2.956362D0-(0.01062387D0*(LA**2))-(0.0000204968D0*(LA**4))-&
   &(0.02195770/((LA**2)-0.01428322D0)))
!
!     DYNASIL INTERPOLATION FORMULA
   DYN(LA)=DSQRT(&
   &((0.6961663D0*(LA**2))/((LA**2)-((0.0684043D0)**2)))+&
   &((0.4079426D0*(LA**2))/((LA**2)-((0.1162414D0)**2)))+&
   &((0.8974794D0*(LA**2))/((LA**2)-((9.896161D0)**2)))+1.0D0)
!
!     SAPPHIRE INTERPOLATION FORMULA
   SAPH(LA)=DSQRT(&
   &((1.023798D0*(LA**2))/((LA**2)-(0.00377588D0)))+&
   &((1.058264D0*(LA**2))/((LA**2)-(0.01225440D0)))+&
   &((5.280792D0*(LA**2))/((LA**2)-(321.361600D0)))+1.0D0)
!
!     YAG INTERPOLATION FORMULA
   YAG(LA)=DSQRT(&
   &((2.293D0*(LA**2))/((LA**2)-(0.1095D0**2)))+&
   &((3.705D0*(LA**2))/((LA**2)-(17.825D0**2)))+1.0D0)
!
!     CSI INTERPOLATION FORMULA
   CSI(LA)=DSQRT(&
   &((0.34617251D0*(LA**2))/((LA**2)-0.00052701D0))+&
   &((1.0080886D0*(LA**2))/((LA**2)-0.02149156D0))+&
   &((0.28551800D0*(LA**2))/((LA**2)-0.032761D0))+&
   &((0.39743178D0*(LA**2))/((LA**2)-0.044944D0))+&
   &((3.3605359D0*(LA**2))/((LA**2)-25621.0D0))+1.0D0)
!
!     KRS5 INTERPOLATION FORMULA
   KRS5(LA)=DSQRT(&
   &((1.8293958D0*(LA**2))/((LA**2)-(0.0225D0**2)))+&
   &((1.6675593D0*(LA**2))/((LA**2)-(0.0625D0**2)))+&
   &((1.1210424D0*(LA**2))/((LA**2)-(0.1225D0**2)))+&
   &((0.04513366D0*(LA**2))/((LA**2)-(0.2025D0**2)))+&
   &((12.380234D0*(LA**2))/((LA**2)-(27089.737D0**2)))+1.0D0)
!
!     As2S3 INTERPOLATION FORMULA
   AS2S3(LA)=DSQRT(&
   &((1.8983678D0*(LA**2))/((LA**2)-(0.15D0**2)))+&
   &((1.9222979D0*(LA**2))/((LA**2)-(0.25D0**2)))+&
   &((0.8765134D0*(LA**2))/((LA**2)-(0.350D0**2)))+&
   &((0.1188704D0*(LA**2))/((LA**2)-(0.450D0**2)))+&
   &((0.9569903D0*(LA**2))/((LA**2)-(27.3861D0**2)))+1.0D0)
!
!     BAF2 INTERPOLATION FORMULA
   BAF2(LA)=DSQRT(&
   &((0.643356D0*(LA**2))/((LA**2)-((0.057789D0)**2)))+&
   &((0.506762D0*(LA**2))/((LA**2)-((0.10968D0)**2)))+&
   &((3.8261D0*(LA**2))/((LA**2)-((46.3864D0)**2)))+1.0D0)
!
!     CAF2 INTERPOLATION FORMULA
   CAF2(LA)=DSQRT(&
   &((0.5675888D0*(LA**2))/((LA**2)-((0.050263605D0)**2)))+&
   &((0.4710914D0*(LA**2))/((LA**2)-((0.1003909D0)**2)))+&
   &((3.8484723D0*(LA**2))/((LA**2)-((34.649040D0)**2)))+1.0D0)
!
!     MGF2o INTERPOLATION FORMULA
   MGF2O(LA)=DSQRT(&
   &1.0D0+&
   &((.48755108D0*(LA**2))/((LA**2)-(0.04338408D0**2)))&
   &+((.39875031D0*(LA**2))/((LA**2)-(0.09461442D0**2)))&
   &+((2.3120353D0*(LA**2))/((LA**2)-(23.7936040D0**2)))&
   &)
!
!     MGF2E INTERPOLATION FORMULA
   MGF2E(LA)=DSQRT(&
   &1.0D0+&
   &((.41344023D0*(LA**2))/((LA**2)-(0.03684262D0**2)))&
   &+((.50497499D0*(LA**2))/((LA**2)-(0.09076162D0**2)))&
   &+((2.4904862D0*(LA**2))/((LA**2)-(12.771995D0**2)))&
   &)
!
!     THIS SUBROUTINE IS USED TO CALCULATE REFRACTIVE
!     INDICES OF THE MATERIALS WHOSE NAMES ARE THEIR COMMANDS
!
   IF(MTYPE.EQ.1) THEN
!     INTERPOLATE THE SINGLE CRYSTALLINE GERMANIUN DATA
      N=20
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR SINGLE CRYSTAL GERMANIUM
!
      DATA (GSCX(J), J = 1,20)/2.0581D0,2.1526D0,2.3126D0,2.4374D0,&
      &2.577D0,&
      &2.7144D0,2.998D0,3.3033D0,3.4188D0,4.258D0,4.866D0,6.238D0,&
      &8.66D0,9.72D0,11.04D0,12.20D0,13.02D0,14.21D0,15.08D0,16.0D0/
!
      DATA (GSCY(J), J = 1,20)/&
      &4.1016D0,4.0919D0,4.0786D0,4.0708D0,&
      &4.0609D0,4.0552D0,4.0452D0,4.0369D0,4.0334D0,&
      &4.0216D0,4.0170D0,4.0094D0,4.0043D0,4.0034D0,&
      &4.0026D0,4.0023D0,4.0021D0,4.0015D0,4.0014D0,4.0012D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=GSCX(1:N)
      Y(1:N)=GSCY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.2) THEN
!     INTERPOLATE THE POLY CRYSTALLINE GERMANIUN DATA
      N=17
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR POLY CRYSTAL GERMANIUM
!
      DATA (GPCX(J), J = 1,17)/2.0581D0,2.1526D0,2.3126D0,2.4374D0,&
      &2.577D0,&
      &2.7144D0,2.998D0,3.3033D0,3.4188D0,4.258D0,4.866D0,6.238D0,&
      &8.66D0,9.72D0,11.04D0,12.20D0,13.02D0/
!
      DATA (GPCY(J), J = 1,17)/4.1018D0,4.0919D0,4.0785D0,4.0709D0,&
      &4.0608D0,4.0554D0,4.0452D0,4.0372D0,4.0339D0,&
      &4.0217D0,4.0167D0,4.0095D0,4.0043D0,4.0033D0,&
      &4.0025D0,4.0020D0,4.0018D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=GPCX(1:N)
      Y(1:N)=GPCY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.3) THEN
!     INTERPOLATE THE SILICON DATA
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 1, SIL(sys_wavelength(1)))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 2, SIL(sys_wavelength(2)))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 3, SIL(sys_wavelength(3)))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 4, SIL(sys_wavelength(4)))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 5, SIL(sys_wavelength(5)))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 6, SIL(sys_wavelength(6)))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 7, SIL(sys_wavelength(7)))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 8, SIL(sys_wavelength(8)))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 9, SIL(sys_wavelength(9)))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 10, SIL(sys_wavelength(10)))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
   END IF
   IF(MTYPE.EQ.4) THEN
!     INTERPOLATE THE IRG100 DATA
      N=25
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR IRG100
!
      DATA (IRGX(J), J = 1,25)/1.0D0,1.5D0,2.0D0,2.5D0,3.0D0,3.5D0 &
      &,4.0D0,4.5D0,5.0D0,5.5D0,6.0D0,6.5D0,7.0D0,7.5D0,8.0D0,8.5D0 &
      &,9.0D0,9.5D0,10.0D0,10.5D0,11.0D0,11.5D0,12.0D0,13.0D0,14.0D0/
!
      DATA (IRGY(J), J = 1,25)/2.7235D0,2.6577D0,2.6404D0,2.6314D0 &
      &,2.6262D0,2.6227D0,2.6201D0,2.6181D0,2.6164D0,2.6148D0,2.6133D0 &
      &,2.6118D0,2.6103D0,2.6088D0,2.6072D0,2.6056D0,2.6039D0,2.6022D0 &
      &,2.6004D0,2.5985D0,2.5966D0,2.5946D0,2.5925D0,2.5880D0,2.5832D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=IRGX(1:N)
      Y(1:N)=IRGY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.5) THEN
!     INTERPOLATE THE ZNSE DATA
      N=56
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR ZNSE
!
      DATA (ZNSEX(J), J = 1,56)/0.54D0,0.58D0 &
      &,0.62D0,0.66D0,0.70D0,0.74D0,0.78D0,0.82D0,0.86D0,0.90D0 &
      &,0.94D0,0.98D0,1.0D0,1.4D0,1.8D0,2.2D0,2.6D0,3.0D0,3.4D0 &
      &,3.8D0,4.2D0,4.6D0,5.0D0,5.4D0,5.8D0,6.2D0,6.6D0,7.0D0,7.4D0 &
      &,7.8D0,8.2D0,8.6D0,9.0D0,9.4D0,9.8D0,10.2D0,10.6D0,11.0D0 &
      &,11.4D0,11.8D0,12.2D0,12.6D0,13.0D0,13.4D0,13.8D0,14.2D0,14.6D0 &
      &,15.0D0,15.4D0,15.8D0,16.2D0,16.6D0,17.0D0,17.4D0,17.8D0,18.2D0/
!
      DATA (ZNSEY(J), J = 1,56)/2.6754D0,2.6312D0,2.5994D0,2.5755D0 &
      &,2.5568D0,2.5418D0,2.5295D0,2.5193D0,2.5107D0,2.5034D0,2.4971D0 &
      &,2.4916D0,2.4892D0,2.4609D0,2.4496D0,2.4437D0,2.4401D0,2.4376D0 &
      &,2.4356D0,2.4339D0,2.4324D0,2.4309D0,2.4295D0,2.4281D0,2.4266D0 &
      &,2.4251D0,2.4235D0,2.4218D0,2.4201D0,2.4183D0,2.4163D0,2.4143D0 &
      &,2.4122D0,2.4100D0,2.4077D0,2.4053D0,2.4028D0,2.4001D0,2.3974D0 &
      &,2.3945D0,2.3915D0,2.3883D0,2.3850D0,2.3816D0,2.3781D0,2.3744D0 &
      &,2.3705D0,2.3665D0,2.3623D0,2.3579D0,2.3534D0,2.3487D0,2.3438D0 &
      &,2.3387D0,2.3333D0,2.3278D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=ZNSEX(1:N)
      Y(1:N)=ZNSEY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.6) THEN
!     INTERPOLATE THE ZNS DATA
      N=59
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR ZNS
!
      DATA (ZNSX(J), J = 1,59)/0.42D0,0.46D0,0.5D0,0.54D0,0.58D0 &
      &,0.62D0,0.66D0,0.70D0,0.74D0,0.78D0,0.82D0,0.86D0,0.90D0 &
      &,0.94D0,0.98D0,1.0D0,1.4D0,1.8D0,2.2D0,2.6D0,3.0D0,3.4D0 &
      &,3.8D0,4.2D0,4.6D0,5.0D0,5.4D0,5.8D0,6.2D0,6.6D0,7.0D0,7.4D0 &
      &,7.8D0,8.2D0,8.6D0,9.0D0,9.4D0,9.8D0,10.2D0,10.6D0,11.0D0 &
      &,11.4D0,11.8D0,12.2D0,12.6D0,13.0D0,13.4D0,13.8D0,14.2D0,14.6D0 &
      &,15.0D0,15.4D0,15.8D0,16.2D0,16.6D0,17.0D0,17.4D0,17.8D0,18.2D0/
!
      DATA (ZNSY(J), J = 1,59)/2.516D0,2.458D0,2.419D0,2.391D0,2.371D0 &
      &,2.355D0,2.342D0,2.332D0,2.323D0,2.316D0,2.310D0,2.305D0,2.301D0 &
      &,2.297D0,2.294D0,2.292D0,2.275D0,2.267D0,2.263D0,2.260D0,2.257D0 &
      &,2.255D0,2.253D0,2.251D0,2.248D0,2.246D0,2.244D0,2.241D0,2.238D0 &
      &,2.235D0,2.232D0,2.228D0,2.225D0,2.221D0,2.217D0,2.212D0,2.208D0 &
      &,2.203D0,2.198D0,2.192D0,2.186D0,2.180D0,2.173D0,2.167D0,2.159D0 &
      &,2.152D0,2.143D0,2.135D0,2.126D0,2.116D0,2.106D0,2.095D0,2.084D0 &
      &,2.072D0,2.059D0,2.045D0,2.030D0,2.015D0,1.998D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=ZNSX(1:N)
      Y(1:N)=ZNSY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.7) THEN
!     INTERPOLATE THE CLRTRAN DATA
      N=29
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR CLRTRAN
!
      DATA (CLRX(J), J = 1,29)/0.4047D0,0.4358D0,0.4678D0,0.48D0 &
      &,0.5086D0,0.5461D0,0.5876D0,0.6438D0,0.6678D0,0.7065D0,0.78D0 &
      &,0.7948D0,0.8521D0,0.8943D0,1.014D0,1.1287D0,1.5296D0,2.0581D0 &
      &,3.0D0,3.5D0,4.0D0,4.5D0,5.0D0,8.0D0,9.0D0,10.0D0,11.25D0,12.0D0 &
      &,13.0D0/
!
      DATA (CLRY(J), J = 1,29)/2.54515D0,2.48918D0,2.44915D0,2.43691D0 &
      &,2.41279D0,2.38838D0,2.36789D0,2.34731D0,2.34033D0,2.33073D0 &
      &,2.31669D0,2.31438D0,2.30659D0,2.30183D0,2.29165D0,2.28485D0 &
      &,2.27191D0,2.26442D0,2.25772D0,2.25498D0,2.25231D0,2.24955D0 &
      &,2.24661D0,2.22334D0,2.21290D0,2.20084D0,2.18317D0,2.17101D0 &
      &,2.15252D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=CLRX(1:N)
      Y(1:N)=CLRY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.8) THEN
!     INTERPOLATE THE WILLOW RUN FUSED SILICA/SIO2
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 1, FSIL(sys_wavelength(1)))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 2, FSIL(sys_wavelength(2)))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 3, FSIL(sys_wavelength(3)))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 4, FSIL(sys_wavelength(4)))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 5, FSIL(sys_wavelength(5)))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 6, FSIL(sys_wavelength(6)))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 7, FSIL(sys_wavelength(7)))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 8, FSIL(sys_wavelength(8)))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 9, FSIL(sys_wavelength(9)))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 10, FSIL(sys_wavelength(10)))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
   END IF
   IF(MTYPE.EQ.9) THEN
!     INTERPOLATE SAPPHIRE
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 1, SAPH(sys_wavelength(1)))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 2, SAPH(sys_wavelength(2)))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 3, SAPH(sys_wavelength(3)))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 4, SAPH(sys_wavelength(4)))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 5, SAPH(sys_wavelength(5)))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 6, SAPH(sys_wavelength(6)))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 7, SAPH(sys_wavelength(7)))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 8, SAPH(sys_wavelength(8)))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 9, SAPH(sys_wavelength(9)))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 10, SAPH(sys_wavelength(10)))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.10) THEN
!     INTERPOLATE DYNASIL
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 1, DYN(sys_wavelength(1)))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 2, DYN(sys_wavelength(2)))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 3, DYN(sys_wavelength(3)))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 4, DYN(sys_wavelength(4)))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 5, DYN(sys_wavelength(5)))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 6, DYN(sys_wavelength(6)))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 7, DYN(sys_wavelength(7)))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 8, DYN(sys_wavelength(8)))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 9, DYN(sys_wavelength(9)))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 10, DYN(sys_wavelength(10)))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.11) THEN
!     INTERPOLATE THE AMTIR1
      N=17
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR AMTIR1
!
      DATA (AMTR1X(J), J = 1,17)/1.0D0,1.064D0,1.5D0,2.0D0,2.4D0 &
      &,3.0D0,4.0D0,5.0D0,6.0D0,7.0D0,8.0D0,9.0D0,10.0D0,11.0D0 &
      &,12.0D0,13.0D0,14.0D0/
!
      DATA (AMTR1Y(J), J = 1,17)/2.6055D0,2.5933D0,2.5469D0,2.5310D0 &
      &,2.5250D0,2.5184D0,2.5146D0,2.5112D0,2.5086D0,2.5062D0,2.5036D0 &
      &,2.5008D0,2.4977D0,2.4942D0,2.4902D0,2.4862D0,2.4825D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=AMTR1X(1:N)
      Y(1:N)=AMTR1Y(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.12) THEN
!     INTERPOLATE THE AMTIR3
      N=12
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR AMTIR3
!
      DATA (AMTR3X(J), J = 1,12)/3.0D0,4.0D0,5.0D0,6.0D0,7.0D0,8.0D0 &
      &,9.0D0,10.0D0,11.0D0,12.0D0,13.0D0,14.0D0/
!
      DATA (AMTR3Y(J), J = 1,12)/2.6266D0,2.6210D0,2.6173D0,2.6142D0 &
      &,2.6117D0,2.6088D0,2.6055D0,2.6023D0,2.5983D0,2.5942D0,2.5892D0 &
      &,2.5843D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=AMTR3X(1:N)
      Y(1:N)=AMTR3Y(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.13) THEN
!     INTERPOLATE THE As2S3
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 1, AS2S3(sys_wavelength(1)))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 2, AS2S3(sys_wavelength(2)))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 3, AS2S3(sys_wavelength(3)))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 4, AS2S3(sys_wavelength(4)))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 5, AS2S3(sys_wavelength(5)))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 6, AS2S3(sys_wavelength(6)))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 7, AS2S3(sys_wavelength(7)))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 8, AS2S3(sys_wavelength(8)))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 9, AS2S3(sys_wavelength(9)))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 10, AS2S3(sys_wavelength(10)))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.14) THEN
!     INTERPOLATE THE GaAs
      N=13
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR GaAs
!
      DATA (GAASX(J), J = 1,13)/2.5D0,3.0D0,4.0D0,5.0D0,6.0D0 &
      &,7.0D0,8.0D0,9.0D0,10.0D0,11.0D0,12.0D0,13.0D0,14.0D0/
!
      DATA (GAASY(J), J = 1,13)/3.3256D0,3.3169D0,3.3069D0 &
      &,3.3010,3.2963D0,3.2923D0,3.2878D0,3.2830D0,3.2778D0 &
      &,3.2725D0,3.2666D0,3.2589D0,3.2509D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=GAASX(1:N)
      Y(1:N)=GAASY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.15) THEN
!     INTERPOLATE THE CdTe
      N=10
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR CdTe
!
      DATA (CDTEX(J), J = 1,10)/3.0D0,4.0D0,5.0D0,6.0D0,7.0D0 &
      &,8.0D0,9.0D0,10.0D0,11.0D0,12.0D0/
!
      DATA (CDTEY(J), J = 1,10)/2.7026D0,2.6971D0,2.6922D0 &
      &,2.6886D0,2.6865D0,2.6846D0,2.6825D0,2.6797D0,2.6766D0 &
      &,2.6749D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=CDTEX(1:N)
      Y(1:N)=CDTEY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.16) THEN
!     INTERPOLATE MGF2O
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 1, MGF2O(sys_wavelength(1)))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 2, MGF2O(sys_wavelength(2)))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 3, MGF2O(sys_wavelength(3)))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 4, MGF2O(sys_wavelength(4)))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 5, MGF2O(sys_wavelength(5)))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 6, MGF2O(sys_wavelength(6)))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 7, MGF2O(sys_wavelength(7)))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 8, MGF2O(sys_wavelength(8)))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 9, MGF2O(sys_wavelength(9)))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 10, MGF2O(sys_wavelength(10)))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.17) THEN
!     INTERPOLATE MGF2E
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 1, MGF2E(sys_wavelength(1)))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 2, MGF2E(sys_wavelength(2)))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 3, MGF2E(sys_wavelength(3)))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 4, MGF2E(sys_wavelength(4)))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 5, MGF2E(sys_wavelength(5)))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 6, MGF2E(sys_wavelength(6)))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 7, MGF2E(sys_wavelength(7)))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 8, MGF2E(sys_wavelength(8)))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 9, MGF2E(sys_wavelength(9)))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 10, MGF2E(sys_wavelength(10)))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.18) THEN
!     INTERPOLATE CAF2
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 1, CAF2(sys_wavelength(1)))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 2, CAF2(sys_wavelength(2)))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 3, CAF2(sys_wavelength(3)))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 4, CAF2(sys_wavelength(4)))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 5, CAF2(sys_wavelength(5)))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 6, CAF2(sys_wavelength(6)))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 7, CAF2(sys_wavelength(7)))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 8, CAF2(sys_wavelength(8)))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 9, CAF2(sys_wavelength(9)))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 10, CAF2(sys_wavelength(10)))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.19) THEN
!     INTERPOLATE MGO
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 1, MGO(sys_wavelength(1)))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 2, MGO(sys_wavelength(2)))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 3, MGO(sys_wavelength(3)))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 4, MGO(sys_wavelength(4)))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 5, MGO(sys_wavelength(5)))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 6, MGO(sys_wavelength(6)))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 7, MGO(sys_wavelength(7)))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 8, MGO(sys_wavelength(8)))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 9, MGO(sys_wavelength(9)))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 10, MGO(sys_wavelength(10)))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.20) THEN
!     INTERPOLATE BAF2
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 1, BAF2(sys_wavelength(1)))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 2, BAF2(sys_wavelength(2)))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 3, BAF2(sys_wavelength(3)))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 4, BAF2(sys_wavelength(4)))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 5, BAF2(sys_wavelength(5)))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 6, BAF2(sys_wavelength(6)))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 7, BAF2(sys_wavelength(7)))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 8, BAF2(sys_wavelength(8)))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 9, BAF2(sys_wavelength(9)))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 10, BAF2(sys_wavelength(10)))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.21) THEN
!     INTERPOLATE KBR
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 1, KBR(sys_wavelength(1)))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 2, KBR(sys_wavelength(2)))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 3, KBR(sys_wavelength(3)))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 4, KBR(sys_wavelength(4)))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 5, KBR(sys_wavelength(5)))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 6, KBR(sys_wavelength(6)))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 7, KBR(sys_wavelength(7)))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 8, KBR(sys_wavelength(8)))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 9, KBR(sys_wavelength(9)))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 10, KBR(sys_wavelength(10)))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.22) THEN
!     INTERPOLATE CSI
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 1, CSI(sys_wavelength(1)))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 2, CSI(sys_wavelength(2)))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 3, CSI(sys_wavelength(3)))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 4, CSI(sys_wavelength(4)))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 5, CSI(sys_wavelength(5)))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 6, CSI(sys_wavelength(6)))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 7, CSI(sys_wavelength(7)))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 8, CSI(sys_wavelength(8)))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 9, CSI(sys_wavelength(9)))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 10, CSI(sys_wavelength(10)))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.23) THEN
!     INTERPOLATE CSBR
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 1, CSBR(sys_wavelength(1)))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 2, CSBR(sys_wavelength(2)))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 3, CSBR(sys_wavelength(3)))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 4, CSBR(sys_wavelength(4)))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 5, CSBR(sys_wavelength(5)))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 6, CSBR(sys_wavelength(6)))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 7, CSBR(sys_wavelength(7)))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 8, CSBR(sys_wavelength(8)))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 9, CSBR(sys_wavelength(9)))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 10, CSBR(sys_wavelength(10)))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.24) THEN
!     INTERPOLATE KRS5
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 1, KRS5(sys_wavelength(1)))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 2, KRS5(sys_wavelength(2)))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 3, KRS5(sys_wavelength(3)))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 4, KRS5(sys_wavelength(4)))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 5, KRS5(sys_wavelength(5)))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 6, KRS5(sys_wavelength(6)))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 7, KRS5(sys_wavelength(7)))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 8, KRS5(sys_wavelength(8)))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 9, KRS5(sys_wavelength(9)))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 10, KRS5(sys_wavelength(10)))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.25) THEN
!     INTERPOLATE THE LiF
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 1, LIF(sys_wavelength(1)))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 2, LIF(sys_wavelength(2)))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 3, LIF(sys_wavelength(3)))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 4, LIF(sys_wavelength(4)))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 5, LIF(sys_wavelength(5)))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 6, LIF(sys_wavelength(6)))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 7, LIF(sys_wavelength(7)))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 8, LIF(sys_wavelength(8)))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 9, LIF(sys_wavelength(9)))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 10, LIF(sys_wavelength(10)))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
   END IF
   IF(MTYPE.EQ.26) THEN
!     INTERPOLATE THE NaCl
      N=60
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR NaCl
!
      DATA (NACLX(J), J = 1,60)/0.589D0,0.64D0,0.6874D0,0.7604D0 &
      &,0.7858D0,0.8835D0,0.9033D0,0.9724D0,1.0084D0,1.0540D0 &
      &,1.0810D0,1.1058D0,1.1420D0,1.1786D0,1.2016D0,1.2604D0 &
      &,1.3126D0,1.4874D0,1.5552D0,1.6368D0,1.6848D0,1.7670D0 &
      &,2.0736D0,2.1824D0,2.2464D0,2.3560D0,2.6505D0,2.9466D0 &
      &,3.2736D0,3.5359D0,3.6288D0,3.8192D0,4.1230D0,4.7120D0 &
      &,5.0092D0,5.3009D0,5.8932D0,6.4825D0,6.8000D0,7.0718D0 &
      &,7.2200D0,7.5900D0,7.6611D0,7.9558D0,8.0400D0,8.8398D0 &
      &,9.0000D0,9.5000D0,10.0184D0,11.7864D0,12.5D0,12.9650D0 &
      &,13.50D0,14.1436D0,14.7330D0,15.3223D0,15.9116D0,17.93D0 &
      &,20.57D0,22.3D0/
!
      DATA (NACLY(J), J = 1,60)/1.54427D0,1.54141D0,1.53930D0 &
      &,1.53682D0,1.53607D0,1.53395D0,1.53361D0,1.53253D0,1.53206D0 &
      &,1.53153D0,1.53123D0,1.53098D0,1.53063D0,1.53031D0,1.53014D0 &
      &,1.52971D0,1.52937D0,1.52845D0,1.52815D0,1.52781D0,1.52764D0 &
      &,1.52736D0,1.52649D0,1.52621D0,1.52606D0,1.52579D0,1.52512D0 &
      &,1.52466D0,1.52371D0,1.52312D0,1.52286D0,1.52238D0,1.52156D0 &
      &,1.51979D0,1.51883D0,1.51790D0,1.51593D0,1.51347D0,1.51200D0 &
      &,1.51093D0,1.51020D0,1.50850D0,1.50822D0,1.50665D0,1.50640D0 &
      &,1.50192D0,1.50100D0,1.49980D0,1.49462D0,1.48171D0,1.47568D0 &
      &,1.47160D0,1.46660D0,1.46044D0,1.45427D0,1.44743D0,1.44090D0 &
      &,1.41490D0,1.37350D0,1.34030D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=NACLX(1:N)
      Y(1:N)=NACLY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.27) THEN
!     INTERPOLATE THE SiO2o
      N=29
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR SiO2o
!
      DATA (SIO2OX(J), J = 1,29)/0.185D0,0.198D0,0.231D0,0.34D0 &
      &,0.394D0,0.434D0,0.508D0,0.5893D0,0.768D0,0.8325D0,0.9914D0 &
      &,1.1592D0,1.3070D0,1.3958D0,1.4792D0,1.5414D0,1.6815D0 &
      &,1.7614D0,1.9457D0,2.0531D0,2.3D0,2.6D0,3.0D0,3.5D0,4.0D0 &
      &,4.2D0,5.0D0,6.45D0,7.0D0/
!
      DATA (SIO2OY(J), J = 1,29)/1.65751D0,1.65087D0,1.61395D0 &
      &,1.56747D0,1.55846D0,1.55396D0,1.54822D0,1.54424D0,1.53903D0 &
      &,1.53773D0,1.53514D0,1.53283D0,1.53090D0,1.52877D0,1.52865D0 &
      &,1.52781D0,1.52583D0,1.52468D0,1.52184D0,1.52005D0,1.51561D0 &
      &,1.50986D0,1.49953D0,1.48451D0,1.46617D0,1.4569D0,1.417D0,1.274D0 &
      &,1.167D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=SIO2OX(1:N)
      Y(1:N)=SIO2OY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.28) THEN
!     INTERPOLATE THE SiO2e
      N=20
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR SiO2e
!
      DATA (SIO2EX(J), J = 1,20)/0.185D0,0.198D0,0.231D0,0.34D0 &
      &,0.394D0,0.434D0,0.508D0,0.5893D0,0.768D0,0.8325D0,0.9914D0 &
      &,1.1592D0,1.3070D0,1.3958D0,1.4792D0,1.5414D0,1.6815D0 &
      &,1.7614D0,1.9457D0,2.0531D0/
!
      DATA (SIO2EY(J), J = 1,20)/1.68988D0,1.66394D0,1.62555D0 &
      &,1.57737D0,1.56805D0,1.56339D0,1.55746D0,1.55335D0,1.54794D0 &
      &,1.54661D0,1.54392D0,1.54152D0,1.53951D0,1.53832D0,1.53716D0 &
      &,1.53630D0,1.53422D0,1.53301D0,1.53004D0,1.52823D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=SIO2EX(1:N)
      Y(1:N)=SIO2EY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.29) THEN
!     INTERPOLATE THE VIR3
      N=18
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR VIR3
!
      DATA (VIR3X(J), J = 1,18)/0.4047D0,0.5461D0,0.7065D0,1.0D0 &
      &,1.0D0,2.0D0,2.5D0,3.0D0,3.25D0,3.5D0,3.75D0,4.0D0,4.25D0 &
      &,4.5D0,4.75D0,5.0D0,5.5D0,6.0D0/
!
      DATA (VIR3Y(J), J = 1,18)/1.92568D0,1.87002D0,1.84694D0 &
      &,1.831D0,1.818D0,1.812D0,1.806D0,1.799D0,1.795D0,1.791D0 &
      &,1.786D0,1.781D0,1.775D0,1.769D0,1.762D0,1.756D0,1.741D0,1.725D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=VIR3X(1:N)
      Y(1:N)=VIR3Y(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.30) THEN
!     INTERPOLATE THE 9754
      N=43
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR 9754
!
      DATA (G9754X(J), J = 1,43)/0.4D0,0.425D0,0.45D0 &
      &,0.475D0,0.5D0,0.525D0 &
      &,0.55D0,0.575D0,0.6D0,0.635D0,0.65D0,0.675D0,0.7D0,0.725D0,0.75D0 &
      &,0.775D0,0.8D0,0.825D0,0.85D0,0.875D0,0.9D0,0.925D0,0.95D0 &
      &,0.975D0,1.0D0,1.25D0,1.5D0,1.75D0,2.0D0,2.25D0,2.5D0,2.75D0 &
      &,3.0D0,3.25D0,3.5D0,3.75D0,4.0D0,4.25D0,4.5D0,4.75D0,5.0D0 &
      &,5.25D0,5.5D0/
!
      DATA (G9754Y(J), J = 1,43)/1.69093D0,1.68502D0,1.68020D0 &
      &,1.67621D0,1.67285D0,1.67000D0,1.66754D0,1.66542D0,1.66356D0 &
      &,1.66192D0,1.66046D0,1.65916D0,1.65800D0,1.65694D0,1.65599D0 &
      &,1.65511D0,1.65431D0,1.65358D0,1.65289D0,1.65226D0,1.65167D0 &
      &,1.65112D0,1.65060D0,1.65011D0,1.64964D0,1.64595D0,1.64310D0 &
      &,1.64049D0,1.63785D0,1.63505D0,1.63203D0,1.62874D0,1.62514D0 &
      &,1.62119D0,1.61686D0,1.61214D0,1.60698D0,1.60135D0,1.59521D0 &
      &,1.58853D0,1.58125D0,1.57332D0,1.66469D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=G9754X(1:N)
      Y(1:N)=G9754Y(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.31) THEN
!     INTERPOLATE THE ALON
      N=14
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR ALON
!
      DATA (ALONX(J), J = 1,14)/0.365D0,0.405D0,0.435D0,0.546D0 &
      &,0.852D0,1.014D0,1.53D0,1.97D0,2.325D0,2.8D0,3.39D0,4.0D0 &
      &,4.6D0,5.0D0/
!
      DATA (ALONY(J), J = 1,14)/1.819D0,1.811D0,1.806D0,1.792D0 &
      &,1.778D0,1.773D0,1.765D0,1.758D0,1.752D0,1.743D0,1.729D0 &
      &,1.710D0,1.689D0,1.672D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=ALONX(1:N)
      Y(1:N)=ALONY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.32) THEN
!     INTERPOLATE THE SPINEL
      N=11
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR SPINEL
!
      DATA (SPINX(J), J = 1,11)/0.4047D0,0.4358D0,0.5461D0,0.8521D0 &
      &,1.014D0,1.53D0,1.97D0,3.0D0,4.0D0,5.0D0,5.5D0/
!
      DATA (SPINY(J), J = 1,11)/1.73574D0,1.73054D0,1.71896D0 &
      &,1.70728D0,1.703D0,1.69468D0,1.68763D0,1.6647D0,1.6414D0 &
      &,1.5978D0,1.5719D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=SPINX(1:N)
      Y(1:N)=SPINY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.33) THEN
!     INTERPOLATE THE CALCIUM ALUMINATE GLASS
      N=12
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR CALAL
!
      DATA (CALALX(J), J = 1,12)/0.4861D0,0.5893D0,0.6563D0,0.8D0 &
      &,1.0D0,1.5D0,2.0D0,2.5D0,3.0D0,3.5D0,4.0D0,4.5D0/
!
      DATA (CALALY(J), J = 1,12)/1.6794D0,1.669D0,1.6647D0,1.6588D0 &
      &,1.6538D0,1.6463D0,1.6403D0,1.6341D0,1.6266D0,1.6180D0 &
      &,1.6074D0,1.5952D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=CALALX(1:N)
      Y(1:N)=CALALY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.34) THEN
!     INTERPOLATE THE B270 GLASS
      N=8
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR B270
!
      DATA (B270X(J), J = 1,8)/&
      &0.4358343D0,0.4799914D0,0.4861327D0 &
      &,0.5460740D0,0.5875618D0,0.5892938D0,0.6438469D0,0.6562725D0/
!
      DATA (B270Y(J), J = 1,8)/&
      &1.534D0,1.5297D0,1.5292D0,1.5251D0 &
      &,1.523D0,1.5229D0,1.5207D0,1.5202D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=B270X(1:N)
      Y(1:N)=B270Y(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.35) THEN
!     INTERPOLATE THE IRG2 GLASS
      N=18
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR IRG2
!
      DATA (IRG2X(J), J = 1,18)/0.365D0,0.4047D0,0.48D0,0.4861D0 &
      &,0.5461D0,0.5876D0,0.6438D0,0.6563D0,0.7065D0,0.8521D0,1.014D0 &
      &,1.5296D0,1.9701D0,2.3254D0,2.674D0,3.303D0,4.258D0,4.586D0/
!
      DATA (IRG2Y(J), J = 1,18)/1.9750D0,1.9462D0,1.9147D0,1.9129D0 &
      &,1.8988D0,1.8918D0,1.8845D0,1.8832D0,1.8785D0,1.8692D0,1.8630D0 &
      &,1.8526D0,1.8464D0,1.8414D0,1.8362D0,1.8253D0,1.8041D0,1.7954D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=IRG2X(1:N)
      Y(1:N)=IRG2Y(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.36) THEN
!     INTERPOLATE THE IRG3 GLASS
      N=17
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR IRG3
!
      DATA (IRG3X(J), J = 1,17)/0.4047D0,0.48D0,0.4861D0 &
      &,0.5461D0,0.5876D0,0.6438D0,0.6563D0,0.7065D0,0.8521D0,1.014D0 &
      &,1.5296D0,1.9701D0,2.3254D0,2.674D0,3.303D0,4.258D0,4.586D0/
!
      DATA (IRG3Y(J), J = 1,17)/1.8925D0,1.8649D0,1.8633D0,1.8510D0 &
      &,1.8449D0,1.8385D0,1.8373D0,1.8331D0,1.8249D0,1.8193D0,1.8089D0 &
      &,1.8021D0,1.7963D0,1.7900D0,1.7764D0,1.7491D0,1.7375D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=IRG3X(1:N)
      Y(1:N)=IRG3Y(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.37) THEN
!     INTERPOLATE THE IRGN6 GLASS
      N=16
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR IRGN6
!
      DATA (IRGN6X(J), J = 1,16)/0.4047D0,0.48D0,0.4861D0 &
      &,0.5461D0,0.5876D0,0.6438D0,0.6563D0,0.7065D0,0.8521D0,1.014D0 &
      &,1.5296D0,1.9701D0,2.3254D0,2.674D0,3.303D0,4.258D0/
!
      DATA (IRGN6Y(J), J = 1,16)/1.6069D0,1.5971D0,1.5965D0,1.5915D0 &
      &,1.5892D0,1.5863D0,1.5857D0,1.5842D0,1.5807D0,1.5777D0,1.5716D0 &
      &,1.5667D0,1.5620D0,1.5567D0,1.5451D0,1.5209D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=IRGN6X(1:N)
      Y(1:N)=IRGN6Y(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.38) THEN
!     INTERPOLATE THE IRG7 GLASS
      N=16
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR IRG7
!
      DATA (IRG7X(J), J = 1,16)/0.365D0,0.4047D0,0.48D0,0.4861D0 &
      &,0.5461D0,0.5876D0,0.6438D0,0.6563D0,0.7065D0,0.8521D0,1.014D0 &
      &,1.5296D0,1.9701D0,2.3254D0,2.674D0,3.303D0/
!
      DATA (IRG7Y(J), J = 1,16)/1.5983D0,1.5871D0,1.5743D0,1.5735D0 &
      &,1.5675D0,1.5644D0,1.5612D0,1.5606D0,1.5585D0,1.5541D0,1.5509D0 &
      &,1.5442D0,1.5389D0,1.5341D0,1.5286D0,1.5164D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=IRG7X(1:N)
      Y(1:N)=IRG7Y(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.39) THEN
!     INTERPOLATE THE IRG9 GLASS
      N=16
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR IRG9
!
      DATA (IRG9X(J), J = 1,16)/0.365D0,0.4047D0,0.48D0,0.4861D0 &
      &,0.5461D0,0.5876D0,0.6438D0,0.6563D0,0.7065D0,0.8521D0,1.014D0 &
      &,1.5296D0,1.9701D0,2.3254D0,2.674D0,3.303D0/
!
      DATA (IRG9Y(J), J = 1,16)/1.5005D0,1.4961D0,1.4905D0,1.4902D0 &
      &,1.4875D0,1.4861D0,1.4845D0,1.4842D0,1.4832D0,1.4810D0,1.4793D0 &
      &,1.4755D0,1.4722D0,1.4692D0,1.4658D0,1.4583D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=IRG9X(1:N)
      Y(1:N)=IRG9Y(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.40) THEN
!     INTERPOLATE THE IRG11 GLASS
      N=16
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR IRG11
!
      DATA (IRG11X(J), J = 1,16)/0.48D0,0.4861D0 &
      &,0.5461D0,0.5876D0,0.6438D0,0.6563D0,0.7065D0,0.8521D0,1.014D0 &
      &,1.5296D0,1.9701D0,2.3254D0,2.674D0,3.303D0,4.258D0,4.586D0/
!
      DATA (IRG11Y(J), J = 1,16)/1.6926D0,1.6917D0,1.6845D0,1.6809D0 &
      &,1.6770D0,1.6763D0,1.6741D0,1.6686D0,1.6650D0,1.6581D0,1.6532D0 &
      &,1.6491D0,1.6445D0,1.6349D0,1.6158D0,1.6077D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=IRG11X(1:N)
      Y(1:N)=IRG11Y(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.41) THEN
!     INTERPOLATE THE IRG15 GLASS
      N=16
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR IRG15
!
      DATA (IRG15X(J), J = 1,16)/0.365D0,0.4047D0,0.48D0,0.4861D0 &
      &,0.5461D0,0.5876D0,0.6438D0,0.6563D0,0.7065D0,0.8521D0,1.014D0 &
      &,1.5296D0,1.9701D0,2.3254D0,2.674D0,3.303D0/
!
      DATA (IRG15Y(J), J = 1,16)/1.5883D0,1.5506D0,1.5415D0,1.5410D0 &
      &,1.5366D0,1.5343D0,1.5318D0,1.5314D0,1.5297D0,1.5263D0,1.5237D0 &
      &,1.5179D0,1.5131D0,1.5086D0,1.5038D0,1.4924D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=IRG15X(1:N)
      Y(1:N)=IRG15Y(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.42) THEN
!     INTERPOLATE THE VAC GLASS
      A=28.79D-5
      B=5.67D-5
      IF(sys_wavelength(1).NE.0.0D0) THEN
         LA1=LM1(A,B,sys_wavelength(1))
         LA1=1.0D0+LA1
         call set_surf_refractive_index(I, 1, 1.0D0/LA1)
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         LA2=LM1(A,B,sys_wavelength(2))
         LA2=1.0D0+LA2
         call set_surf_refractive_index(I, 2, 1.0D0/LA2)
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         LA3=LM1(A,B,sys_wavelength(3))
         LA3=1.0D0+LA3
         call set_surf_refractive_index(I, 3, 1.0D0/LA3)
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         LA4=LM1(A,B,sys_wavelength(4))
         LA4=1.0D0+LA4
         call set_surf_refractive_index(I, 4, 1.0D0/LA4)
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         LA5=LM1(A,B,sys_wavelength(5))
         LA5=1.0D0+LA5
         call set_surf_refractive_index(I, 5, 1.0D0/LA5)
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         LA5=LM1(A,B,sys_wavelength(6))
         LA5=1.0D0+LA5
         call set_surf_refractive_index(I, 6, 1.0D0/LA5)
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         LA5=LM1(A,B,sys_wavelength(7))
         LA5=1.0D0+LA5
         call set_surf_refractive_index(I, 7, 1.0D0/LA5)
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         LA5=LM1(A,B,sys_wavelength(8))
         LA5=1.0D0+LA5
         call set_surf_refractive_index(I, 8, 1.0D0/LA5)
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         LA5=LM1(A,B,sys_wavelength(9))
         LA5=1.0D0+LA5
         call set_surf_refractive_index(I, 9, 1.0D0/LA5)
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         LA5=LM1(A,B,sys_wavelength(10))
         LA5=1.0D0+LA5
         call set_surf_refractive_index(I, 10, 1.0D0/LA5)
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.43) THEN
!     INTERPOLATE THE H2O
      N=21
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR H2O
!
      DATA (H2OX(J), J = 1,21)/0.1829D0,0.20255D0,0.25020D0,0.30822D0,&
      &0.35871D0,0.40466D0,0.44715D0,0.50157D0,0.54607D0,0.58926D0,&
      &0.65628D0,0.70652D0,0.76820D0,0.808D0,0.871D0,0.943D0,1.028D0,&
      &1.130D0,1.256D0,1.617D0,1.968/
!
      DATA (H2OY(J), J = 1,21)/1.46379D0,1.41993D0,1.37734D0,1.35671D0,&
      &1.34795D0,1.342724D0,1.339423D0,1.336363D0,1.334466D0,1.332988D0,&
      &1.331151D0,1.330019D0,1.32890D0,1.3286D0,1.3273D0,1.3262D0,&
      &1.3250D0,1.3234D0,1.3215D0,1.3149D0,1.3078D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=H2OX(1:N)
      Y(1:N)=H2OY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.44) THEN
!     INTERPOLATE THE SUPRASIL
      N=60
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR SUPRASIL
!
      DATA (SUPX(J), J = 1,60)/0.19D0,0.20D0,0.21D0,0.22D0,&
      &0.23D0,0.24D0,0.25D0,0.26D0,0.27D0,0.28D0,&
      &0.29D0,0.30D0,0.32D0,0.34D0,0.36D0,0.36548D0,0.38D0,&
      &0.4D0,0.40465D0,0.43583D0,0.45D0,0.48613D0,0.5D0,0.54607D0,&
      &0.55D0,0.58756D0,0.6D0,0.65D0,0.65627D0,0.7D0,0.75D0,0.8D0,&
      &0.85D0,0.9D0,1.0D0,1.1D0,1.2D0,1.3D0,1.4D0,1.5D0,1.6D0,1.7D0,&
      &1.8D0,1.9D0,2.0D0,2.1D0,2.2D0,2.3D0,2.4D0,2.5D0,2.6D0,2.7D0,&
      &2.8D0,2.9D0,3.0D0,3.1D0,3.2D0,3.3D0,3.4D0,3.5D0/
!
      DATA (SUPY(J), J = 1,60)/&
      &1.56572D0,1.55051D0,1.53836D0,1.52845D0,1.52024D0,1.51333D0,&
      &1.50745D0,1.50239D0,1.49800D0,1.49416D0,1.49079D0,1.48779D0,&
      &1.48274D0,1.47865D0,1.47529D0,1.47447D0,1.47248D0,1.47012D0,&
      &1.46962D0,1.46669D0,1.46557D0,1.46313D0,1.46233D0,1.46008D0,&
      &1.45991D0,1.45846D0,1.45804D0,1.45653D0,1.45637D0,1.45529D0,&
      &1.45424D0,1.45332D0,1.45250D0,1.45175D0,1.45042D0,1.44920D0,&
      &1.44805D0,1.44692D0,1.44758D0,1.44462D0,1.44342D0,1.44217D0,&
      &1.44087D0,1.43951D0,1.43809D0,1.43659D0,1.43501D0,1.43336D0,&
      &1.43163D0,1.42980D0,1.42789D0,1.42588D0,1.42377D0,1.42156D0,&
      &1.41925D0,1.41682D0,1.41427D0,1.41161D0,1.40881D0,1.40589D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=SUPX(1:N)
      Y(1:N)=SUPY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.45) THEN
!     INTERPOLATE THE HOMOSIL
      N=47
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR SUPRASIL
!
      DATA (HOMOX(J), J = 1,47)/&
      &0.34D0,0.36D0,0.36548D0,0.38D0,&
      &0.4D0,0.40465D0,0.43583D0,0.45D0,0.48613D0,0.5D0,0.54607D0,&
      &0.55D0,0.58756D0,0.6D0,0.65D0,0.65627D0,0.7D0,0.75D0,0.8D0,&
      &0.85D0,0.9D0,1.0D0,1.1D0,1.2D0,1.3D0,1.4D0,1.5D0,1.6D0,1.7D0,&
      &1.8D0,1.9D0,2.0D0,2.1D0,2.2D0,2.3D0,2.4D0,2.5D0,2.6D0,2.7D0,&
      &2.8D0,2.9D0,3.0D0,3.1D0,3.2D0,3.3D0,3.4D0,3.5D0/
!
      DATA (HOMOY(J), J = 1,47)/1.47881D0,1.47544D0,1.47462D0,&
      &1.47262D0,1.47025D0,1.46975D0,1.46681D0,1.46568D0,1.46324D0,&
      &1.46243D0,1.46018D0,1.46001D0,1.45856D0,1.45814D0,1.45663D0,&
      &1.45646D0,1.45539D0,1.45433D0,1.45341D0,1.45259D0,1.45185D0,&
      &1.45051D0,1.44930D0,1.44815D0,1.44702D0,1.44589D0,1.44473D0,&
      &1.44353D0,1.44229D0,1.44099D0,1.43964D0,1.43821D0,1.43672D0,&
      &1.43515D0,1.43350D0,1.43177D0,1.42995D0,1.42804D0,1.42604D0,&
      &1.42393D0,1.42172D0,1.41941D0,1.41698D0,1.41444D0,1.41177D0,&
      &1.40897D0,1.40605D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=HOMOX(1:N)
      Y(1:N)=HOMOY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.46) THEN
!     INTERPOLATE THE II-IV ZNS-MS
      N=59
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR ZNS-MS
!
      DATA (ZNSMSX(J), J = 1,59)/&
      &0.42D0,0.46D0,0.50D0,0.54D0,0.58D0,0.62D0,0.66D0,0.70D0,&
      &0.74D0,0.78D0,0.82D0,0.86D0,0.90D0,0.94D0,0.98D0,1.00D0,&
      &1.40D0,1.80D0,2.20D0,2.60D0,3.00D0,3.40D0,3.80D0,4.20D0,&
      &4.60D0,5.00D0,5.40D0,5.80D0,6.20D0,6.60D0,7.00D0,7.40D0,&
      &7.80D0,8.20D0,8.60D0,9.00D0,9.40D0,9.80D0,10.20D0,10.60D0,&
      &11.00D0,11.40D0,11.80D0,12.20D0,12.60D0,13.0D0,13.40D0,13.80D0,&
      &14.20D0,14.60D0,15.00D0,15.40D0,15.80D0,16.2D0,16.60D0,17.00D0,&
      &17.40D0,17.80D0,18.20D0/
!
      DATA (ZNSMSY(J), J = 1,59)/&
      &2.516D0,2.458D0,2.419D0,2.391D0,2.371D0,2.355D0,2.342D0,2.332D0,&
      &2.323D0,2.316D0,2.310D0,2.305D0,2.301D0,2.297D0,2.294D0,2.292D0,&
      &2.275D0,2.267D0,2.263D0,2.260D0,2.257D0,2.255D0,2.253D0,2.251D0,&
      &2.248D0,2.246D0,2.244D0,2.241D0,2.238D0,2.235D0,2.232D0,2.228D0,&
      &2.225D0,2.221D0,2.217D0,2.212D0,2.208D0,2.203D0,2.198D0,2.192D0,&
      &2.186D0,2.180D0,2.173D0,2.167D0,2.159D0,2.152D0,2.143D0,2.135D0,&
      &2.126D0,2.116D0,2.106D0,2.095D0,2.084D0,2.072D0,2.059D0,2.045D0,&
      &2.030D0,2.015D0,1.998D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=ZNSMSX(1:N)
      Y(1:N)=ZNSMSY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
!
   IF(MTYPE.EQ.47) THEN
!     INTERPOLATE THE CEF3
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 1, (((1.63D0-1.59D0)/(0.55D0-2.0D0))*(sys_wavelength(1)-2.0D0))+1.59D0)
         IF(sys_wavelength(1).LE.0.55D0) call set_surf_refractive_index(I, 1, 1.63D0)
         IF(sys_wavelength(1).GE.2.00D0) call set_surf_refractive_index(I, 1, 1.59D0)
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 2, (((1.63D0-1.59D0)/(0.55D0-2.0D0))*(sys_wavelength(2)-2.0D0))+1.59D0)
         IF(sys_wavelength(2).LE.0.55D0) call set_surf_refractive_index(I, 2, 1.63D0)
         IF(sys_wavelength(2).GE.2.00D0) call set_surf_refractive_index(I, 2, 1.59D0)
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 3, (((1.63D0-1.59D0)/(0.55D0-2.0D0))*(sys_wavelength(3)-2.0D0))+1.59D0)
         IF(sys_wavelength(3).LE.0.55D0) call set_surf_refractive_index(I, 3, 1.63D0)
         IF(sys_wavelength(3).GE.2.00D0) call set_surf_refractive_index(I, 3, 1.59D0)
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 4, (((1.63D0-1.59D0)/(0.55D0-2.0D0))*(sys_wavelength(4)-2.0D0))+1.59D0)
         IF(sys_wavelength(4).LE.0.55D0) call set_surf_refractive_index(I, 4, 1.63D0)
         IF(sys_wavelength(4).GE.2.00D0) call set_surf_refractive_index(I, 4, 1.59D0)
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 5, (((1.63D0-1.59D0)/(0.55D0-2.0D0))*(sys_wavelength(5)-2.0D0))+1.59D0)
         IF(sys_wavelength(5).LE.0.55D0) call set_surf_refractive_index(I, 5, 1.63D0)
         IF(sys_wavelength(5).GE.2.00D0) call set_surf_refractive_index(I, 5, 1.59D0)
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 6, (((1.63D0-1.59D0)/(0.55D0-2.0D0))*(sys_wavelength(6)-2.0D0))+1.590)
         IF(sys_wavelength(6).LE.0.55D0) call set_surf_refractive_index(I, 6, 1.63D0)
         IF(sys_wavelength(6).GE.2.00D0) call set_surf_refractive_index(I, 6, 1.59D0)
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 7, (((1.63D0-1.59D0)/(0.55D0-2.0D0))*(sys_wavelength(7)-2.0D0))+1.590)
         IF(sys_wavelength(7).LE.0.55D0) call set_surf_refractive_index(I, 7, 1.63D0)
         IF(sys_wavelength(7).GE.2.00D0) call set_surf_refractive_index(I, 7, 1.59D0)
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 8, (((1.63D0-1.59D0)/(0.55D0-2.0D0))*(sys_wavelength(8)-2.0D0))+1.590)
         IF(sys_wavelength(8).LE.0.55D0) call set_surf_refractive_index(I, 8, 1.63D0)
         IF(sys_wavelength(8).GE.2.00D0) call set_surf_refractive_index(I, 8, 1.59D0)
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 9, (((1.63D0-1.59D0)/(0.55D0-2.0D0))*(sys_wavelength(9)-2.0D0))+1.590)
         IF(sys_wavelength(9).LE.0.55D0) call set_surf_refractive_index(I, 9, 1.63D0)
         IF(sys_wavelength(9).GE.2.00D0) call set_surf_refractive_index(I, 9, 1.59D0)
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 10, (((1.63D0-1.59D0)/(0.55D0-2.0D0))*(sys_wavelength(10)-2.0D0))+1.590)
         IF(sys_wavelength(10).LE.0.55D0) call set_surf_refractive_index(I, 10, 1.63D0)
         IF(sys_wavelength(10).GE.2.00D0) call set_surf_refractive_index(I, 10, 1.59D0)
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.48) THEN
!     INTERPOLATE THE LS203
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 1, (((1.95D0-1.86D0)/(0.55D0-2.0D0))*(sys_wavelength(1)-2.0D0))+1.86D0)
         call set_surf_refractive_index(I, 1, 1.95D0)
         call set_surf_refractive_index(I, 1, 1.86D0)
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 2, (((1.95D0-1.86D0)/(0.55D0-2.0D0))*(sys_wavelength(2)-2.0D0))+1.86D0)
         call set_surf_refractive_index(I, 2, 1.95D0)
         call set_surf_refractive_index(I, 2, 1.86D0)
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 3, (((1.95D0-1.86D0)/(0.55D0-2.0D0))*(sys_wavelength(3)-2.0D0))+1.86D0)
         call set_surf_refractive_index(I, 3, 1.95D0)
         call set_surf_refractive_index(I, 3, 1.86D0)
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 4, (((1.95D0-1.86D0)/(0.55D0-2.0D0))*(sys_wavelength(4)-2.0D0))+1.86D0)
         call set_surf_refractive_index(I, 4, 1.95D0)
         call set_surf_refractive_index(I, 4, 1.86D0)
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 5, (((1.95D0-1.86D0)/(0.55D0-2.0D0))*(sys_wavelength(5)-2.0D0))+1.86D0)
         call set_surf_refractive_index(I, 5, 1.95D0)
         call set_surf_refractive_index(I, 5, 1.86D0)
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 6, (((1.95D0-1.86D0)/(0.55D0-2.0D0))*(sys_wavelength(6)-2.0D0))+1.860)
         call set_surf_refractive_index(I, 6, 1.95D0)
         call set_surf_refractive_index(I, 6, 1.86D0)
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 7, (((1.95D0-1.86D0)/(0.55D0-2.0D0))*(sys_wavelength(7)-2.0D0))+1.860)
         call set_surf_refractive_index(I, 7, 1.95D0)
         call set_surf_refractive_index(I, 7, 1.86D0)
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 8, (((1.95D0-1.86D0)/(0.55D0-2.0D0))*(sys_wavelength(8)-2.0D0))+1.860)
         call set_surf_refractive_index(I, 8, 1.95D0)
         call set_surf_refractive_index(I, 8, 1.86D0)
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 9, (((1.95D0-1.86D0)/(0.55D0-2.0D0))*(sys_wavelength(9)-2.0D0))+1.860)
         call set_surf_refractive_index(I, 9, 1.95D0)
         call set_surf_refractive_index(I, 9, 1.86D0)
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 10, (((1.95D0-1.86D0)/(0.55D0-2.0D0))*(sys_wavelength(10)-2.0D0))+1.860)
         call set_surf_refractive_index(I, 10, 1.95D0)
         call set_surf_refractive_index(I, 10, 1.86D0)
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.49) THEN
!     INTERPOLATE THE THF4
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 1, (((1.52D0-1.51D0)/(0.40D0-0.75D0))*(sys_wavelength(1)-.75D0))+1.51D0)
         call set_surf_refractive_index(I, 1, 1.52D0)
         call set_surf_refractive_index(I, 1, 1.51D0)
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 2, (((1.52D0-1.51D0)/(0.40D0-0.75D0))*(sys_wavelength(2)-.75D0))+1.51D0)
         call set_surf_refractive_index(I, 2, 1.52D0)
         call set_surf_refractive_index(I, 2, 1.51D0)
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 3, (((1.52D0-1.51D0)/(0.40D0-0.75D0))*(sys_wavelength(3)-.75D0))+1.51D0)
         call set_surf_refractive_index(I, 3, 1.52D0)
         call set_surf_refractive_index(I, 3, 1.51D0)
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 4, (((1.52D0-1.51D0)/(0.40D0-0.75D0))*(sys_wavelength(4)-.75D0))+1.51D0)
         call set_surf_refractive_index(I, 4, 1.52D0)
         call set_surf_refractive_index(I, 4, 1.51D0)
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 5, (((1.52D0-1.51D0)/(0.40D0-0.75D0))*(sys_wavelength(5)-.75D0))+1.51D0)
         call set_surf_refractive_index(I, 5, 1.52D0)
         call set_surf_refractive_index(I, 5, 1.51D0)
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 6, (((1.52D0-1.51D0)/(0.40D0-0.75D0))*(sys_wavelength(6)-.75D0))+1.51D0)
         call set_surf_refractive_index(I, 6, 1.52D0)
         call set_surf_refractive_index(I, 6, 1.51D0)
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 7, (((1.52D0-1.51D0)/(0.40D0-0.75D0))*(sys_wavelength(7)-.75D0))+1.51D0)
         call set_surf_refractive_index(I, 7, 1.52D0)
         call set_surf_refractive_index(I, 7, 1.51D0)
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 8, (((1.52D0-1.51D0)/(0.40D0-0.75D0))*(sys_wavelength(8)-.75D0))+1.51D0)
         call set_surf_refractive_index(I, 8, 1.52D0)
         call set_surf_refractive_index(I, 8, 1.51D0)
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 9, (((1.52D0-1.51D0)/(0.40D0-0.75D0))*(sys_wavelength(9)-.75D0))+1.51D0)
         call set_surf_refractive_index(I, 9, 1.52D0)
         call set_surf_refractive_index(I, 9, 1.51D0)
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 10, (((1.52D0-1.51D0)/(0.40D0-0.75D0))*(sys_wavelength(10)-.75D0))+1.51D0)
         call set_surf_refractive_index(I, 10, 1.52D0)
         call set_surf_refractive_index(I, 10, 1.51D0)
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.50) THEN
!     INTERPOLATE THE ZRO2
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 1, (((2.10D0-2.00D0)/(0.55D0-2.0D0))*(sys_wavelength(1)-.75D0))+2.00D0)
         call set_surf_refractive_index(I, 1, 2.10D0)
         call set_surf_refractive_index(I, 1, 2.00D0)
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 2, (((2.10D0-2.00D0)/(0.55D0-2.0D0))*(sys_wavelength(2)-.75D0))+2.00D0)
         call set_surf_refractive_index(I, 2, 2.10D0)
         call set_surf_refractive_index(I, 2, 2.00D0)
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 3, (((2.10D0-2.00D0)/(0.55D0-2.0D0))*(sys_wavelength(3)-.75D0))+2.00D0)
         call set_surf_refractive_index(I, 3, 2.10D0)
         call set_surf_refractive_index(I, 3, 2.00D0)
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 4, (((2.10D0-2.00D0)/(0.55D0-2.0D0))*(sys_wavelength(4)-.75D0))+2.00D0)
         call set_surf_refractive_index(I, 4, 2.10D0)
         call set_surf_refractive_index(I, 4, 2.00D0)
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 5, (((2.10D0-2.00D0)/(0.55D0-2.0D0))*(sys_wavelength(5)-.75D0))+2.00D0)
         call set_surf_refractive_index(I, 5, 2.10D0)
         call set_surf_refractive_index(I, 5, 2.00D0)
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 6, (((2.10D0-2.00D0)/(0.55D0-2.0D0))*(sys_wavelength(6)-.75D0))+2.00D0)
         call set_surf_refractive_index(I, 6, 2.10D0)
         call set_surf_refractive_index(I, 6, 2.00D0)
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 7, (((2.10D0-2.00D0)/(0.55D0-2.0D0))*(sys_wavelength(7)-.75D0))+2.00D0)
         call set_surf_refractive_index(I, 7, 2.10D0)
         call set_surf_refractive_index(I, 7, 2.00D0)
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 8, (((2.10D0-2.00D0)/(0.55D0-2.0D0))*(sys_wavelength(8)-.75D0))+2.00D0)
         call set_surf_refractive_index(I, 8, 2.10D0)
         call set_surf_refractive_index(I, 8, 2.00D0)
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 9, (((2.10D0-2.00D0)/(0.55D0-2.0D0))*(sys_wavelength(9)-.75D0))+2.00D0)
         call set_surf_refractive_index(I, 9, 2.10D0)
         call set_surf_refractive_index(I, 9, 2.00D0)
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 10, (((2.10D0-2.00D0)/(0.55D0-2.0D0))*(sys_wavelength(10)-.75D0))+2.00D0)
         call set_surf_refractive_index(I, 10, 2.10D0)
         call set_surf_refractive_index(I, 10, 2.00D0)
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.EQ.51) THEN
!     INTERPOLATE THE DIAMOND
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 1, DIAMOND(sys_wavelength(1)))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 2, DIAMOND(sys_wavelength(2)))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 3, DIAMOND(sys_wavelength(3)))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 4, DIAMOND(sys_wavelength(4)))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 5, DIAMOND(sys_wavelength(5)))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 6, DIAMOND(sys_wavelength(6)))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 7, DIAMOND(sys_wavelength(7)))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 8, DIAMOND(sys_wavelength(8)))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 9, DIAMOND(sys_wavelength(9)))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 10, DIAMOND(sys_wavelength(10)))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
   END IF
   IF(MTYPE.EQ.52) THEN
!     INTERPOLATE YAG
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 1, YAG(sys_wavelength(1)))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 2, YAG(sys_wavelength(2)))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 3, YAG(sys_wavelength(3)))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 4, YAG(sys_wavelength(4)))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 5, YAG(sys_wavelength(5)))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 6, YAG(sys_wavelength(6)))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 7, YAG(sys_wavelength(7)))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 8, YAG(sys_wavelength(8)))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 9, YAG(sys_wavelength(9)))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         call set_surf_refractive_index(I, 10, YAG(sys_wavelength(10)))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   IF(MTYPE.GE.101.AND.MTYPE.LE.104) THEN
!
!     PLASTICS
!
      DATA (GPLX(J), J = 1,13)/0.36501D0,0.40466D0,0.43484D0 &
      &,0.47999D0,0.48613D0,0.54607D0,0.58756D0,0.58929D0,0.64385D0 &
      &,0.65627D0,0.70652D0,0.85211D0,1.01398D0/
!
      DATA (GPL1Y(J), J = 1,13)/1.513613D0,1.506607D0,1.502557D0 &
      &,1.498258D0,1.497760D0,1.493795D0,1.491757D0,1.491681D0 &
      &,1.489603D0,1.489201D0,1.487552D0,1.484965D0,1.483115D0/
!
      DATA (GPL2Y(J), J = 1,13)/1.643126D0,1.625341D0,1.615466D0 &
      &,1.605241D0,1.604079D0,1.595010D0,1.590481D0,1.590315D0 &
      &,1.585808D0,1.584949D0,1.581954D0,1.576196D0,1.572553D0/
!
      DATA (GPL3Y(J), J = 1,13)/1.643231D0,1.622447D0,1.611519D0 &
      &,1.600654D0,1.599439D0,1.590081D0,1.585470D0,1.585302D0 &
      &,1.580734D0,1.579864D0,1.576831D0,1.570981D0,1.567248D0/
!
      DATA (GPL4Y(J), J = 1,13)/1.612490D0,1.597075D0,1.588640D0 &
      &,1.579985D0,1.579000D0,1.571300D0,1.567400D0,1.567298D0 &
      &,1.563438D0,1.562700D0,1.560119D0,1.555108D0,1.551870D0/
!
      N=13
!
      IF(MTYPE.EQ.101) THEN
!     INTERPOLATE THE ACRYLIC DATA
         X(1:N)=GPLX(1:N)
         Y(1:N)=GPL1Y(1:N)
      END IF
      IF(MTYPE.EQ.102) THEN
!     INTERPOLATE THE POLYSTYRENE DATA
         X(1:N)=GPLX(1:N)
         Y(1:N)=GPL2Y(1:N)
      END IF
      IF(MTYPE.EQ.103) THEN
!     INTERPOLATE THE POLYCARBONATE DATA
         X(1:N)=GPLX(1:N)
         Y(1:N)=GPL3Y(1:N)
      END IF
      IF(MTYPE.EQ.104) THEN
!     INTERPOLATE THE (SAN) DATA
         X(1:N)=GPLX(1:N)
         Y(1:N)=GPL4Y(1:N)
      END IF
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     MOVE TO LARGE ARRAYS
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      IF(sys_wavelength(1).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(1),surf_refractive_index(I, 1))
      ELSE
         call set_surf_refractive_index(I, 1, 1.0D0)
      END IF
      IF(sys_wavelength(2).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(2),surf_refractive_index(I, 2))
      ELSE
         call set_surf_refractive_index(I, 2, 1.0D0)
      END IF
      IF(sys_wavelength(3).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(3),surf_refractive_index(I, 3))
      ELSE
         call set_surf_refractive_index(I, 3, 1.0D0)
      END IF
      IF(sys_wavelength(4).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(4),surf_refractive_index(I, 4))
      ELSE
         call set_surf_refractive_index(I, 4, 1.0D0)
      END IF
      IF(sys_wavelength(5).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(5),surf_refractive_index(I, 5))
      ELSE
         call set_surf_refractive_index(I, 5, 1.0D0)
      END IF
      IF(sys_wavelength(6).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(6),surf_refractive_index(I, 6))
      ELSE
         call set_surf_refractive_index(I, 6, 1.0D0)
      END IF
      IF(sys_wavelength(7).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(7),surf_refractive_index(I, 7))
      ELSE
         call set_surf_refractive_index(I, 7, 1.0D0)
      END IF
      IF(sys_wavelength(8).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(8),surf_refractive_index(I, 8))
      ELSE
         call set_surf_refractive_index(I, 8, 1.0D0)
      END IF
      IF(sys_wavelength(9).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(9),surf_refractive_index(I, 9))
      ELSE
         call set_surf_refractive_index(I, 9, 1.0D0)
      END IF
      IF(sys_wavelength(10).NE.0.0D0) THEN
         CALL SPLINT(X,Y,Y2,N,sys_wavelength(10),surf_refractive_index(I, 10))
      ELSE
         call set_surf_refractive_index(I, 10, 1.0D0)
      END IF
!
   END IF
   RETURN
END
! SUB SPCGL.FOR
SUBROUTINE SPCGL(I,MTYPE)
   use DATLEN
   use DATMAI
   use mod_surface
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   INTEGER I,MTYPE,J,N
!
   real(real64) X(1:100),Y(1:100),Y2(1:100),LA,CSI &
   &,YPN,YP1,GSCX(1:20),GSCY(1:20),GPCX(1:17),GPCY(1:17)&
   &,SIL,GPLX(1:13),GPL1Y(1:13),GPL2Y(1:13),VIR3X(1:18),VIR3Y(1:18)&
   &,GPL3Y(1:13),GPL4Y(1:13),IRGX(1:25),IRGY(1:25),ZNSEX(1:56)&
   &,ZNSEY(1:56),ZNSX(1:59),ZNSY(1:59),CLRX(1:29),CLRY(1:29)&
   &,SAPH,FSIL,MGF2O,MGF2E,CAF2,MGO,G9754X(1:43),G9754Y(1:43)&
   &,AMTR1X(1:17),AMTR1Y(1:17),DYN,BAF2,KBR,CSBR,KRS5 &
   &,AMTR3X(1:12),AMTR3Y(1:12),AS2S3,ALONX(1:14),ALONY(1:14)&
   &,GAASX(1:13),GAASY(1:13),CDTEX(1:10),CDTEY(1:10)&
   &,SPINX(1:11),SPINY(1:11),CALALX(1:12),CALALY(1:12)&
   &,DUMA,DUMB,DUML,ZNSMSX(1:59),ZNSMSY(1:59),YAG
!
   real(real64) SIO2OX(1:29),SIO2OY(1:29),SIO2EX(1:20)&
   &,SIO2EY(1:20),LIF,NACLX(1:60),NACLY(1:60),DIAMOND &
   &,B270X(1:8),B270Y(1:8),LM1,LA1,LA2,LA3,LA4,LA5,A,B,H2OX(1:21)&
   &,H2OY(1:21),SUPX(1:60),SUPY(1:60),HOMOX(1:47),HOMOY(1:47)&
   &,IRG2X(1:18),IRG2Y(1:18),IRG3X(1:17),IRG3Y(1:17),IRGN6X(1:16)&
   &,IRGN6Y(1:16),IRG7X(1:16),IRG7Y(1:16),IRG9X(1:16),IRG9Y(1:16)&
   &,IRG11X(1:16),IRG11Y(1:16),IRG15X(1:16),IRG15Y(1:16)
!
!
!     VACUME
   LM1(DUMA,DUMB,DUML)=DUMA*(1.0D0+(DUMB/(DUML**2)))
!
!     SILICA/SIO2 INTERPOLATION FORMULA (HANDBOOK OF OPTICS 11/14/2000)
   FSIL(LA)=DSQRT(1.0D0+&
   &((0.6961663*(LA**2))/((LA**2)-((0.0684043)**2)))+&
   &((0.4079426*(LA**2))/((LA**2)-((0.1162414)**2)))+&
   &((0.8974794*(LA**2))/((LA**2)-((9.896161)**2)))&
   &)

!
!     DIAMOND INTERPOLATION FORMULA
   DIAMOND(LA)=&
   &2.37837D0+&
   &((1.18897E-2)*(1.0D0/((LA**2)-0.028D0)))+&
   &((-1.0083E-4)*((1.0D0/((LA**2)-0.028D0))**2))+&
   &((-2.3676E-5)*(LA**2))+&
   &((3.24263E-8)*(LA**4))
!
!     LiF INTERPOLATION FORMULA
   LIF(LA)=&
   &1.38761D0+(0.001796D0/((LA**2)-0.028D0))&
   &+(0.000041D0/(((LA**2)-0.028D0)**2))-(0.0023045D0*(LA**2))&
   &-(0.00000557D0*(LA**4))
!
!     SILICON INTERPOLATION FORMULA
   SIL(LA)=&
   &3.41696D0+(0.138497D0/((LA**2)-0.028D0))&
   &+(0.013924D0/(((LA**2)-0.028D0)**2))-(0.0000209D0*(LA**2))&
   &+(0.000000148D0*(LA**4))
!
!     KBr INTERPOLATION FORMULA
   KBR(LA)=DSQRT(&
   &2.361323D0-(3.11497D-4*(LA**2))-(5.8613D-8*(LA**4))+&
   &(0.007676D0/(LA**2))+(0.0156569D0/((LA**2)-0.0324D0)))
!
!     CsBr INTERPOLATION FORMULA
   CSBR(LA)=DSQRT(&
   &5.640752D0-(3.338D-6*(LA**2))+&
   &(0.0018612D0/(LA**2))+(41110.49D0/((LA**2)-14390.4D0))&
   &+(0.0290764D0/((LA**2)-0.024964D0)))
!
!     MGO INTERPOLATION FORMULA
   MGO(LA)=DSQRT(&
   &2.956362D0-(0.01062387D0*(LA**2))-(0.0000204968D0*(LA**4))-&
   &(0.02195770/((LA**2)-0.01428322D0)))
!
!     DYNASIL INTERPOLATION FORMULA
   DYN(LA)=DSQRT(&
   &((0.6961663D0*(LA**2))/((LA**2)-((0.0684043D0)**2)))+&
   &((0.4079426D0*(LA**2))/((LA**2)-((0.1162414D0)**2)))+&
   &((0.8974794D0*(LA**2))/((LA**2)-((9.896161D0)**2)))+1.0D0)
!
!     SAPPHIRE INTERPOLATION FORMULA
   SAPH(LA)=DSQRT(&
   &((1.023798D0*(LA**2))/((LA**2)-(0.00377588D0)))+&
   &((1.058264D0*(LA**2))/((LA**2)-(0.01225440D0)))+&
   &((5.280792D0*(LA**2))/((LA**2)-(321.361600D0)))+1.0D0)
!
!     YAG INTERPOLATION FORMULA
   YAG(LA)=DSQRT(&
   &((2.293D0*(LA**2))/((LA**2)-(0.1095D0**2)))+&
   &((3.705D0*(LA**2))/((LA**2)-(17.825D0**2)))+1.0D0)
!
!     CSI INTERPOLATION FORMULA
   CSI(LA)=DSQRT(&
   &((0.34617251D0*(LA**2))/((LA**2)-0.00052701D0))+&
   &((1.0080886D0*(LA**2))/((LA**2)-0.02149156D0))+&
   &((0.28551800D0*(LA**2))/((LA**2)-0.032761D0))+&
   &((0.39743178D0*(LA**2))/((LA**2)-0.044944D0))+&
   &((3.3605359D0*(LA**2))/((LA**2)-25621.0D0))+1.0D0)
!
!     KRS5 INTERPOLATION FORMULA
   KRS5(LA)=DSQRT(&
   &((1.8293958D0*(LA**2))/((LA**2)-(0.0225D0**2)))+&
   &((1.6675593D0*(LA**2))/((LA**2)-(0.0625D0**2)))+&
   &((1.1210424D0*(LA**2))/((LA**2)-(0.1225D0**2)))+&
   &((0.04513366D0*(LA**2))/((LA**2)-(0.2025D0**2)))+&
   &((12.380234D0*(LA**2))/((LA**2)-(27089.737D0**2)))+1.0D0)
!
!     As2S3 INTERPOLATION FORMULA
   AS2S3(LA)=DSQRT(&
   &((1.8983678D0*(LA**2))/((LA**2)-(0.15D0**2)))+&
   &((1.9222979D0*(LA**2))/((LA**2)-(0.25D0**2)))+&
   &((0.8765134D0*(LA**2))/((LA**2)-(0.350D0**2)))+&
   &((0.1188704D0*(LA**2))/((LA**2)-(0.450D0**2)))+&
   &((0.9569903D0*(LA**2))/((LA**2)-(27.3861D0**2)))+1.0D0)
!
!     BAF2 INTERPOLATION FORMULA
   BAF2(LA)=DSQRT(&
   &((0.643356D0*(LA**2))/((LA**2)-((0.057789D0)**2)))+&
   &((0.506762D0*(LA**2))/((LA**2)-((0.10968D0)**2)))+&
   &((3.8261D0*(LA**2))/((LA**2)-((46.3864D0)**2)))+1.0D0)
!
!     CAF2 INTERPOLATION FORMULA
   CAF2(LA)=DSQRT(&
   &((0.5675888D0*(LA**2))/((LA**2)-((0.050263605D0)**2)))+&
   &((0.4710914D0*(LA**2))/((LA**2)-((0.1003909D0)**2)))+&
   &((3.8484723D0*(LA**2))/((LA**2)-((34.649040D0)**2)))+1.0D0)
!
!     MGF2o INTERPOLATION FORMULA
   MGF2O(LA)=DSQRT(&
   &1.0D0+&
   &((.48755108D0*(LA**2))/((LA**2)-(0.04338408D0**2)))&
   &+((.39875031D0*(LA**2))/((LA**2)-(0.09461442D0**2)))&
   &+((2.3120353D0*(LA**2))/((LA**2)-(23.7936040D0**2)))&
   &)
!
!     MGF2E INTERPOLATION FORMULA
   MGF2E(LA)=DSQRT(&
   &1.0D0+&
   &((.41344023D0*(LA**2))/((LA**2)-(0.03684262D0**2)))&
   &+((.50497499D0*(LA**2))/((LA**2)-(0.09076162D0**2)))&
   &+((2.4904862D0*(LA**2))/((LA**2)-(12.771995D0**2)))&
   &)
!
!     THIS SUBROUTINE IS USED TO CALCULATE REFRACTIVE
!     INDICES OF THE MATERIALS WHOSE NAMES ARE THEIR COMMANDS
!
   IF(MTYPE.EQ.1) THEN
!     INTERPOLATE THE SINGLE CRYSTALLINE GERMANIUN DATA
      N=20
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR SINGLE CRYSTAL GERMANIUM
!
      DATA (GSCX(J), J = 1,20)/2.0581D0,2.1526D0,2.3126D0,2.4374D0,&
      &2.577D0,&
      &2.7144D0,2.998D0,3.3033D0,3.4188D0,4.258D0,4.866D0,6.238D0,&
      &8.66D0,9.72D0,11.04D0,12.20D0,13.02D0,14.21D0,15.08D0,16.0D0/
!
      DATA (GSCY(J), J = 1,20)/4.1016D0,4.0919D0,4.0786D0,4.0708D0,&
      &4.0609D0,4.0552D0,4.0452D0,4.0369D0,4.0334D0,&
      &4.0216D0,4.0170D0,4.0094D0,4.0043D0,4.0034D0,&
      &4.0026D0,4.0023D0,4.0021D0,4.0015D0,4.0014D0,4.0012D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=GSCX(1:N)
      Y(1:N)=GSCY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
      CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
      CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
      CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
      CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
   END IF
   IF(MTYPE.EQ.2) THEN
!     INTERPOLATE THE POLY CRYSTALLINE GERMANIUN DATA
      N=17
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR POLY CRYSTAL GERMANIUM
!
      DATA (GPCX(J), J = 1,17)/2.0581D0,2.1526D0,2.3126D0,2.4374D0,&
      &2.577D0,&
      &2.7144D0,2.998D0,3.3033D0,3.4188D0,4.258D0,4.866D0,6.238D0,&
      &8.66D0,9.72D0,11.04D0,12.20D0,13.02D0/
!
      DATA (GPCY(J), J = 1,17)/4.1018D0,4.0919D0,4.0785D0,4.0709D0,&
      &4.0608D0,4.0554D0,4.0452D0,4.0372D0,4.0339D0,&
      &4.0217D0,4.0167D0,4.0095D0,4.0043D0,4.0033D0,&
      &4.0025D0,4.0020D0,4.0018D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=GPCX(1:N)
      Y(1:N)=GPCY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
      CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
      CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
      CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
      CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
   END IF
   IF(MTYPE.EQ.3) THEN
!     INTERPOLATE THE SILICON DATA
!     CALCULATE THE INDICES
      GPREG(1)=SIL(GLSWV(1))
      GPREG(2)=SIL(GLSWV(2))
      GPREG(3)=SIL(GLSWV(3))
      GPREG(4)=SIL(GLSWV(4))
      GPREG(5)=SIL(GLSWV(5))
   END IF
   IF(MTYPE.EQ.4) THEN
!     INTERPOLATE THE IRG100 DATA
      N=25
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR IRG100
!
      DATA (IRGX(J), J = 1,25)/1.0D0,1.5D0,2.0D0,2.5D0,3.0D0,3.5D0 &
      &,4.0D0,4.5D0,5.0D0,5.5D0,6.0D0,6.5D0,7.0D0,7.5D0,8.0D0,8.5D0 &
      &,9.0D0,9.5D0,10.0D0,10.5D0,11.0D0,11.5D0,12.0D0,13.0D0,14.0D0/
!
      DATA (IRGY(J), J = 1,25)/2.7235D0,2.6577D0,2.6404D0,2.6314D0 &
      &,2.6262D0,2.6227D0,2.6201D0,2.6181D0,2.6164D0,2.6148D0,2.6133D0 &
      &,2.6118D0,2.6103D0,2.6088D0,2.6072D0,2.6056D0,2.6039D0,2.6022D0 &
      &,2.6004D0,2.5985D0,2.5966D0,2.5946D0,2.5925D0,2.5880D0,2.5832D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=IRGX(1:N)
      Y(1:N)=IRGY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
      CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
      CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
      CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
      CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
   END IF
   IF(MTYPE.EQ.5) THEN
!     INTERPOLATE THE ZNSE DATA
      N=56
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR ZNSE
!
      DATA (ZNSEX(J), J = 1,56)/0.54D0,0.58D0 &
      &,0.62D0,0.66D0,0.70D0,0.74D0,0.78D0,0.82D0,0.86D0,0.90D0 &
      &,0.94D0,0.98D0,1.0D0,1.4D0,1.8D0,2.2D0,2.6D0,3.0D0,3.4D0 &
      &,3.8D0,4.2D0,4.6D0,5.0D0,5.4D0,5.8D0,6.2D0,6.6D0,7.0D0,7.4D0 &
      &,7.8D0,8.2D0,8.6D0,9.0D0,9.4D0,9.8D0,10.2D0,10.6D0,11.0D0 &
      &,11.4D0,11.8D0,12.2D0,12.6D0,13.0D0,13.4D0,13.8D0,14.2D0,14.6D0 &
      &,15.0D0,15.4D0,15.8D0,16.2D0,16.6D0,17.0D0,17.4D0,17.8D0,18.2D0/
!
      DATA (ZNSEY(J), J = 1,56)/2.6754D0,2.6312D0,2.5994D0,2.5755D0 &
      &,2.5568D0,2.5418D0,2.5295D0,2.5193D0,2.5107D0,2.5034D0,2.4971D0 &
      &,2.4916D0,2.4892D0,2.4609D0,2.4496D0,2.4437D0,2.4401D0,2.4376D0 &
      &,2.4356D0,2.4339D0,2.4324D0,2.4309D0,2.4295D0,2.4281D0,2.4266D0 &
      &,2.4251D0,2.4235D0,2.4218D0,2.4201D0,2.4183D0,2.4163D0,2.4143D0 &
      &,2.4122D0,2.4100D0,2.4077D0,2.4053D0,2.4028D0,2.4001D0,2.3974D0 &
      &,2.3945D0,2.3915D0,2.3883D0,2.3850D0,2.3816D0,2.3781D0,2.3744D0 &
      &,2.3705D0,2.3665D0,2.3623D0,2.3579D0,2.3534D0,2.3487D0,2.3438D0 &
      &,2.3387D0,2.3333D0,2.3278D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=ZNSEX(1:N)
      Y(1:N)=ZNSEY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
      CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
      CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
      CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
      CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
   END IF
   IF(MTYPE.EQ.6) THEN
!     INTERPOLATE THE ZNS DATA
      N=59
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR ZNS
!
      DATA (ZNSX(J), J = 1,59)/0.42D0,0.46D0,0.5D0,0.54D0,0.58D0 &
      &,0.62D0,0.66D0,0.70D0,0.74D0,0.78D0,0.82D0,0.86D0,0.90D0 &
      &,0.94D0,0.98D0,1.0D0,1.4D0,1.8D0,2.2D0,2.6D0,3.0D0,3.4D0 &
      &,3.8D0,4.2D0,4.6D0,5.0D0,5.4D0,5.8D0,6.2D0,6.6D0,7.0D0,7.4D0 &
      &,7.8D0,8.2D0,8.6D0,9.0D0,9.4D0,9.8D0,10.2D0,10.6D0,11.0D0 &
      &,11.4D0,11.8D0,12.2D0,12.6D0,13.0D0,13.4D0,13.8D0,14.2D0,14.6D0 &
      &,15.0D0,15.4D0,15.8D0,16.2D0,16.6D0,17.0D0,17.4D0,17.8D0,18.2D0/
!
      DATA (ZNSY(J), J = 1,59)/2.516D0,2.458D0,2.419D0,2.391D0,2.371D0 &
      &,2.355D0,2.342D0,2.332D0,2.323D0,2.316D0,2.310D0,2.305D0,2.301D0 &
      &,2.297D0,2.294D0,2.292D0,2.275D0,2.267D0,2.263D0,2.260D0,2.257D0 &
      &,2.255D0,2.253D0,2.251D0,2.248D0,2.246D0,2.244D0,2.241D0,2.238D0 &
      &,2.235D0,2.232D0,2.228D0,2.225D0,2.221D0,2.217D0,2.212D0,2.208D0 &
      &,2.203D0,2.198D0,2.192D0,2.186D0,2.180D0,2.173D0,2.167D0,2.159D0 &
      &,2.152D0,2.143D0,2.135D0,2.126D0,2.116D0,2.106D0,2.095D0,2.084D0 &
      &,2.072D0,2.059D0,2.045D0,2.030D0,2.015D0,1.998D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=ZNSX(1:N)
      Y(1:N)=ZNSY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
      CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
      CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
      CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
      CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
   END IF
   IF(MTYPE.EQ.7) THEN
!     INTERPOLATE THE CLRTRAN DATA
      N=29
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR CLRTRAN
!
      DATA (CLRX(J), J = 1,29)/0.4047D0,0.4358D0,0.4678D0,0.48D0 &
      &,0.5086D0,0.5461D0,0.5876D0,0.6438D0,0.6678D0,0.7065D0,0.78D0 &
      &,0.7948D0,0.8521D0,0.8943D0,1.014D0,1.1287D0,1.5296D0,2.0581D0 &
      &,3.0D0,3.5D0,4.0D0,4.5D0,5.0D0,8.0D0,9.0D0,10.0D0,11.25D0,12.0D0 &
      &,13.0D0/
!
      DATA (CLRY(J), J = 1,29)/2.54515D0,2.48918D0,2.44915D0,2.43691D0 &
      &,2.41279D0,2.38838D0,2.36789D0,2.34731D0,2.34033D0,2.33073D0 &
      &,2.31669D0,2.31438D0,2.30659D0,2.30183D0,2.29165D0,2.28485D0 &
      &,2.27191D0,2.26442D0,2.25772D0,2.25498D0,2.25231D0,2.24955D0 &
      &,2.24661D0,2.22334D0,2.21290D0,2.20084D0,2.18317D0,2.17101D0 &
      &,2.15252D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=CLRX(1:N)
      Y(1:N)=CLRY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
      CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
      CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
      CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
      CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
   END IF
   IF(MTYPE.EQ.8) THEN
!     INTERPOLATE THE WILLOW RUN FUSED SILICA/SIO2
!
!     CALCULATE THE INDICES
      GPREG(1)=FSIL(GLSWV(1))
      GPREG(2)=FSIL(GLSWV(2))
      GPREG(3)=FSIL(GLSWV(3))
      GPREG(4)=FSIL(GLSWV(4))
      GPREG(5)=FSIL(GLSWV(5))
   END IF
   IF(MTYPE.EQ.9) THEN
!     INTERPOLATE SAPPHIRE
!
!     CALCULATE THE INDICES
      GPREG(1)=SAPH(GLSWV(1))
      GPREG(2)=SAPH(GLSWV(2))
      GPREG(3)=SAPH(GLSWV(3))
      GPREG(4)=SAPH(GLSWV(4))
      GPREG(5)=SAPH(GLSWV(5))
!
   END IF
   IF(MTYPE.EQ.10) THEN
!     INTERPOLATE DYNASIL
!
!     CALCULATE THE INDICES
      GPREG(1)=DYN(GLSWV(1))
      GPREG(2)=DYN(GLSWV(2))
      GPREG(3)=DYN(GLSWV(3))
      GPREG(4)=DYN(GLSWV(4))
      GPREG(5)=DYN(GLSWV(5))
!
   END IF
   IF(MTYPE.EQ.11) THEN
!     INTERPOLATE THE AMTIR1
      N=17
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR AMTIR1
!
      DATA (AMTR1X(J), J = 1,17)/1.0D0,1.064D0,1.5D0,2.0D0,2.4D0 &
      &,3.0D0,4.0D0,5.0D0,6.0D0,7.0D0,8.0D0,9.0D0,10.0D0,11.0D0 &
      &,12.0D0,13.0D0,14.0D0/
!
      DATA (AMTR1Y(J), J = 1,17)/2.6055D0,2.5933D0,2.5469D0,2.5310D0 &
      &,2.5250D0,2.5184D0,2.5146D0,2.5112D0,2.5086D0,2.5062D0,2.5036D0 &
      &,2.5008D0,2.4977D0,2.4942D0,2.4902D0,2.4862D0,2.4825D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=AMTR1X(1:N)
      Y(1:N)=AMTR1Y(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
      CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
      CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
      CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
      CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
   END IF
   IF(MTYPE.EQ.12) THEN
!     INTERPOLATE THE AMTIR3
      N=12
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR AMTIR3
!
      DATA (AMTR3X(J), J = 1,12)/3.0D0,4.0D0,5.0D0,6.0D0,7.0D0,8.0D0 &
      &,9.0D0,10.0D0,11.0D0,12.0D0,13.0D0,14.0D0/
!
      DATA (AMTR3Y(J), J = 1,12)/2.6266D0,2.6210D0,2.6173D0,2.6142D0 &
      &,2.6117D0,2.6088D0,2.6055D0,2.6023D0,2.5983D0,2.5942D0,2.5892D0 &
      &,2.5843D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=AMTR3X(1:N)
      Y(1:N)=AMTR3Y(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
      CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
      CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
      CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
      CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
   END IF
   IF(MTYPE.EQ.13) THEN
!     INTERPOLATE THE As2S3
!
!     CALCULATE THE INDICES
      GPREG(1)=AS2S3(GLSWV(1))
      GPREG(2)=AS2S3(GLSWV(2))
      GPREG(3)=AS2S3(GLSWV(3))
      GPREG(4)=AS2S3(GLSWV(4))
      GPREG(5)=AS2S3(GLSWV(5))
!
   END IF
   IF(MTYPE.EQ.14) THEN
!     INTERPOLATE THE GaAs
      N=13
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR GaAs
!
      DATA (GAASX(J), J = 1,13)/2.5D0,3.0D0,4.0D0,5.0D0,6.0D0 &
      &,7.0D0,8.0D0,9.0D0,10.0D0,11.0D0,12.0D0,13.0D0,14.0D0/
!
      DATA (GAASY(J), J = 1,13)/3.3256D0,3.3169D0,3.3069D0 &
      &,3.3010,3.2963D0,3.2923D0,3.2878D0,3.2830D0,3.2778D0 &
      &,3.2725D0,3.2666D0,3.2589D0,3.2509D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=GAASX(1:N)
      Y(1:N)=GAASY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
      CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
      CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
      CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
      CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
   END IF
   IF(MTYPE.EQ.15) THEN
!     INTERPOLATE THE CdTe
      N=10
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR CdTe
!
      DATA (CDTEX(J), J = 1,10)/3.0D0,4.0D0,5.0D0,6.0D0,7.0D0 &
      &,8.0D0,9.0D0,10.0D0,11.0D0,12.0D0/
!
      DATA (CDTEY(J), J = 1,10)/2.7026D0,2.6971D0,2.6922D0 &
      &,2.6886D0,2.6865D0,2.6846D0,2.6825D0,2.6797D0,2.6766D0 &
      &,2.6749D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=CDTEX(1:N)
      Y(1:N)=CDTEY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
      CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
      CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
      CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
      CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
   END IF
   IF(MTYPE.EQ.16) THEN
!     INTERPOLATE MGF2O
!
!     CALCULATE THE INDICES
      GPREG(1)=MGF2O(GLSWV(1))
      GPREG(2)=MGF2O(GLSWV(2))
      GPREG(3)=MGF2O(GLSWV(3))
      GPREG(4)=MGF2O(GLSWV(4))
      GPREG(5)=MGF2O(GLSWV(5))
!
   END IF
   IF(MTYPE.EQ.17) THEN
!     INTERPOLATE MGF2E
!
!     CALCULATE THE INDICES
      GPREG(1)=MGF2E(GLSWV(1))
      GPREG(2)=MGF2E(GLSWV(2))
      GPREG(3)=MGF2E(GLSWV(3))
      GPREG(4)=MGF2E(GLSWV(4))
      GPREG(5)=MGF2E(GLSWV(5))
!
   END IF
   IF(MTYPE.EQ.18) THEN
!     INTERPOLATE CAF2
!
!     CALCULATE THE INDICES
      GPREG(1)=CAF2(GLSWV(1))
      GPREG(2)=CAF2(GLSWV(2))
      GPREG(3)=CAF2(GLSWV(3))
      GPREG(4)=CAF2(GLSWV(4))
      GPREG(5)=CAF2(GLSWV(5))
!
   END IF
   IF(MTYPE.EQ.19) THEN
!     INTERPOLATE MGO
!
!     CALCULATE THE INDICES
      GPREG(1)=MGO(GLSWV(1))
      GPREG(2)=MGO(GLSWV(2))
      GPREG(3)=MGO(GLSWV(3))
      GPREG(4)=MGO(GLSWV(4))
      GPREG(5)=MGO(GLSWV(5))
!
   END IF
   IF(MTYPE.EQ.20) THEN
!     INTERPOLATE BAF2
!
!     CALCULATE THE INDICES
      GPREG(1)=BAF2(GLSWV(1))
      GPREG(2)=BAF2(GLSWV(2))
      GPREG(3)=BAF2(GLSWV(3))
      GPREG(4)=BAF2(GLSWV(4))
      GPREG(5)=BAF2(GLSWV(5))
!
   END IF
   IF(MTYPE.EQ.21) THEN
!     INTERPOLATE KBR
!
!     CALCULATE THE INDICES
      GPREG(1)=KBR(GLSWV(1))
      GPREG(2)=KBR(GLSWV(2))
      GPREG(3)=KBR(GLSWV(3))
      GPREG(4)=KBR(GLSWV(4))
      GPREG(5)=KBR(GLSWV(5))
!
   END IF
   IF(MTYPE.EQ.22) THEN
!     INTERPOLATE CSI
!
!     CALCULATE THE INDICES
      GPREG(1)=CSI(GLSWV(1))
      GPREG(2)=CSI(GLSWV(2))
      GPREG(3)=CSI(GLSWV(3))
      GPREG(4)=CSI(GLSWV(4))
      GPREG(5)=CSI(GLSWV(5))
!
   END IF
   IF(MTYPE.EQ.23) THEN
!     INTERPOLATE CSBR
!
!     CALCULATE THE INDICES
      GPREG(1)=CSBR(GLSWV(1))
      GPREG(2)=CSBR(GLSWV(2))
      GPREG(3)=CSBR(GLSWV(3))
      GPREG(4)=CSBR(GLSWV(4))
      GPREG(5)=CSBR(GLSWV(5))
!
   END IF
   IF(MTYPE.EQ.24) THEN
!     INTERPOLATE KRS5
!
!     CALCULATE THE INDICES
      GPREG(1)=KRS5(GLSWV(1))
      GPREG(2)=KRS5(GLSWV(2))
      GPREG(3)=KRS5(GLSWV(3))
      GPREG(4)=KRS5(GLSWV(4))
      GPREG(5)=KRS5(GLSWV(5))
!
   END IF
   IF(MTYPE.EQ.25) THEN
!     INTERPOLATE THE LiF
!
!     CALCULATE THE INDICES
      GPREG(1)=LIF(GLSWV(1))
      GPREG(2)=LIF(GLSWV(2))
      GPREG(3)=LIF(GLSWV(3))
      GPREG(4)=LIF(GLSWV(4))
      GPREG(5)=LIF(GLSWV(5))
   END IF
   IF(MTYPE.EQ.26) THEN
!     INTERPOLATE THE NaCl
      N=60
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR NaCl
!
      DATA (NACLX(J), J = 1,60)/0.589D0,0.64D0,0.6874D0,0.7604D0 &
      &,0.7858D0,0.8835D0,0.9033D0,0.9724D0,1.0084D0,1.0540D0 &
      &,1.0810D0,1.1058D0,1.1420D0,1.1786D0,1.2016D0,1.2604D0 &
      &,1.3126D0,1.4874D0,1.5552D0,1.6368D0,1.6848D0,1.7670D0 &
      &,2.0736D0,2.1824D0,2.2464D0,2.3560D0,2.6505D0,2.9466D0 &
      &,3.2736D0,3.5359D0,3.6288D0,3.8192D0,4.1230D0,4.7120D0 &
      &,5.0092D0,5.3009D0,5.8932D0,6.4825D0,6.8000D0,7.0718D0 &
      &,7.2200D0,7.5900D0,7.6611D0,7.9558D0,8.0400D0,8.8398D0 &
      &,9.0000D0,9.5000D0,10.0184D0,11.7864D0,12.5D0,12.9650D0 &
      &,13.50D0,14.1436D0,14.7330D0,15.3223D0,15.9116D0,17.93D0 &
      &,20.57D0,22.3D0/
!
      DATA (NACLY(J), J = 1,60)/1.54427D0,1.54141D0,1.53930D0 &
      &,1.53682D0,1.53607D0,1.53395D0,1.53361D0,1.53253D0,1.53206D0 &
      &,1.53153D0,1.53123D0,1.53098D0,1.53063D0,1.53031D0,1.53014D0 &
      &,1.52971D0,1.52937D0,1.52845D0,1.52815D0,1.52781D0,1.52764D0 &
      &,1.52736D0,1.52649D0,1.52621D0,1.52606D0,1.52579D0,1.52512D0 &
      &,1.52466D0,1.52371D0,1.52312D0,1.52286D0,1.52238D0,1.52156D0 &
      &,1.51979D0,1.51883D0,1.51790D0,1.51593D0,1.51347D0,1.51200D0 &
      &,1.51093D0,1.51020D0,1.50850D0,1.50822D0,1.50665D0,1.50640D0 &
      &,1.50192D0,1.50100D0,1.49980D0,1.49462D0,1.48171D0,1.47568D0 &
      &,1.47160D0,1.46660D0,1.46044D0,1.45427D0,1.44743D0,1.44090D0 &
      &,1.41490D0,1.37350D0,1.34030D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=NACLX(1:N)
      Y(1:N)=NACLY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
      CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
      CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
      CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
      CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
   END IF
   IF(MTYPE.EQ.27) THEN
!     INTERPOLATE THE SiO2o
      N=29
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR SiO2o
!
      DATA (SIO2OX(J), J = 1,29)/0.185D0,0.198D0,0.231D0,0.34D0 &
      &,0.394D0,0.434D0,0.508D0,0.5893D0,0.768D0,0.8325D0,0.9914D0 &
      &,1.1592D0,1.3070D0,1.3958D0,1.4792D0,1.5414D0,1.6815D0 &
      &,1.7614D0,1.9457D0,2.0531D0,2.3D0,2.6D0,3.0D0,3.5D0,4.0D0 &
      &,4.2D0,5.0D0,6.45D0,7.0D0/
!
      DATA (SIO2OY(J), J = 1,29)/1.65751D0,1.65087D0,1.61395D0 &
      &,1.56747D0,1.55846D0,1.55396D0,1.54822D0,1.54424D0,1.53903D0 &
      &,1.53773D0,1.53514D0,1.53283D0,1.53090D0,1.52877D0,1.52865D0 &
      &,1.52781D0,1.52583D0,1.52468D0,1.52184D0,1.52005D0,1.51561D0 &
      &,1.50986D0,1.49953D0,1.48451D0,1.46617D0,1.4569D0,1.417D0,1.274D0 &
      &,1.167D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=SIO2OX(1:N)
      Y(1:N)=SIO2OY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
      CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
      CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
      CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
      CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
   END IF
   IF(MTYPE.EQ.28) THEN
!     INTERPOLATE THE SiO2e
      N=20
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR SiO2e
!
      DATA (SIO2EX(J), J = 1,20)/0.185D0,0.198D0,0.231D0,0.34D0 &
      &,0.394D0,0.434D0,0.508D0,0.5893D0,0.768D0,0.8325D0,0.9914D0 &
      &,1.1592D0,1.3070D0,1.3958D0,1.4792D0,1.5414D0,1.6815D0 &
      &,1.7614D0,1.9457D0,2.0531D0/
!
      DATA (SIO2EY(J), J = 1,20)/1.68988D0,1.66394D0,1.62555D0 &
      &,1.57737D0,1.56805D0,1.56339D0,1.55746D0,1.55335D0,1.54794D0 &
      &,1.54661D0,1.54392D0,1.54152D0,1.53951D0,1.53832D0,1.53716D0 &
      &,1.53630D0,1.53422D0,1.53301D0,1.53004D0,1.52823D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=SIO2EX(1:N)
      Y(1:N)=SIO2EY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
      CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
      CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
      CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
      CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
   END IF
   IF(MTYPE.EQ.29) THEN
!     INTERPOLATE THE VIR3
      N=18
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR VIR3
!
      DATA (VIR3X(J), J = 1,18)/0.4047D0,0.5461D0,0.7065D0,1.0D0 &
      &,1.0D0,2.0D0,2.5D0,3.0D0,3.25D0,3.5D0,3.75D0,4.0D0,4.25D0 &
      &,4.5D0,4.75D0,5.0D0,5.5D0,6.0D0/
!
      DATA (VIR3Y(J), J = 1,18)/1.92568D0,1.87002D0,1.84694D0 &
      &,1.831D0,1.818D0,1.812D0,1.806D0,1.799D0,1.795D0,1.791D0 &
      &,1.786D0,1.781D0,1.775D0,1.769D0,1.762D0,1.756D0,1.741D0,1.725D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=VIR3X(1:N)
      Y(1:N)=VIR3Y(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
      CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
      CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
      CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
      CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
   END IF
   IF(MTYPE.EQ.30) THEN
!     INTERPOLATE THE 9754
      N=43
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR 9754
!
      DATA (G9754X(J), J = 1,43)/0.4D0,0.425D0,0.45D0 &
      &,0.475D0,0.5D0,0.525D0 &
      &,0.55D0,0.575D0,0.6D0,0.635D0,0.65D0,0.675D0,0.7D0,0.725D0,0.75D0 &
      &,0.775D0,0.8D0,0.825D0,0.85D0,0.875D0,0.9D0,0.925D0,0.95D0 &
      &,0.975D0,1.0D0,1.25D0,1.5D0,1.75D0,2.0D0,2.25D0,2.5D0,2.75D0 &
      &,3.0D0,3.25D0,3.5D0,3.75D0,4.0D0,4.25D0,4.5D0,4.75D0,5.0D0 &
      &,5.25D0,5.5D0/
!
      DATA (G9754Y(J), J = 1,43)/1.69093D0,1.68502D0,1.68020D0 &
      &,1.67621D0,1.67285D0,1.67000D0,1.66754D0,1.66542D0,1.66356D0 &
      &,1.66192D0,1.66046D0,1.65916D0,1.65800D0,1.65694D0,1.65599D0 &
      &,1.65511D0,1.65431D0,1.65358D0,1.65289D0,1.65226D0,1.65167D0 &
      &,1.65112D0,1.65060D0,1.65011D0,1.64964D0,1.64595D0,1.64310D0 &
      &,1.64049D0,1.63785D0,1.63505D0,1.63203D0,1.62874D0,1.62514D0 &
      &,1.62119D0,1.61686D0,1.61214D0,1.60698D0,1.60135D0,1.59521D0 &
      &,1.58853D0,1.58125D0,1.57332D0,1.66469D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=G9754X(1:N)
      Y(1:N)=G9754Y(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
      CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
      CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
      CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
      CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
   END IF
   IF(MTYPE.EQ.31) THEN
!     INTERPOLATE THE ALON
      N=14
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR ALON
!
      DATA (ALONX(J), J = 1,14)/0.365D0,0.405D0,0.435D0,0.546D0 &
      &,0.852D0,1.014D0,1.53D0,1.97D0,2.325D0,2.8D0,3.39D0,4.0D0 &
      &,4.6D0,5.0D0/
!
      DATA (ALONY(J), J = 1,14)/1.819D0,1.811D0,1.806D0,1.792D0 &
      &,1.778D0,1.773D0,1.765D0,1.758D0,1.752D0,1.743D0,1.729D0 &
      &,1.710D0,1.689D0,1.672D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=ALONX(1:N)
      Y(1:N)=ALONY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
      CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
      CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
      CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
      CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
   END IF
   IF(MTYPE.EQ.32) THEN
!     INTERPOLATE THE SPINEL
      N=11
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR SPINEL
!
      DATA (SPINX(J), J = 1,11)/0.4047D0,0.4358D0,0.5461D0,0.8521D0 &
      &,1.014D0,1.53D0,1.97D0,3.0D0,4.0D0,5.0D0,5.5D0/
!
      DATA (SPINY(J), J = 1,11)/1.73574D0,1.73054D0,1.71896D0 &
      &,1.70728D0,1.703D0,1.69468D0,1.68763D0,1.6647D0,1.6414D0 &
      &,1.5978D0,1.5719D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=SPINX(1:N)
      Y(1:N)=SPINY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
      CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
      CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
      CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
      CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
   END IF
   IF(MTYPE.EQ.33) THEN
!     INTERPOLATE THE CALCIUM ALUMINATE GLASS
      N=12
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     DATA FOR CALAL
!
      DATA (CALALX(J), J = 1,12)/0.4861D0,0.5893D0,0.6563D0,0.8D0 &
      &,1.0D0,1.5D0,2.0D0,2.5D0,3.0D0,3.5D0,4.0D0,4.5D0/
!
      DATA (CALALY(J), J = 1,12)/1.6794D0,1.669D0,1.6647D0,1.6588D0 &
      &,1.6538D0,1.6463D0,1.6403D0,1.6341D0,1.6266D0,1.6180D0 &
      &,1.6074D0,1.5952D0/
!
!     MOVE TO LARGE ARRAYS
      X(1:N)=CALALX(1:N)
      Y(1:N)=CALALY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
      CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
      CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
      CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
      CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
      IF(MTYPE.EQ.34) THEN
!     INTERPOLATE THE B270 GLASS
         N=8
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
         YP1=1.0D35
         YPN=1.0D35
!
!     DATA FOR B270
!
         DATA (B270X(J), J = 1,8)/0.4358343D0,0.4799914D0,0.4861327D0 &
         &,0.5460740D0,0.5875618D0,0.5892938D0,0.6438469D0,0.6562725D0/
!
         DATA (B270Y(J), J = 1,8)/1.534D0,1.5297D0,1.5292D0,1.5251D0 &
         &,11.523D0,1.5229D0,1.5207D0,1.5202D0/
!
!     MOVE TO LARGE ARRAYS
         X(1:N)=B270X(1:N)
         Y(1:N)=B270Y(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
         CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
         CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
         CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
         CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
         CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
         CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
      END IF
      IF(MTYPE.EQ.35) THEN
!     INTERPOLATE THE IRG2 GLASS
         N=18
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
         YP1=1.0D35
         YPN=1.0D35
!
!     DATA FOR IRG2
!
         DATA (IRG2X(J), J = 1,18)/0.365D0,0.4047D0,0.48D0,0.4861D0 &
         &,0.5461D0,0.5876D0,0.6438D0,0.6563D0,0.7065D0,0.8521D0,1.014D0 &
         &,1.5296D0,1.9701D0,2.3254D0,2.674D0,3.303D0,4.258D0,4.586D0/
!
         DATA (IRG2Y(J), J = 1,18)/1.9750D0,1.9462D0,1.9147D0,1.9129D0 &
         &,1.8988D0,1.8918D0,1.8845D0,1.8832D0,1.8785D0,1.8692D0,1.8630D0 &
         &,1.8526D0,1.8464D0,1.8414D0,1.8362D0,1.8253D0,1.8041D0,1.7954D0/
!
!     MOVE TO LARGE ARRAYS
         X(1:N)=IRG2X(1:N)
         Y(1:N)=IRG2Y(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
         CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
         CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
         CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
         CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
         CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
         CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
      END IF
      IF(MTYPE.EQ.36) THEN
!     INTERPOLATE THE IRG3 GLASS
         N=17
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
         YP1=1.0D35
         YPN=1.0D35
!
!     DATA FOR IRG3
!
         DATA (IRG3X(J), J = 1,17)/0.4047D0,0.48D0,0.4861D0 &
         &,0.5461D0,0.5876D0,0.6438D0,0.6563D0,0.7065D0,0.8521D0,1.014D0 &
         &,1.5296D0,1.9701D0,2.3254D0,2.674D0,3.303D0,4.258D0,4.586D0/
!
         DATA (IRG3Y(J), J = 1,17)/1.8925D0,1.8649D0,1.8633D0,1.8510D0 &
         &,1.8449D0,1.8385D0,1.8373D0,1.8331D0,1.8249D0,1.8193D0,1.8089D0 &
         &,1.8021D0,1.7963D0,1.7900D0,1.7764D0,1.7491D0,1.7375D0/
!
!     MOVE TO LARGE ARRAYS
         X(1:N)=IRG3X(1:N)
         Y(1:N)=IRG3Y(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
         CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
         CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
         CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
         CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
         CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
         CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
      END IF
      IF(MTYPE.EQ.37) THEN
!     INTERPOLATE THE IRGN6 GLASS
         N=16
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
         YP1=1.0D35
         YPN=1.0D35
!
!     DATA FOR IRGN6
!
         DATA (IRGN6X(J), J = 1,16)/0.4047D0,0.48D0,0.4861D0 &
         &,0.5461D0,0.5876D0,0.6438D0,0.6563D0,0.7065D0,0.8521D0,1.014D0 &
         &,1.5296D0,1.9701D0,2.3254D0,2.674D0,3.303D0,4.258D0/
!
         DATA (IRGN6Y(J), J = 1,16)/1.6069D0,1.5971D0,1.5965D0,1.5915D0 &
         &,1.5892D0,1.5863D0,1.5857D0,1.5842D0,1.5807D0,1.5777D0,1.5716D0 &
         &,1.5667D0,1.5620D0,1.5567D0,1.5451D0,1.5209D0/
!
!     MOVE TO LARGE ARRAYS
         X(1:N)=IRGN6X(1:N)
         Y(1:N)=IRGN6Y(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
         CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
         CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
         CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
         CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
         CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
         CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
      END IF
      IF(MTYPE.EQ.38) THEN
!     INTERPOLATE THE IRG7 GLASS
         N=16
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
         YP1=1.0D35
         YPN=1.0D35
!
!     DATA FOR IRG7
!
         DATA (IRG7X(J), J = 1,16)/0.365D0,0.4047D0,0.48D0,0.4861D0 &
         &,0.5461D0,0.5876D0,0.6438D0,0.6563D0,0.7065D0,0.8521D0,1.014D0 &
         &,1.5296D0,1.9701D0,2.3254D0,2.674D0,3.303D0/
!
         DATA (IRG7Y(J), J = 1,16)/1.5983D0,1.5871D0,1.5743D0,1.5735D0 &
         &,1.5675D0,1.5644D0,1.5612D0,1.5606D0,1.5585D0,1.5541D0,1.5509D0 &
         &,1.5442D0,1.5389D0,1.5341D0,1.5286D0,1.5164D0/
!
!     MOVE TO LARGE ARRAYS
         X(1:N)=IRG7X(1:N)
         Y(1:N)=IRG7Y(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
         CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
         CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
         CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
         CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
         CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
         CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
      END IF
      IF(MTYPE.EQ.39) THEN
!     INTERPOLATE THE IRG9 GLASS
         N=16
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
         YP1=1.0D35
         YPN=1.0D35
!
!     DATA FOR IRG9
!
         DATA (IRG9X(J), J = 1,16)/0.365D0,0.4047D0,0.48D0,0.4861D0 &
         &,0.5461D0,0.5876D0,0.6438D0,0.6563D0,0.7065D0,0.8521D0,1.014D0 &
         &,1.5296D0,1.9701D0,2.3254D0,2.674D0,3.303D0/
!
         DATA (IRG9Y(J), J = 1,16)/1.5005D0,1.4961D0,1.4905D0,1.4902D0 &
         &,1.4875D0,1.4861D0,1.4845D0,1.4842D0,1.4832D0,1.4810D0,1.4793D0 &
         &,1.4755D0,1.4722D0,1.4692D0,1.4658D0,1.4583D0/
!
!     MOVE TO LARGE ARRAYS
         X(1:N)=IRG9X(1:N)
         Y(1:N)=IRG9Y(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
         CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
         CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
         CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
         CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
         CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
         CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
      END IF
      IF(MTYPE.EQ.40) THEN
!     INTERPOLATE THE IRG11 GLASS
         N=16
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
         YP1=1.0D35
         YPN=1.0D35
!
!     DATA FOR IRG11
!
         DATA (IRG11X(J), J = 1,16)/0.48D0,0.4861D0 &
         &,0.5461D0,0.5876D0,0.6438D0,0.6563D0,0.7065D0,0.8521D0,1.014D0 &
         &,1.5296D0,1.9701D0,2.3254D0,2.674D0,3.303D0,4.258D0,4.586D0/
!
         DATA (IRG11Y(J), J = 1,16)/1.6926D0,1.6917D0,1.6845D0,1.6809D0 &
         &,1.6770D0,1.6763D0,1.6741D0,1.6686D0,1.6650D0,1.6581D0,1.6532D0 &
         &,1.6491D0,1.6445D0,1.6349D0,1.6158D0,1.6077D0/
!
!     MOVE TO LARGE ARRAYS
         X(1:N)=IRG11X(1:N)
         Y(1:N)=IRG11Y(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
         CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
         CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
         CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
         CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
         CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
         CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
      END IF
      IF(MTYPE.EQ.41) THEN
!     INTERPOLATE THE IRG15 GLASS
         N=16
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
         YP1=1.0D35
         YPN=1.0D35
!
!     DATA FOR IRG15
!
         DATA (IRG15X(J), J = 1,16)/0.365D0,0.4047D0,0.48D0,0.4861D0 &
         &,0.5461D0,0.5876D0,0.6438D0,0.6563D0,0.7065D0,0.8521D0,1.014D0 &
         &,1.5296D0,1.9701D0,2.3254D0,2.674D0,3.303D0/
!
         DATA (IRG15Y(J), J = 1,16)/1.5883D0,1.5506D0,1.5415D0,1.5410D0 &
         &,1.5366D0,1.5343D0,1.5318D0,1.5314D0,1.5297D0,1.5263D0,1.5237D0 &
         &,1.5179D0,1.5131D0,1.5086D0,1.5038D0,1.4924D0/
!
!     MOVE TO LARGE ARRAYS
         X(1:N)=IRG15X(1:N)
         Y(1:N)=IRG15Y(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
         CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
         CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
         CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
         CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
         CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
         CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
      END IF
      IF(MTYPE.EQ.42) THEN
!     INTERPOLATE THE VAC GLASS
         A=28.79D-5
         B=5.67D-5
         IF(GLSWV(1).NE.0.0D0) THEN
            LA1=LM1(A,B,GLSWV(1))
            LA1=1.0D0+LA1
            GPREG(1)=1.0D0/LA1
         ELSE
            GPREG(1)=1.0D0
         END IF
         IF(GLSWV(2).NE.0.0D0) THEN
            LA2=LM1(A,B,GLSWV(2))
            LA2=1.0D0+LA2
            GPREG(2)=1.0D0/LA2
         ELSE
            GPREG(2)=1.0D0
         END IF
         IF(GLSWV(3).NE.0.0D0) THEN
            LA3=LM1(A,B,GLSWV(3))
            LA3=1.0D0+LA3
            GPREG(3)=1.0D0/LA3
         ELSE
            GPREG(3)=1.0D0
         END IF
         IF(GLSWV(4).NE.0.0D0) THEN
            LA4=LM1(A,B,GLSWV(4))
            LA4=1.0D0+LA4
            GPREG(4)=1.0D0/LA4
         ELSE
            GPREG(4)=1.0D0
         END IF
         IF(GLSWV(5).NE.0.0D0) THEN
            LA5=LM1(A,B,GLSWV(5))
            LA5=1.0D0+LA5
            GPREG(5)=1.0D0/LA5
         ELSE
            GPREG(5)=1.0D0
         END IF
!
      END IF
!     DATA FOR H2O
      IF(MTYPE.EQ.43) THEN
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
         YP1=1.0D35
         YPN=1.0D35
         N=21
!
         DATA (H2OX(J), J = 1,21)/0.1829D0,0.20255D0,0.25020D0,0.30822D0,&
         &0.35871D0,0.40466D0,0.44715D0,0.50157D0,0.54607D0,0.58926D0,&
         &0.65628D0,0.70652D0,0.76820D0,0.808D0,0.871D0,0.943D0,1.028D0,&
         &1.130D0,1.256D0,1.617D0,1.968/
!
         DATA (H2OY(J), J = 1,21)/1.46379D0,1.41993D0,1.37734D0,1.35671D0,&
         &1.34795D0,1.342724D0,1.339423D0,1.336363D0,1.334466D0,1.332988D0,&
         &1.331151D0,1.330019D0,1.32890D0,1.3286D0,1.3273D0,1.3262D0,&
         &1.3250D0,1.3234D0,1.3215D0,1.3149D0,1.3078D0/
!
!     MOVE TO LARGE ARRAYS
         X(1:N)=H2OX(1:N)
         Y(1:N)=H2OY(1:N)
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
         CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
         CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
         CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
         CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
         CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
         CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
      END IF
      IF(MTYPE.EQ.44) THEN
!     INTERPOLATE THE SUPRASIL
         N=60
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
         YP1=1.0D35
         YPN=1.0D35
!
!     DATA FOR SUPRASIL
!
         DATA (SUPX(J), J = 1,60)/0.19D0,0.20D0,0.21D0,0.22D0,&
         &0.23D0,0.24D0,0.25D0,0.26D0,0.27D0,0.28D0,&
         &0.29D0,0.30D0,0.32D0,0.34D0,0.36D0,0.36548D0,0.38D0,&
         &0.4D0,0.40465D0,0.43583D0,0.45D0,0.48613D0,0.5D0,0.54607D0,&
         &0.55D0,0.58756D0,0.6D0,0.65D0,0.65627D0,0.7D0,0.75D0,0.8D0,&
         &0.85D0,0.9D0,1.0D0,1.1D0,1.2D0,1.3D0,1.4D0,1.5D0,1.6D0,1.7D0,&
         &1.8D0,1.9D0,2.0D0,2.1D0,2.2D0,2.3D0,2.4D0,2.5D0,2.6D0,2.7D0,&
         &2.8D0,2.9D0,3.0D0,3.1D0,3.2D0,3.3D0,3.4D0,3.5D0/
!
         DATA (SUPY(J), J = 1,60)/&
         &1.56572D0,1.55051D0,1.53836D0,1.52845D0,1.52024D0,1.51333D0,&
         &1.50745D0,1.50239D0,1.49800D0,1.49416D0,1.49079D0,1.48779D0,&
         &1.48274D0,1.47865D0,1.47529D0,1.47447D0,1.47248D0,1.47012D0,&
         &1.46962D0,1.46669D0,1.46557D0,1.46313D0,1.46233D0,1.46008D0,&
         &1.45991D0,1.45846D0,1.45804D0,1.45653D0,1.45637D0,1.45529D0,&
         &1.45424D0,1.45332D0,1.45250D0,1.45175D0,1.45042D0,1.44920D0,&
         &1.44805D0,1.44692D0,1.44758D0,1.44462D0,1.44342D0,1.44217D0,&
         &1.44087D0,1.43951D0,1.43809D0,1.43659D0,1.43501D0,1.43336D0,&
         &1.43163D0,1.42980D0,1.42789D0,1.42588D0,1.42377D0,1.42156D0,&
         &1.41925D0,1.41682D0,1.41427D0,1.41161D0,1.40881D0,1.40589D0/
!
!     MOVE TO LARGE ARRAYS
         X(1:N)=SUPX(1:N)
         Y(1:N)=SUPY(1:N)
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
         CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
         CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
         CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
         CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
         CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
         CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
      END IF
      IF(MTYPE.EQ.45) THEN
!     INTERPOLATE THE HOMOSIL
         N=47
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
         YP1=1.0D35
         YPN=1.0D35
!
!     DATA FOR SUPRASIL
!
         DATA (HOMOX(J), J = 1,47)/&
         &0.34D0,0.36D0,0.36548D0,0.38D0,&
         &0.4D0,0.40465D0,0.43583D0,0.45D0,0.48613D0,0.5D0,0.54607D0,&
         &0.55D0,0.58756D0,0.6D0,0.65D0,0.65627D0,0.7D0,0.75D0,0.8D0,&
         &0.85D0,0.9D0,1.0D0,1.1D0,1.2D0,1.3D0,1.4D0,1.5D0,1.6D0,1.7D0,&
         &1.8D0,1.9D0,2.0D0,2.1D0,2.2D0,2.3D0,2.4D0,2.5D0,2.6D0,2.7D0,&
         &2.8D0,2.9D0,3.0D0,3.1D0,3.2D0,3.3D0,3.4D0,3.5D0/
!
         DATA (HOMOY(J), J = 1,47)/1.47881D0,1.47544D0,1.47462D0,&
         &1.47262D0,1.47025D0,1.46975D0,1.46681D0,1.46568D0,1.46324D0,&
         &1.46243D0,1.46018D0,1.46001D0,1.45856D0,1.45814D0,1.45663D0,&
         &1.45646D0,1.45539D0,1.45433D0,1.45341D0,1.45259D0,1.45185D0,&
         &1.45051D0,1.44930D0,1.44815D0,1.44702D0,1.44589D0,1.44473D0,&
         &1.44353D0,1.44229D0,1.44099D0,1.43964D0,1.43821D0,1.43672D0,&
         &1.43515D0,1.43350D0,1.43177D0,1.42995D0,1.42804D0,1.42604D0,&
         &1.42393D0,1.42172D0,1.41941D0,1.41698D0,1.41444D0,1.41177D0,&
         &1.40897D0,1.40605D0/
!
!     MOVE TO LARGE ARRAYS
         X(1:N)=HOMOX(1:N)
         Y(1:N)=HOMOY(1:N)
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
         CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
         CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
         CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
         CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
         CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
         CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
      END IF
      IF(MTYPE.EQ.46) THEN
!     INTERPOLATE THE ZNS-MS II-VI DATA
         N=59
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
         YP1=1.0D35
         YPN=1.0D35
!
!     DATA FOR ZNS-MS II-VI DATA
!
         DATA (ZNSMSX(J), J = 1,59)/&
         &0.42D0,0.46D0,0.50D0,0.54D0,0.58D0,0.62D0,0.66D0,0.70D0,&
         &0.74D0,0.78D0,0.82D0,0.86D0,0.90D0,0.94D0,0.98D0,1.00D0,&
         &1.40D0,1.80D0,2.20D0,2.60D0,3.00D0,3.40D0,3.80D0,4.20D0,&
         &4.60D0,5.00D0,5.40D0,5.80D0,6.20D0,6.60D0,7.00D0,7.40D0,&
         &7.80D0,8.20D0,8.60D0,9.00D0,9.40D0,9.80D0,10.20D0,10.60D0,&
         &11.00D0,11.40D0,11.80D0,12.20D0,12.60D0,13.0D0,13.40D0,13.80D0,&
         &14.20D0,14.60D0,15.00D0,15.40D0,15.80D0,16.2D0,16.60D0,17.00D0,&
         &17.40D0,17.80D0,18.20D0/
!
         DATA (ZNSMSY(J), J = 1,59)/&
         &2.516D0,2.458D0,2.419D0,2.391D0,2.371D0,2.355D0,2.342D0,2.332D0,&
         &2.323D0,2.316D0,2.310D0,2.305D0,2.301D0,2.297D0,2.294D0,2.292D0,&
         &2.275D0,2.267D0,2.263D0,2.260D0,2.257D0,2.255D0,2.253D0,2.251D0,&
         &2.248D0,2.246D0,2.244D0,2.241D0,2.238D0,2.235D0,2.232D0,2.228D0,&
         &2.225D0,2.221D0,2.217D0,2.212D0,2.208D0,2.203D0,2.198D0,2.192D0,&
         &2.186D0,2.180D0,2.173D0,2.167D0,2.159D0,2.152D0,2.143D0,2.135D0,&
         &2.126D0,2.116D0,2.106D0,2.095D0,2.084D0,2.072D0,2.059D0,2.045D0,&
         &2.030D0,2.015D0,1.998D0/
!
!     MOVE TO LARGE ARRAYS
         X(1:N)=ZNSMSX(1:N)
         Y(1:N)=ZNSMSY(1:N)
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
         CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
         CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
         CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
         CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
         CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
         CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
      END IF
   END IF
!
!
   IF(MTYPE.EQ.47) THEN
!     INTERPOLATE THE CEF3
!
!     CALCULATE THE INDICES
      IF(GLSWV(1).NE.0.0D0) THEN
         GPREG(1)&
         &=(((1.63D0-1.59D0)/(0.55D0-2.0D0))*(GLSWV(1)-2.0D0))+1.59D0
         IF(GLSWV(1).LE.0.55D0) GPREG(1)=1.63D0
         IF(GLSWV(1).GE.2.00D0) GPREG(1)=1.59D0
      ELSE
         GPREG(1)=1.0D0
      END IF
      IF(GLSWV(2).NE.0.0D0) THEN
         GPREG(2)&
         &=(((1.63D0-1.59D0)/(0.55D0-2.0D0))*(GLSWV(2)-2.0D0))+1.59D0
         IF(GLSWV(2).LE.0.55D0) GPREG(2)=1.63D0
         IF(GLSWV(2).GE.2.00D0) GPREG(2)=1.59D0
      ELSE
         GPREG(2)=1.0D0
      END IF
      IF(GLSWV(3).NE.0.0D0) THEN
         GPREG(3)&
         &=(((1.63D0-1.59D0)/(0.55D0-2.0D0))*(GLSWV(3)-2.0D0))+1.59D0
         IF(GLSWV(3).LE.0.55D0) GPREG(3)=1.63D0
         IF(GLSWV(3).GE.2.00D0) GPREG(3)=1.59D0
      ELSE
         GPREG(3)=1.0D0
      END IF
      IF(GLSWV(4).NE.0.0D0) THEN
         GPREG(4)&
         &=(((1.63D0-1.59D0)/(0.55D0-2.0D0))*(GLSWV(4)-2.0D0))+1.59D0
         IF(GLSWV(4).LE.0.55D0) GPREG(4)=1.63D0
         IF(GLSWV(4).GE.2.00D0) GPREG(4)=1.59D0
      ELSE
         GPREG(4)=1.0D0
      END IF
      IF(GLSWV(5).NE.0.0D0) THEN
         GPREG(5)&
         &=(((1.63D0-1.59D0)/(0.55D0-2.0D0))*(GLSWV(5)-2.0D0))+1.59D0
         IF(GLSWV(5).LE.0.55D0) GPREG(5)=1.63D0
         IF(GLSWV(5).GE.2.00D0) GPREG(5)=1.59D0
      ELSE
         GPREG(5)=1.0D0
      END IF
!
   END IF
   IF(MTYPE.EQ.48) THEN
!     INTERPOLATE THE LS203
!
!     CALCULATE THE INDICES
      IF(GLSWV(1).NE.0.0D0) THEN
         GPREG(1)&
         &=(((1.95D0-1.86D0)/(0.55D0-2.0D0))*(GLSWV(1)-2.0D0))+1.86D0
         IF(GLSWV(1).LE.0.55D0) GPREG(1)=1.95D0
         IF(GLSWV(1).GE.2.00D0) GPREG(1)=1.86D0
      ELSE
         GPREG(1)=1.0D0
      END IF
      IF(GLSWV(2).NE.0.0D0) THEN
         GPREG(2)&
         &=(((1.95D0-1.86D0)/(0.55D0-2.0D0))*(GLSWV(2)-2.0D0))+1.86D0
         IF(GLSWV(2).LE.0.55D0) GPREG(2)=1.95D0
         IF(GLSWV(2).GE.2.00D0) GPREG(2)=1.86D0
      ELSE
         GPREG(2)=1.0D0
      END IF
      IF(GLSWV(3).NE.0.0D0) THEN
         GPREG(3)&
         &=(((1.95D0-1.86D0)/(0.55D0-2.0D0))*(GLSWV(3)-2.0D0))+1.86D0
         IF(GLSWV(3).LE.0.55D0) GPREG(3)=1.95D0
         IF(GLSWV(3).GE.2.00D0) GPREG(3)=1.86D0
      ELSE
         GPREG(3)=1.0D0
      END IF
      IF(GLSWV(4).NE.0.0D0) THEN
         GPREG(4)&
         &=(((1.95D0-1.86D0)/(0.55D0-2.0D0))*(GLSWV(4)-2.0D0))+1.86D0
         IF(GLSWV(4).LE.0.55D0) GPREG(4)=1.95D0
         IF(GLSWV(4).GE.2.00D0) GPREG(4)=1.86D0
      ELSE
         GPREG(4)=1.0D0
      END IF
      IF(GLSWV(5).NE.0.0D0) THEN
         GPREG(5)&
         &=(((1.95D0-1.86D0)/(0.55D0-2.0D0))*(GLSWV(5)-2.0D0))+1.86D0
         IF(GLSWV(5).LE.0.55D0) GPREG(5)=1.95D0
         IF(GLSWV(5).GE.2.00D0) GPREG(5)=1.86D0
      ELSE
         GPREG(5)=1.0D0
      END IF
!
   END IF
   IF(MTYPE.EQ.49) THEN
!     INTERPOLATE THE THF4
!
!     CALCULATE THE INDICES
      IF(GLSWV(1).NE.0.0D0) THEN
         GPREG(1)&
         &=(((1.52D0-1.51D0)/(0.40D0-0.75D0))*(GLSWV(1)-.75D0))+1.51D0
         IF(GLSWV(1).LE.0.40D0) GPREG(1)=1.52D0
         IF(GLSWV(1).GE.0.75D0) GPREG(1)=1.51D0
      ELSE
         GPREG(1)=1.0D0
      END IF
      IF(GLSWV(2).NE.0.0D0) THEN
         GPREG(2)&
         &=(((1.52D0-1.51D0)/(0.40D0-0.75D0))*(GLSWV(2)-.75D0))+1.51D0
         IF(GLSWV(2).LE.0.40D0) GPREG(2)=1.52D0
         IF(GLSWV(2).GE.0.75D0) GPREG(2)=1.51D0
      ELSE
         GPREG(2)=1.0D0
      END IF
      IF(GLSWV(3).NE.0.0D0) THEN
         GPREG(3)&
         &=(((1.52D0-1.51D0)/(0.40D0-0.75D0))*(GLSWV(3)-.75D0))+1.51D0
         IF(GLSWV(3).LE.0.40D0) GPREG(3)=1.52D0
         IF(GLSWV(3).GE.0.75D0) GPREG(3)=1.51D0
      ELSE
         GPREG(3)=1.0D0
      END IF
      IF(GLSWV(4).NE.0.0D0) THEN
         GPREG(4)&
         &=(((1.52D0-1.51D0)/(0.40D0-0.75D0))*(GLSWV(4)-.75D0))+1.51D0
         IF(GLSWV(4).LE.0.40D0) GPREG(4)=1.52D0
         IF(GLSWV(4).GE.0.75D0) GPREG(4)=1.51D0
      ELSE
         GPREG(4)=1.0D0
      END IF
      IF(GLSWV(5).NE.0.0D0) THEN
         GPREG(5)&
         &=(((1.52D0-1.51D0)/(0.40D0-0.75D0))*(GLSWV(5)-.75D0))+1.51D0
         IF(GLSWV(5).LE.0.40D0) GPREG(5)=1.52D0
         IF(GLSWV(5).GE.0.75D0) GPREG(5)=1.51D0
      ELSE
         GPREG(5)=1.0D0
      END IF
!
   END IF
   IF(MTYPE.EQ.50) THEN
!     INTERPOLATE THE ZRO2
!
!     CALCULATE THE INDICES
      IF(GLSWV(1).NE.0.0D0) THEN
         GPREG(1)&
         &=(((2.10D0-2.00D0)/(0.55D0-2.0D0))*(GLSWV(1)-.75D0))+2.00D0
         IF(GLSWV(1).LE.0.55D0) GPREG(1)=2.10D0
         IF(GLSWV(1).GE..75D0) GPREG(1)=2.00D0
      ELSE
         GPREG(1)=1.0D0
      END IF
      IF(GLSWV(2).NE.0.0D0) THEN
         GPREG(2)&
         &=(((2.10D0-2.00D0)/(0.55D0-2.0D0))*(GLSWV(2)-.75D0))+2.00D0
         IF(GLSWV(2).LE.0.55D0) GPREG(2)=2.10D0
         IF(GLSWV(2).GE..75D0) GPREG(2)=2.00D0
      ELSE
         GPREG(2)=1.0D0
      END IF
      IF(GLSWV(3).NE.0.0D0) THEN
         GPREG(3)&
         &=(((2.10D0-2.00D0)/(0.55D0-2.0D0))*(GLSWV(3)-.75D0))+2.00D0
         IF(GLSWV(3).LE.0.55D0) GPREG(3)=2.10D0
         IF(GLSWV(3).GE..75D0) GPREG(3)=2.00D0
      ELSE
         GPREG(3)=1.0D0
      END IF
      IF(GLSWV(4).NE.0.0D0) THEN
         GPREG(4)&
         &=(((2.10D0-2.00D0)/(0.55D0-2.0D0))*(GLSWV(4)-.75D0))+2.00D0
         IF(GLSWV(4).LE.0.55D0) GPREG(4)=2.10D0
         IF(GLSWV(4).GE..75D0) GPREG(4)=2.00D0
      ELSE
         GPREG(4)=1.0D0
      END IF
      IF(GLSWV(5).NE.0.0D0) THEN
         GPREG(5)&
         &=(((2.10D0-2.00D0)/(0.55D0-2.0D0))*(GLSWV(5)-.75D0))+2.00D0
         IF(GLSWV(5).LE.0.55D0) GPREG(5)=2.10D0
         IF(GLSWV(5).GE..75D0) GPREG(5)=2.00D0
      ELSE
         GPREG(5)=1.0D0
      END IF
!
   END IF
   IF(MTYPE.EQ.51) THEN
!     INTERPOLATE THE DIAMOND
!
!     CALCULATE THE INDICES
      GPREG(1)=DIAMOND(GLSWV(1))
      GPREG(2)=DIAMOND(GLSWV(2))
      GPREG(3)=DIAMOND(GLSWV(3))
      GPREG(4)=DIAMOND(GLSWV(4))
      GPREG(5)=DIAMOND(GLSWV(5))
   END IF
   IF(MTYPE.EQ.52) THEN
!     INTERPOLATE THE YAG
!
!     CALCULATE THE INDICES
      GPREG(1)=YAG(GLSWV(1))
      GPREG(2)=YAG(GLSWV(2))
      GPREG(3)=YAG(GLSWV(3))
      GPREG(4)=YAG(GLSWV(4))
      GPREG(5)=YAG(GLSWV(5))
   END IF
   IF(MTYPE.GE.101.AND.MTYPE.LE.104) THEN
!
!     PLASTICS
!
      DATA (GPLX(J), J = 1,13)/0.36501D0,0.40466D0,0.43484D0 &
      &,0.47999D0,0.48613D0,0.54607D0,0.58756D0,0.58929D0,0.64385D0 &
      &,0.65627D0,0.70652D0,0.85211D0,1.01398D0/
!
      DATA (GPL1Y(J), J = 1,13)/1.513613D0,1.506607D0,1.502557D0 &
      &,1.498258D0,1.497760D0,1.493795D0,1.491757D0,1.491681D0 &
      &,1.489603D0,1.489201D0,1.487552D0,1.484965D0,1.483115D0/
!
      DATA (GPL2Y(J), J = 1,13)/1.643126D0,1.625341D0,1.615466D0 &
      &,1.605241D0,1.604079D0,1.595010D0,1.590481D0,1.590315D0 &
      &,1.585808D0,1.584949D0,1.581954D0,1.576196D0,1.572553D0/
!
      DATA (GPL3Y(J), J = 1,13)/1.643231D0,1.622447D0,1.611519D0 &
      &,1.600654D0,1.599439D0,1.590081D0,1.585470D0,1.585302D0 &
      &,1.580734D0,1.579864D0,1.576831D0,1.570981D0,1.567248D0/
!
      DATA (GPL4Y(J), J = 1,13)/1.612490D0,1.597075D0,1.588640D0 &
      &,1.579985D0,1.579000D0,1.571300D0,1.567400D0,1.567298D0 &
      &,1.563438D0,1.562700D0,1.560119D0,1.555108D0,1.551870D0/
!
      N=13
!
      IF(MTYPE.EQ.101) THEN
!     INTERPOLATE THE ACRYLIC DATA
         X(1:N)=GPLX(1:N)
         Y(1:N)=GPL1Y(1:N)
      END IF
      IF(MTYPE.EQ.102) THEN
!     INTERPOLATE THE POLYSTYRENE DATA
         X(1:N)=GPLX(1:N)
         Y(1:N)=GPL2Y(1:N)
      END IF
      IF(MTYPE.EQ.103) THEN
!     INTERPOLATE THE POLYCARBONATE DATA
         X(1:N)=GPLX(1:N)
         Y(1:N)=GPL3Y(1:N)
      END IF
      IF(MTYPE.EQ.104) THEN
!     INTERPOLATE THE (SAN) DATA
         X(1:N)=GPLX(1:N)
         Y(1:N)=GPL4Y(1:N)
      END IF
!     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
      YP1=1.0D35
      YPN=1.0D35
!
!     MOVE TO LARGE ARRAYS
!
!     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
      CALL SPLINE(X,Y,N,YP1,YPN,Y2)
!
!     CALCULATE THE INDICES
      CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
      CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
      CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
      CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
      CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
!
   END IF
   RETURN
END
