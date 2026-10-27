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
