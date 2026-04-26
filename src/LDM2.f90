! SUB THERM.FOR
SUBROUTINE THERM
!
   use DATLEN
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
   INTEGER I
!
   LOGICAL GONOGO
!
   REAL*8 FACTOR,DSGN
!
   EXTERNAL DSGN
!
!
   IF(SST.EQ.1) THEN
      WRITE(OUTLYNE,*)'"THERM" TAKES NO STRING INPUT'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
   IF(STI.EQ.1) THEN
      WRITE(OUTLYNE,*)'NO ADDITIONAL INFORMATION'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
   IF(S5.EQ.1) THEN
      WRITE(OUTLYNE,*)'"THERM" TAKES NO NUMERIC WORD #5 INPUT'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
   IF(SQ.EQ.0) THEN
      WRITE(OUTLYNE,*)'"THERM" REQUIRES EXPLICIT QUALIFIER INPUT'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
   IF(WQ.NE.'SHAPE'.AND.WQ.NE.'GLASS'.AND.WQ.NE.'THICK'.AND.WQ.NE.'SPACE') THEN
      WRITE(OUTLYNE,*)'INVALID QUALIFIER USED WITH "THERM"'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(DF1.EQ.1) W1=0.0D0
   IF(DF2.EQ.1) W2=SYSTEM(20)
   IF(DF3.EQ.1.OR.DF4.EQ.1) THEN
      WRITE(OUTLYNE,*)'"THERM" REQUIRES EXPLICIT NUMERIC WORDS #3 AND #4'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(W1.LT.0.0D0) THEN
      WRITE(OUTLYNE,*)'STARTING SURFACE NUMBER MUST BE 0 OR GREATER'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(W2.GT.SYSTEM(20)) THEN
      WRITE(OUTLYNE,*)'ENDING SURFACE NUMBER MUST BE LESS THAN',INT(SYSTEM(20)+1.0D0)
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(W2.LT.W1) THEN
      WRITE(OUTLYNE,*)'ERROR:'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'ENDING SURFACE NUMBER LESS THAN STARTING SURFACE NUMBER'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
! DO THE THERM OPERATION
   IF(WQ.EQ.'GLASS') THEN
      IF(INT(W1).EQ.INT(W2)) THEN
         I=INT(W1)
         GONOGO=.TRUE.
         CALL CHKGLS(GONOGO,I)
         IF(.NOT.GONOGO) THEN
            RETURN
         END IF
         IF(GLANAM(I,2).EQ.'REFL         '.OR.GLANAM(I,2).EQ.'REFLTIRO     '.OR.GLANAM(I,2).EQ.'REFLTIR      ') THEN
            WRITE(OUTLYNE,*)'"THERM ',WQ,'" DID NOT MODIFY "REFL" SURFACE # ',I
            CALL SHOWIT(1)
            RETURN
         END IF
         IF(GLANAM(I,2).EQ.'PERFECT      ') THEN
            WRITE(OUTLYNE,*)'"THERM ',WQ,'" DID NOT MODIFY "PERFECT" SURFACE # ',I
            CALL SHOWIT(1)
            RETURN
         END IF
         IF(GLANAM(I,2).EQ.'IDEAL        ') THEN
            WRITE(OUTLYNE,*)'"THERM ',WQ,'" DID NOT MODIFY "IDEAL" SURFACE # ',I
            CALL SHOWIT(1)
            RETURN
         END IF
         GONOGO=.TRUE.
         CALL CHKGLS(GONOGO,I)
         IF(.NOT.GONOGO) THEN
         ELSE
            IF(GLANAM(I,2).NE.'REFL         '.AND.GLANAM(I,2).NE.'PERFECT      '.AND.GLANAM(I,2).NE.'REFLTIR      '.AND.GLANAM(I,2).NE.'REFLTIRO     '.AND.GLANAM(I,2).NE.'IDEAL        ') THEN
               IF(DABS(surf_refractive_index(I, 1)).GT.1.1D0.OR.DABS(surf_refractive_index(I, 2)).GT.1.1D0 .OR.DABS(surf_refractive_index(I, 3)).GT.1.1D0.OR.DABS(surf_refractive_index(I, 4)).GT.1.1D0 .OR.DABS(surf_refractive_index(I, 5)).GT.1.1D0) THEN
                  GLANAM(I,1)='MYGLASS      '
                  IF(DABS(surf_refractive_index(I, 1)).GT.1.1D0)call set_surf_refractive_index(I, 1, surf_refractive_index(I, 1)+(DSGN(surf_refractive_index(I, 1))*W3*W4))
                  IF(DABS(surf_refractive_index(I, 2)).GT.1.1D0)call set_surf_refractive_index(I, 2, surf_refractive_index(I, 2)+(DSGN(surf_refractive_index(I, 1))*W3*W4))
                  IF(DABS(surf_refractive_index(I, 3)).GT.1.1D0)call set_surf_refractive_index(I, 3, surf_refractive_index(I, 3)+(DSGN(surf_refractive_index(I, 3))*W3*W4))
                  IF(DABS(surf_refractive_index(I, 4)).GT.1.1D0)call set_surf_refractive_index(I, 4, surf_refractive_index(I, 4)+(DSGN(surf_refractive_index(I, 4))*W3*W4))
                  IF(DABS(surf_refractive_index(I, 5)).GT.1.1D0)call set_surf_refractive_index(I, 5, surf_refractive_index(I, 5)+(DSGN(surf_refractive_index(I, 5))*W3*W4))
               END IF
               IF(DABS(surf_refractive_index(I, 6)).GT.1.1D0.OR.DABS(surf_refractive_index(I, 7)).GT.1.1D0 .OR.DABS(surf_refractive_index(I, 8)).GT.1.1D0.OR.DABS(surf_refractive_index(I, 9)).GT.1.1D0 .OR.DABS(surf_refractive_index(I, 10)).GT.1.1D0) THEN
                  IF(DABS(surf_refractive_index(I, 6)).GT.1.1D0)call set_surf_refractive_index(I, 6, surf_refractive_index(I, 6)+(DSGN(surf_refractive_index(I, 6))*W3*W4))
                  IF(DABS(surf_refractive_index(I, 2)).GT.1.1D0)call set_surf_refractive_index(I, 7, surf_refractive_index(I, 7)+(DSGN(surf_refractive_index(I, 7))*W3*W4))
                  IF(DABS(surf_refractive_index(I, 3)).GT.1.1D0)call set_surf_refractive_index(I, 8, surf_refractive_index(I, 8)+(DSGN(surf_refractive_index(I, 8))*W3*W4))
                  IF(DABS(surf_refractive_index(I, 4)).GT.1.1D0)call set_surf_refractive_index(I, 9, surf_refractive_index(I, 9)+(DSGN(surf_refractive_index(I, 9))*W3*W4))
                  IF(DABS(surf_refractive_index(I, 5)).GT.1.1D0)call set_surf_refractive_index(I, 10, surf_refractive_index(I, 10)+(DSGN(surf_refractive_index(I, 10))*W3*W4))
               END IF
            END IF
         END IF
      ELSE
         DO I=INT(W1),INT(W2)
            IF(GLANAM(I,2).EQ.'REFL         '.OR.GLANAM(I,2).EQ.'REFLTIRO     '.OR.GLANAM(I,2).EQ.'REFLTIR      ') THEN
               WRITE(OUTLYNE,*)'"THERM ',WQ,'" DID NOT MODIFY "REFL" SURFACE # ',I
               CALL SHOWIT(1)
            END IF
            IF(GLANAM(I,2).EQ.'PERFECT      ') THEN
               WRITE(OUTLYNE,*)'"THERM ',WQ,'" DID NOT MODIFY "PERFECT" SURFACE # ',I
               CALL SHOWIT(1)
            END IF
            IF(GLANAM(I,2).EQ.'IDEAL        ') THEN
               WRITE(OUTLYNE,*)'"THERM ',WQ,'" DID NOT MODIFY "IDEAL" SURFACE # ',I
               CALL SHOWIT(1)
            END IF
            IF(GLANAM(I,2).NE.'REFL         '.AND.GLANAM(I,2).NE.'PERFECT      '.AND.GLANAM(I,2).NE.'REFLTIR      '.AND.GLANAM(I,2).NE.'REFLTIRO     '.AND.GLANAM(I,2).NE.'IDEAL        ') THEN
               IF(DABS(surf_refractive_index(I, 1)).GT.1.1D0.OR.DABS(surf_refractive_index(I, 2)).GT.1.1D0 .OR.DABS(surf_refractive_index(I, 3)).GT.1.1D0.OR.DABS(surf_refractive_index(I, 4)).GT.1.1D0 .OR.DABS(surf_refractive_index(I, 5)).GT.1.1D0) THEN
                  GLANAM(I,1)='MYGLASS      '
                  IF(DABS(surf_refractive_index(I, 1)).GT.1.1D0)call set_surf_refractive_index(I, 1, surf_refractive_index(I, 1)+(DSGN(surf_refractive_index(I, 1))*W3*W4))
                  IF(DABS(surf_refractive_index(I, 2)).GT.1.1D0)call set_surf_refractive_index(I, 2, surf_refractive_index(I, 2)+(DSGN(surf_refractive_index(I, 2))*W3*W4))
                  IF(DABS(surf_refractive_index(I, 3)).GT.1.1D0)call set_surf_refractive_index(I, 3, surf_refractive_index(I, 3)+(DSGN(surf_refractive_index(I, 3))*W3*W4))
                  IF(DABS(surf_refractive_index(I, 4)).GT.1.1D0)call set_surf_refractive_index(I, 4, surf_refractive_index(I, 4)+(DSGN(surf_refractive_index(I, 4))*W3*W4))
                  IF(DABS(surf_refractive_index(I, 5)).GT.1.1D0)call set_surf_refractive_index(I, 5, surf_refractive_index(I, 5)+(DSGN(surf_refractive_index(I, 5))*W3*W4))
               END IF
               IF(DABS(surf_refractive_index(I, 6)).GT.1.1D0.OR.DABS(surf_refractive_index(I, 7)).GT.1.1D0 .OR.DABS(surf_refractive_index(I, 8)).GT.1.1D0.OR.DABS(surf_refractive_index(I, 9)).GT.1.1D0 .OR.DABS(surf_refractive_index(I, 10)).GT.1.1D0) THEN
                  IF(DABS(surf_refractive_index(I, 6)).GT.1.1D0)call set_surf_refractive_index(I, 6, surf_refractive_index(I, 6)+(DSGN(surf_refractive_index(I, 6))*W3*W4))
                  IF(DABS(surf_refractive_index(I, 7)).GT.1.1D0)call set_surf_refractive_index(I, 7, surf_refractive_index(I, 7)+(DSGN(surf_refractive_index(I, 7))*W3*W4))
                  IF(DABS(surf_refractive_index(I, 8)).GT.1.1D0)call set_surf_refractive_index(I, 8, surf_refractive_index(I, 8)+(DSGN(surf_refractive_index(I, 8))*W3*W4))
                  IF(DABS(surf_refractive_index(I, 9)).GT.1.1D0)call set_surf_refractive_index(I, 9, surf_refractive_index(I, 9)+(DSGN(surf_refractive_index(I, 9))*W3*W4))
                  IF(DABS(surf_refractive_index(I, 10)).GT.1.1D0)call set_surf_refractive_index(I, 10, surf_refractive_index(I, 10)+(DSGN(surf_refractive_index(I, 10))*W3*W4))
               END IF
            END IF
         END DO
      END IF
      F1=0
      F6=1
      F22=1
      LNSTYP=1
      CALL LNSEOS
   END IF
   IF(WQ.EQ.'SPACE') THEN
      IF(INT(W1).EQ.INT(W2)) THEN
         I=INT(W1)
         GONOGO=.TRUE.
         CALL CHKTHK(GONOGO,I)
         IF(.NOT.GONOGO) THEN
            RETURN
         END IF
         IF(DABS(surf_refractive_index(I, 1)).LE.1.1D0.AND.DABS(surf_refractive_index(I, 2)).LE.1.1D0 .AND.DABS(surf_refractive_index(I, 3)).LE.1.1D0.AND.DABS(surf_refractive_index(I, 4)).LE.1.1D0 .AND.DABS(surf_refractive_index(I, 5)).LE.1.1D0.AND.DABS(surf_refractive_index(I, 6)).LE.1.1D0.AND.DABS(surf_refractive_index(I, 7)).LE.1.1D0 .AND.DABS(surf_refractive_index(I, 8)).LE.1.1D0.AND.DABS(surf_refractive_index(I, 9)).LE.1.1D0 .AND.DABS(surf_refractive_index(I, 10)).LE.1.1D0) THEN
            call set_surf_thickness(I, surf_thickness(I)+(surf_thickness(I)*W3*W4))
         END IF
      ELSE
         DO I=INT(W1),INT(W2)
            GONOGO=.TRUE.
            CALL CHKTHK(GONOGO,I)
            IF(.NOT.GONOGO) THEN
            ELSE
               IF(DABS(surf_refractive_index(I, 1)).LE.1.1D0.AND.DABS(surf_refractive_index(I, 2)).LE.1.1D0 .AND.DABS(surf_refractive_index(I, 3)).LE.1.1D0.AND.DABS(surf_refractive_index(I, 4)).LE.1.1D0 .AND.DABS(surf_refractive_index(I, 5)).LE.1.1D0.AND.DABS(surf_refractive_index(I, 6)).LE.1.1D0.AND.DABS(surf_refractive_index(I, 7)).LE.1.1D0 .AND.DABS(surf_refractive_index(I, 8)).LE.1.1D0.AND.DABS(surf_refractive_index(I, 9)).LE.1.1D0 .AND.DABS(surf_refractive_index(I, 10)).LE.1.1D0) THEN
                  call set_surf_thickness(I, surf_thickness(I)+(surf_thickness(I)*W3*W4))
               END IF
            END IF
         END DO
      END IF
      F1=0
      F6=1
      F22=1
      LNSTYP=1
      CALL LNSEOS
   END IF
   IF(WQ.EQ.'THICK') THEN
      IF(INT(W1).EQ.INT(W2)) THEN
         I=INT(W1)
         GONOGO=.TRUE.
         CALL CHKTHK(GONOGO,I)
         IF(.NOT.GONOGO) THEN
            RETURN
         END IF
         IF(DABS(surf_refractive_index(I, 1)).GT.1.1D0.OR.DABS(surf_refractive_index(I, 2)).GT.1.1D0 .OR.DABS(surf_refractive_index(I, 3)).GT.1.1D0.OR.DABS(surf_refractive_index(I, 4)).GT.1.1D0 .OR.DABS(surf_refractive_index(I, 5)).GT.1.1D0.OR.DABS(surf_refractive_index(I, 6)).GT.1.1D0.OR.DABS(surf_refractive_index(I, 7)).GT.1.1D0 .OR.DABS(surf_refractive_index(I, 8)).GT.1.1D0.OR.DABS(surf_refractive_index(I, 9)).GT.1.1D0 .OR.DABS(surf_refractive_index(I, 10)).GT.1.1D0) THEN
            call set_surf_thickness(I, surf_thickness(I)+(surf_thickness(I)*W3*W4))
         END IF
      ELSE
         DO I=INT(W1),INT(W2)
            GONOGO=.TRUE.
            CALL CHKTHK(GONOGO,I)
            IF(.NOT.GONOGO) THEN
            ELSE
               IF(DABS(surf_refractive_index(I, 1)).GT.1.1D0.OR.DABS(surf_refractive_index(I, 2)).GT.1.1D0 .OR.DABS(surf_refractive_index(I, 3)).GT.1.1D0.OR.DABS(surf_refractive_index(I, 4)).GT.1.1D0 .OR.DABS(surf_refractive_index(I, 5)).GT.1.1D0.OR.DABS(surf_refractive_index(I, 6)).GT.1.1D0.OR.DABS(surf_refractive_index(I, 7)).GT.1.1D0 .OR.DABS(surf_refractive_index(I, 8)).GT.1.1D0.OR.DABS(surf_refractive_index(I, 9)).GT.1.1D0 .OR.DABS(surf_refractive_index(I, 10)).GT.1.1D0) THEN
                  call set_surf_thickness(I, surf_thickness(I)+(surf_thickness(I)*W3*W4))
               END IF
            END IF
         END DO
      END IF
      F1=0
      F6=1
      F22=1
      LNSTYP=1
      CALL LNSEOS
   END IF
   IF(WQ.EQ.'SHAPE') THEN
      IF(INT(W1).EQ.INT(W2)) THEN
         I=INT(W1)
         GONOGO=.TRUE.
         CALL CHKCVR(GONOGO,I)
         IF(.NOT.GONOGO) THEN
            RETURN
         END IF
!     CURVATURE IF NOT ZERO
         FACTOR=1.0D0+(W3*W4)
!     AC
         call set_surf_asphere_coeff(I, 2, surf_asphere_coeff(I, 2)/FACTOR)
!     AD
         call set_surf_asphere_coeff(I, 4, surf_asphere_coeff(I, 4)/(FACTOR**3))
!     AE
         call set_surf_asphere_coeff(I, 6, surf_asphere_coeff(I, 6)/(FACTOR**5))
!     AF
         call set_surf_asphere_coeff(I, 8, surf_asphere_coeff(I, 8)/(FACTOR**7))
!     AG
         call set_surf_asphere_coeff(I, 10, surf_asphere_coeff(I, 10)/(FACTOR**9))
!     AH
         call set_surf_asphere_coeff(I, 12, surf_asphere_coeff(I, 12)/(FACTOR**11))
!     AI
         call set_surf_asphere_coeff(I, 14, surf_asphere_coeff(I, 14)/(FACTOR**13))
!     AJ
         call set_surf_asphere_coeff(I, 16, surf_asphere_coeff(I, 16)/(FACTOR**15))
!     AK
         call set_surf_asphere_coeff(I, 18, surf_asphere_coeff(I, 18)/(FACTOR**17))
!     AL
         call set_surf_asphere_coeff(I, 20, surf_asphere_coeff(I, 20)/(FACTOR**19))
!     ADTOR
         call set_surf_anamorphic_coeff(I, 4, surf_anamorphic_coeff(I, 4)/(FACTOR**3))
!     AETOR
         call set_surf_anamorphic_coeff(I, 6, surf_anamorphic_coeff(I, 6)/(FACTOR**5))
!     AFTOR
         call set_surf_anamorphic_coeff(I, 8, surf_anamorphic_coeff(I, 8)/(FACTOR**7))
!     AGTOR
         call set_surf_anamorphic_coeff(I, 10, surf_anamorphic_coeff(I, 10)/(FACTOR**9))
!     CV
         call set_surf_curvature(I, surf_curvature(I)*(1/FACTOR))
!     CVTOR
         call set_surf_toric_curvature(I, surf_toric_curvature(I)*(1/FACTOR))
      ELSE
         DO I=INT(W1),INT(W2)
            GONOGO=.TRUE.
            CALL CHKCVR(GONOGO,I)
            IF(.NOT.GONOGO) THEN
            ELSE
               FACTOR=1.0D0+(W3*W4)
!     AC
               call set_surf_asphere_coeff(I, 2, surf_asphere_coeff(I, 2)/FACTOR)
!     AD
               call set_surf_asphere_coeff(I, 4, surf_asphere_coeff(I, 4)/(FACTOR**3))
!     AE
               call set_surf_asphere_coeff(I, 6, surf_asphere_coeff(I, 6)/(FACTOR**5))
!     AF
               call set_surf_asphere_coeff(I, 8, surf_asphere_coeff(I, 8)/(FACTOR**7))
!     AG
               call set_surf_asphere_coeff(I, 10, surf_asphere_coeff(I, 10)/(FACTOR**9))
!     AH
               call set_surf_asphere_coeff(I, 12, surf_asphere_coeff(I, 12)/(FACTOR**11))
!     AI
               call set_surf_asphere_coeff(I, 14, surf_asphere_coeff(I, 14)/(FACTOR**13))
!     AJ
               call set_surf_asphere_coeff(I, 16, surf_asphere_coeff(I, 16)/(FACTOR**15))
!     AK
               call set_surf_asphere_coeff(I, 18, surf_asphere_coeff(I, 18)/(FACTOR**17))
!     AL
               call set_surf_asphere_coeff(I, 20, surf_asphere_coeff(I, 20)/(FACTOR**19))
!     ADTOR
               call set_surf_anamorphic_coeff(I, 4, surf_anamorphic_coeff(I, 4)/(FACTOR**3))
!     AETOR
               call set_surf_anamorphic_coeff(I, 6, surf_anamorphic_coeff(I, 6)/(FACTOR**5))
!     AFTOR
               call set_surf_anamorphic_coeff(I, 8, surf_anamorphic_coeff(I, 8)/(FACTOR**7))
!     AGTOR
               call set_surf_anamorphic_coeff(I, 10, surf_anamorphic_coeff(I, 10)/(FACTOR**9))
!     CV
               call set_surf_curvature(I, surf_curvature(I)*(1/FACTOR))
!     CVTOR
               call set_surf_toric_curvature(I, surf_toric_curvature(I)*(1/FACTOR))
            END IF
         END DO
      END IF
      F1=0
      F6=1
      F22=1
      LNSTYP=1
      CALL LNSEOS
   END IF
   RETURN
END
SUBROUTINE CHKGLS(GONOGO,I)
   use DATLEN
   use mod_surface
   use DATMAI
   IMPLICIT NONE
   LOGICAL GONOGO
   INTEGER I
   GONOGO=.TRUE.
   IF(PIKUP(1,I,20).NE.0.0D0) THEN
      GONOGO=.FALSE.
      WRITE(OUTLYNE,*)'"THERM ',WQ,'" DID NOT MODIFY SURFACE # ',I,' DUE TO GLASS PIKUP'
      CALL SHOWIT(1)
   END IF
   RETURN
END
SUBROUTINE CHKGLSP(GONOGO,I)
   use DATLEN
   use mod_surface
   use DATMAI
   IMPLICIT NONE
   LOGICAL GONOGO
   INTEGER I
   GONOGO=.TRUE.
   IF(PIKUP(1,I,20).NE.0.0D0) THEN
      GONOGO=.FALSE.
      WRITE(OUTLYNE,*)'"PRES ',WQ,'" DID NOT MODIFY SURFACE # ',I,' DUE TO GLASS PIKUP'
      CALL SHOWIT(1)
   END IF
   RETURN
END
SUBROUTINE CHKTHK(GONOGO,I)
   use DATLEN
   use mod_surface
   use DATMAI
   IMPLICIT NONE
   LOGICAL GONOGO,PIKK,SLVV
   INTEGER I
   PIKK=.FALSE.
   SLVV=.FALSE.
   GONOGO=.TRUE.
   IF(surf_solve_flag(I).EQ.1.0D0) SLVV=.TRUE.
   IF(surf_solve_flag(I).EQ.3.0D0) SLVV=.TRUE.
   IF(PIKUP(1,I,3).NE.0.0D0) PIKK=.TRUE.
   IF(PIKUP(1,I,32).NE.0.0D0) PIKK=.TRUE.
   IF(PIKK) THEN
      GONOGO=.FALSE.
      WRITE(OUTLYNE,*)'"THERM ',WQ,'" DID NOT MODIFY SURFACE # ',I
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)' DUE TO "TH" OR "THOAL" PIKUP'
      CALL SHOWIT(1)
   END IF
   IF(SLVV) THEN
      GONOGO=.FALSE.
      WRITE(OUTLYNE,*)'"THERM ',WQ,'" DID NOT MODIFY SURFACE # ',I,' DUE TO "TH" SOLVE'
      CALL SHOWIT(1)
   END IF
   RETURN
END
SUBROUTINE CHKCVR(GONOGO,I)
   use DATLEN
   use mod_surface
   use DATMAI
   IMPLICIT NONE
   LOGICAL GONOGO,PIKK,SLVV
   INTEGER I
   PIKK=.FALSE.
   SLVV=.FALSE.
   GONOGO=.TRUE.
   IF(surf_solve_flag(I).EQ.2.0D0) SLVV=.TRUE.
   IF(surf_solve_flag(I).EQ.3.0D0) SLVV=.TRUE.
   IF(PIKUP(1,I,2).NE.0.0D0) PIKK=.TRUE.
   IF(PIKUP(1,I,1).NE.0.0D0) PIKK=.TRUE.
   IF(PIKUP(1,I,10).NE.0.0D0) PIKK=.TRUE.
   IF(PIKUP(1,I,9).NE.0.0D0) PIKK=.TRUE.
   IF(PIKUP(1,I,4).NE.0.0D0) PIKK=.TRUE.
   IF(PIKUP(1,I,21).NE.0.0D0) PIKK=.TRUE.
   IF(PIKUP(1,I,26).NE.0.0D0) PIKK=.TRUE.
   IF(PIKUP(1,I,5).NE.0.0D0) PIKK=.TRUE.
   IF(PIKUP(1,I,6).NE.0.0D0) PIKK=.TRUE.
   IF(PIKUP(1,I,7).NE.0.0D0) PIKK=.TRUE.
   IF(PIKUP(1,I,8).NE.0.0D0) PIKK=.TRUE.
   IF(PIKUP(1,I,22).NE.0.0D0) PIKK=.TRUE.
   IF(PIKUP(1,I,23).NE.0.0D0) PIKK=.TRUE.
   IF(PIKUP(1,I,24).NE.0.0D0) PIKK=.TRUE.
   IF(PIKUP(1,I,25).NE.0.0D0) PIKK=.TRUE.
   IF(PIKUP(1,I,11).NE.0.0D0) PIKK=.TRUE.
   IF(PIKUP(1,I,12).NE.0.0D0) PIKK=.TRUE.
   IF(PIKK) THEN
      GONOGO=.FALSE.
      WRITE(OUTLYNE,*)'"THERM ',WQ,'" DID NOT MODIFY SURFACE # ',I,' DUE TO A SURFACE SHAPE PIKUP'
      CALL SHOWIT(1)
   END IF
   IF(SLVV) THEN
      GONOGO=.FALSE.
      WRITE(OUTLYNE,*)'"THERM ',WQ,'" DID NOT MODIFY SURFACE # ',I,' DUE TO A CURVATURE SOLVE'
      CALL SHOWIT(1)
   END IF
   RETURN
END
! SUB TELAIM.FOR
SUBROUTINE TELAIM
!
   use DATLEN
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE TELAIM.FOR. THIS SUBROUTINE CONTROLS
!     TELECENTRIC RAY AIMING
!
!
!       CHECK FOR STRING INPUT
   IF(SST.EQ.1.OR.SN.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"TEL" TAKES NO STRING OR NUMERIC INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(STI.EQ.0) THEN
      IF(WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"ON", "YES","OFF" AND "NO"'//'\n'//&
         & ' ARE THE ONLY VALID QUALIFIERS'//'\n'//&
         & 'USED WITH "TEL"'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!     STI=1
   END IF
   IF(STI.EQ.1) THEN
      IF(SYSTEM(63).EQ.0.0D0) WRITE(OUTLYNE,10)
      CALL SHOWIT(0)
10    FORMAT('TELECENTRIC RAY AIMING IS CURRENTLY TURNED "OFF"')
11    FORMAT('TELECENTRIC RAY AIMING IS CURRENTLY TURNED "ON"')
      IF(SYSTEM(63).EQ.1.0D0) WRITE(OUTLYNE,11)
      CALL SHOWIT(0)
      RETURN
   END IF
   IF(WQ.EQ.'OFF') THEN
      SYSTEM(63)=0.0D0
      RETURN
   END IF
   IF(WQ.EQ.'ON') THEN
      IF(DABS(surf_thickness(0)).GE.1.0D10) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'TELECENTRIC RAY AIMING MAY NOT BE ACTIVATED BECAUSE THE'//'\n'//&
         & 'MAGNITUDE OF THE OBJECT DISTANCE IS GREATER THAN OR EQUAL'//'\n'//&
         & 'TO 1.0D+10'//'\n'//&
         & 'NO ACTION TAKEN', 1)
         RETURN
      ELSE
         SYSTEM(63)=1.0D0
         SYSTEM(62)=0.0D0
         SYSTEM(70)=0.0D0
         NEWOBJ=0
         NEWIMG=INT(SYSTEM(20))
!     SHUT OFF REGULAR RAY AIMING
         SYSTEM(62)=0.0D0
      END IF
   END IF
   RETURN
END
! SUB NEARFARNEAR.FOR
SUBROUTINE NEARFARNEAR
!
   use DATLEN
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE NEARFAR.FOR. THIS SUBROUTINE CONTROLS
!     THE CMD LEVEL COMMANDS NEAR AND FAR FOR GOTF AND DOTF UNITS
!     AND WORKS WITH THE O SETTING OF THE SPACE COMMAND
!
   REAL*8 AL
!
   IF(SYSTEM(6).EQ.1.0D0) AL=DABS(surf_thickness(NEWOBJ))*25.4D0
   IF(SYSTEM(6).EQ.2.0D0) AL=DABS(surf_thickness(NEWOBJ))*10.0D0
   IF(SYSTEM(6).EQ.3.0D0) AL=DABS(surf_thickness(NEWOBJ))
   IF(SYSTEM(6).EQ.4.0D0) AL=DABS(surf_thickness(NEWOBJ))*1000.0D0
!
   IF(STI.EQ.1) THEN
      IF(WC.EQ.'FAR')OUTLYNE='"FAR" SETS UNITS TO LP/MRAD'
      IF(WC.EQ.'NEAR')OUTLYNE='"NEAR" SETS UNITS TO LP/MM'
      CALL SHOWIT(1)
      OUTLYNE='FOR GOTF AND DOTF OPTICAL TRANSFER FUNCTION DISPLAYS'
      CALL SHOWIT(1)
      IF(NEAR_FAR.EQ.0) OUTLYNE='CURRENT SETTING IS "NEAR"'
      IF(NEAR_FAR.EQ.1) OUTLYNE='CURRENT SETTING IS "FAR"'
      CALL SHOWIT(1)
      RETURN
   END IF
   IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1) THEN
      IF(WC.EQ.'FAR')OUTLYNE='"FAR" TAKES NO ADDITIONAL INPUT'
      IF(WC.EQ.'NEAR')OUTLYNE='"NEAR" TAKES NO ADDITIONAL INPUT'
      CALL SHOWIT(1)
      IF(NEAR_FAR.EQ.1) OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(WC.EQ.'NEAR'.AND.AL.GT.1.0D5) THEN
      OUTLYNE='OBJECT DISTANCE GREATER THAN 1.0D+5 MILLIMETERS'
      CALL SHOWIT(1)
      IF(NEAR_FAR.EQ.1) OUTLYNE='"NEAR" NOT ALLOWED'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   ELSE
      NEAR_FAR=0
   END IF
   IF(WC.EQ.'FAR') NEAR_FAR=1
   RETURN
END
! SUB OVERBOSE.FOR
SUBROUTINE OVERBOSE
!
   use DATLEN
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE OPTMINIT.FOR. THIS SUBROUTINE CONTROLS
!     THE CMD LEVEL COMMAND OPTMINIT
!
!
!       CHECK FOR STRING INPUT
   IF(SST.EQ.1.OR.SN.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"OVERBOSE" TAKES NO STRING OR NUMERIC INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(STI.EQ.0) THEN
      IF(WQ.NE.'ON'.AND.WQ.NE.'OFF'.AND.SQ.EQ.1) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"ON", "YES","OFF" AND "NO"'//'\n'//&
         & ' ARE THE ONLY VALID QUALIFIERS'//'\n'//&
         & 'USED WITH "OVERBOSE"'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!     STI=1
   END IF
   IF(STI.EQ.1.OR.SQ.EQ.0) THEN
10    FORMAT('VERBOSE OPTIMIZATION OUTPUT IS CURRENTLY "OFF"')
11    FORMAT('VERBOSE OPTIMIZATION OUTPUT IS CURRENTLY "ON"')
      IF(SYSTEM(101).EQ.0.0D0) WRITE(OUTLYNE,10)
      CALL SHOWIT(0)
      IF(SYSTEM(101).EQ.1.0D0) WRITE(OUTLYNE,11)
      CALL SHOWIT(0)
      RETURN
   END IF
   IF(WQ.EQ.'OFF') THEN
      SYSTEM(101)=0.0D0
      RETURN
   END IF
   IF(WQ.EQ.'ON') THEN
      SYSTEM(101)=1.0D0
      RETURN
   END IF
END
! SUB OPTMINIT.FOR
SUBROUTINE OPTMINIT
!
   use DATLEN
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE OPTMINIT.FOR. THIS SUBROUTINE CONTROLS
!     THE CMD LEVEL COMMAND OPTMINIT
!
!
!       CHECK FOR STRING INPUT
   IF(SST.EQ.1.OR.SN.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"OPTMINIT" TAKES NO STRING OR NUMERIC INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(STI.EQ.0) THEN
      IF(WQ.NE.'ON'.AND.WQ.NE.'OFF'.AND.SQ.EQ.1) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"ON", "YES","OFF" AND "NO"'//'\n'//&
         & ' ARE THE ONLY VALID QUALIFIERS'//'\n'//&
         & 'USED WITH "OPTMINIT"'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!     STI=1
   END IF
   IF(STI.EQ.1.OR.SQ.EQ.0) THEN
10    FORMAT('AUTOMATIC OPERAND INITIALIZATION IS CURRENTLY "OFF"')
11    FORMAT('AUTOMATIC OPERAND INITIALIZATION IS CURRENTLY "ON"')
      IF(OPTM_INIT.EQ.0) WRITE(OUTLYNE,10)
      CALL SHOWIT(0)
      IF(OPTM_INIT.EQ.1) WRITE(OUTLYNE,11)
      CALL SHOWIT(0)
      RETURN
   END IF
   IF(WQ.EQ.'OFF') THEN
      OPTM_INIT=0
      RETURN
   END IF
   IF(WQ.EQ.'ON') THEN
      OPTM_INIT=1
      RETURN
   END IF
END
! SUB GEOLEICA.FOR
SUBROUTINE GEOLEICA
!
   use DATSPD
   use DATLEN
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE GEOLEICA.FOR. THIS SUBROUTINE CONTROLS
!       THE COMMAND GEOLEICA
!
   INTEGER I
!
!
   IF(STI.EQ.1) THEN
      DO I=1,10
         IF(GLEICA(I)) WRITE(OUTLYNE,10) I
         IF(.NOT.GLEICA(I)) WRITE(OUTLYNE,11) I
         CALL SHOWIT(0)
10       FORMAT('"GEOLEICA" CURRENTLY TURNED "ON" FOR FREQUENCY # ',I2)
11       FORMAT('"GEOLEICA" CURRENTLY TURNED "OFF" FOR FREQUENCY # ',I2)
      END DO
      RETURN
   END IF
!       CHECK FOR NW1 INPUT
   IF(S1.EQ.0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"GEOLEICA" REQUIRES EXPLICIT NUMERIC WORD #1'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(INT(W1).NE.1.AND.INT(W1).NE.2.AND.INT(W1).NE.3 .AND.INT(W1).NE.4.AND.INT(W1).NE.5.AND.INT(W1).NE.6 .AND.INT(W1).NE.7.AND.INT(W1).NE.8.AND.INT(W1).NE.9 .AND.INT(W1).NE.10) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"GEOLEICA" REQUIRES 1,2,3,4,5,6,7,8,9 OR 10'//'\n'//&
      & 'AS NUMERIC WORD #1 INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(STI.EQ.0) THEN
      IF(WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"ON", "YES","OFF" AND "NO"'//'\n'//&
         & ' ARE THE ONLY VALID QUALIFIERS'//'\n'//&
         & 'USED WITH "TEL"'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!     STI=1
   END IF
!       CHECK FOR STRING INPUT
   IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"GEOLEICA" TAKES NO STRING OR '//'\n'//&
      & 'NUMERIC WORD #2 THROUGH #5 INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(WQ.EQ.'OFF') THEN
      GLEICA(INT(W1))=.FALSE.
      RETURN
   END IF
   IF(WQ.EQ.'ON') THEN
      GLEICA(INT(W1))=.TRUE.
      RETURN
   END IF
   RETURN
END
! SUB DIFLEICA.FOR
SUBROUTINE DIFLEICA
!
   use DATSPD
   use DATLEN
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE DIFLEICA.FOR. THIS SUBROUTINE CONTROLS
!       THE COMMAND DIFLEICA
!
   INTEGER I
!
!
   IF(STI.EQ.1) THEN
      DO I=1,10
         IF(DLEICA(I)) WRITE(OUTLYNE,10) I
         IF(.NOT.DLEICA(I)) WRITE(OUTLYNE,11) I
         CALL SHOWIT(0)
10       FORMAT('"DIFLEICA" CURRENTLY TURNED "ON" FOR FREQUENCY # ',I2)
11       FORMAT('"DIFLEICA" CURRENTLY TURNED "OFF" FOR FREQUENCY # ',I2)
      END DO
      RETURN
   END IF
!       CHECK FOR NW1 INPUT
   IF(S1.EQ.0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"DIFLEICA" REQUIRES EXPLICIT NUMERIC WORD #1'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(INT(W1).NE.1.AND.INT(W1).NE.2.AND.INT(W1).NE.3 .AND.INT(W1).NE.4.AND.INT(W1).NE.5.AND.INT(W1).NE.6 .AND.INT(W1).NE.7.AND.INT(W1).NE.8.AND.INT(W1).NE.9 .AND.INT(W1).NE.10) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"DIFLEICA" REQUIRES 1,2,3,4,5,6,7,8,9 OR 10'//'\n'//&
      & 'AS NUMERIC WORD #1 INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(STI.EQ.0) THEN
      IF(WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"ON", "YES","OFF" AND "NO"'//'\n'//&
         & ' ARE THE ONLY VALID QUALIFIERS'//'\n'//&
         & 'USED WITH "TEL"'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!     STI=1
   END IF
!       CHECK FOR STRING INPUT
   IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"DIFLEICA" TAKES NO STRING OR '//'\n'//&
      & 'NUMERIC WORD #2 THROUGH #5 INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(WQ.EQ.'OFF') THEN
      DLEICA(INT(W1))=.FALSE.
      RETURN
   END IF
   IF(WQ.EQ.'ON') THEN
      DLEICA(INT(W1))=.TRUE.
      RETURN
   END IF
   RETURN
END
! SUB SWV.FOR
SUBROUTINE SWV
!
   use DATLEN
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS SUBROUTINE HANDELS THE ASSIGNMENT OF WAVELENGTHS.
!       SUBROUTINE LENNS SETS THE DEFAULT VALUES OF WV1 TO WV5.
!
!
!       THIS IS SUBROUTINE SWV WHICH IMPLEMENTS THE WV COMMAND
!       AT THE LENS OF UPDATE LENS LEVEL OR THE WV COMMAND AT
!       THE CMD LEVEL. AT THE CMD LEVEL, NUMERIC INPUT INCLUDED
!       WITH THE WV COMMAND IS IGNORED.
!
   IF(F5.EQ.1.OR.F6.EQ.1) THEN
      IF(STI.EQ.1) THEN
         WRITE(OUTLYNE,2001)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,3001)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,10001) SYSTEM(1),SYSTEM(2),SYSTEM(3),SYSTEM(4),SYSTEM(5)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,200)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,300)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,1000) SYSTEM(71),SYSTEM(72),SYSTEM(73),SYSTEM(74),SYSTEM(75)
         CALL SHOWIT(0)
         RETURN
      ELSE
!       NOT STI
      END IF
   ELSE
!       AT CMD
   END IF
   IF(F1.EQ.1) THEN
!
!               CHECK FOR ADDITIONAL INPUT
!       AND IF FOUND PRINT ERROR MESSAGE.
!
      IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'AT THE CMD LEVEL, "WV" TAKES NO EXPLICIT INPUT'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      WRITE(OUTLYNE,2001)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,3001)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,10001) SYSTEM(1),SYSTEM(2),SYSTEM(3),SYSTEM(4),SYSTEM(5)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,200)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,300)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,1000) SYSTEM(71),SYSTEM(72),SYSTEM(73),SYSTEM(74),SYSTEM(75)
      CALL SHOWIT(0)
      RETURN
   ELSE
!               NOT AT CMD LEVEL
!
      IF(F5.EQ.1.OR.F6.EQ.1) THEN
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING
!       AND IF FOUND PRINT ERROR MESSAGE.
!
         IF(SQ.EQ.1.OR.SST.EQ.1) THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"WV" COMMAND ONLY ACCEPTS NUNERIC INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1.AND.DF4.EQ.1 .AND.DF5.EQ.1) THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"WV" REQUIRES SOME EXPLICIT NUMERIC INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
!
!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
!
         IF(DF1.EQ.0) SYSTEM(1)=W1
         IF(DF2.EQ.0) SYSTEM(2)=W2
         IF(DF3.EQ.0) SYSTEM(3)=W3
         IF(DF4.EQ.0) SYSTEM(4)=W4
         IF(DF5.EQ.0) SYSTEM(5)=W5
         IF(DF1.EQ.0) SYSTEM(111)=W1
         IF(DF2.EQ.0) SYSTEM(112)=W2
         IF(DF3.EQ.0) SYSTEM(113)=W3
         IF(DF4.EQ.0) SYSTEM(114)=W4
         IF(DF5.EQ.0) SYSTEM(115)=W5
         IF(SYSTEM(1).EQ.0.0D0) SYSTEM(31)=0.0D0
         IF(SYSTEM(2).EQ.0.0D0) SYSTEM(32)=0.0D0
         IF(SYSTEM(3).EQ.0.0D0) SYSTEM(33)=0.0D0
         IF(SYSTEM(4).EQ.0.0D0) SYSTEM(34)=0.0D0
         IF(SYSTEM(5).EQ.0.0D0) SYSTEM(35)=0.0D0
         IF(SYSTEM(71).EQ.0.0D0) SYSTEM(76)=0.0D0
         IF(SYSTEM(72).EQ.0.0D0) SYSTEM(77)=0.0D0
         IF(SYSTEM(73).EQ.0.0D0) SYSTEM(78)=0.0D0
         IF(SYSTEM(74).EQ.0.0D0) SYSTEM(79)=0.0D0
         IF(SYSTEM(75).EQ.0.0D0) SYSTEM(80)=0.0D0
         F22=1
      ELSE
      END IF
   END IF
2001 FORMAT('CURRENT LENS FILE WAVELENGTHS #1 TO #5 ARE:')
3001 FORMAT(4X,'WV(1)',9X,'WV(2)',9X,'WV(3)',9X,'WV(4)',9X,'WV(5)')
10001 FORMAT(G13.7,1X,G13.7,1X,G13.7,1X,G13.7,1X,G13.7)
200 FORMAT('CURRENT LENS FILE WAVELENGTHS #6 TO #10 ARE:')
300 FORMAT(4X,'WV(6)',9X,'WV(7)',9X,'WV(8)',9X,'WV(9)',9X,'WV(10)')
1000 FORMAT(G13.7,1X,G13.7,1X,G13.7,1X,G13.7,1X,G13.7)
   RETURN
END
! SUB SWV2.FOR
SUBROUTINE SWV2
!
   use DATLEN
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS SUBROUTINE HANDELS THE ASSIGNMENT OF WAVELENGTHS.
!       SUBROUTINE LENNS SETS THE DEFAULT VALUES OF WV6 TO WV10.
!
!
!       THIS IS SUBROUTINE SWV WHICH IMPLEMENTS THE WV COMMAND
!       AT THE LENS OF UPDATE LENS LEVEL OR THE WV COMMAND AT
!       THE CMD LEVEL. AT THE CMD LEVEL, NUMERIC INPUT INCLUDED
!       WITH THE WV COMMAND IS IGNORED.
!
   IF(F5.EQ.1.OR.F6.EQ.1) THEN
      IF(STI.EQ.1) THEN
         WRITE(OUTLYNE,2001)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,3001)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,10001) SYSTEM(1),SYSTEM(2),SYSTEM(3),SYSTEM(4),SYSTEM(5)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,200)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,300)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,1000) SYSTEM(71),SYSTEM(72),SYSTEM(73),SYSTEM(74),SYSTEM(5)
         CALL SHOWIT(0)
         RETURN
      ELSE
!       NOT STI
      END IF
   ELSE
!       AT CMD
   END IF
   IF(F1.EQ.1) THEN
!
!               CHECK FOR ADDITIONAL INPUT
!       AND IF FOUND PRINT ERROR MESSAGE.
!
      IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'AT THE CMD LEVEL, "WV2" TAKES NO EXPLICIT INPUT'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      WRITE(OUTLYNE,2001)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,3001)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,10001) SYSTEM(1),SYSTEM(2),SYSTEM(3),SYSTEM(4),SYSTEM(5)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,200)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,300)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,1000) SYSTEM(71),SYSTEM(72),SYSTEM(73),SYSTEM(74),SYSTEM(75)
      CALL SHOWIT(0)
      RETURN
   ELSE
!               NOT AT CMD LEVEL
!
      IF(F5.EQ.1.OR.F6.EQ.1) THEN
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING
!       AND IF FOUND PRINT ERROR MESSAGE.
!
         IF(SQ.EQ.1.OR.SST.EQ.1) THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"WV2" COMMAND ONLY ACCEPTS NUNERIC INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 0)
            RETURN
         END IF
         IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1.AND.DF4.EQ.1 .AND.DF5.EQ.1) THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"WV2" REQUIRES SOME EXPLICIT NUMERIC INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 0)
            RETURN
         END IF
!
!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
!
         IF(DF1.EQ.0) SYSTEM(71)=W1
         IF(DF2.EQ.0) SYSTEM(72)=W2
         IF(DF3.EQ.0) SYSTEM(73)=W3
         IF(DF4.EQ.0) SYSTEM(74)=W4
         IF(DF5.EQ.0) SYSTEM(75)=W5
         IF(DF1.EQ.0) SYSTEM(116)=W1
         IF(DF2.EQ.0) SYSTEM(117)=W2
         IF(DF3.EQ.0) SYSTEM(118)=W3
         IF(DF4.EQ.0) SYSTEM(119)=W4
         IF(DF5.EQ.0) SYSTEM(120)=W5
         IF(SYSTEM(1).EQ.0.0D0) SYSTEM(31)=0.0D0
         IF(SYSTEM(2).EQ.0.0D0) SYSTEM(32)=0.0D0
         IF(SYSTEM(3).EQ.0.0D0) SYSTEM(33)=0.0D0
         IF(SYSTEM(4).EQ.0.0D0) SYSTEM(34)=0.0D0
         IF(SYSTEM(5).EQ.0.0D0) SYSTEM(35)=0.0D0
         IF(SYSTEM(71).EQ.0.0D0) SYSTEM(76)=0.0D0
         IF(SYSTEM(72).EQ.0.0D0) SYSTEM(77)=0.0D0
         IF(SYSTEM(73).EQ.0.0D0) SYSTEM(78)=0.0D0
         IF(SYSTEM(74).EQ.0.0D0) SYSTEM(79)=0.0D0
         IF(SYSTEM(75).EQ.0.0D0) SYSTEM(80)=0.0D0
         F22=1
      ELSE
      END IF
   END IF
2001 FORMAT('CURRENT LENS FILE WAVELENGTHS #1 TO #5 ARE:')
3001 FORMAT(4X,'WV(1)',9X,'WV(2)',9X,'WV(3)',9X,'WV(4)',9X,'WV(5)')
10001 FORMAT(G13.7,1X,G13.7,1X,G13.7,1X,G13.7,1X,G13.7)
200 FORMAT('CURRENT LENS FILE WAVELENGTHS #6 TO #10 ARE:')
300 FORMAT(4X,'WV(6)',9X,'WV(7)',9X,'WV(8)',9X,'WV(9)',9X,'WV(10)')
1000 FORMAT(G13.7,1X,G13.7,1X,G13.7,1X,G13.7,1X,G13.7)
   RETURN
END
! SUB SVSET.FOR
SUBROUTINE SVSET
!
   use DATLEN
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SVSET .
!       IT RE-SETS ALL THE YZ/XZ-PLANE SOLVE DATUM ACCORDING TO THE
!       CURRENT YZ/YZ-PLANE PARAXIAL DATA
!
   INTEGER ITYPEP,I
!
   COMMON/PTYPE/ITYPEP
!
!
   IF(ITYPEP.EQ.1) THEN
      DO 10 I=0,((INT(SYSTEM(20)))-1)
         IF(SOLVE(6,I).EQ.1.0D0) THEN
!       PY SOLVE
            SOLVE(7,I)=PXTRAY(1,(I+1))
         ELSE
!       NOT PY SOLVE
         END IF
         IF(SOLVE(6,I).EQ.2.0D0) THEN
!       PCY SOLVE
            SOLVE(7,I)=PXTRAY(5,(I+1))
         ELSE
!       NOT PCY SOLVE
         END IF
         IF(SOLVE(8,I).EQ.1.0D0) THEN
!       APY SOLVE
            SOLVE(9,I)=0.0D0
         ELSE
!       NOT APY SOLVE
         END IF
         IF(SOLVE(8,I).EQ.2.0D0) THEN
!       PIY SOLVE
            SOLVE(9,I)=PXTRAY(3,I)
         ELSE
!       NOT PIY SOLVE
         END IF
         IF(SOLVE(8,I).EQ.3.0D0) THEN
!       PUY SOLVE
            SOLVE(9,I)=PXTRAY(2,I)
         ELSE
!       NOT PUY SOLVE
         END IF
         IF(SOLVE(8,I).EQ.4.0D0) THEN
!       APCY SOLVE
            SOLVE(9,I)=0.0D0
         ELSE
!       NOT APCY SOLVE
         END IF
         IF(SOLVE(8,I).EQ.5.0D0) THEN
!       PICY SOLVE
            SOLVE(9,I)=PXTRAY(7,I)
         ELSE
!       NOT PICY SOLVE
         END IF
         IF(SOLVE(8,I).EQ.6.0D0) THEN
!       PUCY SOLVE
            SOLVE(9,I)=PXTRAY(6,I)
         ELSE
!       NOT PUCY SOLVE
         END IF
10    CONTINUE
!       ALL SOLVE TARGETS RE-SET
      RETURN
   ELSE
!       ITYPEP NOT 1
   END IF
!
   IF(ITYPEP.EQ.2) THEN
!
      DO 100 I=0,((INT(SYSTEM(20)))-1)
         IF(SOLVE(6,I).EQ.1.0D0) THEN
!       PX SOLVE
            SOLVE(7,I)=PXTRAX(1,(I+1))
         ELSE
!       NOT PX SOLVE
         END IF
         IF(SOLVE(6,I).EQ.2.0D0) THEN
!       PCX SOLVE
            SOLVE(7,I)=PXTRAX(5,(I+1))
         ELSE
!       NOT PCX SOLVE
         END IF
         IF(SOLVE(2,I).EQ.1.0D0) THEN
!       APX SOLVE
            SOLVE(1,I)=0.0D0
         ELSE
!       NOT APX SOLVE
         END IF
         IF(SOLVE(2,I).EQ.2.0D0) THEN
!       PIX SOLVE
            SOLVE(1,I)=PXTRAX(3,I)
         ELSE
!       NOT PIX SOLVE
         END IF
         IF(SOLVE(2,I).EQ.3.0D0) THEN
!       PUX SOLVE
            SOLVE(1,I)=PXTRAX(2,I)
         ELSE
!       NOT PUX SOLVE
         END IF
         IF(SOLVE(2,I).EQ.4.0D0) THEN
!       APCX SOLVE
            SOLVE(1,I)=0.0D0
         ELSE
!       NOT APCX SOLVE
         END IF
         IF(SOLVE(2,I).EQ.5.0D0) THEN
!       PICX SOLVE
            SOLVE(1,I)=PXTRAX(7,I)
         ELSE
!       NOT PICX SOLVE
         END IF
         IF(SOLVE(2,I).EQ.6.0D0) THEN
!       PUCX SOLVE
            SOLVE(1,I)=PXTRAX(6,I)
         ELSE
!       NOT PUCX SOLVE
         END IF
100   CONTINUE
!       ALL SOLVE TARGETS RE-SET
      RETURN
   ELSE
!       ITYPEP NOT 2
   END IF
   RETURN
END
! SUB SUNITS.FOR
SUBROUTINE SUNITS
!
   use DATLEN
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS SUBROUTINE HANDELS THE UNITS COMMAND BOTH AT
!       THE CMD LEVEL AND AT THE LENS AND LENS UPDATE LEVEL.
!
!
   IF(F5.EQ.1.OR.F6.EQ.1) THEN
      IF(STI.EQ.1) THEN
         IF(SYSTEM(6).EQ.1.0D0) THEN
            WRITE(OUTLYNE,1000)
            CALL SHOWIT(0)
         END IF
         IF(SYSTEM(6).EQ.2.0D0) THEN
            WRITE(OUTLYNE,2000)
            CALL SHOWIT(0)
         END IF
         IF(SYSTEM(6).EQ.3.0D0) THEN
            WRITE(OUTLYNE,3000)
            CALL SHOWIT(0)
         END IF
         IF(SYSTEM(6).EQ.4.0D0) THEN
            WRITE(OUTLYNE,4000)
            CALL SHOWIT(0)
         END IF
         RETURN
      ELSE
!       NOT STI
      END IF
   ELSE
   END IF
   IF(F1.EQ.1) THEN
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
      IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1)THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'AT THE CMD LEVEL, "UNITS" TAKES NO EXPLICIT INPUT'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!       WHAT IF NO SURFACES EXIST
      IF(SYSTEM(20).EQ.0.0D0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'UNITS ARE NOT DEFINED'//'\n'//&
         & 'LENS SYSTEM HAS NO SURFACES'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(SYSTEM(6).EQ.1.0D0) THEN
         WRITE(OUTLYNE,1000)
         CALL SHOWIT(0)
      END IF
      IF(SYSTEM(6).EQ.2.0D0) THEN
         WRITE(OUTLYNE,2000)
         CALL SHOWIT(0)
      END IF
      IF(SYSTEM(6).EQ.3.0D0) THEN
         WRITE(OUTLYNE,3000)
         CALL SHOWIT(0)
      END IF
      IF(SYSTEM(6).EQ.4.0D0) THEN
         WRITE(OUTLYNE,4000)
         CALL SHOWIT(0)
      END IF
      RETURN
   ELSE
!               NOT AT CMD LEVEL
!
      IF(F5.EQ.1.OR.F6.EQ.1) THEN
!
!
!               CHECK FOR PRESENCE OF STRING OR NUMERIC WORDS
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
         IF(SST.EQ.1.OR.SN.EQ.1)THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"UNITS" COMMAND ONLY TAKES QUALIFIER WORD INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF

!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
!
         IF(SQ.EQ.0) THEN
            OUTLYNE='"UNITS" REQUIRES EXPLICIT QUALIFIER WORD INPUT"'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETURN
         END IF
         IF(WQ.EQ.'IN'.OR.WQ.EQ.'INCH'.OR.WQ.EQ.'INCHES') SYSTEM(6)=1.0D0
         IF(WQ.EQ.'CM') SYSTEM(6)=2.0D0
         IF(WQ.EQ.'MM') SYSTEM(6)=3.0D0
         IF(WQ.EQ.'M') SYSTEM(6)=4.0D0
!     2/93 THE ADJUSTMENT OF THE PLOTTED SCALE FACTOR
!
!     THE SCALE FACTOR IS:
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
!
!
         IF(WQ.NE.'IN'.AND.WQ.NE.'INCH'.AND.WQ.NE.'INCHES'.AND.WQ.NE.'CM'.AND.WQ.NE.'MM'.AND.WQ.NE.'M') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & 'INVALID UNITS DESCRIPTION REQUESTED'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
      END IF
   END IF
1000 FORMAT('UNITS = INCHES')
2000 FORMAT('UNITS = CENTIMETERS')
3000 FORMAT('UNITS = MILLIMETERS')
4000 FORMAT('UNITS = METERS')
   RETURN
END
! SUB STORIC.FOR
SUBROUTINE STORIC
!
   use DATLEN
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE STORIC WHICH IMPLEMENTS THE YTORIC OR XTORIC
!       COMMANDS AT THE LENS INPUT AND UPDATE LENS LEVEL.
!       THE TORIC COMMAND MUST BE ISSUED BEFORE TORIC DATA
!       CAN BE ENTERD FOR A SURFACE. WHEN A SURFACE IS CHANGED
!       FROM A Y-TORIC TO AN X-TORIC, ALL SOLVE DATA FOR THAT SURFACE
!       IS DELETED TO PREVENT CATASTROPHIC CHANGES TO THE LENS
!       SYSTEM. A MESSAGE TO THAT EFFECT IS PRINTED. USE PARAXIAL DATA
!       TO SET ANY NEW SOLVES. IN ORDER TO AVOID ANY ARBITRARY ASSIGNMENT OF
!       SOLVES, EXISTING SOLVES ARE DELETED WHEN A NON-TORIC SURFACE IS
!       DEFINED AS A Y-TORIC OR AN X-TORIC.
!
!
   IF(F5.EQ.1.OR.F6.EQ.1) THEN
!       AT LENS OR UPDATE LENS MODE
!
      IF(SURF.EQ.0) THEN
!       AT OBJECT SURFACE
         OUTLYNE='OBJECT SURFACE MAY NOT BE TORIC'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETURN
      END IF
!
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
      IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"X-TORIC" AND "Y-TORIC" TAKE NO EXPLICIT INPUT'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!
!       IF THE SURFACE WAS NOT A TORIC BEFORE THEN JUST SET IT AS
!       SUCH AND RETURN.
!
      IF(surf_toric_flag(SURF).EQ.0.0D0) THEN
!       FIX PIKUP PRO AND NPRO
         IF(PIKUP(1,SURF,11).GT.0.0D0) THEN
            PIKUP(1:6,SURF,11)=0.0D0
            call set_surf_special_type(SURF, surf_special_type(SURF)-1)
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (PRO) DELETED'
            CALL SHOWIT(1)
         END IF
         IF(PIKUP(1,SURF,12).GT.0.0D0) THEN
            PIKUP(1:6,SURF,12)=0.0D0
            call set_surf_special_type(SURF, surf_special_type(SURF)-1)
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (NPRO) DELETED'
            CALL SHOWIT(1)
         END IF
         IF(WC.EQ.'YTORIC')call set_surf_toric_flag(SURF, 1)
         IF(WC.EQ.'XTORIC')call set_surf_toric_flag(SURF, 2)
         IF(surf_asphere_coeff(SURF, 2).NE.0.0D0) THEN
            call set_surf_asphere_coeff(SURF, 2, 0.0D0)
            OUTLYNE='WARNING:'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'FOR SURFACE ',SURF
            CALL SHOWIT(1)
            OUTLYNE='THE "AC" TERM RESET TO 0.0D0 FOR THIS TORIC SURFACE'
            CALL SHOWIT(1)
         END IF
!
!       SET TORIC CURV = TO PROFILE CURVATURE
         IF(WC.EQ.'XTORIC'.OR.WC.EQ.'YTORIC')call set_surf_toric_curvature(SURF, surf_curvature(SURF))
         IF(surf_solve_flag(SURF).NE.0.0D0) THEN
!       THERE WERE SOLVES. PRINT MESSAGE THEN DELETE ALL SOLVES FOR
!       SURF.
            IF(SOLVE(6,SURF).GT.0.0D0)THEN
               OUTLYNE='YZ PLANE THICKNESS SOLVE DELETED'
               CALL SHOWIT(1)
            END IF
            IF(SOLVE(4,SURF).GT.0.0D0) THEN
               OUTLYNE='XZ PLANE THICKNESS SOLVE DELETED'
               CALL SHOWIT(1)
            END IF
            IF(SOLVE(8,SURF).GT.0.0D0) THEN
               OUTLYNE='YZ PLANE THICKNESS SOLVE DELETED'
               CALL SHOWIT(1)
            END IF
            IF(SOLVE(2,SURF).GT.0.0D0) THEN
               OUTLYNE='XZ PLANE THICKNESS SOLVE DELETED'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'SURFACE',SURF,' :NO SOLVES REMAIN'
               CALL SHOWIT(1)
            END IF
            call set_surf_solve_flag(SURF, 0.0D0)
            SOLVE(0:9,SURF)=0.0D0
         ELSE
         END IF
         RETURN
!
      ELSE
      END IF
!
!       surf_toric_flag(SURF) SHOULD BE EITHER 1.0D0 OR 2.0D0,ITS NOT ZER0
!
      IF(surf_toric_flag(SURF).NE.1.0D0.AND.surf_toric_flag(SURF).NE.2.0D0) THEN
         OUTLYNE='SERIOUS ERROR IN ASSIGNING TORICS'
         CALL SHOWIT(1)
         RETURN
      END IF
!       TORIC TYPE CHANGE OR REDEFINED
!
      IF(WC.EQ.'YTORIC'.AND.surf_toric_flag(SURF).EQ.1.0D0) THEN
         WRITE(OUTLYNE,*)'SURFACE',SURF,' ALREADY DEFINED AS Y-TORIC'
         CALL SHOWIT(1)
         RETURN
      END IF
      IF(WC.EQ.'XTORIC'.AND.surf_toric_flag(SURF).EQ.2.0D0) THEN
         WRITE(OUTLYNE,*)'SURFACE',SURF,' ALREADY DEFINED AS X-TORIC'
         CALL SHOWIT(1)
         RETURN
      ELSE
      END IF
      IF(WC.EQ.'YTORIC'.AND.surf_toric_flag(SURF).EQ.2.0D0) THEN
         WRITE(OUTLYNE,*)'SURFACE',SURF,' X-TORIC CHANGED TO Y-TORIC'
         CALL SHOWIT(1)
         IF(PIKUP(1,SURF,11).GT.0.0D0) THEN
            PIKUP(1:6,SURF,11)=0.0D0
            call set_surf_special_type(SURF, surf_special_type(SURF)-1)
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (PRO) DELETED'
            CALL SHOWIT(1)
         END IF
         IF(PIKUP(1,SURF,12).GT.0.0D0) THEN
            PIKUP(1:6,SURF,12)=0.0D0
            call set_surf_special_type(SURF, surf_special_type(SURF)-1)
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (NPRO) DELETED'
            CALL SHOWIT(1)
         END IF
         call set_surf_toric_flag(SURF, 1)
         IF(surf_solve_flag(SURF).NE.0.0D0) THEN
!       THERE WERE SOLVES. PRINT MESSAGE THEN DELETE ALL SOLVES FOR
!       SURF.
            IF(SOLVE(6,SURF).GT.0.0D0) THEN
               OUTLYNE='YZ PLANE THICKNESS SOLVE DELETED'
               CALL SHOWIT(1)
            END IF
            IF(SOLVE(4,SURF).GT.0.0D0) THEN
               OUTLYNE='XZ PLANE THICKNESS SOLVE DELETED'
               CALL SHOWIT(1)
            END IF
            IF(SOLVE(8,SURF).GT.0.0D0) THEN
               OUTLYNE='YZ PLANE THICKNESS SOLVE DELETED'
               CALL SHOWIT(1)
            END IF
            IF(SOLVE(2,SURF).GT.0.0D0) THEN
               OUTLYNE='XZ PLANE THICKNESS SOLVE DELETED'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'SURFACE',SURF,' :NO SOLVES REMAIN'
               CALL SHOWIT(1)
            END IF
            call set_surf_solve_flag(SURF, 0.0D0)
            SOLVE(0:9,SURF)=0.0D0
         ELSE
         END IF
         RETURN
      ELSE
      END IF
      IF(WC.EQ.'XTORIC'.AND.surf_toric_flag(SURF).EQ.1.0D0) THEN
         WRITE(OUTLYNE,*)'SURFACE',SURF,' Y-TORIC CHANGED TO X-TORIC'
         CALL SHOWIT(1)
         IF(PIKUP(1,SURF,11).GT.0.0D0) THEN
            PIKUP(1:6,SURF,11)=0.0D0
            call set_surf_special_type(SURF, surf_special_type(SURF)-1)
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (PRO) DELETED'
            CALL SHOWIT(1)
         END IF
         IF(PIKUP(1,SURF,12).GT.0.0D0) THEN
            PIKUP(1:6,SURF,12)=0.0D0
            call set_surf_special_type(SURF, surf_special_type(SURF)-1)
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (NPRO) DELETED'
            CALL SHOWIT(1)
         END IF
         call set_surf_toric_flag(SURF, 2)
         IF(surf_solve_flag(SURF).NE.0.0D0) THEN
!       THERE WERE SOLVES. PRINT MESSAGE THEN DELETE ALL SOLVES FOR
!       SURF.
            IF(SOLVE(6,SURF).GT.0.0D0) THEN
               OUTLYNE='YZ PLANE THICKNESS SOLVE DELETED'
               CALL SHOWIT(1)
            END IF
            IF(SOLVE(4,SURF).GT.0.0D0) THEN
               OUTLYNE='XZ PLANE THICKNESS SOLVE DELETED'
               CALL SHOWIT(1)
            END IF
            IF(SOLVE(8,SURF).GT.0.0D0) THEN
               OUTLYNE='YZ PLANE THICKNESS SOLVE DELETED'
               CALL SHOWIT(1)
            END IF
            IF(SOLVE(2,SURF).GT.0.0D0) THEN
               OUTLYNE='XZ PLANE THICKNESS SOLVE DELETED'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'SURFACE',SURF,' :NO SOLVES REMAIN'
               CALL SHOWIT(1)
            END IF
            call set_surf_solve_flag(SURF, 0.0D0)
            SOLVE(0:9,SURF)=0.0D0
         ELSE
         END IF
         RETURN
      ELSE
      END IF
   ELSE
   END IF
   RETURN
END
! SUB STORD.FOR
SUBROUTINE STORD
!
   use DATLEN
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE STORD WHICH IMPLEMENTS THE TORD
!       COMMAND AT THE LENS INPUT AND UPDATE LENS LEVEL.
!
   INTEGER I,SF
!
!
   IF(SQ.EQ.1.OR.SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"TORD" TAKES NO STRING OR QUALIFIER'//'\n'//&
      & 'OR NUMERIC WORD #3 THROUGH #5 INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(DF1.EQ.1.AND.DF2.EQ.1) THEN
      IF(SURF.EQ.0) THEN
         CALL REPORT_ERROR_AND_FAIL('OBJECT SURFACE IS ALWAYS PLANO'//'\n'//'RE-ENTER COMMAND', 1)
         RETURN
      ELSE
      END IF
      W1=DBLE(SURF)
      W2=DBLE(SURF)
      S1=1
      S2=1
      DF1=0
      DF2=0
   END IF
   IF(DF1.EQ.1.AND.DF2.EQ.0.OR.DF1.EQ.0.AND.DF2.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"TORD" USES EITHER TWO OR ZERO NUMERIC WORDS'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   ELSE
!       PROCEED
   END IF
   IF(INT(W1).LT.0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'STARTING SURFACE NUMBER MUST BE GREATER THAN OR EQUAL TO 0'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(INT(W2).GT.INT(SYSTEM(20))) THEN
      WRITE(OUTLYNE,*)'ENDING SURFACE NUMBER MUST BE LESS THAN OR EQUAL TO ',INT(SYSTEM(20))
      CALL SHOWIT(1)
      CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(W1.GT.W2) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'THE ENDING SURFACE # MUST BE GREATER THAN OR EQUAL TO#'//'\n'//&
      & 'THE STARTING SURFACE #'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   DO SF=INT(W1),INT(W2)

!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
!
!       IF THE SURFACE WAS NOT A TORIC BEFORE THEN JUST PRINT A
!       MESSAGE AND RETURN.
!
      IF(surf_toric_flag(SF).EQ.0.0D0) THEN
         WRITE(OUTLYNE,*)'SURFACE',SF,' NOT DEFINED AS TORIC'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      ELSE
!
!       IF THE SURFACE WAS A Y-TORIC THEN GET RID OF ALL XZ PLANE
!       SOLVES. IF X-TORIC, GET RID OF ALL YZ PLANE SOLVES
!       SURFACE IS A TORIC
!
         IF(surf_toric_flag(SF).EQ.1.0D0) THEN
!       Y-TORIC,GET RIDE OF XZ PLANE SOLVES, ALL OF THEM
            IF(SOLVE(4,SF).GT.0.0D0.OR.SOLVE(2,SF).GT.0.0D0 .OR.SOLVE(4,SF).GT.0.0D0.AND.SOLVE(2,SF).GT.0.0D0)THEN
               SOLVE(4,SF)=0.0D0
               SOLVE(3,SF)=0.0D0
               SOLVE(2,SF)=0.0D0
               SOLVE(1,SF)=0.0D0
               WRITE(OUTLYNE,*)'SURFACE',SF,' :ALL XZ PLANE SOLVES DELETED'
               CALL SHOWIT(1)
            END IF
         END IF
         IF(surf_toric_flag(SF).EQ.2.0D0) THEN
!       X-TORIC,GET RIDE OF YZ PLANE SOLVES, ALL OF THEM
            IF(SOLVE(6,SF).GT.0.0D0.OR.SOLVE(8,SF).GT.0.0D0 .OR.SOLVE(6,SF).GT.0.0D0.AND.SOLVE(8,SF).GT.0.0D0)THEN
               SOLVE(6,SF)=0.0D0
               SOLVE(7,SF)=0.0D0
               SOLVE(8,SF)=0.0D0
               SOLVE(9,SF)=0.0D0
               WRITE(OUTLYNE,*)'SURFACE',SF,' :ALL YZ PLANE SOLVES DELETED'
               CALL SHOWIT(1)
            END IF
!       THEN MAKE ALL EXISTING XZ PLANE SOLVES INTO YZ PLANE SOLVES
            IF(SOLVE(4,SF).GT.0.0D0.OR.SOLVE(2,SF).GT.0.0D0.OR.SOLVE(4,SF).GT.0.0D0.AND.SOLVE(2,SF).GT.0.0D0) THEN
               IF(SOLVE(4,SF).EQ.4.0D0) SOLVE(6,SF)=1.0D0
               IF(SOLVE(4,SF).EQ.5.0D0) SOLVE(6,SF)=2.0D0
               IF(SOLVE(4,SF).EQ.6.0D0) SOLVE(6,SF)=3.0D0
               IF(SOLVE(2,SF).EQ.8.0D0) SOLVE(8,SF)=1.0D0
               IF(SOLVE(2,SF).EQ.9.0D0) SOLVE(8,SF)=2.0D0
               IF(SOLVE(2,SF).EQ.10.0D0) SOLVE(8,SF)=3.0D0
               IF(SOLVE(2,SF).EQ.11.0D0) SOLVE(8,SF)=4.0D0
               IF(SOLVE(2,SF).EQ.12.0D0) SOLVE(8,SF)=5.0D0
               IF(SOLVE(2,SF).EQ.13.0D0) SOLVE(8,SF)=6.0D0
               IF(SOLVE(2,SF).EQ.14.0D0) SOLVE(8,SF)=7.0D0
               SOLVE(7,SF)=SOLVE(3,SF)
               SOLVE(9,SF)=SOLVE(1,SF)
!       THEN ERASE THE OLD XZ SOLVES
               SOLVE(4,SF)=0.0D0
               SOLVE(3,SF)=0.0D0
               SOLVE(2,SF)=0.0D0
               SOLVE(1,SF)=0.0D0
               WRITE(OUTLYNE,*)'SURFACE',SF,' :ALL XZ PLANE SOLVES CONVERTED TO YZ PLANE SOLVES'
               CALL SHOWIT(1)
            END IF
         END IF
!       RECALCULATE surf_solve_flag(SF)
         call set_surf_solve_flag(SF, 0.0D0)
         IF(SOLVE(6,SF).GT.0.0D0)call set_surf_solve_flag(SF, surf_solve_flag(SF)+1.0D0)
         IF(SOLVE(4,SF).GT.0.0D0)call set_surf_solve_flag(SF, surf_solve_flag(SF)+0.1D0)
         IF(SOLVE(8,SF).GT.0.0D0)call set_surf_solve_flag(SF, surf_solve_flag(SF)+2.0D0)
         IF(SOLVE(2,SF).GT.0.0D0)call set_surf_solve_flag(SF, surf_solve_flag(SF)+0.2D0)
!
!       IF THERE ARE RDTOR OR CVTOR PIKUPS ON THIS SURFACE THEY MUST GO
!       ALSO TORIC CONIC AND ASPHERIC PIKUPS AND ASPHERIC TORIC
!       DEFINITIONS.
!
!
         IF(PIKUP(1,SF,9).EQ.1.0D0) THEN
!       THERE ARE PIKUPS TO REMOVE
            PIKUP(1:6,SF,9)=0.0D0
            WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (CVTOR) DELETED'
            CALL SHOWIT(1)
            call set_surf_special_type(SF, surf_special_type(SF)-1)
         END IF
         IF(PIKUP(1,SF,10).EQ.1.0D0) THEN
            PIKUP(1:6,SF,10)=0.0D0
            WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (RDTOR) DELETED'
            CALL SHOWIT(1)
            call set_surf_special_type(SF, surf_special_type(SF)-1)
         END IF
         IF(PIKUP(1,SF,21).EQ.1.0D0) THEN
            PIKUP(1:6,SF,21)=0.0D0
            WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (CCTOR) DELETED'
            CALL SHOWIT(1)
            call set_surf_special_type(SF, surf_special_type(SF)-1)
         END IF
         IF(PIKUP(1,SF,22).EQ.1.0D0) THEN
            PIKUP(1:6,SF,22)=0.0D0
            WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (ADTOR) DELETED'
            CALL SHOWIT(1)
            call set_surf_special_type(SF, surf_special_type(SF)-1)
         END IF
         IF(PIKUP(1,SF,23).EQ.1.0D0) THEN
            PIKUP(1:6,SF,23)=0.0D0
            WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (AETOR) DELETED'
            CALL SHOWIT(1)
            call set_surf_special_type(SF, surf_special_type(SF)-1)
         END IF
         IF(PIKUP(1,SF,24).EQ.1.0D0) THEN
            PIKUP(1:6,SF,24)=0.0D0
            WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (AFTOR) DELETED'
            CALL SHOWIT(1)
            call set_surf_special_type(SF, surf_special_type(SF)-1)
         END IF
         IF(PIKUP(1,SF,25).EQ.1.0D0) THEN
            PIKUP(1:6,SF,25)=0.0D0
            WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (AGTOR) DELETED'
            CALL SHOWIT(1)
            call set_surf_special_type(SF, surf_special_type(SF)-1)
         END IF
         IF(PIKUP(1,SF,26).EQ.1.0D0) THEN
            PIKUP(1:6,SF,26)=0.0D0
            WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (AC) DELETED'
            CALL SHOWIT(1)
            call set_surf_special_type(SF, surf_special_type(SF)-1)
         END IF
!       DUMP PIKUP PRO AND NPRO IF FOUND
         IF(PIKUP(1,SF,11).GT.0.0D0) THEN
            PIKUP(1:6,SF,11)=0.0D0
            call set_surf_special_type(SF, surf_special_type(SF)-1)
            WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (PRO) DELETED'
            CALL SHOWIT(1)
         END IF
         IF(PIKUP(1,SF,12).GT.0.0D0) THEN
            PIKUP(1:6,SF,12)=0.0D0
            call set_surf_special_type(SF, surf_special_type(SF)-1)
            WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (NPRO) DELETED'
            CALL SHOWIT(1)
         END IF
!
!
!****************************************************************
!       IF ANY SURFACE IN THE LENS IS PIKING UP TORIC DATA FROM
!       THIS SURFACE, THOSE PIKUPS MUST ALSO GO. ALL PIKED
!       UP DATA IS FROZEN AT ITS CURRENT VALUES.
!
         DO 31 I=0,INT(SYSTEM(20))

            IF(PIKUP(2,I,9).EQ.DBLE(SF).AND.PIKUP(1,I,9).NE.0.0D0 .OR.PIKUP(2,I,10).EQ.DBLE(SF).AND.PIKUP(1,I,10).NE.0.0D0 .OR.PIKUP(2,I,21).EQ.DBLE(SF).AND.PIKUP(1,I,21).NE.0.0D0 .OR.PIKUP(2,I,22).EQ.DBLE(SF).AND.PIKUP(1,I,22).NE.0.0D0 .OR.PIKUP(2,I,23).EQ.DBLE(SF).AND.PIKUP(1,I,23).NE.0.0D0 .OR.PIKUP(2,I,24).EQ.DBLE(SF).AND.PIKUP(1,I,24).NE.0.0D0 .OR.PIKUP(2,I,25).EQ.DBLE(SF).AND.PIKUP(1,I,25).NE.0.0D0 .OR.PIKUP(2,I,27).EQ.DBLE(SF).AND.PIKUP(1,I,27).NE.0.0D0)THEN

!
!       SURFACE I IS PIKING UP TORIC DATA FROM SURFACE SF
!       DELETE ALL TORIC PIKUPS FROM SURFACE I
!
               IF(PIKUP(1,I,9).EQ.1.0D0) THEN
                  PIKUP(1:6,I,9)=0.0D0
                  call set_surf_special_type(I, surf_special_type(I)-1)
               ELSE
               END IF
               IF(PIKUP(1,I,10).EQ.1.0D0) THEN
                  PIKUP(1:6,I,10)=0.0D0
                  call set_surf_special_type(I, surf_special_type(I)-1)
               ELSE
               END IF
               IF(PIKUP(1,I,21).EQ.1.0D0) THEN
                  PIKUP(1:6,I,21)=0.0D0
                  call set_surf_special_type(I, surf_special_type(I)-1)
               ELSE
               END IF
               IF(PIKUP(1,I,22).EQ.1.0D0) THEN
                  PIKUP(1:6,I,22)=0.0D0
                  call set_surf_special_type(I, surf_special_type(I)-1)
               ELSE
               END IF
               IF(PIKUP(1,I,23).EQ.1.0D0) THEN
                  PIKUP(1:6,I,23)=0.0D0
                  call set_surf_special_type(I, surf_special_type(I)-1)
               ELSE
               END IF
               IF(PIKUP(1,I,24).EQ.1.0D0) THEN
                  PIKUP(1:6,I,24)=0.0D0
                  call set_surf_special_type(I, surf_special_type(I)-1)
               ELSE
               END IF
               IF(PIKUP(1,I,25).EQ.1.0D0) THEN
                  PIKUP(1:6,I,25)=0.0D0
                  call set_surf_special_type(I, surf_special_type(I)-1)
               ELSE
               END IF
               IF(PIKUP(1,I,27).EQ.1.0D0) THEN
                  PIKUP(1:6,I,27)=0.0D0
                  call set_surf_special_type(I, surf_special_type(I)-1)
               ELSE
               END IF
               WRITE(OUTLYNE,*)'SURFACE',I,' :ALL TORIC PIKUPS DELETED'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'WHICH REFERENCED SURFACE',SF
               CALL SHOWIT(1)
            END IF
!
31       CONTINUE
!       IF ANY SURFACE IN THE LENS IS PIKING UP PRO/NPRO DATA FROM
!       THIS SURFACE, THOSE PIKUPS MUST ALSO GO. ALL PIKED
!       UP DATA IS FROZEN AT ITS CURRENT VALUES.
!
         DO 32 I=0,INT(SYSTEM(20))

            IF(PIKUP(2,I,11).EQ.DBLE(SF).AND.PIKUP(1,I,11).NE.0.0D0 .OR.PIKUP(2,I,12).EQ.DBLE(SF).AND.PIKUP(1,I,12).NE.0.0D0 )THEN

!
!       SURFACE I IS PIKING UP PRO/NPRO DATA FROM SURFACE SF
!       DELETE ALL PRO/NPRO PIKUPS FROM SURFACE I
!
               IF(PIKUP(1,I,11).EQ.1.0D0) THEN
                  PIKUP(1:6,I,11)=0.0D0
                  call set_surf_special_type(I, surf_special_type(I)-1)
                  WRITE(OUTLYNE,*)'SURFACE',I,' :PIKUP (PRO) DELETED'
                  CALL SHOWIT(1)
               END IF
               IF(PIKUP(1,I,12).EQ.1.0D0) THEN
                  PIKUP(1:6,I,12)=0.0D0
                  call set_surf_special_type(I, surf_special_type(I)-1)
                  WRITE(OUTLYNE,*)'SURFACE',I,' :PIKUP (NPRO) DELETED'
                  CALL SHOWIT(1)
               END IF
!
            ELSE
!       NO TORIC PIKUPS FOUND REFERENCING SURFACE SF,PROCEED
            END IF
!
32       CONTINUE
!
!       NOW DUMP THE TORIC DATA COMPLETELY
!
         ALENS(23:24,SF)=0.0D0
         ALENS(36:41,SF)=0.0D0
         WRITE(OUTLYNE,*)'TORIC DELETED FROM SURFACE',SF
         CALL SHOWIT(1)
         RETURN
      END IF
   END DO
   RETURN
END
! SUB STILTD.FOR
SUBROUTINE STILTD
!
   use DATLEN
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE STILTD WHICH IMPLEMENTS THE TILTD COMMAND
!       AT THE LENS UPDATE LEVEL.
!       THIS REMOVES ALL TILTS BUT NOT DECENTERS ON A SURFACE.
!
   INTEGER PIKCNT,I,J,K,SF
!
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SQ.EQ.1.OR.SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"TILTD" TAKES NO STRING OR QUALIFIER'//'\n'//&
      & 'OR NUMERIC WORD #3 THROUGH #5 INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(DF1.EQ.1.AND.DF2.EQ.1) THEN
      W1=DBLE(SURF)
      W2=DBLE(SURF)
      S1=1
      S2=1
      DF1=0
      DF2=0
   END IF
   IF(DF1.EQ.1.AND.DF2.EQ.0.OR.DF1.EQ.0.AND.DF2.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"TILTD" USES EITHER TWO OR ZERO NUMERIC WORDS'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(INT(W1).LT.0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'STARTING SURFACE NUMBER MUST BE GREATER THAN OR EQUAL TO 0'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(INT(W2).GT.INT(SYSTEM(20))) THEN
      WRITE(OUTLYNE,*)'ENDING SURFACE NUMBER MUST BE LESS THAN OR EQUAL TO ',INT(SYSTEM(20))
      CALL SHOWIT(1)
      CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(W1.GT.W2) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'THE ENDING SURFACE # MUST BE GREATER THAN OR EQUAL TO#'//'\n'//&
      & 'THE STARTING SURFACE #'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   DO SF=INT(W1),INT(W2)
!
      IF(SF.EQ.0.AND.W1.EQ.W2) THEN
         CALL REPORT_ERROR_AND_FAIL('OBJECT SURFACE IS NEVER TILTED'//'\n'//'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!
!       WE ARE AT THE LENS UPDATE LEVEL
!
!
!       FIRST SET THE TILT STATUS FLAG surf_tilt_flag(SF) TO ZERO
!       AND SET surf_alpha(SF) TO surf_gamma(SF) TO ZERO
!
      ALENS(25:28,SF)=0.0D0
      ALENS(118:120,SF)=0.0D0
      ALENS(90:95,SF)=0.0D0
      call set_surf_tilt_return_flag(SF, 0)
!       WHAT IF THE SURFACE HAD ALPHA,BETA OR GAMMA PIKUPS ON IT?
!
      DO I=15,17
         IF(PIKUP(1,SF,I).NE.0.0D0) THEN
            PIKUP(1:6,SF,I)=0.0D0
!       FIX THE PIKUP COUNTER
            call set_surf_special_type(SF, surf_special_type(SF)-1)
            IF(I.EQ.15) THEN
               WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (ALPHA) DELETED'
               CALL SHOWIT(1)
            END IF
            IF(I.EQ.16) THEN
               WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (BETA) DELETED'
               CALL SHOWIT(1)
            END IF
            IF(I.EQ.17) THEN
               WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (GAMMA) DELETED'
               CALL SHOWIT(1)
            END IF
         END IF
      END DO
!       WHAT IF THE SURFACE HAD ALPHA,BETA OR GAMMA PIKUPS ON IT?
!
      DO I=37,42
         IF(PIKUP(1,SF,I).NE.0.0D0) THEN
            PIKUP(1:6,SF,I)=0.0D0
!       FIX THE PIKUP COUNTER
            call set_surf_special_type(SF, surf_special_type(SF)-1)
            IF(I.EQ.37) THEN
               WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (GDX) DELETED'
               CALL SHOWIT(1)
            END IF
            IF(I.EQ.38) THEN
               WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (GDY) DELETED'
               CALL SHOWIT(1)
            END IF
            IF(I.EQ.39) THEN
               WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (GDZ) DELETED'
               CALL SHOWIT(1)
            END IF
            IF(I.EQ.40) THEN
               WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (GALPHA) DELETED'
               CALL SHOWIT(1)
            END IF
            IF(I.EQ.41) THEN
               WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (GBETA) DELETED'
               CALL SHOWIT(1)
            END IF
            IF(I.EQ.42) THEN
               WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (GGAMMA) DELETED'
               CALL SHOWIT(1)
            END IF
         END IF
      END DO
!
      PIKCNT=0
      DO 503 J=1,PSIZ
         IF(PIKUP(1,SF,J).NE.0.0D0) THEN
            PIKCNT=PIKCNT+1
         ELSE
         END IF
503   CONTINUE
      IF(PIKCNT.EQ.0)call set_surf_special_type(SF, 0)

!
!       WHAT IF THIS SURFACE WAS THE TARGET OF AN ALPHA,BETE OR GAMMA PIKUP
!       OR DECENTERS
!               PIKUP(I,J,K) WHERE K IS 15,16, OR 17
!       IF SO THEN THE PIKUP MUST BE DELETED AND THE TILT
!       DATA FROZEN ON THE PIKUP SURFACE AT THEIR CURRENT VALUES.
      DO 300 I=0,INT(SYSTEM(20))
         DO J=15,17
            IF(PIKUP(1,I,J).EQ.1.0D0) THEN
!       DOES IT REFER TO SURFACE SF
               IF(INT(PIKUP(2,I,J)).EQ.SF) THEN
!       YES IT REFERS TO THE SURFACE SF WHICH IS HAVING ITS TILT
!       DELETED SO GET RIDE OF THE PIKUP
                  PIKUP(1:6,I,J)=0.0D0
                  call set_surf_special_type(I, surf_special_type(I)-1)
                  IF(J.EQ.15) THEN
                     WRITE(OUTLYNE,*)'(ALPHA) PIKUP DELETED ON SURFACE',I
                     CALL SHOWIT(1)
                  END IF
                  IF(J.EQ.16) THEN
                     WRITE(OUTLYNE,*)'(BETA) PIKUP DELETED ON SURFACE',I
                     CALL SHOWIT(1)
                  END IF
                  IF(J.EQ.17) THEN
                     WRITE(OUTLYNE,*)'(GAMMA) PIKUP DELETED ON SURFACE',I
                     CALL SHOWIT(1)
                  END IF
               END IF
            END IF
         END DO
         DO J=37,42
            IF(PIKUP(1,I,J).EQ.1.0D0) THEN
!       DOES IT REFER TO SURFACE SF
               IF(INT(PIKUP(2,I,J)).EQ.SF) THEN
!       YES IT REFERS TO THE SURFACE SF WHICH IS HAVING ITS TILT
!       DELETED SO GET RIDE OF THE PIKUP
                  PIKUP(1:6,I,J)=0.0D0
                  call set_surf_special_type(I, surf_special_type(I)-1)
                  IF(J.EQ.37) THEN
                     WRITE(OUTLYNE,*)'(GDX) PIKUP DELETED ON SURFACE',I
                     CALL SHOWIT(1)
                  END IF
                  IF(J.EQ.38) THEN
                     WRITE(OUTLYNE,*)'(GDY) PIKUP DELETED ON SURFACE',I
                     CALL SHOWIT(1)
                  END IF
                  IF(J.EQ.39) THEN
                     WRITE(OUTLYNE,*)'(GDZ) PIKUP DELETED ON SURFACE',I
                     CALL SHOWIT(1)
                  END IF
                  IF(J.EQ.40) THEN
                     WRITE(OUTLYNE,*)'(GALPHA) PIKUP DELETED ON SURFACE',I
                     CALL SHOWIT(1)
                  END IF
                  IF(J.EQ.41) THEN
                     WRITE(OUTLYNE,*)'(GBETA) PIKUP DELETED ON SURFACE',I
                     CALL SHOWIT(1)
                  END IF
                  IF(J.EQ.42) THEN
                     WRITE(OUTLYNE,*)'(GGAMMA) PIKUP DELETED ON SURFACE',I
                     CALL SHOWIT(1)
                  END IF
               END IF
            END IF
         END DO
300   CONTINUE
!
!       NOW FIX ALL THE surf_special_type(K) IN THE LENS SYSTEM
!
      DO 400 I=0,INT(SYSTEM(20))
!       CHECK PIKUPS
         PIKCNT=0
         DO 401 J=1,PSIZ
            IF(PIKUP(1,I,J).EQ.1.0D0) THEN
               PIKCNT=PIKCNT+1
            ELSE
            END IF
401      CONTINUE
         IF(PIKCNT.EQ.0)call set_surf_special_type(I, 0)
400   CONTINUE
   END DO
   RETURN
END
! SUB STILTAD.FOR
SUBROUTINE STILTAD
!
   use DATLEN
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE STILT WHICH IMPLEMENTS THE TILT COMMANDS
!       TILT AUTOD
!       COMMAND AT THE LENS INPUT LEVEL OR THE LENS UPDATE LEVEL.
!
   INTEGER PIKCNT,I,J,K,SF
!
!
   IF(SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"TILT AUTOD"'//'\n'//&
      & 'TAKES NO STRING OR NUMERIC WORD #3 THROUGH #5 INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(DF1.EQ.1.AND.DF2.EQ.1) THEN
      W1=DBLE(SURF)
      W2=DBLE(SURF)
      S1=1
      S2=1
      DF1=0
      DF2=0
   END IF
   IF(DF1.EQ.1.AND.DF2.EQ.0.OR.DF1.EQ.0.AND.DF2.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"TILT AUTOD"'//'\n'//&
      & 'USES EITHER TWO OR ZERO NUMERIC WORDS'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(INT(W1).LT.0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'STARTING SURFACE NUMBER MUST BE GREATER THAN OR EQUAL TO 0'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(INT(W2).GT.INT(SYSTEM(20))) THEN
      WRITE(OUTLYNE,*)'ENDING SURFACE NUMBER MUST BE LESS THAN OR EQUAL TO ',INT(SYSTEM(20))
      CALL SHOWIT(1)
      CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(W1.GT.W2) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'THE ENDING SURFACE # MUST BE GREATER THAN OR EQUAL TO#'//'\n'//&
      & 'THE STARTING SURFACE #'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   DO SF=INT(W1),INT(W2)
!
      IF(SF.EQ.0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'OBJECT SURFACE MAY NOT BE TILTED OR DECENTERED'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!
!       WE ARE AT LENS INPUT LEVEL OR LENS UPDATE LEVEL
!
!               CHECK FOR VALID QUALIFIERS, PROCEED
!
!       TILT AUTOD CHANGES TILT AUTO AND TILT AUTOM INTO
!       ORDINARY TILTS.
!                       1.0= TILT AUTOD IF IT WAS 2.0
!                       1.0= TILT AUTOD IF IT WAS 3.0
!
      IF(surf_tilt_flag(SF).NE.2.0D0.AND.surf_tilt_flag(SF).NE.3.0D0) THEN
         WRITE(OUTLYNE,*)'SURFACE',SF,' :NOT DEFINED AS TILT AUTO OR TILT AUTOM'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(surf_tilt_flag(SF).EQ.2.0D0) THEN
         call set_surf_tilt_flag(SF, 1)
      ELSE
      END IF
      IF(surf_tilt_flag(SF).EQ.3.0D0) THEN
         call set_surf_tilt_flag(SF, 1)
      ELSE
      END IF
!       RESOLVE PIKUPS ALPHA,BETA AND GAMMA IF THE EXIST
!       USING THE TILT COMMAND HAS THE SAME EFFECT AS IF
!       A TILTD COMMAND WERE USED FIRST. HANDLE PIKUPS
!       THE SAME WAY THEY ARE HANDLED IN (STILTD)
!       WHAT IF THE SURFACE HAD ALPHA,BETA OR GAMMA PIKUPS ON IT?
!
      DO 500 I=15,17
         IF(PIKUP(1,SF,I).NE.0.0D0) THEN
            PIKUP(1:6,SF,I)=0.0D0
!       FIX THE PIKUP COUNTER
            call set_surf_special_type(SF, surf_special_type(SF)-1)
            IF(I.EQ.15) THEN
               WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (ALPHA) DELETED'
               CALL SHOWIT(1)
            END IF
            IF(I.EQ.16) THEN
               WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (BETA) DELETED'
               CALL SHOWIT(1)
            END IF
            IF(I.EQ.17) THEN
               WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (GAMMA) DELETED'
               CALL SHOWIT(1)
            END IF
         END IF
500   CONTINUE
!
      PIKCNT=0
      DO 503 J=1,PSIZ
         IF(PIKUP(1,SF,J).NE.0.0D0) THEN
            PIKCNT=PIKCNT+1
         ELSE
         END IF
503   CONTINUE
      IF(PIKCNT.EQ.0)call set_surf_special_type(SF, 0)

!
!       WHAT IF THIS SURFACE WAS THE TARGET OF AN ALPHA,BETA OR GAMMA PIKUP
!               PIKUP(I,J,K) WHERE K IS 15,16, OR 17
!       IF SO THEN THE PIKUP MUST BE DELETED AND THE TILT
!       DATA FROZEN ON THE PIKUP SURFACE AT THEIR CURRENT VALUES.
      DO 300 I=0,INT(SYSTEM(20))
         DO 301 J=15,17
            IF(PIKUP(1,I,J).EQ.1.0D0) THEN
!       DOES IT REFER TO SURFACE SF
               IF(INT(PIKUP(2,I,J)).EQ.SF) THEN
!       YES IT REFERS TO THE SURFACE SF WHICH IS HAVING ITS TILT
!       DELETED SO GET RIDE OF THE PIKUP
                  PIKUP(1:6,I,J)=0.0D0
                  call set_surf_special_type(I, surf_special_type(I)-1)
                  IF(J.EQ.15) THEN
                     WRITE(OUTLYNE,*)'(ALPHA) PIKUP DELETED ON SURFACE',I
                     CALL SHOWIT(1)
                  END IF
                  IF(J.EQ.16) THEN
                     WRITE(OUTLYNE,*)'(BETA) PIKUP DELETED ON SURFACE',I
                     CALL SHOWIT(1)
                  END IF
                  IF(J.EQ.17) THEN
                     WRITE(OUTLYNE,*)'(GAMMA) PIKUP DELETED ON SURFACE',I
                     CALL SHOWIT(1)
                  END IF
               END IF
            END IF
301      CONTINUE
300   CONTINUE
!
!       NOW FIX ALL THE surf_special_type(K) IN THE LENS SYSTEM
!
      DO 400 I=0,INT(SYSTEM(20))
!       CHECK PIKUPS
         PIKCNT=0
         DO 401 J=1,PSIZ
            IF(PIKUP(1,I,J).EQ.1.0D0) THEN
               PIKCNT=PIKCNT+1
            ELSE
            END IF
401      CONTINUE
         IF(PIKCNT.EQ.0)call set_surf_special_type(I, 0)
400   CONTINUE
      RETURN
   END DO
END
! SUB STH.FOR
SUBROUTINE STH
!
   use DATLEN
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE STH WHICH IMPLEMENTS THE TH
!       COMMAND AT THE LENS INPUT AND UPDATE LENS LEVEL.
!       IF A THICKNESS
!       SOLVE EXISTS ON THE SURFACE, A THICKNESS
!       ASSIGNMENT WILL DELETE THE SOLVE.
!
   INTEGER PIKCNT,I
!
   REAL*8 X00,Y00,OLDX1,OLDY1,OLDTH,TH,SLOPE ,NEWY1,NEWX1,Y0ANG,X0ANG
!
!
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SST.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL('"TH" TAKES NO STRING INPUT'//'\n'//'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 .OR.S5.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"TH" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(SQ.EQ.1.AND.F5.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"TH" TAKES NO QUALIFIER WORD IN LENS INPUT MODE'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(F6.EQ.1) THEN
      IF(WQ.NE.'        '.AND.WQ.NE.'DELT'.AND.WQ.NE.'CENT') THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'INVALID QUALIFIER WORD USED WITH "TH"'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
   END IF
   IF(DF1.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"TH" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(F6.EQ.1) THEN
!       IF(surf_global_dx(SURF).NE.0.0D0.OR.surf_global_dy(SURF).NE.0.0D0.OR.
!    1surf_global_dz(SURF).NE.0.0D0.OR.surf_global_alpha(SURF).NE.0.0D0.OR.
!    2surf_global_beta(SURF).NE.0.0D0.OR.surf_global_gamma(SURF).NE.0.0D0) THEN
!       WRITE(OUTLYNE,*)
!    1'GLOBAL POSITIONING IS IN EFFECT AT THE CURRENT SURFACE'
!       CALL SHOWIT(1)
!       WRITE(OUTLYNE,*)
!    1'THE "TH" COMMAND IS NOT ALLOWED'
!       CALL SHOWIT(1)
!                       CALL MACFAL
!                       RETURN
!                       END IF
!
!       THE CASE OF CHANGING THE THICKNESS OF THE OBJECT SURFACE
!       IF OBJECT SURFACE, REMEMBER OLD VALUE
      IF(SURF.EQ.0.AND.SYSTEM(26).EQ.-99.0D0) THEN
         SYSTEM(55)=surf_thickness(SURF)
         IF(SQ.EQ.0)call set_surf_thickness(SURF, W1)
         IF(WQ.EQ.'DELT')call set_surf_thickness(SURF, surf_thickness(SURF)+W1)
         IF(WQ.EQ.'CENT')call set_surf_thickness(SURF, surf_thickness(SURF)+(W1*0.01D0*surf_thickness(SURF)))
!       MUST BE NO STOP AS WELL
         IF(SYSTEM(51).NE.0.0D0.OR.SYSTEM(53).NE.0.0D0) THEN
!       RECALCULATE Y1
            IF(SYSTEM(18).EQ.0.0D0) THEN
!       CASE OF SCY INPUT Y00
               Y00=SYSTEM(14)
               OLDY1=SYSTEM(15)
               OLDTH=SYSTEM(55)
               TH=surf_thickness(SURF)
               SLOPE=(OLDY1-Y00)/OLDTH
               NEWY1=Y00+(SLOPE*TH)
               SYSTEM(15)=NEWY1
               SYSTEM(22)=NEWY1
            ELSE
            END IF
            IF(SYSTEM(18).EQ.1.0D0) THEN
!       CASE OF SCY FANG INPUT Y0ANG
               Y0ANG=SYSTEM(21)
               OLDY1=SYSTEM(22)
               OLDTH=SYSTEM(55)
               TH=surf_thickness(SURF)
               Y00=-OLDTH*DTAN((PII/180.0D0)*Y0ANG)+OLDY1
               NEWY1=Y00+(DATAN((PII/180.0D0)*Y0ANG)*TH)
               SYSTEM(15)=NEWY1
               SYSTEM(22)=NEWY1
            ELSE
            END IF
         ELSE
         END IF
         IF(SYSTEM(52).NE.0.0D0.OR.SYSTEM(54).NE.0.0D0) THEN
!       RECALCULATE X1
            IF(SYSTEM(19).EQ.0.0D0) THEN
!       CASE OF SCX INPUT X00
               X00=SYSTEM(16)
               OLDX1=SYSTEM(17)
               OLDTH=SYSTEM(55)
               TH=surf_thickness(SURF)
               SLOPE=(OLDX1-X00)/OLDTH
               NEWY1=X00+(SLOPE*TH)
               SYSTEM(17)=NEWX1
               SYSTEM(24)=NEWX1
            ELSE
            END IF
            IF(SYSTEM(19).EQ.1.0D0) THEN
!       CASE OF SCX FANG INPUT X0ANG
               X0ANG=SYSTEM(23)
               OLDX1=SYSTEM(24)
               OLDTH=SYSTEM(55)
               TH=surf_thickness(SURF)
               X00=-OLDTH*DTAN((PII/180.0D0)*X0ANG)+OLDX1
               NEWX1=X00+(DATAN((PII/180.0D0)*X0ANG)*TH)
               SYSTEM(17)=NEWX1
               SYSTEM(24)=NEWX1
            ELSE
            END IF
         ELSE
         END IF
      ELSE
!       NOT SURFACE ZERO, PROCEED
      END IF
   ELSE
!       NOT AT UPDATE LENS, NO ACTION TAKEN
   END IF
   IF(SQ.EQ.0)call set_surf_thickness(SURF, W1)
   IF(WQ.EQ.'DELT')call set_surf_thickness(SURF, surf_thickness(SURF)+W1)
   IF(WQ.EQ.'CENT')call set_surf_thickness(SURF, surf_thickness(SURF)+(W1*0.01D0*surf_thickness(SURF)))
!       CHECK FOR A SOLVE
   IF(SOLVE(6,SURF).NE.0.0D0.OR.SOLVE(4,SURF).NE.0.0D0.OR.SOLVE(6,SURF).NE.0.0D0.AND. SOLVE(4,SURF).NE.0.0D0) THEN
!       THERE IS A THICKNESS SOLVE. REMOVE IT
      SOLVE(6,SURF)=0.0D0
      SOLVE(7,SURF)=0.0D0
      SOLVE(4,SURF)=0.0D0
      SOLVE(3,SURF)=0.0D0
      call LogTermFOR("MAG SOLVE DELETED HERE?")
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :THICKNESS SOLVE DELETED'
      CALL SHOWIT(1)
      call set_surf_solve_flag(SURF, 0.0D0)
      IF(SOLVE(6,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+1.0D0)
      IF(SOLVE(4,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+0.1D0)
      IF(SOLVE(8,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+2.0D0)
      IF(SOLVE(2,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+0.2D0)
   ELSE
!       NO SOLVE TO DELETE
   END IF
!
!       CHECK FOR A THICKNESS PIKUP AND IF FOUND,DELETE IT
!       THE PIKUP INDICATOR IS surf_special_type(SURF)
   IF(surf_special_type(SURF).EQ.0.0D0) THEN
!       NO PIKUPS OF ANY KIND, DONT DO ANYTHING,JUST RETURN
      RETURN
   ELSE
   END IF
!
!               THERE ARE PIKUPS, ARE THERE ANY THICKNESS PIKUPS
!
   IF(PIKUP(1,SURF,3).EQ.0.0D0.AND.PIKUP(1,SURF,32).EQ.0.0D0)THEN
!       NO THICKNESS PIKUP TO DELETE,JUST RETURN
      RETURN
   ELSE
   END IF
!
!       DELETE THICKNESS PIKUP
!
   IF(PIKUP(1,SURF,3).NE.0.0D0) THEN
      PIKUP(1:6,SURF,3)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
   END IF
   IF(PIKUP(1,SURF,32).NE.0.0D0) THEN
      PIKUP(1:6,SURF,32)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
   END IF
!
!       PRINT MESSAGE THAT PIKUP WAS DELETED
   WRITE(OUTLYNE,*)'SURFACE',SURF,' :THICKNESS PIKUP DELETED'
   CALL SHOWIT(1)
!
!       IF THERE ARE NO OTHER PIKUPS THEN SET THE PIKUP
!       INDICATOR TO ZERO, ELSE LEAVE IT AT 1.0D0
!
   PIKCNT=0
   DO 10 I=1,PSIZ
      IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
         PIKCNT=PIKCNT+1
      ELSE
      END IF
10 CONTINUE
   IF(PIKCNT.EQ.0) THEN
      call set_surf_special_type(SURF, 0)
   ELSE
!       DON'T DO ANYTHING, JUST RETURN
   END IF

!
   RETURN
END
! SUB SPRICE.FOR
SUBROUTINE SPRICE
!
   use DATLEN
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SPRICE WHICH IMPLEMENTS THE PRICE
!       COMMAND AT THE LENS INPUT AND UPDATE LENS LEVEL.
!
   INTEGER PIKCNT,I
!
   REAL*8 X00,Y00,OLDX1,OLDY1,OLDTH,TH,SLOPE ,NEWY1,NEWX1,Y0ANG,X0ANG
!
!
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SST.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL('"PRICE" TAKES NO STRING INPUT'//'\n'//'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 .OR.S5.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"PRICE" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(SQ.EQ.1.AND.F5.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"PRICE" TAKES NO QUALIFIER WORD IN LENS INPUT MODE'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(F6.EQ.1) THEN
      IF(WQ.NE.'        '.AND.WQ.NE.'DELT'.AND.WQ.NE.'CENT') THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'INVALID QUALIFIER WORD USED WITH "PRICE"'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
   END IF
   IF(DF1.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"PRICE" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(F6.EQ.1) THEN
      IF(SQ.EQ.0)call set_surf_price(SURF, W1)
      IF(WQ.EQ.'DELT')call set_surf_price(SURF, surf_price(SURF)+W1)
      IF(WQ.EQ.'CENT')call set_surf_price(SURF, surf_price(SURF)+(W1*0.01D0*surf_price(SURF)))
   END IF
   IF(F5.EQ.1) THEN
      IF(SQ.EQ.0)call set_surf_price(SURF, W1)
   END IF
   RETURN
END
! SUB STHM.FOR
SUBROUTINE STHM
!
   use DATLEN
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE STHM WHICH IMPLEMENTS THE THM
!       COMMAND AT THE LENS INPUT AND UPDATE LENS LEVEL.
!
   INTEGER PIKCNT,I
!
   REAL*8 X00,Y00,OLDX1,OLDY1,OLDTH,TH,SLOPE ,NEWY1,NEWX1,Y0ANG,X0ANG
!
!
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SST.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL('"THM" TAKES NO STRING INPUT'//'\n'//'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 .OR.S5.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"THM" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(SQ.EQ.1.AND.F5.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"THM" TAKES NO QUALIFIER WORD IN LENS INPUT MODE'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(F6.EQ.1) THEN
      IF(WQ.NE.'        '.AND.WQ.NE.'DELT'.AND.WQ.NE.'CENT') THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'INVALID QUALIFIER WORD USED WITH "THM"'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
   END IF
   IF(DF1.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"THM" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(F5.EQ.1) THEN
      IF(SQ.EQ.0)call set_surf_mirror_thickness(SURF, W1)
   END IF
   IF(F6.EQ.1) THEN
      IF(GLANAM(SURF,1).EQ.'             '.AND.GLANAM(SURF,2).EQ.'REFL         '.AND.F6.EQ.1) THEN
         IF(SQ.EQ.0)call set_surf_mirror_thickness(SURF, W1)
         IF(WQ.EQ.'DELT')call set_surf_mirror_thickness(SURF, surf_mirror_thickness(SURF)+W1)
         IF(WQ.EQ.'CENT')call set_surf_mirror_thickness(SURF, surf_mirror_thickness(SURF)+(W1*0.01D0*surf_mirror_thickness(SURF)))
      ELSE
         CALL REPORT_ERROR_AND_FAIL(&
         & '"THM" REQUIRES A MIRROR SURFACE AT THE UPDATE LENS LEVEL'//'\n'//&
         & 'NO ACTION TAKEN', 1)
         RETURN
      END IF
   END IF
   RETURN
END
! SUB SAUTOFUNC.FOR
SUBROUTINE SAUTOFUNC
!
   use DATLEN
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SAUTOFUNC WHICH IMPLEMENTS THE AUTOFUNC
!       COMMAND AT THE LENS INPUT AND UPDATE LENS LEVEL.
!
   INTEGER PIKCNT,I
!
!
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SST.EQ.1.OR.SQ.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"AUTOFUNC" TAKES NO STRING OR QUALIFIER INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 .OR.S5.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"AUTOFUNC" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(DF1.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"AUTOFUNC" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(INT(W1).NE.0.AND.INT(W1).NE.1.AND.INT(W1).NE.2 .AND.INT(W1).NE.3.AND.INT(W1).NE.4.AND.INT(W1).NE.5 .AND.INT(W1).NE.6.AND.INT(W1).NE.7.AND.INT(W1).NE.8 .AND.INT(W1).NE.9.AND.INT(W1).NE.10) THEN
      WRITE(OUTLYNE,*)'"',WC(1:8),'" REQUIRES EXPLICIT INTEGER NUMERIC WORD #1 INPUT'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'BETWEEN 0 AND 10'
      CALL SHOWIT(1)
      CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
      RETURN
   END IF
   SYSTEM(91)=INT(W1)
   F57=1
   RETURN
END
! SUB STASPH.FOR
SUBROUTINE STASPH
!
   use DATLEN
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE STASPH WHICH IMPLEMENTS THE TASPH
!       COMMAND AT THE LENS INPUT OR UPDATE LENS LEVEL.
!       THESE ARE THE ASPHERIC COEFS IN THE PLANE PERPENDICULAR
!       TO THOSE SET USING THE "ASPH" COMMAND AND ARE ONLY VALID
!       FOR TORIC SURFACES.
!       THE NUMERIC WORDS W1 W2 W3 AND W4 ARE THE
!       4TH, 6TH, 8TH AND 10TH ORDER ASPHERIC SURFACE PROFILE
!       COEFFICIENTS. THE DEFAULT AT LENS INITIALIZATION IS
!       ALL COEFFICIENTS = 0.0D0. IF THE SURFACE IS SET AS AN ANAMORPHIC
!       ASPHERIC, surf_anamorphic_flag(SURF) IS SET TO 1.0D0. IF NOT surf_anamorphic_flag(SURF)
!       IS SET BY DEFAULT TO 0.0D0. THIS IS A
!       LABEL MARKING THE SURFACE AS AN ANAMORPHIC ASPHERIC.
!       ALSO HANDELS ASPH AT CMD LEVEL.
!
   INTEGER I,J
!
   REAL*8 CC,AD,AE,AF,AG
!
!
   IF(F5.EQ.1.OR.F6.EQ.1) THEN
!
      IF(STI.EQ.1) THEN
         IF(surf_anamorphic_flag(I).NE.0.0D0) THEN
            WRITE(OUTLYNE,106)SURF
            CALL SHOWIT(0)
            WRITE(OUTLYNE,101)surf_anamorphic_coeff(I, 4)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,102)surf_anamorphic_coeff(I, 6)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,103)surf_anamorphic_coeff(I, 8)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,104)surf_anamorphic_coeff(I, 10)
            CALL SHOWIT(0)
106         FORMAT('"TASPH" VALUES AT SURFACE #',I3,' ARE:')
101         FORMAT('"ADTOR = "',G23.15)
102         FORMAT('"AETOR = "',G23.15)
103         FORMAT('"AFTOR = "',G23.15)
104         FORMAT('"AGTOR = "',G23.15)
         ELSE
!       NOT ANAMORPHIC ASPHERIC
            WRITE(OUTLYNE,305) SURF
            CALL SHOWIT(0)
            WRITE(OUTLYNE,306)
            CALL SHOWIT(0)
         END IF
305      FORMAT('SURFACE #',I3,' IS NOT ANAMORPHIC ASPHERIC')
306      FORMAT('NO ANAMORPHIC ASPHERIC DEFORMATION TERMS EXIST')
         RETURN
      ELSE
!       NOT STI
      END IF
!
      IF(SURF.EQ.0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'OBJECT SURFACE MAY NOT BE ASPHERIC-TORIC'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
      IF(SST.EQ.1.OR.SQ.EQ.1.OR.DF5.EQ.0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"TASPH" ONLY TAKES NUMERIC WORD #1 THROUGH #4 INPUT'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF

!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
!
!       CHECK THAT THE SURFACE IS A TORIC. IF NOT PRINT MESSAGE
!       AND RETURN, ELSE PROCEED.
!
      IF(surf_toric_flag(SURF).EQ.0.0D0) THEN
         WRITE(OUTLYNE,*)'SURFACE',SURF,' MUST BE DEFINED AS A Y-TORIC OR'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('AN X-TORIC BEFORE "TASPH" MAY BE USED.', 1)
         RETURN
      END IF
      IF(DF1.EQ.1) W1=0.0D0
      IF(DF2.EQ.1) W2=0.0D0
      IF(DF3.EQ.1) W3=0.0D0
      IF(DF4.EQ.1) W4=0.0D0
      IF(DF5.EQ.1) W5=0.0D0
!
      call set_surf_anamorphic_flag(SURF, 1)
      IF(DF1.EQ.0)call set_surf_anamorphic_coeff(SURF, 4, W1)
      IF(DF2.EQ.0)call set_surf_anamorphic_coeff(SURF, 6, W2)
      IF(DF3.EQ.0)call set_surf_anamorphic_coeff(SURF, 8, W3)
      IF(DF4.EQ.0)call set_surf_anamorphic_coeff(SURF, 10, W4)
!       DUMP PIKUP PRO AND NPRO IF FOUND
      IF(PIKUP(1,SURF,11).GT.0.0D0) THEN
         PIKUP(1:6,SURF,11)=0.0D0
         call set_surf_special_type(SURF, surf_special_type(SURF)-1)
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (PRO) DELETED'
         CALL SHOWIT(1)
      END IF
      IF(PIKUP(1,SURF,12).GT.0.0D0) THEN
         PIKUP(1:6,SURF,12)=0.0D0
         call set_surf_special_type(SURF, surf_special_type(SURF)-1)
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (NPRO) DELETED'
         CALL SHOWIT(1)
      END IF
!
!               NOTE:
!
!       ANAMORPHIC CONIC CONSTANT IS STORED IN surf_anamorphic_conic(SURF)
!
!
!
   ELSE
      IF(F1.EQ.1) THEN
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
         IF(SST.EQ.1) THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & 'AT THE CMD LEVEL, "TASPH" TAKES NO STRING INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(SQ.EQ.1.AND.S1.EQ.1) THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & 'AT THE CMD LEVEL, "TASPH" TAKES EITHER QUALIFIER OR'//'\n'//&
            & 'NUMERIC WORD #1 INPUT BUT NOT BOTH'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & 'AT THE CMD LEVEL,'//'\n'//&
            & '"TASPH" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
!
!       WHAT IF NO SURFACES EXIST
         IF(SYSTEM(20).EQ.0.0D0) THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & 'NO ASHERIC-TORICS EXIST'//'\n'//&
            & 'LENS SYSTEM HAS NO SURFACES'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
!       W1 DESIGNATES THE SURFACE FOR WHICH THE ANAMORPHIC CONIC
!       OR ASPHERIC
!       DATA IS TO BE OUTPUT.
!       IF THE QUALIFIER "ALL" IS USED, THEN THE ASPHERIC DATA FOR
!       THE ENTIRE LENS IS PRINTED.
!
!       IF surf_anamorphic_conic(SURF) NOT EQUAL TO 0.0 THEN THERE IS CONIC DATA
!       IF surf_anamorphic_flag(SURF) NOT EQUAL TO 0.0 THEN THERE IS ASPHERIC DATA
!
!       PRINT OUT FOR AN INDIVIDUAL SURFACE
!
         IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
!       THIS IS THE SAME AS TASPH,0
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
            IF(DF1.EQ.1) W1=DBLE(INT(SYSTEM(20)))
            I=INT(W1)
            IF(I.GT.(INT(SYSTEM(20))).OR.I.LT.0) THEN
               CALL REPORT_ERROR_AND_FAIL(&
               & 'SURFACE NUMBER BEYOND LEGAL RANGE'//'\n'//&
               & 'RE-ENTER COMMAND', 1)
               RETURN
            END IF
            IF(surf_anamorphic_flag(I).NE.0.0D0) THEN
               CC=surf_anamorphic_conic(I)
               AD=surf_anamorphic_coeff(I, 4)
               AE=surf_anamorphic_coeff(I, 6)
               AF=surf_anamorphic_coeff(I, 8)
               AG=surf_anamorphic_coeff(I, 10)
               IF(HEADIN) WRITE(OUTLYNE,500)
               IF(HEADIN) CALL SHOWIT(0)
               WRITE(OUTLYNE,100)I,CC,AD,AE,AF,AG
               CALL SHOWIT(0)
               RETURN
            ELSE
!       surf_anamorphic_flag(I)=0.0D0
            END IF
            IF(surf_anamorphic_conic(I).NE.0.0D0) THEN
               CC=surf_anamorphic_conic(I)
               IF(HEADIN) WRITE(OUTLYNE,500)
               IF(HEADIN) CALL SHOWIT(0)
               WRITE(OUTLYNE,200)I,CC
               CALL SHOWIT(0)
               RETURN
            ELSE
            END IF
!       NO CONIC OR ASPHERIC DATA FOR THAT SURFACE,RETURN
            WRITE(OUTLYNE,300) I
            CALL SHOWIT(0)
            RETURN
         ELSE
!       THERE WAS A QUALIFIER.
            IF(WQ.NE.'ALL') THEN
               OUTLYNE='INVALID QUALIFIER WORD'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               RETURN
            END IF
!
!       CHECK FOR NO DATA
!
            J=0
            DO 20 I=0,INT(SYSTEM(20))
               IF(surf_anamorphic_conic(I).NE.0.0D0.AND.surf_anamorphic_flag(I).NE.0.0D0.OR.surf_anamorphic_conic(I).NE.0.0D0.OR.surf_anamorphic_flag(I).NE.0.0D0) THEN
                  J=J+1
               ELSE
               END IF
20          CONTINUE
            IF(J.EQ.0) THEN
!       WRITE "NO DATA" AND RETURN
               WRITE(OUTLYNE,310)
               CALL SHOWIT(0)
               RETURN
            ELSE
            END IF
!
!       THERE WAS DATA, WRITE IT
!
!       PRINT HEADER MESSAGE
            WRITE(OUTLYNE,400)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,401)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,500)
            CALL SHOWIT(0)
!
            DO 10 I=0,INT(SYSTEM(20))
               IF(surf_anamorphic_conic(I).NE.0.0D0.AND.surf_anamorphic_flag(I).EQ.0.0D0) THEN
                  CC=surf_anamorphic_conic(I)
                  WRITE(OUTLYNE,200)I,CC
                  CALL SHOWIT(0)
               ELSE
                  IF(surf_anamorphic_flag(I).NE.0.0D0) THEN
                     CC=surf_anamorphic_conic(I)
                     AD=surf_anamorphic_coeff(I, 4)
                     AE=surf_anamorphic_coeff(I, 6)
                     AF=surf_anamorphic_coeff(I, 8)
                     AG=surf_anamorphic_coeff(I, 10)
                     WRITE(OUTLYNE,100)I,CC,AD,AE,AF,AG
                     CALL SHOWIT(0)
                  ELSE
                  END IF
               END IF
10          CONTINUE
            RETURN
         END IF
      ELSE
      END IF
   END IF
100 FORMAT(I3,1X,G13.6,1X,G13.6,1X,G13.6,1X,G13.6,1X,G13.6)
200 FORMAT(I3,1X,G13.6)
300 FORMAT('SURF',1X,I3,1X,' :NO CONIC OR ASPHERIC ANAMORPHIC DATA')
310 FORMAT('NO CONIC OR ASPHERIC ANAMORPHIC DATA')
400 FORMAT('ANAMORPHIC CONIC AND ASPHERIC DATA')
401 FORMAT(1X)
500 FORMAT('SURF',3X,'CCTOR',9X,'ADTOR',9X,'AETOR',9X,'AFTOR',9X,'AGTOR')
   RETURN
END
! SUB STILT.FOR
SUBROUTINE STILT
!
   use DATLEN
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE STILT WHICH IMPLEMENTS THE TILT COMMANDS
!       TILT,RTILT,TILT AUTO,TILT AUTOM,TILT BEN, TILT DAR AND TILT RET
!       AND TILT RETD,BEND AND DARD AND REVD
!       COMMAND AT THE LENS INPUT LEVEL OR THE LENS UPDATE LEVEL.
!
   REAL*8 RAL,RBE,RGAM,CGAM,SGAM
!
   INTEGER PIKCNT,I,J,K,NEXTSURF
!
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SST.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"TILT AND RTILT COMMANDS" TAKE NO STRING INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(SURF.EQ.0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'OBJECT SURFACE MAY NOT BE TILTED OR DECENTERED'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(GLANAM(SURF,2).EQ.'PERFECT'.OR.GLANAM(SURF,2).EQ.'IDEAL') THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"PERFECT" AND "IDEAL" SURFACES MAY NOT BE TILTED'//' OR DECENTERED'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(GLANAM(SURF-1,2).EQ.'PERFECT'.OR.GLANAM(SURF-1,2).EQ.'IDEAL')THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'THE LAST SURFACE MAY NOT BE TILTED OR DECENTERED'//'\n'//&
      & 'IF THE PREVIOUS SURFACE WAS "PERFECT" OR "IDEAL"'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!
!       WE ARE AT LENS INPUT LEVEL OR LENS UPDATE LEVEL
!
!               CHECK FOR VALID QUALIFIERS, PROCEED
!
   IF(WQ.NE.'AUTO'.AND.WQ.NE.'AUTOM'.AND.WQ.NE.'BEN'.AND.WQ.NE.'DAR'.AND.WQ.NE.'RET'.AND.WQ.NE.'RETD'.AND.WQ.NE.'BEND'.AND.WQ.NE.'DARD'.AND.WQ.NE.'REV'.AND.WQ.NE.'REVD'.AND.SQ.NE.0) THEN
      CALL REPORT_ERROR_AND_FAIL('INVALID QUALIFIER INPUT'//'\n'//'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(WQ.EQ.'BEN'.OR.WQ.EQ.'DAR'.OR.WQ.EQ.'RET'.OR.WQ.EQ.'REV') THEN
      IF(SURF.EQ.0.OR.SURF.EQ.1) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"TILT "'//WQ(1:3)//' IS DISALLOWED ON'//'\n'//&
         & 'SURFACES 0 AND 1'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
   END IF
!
!       FIRST SET THE TILT STATUS FLAG surf_tilt_flag(SURF)
!
!                       0.0=NO TILT (DEFAULT VALUE)
!                       1.0= TILT
!                      -1.0= RTILT
!                       2.0= TILT AUTO
!                       3.0= TILT AUTOM
!                       4.0= TILT BEN
!                       5.0= TILT DAR
!                       6.0= TILT RET
!
!       FIRST A STANDARD TILT COMMAND
!
   IF(WQ.EQ.'REV'.AND.SURF.EQ.NEWOBJ+1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"TILT REV" IS NOT ALLOWED ON THE SURFACE IMMEDIATELY'//'\n'//&
      & 'FOLLOWING THE OBJECT SURFACE'//'\n'//&
      & 'NO ACTION TAKEN', 1)
      RETURN
   END IF
   IF(WC.EQ.'TILT'.AND.SQ.EQ.0) THEN
      call set_surf_tilt_flag(SURF, 0)
      ALENS(90:95,SURF)=0.0D0
      call set_surf_tilt_return_flag(SURF, 0)
      IF(DF1.EQ.1)W1=surf_alpha(SURF)
      IF(DF2.EQ.1)W2=surf_beta(SURF)
      IF(DF3.EQ.1)W3=surf_gamma(SURF)
      call set_surf_tilt_flag(SURF, 1)
      call set_surf_alpha(SURF, W1)
      call set_surf_beta(SURF, W2)
      call set_surf_gamma(SURF, W3)
      call set_surf_alpha_deg(SURF, W1)
      call set_surf_beta_deg(SURF, W2)
      call set_surf_gamma_deg(SURF, W3)
   ELSE
   END IF
!
!       THEN AN RTILT
!
   IF(WC.EQ.'RTILT'.AND.SQ.EQ.0) THEN
      call set_surf_tilt_flag(SURF, 0)
      ALENS(90:95,SURF)=0.0D0
      call set_surf_tilt_return_flag(SURF, 0)
      IF(DF1.EQ.1)W1=surf_alpha(SURF)
      IF(DF2.EQ.1)W2=surf_beta(SURF)
      IF(DF3.EQ.1)W3=surf_gamma(SURF)
      call set_surf_tilt_flag(SURF, -1)
      call set_surf_alpha(SURF, W1)
      call set_surf_beta(SURF, W2)
      call set_surf_gamma(SURF, W3)
      call set_surf_alpha_deg(SURF, W1)
      call set_surf_beta_deg(SURF, W2)
      call set_surf_gamma_deg(SURF, W3)
   ELSE
   END IF
!       CHECK FOR RTILT WITH QUALIFIER
   IF(WC.EQ.'RTILT'.AND.SQ.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"RTILT" TAKES NO QUALIFIER WORD INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!
!       THEN A TILT AUTO
!
   IF(WC.EQ.'TILT'.AND.WQ.EQ.'AUTO') THEN
      IF(SURF.LE.INT(SYSTEM(25)))THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"TILT AUTO" NOT ALLOWED BEFORE OR ON THE REFERENCE SURFACE'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(surf_asi_flag(SURF).EQ.24.0D0)THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"TILT AUTO" NOT ALLOWED ON A LENS ARRAY SURFACE'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(DF1.EQ.1)W1=surf_alpha(SURF)
      IF(DF2.EQ.1)W2=surf_beta(SURF)
      IF(DF3.EQ.1)W3=surf_gamma(SURF)
      call set_surf_tilt_flag(SURF, 0)
      ALENS(90:95,SURF)=0.0D0
      call set_surf_tilt_return_flag(SURF, 0)
      call set_surf_tilt_flag(SURF, 2)
      call set_surf_alpha(SURF, W1)
      call set_surf_beta(SURF, W2)
      call set_surf_gamma(SURF, W3)
      call set_surf_alpha_deg(SURF, W1)
      call set_surf_beta_deg(SURF, W2)
      call set_surf_gamma_deg(SURF, W3)
   ELSE
   END IF
   IF(WC.EQ.'TILT'.AND.WQ.EQ.'BEN') THEN
      IF(SN.EQ.1) THEN
         IF(DF1.EQ.1)W1=surf_alpha(SURF)
         IF(DF2.EQ.1)W2=surf_beta(SURF)
         call set_surf_gamma(SURF, 0.0D0)
         call set_surf_gamma_deg(SURF, 0.0D0)
         call set_surf_tilt_flag(SURF, 0)
         ALENS(90:95,SURF)=0.0D0
         call set_surf_tilt_return_flag(SURF, 0)
         call set_surf_tilt_flag(SURF, 4)
         call set_surf_alpha(SURF, W1)
         call set_surf_beta(SURF, W2)
         call set_surf_alpha_deg(SURF, W1)
         call set_surf_beta_deg(SURF, W2)
!     USE THE CODE-V DEFAULT GAMMA ONLY IF IT IS NOT EXPLICITLY INPUT
         RAL=(PII/180.0D0)*W1
         RBE=(PII/180.0D0)*W2
         CGAM=(DCOS(RAL)+DCOS(RBE))/(1.0D0+(DCOS(RAL)*DCOS(RBE)))
         SGAM=-(DSIN(RAL)*DSIN(RBE))/(1.0D0+(DCOS(RAL)*DCOS(RBE)))
         IF(CGAM.LT.-1.0D0) CGAM=-1.0D0
         IF(CGAM.GT.+1.0D0) CGAM=+1.0D0
         IF(SGAM.GE.0.0D0) RGAM=DABS(DACOS(CGAM))
         IF(SGAM.LT.0.0D0) RGAM=-DABS(DACOS(CGAM))
         call set_surf_gamma(SURF, RGAM*180.0D0/PII)
         call set_surf_gamma_deg(SURF, RGAM*180.0D0/PII)
      ELSE
!       NO NUMERICS, JUST CHANGE TYPE
         call set_surf_tilt_flag(SURF, 4)
      END IF
   ELSE
   END IF
   IF(WC.EQ.'TILT'.AND.WQ.EQ.'DAR') THEN
      IF(SN.EQ.1) THEN
         IF(DF1.EQ.1)W1=surf_alpha(SURF)
         IF(DF2.EQ.1)W2=surf_beta(SURF)
         IF(DF3.EQ.1)W3=surf_gamma(SURF)
         call set_surf_tilt_flag(SURF, 0)
         ALENS(90:95,SURF)=0.0D0
         call set_surf_tilt_return_flag(SURF, 0)
         call set_surf_tilt_flag(SURF, 5)
         call set_surf_alpha(SURF, W1)
         call set_surf_beta(SURF, W2)
         call set_surf_gamma(SURF, W3)
         call set_surf_alpha_deg(SURF, W1)
         call set_surf_beta_deg(SURF, W2)
         call set_surf_gamma_deg(SURF, W3)
      ELSE
!       NO NUMERICS, JUST CHANGE TO DAR
         call set_surf_tilt_flag(SURF, 5)
      END IF
   ELSE
   END IF
   IF(WC.EQ.'TILT'.AND.WQ.EQ.'REV') THEN
      IF(SN.EQ.1) THEN
         IF(DF1.EQ.1)W1=surf_alpha(SURF)
         IF(DF2.EQ.1)W2=surf_beta(SURF)
         IF(DF3.EQ.1)W3=surf_gamma(SURF)
         call set_surf_tilt_flag(SURF, 0)
         ALENS(90:95,SURF)=0.0D0
         call set_surf_tilt_return_flag(SURF, 0)
         call set_surf_tilt_flag(SURF, 7)
         call set_surf_alpha(SURF, W1)
         call set_surf_beta(SURF, W2)
         call set_surf_gamma(SURF, W3)
         call set_surf_alpha_deg(SURF, W1)
         call set_surf_beta_deg(SURF, W2)
         call set_surf_gamma_deg(SURF, W3)
      ELSE
!       NO NUMERICS, JUST CHANGE TYPE
         call set_surf_tilt_flag(SURF, 7)
      END IF
   END IF
   IF(WC.EQ.'TILT'.AND.WQ.EQ.'RETD') THEN
      IF(SN.EQ.1.OR.SST.EQ.1) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"TILT RETD" TAKES NO STRING OR NUMERIC INPUT'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
   END IF
   IF(WC.EQ.'TILT'.AND.WQ.EQ.'DARD') THEN
      IF(SN.EQ.1.OR.SST.EQ.1) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"TILT DARD" TAKES NO STRING OR NUMERIC INPUT'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
   END IF
   IF(WC.EQ.'TILT'.AND.WQ.EQ.'REVD') THEN
      IF(SN.EQ.1.OR.SST.EQ.1) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"TILT REVD" TAKES NO STRING OR NUMERIC INPUT'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
   END IF
   IF(WC.EQ.'TILT'.AND.WQ.EQ.'BEND') THEN
      IF(SN.EQ.1.OR.SST.EQ.1) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"TILT BEND" TAKES NO STRING OR NUMERIC INPUT'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
   END IF
   IF(WC.EQ.'TILT'.AND.WQ.EQ.'RET') THEN
      IF(DF1.EQ.1) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"TILT RET" REQUIRES AN EXPLICIT SURFACE #'//'\n'//&
         & 'INPUT FOR NUMERIC WORD #1'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!     NOW CHECK IF IT REFERS TO A PREVIOUS SURFACE
      IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM(20)) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'INVALID SURFACE NUMBER REFERED TO BY "TILT RET"'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(INT(W1).GE.SURF) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"TILT RET" MUST REFER TO A PREVIOUS SURFACE'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!
      call set_surf_ret_surf_num(SURF, NINT(W1))
      ALENS(25:28,SURF)=0.0D0
      ALENS(118:120,SURF)=0.0D0
      ALENS(90:95,SURF)=0.0D0
      call set_surf_global_gamma(SURF, 0.0D0)
      call set_surf_tilt_return_flag(SURF, 0)
      ALENS(114:116,SURF)=0.0D0
      ALENS(29:31,SURF)=0.0D0
      call set_surf_decenter_z(SURF, 0.0D0)
      ALENS(90:95,SURF)=0.0D0
      ALENS(113:116,SURF)=0.0D0
      ALENS(118:120,SURF)=0.0D0
      call set_surf_tilt_flag(SURF, 6)
   ELSE
   END IF
   IF(WC.EQ.'TILT'.AND.WQ.EQ.'RETD') THEN
      IF(surf_tilt_flag(SURF).EQ.6.0D0.OR.surf_tilt_flag(SURF).EQ.1.0D0.AND.surf_tilt_return_flag(SURF).EQ.1.0D0) THEN
         call set_surf_tilt_flag(SURF, 1)
         call set_surf_tilt_return_flag(SURF, 0)
      END IF
   END IF
   IF(WC.EQ.'TILT'.AND.WQ.EQ.'BEND'.AND.surf_tilt_flag(SURF).EQ.4.0D0) THEN
      NEXTSURF=SURF+1
!     CHANGE CURRENT SURFACE
      SAVE_KDP(1)=SAVEINPT(1)
      WC='CHG'
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
      W1=DBLE(NEXTSURF)-1.0D0
      SST=0
      SN=1
      CALL SCHG
      REST_KDP(1)=RESTINPT(1)
!     INSERT FOLLOWING SURFACE
      SAVE_KDP(1)=SAVEINPT(1)
      WC='INSK'
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
      W1=DBLE(NEXTSURF)
      SST=0
      SN=1
      CALL SINS
      REST_KDP(1)=RESTINPT(1)
!     CHANGE TILT TYPE ON CURRENT SURFACE TO "TILT"
      call set_surf_tilt_flag(NEXTSURF-1, 1)
      call set_surf_tilt_flag(NEXTSURF, 1)
      call set_surf_alpha(NEXTSURF, surf_alpha(NEXTSURF-1))
      call set_surf_beta(NEXTSURF, surf_beta(NEXTSURF-1))
      call set_surf_gamma(NEXTSURF, surf_gamma(NEXTSURF-1))
      call set_surf_alpha_deg(NEXTSURF, surf_alpha_deg(NEXTSURF-1))
      call set_surf_beta_deg(NEXTSURF, surf_beta_deg(NEXTSURF-1))
      call set_surf_gamma_deg(NEXTSURF, surf_gamma_deg(NEXTSURF-1))
   END IF
!
!       TILT REVD
!
   IF(WC.EQ.'TILT'.AND.WQ.EQ.'REVD'.AND.surf_tilt_flag(SURF).EQ.7.0D0) THEN
      NEXTSURF=SURF+1
!     CHANGE CURRENT SURFACE
      SAVE_KDP(1)=SAVEINPT(1)
      WC='CHG'
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
      W1=DBLE(NEXTSURF)-1.0D0
      SST=0
      SN=1
      CALL SCHG
      REST_KDP(1)=RESTINPT(1)
!     INSERT FOLLOWING SURFACE
      SAVE_KDP(1)=SAVEINPT(1)
      WC='INSK'
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
      W1=DBLE(NEXTSURF)
      SST=0
      SN=1
      CALL SINS
      REST_KDP(1)=RESTINPT(1)
!     CHANGE TILT TYPE ON CURRENT SURFACE TO "TILT"
      call set_surf_tilt_flag(NEXTSURF-1, 0)
      call set_surf_tilt_flag(NEXTSURF, -1)
      call set_surf_alpha(NEXTSURF, surf_alpha(NEXTSURF-1))
      call set_surf_beta(NEXTSURF, surf_beta(NEXTSURF-1))
      call set_surf_gamma(NEXTSURF, surf_gamma(NEXTSURF-1))
      call set_surf_alpha_deg(NEXTSURF, surf_alpha_deg(NEXTSURF-1))
      call set_surf_beta_deg(NEXTSURF, surf_beta_deg(NEXTSURF-1))
      call set_surf_gamma_deg(NEXTSURF, surf_gamma_deg(NEXTSURF-1))
      call set_surf_alpha(NEXTSURF-1, 0.0D0)
      call set_surf_beta(NEXTSURF-1, 0.0D0)
      call set_surf_gamma(NEXTSURF-1, 0.0D0)
      call set_surf_alpha_deg(NEXTSURF-1, 0.0D0)
      call set_surf_beta_deg(NEXTSURF-1, 0.0D0)
      call set_surf_gamma_deg(NEXTSURF-1, 0.0D0)
      IF(surf_decenter_flag(NEXTSURF-1).NE.0.0D0) THEN
         call set_surf_decenter_y(NEXTSURF, surf_decenter_y(NEXTSURF-1))
         call set_surf_decenter_x(NEXTSURF, surf_decenter_x(NEXTSURF-1))
         call set_surf_decenter_z(NEXTSURF, surf_decenter_z(NEXTSURF-1))
         call set_surf_pivot_axis(NEXTSURF, surf_pivot_axis(NEXTSURF-1))
         call set_surf_focus_dx(NEXTSURF, surf_focus_dx(NEXTSURF-1))
         call set_surf_focus_dy(NEXTSURF, surf_focus_dy(NEXTSURF-1))
         call set_surf_focus_dz(NEXTSURF, surf_focus_dz(NEXTSURF-1))
         call set_surf_decenter_y(NEXTSURF-1, 0.0D0)
         call set_surf_decenter_x(NEXTSURF-1, 0.0D0)
         call set_surf_decenter_z(NEXTSURF-1, 0.0D0)
         call set_surf_pivot_axis(NEXTSURF-1, 0)
         call set_surf_focus_dx(NEXTSURF-1, 0.0D0)
         call set_surf_focus_dy(NEXTSURF-1, 0.0D0)
         call set_surf_focus_dz(NEXTSURF-1, 0.0D0)
      END IF
!     THICKNESSES
      IF(surf_solve_flag(NEXTSURF-1).EQ.1.0D0.OR.surf_solve_flag(NEXTSURF-1).EQ.1.1D0.OR.surf_solve_flag(NEXTSURF-1).EQ.0.1D0.OR.surf_solve_flag(NEXTSURF-1).EQ.0.3D0.OR.surf_solve_flag(NEXTSURF-1).EQ.3.0D0.OR.surf_solve_flag(NEXTSURF-1).EQ.3.3D0) THEN
!     THICKNESS SOLVE NEEDS MOVING
         call set_surf_solve_flag(NEXTSURF, surf_solve_flag(NEXTSURF-1))
         SOLVE(6,NEXTSURF)=SOLVE(6,NEXTSURF-1)
         SOLVE(4,NEXTSURF)=SOLVE(4,NEXTSURF-1)
         SOLVE(7,NEXTSURF)=SOLVE(7,NEXTSURF-1)
         SOLVE(3,NEXTSURF)=SOLVE(3,NEXTSURF-1)
         IF(surf_solve_flag(NEXTSURF-1).EQ.0.1D0)call set_surf_solve_flag(NEXTSURF-1, 0.0D0)
         IF(surf_solve_flag(NEXTSURF-1).EQ.1.0D0)call set_surf_solve_flag(NEXTSURF-1, 0.0D0)
         IF(surf_solve_flag(NEXTSURF-1).EQ.1.1D0)call set_surf_solve_flag(NEXTSURF-1, 0.0D0)
         IF(surf_solve_flag(NEXTSURF-1).EQ.0.3D0)call set_surf_solve_flag(NEXTSURF-1, 0.2D0)
         IF(surf_solve_flag(NEXTSURF-1).EQ.3.0D0)call set_surf_solve_flag(NEXTSURF-1, 2.0D0)
         IF(surf_solve_flag(NEXTSURF-1).EQ.3.3D0)call set_surf_solve_flag(NEXTSURF-1, 2.2D0)
      END IF
      call set_surf_thickness(NEXTSURF, surf_thickness(NEXTSURF-1))
      call set_surf_thickness(NEXTSURF-1, 0.0D0)
      RETURN
   END IF
   IF(WC.EQ.'TILT'.AND.WQ.EQ.'DARD'.AND.surf_tilt_flag(SURF).EQ.5.0D0) THEN
      NEXTSURF=SURF+1
!     CHANGE CURRENT SURFACE
      SAVE_KDP(1)=SAVEINPT(1)
      WC='CHG'
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
      W1=DBLE(NEXTSURF)-1.0D0
      SST=0
      SN=1
      CALL SCHG
      REST_KDP(1)=RESTINPT(1)
!     INSERT FOLLOWING SURFACE
      SAVE_KDP(1)=SAVEINPT(1)
      WC='INSK'
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
      W1=DBLE(NEXTSURF)
      SST=0
      SN=1
      CALL SINS
      REST_KDP(1)=RESTINPT(1)
!     CHANGE TILT TYPE ON CURRENT SURFACE TO "TILT"
      call set_surf_tilt_flag(NEXTSURF-1, 1)
      call set_surf_tilt_flag(NEXTSURF, -1)
      call set_surf_alpha(NEXTSURF, surf_alpha(NEXTSURF-1))
      call set_surf_beta(NEXTSURF, surf_beta(NEXTSURF-1))
      call set_surf_gamma(NEXTSURF, surf_gamma(NEXTSURF-1))
      call set_surf_alpha_deg(NEXTSURF, surf_alpha_deg(NEXTSURF-1))
      call set_surf_beta_deg(NEXTSURF, surf_beta_deg(NEXTSURF-1))
      call set_surf_gamma_deg(NEXTSURF, surf_gamma_deg(NEXTSURF-1))
      IF(surf_decenter_flag(NEXTSURF-1).NE.0.0D0) THEN
         call set_surf_decenter_y(NEXTSURF, surf_decenter_y(NEXTSURF-1))
         call set_surf_decenter_x(NEXTSURF, surf_decenter_x(NEXTSURF-1))
         call set_surf_decenter_z(NEXTSURF, surf_decenter_z(NEXTSURF-1))
         call set_surf_pivot_axis(NEXTSURF, surf_pivot_axis(NEXTSURF-1))
         call set_surf_focus_dx(NEXTSURF, surf_focus_dx(NEXTSURF-1))
         call set_surf_focus_dy(NEXTSURF, surf_focus_dy(NEXTSURF-1))
         call set_surf_focus_dz(NEXTSURF, surf_focus_dz(NEXTSURF-1))
      END IF
!     THICKNESSES
      IF(surf_solve_flag(NEXTSURF-1).EQ.1.0D0.OR.surf_solve_flag(NEXTSURF-1).EQ.1.1D0.OR.surf_solve_flag(NEXTSURF-1).EQ.0.1D0.OR.surf_solve_flag(NEXTSURF-1).EQ.0.3D0.OR.surf_solve_flag(NEXTSURF-1).EQ.3.0D0.OR.surf_solve_flag(NEXTSURF-1).EQ.3.3D0) THEN
!     THICKNESS SOLVE NEEDS MOVING
         call set_surf_solve_flag(NEXTSURF, surf_solve_flag(NEXTSURF-1))
         SOLVE(6,NEXTSURF)=SOLVE(6,NEXTSURF-1)
         SOLVE(4,NEXTSURF)=SOLVE(4,NEXTSURF-1)
         SOLVE(7,NEXTSURF)=SOLVE(7,NEXTSURF-1)
         SOLVE(3,NEXTSURF)=SOLVE(3,NEXTSURF-1)
         IF(surf_solve_flag(NEXTSURF-1).EQ.0.1D0)call set_surf_solve_flag(NEXTSURF-1, 0.0D0)
         IF(surf_solve_flag(NEXTSURF-1).EQ.1.0D0)call set_surf_solve_flag(NEXTSURF-1, 0.0D0)
         IF(surf_solve_flag(NEXTSURF-1).EQ.1.1D0)call set_surf_solve_flag(NEXTSURF-1, 0.0D0)
         IF(surf_solve_flag(NEXTSURF-1).EQ.0.3D0)call set_surf_solve_flag(NEXTSURF-1, 0.2D0)
         IF(surf_solve_flag(NEXTSURF-1).EQ.3.0D0)call set_surf_solve_flag(NEXTSURF-1, 2.0D0)
         IF(surf_solve_flag(NEXTSURF-1).EQ.3.3D0)call set_surf_solve_flag(NEXTSURF-1, 2.2D0)
      END IF
      call set_surf_thickness(NEXTSURF, surf_thickness(NEXTSURF-1))
      call set_surf_thickness(NEXTSURF-1, 0.0D0)
      RETURN
   END IF
!
!       THEN A TILT AUTOM
!
   IF(WC.EQ.'TILT'.AND.WQ.EQ.'AUTOM') THEN
      IF(SURF.LE.INT(SYSTEM(25)))THEN
         OUTLYNE='"TILT AUTOM" NOT ALLOWED BEFORE OR ON THE REFERENCE SURFACE'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETURN
      END IF
      IF(surf_asi_flag(SURF).EQ.24.0D0)THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"TILT AUTOM" NOT ALLOWED ON A LENS ARRAY SURFACE'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(DF1.EQ.1)W1=surf_alpha(SURF)
      IF(DF2.EQ.1)W2=surf_beta(SURF)
      IF(DF3.EQ.1)W3=surf_gamma(SURF)
      call set_surf_tilt_flag(SURF, 3)
      call set_surf_alpha(SURF, W1)
      call set_surf_beta(SURF, W2)
      call set_surf_gamma(SURF, W3)
      call set_surf_alpha_deg(SURF, W1)
      call set_surf_beta_deg(SURF, W2)
      call set_surf_gamma_deg(SURF, W3)
   ELSE
   END IF
!
!       RESOLVE PIKUPS ALPHA,BETA AND GAMMA IF THE EXIST
!       USING THE TILT COMMAND HAS THE SAME EFFECT AS IF
!       A TILTD COMMAND WERE USED FIRST. HANDLE PIKUPS
!       THE SAME WAY THEY ARE HANDLED IN (STILTD)
!       WHAT IF THE SURFACE HAD ALPHA,BETA OR GAMMA PIKUPS ON IT?
!
   DO I=15,17
      IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
         PIKUP(1:6,SURF,I)=0.0D0
!       FIX THE PIKUP COUNTER
         call set_surf_special_type(SURF, surf_special_type(SURF)-1)
         IF(I.EQ.15) THEN
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (ALPHA) DELETED'
            CALL SHOWIT(1)
         END IF
         IF(I.EQ.16) THEN
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (BETA) DELETED'
            CALL SHOWIT(1)
         END IF
         IF(I.EQ.17) THEN
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GAMMA) DELETED'
            CALL SHOWIT(1)
         END IF
      END IF
   END DO
   DO I=37,42
      IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
         PIKUP(1:6,SURF,I)=0.0D0
!       FIX THE PIKUP COUNTER
         call set_surf_special_type(SURF, surf_special_type(SURF)-1)
         IF(I.EQ.37) THEN
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GDX) DELETED'
            CALL SHOWIT(1)
         END IF
         IF(I.EQ.38) THEN
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GDY) DELETED'
            CALL SHOWIT(1)
         END IF
         IF(I.EQ.39) THEN
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GDZ) DELETED'
            CALL SHOWIT(1)
         END IF
         IF(I.EQ.40) THEN
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GALPHA) DELETED'
            CALL SHOWIT(1)
         END IF
         IF(I.EQ.41) THEN
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GBETA) DELETED'
            CALL SHOWIT(1)
         END IF
         IF(I.EQ.42) THEN
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GGAMMA) DELETED'
            CALL SHOWIT(1)
         END IF
      END IF
   END DO
!
   PIKCNT=0
   DO 503 J=1,PSIZ
      IF(PIKUP(1,SURF,J).NE.0.0D0) THEN
         PIKCNT=PIKCNT+1
      ELSE
      END IF
503 CONTINUE
   IF(PIKCNT.EQ.0)call set_surf_special_type(SURF, 0)

!
!       WHAT IF THIS SURFACE WAS THE TARGET OF AN ALPHA,BETA OR GAMMA PIKUP
!               PIKUP(I,J,K) WHERE K IS 15,16, OR 17
!       IF SO THEN THE PIKUP MUST BE DELETED AND THE TILT
!       DATA FROZEN ON THE PIKUP SURFACE AT THEIR CURRENT VALUES.
   DO I=0,INT(SYSTEM(20))
      DO J=15,17
         IF(PIKUP(1,I,J).EQ.1.0D0) THEN
!       DOES IT REFER TO SURFACE SURF
            IF(INT(PIKUP(2,I,J)).EQ.SURF) THEN
!       YES IT REFERS TO THE SURFACE SURF WHICH IS HAVING ITS TILT
!       DELETED SO GET RIDE OF THE PIKUP
               PIKUP(1:6,I,J)=0.0D0
               call set_surf_special_type(I, surf_special_type(I)-1)
               IF(J.EQ.15) THEN
                  WRITE(OUTLYNE,*)'(ALPHA) PIKUP DELETED ON SURFACE',I
                  CALL SHOWIT(1)
               END IF
               IF(J.EQ.16) THEN
                  WRITE(OUTLYNE,*)'(BETA) PIKUP DELETED ON SURFACE',I
                  CALL SHOWIT(1)
               END IF
               IF(J.EQ.17) THEN
                  WRITE(OUTLYNE,*)'(GAMMA) PIKUP DELETED ON SURFACE',I
                  CALL SHOWIT(1)
               END IF
            END IF
         END IF
      END DO
      DO J=37,42
         IF(PIKUP(1,I,J).EQ.1.0D0) THEN
!       DOES IT REFER TO SURFACE SURF
            IF(INT(PIKUP(2,I,J)).EQ.SURF) THEN
!       YES IT REFERS TO THE SURFACE SURF WHICH IS HAVING ITS TILT
!       DELETED SO GET RIDE OF THE PIKUP
               PIKUP(1:6,I,J)=0.0D0
               call set_surf_special_type(I, surf_special_type(I)-1)
               IF(J.EQ.37) THEN
                  WRITE(OUTLYNE,*)'(GDX) PIKUP DELETED ON SURFACE',I
                  CALL SHOWIT(1)
               END IF
               IF(J.EQ.38) THEN
                  WRITE(OUTLYNE,*)'(GDY) PIKUP DELETED ON SURFACE',I
                  CALL SHOWIT(1)
               END IF
               IF(J.EQ.39) THEN
                  WRITE(OUTLYNE,*)'(GDZ) PIKUP DELETED ON SURFACE',I
                  CALL SHOWIT(1)
               END IF
               IF(J.EQ.40) THEN
                  WRITE(OUTLYNE,*)'(GALPHA) PIKUP DELETED ON SURFACE',I
                  CALL SHOWIT(1)
               END IF
               IF(J.EQ.41) THEN
                  WRITE(OUTLYNE,*)'(GBETA) PIKUP DELETED ON SURFACE',I
                  CALL SHOWIT(1)
               END IF
               IF(J.EQ.42) THEN
                  WRITE(OUTLYNE,*)'(GGAMMA) PIKUP DELETED ON SURFACE',I
                  CALL SHOWIT(1)
               END IF
            END IF
         END IF
      END DO
   END DO
!
!       NOW FIX ALL THE surf_special_type(K) IN THE LENS SYSTEM
!
   DO 400 I=0,INT(SYSTEM(20))
!       CHECK PIKUPS
      PIKCNT=0
      DO 401 J=1,PSIZ
         IF(PIKUP(1,I,J).EQ.1.0D0) THEN
            PIKCNT=PIKCNT+1
         ELSE
         END IF
401   CONTINUE
      IF(PIKCNT.EQ.0)call set_surf_special_type(I, 0)
400 CONTINUE
   RETURN
END
