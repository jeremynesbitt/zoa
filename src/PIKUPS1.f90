!       FIRST FILE OF PIKUP ROUTINES

! SUB PIKXYD.FOR
SUBROUTINE PIKXYD
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_last_surf
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PIKXYD. THIS IS THE SUBROUTINE WHICH
!       HANDLES THE X AND Y-DECENTRATION (XD) OR (YD) OR (ZD)
!       PIKUP AT THE LENS INPUT
!       AND UPDATE LENS LEVEL.
!
   INTEGER CT
!
!
!       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
!       SPECIFIC STORAGE LOCATIONS FOR LENS DATA,SOLVE AND
!       PIKUP ITEMS.
!
!       TEST TO SEE IF THE SURFACE FROM WHICH WE ARE PIKING UP
!       DATA IS THE SAME AS THE SURFACE TO WHICH THE PIKUP
!       WILL BE ASSINGED. OPERATION NOT LEGAL
!
   IF(SURF.EQ.INT(W1)) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'A SURFACE MAY NOT PIKUP ITS OWN VALUES'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(SURF.EQ.0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'OBJECT SURFACE MAY NOT PIKUP A DECENTER'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!
   IF(WQ.EQ.'YD') CT=13
   IF(WQ.EQ.'XD') CT=14
   IF(WQ.EQ.'ZD') CT=33
   IF(WQ.EQ.'GDX') CT=37
   IF(WQ.EQ.'GDY') CT=38
   IF(WQ.EQ.'GDZ') CT=39
!
!       THE PIKUP EXISTENCE FLAG IS PIKUP(1,I,J) FOR EVERY
!       PIKUP ON EVERY SURFACE. IF IT IS 1.0 THE PIKUP EXISTS
!       IF IT IS 0.0 THE PIKUP DOES NOT EXIST
!
!       STORE NUMERIC INPUT AND HANDEL DEFAULT DATA

!       IF DF1=1 THIS MEANS PIKING UP FROM THE OBJECT SURFACE
!       ON TO THE CURRENT SURFACE
   IF(DF1.EQ.1) W1=0.0
!       IF DF2=1 THIS MEANS MULTIPLIER IS UNITY
   IF(DF2.EQ.1) W2=1.0
!       IF DF3=1 TIS MEANS ADDITIVE CONSTANT ZERO
   IF(DF3.EQ.1) W3=0.0
!       IF DF4=1 THEN W4=0.0
!       IF DF5=1 THEN W5=0.0
   IF(DF4.EQ.1) W4=0.0
   IF(DF5.EQ.1) W5=0.0
   IF(W1.LT.0.0D0) W1=SURF+W1
   IF(W1.LT.0.0D0.OR.W1.GT.sys_last_surf()) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'SOURCE SURFACE NUMBER BEYOND LEGAL RANGE'//'\n'//&
      & 'PIKUP NOT ALLOWED', 1)
      RETURN
   END IF
!
!
!               INCR. PIKUP INDICATOR TO 1.0
!               I.E. surf_special_type(SURF)
   call set_surf_special_type(SURF, surf_special_type(SURF)+1)
   PIKUP(1,SURF,CT)=1.0
   PIKUP(2,SURF,CT)=W1
   PIKUP(3,SURF,CT)=W2
   PIKUP(4,SURF,CT)=W3
   PIKUP(5,SURF,CT)=W4
   PIKUP(6,SURF,CT)=W5
!
!
!       NOW SET THE DECENTER FLAG CORRECTLY
!       IF surf_decenter_flag(I) IS 1.0 FOR I = INT(W1)
!       THEN surf_decenter_flag(SURF) = 1.0 ELSE SET TO 0.0
   IF(WQ(1:1).NE.'G') THEN
      IF(surf_decenter_flag(INT(W1)).EQ.1.0) THEN
         call set_surf_decenter_flag(SURF, 1)
      ELSE
         call set_surf_decenter_flag(SURF, 0)
      END IF
   END IF
!
!       IF THE SURFACE HAS A TILT AUTO THEN REMOVE THE
!       AUTO AND PRINT MESSAGE
!
   IF(surf_tilt_flag(SURF).EQ.2.0D0.OR.surf_tilt_flag(SURF).EQ.3.0D0) THEN
      call set_surf_tilt_flag(SURF, 1)
      IF(surf_tilt_flag(SURF).EQ.2.0D0) WRITE(OUTLYNE,*)'"AUTO" ADJUST REMOVED FROM SURFACE TILT DEFINITION'
      IF(surf_tilt_flag(SURF).EQ.3.0D0) WRITE(OUTLYNE,*)'"AUTOM" ADJUST REMOVED FROM SURFACE TILT DEFINITION'
      CALL SHOWIT(1)
   END IF
!
!       THERE ARE NO SOLVE DELETIONS TO HANDEL.
!
!
   RETURN
END
! SUB PIKPXYD.FOR
SUBROUTINE PIKPXYD
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_last_surf
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PIKPXYD. THIS IS THE SUBROUTINE WHICH
!       HANDLES THE (PIVX) OR (PIVY) OR (PIVZ)
!       PIKUP AT THE LENS INPUT
!       AND UPDATE LENS LEVEL.
!
   INTEGER CT
!
!
!       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
!       SPECIFIC STORAGE LOCATIONS FOR LENS DATA,SOLVE AND
!       PIKUP ITEMS.
!
!       TEST TO SEE IF THE SURFACE FROM WHICH WE ARE PIKING UP
!       DATA IS THE SAME AS THE SURFACE TO WHICH THE PIKUP
!       WILL BE ASSINGED. OPERATION NOT LEGAL
!
   IF(SURF.EQ.INT(W1)) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'A SURFACE MAY NOT PIKUP ITS OWN VALUES'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(SURF.EQ.0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'OBJECT SURFACE MAY NOT PIKUP A DECENTER'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!
   IF(WQ.EQ.'PIVX') CT=34
   IF(WQ.EQ.'PIVY') CT=35
   IF(WQ.EQ.'PIVZ') CT=36
!
!       THE PIKUP EXISTENCE FLAG IS PIKUP(1,I,J) FOR EVERY
!       PIKUP ON EVERY SURFACE. IF IT IS 1.0 THE PIKUP EXISTS
!       IF IT IS 0.0 THE PIKUP DOES NOT EXIST
!
!       STORE NUMERIC INPUT AND HANDEL DEFAULT DATA

!       IF DF1=1 THIS MEANS PIKING UP FROM THE OBJECT SURFACE
!       ON TO THE CURRENT SURFACE
   IF(DF1.EQ.1) W1=0.0
!       IF DF2=1 THIS MEANS MULTIPLIER IS UNITY
   IF(DF2.EQ.1) W2=1.0
!       IF DF3=1 TIS MEANS ADDITIVE CONSTANT ZERO
   IF(DF3.EQ.1) W3=0.0
!       IF DF4=1 THEN W4=0.0
!       IF DF5=1 THEN W5=0.0
   IF(DF4.EQ.1) W4=0.0
   IF(DF5.EQ.1) W5=0.0
   IF(W1.LT.0.0D0) W1=SURF+W1
   IF(W1.LT.0.0D0.OR.W1.GT.sys_last_surf()) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'SOURCE SURFACE NUMBER BEYOND LEGAL RANGE'//'\n'//&
      & 'PIKUP NOT ALLOWED', 1)
      RETURN
   END IF
!
!
!               INCR. PIKUP INDICATOR TO 1.0
!               I.E. surf_special_type(SURF)
   call set_surf_special_type(SURF, surf_special_type(SURF)+1)
   PIKUP(1,SURF,CT)=1.0
   PIKUP(2,SURF,CT)=W1
   PIKUP(3,SURF,CT)=W2
   PIKUP(4,SURF,CT)=W3
   PIKUP(5,SURF,CT)=W4
   PIKUP(6,SURF,CT)=W5
!
!
!       NOW SET THE PIVOT DECENTER FLAG CORRECTLY
!       IF surf_pivot_flag(I) IS 1.0 FOR I = INT(W1)
!       THEN surf_pivot_flag(SURF) = 1.0 ELSE SET TO 0.0
!
   IF(surf_pivot_flag(INT(W1)).EQ.1.0) THEN
      call set_surf_pivot_flag(SURF, 1)
   ELSE
      call set_surf_pivot_flag(SURF, 0)
   END IF
!
!       IF THE SURFACE HAS A TILT AUTO THEN REMOVE THE
!       AUTO AND PRINT MESSAGE
!
   IF(surf_tilt_flag(SURF).EQ.2.0D0.OR.surf_tilt_flag(SURF).EQ.3.0D0) THEN
      call set_surf_tilt_flag(SURF, 1)
      IF(surf_tilt_flag(SURF).EQ.2.0D0) WRITE(OUTLYNE,*)'"AUTO" ADJUST REMOVED FROM SURFACE TILT DEFINITION'
      IF(surf_tilt_flag(SURF).EQ.3.0D0) WRITE(OUTLYNE,*)'"AUTOM" ADJUST REMOVED FROM SURFACE TILT DEFINITION'
      CALL SHOWIT(1)
   END IF
!
!       THERE ARE NO SOLVE DELETIONS TO HANDEL.
!
!
   RETURN
END
! SUB PIKTH.FOR
SUBROUTINE PIKTH
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_last_surf, sys_astop, sys_astop_adj
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PIKTH. THIS IS THE SUBROUTINE WHICH
!       HANDLES THICKNESS PIKUPS AT THE LENS INPUT
!       AND UPDATE LENS LEVEL.
!
   INTEGER IM1,IM2
!
!
!       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
!       SPECIFIC STORAGE LOCATIONS FOR LENS DATA,SOLVE AND
!       PIKUP ITEMS.
!
!       TEST TO SEE IF THE SURFACE FROM WHICH WE ARE PIKING UP
!       DATA IS THE SAME AS THE SURFACE TO WHICH THE PIKUP
!       WILL BE ASSINGED. OPERATION NOT LEGAL
!
!       CHECK FOR ASTOP EN OR EX OR ENEX INCONSISTENCIES.
!       IF FOUND, DISALLOW THE PIKUP
!
!               IS THERE AN ASTOP?
   IF(sys_astop().NE.-99.0D0) THEN
!               THERE IS AN ASTOP TO CHECK
      IF(sys_astop_adj().EQ.1.0D0.OR.sys_astop_adj().EQ.2.0D0) THEN
!       THERE IS AN EN ADJUSTMENT IN EFFECT
         IF(SURF.EQ.0.0D0.OR.SURF.EQ.1.0D0) THEN
!       YOU ARE TRYING TO PUT A PIKUP WERE YOU CAN'T
            CALL REPORT_ERROR_AND_FAIL(&
            & 'PIKUP WILL CONFLICT WITH ASTOP (EN)'//'\n'//&
            & 'PIKUP TH ASSIGNMENT DISALLOWED', 1)
            RETURN
         END IF
      END IF
      IF(sys_astop_adj().EQ.-1.0D0.OR.sys_astop_adj().EQ.2.0D0) THEN
!       THERE IS AN EX ADJUSTMENT IN EFFECT
         IM2=(INT(sys_last_surf())-2.0D0)
         IM1=(INT(sys_last_surf())-1.0D0)
         IF(SURF.EQ.IM1.OR.SURF.EQ.IM2) THEN
!       YOU ARE TRYING TO PUT A PIKUP WERE YOU CAN'T
            OUTLYNE='PIKUP WILL CONFLICT WITH ASTOP (EX)'
            CALL SHOWIT(1)
            OUTLYNE='PIKUP TH ASSIGNMENT DISALLOWED'
            CALL SHOWIT(1)
         END IF
      END IF
   ELSE
!       NO ASTOP,PROCEED
   END IF
!
   IF(SURF.EQ.INT(W1)) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'A SURFACE CAN NOT PIKUP ITS OWN VALUES'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!
!
!       SINCE THIS IS A THICKNESS PIKUP THE THIRD DIMENSION WILL
!       BE 3
!       THE PIKUP EXISTENCE FLAG IS PIKUP(1,I,J) FOR EVERY
!       PIKUP ON EVERY SURFACE. IF IT IS 1.0D0 THE PIKUP EXISTS
!       IF IT IS 0.0D0 THE PIKUP DOES NOT EXIST
!
!       STORE NUMERIC INPUT AND HANDEL DEFAULT DATA

!       IF DF1=1 THIS MEANS PIKING UP THE OBJECT THICKNESS
!       ON TO THE CURRENT SURFACE
   IF(DF1.EQ.1) W1=0.0D0
!       IF DF2=1 THIS MEANS MULTIPLIER IS UNITY
   IF(DF2.EQ.1) W2=1.0D0
!       IF DF3=1 TIS MEANS ADDITIVE CONSTANT ZERO
   IF(DF3.EQ.1) W3=0.0D0
!       IF DF4=1 THEN W4=0.0D0
!       IF DF5=1 THEN W5=0.0D0
   IF(DF4.EQ.1) W4=0.0D0
   IF(DF5.EQ.1) W5=0.0D0
   IF(W1.LT.0.0D0) W1=SURF+W1
   IF(W1.LT.0.0D0.OR.W1.GT.sys_last_surf()) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'SOURCE SURFACE NUMBER BEYOND LEGAL RANGE'//'\n'//&
      & 'PIKUP NOT ALLOWED', 1)
      RETURN
   END IF
!               INCREMENT PIKUP COUNTER BY 1.0D0
!               I.E. surf_special_type(SURF)
   IF(PIKUP(1,SURF,32).EQ.0.0D0)call set_surf_special_type(SURF, surf_special_type(SURF)+1)
   PIKUP(1,SURF,32)=0.0D0
   PIKUP(1,SURF,3)=1.0D0
   PIKUP(2,SURF,3)=W1
   PIKUP(3,SURF,3)=W2
   PIKUP(4,SURF,3)=W3
   PIKUP(5,SURF,3)=W4
   PIKUP(6,SURF,3)=W5
!       HANDEL SOLVE DELETIONS IF THEY EXIST
!
   IF(SOLVE(6,SURF).GT.0.0D0) THEN
      SOLVE(6,SURF)=0.0D0
      SOLVE(7,SURF)=0.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :YZ PLANE THICKNESS SOLVE DELETED'
      CALL SHOWIT(1)
      call set_surf_solve_flag(SURF, 0.0D0)
      IF(SOLVE(6,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+1.0D0)
      IF(SOLVE(4,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+0.1D0)
      IF(SOLVE(8,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+2.0D0)
      IF(SOLVE(2,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+0.2D0)
   END IF
   IF(SOLVE(4,SURF).GT.0.0D0) THEN
      SOLVE(4,SURF)=0.0D0
      SOLVE(3,SURF)=0.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :XZ PLANE THICKNESS SOLVE DELETED'
      CALL SHOWIT(1)
      call set_surf_solve_flag(SURF, 0.0D0)
      IF(SOLVE(6,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+1.0D0)
      IF(SOLVE(4,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+0.1D0)
      IF(SOLVE(8,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+2.0D0)
      IF(SOLVE(2,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+0.2D0)
   END IF
!
   RETURN
END
! SUB PIKTHOAL.FOR
SUBROUTINE PIKTHOAL
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_last_surf, sys_astop, sys_astop_adj
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PIKTHOAL. THIS IS THE SUBROUTINE WHICH
!       HANDLES THOAL PIKUPS AT THE LENS INPUT
!       AND UPDATE LENS LEVEL.
!
   INTEGER IM1,IM2
!
!
!
!       TEST TO SEE IF THE SURFACE FROM WHICH WE ARE PIKING UP
!       DATA IS THE SAME AS THE SURFACE TO WHICH THE PIKUP
!       WILL BE ASSINGED. OPERATION NOT LEGAL
!
!       CHECK FOR ASTOP EN OR EX OR ENEX INCONSISTENCIES.
!       IF FOUND, DISALLOW THE PIKUP
!
!               IS THERE AN ASTOP?
   IF(sys_astop().NE.-99.0D0) THEN
!               THERE IS AN ASTOP TO CHECK
      IF(sys_astop_adj().EQ.1.0D0.OR.sys_astop_adj().EQ.2.0D0) THEN
!       THERE IS AN EN ADJUSTMENT IN EFFECT
         IF(SURF.EQ.0.0D0.OR.SURF.EQ.1.0D0) THEN
!       YOU ARE TRYING TO PUT A PIKUP WERE YOU CAN'T
            CALL REPORT_ERROR_AND_FAIL(&
            & 'PIKUP WILL CONFLICT WITH ASTOP (EN)'//'\n'//&
            & 'PIKUP TH ASSIGNMENT DISALLOWED', 1)
            RETURN
         END IF
      END IF
      IF(sys_astop_adj().EQ.-1.0D0.OR.sys_astop_adj().EQ.2.0D0) THEN
!       THERE IS AN EX ADJUSTMENT IN EFFECT
         IM2=(INT(sys_last_surf())-2.0D0)
         IM1=(INT(sys_last_surf())-1.0D0)
         IF(SURF.EQ.IM1.OR.SURF.EQ.IM2) THEN
!       YOU ARE TRYING TO PUT A PIKUP WERE YOU CAN'T
            CALL REPORT_ERROR_AND_FAIL(&
            & 'PIKUP WILL CONFLICT WITH ASTOP (EX)'//'\n'//&
            & 'PIKUP TH ASSIGNMENT DISALLOWED', 1)
            RETURN
         END IF
      END IF
   END IF
!
   IF(SURF.GE.INT(W1).AND.SURF.LE.INT(W2)) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'A SURFACE CAN NOT PIKUP ITS OWN VALUE'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!
!
!       SINCE THIS IS A THICKNESS PIKUP THE THIRD DIMENSION WILL
!       BE 32
!       THE PIKUP EXISTENCE FLAG IS PIKUP(1,I,J) FOR EVERY
!       PIKUP ON EVERY SURFACE. IF IT IS 1.0D0 THE PIKUP EXISTS
!       IF IT IS 0.0D0 THE PIKUP DOES NOT EXIST
!
!       STORE NUMERIC INPUT AND HANDEL DEFAULT DATA
   IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"PIKUP THOAL" REQUIRES EXPLICIT SURFACE NUMBER'//'\n'//&
      & 'ENTRIES FOR NUMERIC WORDS #1 AND #2'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF

!       IF DF3=1 THIS MEANS MULTIPLIER IS UNITY
   IF(DF3.EQ.1) W3=1.0D0
!       IF DF4=1 TIS MEANS ADDITIVE CONSTANT ZERO
   IF(DF4.EQ.1) W4=0.0D0
   IF(DF5.EQ.1) W5=0.0D0
   IF(W1.LT.0.0D0.OR.W1.GT.sys_last_surf().OR.W2.LT.0.0D0 .OR.W2.GT.sys_last_surf().OR.W1.GT.W2) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'SOURCE SURFACE NUMBERS BEYOND LEGAL RANGE'//'\n'//&
      & '"PIKUP THOAL" NOT ALLOWED', 1)
      RETURN
   END IF
!               INCREMENT PIKUP COUNTER BY 1.0D0
!               I.E. surf_special_type(SURF)
   IF(PIKUP(1,SURF,3).EQ.0.0D0)call set_surf_special_type(SURF, surf_special_type(SURF)+1)
   PIKUP(1,SURF,3)=0.0D0
   PIKUP(1,SURF,32)=1.0D0
   PIKUP(2,SURF,32)=W1
   PIKUP(3,SURF,32)=W2
   PIKUP(4,SURF,32)=W3
   PIKUP(5,SURF,32)=W4
   PIKUP(6,SURF,32)=W5
!       HANDEL SOLVE DELETIONS IF THEY EXIST
!
   IF(SOLVE(6,SURF).GT.0.0D0) THEN
      SOLVE(6,SURF)=0.0D0
      SOLVE(7,SURF)=0.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :YZ PLANE THICKNESS SOLVE DELETED'
      CALL SHOWIT(1)
      call set_surf_solve_flag(SURF, 0.0D0)
      IF(SOLVE(6,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+1.0D0)
      IF(SOLVE(4,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+0.1D0)
      IF(SOLVE(8,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+2.0D0)
      IF(SOLVE(2,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+0.2D0)
   END IF
   IF(SOLVE(4,SURF).GT.0.0D0) THEN
      SOLVE(4,SURF)=0.0D0
      SOLVE(3,SURF)=0.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :XZ PLANE THICKNESS SOLVE DELETED'
      CALL SHOWIT(1)
      call set_surf_solve_flag(SURF, 0.0D0)
      IF(SOLVE(6,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+1.0D0)
      IF(SOLVE(4,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+0.1D0)
      IF(SOLVE(8,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+2.0D0)
      IF(SOLVE(2,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+0.2D0)
   END IF
!
   RETURN
END
! SUB PIKRES.FOR
SUBROUTINE PIKRES
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_last_surf
   use mod_lens_data_manager, only: ldm
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PIKRES. THIS IS THE SUBROUTINE WHICH
!       HANDLES THE PIKUP RESOLUTIONS FOR A LENS SYSTEM SURFACE
!       DURING EOS FROM LENS INPUT OR LENS UPDATE
!
   CHARACTER TTP*11
!
   INTEGER II,JJ,KK,IT,ITHJK,PKD,I,K,J,PIKCNT,COMI
!
   COMMON/PIKCOM/COMI
!
   real(real64) DUMMY,DUMMY2,AALL1,AALL2,AALL3,AALL4,TOTTH
!
!
   I=COMI
!
!       ARE THERE PIKUPS ON THIS SURFACE ?
!
   IF(surf_special_type(I).GT.0.0D0) THEN
!       YES, RESOLVE THEM
!       HOW MANY ARE THERE ? THERE ARE surf_special_type(I) OF THEM
!       SET PIKUP COUNTER PIKCNT
      PIKCNT=INT(surf_special_type(I))
      DO 10 K=PIKCNT,0,-1
         DO 20 J=1,PSIZ
            IF(PIKUP(1,I,J).GT.0.0D0) THEN
!       FOUND A PIKUP TO RESOLVE
!**************************************************************
               IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIKUP FROM CURRENT LENS
                  IF(surf_curvature(I).EQ.0.0D0) THEN
                     AALL1=0.0D0
                  ELSE
                     AALL1=1.0D0/surf_curvature(I)
                  END IF
                  IF(surf_curvature(INT(PIKUP(2,I,J))).EQ.0.0D0) THEN
                     AALL2=0.0D0
                  ELSE
                     AALL2=1.0D0/(surf_curvature(INT(PIKUP(2,I,J))))
                  END IF
                  IF(surf_toric_curvature(I).EQ.0.0D0) THEN
                     AALL3=0.0D0
                  ELSE
                     AALL3=1.0D0/surf_toric_curvature(I)
                  END IF
                  IF(surf_toric_curvature(INT(PIKUP(2,I,J))).EQ.0.0D0) THEN
                     AALL4=0.0D0
                  ELSE
                     AALL4=1.0D0/(surf_toric_curvature(INT(PIKUP(2,I,J))))
                  END IF
               ELSE
!     PIKUP FROM MAIN LENS
                  IF(ALENP(1,I).EQ.0.0D0) THEN
                     AALL1=0.0D0
                  ELSE
                     AALL1=1.0D0/ALENP(1,I)
                  END IF
                  IF(ALENP(1,INT(PIKUP(2,I,J))).EQ.0.0D0) THEN
                     AALL2=0.0D0
                  ELSE
                     AALL2=1.0D0/(ALENP(1,INT(PIKUP(2,I,J))))
                  END IF
                  IF(ALENP(24,I).EQ.0.0D0) THEN
                     AALL3=0.0D0
                  ELSE
                     AALL3=1.0D0/ALENP(24,I)
                  END IF
                  IF(ALENP(24,INT(PIKUP(2,I,J))).EQ.0.0D0) THEN
                     AALL4=0.0D0
                  ELSE
                     AALL4=1.0D0/(ALENP(24,INT(PIKUP(2,I,J))))
                  END IF
               END IF
! *************************************************************
               IF(J.EQ.1) THEN
!       PIKUP RD IS FOUND
!       HANDLE SPECIAL PIKUP OPTION
                  IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                     PIKUP(4,I,J)=(AALL1)-(PIKUP(3,I,J)*(AALL2))
                     PIKUP(5,I,J)=0.0D0
                  ELSE
!       NO SPECIAL PIKUP OPTION
                  END IF
                  DUMMY=((((AALL2)*(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  IF(DUMMY.EQ.0.0D0) THEN
                     call set_surf_curvature(I, 0.0D0)
!
                     IF(surf_curvature(I).EQ.0.0D0.AND.surf_conic(I).NE.0.0D0) THEN
                        call set_surf_conic(I, 0.0D0)
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE='THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
!
                     IF(surf_curvature(I).NE.0.0D0.AND.surf_asphere_coeff(I, 2).NE.0.0D0) THEN
                        call set_surf_asphere_coeff(I, 2, 0.0D0)
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE='THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
!
                  ELSE
                     call set_surf_curvature(I, (1.0D0/DUMMY))
!
                     IF(surf_curvature(I).EQ.0.0D0.AND.surf_conic(I).NE.0.0D0) THEN
                        call set_surf_conic(SURF, 0.0D0)
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE='THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                     IF(surf_curvature(I).NE.0.0D0.AND.surf_asphere_coeff(I, 2).NE.0.0D0) THEN
                        call set_surf_asphere_coeff(I, 2, 0.0D0)
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE='THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
!
                  END IF
               ELSE
!       J NOT 1, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.2) THEN
!       PIKUP CV IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_curvature(I)-(PIKUP(3,I,J)*(surf_curvature(INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_curvature(I, (((surf_curvature(INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
!
                     IF(surf_curvature(I).EQ.0.0D0.AND.surf_conic(I).NE.0.0D0) THEN
                        call set_surf_conic(I, 0.0D0)
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE='THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                     IF(surf_curvature(I).NE.0.0D0.AND.surf_asphere_coeff(I, 2).NE.0.0D0) THEN
                        call set_surf_asphere_coeff(I, 2, 0.0D0)
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE='THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                  ELSE
!     PIKUP FROM MAIN LENS
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(1,I)-(PIKUP(3,I,J)*(ALENP(1,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_curvature(I, (((ALENP(1,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
!
                     IF(surf_curvature(I).EQ.0.0D0.AND.surf_conic(I).NE.0.0D0) THEN
                        call set_surf_conic(I, 0.0D0)
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE='THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                     IF(surf_curvature(I).NE.0.0D0.AND.surf_asphere_coeff(I, 2).NE.0.0D0) THEN
                        call set_surf_asphere_coeff(I, 2, 0.0D0)
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE='THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                  END IF
!
               ELSE
!       J NOT 2, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.3) THEN
!       PIKUP TH IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIKUP FROM CURRENT LENS
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_thickness(I)-(PIKUP(3,I,J)*(surf_thickness(INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_thickness(I, (((surf_thickness(INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIKUP FROM MAIN LENS
!     PIKUP FROM CURRENT LENS
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(3,I)-(PIKUP(3,I,J)*(ALENP(3,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_thickness(I, (((ALENP(3,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 3, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.32) THEN
!       PIKUP TH IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIKUP FROM CURRENT LENS
!       NO SPECIAL PIKUP OPTION FOR THOAL
                     TOTTH=0.0D0
                     DO ITHJK=INT(PIKUP(2,I,J)),INT(PIKUP(3,I,J))-1
                        TOTTH=TOTTH+surf_thickness(ITHJK)
                     END DO
                     call set_surf_thickness(I, (((TOTTH*(PIKUP(4,I,J)))+PIKUP(5,I,J))))
                  ELSE
!     PIKUP FROM MAIN LENS
!     PIKUP FROM CURRENT LENS
                     TOTTH=0.0D0
                     DO ITHJK=INT(PIKUP(2,I,J)),INT(PIKUP(3,I,J))-1
                        TOTTH=TOTTH+ALENP(3,ITHJK)
                     END DO
                     call set_surf_thickness(I, (((TOTTH*(PIKUP(4,I,J)))+PIKUP(5,I,J))))
                  END IF
               ELSE
!       J NOT 32, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.4) THEN
!       PIKUP CC IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_conic(I)-(PIKUP(3,I,J)*(surf_conic(INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_conic(I, (((surf_conic(INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
!
                     IF(surf_curvature(I).EQ.0.0D0.AND.surf_conic(I).NE.0.0D0) THEN
                        call set_surf_conic(I, 0.0D0)
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE='THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(2,I)-(PIKUP(3,I,J)*(ALENP(2,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_conic(I, (((ALENP(2,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
!
                     IF(surf_curvature(I).EQ.0.0D0.AND.surf_conic(I).NE.0.0D0) THEN
                        call set_surf_conic(I, 0.0D0)
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE='THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                  END IF
!
               ELSE
!       J NOT 4, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.5) THEN
!       PIKUP AD IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_asphere_coeff(I, 4)-(PIKUP(3,I,J)*(surf_asphere_coeff(INT(PIKUP(2,I,J)), 4)))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_asphere_coeff(I, 4, (((surf_asphere_coeff(INT(PIKUP(2,I,J)), 4)*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(4,I)-(PIKUP(3,I,J)*(ALENP(4,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_asphere_coeff(I, 4, (((ALENP(4,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 5, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.6) THEN
!       PIKUP AE IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_asphere_coeff(I, 6)-(PIKUP(3,I,J)*(surf_asphere_coeff(INT(PIKUP(2,I,J)), 6)))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_asphere_coeff(I, 6, (((surf_asphere_coeff(INT(PIKUP(2,I,J)), 6)*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(5,I)-(PIKUP(3,I,J)*(ALENP(5,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_asphere_coeff(I, 6, (((ALENP(5,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 6, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.7) THEN
!       PIKUP AF IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_asphere_coeff(I, 8)-(PIKUP(3,I,J)*(surf_asphere_coeff(INT(PIKUP(2,I,J)), 8)))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_asphere_coeff(I, 8, (((surf_asphere_coeff(INT(PIKUP(2,I,J)), 8)*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(6,I)-(PIKUP(3,I,J)*(ALENP(6,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_asphere_coeff(I, 8, (((ALENP(6,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 7, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.8) THEN
!       PIKUP AG IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_asphere_coeff(I, 10)-(PIKUP(3,I,J)*(surf_asphere_coeff(INT(PIKUP(2,I,J)), 10)))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_asphere_coeff(I, 10, (((surf_asphere_coeff(INT(PIKUP(2,I,J)), 10)*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(7,I)-(PIKUP(3,I,J)*(ALENP(7,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_asphere_coeff(I, 10, (((ALENP(7,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 8, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.27) THEN
!       PIKUP AH IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_asphere_coeff(I, 12)-(PIKUP(3,I,J)*(surf_asphere_coeff(INT(PIKUP(2,I,J)), 12)))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_asphere_coeff(I, 12, (((surf_asphere_coeff(INT(PIKUP(2,I,J)), 12)*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(81,I)-(PIKUP(3,I,J)*(ALENP(81,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_asphere_coeff(I, 12, (((ALENP(81,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 27, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.28) THEN
!       PIKUP AI IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_asphere_coeff(I, 14)-(PIKUP(3,I,J)*(surf_asphere_coeff(INT(PIKUP(2,I,J)), 14)))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_asphere_coeff(I, 14, (((surf_asphere_coeff(INT(PIKUP(2,I,J)), 14)*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(82,I)-(PIKUP(3,I,J)*(ALENP(82,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_asphere_coeff(I, 14, (((ALENP(82,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 28, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.29) THEN
!       PIKUP AJ IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_asphere_coeff(I, 16)-(PIKUP(3,I,J)*(surf_asphere_coeff(INT(PIKUP(2,I,J)), 16)))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_asphere_coeff(I, 16, (((surf_asphere_coeff(INT(PIKUP(2,I,J)), 16)*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(83,I)-(PIKUP(3,I,J)*(ALENP(83,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_asphere_coeff(I, 16, (((ALENP(83,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 29, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.30) THEN
!       PIKUP AK IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_asphere_coeff(I, 18)-(PIKUP(3,I,J)*(surf_asphere_coeff(INT(PIKUP(2,I,J)), 18)))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_asphere_coeff(I, 18, (((surf_asphere_coeff(INT(PIKUP(2,I,J)), 18)*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(84,I)-(PIKUP(3,I,J)*(ALENP(84,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_asphere_coeff(I, 18, (((ALENP(84,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 30, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.31) THEN
!       PIKUP AL IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_asphere_coeff(I, 20)-(PIKUP(3,I,J)*(surf_asphere_coeff(INT(PIKUP(2,I,J)), 20)))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_asphere_coeff(I, 20, (((surf_asphere_coeff(INT(PIKUP(2,I,J)), 20)*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(85,I)-(PIKUP(3,I,J)*(ALENP(85,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_asphere_coeff(I, 20, (((ALENP(85,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 31, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.9) THEN
!       PIKUP CVTOR IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_toric_curvature(I)-(PIKUP(3,I,J)*(surf_toric_curvature(INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
!
!       IF THE SURFACE BEING PICKED UP FROM IS A TORIC AND
!       IF IT IS NOT THE SAME TORIC TYPE AS THE PICKING SURFACE
!       THEN PRINT A MESSAGE TO THE EFFECT THAT THE TORIC TYPE
!       OF THE PICKING SURFACE IS BEING CHANGED TO THE TORIC TYPE
!       OF THE SURFACE BEING PICKED UP FROM.
                     PKD=INT(PIKUP(2,I,J))
                     IF(surf_toric_flag(INT(PIKUP(2,I,J))).EQ.1.0D0) TTP='AN  Y-TORIC'
                     IF(surf_toric_flag(INT(PIKUP(2,I,J))).EQ.2.0D0) TTP='A   X-TORIC'
                     IF(surf_toric_flag(INT(PIKUP(2,I,J))).EQ.0.0D0) TTP='A NON TORIC'
                     IF(surf_toric_flag(I).NE.surf_toric_flag(INT(PIKUP(2,I,J)))) THEN
                        WRITE(OUTLYNE,100) I
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,200) TTP,PKD
                        CALL SHOWIT(1)
                     END IF
!
                     call set_surf_toric_flag(I, surf_toric_flag(INT(PIKUP(2,I,J))))
                     call set_surf_toric_curvature(I, (((surf_toric_curvature(INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(24,I)-(PIKUP(3,I,J)*(ALENP(24,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
!
!       IF THE SURFACE BEING PICKED UP FROM IS A TORIC AND
!       IF IT IS NOT THE SAME TORIC TYPE AS THE PICKING SURFACE
!       THEN PRINT A MESSAGE TO THE EFFECT THAT THE TORIC TYPE
!       OF THE PICKING SURFACE IS BEING CHANGED TO THE TORIC TYPE
!       OF THE SURFACE BEING PICKED UP FROM.
                     PKD=INT(PIKUP(2,I,J))
                     IF(ALENP(23,INT(PIKUP(2,I,J))).EQ.1.0D0) TTP='AN  Y-TORIC'
                     IF(ALENP(23,INT(PIKUP(2,I,J))).EQ.2.0D0) TTP='A   X-TORIC'
                     IF(ALENP(23,INT(PIKUP(2,I,J))).EQ.0.0D0) TTP='A NON TORIC'
                     IF(surf_toric_flag(I).NE.ALENP(23,INT(PIKUP(2,I,J)))) THEN
                        WRITE(OUTLYNE,100) I
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,200) TTP,PKD
                        CALL SHOWIT(1)
                     END IF
!
                     call set_surf_toric_flag(I, INT(ALENP(23,INT(PIKUP(2,I,J)))))
                     call set_surf_toric_curvature(I, (((ALENP(24,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 9, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.10) THEN
!       PIKUP RDTOR IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=(AALL3)-(PIKUP(3,I,J)*(AALL4))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
!
!       IF THE SURFACE BEING PICKED UP FROM IS A TORIC AND
!       IF IT IS NOT THE SAME TORIC TYPE AS THE PICKING SURFACE
!       THEN PRINT A MESSAGE TO THE EFFECT THAT THE TORIC TYPE
!       OF THE PICKING SURFACE IS BEING CHANGED TO THE TORIC TYPE
!       OF THE SURFACE BEING PICKED UP FROM.
                     PKD=INT(PIKUP(2,I,J))
                     IF(surf_toric_flag(INT(PIKUP(2,I,J))).EQ.1.0D0) TTP='AN  Y-TORIC'
                     IF(surf_toric_flag(INT(PIKUP(2,I,J))).EQ.2.0D0) TTP='A   X-TORIC'
                     IF(surf_toric_flag(INT(PIKUP(2,I,J))).EQ.0.0D0) TTP='A NON-TORIC'
                     IF(surf_toric_flag(I).NE.surf_toric_flag(INT(PIKUP(2,I,J)))) THEN
                        WRITE(OUTLYNE,100) I
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,200) TTP,PKD
                        CALL SHOWIT(1)
                     END IF
!
                     call set_surf_toric_flag(I, surf_toric_flag(INT(PIKUP(2,I,J))))
                     DUMMY2=((((AALL4)*(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                     IF(DUMMY.EQ.0.0D0) THEN
                        call set_surf_toric_curvature(I, 0.0D0)
                     ELSE
                        call set_surf_toric_curvature(I, (1.0D0/DUMMY2))
                     END IF
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=(AALL3)-(PIKUP(3,I,J)*(AALL4))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
!
!       IF THE SURFACE BEING PICKED UP FROM IS A TORIC AND
!       IF IT IS NOT THE SAME TORIC TYPE AS THE PICKING SURFACE
!       THEN PRINT A MESSAGE TO THE EFFECT THAT THE TORIC TYPE
!       OF THE PICKING SURFACE IS BEING CHANGED TO THE TORIC TYPE
!       OF THE SURFACE BEING PICKED UP FROM.
                     PKD=INT(PIKUP(2,I,J))
                     IF(ALENP(23,INT(PIKUP(2,I,J))).EQ.1.0D0) TTP='AN  Y-TORIC'
                     IF(ALENP(23,INT(PIKUP(2,I,J))).EQ.2.0D0) TTP='A   X-TORIC'
                     IF(ALENP(23,INT(PIKUP(2,I,J))).EQ.0.0D0) TTP='A NON-TORIC'
                     IF(surf_toric_flag(I).NE.ALENP(23,INT(PIKUP(2,I,J)))) THEN
                        WRITE(OUTLYNE,100) I
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,200) TTP,PKD
                        CALL SHOWIT(1)
                     END IF
!
                     call set_surf_toric_flag(I, INT(ALENP(23,INT(PIKUP(2,I,J)))))
                     DUMMY2=((((AALL4)*(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                     IF(DUMMY.EQ.0.0D0) THEN
                        call set_surf_toric_curvature(I, 0.0D0)
                     ELSE
                        call set_surf_toric_curvature(I, (1.0D0/DUMMY2))
                     END IF
                  END IF
               ELSE
!       J NOT 10, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.11) THEN
!       PIKUP PRO IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       PIKUP PRO HAS NO SPECIAL PIKUP OPTION
!       PIKUP EVERY SURFACE PROILE DATA ITEM FROM THE
!       SOURCE SURFACE TO THE TARGET SURFACE
                     call set_surf_curvature(I, surf_curvature(INT(PIKUP(2,I,J))))
                     call set_surf_conic(I, surf_conic(INT(PIKUP(2,I,J))))
                     call set_surf_asphere_coeff(I, 4, surf_asphere_coeff(INT(PIKUP(2,I,J)), 4))
                     call set_surf_asphere_coeff(I, 6, surf_asphere_coeff(INT(PIKUP(2,I,J)), 6))
                     call set_surf_asphere_coeff(I, 8, surf_asphere_coeff(INT(PIKUP(2,I,J)), 8))
                     call set_surf_asphere_coeff(I, 10, surf_asphere_coeff(INT(PIKUP(2,I,J)), 10))
                     ALENS(8,I)=ALENS(8,INT(PIKUP(2,I,J)))
                     call set_surf_asphere_coeff(I, 12, surf_asphere_coeff(INT(PIKUP(2,I,J)), 12))
                     call set_surf_asphere_coeff(I, 14, surf_asphere_coeff(INT(PIKUP(2,I,J)), 14))
                     call set_surf_asphere_coeff(I, 16, surf_asphere_coeff(INT(PIKUP(2,I,J)), 16))
                     call set_surf_asphere_coeff(I, 18, surf_asphere_coeff(INT(PIKUP(2,I,J)), 18))
                     call set_surf_asphere_coeff(I, 20, surf_asphere_coeff(INT(PIKUP(2,I,J)), 20))
                     call set_surf_diffraction_flag(I, surf_diffraction_flag(INT(PIKUP(2,I,J))))
                     call set_surf_grating_order(I, surf_grating_order(INT(PIKUP(2,I,J))))
                     call set_surf_grating_spacing(I, surf_grating_spacing(INT(PIKUP(2,I,J))))
                     call set_surf_grating_vx(I, surf_grating_vx(INT(PIKUP(2,I,J))))
                     call set_surf_grating_vy(I, surf_grating_vy(INT(PIKUP(2,I,J))))
                     call set_surf_grating_vz(I, surf_grating_vz(INT(PIKUP(2,I,J))))
                     call set_surf_ccr_flag(I, surf_ccr_flag(INT(PIKUP(2,I,J))))
                     call set_surf_roof_a(I, surf_roof_a(INT(PIKUP(2,I,J))))
                     call set_surf_roof_angle_err(I, surf_roof_angle_err(INT(PIKUP(2,I,J))))
                     call set_surf_roof_b(I, surf_roof_b(INT(PIKUP(2,I,J))))
                     call set_surf_ccr_angle_err2(I, surf_ccr_angle_err2(INT(PIKUP(2,I,J))))
                     call set_surf_ccr_b(I, surf_ccr_b(INT(PIKUP(2,I,J))))
                     IF(surf_asi_flag(INT(PIKUP(2,I,J))).NE.0.0D0) THEN
                        call set_surf_asi_flag(I, surf_asi_flag(INT(PIKUP(2,I,J))))
                        FTFL01(1:96,I)=FTFL01(1:96,INT(PIKUP(2,I,J)))
                     END IF

!
                     IF(surf_curvature(I).EQ.0.0D0.AND.surf_conic(I).NE.0.0D0) THEN
                        call set_surf_conic(I, 0.0D0)
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE='THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                     IF(surf_curvature(I).NE.0.0D0.AND.surf_asphere_coeff(I, 2).NE.0.0D0) THEN
                        call set_surf_asphere_coeff(I, 2, 0.0D0)
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE='THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
!
!
!       IF THE SURFACE BEING PICKED UP FROM IS A TORIC AND
!       IF IT IS NOT THE SAME TORIC TYPE AS THE PICKING SURFACE
!       THEN PRINT A MESSAGE TO THE EFFECT THAT THE TORIC TYPE
!       OF THE PICKING SURFACE IS BEING CHANGED TO THE TORIC TYPE
!       OF THE SURFACE BEING PICKED UP FROM.
                     PKD=INT(PIKUP(2,I,J))
                     IF(surf_toric_flag(INT(PIKUP(2,I,J))).EQ.1.0D0) TTP='AN  Y-TORIC'
                     IF(surf_toric_flag(INT(PIKUP(2,I,J))).EQ.2.0D0) TTP='A   X-TORIC'
                     IF(surf_toric_flag(INT(PIKUP(2,I,J))).EQ.0.0D0) TTP='A NON-TORIC'
                     IF(surf_toric_flag(I).NE.surf_toric_flag(INT(PIKUP(2,I,J)))) THEN
                        WRITE(OUTLYNE,100) I
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,200) TTP,PKD
                        CALL SHOWIT(1)
                     END IF
!
                     call set_surf_toric_flag(I, surf_toric_flag(INT(PIKUP(2,I,J))))
                     call set_surf_toric_curvature(I, surf_toric_curvature(INT(PIKUP(2,I,J))))
                     call set_surf_asi_flag(I, surf_asi_flag(INT(PIKUP(2,I,J))))
                     call set_surf_anamorphic_flag(I, surf_anamorphic_flag(INT(PIKUP(2,I,J))))
                     call set_surf_anamorphic_coeff(I, 4, surf_anamorphic_coeff(INT(PIKUP(2,I,J)), 4))
                     call set_surf_anamorphic_coeff(I, 6, surf_anamorphic_coeff(INT(PIKUP(2,I,J)), 6))
                     call set_surf_anamorphic_coeff(I, 8, surf_anamorphic_coeff(INT(PIKUP(2,I,J)), 8))
                     call set_surf_anamorphic_coeff(I, 10, surf_anamorphic_coeff(INT(PIKUP(2,I,J)), 10))
                     call set_surf_anamorphic_conic(I, surf_anamorphic_conic(INT(PIKUP(2,I,J))))
                     call set_surf_asphere_coeff(I, 2, surf_asphere_coeff(INT(PIKUP(2,I,J)), 2))
!
                     IF(surf_curvature(I).NE.0.0D0.AND.surf_asphere_coeff(I, 2).NE.0.0D0) THEN
                        call set_surf_asphere_coeff(I, 2, 0.0D0)
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE='THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                  ELSE
!     PIK FROM MAIN CFG
!       PIKUP PRO HAS NO SPECIAL PIKUP OPTION
!       PIKUP EVERY SURFACE PROILE DATA ITEM FROM THE
!       SOURCE SURFACE TO THE TARGET SURFACE
                     call set_surf_curvature(I, ALENP(1,INT(PIKUP(2,I,J))))
                     call set_surf_conic(I, ALENP(2,INT(PIKUP(2,I,J))))
                     call set_surf_asphere_coeff(I, 4, ALENP(4,INT(PIKUP(2,I,J))))
                     call set_surf_asphere_coeff(I, 6, ALENP(5,INT(PIKUP(2,I,J))))
                     call set_surf_asphere_coeff(I, 8, ALENP(6,INT(PIKUP(2,I,J))))
                     call set_surf_asphere_coeff(I, 10, ALENP(7,INT(PIKUP(2,I,J))))
                     ALENS(8,I)=ALENP(8,INT(PIKUP(2,I,J)))
                     call set_surf_asphere_coeff(I, 12, ALENP(81,INT(PIKUP(2,I,J))))
                     call set_surf_asphere_coeff(I, 14, ALENP(82,INT(PIKUP(2,I,J))))
                     call set_surf_asphere_coeff(I, 16, ALENP(83,INT(PIKUP(2,I,J))))
                     call set_surf_asphere_coeff(I, 18, ALENP(84,INT(PIKUP(2,I,J))))
                     call set_surf_asphere_coeff(I, 20, ALENP(85,INT(PIKUP(2,I,J))))
                     call set_surf_diffraction_flag(I, surf_diffraction_flag(INT(PIKUP(2,I,J))))
                     call set_surf_grating_order(I, surf_grating_order(INT(PIKUP(2,I,J))))
                     call set_surf_grating_spacing(I, surf_grating_spacing(INT(PIKUP(2,I,J))))
                     call set_surf_grating_vx(I, surf_grating_vx(INT(PIKUP(2,I,J))))
                     call set_surf_grating_vy(I, surf_grating_vy(INT(PIKUP(2,I,J))))
                     call set_surf_grating_vz(I, surf_grating_vz(INT(PIKUP(2,I,J))))
!
                     IF(surf_curvature(I).EQ.0.0D0.AND.surf_conic(I).NE.0.0D0) THEN
                        call set_surf_conic(I, 0.0D0)
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE='THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                     IF(surf_curvature(I).NE.0.0D0.AND.surf_asphere_coeff(I, 2).NE.0.0D0) THEN
                        call set_surf_asphere_coeff(I, 2, 0.0D0)
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE='THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
!
!
!       IF THE SURFACE BEING PICKED UP FROM IS A TORIC AND
!       IF IT IS NOT THE SAME TORIC TYPE AS THE PICKING SURFACE
!       THEN PRINT A MESSAGE TO THE EFFECT THAT THE TORIC TYPE
!       OF THE PICKING SURFACE IS BEING CHANGED TO THE TORIC TYPE
!       OF THE SURFACE BEING PICKED UP FROM.
                     PKD=INT(PIKUP(2,I,J))
                     IF(ALENP(23,INT(PIKUP(2,I,J))).EQ.1.0D0) TTP='AN  Y-TORIC'
                     IF(ALENP(23,INT(PIKUP(2,I,J))).EQ.2.0D0) TTP='A   X-TORIC'
                     IF(ALENP(23,INT(PIKUP(2,I,J))).EQ.0.0D0) TTP='A NON-TORIC'
                     IF(surf_toric_flag(I).NE.ALENP(23,INT(PIKUP(2,I,J)))) THEN
                        WRITE(OUTLYNE,100) I
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,200) TTP,PKD
                        CALL SHOWIT(1)
                     END IF
!
                     call set_surf_toric_flag(I, INT(ALENP(23,INT(PIKUP(2,I,J)))))
                     call set_surf_toric_curvature(I, ALENP(24,INT(PIKUP(2,I,J))))
                     call set_surf_asi_flag(I, INT(ALENP(34,INT(PIKUP(2,I,J)))))
                     call set_surf_anamorphic_flag(I, INT(ALENP(36,INT(PIKUP(2,I,J)))))
                     call set_surf_anamorphic_coeff(I, 4, ALENP(37,INT(PIKUP(2,I,J))))
                     call set_surf_anamorphic_coeff(I, 6, ALENP(38,INT(PIKUP(2,I,J))))
                     call set_surf_anamorphic_coeff(I, 8, ALENP(39,INT(PIKUP(2,I,J))))
                     call set_surf_anamorphic_coeff(I, 10, ALENP(40,INT(PIKUP(2,I,J))))
                     call set_surf_anamorphic_conic(I, ALENP(41,INT(PIKUP(2,I,J))))
                     call set_surf_asphere_coeff(I, 2, ALENP(43,INT(PIKUP(2,I,J))))
!
                     IF(surf_curvature(I).NE.0.0D0.AND.surf_asphere_coeff(I, 2).NE.0.0D0) THEN
                        call set_surf_asphere_coeff(I, 2, 0.0D0)
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE='THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                  END IF
!
               ELSE
!       J NOT 11, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.12) THEN
!       PIKUP NPRO IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       PIKUP NPRO HAS NO SPECIAL PIKUP OPTION
!       PIKUP EVERY SURFACE PROILE DATA ITEM FROM THE
!       SOURCE SURFACE TO THE TARGET SURFACE
                     call set_surf_curvature(I, -surf_curvature(INT(PIKUP(2,I,J))))
                     call set_surf_conic(I, surf_conic(INT(PIKUP(2,I,J))))
                     call set_surf_asphere_coeff(I, 4, -surf_asphere_coeff(INT(PIKUP(2,I,J)), 4))
                     call set_surf_asphere_coeff(I, 6, -surf_asphere_coeff(INT(PIKUP(2,I,J)), 6))
                     call set_surf_asphere_coeff(I, 8, -surf_asphere_coeff(INT(PIKUP(2,I,J)), 8))
                     call set_surf_asphere_coeff(I, 10, -surf_asphere_coeff(INT(PIKUP(2,I,J)), 10))
                     ALENS(8,I)=ALENS(8,INT(PIKUP(2,I,J)))
                     call set_surf_asphere_coeff(I, 12, -surf_asphere_coeff(INT(PIKUP(2,I,J)), 12))
                     call set_surf_asphere_coeff(I, 14, -surf_asphere_coeff(INT(PIKUP(2,I,J)), 14))
                     call set_surf_asphere_coeff(I, 16, -surf_asphere_coeff(INT(PIKUP(2,I,J)), 16))
                     call set_surf_asphere_coeff(I, 18, -surf_asphere_coeff(INT(PIKUP(2,I,J)), 18))
                     call set_surf_asphere_coeff(I, 20, -surf_asphere_coeff(INT(PIKUP(2,I,J)), 20))
                     call set_surf_diffraction_flag(I, surf_diffraction_flag(INT(PIKUP(2,I,J))))
                     call set_surf_grating_order(I, surf_grating_order(INT(PIKUP(2,I,J))))
                     call set_surf_grating_spacing(I, surf_grating_spacing(INT(PIKUP(2,I,J))))
                     call set_surf_grating_vx(I, surf_grating_vx(INT(PIKUP(2,I,J))))
                     call set_surf_grating_vy(I, surf_grating_vy(INT(PIKUP(2,I,J))))
                     call set_surf_grating_vz(I, surf_grating_vz(INT(PIKUP(2,I,J))))
                     call set_surf_ccr_flag(I, surf_ccr_flag(INT(PIKUP(2,I,J))))
                     call set_surf_roof_a(I, surf_roof_a(INT(PIKUP(2,I,J))))
                     call set_surf_roof_angle_err(I, -surf_roof_angle_err(INT(PIKUP(2,I,J))))
                     call set_surf_roof_b(I, -surf_roof_b(INT(PIKUP(2,I,J))))
                     call set_surf_ccr_angle_err2(I, -surf_ccr_angle_err2(INT(PIKUP(2,I,J))))
                     call set_surf_ccr_b(I, -surf_ccr_b(INT(PIKUP(2,I,J))))
!
                     IF(surf_curvature(I).EQ.0.0D0.AND.surf_conic(I).NE.0.0D0) THEN
                        call set_surf_conic(I, 0.0D0)
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE='THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                     IF(surf_curvature(I).NE.0.0D0.AND.surf_asphere_coeff(I, 2).NE.0.0D0) THEN
                        call set_surf_asphere_coeff(I, 2, 0.0D0)
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE='THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
!
!
!       IF THE SURFACE BEING PICKED UP FROM IS A TORIC AND
!       IF IT IS NOT THE SAME TORIC TYPE AS THE PICKING SURFACE
!       THEN PRINT A MESSAGE TO THE EFFECT THAT THE TORIC TYPE
!       OF THE PICKING SURFACE IS BEING CHANGED TO THE TORIC TYPE
!       OF THE SURFACE BEING PICKED UP FROM.
                     PKD=INT(PIKUP(2,I,J))
                     IF(surf_toric_flag(INT(PIKUP(2,I,J))).EQ.1.0D0) TTP='AN  Y-TORIC'
                     IF(surf_toric_flag(INT(PIKUP(2,I,J))).EQ.2.0D0) TTP='A   X-TORIC'
                     IF(surf_toric_flag(INT(PIKUP(2,I,J))).EQ.0.0D0) TTP='A NON-TORIC'
                     IF(surf_toric_flag(I).NE.surf_toric_flag(INT(PIKUP(2,I,J)))) THEN
                        WRITE(OUTLYNE,100) I
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,200) TTP,PKD
                        CALL SHOWIT(1)
                     END IF
!
                     call set_surf_toric_flag(I, surf_toric_flag(INT(PIKUP(2,I,J))))
                     call set_surf_toric_curvature(I, -surf_toric_curvature(INT(PIKUP(2,I,J))))
                     call set_surf_asi_flag(I, surf_asi_flag(INT(PIKUP(2,I,J))))
                     call set_surf_anamorphic_flag(I, surf_anamorphic_flag(INT(PIKUP(2,I,J))))
                     call set_surf_anamorphic_coeff(I, 4, -surf_anamorphic_coeff(INT(PIKUP(2,I,J)), 4))
                     call set_surf_anamorphic_coeff(I, 6, -surf_anamorphic_coeff(INT(PIKUP(2,I,J)), 6))
                     call set_surf_anamorphic_coeff(I, 8, -surf_anamorphic_coeff(INT(PIKUP(2,I,J)), 8))
                     call set_surf_anamorphic_coeff(I, 10, -surf_anamorphic_coeff(INT(PIKUP(2,I,J)), 10))
                     call set_surf_anamorphic_conic(I, surf_anamorphic_conic(INT(PIKUP(2,I,J))))
                     call set_surf_asphere_coeff(I, 2, surf_asphere_coeff(INT(PIKUP(2,I,J)), 2))
!
                     IF(surf_curvature(I).NE.0.0D0.AND.surf_asphere_coeff(I, 2).NE.0.0D0) THEN
                        call set_surf_asphere_coeff(I, 2, 0.0D0)
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE='THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                  ELSE
!     PIK FROM MAIN CFG
!       PIKUP NPRO HAS NO SPECIAL PIKUP OPTION
!       PIKUP EVERY SURFACE PROILE DATA ITEM FROM THE
!       SOURCE SURFACE TO THE TARGET SURFACE
                     call set_surf_curvature(I, -ALENP(1,INT(PIKUP(2,I,J))))
                     call set_surf_conic(I, ALENP(2,INT(PIKUP(2,I,J))))
                     call set_surf_asphere_coeff(I, 4, -ALENP(4,INT(PIKUP(2,I,J))))
                     call set_surf_asphere_coeff(I, 6, -ALENP(5,INT(PIKUP(2,I,J))))
                     call set_surf_asphere_coeff(I, 8, -ALENP(6,INT(PIKUP(2,I,J))))
                     call set_surf_asphere_coeff(I, 10, -ALENP(7,INT(PIKUP(2,I,J))))
                     ALENS(8,I)=ALENP(8,INT(PIKUP(2,I,J)))
                     call set_surf_asphere_coeff(I, 12, -ALENP(81,INT(PIKUP(2,I,J))))
                     call set_surf_asphere_coeff(I, 14, -ALENP(82,INT(PIKUP(2,I,J))))
                     call set_surf_asphere_coeff(I, 16, -ALENP(83,INT(PIKUP(2,I,J))))
                     call set_surf_asphere_coeff(I, 18, -ALENP(84,INT(PIKUP(2,I,J))))
                     call set_surf_asphere_coeff(I, 20, -ALENP(85,INT(PIKUP(2,I,J))))
                     call set_surf_diffraction_flag(I, surf_diffraction_flag(INT(PIKUP(2,I,J))))
                     call set_surf_grating_order(I, surf_grating_order(INT(PIKUP(2,I,J))))
                     call set_surf_grating_spacing(I, surf_grating_spacing(INT(PIKUP(2,I,J))))
                     call set_surf_grating_vx(I, surf_grating_vx(INT(PIKUP(2,I,J))))
                     call set_surf_grating_vy(I, surf_grating_vy(INT(PIKUP(2,I,J))))
                     call set_surf_grating_vz(I, surf_grating_vz(INT(PIKUP(2,I,J))))
!
                     IF(surf_curvature(I).EQ.0.0D0.AND.surf_conic(I).NE.0.0D0) THEN
                        call set_surf_conic(I, 0.0D0)
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE='THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                     IF(surf_curvature(I).NE.0.0D0.AND.surf_asphere_coeff(I, 2).NE.0.0D0) THEN
                        call set_surf_asphere_coeff(I, 2, 0.0D0)
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE='THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
!
!
!       IF THE SURFACE BEING PICKED UP FROM IS A TORIC AND
!       IF IT IS NOT THE SAME TORIC TYPE AS THE PICKING SURFACE
!       THEN PRINT A MESSAGE TO THE EFFECT THAT THE TORIC TYPE
!       OF THE PICKING SURFACE IS BEING CHANGED TO THE TORIC TYPE
!       OF THE SURFACE BEING PICKED UP FROM.
                     PKD=INT(PIKUP(2,I,J))
                     IF(ALENP(23,INT(PIKUP(2,I,J))).EQ.1.0D0) TTP='AN  Y-TORIC'
                     IF(ALENP(23,INT(PIKUP(2,I,J))).EQ.2.0D0) TTP='A   X-TORIC'
                     IF(ALENP(23,INT(PIKUP(2,I,J))).EQ.0.0D0) TTP='A NON-TORIC'
                     IF(surf_toric_flag(I).NE.ALENP(23,INT(PIKUP(2,I,J)))) THEN
                        WRITE(OUTLYNE,100) I
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,200) TTP,PKD
                        CALL SHOWIT(1)
                     END IF
!
                     call set_surf_toric_flag(I, INT(ALENP(23,INT(PIKUP(2,I,J)))))
                     call set_surf_toric_curvature(I, -ALENP(24,INT(PIKUP(2,I,J))))
                     call set_surf_asi_flag(I, INT(ALENP(34,INT(PIKUP(2,I,J)))))
                     call set_surf_anamorphic_flag(I, INT(ALENP(36,INT(PIKUP(2,I,J)))))
                     call set_surf_anamorphic_coeff(I, 4, -ALENP(37,INT(PIKUP(2,I,J))))
                     call set_surf_anamorphic_coeff(I, 6, -ALENP(38,INT(PIKUP(2,I,J))))
                     call set_surf_anamorphic_coeff(I, 8, -ALENP(39,INT(PIKUP(2,I,J))))
                     call set_surf_anamorphic_coeff(I, 10, -ALENP(40,INT(PIKUP(2,I,J))))
                     call set_surf_anamorphic_conic(I, ALENP(41,INT(PIKUP(2,I,J))))
                     call set_surf_asphere_coeff(I, 2, ALENP(43,INT(PIKUP(2,I,J))))
!
                     IF(surf_curvature(I).NE.0.0D0.AND.surf_asphere_coeff(I, 2).NE.0.0D0) THEN
                        call set_surf_asphere_coeff(I, 2, 0.0D0)
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE='THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                  END IF
!
               ELSE
!       J NOT 12, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.13) THEN
!       PIKUP YD IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_focus_dy(I)-(PIKUP(3,I,J)*(surf_focus_dy(INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0d0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_decenter_flag(I, (surf_decenter_flag(INT(PIKUP(2,I,J)))))
                     call set_surf_decenter_y(I, (((surf_decenter_y(INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                     call set_surf_focus_dy(I, (((surf_focus_dy(INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(30,I)-(PIKUP(3,I,J)*(ALENP(30,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_decenter_flag(I, INT((ALENP(29,INT(PIKUP(2,I,J))))))
                     call set_surf_decenter_y(I, (((ALENP(30,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                     call set_surf_focus_dy(I, (((ALENP(115,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 13, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.33) THEN
!       PIKUP ZD IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_focus_dz(I)-(PIKUP(3,I,J)*(surf_focus_dz(INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0d0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_decenter_flag(I, (surf_decenter_flag(INT(PIKUP(2,I,J)))))
                     call set_surf_decenter_z(I, (((surf_decenter_z(INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                     call set_surf_focus_dz(I, (((surf_focus_dz(INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(69,I)-(PIKUP(3,I,J)*(ALENP(69,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_decenter_flag(I, INT((ALENP(29,INT(PIKUP(2,I,J))))))
                     call set_surf_decenter_z(I, (((ALENP(69,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                     call set_surf_focus_dz(I, (((ALENP(116,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 33, CONTINUE PROCESSING
               END IF
!**************************************************************
               IF(J.EQ.34) THEN
!       PIKUP PIVX IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_pivot_x(I)-(PIKUP(3,I,J)*(surf_pivot_x(INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_pivot_flag(I, (surf_pivot_flag(INT(PIKUP(2,I,J)))))
                     call set_surf_pivot_x(I, (((surf_pivot_x(INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(78,I)-(PIKUP(3,I,J)*(ALENP(78,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_pivot_flag(I, INT((ALENP(59,INT(PIKUP(2,I,J))))))
                     call set_surf_pivot_x(I, (((ALENP(78,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 34, CONTINUE PROCESSING
               END IF
!**************************************************************
               IF(J.EQ.35) THEN
!       PIKUP PIVY IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_pivot_y(I)-(PIKUP(3,I,J)*(surf_pivot_y(INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_pivot_flag(I, (surf_pivot_flag(INT(PIKUP(2,I,J)))))
                     call set_surf_pivot_y(I, (((surf_pivot_y(INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(79,I)-(PIKUP(3,I,J)*(ALENP(79,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_pivot_flag(I, INT((ALENP(59,INT(PIKUP(2,I,J))))))
                     call set_surf_pivot_y(I, (((ALENP(79,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 35, CONTINUE PROCESSING
               END IF
!**************************************************************
               IF(J.EQ.36) THEN
!       PIKUP PIVZ IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_pivot_z(I)-(PIKUP(3,I,J)*(surf_pivot_z(INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_pivot_flag(I, (surf_pivot_flag(INT(PIKUP(2,I,J)))))
                     call set_surf_pivot_z(I, (((surf_pivot_z(INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(80,I)-(PIKUP(3,I,J)*(ALENP(80,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_pivot_flag(I, INT((ALENP(59,INT(PIKUP(2,I,J))))))
                     call set_surf_pivot_z(I, (((ALENP(80,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 36, CONTINUE PROCESSING
               END IF
!**************************************************************
               IF(J.EQ.14) THEN
!       PIKUP XD IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_focus_dx(I)-(PIKUP(3,I,J)*(surf_focus_dx(INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_decenter_flag(I, (surf_decenter_flag(INT(PIKUP(2,I,J)))))
                     call set_surf_decenter_x(I, (((surf_decenter_x(INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                     call set_surf_focus_dx(I, (((surf_focus_dx(INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(31,I)-(PIKUP(3,I,J)*(ALENP(31,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_decenter_flag(I, INT((ALENP(29,INT(PIKUP(2,I,J))))))
                     call set_surf_decenter_x(I, (((ALENP(31,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                     call set_surf_focus_dx(I, (((ALENP(114,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 14, CONTINUE PROCESSING
               END IF
!**************************************************************
!**************************************************************
               IF(J.EQ.37) THEN
!       PIKUP GDX IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_global_dx(I)-(PIKUP(3,I,J)*(surf_global_dx(INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_global_dx(I, (((surf_global_dx(INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(90,I)-(PIKUP(3,I,J)*(ALENP(90,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_global_dx(I, (((ALENP(90,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 37, CONTINUE PROCESSING
               END IF
!**************************************************************
!**************************************************************
               IF(J.EQ.38) THEN
!       PIKUP GDY IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_global_dy(I)-(PIKUP(3,I,J)*(surf_global_dy(INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_global_dy(I, (((surf_global_dy(INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(91,I)-(PIKUP(3,I,J)*(ALENP(91,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_global_dy(I, (((ALENP(91,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 38, CONTINUE PROCESSING
               END IF
!**************************************************************
!**************************************************************
               IF(J.EQ.39) THEN
!       PIKUP GDZ IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_global_dz(I)-(PIKUP(3,I,J)*(surf_global_dz(INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_global_dz(I, (((surf_global_dz(INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(92,I)-(PIKUP(3,I,J)*(ALENP(92,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_global_dz(I, (((ALENP(92,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 39, CONTINUE PROCESSING
               END IF
!**************************************************************
!**************************************************************
               IF(J.EQ.40) THEN
!       PIKUP GALPHA IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_global_alpha(I)-(PIKUP(3,I,J)*(surf_global_alpha(INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_global_alpha(I, (((surf_global_alpha(INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(93,I)-(PIKUP(3,I,J)*(ALENP(93,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_global_alpha(I, (((ALENP(93,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 40, CONTINUE PROCESSING
               END IF
!**************************************************************
!**************************************************************
               IF(J.EQ.41) THEN
!       PIKUP GBETA IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_global_beta(I)-(PIKUP(3,I,J)*(surf_global_beta(INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_global_beta(I, (((surf_global_beta(INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(94,I)-(PIKUP(3,I,J)*(ALENP(94,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_global_beta(I, (((ALENP(94,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 41, CONTINUE PROCESSING
               END IF
!**************************************************************
!**************************************************************
               IF(J.EQ.42) THEN
!       PIKUP GGAMMA IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_global_gamma(I)-(PIKUP(3,I,J)*(surf_global_gamma(INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_global_gamma(I, (((surf_global_gamma(INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(95,I)-(PIKUP(3,I,J)*(ALENP(95,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_global_gamma(I, (((ALENP(95,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 42, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.15) THEN
!       PIKUP ALPHA IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_alpha(I)-(PIKUP(3,I,J)*(surf_alpha(INT(PIKUP(2,I,J)))))
                        PIKUP(4,I,J)=surf_alpha_deg(I)-(PIKUP(3,I,J)*(surf_alpha_deg(INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_alpha_deg(I, (((surf_alpha_deg(INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                     call set_surf_alpha(I, (((surf_alpha(INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(26,I)-(PIKUP(3,I,J)*(ALENP(26,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_alpha(I, (((ALENP(26,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                     call set_surf_alpha_deg(I, (((ALENP(118,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 15, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.16) THEN
!       PIKUP BETA IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=(surf_beta(I)-(PIKUP(3,I,J)*(surf_beta(INT(PIKUP(2,I,J))))))
                        PIKUP(4,I,J)=(surf_beta_deg(I)-(PIKUP(3,I,J)*(surf_beta_deg(INT(PIKUP(2,I,J))))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_beta_deg(I, (((surf_beta_deg(INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                     call set_surf_beta(I, (((surf_beta(INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=(ALENP(27,I)-(PIKUP(3,I,J)*(ALENP(27,INT(PIKUP(2,I,J))))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_beta(I, (((ALENP(27,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                     call set_surf_beta_deg(I, (((ALENP(119,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 16, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.17) THEN
!       PIKUP GAMMA IS FOUND
!       HANDLE SPECIAL PIKUP OPTION
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_gamma_deg(I)-(PIKUP(3,I,J)*(surf_gamma_deg(INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_gamma(I, (((surf_gamma(INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                     call set_surf_gamma_deg(I, (((surf_gamma_deg(INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(28,I)-(PIKUP(3,I,J)*(ALENP(28,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_gamma(I, (((ALENP(28,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                     call set_surf_gamma_deg(I, (((ALENP(120,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 14, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.18) THEN
!       PIKUP CLAP IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       PIKUP CLAP HAS NO SPECIAL PIKUP OPTION
                     call set_surf_clap_dim(I, 1, (surf_clap_dim(INT(PIKUP(2,I,J)), 1)))
                     call set_surf_clap_dim(I, 2, (surf_clap_dim(INT(PIKUP(2,I,J)), 2)))
                     call set_surf_clap_dim(I, 3, (surf_clap_dim(INT(PIKUP(2,I,J)), 3)))
                     call set_surf_clap_dim(I, 4, (surf_clap_dim(INT(PIKUP(2,I,J)), 4)))
                     call set_surf_clap_dim(I, 5, (surf_clap_dim(INT(PIKUP(2,I,J)), 5)))
                     call set_surf_clap_tilt(I, (surf_clap_tilt(INT(PIKUP(2,I,J)))))
                     call set_surf_cobs_ape_data(I, 1, (surf_cobs_ape_data(INT(PIKUP(2,I,J)), 1)))
                     call set_surf_cobs_ape_data(I, 2, (surf_cobs_ape_data(INT(PIKUP(2,I,J)), 2)))
                     call set_surf_cobs_ape_data(I, 3, (surf_cobs_ape_data(INT(PIKUP(2,I,J)), 3)))
                     call set_surf_cobs_ape_data(I, 4, (surf_cobs_ape_data(INT(PIKUP(2,I,J)), 4)))
                     call set_surf_cobs_ape_data(I, 5, (surf_cobs_ape_data(INT(PIKUP(2,I,J)), 5)))
                     call set_surf_cobs_ape_data(I, 6, (surf_cobs_ape_data(INT(PIKUP(2,I,J)), 6)))
                  ELSE
!     PIK FROM MAIN CFG
!       PIKUP CLAP HAS NO SPECIAL PIKUP OPTION
                     call set_surf_clap_dim(I, 1, (ALENP(10,INT(PIKUP(2,I,J)))))
                     call set_surf_clap_dim(I, 2, (ALENP(11,INT(PIKUP(2,I,J)))))
                     call set_surf_clap_dim(I, 3, (ALENP(12,INT(PIKUP(2,I,J)))))
                     call set_surf_clap_dim(I, 4, (ALENP(13,INT(PIKUP(2,I,J)))))
                     call set_surf_clap_dim(I, 5, (ALENP(14,INT(PIKUP(2,I,J)))))
                     call set_surf_clap_tilt(I, (ALENP(15,INT(PIKUP(2,I,J)))))
                     call set_surf_cobs_ape_data(I, 1, (ALENP(52,INT(PIKUP(2,I,J)))))
                     call set_surf_cobs_ape_data(I, 2, (ALENP(53,INT(PIKUP(2,I,J)))))
                     call set_surf_cobs_ape_data(I, 3, (ALENP(54,INT(PIKUP(2,I,J)))))
                     call set_surf_cobs_ape_data(I, 4, (ALENP(55,INT(PIKUP(2,I,J)))))
                     call set_surf_cobs_ape_data(I, 5, (ALENP(56,INT(PIKUP(2,I,J)))))
                     call set_surf_cobs_ape_data(I, 6, (ALENP(57,INT(PIKUP(2,I,J)))))
                  END IF
               ELSE
!       J NOT 18, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.19) THEN
!       PIKUP COBS IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       PIKUP COBS HAS NO SPECIAL PIKUP OPTION
                     call set_surf_cobs_poly(I, 1, (surf_cobs_poly(INT(PIKUP(2,I,J)), 1)))
                     call set_surf_cobs_poly(I, 2, (surf_cobs_poly(INT(PIKUP(2,I,J)), 2)))
                     call set_surf_cobs_poly(I, 3, (surf_cobs_poly(INT(PIKUP(2,I,J)), 3)))
                     call set_surf_cobs_poly(I, 4, (surf_cobs_poly(INT(PIKUP(2,I,J)), 4)))
                     call set_surf_cobs_poly(I, 5, (surf_cobs_poly(INT(PIKUP(2,I,J)), 5)))
                     call set_surf_cobs_poly(I, 6, (surf_cobs_poly(INT(PIKUP(2,I,J)), 6)))
                     call set_surf_cobs_era_data(I, 1, (surf_cobs_era_data(INT(PIKUP(2,I,J)), 1)))
                     call set_surf_cobs_era_data(I, 2, (surf_cobs_era_data(INT(PIKUP(2,I,J)), 2)))
                     call set_surf_cobs_era_data(I, 3, (surf_cobs_era_data(INT(PIKUP(2,I,J)), 3)))
                     call set_surf_cobs_era_data(I, 4, (surf_cobs_era_data(INT(PIKUP(2,I,J)), 4)))
                     call set_surf_cobs_era_data(I, 5, (surf_cobs_era_data(INT(PIKUP(2,I,J)), 5)))
                     call set_surf_cobs_era_data(I, 6, (surf_cobs_era_data(INT(PIKUP(2,I,J)), 6)))
                  ELSE
!     PIK FROM MAIN CFG
!       PIKUP COBS HAS NO SPECIAL PIKUP OPTION
                     call set_surf_cobs_poly(I, 1, (ALENP(17,INT(PIKUP(2,I,J)))))
                     call set_surf_cobs_poly(I, 2, (ALENP(18,INT(PIKUP(2,I,J)))))
                     call set_surf_cobs_poly(I, 3, (ALENP(19,INT(PIKUP(2,I,J)))))
                     call set_surf_cobs_poly(I, 4, (ALENP(20,INT(PIKUP(2,I,J)))))
                     call set_surf_cobs_poly(I, 5, (ALENP(21,INT(PIKUP(2,I,J)))))
                     call set_surf_cobs_poly(I, 6, (ALENP(22,INT(PIKUP(2,I,J)))))
                     call set_surf_cobs_era_data(I, 1, (ALENP(62,INT(PIKUP(2,I,J)))))
                     call set_surf_cobs_era_data(I, 2, (ALENP(63,INT(PIKUP(2,I,J)))))
                     call set_surf_cobs_era_data(I, 3, (ALENP(64,INT(PIKUP(2,I,J)))))
                     call set_surf_cobs_era_data(I, 4, (ALENP(65,INT(PIKUP(2,I,J)))))
                     call set_surf_cobs_era_data(I, 5, (ALENP(66,INT(PIKUP(2,I,J)))))
                     call set_surf_cobs_era_data(I, 6, (ALENP(67,INT(PIKUP(2,I,J)))))
                  END IF
               ELSE
!       J NOT 19, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.20) THEN
!       PIKUP GLASS IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       PIKUP GLASS HAS NO SPECIAL PIKUP OPTION
                     GLANAM(I,1)=GLANAM(INT(PIKUP(2,I,J)),1)
                     GLANAM(I,2)=GLANAM(INT(PIKUP(2,I,J)),2)
                     call set_surf_refractive_index(I, 1, (surf_refractive_index(INT(PIKUP(2,I,J)), 1)))
                     call set_surf_refractive_index(I, 2, (surf_refractive_index(INT(PIKUP(2,I,J)), 2)))
                     call set_surf_refractive_index(I, 3, (surf_refractive_index(INT(PIKUP(2,I,J)), 3)))
                     call set_surf_refractive_index(I, 4, (surf_refractive_index(INT(PIKUP(2,I,J)), 4)))
                     call set_surf_refractive_index(I, 5, (surf_refractive_index(INT(PIKUP(2,I,J)), 5)))
                     call set_surf_refractive_index(I, 6, (surf_refractive_index(INT(PIKUP(2,I,J)), 6)))
                     call set_surf_refractive_index(I, 7, (surf_refractive_index(INT(PIKUP(2,I,J)), 7)))
                     call set_surf_refractive_index(I, 8, (surf_refractive_index(INT(PIKUP(2,I,J)), 8)))
                     call set_surf_refractive_index(I, 9, (surf_refractive_index(INT(PIKUP(2,I,J)), 9)))
                     call set_surf_refractive_index(I, 10, (surf_refractive_index(INT(PIKUP(2,I,J)), 10)))
                     call set_surf_fict_n(I, (surf_fict_n(INT(PIKUP(2,I,J)))))
                     call set_surf_fict_v(I, (surf_fict_v(INT(PIKUP(2,I,J)))))
                     call set_surf_fict_w(I, (surf_fict_w(INT(PIKUP(2,I,J)))))
                     IF(I.EQ.0.AND.surf_refractive_index(I, 1).LT.0.0D0.AND.GLANAM(I,1)(1:5).NE.'     '.AND.GLANAM(I,2).NE.'REFL'.AND.GLANAM(I,2).NE.'REFLTIRO') THEN
!     AT OBJECT SURFACE, REFRACTIVE INDECIS ARE POSITIVE UNLESS GLASS
!     IS REFL
                        call set_surf_refractive_index(I, 1, -surf_refractive_index(I, 1))
                        call set_surf_refractive_index(I, 2, -surf_refractive_index(I, 2))
                        call set_surf_refractive_index(I, 3, -surf_refractive_index(I, 3))
                        call set_surf_refractive_index(I, 4, -surf_refractive_index(I, 4))
                        call set_surf_refractive_index(I, 5, -surf_refractive_index(I, 5))
                        call set_surf_refractive_index(I, 6, -surf_refractive_index(I, 6))
                        call set_surf_refractive_index(I, 7, -surf_refractive_index(I, 7))
                        call set_surf_refractive_index(I, 8, -surf_refractive_index(I, 8))
                        call set_surf_refractive_index(I, 9, -surf_refractive_index(I, 9))
                        call set_surf_refractive_index(I, 10, -surf_refractive_index(I, 10))
                     END IF
                     IF(I.EQ.0.AND.surf_refractive_index(I, 1).GT.0.0D0.AND.GLANAM(I,1)(1:5).EQ.'     '.AND.GLANAM(I,2).EQ.'REFL'.OR.I.EQ.0.AND.surf_refractive_index(I, 1).GT.0.0D0.AND.GLANAM(I,1)(1:5).EQ.'     '.AND.GLANAM(I,2).EQ.'REFLTIRO') THEN
!     AT OBJECT SURFACE, REFRACTIVE INDECIS ARE POSITIVE UNLESS GLASS
!     IS REFL
!     AT OBJECT SURFACE, REFRACTIVE INDECIS ARE ALWAYS POSITIVE
                        call set_surf_refractive_index(I, 1, -surf_refractive_index(I, 1))
                        call set_surf_refractive_index(I, 2, -surf_refractive_index(I, 2))
                        call set_surf_refractive_index(I, 3, -surf_refractive_index(I, 3))
                        call set_surf_refractive_index(I, 4, -surf_refractive_index(I, 4))
                        call set_surf_refractive_index(I, 5, -surf_refractive_index(I, 5))
                        call set_surf_refractive_index(I, 6, -surf_refractive_index(I, 6))
                        call set_surf_refractive_index(I, 7, -surf_refractive_index(I, 7))
                        call set_surf_refractive_index(I, 8, -surf_refractive_index(I, 8))
                        call set_surf_refractive_index(I, 9, -surf_refractive_index(I, 9))
                        call set_surf_refractive_index(I, 10, -surf_refractive_index(I, 10))
                     END IF
                     IF(I.GT.0.AND.surf_refractive_index(I-1, 1).LT.0.0D0) THEN
!     NOT AT OBJECT AND PREVIOUS SURFACE HAS NEG INDEX
!     IF CURRENT SURFACE IS REFL, AND INDECIS ARE NEG, MAKE POS
                        IF(GLANAM(I,1)(1:5).EQ.'     '.AND.GLANAM(I,2).EQ.'REFL'.AND.surf_refractive_index(I, 1).LT.0.0D0.OR.GLANAM(I,1)(1:5).EQ.'     '.AND.GLANAM(I,2).EQ.'REFLTIRO'.AND.surf_refractive_index(I, 1).LT.0.0D0) THEN
                           call set_surf_refractive_index(I, 1, -surf_refractive_index(I, 1))
                           call set_surf_refractive_index(I, 2, -surf_refractive_index(I, 2))
                           call set_surf_refractive_index(I, 3, -surf_refractive_index(I, 3))
                           call set_surf_refractive_index(I, 4, -surf_refractive_index(I, 4))
                           call set_surf_refractive_index(I, 5, -surf_refractive_index(I, 5))
                           call set_surf_refractive_index(I, 6, -surf_refractive_index(I, 6))
                           call set_surf_refractive_index(I, 7, -surf_refractive_index(I, 7))
                           call set_surf_refractive_index(I, 8, -surf_refractive_index(I, 8))
                           call set_surf_refractive_index(I, 9, -surf_refractive_index(I, 9))
                           call set_surf_refractive_index(I, 10, -surf_refractive_index(I, 10))
                        END IF
!     IF CURRENT SURFACE IS NOT REFL, AND INDECIS ARE POS, MAKE NEG
                        IF(GLANAM(I,1)(1:5).NE.'     '.AND.GLANAM(I,2).NE.'REFL'.AND.surf_refractive_index(I, 1).GT.0.0D0.AND.GLANAM(I,2).NE.'REFLTIRO') THEN
                           call set_surf_refractive_index(I, 1, -surf_refractive_index(I, 1))
                           call set_surf_refractive_index(I, 2, -surf_refractive_index(I, 2))
                           call set_surf_refractive_index(I, 3, -surf_refractive_index(I, 3))
                           call set_surf_refractive_index(I, 4, -surf_refractive_index(I, 4))
                           call set_surf_refractive_index(I, 5, -surf_refractive_index(I, 5))
                           call set_surf_refractive_index(I, 6, -surf_refractive_index(I, 6))
                           call set_surf_refractive_index(I, 7, -surf_refractive_index(I, 7))
                           call set_surf_refractive_index(I, 8, -surf_refractive_index(I, 8))
                           call set_surf_refractive_index(I, 9, -surf_refractive_index(I, 9))
                           call set_surf_refractive_index(I, 10, -surf_refractive_index(I, 10))
                        END IF
                     END IF
                     IF(I.GT.0.AND.surf_refractive_index(I-1, 1).GT.0.0D0) THEN
!     NOT AT OBJECT AND PREVIOUS SURFACE HAS POS INDEX
!     IF CURRENT SURFACE IS REFL, AND INDECIS ARE POS, MAKE NEG
                        IF(GLANAM(I,1)(1:5).EQ.'     '.AND.GLANAM(I,2).EQ.'REFL'.AND.surf_refractive_index(I, 1).GT.0.0D0.OR.GLANAM(I,1)(1:5).EQ.'     '.AND.GLANAM(I,2).EQ.'REFLTIRO'.AND.surf_refractive_index(I, 1).GT.0.0D0) THEN
                           call set_surf_refractive_index(I, 1, -surf_refractive_index(I, 1))
                           call set_surf_refractive_index(I, 2, -surf_refractive_index(I, 2))
                           call set_surf_refractive_index(I, 3, -surf_refractive_index(I, 3))
                           call set_surf_refractive_index(I, 4, -surf_refractive_index(I, 4))
                           call set_surf_refractive_index(I, 5, -surf_refractive_index(I, 5))
                           call set_surf_refractive_index(I, 6, -surf_refractive_index(I, 6))
                           call set_surf_refractive_index(I, 7, -surf_refractive_index(I, 7))
                           call set_surf_refractive_index(I, 8, -surf_refractive_index(I, 8))
                           call set_surf_refractive_index(I, 9, -surf_refractive_index(I, 9))
                           call set_surf_refractive_index(I, 10, -surf_refractive_index(I, 10))
                        END IF
                        IF(GLANAM(I,1)(1:5).NE.'     '.AND.GLANAM(I,2).NE.'REFL'.AND.surf_refractive_index(I, 1).LT.0.0D0.AND.GLANAM(I,2).NE.'REFLTIRO') THEN
                           call set_surf_refractive_index(I, 1, -surf_refractive_index(I, 1))
                           call set_surf_refractive_index(I, 2, -surf_refractive_index(I, 2))
                           call set_surf_refractive_index(I, 3, -surf_refractive_index(I, 3))
                           call set_surf_refractive_index(I, 4, -surf_refractive_index(I, 4))
                           call set_surf_refractive_index(I, 5, -surf_refractive_index(I, 5))
                           call set_surf_refractive_index(I, 6, -surf_refractive_index(I, 6))
                           call set_surf_refractive_index(I, 7, -surf_refractive_index(I, 7))
                           call set_surf_refractive_index(I, 8, -surf_refractive_index(I, 8))
                           call set_surf_refractive_index(I, 9, -surf_refractive_index(I, 9))
                           call set_surf_refractive_index(I, 10, -surf_refractive_index(I, 10))
                        END IF
                     END IF
                  ELSE
!     PIK FROM MAIN CFG
!       PIKUP GLASS HAS NO SPECIAL PIKUP OPTION
                     GLANAM(I,1)=GLANMP(INT(PIKUP(2,I,J)),1)
                     GLANAM(I,2)=GLANMP(INT(PIKUP(2,I,J)),2)
                     call set_surf_refractive_index(I, 1, (ALENP(46,INT(PIKUP(2,I,J)))))
                     call set_surf_refractive_index(I, 2, (ALENP(47,INT(PIKUP(2,I,J)))))
                     call set_surf_refractive_index(I, 3, (ALENP(48,INT(PIKUP(2,I,J)))))
                     call set_surf_refractive_index(I, 4, (ALENP(49,INT(PIKUP(2,I,J)))))
                     call set_surf_refractive_index(I, 5, (ALENP(50,INT(PIKUP(2,I,J)))))
                     call set_surf_refractive_index(I, 6, (ALENP(71,INT(PIKUP(2,I,J)))))
                     call set_surf_refractive_index(I, 7, (ALENP(72,INT(PIKUP(2,I,J)))))
                     call set_surf_refractive_index(I, 8, (ALENP(73,INT(PIKUP(2,I,J)))))
                     call set_surf_refractive_index(I, 9, (ALENP(74,INT(PIKUP(2,I,J)))))
                     call set_surf_refractive_index(I, 10, (ALENP(75,INT(PIKUP(2,I,J)))))
                     call set_surf_fict_n(I, (ALENP(86,INT(PIKUP(2,I,J)))))
                     call set_surf_fict_v(I, (ALENP(87,INT(PIKUP(2,I,J)))))
                     call set_surf_fict_w(I, (ALENP(89,INT(PIKUP(2,I,J)))))
                     IF(I.EQ.0.AND.surf_refractive_index(I, 1).LT.0.0D0.AND.GLANAM(I,1)(1:5).NE.'     '.AND.GLANAM(I,2).NE.'REFL'.AND.GLANAM(I,2).NE.'REFLTIRO') THEN
!     AT OBJECT SURFACE, REFRACTIVE INDECIS ARE POSITIVE UNLESS GLASS
!     IS REFL
!     AT OBJECT SURFACE, REFRACTIVE INDECIS ARE ALWAYS POSITIVE
                        call set_surf_refractive_index(I, 1, -surf_refractive_index(I, 1))
                        call set_surf_refractive_index(I, 2, -surf_refractive_index(I, 2))
                        call set_surf_refractive_index(I, 3, -surf_refractive_index(I, 3))
                        call set_surf_refractive_index(I, 4, -surf_refractive_index(I, 4))
                        call set_surf_refractive_index(I, 5, -surf_refractive_index(I, 5))
                        call set_surf_refractive_index(I, 6, -surf_refractive_index(I, 6))
                        call set_surf_refractive_index(I, 7, -surf_refractive_index(I, 7))
                        call set_surf_refractive_index(I, 8, -surf_refractive_index(I, 8))
                        call set_surf_refractive_index(I, 9, -surf_refractive_index(I, 9))
                        call set_surf_refractive_index(I, 10, -surf_refractive_index(I, 10))
                     END IF
                     IF(I.EQ.0.AND.surf_refractive_index(I, 1).GT.0.0D0.AND.GLANAM(I,1)(1:5).EQ.'     '.AND.GLANAM(I,2).EQ.'REFL'.OR.I.EQ.0.AND.surf_refractive_index(I, 1).GT.0.0D0.AND.GLANAM(I,1)(1:5).EQ.'     '.AND.GLANAM(I,2).EQ.'REFLTIRO') THEN
!     AT OBJECT SURFACE, REFRACTIVE INDECIS ARE POSITIVE UNLESS GLASS
!     IS REFL
!     AT OBJECT SURFACE, REFRACTIVE INDECIS ARE ALWAYS POSITIVE
                        call set_surf_refractive_index(I, 1, -surf_refractive_index(I, 1))
                        call set_surf_refractive_index(I, 2, -surf_refractive_index(I, 2))
                        call set_surf_refractive_index(I, 3, -surf_refractive_index(I, 3))
                        call set_surf_refractive_index(I, 4, -surf_refractive_index(I, 4))
                        call set_surf_refractive_index(I, 5, -surf_refractive_index(I, 5))
                        call set_surf_refractive_index(I, 6, -surf_refractive_index(I, 6))
                        call set_surf_refractive_index(I, 7, -surf_refractive_index(I, 7))
                        call set_surf_refractive_index(I, 8, -surf_refractive_index(I, 8))
                        call set_surf_refractive_index(I, 9, -surf_refractive_index(I, 9))
                        call set_surf_refractive_index(I, 10, -surf_refractive_index(I, 10))
                     END IF
                     IF(I.GT.0.AND.surf_refractive_index(I-1, 1).LT.0.0D0) THEN
!     NOT AT OBJECT AND PREVIOUS SURFACE HAS NEG INDEX
!     IF CURRENT SURFACE IS REFL, AND INDECIS ARE NEG, MAKE POS
                        IF(GLANAM(I,1)(1:5).EQ.'     '.AND.GLANAM(I,2).EQ.'REFL'.AND.surf_refractive_index(I, 1).LT.0.0D0.OR.GLANAM(I,1)(1:5).EQ.'     '.AND.GLANAM(I,2).EQ.'REFLTIRO'.AND.surf_refractive_index(I, 1).LT.0.0D0) THEN
                           call set_surf_refractive_index(I, 1, -surf_refractive_index(I, 1))
                           call set_surf_refractive_index(I, 2, -surf_refractive_index(I, 2))
                           call set_surf_refractive_index(I, 3, -surf_refractive_index(I, 3))
                           call set_surf_refractive_index(I, 4, -surf_refractive_index(I, 4))
                           call set_surf_refractive_index(I, 5, -surf_refractive_index(I, 5))
                           call set_surf_refractive_index(I, 6, -surf_refractive_index(I, 6))
                           call set_surf_refractive_index(I, 7, -surf_refractive_index(I, 7))
                           call set_surf_refractive_index(I, 8, -surf_refractive_index(I, 8))
                           call set_surf_refractive_index(I, 9, -surf_refractive_index(I, 9))
                           call set_surf_refractive_index(I, 10, -surf_refractive_index(I, 10))
                        END IF
!     IF CURRENT SURFACE IS NOT REFL, AND INDECIS ARE POS, MAKE NEG
                        IF(GLANAM(I,1)(1:5).NE.'     '.AND.GLANAM(I,2).NE.'REFL'.AND.surf_refractive_index(I, 1).GT.0.0D0.AND.GLANAM(I,2).NE.'REFLTIRO') THEN
                           call set_surf_refractive_index(I, 1, -surf_refractive_index(I, 1))
                           call set_surf_refractive_index(I, 2, -surf_refractive_index(I, 2))
                           call set_surf_refractive_index(I, 3, -surf_refractive_index(I, 3))
                           call set_surf_refractive_index(I, 4, -surf_refractive_index(I, 4))
                           call set_surf_refractive_index(I, 5, -surf_refractive_index(I, 5))
                           call set_surf_refractive_index(I, 6, -surf_refractive_index(I, 6))
                           call set_surf_refractive_index(I, 7, -surf_refractive_index(I, 7))
                           call set_surf_refractive_index(I, 8, -surf_refractive_index(I, 8))
                           call set_surf_refractive_index(I, 9, -surf_refractive_index(I, 9))
                           call set_surf_refractive_index(I, 10, -surf_refractive_index(I, 10))
                        END IF
                     END IF
                     IF(I.GT.0.AND.surf_refractive_index(I-1, 1).GT.0.0D0) THEN
!     NOT AT OBJECT AND PREVIOUS SURFACE HAS POS INDEX
!     IF CURRENT SURFACE IS REFL, AND INDECIS ARE POS, MAKE NEG
                        IF(GLANAM(I,1)(1:5).EQ.'     '.AND.GLANAM(I,2).EQ.'REFL'.AND.surf_refractive_index(I, 1).GT.0.0D0.OR.GLANAM(I,1)(1:5).EQ.'     '.AND.GLANAM(I,2).EQ.'REFLTIRO'.AND.surf_refractive_index(I, 1).GT.0.0D0) THEN
                           call set_surf_refractive_index(I, 1, -surf_refractive_index(I, 1))
                           call set_surf_refractive_index(I, 2, -surf_refractive_index(I, 2))
                           call set_surf_refractive_index(I, 3, -surf_refractive_index(I, 3))
                           call set_surf_refractive_index(I, 4, -surf_refractive_index(I, 4))
                           call set_surf_refractive_index(I, 5, -surf_refractive_index(I, 5))
                           call set_surf_refractive_index(I, 6, -surf_refractive_index(I, 6))
                           call set_surf_refractive_index(I, 7, -surf_refractive_index(I, 7))
                           call set_surf_refractive_index(I, 8, -surf_refractive_index(I, 8))
                           call set_surf_refractive_index(I, 9, -surf_refractive_index(I, 9))
                           call set_surf_refractive_index(I, 10, -surf_refractive_index(I, 10))
                        END IF
!     IF CURRENT SURFACE IS NOT REFL, AND INDECIS ARE NEG, MAKE POS
                        IF(GLANAM(I,1)(1:5).NE.'     '.AND.GLANAM(I,2).NE.'REFLTIRO'.AND.GLANAM(I,2).NE.'REFL'.AND.surf_refractive_index(I, 1).LT.0.0D0) THEN
                           call set_surf_refractive_index(I, 1, -surf_refractive_index(I, 1))
                           call set_surf_refractive_index(I, 2, -surf_refractive_index(I, 2))
                           call set_surf_refractive_index(I, 3, -surf_refractive_index(I, 3))
                           call set_surf_refractive_index(I, 4, -surf_refractive_index(I, 4))
                           call set_surf_refractive_index(I, 5, -surf_refractive_index(I, 5))
                           call set_surf_refractive_index(I, 6, -surf_refractive_index(I, 6))
                           call set_surf_refractive_index(I, 7, -surf_refractive_index(I, 7))
                           call set_surf_refractive_index(I, 8, -surf_refractive_index(I, 8))
                           call set_surf_refractive_index(I, 9, -surf_refractive_index(I, 9))
                           call set_surf_refractive_index(I, 10, -surf_refractive_index(I, 10))
                        END IF
                     END IF
                  END IF
               ELSE
!       J NOT 20, CONTINUE PROCESSING
               END IF
! DO A SPECIAL RESOLUTION OF INDEX VALUES TO COVER PICKING UP A REFLECTOR

!********************************************************************
!     THIS WAS MOVED HERE AFTER PIKUPS TO MAKE REFL WORK WITH
!     PIKUP GLASS  FIXED ON 6/14/96 FOUND BY HANS PEW OF MOXTEC
!
               DO 201 KK=0,INT(sys_last_surf())
                  call set_surf_refractive_index(KK, 1, DABS(surf_refractive_index(KK, 1)))
                  call set_surf_refractive_index(KK, 2, DABS(surf_refractive_index(KK, 2)))
                  call set_surf_refractive_index(KK, 3, DABS(surf_refractive_index(KK, 3)))
                  call set_surf_refractive_index(KK, 4, DABS(surf_refractive_index(KK, 4)))
                  call set_surf_refractive_index(KK, 5, DABS(surf_refractive_index(KK, 5)))
                  call set_surf_refractive_index(KK, 6, DABS(surf_refractive_index(KK, 6)))
                  call set_surf_refractive_index(KK, 7, DABS(surf_refractive_index(KK, 7)))
                  call set_surf_refractive_index(KK, 8, DABS(surf_refractive_index(KK, 8)))
                  call set_surf_refractive_index(KK, 9, DABS(surf_refractive_index(KK, 9)))
                  call set_surf_refractive_index(KK, 10, DABS(surf_refractive_index(KK, 10)))
201            CONTINUE
!
               F21=0
               DO 101 II=1,INT(sys_last_surf())
                  IF(GLANAM(II,2).EQ.'REFL'.OR.GLANAM(II,2).EQ.'REFLTIRO') THEN
                     IF(F21.EQ.1.OR.F21.EQ.0) THEN
                        F21=-1
                        call set_surf_refractive_index(II, 1, surf_refractive_index((II-1), 1))
                        call set_surf_refractive_index(II, 2, surf_refractive_index((II-1), 2))
                        call set_surf_refractive_index(II, 3, surf_refractive_index((II-1), 3))
                        call set_surf_refractive_index(II, 4, surf_refractive_index((II-1), 4))
                        call set_surf_refractive_index(II, 5, surf_refractive_index((II-1), 5))
                        call set_surf_refractive_index(II, 6, surf_refractive_index((II-1), 6))
                        call set_surf_refractive_index(II, 7, surf_refractive_index((II-1), 7))
                        call set_surf_refractive_index(II, 8, surf_refractive_index((II-1), 8))
                        call set_surf_refractive_index(II, 9, surf_refractive_index((II-1), 9))
                        call set_surf_refractive_index(II, 10, surf_refractive_index((II-1), 10))
                     ELSE
                        IF(F21.EQ.-1)THEN
                           F21=1
                           call set_surf_refractive_index(II, 1, surf_refractive_index((II-1), 1))
                           call set_surf_refractive_index(II, 2, surf_refractive_index((II-1), 2))
                           call set_surf_refractive_index(II, 3, surf_refractive_index((II-1), 3))
                           call set_surf_refractive_index(II, 4, surf_refractive_index((II-1), 4))
                           call set_surf_refractive_index(II, 5, surf_refractive_index((II-1), 5))
                           call set_surf_refractive_index(II, 6, surf_refractive_index((II-1), 6))
                           call set_surf_refractive_index(II, 7, surf_refractive_index((II-1), 7))
                           call set_surf_refractive_index(II, 8, surf_refractive_index((II-1), 8))
                           call set_surf_refractive_index(II, 9, surf_refractive_index((II-1), 9))
                           call set_surf_refractive_index(II, 10, surf_refractive_index((II-1), 10))
                        END IF
                     END IF
                  END IF
                  IF(F21.EQ.0) GO TO 101
                  IF(F21.EQ.-1) THEN
                     IF(surf_refractive_index(II, 1).GT.0.0D0)call set_surf_refractive_index(II, 1, -surf_refractive_index(II, 1))
                     IF(surf_refractive_index(II, 2).GT.0.0D0)call set_surf_refractive_index(II, 2, -surf_refractive_index(II, 2))
                     IF(surf_refractive_index(II, 3).GT.0.0D0)call set_surf_refractive_index(II, 3, -surf_refractive_index(II, 3))
                     IF(surf_refractive_index(II, 4).GT.0.0D0)call set_surf_refractive_index(II, 4, -surf_refractive_index(II, 4))
                     IF(surf_refractive_index(II, 5).GT.0.0D0)call set_surf_refractive_index(II, 5, -surf_refractive_index(II, 5))
                     IF(surf_refractive_index(II, 6).GT.0.0D0)call set_surf_refractive_index(II, 6, -surf_refractive_index(II, 6))
                     IF(surf_refractive_index(II, 7).GT.0.0D0)call set_surf_refractive_index(II, 7, -surf_refractive_index(II, 7))
                     IF(surf_refractive_index(II, 8).GT.0.0D0)call set_surf_refractive_index(II, 8, -surf_refractive_index(II, 8))
                     IF(surf_refractive_index(II, 9).GT.0.0D0)call set_surf_refractive_index(II, 9, -surf_refractive_index(II, 9))
                     IF(surf_refractive_index(II, 10).GT.0.0D0)call set_surf_refractive_index(II, 10, -surf_refractive_index(II, 10))
                  END IF
                  IF(F21.EQ.1) THEN
                     IF(surf_refractive_index(II, 1).LT.0.0D0)call set_surf_refractive_index(II, 1, DABS(surf_refractive_index(II, 1)))
                     IF(surf_refractive_index(II, 2).LT.0.0D0)call set_surf_refractive_index(II, 2, DABS(surf_refractive_index(II, 2)))
                     IF(surf_refractive_index(II, 3).LT.0.0D0)call set_surf_refractive_index(II, 3, DABS(surf_refractive_index(II, 3)))
                     IF(surf_refractive_index(II, 4).LT.0.0D0)call set_surf_refractive_index(II, 4, DABS(surf_refractive_index(II, 4)))
                     IF(surf_refractive_index(II, 5).LT.0.0D0)call set_surf_refractive_index(II, 5, DABS(surf_refractive_index(II, 5)))
                     IF(surf_refractive_index(II, 6).LT.0.0D0)call set_surf_refractive_index(II, 6, DABS(surf_refractive_index(II, 6)))
                     IF(surf_refractive_index(II, 7).LT.0.0D0)call set_surf_refractive_index(II, 7, DABS(surf_refractive_index(II, 7)))
                     IF(surf_refractive_index(II, 8).LT.0.0D0)call set_surf_refractive_index(II, 8, DABS(surf_refractive_index(II, 8)))
                     IF(surf_refractive_index(II, 9).LT.0.0D0)call set_surf_refractive_index(II, 9, DABS(surf_refractive_index(II, 9)))
                     IF(surf_refractive_index(II, 10).LT.0.0D0)call set_surf_refractive_index(II, 10, DABS(surf_refractive_index(II, 10)))
                  END IF
101            CONTINUE
!
!********************************************************************
! *************************************************************
               IF(J.EQ.21) THEN
!       PIKUP CCTOR IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_anamorphic_conic(I)-(PIKUP(3,I,J)*(surf_anamorphic_conic(INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_anamorphic_conic(I, (((surf_anamorphic_conic(INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(41,I)-(PIKUP(3,I,J)*(ALENP(41,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_anamorphic_conic(I, (((ALENP(41,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 21, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.22) THEN
!       PIKUP ADTOR IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_anamorphic_coeff(I, 4)-(PIKUP(3,I,J)*(surf_anamorphic_coeff(INT(PIKUP(2,I,J)), 4)))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_anamorphic_flag(I, surf_anamorphic_flag(INT(PIKUP(2,I,J))))
                     call set_surf_anamorphic_coeff(I, 4, (((surf_anamorphic_coeff(INT(PIKUP(2,I,J)), 4)*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(37,I)-(PIKUP(3,I,J)*(ALENP(37,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_anamorphic_flag(I, INT(ALENP(36,INT(PIKUP(2,I,J)))))
                     call set_surf_anamorphic_coeff(I, 4, (((ALENP(37,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 22, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.23) THEN
!       PIKUP AETOR IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_anamorphic_coeff(I, 6)-(PIKUP(3,I,J)*(surf_anamorphic_coeff(INT(PIKUP(2,I,J)), 6)))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_anamorphic_flag(I, surf_anamorphic_flag(INT(PIKUP(2,I,J))))
                     call set_surf_anamorphic_coeff(I, 6, (((surf_anamorphic_coeff(INT(PIKUP(2,I,J)), 6)*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(38,I)-(PIKUP(3,I,J)*(ALENP(38,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_anamorphic_flag(I, INT(ALENP(36,INT(PIKUP(2,I,J)))))
                     call set_surf_anamorphic_coeff(I, 6, (((ALENP(38,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 23, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.24) THEN
!       PIKUP AFTOR IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_anamorphic_coeff(I, 8)-(PIKUP(3,I,J)*(surf_anamorphic_coeff(INT(PIKUP(2,I,J)), 8)))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_anamorphic_flag(I, surf_anamorphic_flag(INT(PIKUP(2,I,J))))
                     call set_surf_anamorphic_coeff(I, 8, (((surf_anamorphic_coeff(INT(PIKUP(2,I,J)), 8)*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(39,I)-(PIKUP(3,I,J)*(ALENP(39,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_anamorphic_flag(I, INT(ALENP(36,INT(PIKUP(2,I,J)))))
                     call set_surf_anamorphic_coeff(I, 8, (((ALENP(39,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 24, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.25) THEN
!       PIKUP AGTOR IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=surf_anamorphic_coeff(I, 10)-(PIKUP(3,I,J)*(surf_anamorphic_coeff(INT(PIKUP(2,I,J)), 10)))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_anamorphic_flag(I, surf_anamorphic_flag(INT(PIKUP(2,I,J))))
                     call set_surf_anamorphic_coeff(I, 10, (((surf_anamorphic_coeff(INT(PIKUP(2,I,J)), 10)*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(40,I)-(PIKUP(3,I,J)*(ALENP(40,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     call set_surf_anamorphic_flag(I, INT(ALENP(36,INT(PIKUP(2,I,J)))))
                     call set_surf_anamorphic_coeff(I, 10, (((ALENP(40,INT(PIKUP(2,I,J)))*(PIKUP(3,I,J)))+PIKUP(4,I,J))))
                  END IF
               ELSE
!       J NOT 25, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.43) THEN
!       PIKUP GRT IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
                     call set_surf_diffraction_flag(I, surf_diffraction_flag(INT(PIKUP(2,I,J))))
                     call set_surf_grating_order(I, surf_grating_order(INT(PIKUP(2,I,J))))
                     call set_surf_grating_spacing(I, surf_grating_spacing(INT(PIKUP(2,I,J))))
                     call set_surf_grating_vx(I, surf_grating_vx(INT(PIKUP(2,I,J))))
                     call set_surf_grating_vy(I, surf_grating_vy(INT(PIKUP(2,I,J))))
                     call set_surf_grating_vz(I, surf_grating_vz(INT(PIKUP(2,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
                     call set_surf_diffraction_flag(I, INT(ALENP(96,INT(PIKUP(2,I,J)))))
                     call set_surf_grating_order(I, ALENP(97,INT(PIKUP(2,I,J))))
                     call set_surf_grating_spacing(I, ALENP(98,INT(PIKUP(2,I,J))))
                     call set_surf_grating_vx(I, ALENP(99,INT(PIKUP(2,I,J))))
                     call set_surf_grating_vy(I, ALENP(100,INT(PIKUP(2,I,J))))
                     call set_surf_grating_vz(I, ALENP(101,INT(PIKUP(2,I,J))))
                  END IF
               ELSE
!       J NOT 43, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.44) THEN
!       PIKUP COATING IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
                     call set_surf_coating_index(I, surf_coating_index(INT(PIKUP(2,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
                     call set_surf_coating_index(I, INT(ALENP(112,INT(PIKUP(2,I,J)))))
                  END IF
               ELSE
!       J NOT 44, CONTINUE PROCESSING
               END IF
!**************************************************************
!**************************************************************
            ELSE
!       CHECK THE NEXT J VALUE
            END IF
20       CONTINUE
!       RAN THROUGH ALL THE TYPES
10    CONTINUE
!       RESOLVED ALL THE PIKUPS
!       The resolutions above wrote curvature/conic/thickness/asphere values
!       into ALENS via set_surf_*; bring the typed surface store current so the
!       paraxial/real trace (which refracts through ldm%surfaces(I)%s%cv) uses
!       the freshly picked-up geometry.  Doing it here covers every PIKRES call
!       site (PRTRA_NEW inline calls, resolvePikup, PARAX3 helpers).
      call ldm%refresh_typed_surf_geom(I)
   ELSE
!       NO PIKUPS,JUST RETURN
   END IF
!
   RETURN
100 FORMAT('THE TORIC TYPE OF SURFACE ',I3,' IS BEING CHANGED')
200 FORMAT('FROM ',A11,' TO MATCH THAT OF SURFACE ',I3)
END
! SUB PIKPRO.FOR
SUBROUTINE PIKPRO
!
   use mod_lens_data_manager, only: ldm
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_last_surf
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PIKPRO. THIS IS THE SUBROUTINE WHICH
!       HANDLES (PRO) AND (NPRO)PIKUPS AT THE LENS INPUT
!       AND UPDATE LENS LEVEL.
!
!       PRO AND NPRO PIKUP:
!               CURVATURE (CV)
!               CONIC CONSTANT (CC)
!       ASPHERIC DEFINITION AND TERMS (AC TO AG)
!       TORIC DEFINITION AND VALUES (CVTOR) AND TYPE
!       TORIC CONIC AND TORIC ASPHERIC
!       TERMS (CCTOR AND ADTOR TO AGTOR)
!       AND SPECIAL SURFACE TYPE DEFINED WITH SPSRF
!       IT DESTROYS WHATEVER WAS ON THE TARGET SURFACE
!       AND PRINTS A SIMPLE MESSAGE THAT THE SURFACE NOW
!       IS EITHER IDENTICAL TO OR THE NEGATIVE OF THE
!       SOURCE SURFACE. CONIC TERMS ARE THE
!       SAME UNDER PRO AND NPRO, BUT ASPHERIC
!       TERMS AND CURVATURES HAVE A SIGN REVERSAL
!
!
!       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
!       SPECIFIC STORAGE LOCATIONS FOR LENS DATA,SOLVE AND
!       PIKUP ITEMS.
!
!       TEST TO SEE IF THE SURFACE FROM WHICH WE ARE PIKING UP
!       DATA IS THE SAME AS THE SURFACE TO WHICH THE PIKUP
!       WILL BE ASSINGED. OPERATION NOT LEGAL
!
   IF(SURF.EQ.INT(W1)) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'A SURFACE CAN NOT PIKUP ITS OWN VALUES'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!
!
!       FOR (PRO)
!       THE THIRD DIMENSION WILL BE 11
!       FOR (NPRO)
!       THE THIRD DIMENSION WILL BE 12
!
!       THE PIKUP EXISTENCE FLAG IS PIKUP(1,I,J) FOR EVERY
!       PIKUP ON EVERY SURFACE. IF IT IS 1.0D0 THE PIKUP EXISTS
!       IF IT IS 0.0D0 THE PIKUP DOES NOT EXIST
!
!       STORE NUMERIC INPUT AND HANDEL DEFAULT DATA

!       IF DF1=1 THIS MEANS PIKING UP FROM THE OBJECT SURFACE
!       ON TO THE CURRENT SURFACE
   IF(DF1.EQ.1) W1=0.0D0
   W2=0.0D0
   W3=0.0D0
   W4=0.0D0
   IF(DF5.EQ.1) W5=0.0D0
   IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1) THEN
      OUTLYNE='"PIKUP (PRO OR NPRO)" ONLY TAKE NUMERIC WORD #1 AND #5 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      RETURN
   END IF
   IF(W1.LT.0.0D0) W1=SURF+W1
!
   IF(W1.LT.0.0D0.OR.W1.GT.sys_last_surf()) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'SOURCE SURFACE NUMBER BEYOND LEGAL RANGE'//'\n'//&
      & 'PIKUP NOT ALLOWED', 1)
      RETURN
   END IF
!
!       IF SOLVES EXIST ON THE SOURCE SURFACE THEN
!       PRO AND NPRO WILL ONLY BE VALID IF THE TARGET
!       SURFACE HAS A HIGHER SURFACE NUMBER (COMES AFTER)
!       THE SOURCE SURFACE. ALL CURVATURE SOLVES
!       ON THE TARGET SURFACE WILL BE DESTROYED AND SOLVES
!       WILL NO BE ASSIGNED TO THE TARGET SURFACE BY PRO
!       OR NPRO.
!
   IF(SOLVE(8,SURF).GT.0.0D0) THEN
      SOLVE(8,SURF)=0.0D0
      SOLVE(9,SURF)=0.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' : ALL YZ PLANE CURVATURE SOLVES DELETED'
      CALL SHOWIT(1)
   END IF
   IF(SOLVE(2,SURF).GT.0.0D0) THEN
      SOLVE(2,SURF)=0.0D0
      SOLVE(1,SURF)=0.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' : ALL XZ PLANE CURVATURE SOLVES DELETED'
      CALL SHOWIT(1)
   ELSE
!       RESOLVE surf_solve_flag(SURF)
      call set_surf_solve_flag(SURF, 0.0D0)
      IF(SOLVE(6,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+1.0D0)
      IF(SOLVE(4,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+0.1D0)
      IF(SOLVE(8,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+2.0D0)
      IF(SOLVE(2,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+0.2D0)
   END IF
!       DUMP OLD CV  AND CV OR RD PIKUPS
   IF(PIKUP(1,SURF,1).GT.0.0D0) THEN
      PIKUP(1:6,SURF,1)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (RD) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,2).GT.0.0D0) THEN
      PIKUP(1:6,SURF,2)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (CV) DELETED'
      CALL SHOWIT(1)
   END IF
   call set_surf_curvature(SURF, 0.0D0)
   call set_surf_conic(SURF, 0.0D0)
!       CONIC CONSTANT REMOVAL
   IF(PIKUP(1,SURF,4).GT.0.0D0) THEN
      PIKUP(1:6,SURF,4)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (CC) DELETED'
      CALL SHOWIT(1)
   END IF
   call set_surf_conic(SURF, 0.0D0)
!       ASPHERIC DEFINITION,TERM VALUES AND PIKUPS
   IF(PIKUP(1,SURF,26).GT.0.0D0) THEN
      PIKUP(1:6,SURF,26)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AC) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,5).GT.0.0D0) THEN
      PIKUP(1:6,SURF,5)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AD) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,6).GT.0.0D0) THEN
      PIKUP(1:6,SURF,6)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AE) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,7).GT.0.0D0) THEN
      PIKUP(1:6,SURF,7)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AF) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,8).GT.0.0D0) THEN
      PIKUP(1:6,SURF,8)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AG) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,27).GT.0.0D0) THEN
      PIKUP(1:6,SURF,27)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AH) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,28).GT.0.0D0) THEN
      PIKUP(1:6,SURF,28)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AI) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,29).GT.0.0D0) THEN
      PIKUP(1:6,SURF,29)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AJ) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,30).GT.0.0D0) THEN
      PIKUP(1:6,SURF,30)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AK) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,31).GT.0.0D0) THEN
      PIKUP(1:6,SURF,31)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AL) DELETED'
      CALL SHOWIT(1)
   END IF
   ALENS(5:8,SURF)=0.0D0
   call ldm%clearHigherOrderAsphericCoeffs(SURF)
!       TORIC DEFINITION,CONICS,ASPH AND CURVES
   IF(PIKUP(1,SURF,9).GT.0.0D0) THEN
      PIKUP(1:6,SURF,9)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (RDTOR) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,10).GT.0.0D0) THEN
      PIKUP(1:6,SURF,10)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (CVTOR) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,21).GT.0.0D0) THEN
      PIKUP(1:6,SURF,21)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (CCTOR) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,22).GT.0.0D0) THEN
      PIKUP(1:6,SURF,22)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (ADTOR) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,23).GT.0.0D0) THEN
      PIKUP(1:6,SURF,23)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AETOR) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,24).GT.0.0D0) THEN
      PIKUP(1:6,SURF,24)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AFTOR) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,25).GT.0.0D0) THEN
      PIKUP(1:6,SURF,25)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AGTOR) DELETED'
      CALL SHOWIT(1)
   END IF
!       TORICS
   call ldm%clearToricFlagAndCurv(SURF)
!       SPSRF
   call set_surf_asi_flag(SURF, 0)
!       ASPHT
   call ldm%clearAnamorphicCoeffs(SURF)
!       CCTOR
   call set_surf_anamorphic_conic(SURF, 0.0D0)
   IF(PIKUP(1,SURF,11).GT.0.0D0) THEN
      PIKUP(1:6,SURF,11)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (PRO) DELETED'
      CALL SHOWIT(1)
   END IF
   call ldm%clearCurvAndConic(SURF)
   call ldm%clearAsphericCoeffsAndFlag(SURF)
   call ldm%clearHigherOrderAsphericCoeffs(SURF)
   call ldm%clearToricFlagAndCurv(SURF)
   call set_surf_asi_flag(SURF, 0)
   call ldm%clearAnamorphicData(SURF)
   call set_surf_asphere_coeff(SURF, 2, 0.0D0)
   IF(PIKUP(1,SURF,12).GT.0.0D0) THEN
      PIKUP(1:6,SURF,12)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (NPRO) DELETED'
      CALL SHOWIT(1)
   END IF
   call ldm%clearCurvAndConic(SURF)
   call ldm%clearAsphericCoeffsAndFlag(SURF)
   call ldm%clearHigherOrderAsphericCoeffs(SURF)
   call ldm%clearToricFlagAndCurv(SURF)
   call set_surf_asi_flag(SURF, 0)
   call ldm%clearAnamorphicData(SURF)
   call set_surf_asphere_coeff(SURF, 2, 0.0D0)
!               INCREMENT THEPIKUP COUNTER BY 1.0D0
!               I.E. surf_special_type(SURF)
   call set_surf_special_type(SURF, surf_special_type(SURF)+1)
   IF(WQ.EQ.'PRO') THEN
      PIKUP(1,SURF,11)=1.0D0
      PIKUP(2,SURF,11)=W1
      PIKUP(3,SURF,11)=W2
      PIKUP(4,SURF,11)=W3
      PIKUP(5,SURF,11)=W4
      PIKUP(6,SURF,11)=W5
   ELSE
   END IF
   IF(WQ.EQ.'NPRO') THEN
      PIKUP(1,SURF,12)=1.0D0
      PIKUP(2,SURF,12)=W1
      PIKUP(3,SURF,12)=W2
      PIKUP(4,SURF,12)=W3
      PIKUP(5,SURF,12)=W4
      PIKUP(6,SURF,12)=W5
   ELSE
   END IF
!
!       NOW ALL OLD SOLVES AND CURVATURE DATA IS GONE
   IF(WQ.EQ.'PRO') THEN
   ELSE
   END IF
   IF(WQ.EQ.'NPRO') THEN
   ELSE
   END IF
   RETURN
END
! SUB PIKCOAT.FOR
SUBROUTINE PIKCOAT
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_last_surf
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   INTEGER I
!
!       THIS IS SUBROUTINE PIKCOAT. THIS IS THE SUBROUTINE WHICH
!       HANDLES SURFACE COATING PIKUPS AT THE LENS INPUT
!       AND UPDATE LENS LEVEL.
!
!
!       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
!       SPECIFIC STORAGE LOCATIONS FOR LENS DATA,SOLVE AND
!       PIKUP ITEMS.
!
!       TEST TO SEE IF THE SURFACE FROM WHICH WE ARE PIKING UP
!       DATA IS THE SAME AS THE SURFACE TO WHICH THE PIKUP
!       WILL BE ASSINGED. OPERATION NOT LEGAL
!
   IF(SURF.EQ.INT(W1)) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'A SURFACE CAN NOT PIKUP ITS OWN VALUES'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!
!       ONLY NUMERIC WORD 1 AND 5 ARE VALID FOR A GLASS COATING
!
   IF(DF2.EQ.0.OR.DF3.EQ.0.OR.DF4.EQ.0) THEN
      OUTLYNE='"PIKUP COATING" ONLY USES NUMERIC WORD #1 AND #5 DATA'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      RETURN
   END IF
!       SINCE THIS IS A COATING PIKUP,
!       THE THIRD DIMENSION WILL BE 44
!
!       THE PIKUP EXISTENCE FLAG IS PIKUP(1,I,J) FOR EVERY
!       PIKUP ON EVERY SURFACE. IF IT IS 1.0D0 THE PIKUP EXISTS
!       IF IT IS 0.0D0 THE PIKUP DOES NOT EXIST
!
!       STORE NUMERIC INPUT AND HANDEL DEFAULT DATA

!       IF DF1=1 THIS MEANS PIKING UP FROM THE OBJECT SURFACE
!       ON TO THE CURRENT SURFACE
   IF(DF1.EQ.1) W1=0.0D0
   W2=0.0D0
   W3=0.0D0
   W4=0.0D0
   IF(DF5.EQ.1) W5=0.0D0
   IF(W1.LT.0.0D0) W1=SURF+W1
   IF(W1.LT.0.0D0.OR.W1.GT.sys_last_surf()) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'SOURCE SURFACE NUMBER BEYOND LEGAL RANGE'//'\n'//&
      & 'PIKUP NOT ALLOWED', 1)
      RETURN
   END IF
!
!               INCREMENT THE PIKUP COUNTER BY 1.0D0
!               I.E. surf_special_type(SURF)
   call set_surf_special_type(SURF, surf_special_type(SURF)+1)
   PIKUP(1,SURF,44)=1.0D0
   PIKUP(2,SURF,44)=W1
   PIKUP(3,SURF,44)=W2
   PIKUP(4,SURF,44)=W3
   PIKUP(5,SURF,44)=W4
   PIKUP(6,SURF,44)=W5
   F22=1
!       THERE ARE NO SOLVES FOR GLASS TYPE
!       SO THERE ARE NO SOLVE DELETIONS TO HANDEL.
!
   RETURN
END
! SUB PIKGLS.FOR
SUBROUTINE PIKGLS
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_last_surf, sys_set_last_surf
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   INTEGER I
!
!       THIS IS SUBROUTINE PIKGLS. THIS IS THE SUBROUTINE WHICH
!       HANDLES MATERIAL AND INDEX PIKUPS AT THE LENS INPUT
!       AND UPDATE LENS LEVEL.
!
!
!       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
!       SPECIFIC STORAGE LOCATIONS FOR LENS DATA,SOLVE AND
!       PIKUP ITEMS.
!
!       TEST TO SEE IF THE SURFACE FROM WHICH WE ARE PIKING UP
!       DATA IS THE SAME AS THE SURFACE TO WHICH THE PIKUP
!       WILL BE ASSINGED. OPERATION NOT LEGAL
!
!       PIKUP GLASS IS A SURFACE TERMINAL COMMAND IN THE LENS INPUT
!       ROUTINE F5=1. IT CAUSES THE SURFACE COUNTING VARIABLE
!       (SURF) AND sys_last_surf() TO BE INCREMENTED BY 1 AND 1.0D0
!       AND CAUSES THE NEXT SURFACE DATA TO BE STORED FOR THE NEXT
!       SURFACE. AT F6=1 (UPDATE ROUTINE) THIS INCREMENTING DOES
!       NO OCCUR. THE LAST SURFACE FOR WHICH DATA MAY BE ENTERED
!       IS SURFACE MAXSUR. SURF AND sys_last_surf() ARE NOT INCREMENTED
!       PASSED MAXSUR SO IF AN ATTEMPT IS MADE TO INPUT PASSED
!       SURFACE MAXSUR IN THE LENS INPUT MODE, THE DATA FOR SURFACE
!       MAXSUR WILL BE OVERWRITTEN.
!
   IF(SURF.EQ.INT(W1)) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'A SURFACE CAN NOT PIKUP ITS OWN VALUES'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!
!       ONLY NUMERIC WORD 1 AND 5 ARE VALID FOR A GLASS PIKUP
!
   IF(DF2.EQ.0.OR.DF3.EQ.0.OR.DF4.EQ.0) THEN
      OUTLYNE='"PIKUP GLASS" ONLY USES NUMERIC WORD #1 AND #5 DATA'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      RETURN
   END IF
!       SINCE THIS IS A GLASSPIKUP,
!       THE THIRD DIMENSION WILL BE 20
!
!       THE PIKUP EXISTENCE FLAG IS PIKUP(1,I,J) FOR EVERY
!       PIKUP ON EVERY SURFACE. IF IT IS 1.0D0 THE PIKUP EXISTS
!       IF IT IS 0.0D0 THE PIKUP DOES NOT EXIST
!
!       STORE NUMERIC INPUT AND HANDEL DEFAULT DATA

!       IF DF1=1 THIS MEANS PIKING UP FROM THE OBJECT SURFACE
!       ON TO THE CURRENT SURFACE
   IF(DF1.EQ.1) W1=0.0D0
   W2=0.0D0
   W3=0.0D0
   W4=0.0D0
   IF(DF5.EQ.1) W5=0.0D0
   IF(W1.LT.0.0D0) W1=SURF+W1
   IF(W1.LT.0.0D0.OR.W1.GT.sys_last_surf()) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'SOURCE SURFACE NUMBER BEYOND LEGAL RANGE'//'\n'//&
      & 'PIKUP NOT ALLOWED', 1)
      RETURN
   END IF
!
!               INCREMENT THE PIKUP COUNTER BY 1.0D0
!               I.E. surf_special_type(SURF)
   call set_surf_special_type(SURF, surf_special_type(SURF)+1)
   PIKUP(1,SURF,20)=1.0D0
   PIKUP(2,SURF,20)=W1
   PIKUP(3,SURF,20)=W2
   PIKUP(4,SURF,20)=W3
   PIKUP(5,SURF,20)=W4
   PIKUP(6,SURF,20)=W5
   F22=1
   IF(F5.EQ.1.AND.SURF.LT.MAXSUR) THEN
      SURF=SURF+1
      ALENS(1:LSIZ,SURF)=0.0D0
      call sys_set_last_surf(DBLE(SURF))
   ELSE
!       DON'T INCREMENT SURF AND sys_last_surf()
   END IF
!       THERE ARE NO SOLVES FOR GLASS TYPE
!       SO THERE ARE NO SOLVE DELETIONS TO HANDEL.
!
!
   RETURN
END
! SUB PIKGRT.FOR
SUBROUTINE PIKGRT
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_last_surf
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   INTEGER I
!
!       THIS IS SUBROUTINE PIKGRT. THIS IS THE SUBROUTINE WHICH
!       HANDLES GRT PIKUPS AT THE LENS INPUT
!       AND UPDATE LENS LEVEL.
!
!
!       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
!       SPECIFIC STORAGE LOCATIONS FOR LENS DATA,SOLVE AND
!       PIKUP ITEMS.
!
!       TEST TO SEE IF THE SURFACE FROM WHICH WE ARE PIKING UP
!       DATA IS THE SAME AS THE SURFACE TO WHICH THE PIKUP
!       WILL BE ASSINGED. OPERATION NOT LEGAL
!
!
   IF(SURF.EQ.INT(W1)) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'A SURFACE CAN NOT PIKUP ITS OWN VALUES'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!
!       ONLY NUMERIC WORD 1 AND 5 ARE VALID FOR A GRT PIKUP
!
   IF(DF2.EQ.0.OR.DF3.EQ.0.OR.DF4.EQ.0) THEN
      OUTLYNE='"PIKUP GRT" ONLY USES NUMERIC WORD #1 AND #5 DATA'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      RETURN
   END IF
!       SINCE THIS IS A GRT PIKUP,
!       THE THIRD DIMENSION WILL BE 43
!
!       THE PIKUP EXISTENCE FLAG IS PIKUP(1,I,J) FOR EVERY
!       PIKUP ON EVERY SURFACE. IF IT IS 1.0D0 THE PIKUP EXISTS
!       IF IT IS 0.0D0 THE PIKUP DOES NOT EXIST
!
!       STORE NUMERIC INPUT AND HANDEL DEFAULT DATA

!       IF DF1=1 THIS MEANS PIKING UP FROM THE OBJECT SURFACE
!       ON TO THE CURRENT SURFACE
   IF(DF1.EQ.1) W1=0.0D0
   W2=0.0D0
   W3=0.0D0
   W4=0.0D0
   IF(DF5.EQ.1) W5=0.0D0
   IF(W1.LT.0.0D0) W1=SURF+W1
   IF(W1.LT.0.0D0.OR.W1.GT.sys_last_surf()) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'SOURCE SURFACE NUMBER BEYOND LEGAL RANGE'//'\n'//&
      & 'PIKUP NOT ALLOWED', 1)
      RETURN
   END IF
!
!               INCREMENT THE PIKUP COUNTER BY 1.0D0
!               I.E. surf_special_type(SURF)
   call set_surf_special_type(SURF, surf_special_type(SURF)+1)
   PIKUP(1,SURF,43)=1.0D0
   PIKUP(2,SURF,43)=W1
   PIKUP(3,SURF,43)=0.0D0
   PIKUP(4,SURF,43)=0.0D0
   PIKUP(5,SURF,43)=0.0D0
   PIKUP(6,SURF,43)=W5
!
   RETURN
END
! SUB PIKCVT.FOR
SUBROUTINE PIKCVT
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_last_surf
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PIKCVT. THIS IS THE SUBROUTINE WHICH
!       HANDLES THE TORIC CURVATURE (CVTOR) AND TORIC RADIUS (RDTOR)
!       PIKUP AT THE LENS INPUT
!       AND UPDATE LENS LEVEL.
!
   INTEGER FSURF
!
!
!       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
!       SPECIFIC STORAGE LOCATIONS FOR LENS DATA,SOLVE AND
!       PIKUP ITEMS.
!
!       TEST TO SEE IF THE SURFACE FROM WHICH WE ARE PIKING UP
!       DATA IS THE SAME AS THE SURFACE TO WHICH THE PIKUP
!       WILL BE ASSINGED. OPERATION NOT LEGAL
!
   IF(SURF.EQ.INT(W1)) THEN

      CALL REPORT_ERROR_AND_FAIL(&
      & 'A SURFACE CAN NOT PIKUP ITS OWN VALUES'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!
!
!       THE PIKUP EXISTENCE FLAG IS PIKUP(1,I,J) FOR EVERY
!       PIKUP ON EVERY SURFACE. IF IT IS 1.0D0 THE PIKUP EXISTS
!       IF IT IS 0.0D0 THE PIKUP DOES NOT EXIST
!
!       STORE NUMERIC INPUT AND HANDEL DEFAULT DATA

!       IF DF1=1 THIS MEANS PIKING UP FROM THE OBJECT SURFACE
!       ON TO THE CURRENT SURFACE
   IF(DF1.EQ.1) W1=0.0D0
!       IF DF2=1 THIS MEANS MULTIPLIER IS UNITY
   IF(DF2.EQ.1) W2=1.0D0
!       IF DF3=1 TIS MEANS ADDITIVE CONSTANT ZERO
   IF(DF3.EQ.1) W3=0.0D0
!       IF DF4=1 THEN W4=0.0D0
!       IF DF5=1 THEN W5=0.0D0
   IF(DF4.EQ.1) W4=0.0D0
   IF(DF5.EQ.1) W5=0.0D0
   IF(W1.LT.0.0D0) W1=SURF+W1
   IF(W1.LT.0.0D0.OR.W1.GT.sys_last_surf()) THEN

      CALL REPORT_ERROR_AND_FAIL(&
      & 'SOURCE SURFACE NUMBER BEYOND LEGAL RANGE'//'\n'//&
      & 'PIKUP NOT ALLOWED', 1)
      RETURN
   END IF
!
!       IF THE SURFACE BEING PIKED UP FROM IS NOT A TORIC
!       THEN THE PIKUP IS NOT ALLOWED. CHECK STATUS OF
!       surf_toric_flag() FOR THAT SURFACE.
!
   FSURF=INT(W1)
   IF(surf_toric_flag(FSURF).EQ.0.0D0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'SURFACE TO BE PIKED UP FROM IS NOT DEFINED AS TORIC'//'\n'//&
      & 'TORIC PIKUP IS NOT ALLOWED', 1)
      RETURN
   END IF
!       IF PIKING SURFACE IS NOT A TORIC, THE CVTOR/RDTOR PIKUP IS
!       NOT ALLOWED.
   IF(surf_toric_flag(SURF).NE.1.0D0.AND.surf_toric_flag(SURF).NE.2.0D0) THEN
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :NOT DEFINED AS TORIC'
      CALL SHOWIT(1)
      CALL REPORT_ERROR_AND_FAIL('('//WQ(1:5)//') PIKUP NOT ALLOWED ON NON-TORIC SURFACE', 1)
      RETURN
   END IF
!
!               INCR. PIKUP INDICATOR BY 1.0D0
!               I.E. surf_special_type(SURF)
   IF(WQ.EQ.'CVTOR') THEN
      IF(PIKUP(1,SURF,10).EQ.0.0D0)call set_surf_special_type(SURF, surf_special_type(SURF)+1)
      PIKUP(1,SURF,10)=0.0D0
      PIKUP(1,SURF,9)=1.0D0
      PIKUP(2,SURF,9)=W1
      PIKUP(3,SURF,9)=W2
      PIKUP(4,SURF,9)=W3
      PIKUP(5,SURF,9)=W4
      PIKUP(6,SURF,9)=W5
   END IF
   IF(WQ.EQ.'RDTOR') THEN
      IF(PIKUP(1,SURF,9).EQ.0.0D0)call set_surf_special_type(SURF, surf_special_type(SURF)+1)
      PIKUP(1,SURF,9)=0.0D0
      PIKUP(1,SURF,10)=1.0D0
      PIKUP(2,SURF,10)=W1
      PIKUP(3,SURF,10)=W2
      PIKUP(4,SURF,10)=W3
      PIKUP(5,SURF,10)=W4
      PIKUP(6,SURF,10)=W5
   END IF
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
!       IF PIKING SURFACE IS NOT A TORIC, IT WILL BE REDEFINED AS ONE
!       DURING THE RECONCILLIATION AT (EOS) WHEN ALL PIKUPS ARE
!       RECONCILLED.
!
!       TORIC INDICATOR SET CORRECTLY NOW
!
!       THERE MAY BE SOLVES TO DELETE. SINCE THIS
!       SURFACE IS NOW A TORIC (Y OR X) WITH PIKED UP DATA
!       THEN IF IT HAD A CURVATURE SOLVE ON IT, THAT CURVATURE
!       SOLVE MAY NEED TO BE REMOVED. THERE ARE DIFFERENT
!       COMBINATIONS OF TORIC TYPES, PIKUPS AND SOLVE TO
!       CONSIDER.
!
!       FIRST, IF THE TORIC TYPE ON SURFACE SURF IS A
!               Y-TORIC. FOR A Y-TORIC
!       PIKING UP THR CVTOR EFFECTS THE TORIC CURVATURE OF
!       REVOLUTION IN THE XZ PLANE SINCE THE REVOLUTION
!       AXIS IS IN THE Y DIRECTION. THUS THIS PIKUP
!       WOULD ONLY DELETE A CURVATURE SOLVE IN THE XZ PLANE
!
!       SECOND, IF THE TORIC TYPE ON SURFACE SURF IS AN
!               X-TORIC. FOR AN X-TORIC
!       PIKING UP THE CVTOR EFFECTS THE TORIC CURVATURE OF
!       REVOLUTION IN THE YZ PLANE SINCE THE REVOLUTION
!       AXIS IS IN THE X DIRECTION. THUS THIS PIKUP
!       WOULD ONLY DELETE A CURVATURE SOLVE IN THE YZ PLANE
!
   IF(surf_toric_flag(SURF).EQ.1.0D0) THEN
!       SURF IS Y-TORIC, REMOVE ANY XZ SOLVES
      IF(SOLVE(2,SURF).GT.0.0D0) THEN
         SOLVE(2,SURF)=0.0D0
         SOLVE(1,SURF)=0.0D0
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :XZ PLANE CURVATURE SOLVES DELETED'
         CALL SHOWIT(1)
      END IF
   END IF
   IF(surf_toric_flag(SURF).EQ.2.0D0) THEN
!       SURF IS X-TORIC, REMOVE ANY YZ SOLVES
      IF(SOLVE(8,SURF).GT.0.0D0) THEN
         SOLVE(8,SURF)=0.0D0
         SOLVE(9,SURF)=0.0D0
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :YZ PLANE CURVATURE SOLVES DELETE'
         CALL SHOWIT(1)
      END IF
   END IF
!       RESOLVE surf_solve_flag(SURF)
   call set_surf_solve_flag(SURF, 0.0D0)
   IF(SOLVE(6,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+1.0D0)
   IF(SOLVE(4,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+0.1D0)
   IF(SOLVE(8,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+2.0D0)
   IF(SOLVE(2,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+0.2D0)
!
!       SOLVE CONFLICTS RESOLVED
!
   RETURN
END
