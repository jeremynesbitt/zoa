!       SECOND FILE OF PIKUP ROUTINES

! SUB PIKCV.FOR
SUBROUTINE PIKCV
!
   use DATLEN
   use mod_surface
   use mod_system, only: sys_last_surf
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PIKCV. THIS IS THE SUBROUTINE WHICH
!       HANDLES RADIUS AND CURVATURE PIKUPS AT THE LENS INPUT
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
!
!       SINCE THIS IS A RADIUS OR CURVATURE PIKUP,
!       THE THIRD DIMENSION WILL
!       BE 1 OR 2 RESPECTIVELY
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
   IF(WQ.EQ.'RD') THEN
!               INCREMENT THE PIKUP COUNTER
!               I.E. surf_special_type(SURF)
      IF(PIKUP(1,SURF,2).EQ.0.0D0)call set_surf_special_type(SURF, surf_special_type(SURF)+1)
      PIKUP(1,SURF,1)=1.0D0
      PIKUP(1,SURF,2)=0.0D0
      PIKUP(2,SURF,1)=W1
      PIKUP(3,SURF,1)=W2
      PIKUP(4,SURF,1)=W3
      PIKUP(5,SURF,1)=W4
      PIKUP(6,SURF,1)=W5
   ELSE
      IF(WQ.EQ.'CV') THEN
!               INCREMENT THE PIKUP COUNTER
!               I.E. surf_special_type(SURF)
         IF(PIKUP(1,SURF,1).EQ.0.0D0)call set_surf_special_type(SURF, surf_special_type(SURF)+1)
         call set_surf_special_type(SURF, surf_special_type(SURF)+1)
         PIKUP(1,SURF,1)=0.0D0
         PIKUP(1,SURF,2)=1.0D0
         PIKUP(2,SURF,2)=W1
         PIKUP(3,SURF,2)=W2
         PIKUP(4,SURF,2)=W3
         PIKUP(5,SURF,2)=W4
         PIKUP(6,SURF,2)=W5
      ELSE
      END IF
   END IF
!       DUMP PIKUP PRO AND NPRO IF FOUND
   IF(PIKUP(1,SURF,11).GT.0.0D0) THEN
      PIKUP(1,SURF,11)=0.0D0
      PIKUP(2,SURF,11)=0.0D0
      PIKUP(3,SURF,11)=0.0D0
      PIKUP(4,SURF,11)=0.0D0
      PIKUP(5,SURF,11)=0.0D0
      PIKUP(6,SURF,11)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (PRO) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,12).GT.0.0D0) THEN
      PIKUP(1,SURF,12)=0.0D0
      PIKUP(2,SURF,12)=0.0D0
      PIKUP(3,SURF,12)=0.0D0
      PIKUP(4,SURF,12)=0.0D0
      PIKUP(5,SURF,12)=0.0D0
      PIKUP(6,SURF,12)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (NPRO) DELETED'
      CALL SHOWIT(1)
   END IF
!
!       IF THE SURFACE IS NON-TORIC OR IF IT IS A YTORIC
!       THEN DELETE YZ PLANE SOLVES. DON'T DELETE THEM IF SURFACE
!       IS AN XTORIC
   IF(surf_toric_flag(SURF).EQ.0.0D0.OR.surf_toric_flag(SURF).EQ.1.0D0) THEN
      IF(SOLVE(8,SURF).GT.0.0D0) THEN
         SOLVE(8,SURF)=0.0D0
         SOLVE(9,SURF)=0.0D0
!       RECALCULATE surf_solve_flag(SURF)
         call set_surf_solve_flag(SURF, 0.0D0)
         IF(SOLVE(6,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+1.0D0)
         IF(SOLVE(4,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+0.1D0)
         IF(SOLVE(8,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+2.0D0)
         IF(SOLVE(2,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+0.2D0)
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :YZ PLANE CURVATURE SOLVE DELETED'
         CALL SHOWIT(1)
      END IF
   END IF
!       IF SURFACE IS X-TORIC OR NON-TORIC, DELETE XZ PLANE
!       CURVATURE SOLVES
   IF(surf_toric_flag(SURF).EQ.0.0D0.OR.surf_toric_flag(SURF).EQ.2.0D0) THEN
      IF(SOLVE(2,SURF).GT.0.0D0) THEN
         SOLVE(2,SURF)=0.0D0
         SOLVE(1,SURF)=0.0D0
!       RECALCULATE surf_solve_flag(SURF)
         call set_surf_solve_flag(SURF, 0.0D0)
         IF(SOLVE(6,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+1.0D0)
         IF(SOLVE(4,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+0.1D0)
         IF(SOLVE(8,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+2.0D0)
         IF(SOLVE(2,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+0.2D0)
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :XZ PLANE CURVATURE SOLVE DELETED'
         CALL SHOWIT(1)
      END IF
   END IF
   RETURN
END
! SUB PIKCON.FOR
SUBROUTINE PIKCON
!
   use DATLEN
   use mod_surface
   use mod_system, only: sys_last_surf
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PIKCON. THIS IS THE SUBROUTINE WHICH
!       HANDLES CONIC AND TORIC CONIC CONSTANT PIKUPS AT THE LENS INPUT
!       AND UPDATE LENS LEVEL.
!
   INTEGER CT
!
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
   IF(WQ.EQ.'CC') CT=4
   IF(WQ.EQ.'CCTOR') CT=21
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
!
   IF(W1.LT.0.0D0) W1=SURF+W1
   IF(W1.LT.0.0D0.OR.W1.GT.sys_last_surf()) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'SOURCE SURFACE NUMBER BEYOND LEGAL RANGE'//'\n'//&
      & 'PIKUP NOT ALLOWED', 1)
      RETURN
   END IF
!               INCREMENT THEPIKUP COUNTER BY 1.0D0
!               I.E. surf_special_type(SURF)
   call set_surf_special_type(SURF, surf_special_type(SURF)+1)
   PIKUP(1,SURF,CT)=1.0D0
   PIKUP(2,SURF,CT)=W1
   PIKUP(3,SURF,CT)=W2
   PIKUP(4,SURF,CT)=W3
   PIKUP(5,SURF,CT)=W4
   PIKUP(6,SURF,CT)=W5
!       DUMP PIKUP PRO AND NPRO IF FOUND
   IF(PIKUP(1,SURF,11).GT.0.0D0) THEN
      PIKUP(1,SURF,11)=0.0D0
      PIKUP(2,SURF,11)=0.0D0
      PIKUP(3,SURF,11)=0.0D0
      PIKUP(4,SURF,11)=0.0D0
      PIKUP(5,SURF,11)=0.0D0
      PIKUP(6,SURF,11)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (PRO) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,12).GT.0.0D0) THEN
      PIKUP(1,SURF,12)=0.0D0
      PIKUP(2,SURF,12)=0.0D0
      PIKUP(3,SURF,12)=0.0D0
      PIKUP(4,SURF,12)=0.0D0
      PIKUP(5,SURF,12)=0.0D0
      PIKUP(6,SURF,12)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (NPRO) DELETED'
      CALL SHOWIT(1)
   END IF
!
!       THERE ARE NO SOLVES FOR CONIC CONSTANT
!       SO THERE ARE NO SOLVE DELETIONS TO HANDEL.
!
   RETURN
END
! SUB PIKASP.FOR
SUBROUTINE PIKASP
!
   use DATLEN
   use mod_surface
   use mod_system, only: sys_last_surf
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PIKASP. THIS IS THE SUBROUTINE WHICH
!       HANDLES ASPHERIC OR ASPHERIC TORIC
!       TERM PIKUPS AT THE LENS INPUT
!       AND UPDATE LENS LEVEL. IF THE SURFACE TO WHICH THE PIKUP
!       IS GOING IS NOT DEFINED AS ASPHERIC OR ASPHERIC TORIC
!       , IT IS SET AS SUCH
!       AUTOMATICALLY AND A MESSAGE IS PRINTED.
!
   INTEGER FSURF,CT
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
   IF(WQ.EQ.'AC') CT=26
   IF(WQ.EQ.'AD') CT=5
   IF(WQ.EQ.'AE') CT=6
   IF(WQ.EQ.'AF') CT=7
   IF(WQ.EQ.'AG') CT=8
   IF(WQ.EQ.'AH') CT=27
   IF(WQ.EQ.'AI') CT=28
   IF(WQ.EQ.'AJ') CT=29
   IF(WQ.EQ.'AK') CT=30
   IF(WQ.EQ.'AL') CT=31
   IF(WQ.EQ.'ADTOR') CT=22
   IF(WQ.EQ.'AETOR') CT=23
   IF(WQ.EQ.'AFTOR') CT=24
   IF(WQ.EQ.'AGTOR') CT=25
!       FOR AC THIRD TERM IS 26
!       FOR AD THIRD TERM IS 5
!       FOR AE THIRD TERM IS 6
!       FOR AF THIRD TERM IS 7
!       FOR AG THIRD TERM IS 8
!       FOR AH THIRD TERM IS 27
!       FOR AI THIRD TERM IS 28
!       FOR AJ THIRD TERM IS 29
!       FOR AK THIRD TERM IS 30
!       FOR AL THIRD TERM IS 31
!       FOR ADTOR THIRD TERM IS 22
!       FOR AETOR THIRD TERM IS 23
!       FOR AFTOR THIRD TERM IS 24
!       FOR AGTOR THIRD TERM IS 25
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
!
   IF(W1.LT.0.0D0.OR.W1.GT.sys_last_surf()) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'SOURCE SURFACE NUMBER BEYOND LEGAL RANGE'//'\n'//&
      & 'PIKUP NOT ALLOWED', 1)
      RETURN
   END IF
!
!       THE SURFACE BEING PIKED UP FROM MUST BE ASPHERIC OR THE
!       PIKUP IS NOT ALLOWED.
!
   FSURF=INT(W1)
   IF(WQ.EQ.'AC'.OR.WQ.EQ.'AF'.OR.WQ.EQ.'AD'.OR.WQ.EQ.'AH'.OR.WQ.EQ.'AI'.OR.WQ.EQ.'AJ'.OR.WQ.EQ.'AK'.OR.WQ.EQ.'AL'.OR.WQ.EQ.'AE'.OR.WQ.EQ.'AG')THEN
      IF(.NOT.surf_is_asphere(FSURF)) THEN
         WRITE(OUTLYNE,*)'SURFACE TO BE PIKED UP FROM IS NOT DEFINED AS ASPHERIC'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('PIKUP ('//WQ(1:2)//') NOT ALLOWED', 1)
         RETURN
      END IF
   END IF
   IF(WQ.EQ.'ADTOR'.OR.WQ.EQ.'AFTOR'.OR.WC.EQ.'AETOR'.OR.WQ.EQ.'AGTOR') THEN
      IF(surf_anamorphic_flag(FSURF).EQ.0D0) THEN
         WRITE(OUTLYNE,*)'SURFACE TO BE PIKED UP FROM IS NOT DEFINED AS ASPHERIC TORIC'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('PIKUP ('//WQ(1:5)//') NOT ALLOWED', 1)
         RETURN
      ELSE
      END IF
   ELSE
   END IF
!
!               INCR.PIKUP INDICATOR BY 1.0D0
!               I.E. surf_special_type(SURF)
   call set_surf_special_type(SURF, surf_special_type(SURF)+1)
   PIKUP(1,SURF,CT)=1.0D0
   PIKUP(2,SURF,CT)=W1
   PIKUP(3,SURF,CT)=W2
   PIKUP(4,SURF,CT)=W3
   PIKUP(5,SURF,CT)=W4
   PIKUP(6,SURF,CT)=W5
!       DUMP PIKUP PRO AND NPRO IF FOUND
   IF(PIKUP(1,SURF,11).GT.0.0D0) THEN
      PIKUP(1,SURF,11)=0.0D0
      PIKUP(2,SURF,11)=0.0D0
      PIKUP(3,SURF,11)=0.0D0
      PIKUP(4,SURF,11)=0.0D0
      PIKUP(5,SURF,11)=0.0D0
      PIKUP(6,SURF,11)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (PRO) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,12).GT.0.0D0) THEN
      PIKUP(1,SURF,12)=0.0D0
      PIKUP(2,SURF,12)=0.0D0
      PIKUP(3,SURF,12)=0.0D0
      PIKUP(4,SURF,12)=0.0D0
      PIKUP(5,SURF,12)=0.0D0
      PIKUP(6,SURF,12)=0.0D0
      call set_surf_special_type(SURF, surf_special_type(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (NPRO) DELETED'
      CALL SHOWIT(1)
   END IF
!
!
   IF(WQ.EQ.'AD'.OR.WQ.EQ.'AE'.OR.WQ.EQ.'AF'.OR.WQ.EQ.'AH'.OR.WQ.EQ.'AI'.OR.WQ.EQ.'AJ'.OR.WQ.EQ.'AJ'.OR.WQ.EQ.'AL'.OR.WQ.EQ.'AG'.OR.WQ.EQ.'AC')THEN
!       IF ALENS(8,SURF)=0.0D0 THEN THE SURFACE WAS NOT AN ASPHERE
!       MAKE IT ONE AND PRINT MESSAGE ELSE DO NOTHING
!
      IF(.NOT.surf_is_asphere(SURF)) THEN
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :RE-DEFINED AS ASPHERIC'
         CALL SHOWIT(1)
         call set_surf_asphere_flag(SURF, .TRUE.)
      ELSE
      END IF
   ELSE
   END IF
!
   IF(WQ.EQ.'ADTOR'.OR.WQ.EQ.'AETOR'.OR.WQ.EQ.'AFTOR'.OR.WQ.EQ.'AGTOR')THEN
!       IF surf_anamorphic_flag(SURF)=0.0D0 THEN THE SURFACE WAS NOT AN ASPHERE
!       TORIC, MAKE IT ONE AND PRINT MESSAGE ELSE DO NOTHING
!
      IF(surf_anamorphic_flag(SURF).EQ.0.0D0) THEN
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :RE-DEFINED AS ASPHERIC TORIC'
         CALL SHOWIT(1)
         call set_surf_anamorphic_flag(SURF, 1)
      END IF
   END IF
!
!       THERE ARE NO SOLVE DELETIONS TO HANDEL.
!
   RETURN
END
! SUB PIKAPE.FOR
SUBROUTINE PIKAPE
!
   use DATLEN
   use mod_surface
   use mod_system, only: sys_last_surf
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PIKAPE. THIS IS THE SUBROUTINE WHICH
!       HANDLES CLEAR APERTURE (CLAP) PIKUP AND OBSCURATION PIKUPS
!       AT THE LENS INPUT
!       AND UPDATE LENS LEVEL.
!
   INTEGER CT,I
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
   IF(WQ.EQ.'CLAP') CT=18
   IF(WQ.EQ.'COBS') CT=19
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
   IF(DF2.NE.1.OR.DF3.NE.1.OR.DF4.NE.1) THEN
      OUTLYNE='"PIKUP (CLAP OR COBS)" ONLY TAKE NUMERIC WORD #1 AND #5 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      RETURN
   END IF
   IF(W1.LT.0.0D0) W1=SURF+W1
   IF(W1.LT.0.0D0.OR.W1.GT.sys_last_surf()) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'SOURCE SURFACE NUMBER BEYOND LEGAL RANGE'//'\n'//&
      & 'PIKUP NOT ALLOWED', 1)
      RETURN
   END IF
!
!       CHECK THAT THE SURFACE BEING PIKED UP
!       FROM HAS A CLEAR APERTURE ASSIGNED. IF NOT
!       PRINT MESSAGE AND DON'T ASSIGN PIKUP
!
   IF(WQ.EQ.'CLAP'.AND.surf_clap_type(INT(W1)).EQ.0.0D0) THEN
      OUTLYNE='SOURCE SURFACE OF (CLAP) PIKUP'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'(SURFACE',INT(W1),') HAS NO (CLAP)'
      CALL SHOWIT(1)
      CALL REPORT_ERROR_AND_FAIL('ASSIGNED :  PIKUP (CLAP) NOT ALLOWED', 1)
      RETURN
   END IF
   IF(WQ.EQ.'COBS'.AND.surf_coat_type(INT(W1)).EQ.0.0D0) THEN
      OUTLYNE='SOURCE SURFACE OF (COBS) PIKUP'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'(SURFACE',INT(W1),') HAS NO (COBS)'
      CALL SHOWIT(1)
      CALL REPORT_ERROR_AND_FAIL('ASSIGNED :  PIKUP (COBS) NOT ALLOWED', 1)
      RETURN
   END IF
!
!               INCR PIKUP INDICATOR BY 1.0D0
!               I.E. surf_special_type(SURF)
   call set_surf_special_type(SURF, surf_special_type(SURF)+1)
   PIKUP(1,SURF,CT)=1.0D0
   PIKUP(2,SURF,CT)=W1
   PIKUP(3,SURF,CT)=0.0D0
   PIKUP(4,SURF,CT)=0.0D0
   PIKUP(5,SURF,CT)=0.0D0
   PIKUP(6,SURF,CT)=W5
!
!       NOW SET THE CLAP AND COBS FLAG CORRECTLY
!       IF IT IS SET ON THE SURFACE BEING PIKED UP FROM
!       IT SHOULD BE SET ON THE TARGET SURFACE
!       THIS IMMEDIATELLY SETS THE CLAP TYPE
!       ON THE PIKING SURFACE.
   IF(surf_clap_type(INT(W1)).NE.0.0D0) THEN
      call set_surf_clap_type(SURF, surf_clap_type(INT(W1)))
      call set_surf_cobs_ape_type(SURF, surf_cobs_ape_type(INT(W1)))
   ELSE
      call set_surf_clap_type(SURF, 0)
      call set_surf_cobs_ape_type(SURF, 0)
   END IF
   IF(surf_multi_clap_flag(INT(W1)).NE.0.0D0) THEN
      call set_surf_multi_clap_flag(SURF, surf_multi_clap_flag(INT(W1)))
      I=INT(surf_multi_clap_flag(INT(W1)))
      MULTCLAP(1:I,1:2,SURF)=MULTCLAP(1:I,1:2,INT(W1))
      MULTCOBS(1:I,1:2,SURF)=MULTCOBS(1:I,1:2,INT(W1))
   END IF
   IF(surf_coat_type(INT(W1)).NE.0.0D0) THEN
      call set_surf_coat_type(SURF, surf_coat_type(INT(W1)))
      call set_surf_cobs_era_type(SURF, surf_cobs_era_type(INT(W1)))
   ELSE
      call set_surf_coat_type(SURF, 0)
      call set_surf_cobs_era_type(SURF, 0)
   END IF
!
!       THERE ARE NO SOLVE DELETIONS TO HANDEL.
!
!
   RETURN
END
! SUB SPIKD.FOR
!
!       THIS IS SUBROUTINE SPIKD. THIS IS THE SUBROUTINE WHICH
!       HANDLES THE LENS PARAMETER PIKUP DELETIONS AT THE
!       UPDATE LENS LEVEL.
!
!       Pilot conversion for refactor #9: takes the parsed command as an
!       explicit argument instead of reading/mutating the DATMAI parse
!       globals.  Callers build the record with capture_command().  The
!       default-filling that used to write W1/W2/S1/S2/DF1/DF2 back into the
!       globals now stays local to the record (nothing downstream consumed
!       those writes).  The 44-line qualifier ladder is replaced by the
!       pickup_manager kind table.
SUBROUTINE SPIKD(cmd)
!
   use DATLEN
   use mod_surface
   use mod_system, only: sys_last_surf
   use DATMAI
   use mod_parsed_command, only: parsed_command
   use pickup_manager, only: pickup_j_from_qual
   IMPLICIT NONE
!
   type(parsed_command), intent(inout) :: cmd
   INTEGER PIKCNT,I,K,SF
!
!       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
!       SPECIFIC STORAGE LOCATIONS FOR LENS DATA,SOLVE AND
!       PIKUP ITEMS.
!
   IF(cmd%sst.EQ.1.OR.cmd%s3.EQ.1.OR.cmd%s4.EQ.1.OR.cmd%s5.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"PIKD" TAKES NO STRING OR NUMERIC WORD #3 THROUGH #5 INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(cmd%df1.EQ.1.AND.cmd%df2.EQ.1) THEN
      cmd%w1=DBLE(SURF)
      cmd%w2=DBLE(SURF)
      cmd%s1=1
      cmd%s2=1
      cmd%df1=0
      cmd%df2=0
   END IF
   IF(cmd%df1.EQ.0.AND.cmd%df2.EQ.1) THEN
      cmd%w2=cmd%w1
      cmd%s1=1
      cmd%s2=1
      cmd%df1=0
      cmd%df2=0
   END IF
   IF(cmd%df1.EQ.1.AND.cmd%df2.EQ.0.OR.cmd%df1.EQ.0.AND.cmd%df2.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"PIKD" USES EITHER TWO OR ZERO NUMERIC WORDS'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(INT(cmd%w1).LT.0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'STARTING SURFACE NUMBER MUST BE GREATER THAN OR EQUAL TO 0'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(INT(cmd%w2).GT.INT(sys_last_surf())) THEN
      WRITE(OUTLYNE,*)'ENDING SURFACE NUMBER MUST BE LESS THAN OR EQUAL TO ',INT(sys_last_surf())
      CALL SHOWIT(1)
      CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(cmd%w1.GT.cmd%w2) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'THE ENDING SURFACE # MUST BE GREATER THAN OR EQUAL TO#'//'\n'//&
      & 'THE STARTING SURFACE #'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   DO SF=INT(cmd%w1),INT(cmd%w2)
!
!       DECIDE WHICH PARAMETER PIKUP IS TO BE DELETED
!       IF SQ=0, ALL PIKUPS MUST GO
      IF(cmd%sq.EQ.0) THEN
         IF(surf_special_type(SF).EQ.0.0D0) THEN
!
!       NO PIKUPS TO DELETE
!
            WRITE(OUTLYNE,*)'SURFACE',SF,' :NO PIKUPS TO BE DELETED'
            CALL SHOWIT(1)
         END IF
         call set_surf_special_type(SF, 0)
         PIKUP(1:6,SF,1:PSIZ)=0.0D0

      ELSE
!       SQ NOT 0 SO THERE WAS A QUALIFIER AND ONLY SOME OF THE
!       PIKUPS MUST GO.  Qualifier -> PIKUP J index via the kind table.
!
         K = pickup_j_from_qual(cmd%wq)
         IF(K.EQ.0) THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & 'INVALID QUALIFIER WORD ISSUED WITH "PIKD"'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
!
         IF(PIKUP(1,SF,K).EQ.0.0D0) THEN
!
!       PIKUP NOT THERE TO BE DELETED
            WRITE(OUTLYNE,*)'SURFACE',SF,' :',TRIM(cmd%wq),' PIKUP TO BE DELETED WAS NOT FOUND'
            CALL SHOWIT(1)
         ELSE
            PIKUP(1:6,SF,K)=0.0D0
            WRITE(OUTLYNE,*)'SURFACE',SF,' ',cmd%wq,' :PIKUP DELETED'
            CALL SHOWIT(1)
!
!       DECREMENT THE PIKUP COUNTER
!
            call set_surf_special_type(SF, surf_special_type(SF)-1)
         END IF
!
!       NOW CHECK IF THERE ARE ANY PIKUPS LEFT.
!       IF NOT,SET surf_special_type(SF) TO 0.0D0
!
         PIKCNT=0
         DO 4 I=1,PSIZ
            IF(PIKUP(1,SF,I).NE.0.0D0) THEN
               PIKCNT=PIKCNT+1
            ELSE
            END IF
4        CONTINUE
         IF(PIKCNT.EQ.0)call set_surf_special_type(SF, 0)
      END IF
!       THATS IT,ALL DONE!
   END DO
!
   RETURN
END
SUBROUTINE LINKIT(LERROR)
   use DATLEN
   use mod_surface
   use mod_system, only: sys_last_surf
   use DATMAI
   use mod_parsed_command, only: parsed_command, capture_command, apply_command
   IMPLICIT NONE
   INTEGER IVAL
   LOGICAL LERROR
   CHARACTER IAVAL*3
   type(parsed_command) :: saved, shifted, cmdRec
      CALL KDP_EXEC('UPDATE LENS')
   IVAL=W1
   IF(IVAL.LT.0.OR.IVAL.GT.sys_last_surf())OUTLYNE='INVALID TARGET SURFACE NUMBER FOR LINKING'
   IF(IVAL.LT.0.OR.IVAL.GT.sys_last_surf())CALL SHOWIT(1)
   IF(IVAL.LT.0.OR.IVAL.GT.sys_last_surf()) THEN
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
   END IF
   LERROR=.TRUE.
   CALL MACFAL
   CALL NTOAN1(IVAL,IAVAL)
   IF(WC.EQ.'LINK') THEN
            CALL KDP_EXEC('CHG '//IAVAL)
   END IF
   IF(WC.EQ.'LINK') THEN
!       "LINK t <qual> a b c" behaves as "PIKUP <qual> a b c" on surface t:
!       build the shifted parse EXPLICITLY instead of mutating the globals
!       word-by-word in place (this was the codebase's last manual
!       W1<-W2/S1<-S2 shift hack; refactor #9).
      saved = capture_command()
      shifted = saved
      shifted%wc  = 'PIKUP'
      shifted%w1  = saved%w2;   shifted%w2  = saved%w3
      shifted%w3  = saved%w4;   shifted%w4  = saved%w5
      shifted%w5  = 0.0D0
      shifted%s1  = saved%s2;   shifted%s2  = saved%s3
      shifted%s3  = saved%s4;   shifted%s4  = saved%s5
      shifted%s5  = 0
      shifted%df1 = saved%df2;  shifted%df2 = saved%df3
      shifted%df3 = saved%df4;  shifted%df4 = saved%df5
      shifted%df5 = 1
      call apply_command(shifted)
      CALL SPIKUP
      call apply_command(saved)
   END IF
   IF(WC.EQ.'LINKD') THEN
      WC='PIKD'
      cmdRec = capture_command()
      CALL SPIKD(cmdRec)
   END IF
      CALL KDP_EXEC('EOS')
   LERROR=.FALSE.
   RETURN
END
! SUB PIKANG.FOR
SUBROUTINE PIKANG
!
   use DATLEN
   use mod_surface
   use mod_system, only: sys_last_surf
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PIKANG. THIS IS THE SUBROUTINE WHICH
!       HANDLES ALPHA,BETA AND GAMMA ANGLE PIKUP AT THE LENS INPUT
!       AND UPDATE LENS LEVEL. IF THE SURFACE TO WHICH THE PIKUP
!       IS GOING IS NOT DEFINED AS TILTED, THE PIKUP IS NOT ALLOWED.
!     AND ALSO GLOBAL TILTS
!
   INTEGER FSURF,CT
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
   IF(WQ.EQ.'ALPHA') CT=15
   IF(WQ.EQ.'BETA') CT=16
   IF(WQ.EQ.'GAMMA') CT=17
   IF(WQ.EQ.'GALPHA') CT=40
   IF(WQ.EQ.'GBETA') CT=41
   IF(WQ.EQ.'GGAMMA') CT=42
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
!       IF surf_tilt_flag(SURF)=0.0D0 THEN THE SURFACE WAS NOT TILTED
!       SINCE THE SURFACE COULD BE ONE OF SEVERAL TILT TYPES
!       THE PIKUP IS NOT ALLOWED TILL THE TILT DEFINITION
!       IS MADE.
!
   IF(surf_tilt_flag(SURF).EQ.0.0D0) THEN
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :SURFACE NOT DEFINED TILTED'
      CALL SHOWIT(1)
      IF(WQ.EQ.'ALPHA') OUTLYNE='PIKUP (ALPHA) NOT ALLOWED'
      IF(WQ.EQ.'BETA')  OUTLYNE='PIKUP (BETA) NOT ALLOWED'
      IF(WQ.EQ.'GAMMA') OUTLYNE='PIKUP (GAMMA) NOT ALLOWED'
      IF(WQ.EQ.'GALPHA')OUTLYNE='PIKUP (GALPHA) NOT ALLOWED'
      IF(WQ.EQ.'GBETA') OUTLYNE='PIKUP (GBETA) NOT ALLOWED'
      IF(WQ.EQ.'GGAMMA')OUTLYNE='PIKUP (GGAMMA) NOT ALLOWED'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!       THE SURFACE BEING PIKED UP FROM MUST
!       BE DEFINED AS TILTED OR THE
!       PIKUP IS NOT ALLOWED.
!
   FSURF=INT(W1)
   IF(surf_tilt_flag(FSURF).EQ.0D0) THEN
!
      IF(WQ.EQ.'ALPHA') THEN
         OUTLYNE='SURFACE TO BE PIKED UP FROM IS NOT DEFINED AS TILTED'
         CALL SHOWIT(1)
         IF(WQ.EQ.'ALPHA ')  OUTLYNE='PIKUP (ALPHA) NOT ALLOWED'
         IF(WQ.EQ.'BETA  ')  OUTLYNE='PIKUP (BETA) NOT ALLOWED'
         IF(WQ.EQ.'GAMMA ')  OUTLYNE='PIKUP (GAMMA) NOT ALLOWED'
         IF(WQ.EQ.'GALPHA')  OUTLYNE='PIKUP (GALPHA) NOT ALLOWED'
         IF(WQ.EQ.'GBETA ')  OUTLYNE='PIKUP (GBETA) NOT ALLOWED'
         IF(WQ.EQ.'GGAMMA')  OUTLYNE='PIKUP (GGAMMA) NOT ALLOWED'
         CALL SHOWIT(1)
      END IF
      CALL MACFAL
      RETURN
   END IF
!
!               INCR.PIKUP INDICATOR BY 1.0D0
!               I.E. surf_special_type(SURF)
   call set_surf_special_type(SURF, surf_special_type(SURF)+1)
   PIKUP(1,SURF,CT)=1.0D0
   PIKUP(2,SURF,CT)=W1
   PIKUP(3,SURF,CT)=W2
   IF(WQ.EQ.'ALPHA')PIKUP(4,SURF,CT)=W3
   IF(WQ.EQ.'BETA')PIKUP(4,SURF,CT)=W3
   IF(WQ.EQ.'GALPHA')PIKUP(4,SURF,CT)=W3
   IF(WQ.EQ.'GBETA')PIKUP(4,SURF,CT)=W3
   PIKUP(5,SURF,CT)=W4
   PIKUP(6,SURF,CT)=W5
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
!       THERE ARE NO SOLVE DELETIONS TO HANDEL.
!
   RETURN
END
! SUB SPIKUP.FOR
SUBROUTINE SPIKUP
!
   use DATLEN
   use mod_surface
   use mod_system, only: sys_last_surf
   use DATMAI
   use command_utils, only: is_command_query
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SPIKUP. THIS IS THE SUBROUTINE WHICH
!       HANDLES THE LENS PARAMETER PIKUPS AT THE LENS INPUT
!       AND UPDATE LENS LEVEL.
!
   INTEGER I
!
!
   IF(is_command_query()) THEN
200   FORMAT('FROM SURFACE #',I3)
300   FORMAT('A(MULT) = ',G23.15)
400   FORMAT('B(ADD) = ',G23.15)
      DO I=1,43
         IF(PIKUP(1,SURF,I).EQ.1.0D0) THEN
!       PIKUP EXISTS, WRITE SOMETHING
!
            IF(I.EQ.1) THEN
               WRITE(OUTLYNE,101)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
101            FORMAT('"RD" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.2) THEN
               WRITE(OUTLYNE,102)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
102            FORMAT('"CV" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.3) THEN
               WRITE(OUTLYNE,103)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
103            FORMAT('"TH" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.32) THEN
               WRITE(OUTLYNE,901)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,902) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,903) INT(PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,904) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,905) (PIKUP(5,SURF,I))
               CALL SHOWIT(0)
902            FORMAT('FROM SURFACE #',I3)
903            FORMAT('  TO SURFACE #',I3)
904            FORMAT('A(MULT) = ',G23.15)
905            FORMAT('B(ADD) = ',G23.15)
901            FORMAT('"THOAL" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.4) THEN
               WRITE(OUTLYNE,104)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
104            FORMAT('"CC" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.5) THEN
               WRITE(OUTLYNE,105)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
105            FORMAT('"AD" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.6) THEN
               WRITE(OUTLYNE,106)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
106            FORMAT('"AE" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.7) THEN
               WRITE(OUTLYNE,107)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
107            FORMAT('"AF" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.8) THEN
               WRITE(OUTLYNE,108)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
108            FORMAT('"AG" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.9) THEN
               WRITE(OUTLYNE,109)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
109            FORMAT('"CVTOR" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.10) THEN
               WRITE(OUTLYNE,110)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
110            FORMAT('"RDTOR" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.11) THEN
               WRITE(OUTLYNE,111)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
111            FORMAT('"PRO" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.12) THEN
               WRITE(OUTLYNE,112)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
112            FORMAT('"NPRO" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.13) THEN
               WRITE(OUTLYNE,113)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
113            FORMAT('"YD" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.14) THEN
               WRITE(OUTLYNE,114)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
114            FORMAT('"XD" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.15) THEN
               WRITE(OUTLYNE,115)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
115            FORMAT('"ALPHA" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.16) THEN
               WRITE(OUTLYNE,116)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
116            FORMAT('"BETA" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.17) THEN
               WRITE(OUTLYNE,117)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
117            FORMAT('"GAMMA" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.18) THEN
               WRITE(OUTLYNE,118)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
118            FORMAT('"CLAP" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.19) THEN
               WRITE(OUTLYNE,119)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
119            FORMAT('"COBS" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.20) THEN
               WRITE(OUTLYNE,120)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
120            FORMAT('"GLASS" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.21) THEN
               WRITE(OUTLYNE,121)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
121            FORMAT('"CCTOR" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.22) THEN
               WRITE(OUTLYNE,122)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
122            FORMAT('"ADTOR" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.23) THEN
               WRITE(OUTLYNE,123)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
123            FORMAT('"AETOR" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.24) THEN
               WRITE(OUTLYNE,124)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
124            FORMAT('"AFTOR" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.25) THEN
               WRITE(OUTLYNE,125)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
125            FORMAT('"AGTOR" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.26) THEN
               WRITE(OUTLYNE,126)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
126            FORMAT('"AC" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.27) THEN
               WRITE(OUTLYNE,127)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
127            FORMAT('"AH" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.28) THEN
               WRITE(OUTLYNE,128)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
128            FORMAT('"AI" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.29) THEN
               WRITE(OUTLYNE,129)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
129            FORMAT('"AJ" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.30) THEN
               WRITE(OUTLYNE,130)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
130            FORMAT('"AK" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
            IF(I.EQ.31) THEN
               WRITE(OUTLYNE,131)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
131            FORMAT('"AL" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
            IF(I.EQ.33) THEN
               WRITE(OUTLYNE,133)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
133            FORMAT('"ZD" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
            IF(I.EQ.34) THEN
               WRITE(OUTLYNE,134)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
134            FORMAT('"PIVX" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
            IF(I.EQ.35) THEN
               WRITE(OUTLYNE,135)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
135            FORMAT('"PIVY" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
            IF(I.EQ.36) THEN
               WRITE(OUTLYNE,136)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
136            FORMAT('"PIVZ" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
            IF(I.EQ.37) THEN
               WRITE(OUTLYNE,137)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
137            FORMAT('"GDX" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
            IF(I.EQ.38) THEN
               WRITE(OUTLYNE,138)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
138            FORMAT('"GDY" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
            IF(I.EQ.39) THEN
               WRITE(OUTLYNE,139)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
139            FORMAT('"GDZ" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
            IF(I.EQ.40) THEN
               WRITE(OUTLYNE,140)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
140            FORMAT('"GALPHA" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
            IF(I.EQ.41) THEN
               WRITE(OUTLYNE,141)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
141            FORMAT('"GBETA" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
            IF(I.EQ.42) THEN
               WRITE(OUTLYNE,142)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,300) (PIKUP(3,SURF,I))
               CALL SHOWIT(0)
               WRITE(OUTLYNE,400) (PIKUP(4,SURF,I))
               CALL SHOWIT(0)
142            FORMAT('"GGAMMA" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
            IF(I.EQ.43) THEN
               WRITE(OUTLYNE,143)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
143            FORMAT('"GRT" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
            IF(I.EQ.44) THEN
               WRITE(OUTLYNE,144)SURF
               CALL SHOWIT(0)
               WRITE(OUTLYNE,200) INT(PIKUP(2,SURF,I))
               CALL SHOWIT(0)
144            FORMAT('"COATING" PIKUP ON SURFACE #',I3)
            ELSE
            END IF
!
         ELSE
!       GO TO NEXT I
         END IF
      END DO
      RETURN
   ELSE
   END IF
!       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
!       SPECIFIC STORAGE LOCATIONS FOR LENS DATA,SOLVE AND
!       PIKUP ITEMS.
!
!       IF THE FIRST NUMERIC WORD OF A PIKUP RECORD IS
!       NEGATIVE, THE PIKUP REFERS TO THE SURFACE WHICH IS
!       BACK IN THE LENS BY THAT MANY SURFACES. SET THAT
!       ABSOLUTE ADDRESS NOW.
!       SURF IS THE SURFACE UPON WHICH THE PIKUP WILL BE
!       PLACED. SO IF W1=-3.0D0, W1 SHOULD BE RESET TO
!       W1=DBLE(SURF)+W1 AND ONLY IF W1 IS LESS THAN ZERO.
!
   IF(W1.LT.0.0D0) W1=DBLE(SURF)+W1
!
!       THE PIKUP COMMAND NEVER TAKES STRING INPUT
!
   IF(SST.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL('"PIKUP" TAKES NO STRING INPUT'//'\n'//'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!
!       HERE IS WHERE THE DECISION ON SPECIFIC SUBROUTINE
!       CALLS IS MADE FOR THE PROPER DISPOSITION OF PIKUP
!       INPUT DATA.
!
!       PIKUP TH
   IF(WQ.EQ.'TH') THEN
      CALL PIKTH
      RETURN
   ELSE
   END IF
!       PIKUP TH
   IF(WQ.EQ.'THOAL') THEN
      CALL PIKTHOAL
      RETURN
   ELSE
   END IF
!       PIKUP RD OR PIKUP CV
   IF(WQ.EQ.'RD'.OR.WQ.EQ.'CV') THEN
      IF(SURF.EQ.0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"'//WQ(1:2)//'" PIKUP NOT ALLOWED ON OBJECT SURFACE'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      CALL PIKCV
      RETURN
   END IF
!       PIKUP CC
   IF(WQ.EQ.'CC'.OR.WC.EQ.'CCTOR') THEN
      IF(SURF.EQ.0) THEN
         IF(WQ.EQ.'CC') OUTLYNE='"'//WQ(1:2)//'" PIKUP NOT ALLOWED ON OBJECT SURFACE'
         IF(WQ.EQ.'CCTOR') OUTLYNE='"'//WQ(1:5)//'" PIKUP NOT ALLOWED ON OBJECT SURFACE'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      CALL PIKCON
      RETURN
   END IF
!       PIKUP ASPHERIC TERMS
   IF(WQ.EQ.'AD'.OR.WQ.EQ.'AE'.OR.WQ.EQ.'AF'.OR.WQ.EQ.'AG'.OR.WQ.EQ.'AC'.OR.WQ.EQ.'AH'.OR.WQ.EQ.'AI'.OR.WQ.EQ.'AJ'.OR.WQ.EQ.'AK'.OR.WQ.EQ.'AL') THEN
      IF(SURF.EQ.0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"'//WQ(1:2)//'" PIKUP NOT ALLOWED ON OBJECT SURFACE'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      CALL PIKASP
      RETURN
   END IF
!       PIKUP ASPHERIC TORIC TERMS
   IF(WQ.EQ.'ADTOR'.OR.WQ.EQ.'AETOR'.OR.WQ.EQ.'AFTOR'.OR.WQ.EQ.'AGTOR') THEN
      IF(SURF.EQ.0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"'//WQ(1:5)//'" PIKUP NOT ALLOWED ON OBJECT SURFACE'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      CALL PIKASP
      RETURN
   END IF
!       PIKUP CVTOR OR RDTOR
   IF(WQ.EQ.'CVTOR'.OR.WQ.EQ.'RDTOR') THEN
      IF(SURF.EQ.0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"'//WQ(1:5)//'" PIKUP NOT ALLOWED ON OBJECT SURFACE'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      CALL PIKCVT
      RETURN
   END IF
!       PIKUP PRO OR NPRO
   IF(WQ.EQ.'PRO'.OR.WQ.EQ.'NPRO') THEN
      IF(SURF.EQ.0.AND.WQ.EQ.'PRO') THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"PRO" PIKUP NOT ALLOWED ON OBJECT SURFACE'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(SURF.EQ.0.AND.WQ.EQ.'NPRO') THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"NPRO" PIKUP NOT ALLOWED ON OBJECT SURFACE'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      CALL PIKPRO
      RETURN
   ELSE
   END IF
!       PIKUP XD YD ZD
   IF(WQ.EQ.'XD'.OR.WQ.EQ.'YD'.OR.WQ.EQ.'ZD') THEN
      IF(SURF.EQ.0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"'//WQ(1:2)//'" PIKUP NOT ALLOWED ON OBJECT SURFACE'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      CALL PIKXYD
      RETURN
   END IF
!       PIKUP GDX, GDY, GDZ
   IF(WQ.EQ.'GDX'.OR.WQ.EQ.'GDY'.OR.WQ.EQ.'GDZ') THEN
      IF(SURF.EQ.0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"'//WQ(1:3)//'" PIKUP NOT ALLOWED ON OBJECT SURFACE'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      CALL PIKXYD
      RETURN
   END IF
!       PIKUP ALPHA,BETA,GAMMA
   IF(WQ.EQ.'ALPHA'.OR.WQ.EQ.'BETA'.OR.WQ.EQ.'GAMMA'.OR.WQ.EQ.'GALPHA'.OR.WQ.EQ.'GBETA'.OR.WQ.EQ.'GGAMMA') THEN
      IF(SURF.EQ.0) THEN
         IF(WQ.EQ.'ALPHA')OUTLYNE='"ALPHA" PIKUP NOT ALLOWED ON OBJECT SURFACE'
         IF(WQ.EQ.'BETA')OUTLYNE='"BETA" PIKUP NOT ALLOWED ON OBJECT SURFACE'
         IF(WQ.EQ.'GAMMA')OUTLYNE='"GAMMA" PIKUP NOT ALLOWED ON OBJECT SURFACE'
         IF(WQ.EQ.'GALPHA')OUTLYNE='"GALPHA" PIKUP NOT ALLOWED ON OBJECT SURFACE'
         IF(WQ.EQ.'GBETA')OUTLYNE='"GBETA" PIKUP NOT ALLOWED ON OBJECT SURFACE'
         IF(WQ.EQ.'GGAMMA')OUTLYNE='"GGAMMA" PIKUP NOT ALLOWED ON OBJECT SURFACE'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(surf_tilt_flag(SURF).NE.6.0D0) THEN
      ELSE
         IF(WQ.EQ.'ALPHA')OUTLYNE='"ALPHA" PIKUP NOT ALLOWED ON A "TILT RET" SURFACE'
         IF(WQ.EQ.'BETA')OUTLYNE='"BETA" PIKUP NOT ALLOWED ON A "TILT RET" SURFACE'
         IF(WQ.EQ.'GAMMA')OUTLYNE='"GAMMA" PIKUP NOT ALLOWED ON A "TILT RET" SURFACE'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(WQ.EQ.'ALPHA'.OR.WQ.EQ.'BETA'.OR.WQ.EQ.'GAMMA'.OR.WQ.EQ.'GALPHA'.OR.WQ.EQ.'GBETA'.OR.WQ.EQ.'GGAMMA') CALL PIKANG
      RETURN
   ELSE
   END IF
!       PIKUP CLAP AND COBS
   IF(WQ.EQ.'CLAP'.OR.WQ.EQ.'COBS') THEN
      CALL PIKAPE
      RETURN
   ELSE
   END IF
!       PIKUP GLASS
   IF(WQ.EQ.'GLASS') THEN
      CALL PIKGLS
      RETURN
   ELSE
   END IF
!       PIKUP COATING
   IF(WQ.EQ.'COATING') THEN
      CALL PIKCOAT
      RETURN
   ELSE
   END IF
!       PIKUP GRT
   IF(WQ.EQ.'GRT') THEN
      CALL PIKGRT
      RETURN
   ELSE
   END IF
!       PIKUP PIVX OR PIVY OR PIVZ
   IF(WQ.EQ.'PIVX'.OR.WQ.EQ.'PIVY'.OR.WQ.EQ.'PIVZ') THEN
      CALL PIKPXYD
      RETURN
   ELSE
   END IF
   CALL REPORT_ERROR_AND_FAIL('INVALID QUALIFIER FOUND'//'\n'//'RE-ENTER COMMAND', 1)
   RETURN
END
