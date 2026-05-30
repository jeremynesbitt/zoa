!       SECOND GROUP OF SPECIAL SURFACE AND FITTING FILES

! SUB SPCOEF.FOR
SUBROUTINE SPCOEF(ITP)
!
   use DATLEN
   use mod_system, only: sys_last_surf
   use DATMAI
   use mod_surface, only: surf_special_type, surf_pickup_count, set_surf_pickup_count
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SPCOEF. THIS IS THE SUBROUTINE WHICH
!       HANDLES THE "C1" THHROUGH "C96"
!       COMMANDS FROM SPSRF OR UPDATE SPSRF LEVEL
!
   LOGICAL CNOT
!
   INTEGER PIKCNT,I,ITP
!
!
   IF(SST.EQ.1 .OR.SQ.EQ.1 .OR.S3.EQ.1 .OR.S4.EQ.1 .OR.S5.EQ.1) THEN
      OUTLYNE='" '//WC//&
      &' " ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
      CALL SHOWIT(1)
      CALL REPORT_ERROR_AND_FAIL('RE-ENTER DATA', 1)
      RETURN
   END IF
   IF(DF1.EQ.1 .OR.DF2.EQ.1) THEN
      OUTLYNE=&
      &'" '//WC//&
      &' " REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
      CALL SHOWIT(1)
      CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
      RETURN
   END IF
!       PROCEED WITH C1 TO C96
!
   IF(W1.EQ.0.0D0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'SPECIAL SURFACE TYPE NOT VALID ON OBJECT SURFACE'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(W1.LT.0.0D0) W1=sys_last_surf()+W1
   IF(INT(W1).LT.1 .OR.INT(W1).GT.INT(sys_last_surf())) THEN
      OUTLYNE=&
      &'SURFACE NUMBER BEYOND LEGAL RANGE'
      CALL SHOWIT(1)
      CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(surf_special_type(INT(W1)).EQ.0.0D0) THEN
      WRITE(OUTLYNE,*)&
      &'WARNING: SURFACE ',INT(W1),' NOT A SPECIAL SURFACE TYPE'
      CALL SHOWIT(1)
      CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(surf_special_type(INT(W1)).NE.0.0D0) THEN
!
!       PERFORM REQUIRED ACTION
!
!     CHECK FOR THINGS NOT ALLOWED
      CNOT=.FALSE.
!
      IF(surf_special_type(INT(W1)) == 1 .OR.&
      &surf_special_type(INT(W1)) == 6) THEN
         IF(WC.EQ.'C1') CNOT=.TRUE.
         IF(WC.EQ.'C2') CNOT=.TRUE.
         IF(WC.EQ.'C3') CNOT=.TRUE.
         IF(WC.EQ.'C4') CNOT=.TRUE.
         IF(WC.EQ.'C5') CNOT=.TRUE.
         IF(WC.EQ.'C6') CNOT=.TRUE.
         IF(WC.EQ.'C7') CNOT=.TRUE.
         IF(WC.EQ.'C8') CNOT=.TRUE.
         IF(WC.EQ.'C49') CNOT=.TRUE.
         IF(WC.EQ.'C50') CNOT=.TRUE.
         IF(WC.EQ.'C51') CNOT=.TRUE.
         IF(WC.EQ.'C52') CNOT=.TRUE.
         IF(WC.EQ.'C53') CNOT=.TRUE.
         IF(WC.EQ.'C54') CNOT=.TRUE.
         IF(WC.EQ.'C55') CNOT=.TRUE.
         IF(WC.EQ.'C56') CNOT=.TRUE.
         IF(WC.EQ.'C57') CNOT=.TRUE.
         IF(WC.EQ.'C58') CNOT=.TRUE.
         IF(WC.EQ.'C59') CNOT=.TRUE.
         IF(WC.EQ.'C60') CNOT=.TRUE.
         IF(WC.EQ.'C61') CNOT=.TRUE.
         IF(WC.EQ.'C62') CNOT=.TRUE.
         IF(WC.EQ.'C63') CNOT=.TRUE.
         IF(WC.EQ.'C64') CNOT=.TRUE.
         IF(WC.EQ.'C65') CNOT=.TRUE.
         IF(WC.EQ.'C66') CNOT=.TRUE.
         IF(WC.EQ.'C67') CNOT=.TRUE.
         IF(WC.EQ.'C68') CNOT=.TRUE.
         IF(WC.EQ.'C69') CNOT=.TRUE.
         IF(WC.EQ.'C70') CNOT=.TRUE.
         IF(WC.EQ.'C71') CNOT=.TRUE.
         IF(WC.EQ.'C72') CNOT=.TRUE.
         IF(WC.EQ.'C73') CNOT=.TRUE.
         IF(WC.EQ.'C74') CNOT=.TRUE.
         IF(WC.EQ.'C75') CNOT=.TRUE.
         IF(WC.EQ.'C76') CNOT=.TRUE.
         IF(WC.EQ.'C77') CNOT=.TRUE.
         IF(WC.EQ.'C78') CNOT=.TRUE.
         IF(WC.EQ.'C79') CNOT=.TRUE.
         IF(WC.EQ.'C80') CNOT=.TRUE.
         IF(WC.EQ.'C81') CNOT=.TRUE.
         IF(WC.EQ.'C82') CNOT=.TRUE.
         IF(WC.EQ.'C83') CNOT=.TRUE.
         IF(WC.EQ.'C84') CNOT=.TRUE.
         IF(WC.EQ.'C85') CNOT=.TRUE.
         IF(WC.EQ.'C86') CNOT=.TRUE.
         IF(WC.EQ.'C87') CNOT=.TRUE.
         IF(WC.EQ.'C88') CNOT=.TRUE.
         IF(WC.EQ.'C89') CNOT=.TRUE.
         IF(WC.EQ.'C90') CNOT=.TRUE.
         IF(WC.EQ.'C91') CNOT=.TRUE.
         IF(WC.EQ.'C92') CNOT=.TRUE.
         IF(WC.EQ.'C93') CNOT=.TRUE.
         IF(WC.EQ.'C94') CNOT=.TRUE.
         IF(WC.EQ.'C95') CNOT=.TRUE.
         IF(WC.EQ.'C96') CNOT=.TRUE.
         IF(CNOT) THEN
            OUTLYNE=&
            &'SPECIAL SURFACE TYPES 1 AND 6'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'ONLY USE COEFFICIENTS C9 THROUGH C48'
            CALL SHOWIT(1)
            CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
            RETURN
         END IF
      END IF
      IF(surf_special_type(INT(W1)) == 24) THEN
         IF(WC.EQ.'C5') CNOT=.TRUE.
         IF(WC.EQ.'C6') CNOT=.TRUE.
         IF(WC.EQ.'C7') CNOT=.TRUE.
         IF(WC.EQ.'C8') CNOT=.TRUE.
         IF(WC.EQ.'C9') CNOT=.TRUE.
         IF(WC.EQ.'C10') CNOT=.TRUE.
         IF(WC.EQ.'C11') CNOT=.TRUE.
         IF(WC.EQ.'C12') CNOT=.TRUE.
         IF(WC.EQ.'C13') CNOT=.TRUE.
         IF(WC.EQ.'C14') CNOT=.TRUE.
         IF(WC.EQ.'C15') CNOT=.TRUE.
         IF(WC.EQ.'C16') CNOT=.TRUE.
         IF(WC.EQ.'C17') CNOT=.TRUE.
         IF(WC.EQ.'C18') CNOT=.TRUE.
         IF(WC.EQ.'C19') CNOT=.TRUE.
         IF(WC.EQ.'C20') CNOT=.TRUE.
         IF(WC.EQ.'C21') CNOT=.TRUE.
         IF(WC.EQ.'C22') CNOT=.TRUE.
         IF(WC.EQ.'C23') CNOT=.TRUE.
         IF(WC.EQ.'C24') CNOT=.TRUE.
         IF(WC.EQ.'C25') CNOT=.TRUE.
         IF(WC.EQ.'C26') CNOT=.TRUE.
         IF(WC.EQ.'C27') CNOT=.TRUE.
         IF(WC.EQ.'C28') CNOT=.TRUE.
         IF(WC.EQ.'C29') CNOT=.TRUE.
         IF(WC.EQ.'C30') CNOT=.TRUE.
         IF(WC.EQ.'C31') CNOT=.TRUE.
         IF(WC.EQ.'C32') CNOT=.TRUE.
         IF(WC.EQ.'C33') CNOT=.TRUE.
         IF(WC.EQ.'C34') CNOT=.TRUE.
         IF(WC.EQ.'C35') CNOT=.TRUE.
         IF(WC.EQ.'C36') CNOT=.TRUE.
         IF(WC.EQ.'C37') CNOT=.TRUE.
         IF(WC.EQ.'C38') CNOT=.TRUE.
         IF(WC.EQ.'C39') CNOT=.TRUE.
         IF(WC.EQ.'C40') CNOT=.TRUE.
         IF(WC.EQ.'C41') CNOT=.TRUE.
         IF(WC.EQ.'C42') CNOT=.TRUE.
         IF(WC.EQ.'C43') CNOT=.TRUE.
         IF(WC.EQ.'C44') CNOT=.TRUE.
         IF(WC.EQ.'C45') CNOT=.TRUE.
         IF(WC.EQ.'C46') CNOT=.TRUE.
         IF(WC.EQ.'C47') CNOT=.TRUE.
         IF(WC.EQ.'C48') CNOT=.TRUE.
         IF(WC.EQ.'C49') CNOT=.TRUE.
         IF(WC.EQ.'C50') CNOT=.TRUE.
         IF(WC.EQ.'C51') CNOT=.TRUE.
         IF(WC.EQ.'C52') CNOT=.TRUE.
         IF(WC.EQ.'C53') CNOT=.TRUE.
         IF(WC.EQ.'C54') CNOT=.TRUE.
         IF(WC.EQ.'C55') CNOT=.TRUE.
         IF(WC.EQ.'C56') CNOT=.TRUE.
         IF(WC.EQ.'C57') CNOT=.TRUE.
         IF(WC.EQ.'C58') CNOT=.TRUE.
         IF(WC.EQ.'C59') CNOT=.TRUE.
         IF(WC.EQ.'C60') CNOT=.TRUE.
         IF(WC.EQ.'C61') CNOT=.TRUE.
         IF(WC.EQ.'C62') CNOT=.TRUE.
         IF(WC.EQ.'C63') CNOT=.TRUE.
         IF(WC.EQ.'C64') CNOT=.TRUE.
         IF(WC.EQ.'C65') CNOT=.TRUE.
         IF(WC.EQ.'C66') CNOT=.TRUE.
         IF(WC.EQ.'C67') CNOT=.TRUE.
         IF(WC.EQ.'C68') CNOT=.TRUE.
         IF(WC.EQ.'C69') CNOT=.TRUE.
         IF(WC.EQ.'C70') CNOT=.TRUE.
         IF(WC.EQ.'C71') CNOT=.TRUE.
         IF(WC.EQ.'C72') CNOT=.TRUE.
         IF(WC.EQ.'C73') CNOT=.TRUE.
         IF(WC.EQ.'C74') CNOT=.TRUE.
         IF(WC.EQ.'C75') CNOT=.TRUE.
         IF(WC.EQ.'C76') CNOT=.TRUE.
         IF(WC.EQ.'C77') CNOT=.TRUE.
         IF(WC.EQ.'C78') CNOT=.TRUE.
         IF(WC.EQ.'C79') CNOT=.TRUE.
         IF(WC.EQ.'C80') CNOT=.TRUE.
         IF(WC.EQ.'C81') CNOT=.TRUE.
         IF(WC.EQ.'C82') CNOT=.TRUE.
         IF(WC.EQ.'C83') CNOT=.TRUE.
         IF(WC.EQ.'C84') CNOT=.TRUE.
         IF(WC.EQ.'C85') CNOT=.TRUE.
         IF(WC.EQ.'C86') CNOT=.TRUE.
         IF(WC.EQ.'C87') CNOT=.TRUE.
         IF(WC.EQ.'C88') CNOT=.TRUE.
         IF(WC.EQ.'C89') CNOT=.TRUE.
         IF(WC.EQ.'C90') CNOT=.TRUE.
         IF(WC.EQ.'C91') CNOT=.TRUE.
         IF(WC.EQ.'C92') CNOT=.TRUE.
         IF(WC.EQ.'C93') CNOT=.TRUE.
         IF(WC.EQ.'C94') CNOT=.TRUE.
         IF(WC.EQ.'C95') CNOT=.TRUE.
         IF(WC.EQ.'C96') CNOT=.TRUE.
         IF(CNOT) THEN
            OUTLYNE=&
            &'SPECIAL SURFACE TYPE 24'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'ONLY USES COEFFICIENTS C1 THROUGH C4'
            CALL SHOWIT(1)
            CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
            RETURN
         END IF
      END IF
      IF(surf_special_type(INT(W1)) == 2 .OR.&
      &surf_special_type(INT(W1)) == 9) THEN
         IF(WC.EQ.'C67') CNOT=.TRUE.
         IF(WC.EQ.'C68') CNOT=.TRUE.
         IF(WC.EQ.'C69') CNOT=.TRUE.
         IF(WC.EQ.'C70') CNOT=.TRUE.
         IF(WC.EQ.'C71') CNOT=.TRUE.
         IF(WC.EQ.'C72') CNOT=.TRUE.
         IF(WC.EQ.'C73') CNOT=.TRUE.
         IF(WC.EQ.'C74') CNOT=.TRUE.
         IF(WC.EQ.'C75') CNOT=.TRUE.
         IF(WC.EQ.'C76') CNOT=.TRUE.
         IF(WC.EQ.'C77') CNOT=.TRUE.
         IF(WC.EQ.'C78') CNOT=.TRUE.
         IF(WC.EQ.'C79') CNOT=.TRUE.
         IF(WC.EQ.'C80') CNOT=.TRUE.
         IF(WC.EQ.'C81') CNOT=.TRUE.
         IF(WC.EQ.'C82') CNOT=.TRUE.
         IF(WC.EQ.'C83') CNOT=.TRUE.
         IF(WC.EQ.'C84') CNOT=.TRUE.
         IF(WC.EQ.'C85') CNOT=.TRUE.
         IF(WC.EQ.'C86') CNOT=.TRUE.
         IF(WC.EQ.'C87') CNOT=.TRUE.
         IF(WC.EQ.'C88') CNOT=.TRUE.
         IF(WC.EQ.'C89') CNOT=.TRUE.
         IF(WC.EQ.'C90') CNOT=.TRUE.
         IF(WC.EQ.'C91') CNOT=.TRUE.
         IF(WC.EQ.'C92') CNOT=.TRUE.
         IF(WC.EQ.'C93') CNOT=.TRUE.
         IF(WC.EQ.'C94') CNOT=.TRUE.
         IF(WC.EQ.'C95') CNOT=.TRUE.
         IF(WC.EQ.'C96') CNOT=.TRUE.
         IF(CNOT) THEN
            OUTLYNE=&
            &'SPECIAL SURFACE TYPES 2 AND 9'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'ONLY USE COEFFICIENTS C1 THROUGH C66'
            CALL SHOWIT(1)
            CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
            RETURN
         END IF
      END IF
      IF(surf_special_type(INT(W1)) == 14 .OR.&
      &surf_special_type(INT(W1)) == 15) THEN
         IF(WC.EQ.'C49') CNOT=.TRUE.
         IF(WC.EQ.'C50') CNOT=.TRUE.
         IF(WC.EQ.'C51') CNOT=.TRUE.
         IF(WC.EQ.'C52') CNOT=.TRUE.
         IF(WC.EQ.'C53') CNOT=.TRUE.
         IF(WC.EQ.'C54') CNOT=.TRUE.
         IF(WC.EQ.'C55') CNOT=.TRUE.
         IF(WC.EQ.'C56') CNOT=.TRUE.
         IF(WC.EQ.'C57') CNOT=.TRUE.
         IF(WC.EQ.'C58') CNOT=.TRUE.
         IF(WC.EQ.'C59') CNOT=.TRUE.
         IF(WC.EQ.'C60') CNOT=.TRUE.
         IF(WC.EQ.'C61') CNOT=.TRUE.
         IF(WC.EQ.'C62') CNOT=.TRUE.
         IF(WC.EQ.'C63') CNOT=.TRUE.
         IF(WC.EQ.'C64') CNOT=.TRUE.
         IF(WC.EQ.'C65') CNOT=.TRUE.
         IF(WC.EQ.'C66') CNOT=.TRUE.
         IF(WC.EQ.'C67') CNOT=.TRUE.
         IF(WC.EQ.'C68') CNOT=.TRUE.
         IF(WC.EQ.'C69') CNOT=.TRUE.
         IF(WC.EQ.'C70') CNOT=.TRUE.
         IF(WC.EQ.'C71') CNOT=.TRUE.
         IF(WC.EQ.'C72') CNOT=.TRUE.
         IF(WC.EQ.'C73') CNOT=.TRUE.
         IF(WC.EQ.'C74') CNOT=.TRUE.
         IF(WC.EQ.'C75') CNOT=.TRUE.
         IF(WC.EQ.'C76') CNOT=.TRUE.
         IF(WC.EQ.'C77') CNOT=.TRUE.
         IF(WC.EQ.'C78') CNOT=.TRUE.
         IF(WC.EQ.'C79') CNOT=.TRUE.
         IF(WC.EQ.'C80') CNOT=.TRUE.
         IF(WC.EQ.'C81') CNOT=.TRUE.
         IF(WC.EQ.'C82') CNOT=.TRUE.
         IF(WC.EQ.'C83') CNOT=.TRUE.
         IF(WC.EQ.'C84') CNOT=.TRUE.
         IF(WC.EQ.'C85') CNOT=.TRUE.
         IF(WC.EQ.'C86') CNOT=.TRUE.
         IF(WC.EQ.'C87') CNOT=.TRUE.
         IF(WC.EQ.'C88') CNOT=.TRUE.
         IF(WC.EQ.'C89') CNOT=.TRUE.
         IF(WC.EQ.'C90') CNOT=.TRUE.
         IF(WC.EQ.'C91') CNOT=.TRUE.
         IF(WC.EQ.'C92') CNOT=.TRUE.
         IF(WC.EQ.'C93') CNOT=.TRUE.
         IF(WC.EQ.'C94') CNOT=.TRUE.
         IF(WC.EQ.'C95') CNOT=.TRUE.
         IF(WC.EQ.'C96') CNOT=.TRUE.
         IF(CNOT) THEN
            OUTLYNE=&
            &'SPECIAL SURFACE TYPES 14 AND 15'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'ONLY USE COEFFICIENTS C1 THROUGH C48'
            CALL SHOWIT(1)
            CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
            RETURN
         END IF
      END IF
      IF(surf_special_type(INT(W1)) == 16) THEN
         IF(WC.EQ.'C12') CNOT=.TRUE.
         IF(WC.EQ.'C13') CNOT=.TRUE.
         IF(WC.EQ.'C14') CNOT=.TRUE.
         IF(WC.EQ.'C15') CNOT=.TRUE.
         IF(WC.EQ.'C16') CNOT=.TRUE.
         IF(WC.EQ.'C17') CNOT=.TRUE.
         IF(WC.EQ.'C18') CNOT=.TRUE.
         IF(WC.EQ.'C19') CNOT=.TRUE.
         IF(WC.EQ.'C20') CNOT=.TRUE.
         IF(WC.EQ.'C21') CNOT=.TRUE.
         IF(WC.EQ.'C22') CNOT=.TRUE.
         IF(WC.EQ.'C23') CNOT=.TRUE.
         IF(WC.EQ.'C24') CNOT=.TRUE.
         IF(WC.EQ.'C25') CNOT=.TRUE.
         IF(WC.EQ.'C26') CNOT=.TRUE.
         IF(WC.EQ.'C27') CNOT=.TRUE.
         IF(WC.EQ.'C28') CNOT=.TRUE.
         IF(WC.EQ.'C29') CNOT=.TRUE.
         IF(WC.EQ.'C30') CNOT=.TRUE.
         IF(WC.EQ.'C31') CNOT=.TRUE.
         IF(WC.EQ.'C32') CNOT=.TRUE.
         IF(WC.EQ.'C33') CNOT=.TRUE.
         IF(WC.EQ.'C34') CNOT=.TRUE.
         IF(WC.EQ.'C35') CNOT=.TRUE.
         IF(WC.EQ.'C36') CNOT=.TRUE.
         IF(WC.EQ.'C37') CNOT=.TRUE.
         IF(WC.EQ.'C38') CNOT=.TRUE.
         IF(WC.EQ.'C39') CNOT=.TRUE.
         IF(WC.EQ.'C40') CNOT=.TRUE.
         IF(WC.EQ.'C41') CNOT=.TRUE.
         IF(WC.EQ.'C42') CNOT=.TRUE.
         IF(WC.EQ.'C43') CNOT=.TRUE.
         IF(WC.EQ.'C44') CNOT=.TRUE.
         IF(WC.EQ.'C45') CNOT=.TRUE.
         IF(WC.EQ.'C46') CNOT=.TRUE.
         IF(WC.EQ.'C47') CNOT=.TRUE.
         IF(WC.EQ.'C48') CNOT=.TRUE.
         IF(WC.EQ.'C49') CNOT=.TRUE.
         IF(WC.EQ.'C50') CNOT=.TRUE.
         IF(WC.EQ.'C51') CNOT=.TRUE.
         IF(WC.EQ.'C52') CNOT=.TRUE.
         IF(WC.EQ.'C53') CNOT=.TRUE.
         IF(WC.EQ.'C54') CNOT=.TRUE.
         IF(WC.EQ.'C55') CNOT=.TRUE.
         IF(WC.EQ.'C56') CNOT=.TRUE.
         IF(WC.EQ.'C57') CNOT=.TRUE.
         IF(WC.EQ.'C58') CNOT=.TRUE.
         IF(WC.EQ.'C59') CNOT=.TRUE.
         IF(WC.EQ.'C60') CNOT=.TRUE.
         IF(WC.EQ.'C61') CNOT=.TRUE.
         IF(WC.EQ.'C62') CNOT=.TRUE.
         IF(WC.EQ.'C62') CNOT=.TRUE.
         IF(WC.EQ.'C63') CNOT=.TRUE.
         IF(WC.EQ.'C64') CNOT=.TRUE.
         IF(WC.EQ.'C65') CNOT=.TRUE.
         IF(WC.EQ.'C66') CNOT=.TRUE.
         IF(WC.EQ.'C67') CNOT=.TRUE.
         IF(WC.EQ.'C68') CNOT=.TRUE.
         IF(WC.EQ.'C69') CNOT=.TRUE.
         IF(WC.EQ.'C70') CNOT=.TRUE.
         IF(WC.EQ.'C71') CNOT=.TRUE.
         IF(WC.EQ.'C72') CNOT=.TRUE.
         IF(WC.EQ.'C73') CNOT=.TRUE.
         IF(WC.EQ.'C74') CNOT=.TRUE.
         IF(WC.EQ.'C75') CNOT=.TRUE.
         IF(WC.EQ.'C76') CNOT=.TRUE.
         IF(WC.EQ.'C77') CNOT=.TRUE.
         IF(WC.EQ.'C78') CNOT=.TRUE.
         IF(WC.EQ.'C79') CNOT=.TRUE.
         IF(WC.EQ.'C80') CNOT=.TRUE.
         IF(WC.EQ.'C81') CNOT=.TRUE.
         IF(WC.EQ.'C82') CNOT=.TRUE.
         IF(WC.EQ.'C83') CNOT=.TRUE.
         IF(WC.EQ.'C84') CNOT=.TRUE.
         IF(WC.EQ.'C85') CNOT=.TRUE.
         IF(WC.EQ.'C86') CNOT=.TRUE.
         IF(WC.EQ.'C87') CNOT=.TRUE.
         IF(WC.EQ.'C88') CNOT=.TRUE.
         IF(WC.EQ.'C89') CNOT=.TRUE.
         IF(WC.EQ.'C90') CNOT=.TRUE.
         IF(WC.EQ.'C91') CNOT=.TRUE.
         IF(WC.EQ.'C92') CNOT=.TRUE.
         IF(WC.EQ.'C93') CNOT=.TRUE.
         IF(WC.EQ.'C94') CNOT=.TRUE.
         IF(WC.EQ.'C95') CNOT=.TRUE.
         IF(WC.EQ.'C96') CNOT=.TRUE.
         IF(CNOT) THEN
            OUTLYNE=&
            &'SPECIAL SURFACE TYPE 16'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'ONLY USES COEFFICIENTS C1 THROUGH C11'
            CALL SHOWIT(1)
            CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
            RETURN
         END IF
      END IF
      IF(surf_special_type(INT(W1)) == 18) THEN
         IF(WC.EQ.'C19') CNOT=.TRUE.
         IF(WC.EQ.'C20') CNOT=.TRUE.
         IF(WC.EQ.'C21') CNOT=.TRUE.
         IF(WC.EQ.'C22') CNOT=.TRUE.
         IF(WC.EQ.'C23') CNOT=.TRUE.
         IF(WC.EQ.'C24') CNOT=.TRUE.
         IF(WC.EQ.'C25') CNOT=.TRUE.
         IF(WC.EQ.'C26') CNOT=.TRUE.
         IF(WC.EQ.'C27') CNOT=.TRUE.
         IF(WC.EQ.'C28') CNOT=.TRUE.
         IF(WC.EQ.'C29') CNOT=.TRUE.
         IF(WC.EQ.'C30') CNOT=.TRUE.
         IF(WC.EQ.'C31') CNOT=.TRUE.
         IF(WC.EQ.'C32') CNOT=.TRUE.
         IF(WC.EQ.'C33') CNOT=.TRUE.
         IF(WC.EQ.'C34') CNOT=.TRUE.
         IF(WC.EQ.'C35') CNOT=.TRUE.
         IF(WC.EQ.'C36') CNOT=.TRUE.
         IF(WC.EQ.'C37') CNOT=.TRUE.
         IF(WC.EQ.'C38') CNOT=.TRUE.
         IF(WC.EQ.'C39') CNOT=.TRUE.
         IF(WC.EQ.'C40') CNOT=.TRUE.
         IF(WC.EQ.'C41') CNOT=.TRUE.
         IF(WC.EQ.'C42') CNOT=.TRUE.
         IF(WC.EQ.'C43') CNOT=.TRUE.
         IF(WC.EQ.'C44') CNOT=.TRUE.
         IF(WC.EQ.'C45') CNOT=.TRUE.
         IF(WC.EQ.'C46') CNOT=.TRUE.
         IF(WC.EQ.'C47') CNOT=.TRUE.
         IF(WC.EQ.'C48') CNOT=.TRUE.
         IF(WC.EQ.'C49') CNOT=.TRUE.
         IF(WC.EQ.'C50') CNOT=.TRUE.
         IF(WC.EQ.'C51') CNOT=.TRUE.
         IF(WC.EQ.'C52') CNOT=.TRUE.
         IF(WC.EQ.'C53') CNOT=.TRUE.
         IF(WC.EQ.'C54') CNOT=.TRUE.
         IF(WC.EQ.'C55') CNOT=.TRUE.
         IF(WC.EQ.'C56') CNOT=.TRUE.
         IF(WC.EQ.'C57') CNOT=.TRUE.
         IF(WC.EQ.'C58') CNOT=.TRUE.
         IF(WC.EQ.'C59') CNOT=.TRUE.
         IF(WC.EQ.'C60') CNOT=.TRUE.
         IF(WC.EQ.'C61') CNOT=.TRUE.
         IF(WC.EQ.'C62') CNOT=.TRUE.
         IF(WC.EQ.'C62') CNOT=.TRUE.
         IF(WC.EQ.'C63') CNOT=.TRUE.
         IF(WC.EQ.'C64') CNOT=.TRUE.
         IF(WC.EQ.'C65') CNOT=.TRUE.
         IF(WC.EQ.'C66') CNOT=.TRUE.
         IF(WC.EQ.'C67') CNOT=.TRUE.
         IF(WC.EQ.'C68') CNOT=.TRUE.
         IF(WC.EQ.'C69') CNOT=.TRUE.
         IF(WC.EQ.'C70') CNOT=.TRUE.
         IF(WC.EQ.'C71') CNOT=.TRUE.
         IF(WC.EQ.'C72') CNOT=.TRUE.
         IF(WC.EQ.'C73') CNOT=.TRUE.
         IF(WC.EQ.'C74') CNOT=.TRUE.
         IF(WC.EQ.'C75') CNOT=.TRUE.
         IF(WC.EQ.'C76') CNOT=.TRUE.
         IF(WC.EQ.'C77') CNOT=.TRUE.
         IF(WC.EQ.'C78') CNOT=.TRUE.
         IF(WC.EQ.'C79') CNOT=.TRUE.
         IF(WC.EQ.'C80') CNOT=.TRUE.
         IF(WC.EQ.'C81') CNOT=.TRUE.
         IF(WC.EQ.'C82') CNOT=.TRUE.
         IF(WC.EQ.'C83') CNOT=.TRUE.
         IF(WC.EQ.'C84') CNOT=.TRUE.
         IF(WC.EQ.'C85') CNOT=.TRUE.
         IF(WC.EQ.'C86') CNOT=.TRUE.
         IF(WC.EQ.'C87') CNOT=.TRUE.
         IF(WC.EQ.'C88') CNOT=.TRUE.
         IF(WC.EQ.'C89') CNOT=.TRUE.
         IF(WC.EQ.'C90') CNOT=.TRUE.
         IF(WC.EQ.'C91') CNOT=.TRUE.
         IF(WC.EQ.'C92') CNOT=.TRUE.
         IF(WC.EQ.'C93') CNOT=.TRUE.
         IF(WC.EQ.'C94') CNOT=.TRUE.
         IF(WC.EQ.'C95') CNOT=.TRUE.
         IF(WC.EQ.'C96') CNOT=.TRUE.
         IF(CNOT) THEN
            OUTLYNE=&
            &'SPECIAL SURFACE TYPE 18'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'ONLY USES COEFFICIENTS C1 THROUGH C18'
            CALL SHOWIT(1)
            CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
            RETURN
         END IF
      END IF
      IF(surf_special_type(INT(W1)) == 3 .OR.&
      &surf_special_type(INT(W1)) == 10) THEN
         IF(WC.EQ.'C38') CNOT=.TRUE.
         IF(WC.EQ.'C39') CNOT=.TRUE.
         IF(WC.EQ.'C40') CNOT=.TRUE.
         IF(WC.EQ.'C41') CNOT=.TRUE.
         IF(WC.EQ.'C42') CNOT=.TRUE.
         IF(WC.EQ.'C43') CNOT=.TRUE.
         IF(WC.EQ.'C44') CNOT=.TRUE.
         IF(WC.EQ.'C45') CNOT=.TRUE.
         IF(WC.EQ.'C46') CNOT=.TRUE.
         IF(WC.EQ.'C47') CNOT=.TRUE.
         IF(WC.EQ.'C48') CNOT=.TRUE.
         IF(WC.EQ.'C49') CNOT=.TRUE.
         IF(WC.EQ.'C50') CNOT=.TRUE.
         IF(WC.EQ.'C51') CNOT=.TRUE.
         IF(WC.EQ.'C52') CNOT=.TRUE.
         IF(WC.EQ.'C53') CNOT=.TRUE.
         IF(WC.EQ.'C54') CNOT=.TRUE.
         IF(WC.EQ.'C55') CNOT=.TRUE.
         IF(WC.EQ.'C56') CNOT=.TRUE.
         IF(WC.EQ.'C57') CNOT=.TRUE.
         IF(WC.EQ.'C58') CNOT=.TRUE.
         IF(WC.EQ.'C59') CNOT=.TRUE.
         IF(WC.EQ.'C60') CNOT=.TRUE.
         IF(WC.EQ.'C61') CNOT=.TRUE.
         IF(WC.EQ.'C62') CNOT=.TRUE.
         IF(WC.EQ.'C63') CNOT=.TRUE.
         IF(WC.EQ.'C64') CNOT=.TRUE.
         IF(WC.EQ.'C65') CNOT=.TRUE.
         IF(WC.EQ.'C66') CNOT=.TRUE.
         IF(WC.EQ.'C67') CNOT=.TRUE.
         IF(WC.EQ.'C68') CNOT=.TRUE.
         IF(WC.EQ.'C69') CNOT=.TRUE.
         IF(WC.EQ.'C70') CNOT=.TRUE.
         IF(WC.EQ.'C71') CNOT=.TRUE.
         IF(WC.EQ.'C72') CNOT=.TRUE.
         IF(WC.EQ.'C73') CNOT=.TRUE.
         IF(WC.EQ.'C74') CNOT=.TRUE.
         IF(WC.EQ.'C75') CNOT=.TRUE.
         IF(WC.EQ.'C76') CNOT=.TRUE.
         IF(WC.EQ.'C77') CNOT=.TRUE.
         IF(WC.EQ.'C78') CNOT=.TRUE.
         IF(WC.EQ.'C79') CNOT=.TRUE.
         IF(WC.EQ.'C80') CNOT=.TRUE.
         IF(WC.EQ.'C81') CNOT=.TRUE.
         IF(WC.EQ.'C82') CNOT=.TRUE.
         IF(WC.EQ.'C83') CNOT=.TRUE.
         IF(WC.EQ.'C84') CNOT=.TRUE.
         IF(WC.EQ.'C85') CNOT=.TRUE.
         IF(WC.EQ.'C86') CNOT=.TRUE.
         IF(WC.EQ.'C87') CNOT=.TRUE.
         IF(WC.EQ.'C88') CNOT=.TRUE.
         IF(WC.EQ.'C89') CNOT=.TRUE.
         IF(WC.EQ.'C90') CNOT=.TRUE.
         IF(WC.EQ.'C91') CNOT=.TRUE.
         IF(WC.EQ.'C92') CNOT=.TRUE.
         IF(WC.EQ.'C93') CNOT=.TRUE.
         IF(WC.EQ.'C94') CNOT=.TRUE.
         IF(WC.EQ.'C95') CNOT=.TRUE.
         IF(WC.EQ.'C96') CNOT=.TRUE.
         IF(CNOT) THEN
            OUTLYNE=&
            &'SPECIAL SURFACE TYPES 3 AND 10'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'ONLY USE COEFFICIENTS C1 THROUGH C37'
            CALL SHOWIT(1)
            CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
            RETURN
         END IF
      END IF
      IF(surf_special_type(INT(W1)) == 19) THEN
         IF(WC.EQ.'C5') CNOT=.TRUE.
         IF(WC.EQ.'C6') CNOT=.TRUE.
         IF(WC.EQ.'C7') CNOT=.TRUE.
         IF(WC.EQ.'C8') CNOT=.TRUE.
         IF(WC.EQ.'C9') CNOT=.TRUE.
         IF(WC.EQ.'C10') CNOT=.TRUE.
         IF(WC.EQ.'C11') CNOT=.TRUE.
         IF(WC.EQ.'C12') CNOT=.TRUE.
         IF(WC.EQ.'C13') CNOT=.TRUE.
         IF(WC.EQ.'C14') CNOT=.TRUE.
         IF(WC.EQ.'C15') CNOT=.TRUE.
         IF(WC.EQ.'C16') CNOT=.TRUE.
         IF(WC.EQ.'C17') CNOT=.TRUE.
         IF(WC.EQ.'C18') CNOT=.TRUE.
         IF(WC.EQ.'C19') CNOT=.TRUE.
         IF(WC.EQ.'C20') CNOT=.TRUE.
         IF(WC.EQ.'C21') CNOT=.TRUE.
         IF(WC.EQ.'C22') CNOT=.TRUE.
         IF(WC.EQ.'C23') CNOT=.TRUE.
         IF(WC.EQ.'C24') CNOT=.TRUE.
         IF(WC.EQ.'C25') CNOT=.TRUE.
         IF(WC.EQ.'C26') CNOT=.TRUE.
         IF(WC.EQ.'C27') CNOT=.TRUE.
         IF(WC.EQ.'C28') CNOT=.TRUE.
         IF(WC.EQ.'C29') CNOT=.TRUE.
         IF(WC.EQ.'C30') CNOT=.TRUE.
         IF(WC.EQ.'C31') CNOT=.TRUE.
         IF(WC.EQ.'C32') CNOT=.TRUE.
         IF(WC.EQ.'C33') CNOT=.TRUE.
         IF(WC.EQ.'C34') CNOT=.TRUE.
         IF(WC.EQ.'C35') CNOT=.TRUE.
         IF(WC.EQ.'C36') CNOT=.TRUE.
         IF(WC.EQ.'C37') CNOT=.TRUE.
         IF(WC.EQ.'C38') CNOT=.TRUE.
         IF(WC.EQ.'C39') CNOT=.TRUE.
         IF(WC.EQ.'C40') CNOT=.TRUE.
         IF(WC.EQ.'C41') CNOT=.TRUE.
         IF(WC.EQ.'C42') CNOT=.TRUE.
         IF(WC.EQ.'C43') CNOT=.TRUE.
         IF(WC.EQ.'C44') CNOT=.TRUE.
         IF(WC.EQ.'C45') CNOT=.TRUE.
         IF(WC.EQ.'C46') CNOT=.TRUE.
         IF(WC.EQ.'C47') CNOT=.TRUE.
         IF(WC.EQ.'C48') CNOT=.TRUE.
         IF(WC.EQ.'C49') CNOT=.TRUE.
         IF(WC.EQ.'C50') CNOT=.TRUE.
         IF(WC.EQ.'C51') CNOT=.TRUE.
         IF(WC.EQ.'C52') CNOT=.TRUE.
         IF(WC.EQ.'C53') CNOT=.TRUE.
         IF(WC.EQ.'C54') CNOT=.TRUE.
         IF(WC.EQ.'C55') CNOT=.TRUE.
         IF(WC.EQ.'C56') CNOT=.TRUE.
         IF(WC.EQ.'C57') CNOT=.TRUE.
         IF(WC.EQ.'C58') CNOT=.TRUE.
         IF(WC.EQ.'C59') CNOT=.TRUE.
         IF(WC.EQ.'C60') CNOT=.TRUE.
         IF(WC.EQ.'C61') CNOT=.TRUE.
         IF(WC.EQ.'C62') CNOT=.TRUE.
         IF(WC.EQ.'C63') CNOT=.TRUE.
         IF(WC.EQ.'C64') CNOT=.TRUE.
         IF(WC.EQ.'C65') CNOT=.TRUE.
         IF(WC.EQ.'C66') CNOT=.TRUE.
         IF(WC.EQ.'C67') CNOT=.TRUE.
         IF(WC.EQ.'C68') CNOT=.TRUE.
         IF(WC.EQ.'C69') CNOT=.TRUE.
         IF(WC.EQ.'C70') CNOT=.TRUE.
         IF(WC.EQ.'C71') CNOT=.TRUE.
         IF(WC.EQ.'C72') CNOT=.TRUE.
         IF(WC.EQ.'C73') CNOT=.TRUE.
         IF(WC.EQ.'C74') CNOT=.TRUE.
         IF(WC.EQ.'C75') CNOT=.TRUE.
         IF(WC.EQ.'C76') CNOT=.TRUE.
         IF(WC.EQ.'C77') CNOT=.TRUE.
         IF(WC.EQ.'C78') CNOT=.TRUE.
         IF(WC.EQ.'C79') CNOT=.TRUE.
         IF(WC.EQ.'C80') CNOT=.TRUE.
         IF(WC.EQ.'C81') CNOT=.TRUE.
         IF(WC.EQ.'C82') CNOT=.TRUE.
         IF(WC.EQ.'C83') CNOT=.TRUE.
         IF(WC.EQ.'C84') CNOT=.TRUE.
         IF(WC.EQ.'C85') CNOT=.TRUE.
         IF(WC.EQ.'C86') CNOT=.TRUE.
         IF(WC.EQ.'C87') CNOT=.TRUE.
         IF(WC.EQ.'C88') CNOT=.TRUE.
         IF(WC.EQ.'C89') CNOT=.TRUE.
         IF(WC.EQ.'C90') CNOT=.TRUE.
         IF(WC.EQ.'C91') CNOT=.TRUE.
         IF(WC.EQ.'C92') CNOT=.TRUE.
         IF(WC.EQ.'C93') CNOT=.TRUE.
         IF(WC.EQ.'C94') CNOT=.TRUE.
         IF(WC.EQ.'C95') CNOT=.TRUE.
         IF(WC.EQ.'C96') CNOT=.TRUE.
         IF(CNOT) THEN
            OUTLYNE=&
            &'SPECIAL SURFACE TYPE 19'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'ONLY USE COEFFICIENTS C1 THROUGH C4'
            CALL SHOWIT(1)
            CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
            RETURN
         END IF
!     CHECK FOR VALID INPUTS
         IF(WC.EQ.'C1') THEN
            IF(W2.LT.0.0D0 .OR.W1.GT.99.0D0) THEN
               OUTLYNE='FOR A TYPE 19 SPECIAL SURFACE'
               CALL SHOWIT(1)
               OUTLYNE='"C1" MUST BE SET TO AN INTEGER VALUE BETWEEN'
               CALL SHOWIT(1)
               OUTLYNE='1 AND 99'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
            END IF
         END IF
         IF(WC.EQ.'C2') THEN
            IF(W2.LT.5.0D0) THEN
               OUTLYNE='FOR A TYPE 19 SPECIAL SURFACE'
               CALL SHOWIT(1)
               OUTLYNE='"C2" MUST BE SET TO AN INTEGER VALUE GREATER THAN 4'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
            END IF
         END IF
         IF(WC.EQ.'C3') THEN
            IF(W3.LT.0.0D0) THEN
               OUTLYNE='FOR A TYPE 19 SPECIAL SURFACE'
               CALL SHOWIT(1)
               OUTLYNE='"C3" MAY NOT BE SET NEGATIVE'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
            END IF
         END IF
      END IF
      IF(surf_special_type(INT(W1)) == 20) THEN
         IF(WC.EQ.'C6') CNOT=.TRUE.
         IF(WC.EQ.'C7') CNOT=.TRUE.
         IF(WC.EQ.'C8') CNOT=.TRUE.
         IF(WC.EQ.'C9') CNOT=.TRUE.
         IF(WC.EQ.'C10') CNOT=.TRUE.
         IF(WC.EQ.'C11') CNOT=.TRUE.
         IF(WC.EQ.'C12') CNOT=.TRUE.
         IF(WC.EQ.'C13') CNOT=.TRUE.
         IF(WC.EQ.'C14') CNOT=.TRUE.
         IF(WC.EQ.'C15') CNOT=.TRUE.
         IF(WC.EQ.'C16') CNOT=.TRUE.
         IF(WC.EQ.'C17') CNOT=.TRUE.
         IF(WC.EQ.'C18') CNOT=.TRUE.
         IF(WC.EQ.'C19') CNOT=.TRUE.
         IF(WC.EQ.'C20') CNOT=.TRUE.
         IF(WC.EQ.'C21') CNOT=.TRUE.
         IF(WC.EQ.'C22') CNOT=.TRUE.
         IF(WC.EQ.'C23') CNOT=.TRUE.
         IF(WC.EQ.'C24') CNOT=.TRUE.
         IF(WC.EQ.'C25') CNOT=.TRUE.
         IF(WC.EQ.'C26') CNOT=.TRUE.
         IF(WC.EQ.'C27') CNOT=.TRUE.
         IF(WC.EQ.'C28') CNOT=.TRUE.
         IF(WC.EQ.'C29') CNOT=.TRUE.
         IF(WC.EQ.'C30') CNOT=.TRUE.
         IF(WC.EQ.'C31') CNOT=.TRUE.
         IF(WC.EQ.'C32') CNOT=.TRUE.
         IF(WC.EQ.'C33') CNOT=.TRUE.
         IF(WC.EQ.'C34') CNOT=.TRUE.
         IF(WC.EQ.'C35') CNOT=.TRUE.
         IF(WC.EQ.'C36') CNOT=.TRUE.
         IF(WC.EQ.'C37') CNOT=.TRUE.
         IF(WC.EQ.'C38') CNOT=.TRUE.
         IF(WC.EQ.'C39') CNOT=.TRUE.
         IF(WC.EQ.'C40') CNOT=.TRUE.
         IF(WC.EQ.'C41') CNOT=.TRUE.
         IF(WC.EQ.'C42') CNOT=.TRUE.
         IF(WC.EQ.'C43') CNOT=.TRUE.
         IF(WC.EQ.'C44') CNOT=.TRUE.
         IF(WC.EQ.'C45') CNOT=.TRUE.
         IF(WC.EQ.'C46') CNOT=.TRUE.
         IF(WC.EQ.'C47') CNOT=.TRUE.
         IF(WC.EQ.'C48') CNOT=.TRUE.
         IF(WC.EQ.'C49') CNOT=.TRUE.
         IF(WC.EQ.'C50') CNOT=.TRUE.
         IF(WC.EQ.'C51') CNOT=.TRUE.
         IF(WC.EQ.'C52') CNOT=.TRUE.
         IF(WC.EQ.'C53') CNOT=.TRUE.
         IF(WC.EQ.'C54') CNOT=.TRUE.
         IF(WC.EQ.'C55') CNOT=.TRUE.
         IF(WC.EQ.'C56') CNOT=.TRUE.
         IF(WC.EQ.'C57') CNOT=.TRUE.
         IF(WC.EQ.'C58') CNOT=.TRUE.
         IF(WC.EQ.'C59') CNOT=.TRUE.
         IF(WC.EQ.'C60') CNOT=.TRUE.
         IF(WC.EQ.'C61') CNOT=.TRUE.
         IF(WC.EQ.'C62') CNOT=.TRUE.
         IF(WC.EQ.'C63') CNOT=.TRUE.
         IF(WC.EQ.'C64') CNOT=.TRUE.
         IF(WC.EQ.'C65') CNOT=.TRUE.
         IF(WC.EQ.'C66') CNOT=.TRUE.
         IF(WC.EQ.'C67') CNOT=.TRUE.
         IF(WC.EQ.'C68') CNOT=.TRUE.
         IF(WC.EQ.'C69') CNOT=.TRUE.
         IF(WC.EQ.'C70') CNOT=.TRUE.
         IF(WC.EQ.'C71') CNOT=.TRUE.
         IF(WC.EQ.'C72') CNOT=.TRUE.
         IF(WC.EQ.'C73') CNOT=.TRUE.
         IF(WC.EQ.'C74') CNOT=.TRUE.
         IF(WC.EQ.'C75') CNOT=.TRUE.
         IF(WC.EQ.'C76') CNOT=.TRUE.
         IF(WC.EQ.'C77') CNOT=.TRUE.
         IF(WC.EQ.'C78') CNOT=.TRUE.
         IF(WC.EQ.'C79') CNOT=.TRUE.
         IF(WC.EQ.'C80') CNOT=.TRUE.
         IF(WC.EQ.'C81') CNOT=.TRUE.
         IF(WC.EQ.'C82') CNOT=.TRUE.
         IF(WC.EQ.'C83') CNOT=.TRUE.
         IF(WC.EQ.'C84') CNOT=.TRUE.
         IF(WC.EQ.'C85') CNOT=.TRUE.
         IF(WC.EQ.'C86') CNOT=.TRUE.
         IF(WC.EQ.'C87') CNOT=.TRUE.
         IF(WC.EQ.'C88') CNOT=.TRUE.
         IF(WC.EQ.'C89') CNOT=.TRUE.
         IF(WC.EQ.'C90') CNOT=.TRUE.
         IF(WC.EQ.'C91') CNOT=.TRUE.
         IF(WC.EQ.'C92') CNOT=.TRUE.
         IF(WC.EQ.'C93') CNOT=.TRUE.
         IF(WC.EQ.'C94') CNOT=.TRUE.
         IF(WC.EQ.'C95') CNOT=.TRUE.
         IF(WC.EQ.'C96') CNOT=.TRUE.
         IF(CNOT) THEN
            OUTLYNE=&
            &'SPECIAL SURFACE TYPE 20'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'ONLY USE COEFFICIENTS C1 THROUGH C5'
            CALL SHOWIT(1)
            CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(WC.EQ.'C1') THEN
            IF(W2.LT.0.0D0 .OR.W1.GT.99.0D0) THEN
               OUTLYNE='FOR A TYPE 20 SPECIAL SURFACE'
               CALL SHOWIT(1)
               OUTLYNE='"C1" MUST BE SET TO AN INTEGER VALUE BETWEEN'
               CALL SHOWIT(1)
               OUTLYNE='1 AND 99'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
            END IF
         END IF
         IF(WC.EQ.'C2') THEN
            IF(W2.LT.5.0D0) THEN
               OUTLYNE='FOR A TYPE 20 SPECIAL SURFACE'
               CALL SHOWIT(1)
               OUTLYNE='"C2" MUST BE SET TO AN INTEGER VALUE GREATER THAN 4'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
            END IF
         END IF
         IF(WC.EQ.'C3') THEN
            IF(W3.LT.0.0D0) THEN
               OUTLYNE='FOR A TYPE 20 SPECIAL SURFACE'
               CALL SHOWIT(1)
               OUTLYNE='"C3" MAY NOT BE SET NEGATIVE'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
            END IF
         END IF
         IF(WC.EQ.'C4') THEN
            IF(W2.NE.0.0D0 .AND.W2.NE.1.0D0 .AND.W2.NE.2.0D0) THEN
               OUTLYNE='FOR A TYPE 20 SPECIAL SURFACE'
               CALL SHOWIT(1)
               OUTLYNE='"C4" CAN ONLY BE SET TO 0, 1 OR 2'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
            END IF
         END IF
      END IF
      IF(surf_special_type(INT(W1)) == 22) THEN
         IF(WC.EQ.'C5') CNOT=.TRUE.
         IF(WC.EQ.'C6') CNOT=.TRUE.
         IF(WC.EQ.'C7') CNOT=.TRUE.
         IF(WC.EQ.'C8') CNOT=.TRUE.
         IF(WC.EQ.'C9') CNOT=.TRUE.
         IF(WC.EQ.'C10') CNOT=.TRUE.
         IF(WC.EQ.'C11') CNOT=.TRUE.
         IF(WC.EQ.'C12') CNOT=.TRUE.
         IF(WC.EQ.'C13') CNOT=.TRUE.
         IF(WC.EQ.'C14') CNOT=.TRUE.
         IF(WC.EQ.'C15') CNOT=.TRUE.
         IF(WC.EQ.'C16') CNOT=.TRUE.
         IF(WC.EQ.'C17') CNOT=.TRUE.
         IF(WC.EQ.'C18') CNOT=.TRUE.
         IF(WC.EQ.'C19') CNOT=.TRUE.
         IF(WC.EQ.'C20') CNOT=.TRUE.
         IF(WC.EQ.'C21') CNOT=.TRUE.
         IF(WC.EQ.'C22') CNOT=.TRUE.
         IF(WC.EQ.'C23') CNOT=.TRUE.
         IF(WC.EQ.'C24') CNOT=.TRUE.
         IF(WC.EQ.'C25') CNOT=.TRUE.
         IF(WC.EQ.'C26') CNOT=.TRUE.
         IF(WC.EQ.'C27') CNOT=.TRUE.
         IF(WC.EQ.'C28') CNOT=.TRUE.
         IF(WC.EQ.'C29') CNOT=.TRUE.
         IF(WC.EQ.'C30') CNOT=.TRUE.
         IF(WC.EQ.'C31') CNOT=.TRUE.
         IF(WC.EQ.'C32') CNOT=.TRUE.
         IF(WC.EQ.'C33') CNOT=.TRUE.
         IF(WC.EQ.'C34') CNOT=.TRUE.
         IF(WC.EQ.'C35') CNOT=.TRUE.
         IF(WC.EQ.'C36') CNOT=.TRUE.
         IF(WC.EQ.'C37') CNOT=.TRUE.
         IF(WC.EQ.'C38') CNOT=.TRUE.
         IF(WC.EQ.'C39') CNOT=.TRUE.
         IF(WC.EQ.'C40') CNOT=.TRUE.
         IF(WC.EQ.'C41') CNOT=.TRUE.
         IF(WC.EQ.'C42') CNOT=.TRUE.
         IF(WC.EQ.'C43') CNOT=.TRUE.
         IF(WC.EQ.'C44') CNOT=.TRUE.
         IF(WC.EQ.'C45') CNOT=.TRUE.
         IF(WC.EQ.'C46') CNOT=.TRUE.
         IF(WC.EQ.'C47') CNOT=.TRUE.
         IF(WC.EQ.'C48') CNOT=.TRUE.
         IF(WC.EQ.'C49') CNOT=.TRUE.
         IF(WC.EQ.'C50') CNOT=.TRUE.
         IF(WC.EQ.'C51') CNOT=.TRUE.
         IF(WC.EQ.'C52') CNOT=.TRUE.
         IF(WC.EQ.'C53') CNOT=.TRUE.
         IF(WC.EQ.'C54') CNOT=.TRUE.
         IF(WC.EQ.'C55') CNOT=.TRUE.
         IF(WC.EQ.'C56') CNOT=.TRUE.
         IF(WC.EQ.'C57') CNOT=.TRUE.
         IF(WC.EQ.'C58') CNOT=.TRUE.
         IF(WC.EQ.'C59') CNOT=.TRUE.
         IF(WC.EQ.'C60') CNOT=.TRUE.
         IF(WC.EQ.'C61') CNOT=.TRUE.
         IF(WC.EQ.'C62') CNOT=.TRUE.
         IF(WC.EQ.'C63') CNOT=.TRUE.
         IF(WC.EQ.'C64') CNOT=.TRUE.
         IF(WC.EQ.'C65') CNOT=.TRUE.
         IF(WC.EQ.'C66') CNOT=.TRUE.
         IF(WC.EQ.'C67') CNOT=.TRUE.
         IF(WC.EQ.'C68') CNOT=.TRUE.
         IF(WC.EQ.'C69') CNOT=.TRUE.
         IF(WC.EQ.'C70') CNOT=.TRUE.
         IF(WC.EQ.'C71') CNOT=.TRUE.
         IF(WC.EQ.'C72') CNOT=.TRUE.
         IF(WC.EQ.'C73') CNOT=.TRUE.
         IF(WC.EQ.'C74') CNOT=.TRUE.
         IF(WC.EQ.'C75') CNOT=.TRUE.
         IF(WC.EQ.'C76') CNOT=.TRUE.
         IF(WC.EQ.'C77') CNOT=.TRUE.
         IF(WC.EQ.'C78') CNOT=.TRUE.
         IF(WC.EQ.'C79') CNOT=.TRUE.
         IF(WC.EQ.'C80') CNOT=.TRUE.
         IF(WC.EQ.'C81') CNOT=.TRUE.
         IF(WC.EQ.'C82') CNOT=.TRUE.
         IF(WC.EQ.'C83') CNOT=.TRUE.
         IF(WC.EQ.'C84') CNOT=.TRUE.
         IF(WC.EQ.'C85') CNOT=.TRUE.
         IF(WC.EQ.'C86') CNOT=.TRUE.
         IF(WC.EQ.'C87') CNOT=.TRUE.
         IF(WC.EQ.'C88') CNOT=.TRUE.
         IF(WC.EQ.'C89') CNOT=.TRUE.
         IF(WC.EQ.'C90') CNOT=.TRUE.
         IF(WC.EQ.'C91') CNOT=.TRUE.
         IF(WC.EQ.'C92') CNOT=.TRUE.
         IF(WC.EQ.'C93') CNOT=.TRUE.
         IF(WC.EQ.'C94') CNOT=.TRUE.
         IF(WC.EQ.'C95') CNOT=.TRUE.
         IF(WC.EQ.'C96') CNOT=.TRUE.
         IF(CNOT) THEN
            OUTLYNE=&
            &'SPECIAL SURFACE TYPE 22'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'ONLY USE COEFFICIENTS C1 THROUGH C4'
            CALL SHOWIT(1)
            CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(WC.EQ.'C1') THEN
            IF(W2.LT.0.0D0 .OR.W1.GT.99.0D0) THEN
               OUTLYNE='FOR A TYPE 22 SPECIAL SURFACE'
               CALL SHOWIT(1)
               OUTLYNE='"C1" MUST BE SET TO AN INTEGER VALUE BETWEEN'
               CALL SHOWIT(1)
               OUTLYNE='1 AND 99'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
            END IF
         END IF
         IF(WC.EQ.'C2') THEN
            IF(W2.LT.5.0D0) THEN
               OUTLYNE='FOR A TYPE 22 SPECIAL SURFACE'
               CALL SHOWIT(1)
               OUTLYNE='"C2" MUST BE SET TO AN INTEGER VALUE GREATER THAN 4'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
            END IF
         END IF
         IF(WC.EQ.'C3') THEN
            IF(W3.LT.0.0D0) THEN
               OUTLYNE='FOR A TYPE 22 SPECIAL SURFACE'
               CALL SHOWIT(1)
               OUTLYNE='"C3" MAY NOT BE SET NEGATIVE'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
            END IF
         END IF
      END IF
      IF(surf_special_type(INT(W1)) == 4) THEN
         IF(WC.NE.'C1'.AND.WC.NE.'C2'.AND.WC.NE.'C3'&
         &.AND.WC.NE.'C4'.AND.WC.NE.'C5'.AND.WC.NE.'C6'&
         &.AND.WC.NE.'C7'.AND.WC.NE.'C8'.AND.WC.NE.'C9'&
         &.AND.WC.NE.'C10'.AND.WC.NE.'C11'.AND.WC.NE.'C12'&
         &.AND.WC.NE.'C13'.AND.WC.NE.'C14'.AND.WC.NE.'C15')THEN
            OUTLYNE=&
            &'SPECIAL SURFACE TYPE 4 ONLY USES COEFFICIENTS C1 THROUGH C15'
            CALL SHOWIT(1)
            CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
            RETURN
         END IF
      END IF
      IF(surf_special_type(INT(W1)) == 7 .OR.&
      &surf_special_type(INT(W1)) == 8) THEN
         IF(WC.EQ.'C93') CNOT=.TRUE.
         IF(WC.EQ.'C94') CNOT=.TRUE.
         IF(WC.EQ.'C95') CNOT=.TRUE.
         IF(WC.EQ.'C96') CNOT=.TRUE.
         IF(CNOT) THEN
            OUTLYNE=&
            &'SPECIAL SURFACE TYPES 7 AND 8'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'ONLY USE COEFFICIENTS C1 THROUGH C91'
            CALL SHOWIT(1)
            CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
            RETURN
         END IF
      END IF
      IF(surf_special_type(INT(W1)) == 12) THEN
         IF(WC.EQ.'C90') CNOT=.TRUE.
         IF(WC.EQ.'C91') CNOT=.TRUE.
         IF(WC.EQ.'C92') CNOT=.TRUE.
         IF(WC.EQ.'C93') CNOT=.TRUE.
         IF(WC.EQ.'C94') CNOT=.TRUE.
         IF(WC.EQ.'C95') CNOT=.TRUE.
         IF(WC.EQ.'C96') CNOT=.TRUE.
         IF(CNOT) THEN
            OUTLYNE=&
            &'SPECIAL SURFACE TYPE 12'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'ONLY USES COEFFICIENTS C1 THROUGH C89'
            CALL SHOWIT(1)
            CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
            RETURN
         END IF
      END IF
      IF(surf_special_type(INT(W1)) == 13) THEN
         IF(WC.EQ.'C15') CNOT=.TRUE.
         IF(WC.EQ.'C16') CNOT=.TRUE.
         IF(WC.EQ.'C17') CNOT=.TRUE.
         IF(WC.EQ.'C18') CNOT=.TRUE.
         IF(WC.EQ.'C19') CNOT=.TRUE.
         IF(WC.EQ.'C20') CNOT=.TRUE.
         IF(WC.EQ.'C21') CNOT=.TRUE.
         IF(WC.EQ.'C22') CNOT=.TRUE.
         IF(WC.EQ.'C23') CNOT=.TRUE.
         IF(WC.EQ.'C24') CNOT=.TRUE.
         IF(WC.EQ.'C25') CNOT=.TRUE.
         IF(WC.EQ.'C26') CNOT=.TRUE.
         IF(WC.EQ.'C27') CNOT=.TRUE.
         IF(WC.EQ.'C28') CNOT=.TRUE.
         IF(WC.EQ.'C29') CNOT=.TRUE.
         IF(WC.EQ.'C30') CNOT=.TRUE.
         IF(WC.EQ.'C31') CNOT=.TRUE.
         IF(WC.EQ.'C32') CNOT=.TRUE.
         IF(WC.EQ.'C33') CNOT=.TRUE.
         IF(WC.EQ.'C34') CNOT=.TRUE.
         IF(WC.EQ.'C35') CNOT=.TRUE.
         IF(WC.EQ.'C36') CNOT=.TRUE.
         IF(WC.EQ.'C37') CNOT=.TRUE.
         IF(WC.EQ.'C38') CNOT=.TRUE.
         IF(WC.EQ.'C39') CNOT=.TRUE.
         IF(WC.EQ.'C40') CNOT=.TRUE.
         IF(WC.EQ.'C41') CNOT=.TRUE.
         IF(WC.EQ.'C42') CNOT=.TRUE.
         IF(WC.EQ.'C43') CNOT=.TRUE.
         IF(WC.EQ.'C44') CNOT=.TRUE.
         IF(WC.EQ.'C45') CNOT=.TRUE.
         IF(WC.EQ.'C46') CNOT=.TRUE.
         IF(WC.EQ.'C47') CNOT=.TRUE.
         IF(WC.EQ.'C48') CNOT=.TRUE.
         IF(WC.EQ.'C49') CNOT=.TRUE.
         IF(WC.EQ.'C50') CNOT=.TRUE.
         IF(WC.EQ.'C51') CNOT=.TRUE.
         IF(WC.EQ.'C52') CNOT=.TRUE.
         IF(WC.EQ.'C53') CNOT=.TRUE.
         IF(WC.EQ.'C54') CNOT=.TRUE.
         IF(WC.EQ.'C55') CNOT=.TRUE.
         IF(WC.EQ.'C56') CNOT=.TRUE.
         IF(WC.EQ.'C57') CNOT=.TRUE.
         IF(WC.EQ.'C58') CNOT=.TRUE.
         IF(WC.EQ.'C59') CNOT=.TRUE.
         IF(WC.EQ.'C60') CNOT=.TRUE.
         IF(WC.EQ.'C61') CNOT=.TRUE.
         IF(WC.EQ.'C62') CNOT=.TRUE.
         IF(WC.EQ.'C63') CNOT=.TRUE.
         IF(WC.EQ.'C64') CNOT=.TRUE.
         IF(WC.EQ.'C65') CNOT=.TRUE.
         IF(WC.EQ.'C66') CNOT=.TRUE.
         IF(WC.EQ.'C67') CNOT=.TRUE.
         IF(WC.EQ.'C68') CNOT=.TRUE.
         IF(WC.EQ.'C69') CNOT=.TRUE.
         IF(WC.EQ.'C70') CNOT=.TRUE.
         IF(WC.EQ.'C71') CNOT=.TRUE.
         IF(WC.EQ.'C72') CNOT=.TRUE.
         IF(WC.EQ.'C73') CNOT=.TRUE.
         IF(WC.EQ.'C74') CNOT=.TRUE.
         IF(WC.EQ.'C75') CNOT=.TRUE.
         IF(WC.EQ.'C76') CNOT=.TRUE.
         IF(WC.EQ.'C77') CNOT=.TRUE.
         IF(WC.EQ.'C78') CNOT=.TRUE.
         IF(WC.EQ.'C79') CNOT=.TRUE.
         IF(WC.EQ.'C80') CNOT=.TRUE.
         IF(WC.EQ.'C81') CNOT=.TRUE.
         IF(WC.EQ.'C82') CNOT=.TRUE.
         IF(WC.EQ.'C83') CNOT=.TRUE.
         IF(WC.EQ.'C84') CNOT=.TRUE.
         IF(WC.EQ.'C85') CNOT=.TRUE.
         IF(WC.EQ.'C86') CNOT=.TRUE.
         IF(WC.EQ.'C87') CNOT=.TRUE.
         IF(WC.EQ.'C88') CNOT=.TRUE.
         IF(WC.EQ.'C89') CNOT=.TRUE.
         IF(WC.EQ.'C90') CNOT=.TRUE.
         IF(WC.EQ.'C91') CNOT=.TRUE.
         IF(WC.EQ.'C92') CNOT=.TRUE.
         IF(WC.EQ.'C93') CNOT=.TRUE.
         IF(WC.EQ.'C94') CNOT=.TRUE.
         IF(WC.EQ.'C95') CNOT=.TRUE.
         IF(WC.EQ.'C96') CNOT=.TRUE.
         IF(CNOT) THEN
            OUTLYNE=&
            &'SPECIAL SURFACE TYPE 13'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'ONLY USES COEFFICIENTS C1 THROUGH C14'
            CALL SHOWIT(1)
            CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
            RETURN
         END IF
      END IF
      IF(WC.EQ.'C1') FTFL01(1,INT(W1))=W2
      IF(WC.EQ.'C2') FTFL01(2,INT(W1))=W2
      IF(WC.EQ.'C3') FTFL01(3,INT(W1))=W2
      IF(WC.EQ.'C4') FTFL01(4,INT(W1))=W2
      IF(WC.EQ.'C5') FTFL01(5,INT(W1))=W2
      IF(WC.EQ.'C6') FTFL01(6,INT(W1))=W2
      IF(WC.EQ.'C7') FTFL01(7,INT(W1))=W2
      IF(WC.EQ.'C8') FTFL01(8,INT(W1))=W2
      IF(WC.EQ.'C9') FTFL01(9,INT(W1))=W2
      IF(WC.EQ.'C10') FTFL01(10,INT(W1))=W2
      IF(WC.EQ.'C11') FTFL01(11,INT(W1))=W2
      IF(WC.EQ.'C12') FTFL01(12,INT(W1))=W2
      IF(WC.EQ.'C13') FTFL01(13,INT(W1))=W2
      IF(WC.EQ.'C14') FTFL01(14,INT(W1))=W2
      IF(WC.EQ.'C15') FTFL01(15,INT(W1))=W2
      IF(WC.EQ.'C16') FTFL01(16,INT(W1))=W2
      IF(WC.EQ.'C17') FTFL01(17,INT(W1))=W2
      IF(WC.EQ.'C18') FTFL01(18,INT(W1))=W2
      IF(WC.EQ.'C19') FTFL01(19,INT(W1))=W2
      IF(WC.EQ.'C20') FTFL01(20,INT(W1))=W2
      IF(WC.EQ.'C21') FTFL01(21,INT(W1))=W2
      IF(WC.EQ.'C22') FTFL01(22,INT(W1))=W2
      IF(WC.EQ.'C23') FTFL01(23,INT(W1))=W2
      IF(WC.EQ.'C24') FTFL01(24,INT(W1))=W2
      IF(WC.EQ.'C25') FTFL01(25,INT(W1))=W2
      IF(WC.EQ.'C26') FTFL01(26,INT(W1))=W2
      IF(WC.EQ.'C27') FTFL01(27,INT(W1))=W2
      IF(WC.EQ.'C28') FTFL01(28,INT(W1))=W2
      IF(WC.EQ.'C29') FTFL01(29,INT(W1))=W2
      IF(WC.EQ.'C30') FTFL01(30,INT(W1))=W2
      IF(WC.EQ.'C31') FTFL01(31,INT(W1))=W2
      IF(WC.EQ.'C32') FTFL01(32,INT(W1))=W2
      IF(WC.EQ.'C33') FTFL01(33,INT(W1))=W2
      IF(WC.EQ.'C34') FTFL01(34,INT(W1))=W2
      IF(WC.EQ.'C35') FTFL01(35,INT(W1))=W2
      IF(WC.EQ.'C36') FTFL01(36,INT(W1))=W2
      IF(WC.EQ.'C37') FTFL01(37,INT(W1))=W2
      IF(WC.EQ.'C38') FTFL01(38,INT(W1))=W2
      IF(WC.EQ.'C39') FTFL01(39,INT(W1))=W2
      IF(WC.EQ.'C40') FTFL01(40,INT(W1))=W2
      IF(WC.EQ.'C41') FTFL01(41,INT(W1))=W2
      IF(WC.EQ.'C42') FTFL01(42,INT(W1))=W2
      IF(WC.EQ.'C43') FTFL01(43,INT(W1))=W2
      IF(WC.EQ.'C44') FTFL01(44,INT(W1))=W2
      IF(WC.EQ.'C45') FTFL01(45,INT(W1))=W2
      IF(WC.EQ.'C46') FTFL01(46,INT(W1))=W2
      IF(WC.EQ.'C47') FTFL01(47,INT(W1))=W2
      IF(WC.EQ.'C48') FTFL01(48,INT(W1))=W2
      IF(WC.EQ.'C49') FTFL01(49,INT(W1))=W2
      IF(WC.EQ.'C50') FTFL01(50,INT(W1))=W2
      IF(WC.EQ.'C51') FTFL01(51,INT(W1))=W2
      IF(WC.EQ.'C52') FTFL01(52,INT(W1))=W2
      IF(WC.EQ.'C53') FTFL01(53,INT(W1))=W2
      IF(WC.EQ.'C54') FTFL01(54,INT(W1))=W2
      IF(WC.EQ.'C55') FTFL01(55,INT(W1))=W2
      IF(WC.EQ.'C56') FTFL01(56,INT(W1))=W2
      IF(WC.EQ.'C57') FTFL01(57,INT(W1))=W2
      IF(WC.EQ.'C58') FTFL01(58,INT(W1))=W2
      IF(WC.EQ.'C59') FTFL01(59,INT(W1))=W2
      IF(WC.EQ.'C60') FTFL01(60,INT(W1))=W2
      IF(WC.EQ.'C61') FTFL01(61,INT(W1))=W2
      IF(WC.EQ.'C62') FTFL01(62,INT(W1))=W2
      IF(WC.EQ.'C63') FTFL01(63,INT(W1))=W2
      IF(WC.EQ.'C64') FTFL01(64,INT(W1))=W2
      IF(WC.EQ.'C65') FTFL01(65,INT(W1))=W2
      IF(WC.EQ.'C66') FTFL01(66,INT(W1))=W2
      IF(WC.EQ.'C67') FTFL01(67,INT(W1))=W2
      IF(WC.EQ.'C68') FTFL01(68,INT(W1))=W2
      IF(WC.EQ.'C69') FTFL01(69,INT(W1))=W2
      IF(WC.EQ.'C70') FTFL01(70,INT(W1))=W2
      IF(WC.EQ.'C71') FTFL01(71,INT(W1))=W2
      IF(WC.EQ.'C72') FTFL01(72,INT(W1))=W2
      IF(WC.EQ.'C73') FTFL01(73,INT(W1))=W2
      IF(WC.EQ.'C74') FTFL01(74,INT(W1))=W2
      IF(WC.EQ.'C75') FTFL01(75,INT(W1))=W2
      IF(WC.EQ.'C76') FTFL01(76,INT(W1))=W2
      IF(WC.EQ.'C77') FTFL01(77,INT(W1))=W2
      IF(WC.EQ.'C78') FTFL01(78,INT(W1))=W2
      IF(WC.EQ.'C79') FTFL01(79,INT(W1))=W2
      IF(WC.EQ.'C80') FTFL01(80,INT(W1))=W2
      IF(WC.EQ.'C81') FTFL01(81,INT(W1))=W2
      IF(WC.EQ.'C82') FTFL01(82,INT(W1))=W2
      IF(WC.EQ.'C83') FTFL01(83,INT(W1))=W2
      IF(WC.EQ.'C84') FTFL01(84,INT(W1))=W2
      IF(WC.EQ.'C85') FTFL01(85,INT(W1))=W2
      IF(WC.EQ.'C86') FTFL01(86,INT(W1))=W2
      IF(WC.EQ.'C87') FTFL01(87,INT(W1))=W2
      IF(WC.EQ.'C88') FTFL01(88,INT(W1))=W2
      IF(WC.EQ.'C89') FTFL01(89,INT(W1))=W2
      IF(WC.EQ.'C90') FTFL01(90,INT(W1))=W2
      IF(WC.EQ.'C91') FTFL01(91,INT(W1))=W2
      IF(WC.EQ.'C92') FTFL01(92,INT(W1))=W2
      IF(WC.EQ.'C93') FTFL01(93,INT(W1))=W2
      IF(WC.EQ.'C94') FTFL01(94,INT(W1))=W2
      IF(WC.EQ.'C95') FTFL01(95,INT(W1))=W2
      IF(WC.EQ.'C96') FTFL01(96,INT(W1))=W2
      IF(PIKUP(1,INT(W1),11).GT.0.0D0) THEN
         PIKUP(1,INT(W1),11)=0.0D0
         PIKUP(2,INT(W1),11)=0.0D0
         PIKUP(3,INT(W1),11)=0.0D0
         PIKUP(4,INT(W1),11)=0.0D0
         PIKUP(5,INT(W1),11)=0.0D0
         PIKUP(6,INT(W1),11)=0.0D0
         call set_surf_pickup_count(INT(W1), surf_pickup_count(INT(W1)) - 1)
         WRITE(OUTLYNE,*)'SURFACE',INT(W1),' :PIKUP (PRO) DELETED'
         CALL SHOWIT(1)
      END IF
      IF(PIKUP(1,INT(W1),12).GT.0.0D0) THEN
         PIKUP(1,INT(W1),12)=0.0D0
         PIKUP(2,INT(W1),12)=0.0D0
         PIKUP(3,INT(W1),12)=0.0D0
         PIKUP(4,INT(W1),12)=0.0D0
         PIKUP(5,INT(W1),12)=0.0D0
         PIKUP(6,INT(W1),12)=0.0D0
         call set_surf_pickup_count(INT(W1), surf_pickup_count(INT(W1)) - 1)
         WRITE(OUTLYNE,*)'SURFACE',INT(W1),' :PIKUP (NPRO) DELETED'
         CALL SHOWIT(1)
      END IF
!
!       ARE THERE MORE PIKUPS? IF NOT SET ALENS(32,INT(W1)) TO ZERO.
!
      PIKCNT=0
      DO 10 I=1,PSIZ
         IF(PIKUP(1,INT(W1),I).NE.0.0D0) THEN
            PIKCNT=PIKCNT+1
         ELSE
         END IF
10    CONTINUE
!
      IF(PIKCNT.EQ.0) call set_surf_pickup_count(INT(W1), 0)
   ELSE
!       NO ACTION TAKEN
   END IF
   RETURN
END
! SUB ZERNREPT.FOR
SUBROUTINE ZERNREPT
!
   use DATLEN
   use mod_system, only: sys_last_surf
   use DATMAI
   use mod_surface, only: surf_special_type
   use command_utils, only: is_command_query
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE ZERNREPT THAT DOES THE ZERNREPT COMMAND
!
   real(real64) HIGHORD,VCF(1:37),EMN,NV &
   &,VC1,VC2,VC3,VC4,VC5,VC6,VC7,VC8,VC9
!
   INTEGER I
!
!
!
!               THIS IS A CMD LEVEL COMMAND ONLY
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(is_command_query()) THEN
      WRITE(OUTLYNE,*)&
      &'"ZERNREPT" GENERATES A REPORT FOR 37-TERM FRINGE ZERNIKE'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)&
      &'SURFACES (TYPE 3 AND TYPE 10 SPECIAL SURFACES)'
      CALL SHOWIT(1)
      RETURN
   END IF
   IF(SST.EQ.1 .OR.S2.EQ.1 .OR.S3.EQ.1 .OR.S4.EQ.1 .OR.S5.EQ.1 &
   &.OR.SQ.EQ.1) THEN
      WRITE(OUTLYNE,*)&
      &'"ZERNREPT" ONLY TAKES NUMERIC WORD #1 INPUT'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(W1.LT.0.0D0 .OR.W1.GT.sys_last_surf()) THEN
      WRITE(OUTLYNE,*)&
      &'SURFACE NUMBER BEYOND LEGAL RANGE '
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(surf_special_type(INT(W1)).NE.3.0D0 .AND.surf_special_type(INT(W1))&
   &.NE.10.0D0) THEN
      WRITE(OUTLYNE,*)&
      &'SPECIFIED SURFACE IS NOT A TYPE 3 OR TYPE 10 SPECIAL SURFACE'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'NO ZERN REPORT WILL BE GENERATED'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   WRITE(OUTLYNE,6)
6  FORMAT('  ')
   CALL SHOWIT(0)
   WRITE(OUTLYNE,5)
5  FORMAT('37-TERN FRINGE ZERNIKE REPORT')
   IF(surf_special_type(INT(W1)).EQ.3.0D0) WRITE(OUTLYNE,10) INT(W1)
   IF(surf_special_type(INT(W1)).EQ.10.0D0) WRITE(OUTLYNE,20) INT(W1)
10 FORMAT('SURFACE ',I3,' IS A ZERNIKE SURFACE DEFORMATION SURFACE')
20 FORMAT('SURFACE ',I3,' IS A ZERNIKE WAVEFRONT PHASE SURFACE')
   CALL SHOWIT(0)
   IF(surf_special_type(INT(W1)).EQ.3.0D0) WRITE(OUTLYNE,30)
   IF(surf_special_type(INT(W1)).EQ.10.0D0) WRITE(OUTLYNE,40)
30 FORMAT('RMS SURFACE ERROR, WAVES (AS MEASURED OR FIT)')
40 FORMAT('RMS WAVEFRONT ERROR, WAVES (AS MEASURED OR FIT)')
   CALL SHOWIT(0)
   NV=0.0D0
   EMN=2.0D0
   VCF(1)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(1,INT(W1))**2)
   NV=1.0D0
   EMN=1.0D0
   VCF(2)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(2,INT(W1))**2)
   NV=1.0D0
   EMN=1.0D0
   VCF(3)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(3,INT(W1))**2)
   NV=2.0D0
   EMN=2.0D0
   VCF(4)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(4,INT(W1))**2)
   NV=2.0D0
   EMN=1.0D0
   VCF(5)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(5,INT(W1))**2)
   NV=2.0D0
   EMN=1.0D0
   VCF(6)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(6,INT(W1))**2)
   NV=3.0D0
   EMN=1.0D0
   VCF(7)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(7,INT(W1))**2)
   NV=3.0D0
   EMN=1.0D0
   VCF(8)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(8,INT(W1))**2)
   NV=4.0D0
   EMN=2.0D0
   VCF(9)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(9,INT(W1))**2)
   NV=3.0D0
   EMN=1.0D0
   VCF(10)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(10,INT(W1))**2)
   NV=3.0D0
   EMN=1.0D0
   VCF(11)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(11,INT(W1))**2)
   NV=4.0D0
   EMN=1.0D0
   VCF(12)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(12,INT(W1))**2)
   NV=4.0D0
   EMN=1.0D0
   VCF(13)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(13,INT(W1))**2)
   NV=5.0D0
   EMN=1.0D0
   VCF(14)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(14,INT(W1))**2)
   NV=5.0D0
   EMN=1.0D0
   VCF(15)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(15,INT(W1))**2)
   NV=6.0D0
   EMN=2.0D0
   VCF(16)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(16,INT(W1))**2)
   NV=4.0D0
   EMN=1.0D0
   VCF(17)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(17,INT(W1))**2)
   NV=4.0D0
   EMN=1.0D0
   VCF(18)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(18,INT(W1))**2)
   NV=5.0D0
   EMN=1.0D0
   VCF(19)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(19,INT(W1))**2)
   NV=5.0D0
   EMN=1.0D0
   VCF(20)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(20,INT(W1))**2)
   NV=6.0D0
   EMN=1.0D0
   VCF(21)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(21,INT(W1))**2)
   NV=6.0D0
   EMN=1.0D0
   VCF(22)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(22,INT(W1))**2)
   NV=7.0D0
   EMN=1.0D0
   VCF(23)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(23,INT(W1))**2)
   NV=7.0D0
   EMN=1.0D0
   VCF(24)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(24,INT(W1))**2)
   NV=8.0D0
   EMN=1.0D0
   VCF(25)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(25,INT(W1))**2)
   NV=5.0D0
   EMN=1.0D0
   VCF(26)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(26,INT(W1))**2)
   NV=5.0D0
   EMN=1.0D0
   VCF(27)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(27,INT(W1))**2)
   NV=6.0D0
   EMN=1.0D0
   VCF(28)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(28,INT(W1))**2)
   NV=6.0D0
   EMN=1.0D0
   VCF(29)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(29,INT(W1))**2)
   NV=7.0D0
   EMN=1.0D0
   VCF(30)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(30,INT(W1))**2)
   NV=7.0D0
   EMN=1.0D0
   VCF(31)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(31,INT(W1))**2)
   NV=8.0D0
   EMN=1.0D0
   VCF(32)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(32,INT(W1))**2)
   NV=8.0D0
   EMN=1.0D0
   VCF(33)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(33,INT(W1))**2)
   NV=9.0D0
   EMN=1.0D0
   VCF(34)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(34,INT(W1))**2)
   NV=9.0D0
   EMN=1.0D0
   VCF(35)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(35,INT(W1))**2)
   NV=10.0D0
   EMN=2.0D0
   VCF(36)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(36,INT(W1))**2)
   NV=12.0D0
   EMN=2.0D0
   VCF(37)=(EMN/(2.0D0*(NV+1.0D0)))*(FTFL01(37,INT(W1))**2)
   VC1=DSQRT(VCF(1))
   VC2=DSQRT(VCF(2))
   VC3=DSQRT(VCF(3))
   VC4=DSQRT(VCF(4))
   VC5=DSQRT(VCF(5))
   VC6=DSQRT(VCF(6))
   VC7=DSQRT(VCF(7))
   VC8=DSQRT(VCF(8))
   VC9=DSQRT(VCF(9))
   WRITE(OUTLYNE,100) VC1
   CALL SHOWIT(0)
100 FORMAT('             CONSTANT TERM = ',G20.12)
   WRITE(OUTLYNE,200) VC2
   CALL SHOWIT(0)
200 FORMAT('          X-AXIS TILT TERM = ',G20.12)
   WRITE(OUTLYNE,300) VC3
   CALL SHOWIT(0)
300 FORMAT('          Y-AXIS TILT TERM = ',G20.12)
   WRITE(OUTLYNE,400) VC4
   CALL SHOWIT(0)
400 FORMAT('                FOCUS TERM = ',G20.12)
   WRITE(OUTLYNE,500) VC5
   CALL SHOWIT(0)
500 FORMAT('   0 OR 90 DEG ASTIG. TERM = ',G20.12)
   WRITE(OUTLYNE,600) VC6
   CALL SHOWIT(0)
600 FORMAT('     +/-45 DEG ASTIG. TERM = ',G20.12)
   WRITE(OUTLYNE,700) VC9
   CALL SHOWIT(0)
700 FORMAT('  3RD ORDER SPHERICAL TERM = ',G20.12)
   WRITE(OUTLYNE,800) VC8
   CALL SHOWIT(0)
800 FORMAT('3RD ORDER X-AXIS COMA TERM = ',G20.12)
   WRITE(OUTLYNE,900) VC7
   CALL SHOWIT(0)
900 FORMAT('3RD ORDER Y-AXIS COMA TERM = ',G20.12)
   HIGHORD=0.0D0
   DO I=10,37
      HIGHORD=HIGHORD+VCF(I)
   END DO
   WRITE(OUTLYNE,1000) DSQRT(HIGHORD)
   CALL SHOWIT(0)
1000 FORMAT('        HIGHER ORDER TERMS = ',G20.12)
   HIGHORD=0.0D0
   DO I=1,37
      HIGHORD=HIGHORD+VCF(I)
   END DO
   WRITE(OUTLYNE,1100) DSQRT(HIGHORD)
   CALL SHOWIT(0)
1100 FORMAT('(TOTAL) ALL TERMS INCLUDED = ',G20.12)
   HIGHORD=0.0D0
   DO I=4,37
      HIGHORD=HIGHORD+VCF(I)
   END DO
   WRITE(OUTLYNE,1200) DSQRT(HIGHORD)
   CALL SHOWIT(0)
1200 FORMAT('(TOTAL) MINUS CONST & TILT = ',G20.12)
   HIGHORD=0.0D0
   DO I=5,37
      HIGHORD=HIGHORD+VCF(I)
   END DO
   WRITE(OUTLYNE,1300)
   CALL SHOWIT(0)
   WRITE(OUTLYNE,1301) DSQRT(HIGHORD)
   CALL SHOWIT(0)
1300 FORMAT('     (TOTAL) MINUS CONST &')
1301 FORMAT('              TILT & FOCUS = ',G20.12)
!
   RETURN
END
! SUB PPRSPR.FOR
SUBROUTINE PPRSPR
!
   use DATLEN
   use mod_system, only: sys_last_surf, sys_units
   use DATMAI
   use mod_surface, only: surf_special_type, surf_toric_flag
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PPRSPR WHICH IMPLEMENTS THE PRSPR
!       SPECIAL SURFACE DATA OUTPUT OPTION AT THE CMD LEVEL
!       THE ONLY VALID QUALIFIERS ARE (BLANK) AND "ALL","OB" AND "OBJ"
!       THE NUMERIC ENTRY W1, IS THE SURFACE DESIGNATOR
!       FOR PRINTOUT OF ONE SURFACE'S DATA ONLY.
!
   CHARACTER TYPE*9,TYPE2*80,UN*6,REALL*8
!
   INTEGER SPSCNT,I,JK,ITY
!
   real(real64) COEF(1:96)
!
!
   IF(sys_units().EQ.1) UN='INCHES'
   IF(sys_units().EQ.2) UN='CM'
   IF(sys_units().EQ.3) UN='MM'
   IF(sys_units().EQ.4) UN='METERS'
!
!               THIS IS A CMD LEVEL COMMAND ONLY
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SST.EQ.1 .OR.S2.EQ.1 .OR.S3.EQ.1 .OR.S4.EQ.1 .OR.S5.EQ.1) THEN
      WRITE(OUTLYNE,*)&
      &'"PRSPR" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.1 .AND.S1.EQ.1) THEN
      WRITE(OUTLYNE,*)&
      &'"PRSPR" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)&
      &'BUT NOT BOTH'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!       W1 DESIGNATES THE SURFACE FOR WHICH THE SPECIAL SURFACE
!       (SPSRF) DATA IS TO BE OUTPUT.
!       IF THE QUALIFIER "ALL" IS USED, THEN THE SPSRF DATA FOR
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
      W1=0.0
   END IF
   IF(SQ.EQ.1 .AND.WQ.NE.'ALL') THEN
!
!       WE HAVE INVALID QUALIFIER
      WRITE(OUTLYNE,*)'INVALID QUALIFIER INPUT'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.0) THEN
!       HANDEL AN INDIVIDUAL SURFACE INCLUDING "OB" AND "OBJ"
      IF(DF1.EQ.1) W1=DBLE(INT(sys_last_surf()))
      SURF=INT(W1)
      IF(SURF.GT.INT(sys_last_surf())) THEN
!       WE HAVE INVALID SURFACE #
         WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!
!       surf_special_type(SURF) CONTAINS THE FLAG FOR SPSRF
!       DATA PRESENT ON A SURFACE
!
      IF(surf_special_type(SURF) == 0) THEN
!       NO SPSRF DATA, WRITE MESSAGE AND RETURN
         WRITE(OUTLYNE,110) SURF
         CALL SHOWIT(0)
         RETURN
      END IF
      TYPE2=AA//AA//AA//AA
!       THERE IS SPECIAL SURFACE DATA
      IF(surf_special_type(SURF) == 1)&
      &TYPE2='RADIAL POLYNOMIAL SURFACE'
      IF(surf_special_type(SURF) == 2)&
      &TYPE2='30 TERM ZERNIKE POLYNOMIAL SURFACE'
      IF(surf_special_type(SURF) == 3)&
      &TYPE2='37 TERM FRINGE ZERNIKE POLYNOMIAL SURFACE'
      IF(surf_special_type(SURF) == 4)&
      &TYPE2='SINUSOIDAL ERROR SURFACE'
      IF(surf_special_type(SURF) == 5)&
      &TYPE2='USER DEFINED SURFACE #1'
      IF(surf_special_type(SURF) == 6)&
      &TYPE2='RADIAL POLYNOMIAL PHASE SURFACE'
      IF(surf_special_type(SURF) == 7)&
      &TYPE2='RECTANGULAR POLYNOMIAL PHASE SURFACE'
      IF(surf_special_type(SURF) == 8)&
      &TYPE2='RECTANGULAR POLYNOMIAL SURFACE'
      IF(surf_special_type(SURF) == 9)&
      &TYPE2='30 TERM ZERNIKE POLYNOMIAL PHASE SURFACE'
      IF(surf_special_type(SURF) == 10)&
      &TYPE2='37 TERM ZERNIKE POLYNOMIAL PHASE SURFACE'
      IF(surf_special_type(SURF) == 11)&
      &TYPE2='37 USER DEFINED PHASE SURFACE'
      IF(surf_special_type(SURF) == 13 .AND.F12.EQ.1)&
      &TYPE2='REAL RAY HOE'
      IF(surf_special_type(SURF) == 12 .AND.FTFL01(11,SURF).EQ.0.0D0)&
      &TYPE2='HOE WITHOUT ADDITIONAL PHASE TERMS'
      IF(surf_special_type(SURF) == 12 .AND.FTFL01(11,SURF).EQ.1.0D0)&
      &TYPE2='HOE WITH RADIAL POLYNOMIAL PHASE TERMS (R)'
      IF(surf_special_type(SURF) == 12 .AND.FTFL01(11,SURF).EQ.2.0D0)&
      &TYPE2='HOE WITH RECTANGULAR POLYNOMIAL PHASE TERMS (XY)'
      IF(surf_special_type(SURF) == 12 .AND.FTFL01(11,SURF).EQ.3.0D0)&
      &TYPE2='HOE WITH RECTANGULAR POLYNOMIAL PHASE TERMS (AXY)'
      IF(surf_special_type(SURF) == 12 .AND.FTFL01(11,SURF).EQ.4.0D0)&
      &TYPE2='HOE WITH RECTANGULAR POLYNOMIAL PHASE TERMS (XAY)'
      IF(surf_special_type(SURF) == 12 .AND.FTFL01(11,SURF).EQ.5.0D0)&
      &TYPE2='HOE WITH RECTANGULAR POLYNOMIAL PHASE TERMS (XYA)'
      IF(surf_special_type(SURF) == 12 .AND.FTFL01(11,SURF).EQ.6.0D0)&
      &TYPE2='HOE WITH RADIAL POLYNOMIAL PHASE TERMS (AR)'
      IF(surf_special_type(SURF) == 12 .AND.FTFL01(11,SURF).EQ.7.0D0)&
      &TYPE2='HOE WITH USER DEFINED PHASE (MACRO FUNCTION-FUN09)'
      IF(surf_special_type(SURF) == 14)&
      &TYPE2='ABERRATION POLYNOMIAL SURFACE'
      IF(surf_special_type(SURF) == 15)&
      &TYPE2='ABERRATION POLYNOMIAL PHASE SURFACE'
      IF(surf_special_type(SURF) == 16 .AND.surf_toric_flag(SURF) == 0)&
      &TYPE2='FRESNEL-1 SURFACE (ROTATIONALLY SYMMETRIC TYPE)'
      IF(surf_special_type(SURF) == 16 .AND.surf_toric_flag(SURF) == 1)&
      &TYPE2='FRESNEL-1 SURFACE (Y-TORIC CYLINDER TYPE)'
      IF(surf_special_type(SURF) == 16 .AND.surf_toric_flag(SURF) == 2)&
      &TYPE2='FRESNEL-1 SURFACE (X-TORIC CYLINDER TYPE)'
      IF(surf_special_type(SURF) == 17)&
      &TYPE2='USER DEFINED SURFACE #2'
      IF(surf_special_type(SURF) == 18)&
      &TYPE2='GRAZING INCIDENCE SURFACE'
      IF(surf_special_type(SURF) == 19)&
      &TYPE2='GRID APODIZATION SURFACE'
      IF(surf_special_type(SURF) == 20)&
      &TYPE2='GRID PHASE SURFACE'
      IF(surf_special_type(SURF) == 21)&
      &TYPE2='USER DEFINED SUBROUTINE SURFACE'
      IF(surf_special_type(SURF) == 22)&
      &TYPE2='GRID SAG SURFACE'
      IF(surf_special_type(SURF) == 23)&
      &TYPE2='NORMAL SYMMETRIC CUBIC SPLINE SURFACE'
      ITY=surf_special_type(SURF)
      IF(surf_special_type(SURF).EQ.1.0)  TYPE='1- ON    '
      IF(surf_special_type(SURF).EQ.2.0)  TYPE='2- ON    '
      IF(surf_special_type(SURF).EQ.-1.0) TYPE='1- OFF   '
      IF(surf_special_type(SURF).EQ.-2.0) TYPE='2- OFF   '
      IF(surf_special_type(SURF).EQ.3.0)  TYPE='3- ON    '
      IF(surf_special_type(SURF).EQ.-3.0) TYPE='3- OFF   '
      IF(surf_special_type(SURF).EQ.4.0)  TYPE='4- ON    '
      IF(surf_special_type(SURF).EQ.-4.0) TYPE='4- OFF   '
      IF(surf_special_type(SURF).EQ.5.0)  TYPE='5- ON    '
      IF(surf_special_type(SURF).EQ.-5.0) TYPE='5- OFF   '
      IF(surf_special_type(SURF).EQ.6.0)  TYPE='6- ON    '
      IF(surf_special_type(SURF).EQ.-6.0) TYPE='6- OFF   '
      IF(surf_special_type(SURF).EQ.7.0)  TYPE='7- ON    '
      IF(surf_special_type(SURF).EQ.-7.0) TYPE='7- OFF   '
      IF(surf_special_type(SURF).EQ.8.0)  TYPE='8- ON    '
      IF(surf_special_type(SURF).EQ.-8.0) TYPE='8- OFF   '
      IF(surf_special_type(SURF).EQ.9.0)  TYPE='9- ON    '
      IF(surf_special_type(SURF).EQ.-9.0) TYPE='9- OFF   '
      IF(surf_special_type(SURF).EQ.10.0)  TYPE='10- ON   '
      IF(surf_special_type(SURF).EQ.-10.0) TYPE='10- OFF  '
      IF(surf_special_type(SURF).EQ.11.0)  TYPE='11- ON   '
      IF(surf_special_type(SURF).EQ.-11.0) TYPE='11- OFF  '
      IF(surf_special_type(SURF).EQ.12.0)  TYPE='12- ON   '
      IF(surf_special_type(SURF).EQ.-12.0) TYPE='12- OFF  '
      IF(surf_special_type(SURF).EQ.13.0)  TYPE='13- ON   '
      IF(surf_special_type(SURF).EQ.-13.0) TYPE='13- OFF  '
      IF(surf_special_type(SURF).EQ.14.0)  TYPE='14- ON   '
      IF(surf_special_type(SURF).EQ.-14.0) TYPE='14- OFF  '
      IF(surf_special_type(SURF).EQ.15.0)  TYPE='15- ON   '
      IF(surf_special_type(SURF).EQ.-15.0) TYPE='15- OFF  '
      IF(surf_special_type(SURF).EQ.16.0)  TYPE='16- ON   '
      IF(surf_special_type(SURF).EQ.-16.0) TYPE='16- OFF  '
      IF(surf_special_type(SURF).EQ.17.0)  TYPE='17- ON   '
      IF(surf_special_type(SURF).EQ.-17.0) TYPE='17- OFF  '
      IF(surf_special_type(SURF).EQ.18.0)  TYPE='18- ON   '
      IF(surf_special_type(SURF).EQ.-18.0) TYPE='18- OFF  '
      IF(surf_special_type(SURF).EQ.19.0)  TYPE='19- ON   '
      IF(surf_special_type(SURF).EQ.-19.0) TYPE='19- OFF  '
      IF(surf_special_type(SURF).EQ.20.0)  TYPE='20- ON   '
      IF(surf_special_type(SURF).EQ.-20.0) TYPE='20- OFF  '
      IF(surf_special_type(SURF).EQ.21.0)  TYPE='21- ON   '
      IF(surf_special_type(SURF).EQ.-21.0) TYPE='21- OFF  '
      IF(surf_special_type(SURF).EQ.22.0)  TYPE='22- ON   '
      IF(surf_special_type(SURF).EQ.-22.0) TYPE='22- OFF  '
      IF(surf_special_type(SURF).EQ.23.0)  TYPE='23- ON   '
      IF(surf_special_type(SURF).EQ.-23.0) TYPE='23- OFF  '
      IF(surf_special_type(SURF).EQ.24.0)  TYPE='24- ON   '
      IF(surf_special_type(SURF).EQ.-24.0) TYPE='24- OFF  '
      IF(surf_special_type(SURF) > 24) THEN
         WRITE(OUTLYNE,*)'SPECIAL SURFACE TYPE NOT RECOGNIZED'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'VALID TYPES RANGE FROM 1 TO 24'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!        SET PRINTING ARRAY COEF TO PROPER VALUES
!
      DO 9000 I=1,96
         IF(surf_special_type(SURF) >= 1 .AND.&
         &surf_special_type(SURF) <= 30) THEN
            COEF(I)=FTFL01(I,SURF)
         END IF
9000  CONTINUE
      IF(surf_special_type(SURF) /= 13 .OR.&
      &surf_special_type(SURF) == 13 .AND.F12.EQ.1) THEN
         WRITE(OUTLYNE,101) SURF,TYPE
         CALL SHOWIT(0)
         WRITE(OUTLYNE,102) TYPE2(1:79)
         CALL SHOWIT(0)
      END IF
      IF(ITY.EQ.1 .OR.ITY.EQ.6) THEN
         WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),&
         &COEF(12)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),&
         &COEF(16)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),&
         &COEF(20)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),&
         &COEF(24)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),&
         &COEF(28)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),&
         &COEF(32)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),&
         &COEF(36)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,7041) COEF(37),COEF(38),COEF(39)
         CALL SHOWIT(0)
      END IF
      IF(ITY.EQ.2 .OR.ITY.EQ.9) THEN
         WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),&
         &COEF(4)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),&
         &COEF(8)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),&
         &COEF(12)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),&
         &COEF(16)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),&
         &COEF(20)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),&
         &COEF(24)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),&
         &COEF(28)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),&
         &COEF(32)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),&
         &COEF(36)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),&
         &COEF(40)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),&
         &COEF(44)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),&
         &COEF(48)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,707) COEF(49),COEF(50),COEF(51),&
         &COEF(52)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,708) COEF(53),COEF(54),COEF(55),&
         &COEF(56)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,709) COEF(57),COEF(58),COEF(59),&
         &COEF(60)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,710) COEF(61),COEF(62),COEF(63),&
         &COEF(64)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,7004) COEF(65),COEF(66)
         CALL SHOWIT(0)
      END IF
      IF(ITY.EQ.3 .OR.ITY.EQ.10) THEN





         WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),&
         &COEF(4)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),&
         &COEF(8)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),&
         &COEF(12)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),&
         &COEF(16)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),&
         &COEF(20)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),&
         &COEF(24)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),&
         &COEF(28)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),&
         &COEF(32)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),&
         &COEF(36)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,7042) COEF(37)
         CALL SHOWIT(0)
      END IF
      IF(ITY.EQ.4) THEN
         WRITE(OUTLYNE,31001) COEF(1)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,31002) COEF(2),UN
         CALL SHOWIT(0)
         WRITE(OUTLYNE,31003) COEF(3),UN
         CALL SHOWIT(0)
         WRITE(OUTLYNE,32001) COEF(4)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,32002) COEF(5),UN
         CALL SHOWIT(0)
         WRITE(OUTLYNE,32003) COEF(6),UN
         CALL SHOWIT(0)
         WRITE(OUTLYNE,33001) COEF(7)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,33002) COEF(8),UN
         CALL SHOWIT(0)
         WRITE(OUTLYNE,33003) COEF(9),UN
         CALL SHOWIT(0)
         WRITE(OUTLYNE,34001) COEF(10)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,34002) COEF(11),UN
         CALL SHOWIT(0)
         WRITE(OUTLYNE,34003) COEF(12),UN
         CALL SHOWIT(0)
         WRITE(OUTLYNE,35001) COEF(13)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,35002) COEF(14),UN
         CALL SHOWIT(0)
         WRITE(OUTLYNE,35003) COEF(15),UN
         CALL SHOWIT(0)
      END IF
      IF(ITY.EQ.18) THEN





         WRITE(OUTLYNE,5001) COEF(1),UN
         CALL SHOWIT(0)





         WRITE(OUTLYNE,5002) COEF(2),UN
         CALL SHOWIT(0)





         WRITE(OUTLYNE,5003) COEF(3),UN
         CALL SHOWIT(0)





         WRITE(OUTLYNE,5004) COEF(4),COEF(5),COEF(6),&
         &COEF(7)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,5005) COEF(8),COEF(9),COEF(10),&
         &COEF(11)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,5006) COEF(12),COEF(13),COEF(14),&
         &COEF(15)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,5007) COEF(16),COEF(17),COEF(18)
         CALL SHOWIT(0)
      END IF
      IF(ITY.EQ.19) THEN





         IF(COEF(1).NE.0.0D0)WRITE(OUTLYNE,8001) INT(COEF(1))
         IF(COEF(1).EQ.0.0D0)WRITE(OUTLYNE,8006)
8001     FORMAT('GRID SOURCE FILE # (APGRIDxx.DAT), xx = ',I2)
8006     FORMAT('NO APODIZATION SOURCE GRID FILE HAS BEEN SPECIFIED')
         CALL SHOWIT(0)





         WRITE(OUTLYNE,8002) INT(COEF(2))
8002     FORMAT('APODIZATION GRID DIMENSION NxN, N = ',I5)
         CALL SHOWIT(0)




         COEF(3)=FTFL01(3,SURF)
         WRITE(OUTLYNE,8003) COEF(3)
8003     FORMAT('GRID CLEAR APERTURE(RADIUS) = ',G15.8,1X,A6)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,9030) COEF(4)
9030     FORMAT('APODIZATION SCALE FACTOR = ',G15.8)
         CALL SHOWIT(0)

      END IF
      IF(ITY.EQ.22) THEN





         IF(COEF(1).NE.0.0D0)WRITE(OUTLYNE,8101) INT(COEF(1))
         IF(COEF(1).EQ.0.0D0)WRITE(OUTLYNE,8106)
8101     FORMAT('GRID SAG FILE # (SGGRIDxx.DAT), xx = ',I2)
8106     FORMAT('NO SAG SOURCE GRID FILE HAS BEEN SPECIFIED')
         CALL SHOWIT(0)





         WRITE(OUTLYNE,8102) INT(COEF(2))
8102     FORMAT('SAG GRID DIMENSION NxN, N = ',I5)
         CALL SHOWIT(0)





         COEF(3)=FTFL01(3,SURF)
         WRITE(OUTLYNE,8103) COEF(3)
8103     FORMAT('GRID CLEAR APERTURE(RADIUS) = ',G15.8,1X,A6)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,9050) COEF(4)
9050     FORMAT('SAG SCALE FACTOR = ',G15.8)
         CALL SHOWIT(0)


      END IF
      IF(ITY.EQ.23) THEN
!     NORMAL CUBIC SPLINE




         WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),&
         &COEF(4)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),&
         &COEF(8)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),&
         &COEF(12)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),&
         &COEF(16)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),&
         &COEF(20)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),&
         &COEF(24)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),&
         &COEF(28)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),&
         &COEF(32)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),&
         &COEF(36)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),&
         &COEF(40)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),&
         &COEF(44)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),&
         &COEF(48)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,707) COEF(49),COEF(50),COEF(51),&
         &COEF(52)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,708) COEF(53),COEF(54),COEF(55),&
         &COEF(56)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,709) COEF(57),COEF(58),COEF(59),&
         &COEF(60)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,710) COEF(61),COEF(62),COEF(63),&
         &COEF(64)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,711) COEF(65),COEF(66),COEF(67),&
         &COEF(68)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,712) COEF(69),COEF(70),COEF(71),&
         &COEF(72)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,713) COEF(73),COEF(74),COEF(75),&
         &COEF(76)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,714) COEF(77),COEF(78),COEF(79),&
         &COEF(80)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,715) COEF(81),COEF(82),COEF(83),&
         &COEF(84)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,716) COEF(84),COEF(85),COEF(86),&
         &COEF(87)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,717) COEF(89),COEF(90),COEF(91),&
         &COEF(92)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,718) COEF(93),COEF(94),COEF(95),&
         &COEF(96)
         CALL SHOWIT(0)
      END IF
!
      IF(ITY.EQ.20) THEN





         IF(COEF(1).NE.0.0D0)WRITE(OUTLYNE,9001) INT(COEF(1))
         IF(COEF(1).EQ.0.0D0)WRITE(OUTLYNE,9006)
9001     FORMAT('GRID SOURCE FILE # (PHGRIDxx.DAT), xx = ',I2)
9006     FORMAT('NO PHASE SOURCE GRID FILE HAS BEEN SPECIFIED')
         CALL SHOWIT(0)





         WRITE(OUTLYNE,9002) INT(COEF(2))
9002     FORMAT('PHASE GRID DIMENSION NxN, N = ',I5)
         CALL SHOWIT(0)





         COEF(3)=FTFL01(3,SURF)
         WRITE(OUTLYNE,9003) COEF(3),UN
9003     FORMAT('GRID CLEAR APERTURE(RADIUS) = ',G15.8,1X,A6)
         CALL SHOWIT(0)

         IF(COEF(4).EQ.0.0D0)WRITE(OUTLYNE,9007)
         IF(COEF(4).EQ.1.0D0)WRITE(OUTLYNE,9008) UN
         IF(COEF(4).EQ.2.0D0)WRITE(OUTLYNE,9009)
         IF(COEF(4).GT.3.0D0)WRITE(OUTLYNE,9010)
9007     FORMAT &
         &('INPUT PHASE(OPD) IN WAVES AT THE CURRENT REF. WAVELENGTH')
9008     FORMAT &
         &('INPUT PHASE(OPD) UNITS ARE CURRENT LENS UNITS = ',A6)
9009     FORMAT &
         &('INPUT PHASE(OPD) UNITS ARE MICRONS')
9010     FORMAT &
         &('INVALID UNITS CODE FOUND IN TYPE 20 SPECIAL SURFACE')
         CALL SHOWIT(0)





         WRITE(OUTLYNE,9040) COEF(5)
9040     FORMAT('PHASE SCALE FACTOR = ',G15.8)
         CALL SHOWIT(0)

      END IF
      IF(ITY.EQ.5 .OR.ITY.EQ.11) THEN





         WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),&
         &COEF(4)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),&
         &COEF(8)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),&
         &COEF(12)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),&
         &COEF(16)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),&
         &COEF(20)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),&
         &COEF(24)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),&
         &COEF(28)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),&
         &COEF(32)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),&
         &COEF(36)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),&
         &COEF(40)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),&
         &COEF(44)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),&
         &COEF(48)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,707) COEF(49),COEF(50),COEF(51),&
         &COEF(52)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,708) COEF(53),COEF(54),COEF(55),&
         &COEF(56)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,709) COEF(57),COEF(58),COEF(59),&
         &COEF(60)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,710) COEF(61),COEF(62),COEF(63),&
         &COEF(64)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,711) COEF(65),COEF(66),COEF(67),&
         &COEF(68)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,712) COEF(69),COEF(70),COEF(71),&
         &COEF(72)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,713) COEF(73),COEF(74),COEF(75),&
         &COEF(76)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,714) COEF(77),COEF(78),COEF(79),&
         &COEF(80)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,715) COEF(81),COEF(82),COEF(83),&
         &COEF(84)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,716) COEF(84),COEF(85),COEF(86),&
         &COEF(87)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,717) COEF(89),COEF(90),COEF(91),&
         &COEF(92)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,718) COEF(93),COEF(94),COEF(95),&
         &COEF(96)
         CALL SHOWIT(0)
      END IF
      IF(ITY.EQ.21) THEN





         WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),&
         &COEF(4)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),&
         &COEF(8)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),&
         &COEF(12)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),&
         &COEF(16)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),&
         &COEF(20)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),&
         &COEF(24)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),&
         &COEF(28)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),&
         &COEF(32)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),&
         &COEF(36)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),&
         &COEF(40)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),&
         &COEF(44)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),&
         &COEF(48)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,707) COEF(49),COEF(50),COEF(51),&
         &COEF(52)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,708) COEF(53),COEF(54),COEF(55),&
         &COEF(56)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,709) COEF(57),COEF(58),COEF(59),&
         &COEF(60)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,710) COEF(61),COEF(62),COEF(63),&
         &COEF(64)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,711) COEF(65),COEF(66),COEF(67),&
         &COEF(68)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,712) COEF(69),COEF(70),COEF(71),&
         &COEF(72)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,713) COEF(73),COEF(74),COEF(75),&
         &COEF(76)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,714) COEF(77),COEF(78),COEF(79),&
         &COEF(80)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,715) COEF(81),COEF(82),COEF(83),&
         &COEF(84)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,716) COEF(84),COEF(85),COEF(86),&
         &COEF(87)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,717) COEF(89),COEF(90),COEF(91),&
         &COEF(92)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,718) COEF(93),COEF(94),COEF(95),&
         &COEF(96)
         CALL SHOWIT(0)
      END IF
      IF(ITY.EQ.14 .OR.ITY.EQ.15) THEN





         WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),&
         &COEF(4)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),&
         &COEF(8)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),&
         &COEF(12)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),&
         &COEF(16)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),&
         &COEF(20)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),&
         &COEF(24)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),&
         &COEF(28)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),&
         &COEF(32)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),&
         &COEF(36)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),&
         &COEF(40)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),&
         &COEF(44)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),&
         &COEF(48)
         CALL SHOWIT(0)
      END IF
      IF(ITY.EQ.16) THEN





         WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),&
         &COEF(4)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),&
         &COEF(8)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,401) COEF(9),COEF(10),COEF(11)
         CALL SHOWIT(0)

      END IF
      IF(ITY.EQ.7 .OR.ITY.EQ.8) THEN





         WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),&
         &COEF(4)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),&
         &COEF(8)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),&
         &COEF(12)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),&
         &COEF(16)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),&
         &COEF(20)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),&
         &COEF(24)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),&
         &COEF(28)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),&
         &COEF(32)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),&
         &COEF(36)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),&
         &COEF(40)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),&
         &COEF(44)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),&
         &COEF(48)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,707) COEF(49),COEF(50),COEF(51),&
         &COEF(52)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,708) COEF(53),COEF(54),COEF(55),&
         &COEF(56)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,709) COEF(57),COEF(58),COEF(59),&
         &COEF(60)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,710) COEF(61),COEF(62),COEF(63),&
         &COEF(64)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,711) COEF(65),COEF(66),COEF(67),&
         &COEF(68)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,712) COEF(69),COEF(70),COEF(71),&
         &COEF(72)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,713) COEF(73),COEF(74),COEF(75),&
         &COEF(76)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,714) COEF(77),COEF(78),COEF(79),&
         &COEF(80)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,715) COEF(81),COEF(82),COEF(83),&
         &COEF(84)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,716) COEF(84),COEF(85),COEF(86),&
         &COEF(87)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,7171) COEF(89),COEF(90),COEF(91)
         CALL SHOWIT(0)
      END IF
      IF(ITY.EQ.13 .AND.F12.EQ.1) THEN





         WRITE(OUTLYNE,2201) COEF(1)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2202) COEF(2)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2203) COEF(3),UN
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2204) COEF(4),UN
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2205) COEF(5),UN
         CALL SHOWIT(0)





         IF(COEF(6).GT.0.0D0) REALL='POS.    '
         IF(COEF(6).LT.0.0D0) REALL='NEG.    '
         WRITE(OUTLYNE,2206) INT(COEF(6)),REALL
         CALL SHOWIT(0)
         IF(COEF(7).GT.0.0D0) REALL='REAL    '
         IF(COEF(7).LT.0.0D0) REALL='VIRTUAL '
         IF(COEF(7).GT.0.0D0) COEF(7)=1.0D0
         IF(COEF(7).LT.0.0D0) COEF(7)=-1.0D0
         WRITE(OUTLYNE,2207) INT(COEF(7)),REALL
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2208) COEF(8),UN
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2209) COEF(9),UN
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2210) COEF(10),UN
         CALL SHOWIT(0)





         IF(COEF(11).GT.0.0D0) REALL='POS.   '
         IF(COEF(11).LT.0.0D0) REALL='NEG.   '
         WRITE(OUTLYNE,2211) INT(COEF(11)),REALL
         CALL SHOWIT(0)
         IF(COEF(12).GT.0.0D0) REALL='REAL   '
         IF(COEF(12).LT.0.0D0) REALL='VIRTUAL'
         IF(COEF(12).GT.0.0D0) COEF(12)=1.0D0
         IF(COEF(12).LT.0.0D0) COEF(12)=-1.0D0
         WRITE(OUTLYNE,2212) INT(COEF(12)),REALL
         CALL SHOWIT(0)
         WRITE(OUTLYNE,2213) INT(COEF(13))
         CALL SHOWIT(0)
         WRITE(OUTLYNE,2214) INT(COEF(14))
         CALL SHOWIT(0)
      END IF
      IF(ITY.EQ.12 .AND.COEF(11).EQ.0.0D0) THEN





         WRITE(OUTLYNE,2001) COEF(1)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2002) COEF(2)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2003) COEF(3),UN
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2004) COEF(4),UN
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2005) COEF(5),UN
         CALL SHOWIT(0)





         IF(COEF(6).GT.0.0D0) REALL='REAL    '
         IF(COEF(6).LT.0.0D0) REALL='VIRTUAL '
         IF(COEF(6).GT.0.0D0) COEF(6)=1.0D0
         IF(COEF(6).LT.0.0D0) COEF(6)=-1.0D0
         WRITE(OUTLYNE,2006) INT(COEF(6)),REALL
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2007) COEF(7),UN
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2008) COEF(8),UN
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2009) COEF(9),UN
         CALL SHOWIT(0)





         IF(COEF(10).GT.0.0D0) REALL='REAL   '
         IF(COEF(10).LT.0.0D0) REALL='VIRTUAL'
         IF(COEF(10).GT.0.0D0) COEF(10)=1.0D0
         IF(COEF(10).LT.0.0D0) COEF(10)=-1.0D0
         WRITE(OUTLYNE,2010) INT(COEF(10)),REALL
         CALL SHOWIT(0)
      END IF
      IF(ITY.EQ.12 .AND.COEF(11).EQ.6.0D0) THEN





         WRITE(OUTLYNE,2001) COEF(1)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2002) COEF(2)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2003) COEF(3),UN
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2004) COEF(4),UN
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2005) COEF(5),UN
         CALL SHOWIT(0)





         IF(COEF(6).GT.0.0D0) REALL='REAL    '
         IF(COEF(6).LT.0.0D0) REALL='VIRTUAL '
         IF(COEF(6).GT.0.0D0) COEF(6)=1.0D0
         IF(COEF(6).LT.0.0D0) COEF(6)=-1.0D0
         WRITE(OUTLYNE,2006) INT(COEF(6)),REALL
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2007) COEF(7),UN
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2008) COEF(8),UN
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2009) COEF(9),UN
         CALL SHOWIT(0)





         IF(COEF(10).GT.0.0D0) REALL='REAL   '
         IF(COEF(10).LT.0.0D0) REALL='VIRTUAL'
         IF(COEF(10).GT.0.0D0) COEF(10)=1.0D0
         IF(COEF(10).LT.0.0D0) COEF(10)=-1.0D0
         WRITE(OUTLYNE,2010) INT(COEF(10)),REALL
         CALL SHOWIT(0)
         WRITE(OUTLYNE,4001) COEF(11),COEF(12)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),&
         &COEF(16)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),&
         &COEF(20)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,7001) COEF(21),COEF(22),COEF(23)&
         &,COEF(24)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,7002) COEF(25),COEF(26),COEF(27)&
         &,COEF(28)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,7003) COEF(29),COEF(30),COEF(31)
         CALL SHOWIT(0)
      END IF
      IF(ITY.EQ.12 .AND.COEF(11).EQ.1.0D0) THEN





         WRITE(OUTLYNE,2001) COEF(1)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2002) COEF(2)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2003) COEF(3),UN
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2004) COEF(4),UN
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2005) COEF(5),UN
         CALL SHOWIT(0)





         IF(COEF(6).GT.0.0D0) REALL='REAL    '
         IF(COEF(6).LT.0.0D0) REALL='VIRTUAL '
         IF(COEF(6).GT.0.0D0) COEF(6)=1.0D0
         IF(COEF(6).LT.0.0D0) COEF(6)=-1.0D0
         WRITE(OUTLYNE,2006) INT(COEF(6)),REALL
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2007) COEF(7),UN
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2008) COEF(8),UN
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2009) COEF(9),UN
         CALL SHOWIT(0)





         IF(COEF(10).GT.0.0D0) REALL='REAL   '
         IF(COEF(10).LT.0.0D0) REALL='VIRTUAL'
         IF(COEF(10).GT.0.0D0) COEF(10)=1.0D0
         IF(COEF(10).LT.0.0D0) COEF(10)=-1.0D0
         WRITE(OUTLYNE,2010) INT(COEF(10)),REALL
         CALL SHOWIT(0)
         WRITE(OUTLYNE,4001) COEF(11),COEF(12)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),&
         &COEF(16)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),&
         &COEF(20)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,7005) COEF(21)
         CALL SHOWIT(0)
      END IF
      IF(ITY.EQ.12 .AND.COEF(11).EQ.7.0D0) THEN





         WRITE(OUTLYNE,2001) COEF(1)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2002) COEF(2)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2003) COEF(3),UN
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2004) COEF(4),UN
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2005) COEF(5),UN
         CALL SHOWIT(0)





         IF(COEF(6).GT.0.0D0) REALL='REAL    '
         IF(COEF(6).LT.0.0D0) REALL='VIRTUAL '
         IF(COEF(6).GT.0.0D0) COEF(6)=1.0D0
         IF(COEF(6).LT.0.0D0) COEF(6)=-1.0D0
         WRITE(OUTLYNE,2006) INT(COEF(6)),REALL
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2007) COEF(7),UN
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2008) COEF(8),UN
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2009) COEF(9),UN
         CALL SHOWIT(0)





         IF(COEF(10).GT.0.0D0) REALL='REAL   '
         IF(COEF(10).LT.0.0D0) REALL='VIRTUAL'
         IF(COEF(10).GT.0.0D0) COEF(10)=1.0D0
         IF(COEF(10).LT.0.0D0) COEF(10)=-1.0D0
         WRITE(OUTLYNE,2010) INT(COEF(10)),REALL
         CALL SHOWIT(0)
         WRITE(OUTLYNE,4001) COEF(11),COEF(12)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),&
         &COEF(16)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),&
         &COEF(20)
         CALL SHOWIT(0)



         WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),&
         &COEF(24)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),&
         &COEF(28)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),&
         &COEF(32)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),&
         &COEF(36)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),&
         &COEF(40)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),&
         &COEF(44)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),&
         &COEF(48)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,707) COEF(49),COEF(50),COEF(51),&
         &COEF(52)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,708) COEF(53),COEF(54),COEF(55),&
         &COEF(56)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,709) COEF(57),COEF(58),COEF(59),&
         &COEF(60)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,710) COEF(61),COEF(62),COEF(63),&
         &COEF(64)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,711) COEF(65),COEF(66),COEF(67),&
         &COEF(68)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,712) COEF(69),COEF(70),COEF(71),&
         &COEF(72)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,713) COEF(73),COEF(74),COEF(75),&
         &COEF(76)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,714) COEF(77),COEF(78),COEF(79),&
         &COEF(80)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,715) COEF(81),COEF(82),COEF(83),&
         &COEF(84)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,716) COEF(84),COEF(85),COEF(86),&
         &COEF(87)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,717) COEF(89),COEF(90),COEF(91),&
         &COEF(92)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,718) COEF(93),COEF(94),COEF(95),&
         &COEF(96)
         CALL SHOWIT(0)



      END IF
      IF(ITY.EQ.12 .AND.COEF(11).EQ.2.0D0 .OR.&
      &ITY.EQ.12 .AND.COEF(11).EQ.3.0D0 .OR.&
      &ITY.EQ.12 .AND.COEF(11).EQ.4.0D0 .OR.&
      &ITY.EQ.12 .AND.COEF(11).EQ.5.0D0) THEN





         WRITE(OUTLYNE,2001) COEF(1)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2002) COEF(2)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2003) COEF(3),UN
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2004) COEF(4),UN
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2005) COEF(5),UN
         CALL SHOWIT(0)





         IF(COEF(6).GT.0.0D0) REALL='REAL    '
         IF(COEF(6).LT.0.0D0) REALL='VIRTUAL '
         IF(COEF(6).GT.0.0D0) COEF(6)=1.0D0
         IF(COEF(6).LT.0.0D0) COEF(6)=-1.0D0
         WRITE(OUTLYNE,2006) INT(COEF(6)),REALL
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2007) COEF(7),UN
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2008) COEF(8),UN
         CALL SHOWIT(0)





         WRITE(OUTLYNE,2009) COEF(9),UN
         CALL SHOWIT(0)





         IF(COEF(10).GT.0.0D0) REALL='REAL   '
         IF(COEF(10).LT.0.0D0) REALL='VIRTUAL'
         IF(COEF(10).GT.0.0D0) COEF(10)=1.0D0
         IF(COEF(10).LT.0.0D0) COEF(10)=-1.0D0
         WRITE(OUTLYNE,2010) INT(COEF(10)),REALL
         CALL SHOWIT(0)
         WRITE(OUTLYNE,4001) COEF(11),COEF(12)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),&
         &COEF(16)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),&
         &COEF(20)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),COEF(24)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),&
         &COEF(28)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),&
         &COEF(32)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),&
         &COEF(36)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),&
         &COEF(40)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),&
         &COEF(44)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),&
         &COEF(48)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,707) COEF(49),COEF(50),COEF(51),&
         &COEF(52)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,708) COEF(53),COEF(54),COEF(55),&
         &COEF(56)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,709) COEF(57),COEF(58),COEF(59),&
         &COEF(60)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,710) COEF(61),COEF(62),COEF(63),&
         &COEF(64)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,711) COEF(65),COEF(66),COEF(67),&
         &COEF(68)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,712) COEF(69),COEF(70),COEF(71),&
         &COEF(72)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,713) COEF(73),COEF(74),COEF(75),&
         &COEF(76)
         CALL SHOWIT(0)





         WRITE(OUTLYNE,7141) COEF(77),COEF(78),COEF(79)
         CALL SHOWIT(0)
      END IF
      RETURN
   ELSE
!*********************************************************************
!       SQ=1 AND ONLY ALLOWED WQ BY NOW IS "ALL"
!       HANDEL THE WHOLE LENS
      SPSCNT=0
!
      DO 16 JK=0,INT(sys_last_surf())
         IF(surf_special_type(JK) /= 0) THEN
            SPSCNT=SPSCNT+1
         END IF
16    CONTINUE
      IF(SPSCNT.EQ.0) THEN
         WRITE(OUTLYNE,100)
         CALL SHOWIT(0)
         RETURN
      END IF
      DO JK=0,INT(sys_last_surf())
         IF(surf_special_type(JK) == 13 .AND.F12.EQ.1 .OR.&
         &surf_special_type(JK) /= 13 .AND.surf_special_type(JK) /= 0) THEN
            WRITE(OUTLYNE,1000)
            CALL SHOWIT(0)
         END IF
      END DO
      DO 15 JK=0,INT(sys_last_surf())
!
!       SET SPECIAL SURFACE TYPE
!
         IF(surf_special_type(JK) == 0) THEN
!       NOT A SPECIAL SURFACE TYPE. JUMP TO 15 AND
!       GO TO THE NEXT SURFACE NUMBER
            GO TO 15
         END IF
         TYPE2=AA//AA//AA//AA
!       THERE IS SPECIAL SURFACE DATA
         IF(surf_special_type(JK) == 1)&
         &TYPE2='RADIAL POLYNOMIAL SURFACE'
         IF(surf_special_type(JK) == 2)&
         &TYPE2='30 TERM ZERNIKE POLYNOMIAL SURFACE'
         IF(surf_special_type(JK) == 3)&
         &TYPE2='37 TERM FRINGE ZERNIKE POLYNOMIAL SURFACE'
         IF(surf_special_type(JK) == 4)&
         &TYPE2='SINUSOIDAL ERROR SURFACE'
         IF(surf_special_type(JK) == 5)&
         &TYPE2='USER DEFINED SURFACE #1'
         IF(surf_special_type(JK) == 6)&
         &TYPE2='RADIAL POLYNOMIAL PHASE SURFACE'
         IF(surf_special_type(JK) == 7)&
         &TYPE2='RECTANGULAR POLYNOMIAL PHASE SURFACE'
         IF(surf_special_type(JK) == 8)&
         &TYPE2='RECTANGULAR POLYNOMIAL SURFACE'
         IF(surf_special_type(JK) == 9)&
         &TYPE2='30 TERM ZERNIKE POLYNOMIAL PHASE SURFACE'
         IF(surf_special_type(JK) == 10)&
         &TYPE2='37 TERM ZERNIKE POLYNOMIAL PHASE SURFACE'
         IF(surf_special_type(JK) == 11)&
         &TYPE2='37 USER DEFINED PHASE SURFACE'
         IF(surf_special_type(SURF) == 13 .AND.F12.EQ.1)&
         &TYPE2='REAL RAY HOE'
         IF(surf_special_type(JK) == 12 .AND.FTFL01(11,JK).EQ.0.0D0)&
         &TYPE2='HOE WITHOUT ADDITIONAL PHASE TERMS'
         IF(surf_special_type(JK) == 12 .AND.FTFL01(11,JK).EQ.1.0D0)&
         &TYPE2='HOE WITH RADIAL POLYNOMIAL PHASE TERMS (R)'
         IF(surf_special_type(JK) == 12 .AND.FTFL01(11,JK).EQ.2.0D0)&
         &TYPE2='HOE WITH RECTANGULAR POLYNOMIAL PHASE TERMS (XY)'
         IF(surf_special_type(JK) == 12 .AND.FTFL01(11,JK).EQ.3.0D0)&
         &TYPE2='HOE WITH RECTANGULAR POLYNOMIAL PHASE TERMS (AXY)'
         IF(surf_special_type(JK) == 12 .AND.FTFL01(11,JK).EQ.4.0D0)&
         &TYPE2='HOE WITH RECTANGULAR POLYNOMIAL PHASE TERMS (XAY)'
         IF(surf_special_type(JK) == 12 .AND.FTFL01(11,JK).EQ.5.0D0)&
         &TYPE2='HOE WITH RECTANGULAR POLYNOMIAL PHASE TERMS (XYA)'
         IF(surf_special_type(JK) == 12 .AND.FTFL01(11,JK).EQ.6.0D0)&
         &TYPE2='HOE WITH RADIAL POLYNOMIAL PHASE TERMS (AR)'
         IF(surf_special_type(JK) == 12 .AND.FTFL01(11,SURF).EQ.7.0D0)&
         &TYPE2='HOE WITH USER DEFINED PHASE (MACRO FUNCTION-FUN09)'
         IF(surf_special_type(JK) == 14)&
         &TYPE2='STANDARD ABERRATION POLYNOMIAL PHASE TERMS'
         IF(surf_special_type(JK) == 15)&
         &TYPE2='ABERRATION POLYNOMIAL PHASE SURFACE'
         IF(surf_special_type(JK) == 16)&
         &TYPE2='FRESNEL-1 SURFACE'
         IF(surf_special_type(JK) == 17)&
         &TYPE2='USER DEFINED SURFACE #2'
         IF(surf_special_type(JK) == 18)&
         &TYPE2='GRAZING INCIDENCE SURFACE'
         IF(surf_special_type(JK) == 19)&
         &TYPE2='GRID APODIZATION SURFACE'
         IF(surf_special_type(JK) == 20)&
         &TYPE2='GRID PHASE SURFACE'
         IF(surf_special_type(JK) == 21)&
         &TYPE2='USER DEFINED SUBROUTINE SURFACE'
         IF(surf_special_type(JK) == 22)&
         &TYPE2='GRID SAG SURFACE'
         IF(surf_special_type(JK) == 23)&
         &TYPE2='NORMAL SYMMETRIC CUBIC SPLINE SURFACE'
         ITY=surf_special_type(JK)
         IF(surf_special_type(JK).EQ.1.0)  TYPE='1- ON    '
         IF(surf_special_type(JK).EQ.2.0)  TYPE='2- ON    '
         IF(surf_special_type(JK).EQ.-1.0) TYPE='1- OFF   '
         IF(surf_special_type(JK).EQ.-2.0) TYPE='2- OFF   '
         IF(surf_special_type(JK).EQ.3.0)  TYPE='3- ON    '
         IF(surf_special_type(JK).EQ.-3.0) TYPE='3- OFF   '
         IF(surf_special_type(JK).EQ.4.0)  TYPE='4- ON    '
         IF(surf_special_type(JK).EQ.-4.0) TYPE='4- OFF   '
         IF(surf_special_type(JK).EQ.5.0)  TYPE='5- ON    '
         IF(surf_special_type(JK).EQ.-5.0) TYPE='5- OFF   '
         IF(surf_special_type(JK).EQ.6.0)  TYPE='6- ON    '
         IF(surf_special_type(JK).EQ.-6.0) TYPE='6- OFF   '
         IF(surf_special_type(JK).EQ.7.0)  TYPE='7- ON    '
         IF(surf_special_type(JK).EQ.-7.0) TYPE='7- OFF   '
         IF(surf_special_type(JK).EQ.8.0)  TYPE='8- ON    '
         IF(surf_special_type(JK).EQ.-8.0) TYPE='8- OFF   '
         IF(surf_special_type(JK).EQ.9.0)  TYPE='9- ON    '
         IF(surf_special_type(JK).EQ.-9.0) TYPE='9- OFF   '
         IF(surf_special_type(JK).EQ.10.0)  TYPE='10- ON   '
         IF(surf_special_type(JK).EQ.-10.0) TYPE='10- OFF  '
         IF(surf_special_type(JK).EQ.11.0)  TYPE='11- ON   '
         IF(surf_special_type(JK).EQ.-11.0) TYPE='11- OFF  '
         IF(surf_special_type(JK).EQ.12.0)  TYPE='12- ON   '
         IF(surf_special_type(JK).EQ.-12.0) TYPE='12- OFF  '
         IF(surf_special_type(JK).EQ.13.0)  TYPE='13- ON   '
         IF(surf_special_type(JK).EQ.-13.0) TYPE='13- OFF  '
         IF(surf_special_type(JK).EQ.14.0)  TYPE='14- ON   '
         IF(surf_special_type(JK).EQ.-14.0) TYPE='14- OFF  '
         IF(surf_special_type(JK).EQ.15.0)  TYPE='15- ON   '
         IF(surf_special_type(JK).EQ.-15.0) TYPE='15- OFF  '
         IF(surf_special_type(JK).EQ.16.0)  TYPE='16- ON   '
         IF(surf_special_type(JK).EQ.-16.0) TYPE='16- OFF  '
         IF(surf_special_type(JK).EQ.17.0)  TYPE='17- ON   '
         IF(surf_special_type(JK).EQ.-17.0) TYPE='17- OFF  '
         IF(surf_special_type(JK).EQ.18.0)  TYPE='18- ON   '
         IF(surf_special_type(JK).EQ.-18.0) TYPE='18- OFF  '
         IF(surf_special_type(JK).EQ.19.0)  TYPE='19- ON   '
         IF(surf_special_type(JK).EQ.-19.0) TYPE='19- OFF  '
         IF(surf_special_type(JK).EQ.20.0)  TYPE='20- ON   '
         IF(surf_special_type(JK).EQ.-20.0) TYPE='20- OFF  '
         IF(surf_special_type(JK).EQ.21.0)  TYPE='21- ON   '
         IF(surf_special_type(JK).EQ.-21.0) TYPE='21- OFF  '
         IF(surf_special_type(JK).EQ.22.0)  TYPE='22- ON   '
         IF(surf_special_type(JK).EQ.-22.0) TYPE='22- OFF  '
         IF(surf_special_type(JK).EQ.23.0)  TYPE='23- ON   '
         IF(surf_special_type(JK).EQ.-23.0) TYPE='23- OFF  '
         IF(surf_special_type(JK).EQ.24.0)  TYPE='24- ON   '
         IF(surf_special_type(JK).EQ.-24.0) TYPE='24- OFF  '
         IF(surf_special_type(JK) > 24) THEN




            WRITE(OUTLYNE,*)'SPECIAL SURFACE TYPE NOT RECOGNIZED'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'VALID TYPES RANGE FROM 1 TO 24'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
!        SET PRINTING ARRAY COEF TO PROPER VALUES
!
         DO I=1,96
            IF(surf_special_type(JK) >= 1 .AND.&
            &surf_special_type(JK) <= 30) THEN
               COEF(I)=FTFL01(I,JK)
            END IF
         END DO
         IF(surf_special_type(JK) /= 13 .OR.&
         &surf_special_type(JK) == 13 .AND.F12.EQ.1) THEN
            WRITE(OUTLYNE,101) JK,TYPE
            CALL SHOWIT(0)
         END IF





         WRITE(OUTLYNE,102) TYPE2(1:79)
         CALL SHOWIT(0)
         IF(ITY.EQ.1 .OR.ITY.EQ.6) THEN





            WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),&
            &COEF(12)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),&
            &COEF(16)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),&
            &COEF(20)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),&
            &COEF(24)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),&
            &COEF(28)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),&
            &COEF(32)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),&
            &COEF(36)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,7041) COEF(37),COEF(38),COEF(39)
            CALL SHOWIT(0)
         END IF
         IF(ITY.EQ.2 .OR.ITY.EQ.9) THEN





            WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),&
            &COEF(4)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),&
            &COEF(8)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),&
            &COEF(12)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),&
            &COEF(16)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),&
            &COEF(20)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),&
            &COEF(24)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),&
            &COEF(28)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),&
            &COEF(32)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),&
            &COEF(36)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),&
            &COEF(40)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),&
            &COEF(44)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),&
            &COEF(48)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,707) COEF(49),COEF(50),COEF(51),&
            &COEF(52)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,708) COEF(53),COEF(54),COEF(55),&
            &COEF(56)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,709) COEF(57),COEF(58),COEF(59),&
            &COEF(60)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,710) COEF(61),COEF(62),COEF(63),&
            &COEF(64)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,7004) COEF(65),COEF(66)
            CALL SHOWIT(0)
         END IF
         IF(ITY.EQ.3 .OR.ITY.EQ.10) THEN





            WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),&
            &COEF(4)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),&
            &COEF(8)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),&
            &COEF(12)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),&
            &COEF(16)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),&
            &COEF(20)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),&
            &COEF(24)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),&
            &COEF(28)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),&
            &COEF(32)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),&
            &COEF(36)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,7042) COEF(37)
            CALL SHOWIT(0)
         END IF
         IF(ITY.EQ.4) THEN
            WRITE(OUTLYNE,31001) COEF(1)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,31002) COEF(2),UN
            CALL SHOWIT(0)
            WRITE(OUTLYNE,31003) COEF(3),UN
            CALL SHOWIT(0)
            WRITE(OUTLYNE,32001) COEF(4)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,32002) COEF(5),UN
            CALL SHOWIT(0)
            WRITE(OUTLYNE,32003) COEF(6),UN
            CALL SHOWIT(0)
            WRITE(OUTLYNE,33001) COEF(7)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,33002) COEF(8),UN
            CALL SHOWIT(0)
            WRITE(OUTLYNE,33003) COEF(9),UN
            CALL SHOWIT(0)
            WRITE(OUTLYNE,34001) COEF(10)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,34002) COEF(11),UN
            CALL SHOWIT(0)
            WRITE(OUTLYNE,34003) COEF(12),UN
            CALL SHOWIT(0)
            WRITE(OUTLYNE,35001) COEF(13)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,35002) COEF(14),UN
            CALL SHOWIT(0)
            WRITE(OUTLYNE,35003) COEF(15),UN
            CALL SHOWIT(0)
         END IF
         IF(ITY.EQ.18) THEN





            WRITE(OUTLYNE,5001) COEF(1),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,5002) COEF(2),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,5003) COEF(3),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,5004) COEF(4),COEF(5),COEF(6),&
            &COEF(7)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,5005) COEF(8),COEF(9),COEF(10),&
            &COEF(11)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,5006) COEF(12),COEF(13),COEF(14),&
            &COEF(15)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,5007) COEF(16),COEF(17),COEF(18)
            CALL SHOWIT(0)
         END IF
         IF(ITY.EQ.19) THEN





            IF(COEF(1).NE.0.0D0)WRITE(OUTLYNE,8001) INT(COEF(1))
            IF(COEF(1).EQ.0.0D0)WRITE(OUTLYNE,8006)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,8002) INT(COEF(2))
            CALL SHOWIT(0)





            COEF(3)=FTFL01(3,JK)
            WRITE(OUTLYNE,8003) COEF(3),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,9030) COEF(4)
            CALL SHOWIT(0)

         END IF
         IF(ITY.EQ.22) THEN





            IF(COEF(1).NE.0.0D0)WRITE(OUTLYNE,8101) INT(COEF(1))
            IF(COEF(1).EQ.0.0D0)WRITE(OUTLYNE,8106)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,8102) INT(COEF(2))
            CALL SHOWIT(0)





            COEF(3)=FTFL01(3,JK)
            WRITE(OUTLYNE,8103) COEF(3),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,9050) COEF(4)
            CALL SHOWIT(0)

         END IF
         IF(ITY.EQ.23) THEN
!     NORMAL CUBIC SPLINE




            WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),&
            &COEF(4)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),&
            &COEF(8)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),&
            &COEF(12)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),&
            &COEF(16)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),&
            &COEF(20)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),&
            &COEF(24)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),&
            &COEF(28)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),&
            &COEF(32)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),&
            &COEF(36)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),&
            &COEF(40)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),&
            &COEF(44)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),&
            &COEF(48)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,707) COEF(49),COEF(50),COEF(51),&
            &COEF(52)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,708) COEF(53),COEF(54),COEF(55),&
            &COEF(56)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,709) COEF(57),COEF(58),COEF(59),&
            &COEF(60)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,710) COEF(61),COEF(62),COEF(63),&
            &COEF(64)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,711) COEF(65),COEF(66),COEF(67),&
            &COEF(68)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,712) COEF(69),COEF(70),COEF(71),&
            &COEF(72)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,713) COEF(73),COEF(74),COEF(75),&
            &COEF(76)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,714) COEF(77),COEF(78),COEF(79),&
            &COEF(80)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,715) COEF(81),COEF(82),COEF(83),&
            &COEF(84)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,716) COEF(84),COEF(85),COEF(86),&
            &COEF(87)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,717) COEF(89),COEF(90),COEF(91),&
            &COEF(92)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,718) COEF(93),COEF(94),COEF(95),&
            &COEF(96)
            CALL SHOWIT(0)
         END IF
         IF(ITY.EQ.20) THEN





            IF(COEF(1).NE.0.0D0)WRITE(OUTLYNE,9001) INT(COEF(1))
            IF(COEF(1).EQ.0.0D0)WRITE(OUTLYNE,9006)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,9002) INT(COEF(2))
            CALL SHOWIT(0)





            COEF(3)=FTFL01(3,JK)
            WRITE(OUTLYNE,9003) COEF(3)
            CALL SHOWIT(0)
            IF(COEF(4).EQ.0.0D0)WRITE(OUTLYNE,9007)
            IF(COEF(4).EQ.1.0D0)WRITE(OUTLYNE,9008) UN
            IF(COEF(4).EQ.2.0D0)WRITE(OUTLYNE,9009)
            IF(COEF(4).EQ.3.0D0)WRITE(OUTLYNE,9010)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,9040) COEF(5)
            CALL SHOWIT(0)
         END IF

         IF(ITY.EQ.5 .OR.ITY.EQ.11) THEN





            WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),&
            &COEF(4)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),&
            &COEF(8)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),&
            &COEF(12)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),&
            &COEF(16)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),&
            &COEF(20)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),&
            &COEF(24)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),&
            &COEF(28)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),&
            &COEF(32)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),&
            &COEF(36)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),&
            &COEF(40)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),&
            &COEF(44)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),&
            &COEF(48)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,707) COEF(49),COEF(50),COEF(51),&
            &COEF(52)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,708) COEF(53),COEF(54),COEF(55),&
            &COEF(56)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,709) COEF(57),COEF(58),COEF(59),&
            &COEF(60)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,710) COEF(61),COEF(62),COEF(63),&
            &COEF(64)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,711) COEF(65),COEF(66),COEF(67),&
            &COEF(68)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,712) COEF(69),COEF(70),COEF(71),&
            &COEF(72)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,713) COEF(73),COEF(74),COEF(75),&
            &COEF(76)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,714) COEF(77),COEF(78),COEF(79),&
            &COEF(80)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,715) COEF(81),COEF(82),COEF(83),&
            &COEF(84)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,716) COEF(84),COEF(85),COEF(86),&
            &COEF(87)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,717) COEF(89),COEF(90),COEF(91),&
            &COEF(92)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,718) COEF(93),COEF(94),COEF(95),&
            &COEF(96)
            CALL SHOWIT(0)
         END IF
         IF(ITY.EQ.21) THEN





            WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),&
            &COEF(4)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),&
            &COEF(8)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),&
            &COEF(12)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),&
            &COEF(16)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),&
            &COEF(20)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),&
            &COEF(24)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),&
            &COEF(28)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),&
            &COEF(32)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),&
            &COEF(36)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),&
            &COEF(40)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),&
            &COEF(44)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),&
            &COEF(48)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,707) COEF(49),COEF(50),COEF(51),&
            &COEF(52)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,708) COEF(53),COEF(54),COEF(55),&
            &COEF(56)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,709) COEF(57),COEF(58),COEF(59),&
            &COEF(60)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,710) COEF(61),COEF(62),COEF(63),&
            &COEF(64)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,711) COEF(65),COEF(66),COEF(67),&
            &COEF(68)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,712) COEF(69),COEF(70),COEF(71),&
            &COEF(72)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,713) COEF(73),COEF(74),COEF(75),&
            &COEF(76)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,714) COEF(77),COEF(78),COEF(79),&
            &COEF(80)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,715) COEF(81),COEF(82),COEF(83),&
            &COEF(84)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,716) COEF(84),COEF(85),COEF(86),&
            &COEF(87)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,717) COEF(89),COEF(90),COEF(91),&
            &COEF(92)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,718) COEF(93),COEF(94),COEF(95),&
            &COEF(96)
            CALL SHOWIT(0)
         END IF
         IF(ITY.EQ.14 .OR.ITY.EQ.15) THEN





            WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),&
            &COEF(4)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),&
            &COEF(8)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),&
            &COEF(12)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),&
            &COEF(16)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),&
            &COEF(20)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),&
            &COEF(24)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),&
            &COEF(28)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),&
            &COEF(32)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),&
            &COEF(36)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),&
            &COEF(40)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),&
            &COEF(44)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),&
            &COEF(48)
            CALL SHOWIT(0)
         END IF
         IF(ITY.EQ.16) THEN





            WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),&
            &COEF(4)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),&
            &COEF(8)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,401) COEF(9),COEF(10),COEF(11)
            CALL SHOWIT(0)

         END IF
         IF(ITY.EQ.7 .OR.ITY.EQ.8) THEN





            WRITE(OUTLYNE,200) COEF(1),COEF(2),COEF(3),&
            &COEF(4)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,300) COEF(5),COEF(6),COEF(7),&
            &COEF(8)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,400) COEF(9),COEF(10),COEF(11),&
            &COEF(12)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),&
            &COEF(16)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),&
            &COEF(20)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),&
            &COEF(24)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),&
            &COEF(28)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),&
            &COEF(32)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),&
            &COEF(36)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),&
            &COEF(40)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),&
            &COEF(44)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),&
            &COEF(48)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,707) COEF(49),COEF(50),COEF(51),&
            &COEF(52)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,708) COEF(53),COEF(54),COEF(55),&
            &COEF(56)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,709) COEF(57),COEF(58),COEF(59),&
            &COEF(60)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,710) COEF(61),COEF(62),COEF(63),&
            &COEF(64)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,711) COEF(65),COEF(66),COEF(67),&
            &COEF(68)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,712) COEF(69),COEF(70),COEF(71),&
            &COEF(72)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,713) COEF(73),COEF(74),COEF(75),&
            &COEF(76)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,714) COEF(77),COEF(78),COEF(79),&
            &COEF(80)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,715) COEF(81),COEF(82),COEF(83),&
            &COEF(84)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,716) COEF(84),COEF(85),COEF(86),&
            &COEF(87)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,7171) COEF(89),COEF(90),COEF(91)
            CALL SHOWIT(0)
         END IF
         IF(ITY.EQ.13 .AND.F12.EQ.1) THEN





            WRITE(OUTLYNE,2201) COEF(1)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2202) COEF(2)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2203) COEF(3),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2204) COEF(4),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2205) COEF(5),UN
            CALL SHOWIT(0)





            IF(COEF(6).GT.0.0D0) REALL='POS.    '
            IF(COEF(6).LT.0.0D0) REALL='NEG.    '
            WRITE(OUTLYNE,2206) INT(COEF(6)),REALL
            CALL SHOWIT(0)
            IF(COEF(7).GT.0.0D0) REALL='REAL    '
            IF(COEF(7).LT.0.0D0) REALL='VIRTUAL '
            IF(COEF(7).GT.0.0D0) COEF(7)=1.0D0
            IF(COEF(7).LT.0.0D0) COEF(7)=-1.0D0
            WRITE(OUTLYNE,2207) INT(COEF(7)),REALL
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2208) COEF(8),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2209) COEF(9),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2210) COEF(10),UN
            CALL SHOWIT(0)





            IF(COEF(11).GT.0.0D0) REALL='POS.   '
            IF(COEF(11).LT.0.0D0) REALL='NEG.   '
            WRITE(OUTLYNE,2211) INT(COEF(11)),REALL
            CALL SHOWIT(0)
            IF(COEF(12).GT.0.0D0) REALL='REAL   '
            IF(COEF(12).LT.0.0D0) REALL='VIRTUAL'
            IF(COEF(12).GT.0.0D0) COEF(12)=1.0D0
            IF(COEF(12).LT.0.0D0) COEF(12)=-1.0D0
            WRITE(OUTLYNE,2212) INT(COEF(12)),REALL
            CALL SHOWIT(0)
            WRITE(OUTLYNE,2213) INT(COEF(13))
            CALL SHOWIT(0)
            WRITE(OUTLYNE,2214) INT(COEF(14))
            CALL SHOWIT(0)
         END IF
         IF(ITY.EQ.12 .AND.COEF(11).EQ.0.0D0) THEN





            WRITE(OUTLYNE,2001) COEF(1)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2002) COEF(2)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2003) COEF(3),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2004) COEF(4),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2005) COEF(5),UN
            CALL SHOWIT(0)





            IF(COEF(6).GT.0.0D0) REALL='REAL    '
            IF(COEF(6).LT.0.0D0) REALL='VIRTUAL '
            IF(COEF(6).GT.0.0D0) COEF(6)=1.0D0
            IF(COEF(6).LT.0.0D0) COEF(6)=-1.0D0
            WRITE(OUTLYNE,2006) INT(COEF(6)),REALL
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2007) COEF(7),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2008) COEF(8),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2009) COEF(9),UN
            CALL SHOWIT(0)





            IF(COEF(10).GT.0.0D0) REALL='REAL   '
            IF(COEF(10).LT.0.0D0) REALL='VIRTUAL'
            IF(COEF(10).GT.0.0D0) COEF(10)=1.0D0
            IF(COEF(10).LT.0.0D0) COEF(10)=-1.0D0
            WRITE(OUTLYNE,2010) INT(COEF(10)),REALL
            CALL SHOWIT(0)
         END IF
         IF(ITY.EQ.12 .AND.COEF(11).EQ.6.0D0) THEN





            WRITE(OUTLYNE,2001) COEF(1)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2002) COEF(2)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2003) COEF(3),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2004) COEF(4),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2005) COEF(5),UN
            CALL SHOWIT(0)





            IF(COEF(6).GT.0.0D0) REALL='REAL    '
            IF(COEF(6).LT.0.0D0) REALL='VIRTUAL '
            IF(COEF(6).GT.0.0D0) COEF(6)=1.0D0
            IF(COEF(6).LT.0.0D0) COEF(6)=-1.0D0
            WRITE(OUTLYNE,2006) INT(COEF(6)),REALL
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2007) COEF(7),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2008) COEF(8),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2009) COEF(9),UN
            CALL SHOWIT(0)





            IF(COEF(10).GT.0.0D0) REALL='REAL   '
            IF(COEF(10).LT.0.0D0) REALL='VIRTUAL'
            IF(COEF(10).GT.0.0D0) COEF(10)=1.0D0
            IF(COEF(10).LT.0.0D0) COEF(10)=-1.0D0
            WRITE(OUTLYNE,2010) INT(COEF(10)),REALL
            CALL SHOWIT(0)
            WRITE(OUTLYNE,4001) COEF(11),COEF(12)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),&
            &COEF(16)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),&
            &COEF(20)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,7005) COEF(21)
            CALL SHOWIT(0)
         END IF
         IF(ITY.EQ.12 .AND.COEF(11).EQ.1.0D0) THEN





            WRITE(OUTLYNE,2001) COEF(1)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2002) COEF(2)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2003) COEF(3),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2004) COEF(4),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2005) COEF(5),UN
            CALL SHOWIT(0)





            IF(COEF(6).GT.0.0D0) REALL='REAL    '
            IF(COEF(6).LT.0.0D0) REALL='VIRTUAL '
            IF(COEF(6).GT.0.0D0) COEF(6)=1.0D0
            IF(COEF(6).LT.0.0D0) COEF(6)=-1.0D0
            WRITE(OUTLYNE,2006) INT(COEF(6)),REALL
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2007) COEF(7),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2008) COEF(8),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2009) COEF(9),UN
            CALL SHOWIT(0)





            IF(COEF(10).GT.0.0D0) REALL='REAL   '
            IF(COEF(10).LT.0.0D0) REALL='VIRTUAL'
            IF(COEF(10).GT.0.0D0) COEF(10)=1.0D0
            IF(COEF(10).LT.0.0D0) COEF(10)=-1.0D0
            WRITE(OUTLYNE,2010) INT(COEF(10)),REALL
            CALL SHOWIT(0)
            WRITE(OUTLYNE,4001) COEF(11),COEF(12)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),&
            &COEF(16)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),&
            &COEF(20)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,7005) COEF(21)
            CALL SHOWIT(0)
         END IF
         IF(ITY.EQ.12 .AND.COEF(11).EQ.7.0D0) THEN





            WRITE(OUTLYNE,2001) COEF(1)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2002) COEF(2)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2003) COEF(3),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2004) COEF(4),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2005) COEF(5),UN
            CALL SHOWIT(0)





            IF(COEF(6).GT.0.0D0) REALL='REAL    '
            IF(COEF(6).LT.0.0D0) REALL='VIRTUAL '
            IF(COEF(6).GT.0.0D0) COEF(6)=1.0D0
            IF(COEF(6).LT.0.0D0) COEF(6)=-1.0D0
            WRITE(OUTLYNE,2006) INT(COEF(6)),REALL
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2007) COEF(7),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2008) COEF(8),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2009) COEF(9),UN
            CALL SHOWIT(0)





            IF(COEF(10).GT.0.0D0) REALL='REAL   '
            IF(COEF(10).LT.0.0D0) REALL='VIRTUAL'
            IF(COEF(10).GT.0.0D0) COEF(10)=1.0D0
            IF(COEF(10).LT.0.0D0) COEF(10)=-1.0D0
            WRITE(OUTLYNE,2010) INT(COEF(10)),REALL
            CALL SHOWIT(0)
            WRITE(OUTLYNE,4001) COEF(11),COEF(12)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),&
            &COEF(16)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),&
            &COEF(20)
            CALL SHOWIT(0)



            WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),&
            &COEF(24)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),&
            &COEF(28)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),&
            &COEF(32)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),&
            &COEF(36)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),&
            &COEF(40)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),&
            &COEF(44)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),&
            &COEF(48)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,707) COEF(49),COEF(50),COEF(51),&
            &COEF(52)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,708) COEF(53),COEF(54),COEF(55),&
            &COEF(56)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,709) COEF(57),COEF(58),COEF(59),&
            &COEF(60)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,710) COEF(61),COEF(62),COEF(63),&
            &COEF(64)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,711) COEF(65),COEF(66),COEF(67),&
            &COEF(68)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,712) COEF(69),COEF(70),COEF(71),&
            &COEF(72)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,713) COEF(73),COEF(74),COEF(75),&
            &COEF(76)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,714) COEF(77),COEF(78),COEF(79),&
            &COEF(80)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,715) COEF(81),COEF(82),COEF(83),&
            &COEF(84)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,716) COEF(84),COEF(85),COEF(86),&
            &COEF(87)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,717) COEF(89),COEF(90),COEF(91),&
            &COEF(92)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,718) COEF(93),COEF(94),COEF(95),&
            &COEF(96)
            CALL SHOWIT(0)



         END IF
         IF(ITY.EQ.12 .AND.COEF(11).EQ.2.0D0 .OR.&
         &ITY.EQ.12 .AND.COEF(11).EQ.3.0D0 .OR.&
         &ITY.EQ.12 .AND.COEF(11).EQ.4.0D0 .OR.&
         &ITY.EQ.12 .AND.COEF(11).EQ.5.0D0) THEN





            WRITE(OUTLYNE,2001) COEF(1)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2002) COEF(2)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2003) COEF(3),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2004) COEF(4),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2005) COEF(5),UN
            CALL SHOWIT(0)





            IF(COEF(6).GT.0.0D0) REALL='REAL    '
            IF(COEF(6).LT.0.0D0) REALL='VIRTUAL '
            IF(COEF(6).GT.0.0D0) COEF(6)=1.0D0
            IF(COEF(6).LT.0.0D0) COEF(6)=-1.0D0
            WRITE(OUTLYNE,2006) INT(COEF(6)),REALL
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2007) COEF(7),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2008) COEF(8),UN
            CALL SHOWIT(0)





            WRITE(OUTLYNE,2009) COEF(9),UN
            CALL SHOWIT(0)





            IF(COEF(10).GT.0.0D0) REALL='REAL   '
            IF(COEF(10).LT.0.0D0) REALL='VIRTUAL'
            IF(COEF(10).GT.0.0D0) COEF(10)=1.0D0
            IF(COEF(10).LT.0.0D0) COEF(10)=-1.0D0
            WRITE(OUTLYNE,2010) INT(COEF(10)),REALL
            CALL SHOWIT(0)
            WRITE(OUTLYNE,4001) COEF(11),COEF(12)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,500) COEF(13),COEF(14),COEF(15),&
            &COEF(16)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,600) COEF(17),COEF(18),COEF(19),&
            &COEF(20)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,700) COEF(21),COEF(22),COEF(23),COEF(24)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,701) COEF(25),COEF(26),COEF(27),&
            &COEF(28)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,702) COEF(29),COEF(30),COEF(31),&
            &COEF(32)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,703) COEF(33),COEF(34),COEF(35),&
            &COEF(36)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,704) COEF(37),COEF(38),COEF(39),&
            &COEF(40)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,705) COEF(41),COEF(42),COEF(43),&
            &COEF(44)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,706) COEF(45),COEF(46),COEF(47),&
            &COEF(48)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,707) COEF(49),COEF(50),COEF(51),&
            &COEF(52)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,708) COEF(53),COEF(54),COEF(55),&
            &COEF(56)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,709) COEF(57),COEF(58),COEF(59),&
            &COEF(60)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,710) COEF(61),COEF(62),COEF(63),&
            &COEF(64)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,711) COEF(65),COEF(66),COEF(67),&
            &COEF(68)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,712) COEF(69),COEF(70),COEF(71),&
            &COEF(72)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,713) COEF(73),COEF(74),COEF(75),&
            &COEF(76)
            CALL SHOWIT(0)





            WRITE(OUTLYNE,7141) COEF(77),COEF(78),COEF(79)
            CALL SHOWIT(0)
         END IF
15    CONTINUE
   END IF
!
101 FORMAT('SURFACE =',I3,2X,&
   &'SPECIAL SURFACE TYPE =',1X,A9)
102 FORMAT(A79)
200 FORMAT('C(01)=',G12.5,1X,'C(02)=',&
   &G12.5,1X,'C(03)=',G12.5,1X,'C(04)=',G12.5)
2001 FORMAT('HOE ORDER NUMBER      (C1) = ',G15.8)
2002 FORMAT('CONST. WAVELENGTH     (C2) = ',G15.8,' MICRONS')
2003 FORMAT('X-COORD. SOURCE POINT (C3) = ',G15.8,1X,A6)
2004 FORMAT('Y-COORD. SOURCE POINT (C4) = ',G15.8,1X,A6)
2005 FORMAT('Z-COORD. SOURCE POINT (C5) = ',G15.8,1X,A6)
2006 FORMAT('SOURCE REALITY        (C6) = ',I2,1X,A8)
2007 FORMAT('X-COORD. REF. POINT   (C7) = ',G15.8,1X,A6)
2008 FORMAT('Y-COORD. REF. POINT   (C8) = ',G15.8,1X,A6)
2009 FORMAT('Z-COORD. REF. POINT   (C9) = ',G15.8,1X,A6)
2010 FORMAT('REFERENCE REALITY    (C10) = ',I2,1X,A8)
!
2201 FORMAT('HOE ORDER NUMBER      (C1) = ',G15.8)
2202 FORMAT('CONST. WAVELENGTH     (C2) = ',G15.8,' MICRONS')
2203 FORMAT('X-COORD. SOURCE POINT (C3) = ',G15.8,1X,A6)
2204 FORMAT('Y-COORD. SOURCE POINT (C4) = ',G15.8,1X,A6)
2205 FORMAT('Z-COORD. SOURCE POINT (C5) = ',G15.8,1X,A6)
2206 FORMAT('SOURCE BEAM DIR.      (C6) = ',I2,1X,A8)
2207 FORMAT('SOURCE REALITY        (C7) = ',I2,1X,A8)
2208 FORMAT('X-COORD. REF. POINT   (C8) = ',G15.8,1X,A6)
2209 FORMAT('Y-COORD. REF. POINT   (C9) = ',G15.8,1X,A6)
2210 FORMAT('Z-COORD. REF. POINT  (C10) = ',G15.8,1X,A6)
2211 FORMAT('REFERENCE BEAM DIR.  (C11) = ',I2,1X,A8)
2212 FORMAT('REFERENCE REALITY    (C12) = ',I2,1X,A8)
2213 FORMAT('SOURCE CFG#          (C13) = ',I2,1X,A8)
2214 FORMAT('REFERENCE CFG#       (C14) = ',I2,1X,A8)
300 FORMAT('C(05)=',G12.5,1X,'C(06)=',&
   &G12.5,1X,'C(07)=',G12.5,1X,'C(08)=',G12.5)
31001 FORMAT('1ST PEAK TO VALLY ERROR (WAVES)  = ',G15.8)
31002 FORMAT('1ST X-PERIOD (SPATIAL FREQUENCY) = ',G15.8,1X,A6)
31003 FORMAT('1ST Y-PERIOD (SPATIAL FREQUENCY) = ',G15.8,1X,A6)
32001 FORMAT('2ND PEAK TO VALLY ERROR (WAVES)  = ',G15.8)
32002 FORMAT('2ND X-PERIOD (SPATIAL FREQUENCY) = ',G15.8,1X,A6)
32003 FORMAT('2ND Y-PERIOD (SPATIAL FREQUENCY) = ',G15.8,1X,A6)
33001 FORMAT('3RD PEAK TO VALLY ERROR (WAVES)  = ',G15.8)
33002 FORMAT('3RD X-PERIOD (SPATIAL FREQUENCY) = ',G15.8,1X,A6)
33003 FORMAT('3RD Y-PERIOD (SPATIAL FREQUENCY) = ',G15.8,1X,A6)
34001 FORMAT('4TH PEAK TO VALLY ERROR (WAVES)  = ',G15.8)
34002 FORMAT('4TH X-PERIOD (SPATIAL FREQUENCY) = ',G15.8,1X,A6)
34003 FORMAT('4TH Y-PERIOD (SPATIAL FREQUENCY) = ',G15.8,1X,A6)
35001 FORMAT('5TH PEAK TO VALLY ERROR (WAVES)  = ',G15.8)
35002 FORMAT('5TH X-PERIOD (SPATIAL FREQUENCY) = ',G15.8,1X,A6)
35003 FORMAT('5TH Y-PERIOD (SPATIAL FREQUENCY) = ',G15.8,1X,A6)
5001 FORMAT('FORWARD Z-COORDINATE  = ',G15.8,1X,A6)
5002 FORMAT('    AFT Z-COORDINATE  = ',G15.8,1X,A6)
5003 FORMAT('CENTRAL Z-COORDINATE  = ',G15.8,1X,A6)
5004 FORMAT('F(01)=',G12.5,1X,'F(02)=',&
   &G12.5,1X,'F(03)=',G12.5,1X,'F(04)=',G12.5)
5005 FORMAT('F(05)=',G12.5,1X,'F(06)=',&
   &G12.5,1X,'F(07)=',G12.5,1X,'F(08)=',G12.5)
5006 FORMAT('F(09)=',G12.5,1X,'F(10)=',&
   &G12.5,1X,'F(11)=',G12.5,1X,'F(12)=',G12.5)
5007 FORMAT('F(13)=',G12.5,1X,'F(14)=',&
   &G12.5,1X,'F(15)=',G12.5)
400 FORMAT('C(09)=',G12.5,1X,'C(10)=',&
   &G12.5,1X,'C(11)=',G12.5,1X,'C(12)=',G12.5)
401 FORMAT('C(09)=',G12.5,1X,'C(10)=',&
   &G12.5,1X,'C(11)=',G12.5)
4001 FORMAT('C(11)=',G12.5,1X,'C(12)=',G12.5)
500 FORMAT('C(13)=',G12.5,1X,'C(14)=',&
   &G12.5,1X,'C(15)=',G12.5,1X,'C(16)=',G12.5)
5011 FORMAT('C(09)=',G12.5,1X,'C(10)=',&
   &G12.5,1X,'C(11)=',G12.5)
600 FORMAT('C(17)=',G12.5,1X,'C(18)=',&
   &G12.5,1X,'C(19)=',G12.5,1X,'C(20)=',G12.5)
700 FORMAT('C(21)=',G12.5,1X,'C(22)=',&
   &G12.5,1X,'C(23)=',G12.5,1X,'C(24)=',G12.5)
7001 FORMAT('C(21)=',G12.5,1X,'C(22)=',&
   &G12.5,1X,'C(23)=',G12.5,1X,'C(24)=',G12.5)
7002 FORMAT('C(25)=',G12.5,1X,'C(26)=',&
   &G12.5,1X,'C(27)=',G12.5,1X,'C(28)=',G12.5)
7003 FORMAT('C(29)=',G12.5,1X,'C(30)=',&
   &G12.5,1X,'C(31)=',G12.5)
7004 FORMAT('C(65)=',G12.5,1X,'C(66)=',&
   &G12.5)
7005 FORMAT('C(21)=',G12.5)
7006 FORMAT('C(32)=',G12.5)
701 FORMAT('C(25)=',G12.5,1X,'C(26)=',&
   &G12.5,1X,'C(27)=',G12.5,1X,'C(28)=',G12.5)
702 FORMAT('C(29)=',G12.5,1X,'C(30)=',&
   &G12.5,1X,'C(31)=',G12.5,1X,'C(32)=',G12.5)
7021 FORMAT('C(29)=',G12.5,1X,'C(30)=',&
   &G12.5)
703 FORMAT('C(33)=',G12.5,1X,'C(34)=',&
   &G12.5,1X,'C(35)=',G12.5,1X,'C(36)=',G12.5)
704 FORMAT('C(37)=',G12.5,1X,'C(38)=',&
   &G12.5,1X,'C(39)=',G12.5,1X,'C(40)=',G12.5)
7041 FORMAT('C(37)=',G12.5,1X,'C(38)=',&
   &G12.5,1X,'C(39)=',G12.5)
7042 FORMAT('C(37)=',G12.5)
705 FORMAT('C(41)=',G12.5,1X,'C(42)=',&
   &G12.5,1X,'C(43)=',G12.5,1X,'C(44)=',G12.5)
706 FORMAT('C(45)=',G12.5,1X,'C(46)=',&
   &G12.5,1X,'C(47)=',G12.5,1X,'C(48)=',G12.5)
707 FORMAT('C(49)=',G12.5,1X,'C(50)=',&
   &G12.5,1X,'C(51)=',G12.5,1X,'C(52)=',G12.5)
708 FORMAT('C(53)=',G12.5,1X,'C(54)=',&
   &G12.5,1X,'C(55)=',G12.5,1X,'C(56)=',G12.5)
709 FORMAT('C(57)=',G12.5,1X,'C(58)=',&
   &G12.5,1X,'C(59)=',G12.5,1X,'C(60)=',G12.5)
710 FORMAT('C(61)=',G12.5,1X,'C(62)=',&
   &G12.5,1X,'C(63)=',G12.5,1X,'C(64)=',G12.5)
711 FORMAT('C(65)=',G12.5,1X,'C(66)=',&
   &G12.5,1X,'C(67)=',G12.5,1X,'C(68)=',G12.5)
712 FORMAT('C(69)=',G12.5,1X,'C(70)=',&
   &G12.5,1X,'C(71)=',G12.5,1X,'C(72)=',G12.5)
713 FORMAT('C(73)=',G12.5,1X,'C(74)=',&
   &G12.5,1X,'C(75)=',G12.5,1X,'C(76)=',G12.5)
714 FORMAT('C(77)=',G12.5,1X,'C(78)=',&
   &G12.5,1X,'C(79)=',G12.5,1X,'C(80)=',G12.5)
7141 FORMAT('C(81)=',G12.5,1X,'C(82)=',&
   &G12.5,1X,'C(83)=',G12.5)
715 FORMAT('C(81)=',G12.5,1X,'C(82)=',&
   &G12.5,1X,'C(83)=',G12.5,1X,'C(84)=',G12.5)
716 FORMAT('C(85)=',G12.5,1X,'C(86)=',&
   &G12.5,1X,'C(87)=',G12.5,1X,'C(88)=',G12.5)
717 FORMAT('C(89)=',G12.5,1X,'C(90)=',&
   &G12.5,1X,'C(91)=',G12.5,1X,'C(92)=',G12.5)
7171 FORMAT('C(89)=',G12.5,1X,'C(90)=',&
   &G12.5,1X,'C(91)=',G12.5)
718 FORMAT('C(93)=',G12.5,1X,'C(94)=',&
   &G12.5,1X,'C(95)=',G12.5,1X,'C(96)=',G12.5)
110 FORMAT('SURF',1X,I3,1X,&
   &':NO SPECIAL SURFACE DATA')
100 FORMAT('NO SPECIAL SURFACE DATA')
1000 FORMAT('SPECIAL SURFACE DATA')
   RETURN
END
