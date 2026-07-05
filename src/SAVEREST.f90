!     SAVEINPT/RESTINPT: save/restore the parsed-command state around a
!     synthesized-command re-entry (INPUT='...' + CALL PROCES).
!
!     The state itself is now a parsed_command record (mod_parsed_command);
!     the old 26 parallel KDP_* COMMON arrays are gone, so a new parser field
!     can never again be silently dropped from save/restore (each array also
!     had its own dimension declaration -- KDP_SST/KDP_S5 were 1:200 while
!     the rest were 0:200).
!
!     Signatures are unchanged: callers keep the historical
!     SAVE_KDP(i)=SAVEINPT(i) ... REST_KDP(i)=RESTINPT(i) pattern with
!     caller-chosen slot indices.  (kdp_exec in mod_parsed_command supersedes
!     this pattern for new code.)
FUNCTION SAVEINPT(I)
   use mod_parsed_command, only: capture_command, kdp_slot, KDP_SLOT_MAX
   IMPLICIT NONE
   LOGICAL SAVEINPT
   INTEGER I

   IF (I >= 0 .AND. I <= KDP_SLOT_MAX) THEN
      kdp_slot(I) = capture_command()
   END IF
   SAVEINPT=.TRUE.
   RETURN
END

FUNCTION RESTINPT(I)
   use mod_parsed_command, only: apply_command, kdp_slot, KDP_SLOT_MAX
   IMPLICIT NONE
   LOGICAL RESTINPT
   INTEGER I

   IF (I >= 0 .AND. I <= KDP_SLOT_MAX) THEN
      call apply_command(kdp_slot(I))
   END IF
   RESTINPT=.TRUE.
   RETURN
END
