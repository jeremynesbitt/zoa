!       TWELFTH FILE FOR LENS DATABASE MANAGER FILES

! SUB ILF.FOR
SUBROUTINE ILF
   use zoa_file_handler
!
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS SUBROUTINE INITIALIZES OR BLANKS OUT THE CURRENT
!       LENS LIBRARY DIRECTORY.
!
   INTEGER I,II,N,J, uLIB, uLIBTAG
!
   CHARACTER BLANK*80,FN*10,AN*3
!
   LOGICAL EXISJK
!
!
   BLANK=AA//AA//AA//AA
!
!       OPEN UNIT 22 FOR I/O
!       OPEN UNIT 27 FOR I/O
!
   call clear_file(trim(LIBLEN)//'LIB.DAT')
   call clear_file(trim(LIBLEN)//'LIBTAG.DAT')

   DO N=1,999
      CALL CCOONN(N,AN)
      IF(N.GT.0.AND.N.LE.9) FN='LIB00'//AN(3:3)//'.DAT'
      IF(N.GT.9.AND.N.LE.99) FN='LIB0'//AN(2:3)//'.DAT'
      IF(N.GT.99.AND.N.LE.999) FN='LIB'//AN(1:3)//'.DAT'
      EXISJK=.FALSE.
      INQUIRE(FILE=trim(LIBLEN)//FN,EXIST=EXISJK)
      IF(EXISJK) THEN
         OPEN(UNIT=22,ACCESS='SEQUENTIAL',BLANK='NULL',FORM='FORMATTED',FILE=trim(LIBLEN)//FN ,STATUS='UNKNOWN')
         CALL CLOSE_FILE(22,0)
      END IF
   END DO
!
   call OPENLIBDAT(uLIB)
   call OPENLIBTAGDAT(uLIBTAG)

!
   II=0
   DO 25 I=1,999
      WRITE(UNIT=uLIB,REC=I)II,BLANK
      DO J=1,10
         WRITE(UNIT=uLIBTAG,REC=I-1+J) BLANK(1:75)
      END DO
25 CONTINUE
!
   CALL CLOSE_FILE(uLIB,1)
   CALL CLOSE_FILE(uLIBTAG,1)
   OUTLYNE='LENS LIBRARY INITIALIZED'
   CALL SHOWIT(1)
   RETURN
END
! SUB CVSOLV.FOR
SUBROUTINE CVSOLV
!
   use DATLEN
   use mod_surface
   use DATMAI
   use command_utils, only: is_command_query
   use mod_system, only: sys_last_surf, sys_astop, sys_wavelength
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE CVSOLV WHICH IMPLEMENTS THE
!       CURVATURE SOLVES
!       COMMAND AT THE LENS INPUT AND UPDATE LENS LEVEL.
!
   INTEGER SF
!
!
   IF(WC.EQ.'APY'.OR.WC.EQ.'APX'.OR.WC.EQ.'PIY'.OR.WC.EQ.'PIX'.OR.WC.EQ.'PUY'.OR.WC.EQ.'PUX'.OR.WC.EQ.'APCY'.OR.WC.EQ.'APCX'.OR.WC.EQ.'PICY'.OR.WC.EQ.'PICX'.OR.WC.EQ.'PUCY'.OR.WC.EQ.'PUCX'.OR.WC.EQ.'COCY'.OR.WC.EQ.'COCX') THEN
      IF(is_command_query()) THEN
!
         IF(WC.EQ.'APY') THEN
            IF(SOLVE(8,SURF).EQ.1.0D0)WRITE(OUTLYNE,101)SOLVE(9,SURF),SURF
101         FORMAT('"APY" = ',G23.15,' AT SURFACE #',I3)
         END IF
!
         IF(WC.EQ.'APX') THEN
            IF(SOLVE(2,SURF).EQ.8.0D0)WRITE(OUTLYNE,102)SOLVE(1,SURF),SURF
102         FORMAT('"APX" = ',G23.15,' AT SURFACE #',I3)
         END IF
!
         IF(WC.EQ.'PIY') THEN
            IF(SOLVE(8,SURF).EQ.2.0D0)WRITE(OUTLYNE,103)SOLVE(9,SURF),SURF
103         FORMAT('"PIY" = ',G23.15,' AT SURFACE #',I3)
         END IF
!
         IF(WC.EQ.'PIX') THEN
            IF(SOLVE(2,SURF).EQ.9.0D0)WRITE(OUTLYNE,104)SOLVE(1,SURF),SURF
104         FORMAT('"PIX" = ',G23.15,' AT SURFACE #',I3)
         END IF
!
         IF(WC.EQ.'PUY') THEN
            IF(SOLVE(8,SURF).EQ.3.0D0)WRITE(OUTLYNE,105)SOLVE(9,SURF),SURF
105         FORMAT('"PUY" = ',G23.15,' AT SURFACE #',I3)
         END IF
!
         IF(WC.EQ.'PUX') THEN
            IF(SOLVE(2,SURF).EQ.10.0D0)WRITE(OUTLYNE,106)SOLVE(1,SURF),SURF
106         FORMAT('"PUX" = ',G23.15,' AT SURFACE #',I3)
         END IF
!
         IF(WC.EQ.'APCY') THEN
            IF(SOLVE(8,SURF).EQ.4.0D0)WRITE(OUTLYNE,107)SOLVE(9,SURF),SURF
107         FORMAT('"APCY" = ',G23.15,' AT SURFACE #',I3)
         END IF
!
         IF(WC.EQ.'APCX') THEN
            IF(SOLVE(2,SURF).EQ.11.0D0)WRITE(OUTLYNE,108)SOLVE(1,SURF),SURF
108         FORMAT('"APCX" = ',G23.15,' AT SURFACE #',I3)
         END IF
!
         IF(WC.EQ.'PICY') THEN
            IF(SOLVE(8,SURF).EQ.5.0D0)WRITE(OUTLYNE,109)SOLVE(9,SURF),SURF
109         FORMAT('"PICY" = ',G23.15,' AT SURFACE #',I3)
         END IF
!
         IF(WC.EQ.'PICX') THEN
            IF(SOLVE(2,SURF).EQ.12.0D0)WRITE(OUTLYNE,110)SOLVE(1,SURF),SURF
110         FORMAT('"PICX" = ',G23.15,' AT SURFACE #',I3)
         END IF
!
         IF(WC.EQ.'PUCY') THEN
            IF(SOLVE(8,SURF).EQ.6.0D0)WRITE(OUTLYNE,111)SOLVE(9,SURF),SURF
111         FORMAT('"PUCY" = ',G23.15,' AT SURFACE #',I3)
         ELSE
         END IF
!
         IF(WC.EQ.'PUCX') THEN
            IF(SOLVE(2,SURF).EQ.13.0D0)WRITE(OUTLYNE,112)SOLVE(1,SURF),SURF
112         FORMAT('"PUCX" = ',G23.15,' AT SURFACE #',I3)
         END IF
!
         IF(WC.EQ.'COCY') THEN
            IF(SOLVE(8,SURF).EQ.7.0D0)WRITE(OUTLYNE,113)SOLVE(9,SURF),SURF
113         FORMAT('"COCY" = ',G23.15,' AT SURFACE #',I3)
         END IF
!
         IF(WC.EQ.'COCX') THEN
            IF(SOLVE(2,SURF).EQ.14.0D0)WRITE(OUTLYNE,114)SOLVE(1,SURF),SURF
114         FORMAT('"COCX" = ',G23.15,' AT SURFACE #',I3)
         END IF
         CALL SHOWIT(0)
         RETURN
      END IF
   END IF
   IF(WC.EQ.'CSD'.OR.WC.EQ.'CSDX'.OR.WC.EQ.'CSDY') THEN
      IF(SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"CSD", "CSDX" AND "CSDY" TAKE NO STRING'//'\n'//&
         & 'OR NUMERIC WORD #3 THROUGH #5 INPUT'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      ELSE
!       PROCEED
      END IF
      IF(SQ.EQ.1.AND.WQ.NE.'ALL     ') THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"CSD", "CSDX" AND "CSDY" ONLY ACCEPT "ALL" AS'//'\n'//&
         & 'VALID QUALIFIER INPUT'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      ELSE
!       PROCEED
      END IF
      IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
         W1=0.0
         W2=sys_last_surf()
         S1=1
         S2=1
         DF1=0
         DF2=0
         SN=1
      END IF

      IF(DF1.EQ.1.AND.DF2.EQ.1) THEN
         W1=DBLE(SURF)
         W2=DBLE(SURF)
         S1=1
         S2=1
         SN=1
         DF1=0
         DF2=0
      END IF
      IF(DF1.EQ.1.AND.DF2.EQ.0.OR.DF1.EQ.0.AND.DF2.EQ.1) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"CSD", "CSDX" AND "CSDY"'//'\n'//&
         & 'USE EITHER TWO OR ZERO NUMERIC WORDS OR QUALIFIER INPUT'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(INT(W1).LT.0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'STARTING SURFACE NUMBER MUST BE GREATER THAN OR EQUAL TO 0'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(INT(W2).GT.INT(sys_last_surf())) THEN
         WRITE(OUTLYNE,*)'ENDING SURFACE NUMBER MUST BE LESS THAN OR EQUAL TO ',INT(sys_last_surf())
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(W1.GT.W2) THEN
         WRITE(OUTLYNE,*)'THE ENDING SURFACE # MUST BE GREATER THAN OR EQUAL TO#'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('THE STARTING SURFACE #'//'\n'//'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      DO SF=INT(W1),INT(W2)
         IF(SF.EQ.0) THEN
            OUTLYNE='OBJECT SURFACE NEVER HAS SOLVES'
            CALL SHOWIT(1)
            GO TO 900
         END IF
!
!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
!
         IF(WC.EQ.'CSD') THEN
            IF(SOLVE(8,SF).NE.0.0D0.OR.SOLVE(2,SF).NE.0.0D0 .OR.SOLVE(9,SF).NE.0.0D0.OR.SOLVE(1,SF).NE.0.0D0) THEN
               SOLVE(8,SF)=0.0D0
               SOLVE(9,SF)=0.0D0
               SOLVE(2,SF)=0.0D0
               SOLVE(1,SF)=0.0D0
               WRITE(OUTLYNE,*)'SURFACE',SF,' :ALL CURVATURE SOLVES DELETED'
               CALL SHOWIT(1)
            ELSE
               WRITE(OUTLYNE,*)'SURFACE',SF,' :NO CURVATURE SOLVE TO DELETE'
               CALL SHOWIT(1)
            END IF
         END IF
         IF(WC.EQ.'CSDY') THEN
            IF(SOLVE(8,SF).NE.0.0D0.OR.SOLVE(9,SF).NE.0.0D0) THEN
               SOLVE(8,SF)=0.0D0
               SOLVE(9,SF)=0.0D0
               WRITE(OUTLYNE,*)'SURFACE',SF,' :YZ PLANE CURVATURE SOLVE DELETED'
               CALL SHOWIT(1)
            ELSE
               WRITE(OUTLYNE,*)'SURFACE',SF,' :NO YZ CURVATURE SOLVE TO DELETE'
               CALL SHOWIT(1)
            END IF
         END IF
         IF(WC.EQ.'CSDX') THEN
            IF(SOLVE(2,SF).NE.0.0D0.OR.SOLVE(1,SF).NE.0.0D0) THEN
               SOLVE(2,SF)=0.0D0
               SOLVE(1,SF)=0.0D0
               WRITE(OUTLYNE,*)'SURFACE',SF,' :XZ PLANE CURVATURE SOLVE DELETED'
               CALL SHOWIT(1)
            ELSE
               WRITE(OUTLYNE,*)'SURFACE',SF,' :NO XZ CURVATURE SOLVE TO DELETE'
               CALL SHOWIT(1)
            END IF
         END IF
!
!
!       RE CALCULATE surf_solve_flag(SF)
!
         call set_surf_solve_flag(SF, 0.0D0)
         IF(SOLVE(6,SF).GT.0.0D0)call set_surf_solve_flag(SF, surf_solve_flag(SF)+1.0D0)
         IF(SOLVE(4,SF).GT.0.0D0)call set_surf_solve_flag(SF, surf_solve_flag(SF)+0.1D0)
         IF(SOLVE(8,SF).GT.0.0D0)call set_surf_solve_flag(SF, surf_solve_flag(SF)+2.0D0)
         IF(SOLVE(2,SF).GT.0.0D0)call set_surf_solve_flag(SF, surf_solve_flag(SF)+0.2D0)
900      CONTINUE
      END DO
      RETURN
   ELSE
!       NOT CSD,CSDX OR CSDY
   END IF
   IF(WC.NE.'CSD'.AND.WC.NE.'CSDX'.AND.WC.NE.'CSDY') THEN
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
      IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
!
!
         IF(WC.EQ.'APY') THEN
            CALL REPORT_ERROR_AND_FAIL('APY" TAKES NO EXPLICIT INPUT'//'\n'//'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(WC.EQ.'APX') THEN
            CALL REPORT_ERROR_AND_FAIL('"APX" TAKES NO EXPLICIT INPUT'//'\n'//'RE-ENTER COMMAND', 1)
            RETURN
         END IF
      END IF
      IF(SST.EQ.1.OR.SQ.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 .OR.S5.EQ.1) THEN
         IF(WC.EQ.'PIY') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"PIY" ONLY ACCEPTS NUMERIC WORD #1 INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(WC.EQ.'PIX') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"PIX" ONLY ACCEPTS NUMERIC WORD #1 INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
      END IF
      IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 .OR.S5.EQ.1) THEN
         IF(WC.EQ.'PUY') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"PUY" ONLY ACCEPTS QUALIFIER AND NUMERIC WORD #1 INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(WC.EQ.'PUX') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"PUX" ONLY ACCEPTS QUALIFIER AND NUMERIC WORD #1 INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
      END IF
      IF(SQ.EQ.1.AND.WQ.NE.'FN') THEN
         IF(WC.EQ.'PUY') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & 'INVALID UALIFIER USED WITH "PUY"'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(WC.EQ.'PUX') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & 'INVALID QUALIFIER USED WITH "PUX"'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
      END IF
      IF(WQ.EQ.'FN'.AND.DF1.EQ.1) THEN
         IF(WC.EQ.'PUY') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"PUY FN" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(WC.EQ.'PUX') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"PUX FN" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
      END IF
      IF(WQ.EQ.'FN'.AND.W1.EQ.0.0D0) THEN
         IF(WC.EQ.'PUY') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"PUY FN" REQUIRES NON-ZERO NUMERIC WORD #1 INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(WC.EQ.'PUX') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"PUX FN" REQUIRES NON-ZERO NUMERIC WORD #1 INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
      END IF
      IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
         IF(WC.EQ.'APCY') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"APCY" TAKES NO EXPLICIT INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(WC.EQ.'APCX') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"APCX" TAKES NO EXPLICIT INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
      END IF
      IF(SST.EQ.1.OR.SQ.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 .OR.S5.EQ.1) THEN
         IF(WC.EQ.'PICY') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"PICY" ONLY ACCEPTS NUMERIC WORD #1 INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(WC.EQ.'PICX') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"PICX" ONLY ACCEPTS NUMERIC WORD #1 INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(WC.EQ.'PUCY') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"PUCY" ONLY ACCEPTS NUMERIC WORD #1 INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(WC.EQ.'PUCX') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"PUCX" ONLY ACCEPTS NUMERIC WORD #1 INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(WC.EQ.'COCY') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"COCY" ONLY ACCEPTS NUMERIC WORD #1 INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(WC.EQ.'COCX') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"COCX" ONLY ACCEPTS NUMERIC WORD #1 INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
      END IF
   END IF
!
!       NO CURVATURE SOLVES ARE ALLOWED ON THE OBJECT SURFACE
!       OR ON SURFACE 1
!
   IF(WC.EQ.'PUCY'.OR.WC.EQ.'PUCX'.OR.WC.EQ.'PICY'.OR.WC.EQ.'PICX'.OR.WC.EQ.'APCX'.OR.WC.EQ.'APCY') THEN
      IF(SURF.LT.1.AND.sys_astop().EQ.-99.0D0.OR.SURF.LT.INT(sys_astop()).AND.sys_astop().NE.-99.0D0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '             CHIEF RAY SOLVES ARE NOT ALLOWED'//'\n'//&
         & '                       OBJECT SURFACE'//'\n'//&
         & '                            OR'//'\n'//&
         & '               BEFORE THE APERTURE STOP SURFACE.'//'\n'//&
         & '                       RE-ENTER COMMAND', 1)
         RETURN
      END IF
   END IF
!
!       HANDEL PIKUPS RD,CV,RDTOR,CVTOR I.E. REMOVE AS APPROPRIATE
!       AND PRO AND NPRO
!
!       IF ANY CURVATURE SOLVE OCCURED, THEN PRO AND NPRO
!       PIKUPS GO
!
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
!       IF SURFACE IS NOT TORIC THEN NO RDTOR OR CVTOR TO CONSIDER
   IF(surf_toric_flag(SURF).EQ.0.0D0) THEN
!       NON-TORIC
      IF(WC.EQ.'APY'.OR.WC.EQ.'PIY'.OR.WC.EQ.'PUY'.OR.WC.EQ.'APCY'.OR.WC.EQ.'PICY'.OR.WC.EQ.'PUCY'.OR.WC.EQ.'COCY') THEN
         IF(PIKUP(1,SURF,1).EQ.1.0D0) THEN
            PIKUP(1:6,SURF,1)=0.0D0
            call set_surf_special_type(SURF, surf_special_type(SURF)-1)
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :RADIUS PIKUP DELETED'
            CALL SHOWIT(1)
            IF(PIKUP(1,SURF,2).EQ.1.0D0) THEN
               PIKUP(1:6,SURF,2)=0.0D0
               call set_surf_special_type(SURF, surf_special_type(SURF)-1)
               WRITE(OUTLYNE,*)'SURFACE',SURF,' :CURVATURE PIKUP DELETED'
               CALL SHOWIT(1)
            END IF
         END IF
      END IF
      IF(surf_toric_flag(SURF).EQ.1.0) THEN
!       SURFACE IS A YTORIC
         IF(WC.EQ.'APY'.OR.WC.EQ.'PIY'.OR.WC.EQ.'PUY'.OR.WC.EQ.'APCY'.OR.WC.EQ.'PICY'.OR.WC.EQ.'PUCY'.OR.WC.EQ.'COCY') THEN
            IF(PIKUP(1,SURF,1).EQ.1.0D0) THEN
               PIKUP(1:6,SURF,1)=0.0D0
               call set_surf_special_type(SURF, surf_special_type(SURF)-1)
               WRITE(OUTLYNE,*)'SURFACE',SURF,' :RADIUS PIKUP DELETED'
               CALL SHOWIT(1)
            END IF
            IF(PIKUP(1,SURF,2).EQ.1.0D0) THEN
               PIKUP(1:6,SURF,2)=0.0D0
               call set_surf_special_type(SURF, surf_special_type(SURF)-1)
               WRITE(OUTLYNE,*)'SURFACE',SURF,' :CURVATURE PIKUP DELETED'
               CALL SHOWIT(1)
            END IF
         END IF
         IF(WC.EQ.'APX'.OR.WC.EQ.'PIX'.OR.WC.EQ.'PUX'.OR.WC.EQ.'APCX'.OR.WC.EQ.'PICX'.OR.WC.EQ.'PUCX'.OR.WC.EQ.'COCX') THEN
            IF(PIKUP(1,SURF,9).EQ.1.0D0) THEN
               PIKUP(1:6,SURF,9)=0.0D0
               call set_surf_special_type(SURF, surf_special_type(SURF)-1)
               WRITE(OUTLYNE,*)'SURFACE',SURF,' :(CVTOR) PIKUP DELETED'
               CALL SHOWIT(1)
            END IF
            IF(PIKUP(1,SURF,10).EQ.1.0D0) THEN
               PIKUP(1:6,SURF,10)=0.0D0
               call set_surf_special_type(SURF, surf_special_type(SURF)-1)
               WRITE(OUTLYNE,*)'SURFACE',SURF,' :(RDTOR) PIKUP DELETED'
               CALL SHOWIT(1)
            END IF
         END IF
      END IF
      IF(surf_toric_flag(SURF).EQ.2.0D0) THEN
!       SURFACE IS A XTORIC
         IF(WC.EQ.'APY'.OR.WC.EQ.'PIY'.OR.WC.EQ.'PUY'.OR.WC.EQ.'APCY'.OR.WC.EQ.'PICY'.OR.WC.EQ.'PUCY'.OR.WC.EQ.'COCY') THEN
            IF(PIKUP(1,SURF,9).EQ.1.0D0) THEN
               PIKUP(1:6,SURF,9)=0.0D0
               call set_surf_special_type(SURF, surf_special_type(SURF)-1)
               WRITE(OUTLYNE,*)'SURFACE',SURF,' :(CVTOR) PIKUP DELETED'
               CALL SHOWIT(1)
            END IF
            IF(PIKUP(1,SURF,10).EQ.1.0D0) THEN
               PIKUP(1:6,SURF,10)=0.0D0
               call set_surf_special_type(SURF, surf_special_type(SURF)-1)
               WRITE(OUTLYNE,*)'SURFACE',SURF,' :(RDTOR) PIKUP DELETED'
               CALL SHOWIT(1)
            END IF
         END IF
         IF(WC.EQ.'APX'.OR.WC.EQ.'PIX'.OR.WC.EQ.'PUX'.OR.WC.EQ.'APCX'.OR.WC.EQ.'PICX'.OR.WC.EQ.'PUCX'.OR.WC.EQ.'COCX') THEN
            IF(PIKUP(1,SURF,1).EQ.1.0D0) THEN
               PIKUP(1:6,SURF,1)=0.0D0
               call set_surf_special_type(SURF, surf_special_type(SURF)-1)
               WRITE(OUTLYNE,*)'SURFACE',SURF,' :RADIUS PIKUP DELETED'
               CALL SHOWIT(1)
            END IF
            IF(PIKUP(1,SURF,2).EQ.1.0D0) THEN
               PIKUP(1:6,SURF,2)=0.0D0
               call set_surf_special_type(SURF, surf_special_type(SURF)-1)
               WRITE(OUTLYNE,*)'SURFACE',SURF,' :CURVATURE PIKUP DELETED'
               CALL SHOWIT(1)
            END IF
         END IF
      END IF
!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
!
      IF(WC.EQ.'APY')  SOLVE(8,SURF)=1.0D0
      IF(WC.EQ.'PIY')  SOLVE(8,SURF)=2.0D0
      IF(WC.EQ.'PUY')  SOLVE(8,SURF)=3.0D0
      IF(WC.EQ.'APCY') SOLVE(8,SURF)=4.0D0
      IF(WC.EQ.'PICY') SOLVE(8,SURF)=5.0D0
      IF(WC.EQ.'PUCY') SOLVE(8,SURF)=6.0D0
      IF(WC.EQ.'PUY'.AND.SQ.EQ.0) SOLVE(9,SURF)=W1
      IF(WC.EQ.'PUY'.AND.WQ.EQ.'FN')SOLVE(9,SURF)=-1.0D0/(2.0D0*W1)
      IF(WC.EQ.'PIY'.OR.WC.EQ.'PICY'.OR.WC.EQ.'PUCY')SOLVE(9,SURF)=W1
      IF(WC.EQ.'APY'.OR.WC.EQ.'APCY')SOLVE(9,SURF)=0.0D0
      PRINT *, "DEBUG:SOLVE(8,SURF) IS ", SOLVE(8,SURF)
      PRINT *, "DEBUG:SURF IS ", SURF
      PRINT *, "DEBUG:SOLVE(9,SURF) IS ", SOLVE(9,SURF)
      IF(W1.LT.0.0D0) W1=sys_last_surf()+W1
      IF(WC.EQ.'COCY'.AND.W1.GT.sys_last_surf()) THEN
         OUTLYNE='(COCY) SOLVE REFERED TO A SURFACE BEYOND'
         CALL SHOWIT(1)
         OUTLYNE='       THE IMAGE SURFACE. SOLVE IGNORED'
         CALL SHOWIT(1)
      ELSE
         IF(WC.EQ.'COCY'.AND.W1.EQ.DBLE(SURF)) THEN
            OUTLYNE='(COCY) SOLVE CAN NOT REFER TO ITSELF'
            CALL SHOWIT(1)
            OUTLYNE='SOLVE IGNORED'
            CALL SHOWIT(1)
         ELSE
            IF(W1.LT.0.0D0) W1=sys_last_surf()+W1
            IF(WC.EQ.'COCY'.AND.W1.LE.sys_last_surf()) THEN
               SOLVE(8,SURF)=7.0D0
               SOLVE(9,SURF)=W1
            ELSE
            END IF
         END IF
      END IF
!
!
      IF(WC.EQ.'APX'.AND.surf_toric_flag(SURF).EQ.0.0D0 .OR.WC.EQ.'PIX'.AND.surf_toric_flag(SURF).EQ.0.0D0 .OR.WC.EQ.'PUX'.AND.surf_toric_flag(SURF).EQ.0.0D0 .OR.WC.EQ.'APCX'.AND.surf_toric_flag(SURF).EQ.0.0D0 .OR.WC.EQ.'PICX'.AND.surf_toric_flag(SURF).EQ.0.0D0 .OR.WC.EQ.'PUCX'.AND.surf_toric_flag(SURF).EQ.0.0D0 .OR.WC.EQ.'COCX'.AND.surf_toric_flag(SURF).EQ.0.0D0) THEN
         OUTLYNE='XZ CURVATURE SOLVES VALID ONLY FOR TORIC SURFACES'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'XZ CURVATURE SOLVE NOT ASSIGNED FOR SURFACE',SURF
         CALL SHOWIT(1)
         SOLVE(2,SURF)=0.0D0
         SOLVE(1,SURF)=0.0D0
      ELSE
         IF(surf_toric_flag(SURF).GT.0.0D0) THEN
!       SURFACE IS A TORIC.
            IF(WC.EQ.'APX')THEN
               SOLVE(2,SURF)=8.0D0
               SOLVE(1,SURF)=0.0D0
            ELSE
            END IF
            IF(WC.EQ.'PIX') THEN
               SOLVE(2,SURF)=9.0D0
               SOLVE(1,SURF)=W1
            ELSE
            END IF
            IF(WC.EQ.'PUX'.AND.SQ.EQ.0) THEN
               SOLVE(2,SURF)=10.0D0
               SOLVE(1,SURF)=W1
            ELSE
            END IF
            IF(WC.EQ.'PUX'.AND.WQ.EQ.'FN') THEN
               SOLVE(2,SURF)=10.0D0
               SOLVE(1,SURF)=-1.0D0/(2.0D0*W1)
            ELSE
            END IF
            IF(WC.EQ.'APCX') THEN
               SOLVE(2,SURF)=11.0D0
               SOLVE(1,SURF)=0.0D0
            ELSE
            END IF
            IF(WC.EQ.'PICX') THEN
               SOLVE(2,SURF)=12.0D0
               SOLVE(1,SURF)=W1
            ELSE
            END IF
            IF(WC.EQ.'PUCX') THEN
               SOLVE(2,SURF)=13.0D0
               SOLVE(1,SURF)=W1
            ELSE
            END IF
            IF(W1.LT.0.0D0) W1=sys_last_surf()+W1
            IF(WC.EQ.'COCX'.AND.W1.GT.sys_last_surf()) THEN
               OUTLYNE='(COCX) SOLVE REFERED TO A SURFACE BEYOND'
               CALL SHOWIT(1)
               OUTLYNE='       THE IMAGE SURFACE. SOLVE IGNORED'
               CALL SHOWIT(1)
            ELSE
               IF(WC.EQ.'COCX'.AND.W1.EQ.DBLE(SURF)) THEN
                  OUTLYNE='(COCX) SOLVE CAN NOT REFER TO ITSELF'
                  CALL SHOWIT(1)
                  OUTLYNE='SOLVE IGNORED'
                  CALL SHOWIT(1)
               ELSE
                  IF(W1.LT.0.0D0) W1=sys_last_surf()+W1
                  IF(WC.EQ.'COCX'.AND.W1.LE.sys_last_surf()) THEN
                     SOLVE(8,SURF)=14.0D0
                     SOLVE(9,SURF)=W1
                  ELSE
                  END IF
               END IF
            END IF
!       XZ CURVATURE SOLVE WAS ASSIGNED TO A TORIC
         ELSE
         END IF
      END IF
!
!       NOW UPDATE THE STATUS OF surf_solve_flag(SURF) TO PROPERLY
!       REPRESENT THE SOLVE STATUS ON SURFACE (SURF)
!
      call set_surf_solve_flag(SURF, 0.0D0)
      IF(SOLVE(6,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+1.0D0)
      IF(SOLVE(4,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+0.1D0)
      IF(SOLVE(8,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+2.0D0)
      IF(SOLVE(2,SURF).GT.0.0D0)call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+0.2D0)
      RETURN
   ELSE
!       MUST BE A DELETION
   END IF
   RETURN
END
! SUB COERRS.FOR

SUBROUTINE COERRS
!
   use DATLEN
   use mod_surface
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE COERRS.FOR. THIS SUBROUTINE IMPLEMENTS
!       THE OBSCURATION ERASE CHECKING AS CALLED BY CACHEK.FOR
!
   EXTERNAL INSID2
!
   INTEGER CAERAS,COERAS
!
   INTEGER II,I,N,III
!
   real(real64) X,Y,Z,ANGLE,XR,YR,LS1,LS,RS,XRD,YRD,X1,X2,X3,X4,Y1,Y2,Y3,Y4,X5,X6,X7,X8,Y5,Y6,Y7,Y8,XC1,XC2,XC3,XC4,YC1,YC2,YC3,YC4,RAD2,MAXSID,CS1,CS2,CS3,CS4
!
   COMMON/CACO/CAERAS,COERAS,LS
!
   LOGICAL INS,INSID2
!
!
!       NOW ALL THE COERAS WAS ALREADY SET AND THE RAY WAS
!       BLOCKED BY A COBS ON SURFACE I. DOES THE BLOCK GET CANCELED
!       BY THE COBS ERASE ON I+I = II
!
   I=R_I
   X=R_X
   Y=R_Y
   Z=R_Z
   II=I
!
!       COERAS=1 CIRCULAR COBS ERASE, DOES IT STOP THE RAY
   IF(COERAS.EQ.1) THEN
      LS1=0.0D0
!
!       CIRCULAR COBS EQUATION IS:
!
!       (X)**2 + (Y)**2 = R**2
!
!       SUB X AND Y IN THE LEFT SIDE. IF THE LEFT SIDE IS
!       LESS THAN THE RIGHT SIDE, THE RAY IS BLOCKED
!       ELSE IT IS NOT BLOCKED.
!
!       IF NON-ZERO COBS ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
!       IN COBS ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
!       TO XR AND YR IN THE COORDINATE SYSTEM OF THE DECENTERED
!       COBS ERASE. REMEMBER.
!
      XR=X-surf_cobs_era_data(II, 4)
      YR=Y-surf_cobs_era_data(II, 3)
!
      LS1=DSQRT((XR**2)+(YR**2))
!
      RS=DSQRT(surf_cobs_era_data(II, 1)**2)+AIMTOL
      IF(REAL(LS1).GT.REAL(RS)) THEN
         LS1=10.0D0
      ELSE
         LS1=0.0D0
      END IF
      IF(LS1.EQ.0.0D0) LS=0.0D0
      IF(LS1.EQ.10.0D0) LS=10.0D0
      RETURN
!       COERAS NOT 1
   END IF
!
!
!       COERAS=2 RECTANGULAR COBS ERASE, DOES IT STOP THE RAY
   IF(COERAS.EQ.2) THEN
      LS1=0.0D0
!
!       IN THE COORDINATE SYSTEM OF THE RECTANGLE, THE CORNER
!       COORDINATES ARE:
      X1=-surf_cobs_era_data(II, 2)-AIMTOL
      Y1=surf_cobs_era_data(II, 1)+AIMTOL
      X2=-surf_cobs_era_data(II, 2)-AIMTOL
      Y2=-surf_cobs_era_data(II, 1)-AIMTOL
      X3=surf_cobs_era_data(II, 2)+AIMTOL
      Y3=-surf_cobs_era_data(II, 1)-AIMTOL
      X4=surf_cobs_era_data(II, 2)+AIMTOL
      Y4=surf_cobs_era_data(II, 1)+AIMTOL
!
      XRD=X
      YRD=Y
      XRD=XRD-surf_cobs_era_data(II, 4)
      YRD=YRD-surf_cobs_era_data(II, 3)
      XR=(XRD*DCOS(surf_cobs_era_data(II, 6)))+(YRD*DSIN(surf_cobs_era_data(II, 6)))
      YR=(YRD*DCOS(surf_cobs_era_data(II, 6)))-(XRD*DSIN(surf_cobs_era_data(II, 6)))
!
!       ARE THE POINTS XR AND YR ON THE RECTANGLE OR INSIDE
      XT(1)=X1
      YT(1)=Y1
      XT(2)=X2
      YT(2)=Y2
      XT(3)=X3
      YT(3)=Y3
      XT(4)=X4
      YT(4)=Y4
      NP=4
      X0=XR
      Y0=YR
      INS=INSID2()
      IF(INS) THEN
!       RAY NOT BLOCKED
         LS1=0.0D0
      ELSE
!       RAY BLOCKED
         LS1=10.0D0
      END IF
      IF(LS1.EQ.0.0D0) LS=0.0D0
      IF(LS1.EQ.10.0D0) LS=10.0D0
      RETURN
!       COERAS NOT 2
   END IF
!
!       COERAS=3 ELLIPTICAL COBS, DOES IT STOP THE RAY
   IF(COERAS.EQ.3) THEN
      LS1=0.0D0
!
!       ELLIPTICAL COBS EQUATION IS:
!
!       (X)**2/A**2  + (Y)**2/B**2 = 1
!
!       SUB X AND Y IN THE LEFT SIDE. IF THE LEFT SIDE IS
!       LESS THAN 1.0D0, THE RAY IS BLOCKED
!       ELSE IT IS NOT BLOCKED.
      XRD=X
      YRD=Y
      XRD=XRD-surf_cobs_era_data(II, 4)
      YRD=YRD-surf_cobs_era_data(II, 3)
      XR=(XRD*DCOS(surf_cobs_era_data(II, 6)))+(YRD*DSIN(surf_cobs_era_data(II, 6)))
      YR=(YRD*DCOS(surf_cobs_era_data(II, 6)))-(XRD*DSIN(surf_cobs_era_data(II, 6)))
!
      LS=((XR**2)/(surf_cobs_era_data(II, 2)**2))+((YR**2)/(surf_cobs_era_data(II, 1)**2))
!
      IF(REAL(LS).GT.(1.0*(AIMTOL**2))) THEN
!       RAY BLOCKED
         LS1=10.0D0
      ELSE
         LS1=0.0D0
!       NOT BLOCKED
      END IF
      IF(LS1.EQ.0.0D0) LS=0.0D0
      IF(LS1.EQ.10.0D0) LS=10.0D0
      RETURN
!       COERAS NOT 3
   END IF
!
!
!       COERAS=4 RACETRACK COBS ERASE, DOES IT STOP THE RAY
   IF(COERAS.EQ.4) THEN
      LS1=0.0D0
!
!       IF THE RADIUS OF THE CORNERS IS LESS THAN THE LARGER
!       OF THE TWO SIDE DIMENSIONS, AN 8 SIDED POLYGON NEEDS TO BE
!       CHECKED AS WELL AS THE FOUR CIRCLES. IF NOT, THE BASE RECTANGE
!       AND THE FOUR CIRCLES NEED TO BE CHECKED.
!
      IF(surf_cobs_era_data(II, 1).LE.surf_cobs_era_data(II, 2)) THEN
!       surf_cobs_era_data(II, 2) = MAXSID
         MAXSID=surf_cobs_era_data(II, 2)
      ELSE
         MAXSID=surf_cobs_era_data(II, 1)
      END IF
      IF(surf_cobs_era_data(II, 5).LT.MAXSID) THEN
!       SETUP THE 8 SIDED BOX
         N=8
         X1=-surf_cobs_era_data(II, 2)+surf_cobs_era_data(II, 5)-AIMTOL
         Y1=surf_cobs_era_data(II, 1)+AIMTOL
         X2=-surf_cobs_era_data(II, 2)-AIMTOL
         Y2=surf_cobs_era_data(II, 1)-surf_cobs_era_data(II, 5)+AIMTOL
         X3=-surf_cobs_era_data(II, 2)-AIMTOL
         Y3=-surf_cobs_era_data(II, 1)+surf_cobs_era_data(II, 5)-AIMTOL
         X4=-surf_cobs_era_data(II, 2)+surf_cobs_era_data(II, 5)-AIMTOL
         Y4=-surf_cobs_era_data(II, 1)-AIMTOL
         X5=surf_cobs_era_data(II, 2)-surf_cobs_era_data(II, 5)+AIMTOL
         Y5=-surf_cobs_era_data(II, 1)-AIMTOL
         X6=surf_cobs_era_data(II, 2)+AIMTOL
         Y6=-surf_cobs_era_data(II, 1)+surf_cobs_era_data(II, 5)-AIMTOL
         X7=surf_cobs_era_data(II, 2)+AIMTOL
         Y7=surf_cobs_era_data(II, 1)-surf_cobs_era_data(II, 5)+AIMTOL
         X8=surf_cobs_era_data(II, 2)-surf_cobs_era_data(II, 5)+AIMTOL
         Y8=surf_cobs_era_data(II, 1)+AIMTOL
      ELSE
!       SET UP THE FOUR SIDED BOX
         N=4
         X1=-surf_cobs_era_data(II, 2)-AIMTOL
         Y1=surf_cobs_era_data(II, 1)+AIMTOL
         X2=-surf_cobs_era_data(II, 2)-AIMTOL
         Y2=-surf_cobs_era_data(II, 1)-AIMTOL
         X3=surf_cobs_era_data(II, 2)+AIMTOL
         Y3=-surf_cobs_era_data(II, 1)-AIMTOL
         X4=surf_cobs_era_data(II, 2)+AIMTOL
         Y4=surf_cobs_era_data(II, 1)+AIMTOL
      END IF
!
      XRD=X
      YRD=Y
      XRD=XRD-surf_cobs_era_data(II, 4)
      YRD=YRD-surf_cobs_era_data(II, 3)
      XR=(XRD*DCOS(surf_cobs_era_data(II, 6)))+(YRD*DSIN(surf_cobs_era_data(II, 6)))
      YR=(YRD*DCOS(surf_cobs_era_data(II, 6)))-(XRD*DSIN(surf_cobs_era_data(II, 6)))
!
!       ARE THE POINTS XR AND YR ON THE POLYGON OR INSIDE
      IF(N.EQ.4) THEN
         XT(1)=X1
         YT(1)=Y1
         XT(2)=X2
         YT(2)=Y2
         XT(3)=X3
         YT(3)=Y3
         XT(4)=X4
         YT(4)=Y4
      ELSE
         XT(1)=X1
         YT(1)=Y1
         XT(2)=X2
         YT(2)=Y2
         XT(3)=X3
         YT(3)=Y3
         XT(4)=X4
         YT(4)=Y4
         XT(5)=X5
         YT(5)=Y5
         XT(6)=X6
         YT(6)=Y6
         XT(7)=X7
         YT(7)=Y7
         XT(8)=X8
         YT(8)=Y8
      END IF
      NP=N
      X0=XR
      Y0=XR
      INS=INSID2()
      IF(INS) THEN
!       RAY NOT BLOCKED
         LS1=0.0D0
      ELSE
!       RAY BLOCKED
         LS1=10.0D0
      END IF
! NOW IS THE POINT INSIDE ANY OF THE FOUR CIRCLES
!       CENTER OF THE FIRST CIRCLE IS AT
      XC1=-surf_cobs_era_data(II, 2)+surf_cobs_era_data(II, 5)
      YC1= surf_cobs_era_data(II, 1)-surf_cobs_era_data(II, 5)
      CS1=DSQRT(((XR-XC1)**2)+((YR-YC1)**2))
!       CENTER OF THE SECOND CIRCLE IS AT
      XC2= -surf_cobs_era_data(II, 2)+surf_cobs_era_data(II, 5)
      YC2= -surf_cobs_era_data(II, 1)+surf_cobs_era_data(II, 5)
      CS2=DSQRT(((XR-XC2)**2)+((YR-YC2)**2))
!       CENTER OF THE THIRD CIRCLE IS AT
      XC3= surf_cobs_era_data(II, 2)-surf_cobs_era_data(II, 5)
      YC3=-surf_cobs_era_data(II, 1)+surf_cobs_era_data(II, 5)
      CS3=DSQRT(((XR-XC3)**2)+((YR-YC3)**2))
!       CENTER OF THE FIRST CIRCLE IS AT
      XC4=surf_cobs_era_data(II, 2)-surf_cobs_era_data(II, 5)
      YC4=surf_cobs_era_data(II, 1)-surf_cobs_era_data(II, 5)
      CS4=DSQRT(((XR-XC4)**2)+((YR-YC4)**2))
!
      RAD2=DSQRT(surf_cobs_era_data(II, 5)**2)+AIMTOL
!
      IF(INS.OR.REAL(CS1).GT.REAL(RAD2).OR.REAL(CS2).GT.REAL(RAD2).OR.REAL(CS3).GT.REAL(RAD2).OR.REAL(CS4).GT.REAL(RAD2)) THEN
!     RAD BLOCKED BY BOX OR A CIRCLE
         LS1=10.0D0
      ELSE
         LS1=0.0D0
      END IF
!
      IF(LS1.EQ.0.0D0) LS=0.0D0
      IF(LS1.EQ.10.0D0) LS=10.0D0
!
      RETURN
!       COERAS NOT 4
   END IF
!
!       COERAS=5 POLY COBS ERASE, DOES IT STOP THE RAY
   IF(COERAS.EQ.5) THEN
      LS1=0.0D0
!
!       IN THE COORDINATE SYSTEM OF THE POLYGON, THE CORNER
!       NUMBER OF POINTS IS surf_cobs_era_data(II, 2), CENTER TO CORNER DISTANCE
!       IS surf_cobs_era_data(II, 1). POINTS GO COUNTER CLOCKWISE LOOKING
!       TOWARD THE +Z DIRECTION
!       COORDINATES ARE:
      ANGLE=0.0D0
      DO III=1,INT(surf_cobs_era_data(II, 2))
         XT(III)=surf_cobs_era_data(II, 1)*DCOS(ANGLE+(PII/2.0D0))
         YT(III)=surf_cobs_era_data(II, 1)*DSIN(ANGLE+(PII/2.0D0))
         ANGLE=ANGLE+((TWOPII)/surf_cobs_era_data(II, 2))
      END DO
      XRD=X
      YRD=Y
!
!       IF NON-ZERO COBS ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
!       IN COBS ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
!       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
!       OBSCURATION ERASE. REMEMBER.
!
      XRD=XRD-surf_cobs_era_data(II, 4)
      YRD=YRD-surf_cobs_era_data(II, 3)
!
!       IF A NON-ZERO COBS ERASE TILT EXISTS, IT MUST BE CONSIDERED
!       IN COBS ERASE CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
!       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
!       CLEAR APERTURE ERASE. REMEMBER.

      XR=(XRD*DCOS(surf_cobs_era_data(II, 6)))+(YRD*DSIN(surf_cobs_era_data(II, 6)))
      YR=(YRD*DCOS(surf_cobs_era_data(II, 6)))-(XRD*DSIN(surf_cobs_era_data(II, 6)))
!
!       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
      X0=XR
      Y0=YR
      NP=INT(surf_cobs_era_data(II, 2))
      INS=INSID2()
      IF(INS) THEN
!       RAY NOT BLOCKED
         LS1=0.0D0
      ELSE
!       RAY BLOCKED
         LS1=10.0D0
      END IF
      IF(LS1.EQ.0.0D0) LS=0.0D0
      IF(LS1.EQ.10.0D0) LS=10.0D0
      RETURN
!       COERAS NOT 5
   END IF
!
!       COERAS=6 IPOLY COBS ERASE, DOES IT STOP THE RAY
   IF(COERAS.EQ.6) THEN
      LS1=0.0D0
!
!       IN THE COORDINATE SYSTEM OF THE POLYGON, THE CORNER
!       NUMBER OF POINTS IS surf_cobs_era_data(II, 2), CENTER TO CORNER DISTANCE
!       IS surf_cobs_era_data(II, 1). POINTS GO COUNTER CLOCKWISE LOOKING
!       TOWARD THE +Z DIRECTION
!       COORDINATES ARE:
      ANGLE=0.0D0
      DO III=1,INT(surf_cobs_era_data(II, 2))
         XT(III)=IPOLYX(III,II,4)
         YT(III)=IPOLYY(III,II,4)
      END DO
      XRD=X
      YRD=Y
!
!       IF NON-ZERO COBS ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
!       IN COBS ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
!       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
!       OBSCURATION ERASE. REMEMBER.
!
      XRD=XRD-surf_cobs_era_data(II, 4)
      YRD=YRD-surf_cobs_era_data(II, 3)
!
!       IF A NON-ZERO COBS ERASE TILT EXISTS, IT MUST BE CONSIDERED
!       IN COBS ERASE CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
!       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
!       CLEAR APERTURE ERASE. REMEMBER.

      XR=(XRD*DCOS(surf_cobs_era_data(II, 6)))+(YRD*DSIN(surf_cobs_era_data(II, 6)))
      YR=(YRD*DCOS(surf_cobs_era_data(II, 6)))-(XRD*DSIN(surf_cobs_era_data(II, 6)))
!
!       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
      X0=XR
      Y0=YR
      NP=INT(surf_cobs_era_data(II, 2))
      INS=INSID2()
      IF(INS) THEN
!       RAY NOT BLOCKED
         LS1=0.0D0
      ELSE
!       RAY BLOCKED
         LS1=10.0D0
      END IF
      IF(LS1.EQ.0.0D0) LS=0.0D0
      IF(LS1.EQ.10.0D0) LS=10.0D0
      RETURN
!       COERAS NOT 6
   END IF
!
   RETURN
END
! SUB CAERRS.FOR

SUBROUTINE CAERRS
!
   use DATLEN
   use mod_surface
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE CAERRS.FOR. THIS SUBROUTINE IMPLEMENTS
!       THE CLEAR APERTURE ERASE CHECKING AS CALLED BY CACHEK.FOR
!
   EXTERNAL INSID1
!
   INTEGER II,I,N,III
!
   INTEGER CAERAS,COERAS
!
   real(real64) X,Y,Z,ANGLE,XR,YR,LS1,LS,RS,XRD,YRD,X1,X2,X3,X4,Y1,Y2,Y3,Y4,X5,X6,X7,X8,Y5,Y6,Y7,Y8,XC1,XC2,XC3,XC4,YC1,YC2,YC3,YC4,RAD2,MAXSID,CS1,CS2,CS3,CS4
!
   COMMON/CACO/CAERAS,COERAS,LS
!
   LOGICAL INS,INSID1
!
!
!       NOW ALL THE CAERAS WAS ALREADY SET AND THE RAY WAS
!       BLOCKED BY A CLAP ON SURFACE I. DOES THE BLOCK GET CANCELED
!       BY THE CLAP ERASE ON I+I = II
!
   I=R_I
   X=R_X
   Y=R_Y
   Z=R_Z
   II=I
!
!       CAERAS=1 CIRCULAR CLAP ERASE, DOES IT STOP THE RAY
   IF(CAERAS.EQ.1) THEN
      LS1=0.0D0
!
!       CIRCULAR CLAP EQUATION IS:
!
!       (X)**2 + (Y)**2 = R**2
!
!       SUB X AND Y IN THE LEFT SIDE. IF THE LEFT SIDE IS
!       GREATER THAN THE RIGHT SIDE, THE RAY IS BLOCKED
!       ELSE IT IS NOT BLOCKED.
!
!       IF NON-ZERO CLAP ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
!       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
!       TO XR AND YR IN THE COORDINATE SYSTEM OF THE DECENTERED
!       CLEAR APERTURE ERASE. REMEMBER.

      XR=X-surf_cobs_ape_data(II, 4)
      YR=Y-surf_cobs_ape_data(II, 3)
!
      LS1=DSQRT((XR**2)+(YR**2))
!
      RS=DSQRT(surf_cobs_ape_data(II, 1)**2)+AIMTOL
      IF(REAL(LS1).GT.REAL(RS)) THEN
         LS1=10.0D0
      ELSE
         LS1=0.0D0
      END IF
      IF(LS1.EQ.0.0D0) LS=0.0D0
      IF(LS1.EQ.10.0D0) LS=10.0D0
      RETURN
!       CAERAS NOT 1
   END IF
!
!
!       CAERAS=2 RECTANGULAR CLAP ERASE, DOES IT STOP THE RAY
   IF(CAERAS.EQ.2) THEN
      LS1=0.0D0
!
!       IN THE COORDINATE SYSTEM OF THE RECTANGLE, THE CORNER
!       COORDINATES ARE:
      X1=-surf_cobs_ape_data(II, 2)-AIMTOL
      Y1=surf_cobs_ape_data(II, 1)+AIMTOL
      X2=-surf_cobs_ape_data(II, 2)-AIMTOL
      Y2=-surf_cobs_ape_data(II, 1)-AIMTOL
      X3=surf_cobs_ape_data(II, 2)+AIMTOL
      Y3=-surf_cobs_ape_data(II, 1)-AIMTOL
      X4=surf_cobs_ape_data(II, 2)+AIMTOL
      Y4=surf_cobs_ape_data(II, 1)+AIMTOL
!
      XRD=X
      YRD=Y
!
!       IF NON-ZERO CLAP ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
!       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
!       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
!       CLEAR APERTURE ERASE. REMEMBER.
!
      XRD=XRD-surf_cobs_ape_data(II, 4)
      YRD=YRD-surf_cobs_ape_data(II, 3)
!
!       IF A NON-ZERO CLAP ERASE TILT EXISTS, IT MUST BE CONSIDERED
!       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
!       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
!       CLEAR APERTURE ERASE. REMEMBER.

      XR=(XRD*DCOS(surf_cobs_ape_data(II, 6)))+(YRD*DSIN(surf_cobs_ape_data(II, 6)))
      YR=(YRD*DCOS(surf_cobs_ape_data(II, 6)))-(XRD*DSIN(surf_cobs_ape_data(II, 6)))
!
!       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
      XT(1)=X1
      YT(1)=Y1
      XT(2)=X2
      YT(2)=Y2
      XT(3)=X3
      YT(3)=Y3
      XT(4)=X4
      YT(4)=Y4
      X0=XR
      Y0=YR
      NP=4
      INS=INSID1()
      IF(INS) THEN
!       RAY NOT BLOCKED
         LS1=0.0D0
      ELSE
!       RAY BLOCKED
         LS1=10.0D0
      END IF
      IF(LS1.EQ.0.0D0) LS=0.0D0
      IF(LS1.EQ.10.0D0) LS=10.0D0
      RETURN
!       CAERAS NOT 2
   END IF
!
!       CAERAS=3 ELLIPTICAL CLAP, DOES IT STOP THE RAY
   IF(CAERAS.EQ.3) THEN
      LS1=0.0D0
!
!       ELLIPTICAL CLAP EQUATION IS:
!
!       (X)**2/A**2  + (Y)**2/B**2 = 1
!
!       SUB X AND Y IN THE LEFT SIDE. IF THE LEFT SIDE IS
!       GREATER THAN 1.0D0, THE RAY IS BLOCKED
!       ELSE IT IS NOT BLOCKED.
!
      XRD=X
      YRD=Y
!       IF NON-ZERO CLAP ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
!       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
!       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
!       CLEAR APERTURE ERASE. REMEMBER.
!
      XRD=XRD-surf_cobs_ape_data(II, 4)
      YRD=YRD-surf_cobs_ape_data(II, 3)
!
!       IF A NON-ZERO CLAP ERASE TILT EXISTS, IT MUST BE CONSIDERED
!       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
!       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
!       CLEAR APERTURE ERASE.
!
      XR=(XRD*DCOS(surf_cobs_ape_data(II, 6)))+(YRD*DSIN(surf_cobs_ape_data(II, 6)))
      YR=(YRD*DCOS(surf_cobs_ape_data(II, 6)))-(XRD*DSIN(surf_cobs_ape_data(II, 6)))
!
!
      LS=((XR**2)/(surf_cobs_ape_data(II, 2)**2))+((YR**2)/(surf_cobs_ape_data(II, 1)**2))
!
      IF(REAL(LS).GT.(1.0+(AIMTOL**2))) THEN
!       RAY BLOCKED
         LS1=10.0D0
      ELSE
!       NOT BLOCKED
         LS1=0.0D0
      END IF
      IF(LS1.EQ.0.0D0) LS=0.0D0
      IF(LS1.EQ.10.0D0) LS=10.0D0
      RETURN
!       CAERAS NOT 3
   END IF
!
!
!       CAERAS=4 RACETRACK CLAP ERASE, DOES IT STOP THE RAY
   IF(CAERAS.EQ.4) THEN
      LS1=0.0D0
!
!       IF THE RADIUS OF THE CORNERS IS LESS THAN THE LARGER
!       OF THE TWO SIDE DIMENSIONS, AN 8 SIDED POLYGON NEEDS TO BE
!       CHECKED AS WELL AS THE FOUR CIRCLES. IF NOT, THE BASE RECTANGE
!       AND THE FOUR CIRCLES NEED TO BE CHECKED.
!
      IF(surf_cobs_ape_data(II, 1).LE.surf_cobs_ape_data(II, 2)) THEN
!       surf_cobs_ape_data(II, 2) = MAXSID
         MAXSID=surf_cobs_ape_data(II, 2)
      ELSE
         MAXSID=surf_cobs_ape_data(II, 1)
      END IF
      IF(surf_cobs_ape_data(II, 5).LT.MAXSID) THEN
!       SETUP THE 8 SIDED BOX
         N=8
         X1=-surf_cobs_ape_data(II, 2)+surf_cobs_ape_data(II, 5)-AIMTOL
         Y1=surf_cobs_ape_data(II, 1)+AIMTOL
         X2=-surf_cobs_ape_data(II, 2)-AIMTOL
         Y2=surf_cobs_ape_data(II, 1)-surf_cobs_ape_data(II, 5)+AIMTOL
         X3=-surf_cobs_ape_data(II, 2)-AIMTOL
         Y3=-surf_cobs_ape_data(II, 1)+surf_cobs_ape_data(II, 5)-AIMTOL
         X4=-surf_cobs_ape_data(II, 2)+surf_cobs_ape_data(II, 5)-AIMTOL
         Y4=-surf_cobs_ape_data(II, 1)-AIMTOL
         X5=surf_cobs_ape_data(II, 2)-surf_cobs_ape_data(II, 5)+AIMTOL
         Y5=-surf_cobs_ape_data(II, 1)-AIMTOL
         X6=surf_cobs_ape_data(II, 2)+AIMTOL
         Y6=-surf_cobs_ape_data(II, 1)+surf_cobs_ape_data(II, 5)-AIMTOL
         X7=surf_cobs_ape_data(II, 2)+AIMTOL
         Y7=surf_cobs_ape_data(II, 1)-surf_cobs_ape_data(II, 5)+AIMTOL
         X8=surf_cobs_ape_data(II, 2)-surf_cobs_ape_data(II, 5)+AIMTOL
         Y8=surf_cobs_ape_data(II, 1)+AIMTOL
      ELSE
!       SET UP THE FOUR SIDED BOX
         N=4
         X1=-surf_cobs_ape_data(II, 2)-AIMTOL
         Y1=surf_cobs_ape_data(II, 1)+AIMTOL
         X2=-surf_cobs_ape_data(II, 2)-AIMTOL
         Y2=-surf_cobs_ape_data(II, 1)-AIMTOL
         X3=surf_cobs_ape_data(II, 2)+AIMTOL
         Y3=-surf_cobs_ape_data(II, 1)-AIMTOL
         X4=surf_cobs_ape_data(II, 2)+AIMTOL
         Y4=surf_cobs_ape_data(II, 1)+AIMTOL
      END IF
!
      XRD=X
      YRD=Y
!
!       IF NON-ZERO CLAP ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
!       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
!       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
!       CLEAR APERTURE ERASE. REMEMBER.
!
      XRD=XRD-surf_cobs_ape_data(II, 4)
      YRD=YRD-surf_cobs_ape_data(II, 3)
!
!       IF A NON-ZERO CLAP ERASE TILT EXISTS, IT MUST BE CONSIDERED
!       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
!       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
!       CLEAR APERTURE ERASE .
!
      XR=(XRD*DCOS(surf_cobs_ape_data(II, 6)))+(YRD*DSIN(surf_cobs_ape_data(II, 6)))
      YR=(YRD*DCOS(surf_cobs_ape_data(II, 6)))-(XRD*DSIN(surf_cobs_ape_data(II, 6)))
!
!
!       ARE THE POINTS XR AND YR ON THE POLYGON OR OUTSIDE
      IF(N.EQ.4) THEN
         XT(1)=X1
         YT(1)=Y1
         XT(2)=X2
         YT(2)=Y2
         XT(3)=X3
         YT(3)=Y3
         XT(4)=X4
         YT(4)=Y4
      ELSE
         XT(1)=X1
         YT(1)=Y1
         XT(2)=X2
         YT(2)=Y2
         XT(3)=X3
         YT(3)=Y3
         XT(4)=X4
         YT(4)=Y4
         XT(5)=X5
         YT(5)=Y5
         XT(6)=X6
         YT(6)=Y6
         XT(7)=X7
         YT(7)=Y7
         XT(8)=X8
         YT(8)=Y8
      END IF
      NP=N
      X0=XR
      Y0=YR
      INS=INSID1()
      IF(INS) THEN
!       RAY NOT BLOCKED
         LS1=0.0D0
      ELSE
!       RAY BLOCKED
         LS1=10.0D0
      END IF
! NOW IS THE POINT INSIDE ANY OF THE FOUR CIRCLES
!       CENTER OF THE FIRST CIRCLE IS AT
      XC1=-surf_cobs_ape_data(II, 2)+surf_cobs_ape_data(II, 5)
      YC1= surf_cobs_ape_data(II, 1)-surf_cobs_ape_data(II, 5)
      CS1=DSQRT(((XR-XC1)**2)+((YR-YC1)**2))
!       CENTER OF THE SECOND CIRCLE IS AT
      XC2=-surf_cobs_ape_data(II, 2)+surf_cobs_ape_data(II, 5)
      YC2=-surf_cobs_ape_data(II, 1)+surf_cobs_ape_data(II, 5)
      CS2=DSQRT(((XR-XC2)**2)+((YR-YC2)**2))
!       CENTER OF THE THIRD CIRCLE IS AT
      XC3= surf_cobs_ape_data(II, 2)-surf_cobs_ape_data(II, 5)
      YC3=-surf_cobs_ape_data(II, 1)+surf_cobs_ape_data(II, 5)
      CS3=DSQRT(((XR-XC3)**2)+((YR-YC3)**2))
!       CENTER OF THE FIRST CIRCLE IS AT
      XC4=surf_cobs_ape_data(II, 2)-surf_cobs_ape_data(II, 5)
      YC4=surf_cobs_ape_data(II, 1)-surf_cobs_ape_data(II, 5)
      CS4=DSQRT(((XR-XC4)**2)+((YR-YC4)**2))
!
      RAD2=DSQRT(surf_cobs_ape_data(II, 5)**2)+AIMTOL
!
      IF(.NOT.INS.AND.REAL(CS1).GT.REAL(RAD2).AND.REAL(CS2).GT.REAL(RAD2).AND.REAL(CS3).GT.REAL(RAD2).AND.REAL(CS4).GT.REAL(RAD2)) THEN
!     RAY BLOCKED BY BOX AND CIRCLES
         LS1=10.0D0
      ELSE
         LS1=0.0D0
      END IF
      IF(LS1.EQ.0.0D0) LS=0.0D0
      IF(LS1.EQ.10.0D0) LS=10.0D0
!
      RETURN
!       CAERAS NOT 4
   END IF
!
!       CAERAS=5 POLY CLAP ERASE, DOES IT STOP THE RAY
   IF(CAERAS.EQ.5) THEN
      LS1=0.0D0
!
!       IN THE COORDINATE SYSTEM OF THE POLYGON, THE CORNER
!       NUMBER OF POINTS IS surf_cobs_ape_data(II, 2), CENTER TO CORNER DISTANCE
!       IS surf_cobs_ape_data(II, 1). POINTS GO COUNTER CLOCKWISE LOOKING
!       TOWARD THE +Z DIRECTION
!       COORDINATES ARE:
      ANGLE=0.0D0
      DO III=1,INT(surf_cobs_ape_data(II, 2))
         XT(III)=surf_cobs_ape_data(II, 1)*DCOS(ANGLE+(PII/2.0D0))
         YT(III)=surf_cobs_ape_data(II, 1)*DSIN(ANGLE+(PII/2.0D0))
         ANGLE=ANGLE+((TWOPII)/surf_cobs_ape_data(II, 2))
      END DO
      XRD=X
      YRD=Y
!
!       IF NON-ZERO CLAP ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
!       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
!       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
!       CLEAR APERTURE ERASE. REMEMBER.
!
      XRD=XRD-surf_cobs_ape_data(II, 4)
      YRD=YRD-surf_cobs_ape_data(II, 3)
!
!       IF A NON-ZERO CLAP ERASE TILT EXISTS, IT MUST BE CONSIDERED
!       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
!       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
!       CLEAR APERTURE ERASE. REMEMBER.

      XR=(XRD*DCOS(surf_cobs_ape_data(II, 6)))+(YRD*DSIN(surf_cobs_ape_data(II, 6)))
      YR=(YRD*DCOS(surf_cobs_ape_data(II, 6)))-(XRD*DSIN(surf_cobs_ape_data(II, 6)))
!
!       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
      X0=XR
      Y0=YR
      NP=INT(surf_cobs_ape_data(II, 2))
      INS=INSID1()
      IF(INS) THEN
!       RAY NOT BLOCKED
         LS1=0.0D0
      ELSE
!       RAY BLOCKED
         LS1=10.0D0
      END IF
      IF(LS1.EQ.0.0D0) LS=0.0D0
      IF(LS1.EQ.10.0D0) LS=10.0D0
      RETURN
!       CAERAS NOT 5
   END IF
!       CAERAS=6 IPOLY CLAP ERASE, DOES IT STOP THE RAY
   IF(CAERAS.EQ.6) THEN
      LS1=0.0D0
!
!       IN THE COORDINATE SYSTEM OF THE POLYGON, THE CORNER
!       NUMBER OF POINTS IS surf_cobs_ape_data(II, 2), CENTER TO CORNER DISTANCE
!       IS surf_cobs_ape_data(II, 1). POINTS GO COUNTER CLOCKWISE LOOKING
!       TOWARD THE +Z DIRECTION
!       COORDINATES ARE:
      ANGLE=0.0D0
      DO III=1,INT(surf_cobs_ape_data(II, 2))
         XT(III)=IPOLYX(III,II,2)
         YT(III)=IPOLYY(III,II,2)
      END DO
      XRD=X
      YRD=Y
!
!       IF NON-ZERO CLAP ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
!       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
!       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
!       CLEAR APERTURE ERASE. REMEMBER.
!
      XRD=XRD-surf_cobs_ape_data(II, 4)
      YRD=YRD-surf_cobs_ape_data(II, 3)
!
!       IF A NON-ZERO CLAP ERASE TILT EXISTS, IT MUST BE CONSIDERED
!       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
!       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
!       CLEAR APERTURE ERASE. REMEMBER.

      XR=(XRD*DCOS(surf_cobs_ape_data(II, 6)))+(YRD*DSIN(surf_cobs_ape_data(II, 6)))
      YR=(YRD*DCOS(surf_cobs_ape_data(II, 6)))-(XRD*DSIN(surf_cobs_ape_data(II, 6)))
!
!       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
      X0=XR
      Y0=YR
      NP=INT(surf_cobs_ape_data(II, 2))
      INS=INSID1()
      IF(INS) THEN
!       RAY NOT BLOCKED
         LS1=0.0D0
      ELSE
!       RAY BLOCKED
         LS1=10.0D0
      END IF
      IF(LS1.EQ.0.0D0) LS=0.0D0
      IF(LS1.EQ.10.0D0) LS=10.0D0
      RETURN
!       CAERAS NOT 6
   END IF
!
   RETURN
END
! SUB FNBDE.FOR
SUBROUTINE FNBDE(I)
!
   use DATCFG
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE FNBDE(I) WHICH REMOVES ALL
!       FNBY,FNBX,ERY,ERX,MAGY AND MAGX ENTRIES FROM CONFIG I.
!
   INTEGER SCRCNT,I,J,ALLOERR,II
!
   CHARACTER BLANK*140
!
   COMMON/BLAAA/BLANK
!
!
   CHARACTER*140 SCRATH
   DIMENSION SCRATH(:)
   ALLOCATABLE :: SCRATH
   INTEGER NANA
   NANA=2000
   DEALLOCATE (SCRATH,STAT=ALLOERR)
   ALLOCATE (SCRATH(NANA),STAT=ALLOERR)
!
   BLANK=AA//AA//AA//AA//AA//AA//AA
   SCRATH(1:2000)=BLANK
   SCRCNT=0
   DO 15 J=1,CFGCNT(I)
      EE12=CONFG(I,J)
      HOLDER=EE12
      IF((HOLDER(1:3)).EQ.'MAG'.OR.(HOLDER(1:3)).EQ.'FNB'.OR.(HOLDER(1:2)).EQ.'ER') THEN
         IF((HOLDER(1:4)).EQ.'FNBY') THEN
            WRITE(OUTLYNE,*)'"FNBY" REMOVED FROM CONFIGURATION #',I
            CALL SHOWIT(1)
         ELSE
         END IF
         IF((HOLDER(1:4)).EQ.'FNBX') THEN

            WRITE(OUTLYNE,*)'"FNBX" REMOVED FROM CONFIGURATION #',I
            CALL SHOWIT(1)
         END IF
         IF((HOLDER(1:3)).EQ.'ERY') THEN

            WRITE(OUTLYNE,*)'"ERY" REMOVED FROM CONFIGURATION #',I
            CALL SHOWIT(1)
         END IF
         IF((HOLDER(1:3)).EQ.'ERX') THEN

            WRITE(OUTLYNE,*)'"ERX" REMOVED FROM CONFIGURATION #',I
            CALL SHOWIT(1)
         END IF
         IF((HOLDER(1:4)).EQ.'MAGY') THEN

            WRITE(OUTLYNE,*)'"MAGY" REMOVED FROM CONFIGURATION #',I
            CALL SHOWIT(1)
         END IF
         IF((HOLDER(1:4)).EQ.'MAGX') THEN

            WRITE(OUTLYNE,*)'"MAGX" REMOVED FROM CONFIGURATION #',I
            CALL SHOWIT(1)
         END IF
         HOLDER=BLANK
         GO TO 15
      ELSE
         SCRATH(J)=HOLDER
         SCRCNT=SCRCNT+1
      END IF
15 CONTINUE
   DO 16 J=1,SCRCNT
      EE12=SCRATH(J)(1:140)
      CONFG(I,J)=EE12
16 CONTINUE
   CFGCNT(I)=SCRCNT
   DEALLOCATE(SCRATH,STAT=ALLOERR)
   RETURN
END
! SUB GLSWVL.FOR
SUBROUTINE GLSWVL
   USE NSSMOD
!
   use DATLEN
   use mod_surface
   use DATMAI
   use command_utils, only: is_command_query
   use mod_system, only: sys_wavelength, sys_wl_weight
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS CONTROLS THE OPERATION OF THE "GLASSWV" COMMAND
!       COMMAND
!
!
   IF(SST.EQ.1) THEN
      WRITE(OUTLYNE,*)'"GLASSWV" TAKES NO STRING INPUT'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      RETURN
   END IF
   IF(SQ.EQ.1.AND.WQ.NE.'LENS'.AND.WQ.NE.'NSSLENS') THEN
      WRITE(OUTLYNE,*)'"GLASSWV" TAKES NO QUALIFIER WORD OR IT TAKES'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'"LENS" OR "NSSLENS"'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(WQ.EQ.'LENS') THEN
      GLSWV(1:10)=0.0D0
      GLSWV(1)=sys_wavelength(1)
      GLSWV(2)=sys_wavelength(2)
      GLSWV(3)=sys_wavelength(3)
      GLSWV(4)=sys_wavelength(4)
      GLSWV(5)=sys_wavelength(5)
      GLSWV(6)=sys_wavelength(6)
      GLSWV(7)=sys_wavelength(7)
      GLSWV(8)=sys_wavelength(8)
      GLSWV(9)=sys_wavelength(9)
      GLSWV(10)=sys_wavelength(10)
   END IF
   IF(WQ.EQ.'NSSLENS') THEN
      IF(NEXISTN) THEN
         GLSWV(1:10)=0.0D0
         GLSWV(1)=NSSSYSTEM(1)
         GLSWV(2)=NSSSYSTEM(2)
         GLSWV(3)=NSSSYSTEM(3)
         GLSWV(4)=NSSSYSTEM(4)
         GLSWV(5)=NSSSYSTEM(5)
         GLSWV(6)=NSSSYSTEM(6)
         GLSWV(7)=NSSSYSTEM(7)
         GLSWV(8)=NSSSYSTEM(8)
         GLSWV(9)=NSSSYSTEM(9)
         GLSWV(10)=NSSSYSTEM(10)
      ELSE
         WRITE(OUTLYNE,*)'NO NSS DATABASE EXISTS, "GLASSWV NSSLENS"'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'CAN NOT SET GLASS WAVELENGTHS'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(is_command_query().OR.SN.EQ.0.AND.SQ.EQ.0.AND.SST.EQ.0) THEN
100   FORMAT('THE TEN GLASS EVALUATION WAVELENGTHS IN MICRONS CURRENTLY ARE:')
101   FORMAT('WAVELENGTH #1  = ',G13.6,'MICRONS')
102   FORMAT('WAVELENGTH #2  = ',G13.6,'MICRONS')
103   FORMAT('WAVELENGTH #3  = ',G13.6,'MICRONS')
104   FORMAT('WAVELENGTH #4  = ',G13.6,'MICRONS')
105   FORMAT('WAVELENGTH #5  = ',G13.6,'MICRONS')
106   FORMAT('WAVELENGTH #6  = ',G13.6,'MICRONS')
107   FORMAT('WAVELENGTH #7  = ',G13.6,'MICRONS')
108   FORMAT('WAVELENGTH #8  = ',G13.6,'MICRONS')
109   FORMAT('WAVELENGTH #9  = ',G13.6,'MICRONS')
110   FORMAT('WAVELENGTH #10 = ',G13.6,'MICRONS')
      WRITE(OUTLYNE,101) GLSWV(1)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,102) GLSWV(2)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,103) GLSWV(3)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,104) GLSWV(4)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,105) GLSWV(5)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,106) GLSWV(6)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,107) GLSWV(7)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,108) GLSWV(8)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,109) GLSWV(9)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,110) GLSWV(10)
      CALL SHOWIT(0)
      RETURN
   END IF
   IF(SQ.EQ.0) THEN
      GLSWV(1:10)=0.0D0
      IF(DF1.EQ.0) GLSWV(1)=W1
      IF(DF2.EQ.0) GLSWV(2)=W2
      IF(DF3.EQ.0) GLSWV(3)=W3
      IF(DF4.EQ.0) GLSWV(4)=W4
      IF(DF5.EQ.0) GLSWV(5)=W5
   END IF
!     ASSIGN NEW VALUES

   RETURN
END
! SUB GLSRIN.FOR
SUBROUTINE GLSRIN
   USE GLOBALS
   use glass_manager
   use type_utils, only: real2str
!
   use DATLEN
   use mod_surface
   use DATMAI
   use command_utils, only: is_command_query
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       IMPLICIT NONE
!
   LOGICAL LOWER,EXIS36
   logical :: boolResult
!
   CHARACTER FLNAME*12,NAME*13,NUMBER*13,FLTP*12
!
   CHARACTER*13 NAME1,NAME2
!
   INTEGER I,J,TOTAL,COUNT,LASCNT, uG, gdb_loc
!
   real(real64) LMAX,LMIN,LAMBDA,A0,A1,A2,A3,A4,A5,PN,LAM(1:10),LAMLOW,LAMUPP,PNSC
!
!
!       NOW RESOLVE THE CALCULATION OF REFRACTIVE INDICES VIA
!       GLASS CATALOG SEARCHES.
!
!       THERE IS AN EQUATION FOR CALCULATION OF INDICES USED
!       ACROSS THE INDUSTRY. IT IS
!
   PN(LAMBDA,A0,A1,A2,A3,A4,A5)=DSQRT(A0+(A1*(LAMBDA**2))+(A2*(1.0D0/(LAMBDA**2)))+(A3*(1.0D0/(LAMBDA**4)))+(A4*(1.0D0/(LAMBDA**6)))+(A5*(1.0D0/(LAMBDA**8))))
!
   PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)=DSQRT(((A0*(LAMBDA**2))/((LAMBDA**2)-A3))+((A1*(LAMBDA**2))/((LAMBDA**2)-A4))+((A2*(LAMBDA**2))/((LAMBDA**2)-A5))+1.0D0)


   !call LogTermFOR("In GLSRIN")
!
!       DETERMINE MINIMUM AND MAX GLSWV WAVELENGTHS
   LMIN=GLSWV(1)
   IF(GLSWV(2).LT.LMIN) LMIN=GLSWV(2)
   IF(GLSWV(3).LT.LMIN) LMIN=GLSWV(3)
   IF(GLSWV(4).LT.LMIN) LMIN=GLSWV(4)
   IF(GLSWV(5).LT.LMIN) LMIN=GLSWV(5)
   LMAX=GLSWV(1)
   IF(GLSWV(2).GT.LMAX) LMAX=GLSWV(2)
   IF(GLSWV(3).GT.LMAX) LMAX=GLSWV(3)
   IF(GLSWV(4).GT.LMAX) LMAX=GLSWV(4)
   IF(GLSWV(5).GT.LMAX) LMAX=GLSWV(5)
   IF(LMIN.EQ.0.0D0) LMIN=LMAX
   IF(LMAX.EQ.0.0D0) LMAX=LMIN
!       SET LAM(1) to LAM(10) TO THE 10 WAVELENGTHS
   LAM(1)=GLSWV(1)
   LAM(2)=GLSWV(2)
   LAM(3)=GLSWV(3)
   LAM(4)=GLSWV(4)
   LAM(5)=GLSWV(5)
   LAM(6)=GLSWV(6)
   LAM(7)=GLSWV(7)
   LAM(8)=GLSWV(8)
   LAM(9)=GLSWV(9)
   LAM(10)=GLSWV(10)
   IF(WC.EQ.'AIR') THEN
      GPREG(1)=1.0D0
      GPREG(2)=1.0D0
      GPREG(3)=1.0D0
      GPREG(4)=1.0D0
      GPREG(5)=1.0D0
      GPREG(6)=1.0D0
      GPREG(7)=1.0D0
      GPREG(8)=1.0D0
      GPREG(9)=1.0D0
      GPREG(10)=1.0D0
      RETURN
   END IF
!
!
   IF(SST.EQ.0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"'//WC//'" REQUIRES AN EXPLICIT GLASS NAME'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(SQ.EQ.1.OR.SN.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"'//WC//'" TAKES NO QUALIFIER OR NUMERIC INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(is_command_query()) THEN
      OUTLYNE='NO ADDITIONAL INFORMATION AVAILABLE'
      CALL SHOWIT(1)
      RETURN
   END IF
!
!       IS THE SURFACE MATERIAL A CATALOG MATERIAL?
   IF(gdb%isNameInCatalog(trim(WC)).OR.WC.EQ.'USER'.OR.WC.EQ.'GLAK'.OR.WC.EQ.'GLCAT'.OR.WC.EQ.'MATL') THEN

      CALL CHECKGLASSCATWAVELENGTHBOUNDS(boolResult, FLNAME)
      IF (.NOT.boolResult) THEN

         GPREG(1)=1.0D0
         GPREG(2)=1.0D0
         GPREG(3)=1.0D0
         GPREG(4)=1.0D0
         GPREG(5)=1.0D0
         GPREG(6)=1.0D0
         GPREG(7)=1.0D0
         GPREG(8)=1.0D0
         GPREG(9)=1.0D0
         GPREG(10)=1.0D0
         RETURN
      ELSE
!       PROCEED, THE GLASS IS DEFINED WITHIN THE CURRENT
!       LENS GLSWV WAVELENGTHS
      END IF
!       NOW LOOK UP THE GLASS IN THE CATALOG, RETRIEVE THE
!       COEFFICIENTS A0 TO A5 AND CALCULATE AND SET THE
!       REFRACTIVE INDICES.
!
      FLTP='            '
      IF(gdb%isNameInCatalog(trim(WC), gdb_loc)) THEN
         FLNAME=gdb%catalogFileNames(gdb_loc)
      END IF
      IF(WC.EQ.'USER')     FLNAME='USER.DAT  '
      IF(WC.EQ.'GLCAT')    FLTP='MULTI       '
      IF(WC.EQ.'GLAK')      FLTP='MULTI       '
      IF(WC.EQ.'MATL  ')   FLTP='MATL        '
      IF(WC.EQ.'RUSSIAN')  FLTP='RUSSIAN     '

!
      IF(FLTP.EQ.'            '.AND.FLNAME.NE.'USER.DAT  ') THEN
!     REGULAR GLASS CATALOG
!
         EXIS36=.FALSE.
         call logger%logText('Looking for '//trim(LIBGLA)//FLNAME)
         INQUIRE(FILE=trim(LIBGLA)//FLNAME,EXIST=EXIS36)
         IF(EXIS36) THEN
!               PROCEED
            PRINT *, "About to open ", trim(LIBGLA)//FLNAME
            call OPENGLASSFILE(trim(LIBGLA)//FLNAME, uG, TOTAL)
            DO 300 J=2,TOTAL+1
               READ(UNIT=uG,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
               IF(WS.EQ.NAME.OR.WS.EQ.NUMBER) THEN
!       CALCULATE INDEX THEN RETURN
                  IF(LAM(1).EQ.0.0D0) THEN
                     GPREG(1)=1.0D0
                  ELSE
!
                     IF(FLNAME.EQ.'HOYA.BIN')GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'HIKARI.BIN')GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'CORNIN.BIN')GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'CHANCE.BIN')GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'RADHARD.BIN')GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'OHARA-O.BIN')GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
!
                     IF(FLNAME.EQ.'SCHOTT.BIN')GPREG(1)=PNSC(LAM(1),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'SCH2000.BIN')GPREG(1)=PNSC(LAM(1),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'OHARA.BIN')GPREG(1)=PNSC(LAM(1),A0,A1,A2,A3,A4,A5)
                  END IF
                  IF(LAM(2).EQ.0.0D0) THEN
                     GPREG(2)=1.0D0
                  ELSE
!
                     IF(FLNAME.EQ.'HOYA.BIN')GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'HIKARI.BIN')GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'CORNIN.BIN')GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'CHANCE.BIN')GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'RADHARD.BIN')GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'OHARA-O.BIN')GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
!
                     IF(FLNAME.EQ.'SCHOTT.BIN')GPREG(2)=PNSC(LAM(2),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'SCH2000.BIN')GPREG(2)=PNSC(LAM(2),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'OHARA.BIN')GPREG(2)=PNSC(LAM(2),A0,A1,A2,A3,A4,A5)
                  END IF
                  IF(LAM(3).EQ.0.0D0) THEN
                     GPREG(3)=1.0D0
                  ELSE
!
                     IF(FLNAME.EQ.'HOYA.BIN')GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'HIKARI.BIN')GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'CORNIN.BIN')GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'CHANCE.BIN')GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'RADHARD.BIN')GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'OHARA-O.BIN')GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
!
                     IF(FLNAME.EQ.'SCHOTT.BIN')GPREG(3)=PNSC(LAM(3),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'SCH2000.BIN')GPREG(3)=PNSC(LAM(3),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'OHARA.BIN')GPREG(3)=PNSC(LAM(3),A0,A1,A2,A3,A4,A5)
                  END IF
                  IF(LAM(4).EQ.0.0D0) THEN
                     GPREG(4)=1.0D0
                  ELSE
!
                     IF(FLNAME.EQ.'HOYA.BIN')GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'HIKARI.BIN')GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'CORNIN.BIN')GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'CHANCE.BIN')GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'RADHARD.BIN')GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'OHARA-O.BIN')GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
!
                     IF(FLNAME.EQ.'SCHOTT.BIN')GPREG(4)=PNSC(LAM(4),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'SCH2000.BIN')GPREG(4)=PNSC(LAM(4),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'OHARA.BIN')GPREG(4)=PNSC(LAM(4),A0,A1,A2,A3,A4,A5)
                  END IF
                  IF(LAM(5).EQ.0.0D0) THEN
                     GPREG(5)=1.0D0
                  ELSE
!
                     IF(FLNAME.EQ.'HOYA.BIN')GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'HIKARI.BIN')GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'CORNIN.BIN')GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'CHANCE.BIN')GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'RADHARD.BIN')GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'OHARA-O.BIN')GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
!
                     IF(FLNAME.EQ.'SCHOTT.BIN')GPREG(5)=PNSC(LAM(5),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'SCH2000.BIN')GPREG(5)=PNSC(LAM(5),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'OHARA.BIN')GPREG(5)=PNSC(LAM(5),A0,A1,A2,A3,A4,A5)
                  END IF
                  IF(LAM(6).EQ.0.0D0) THEN
                     GPREG(6)=1.0D0
                  ELSE
!
                     IF(FLNAME.EQ.'HOYA.BIN')GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'HIKARI.BIN')GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'CORNIN.BIN')GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'CHANCE.BIN')GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'RADHARD.BIN')GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'OHARA-O.BIN')GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
!
                     IF(FLNAME.EQ.'SCHOTT.BIN')GPREG(6)=PNSC(LAM(6),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'SCH2000.BIN')GPREG(6)=PNSC(LAM(6),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'OHARA.BIN')GPREG(6)=PNSC(LAM(6),A0,A1,A2,A3,A4,A5)
                  END IF
                  IF(LAM(7).EQ.0.0D0) THEN
                     GPREG(7)=1.0D0
                  ELSE
!
                     IF(FLNAME.EQ.'HOYA.BIN')GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'HIKARI.BIN')GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'CORNIN.BIN')GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'CHANCE.BIN')GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'RADHARD.BIN')GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'OHARA-O.BIN')GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
!
                     IF(FLNAME.EQ.'SCHOTT.BIN')GPREG(7)=PNSC(LAM(7),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'SCH2000.BIN')GPREG(7)=PNSC(LAM(7),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'OHARA.BIN')GPREG(7)=PNSC(LAM(7),A0,A1,A2,A3,A4,A5)
                  END IF
                  IF(LAM(8).EQ.0.0D0) THEN
                     GPREG(8)=1.0D0
                  ELSE
!
                     IF(FLNAME.EQ.'HOYA.BIN')GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'HIKARI.BIN')GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'CORNIN.BIN')GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'CHANCE.BIN')GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'RADHARD.BIN')GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'OHARA-O.BIN')GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
!
                     IF(FLNAME.EQ.'SCHOTT.BIN')GPREG(8)=PNSC(LAM(8),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'SCH2000.BIN')GPREG(8)=PNSC(LAM(8),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'OHARA.BIN')GPREG(8)=PNSC(LAM(8),A0,A1,A2,A3,A4,A5)
                  END IF
                  IF(LAM(9).EQ.0.0D0) THEN
                     GPREG(9)=1.0D0
                  ELSE
!
                     IF(FLNAME.EQ.'HOYA.BIN')GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'HIKARI.BIN')GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'CORNIN.BIN')GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'CHANCE.BIN')GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'RADHARD.BIN')GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'OHARA-O.BIN')GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
!
                     IF(FLNAME.EQ.'SCHOTT.BIN')GPREG(9)=PNSC(LAM(9),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'SCH2000.BIN')GPREG(9)=PNSC(LAM(9),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'OHARA.BIN')GPREG(9)=PNSC(LAM(9),A0,A1,A2,A3,A4,A5)
                  END IF
                  IF(LAM(10).EQ.0.0D0) THEN
                     GPREG(10)=1.0D0
                  ELSE
!
                     IF(FLNAME.EQ.'HOYA.BIN')GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'HIKARI.BIN')GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'CORNIN.BIN')GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'CHANCE.BIN')GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'RADHARD.BIN')GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'OHARA-O.BIN')GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
!
                     IF(FLNAME.EQ.'SCHOTT.BIN')GPREG(10)=PNSC(LAM(10),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'SCH2000.BIN')GPREG(10)=PNSC(LAM(10),A0,A1,A2,A3,A4,A5)
                     IF(FLNAME.EQ.'OHARA.BIN')GPREG(10)=PNSC(LAM(10),A0,A1,A2,A3,A4,A5)
                  END IF
                  RETURN
               ELSE
!       KEEP SEARCHING THE CATALOG
               END IF
300         CONTINUE
!       CAN'T FIND THE GLASS, DO WARNING AND RETURN TO CMD LEVEL
            OUTLYNE='GLASS NOT FOUND IN THE '//WC//' CATALOG'
            CALL SHOWIT(1)
            OUTLYNE='REFRACTIVE INDICES SET TO 1.0'
            CALL SHOWIT(1)
            GPREG(1:5)=1.0D0
!
         ELSE
!       CATALOG REQUESTED NOT YET INSTALLED.
            OUTLYNE=WC//' GLASS CATALOG NOT YET INSTALLED'
            CALL SHOWIT(1)
            OUTLYNE='REFRACTIVE INDICES SET TO 1.0'
            CALL SHOWIT(1)
            GPREG(1:5)=1.0D0
         END IF
      ELSE
!     FLTP WAS NOT BLANK
      END IF
!
      IF(FLTP.EQ.'MULTI       '.AND.FLNAME.NE.'USER.DAT  ') THEN
         !call LogTermFOR("About to search all catalogs")
         IF (gdb%isGlassInAnyCatalog(WS, COUNT)) THEN
            WRITE(OUTLYNE,*) 'GLASS MFG. IS '//gdb%catalogs(COUNT)
            CALL SHOWIT(1)

            CALL CHECKGLASSCATWAVELENGTHBOUNDS(boolResult, FLNAME)
            IF (boolResult) THEN

               DO J=1,10
                  GPREG(J) = gdb%calcIndexForCurrentGlass(LAM(J))
                  !call LogTermFOR("Index is "//trim(real2str(GPREG(J))))
               END DO

            ELSE
!       CAN'T FIND THE GLASS, DO WARNING AND RETURN TO CMD LEVEL

               OUTLYNE='GLASS NOT FOUND IN ANY STANDARD OPTICAL GLASS CATALOG'
               CALL SHOWIT(1)
               OUTLYNE='REFRACTIVE INDICES SET TO 1.0'
               CALL SHOWIT(1)
               GPREG(1:10)=1.0D0

            END IF ! calc index
         END IF ! glass catalog

         !LASCNT=0
!                         DO COUNT=1,9
!                 LASCNT=COUNT
!                         IF(COUNT.EQ.1) FLNAME='SCHOTT.BIN'
!                         IF(COUNT.EQ.2) FLNAME='SCH2000.BIN'
!                         IF(COUNT.EQ.3) FLNAME='HOYA.BIN'
!                         IF(COUNT.EQ.4) FLNAME='OHARA.BIN'
!                         IF(COUNT.EQ.5) FLNAME='OHARA-O.BIN'
!                         IF(COUNT.EQ.6) FLNAME='CORNIN.BIN'
!                         IF(COUNT.EQ.7) FLNAME='CHANCE.BIN'
!                         IF(COUNT.EQ.8) FLNAME='RADHARD.BIN'
!                         IF(COUNT.EQ.9) FLNAME='HIKARI.BIN'
!                         IF(COUNT.EQ.1) WC='SCHOTT  '
!                         IF(COUNT.EQ.2) WC='SCH2000 '
!                         IF(COUNT.EQ.3) WC='HOYA    '
!                         IF(COUNT.EQ.4) WC='OHARA   '
!                         IF(COUNT.EQ.5) WC='OHARA   '
!                         IF(COUNT.EQ.6) WC='CORNIN  '
!                         IF(COUNT.EQ.7) WC='CHANCE  '
!                         IF(COUNT.EQ.8) WC='RADHARD '
!                         IF(COUNT.EQ.9) WC='HIKARI  '
!       EXIS36=.FALSE.
!         INQUIRE(FILE=trim(LIBGLA)//FLNAME,EXIST=EXIS36)
!         IF(EXIS36) THEN
! C               PROCEED
!         call LogTermFOR("About to Open "//trim(LIBGLA)//FLNAME)
!         !PRINT *, "About to open ", trim(LIBGLA)//FLNAME
!         call OPENGLASSFILE(trim(LIBGLA)//FLNAME, uG, TOTAL)
!         READ(UNIT=36,REC=1) TOTAL
!                         DO J=2,TOTAL+1
!         READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
!         IF(WS.EQ.NAME.OR.WS.EQ.NUMBER) THEN
! C     FOUND THE GLASS
!       IF(COUNT.EQ.1) WRITE(OUTLYNE,*) 'GLASS MFG. IS SCHOTT'
!       IF(COUNT.EQ.2) WRITE(OUTLYNE,*) 'GLASS MFG. IS SCHOTT(POST 2000)'
!       IF(COUNT.EQ.3) WRITE(OUTLYNE,*) 'GLASS MFG. IS HOYA'
!       IF(COUNT.EQ.4) WRITE(OUTLYNE,*) 'GLASS MFG. IS OHARA'
!       IF(COUNT.EQ.5) WRITE(OUTLYNE,*) 'GLASS MFG. IS OHARA'
!       IF(COUNT.EQ.6) WRITE(OUTLYNE,*) 'GLASS MFG. IS CORNING'
!       IF(COUNT.EQ.7) WRITE(OUTLYNE,*) 'GLASS MFG. IS CHANCE-PILKINGTON'
!       IF(COUNT.EQ.8) WRITE(OUTLYNE,*) 'GLASS MFG. IS HIKARI'
!       IF(COUNT.GE.1.AND.COUNT.LE.8) CALL SHOWIT(1)
! C
! C     NOW RECHECK THE WAVELENGTH RANGE
!       NAME1=WC//'     '
!       NAME2=WS(1:13)
!       CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
! C
! C       COMPARE LAMLOW,LAMUPP,LMIN AND LMAX
!         IF(LMAX.GT.LAMUPP.AND.LMAX.NE.0.0D0.OR.LMIN.LT.LAMLOW
!      1.AND.LMIN.NE.0.0D0) THEN
! C       STOP AND GO TO CMD LEVEL AND PRINT WARNING
!       WRITE(OUTLYNE,*)
!      1  'GLASS DEFINITION WARNING: FOR SURFACE ',I
!       CALL SHOWIT(1)
!         OUTLYNE=
!      1  'THE GLASS - '//WS
!       CALL SHOWIT(1)
!         OUTLYNE=
!      1  'IS NOT DEFINED ACROSS THE CURRENT SYSTEM WAVELENGTH RANGE'
!       CALL SHOWIT(1)
!         WRITE(OUTLYNE,200) LAMLOW,LAMUPP
!       CALL SHOWIT(1)
!         OUTLYNE=
!      1'ALL REFRACTIVE INDICES FOR THIS SURFACE HAVE BEEN SET TO 1.0'
!       CALL SHOWIT(1)
!                 GPREG(1:5)=1.0D0
!                            END IF
! C       CALCULATE INDEX THEN RETURN
!         IF(LAM(1).EQ.0.0D0) THEN
!         GPREG(1)=1.0D0
!                         ELSE
! C
!       IF(FLNAME.EQ.'HOYA.BIN')
!      1  GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'HIKARI.BIN')
!      1  GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'CORNIN.BIN')
!      1  GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'CHANCE.BIN')
!      1  GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'RADHARD.BIN')
!      1  GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'OHARA-O.BIN')
!      1  GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
! C
!       IF(FLNAME.EQ.'SCHOTT.BIN')
!      1  GPREG(1)=PNSC(LAM(1),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'SCH2000.BIN')
!      1  GPREG(1)=PNSC(LAM(1),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'OHARA.BIN')
!      1  GPREG(1)=PNSC(LAM(1),A0,A1,A2,A3,A4,A5)
!                         END IF
!         IF(LAM(2).EQ.0.0D0) THEN
!         GPREG(2)=1.0D0
!                         ELSE
! C
!       IF(FLNAME.EQ.'HOYA.BIN')
!      1  GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'HIKARI.BIN')
!      1  GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'CORNIN.BIN')
!      1  GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'CHANCE.BIN')
!      1  GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'RADHARD.BIN')
!      1  GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'OHARA-O.BIN')
!      1  GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
! C
!       IF(FLNAME.EQ.'SCHOTT.BIN')
!      1  GPREG(2)=PNSC(LAM(2),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'SCH2000.BIN')
!      1  GPREG(2)=PNSC(LAM(2),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'OHARA.BIN')
!      1  GPREG(2)=PNSC(LAM(2),A0,A1,A2,A3,A4,A5)
!                         END IF
!         IF(LAM(3).EQ.0.0D0) THEN
!         GPREG(3)=1.0D0
!                         ELSE
! C
!       IF(FLNAME.EQ.'HOYA.BIN')
!      1  GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'HIKARI.BIN')
!      1  GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'CORNIN.BIN')
!      1  GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'CHANCE.BIN')
!      1  GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'RADHARD.BIN')
!      1  GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'OHARA-O.BIN')
!      1  GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
! C
!       IF(FLNAME.EQ.'SCHOTT.BIN')
!      1  GPREG(3)=PNSC(LAM(3),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'SCH2000.BIN')
!      1  GPREG(3)=PNSC(LAM(3),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'OHARA.BIN')
!      1  GPREG(3)=PNSC(LAM(3),A0,A1,A2,A3,A4,A5)
!                         END IF
!         IF(LAM(4).EQ.0.0D0) THEN
!         GPREG(4)=1.0D0
!                         ELSE
! C
!       IF(FLNAME.EQ.'HOYA.BIN')
!      1  GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'HIKARI.BIN')
!      1  GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'CORNIN.BIN')
!      1  GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'CHANCE.BIN')
!      1  GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'RADHARD.BIN')
!      1  GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'OHARA-O.BIN')
!      1  GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
! C
!       IF(FLNAME.EQ.'SCHOTT.BIN')
!      1  GPREG(4)=PNSC(LAM(4),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'SCH2000.BIN')
!      1  GPREG(4)=PNSC(LAM(4),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'OHARA.BIN')
!      1  GPREG(4)=PNSC(LAM(4),A0,A1,A2,A3,A4,A5)
!                         END IF
!         IF(LAM(5).EQ.0.0D0) THEN
!         GPREG(5)=1.0D0
!                         ELSE
! C
!       IF(FLNAME.EQ.'HOYA.BIN')
!      1  GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'HIKARI.BIN')
!      1  GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'CORNIN.BIN')
!      1  GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'CHANCE.BIN')
!      1  GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'RADHARD.BIN')
!      1  GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'OHARA-O.BIN')
!      1  GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
! C
!       IF(FLNAME.EQ.'SCHOTT.BIN')
!      1  GPREG(5)=PNSC(LAM(5),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'SCH2000.BIN')
!      1  GPREG(5)=PNSC(LAM(5),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'OHARA.BIN')
!      1  GPREG(5)=PNSC(LAM(5),A0,A1,A2,A3,A4,A5)
!                         END IF
!         IF(LAM(6).EQ.0.0D0) THEN
!         GPREG(6)=1.0D0
!                         ELSE
! C
!       IF(FLNAME.EQ.'HOYA.BIN')
!      1  GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'HIKARI.BIN')
!      1  GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'CORNIN.BIN')
!      1  GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'CHANCE.BIN')
!      1  GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'RADHARD.BIN')
!      1  GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'OHARA-O.BIN')
!      1  GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
! C
!       IF(FLNAME.EQ.'SCHOTT.BIN')
!      1  GPREG(6)=PNSC(LAM(6),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'SCH2000.BIN')
!      1  GPREG(6)=PNSC(LAM(6),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'OHARA.BIN')
!      1  GPREG(6)=PNSC(LAM(6),A0,A1,A2,A3,A4,A5)
!                         END IF
!         IF(LAM(7).EQ.0.0D0) THEN
!         GPREG(7)=1.0D0
!                         ELSE
! C
!       IF(FLNAME.EQ.'HOYA.BIN')
!      1  GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'HIKARI.BIN')
!      1  GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'CORNIN.BIN')
!      1  GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'CHANCE.BIN')
!      1  GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'RADHARD.BIN')
!      1  GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'OHARA-O.BIN')
!      1  GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
! C
!       IF(FLNAME.EQ.'SCHOTT.BIN')
!      1  GPREG(7)=PNSC(LAM(7),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'SCH2000.BIN')
!      1  GPREG(7)=PNSC(LAM(7),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'OHARA.BIN')
!      1  GPREG(7)=PNSC(LAM(7),A0,A1,A2,A3,A4,A5)
!                         END IF
!         IF(LAM(8).EQ.0.0D0) THEN
!         GPREG(8)=1.0D0
!                         ELSE
! C
!       IF(FLNAME.EQ.'HOYA.BIN')
!      1  GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'HIKARI.BIN')
!      1  GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'CORNIN.BIN')
!      1  GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'CHANCE.BIN')
!      1  GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'RADHARD.BIN')
!      1  GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'OHARA-O.BIN')
!      1  GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
! C
!       IF(FLNAME.EQ.'SCHOTT.BIN')
!      1  GPREG(8)=PNSC(LAM(8),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'SCH2000.BIN')
!      1  GPREG(8)=PNSC(LAM(8),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'OHARA.BIN')
!      1  GPREG(8)=PNSC(LAM(8),A0,A1,A2,A3,A4,A5)
!                         END IF
!         IF(LAM(9).EQ.0.0D0) THEN
!         GPREG(9)=1.0D0
!                         ELSE
! C
!       IF(FLNAME.EQ.'HOYA.BIN')
!      1  GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'HIKARI.BIN')
!      1  GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'CORNIN.BIN')
!      1  GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'CHANCE.BIN')
!      1  GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'RADHARD.BIN')
!      1  GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'OHARA-O.BIN')
!      1  GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
! C
!       IF(FLNAME.EQ.'SCHOTT.BIN')
!      1  GPREG(9)=PNSC(LAM(9),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'SCH2000.BIN')
!      1  GPREG(9)=PNSC(LAM(9),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'OHARA.BIN')
!      1  GPREG(9)=PNSC(LAM(9),A0,A1,A2,A3,A4,A5)
!                         END IF
!         IF(LAM(10).EQ.0.0D0) THEN
!         GPREG(10)=1.0D0
!                         ELSE
! C
!       IF(FLNAME.EQ.'HOYA.BIN')
!      1  GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'HIKARI.BIN')
!      1  GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'CORNIN.BIN')
!      1  GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'CHANCE.BIN')
!      1  GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'RADHARD.BIN')
!      1  GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'OHARA-O.BIN')
!      1  GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
! C
!       IF(FLNAME.EQ.'SCHOTT.BIN')
!      1  GPREG(10)=PNSC(LAM(10),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'SCH2000.BIN')
!      1  GPREG(10)=PNSC(LAM(10),A0,A1,A2,A3,A4,A5)
!       IF(FLNAME.EQ.'OHARA.BIN')
!      1  GPREG(10)=PNSC(LAM(10),A0,A1,A2,A3,A4,A5)
!                         END IF
!                         RETURN
!                         ELSE
! C       KEEP SEARCHING THE CATALOG
!                         END IF
!                         END DO
! C       CAN'T FIND THE GLASS, DO WARNING AND RETURN TO CMD LEVEL
!         IF(LASCNT.EQ.8) THEN
!         OUTLYNE=
!      1  'GLASS NOT FOUND IN ANY STANDARD OPTICAL GLASS CATALOG'
!       CALL SHOWIT(1)
!         OUTLYNE='REFRACTIVE INDICES SET TO 1.0'
!       CALL SHOWIT(1)
!                 GPREG(1:5)=1.0D0
!                         END IF
! C
!                         ELSE
! C       CATALOG REQUESTED NOT YET INSTALLED.
!         OUTLYNE=
!      1  WC//' GLASS CATALOG NOT YET INSTALLED'
!       CALL SHOWIT(1)
!         OUTLYNE='REFRACTIVE INDICES SET TO 1.0'
!       CALL SHOWIT(1)
!                 GPREG(1:5)=1.0D0
!                         END IF
!                         END DO ! DO COUNT
      ELSE
!     FLTP NOT MULTI
      END IF
      IF(FLTP.EQ.'MULTIOHARA  '.AND.FLNAME.NE.'USER.DAT  ') THEN
!     SEARCH 5 EXISTING CATALOGS
         LASCNT=0
         DO COUNT=1,2
            LASCNT=COUNT
            IF(COUNT.EQ.1) FLNAME='OHARA.BIN'
            IF(COUNT.EQ.2) FLNAME='OHARA-O.BIN'
            WC='OHARA   '
            EXIS36=.FALSE.
            INQUIRE(FILE=trim(LIBGLA)//FLNAME,EXIST=EXIS36)
            IF(EXIS36) THEN
!               PROCEED
               PRINT *, "About to open ", trim(LIBGLA)//FLNAME
               call OPENGLASSFILE(trim(LIBGLA)//FLNAME, uG, TOTAL)
               READ(UNIT=36,REC=1) TOTAL
               DO J=2,TOTAL+1
                  READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                  IF(WS.EQ.NAME.OR.WS.EQ.NUMBER) THEN
!     FOUND THE GLASS
                     IF(COUNT.GE.1.AND.COUNT.LE.2) CALL SHOWIT(1)
!     NOW RECHECK THE WAVELENGTH RANGE
                     CALL CHECKGLASSCATWAVELENGTHBOUNDS(boolResult, FLNAME)
                     IF (boolResult) THEN

                     ELSE
                        GPREG(1)=1.0D0
                        GPREG(2)=1.0D0
                        GPREG(3)=1.0D0
                        GPREG(4)=1.0D0
                        GPREG(5)=1.0D0
!         PROCEED
                     END IF
!       CALCULATE INDEX THEN RETURN
                     IF(LAM(1).EQ.0.0D0) THEN
                        GPREG(1)=1.0D0
                     ELSE
!
                        IF(FLNAME.EQ.'HOYA.BIN')GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'HIKARI.BIN')GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'CORNIN.BIN')GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'CHANCE.BIN')GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'RADHARD.BIN')GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'OHARA-O.BIN')GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
!
                        IF(FLNAME.EQ.'SCHOTT.BIN')GPREG(1)=PNSC(LAM(1),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'SCH2000.BIN')GPREG(1)=PNSC(LAM(1),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'OHARA.BIN')GPREG(1)=PNSC(LAM(1),A0,A1,A2,A3,A4,A5)
                     END IF
                     IF(LAM(2).EQ.0.0D0) THEN
                        GPREG(2)=1.0D0
                     ELSE
!
                        IF(FLNAME.EQ.'HOYA.BIN')GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'HIKARI.BIN')GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'CORNIN.BIN')GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'CHANCE.BIN')GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'RADHARD.BIN')GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'OHARA-O.BIN')GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
!
                        IF(FLNAME.EQ.'SCHOTT.BIN')GPREG(2)=PNSC(LAM(2),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'SCH2000.BIN')GPREG(2)=PNSC(LAM(2),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'OHARA.BIN')GPREG(2)=PNSC(LAM(2),A0,A1,A2,A3,A4,A5)
                     END IF
                     IF(LAM(3).EQ.0.0D0) THEN
                        GPREG(3)=1.0D0
                     ELSE
!
                        IF(FLNAME.EQ.'HOYA.BIN')GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'HIKARI.BIN')GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'CORNIN.BIN')GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'CHANCE.BIN')GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'RADHARD.BIN')GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'OHARA-O.BIN')GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
!
                        IF(FLNAME.EQ.'SCHOTT.BIN')GPREG(3)=PNSC(LAM(3),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'SCH2000.BIN')GPREG(3)=PNSC(LAM(3),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'OHARA.BIN')GPREG(3)=PNSC(LAM(3),A0,A1,A2,A3,A4,A5)
                     END IF
                     IF(LAM(4).EQ.0.0D0) THEN
                        GPREG(4)=1.0D0
                     ELSE
!
                        IF(FLNAME.EQ.'HOYA.BIN')GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'HIKARI.BIN')GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'CORNIN.BIN')GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'CHANCE.BIN')GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'RADHARD.BIN')GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'OHARA-O.BIN')GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
!
                        IF(FLNAME.EQ.'SCHOTT.BIN')GPREG(4)=PNSC(LAM(4),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'SCH2000.BIN')GPREG(4)=PNSC(LAM(4),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'OHARA.BIN')GPREG(4)=PNSC(LAM(4),A0,A1,A2,A3,A4,A5)
                     END IF
                     IF(LAM(5).EQ.0.0D0) THEN
                        GPREG(5)=1.0D0
                     ELSE
!
                        IF(FLNAME.EQ.'HOYA.BIN')GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'HIKARI.BIN')GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'CORNIN.BIN')GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'CHANCE.BIN')GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'RADHARD.BIN')GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'OHARA-O.BIN')GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
!
                        IF(FLNAME.EQ.'SCHOTT.BIN')GPREG(5)=PNSC(LAM(5),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'SCH2000.BIN')GPREG(5)=PNSC(LAM(5),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'OHARA.BIN')GPREG(5)=PNSC(LAM(5),A0,A1,A2,A3,A4,A5)
                     END IF
                     IF(LAM(6).EQ.0.0D0) THEN
                        GPREG(6)=1.0D0
                     ELSE
!
                        IF(FLNAME.EQ.'HOYA.BIN')GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'HIKARI.BIN')GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'CORNIN.BIN')GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'CHANCE.BIN')GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'RADHARD.BIN')GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'OHARA-O.BIN')GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
!
                        IF(FLNAME.EQ.'SCHOTT.BIN')GPREG(6)=PNSC(LAM(6),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'SCH2000.BIN')GPREG(6)=PNSC(LAM(6),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'OHARA.BIN')GPREG(6)=PNSC(LAM(6),A0,A1,A2,A3,A4,A5)
                     END IF
                     IF(LAM(7).EQ.0.0D0) THEN
                        GPREG(7)=1.0D0
                     ELSE
!
                        IF(FLNAME.EQ.'HOYA.BIN')GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'HIKARI.BIN')GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'CORNIN.BIN')GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'CHANCE.BIN')GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'RADHARD.BIN')GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'OHARA-O.BIN')GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
!
                        IF(FLNAME.EQ.'SCHOTT.BIN')GPREG(7)=PNSC(LAM(7),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'SCH2000.BIN')GPREG(7)=PNSC(LAM(7),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'OHARA.BIN')GPREG(7)=PNSC(LAM(7),A0,A1,A2,A3,A4,A5)
                     END IF
                     IF(LAM(8).EQ.0.0D0) THEN
                        GPREG(8)=1.0D0
                     ELSE
!
                        IF(FLNAME.EQ.'HOYA.BIN')GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'HIKARI.BIN')GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'CORNIN.BIN')GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'CHANCE.BIN')GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'RADHARD.BIN')GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'OHARA-O.BIN')GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
!
                        IF(FLNAME.EQ.'SCHOTT.BIN')GPREG(8)=PNSC(LAM(8),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'SCH2000.BIN')GPREG(8)=PNSC(LAM(8),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'OHARA.BIN')GPREG(8)=PNSC(LAM(8),A0,A1,A2,A3,A4,A5)
                     END IF
                     IF(LAM(9).EQ.0.0D0) THEN
                        GPREG(9)=1.0D0
                     ELSE
!
                        IF(FLNAME.EQ.'HOYA.BIN')GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'HIKARI.BIN')GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'CORNIN.BIN')GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'CHANCE.BIN')GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'RADHARD.BIN')GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'OHARA-O.BIN')GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
!
                        IF(FLNAME.EQ.'SCHOTT.BIN')GPREG(9)=PNSC(LAM(9),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'SCH2000.BIN')GPREG(9)=PNSC(LAM(9),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'OHARA.BIN')GPREG(9)=PNSC(LAM(9),A0,A1,A2,A3,A4,A5)
                     END IF
                     IF(LAM(10).EQ.0.0D0) THEN
                        GPREG(10)=1.0D0
                     ELSE
!
                        IF(FLNAME.EQ.'HOYA.BIN')GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'HIKARI.BIN')GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'CORNIN.BIN')GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'CHANCE.BIN')GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'RADHARD.BIN')GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'OHARA-O.BIN')GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
!
                        IF(FLNAME.EQ.'SCHOTT.BIN')GPREG(10)=PNSC(LAM(10),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'SCH2000.BIN')GPREG(10)=PNSC(LAM(10),A0,A1,A2,A3,A4,A5)
                        IF(FLNAME.EQ.'OHARA.BIN')GPREG(10)=PNSC(LAM(10),A0,A1,A2,A3,A4,A5)
                     END IF
                     RETURN
                  ELSE
!       KEEP SEARCHING THE CATALOG
                  END IF
               END DO
!       CAN'T FIND THE GLASS, DO WARNING AND RETURN TO CMD LEVEL
               IF(LASCNT.EQ.2) THEN
                  OUTLYNE='GLASS NOT FOUND IN ANY STANDARD OPTICAL GLASS CATALOG'
                  CALL SHOWIT(1)
                  OUTLYNE='REFRACTIVE INDICES SET TO 1.0'
                  CALL SHOWIT(1)
                  GPREG(1:5)=1.0D0
               ELSE
!     LASCNT NOT 2
               END IF
!
            ELSE
!       CATALOG REQUESTED NOT YET INSTALLED.
               OUTLYNE=WC//' GLASS CATALOG NOT YET INSTALLED'
               CALL SHOWIT(1)
               OUTLYNE='REFRACTIVE INDICES SET TO 1.0'
               CALL SHOWIT(1)
               GPREG(1:5)=1.0D0
            END IF
         END DO
      ELSE
!     FLTP NOT MULTI
      END IF
!
      IF(FLNAME.EQ.'USER.DAT  ') THEN
         EXIS36=.FALSE.
         INQUIRE(FILE=FLNAME,EXIST=EXIS36)
         IF(EXIS36) THEN
!               PROCEED
            OPEN(UNIT=36,ACCESS='SEQUENTIAL',FILE=FLNAME,RECL=(NRECL*33),STATUS='UNKNOWN')
            REWIND(UNIT=36)
15          READ(UNIT=36,FMT=*,END=9915) NAME,A0,A1,A2,A3,A4,A5
            IF(WS.EQ.NAME) THEN
!       CALCULATE INDEX THEN RETURN
               IF(LAM(1).EQ.0.0D0) THEN
                  GPREG(1)=1.0D0
               ELSE
                  GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
               END IF
               IF(LAM(2).EQ.0.0D0) THEN
                  GPREG(2)=1.0D0
               ELSE
                  GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
               END IF
               IF(LAM(3).EQ.0.0D0) THEN
                  GPREG(3)=1.0D0
               ELSE
                  GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
               END IF
               IF(LAM(4).EQ.0.0D0) THEN
                  GPREG(4)=1.0D0
               ELSE
                  GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
               END IF
               IF(LAM(5).EQ.0.0D0) THEN
                  GPREG(5)=1.0D0
               ELSE
                  GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
               END IF
               IF(LAM(6).EQ.0.0D0) THEN
                  GPREG(6)=1.0D0
               ELSE
                  GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
               END IF
               IF(LAM(7).EQ.0.0D0) THEN
                  GPREG(7)=1.0D0
               ELSE
                  GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
               END IF
               IF(LAM(8).EQ.0.0D0) THEN
                  GPREG(8)=1.0D0
               ELSE
                  GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
               END IF
               IF(LAM(9).EQ.0.0D0) THEN
                  GPREG(9)=1.0D0
               ELSE
                  GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
               END IF
               IF(LAM(10).EQ.0.0D0) THEN
                  GPREG(10)=1.0D0
               ELSE
                  GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
               END IF
               CALL CLOSE_FILE(36,1)
               RETURN
            ELSE
!       KEEP SEARCHING THE CATALOG
               GO TO 15
            END IF
9915        CONTINUE
            CALL CLOSE_FILE(36,1)
!       CAN'T FIND THE GLASS, DO WARNING AND RETURN TO CMD LEVEL
            OUTLYNE='GLASS NOT FOUND IN THE '//WC//' CATALOG'
            CALL SHOWIT(1)
            OUTLYNE='REFRACTIVE INDICES SET TO 1.0'
            CALL SHOWIT(1)
            GPREG(1:5)=1.0D0
!
         ELSE
!       CATALOG REQUESTED NOT YET INSTALLED.
            OUTLYNE=WC//' GLASS CATALOG NOT YET INSTALLED'
            CALL SHOWIT(1)
            OUTLYNE='REFRACTIVE INDICES SET TO 1.0'
            CALL SHOWIT(1)
            GPREG(1:5)=1.0D0
         END IF
      ELSE
!       FLNAME WAS NOT USER
      END IF
!     HERE IS WHERE OTHER CATALOG CALLS GO
!
!     DO MATL
      IF(FLTP.EQ.'MATL        ') THEN
!     SECOND ENTRY IDENTIFIES THE MATERIAL TO SPCGLS
!
         IF(WS.EQ.'GERMSC')  THEN
            CALL SPCGL(I,1)
            RETURN
         END IF
         IF(WS.EQ.'GERMPC')  THEN
            CALL SPCGL(I,2)
            RETURN
         END IF
         IF(WS.EQ.'SILICON') THEN
            CALL SPCGL(I,3)
            RETURN
         END IF
         IF(WS.EQ.'IRG100')  THEN
            CALL SPCGL(I,4)
            RETURN
         END IF
         IF(WS.EQ.'ZNSE')    THEN
            CALL SPCGL(I,5)
            RETURN
         END IF
         IF(WS.EQ.'IRTRAN4')    THEN
            CALL SPCGL(I,5)
            RETURN
         END IF
         IF(WS.EQ.'ZNS')     THEN
            CALL SPCGL(I,6)
            RETURN
         END IF
         IF(WS.EQ.'IRTRAN2')     THEN
            CALL SPCGL(I,6)
            RETURN
         END IF
         IF(WS.EQ.'CLRTRAN') THEN
            CALL SPCGL(I,7)
            RETURN
         END IF
         IF(WS.EQ.'SILICA')  THEN
            CALL SPCGL(I,8)
            RETURN
         END IF
         IF(WS.EQ.'SIO2  ')  THEN
            CALL SPCGL(I,8)
            RETURN
         END IF
         IF(WS.EQ.'SAPPHIRE'.OR.WS.EQ.'SAPHIR') THEN
            CALL SPCGL(I,9)
            RETURN
         END IF
         IF(WS.EQ.'DYNASIL') THEN
            CALL SPCGL(I,10)
            RETURN
         END IF
         IF(WS.EQ.'AMTIR1')  THEN
            CALL SPCGL(I,11)
            RETURN
         END IF
         IF(WS.EQ.'AMTIR3')  THEN
            CALL SPCGL(I,12)
            RETURN
         END IF
         IF(WS.EQ.'AS2S3')   THEN
            CALL SPCGL(I,13)
            RETURN
         END IF
         IF(WS.EQ.'GAAS')    THEN
            CALL SPCGL(I,14)
            RETURN
         END IF
         IF(WS.EQ.'CDTE')    THEN
            CALL SPCGL(I,15)
            RETURN
         END IF
         IF(WS.EQ.'IRTRAN6')    THEN
            CALL SPCGL(I,15)
            RETURN
         END IF
         IF(WS.EQ.'MGF2(O)'.OR.WS.EQ.'MGF2')   THEN
            CALL SPCGL(I,16)
            RETURN
         END IF
         IF(WS.EQ.'IRTRAN1')   THEN
            CALL SPCGL(I,16)
            RETURN
         END IF
         IF(WS.EQ.'MGF2(E)')   THEN
            CALL SPCGL(I,17)
            RETURN
         END IF
         IF(WS.EQ.'CAF2')    THEN
            CALL SPCGL(I,18)
            RETURN
         END IF
         IF(WS.EQ.'IRTRAN3')    THEN
            CALL SPCGL(I,18)
            RETURN
         END IF
         IF(WS.EQ.'MGO')     THEN
            CALL SPCGL(I,19)
            RETURN
         END IF
         IF(WS.EQ.'IRTRAN5')     THEN
            CALL SPCGL(I,19)
            RETURN
         END IF
         IF(WS.EQ.'BAF2')    THEN
            CALL SPCGL(I,20)
            RETURN
         END IF
         IF(WS.EQ.'KBR')     THEN
            CALL SPCGL(I,21)
            RETURN
         END IF
         IF(WS.EQ.'CSI')     THEN
            CALL SPCGL(I,22)
            RETURN
         END IF
         IF(WS.EQ.'CSBR')    THEN
            CALL SPCGL(I,23)
            RETURN
         END IF
         IF(WS.EQ.'KRS5')    THEN
            CALL SPCGL(I,24)
            RETURN
         END IF
         IF(WS.EQ.'LIF')     THEN
            CALL SPCGL(I,25)
            RETURN
         END IF
         IF(WS.EQ.'NACL')    THEN
            CALL SPCGL(I,26)
            RETURN
         END IF
         IF(WS.EQ.'SIO2O')   THEN
            CALL SPCGL(I,27)
            RETURN
         END IF
         IF(WS.EQ.'SIO2E')   THEN
            CALL SPCGL(I,28)
            RETURN
         END IF
         IF(WS.EQ.'VIR3')    THEN
            CALL SPCGL(I,29)
            RETURN
         END IF
         IF(WS.EQ.'9754')    THEN
            CALL SPCGL(I,30)
            RETURN
         END IF
         IF(WS.EQ.'ALON')    THEN
            CALL SPCGL(I,31)
            RETURN
         END IF
         IF(WS.EQ.'SPINEL')  THEN
            CALL SPCGL(I,32)
            RETURN
         END IF
         IF(WS.EQ.'CALAL')   THEN
            CALL SPCGL(I,33)
            RETURN
         END IF
         IF(WS.EQ.'B270')    THEN
            CALL SPCGL(I,34)
            RETURN
         END IF
         IF(WS.EQ.'IRG2')    THEN
            CALL SPCGL(I,35)
            RETURN
         END IF
         IF(WS.EQ.'IRG3')    THEN
            CALL SPCGL(I,36)
            RETURN
         END IF
         IF(WS.EQ.'IRGN6')   THEN
            CALL SPCGL(I,37)
            RETURN
         END IF
         IF(WS.EQ.'IRG7')    THEN
            CALL SPCGL(I,38)
            RETURN
         END IF
         IF(WS.EQ.'IRG9')    THEN
            CALL SPCGL(I,39)
            RETURN
         END IF
         IF(WS.EQ.'IRG11')   THEN
            CALL SPCGL(I,40)
            RETURN
         END IF
         IF(WS.EQ.'IRG15')   THEN
            CALL SPCGL(I,41)
            RETURN
         END IF
         IF(WS.EQ.'VAC')     THEN
            CALL SPCGL(I,42)
            RETURN
         END IF
         IF(WS.EQ.'WATER')     THEN
            CALL SPCGL(I,43)
            RETURN
         END IF
         IF(WS.EQ.'SUPRASIL')  THEN
            CALL SPCGL(I,44)
            RETURN
         END IF
         IF(WS.EQ.'HOMOSIL')  THEN
            CALL SPCGL(I,45)
            RETURN
         END IF
         IF(WS.EQ.'ZNS-MS')  THEN
            CALL SPCGL(I,46)
            RETURN
         END IF
         IF(WS.EQ.'CEF3')  THEN
            CALL SPCGL(I,47)
            RETURN
         END IF
         IF(WS.EQ.'LA2O3')  THEN
            CALL SPCGL(I,48)
            RETURN
         END IF
         IF(WS.EQ.'THF4')  THEN
            CALL SPCGL(I,49)
            RETURN
         END IF
         IF(WS.EQ.'ZRO2')  THEN
            CALL SPCGL(I,50)
            RETURN
         END IF
         IF(WS.EQ.'DIAMOND')  THEN
            CALL SPCGL(I,51)
            RETURN
         END IF
         IF(WS.EQ.'YAG')  THEN
            CALL SPCGL(I,52)
            RETURN
         END IF
         IF(WS.EQ.'ACRYLIC')  THEN
            CALL SPCGL(I,101)
            RETURN
         END IF
         IF(WS.EQ.'PLYSTY')   THEN
            CALL SPCGL(I,102)
            RETURN
         END IF
         IF(WS.EQ.'POLYCARB') THEN
            CALL SPCGL(I,103)
            RETURN
         END IF
         IF(WS.EQ.'SAN')      THEN
            CALL SPCGL(I,104)
            RETURN
         END IF
         CALL REPORT_ERROR_AND_FAIL('GLASS NOT FOUND IN THE '//GLANAM(I,1)//' CATALOG', 1)
         RETURN
      END IF
   END IF

!
!       GLASS RESOLUTIONS COMPLETED
   CALL CLOSE_FILE(36,1)
   RETURN
END
! SUB GLSRES.FOR
! JN 4/14/24.  Major surgery to better support new catalogs and improve readability
! When I started, this fnc was ~2000 lines
! After some pruning, got down to ~1050 lines
! To reduce further, need to move the MATL catalog to be a type of glass catalog.
! After removing MATL and USER support, down to ~ 300 lines.  Still too big, but
! more manageable.
SUBROUTINE GLSRES
   use GLOBALS
   use global_widgets, only: sysConfig, curr_lens_data
   use type_utils, only: real2str
   use glass_manager
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_last_surf
   use mod_lens_data_manager, only: ldm
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE GLSRES. THIS IS THE SUBROUTINE WHICH
!       HANDLES THE GLASS CATALOG SEARCH AND REFRACTIVE INDEX
!       CALCULATION. THE VALUE BADGLS IS SET TO TRUE AND
!       AN IMMEDIATE RETURN IS MADE IF A GLASS CAN NOT BE FOUND.
!       ERROR MESSAGES SENT FROM HERE IN THAT CASE.
!
   LOGICAL BADGLS,GOGO,EXIS36,LOWER,ISOK
   logical boolResult

!
   CHARACTER FLNAME*12,NAME*13,NUMBER*13,FLTP*12
!
   INTEGER I,J,TOTAL,COUNT,LASCNT,GNUMBER1,GNUMBER2
   INTEGER unit, uG, gdb_loc
!
   EXTERNAL GNUMBER1,GNUMBER2
!
   real(real64) LMAX,LMIN,LAMBDA,A0,A1,A2,A3,A4,A5,PN
!
   COMMON/GLSBAD/BADGLS
!
   CHARACTER*13 NAME1,NAME2
!
!
!       NOW RESOLVE THE CALCULATION OF REFRACTIVE INDICES VIA
!       GLASS CATALOG SEARCHES.
!

!
!       SET BADGLS
   BADGLS=.FALSE.
   GOGO=.TRUE.
   !WRITE(OUTLYNE, *) "Glass Name is ", GLANAM(0,1)
   !call logger%logText(OUTLYNE)
   !call LogTermFOR("In GLSRES")

   call sysConfig%updateParameters()
!
!
!       DETERMINE MINIMUM AND MAX SYSTEM WAVELENGTHS
   LMIN = minval(sysConfig%wavelengths(1:sysConfig%numWavelengths))
   LMAX = maxval(sysConfig%wavelengths(1:sysConfig%numWavelengths))

   !call LogTermFOR("LMIN is "//trim(real2str(LMIN)))
   !call LogTermFOR("LMAX is "//trim(real2str(LMAX)))

   DO 10 I=0,INT(sys_last_surf())
      IF(LNSTYP.NE.1.AND.GLANAM(I,1).NE.'MODEL') GO TO 10
!
!       IF THE SURFACE HAS A GLASS PIKUP, SKIP AND GO TO NEXT
!       SURFACE. (JUST JUMP TO 10 AND GO TO THE NEXT SURFACE.
!
      IF(PIKUP(1,I,20).NE.1.0D0) THEN

!       IS THE SURFACE MATERIAL A CATALOG MATERIAL?
         IF(gdb%isNameInCatalog(GLANAM(I,1), gdb_loc).OR.GLANAM(I,1).EQ.'GLAK'.OR.GLANAM(I,1).EQ.'GLCAT') THEN

            CALL CHECKGLSRESWAVELENGTHBOUNDS(boolResult,GLANAM(I,1),GLANAM(I,2),' ', LMIN, LMAX, I)

            if(.not.boolResult) then

               BADGLS=.TRUE.
               call ldm%clearGlassColorData(I)
               call ldm%initRefractiveIndices(I)
               GO TO 10
            end if
!       PROCEED, THE GLASS IS DEFINED WITHIN THE CURRENT
!       LENS SYSTEM WAVELENGTHS

!       NOW LOOK UP THE GLASS IN THE CATALOG, RETRIEVE THE
!       COEFFICIENTS A0 TO A5 AND CALCULATE AND SET THE SURFACE
!       REFRACTIVE INDICES.
!
            FLTP='            '
            !call LogTermFOR("GLANAM(I,1) is "//GLANAM(I,1))
            IF(gdb%isNameInCatalog(GLANAM(I,1), gdb_loc)) THEN
               FLNAME=gdb%catalogFileNames(gdb_loc)
            END IF
            IF(GLANAM(I,1).EQ.'GLCAT')    FLTP='MULTI       '
            IF(GLANAM(I,1).EQ.'GLAK')      FLTP='MULTI       '

            !call LogTermFOR("FLNAME is "//FLNAME)

!
            IF(FLTP.EQ.'            ') THEN
!     REGULAR GLASS CATALOG

               !call LogTermFOR("About to open glass file")
               IF (gdb%isGlassInCatalog(GLANAM(I,2), gdb_loc)) THEN
                  !call LogTermFOR("Found Glass and updated coeffs!")

                  DO J=1,5
                     !call LogTermFOR("Wavelength is "//
                     !1    real2str(sysConfig%wavelengths(J)))
                     ALENS(45+J,I) = gdb%calcIndexForCurrentGlass (sysConfig%wavelengths(J))
                     !call LogTermFOR("ALENS is "//real2str(ALENS(45+J,I)))
                  END DO
                  DO J=1,4
                     ALENS(70+J,I) = gdb%calcIndexForCurrentGlass (sysConfig%wavelengths(J+5))
                     !call LogTermFOR("ALENS is "//real2str(ALENS(70+J,I)))

                  END DO

                  J = 1
                  ! This is here because the way the fcn is built, without this
                  ! goto statement, it will continue to try other options to get the index
                  ! and evantually set eveyting to 1.  Once this is completely rebuild
                  ! where it only looks in the glass database for stuff it should not be needed
                  ! a nice select case block should work

                  GO TO 10

               ELSE
!       CATALOG REQUESTED NOT YET INSTALLED.
                  WRITE(OUTLYNE,*)'WARNING: FOR SURFACE ',I
                  CALL SHOWIT(1)
                  OUTLYNE=GLANAM(I,1)//' GLASS CATALOG NOT YET INSTALLED'
                  CALL SHOWIT(1)
                  OUTLYNE='REFRACTIVE INDICES FOR SURFACE SET TO 1.0'
                  CALL SHOWIT(1)
                  call ldm%clearGlassColorData(I)
                  call ldm%initRefractiveIndices(I)
               END IF
            ELSE
!     FLTP WAS NOT BLANK
            END IF
!
            IF(FLTP.EQ.'MULTI       ') THEN
               IF (gdb%isGlassInAnyCatalog(GLANAM(I,2), COUNT)) THEN
                  ! Update Glass Name
                  GLANAM(I,1) = gdb%catalogs(COUNT)
                  !call LogTermFOR("Found glass in catalog "//trim(GLANAM(I,1)))

                  CALL CHECKGLASSCATWAVELENGTHBOUNDS(boolResult, FLNAME)
                  IF (boolResult) THEN


                     DO J=1,5
                        !call LogTermFOR("Wavelength is "
                        ! 1    //real2str(sysConfig%wavelengths(J)))
                        ALENS(45+J,I) = gdb%calcIndexForCurrentGlass (sysConfig%wavelengths(J))
                        !call LogTermFOR("ALENS is "//real2str(ALENS(45+J,I)))
                     END DO
                     DO J=1,5
                        ALENS(70+J,I) = gdb%calcIndexForCurrentGlass (sysConfig%wavelengths(J+5))
                        !call LogTermFOR("ALENS is "//real2str(ALENS(70+J,I)))

                     END DO
                     ! SEE NOTE ABOVE FOR WHY THIS IS TEMPORARILY NECESSARy
                     !call LogTermFOR("About to go to 10?")
                     GO TO 10

                  END IF
               END IF
!     NOTHING FOUND IN THE ABOVE CATALOGS
               ! Removed support for USER.DAT.  Model glass support
               ! serves this purpose IMO,
               ! JRN

!     MATL LIMITS
               CALL CHECKGLSRESWAVELENGTHBOUNDS(boolResult,GLANAM(I,1),GLANAM(I,2),' ', LMIN, LMAX, I)

               if(.not.boolResult) then

                  BADGLS=.TRUE.
                  call ldm%clearGlassColorData(I)
                  call ldm%initRefractiveIndices(I)
                  GO TO 10
               end if

               BADGLS=.TRUE.
               WRITE(OUTLYNE, *) "Glass Name is ", GLANAM(I,1)
               call logger%logText(OUTLYNE)
               WRITE(OUTLYNE,*)'WARNING: FOR SURFACE ',I
               CALL SHOWIT(1)
               OUTLYNE='GLASS NOT FOUND IN ANY STANDARD OPTICAL GLASS CATALOG'
               CALL SHOWIT(1)
               OUTLYNE='REFRACTIVE INDICES FOR SURFACE SET TO 1.0'
               CALL SHOWIT(1)
               call ldm%clearGlassColorData(I)
               call ldm%initRefractiveIndices(I)
            ELSE
!     FLTP NOT MULTI
            END IF


222         CONTINUE
         ELSE
!       NOT A CATALOG MATERIAL, GO TO NEXT SURFACE
!       THIS IS WHERE 'GLASS', 'AIR' AND 'REFL' AND 'MODEL'
!       AND 'REFLTIR', 'REFLTIRO'
!       'IDEAL' AND 'PERFECT' ARE TAKEN CARE OF.
            IF(GLANAM(I,1).EQ.'MODEL') CALL FICTRES(I)
         END IF
      ELSE
!       SURFACE HAD GLASS PIKUP ON IT
      END IF
10 CONTINUE
!

!       GLASS RESOLUTIONS COMPLETED
   CALL CLOSE_FILE(36,1)
   RETURN
END

! SUB FICTRES.FOR
SUBROUTINE FICTRES(I)
!
   use DATLEN
   use mod_surface
   use mod_system, only: sys_wavelength, sys_wl_weight
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   INTEGER I,J
!
   real(real64) CCEE,CBAR,XND,XNC,XNF,XNE,XV,XD,NE,DDDISS
!
!
   real(real64) PNSC,LAMBDA,A0,A1,A2,A3,A4,A5,DISP ,A01,A11,A21,A31,A41,A51,A02,A12,A22,A32,A42,A52,XDISP ,XVD,ND1,NF1,NC1,ND2,NF2,NC2,DISP1,DISP2,C1,C2,NX1,NX2 ,P1,P2,V1,V2,SLOPE,NEWP
!
   PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)=DSQRT(((A0*(LAMBDA**2))/((LAMBDA**2)-A3))+((A1*(LAMBDA**2))/((LAMBDA**2)-A4))+((A2*(LAMBDA**2))/((LAMBDA**2)-A5))+1.0D0)
!
!     FORMULA FOR GETTING THE NF-NC FROM Nd AND Vd
   DDDISS(XND,XVD)=(XND-1.0D0)/XVD
!
!      FOR K7
   A01=1.12735550D+0
   A11=1.24412303D-1
   A21=8.27100531D-1
   A31=7.20341707D-3
   A41=2.69835916D-3
   A51=1.00384588D+2

!      FOR F2
   A02=1.34533359D+0
   A12=2.09073176D-1
   A22=9.37357162D-1
   A32=9.97743871D-3
   A42=4.70450767D-2
   A52=1.11886764D+2
!
!                          PROCEEDURE FOLLOWS
!
!     CALCULATE THE REFRACTIVE INDEX VALUES FOR THE BASE GLASSES AT THE
!     STANDARD SCHOTT WAVELENGTHS
!
!     LOAD UP THE SURFACE INPUT VALUES OF ND, VD AND DISP
   XND=surf_fict_n(I)
   XVD=surf_fict_v(I)
   XDISP=DDDISS(XND,XVD)
!     CALCULATE THE ND,NC,NF AND DISP VALUES FOR THE STANDARD GLASSES
   A0=A01
   A1=A11
   A2=A21
   A3=A31
   A4=A41
   A5=A51
   LAMBDA=0.5875618D0
   ND1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
   LAMBDA=0.48613D0
   NF1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
   LAMBDA=0.65627D0
   NC1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
   DISP1=NF1-NC1
   A0=A02
   A1=A12
   A2=A22
   A3=A32
   A4=A42
   A5=A52
   LAMBDA=0.5875618D0
   ND2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
   LAMBDA=0.48613D0
   NF2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
   LAMBDA=0.65627D0
   NC2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
   DISP2=NF2-NC2
!     FOR EACH WAVELENGTH WHICH IS NON-ZERO WE CALCULATE THE REFRACIVE
!     INDEX
!
   DO J=1,10
!                               J=1
      IF(J.EQ.1) THEN
         IF(sys_wavelength(1).NE.0.0D0.AND.sys_wl_weight(1).NE.0.0D0) THEN
            A0=A01
            A1=A11
            A2=A21
            A3=A31
            A4=A41
            A5=A51
            LAMBDA=sys_wavelength(1)
            NX1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
            A0=A02
            A1=A12
            A2=A22
            A3=A32
            A4=A42
            A5=A52
            LAMBDA=sys_wavelength(1)
            NX2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
            P1=(NX1-ND1)/(NF1-NC1)
            P2=(NX2-ND2)/(NF2-NC2)
            V1=(ND1-1.0D0)/(NF1-NC1)
            V2=(ND2-1.0D0)/(NF2-NC2)
            SLOPE=(P1-P2)/(V1-V2)
            NEWP=SLOPE*(XVD)-(SLOPE*V2)+P2+surf_fict_w(I)
            call set_surf_refractive_index(I, 1, (NEWP*XDISP)+XND)
!     CALCULATE MODEL GLASS INDEX AT THIS WAVELENGTH
         ELSE
            call set_surf_refractive_index(I, 1, 1.0D0)
         END IF
      END IF
!                               J=2
      IF(J.EQ.2) THEN
         IF(sys_wavelength(2).NE.0.0D0.AND.sys_wl_weight(2).NE.0.0D0) THEN
!     CALCULATE CBAR
            A0=A01
            A1=A11
            A2=A21
            A3=A31
            A4=A41
            A5=A51
            LAMBDA=sys_wavelength(2)
            NX1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
            A0=A02
            A1=A12
            A2=A22
            A3=A32
            A4=A42
            A5=A52
            LAMBDA=sys_wavelength(2)
            NX2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
            P1=(NX1-ND1)/(NF1-NC1)
            P2=(NX2-ND2)/(NF2-NC2)
            V1=(ND1-1.0D0)/(NF1-NC1)
            V2=(ND2-1.0D0)/(NF2-NC2)
            SLOPE=(P1-P2)/(V1-V2)
            NEWP=SLOPE*(XVD)-(SLOPE*V2)+P2+surf_fict_w(I)
            call set_surf_refractive_index(I, 2, (NEWP*XDISP)+XND)
         ELSE
            call set_surf_refractive_index(I, 2, 1.0D0)
         END IF
      END IF
!                               J=3
      IF(J.EQ.3) THEN
         IF(sys_wavelength(3).NE.0.0D0.AND.sys_wl_weight(3).NE.0.0D0) THEN
!     CALCULATE CBAR
            A0=A01
            A1=A11
            A2=A21
            A3=A31
            A4=A41
            A5=A51
            LAMBDA=sys_wavelength(3)
            NX1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
            A0=A02
            A1=A12
            A2=A22
            A3=A32
            A4=A42
            A5=A52
            LAMBDA=sys_wavelength(3)
            NX2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
            P1=(NX1-ND1)/(NF1-NC1)
            P2=(NX2-ND2)/(NF2-NC2)
            V1=(ND1-1.0D0)/(NF1-NC1)
            V2=(ND2-1.0D0)/(NF2-NC2)
            SLOPE=(P1-P2)/(V1-V2)
            NEWP=SLOPE*(XVD)-(SLOPE*V2)+P2+surf_fict_w(I)
            call set_surf_refractive_index(I, 3, (NEWP*XDISP)+XND)
         ELSE
            call set_surf_refractive_index(I, 3, 1.0D0)
         END IF
      END IF
!                               J=4
      IF(J.EQ.4) THEN
         IF(sys_wavelength(4).NE.0.0D0.AND.sys_wl_weight(4).NE.0.0D0) THEN
!     CALCULATE CBAR
            A0=A01
            A1=A11
            A2=A21
            A3=A31
            A4=A41
            A5=A51
            LAMBDA=sys_wavelength(4)
            NX1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
            A0=A02
            A1=A12
            A2=A22
            A3=A32
            A4=A42
            A5=A52
            LAMBDA=sys_wavelength(4)
            NX2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
            P1=(NX1-ND1)/(NF1-NC1)
            P2=(NX2-ND2)/(NF2-NC2)
            V1=(ND1-1.0D0)/(NF1-NC1)
            V2=(ND2-1.0D0)/(NF2-NC2)
            SLOPE=(P1-P2)/(V1-V2)
            NEWP=SLOPE*(XVD)-(SLOPE*V2)+P2+surf_fict_w(I)
            call set_surf_refractive_index(I, 4, (NEWP*XDISP)+XND)
         ELSE
            call set_surf_refractive_index(I, 4, 1.0D0)
         END IF
      END IF
!                               J=5
      IF(J.EQ.5) THEN
         IF(sys_wavelength(5).NE.0.0D0.AND.sys_wl_weight(5).NE.0.0D0) THEN
!     CALCULATE CBAR
            A0=A01
            A1=A11
            A2=A21
            A3=A31
            A4=A41
            A5=A51
            LAMBDA=sys_wavelength(5)
            NX1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
            A0=A02
            A1=A12
            A2=A22
            A3=A32
            A4=A42
            A5=A52
            LAMBDA=sys_wavelength(5)
            NX2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
            P1=(NX1-ND1)/(NF1-NC1)
            P2=(NX2-ND2)/(NF2-NC2)
            V1=(ND1-1.0D0)/(NF1-NC1)
            V2=(ND2-1.0D0)/(NF2-NC2)
            SLOPE=(P1-P2)/(V1-V2)
            NEWP=SLOPE*(XVD)-(SLOPE*V2)+P2+surf_fict_w(I)
            call set_surf_refractive_index(I, 5, (NEWP*XDISP)+XND)
         ELSE
            call set_surf_refractive_index(I, 5, 1.0D0)
         END IF
      END IF
!                               J=6
      IF(J.EQ.6) THEN
         IF(sys_wavelength(6).NE.0.0D0.AND.sys_wl_weight(6).NE.0.0D0) THEN
!     CALCULATE CBAR
            A0=A01
            A1=A11
            A2=A21
            A3=A31
            A4=A41
            A5=A51
            LAMBDA=sys_wavelength(6)
            NX1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
            A0=A02
            A1=A12
            A2=A22
            A3=A32
            A4=A42
            A5=A52
            LAMBDA=sys_wavelength(6)
            NX2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
            P1=(NX1-ND1)/(NF1-NC1)
            P2=(NX2-ND2)/(NF2-NC2)
            V1=(ND1-1.0D0)/(NF1-NC1)
            V2=(ND2-1.0D0)/(NF2-NC2)
            SLOPE=(P1-P2)/(V1-V2)
            NEWP=SLOPE*(XVD)-(SLOPE*V2)+P2+surf_fict_w(I)
            call set_surf_refractive_index(I, 6, (NEWP*XDISP)+XND)
         ELSE
            call set_surf_refractive_index(I, 6, 1.0D0)
         END IF
      END IF
!                               J=7
      IF(J.EQ.7) THEN
         IF(sys_wavelength(7).NE.0.0D0.AND.sys_wl_weight(7).NE.0.0D0) THEN
!     CALCULATE CBAR
            A0=A01
            A1=A11
            A2=A21
            A3=A31
            A4=A41
            A5=A51
            LAMBDA=sys_wavelength(7)
            NX1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
            A0=A02
            A1=A12
            A2=A22
            A3=A32
            A4=A42
            A5=A52
            LAMBDA=sys_wavelength(7)
            NX2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
            P1=(NX1-ND1)/(NF1-NC1)
            P2=(NX2-ND2)/(NF2-NC2)
            V1=(ND1-1.0D0)/(NF1-NC1)
            V2=(ND2-1.0D0)/(NF2-NC2)
            SLOPE=(P1-P2)/(V1-V2)
            NEWP=SLOPE*(XVD)-(SLOPE*V2)+P2+surf_fict_w(I)
            call set_surf_refractive_index(I, 7, (NEWP*XDISP)+XND)
         ELSE
            call set_surf_refractive_index(I, 7, 1.0D0)
         END IF
      END IF
!                               J=8
      IF(J.EQ.8) THEN
         IF(sys_wavelength(8).NE.0.0D0.AND.sys_wl_weight(8).NE.0.0D0) THEN
!     CALCULATE CBAR
            A0=A01
            A1=A11
            A2=A21
            A3=A31
            A4=A41
            A5=A51
            LAMBDA=sys_wavelength(8)
            NX1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
            A0=A02
            A1=A12
            A2=A22
            A3=A32
            A4=A42
            A5=A52
            LAMBDA=sys_wavelength(8)
            NX2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
            P1=(NX1-ND1)/(NF1-NC1)
            P2=(NX2-ND2)/(NF2-NC2)
            V1=(ND1-1.0D0)/(NF1-NC1)
            V2=(ND2-1.0D0)/(NF2-NC2)
            SLOPE=(P1-P2)/(V1-V2)
            NEWP=SLOPE*(XVD)-(SLOPE*V2)+P2+surf_fict_w(I)
            call set_surf_refractive_index(I, 8, (NEWP*XDISP)+XND)
         ELSE
            call set_surf_refractive_index(I, 8, 1.0D0)
         END IF
      END IF
!                               J=9
      IF(J.EQ.9) THEN
         IF(sys_wavelength(9).NE.0.0D0.AND.sys_wl_weight(9).NE.0.0D0) THEN
!     CALCULATE CBAR
            A0=A01
            A1=A11
            A2=A21
            A3=A31
            A4=A41
            A5=A51
            LAMBDA=sys_wavelength(9)
            NX1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
            A0=A02
            A1=A12
            A2=A22
            A3=A32
            A4=A42
            A5=A52
            LAMBDA=sys_wavelength(9)
            NX2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
            P1=(NX1-ND1)/(NF1-NC1)
            P2=(NX2-ND2)/(NF2-NC2)
            V1=(ND1-1.0D0)/(NF1-NC1)
            V2=(ND2-1.0D0)/(NF2-NC2)
            SLOPE=(P1-P2)/(V1-V2)
            NEWP=SLOPE*(XVD)-(SLOPE*V2)+P2+surf_fict_w(I)
            call set_surf_refractive_index(I, 9, (NEWP*XDISP)+XND)
         ELSE
            call set_surf_refractive_index(I, 9, 1.0D0)
         END IF
      END IF
!                               J=10
      IF(J.EQ.10) THEN
         IF(sys_wavelength(10).NE.0.0D0.AND.sys_wl_weight(10).NE.0.0D0) THEN
!     CALCULATE CBAR
            A0=A01
            A1=A11
            A2=A21
            A3=A31
            A4=A41
            A5=A51
            LAMBDA=sys_wavelength(10)
            NX1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
            A0=A02
            A1=A12
            A2=A22
            A3=A32
            A4=A42
            A5=A52
            LAMBDA=sys_wavelength(10)
            NX2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
            P1=(NX1-ND1)/(NF1-NC1)
            P2=(NX2-ND2)/(NF2-NC2)
            V1=(ND1-1.0D0)/(NF1-NC1)
            V2=(ND2-1.0D0)/(NF2-NC2)
            SLOPE=(P1-P2)/(V1-V2)
            NEWP=SLOPE*(XVD)-(SLOPE*V2)+P2+surf_fict_w(I)
            call set_surf_refractive_index(I, 10, (NEWP*XDISP)+XND)
         ELSE
            call set_surf_refractive_index(I, 10, 1.0D0)
         END IF
      END IF
   END DO
!
   RETURN
END
FUNCTION GNUMBER1(NUMBER)
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   INTEGER GNUMBER1,I
   CHARACTER NUMBER*13,ANUMBER*3,B*80
   ANUMBER=NUMBER(1:3)
   WRITE(B,201) ANUMBER
   READ(B,101) GNUMBER1
101 FORMAT(I3)
201 FORMAT(A3)
   RETURN
END
FUNCTION GNUMBER2(NUMBER)
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   INTEGER GNUMBER2,I
   CHARACTER NUMBER*13,ANUMBER*3,B*80
   ANUMBER=NUMBER(4:6)
   WRITE(B,201) ANUMBER
   READ(B,101) GNUMBER2
101 FORMAT(I3)
201 FORMAT(A3)
   RETURN
END
! SUB FICT.FOR
SUBROUTINE FICT(I,INDEX,ND,ABBE,DPART)
!       USED TO CONVERT CAT GLASS INTO MODEL GLASS
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_last_surf, sys_wavelength
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   INTEGER I,J
!
   real(real64) CCEE,CBAR,XND,XNC,XNF,XNE,XV,XD,NE,DDDISS
!
   real(real64) ND,ABBE,DPART,INDEX
!
!
   real(real64) PNSC,LAMBDA,A0,A1,A2,A3,A4,A5,DISP ,A01,A11,A21,A31,A41,A51,A02,A12,A22,A32,A42,A52,XDISP ,XVD,ND1,NF1,NC1,ND2,NF2,NC2,DISP1,DISP2,C1,C2,NX1,NX2 ,P1,P2,V1,V2,SLOPE,NEWP
!
   PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)=DSQRT(((A0*(LAMBDA**2))/((LAMBDA**2)-A3))+((A1*(LAMBDA**2))/((LAMBDA**2)-A4))+((A2*(LAMBDA**2))/((LAMBDA**2)-A5))+1.0D0)
!
!     FORMULA FOR GETTING THE NF-NC FROM Nd AND Vd
   DDDISS(XND,XVD)=(XND-1.0D0)/XVD
!
!      FOR K7
   A01=1.12735550D+0
   A11=1.24412303D-1
   A21=8.27100531D-1
   A31=7.20341707D-3
   A41=2.69835916D-3
   A51=1.00384588D+2

!      FOR F2
   A02=1.34533359D+0
   A12=2.09073176D-1
   A22=9.37357162D-1
   A32=9.97743871D-3
   A42=4.70450767D-2
   A52=1.11886764D+2
!
!                          PROCEEDURE FOLLOWS
!
!     CALCULATE THE REFRACTIVE INDEX VALUES FOR THE BASE GLASSES AT THE
!     STANDARD SCHOTT WAVELENGTHS
!
!     LOAD UP THE SURFACE INPUT VALUES OF ND, VD AND DISP
   XND=ND
   XVD=ABBE
   XDISP=DDDISS(XND,XVD)
!     CALCULATE THE ND,NC,NF AND DISP VALUES FOR THE STANDARD GLASSES
   A0=A01
   A1=A11
   A2=A21
   A3=A31
   A4=A41
   A5=A51
   LAMBDA=0.5875618D0
   ND1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
   LAMBDA=0.48613D0
   NF1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
   LAMBDA=0.65627D0
   NC1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
   DISP1=NF1-NC1
   A0=A02
   A1=A12
   A2=A22
   A3=A32
   A4=A42
   A5=A52
   LAMBDA=0.5875618D0
   ND2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
   LAMBDA=0.48613D0
   NF2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
   LAMBDA=0.65627D0
   NC2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
   DISP2=NF2-NC2
!     FOR EACH WAVELENGTH WHICH IS NON-ZERO WE CALCULATE THE REFRACIVE
!     INDEX
!
   A0=A01
   A1=A11
   A2=A21
   A3=A31
   A4=A41
   A5=A51
   LAMBDA=0.5875618D0
   NX1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
   A0=A02
   A1=A12
   A2=A22
   A3=A32
   A4=A42
   A5=A52
   LAMBDA=sys_wavelength(1)
   NX2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
   P1=(NX1-ND1)/(NF1-NC1)
   P2=(NX2-ND2)/(NF2-NC2)
   V1=(ND1-1.0D0)/(NF1-NC1)
   V2=(ND2-1.0D0)/(NF2-NC2)
   SLOPE=(P1-P2)/(V1-V2)
   NEWP=SLOPE*(XVD)-(SLOPE*V2)+P2+DPART
   INDEX=(NEWP*XDISP)+XND
   RETURN
END



SUBROUTINE OPENGLASSFILE(filePath, fileID, TOTAL)
   use iso_fortran_env, only: real64
   character(len=*) :: filePath
   integer :: TOTAL, fileID, rec11, rec15, tstUnit
   CHARACTER(len=13) :: NAME,NUMBER
   real(real64) A0,A1,A2,A3,A4,A5
   logical :: isFile
   integer :: ierr


   fileID = 36
   inquire(iolength=rec11) TOTAL
   inquire(iolength=rec15) NAME,NUMBER,A0,A1,A2,A3,A4,A5

   inquire(FILE=filePath, number=tstUnit, EXIST=isFile)
   if (tstUnit.EQ.fileID) close(fileID)

   if (isFile) then

      OPEN(UNIT=fileID,ACCESS='DIRECT',FILE=filePath,FORM='UNFORMATTED',RECL=rec11,STATUS='UNKNOWN', iostat=ierr)
      if(ierr == 0 ) then
         READ(UNIT=fileID,REC=1) TOTAL
         CLOSE(UNIT=fileID, STATUS='KEEP')

         OPEN(UNIT=fileID,ACCESS='DIRECT',FILE=filePath,FORM='UNFORMATTED',RECL=rec15,STATUS='UNKNOWN')
      end if
   end if


END SUBROUTINE

 ! This should be refactored with the next one, but
 ! save for another day..

SUBROUTINE CHECKGLSRESWAVELENGTHBOUNDS(boolResult,NAME1, NAME2, FLNAME, LMIN, LMAX, sI)
   use DATLEN
   use mod_surface
   use DATMAI
   use iso_fortran_env, only: real64
   logical, intent(inout) :: boolResult
   character(len=*) :: NAME1, NAME2, FLNAME
   integer :: sI ! Surface Number

   real(real64) LMAX,LMIN,LAMLOW,LAMUPP



   boolResult = .TRUE.

   CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
!
!       COMPARE LAMLOW,LAMUPP,LMIN AND LMAX
   IF(LMAX.GT.LAMUPP.AND.LMAX.NE.0.0D0.OR.LMIN.LT.LAMLOW .AND.LMIN.NE.0.0D0) THEN
!       STOP AND GO TO CMD LEVEL AND PRINT WARNING
      boolResult=.FALSE.
      WRITE(OUTLYNE,*)'GLASS DEFINITION WARNING: FOR SURFACE ',sI
      CALL SHOWIT(1)
      OUTLYNE='THE GLASS - '//NAME2
      CALL SHOWIT(1)
      OUTLYNE='IS NOT DEFINED ACROSS THE CURRENT SYSTEM WAVELENGTH RANGE'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,200) LAMLOW,LAMUPP
      CALL SHOWIT(1)
200   FORMAT('IT IS DEFINED ONLY FROM ',F9.4,' TO ',F9.4,' MICRON(S)')
      OUTLYNE='ALL REFRACTIVE INDICES FOR THIS SURFACE HAVE BEEN SET TO 1.0'
      CALL SHOWIT(1)
   END IF


END SUBROUTINE

SUBROUTINE CHECKGLASSCATWAVELENGTHBOUNDS(boolResult, FLNAME)
   use type_utils, only: real2str
   use DATLEN
   use mod_surface
   use DATMAI
   use iso_fortran_env, only: real64

   logical, intent(inout) :: boolResult
   character(len=*) :: FLNAME
   CHARACTER*13 NAME1,NAME2

   real(real64) LMAX,LMIN,LAMLOW,LAMUPP,PNSC


   boolResult = .TRUE.

!       DETERMINE MINIMUM AND MAX GLSWV WAVELENGTHS
   LMIN=GLSWV(1)
   IF(GLSWV(2).LT.LMIN) LMIN=GLSWV(2)
   IF(GLSWV(3).LT.LMIN) LMIN=GLSWV(3)
   IF(GLSWV(4).LT.LMIN) LMIN=GLSWV(4)
   IF(GLSWV(5).LT.LMIN) LMIN=GLSWV(5)
   LMAX=GLSWV(1)
   IF(GLSWV(2).GT.LMAX) LMAX=GLSWV(2)
   IF(GLSWV(3).GT.LMAX) LMAX=GLSWV(3)
   IF(GLSWV(4).GT.LMAX) LMAX=GLSWV(4)
   IF(GLSWV(5).GT.LMAX) LMAX=GLSWV(5)
   IF(LMIN.EQ.0.0D0) LMIN=LMAX
   IF(LMAX.EQ.0.0D0) LMAX=LMIN

!
!     NOW RECHECK THE WAVELENGTH RANGE
   NAME1=WC//'     '
   NAME2=WS(1:13)
   !call LogTermFOR("NAME1 is "//NAME1)
   !call LogTermFOR("FLNAME is "//FLNAME)
   !call LogTermFOR("NAME2 is "//NAME2)

   CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)

   !call LogTermFOR("LAMUPP is "//trim(real2str(LAMUPP)))
   !call LogTermFOR("LAMLOW is "//trim(real2str(LAMLOW)))


!
!       COMPARE LAMLOW,LAMUPP,LMIN AND LMAX
   IF(LMAX.GT.LAMUPP.AND.LMAX.NE.0.0D0.OR.LMIN.LT.LAMLOW .AND.LMIN.NE.0.0D0) THEN

      boolResult = .FALSE.

!       STOP AND GO TO CMD LEVEL AND PRINT WARNING
      WRITE(OUTLYNE,*)'GLASS DEFINITION WARNING: FOR SURFACE '
      CALL SHOWIT(1)
      OUTLYNE='THE GLASS - '//WS
      CALL SHOWIT(1)
      OUTLYNE='IS NOT DEFINED ACROSS THE CURRENT SYSTEM WAVELENGTH RANGE'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,200) LAMLOW,LAMUPP
      CALL SHOWIT(1)
      OUTLYNE='ALL REFRACTIVE INDICES FOR THIS SURFACE HAVE BEEN SET TO 1.0'
      CALL SHOWIT(1)
      GPREG(1:5)=1.0D0
   END IF
200 FORMAT('IT IS DEFINED ONLY FROM ',F9.4,' TO ',F9.4,' MICRON(S)')

END SUBROUTINE
