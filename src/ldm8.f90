!       EIGTH FILE FOR LENS DATABASE MANAGER FILES

! SUB SASTOP.FOR
SUBROUTINE SASTOP
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SASTOP WHICH IMPLEMENTS THE ASTOP
!       COMMAND AT THE LENS OF UPDATE LENS LEVEL.
!
   CHARACTER DD*8
!
   INTEGER I,J,ASURF
!
!
   DD='        '
!
!       BEHAVIOR AT CMD LEVEL
   IF(F1.EQ.1) THEN
!
!               CHECK FOR PRESENCE ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
      IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1)THEN
         OUTLYNE='AT THE CMD LEVEL, "ASTOP" TAKES NO EXPLICIT INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF

      IF(SYSTEM(26).EQ.-99.0D0) THEN
!       THERE IS NO APERTURE STOP DEFINED
         WRITE(OUTLYNE,100)
         CALL SHOWIT(0)
         RETURN
      ELSE
!       THERE IS AN APERTURE STOP DEFINED
      END IF
      ASURF=INT(SYSTEM(26))
      WRITE(OUTLYNE,1000) ASURF
      CALL SHOWIT(0)
      IF(SYSTEM(27).EQ.1.0)  THEN
         WRITE(OUTLYNE,2000)
         CALL SHOWIT(0)
      ELSE
      END IF
      IF(SYSTEM(27).EQ.-1.0) THEN
         WRITE(OUTLYNE,3000)
         CALL SHOWIT(0)
      ELSE
      END IF
      IF(SYSTEM(27).EQ.2.0)  THEN
         WRITE(OUTLYNE,4000)
         CALL SHOWIT(0)
      ELSE
      END IF
      RETURN
   ELSE
!       NOT AT CMD LEVEL
   END IF
!
!
!       BEHAVIOR AT LENS INPUT AND LENS UPDATE LEVEL
!
   IF(F5.EQ.1.OR.F6.EQ.1) THEN
!
!               CHECK FOR PRESENCE OF STRING OR NUMERIC
!               INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
      IF(SST.EQ.1.OR.SN.EQ.1) THEN
         OUTLYNE='"ASTOP" TAKES NO STRING OR NUMERIC INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
!
!       IF (ASTOP DEL) IS ENTERED, REMOVE THE APERTURE STOP DEFINITION
!       BY STORING -99 IN SYSTEM(26) AND 0.0 IN SYSTEM(27)
!
      IF(WQ.EQ.'DELK') THEN
         SYSTEM(26)=-99.0D0
         SYSTEM(27)=0.0
         RETURN
      ELSE
!       NOT REMOVING THE APERTURE STOP
      END IF
!       PROCEED WITH PROCESSING
!
!       CHECK THAT ASTOP NOT PLACED ON OBJECT SURF
!
      IF(SURF.EQ.0) THEN
         OUTLYNE='ASTOP MAY NOT BE ASSIGNED TO THE OBJECT SURFACE'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(ALENS(127,SURF).NE.0.0D0) THEN
         OUTLYNE='THE CURRENT SURFACE HAS MULTIPLE APERTURES ASSIGNED'
         CALL SHOWIT(1)
         OUTLYNE='AND THEREFORE MAY NOT BE SET AS THE ASTOP SURFACE'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL MACFAL
         RETURN
      END IF
!
!       DELETE SOLVES BETWEEN 0 AND DBLE(SURF) IF THERE ARE
!       ARE ANY
      DO I=0,SURF-1
         IF(ALENS(33,I).EQ.1.0D0) THEN
            ALENS(33,I)=0.0D0
            OUTLYNE='THICKNESS SOLVE INFRONT OF APERTURE STOP'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'IS BEING DELETED ON SURFACE ',SURF
            CALL SHOWIT(1)
            CALL MACFAL
            SOLVE(0:9,I)=0.0D0
         END IF
!
         IF(ALENS(33,I).EQ.2.0D0) THEN
            ALENS(33,I)=0.0D0
            OUTLYNE='CURVATURE SOLVE INFRONT OF APERTURE STOP'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'IS BEING DELETED ON SURFACE ',SURF
            CALL SHOWIT(1)
            CALL MACFAL
            SOLVE(0:9,I)=0.0D0
         END IF
!
         IF(ALENS(33,I).EQ.3.0D0) THEN
            ALENS(33,I)=0.0D0
            OUTLYNE=&
            &'CURVATURE AND THICKNESS SOLVES INFRONT OF APERTURE STOP'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'ARE BEING DELETED ON SURFACE ',SURF
            CALL SHOWIT(1)
            CALL MACFAL
            SOLVE(0:9,I)=0.0D0
         END IF
      END DO
!
      SYSTEM(26)=DBLE(SURF)
!       SO Y1 AND X1 WON'T BE READJUSTED IF EXPLICITLY INPUT
!       SET EN/EX FLAG TO 0.0 (NO ADJUSTMENT)
      SYSTEM(27)=0.0
!       SET EN/EX FLAG IN SYSTEM(27)
!
!       CHECK FOR VALID QUALIFIERS
!
      IF(WQ.NE.'EN'.AND.WQ.NE.'EX'.AND.WQ.NE.DD.AND.&
      &WQ.NE.'EN/EX'.AND.WQ.NE.'ENEX') THEN
         OUTLYNE='INVALID QUALIFIER WORD FOR ASTOP COMMAND'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(WQ.EQ.'EN') SYSTEM(27)=1.0
      IF(WQ.EQ.'EX') SYSTEM(27)=-1.0
      IF(WQ.EQ.'EN/EX'.OR.WQ.EQ.'ENEX') SYSTEM(27)=2.0
   ELSE
      OUTLYNE='"ASTOP" NOT VALID AT THIS PROGRAM LEVEL'
      CALL SHOWIT(1)
   END IF
1000 FORMAT('THE CURRENT APERTURE STOP SURFACE IS SURFACE # ',I3)
2000 FORMAT('ENTRANCE PUPIL ADJUSTED')
3000 FORMAT('EXIT PUPIL ADJUSTED')
4000 FORMAT('ENTRANCE AND EXIT PUPILS ADJUSTED')
100 FORMAT('NO APERTURE STOP DEFINED')
!
   RETURN
END
! SUB SASPHD.FOR
SUBROUTINE SASPHD
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SASPHD WHICH IMPLEMENTS THE ASPHD
!       AND TASPHD COMMAND AT THE UPDATE LENS LEVEL.
!       ASPHD REMOVES THE ASPHERIC BUT NOT CONIC DEFINITION
!       ON THE CURRENT SURFACE.
!
   INTEGER I,J,K,SF,II,PIKCNT
!
!
!               CHECK FOR PRESENCE OF QUALIFIER,STRING OR NUMERIC INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SQ.EQ.1.OR.SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      IF(WC.EQ.'ASPHD') OUTLYNE='"ASPHD"'
      IF(WC.EQ.'TASPHD') OUTLYNE='"TASPHD"'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'TAKES NO STRING OR QUALIFIER'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'OR NUMERIC WORD #3 THROUGH #5 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
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
      IF(WC.EQ.'ASPHD') OUTLYNE='"ASPHD"'
      IF(WC.EQ.'TASPHD') OUTLYNE='"TASPHD"'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'USES EITHER TWO OR ZERO NUMERIC WORDS'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(INT(W1).LT.0) THEN
      OUTLYNE=&
      &'STARTING SURFACE NUMBER MUST BE GREATER THAN OR EQUAL TO 0'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(INT(W2).GT.INT(SYSTEM(20))) THEN
      WRITE(OUTLYNE,*)&
      &'ENDING SURFACE NUMBER MUST BE LESS THAN OR EQUAL TO ',&
      &INT(SYSTEM(20))
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(W1.GT.W2) THEN
      OUTLYNE=&
      &'THE ENDING SURFACE # MUST BE GREATER THAN OR EQUAL TO#'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'THE STARTING SURFACE #'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   DO SF=INT(W1),INT(W2)
!
      IF(SF.EQ.0) THEN
         OUTLYNE='OBJECT SURFACE IS ALWAYS PLANO'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!
      IF(WC.EQ.'ASPHD') THEN
         IF(ALENS(8,SF).EQ.1.0D0) THEN
            ALENS(8,SF)=0.0D0
            ALENS(4:7,SF)=0.0D0
            ALENS(81:85,SF)=0.0D0
            ALENS(43,SF)=0.0D0
            WRITE(OUTLYNE,*)'ASPHERIC DEFINITION DELETED FROM SURFACE ',SF
            CALL SHOWIT(1)
!       DUMP PIKUP PRO AND NPRO IF FOUND
            IF(PIKUP(1,SF,11).GT.0.0D0) THEN
               PIKUP(1:6,SF,11)=0.0D0
               ALENS(32,SF)=ALENS(32,SF)-1.0D0
               WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (PRO) DELETED'
               CALL SHOWIT(1)
            END IF
            IF(PIKUP(1,SF,12).GT.0.0D0) THEN
               PIKUP(1:6,SF,12)=0.0D0
               ALENS(32,SF)=ALENS(32,SF)-1.0D0
               WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (NPRO) DELETED'
               CALL SHOWIT(1)
            END IF
!
!       WHAT IF THE SURFACE HAD ASPHERIC PIKUPS ON IT?
!
            DO I=5,9
               IF(I.NE.9) II=I
               IF(I.EQ.9) II=26
               IF(PIKUP(1,SF,II).NE.0.0D0) THEN
                  PIKUP(1:6,SF,II)=0.0D0
!       FIX THE PIKUP COUNTER
                  ALENS(32,SF)=ALENS(32,SF)-1.0D0
                  IF(II.EQ.5) THEN
                     WRITE(OUTLYNE,*)'SURFACE',SF,'PIKUP (AD) DELETED'
                     CALL SHOWIT(1)
                  END IF
                  IF(II.EQ.6) THEN
                     WRITE(OUTLYNE,*)'SURFACE',SF,'PIKUP (AE) DELETED'
                     CALL SHOWIT(1)
                  END IF
                  IF(II.EQ.7) THEN
                     WRITE(OUTLYNE,*)'SURFACE',SF,'PIKUP (AF) DELETED'
                     CALL SHOWIT(1)
                  END IF
                  IF(II.EQ.8) THEN
                     WRITE(OUTLYNE,*)'SURFACE',SF,'PIKUP (AG) DELETED'
                     CALL SHOWIT(1)
                  END IF
                  IF(II.EQ.26) THEN
                     WRITE(OUTLYNE,*)'SURFACE',SF,'PIKUP (AC) DELETED'
                     CALL SHOWIT(1)
                  END IF
               END IF
            END DO
            DO I=27,31
               II=I
               IF(PIKUP(1,SF,II).NE.0.0D0) THEN
                  PIKUP(1:6,SF,II)=0.0D0
!       FIX THE PIKUP COUNTER
                  ALENS(32,SF)=ALENS(32,SF)-1.0D0
                  IF(II.EQ.27) THEN
                     WRITE(OUTLYNE,*)'SURFACE',SF,'PIKUP (AH) DELETED'
                     CALL SHOWIT(1)
                  END IF
                  IF(II.EQ.28) THEN
                     WRITE(OUTLYNE,*)'SURFACE',SF,'PIKUP (AI) DELETED'
                     CALL SHOWIT(1)
                  END IF
                  IF(II.EQ.29) THEN
                     WRITE(OUTLYNE,*)'SURFACE',SF,'PIKUP (AJ) DELETED'
                     CALL SHOWIT(1)
                  END IF
                  IF(II.EQ.30) THEN
                     WRITE(OUTLYNE,*)'SURFACE',SF,'PIKUP (AK) DELETED'
                     CALL SHOWIT(1)
                  END IF
                  IF(II.EQ.31) THEN
                     WRITE(OUTLYNE,*)'SURFACE',SF,'PIKUP (AL) DELETED'
                     CALL SHOWIT(1)
                  END IF
               END IF
            END DO
!
            PIKCNT=0
            DO 502 I=0,INT(SYSTEM(20))
               IF(ALENS(32,I).NE.0.0D0) THEN
                  DO 503 J=1,PSIZ
                     IF(PIKUP(1,I,J).NE.0.0D0) THEN
                        PIKCNT=PIKCNT+1
                     ELSE
                     END IF
503               CONTINUE
                  IF(PIKCNT.EQ.0) ALENS(32,I)=0.0D0
               ELSE
               END IF
502         CONTINUE

!
!       THERE ARE NO ASPHERIC PIKUPS ON SF ANYMORE
!
!       WHAT IF THIS SURFACE WAS THE TARGET OF AN ASPERIC PIKUP
!               PIKUP(I,J,K) WHERE K IS 5,6,7 OR 8
!       IF SO THEN THE PIKUP MUST BE DELETED AND THE ASPHERIC
!       DATA FROZEN ON THE PIKUP SURFACE AT THEIR CURRENT VALUES.
            DO I=0,INT(SYSTEM(20))
               IF(ALENS(32,I).NE.0.0D0) THEN
!       FOUND A PIKUP,IS IT FOR ASPHERICS
                  DO J=5,9
                     IF(J.NE.9) II=J
                     IF(J.EQ.9) II=26
                     IF(PIKUP(1,I,II).EQ.1.0D0) THEN
!       FOUND AN ASPHERIC PIKUP
!       DOES IT REFER TO SURFACE SF
                        IF(INT(PIKUP(2,I,II)).EQ.SF) THEN
!       YES IT REFERS TO THE SURFACE SURF WHICH IS HAVING ITS ASPH
!       DELETED SO GET RIDE OF THE PIKUP
                           PIKUP(1:6,I,II)=0.0D0
                           ALENS(32,I)=ALENS(32,I)-1.0D0
                           IF(II.EQ.5) THEN
                              WRITE(OUTLYNE,*)'SURFACE',SF,'PIKUP (AD) DELETED'
                              CALL SHOWIT(1)
                           END IF
                           IF(II.EQ.6) THEN
                              WRITE(OUTLYNE,*)'SURFACE',SF,'PIKUP (AE) DELETED'
                              CALL SHOWIT(1)
                           END IF
                           IF(II.EQ.7) THEN
                              WRITE(OUTLYNE,*)'SURFACE',SF,'PIKUP (AF) DELETED'
                              CALL SHOWIT(1)
                           END IF
                           IF(II.EQ.8) THEN
                              WRITE(OUTLYNE,*)'SURFACE',SF,'PIKUP (AG) DELETED'
                              CALL SHOWIT(1)
                           END IF
                           IF(II.EQ.26) THEN
                              WRITE(OUTLYNE,*)'SURFACE',SF,'PIKUP (AC) DELETED'
                              CALL SHOWIT(1)
                           END IF
                        END IF
                     END IF
                  END DO
               END IF
            END DO
            DO I=0,INT(SYSTEM(20))
               IF(ALENS(32,I).NE.0.0D0) THEN
!       FOUND A PIKUP,IS IT FOR ASPHERICS
                  DO J=27,31
                     II=J
                     IF(PIKUP(1,I,II).EQ.1.0D0) THEN
!       FOUND AN ASPHERIC PIKUP
!       DOES IT REFER TO SURFACE SF
                        IF(INT(PIKUP(2,I,II)).EQ.SF) THEN
!       YES IT REFERS TO THE SURFACE SURF WHICH IS HAVING ITS ASPH
!       DELETED SO GET RIDE OF THE PIKUP
                           PIKUP(1:6,I,II)=0.0D0
                           ALENS(32,I)=ALENS(32,I)-1.0D0
                           IF(II.EQ.27) THEN
                              WRITE(OUTLYNE,*)'SURFACE',SF,'PIKUP (AH) DELETED'
                              CALL SHOWIT(1)
                           END IF
                           IF(II.EQ.28) THEN
                              WRITE(OUTLYNE,*)'SURFACE',SF,'PIKUP (AI) DELETED'
                              CALL SHOWIT(1)
                           END IF
                           IF(II.EQ.29) THEN
                              WRITE(OUTLYNE,*)'SURFACE',SF,'PIKUP (AJ) DELETED'
                              CALL SHOWIT(1)
                           END IF
                           IF(II.EQ.30) THEN
                              WRITE(OUTLYNE,*)'SURFACE',SF,'PIKUP (AK) DELETED'
                              CALL SHOWIT(1)
                           END IF
                           IF(II.EQ.31) THEN
                              WRITE(OUTLYNE,*)'SURFACE',SF,'PIKUP (AL) DELETED'
                              CALL SHOWIT(1)
                           END IF
                        END IF
                     END IF
                  END DO
               END IF
            END DO
!       WHAT IF THIS SURFACE WAS THE TARGET OF A PIKUP PRO OR NPRO
!               PIKUP(I,J,K) WHERE K IS 11 OR 12
!       IF SO THEN THE PIKUP MUST BE DELETED
            DO 330 I=0,INT(SYSTEM(20))
               IF(ALENS(32,I).NE.0.0D0) THEN
!       FOUND A PIKUP,IS IT FOR PRO OR NPRO
                  DO 331 J=11,12
                     IF(PIKUP(1,I,J).EQ.1.0D0) THEN
!       FOUND A PIKUP
!       DOES IT REFER TO SURFACE SF
                        IF(INT(PIKUP(2,I,J)).EQ.SF) THEN
!       YES IT REFERS TO THE SURFACE SF WHICH IS HAVING ITS ASPH
!       DELETED SO GET RIDE OF THE PIKUP
                           PIKUP(1:6,I,J)=0.0D0
                           ALENS(32,I)=ALENS(32,I)-1.0D0
                           IF(J.EQ.11) THEN
                              WRITE(OUTLYNE,*)'SURFACE',I,'PIKUP (PRO) DELETED'
                              CALL SHOWIT(1)
                           END IF
                           IF(J.EQ.12) THEN
                              WRITE(OUTLYNE,*)'SURFACE',I,'PIKUP (NPRO) DELETED'
                              CALL SHOWIT(1)
                           END IF
                        END IF
                     END IF
331               CONTINUE
               END IF
330         CONTINUE
!
!       NOW FIX ALL THE ALENS(32,K) IN THE LENS SYSTEM
!
            DO 400 I=0,INT(SYSTEM(20))
               IF(ALENS(32,I).NE.1) THEN
!       CHECK PIKUPS
                  PIKCNT=0
                  DO 401 J=1,PSIZ
                     IF(PIKUP(1,I,J).EQ.1.0D0) THEN
                        PIKCNT=PIKCNT+1
                     ELSE
                     END IF
401               CONTINUE
                  IF(PIKCNT.EQ.0) ALENS(32,I)=0.0D0
               ELSE
!       PROCEDD WITH SEARCH
               END IF
400         CONTINUE
!
!       NOW ALL ALENS(32,I) HAVE BEEN CORRECTED
!
            RETURN
         ELSE
!       NO ASPHERIC DATA TO BE REMOVED
            WRITE(OUTLYNE,*)'SURFACE ',SF,' NOT DEFINED AS ASPHERIC'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(WC.EQ.'TASPHD') THEN
!       CHECK IF TORIC
         IF(ALENS(23,SF).EQ.0.0D0) THEN
            WRITE(OUTLYNE,*)'SURFACE',SF,' NOT ANAMORPHIC ASPHERIC'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
!       SURFACE IS TORIC
         IF(ALENS(36,SF).EQ.0.0D0) THEN
            WRITE(OUTLYNE,*)'SURFACE',SF,' NOT ANAMORPHIC ASPHERIC'
            CALL SHOWIT(1)
            RETURN
         END IF
         ALENS(36:40,SF)=0.0D0
!
!       ARE THERE ANAMORPHIC ASPHERIC PIKUPS TO DELETE
!       EITHER ON THIS SURFACE OR ON A SURFACE REFERENCING
!       THIS SURFACE WITH A PIKUP ?.
!
!
!       IF THERE ANAMORPHIC ASPHERIC PIKUPS ON THIS SURFACE THEY MUST GO
!
         IF(PIKUP(1,SF,22).EQ.1.0D0) THEN
            WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (ADTOR) DELETED'
            CALL SHOWIT(1)
            PIKUP(1:6,SF,22)=0.0D0
            ALENS(32,SF)=ALENS(32,SF)-1.0D0
         ELSE
         END IF
         IF(PIKUP(1,SF,23).EQ.1.0D0) THEN
            PIKUP(1:6,SF,23)=0.0D0
            WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (AETOR) DELETED'
            CALL SHOWIT(1)
            ALENS(32,SF)=ALENS(32,SF)-1.0D0
         END IF
         IF(PIKUP(1,SF,24).EQ.1.0D0) THEN
            PIKUP(1:6,SF,24)=0.0D0
            WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (AFTOR) DELETED'
            CALL SHOWIT(1)
            ALENS(32,SF)=ALENS(32,SF)-1.0D0
         END IF
         IF(PIKUP(1,SF,25).EQ.1.0D0) THEN
            PIKUP(1:6,SF,25)=0.0D0
            WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (AGTOR) DELETED'
            CALL SHOWIT(1)
            ALENS(32,SF)=ALENS(32,SF)-1.0D0
         END IF
         IF(PIKUP(1,SF,26).EQ.1.0D0) THEN
            PIKUP(1:6,SF,26)=0.0D0
            WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (AC) DELETED'
            CALL SHOWIT(1)
            ALENS(32,SF)=ALENS(32,SF)-1.0D0
         END IF
!       DUMP PIKUP PRO AND NPRO IF FOUND
         IF(PIKUP(1,SF,11).GT.0.0D0) THEN
            PIKUP(1:6,SF,11)=0.0D0
            ALENS(32,SF)=ALENS(32,SF)-1.0D0
            WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (PRO) DELETED'
            CALL SHOWIT(1)
         END IF
         IF(PIKUP(1,SF,12).GT.0.0D0) THEN
            PIKUP(1:6,SF,12)=0.0D0
            ALENS(32,SF)=ALENS(32,SF)-1.0D0
            WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (NPRO) DELETED'
            CALL SHOWIT(1)
         END IF
!****************************************************************
!       IF ANY SURFACE IN THE LENS IS PIKING UP TORIC DATA FROM
!       THIS SURFACE, THOSE PIKUPS MUST ALSO GO. ALL PIKED
!       UP DATA IS FROZEN AT ITS CURRENT VALUES.
!
         DO 31 I=0,INT(SYSTEM(20))
            IF(PIKUP(2,I,22).EQ.DBLE(SF).AND.&
            &PIKUP(1,I,22).NE.0.0D0.OR.&
            &PIKUP(2,I,23).EQ.DBLE(SF).AND.&
            &PIKUP(1,I,23).NE.0.0D0.OR.&
            &PIKUP(2,I,24).EQ.DBLE(SF).AND.&
            &PIKUP(1,I,24).NE.0.0D0.OR.&
            &PIKUP(2,I,25).EQ.DBLE(SF).AND.&
            &PIKUP(1,I,25).NE.0.0D0.OR.&
            &PIKUP(2,I,26).EQ.DBLE(SF).AND.&
            &PIKUP(1,I,26).NE.0.0D0)THEN
!
!       SURFACE I IS PIKING UP ANAMORPHIC ASPHERIC
!       DATA FROM SURFACE SF
!       DELETE ALL ANAMORPHIC ASPHERIC PIKUPS FROM SURFACE I
!
               IF(PIKUP(1,I,22).EQ.1.0D0) THEN
                  PIKUP(1:6,I,22)=0.0D0
                  WRITE(OUTLYNE,*)'SURFACE',I,' :PIKUP (ADTOR) DELETED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'WHICH REFERENCED SURFACE',SF
                  CALL SHOWIT(1)
                  ALENS(32,I)=ALENS(32,I)-1.0D0
               END IF
               IF(PIKUP(1,I,23).EQ.1.0D0) THEN
                  PIKUP(1:6,I,23)=0.0D0
                  WRITE(OUTLYNE,*)'SURFACE',I,' :PIKUP (AETOR) DELETED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'WHICH REFERENCED SURFACE',SF
                  CALL SHOWIT(1)
                  ALENS(32,I)=ALENS(32,I)-1.0D0
               END IF
               IF(PIKUP(1,I,24).EQ.1.0D0) THEN
                  PIKUP(1:6,I,24)=0.0D0
                  WRITE(OUTLYNE,*)'SURFACE',I,' :PIKUP (AFTOR) DELETED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'WHICH REFERENCED SURFACE',SF
                  CALL SHOWIT(1)
                  ALENS(32,I)=ALENS(32,I)-1.0D0
               END IF
               IF(PIKUP(1,I,25).EQ.1.0D0) THEN
                  PIKUP(1:6,I,25)=0.0D0
                  WRITE(OUTLYNE,*)'SURFACE',I,' :PIKUP (AGTOR) DELETED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'WHICH REFERENCED SURFACE',SF
                  CALL SHOWIT(1)
                  ALENS(32,I)=ALENS(32,I)-1.0D0
               END IF
               IF(PIKUP(1,I,26).EQ.1.0D0) THEN
                  PIKUP(1:6,I,26)=0.0D0
                  WRITE(OUTLYNE,*)'SURFACE',I,' :PIKUP (AC) DELETED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'WHICH REFERENCED SURFACE',SF
                  CALL SHOWIT(1)
                  ALENS(32,I)=ALENS(32,I)-1.0D0
               END IF
            END IF
31       CONTINUE
!
!       IF ANY SURFACE IN THE LENS IS PIKING UP
!       PRO OR NPRO DATA FROM
!       THIS SURFACE, THOSE PIKUPS MUST ALSO GO. ALL PIKED
!       UP DATA IS FROZEN AT ITS CURRENT VALUES.
!
         DO 32 I=0,INT(SYSTEM(20))
            IF(PIKUP(2,I,11).EQ.DBLE(SF).AND.&
            &PIKUP(1,I,11).NE.0.0D0.OR.&
            &PIKUP(2,I,12).EQ.DBLE(SF).AND.&
            &PIKUP(1,I,12).NE.0.0D0) THEN
!
!       SURFACE I IS PIKING UP PRO OR NPRO
!       DATA FROM SURFACE SF
!       DELETE ALL PRO/NPRO PIKUPS FROM SURFACE I
!
               IF(PIKUP(1,I,11).EQ.1.0D0) THEN
                  PIKUP(1:6,I,11)=0.0D0
                  WRITE(OUTLYNE,*)'SURFACE',I,' :PIKUP (NPRO) DELETED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'WHICH REFERENCED SURFACE',SF
                  CALL SHOWIT(1)
                  ALENS(32,I)=ALENS(32,I)-1.0D0
               END IF
               IF(PIKUP(1,I,12).EQ.1.0D0) THEN
                  PIKUP(1:6,I,12)=0.0D0
                  WRITE(OUTLYNE,*)'SURFACE',I,' :PIKUP (NPRO) DELETED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'WHICH REFERENCED SURFACE',SF
                  CALL SHOWIT(1)
                  ALENS(32,I)=ALENS(32,I)-1.0D0
               END IF
            END IF
!
32       CONTINUE
      END IF
   END DO
   RETURN
END
! SUB DELDEFIT.FOR
SUBROUTINE DELDEFIT
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE DELDEFIT WHICH IMPLEMENTS THE DELDEFOR COMMAND
!
   INTEGER I,J,K,SF,II,PIKCNT
!
!
!               CHECK FOR PRESENCE OF QUALIFIER,STRING OR NUMERIC INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SQ.EQ.1.OR.SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      OUTLYNE='"DELDEFOR"'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'TAKES NO STRING OR QUALIFIER'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'OR NUMERIC WORD #3 THROUGH #5 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
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
      OUTLYNE='"DELDEFOR"'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'USES EITHER TWO OR ZERO NUMERIC WORDS'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(INT(W1).LT.0) THEN
      OUTLYNE=&
      &'STARTING SURFACE NUMBER MUST BE GREATER THAN OR EQUAL TO 0'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(INT(W2).GT.INT(SYSTEM(20))) THEN
      WRITE(OUTLYNE,*)&
      &'ENDING SURFACE NUMBER MUST BE LESS THAN OR EQUAL TO ',&
      &INT(SYSTEM(20))
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(W1.GT.W2) THEN
      OUTLYNE=&
      &'THE ENDING SURFACE # MUST BE GREATER THAN OR EQUAL TO#'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'THE STARTING SURFACE #'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   DO SF=INT(W1),INT(W2)
!

      IF(ALENS(103,SF).EQ.1.0D0) THEN
         ALENS(103:106,SF)=0.0D0
         WRITE(OUTLYNE,*)&
         &'DEFORMED SURFACE DEFINITION DELETED FROM SURFACE ',SF
         CALL SHOWIT(1)
502      CONTINUE

         RETURN
      ELSE
!       NO DEFORMED DATA TO BE REMOVED
         WRITE(OUTLYNE,*)'SURFACE ',SF,' NOT A DEFORMABLE SURFACE'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END DO
   RETURN
END
! SUB SARRAYD.FOR
SUBROUTINE SARRAYD
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE ARRAYD WHICH IMPLEMENTS THE ARRAYD
!
   INTEGER SF,I,J,PIKCNT
!
!
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
!               OR NUMERIC INPUT.
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SQ.EQ.1.OR.SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      OUTLYNE=&
      &'"ARRAYD" TAKES NO STRING OR QUALIFIER'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'OR NUMERIC WORD #3 THROUGH #5 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
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
      OUTLYNE=&
      &'"ARRAYD" USES EITHER TWO OR ZERO NUMERIC WORDS'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(INT(W1).LT.0) THEN
      OUTLYNE=&
      &'STARTING SURFACE NUMBER MUST BE GREATER THAN OR EQUAL TO 0'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(INT(W2).GT.INT(SYSTEM(20))) THEN
      WRITE(OUTLYNE,*)&
      &'ENDING SURFACE NUMBER MUST BE LESS THAN OR EQUAL TO ',&
      &INT(SYSTEM(20))
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(W1.GT.W2) THEN
      OUTLYNE=&
      &'THE ENDING SURFACE # MUST BE GREATER THAN OR EQUAL TO#'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'THE STARTING SURFACE #'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   DO SF=INT(W1),INT(W2)
!
      IF(SF.EQ.0) THEN
         OUTLYNE='"ARRAYD" ON OBJECT SURFACE NOT AVALIABLE'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      ELSE
      END IF
      IF(ALENS(133,SF).NE.0.0) THEN
         ALENS(131:133,SF)=0.0D0
         WRITE(OUTLYNE,*)&
         &'ARRAY SURFACE DEFINITION FOR SURFACE',SF,' DELETED'
         CALL SHOWIT(1)
      ELSE
         WRITE(OUTLYNE,*)&
         &'ARRAY SURFACE DEFINITION NOT DEFINED FOR SURFACE'&
         &,SF
         CALL SHOWIT(1)
         CALL MACFAL
      END IF
   END DO
!       DUMP PIKUP PRO AND NPRO IF FOUND
   IF(PIKUP(1,SF,11).GT.0.0D0) THEN
      PIKUP(1:6,SF,11)=0.0D0
      ALENS(32,SF)=ALENS(32,SF)-1.0D0
      WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (PRO) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SF,12).GT.0.0D0) THEN
      PIKUP(1:6,SF,12)=0.0D0
      ALENS(32,SF)=ALENS(32,SF)-1.0D0

      WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (NPRO) DELETED'
      CALL SHOWIT(1)
   END IF
!       WHAT IF THIS SURFACE WAS THE TARGET OF A PIKUP PRO OR NPRO
!               PIKUP(I,J,K) WHERE K IS 11 OR 12
!       IF SO THEN THE PIKUP MUST BE DELETED
   DO 330 I=0,INT(SYSTEM(20))
      IF(ALENS(32,I).NE.0.0D0) THEN
!       FOUND A PIKUP,IS IT FOR PRO OR NPRO
         DO 331 J=11,12
            IF(PIKUP(1,I,J).EQ.1.0D0) THEN
!       FOUND A PIKUP
!       DOES IT REFER TO SURFACE SF
               IF(INT(PIKUP(2,I,J)).EQ.SF) THEN
!       YES IT REFERS TO THE SURFACE SF WHICH IS HAVING ITS ASPH
!       DELETED SO GET RIDE OF THE PIKUP
                  PIKUP(1:6,I,J)=0.0D0
                  ALENS(32,I)=ALENS(32,I)-1.0D0
                  IF(J.EQ.11) THEN
                     WRITE(OUTLYNE,*)'SURFACE',I,'PIKUP (PRO) DELETED'
                     CALL SHOWIT(1)
                  END IF
                  IF(J.EQ.12) THEN
                     WRITE(OUTLYNE,*)'SURFACE',I,'PIKUP (NPRO) DELETED'
                     CALL SHOWIT(1)
                  END IF
               END IF
            END IF
331      CONTINUE
      END IF
330 CONTINUE
!
!       NOW FIX ALL THE ALENS(32,K) IN THE LENS SYSTEM
!
   DO 400 I=0,INT(SYSTEM(20))
      IF(ALENS(32,I).NE.1) THEN
!       CHECK PIKUPS
         PIKCNT=0
         DO 401 J=1,PSIZ
            IF(PIKUP(1,I,J).EQ.1.0D0) THEN
               PIKCNT=PIKCNT+1
            ELSE
            END IF
401      CONTINUE
         IF(PIKCNT.EQ.0) ALENS(32,I)=0.0D0
      ELSE
!       PROCEDD WITH SEARCH
      END IF
400 CONTINUE
!
!       NOW ALL ALENS(32,I) HAVE BEEN CORRECTED
   RETURN
END
! SUB DEFIT.FOR
SUBROUTINE DEFIT
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE DEFIT WHICH IMPLEMENTS THE DEFOR
!       COMMAND AT THE LENS INPUT OR UPDATE LENS LEVEL.
!       AND AT THE CMDER LEVEL
!
   INTEGER I,J,JK,ISURF
!
   LOGICAL GERROR1,GERROR2
!
   REAL*8 D1,D2,D3,D4,D5,D6
!
!
   IF(F5.EQ.1.OR.F6.EQ.1) THEN
!     NOT CMD LEVEL
!
      IF(STI.EQ.1) THEN
         IF(ALENS(103,SURF).EQ.1.0D0) THEN
            WRITE(OUTLYNE,106)SURF
            CALL SHOWIT(0)
            WRITE(OUTLYNE,107)ALENS(104,SURF)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,101)ALENS(105,SURF)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,103)ALENS(106,SURF)
            CALL SHOWIT(0)
106         FORMAT('"DEFORM" PARAMETERS AT SURFACE #',I3,' ARE:')
107         FORMAT('"  FILE# = "',G23.15)
101         FORMAT('"     n  = "',G23.15)
103         FORMAT('"z-scale = "',G23.15)
         ELSE
!       NOT DEFORMED
            WRITE(OUTLYNE,305) SURF
            CALL SHOWIT(0)
         END IF
305      FORMAT('SURFACE #',I3,&
         &' IS NOT A DEFORMABLE SURFACE')
         RETURN
      ELSE
!       NOT STI
      END IF
      IF(SST.EQ.1) THEN
         OUTLYNE=&
         &'"DEFORM" TAKES NO STRING INPUT'
         CALL SHOWIT(1)
         OUTLYNE=&
         &'"AT THE LENS OR UPDATE LENS LEVEL'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         OUTLYNE=&
         &'"DEFORM" TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'
         CALL SHOWIT(1)
         OUTLYNE=&
         &'"AT THE LENS OR UPDATE LENS LEVEL'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(SQ.EQ.0) THEN
         SQ=1
         WQ='F01'
      END IF
      IF(DF1.EQ.1.OR.DF2.EQ.1)&
      &THEN
         OUTLYNE=&
         &'"DEFORM" REQUIRES EXPLICIT NUMERIC WORD #1 THROUGH #3 INPUT'
         CALL SHOWIT(1)
         OUTLYNE=&
         &'"AT THE LENS OR UPDATE LENS LEVEL'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(WQ.NE.'F01'.AND.WQ.NE.'F02'.AND.WQ.NE.'F03'&
      &.AND.WQ.NE.'F04'.AND.WQ.NE.'F05'.AND.WQ.NE.'F06'&
      &.AND.WQ.NE.'F07'.AND.WQ.NE.'F08'.AND.WQ.NE.'F09'&
      &.AND.WQ.NE.'F10') THEN
         OUTLYNE=&
         &'"DEFORM" FILE NUMBER MUST BE FROM F01 TO F10'
         CALL SHOWIT(1)
         OUTLYNE=&
         &'"AT THE LENS OR UPDATE LENS LEVEL'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(W1.LT.1.0D0.OR.W1.GT.3969.0D0) THEN
         OUTLYNE=&
         &'"n" BE FROM 1 TO 3969'
         CALL SHOWIT(1)
         OUTLYNE=&
         &'"AT THE LENS OR UPDATE LENS LEVEL'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(SURF.EQ.0) THEN
         OUTLYNE='OBJECT SURFACE MAY NOT BE A DEFORMABLE SURFACE'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
      ALENS(103,SURF)=1.0D0
      IF(WQ.EQ.'F01') ALENS(104,SURF)=1.0D0
      IF(WQ.EQ.'F02') ALENS(104,SURF)=2.0D0
      IF(WQ.EQ.'F03') ALENS(104,SURF)=3.0D0
      IF(WQ.EQ.'F04') ALENS(104,SURF)=4.0D0
      IF(WQ.EQ.'F05') ALENS(104,SURF)=5.0D0
      IF(WQ.EQ.'F06') ALENS(104,SURF)=6.0D0
      IF(WQ.EQ.'F07') ALENS(104,SURF)=7.0D0
      IF(WQ.EQ.'F08') ALENS(104,SURF)=8.0D0
      IF(WQ.EQ.'F09') ALENS(104,SURF)=9.0D0
      IF(WQ.EQ.'F010') ALENS(104,SURF)=10.0D0
      ALENS(105,SURF)=W1
      ALENS(106,SURF)=W2
      ISURF=SURF
      GERROR1=.FALSE.
      GERROR2=.FALSE.
      CALL DEFGRIDS(1,ISURF,GERROR1,GERROR2)
      CALL DEFGRIDS(3,ISURF,GERROR1,GERROR2)
      RETURN
   END IF
!
   IF(F1.EQ.1) THEN
!     AT CMD LEVEL
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
      IF(SST.EQ.1) THEN
         OUTLYNE='AT THE CMD LEVEL, "DEFORM" TAKES NO STRING INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      ELSE
      END IF
      IF(SQ.EQ.1.AND.S1.EQ.1) THEN
         OUTLYNE=&
         &'AT THE CMD LEVEL, "DEFORM" TAKES EITHER QUALIFIER OR'
         CALL SHOWIT(1)
         OUTLYNE='NUMERIC WORD #1 INPUT BUT NOT BOTH'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         OUTLYNE=&
         &'AT THE CMD LEVEL,'
         CALL SHOWIT(1)
         OUTLYNE=&
         &'"DEFORM" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!       WHAT IF NO SURFACES EXIST
      IF(SYSTEM(20).EQ.0.0D0) THEN
         OUTLYNE='NO DEFORMABLE SURFACE DATA EXISTS'
         CALL SHOWIT(1)
         OUTLYNE='LENS SYSTEM HAS NO SURFACES'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!
!       W1 DESIGNATES THE SURFACE FOR WHICH THE DEFORMABLE
!       DATA IS TO BE OUTPUT.
!       IF THE QUALIFIER "ALL" IS USED, THEN THE DEFORMABLE DATA FOR
!       THE ENTIRE LENS IS PRINTED.
!
!       IF ALENS(103,SURF) NOT EQUAL TO 0.0 THEN THERE IS DEFORM DATA
!
!       PRINT OUT FOR AN INDIVIDUAL SURFACE
!
      IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
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
         IF(W1.LT.0.0D0) W1=SYSTEM(20)+W1
         IF(DF1.EQ.1) W1=DBLE(INT(SYSTEM(20)))
         I=INT(W1)
         IF(I.GT.INT(SYSTEM(20)).OR.I.LT.0) THEN
            OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
!
         D1=ALENS(104,I)
         D2=ALENS(105,I)
         D3=ALENS(106,I)
         IF(ALENS(103,I).EQ.1.0D0) THEN
            IF(HEADIN) WRITE(OUTLYNE,500)
            IF(HEADIN)CALL SHOWIT(0)
            WRITE(OUTLYNE,100)I,INT(D1),D2,D3
            CALL SHOWIT(0)
            RETURN
         ELSE
!       NO DEFORMABLE DATA FOR THAT SURFACE,RETURN
            WRITE(OUTLYNE,300) I
            CALL SHOWIT(0)
            RETURN
         END IF
!       THERE WAS A QUALIFIER.
      ELSE
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
!
         J=0
         DO I=0,INT(SYSTEM(20))
            IF(ALENS(103,I).EQ.1.0D0) THEN
               J=J+1
            ELSE
            END IF
         END DO
         IF(J.EQ.0) THEN
!       WRITE "NO DEFOMABLE SURFACE DATA DATA" AND RETURN
            WRITE(OUTLYNE,301)
            CALL SHOWIT(0)
            RETURN
         END IF
!
!       THERE WAS DATA, WRITE IT
!
         JK=0
         DO I=0,INT(SYSTEM(20))
            IF(ALENS(103,I).EQ.1.0D0) JK=1
         END DO
!       PRINT HEADER MESSAGE
         WRITE(OUTLYNE,400)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,401)
         CALL SHOWIT(0)
         IF(JK.EQ.1) THEN
            WRITE(OUTLYNE,500)
            CALL SHOWIT(0)
         ELSE
         END IF
!
         DO I=0,INT(SYSTEM(20))
            IF(ALENS(103,I).NE.0.0D0) THEN
               D1=ALENS(104,I)
               D2=ALENS(105,I)
               D3=ALENS(106,I)
               WRITE(OUTLYNE,100)I,INT(D1),D2,D3
               CALL SHOWIT(0)
            ELSE
!     NO OUTPUT
            END IF
         END DO
      END IF
   END IF
100 FORMAT(I3,2X,I3,1X,G13.6,1X,G13.6)
300 FORMAT('SURF',1X,I3,1X,&
   &':NO DEFORMABLE SURFACE DATA')
301 FORMAT('NO DEFORMABLE SURFACE DATA')
401 FORMAT(1X)
500 FORMAT('SURF',1X,'FILE#',4X,' n',12X,'z-scale')
400 FORMAT('DEFORMABLE SURFACE DATA')
   RETURN
END
! SUB SASPH.FOR
SUBROUTINE SASPH
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SASPH WHICH IMPLEMENTS THE ASPH
!       COMMAND AT THE LENS INPUT OR UPDATE LENS LEVEL.
!       THE NUMERIC WORDS W1 W2 W3 W4 AND W5 ARE THE
!       4TH, 6TH, 8TH, 10TH AND 2ND ORDER ASPHERIC SURFACE PROFILE
!       COEFFICIENTS. THE DEFAULT AT LENS INITIALIZATION IS
!       ALL COEFFICIENTS = 0.0. IF THE SURFACE IS SET AS AN
!       ASPHERIC, ALENS(8,SURF) IS SET TO 1.0 IF NOT ALENS(8,SURF)
!       IS SET BY DEFAULT TO 0.0. THIS IS A
!       LABEL MARKING THE SURFACE AS AN ASPHERIC. ALSO HANDELS ASPH AT
!       CMD LEVEL.
!
   INTEGER I,J,JK
!
   REAL*8 CC,AC,AD,AE,AF,AG,AH,AI,AJ,AK,AL
!
!
   IF(F5.EQ.1.OR.F6.EQ.1) THEN
      IF(SQ.EQ.1) THEN
         OUTLYNE=&
         &'"ASPH" TAKES NO QUALIFIER AT THE LENS OR UPDATE LENS LEVEL'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!
      IF(STI.EQ.1) THEN
         IF(ALENS(8,I).NE.0.0D0) THEN
            WRITE(OUTLYNE,106)SURF
            CALL SHOWIT(0)
            WRITE(OUTLYNE,101)ALENS(43,I)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,102)ALENS(4,I)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,103)ALENS(5,I)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,104)ALENS(6,I)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,105)ALENS(7,I)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,111)ALENS(81,I)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,112)ALENS(82,I)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,113)ALENS(83,I)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,114)ALENS(84,I)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,115)ALENS(85,I)
            CALL SHOWIT(0)
106         FORMAT('"ASPH" VALUES AT SURFACE #',I3,' ARE:')
101         FORMAT('"AC = "',G23.15)
102         FORMAT('"AD = "',G23.15)
103         FORMAT('"AE = "',G23.15)
104         FORMAT('"AF = "',G23.15)
105         FORMAT('"AG = "',G23.15)
111         FORMAT('"AH = "',G23.15)
112         FORMAT('"AI = "',G23.15)
113         FORMAT('"AJ = "',G23.15)
114         FORMAT('"AK = "',G23.15)
115         FORMAT('"AL = "',G23.15)
         ELSE
!       NOT ASPHERIC
            WRITE(OUTLYNE,305) SURF
            CALL SHOWIT(0)
         END IF
305      FORMAT('SURFACE #',I3,&
         &' IS NOT ASPHERIC, NO ASPHERIC DEFORMATION TERMS EXIST')
         RETURN
      ELSE
!       NOT STI
      END IF
      IF(SURF.EQ.0) THEN
         OUTLYNE='OBJECT SURFACE MAY NOT BE ASPHERIC'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
      IF(SST.EQ.1.OR.SQ.EQ.1) THEN
!
         RETURN
!
      ELSE
      END IF
      IF(WC.EQ.'ASPH') THEN
         IF(ALENS(1,SURF).NE.0.0D0.AND.W5.NE.0.0D0) THEN
            OUTLYNE=&
            &'WARNING:'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'FOR SURFACE ',SURF
            CALL SHOWIT(1)
            OUTLYNE=&
            &'NUMERIC WORD #5 WILL BE IGNORED FOR THIS NON-PLANO SURFACE'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
      END IF
!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
      IF(SN.EQ.1) ALENS(8,SURF)=1.0D0
!       ONLY CHANGE COEF VALUES IF INPUT EXPLICITLY
      IF(WC.EQ.'ASPH'.OR.WC.EQ.'AC'.OR.WC.EQ.'AD'.OR.WC.EQ.'AE'&
      &.OR.WC.EQ.'AF'.OR.WC.EQ.'AG') THEN
         IF(SN.EQ.0) THEN
            ALENS(43,SURF)=0.0D0
            ALENS(4:7,SURF)=0.0D0
         ELSE
            IF(DF5.EQ.0) ALENS(43,SURF)=W5
            IF(DF1.EQ.0) ALENS(4,SURF)=W1
            IF(DF2.EQ.0) ALENS(5,SURF)=W2
            IF(DF3.EQ.0) ALENS(6,SURF)=W3
            IF(DF4.EQ.0) ALENS(7,SURF)=W4
         END IF
      END IF
      IF(WC.EQ.'ASPH2'.OR.WC.EQ.'AH'.OR.WC.EQ.'AI'.OR.WC.EQ.'AJ'&
      &.OR.WC.EQ.'AK'.OR.WC.EQ.'AL') THEN
         IF(SN.EQ.0) THEN
            ALENS(81:85,SURF)=0.0D0
         ELSE
            IF(DF1.EQ.0) ALENS(81,SURF)=W1
            IF(DF2.EQ.0) ALENS(82,SURF)=W2
            IF(DF3.EQ.0) ALENS(83,SURF)=W3
            IF(DF4.EQ.0) ALENS(84,SURF)=W4
            IF(DF5.EQ.0) ALENS(85,SURF)=W5
         END IF
      END IF
!
!     DUMP OTHER PIKUPS
      IF(WC.EQ.'ASPH'.OR.WC.EQ.'AC') THEN
         IF(PIKUP(1,SURF,26).GT.0.0D0) THEN
            PIKUP(1:6,SURF,26)=0.0D0
            ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AC) DELETED'
            CALL SHOWIT(1)
         END IF
      END IF
      IF(WC.EQ.'ASPH'.OR.WC.EQ.'AD') THEN
         IF(PIKUP(1,SURF,5).GT.0.0D0) THEN
            PIKUP(1:6,SURF,5)=0.0D0
            ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AD) DELETED'
            CALL SHOWIT(1)
         END IF
      END IF
      IF(WC.EQ.'ASPH'.OR.WC.EQ.'AE') THEN
         IF(PIKUP(1,SURF,6).GT.0.0D0) THEN
            PIKUP(1:6,SURF,6)=0.0D0
            ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AE) DELETED'
            CALL SHOWIT(1)
         END IF
      END IF
      IF(WC.EQ.'ASPH'.OR.WC.EQ.'AF') THEN
         IF(PIKUP(1,SURF,7).GT.0.0D0) THEN
            PIKUP(1:6,SURF,7)=0.0D0
            ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AF) DELETED'
            CALL SHOWIT(1)
         END IF
      END IF
      IF(WC.EQ.'ASPH'.OR.WC.EQ.'AG') THEN
         IF(PIKUP(1,SURF,8).GT.0.0D0) THEN
            PIKUP(1:6,SURF,8)=0.0D0
            ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AG) DELETED'
            CALL SHOWIT(1)
         END IF
      END IF
      IF(WC.EQ.'ASPH2'.OR.WC.EQ.'AH') THEN
         IF(PIKUP(1,SURF,27).GT.0.0D0) THEN
            PIKUP(1:6,SURF,27)=0.0D0
            ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AH) DELETED'
            CALL SHOWIT(1)
         END IF
      END IF
      IF(WC.EQ.'ASPH2'.OR.WC.EQ.'AI') THEN
         IF(PIKUP(1,SURF,28).GT.0.0D0) THEN
            PIKUP(1:6,SURF,28)=0.0D0
            ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AI) DELETED'
            CALL SHOWIT(1)
         END IF
      END IF
      IF(WC.EQ.'ASPH2'.OR.WC.EQ.'AJ') THEN
         IF(PIKUP(1,SURF,29).GT.0.0D0) THEN
            PIKUP(1:6,SURF,29)=0.0D0
            ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AJ) DELETED'
            CALL SHOWIT(1)
         END IF
      END IF
      IF(WC.EQ.'ASPH2'.OR.WC.EQ.'AK') THEN
         IF(PIKUP(1,SURF,30).GT.0.0D0) THEN
            PIKUP(1:6,SURF,30)=0.0D0
            ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AK) DELETED'
            CALL SHOWIT(1)
         END IF
      END IF
      IF(WC.EQ.'ASPH2'.OR.WC.EQ.'AL') THEN
         IF(PIKUP(1,SURF,31).GT.0.0D0) THEN
            PIKUP(1:6,SURF,31)=0.0D0
            ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AL) DELETED'
            CALL SHOWIT(1)
         END IF
      END IF
!
!     NOW PRO AND NPRO
!
!       DUMP PIKUP PRO AND NPRO IF FOUND
      IF(PIKUP(1,SURF,11).GT.0.0D0) THEN
         PIKUP(1:6,SURF,11)=0.0D0
         ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (PRO) DELETED'
         CALL SHOWIT(1)
      END IF
      IF(PIKUP(1,SURF,12).GT.0.0D0) THEN
         PIKUP(1:6,SURF,12)=0.0D0
         ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (NPRO) DELETED'
         CALL SHOWIT(1)
      END IF
!
   ELSE
      IF(F1.EQ.1) THEN
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
         IF(SST.EQ.1) THEN
            IF(WC.EQ.'ASPH') THEN
               OUTLYNE='AT THE CMD LEVEL, "ASPH" TAKES NO STRING INPUT'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
            END IF
            IF(WC.EQ.'ASPH2') THEN
               OUTLYNE='AT THE CMD LEVEL, "ASPH2" TAKES NO STRING INPUT'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
            END IF
            CALL MACFAL
            RETURN
         ELSE
         END IF
         IF(SQ.EQ.1.AND.S1.EQ.1) THEN
            IF(WC.EQ.'ASPH') THEN
               OUTLYNE=&
               &'AT THE CMD LEVEL, "ASPH" TAKES EITHER QUALIFIER OR'
               CALL SHOWIT(1)
               OUTLYNE='NUMERIC WORD #1 INPUT BUT NOT BOTH'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
            END IF
            IF(WC.EQ.'ASPH2') THEN
               OUTLYNE=&
               &'AT THE CMD LEVEL, "ASPH2" TAKES EITHER QUALIFIER OR'
               CALL SHOWIT(1)
               OUTLYNE='NUMERIC WORD #1 INPUT BUT NOT BOTH'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
            END IF
            CALL MACFAL
            RETURN
         ELSE
         END IF
         IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
            IF(WC.EQ.'ASPH') THEN
               OUTLYNE=&
               &'AT THE CMD LEVEL,'
               CALL SHOWIT(1)
               OUTLYNE=&
               &'"ASPH" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
            END IF
            IF(WC.EQ.'ASPH2') THEN
               OUTLYNE=&
               &'AT THE CMD LEVEL,'
               CALL SHOWIT(1)
               OUTLYNE=&
               &'"ASPH2" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
            END IF
            CALL MACFAL
            RETURN
         ELSE
         END IF
!       WHAT IF NO SURFACES EXIST
         IF(SYSTEM(20).EQ.0.0D0) THEN
            OUTLYNE='NO ASPHERIC DATA EXISTS'
            CALL SHOWIT(1)
            OUTLYNE='LENS SYSTEM HAS NO SURFACES'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
!
!       W1 DESIGNATES THE SURFACE FOR WHICH THE CONIC OR ASPHERIC
!       DATA IS TO BE OUTPUT.
!       IF THE QUALIFIER "ALL" IS USED, THEN THE ASPHERIC DATA FOR
!       THE ENTIRE LENS IS PRINTED.
!
!       IF ALENS(2,SURF) NOT EQUAL TO 0.0 THEN THERE IS CONIC DATA
!       IF ALENS(8,SURF) NOT EQUAL TO 0.0 THEN THERE IS ASPHERIC DATA
!
!       PRINT OUT FOR AN INDIVIDUAL SURFACE
!
         IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
!       THIS IS THE SAME AS ASPH,0
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
            IF(W1.LT.0.0D0) W1=SYSTEM(20)+W1
            IF(DF1.EQ.1) W1=DBLE(INT(SYSTEM(20)))
            I=INT(W1)
            IF(I.GT.INT(SYSTEM(20)).OR.I.LT.0) THEN
               OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
            IF(WC.EQ.'ASPH') THEN
               IF(ALENS(2,I).NE.0.0D0) THEN
                  IF(ALENS(8,I).NE.0.0D0) THEN
                     CC=ALENS(2,I)
                     AC=ALENS(43,I)
                     AD=ALENS(4,I)
                     AE=ALENS(5,I)
                     AF=ALENS(6,I)
                     AG=ALENS(7,I)
                     IF(ALENS(1,I).NE.0.0D0) THEN
                        IF(HEADIN) WRITE(OUTLYNE,500)
                        IF(HEADIN)CALL SHOWIT(0)
                        WRITE(OUTLYNE,100)I,CC,AD,AE,AF,AG
                        CALL SHOWIT(0)
                     ELSE
                        IF(HEADIN) WRITE(OUTLYNE,600)
                        IF(HEADIN) CALL SHOWIT(0)
                        WRITE(OUTLYNE,700)I,AC,AD,AE,AF,AG
                        CALL SHOWIT(0)
                     END IF
                     RETURN
                  ELSE
                     CC=ALENS(2,I)
                     IF(HEADIN) WRITE(OUTLYNE,501)
                     IF(HEADIN)CALL SHOWIT(0)
                     WRITE(OUTLYNE,200)I,CC
                     CALL SHOWIT(0)
                     RETURN
                  END IF
               ELSE
!       NO CONIC BUT MAY BE ASPHERS
                  IF(ALENS(8,I).NE.0.0D0) THEN
                     CC=ALENS(2,I)
                     AC=ALENS(43,I)
                     AD=ALENS(4,I)
                     AE=ALENS(5,I)
                     AF=ALENS(6,I)
                     AG=ALENS(7,I)
                     IF(ALENS(1,I).NE.0.0D0) THEN
                        IF(HEADIN) WRITE(OUTLYNE,500)
                        IF(HEADIN)CALL SHOWIT(0)
                        WRITE(OUTLYNE,100) I,CC,AD,AE,AF,AG
                        CALL SHOWIT(0)
                     ELSE
                        IF(HEADIN) WRITE(OUTLYNE,600)
                        IF(HEADIN)CALL SHOWIT(0)
                        WRITE(OUTLYNE,700) I,AC,AD,AE,AF,AG
                        CALL SHOWIT(0)
                     END IF
                     RETURN
                  ELSE
                  END IF
!       NO CONIC OR ASPHERIC DATA FOR THAT SURFACE,RETURN
                  WRITE(OUTLYNE,300) I
                  CALL SHOWIT(0)
                  RETURN
               END IF
            END IF
            IF(WC.EQ.'ASPH2') THEN
               IF(ALENS(8,I).NE.0.0D0) THEN
                  AH=ALENS(81,I)
                  AI=ALENS(82,I)
                  AJ=ALENS(83,I)
                  AK=ALENS(84,I)
                  AL=ALENS(85,I)
                  IF(HEADIN) WRITE(OUTLYNE,511)
                  IF(HEADIN)CALL SHOWIT(0)
                  WRITE(OUTLYNE,100) I,AH,AI,AJ,AK,AL
                  CALL SHOWIT(0)
                  RETURN
               ELSE
               END IF
!       NO ASPHERIC DATA FOR THAT SURFACE,RETURN

               IF(ALENS(81,I).EQ.0.0D0.AND.ALENS(82,I).EQ.0.0D0.AND.&
               &ALENS(83,I).EQ.0.0D0.AND.ALENS(84,I).EQ.0.0D0.AND.&
               &ALENS(85,I).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,3111) I
                  CALL SHOWIT(0)
               END IF
               RETURN
            END IF
         ELSE
!       THERE WAS A QUALIFIER.
!
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
!
            IF(WC.EQ.'ASPH') THEN
               J=0
               DO I=0,INT(SYSTEM(20))
                  IF(ALENS(2,I).NE.0.0D0.AND.ALENS(8,I).NE.0.0D0.OR.&
                  &ALENS(2,I).NE.0.0D0.OR.ALENS(8,I).NE.0.0D0) THEN
                     J=J+1
                  ELSE
                  END IF
               END DO
               IF(J.EQ.0) THEN
!       WRITE "NO CONIC OR ASPHERIC DATA" AND RETURN
                  WRITE(OUTLYNE,310)
                  CALL SHOWIT(0)
                  RETURN
               END IF
!
!       THERE WAS DATA, WRITE IT
!
               JK=0
               DO I=0,INT(SYSTEM(20))
                  IF(ALENS(8,I).EQ.1.0D0) JK=1
               END DO
!       PRINT HEADER MESSAGE
               WRITE(OUTLYNE,400)
               CALL SHOWIT(0)
               WRITE(OUTLYNE,402)
               CALL SHOWIT(0)
               WRITE(OUTLYNE,403)
               CALL SHOWIT(0)
               WRITE(OUTLYNE,401)
               CALL SHOWIT(0)
               IF(JK.EQ.1) THEN
                  WRITE(OUTLYNE,800)
                  CALL SHOWIT(0)
               ELSE
               END IF
               IF(JK.EQ.0) THEN
                  WRITE(OUTLYNE,501)
                  CALL SHOWIT(0)
               ELSE
               END IF
!
               DO I=0,INT(SYSTEM(20))
                  IF(ALENS(2,I).NE.0.0D0.AND.ALENS(8,I).EQ.0.0D0) THEN
                     CC=ALENS(2,I)
                     WRITE(OUTLYNE,200)I,CC
                     CALL SHOWIT(0)
                  ELSE
                     IF(ALENS(8,I).NE.0.0D0) THEN
                        CC=ALENS(2,I)
                        AC=ALENS(43,I)
                        AD=ALENS(4,I)
                        AE=ALENS(5,I)
                        AF=ALENS(6,I)
                        AG=ALENS(7,I)
                        IF(ALENS(1,I).NE.0.0D0) THEN
                           WRITE(OUTLYNE,100)I,CC,AD,AE,AF,AG
                           CALL SHOWIT(0)
                        ELSE
                           WRITE(OUTLYNE,700)I,AC,AD,AE,AF,AG
                           CALL SHOWIT(0)
                        END IF
                     ELSE
                     END IF
                  END IF
               END DO
               RETURN
            END IF
         END IF
         IF(WC.EQ.'ASPH2') THEN
            J=0
            DO I=0,INT(SYSTEM(20))
               IF(ALENS(81,I).NE.0.0D0.OR.ALENS(82,I).NE.0.0D0.OR.&
               &ALENS(83,I).NE.0.0D0.OR.ALENS(84,I).NE.0.0D0.OR.&
               &ALENS(85,I).NE.0.0D0) THEN
                  J=J+1
               ELSE
               END IF
            END DO
            IF(J.EQ.0) THEN
!       WRITE "ASPHERIC DATA" AND RETURN
               WRITE(OUTLYNE,321)
               CALL SHOWIT(0)
               RETURN
            ELSE
            END IF
!
!       THERE WAS DATA, WRITE IT
!
            JK=0
            DO I=0,INT(SYSTEM(20))
               IF(ALENS(8,I).EQ.1.0D0) JK=1
            END DO
!       PRINT HEADER MESSAGE
            WRITE(OUTLYNE,4211)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,401)
            CALL SHOWIT(0)
            IF(JK.EQ.1) THEN
               WRITE(OUTLYNE,511)
               CALL SHOWIT(0)
            ELSE
            END IF
!
            DO I=0,INT(SYSTEM(20))
               IF(ALENS(8,I).NE.0.0D0) THEN
                  AH=ALENS(81,I)
                  AI=ALENS(82,I)
                  AJ=ALENS(83,I)
                  AK=ALENS(84,I)
                  AL=ALENS(85,I)
                  WRITE(OUTLYNE,100)I,AH,AI,AJ,AK,AL
                  CALL SHOWIT(0)
               END IF
            END DO
            RETURN
         ELSE
         END IF
      END IF
   END IF
100 FORMAT(I3,1X,G13.6,1X,G13.6,1X,G13.6,1X,G13.6,1X,G13.6)
700 FORMAT(I3,1X,G13.6,1X,G13.6,1X,G13.6,1X,G13.6,1X,G13.6 &
   &,1X,'(+)')
200 FORMAT(I3,1X,G13.6)
300 FORMAT('SURF',1X,I3,1X,&
   &':NO CONIC OR 4th through 10th ORDER ASPHERIC DATA')
311 FORMAT('SURF',1X,I3,1X,&
   &':NO (2nd through 10th ORDER ASPHERIC DATA')
3111 FORMAT('SURF',1X,I3,1X,&
   &':NO (12th through 20th ORDER ASPHERIC DATA')
310 FORMAT('NO CONIC OR 4th through 10th ORDER ASPHERIC DATA')
321 FORMAT('NO 12th through 20th ORDER ASPHERIC DATA')
400 FORMAT('CONIC AND 4th THROUGH 10th ORDER ASPHERIC DATA')
4211 FORMAT('ASPHERIC DATA (12th THROUGH 20th ORDERS')
402 FORMAT(&
   &'(+) - DESIGNATES A PLANO SURFACE WITH A 2ND ORDER ASPHERIC')
403 FORMAT(&
   &'TERM IN THE SECOND COLUMN INSTEAD OF A CONIC CONSTANT')
401 FORMAT(1X)
500 FORMAT('SURF',5X,'CC',12X,'AD',12X,'AE',12X,'AF',&
   &12X,'AG',12X)
511 FORMAT('SURF',5X,'AH',12X,'AI',12X,'AJ',12X,'AK',&
   &12X,'AL',12X)
800 FORMAT('SURF',3X,'CC/AC',11X,'AD',12X,'AE',12X,'AF',&
   &12X,'AG',12X)
600 FORMAT('SURF',5X,'AC',12X,'AD',12X,'AE',12X,'AF',&
   &12X,'AG',12X)
501 FORMAT('SURF',5X,'CC')
   RETURN
END
! SUB SARRAY.FOR
SUBROUTINE SARRAY(PRINT_NOT_PRESENT)
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SARRAY WHICH IMPLEMENTS THE ARRAY
!       COMMAND AT THE LENS INPUT OR UPDATE LENS LEVEL.
!       AND AT THE CMDER LEVEL
!
   INTEGER I,J,JK,ISURF
!
   LOGICAL GERROR1,GERROR2,PRINT_NOT_PRESENT
!
   REAL*8 D1,D2,D3,D4,D5
!
!
   IF(F5.EQ.1.OR.F6.EQ.1) THEN
!     NOT CMD LEVEL
!
      IF(STI.EQ.1) THEN
         IF(ALENS(133,SURF).NE.0.0D0) THEN
            WRITE(OUTLYNE,106)SURF
            CALL SHOWIT(0)
            WRITE(OUTLYNE,101)ALENS(131,SURF)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,102)ALENS(132,SURF)
            CALL SHOWIT(0)
            IF(ALENS(133,SURF).EQ.-1.0D0) WRITE(OUTLYNE,103)
            IF(ALENS(133,SURF).EQ. 1.0D0) WRITE(OUTLYNE,104)
            CALL SHOWIT(0)
106         FORMAT('"LENS ARRAY" PARAMETERS AT SURFACE #',I3,' ARE:')
101         FORMAT('DX = "',G23.15)
102         FORMAT('DY = "',G23.15)
103         FORMAT('GRID IS ODD')
104         FORMAT('GRID IS EVEN')
         ELSE
!       NO ARRAY DATA
            WRITE(OUTLYNE,305) SURF
            CALL SHOWIT(0)
         END IF
305      FORMAT('SURFACE #',I3,&
         &' IS NOT AN ARRAY LENS SURFACE')
         RETURN
      ELSE
!       NOT STI
      END IF
      IF(SST.EQ.1) THEN
         OUTLYNE=&
         &'"ARRAY" TAKES NO STRING AT THE LENS OR UPDATE LENS LEVEL'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(SQ.EQ.1.AND.WQ.NE.'ODD'.AND.WQ.NE.'EVEN') THEN
         OUTLYNE=&
         &'"ARRAY" ONLY TAKES "ODD" OR "EVEN" AS QUALIFIERS'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1.OR.DF2.EQ.1)&
      &THEN
         OUTLYNE=&
         &'"ARRAY" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
         CALL SHOWIT(1)
         OUTLYNE=&
         &'"AT THE LENS OR UPDATE LENS LEVEL'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1)&
      &THEN
         OUTLYNE=&
         &'"ARRAY" TAKES NO NUMERIC WORD #3 THROUGH #5 INPUT'
         CALL SHOWIT(1)
         OUTLYNE=&
         &'"AT THE LENS OR UPDATE LENS LEVEL'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(W1.LE.0.0D0.OR.W2.LE.0.0D0) THEN
         OUTLYNE=&
         &'"ARRAY" REQUIRES POSITIVE, INTEGER NUMERIC WORD #1 AND #2 INPUT'
         CALL SHOWIT(1)
         OUTLYNE=&
         &'"AT THE LENS OR UPDATE LENS LEVEL'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(ALENS(34,SURF).NE.0.0D0) THEN
         OUTLYNE=&
         &'SPECIAL SURFACES CAN NOT BE ASSIGNED AS ARRAY SURFACES'
         CALL SHOWIT(1)
         OUTLYNE='NO ACTION TAKEN'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(SURF.EQ.0) THEN
         OUTLYNE='OBJECT SURFACE MAY NOT BE AN ARRAY LENS SURFACE'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
      IF(SQ.EQ.0) THEN
         SQ=1
         WQ='ODD'
      END IF
      IF(WQ.EQ.'ODD') ALENS(133,SURF)=-1.0D0
      IF(WQ.EQ.'EVEN') ALENS(133,SURF)=1.0D0
      ALENS(131,SURF)=W1
      ALENS(132,SURF)=W2
      RETURN
   END IF
!
   IF(F1.EQ.1) THEN
!     AT CMD LEVEL
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
      IF(SST.EQ.1) THEN
         OUTLYNE='AT THE CMD LEVEL, "ARRAY" TAKES NO STRING INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      ELSE
      END IF
      IF(SQ.EQ.1.AND.S1.EQ.1) THEN
         OUTLYNE=&
         &'AT THE CMD LEVEL, "ARRAY" TAKES EITHER QUALIFIER OR'
         CALL SHOWIT(1)
         OUTLYNE='NUMERIC WORD #1 INPUT BUT NOT BOTH'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         OUTLYNE=&
         &'AT THE CMD LEVEL,'
         CALL SHOWIT(1)
         OUTLYNE=&
         &'"ARRAY" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!       WHAT IF NO SURFACES EXIST
      IF(SYSTEM(20).EQ.0.0D0) THEN
         OUTLYNE='NO ARRAY LENS SURFACE DATA EXISTS'
         CALL SHOWIT(1)
         OUTLYNE='LENS SYSTEM HAS NO SURFACES'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!
!       W1 DESIGNATES THE SURFACE FOR WHICH THE ARRAY LENS
!       DATA IS TO BE OUTPUT.
!       IF THE QUALIFIER "ALL" IS USED, THEN THE ARRAY LENS DATA FOR
!       THE ENTIRE LENS IS PRINTED.
!
!       IF ALENS(133,SURF) NOT EQUAL TO 0.0 THEN THERE IS ARRAY LENS DATA
!
!       PRINT OUT FOR AN INDIVIDUAL SURFACE
!
      IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
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
         IF(W1.LT.0.0D0) W1=SYSTEM(20)+W1
         IF(DF1.EQ.1) W1=DBLE(INT(SYSTEM(20)))
         I=INT(W1)
         IF(I.GT.INT(SYSTEM(20)).OR.I.LT.0) THEN
            OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
!
         D1=ALENS(131,I)
         D2=ALENS(132,I)
         IF(ALENS(133,I).NE.0.0D0) THEN
            IF(HEADIN) WRITE(OUTLYNE,500)
            IF(HEADIN)CALL SHOWIT(0)
            IF(ALENS(133,I).EQ.-1.0D0) WRITE(OUTLYNE,100)I,D1,D2
            IF(ALENS(133,I).EQ. 1.0D0) WRITE(OUTLYNE,1001)I,D1,D2
            CALL SHOWIT(0)
            RETURN
         ELSE
!       NO ARRAY LENS DATA FOR THAT SURFACE,RETURN
            IF(PRINT_NOT_PRESENT) THEN
               WRITE(OUTLYNE,300) I
               CALL SHOWIT(0)
            END IF
            RETURN
         END IF
!       THERE WAS A QUALIFIER.
      ELSE
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
!
         J=0
         DO I=0,INT(SYSTEM(20))
            IF(ALENS(133,I).NE.0.0D0) THEN
               J=J+1
            ELSE
            END IF
         END DO
         IF(J.EQ.0) THEN
!       WRITE "NO ARRAY LENS SURFACE DATA DATA" AND RETURN
            IF(PRINT_NOT_PRESENT) THEN
               WRITE(OUTLYNE,301)
               CALL SHOWIT(0)
            END IF
            RETURN
         END IF
!
!       THERE WAS DATA, WRITE IT
!
         JK=0
         DO I=0,INT(SYSTEM(20))
            IF(ALENS(133,I).NE.0.0D0) JK=1
         END DO
         IF(JK.EQ.0) THEN
            IF(PRINT_NOT_PRESENT) THEN
               WRITE(OUTLYNE,301)
               CALL SHOWIT(0)
            END IF
            RETURN
         END IF
!       PRINT HEADER MESSAGE
         WRITE(OUTLYNE,400)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,401)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,500)
         CALL SHOWIT(0)
!
         DO I=0,INT(SYSTEM(20))
            IF(ALENS(133,I).NE.0.0D0) THEN
               D1=ALENS(131,I)
               D2=ALENS(132,I)
               IF(ALENS(133,I).EQ.-1.0D0) WRITE(OUTLYNE,100)I,D1,D2
               IF(ALENS(133,I).EQ.1.0D0) WRITE(OUTLYNE,1001)I,D1,D2
               CALL SHOWIT(0)
            ELSE
!     NO OUTPUT
            END IF
         END DO
      END IF
   END IF
100 FORMAT(I3,6X,'ODD  ARRAY LENS',2X,G13.6,1X,G13.6)
1001 FORMAT(I3,6X,'EVEN ARRAY LENS',2X,G13.6,1X,G13.6)
300 FORMAT('SURF',1X,I3,1X,&
   &':NO ARRAY LENS SURFACE DATA')
301 FORMAT('NO ARRAY LENS SURFACE DATA')
401 FORMAT(1X)
500 FORMAT('SURF',5X,'   GRID TYPE   ',1X,'     DX      ',1X,&
   &'     DY')
400 FORMAT('ARRAY LENS SURFACE DATA')
   RETURN
END
! SUB SNODUM.FOR
SUBROUTINE SNODUM
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SNODUM WHICH IMPLEMENTS THE NODUM COMMAND
!
!
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
!               OR NUMERIC INPUT.
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(STI.EQ.1) THEN
      OUTLYNE='"NODUM" FORCES A SURFACE TO BE NON-DUMMY'
      CALL SHOWIT(1)
      OUTLYNE='WITH A "YES" OR "ON" QUALIFIER INPUT'
      CALL SHOWIT(1)
      OUTLYNE='AND IS TURNED OFF WITH A "NO" OR "OFF"'
      CALL SHOWIT(1)
      OUTLYNE='QUALIFIER INPUT'
      CALL SHOWIT(1)
      IF(ALENS(68,SURF).EQ.0.0D0) THEN
         OUTLYNE='"NODUM" IS CURRENTLY "NO" OR "OFF"'
         WRITE(OUTLYNE,*)'FOR SURFACE NUMBER ',SURF
      ELSE
         OUTLYNE='"NODUM" IS CURRENTLY "YES" OR "ON"'
         WRITE(OUTLYNE,*)'FOR SURFACE NUMBER ',SURF
      END IF
      CALL SHOWIT(1)
      RETURN
   END IF
   IF(SST.EQ.1.OR.SN.EQ.1) THEN
      OUTLYNE='"NODUM" ONLY TAKES QUALIFIER INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(WQ.NE.'ON'.AND.WQ.NE.'OFF'.AND.WQ.NE.&
   &'YES'.AND.WQ.NE.'NO') THEN
      OUTLYNE='"NODUM" ONLY TAKES "YES", "ON", "NO" OR "OFF"'
      CALL SHOWIT(1)
      OUTLYNE='AS QUALIFIERS'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(WQ.EQ.'ON'.OR.WQ.EQ.'YES') ALENS(68,SURF)=1.0D0
   IF(WQ.EQ.'OFF'.OR.WQ.EQ.'NO') ALENS(68,SURF)=0.0D0
!
   RETURN
END
! SUB SURF_TYPE.FOR
SUBROUTINE SURF_TYPE
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!     THIS DOES THE REAL AND PARAX COMMANDS
!
!
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
!               OR NUMERIC INPUT.
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
      IF(WC.EQ.'REAL') OUTLYNE='"REAL" TAKES NO EXPLICIT INPUT'
      IF(WC.EQ.'PARAX') OUTLYNE='"PARAX" TAKES NO EXPLICIT INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
   IF(WC.EQ.'REAL') ALENS(124,SURF)=0.0D0
   IF(WC.EQ.'PARAX') ALENS(124,SURF)=1.0D0
   RETURN
END
! SUB ZERO.FOR
SUBROUTINE ZERO
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
   INTEGER I,J,K,SF
!
!       INITIALIZE THE ARRAYS
   SF=SURF
   SOLVE(0:9,SF)=0.0D0
   PIKUP(1:6,SF,1:PSIZ)=0.0D0
   ALENS(1:LSIZ,SF)=0.0D0
   ALENS(46:50,SF)=1.0D0
   ALENS(71:75,SF)=1.0D0
   ALENS(76:85,SF)=0.0D0
   ALENS(51:70,SF)=0.0D0
   ALENS(127:128,SF)=0.0D0
   ALENS(134:137,SF)=0.0D0
   MULTCLAP(1:1000,1:3,SF)=0.0D0
   MULTCOBS(1:1000,1:3,SF)=0.0D0
   GLANAM(SF,1)='             '
   GLANAM(SF,2)='AIR          '
   LBL(SF)(1:80)=' '
   RETURN
END
! SUB SAPED.FOR
SUBROUTINE SAPED
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SAPED WHICH IMPLEMENTS THE CLAPD
!       AND COBSD COMMAND AT THE UPDATE LENS LEVEL.
!
   INTEGER PIKCNT,K,I,J,SF
!
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SQ.EQ.1.OR.SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      OUTLYNE='"CLAPD" AND "COBSD"'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'TAKE NO STRING OR QUALIFIER'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'OR NUMERIC WORD #3 THROUGH #5 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
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
      OUTLYNE='"CLAPD" AND "COBSD"'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'USE EITHER TWO OR ZERO NUMERIC WORDS'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(INT(W1).LT.0) THEN
      OUTLYNE=&
      &'STARTING SURFACE NUMBER MUST BE GREATER THAN OR EQUAL TO 0'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(INT(W2).GT.INT(SYSTEM(20))) THEN
      WRITE(OUTLYNE,*)&
      &'ENDING SURFACE NUMBER MUST BE LESS THAN OR EQUAL TO ',&
      &INT(SYSTEM(20))
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(W1.GT.W2) THEN
      OUTLYNE=&
      &'THE ENDING SURFACE # MUST BE GREATER THAN OR EQUAL TO#'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'THE STARTING SURFACE #'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   DO SF=INT(W1),INT(W2)
!
!               WE ARE AT THE LENS UPDATE LEVEL
!
!       CHECK IF A CLAP EXISTS, IF NOT SAY SO
      IF(WC.EQ.'CLAPD'.AND.ALENS(9,SF).EQ.0.0D0) THEN
!       NO CLAP
!     MAKE SURE CLAP ERASE IS OFF
         ALENS(51:57,SF)=0.0D0
         WRITE(OUTLYNE,*)'SURFACE',SF,' :NO CLEAR APERTURE TO DELETE'
         CALL SHOWIT(1)
      END IF
!       CHECK IF A COBS EXISTS, IF NOT SAY SO AND RETURN
      IF(WC.EQ.'COBSD'.AND.ALENS(16,SF).EQ.0.0D0) THEN
!       NO COBS
!     MAKE SURE COBS ERASE IS OFF
         ALENS(61:67,SF)=0.0D0
         WRITE(OUTLYNE,*)'SURFACE',SF,' :NO OBSCURATION TO DELETE'
         CALL SHOWIT(1)
      END IF
!
      IF(WC.EQ.'CLAPD') THEN
         ALENS(9,SF)=0.0D0
         ALENS(127,SF)=0.0D0
         ALENS(10:15,SF)=0.0D0
         ALENS(51:57,SF)=0.0D0
!
!       WHAT IF THE SURFACE HAD A CLAP PIKUP ON IT?
!
         IF(PIKUP(1,SF,18).NE.0.0D0) THEN
            PIKUP(1:6,SF,18)=0.0D0
501         CONTINUE
!       FIX THE PIKUP COUNTER
            ALENS(32,SF)=ALENS(32,SF)-1.0D0
            WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (CLAP) DELETED'
            CALL SHOWIT(1)
         END IF
!
         PIKCNT=0
         DO 502 I=0,INT(SYSTEM(20))
            DO 503 J=1,PSIZ
               IF(PIKUP(1,I,J).NE.0.0D0) THEN
                  PIKCNT=PIKCNT+1
               ELSE
               END IF
503         CONTINUE
            IF(PIKCNT.EQ.0) ALENS(32,I)=0.0D0
502      CONTINUE
!
!       THERE IS NO PIKUP CLAP ON SURF ANYMORE
!
!       WHAT IF THIS SURFACE WAS THE TARGET OF A CLAP PIKUP
!
!       IF SO THEN THE PIKUP MUST BE DELETED AND THE CLAP
!       DATA FROZEN ON THE PIKUP SURFACE AT ITS CURRENT VALUE.
         DO 300 I=0,INT(SYSTEM(20))
            IF(PIKUP(1,I,18).EQ.1.0D0) THEN
!       FOUND A CLAP PIKUP
!       DOES IT REFER TO SURFACE SF
               IF(INT(PIKUP(2,I,18)).EQ.SF) THEN
!       YES IT REFERS TO THE SURFACE SF WHICH IS HAVING ITS CLAP
!       DELETED SO GET RIDE OF THE PIKUP
                  PIKUP(1:6,I,18)=0.0D0
                  ALENS(32,I)=ALENS(32,I)-1.0D0
                  IF(J.EQ.5) THEN
                     WRITE(OUTLYNE,*)'(CLAP) PIKUP DELETED ON SURFACE',I
                     CALL SHOWIT(1)
                  END IF
               END IF
            END IF
300      CONTINUE
!
!       NOW FIX ALL THE ALENS(32,K) IN THE LENS SYSTEM
!
         DO 400 I=0,INT(SYSTEM(20))
!       CHECK PIKUPS
            PIKCNT=0
            DO 401 J=1,PSIZ
               IF(PIKUP(1,I,J).EQ.1.0D0) THEN
                  PIKCNT=PIKCNT+1
               ELSE
               END IF
401         CONTINUE
            IF(PIKCNT.EQ.0) ALENS(32,I)=0.0D0
!
400      CONTINUE
      END IF
      IF(WC.EQ.'COBSD') THEN
         ALENS(16:22,SF)=0.0D0
         ALENS(61:67,SF)=0.0D0
!       WHAT IF THE SURFACE HAD A COBS PIKUP ON IT?
!
         IF(PIKUP(1,SF,19).NE.0.0D0) THEN
            PIKUP(1:6,SF,19)=0.0D0
!       FIX THE PIKUP COUNTER
            ALENS(32,SF)=ALENS(32,SF)-1.0D0
            WRITE(OUTLYNE,*)'SURFACE',SF,' :PIKUP (COBS) DELETED'
            CALL SHOWIT(1)
         END IF
!
         PIKCNT=0
         DO 5021 I=0,INT(SYSTEM(20))
            DO 5031 J=1,PSIZ
               IF(PIKUP(1,I,J).NE.0.0D0) THEN
                  PIKCNT=PIKCNT+1
               ELSE
               END IF
5031        CONTINUE
            IF(PIKCNT.EQ.0) ALENS(32,I)=0.0D0
5021     CONTINUE
!
!       THERE IS NO PIKUP COBS ON SF ANYMORE
!
!       WHAT IF THIS SURFACE WAS THE TARGET OF A COBS PIKUP
!
!       IF SO THEN THE PIKUP MUST BE DELETED AND THE COBS
!       DATA FROZEN ON THE PIKUP SURFACE AT ITS CURRENT VALUE.
         DO 3001 I=0,INT(SYSTEM(20))
            IF(PIKUP(1,I,19).EQ.1.0D0) THEN
!       FOUND A COBS PIKUP
!       DOES IT REFER TO SURFACE SF
               IF(INT(PIKUP(2,I,19)).EQ.SF) THEN
!       YES IT REFERS TO THE SURFACE SF WHICH IS HAVING ITS COBS
!       DELETED SO GET RIDE OF THE PIKUP
                  PIKUP(1:6,I,19)=0.0D0
                  ALENS(32,I)=ALENS(32,I)-1.0D0
                  IF(J.EQ.5) THEN
                     WRITE(OUTLYNE,*)'(COBS) PIKUP DELETED ON SURFACE',I
                     CALL SHOWIT(1)
                  END IF
               END IF
            END IF
3001     CONTINUE
!
!       NOW FIX ALL THE ALENS(32,K) IN THE LENS SYSTEM
!
         DO 4001 I=0,INT(SYSTEM(20))
!       CHECK PIKUPS
            PIKCNT=0
            DO 4011 J=1,PSIZ
               IF(PIKUP(1,I,J).EQ.1.0D0) THEN
                  PIKCNT=PIKCNT+1
               ELSE
               END IF
4011        CONTINUE
            IF(PIKCNT.EQ.0) ALENS(32,I)=0.0D0
!
4001     CONTINUE
      END IF
   END DO
   RETURN
END
! SUB SAPE.FOR
SUBROUTINE SAPE
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SAPE WHICH IMPLEMENTS THE CLAP
!       AND COBS COMMAND AT THE LENS INPUT AND UPDATE LENS LEVEL.
!
   CHARACTER DD*8,COTYPE*8,CLTYPE*8
!
   INTEGER PIKCNT,I,K
!
   REAL*8 MVAL
!
!
   DD='        '
   IF(SQ.EQ.0) WQ=DD
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
   IF(STI.EQ.1) THEN
      IF(WC.EQ."CLAP") THEN
         IF(ALENS(9,SURF).NE.0.0D0) THEN
            IF(ALENS(9,SURF).EQ.1.0D0) CLTYPE ='    CIRC'
            IF(ALENS(9,SURF).EQ.2.0D0) CLTYPE ='    RECT'
            IF(ALENS(9,SURF).EQ.3.0D0) CLTYPE ='    ELIP'
            IF(ALENS(9,SURF).EQ.4.0D0) CLTYPE ='    RCTK'
            IF(ALENS(9,SURF).EQ.5.0D0) CLTYPE ='    POLY'
            IF(ALENS(9,SURF).EQ.6.0D0) CLTYPE ='   IPOLY'
            WRITE(OUTLYNE,200)SURF
            CALL SHOWIT(0)
200         FORMAT('THE LAST "CLAP" ASSIGNED TO SURFACE # ',I3)
            WRITE(OUTLYNE,100)CLTYPE
            CALL SHOWIT(0)
            WRITE(OUTLYNE,109)ALENS(10,SURF)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,102)ALENS(11,SURF)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,103)ALENS(12,SURF)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,104)ALENS(13,SURF)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,105)ALENS(14,SURF)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,106) ALENS(15,SURF)
            CALL SHOWIT(0)
100         FORMAT('WAS : "',A8,'" WITH NUMERIC INPUT :')
109         FORMAT('NW1 = ',G23.15)
102         FORMAT('NW2 = ',G23.15)
103         FORMAT('NW3 = ',G23.15)
104         FORMAT('NW4 = ',G23.15)
105         FORMAT('NW5 = ',G23.15)
106         FORMAT('WITH "CLAP TILT" = ',D23.15)
         ELSE
            WRITE(OUTLYNE,500) SURF
            CALL SHOWIT(0)
500         FORMAT('NO "CLAP" IS CURRENTLY ASSIGNED TO SURFACE # ',I3)
!       NO CLAP ON SURFACE
         END IF
      ELSE
!       WC NOT "CLAP"
      END IF
      IF(WC.EQ."CLAP") THEN
         IF(ALENS(51,SURF).NE.0.0D0) THEN
            IF(ALENS(51,SURF).EQ.1.0D0) CLTYPE='   ERASE'
            IF(ALENS(51,SURF).EQ.2.0D0) CLTYPE='   RECTE'
            IF(ALENS(51,SURF).EQ.3.0D0) CLTYPE='   ELIPE'
            IF(ALENS(51,SURF).EQ.4.0D0) CLTYPE='   RCTKE'
            IF(ALENS(51,SURF).EQ.5.0D0) CLTYPE='   POLYE'
            IF(ALENS(51,SURF).EQ.6.0D0) CLTYPE='  IPOLYE'
            WRITE(OUTLYNE,201)SURF
            CALL SHOWIT(0)
201         FORMAT('THE LAST "CLAP ERASE" ASSIGNED TO SURFACE # ',I3)
            WRITE(OUTLYNE,100)CLTYPE
            CALL SHOWIT(0)
            WRITE(OUTLYNE,109)ALENS(52,SURF)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,102)ALENS(53,SURF)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,103)ALENS(54,SURF)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,104)ALENS(55,SURF)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,105)ALENS(56,SURF)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,106) ALENS(57,SURF)
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,501) SURF
            CALL SHOWIT(0)
501         FORMAT(&
            &'NO "CLAP ERASE" IS CURRENTLY ASSIGNED TO SURFACE # ',I3)
!       NO CLAP ON SURFACE
         END IF
      ELSE
!       WC NOT "CLAP"
      END IF
      IF(WC.EQ."COBS") THEN
         IF(ALENS(16,SURF).NE.0.0D0) THEN
            IF(ALENS(16,SURF).EQ.1.0D0) COTYPE= 'OB  CIRC'
            IF(ALENS(16,SURF).EQ.2.0D0) COTYPE= 'OB  RECT'
            IF(ALENS(16,SURF).EQ.3.0D0) COTYPE= 'OB  ELIP'
            IF(ALENS(16,SURF).EQ.4.0D0) COTYPE= 'OB  RCTK'
            IF(ALENS(16,SURF).EQ.5.0D0) COTYPE= 'OB  POLY'
            IF(ALENS(16,SURF).EQ.6.0D0) COTYPE= 'OB IPOLY'
            WRITE(OUTLYNE,400)SURF
            CALL SHOWIT(0)
400         FORMAT('THE LAST "COBS" ASSIGNED TO SURFACE # ',I3)
            WRITE(OUTLYNE,300)COTYPE
            CALL SHOWIT(0)
            WRITE(OUTLYNE,301)ALENS(17,SURF)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,302)ALENS(18,SURF)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,303)ALENS(19,SURF)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,304)ALENS(20,SURF)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,305)ALENS(21,SURF)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,306) ALENS(22,SURF)
            CALL SHOWIT(0)
300         FORMAT('WAS : "',A8,'" WITH NUMERIC INPUT :')
301         FORMAT('NW1 = ',G23.15)
302         FORMAT('NW2 = ',G23.15)
303         FORMAT('NW3 = ',G23.15)
304         FORMAT('NW4 = ',G23.15)
305         FORMAT('NW5 = ',G23.15)
306         FORMAT('WITH "COBS TILT" = ',D23.15)
         ELSE
            WRITE(OUTLYNE,600) SURF
            CALL SHOWIT(0)
600         FORMAT('NO "COBS" IS CURRENTLY ASSIGNED TO SURFACE # ',I3)
!       NO COBS ON SURFACE
         END IF
      ELSE
!       WC NOT "COBS"
      END IF
      IF(WC.EQ."COBS") THEN
         IF(ALENS(61,SURF).NE.0.0D0) THEN
            IF(ALENS(61,SURF).EQ.1.0D0) COTYPE='OB ERASE'
            IF(ALENS(61,SURF).EQ.2.0D0) COTYPE='OB RECTE'
            IF(ALENS(61,SURF).EQ.3.0D0) COTYPE='OB ELIPE'
            IF(ALENS(61,SURF).EQ.4.0D0) COTYPE='OB RCTKE'
            IF(ALENS(61,SURF).EQ.5.0D0) COTYPE='OB POLYE'
            IF(ALENS(61,SURF).EQ.6.0D0) COTYPE='OBIPOLYE'
            WRITE(OUTLYNE,401)SURF
            CALL SHOWIT(0)
401         FORMAT(&
            &'THE LAST "COBS ERASE" ASSIGNED TO SURFACE # ',I3)
            WRITE(OUTLYNE,300)COTYPE
            CALL SHOWIT(0)
            WRITE(OUTLYNE,301)ALENS(62,SURF)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,302)ALENS(63,SURF)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,303)ALENS(64,SURF)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,304)ALENS(65,SURF)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,305)ALENS(66,SURF)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,306) ALENS(67,SURF)
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,601) SURF
            CALL SHOWIT(0)
601         FORMAT(&
            &'NO "COBS ERASE" IS CURRENTLY ASSIGNED TO SURFACE # ',I3)
!       NO COBS ON SURFACE
         END IF
      ELSE
!       WC NOT "COBS"
      END IF
      RETURN
   ELSE
!       NOT STI
   END IF
!
   IF(SST.EQ.1) THEN
      OUTLYNE='"'//WC(1:4)//'" TAKES NO STRING INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!       DEFAULT INPUT CHECKING
!       CLAP COMMANDS ARE SENSLESS IF INPUT WITH ALL
!       ZERO VALUES
   IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1.AND.DF4 &
   &.EQ.1.AND.DF5.EQ.1) THEN
      OUTLYNE='ALL "'//WC(1:4)//'" COMMANDS REQUIRE SOME EXPLICIT'
      CALL SHOWIT(1)
      OUTLYNE='NUMERICAL INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!       CLAP COMMANDS ARE SENSLESS IF INPUT WITH NW1 VALUE
!       ZERO
   IF(DF1.EQ.1) THEN
      OUTLYNE=&
      &'ALL "'//WC(1:4)//'" COMMANDS REQUIRE EXPLICIT NW1 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(WQ.NE.'TILT'.AND.W1.LT.0.0D0.AND.WQ.NE.'TILTE'.AND.&
   &W1.LT.0.0D0) THEN
      OUTLYNE='NEGATIVE VALUES NOT ALLOWED FOR '//WC(1:4)
      CALL SHOWIT(1)
      OUTLYNE='"CLAP" OR "COBS" NUMERIC WORD #1 VALUES'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(WQ.NE.'TILT'.AND.WQ.NE.' '.AND.W2.LT.0.0D0.AND.&
   &WQ.NE.'TILTE'.AND.W2.LT.0.0D0) THEN
      OUTLYNE='NEGATIVE VALUES NOT ALLOWED FOR '//WC(1:4)
      CALL SHOWIT(1)
      OUTLYNE='"CLAP" OR"COBS" NUMERIC WORD #2 VALUES'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!       SPECIFIC CASES OF DEFAULT INPUT
!       RECT- IF RECT THEN IF DF2=1, NW2=NW1 BY DEFAULT
!       ELIP- IF ELIP THEN IF DF2=1, NW2=NW1 BY DEFAULT
!       RCTK- IF RCTK THEN IF DF2=1, NW2=NW1 BY DEFAULT
!       POLY- IF POLY THEN IF NW2 EXPLICITLY REQUIRED
!               AND RADIUS IS 0.01 BY DEFAULT
!
!       ALL VALUES FOR TILT DEFAULT TO 0.0
!       ALL VALUES OF YDEC AND XDEC DEFAULT TO 0.0
!
!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
!               CHECK FOR VALID QUALIFIERS
   IF(WQ.EQ.DD.OR.WQ.EQ.'RECT'.OR.WQ.EQ.'ELIP'.OR.WQ.EQ.'POLYE'&
   &.OR.WQ.EQ.'RCTK'.OR.WQ.EQ.'TILT'.OR.WQ.EQ.'POLY'&
   &.OR.WQ.EQ.'ERASE'.OR.WQ.EQ.'RECTE'.OR.WQ.EQ.'IPOLY'.OR.WQ.EQ.&
   &'IPOLYE'.OR.WQ.EQ.&
   &'ELIPE'.OR.WQ.EQ.'RCTKE'.OR.WQ.EQ.'TILTE') THEN
!     PROCEED
   ELSE
!               INVALID QUALIFIER,ERROR AND STOP
      OUTLYNE='INVALID QUALIFIER INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
   IF(WC.EQ.'COBS') THEN
!
      IF(WQ.EQ.DD) ALENS(16,SURF)=1.0D0
      IF(WQ.EQ.'RECT') ALENS(16,SURF)=2.0D0
      IF(WQ.EQ.'ELIP') ALENS(16,SURF)=3.0D0
      IF(WQ.EQ.'RCTK') ALENS(16,SURF)=4.0D0
      IF(WQ.EQ.'POLY') ALENS(16,SURF)=5.0D0
      IF(WQ.EQ.'IPOLY') ALENS(16,SURF)=6.0D0
      IF(WQ.EQ.'ERASE') ALENS(61,SURF)=1.0D0
      IF(WQ.EQ.'RECTE') ALENS(61,SURF)=2.0D0
      IF(WQ.EQ.'ELIPE') ALENS(61,SURF)=3.0D0
      IF(WQ.EQ.'RCTKE') ALENS(61,SURF)=4.0D0
      IF(WQ.EQ.'POLYE') ALENS(61,SURF)=5.0D0
      IF(WQ.EQ.'IPOLYE') ALENS(61,SURF)=6.0D0
      IF(WQ.NE.'TILT'.AND.WQ.NE.'TILTE') THEN
         IF(DF1.EQ.1) W1=0.0D0
         IF(DF2.EQ.1) W2=0.0D0
         IF(DF3.EQ.1) W3=0.0D0
         IF(DF4.EQ.1) W4=0.0D0
         IF(DF5.EQ.1) W5=0.0D0
         IF(WQ.EQ.DD) THEN
            ALENS(17,SURF)=W1
            ALENS(18,SURF)=0.0D0
            ALENS(19,SURF)=W2
            ALENS(20,SURF)=W3
            ALENS(21,SURF)=0.0D0
            ALENS(22,SURF)=0.0D0
         END IF
         IF(WQ.EQ.'RECT'.OR.WQ.EQ.'ELIP'.OR.WQ.EQ.'RCTK'&
         &.OR.WQ.EQ.'POLY'.OR.WQ.EQ.'IPOLY') THEN
            ALENS(17,SURF)=W1
            ALENS(18,SURF)=W2
            ALENS(19,SURF)=W3
            ALENS(20,SURF)=W4
            ALENS(21,SURF)=W5
            ALENS(22,SURF)=0.0D0
         END IF
         IF(WQ.EQ.'ERASE') THEN
            ALENS(62,SURF)=W1
            ALENS(63,SURF)=0.0D0
            ALENS(64,SURF)=W2
            ALENS(65,SURF)=W3
            ALENS(66,SURF)=0.0D0
            ALENS(67,SURF)=0.0D0
         END IF
         IF(WQ.EQ.'RECTE'.OR.WQ.EQ.'ELIPE'.OR.WQ.EQ.'RCTKE'&
         &.OR.WQ.EQ.'POLYE'.OR.WQ.EQ.'IPOLYE') THEN
            ALENS(62,SURF)=W1
            ALENS(63,SURF)=W2
            ALENS(64,SURF)=W3
            ALENS(65,SURF)=W4
            ALENS(66,SURF)=W5
            ALENS(67,SURF)=0.0D0
         END IF
         IF(ALENS(16,SURF).EQ.1.0D0) THEN
!       REMOVE EXISTING COBS TILT
            IF(ALENS(22,SURF).NE.0.0D0) THEN
               OUTLYNE='EXISTING CLAP TILT REMOVED'
               CALL SHOWIT(1)
               ALENS(22,SURF)=0.0D0
            END IF
            IF(DF4.EQ.0) THEN
               OUTLYNE=&
               &'FOR CIRCULAR COBS OR COBS ERASE, NW4 IS NOT USED'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
            IF(DF5.EQ.0) THEN
               OUTLYNE=&
               &'FOR CIRCULAR COBS OR COBS ERASE, NW5 IS NOT USED'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
         END IF
         IF(ALENS(16,SURF).EQ.2.0D0) THEN
            IF(ALENS(21,SURF).NE.0.0D0) THEN
               OUTLYNE=&
               &'FOR RECTANGULAR COBS OR COBS ERASE, NW5 IS NOT USED'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
         END IF
         IF(ALENS(16,SURF).EQ.3.0D0) THEN
            IF(ALENS(21,SURF).NE.0.0D0) THEN
               OUTLYNE=&
               &'FOR ELLIPTICAL COBS OR COBS ERASE, NW5 IS NOT USED'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
         END IF
         IF(ALENS(16,SURF).EQ.5.0D0) THEN
            IF(ALENS(21,SURF).NE.0.0D0) THEN
               OUTLYNE=&
               &'FOR POLY COBS OR COBS ERASE, NW5 IS NOT USED'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
         END IF
         IF(ALENS(16,SURF).EQ.5.0D0) THEN
            IF(ALENS(18,SURF).LT.3.0D0.OR.ALENS(18,SURF).GT.200.0D0) THEN
               OUTLYNE=&
               &'FOR POLY COBS OR COBS ERASE, NW2 MUST BE'
               CALL SHOWIT(1)
               OUTLYNE='3 OR GREATER BUT LESS THAN 201'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
         END IF
         IF(ALENS(16,SURF).EQ.6.0D0) THEN
            IF(DF1.EQ.1.OR.DF2.EQ.1.OR.DF3.EQ.1.OR.DF4.EQ.1.OR.DF5.EQ.1)THEN
               OUTLYNE=&
               &'IPOLY COBS REQUIRES ALL FIVE EXPLICIT NUMERIC WORDS'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
         END IF
         IF(ALENS(16,SURF).EQ.6.0D0) THEN
            IF(ALENS(17,SURF).LT.1.0D0.OR.ALENS(17,SURF).GT.99.0D0) THEN
               OUTLYNE=&
               &'FOR IPOLY COBS OR COBS ERASE, NW1 MUST BE'
               CALL SHOWIT(1)
               OUTLYNE='1 OR GREATER AND LESS THAN OR EQUAL TO 99'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
         END IF
      END IF
!       DEFAULT ASSIGNMENT
      IF(WQ.EQ.'RECT'.AND.DF2.EQ.1)&
      &ALENS(18,SURF)=ALENS(17,SURF)
      IF(WQ.EQ.'ELIP'.AND.DF2.EQ.1)&
      &ALENS(18,SURF)=ALENS(17,SURF)
      IF(WQ.EQ.'RCTK'.AND.DF2.EQ.1)&
      &ALENS(18,SURF)=ALENS(17,SURF)
      IF(WQ.EQ.'RCTK'.AND.DF5.EQ.1)&
      &ALENS(21,SURF)=0.0D0
      IF(WQ.EQ.'POLY'.AND.DF5.EQ.1)&
      &ALENS(21,SURF)=0.0D0
!     ERASES
      IF(ALENS(61,SURF).EQ.1.0D0) THEN
!       REMOVE EXISTING COBS TILT
         IF(ALENS(67,SURF).NE.0.0D0) THEN
            OUTLYNE='EXISTING CLAP TILTE REMOVED'
            CALL SHOWIT(1)
            ALENS(67,SURF)=0.0D0
         END IF
         IF(DF4.EQ.0) THEN
            OUTLYNE=&
            &'FOR CIRCULAR COBS ERASE, NW4 IS NOT USED'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(DF5.EQ.0) THEN
            OUTLYNE=&
            &'FOR CIRCULAR COBS ERASE, NW5 IS NOT USED'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(ALENS(61,SURF).EQ.2.0D0) THEN
         IF(ALENS(66,SURF).NE.0.0D0) THEN
            OUTLYNE=&
            &'FOR RECTANGULAR COBS ERASE, NW5 IS NOT USED'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(ALENS(61,SURF).EQ.3.0D0) THEN
         IF(ALENS(66,SURF).NE.0.0D0) THEN
            OUTLYNE=&
            &'FOR ELLIPTICAL COBS ERASE, NW5 IS NOT USED'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(ALENS(61,SURF).EQ.5.0D0) THEN
         IF(ALENS(66,SURF).NE.0.0D0) THEN
            OUTLYNE=&
            &'FOR POLY COBS ERASE, NW5 IS NOT USED'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(ALENS(61,SURF).EQ.5.0D0) THEN
         IF(ALENS(63,SURF).LT.3.0D0.OR.ALENS(63,SURF).GT.200.0D0) THEN
            OUTLYNE=&
            &'FOR POLY COBS ERASE, NW2 MUST BE'
            CALL SHOWIT(1)
            OUTLYNE='3 OR GREATER BUT LESS THAN 201'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(ALENS(61,SURF).EQ.6.0D0) THEN
         IF(DF1.EQ.1.OR.DF2.EQ.1.OR.DF3.EQ.1.OR.DF4.EQ.1.OR.DF5.EQ.1)THEN
            OUTLYNE=&
            &'IPOLY COBS ERASE REQUIRES ALL FIVE EXPLICIT NUMERIC WORDS'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(ALENS(61,SURF).EQ.6.0D0) THEN
         IF(ALENS(62,SURF).LT.1.0D0.OR.ALENS(61,SURF).GT.99.0D0) THEN
            OUTLYNE=&
            &'FOR POLY COBS ERASE, NW1 MUST BE'
            CALL SHOWIT(1)
            OUTLYNE='1 OR GREATER AND LESS THAN OR EQUAL TO 99'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
   END IF
   OUTLYNE='1 OR GREATER AND LESS THAN OR EQUAL TO 10'
!       DEFAULT ASSIGNMENT
   IF(WQ.EQ.'RECTE'.AND.DF2.EQ.1)&
   &ALENS(63,SURF)=ALENS(62,SURF)
   IF(WQ.EQ.'ELIPE'.AND.DF2.EQ.1)&
   &ALENS(63,SURF)=ALENS(62,SURF)
   IF(WQ.EQ.'RCTKE'.AND.DF2.EQ.1)&
   &ALENS(63,SURF)=ALENS(62,SURF)
   IF(WQ.EQ.'RCTKE'.AND.DF5.EQ.1)&
   &ALENS(66,SURF)=0.0D0
   IF(WQ.EQ.'POLYE'.AND.DF5.EQ.1)&
   &ALENS(66,SURF)=0.0D0
!
!               END OF DEFAULT ASSIGNMENTS
!
!     CHECK THE SIZE OF THE RACETRACK APERTURE FOR COBS RCTK
   IF(ALENS(16,SURF).EQ.4.0D0) THEN
      IF(ALENS(21,SURF).GT.ALENS(17,SURF).OR.ALENS(21,SURF).GT.&
      &ALENS(18,SURF)) THEN
         MVAL=ALENS(17,SURF)
         IF(ALENS(18,SURF).LT.ALENS(17,SURF)) MVAL=ALENS(18,SURF)
         ALENS(21,SURF)=DABS(MVAL)
         OUTLYNE='FOR RACETRACK COBS'
         CALL SHOWIT(1)
         OUTLYNE='NW5 MAY NOT EXCEED NW1 OR NW2'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RACETRACK RADIUS SET TO ',MVAL
         CALL SHOWIT(1)
      END IF
   END IF
!     CHECK THE SIZE OF THE RACETRACK APERTURE FOR COBS RCTK
!     ERASE
   IF(ALENS(61,SURF).EQ.4.0D0) THEN
      IF(ALENS(66,SURF).GT.ALENS(62,SURF).OR.ALENS(66,SURF).GT.&
      &ALENS(63,SURF)) THEN
         MVAL=ALENS(63,SURF)
         IF(ALENS(62,SURF).LT.ALENS(63,SURF)) MVAL=ALENS(62,SURF)
         ALENS(66,SURF)=DABS(MVAL)
         OUTLYNE='FOR RACETRACK COBS ERASE'
         CALL SHOWIT(1)
         OUTLYNE='NW5 MAY NOT EXCEED NW1 OR NW2'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RACETRACK RADIUS SET TO ',MVAL
         CALL SHOWIT(1)
      END IF
   END IF
!
!       COBS TILT NOW
   IF(WC.EQ.'COBS'.AND.WQ.EQ.'TILT'.OR.&
   &WC.EQ.'COBS'.AND.WQ.EQ.'TILTE') THEN
      IF(WC.EQ.'COBS'.AND.WQ.EQ.'TILT') THEN
         IF(ALENS(16,SURF).EQ.0.0D0) THEN
!       NO COBS OR COBS ERASE EXISTS, CAN'T ASSIGN A TILT
            OUTLYNE=&
            &'NO COBS OR COBS ERASE IS ASSIGNED TO THE CURRENT SURFACE'
            CALL SHOWIT(1)
            OUTLYNE='THE "COBS TILT" COMMAND WAS IGNORED'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         ELSE
            ALENS(22,SURF)=W1
            IF(ALENS(16,SURF).EQ.1.0D0)THEN
               IF(ALENS(22,SURF).NE.0.0D0) THEN
                  OUTLYNE=&
                  &'"COBS TILT" NOT USED WITH CIRCULAR COBS'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
               END IF
            END IF
         END IF
      END IF
!       COBS TILTE NOW
      IF(WC.EQ.'COBS'.AND.WQ.EQ.'TILTE') THEN
         IF(ALENS(61,SURF).EQ.0.0D0) THEN
!       NO COBS ERASE EXISTS, CAN'T ASSIGN A TILT
            OUTLYNE=&
            &'NO COBS ERASE IS ASSIGNED TO THE CURRENT SURFACE'
            CALL SHOWIT(1)
            OUTLYNE='THE "COBS TILTE" COMMAND WAS IGNORED'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         ELSE
            ALENS(67,SURF)=W1
            IF(ALENS(61,SURF).EQ.1.0D0)THEN
               IF(ALENS(67,SURF).NE.0.0D0) THEN
                  OUTLYNE=&
                  &'"COBS TILTE" NOT USED WITH CIRCULAR COBS ERASE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
               END IF
            END IF
         END IF
      END IF
!
!       ANY REDEFINITION OF THE COBS ON SURFACE SURF
!       DELETES AN EXISTING PIKUP COBS
!
!       CHECK FOR COBS PIKUPS AND DELETE IF FOUND
!
      IF(PIKUP(1,SURF,19).EQ.0.0D0) THEN
!
!       NO COBS PIKUPS, JUST RETURN
!
         RETURN
      ELSE
      END IF
!
!       DELETE THE PIKUP
      PIKUP(1:6,SURF,19)=0.0D0
!
!
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
!
!
!       PRINT MESSAGE
      WRITE(OUTLYNE,*)'SURFACE',SURF,' : PIKUP (COBS) DELETED'
      CALL SHOWIT(1)
!
!       ARE THERE MORE PIKUPS? IF NOT SET ALENS(32,SURF) TO ZERO.
!
      PIKCNT=0
      DO 10 I=1,PSIZ
         IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
            PIKCNT=PIKCNT+1
         ELSE
         END IF
10    CONTINUE
!
      IF(PIKCNT.EQ.0) ALENS(32,SURF)=0.0D0
!
   END IF
!
   IF(WC(1:4).EQ.'CLAP'.OR.WC(1:4).EQ.'COBS') THEN
      IF(SURF.EQ.0.0D0) THEN
         WRITE(OUTLYNE,*)&
         &'APERTURES AND OBSCURATIONS MAY NOT EXIST ON SURFACE ZERO'
         WRITE(OUTLYNE,*)&
         &'NO ACTION TAKEN'
         RETURN
      END IF
   END IF
   IF(WC.EQ.'CLAP') THEN
!
      IF(WQ.EQ.DD) ALENS(9,SURF)=1.0D0
      IF(WQ.EQ.'RECT ') ALENS(9,SURF)=2.0D0
      IF(WQ.EQ.'ELIP ') ALENS(9,SURF)=3.0D0
      IF(WQ.EQ.'RCTK ') ALENS(9,SURF)=4.0D0
      IF(WQ.EQ.'POLY ') ALENS(9,SURF)=5.0D0
      IF(WQ.EQ.'IPOLY') ALENS(9,SURF)=6.0D0
      IF(WQ.EQ.'ERASE') ALENS(51,SURF)=1.0D0
      IF(WQ.EQ.'RECTE') ALENS(51,SURF)=2.0D0
      IF(WQ.EQ.'ELIPE') ALENS(51,SURF)=3.0D0
      IF(WQ.EQ.'RCTKE') ALENS(51,SURF)=4.0D0
      IF(WQ.EQ.'POLYE') ALENS(51,SURF)=5.0D0
      IF(WQ.EQ.'IPOLYE') ALENS(51,SURF)=6.0D0
      IF(WQ.NE.'TILT'.AND.WQ.NE.'TILTE') THEN
         IF(DF1.EQ.1) W1=0.0D0
         IF(DF2.EQ.1) W2=0.0D0
         IF(DF3.EQ.1) W3=0.0D0
         IF(DF4.EQ.1) W4=0.0D0
         IF(DF5.EQ.1) W5=0.0D0
         IF(WQ.EQ.DD.AND.WC.EQ.'CLAP') THEN
            ALENS(10,SURF)=DABS(W1)
            IF(DF4.EQ.0) THEN
               IF(W4.EQ.0.0D0) W4=W1
               ALENS(11,SURF)=DABS(W4)
            END IF
            IF(DF4.EQ.1) ALENS(11,SURF)=DABS(W1)
            IF(DF4.EQ.1) W4=W1
            ALENS(11,SURF)=DABS(W4)
            ALENS(12,SURF)=W2
            ALENS(13,SURF)=W3
            ALENS(14,SURF)=W5
            ALENS(15,SURF)=0.0D0
         END IF
         IF(WQ.EQ.'RECT'.OR.WQ.EQ.'ELIP'.OR.WQ.EQ.'RCTK'&
         &.OR.WQ.EQ.'POLY'.OR.WQ.EQ.'IPOLY') THEN
            ALENS(10,SURF)=DABS(W1)
            ALENS(11,SURF)=DABS(W2)
            ALENS(12,SURF)=W3
            ALENS(13,SURF)=W4
            ALENS(14,SURF)=DABS(W5)
            ALENS(15,SURF)=0.0D0
         END IF
         IF(WQ.EQ.'ERASE'.AND.WC.EQ.'CLAP') THEN
            ALENS(52,SURF)=DABS(W1)
            ALENS(53,SURF)=0.0D0
            ALENS(54,SURF)=W2
            ALENS(55,SURF)=W3
            ALENS(56,SURF)=W5
            ALENS(57,SURF)=0.0D0
         END IF
         IF(WQ.EQ.'RECTE'.OR.WQ.EQ.'ELIPE'.OR.WQ.EQ.'RCTKE'&
         &.OR.WQ.EQ.'POLYE'.OR.WQ.EQ.'IPOLYE') THEN
            ALENS(52,SURF)=DABS(W1)
            ALENS(53,SURF)=DABS(W2)
            ALENS(54,SURF)=W3
            ALENS(55,SURF)=W4
            ALENS(56,SURF)=DABS(W5)
            ALENS(57,SURF)=0.0D0
         END IF
         IF(DF4.EQ.0.AND.WQ.EQ.DD.AND.WC.EQ.'CLAP') THEN
            IF(DABS(W4).GT.DABS(W1)) ALENS(11,SURF)=DABS(W1)
         END IF
         IF(ALENS(9,SURF).EQ.1.0D0)THEN
!       REMOVE EXISTING CLAP TILT
            IF(ALENS(15,SURF).NE.0.0D0) THEN
               OUTLYNE='EXISTING CLAP TILT REMOVED'
               CALL SHOWIT(1)
               ALENS(15,SURF)=0.0D0
            END IF
         END IF
         IF(ALENS(9,SURF).EQ.2.0D0)THEN
            IF(ALENS(14,SURF).NE.0.0D0) THEN
               OUTLYNE='FOR RECTANGULAR CLAP, NW5 NOT USED'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
         END IF
         IF(ALENS(9,SURF).EQ.3.0D0)THEN
            IF(ALENS(14,SURF).NE.0.0D0) THEN
               OUTLYNE='FOR ELLIPTICAL CLAP, NW5 NOT USED'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
         END IF
         IF(ALENS(9,SURF).EQ.5.0D0)THEN
            IF(ALENS(14,SURF).NE.0.0D0) THEN
               OUTLYNE='FOR POLY CLAP, NW5 NOT USED'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
         END IF
         IF(ALENS(9,SURF).EQ.5.0D0)THEN
            IF(ALENS(11,SURF).LT.3.0D0.OR.ALENS(11,SURF).GT.200.0D0) THEN
               OUTLYNE='FOR POLY CLAP, NW2 MUST BE'
               CALL SHOWIT(1)
               OUTLYNE='3 OR GREATER BUT LESS THAN 201'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
         END IF
         IF(ALENS(9,SURF).EQ.6.0D0)THEN
            IF(DF1.EQ.1.OR.DF2.EQ.1.OR.DF3.EQ.1.OR.DF4.EQ.1.OR.DF5.EQ.1)THEN
               OUTLYNE=&
               &'IPOLY CLAP REQUIRES ALL FIVE EXPLICIT NUMERIC WORDS'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
         END IF
         IF(ALENS(9,SURF).EQ.6.0D0)THEN
            IF(ALENS(10,SURF).LT.1.0D0.OR.ALENS(10,SURF).GT.99.0D0) THEN
               OUTLYNE='FOR IPOLY CLAP, NW1 MUST BE'
               CALL SHOWIT(1)
               OUTLYNE='1 OR GREATER BUT LESS THAN OR EQUAL TO 99'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
         END IF
      END IF
!       DEFAULT ASSIGNMENT
      IF(WQ.EQ.'RECT'.AND.DF2.EQ.1)&
      &ALENS(11,SURF)=ALENS(10,SURF)
      IF(WQ.EQ.'ELIP'.AND.DF2.EQ.1)&
      &ALENS(11,SURF)=ALENS(10,SURF)
      IF(WQ.EQ.'RCTK'.AND.DF2.EQ.1)&
      &ALENS(11,SURF)=ALENS(10,SURF)
      IF(WQ.EQ.'RCTK'.AND.DF5.EQ.1)&
      &ALENS(14,SURF)=0.0D0
      IF(WQ.EQ.'POLY'.AND.DF5.EQ.1)&
      &ALENS(14,SURF)=0.0D0
!               END OF DEFAULT ASSIGNMENTS
!     ERASES
      IF(ALENS(51,SURF).EQ.1.0D0)THEN
!       REMOVE EXISTING CLAP TILTE
         IF(ALENS(57,SURF).NE.0.0D0) THEN
            OUTLYNE='EXISTING CLAP TILTE REMOVED'
            CALL SHOWIT(1)
            ALENS(57,SURF)=0.0D0
         END IF
         IF(DF4.EQ.0) THEN
            OUTLYNE='FOR CIRCULAR CLAP ERASE, NW4 NOT USED'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(DF5.EQ.0) THEN
            OUTLYNE='FOR CIRCULAR CLAP ERASE, NW5 NOT USED'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(ALENS(51,SURF).EQ.2.0D0)THEN
         IF(ALENS(56,SURF).NE.0.0D0) THEN
            OUTLYNE='FOR RECTANGULAR CLAP ERASE, NW5 NOT USED'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(ALENS(51,SURF).EQ.3.0D0)THEN
         IF(ALENS(56,SURF).NE.0.0D0) THEN
            OUTLYNE='FOR ELLIPTICAL CLAP ERASE, NW5 NOT USED'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(ALENS(51,SURF).EQ.5.0D0)THEN
         IF(ALENS(56,SURF).NE.0.0D0) THEN
            OUTLYNE='FOR POLY CLAP ERASE, NW5 NOT USED'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(ALENS(51,SURF).EQ.5.0D0)THEN
         IF(ALENS(53,SURF).NE.0.0D0) THEN
            OUTLYNE='FOR POLY CLAP ERASE, NW2 MUST BE'
            CALL SHOWIT(1)
            OUTLYNE='3 OR GREATER BUT LESS THAN 201'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(ALENS(51,SURF).EQ.6.0D0)THEN
         IF(DF1.EQ.1.OR.DF2.EQ.1.OR.DF3.EQ.1.OR.DF4.EQ.1.OR.DF5.EQ.1)THEN
            OUTLYNE=&
            &'IPOLY CLAP ERASE REQUIRES ALL FIVE EXPLICIT NUMERIC WORDS'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(ALENS(51,SURF).EQ.6.0D0)THEN
         IF(ALENS(52,SURF).LT.1.0D0.OR.ALENS(52,SURF).GT.99.0D0) THEN
            OUTLYNE='FOR IPOLY CLAP ERASE, NW1 MUST BE'
            CALL SHOWIT(1)
            OUTLYNE='1 OR GREATER AND LESS THAN OR EQUAL TO 99'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
   END IF
!       DEFAULT ASSIGNMENT
   IF(WQ.EQ.'RECTE'.AND.DF2.EQ.1)&
   &ALENS(53,SURF)=ALENS(52,SURF)
   IF(WQ.EQ.'ELIPE'.AND.DF2.EQ.1)&
   &ALENS(53,SURF)=ALENS(52,SURF)
   IF(WQ.EQ.'RCTKE'.AND.DF2.EQ.1)&
   &ALENS(53,SURF)=ALENS(52,SURF)
   IF(WQ.EQ.'RCTKE'.AND.DF5.EQ.1)&
   &ALENS(56,SURF)=0.0D0
   IF(WQ.EQ.'POLYE'.AND.DF5.EQ.1)&
   &ALENS(56,SURF)=0.0D0
!               END OF DEFAULT ASSIGNMENTS
!
!     CHECK THE SIZE OF THE RACETRACK APERTURE FOR CLAP RCTK
   IF(ALENS(9,SURF).EQ.4.0D0) THEN
      IF(ALENS(14,SURF).GT.ALENS(10,SURF).OR.ALENS(14,SURF).GT.&
      &ALENS(11,SURF)) THEN
         MVAL=ALENS(10,SURF)
         IF(ALENS(11,SURF).LT.ALENS(10,SURF)) MVAL=ALENS(11,SURF)
         ALENS(14,SURF)=DABS(MVAL)
         OUTLYNE='FOR RACETRACK CLAP'
         CALL SHOWIT(1)
         OUTLYNE='NW5 MAY NOT EXCEED NW1 OR NW2'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RACETRACK RADIUS SET TO ',MVAL
         CALL SHOWIT(1)
      END IF
   END IF
!     ERASE
   IF(ALENS(51,SURF).EQ.4.0D0) THEN
      IF(ALENS(56,SURF).GT.ALENS(52,SURF).OR.ALENS(56,SURF).GT.&
      &ALENS(53,SURF)) THEN
         MVAL=ALENS(52,SURF)
         IF(ALENS(53,SURF).LT.ALENS(52,SURF)) MVAL=ALENS(53,SURF)
         ALENS(56,SURF)=DABS(MVAL)
         OUTLYNE='FOR RACETRACK CLAP ERASE'
         CALL SHOWIT(1)
         OUTLYNE='NW5 MAY NOT EXCEED NW1 OR NW2'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RACETRACK RADIUS SET TO ',MVAL
         CALL SHOWIT(1)
      END IF
   END IF
!
!       NOW CLAP TILT
   IF(WC.EQ.'CLAP'.AND.WQ.EQ.'TILT'.OR.&
   &WC.EQ.'CLAP'.AND.WQ.EQ.'TILTE') THEN
      IF(WC.EQ.'CLAP'.AND.WQ.EQ.'TILT') THEN
         IF(ALENS(9,SURF).EQ.0.0D0) THEN
!       NO CLAP OR CLAP ERASE, NO TILT WAS ASSIGNED
            OUTLYNE=&
            &'NO CLAP IS ASSIGNED TO THE CURRENT SURFACE'
            CALL SHOWIT(1)
            OUTLYNE='THE "CLAP TILT" COMMAND WAS IGNORED'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         ELSE
            ALENS(15,SURF)=W1
            IF(ALENS(9,SURF).EQ.1.0D0) THEN
               IF(ALENS(15,SURF).NE.0.0D0) THEN
                  OUTLYNE=&
                  &'CLAP TILT NOT USED WITH CIRCULAR CLAP'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
               END IF
            END IF
         END IF
      END IF
!       NOW CLAP TILTE
      IF(WC.EQ.'CLAP'.AND.WQ.EQ.'TILTE') THEN
         IF(ALENS(51,SURF).EQ.0.0D0) THEN
!       NO CLAP ERASE, NO TILTE WAS ASSIGNED
            OUTLYNE=&
            &'NO CLAP ERASE IS ASSIGNED TO THE CURRENT SURFACE'
            CALL SHOWIT(1)
            OUTLYNE='THE "CLAP TILTE" COMMAND WAS IGNORED'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         ELSE
            ALENS(57,SURF)=W1
            IF(ALENS(51,SURF).EQ.1.0D0) THEN
               IF(ALENS(57,SURF).NE.0.0D0) THEN
                  OUTLYNE=&
                  &'CLAP TILTE NOT USED WITH CIRCULAR CLAP ERASE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
               END IF
            END IF
         END IF
      END IF
!
!       ANY REDEFINITION OF THE CLAP ON SURFACE SURF
!       DELETES AN EXISTING PIKUP CLAP
!
!       CHECK FOR CLAP PIKUPS AND DELETE IF FOUND
!
!
      IF(PIKUP(1,SURF,18).EQ.0.0D0) THEN
!
!       NO CLAP PIKUPS, JUST RETURN
!
         RETURN
      ELSE
      END IF
!
!       DELETE THE PIKUP
      PIKUP(1:6,SURF,18)=0.0D0
!
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' : PIKUP (CLAP) DELETED'
      CALL SHOWIT(1)
!
!       ARE THERE MORE PIKUPS? IF NOT SET ALENS(32,SURF) TO ZERO.
!
      PIKCNT=0
      DO 101 I=1,PSIZ
         IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
            PIKCNT=PIKCNT+1
         ELSE
         END IF
101   CONTINUE
!
      IF(PIKCNT.EQ.0) ALENS(32,SURF)=0.0D0
   END IF
   RETURN
END
! SUB MULT_CLAP.FOR
SUBROUTINE MULT_CLAP
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE MULTCLAP WHICH IMPLEMENTS THE MULTCLAP
!       COMMAND AT THE LENS INPUT AND UPDATE LENS LEVEL.
!
!
   INTEGER N,I,J
   REAL*8 X,Y,GAM
   IF(SST.EQ.1) THEN
      OUTLYNE=&
      &'"MULTCLAP" TAKES NOSTRING INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.1.AND.WQ.NE.'DELETE') THEN
      OUTLYNE=&
      &'"MULTCLAP" ONLY TAKES "DELETE" AS A VALID QUALIFIER WORD'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.1.AND.SN.EQ.1) THEN
      OUTLYNE=&
      &'"MULTCLAP" ONLY TAKES QUALIFIER OR NUMERIC INPUT'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'BUT NOT BOTH'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.1.AND.WQ.EQ.'DELETE') THEN
      ALENS(127,SURF)=0.0D0
      MULTCLAP(1:1000,1:3,SURF)=0.0D0
      WRITE(OUTLYNE,*)&
      &'DELETEING ANY EXISTING MULTIPLE APERTURE DEFINITIONS'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)&
      &'FROM SURFACE # ',SURF
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(DF5.EQ.0) THEN
      OUTLYNE=&
      &'"MULTCLAP" TAKES NO NUMERIC WORD #5 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(DF1.EQ.1.OR.DF2.EQ.1.OR.DF3.EQ.1) THEN
      OUTLYNE=&
      &'"MULTCLAP" REQUIRES EXPLICIT NUMERIC WORD #1 THROUGH #3 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(DF4.EQ.1) W4=0.0D0
   IF(INT(W1).LT.0.OR.INT(W1).GT.1000)&
   &THEN
      WRITE(OUTLYNE,*)&
      &'"MULTCLAP" REQUIRES NUMERIC WORD #1 TO BE FROM 1 TO 1000'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SURF.EQ.INT(SYSTEM(25))) THEN
      OUTLYNE=&
      &'"MULTCLAP" MAY NOT BE ASSIGNED TO THE REFERENCE SURFACE'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SURF.EQ.INT(SYSTEM(26))) THEN
      OUTLYNE=&
      &'"MULTCLAP" MAY NOT BE ASSIGNED TO THE ASTOP SURFACE'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(ALENS(9,SURF).EQ.0.0D0) THEN
      OUTLYNE=&
      &'"MULTCLAP" REQUIRES A PRE-EXISTING "CLAP" TO BE ASSIGNED'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   N=INT(W1)
   X=W2
   Y=W3
   GAM=W4
!     SET THE ARRAY VALUE
   MULTCLAP(N,1,SURF)=X
   MULTCLAP(N,2,SURF)=Y
   MULTCLAP(N,3,SURF)=GAM
   IF(INT(ALENS(127,SURF)).EQ.0) ALENS(127,SURF)=DBLE(N)
   IF(N.GT.INT(ALENS(127,SURF))) ALENS(127,SURF)=DBLE(N)
   RETURN
END
! SUB MULT_COBS.FOR
SUBROUTINE MULT_COBS
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE MULTCOBS WHICH IMPLEMENTS THE MULTCOBS
!       COMMAND AT THE LENS INPUT AND UPDATE LENS LEVEL.
!
!
   INTEGER N,I,J
   REAL*8 X,Y,GAM
   IF(SST.EQ.1) THEN
      OUTLYNE=&
      &'"MULTCOBS" TAKES NOSTRING INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.1.AND.WQ.NE.'DELETE') THEN
      OUTLYNE=&
      &'"MULTCOBS" ONLY TAKES "DELETE" AS A VALID QUALIFIER WORD'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.1.AND.SN.EQ.1) THEN
      OUTLYNE=&
      &'"MULTCOBS" ONLY TAKES QUALIFIER OR NUMERIC INPUT'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'BUT NOT BOTH'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.1.AND.WQ.EQ.'DELETE') THEN
      ALENS(128,SURF)=0.0D0
      ALENS(134:137,SURF)=0.0D0
      MULTCOBS(1:1000,1:3,SURF)=0.0D0
      WRITE(OUTLYNE,*)&
      &'DELETEING ANY EXISTING MULTIPLE APERTURE DEFINITIONS'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)&
      &'FROM SURFACE # ',SURF
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(DF5.EQ.0) THEN
      OUTLYNE=&
      &'"MULTCOBS" TAKES NO NUMERIC WORD #5 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(DF1.EQ.1.OR.DF2.EQ.1.OR.DF3.EQ.1) THEN
      OUTLYNE=&
      &'"MULTCOBS" REQUIRES EXPLICIT NUMERIC WORD #1 THROUGH #3 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(ALENS(16,SURF).EQ.0.0D0) THEN
      OUTLYNE=&
      &'"MULTCLAP" REQUIRES A PRE-EXISTING "COBS" TO BE ASSIGNED'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(DF4.EQ.1) W4=0.0D0
   IF(INT(W1).LT.0.OR.INT(W1).GT.1000)&
   &THEN
      WRITE(OUTLYNE,*)&
      &'"MULTCOBS" REQUIRES NUMERIC WORD #1 TO BE FROM 1 TO 1000'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   N=INT(W1)
   X=W2
   Y=W3
   GAM=W4
!     SET THE ARRAY VALUE
   MULTCOBS(N,1,SURF)=X
   MULTCOBS(N,2,SURF)=Y
   MULTCOBS(N,3,SURF)=GAM
   IF(INT(ALENS(128,SURF)).EQ.0) ALENS(128,SURF)=DBLE(N)
   IF(N.GT.INT(ALENS(128,SURF))) ALENS(128,SURF)=DBLE(N)
   RETURN
END
! SUB SSPIDER.FOR
SUBROUTINE SSPIDER
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SSPIDER WHICH IMPLEMENTS THE SPIDER
!       COMMAND AT THE LENS INPUT AND UPDATE LENS LEVEL.
!
!
   INTEGER N,I,J
   REAL*8 W,L,X,Y,GAM,DTHETA,THETA,RAD
   IF(SST.EQ.1) THEN
      OUTLYNE=&
      &'"SPIDER" TAKES NOSTRING INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.1.AND.WQ.NE.'DELETE') THEN
      OUTLYNE=&
      &'"SPIDER" ONLY TAKES "DELETE" AS A VALID QUALIFIER WORD'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.1.AND.SN.EQ.1) THEN
      OUTLYNE=&
      &'"SPIDER" ONLY TAKES QUALIFIER OR NUMERIC INPUT'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'BUT NOT BOTH'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.1.AND.WQ.EQ.'DELETE') THEN
      ALENS(128,SURF)=0.0D0
      ALENS(16,SURF)=0.0D0
      ALENS(134:137,SURF)=0.0D0
      MULTCOBS(1:1000,1:3,SURF)=0.0D0
      WRITE(OUTLYNE,*)&
      &'DELETEING ANY EXISTING SPIDER DEFINITION'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)&
      &'FROM SURFACE # ',SURF
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(DF4.EQ.0.OR.DF5.EQ.0) THEN
      OUTLYNE=&
      &'"SPIDER" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(DF1.EQ.1.OR.DF2.EQ.1.OR.DF3.EQ.1) THEN
      OUTLYNE=&
      &'"SPIDER" REQUIRES EXPLICIT NUMERIC WORD #1 THROUGH #3 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(INT(W1).LT.1.OR.INT(W1).GT.1000)&
   &THEN
      WRITE(OUTLYNE,*)&
      &'"SPIDER" REQUIRES NUMERIC WORD #1 TO BE GREATER THAN OR'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)&
      &'"EQUAL TO 1 AND LESS THAN OR EQUAL TO 1000'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(W2.LE.0.0D0.OR.W3.LE.0.0D0)&
   &THEN
      WRITE(OUTLYNE,*)&
      &'"W" AND "L" MUST BE GREATER THAN ZERO'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   N=INT(W1)
   W=W2
   L=W3
!     SET THE ARRAY VALUE
   ALENS(134,SURF)=1.0D0
   ALENS(135,SURF)=DBLE(N)
   ALENS(136,SURF)=W2
   ALENS(137,SURF)=W3
   ALENS(16,SURF)=2.0D0
   ALENS(17,SURF)=L/2.0D0
   ALENS(18,SURF)=W/2.0D0
   ALENS(19:22,SURF)=0.0D0
   ALENS(128,SURF)=DBLE(N)
   THETA=0.0D0
   DTHETA=(TWOPII)/DBLE(N)
   RAD=L/2.0D0
   DO I=1,N
      X=RAD*DCOS(THETA)
      Y=RAD*DSIN(THETA)
      MULTCOBS(I,1,SURF)=X
      MULTCOBS(I,2,SURF)=Y
      MULTCOBS(I,3,SURF)=(THETA*180.0D0/PII)-90.0D0
      THETA=THETA+DTHETA
   END DO
   RETURN
END
! SUB SANGLE.FOR
SUBROUTINE SANGLE
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SANGLE WHICH IMPLEMENTS THE ALPHA
!       BETA AND GAMMA COMMAND AT THE LENS UPDATE LEVEL.
!
   INTEGER PIKCNT,I,CT
!
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SST.EQ.1) THEN
      IF(WC.EQ.'ALPHA')&
      &OUTLYNE='"ALPHA" TAKES NO STRING INPUT'
      IF(WC.EQ.'BETA')&
      &OUTLYNE='"BETA" TAKES NO STRING INPUT'
      IF(WC.EQ.'GAMMA')&
      &OUTLYNE='"GAMMA" TAKES NO STRING INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      IF(WC.EQ.'ALPHA')&
      &OUTLYNE='"ALPHA" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
      IF(WC.EQ.'BETA')&
      &OUTLYNE='"BETA" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
      IF(WC.EQ.'GAMMA')&
      &OUTLYNE='"GAMMA" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.1.AND.F5.EQ.1) THEN
      IF(WC.EQ.'ALPHA')&
      &OUTLYNE='"ALPHA" TAKES NO QUALIFIER IN LENS INPUT MODE'
      IF(WC.EQ.'BETA')&
      &OUTLYNE='"BETA" TAKES NO QUALIFIER IN LENS INPUT MODE'
      IF(WC.EQ.'GAMMA')&
      &OUTLYNE='"GAMMA" TAKES NO QUALIFIER IN LENS INPUT MODE'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(F6.EQ.1) THEN
!
      IF(WQ.NE.'        '.AND.WQ.NE.'DELT'.AND.WQ.NE.'CENT') THEN
         IF(WC.EQ.'ALPHA')&
         &OUTLYNE='INVALID QUALIFIER USED WITH "ALPHA"'
         IF(WC.EQ.'BETA')&
         &OUTLYNE='INVALID QUALIFIER USED WITH "BETA"'
         IF(WC.EQ.'GAMMA')&
         &OUTLYNE='INVALID QUALIFIER USED WITH "GAMMA"'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   ELSE
   END IF
   IF(DF1.EQ.1) THEN
      IF(WC.EQ.'ALPHA')&
      &OUTLYNE='"ALPHA" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
      IF(WC.EQ.'BETA')&
      &OUTLYNE='"BETA" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
      IF(WC.EQ.'GAMMA')&
      &OUTLYNE='"GAMMA" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
   IF(SURF.EQ.0) THEN
      OUTLYNE='OBJECT SURFACE CAN NOT BE TILTED'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
   IF(F6.EQ.1.OR.F5.EQ.1) THEN
      IF(ALENS(25,SURF).EQ.0.0D0) THEN
!       THE SURFACE WAS NOT PERVIOUSLY DEFINED AS TILTED
!       DEFINE IT AS SUCH AND PRINT A MESSAGE
         ALENS(25,SURF)=1.0D0
         ALENS(26:28,SURF)=0.0D0
         ALENS(118:120,SURF)=0.0D0
      END IF
      IF(ALENS(25,SURF).NE.0.0D0) THEN
         IF(WC.EQ.'ALPHA') THEN
            IF(SQ.EQ.0) THEN
               ALENS(26,SURF)=W1
               ALENS(118,SURF)=W1
            END IF
            IF(WQ.EQ.'DELT') THEN
               ALENS(26,SURF)=ALENS(26,SURF)+W1
               ALENS(118,SURF)=ALENS(118,SURF)+W1
            END IF
            IF(WQ.EQ.'CENT') THEN
               ALENS(26,SURF)=ALENS(26,SURF)+(W1*0.01D0*ALENS(26,SURF))
               ALENS(118,SURF)=ALENS(118,SURF)+(W1*0.01D0*ALENS(118,SURF))
            END IF
            CT=15
         ELSE
         END IF
         IF(WC.EQ.'BETA') THEN
            IF(SQ.EQ.0) THEN
               ALENS(27,SURF)=W1
               ALENS(119,SURF)=W1
            END IF
            IF(WQ.EQ.'DELT') THEN
               ALENS(27,SURF)=ALENS(27,SURF)+W1
               ALENS(119,SURF)=ALENS(119,SURF)+W1
            END IF
            IF(WQ.EQ.'CENT') THEN
               ALENS(27,SURF)=ALENS(27,SURF)+(W1*0.01D0*ALENS(27,SURF))
               ALENS(119,SURF)=ALENS(119,SURF)+(W1*0.01D0*ALENS(119,SURF))
            END IF
            CT=16
         ELSE
         END IF
         IF(WC.EQ.'GAMMA') THEN
            IF(SQ.EQ.0)ALENS(28,SURF)=W1
            IF(SQ.EQ.0)ALENS(120,SURF)=W1
            IF(WQ.EQ.'DELT') ALENS(28,SURF)=ALENS(28,SURF)+W1
            IF(WQ.EQ.'DELT') ALENS(120,SURF)=ALENS(28,SURF)+W1
            IF(WQ.EQ.'CENT')&
            &ALENS(28,SURF)=ALENS(28,SURF)+(W1*0.01D0*ALENS(28,SURF))
            IF(WQ.EQ.'CENT')&
            &ALENS(120,SURF)=ALENS(120,SURF)+(W1*0.01D0*ALENS(120,SURF))
            CT=17
         ELSE
         END IF
!       CHECK FOR PIKUP AND DELETE THEN RESOLVE ALENS(32,SURF)
!
         IF(PIKUP(1,SURF,CT).EQ.1.0D0) THEN
            PIKUP(1:6,SURF,CT)=0.0D0
            ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
            IF(WC.EQ.'ALPHA')&
            &WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (ALPHA) DELETED'
            IF(WC.EQ.'BETA')&
            &WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (BETA) DELETED'
            IF(WC.EQ.'GAMMA')&
            &WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GAMMA) DELETED'
            CALL SHOWIT(1)
         ELSE
         END IF
         PIKCNT=0
         DO 10 I=1,PSIZ
            IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
               PIKCNT=PIKCNT+1
            ELSE
            END IF
10       CONTINUE
         IF(PIKCNT.EQ.0) ALENS(32,SURF)=0.0D0
!
      ELSE
      END IF
!
!       IF THE SURFACE HAS A TILT AUTO THEN REMOVE THE
!       AUTO AND PRINT MESSAGE
!
      IF(ALENS(25,SURF).EQ.2.0D0.OR.ALENS(25,SURF).EQ.&
      &3.0D0) THEN
         ALENS(25,SURF)=1.0D0
         IF(ALENS(25,SURF).EQ.2.0D0) OUTLYNE=&
         &'"AUTO" ADJUST REMOVED FROM SURFACE TILT DEFINITION'
         IF(ALENS(25,SURF).EQ.3.0D0) OUTLYNE=&
         &'"AUTOM" ADJUST REMOVED FROM SURFACE TILT DEFINITION'
         IF(ALENS(25,SURF).EQ.2.0D0) CALL SHOWIT(1)
         IF(ALENS(25,SURF).EQ.3.0D0) CALL SHOWIT(1)
      END IF
!
!       NOW REMOVE PIKUPS
   END IF
!
   RETURN
END
! SUB SGANGLE.FOR
SUBROUTINE SGANGLE
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SGANGLE WHICH IMPLEMENTS THE GALPHA
!       GBETA AND GGAMMA COMMAND AT THE LENS UPDATE LEVEL.
!
   INTEGER PIKCNT,I,CT
!
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SST.EQ.1) THEN
      IF(WC.EQ.'GALPHA')&
      &OUTLYNE='"GALPHA" TAKES NO STRING INPUT'
      IF(WC.EQ.'GBETA')&
      &OUTLYNE='"GBETA" TAKES NO STRING INPUT'
      IF(WC.EQ.'GGAMMA')&
      &OUTLYNE='"GGAMMA" TAKES NO STRING INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      IF(WC.EQ.'GALPHA')&
      &OUTLYNE='"GALPHA" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
      IF(WC.EQ.'GBETA')&
      &OUTLYNE='"GBETA" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
      IF(WC.EQ.'GGAMMA')&
      &OUTLYNE='"GGAMMA" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.1.AND.F5.EQ.1) THEN
      IF(WC.EQ.'GALPHA')&
      &OUTLYNE='"GALPHA" TAKES NO QUALIFIER IN LENS INPUT MODE'
      IF(WC.EQ.'GBETA')&
      &OUTLYNE='"GBETA" TAKES NO QUALIFIER IN LENS INPUT MODE'
      IF(WC.EQ.'GGAMMA')&
      &OUTLYNE='"GGAMMA" TAKES NO QUALIFIER IN LENS INPUT MODE'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(F6.EQ.1) THEN
      IF(WQ.NE.'        '.AND.WQ.NE.'DELT'.AND.WQ.NE.'CENT') THEN
         IF(WC.EQ.'GALPHA')&
         &OUTLYNE='INVALID QUALIFIER USED WITH "GALPHA"'
         IF(WC.EQ.'GBETA')&
         &OUTLYNE='INVALID QUALIFIER USED WITH "GBETA"'
         IF(WC.EQ.'GGAMMA')&
         &OUTLYNE='INVALID QUALIFIER USED WITH "GGAMMA"'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   ELSE
   END IF
   IF(DF1.EQ.1) THEN
      IF(WC.EQ.'GALPHA')&
      &OUTLYNE='"GALPHA" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
      IF(WC.EQ.'GBETA')&
      &OUTLYNE='"GBETA" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
      IF(WC.EQ.'GGAMMA')&
      &OUTLYNE='"GGAMMA" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
   IF(SURF.EQ.0) THEN
      OUTLYNE='OBJECT SURFACE CAN NOT BE TILTED'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
   IF(ALENS(25,SURF).EQ.6.0D0.OR.ALENS(25,SURF).EQ.1.0D0 &
   &.AND.ALENS(77,SURF).EQ.1.0D0) THEN
   ELSE
      IF(WC.EQ.'GALPHA')&
      &OUTLYNE='"GALPHA" REQUIRES A "TILT RET" ON THE SURFACE'
      IF(WC.EQ.'GBETA')&
      &OUTLYNE='"GBETA" REQUIRES A "TILT RET" ON THE SURFACE'
      IF(WC.EQ.'GGAMMA')&
      &OUTLYNE='"GGAMMA" REQUIRES A "TILT RET" ON THE SURFACE'
      CALL SHOWIT(1)
      OUTLYNE='NO-ACTION TAKEN'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(WC.EQ.'GALPHA') THEN
      IF(SQ.EQ.0) THEN
         ALENS(93,SURF)=W1
      END IF
      IF(WQ.EQ.'DELT') THEN
         ALENS(93,SURF)=ALENS(93,SURF)+W1
      END IF
      IF(WQ.EQ.'CENT') THEN
         ALENS(93,SURF)=ALENS(93,SURF)+(W1*0.01D0*ALENS(93,SURF))
         ALENS(93,SURF)=ALENS(93,SURF)+(W1*0.01D0*ALENS(93,SURF))
      END IF
      CT=40
   ELSE
   END IF
   IF(WC.EQ.'GBETA') THEN
      IF(SQ.EQ.0) THEN
         ALENS(94,SURF)=W1
      END IF
      IF(WQ.EQ.'DELT') THEN
         ALENS(94,SURF)=ALENS(94,SURF)+W1
      END IF
      IF(WQ.EQ.'CENT') THEN
         ALENS(94,SURF)=ALENS(94,SURF)+(W1*0.01D0*ALENS(94,SURF))
      END IF
      CT=41
   ELSE
   END IF
   IF(WC.EQ.'GGAMMA') THEN
      IF(SQ.EQ.0) ALENS(95,SURF)=W1
      IF(WQ.EQ.'DELT') ALENS(95,SURF)=ALENS(95,SURF)+W1
      IF(WQ.EQ.'CENT')&
      &ALENS(95,SURF)=ALENS(95,SURF)+(W1*0.01D0*ALENS(95,SURF))
      CT=42
   ELSE
   END IF
!       CHECK FOR PIKUP AND DELETE THEN RESOLVE ALENS(32,SURF)
!
   IF(PIKUP(1,SURF,CT).EQ.1.0D0) THEN
      PIKUP(1:6,SURF,CT)=0.0D0
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
      IF(WC.EQ.'GALPHA')&
      &WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GALPHA) DELETED'
      IF(WC.EQ.'GBETA')&
      &WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GBETA) DELETED'
      IF(WC.EQ.'GGAMMA')&
      &WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GGAMMA) DELETED'
      CALL SHOWIT(1)
      PIKCNT=0
      DO 10 I=1,PSIZ
         IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
            PIKCNT=PIKCNT+1
         ELSE
         END IF
10    CONTINUE
      IF(PIKCNT.EQ.0) ALENS(32,SURF)=0.0D0
!
   ELSE
   END IF
!
   RETURN
END
! SUB FICTCHG.FOR
SUBROUTINE FICTCHG
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!     DOES INDEX AND VNUM AND DPART COMMANDS FOR FICT GLASS TYPE
!
   INTEGER PIKCNT,I
!
!
!               CHECK FOR STRING OR QUALIFIER
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SST.EQ.1) THEN
      OUTLYNE=&
      &'"INDEX", "VNUM" AND "DPART" COMMANDS TAKE NO STRING INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(S2.EQ.1.OR.S3.EQ.1.OR.&
   &S4.EQ.1.OR.S5.EQ.1) THEN
      OUTLYNE=&
      &'"INDEX", "VNUM" AND "DPART" COMMANDS TAKE NO'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'NUMERIC WORD #2 THROUGH #5 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.1.AND.F5.EQ.1) THEN
      OUTLYNE=&
      &'"INDEX", "VNUM" AND "DPART" COMMANDS TAKE NO'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'QUALIFIER WORD IN LENS INPUT MODE'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(F6.EQ.1) THEN
      IF(WQ.NE.'        '.AND.WQ.NE.'CENT'.AND.WQ.NE.'DELT') THEN
         OUTLYNE=&
         &'INVALID QUALIFIER WORD USED WITH "INDEX", "VNUM" OR "DPART"'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(DF1.EQ.1) THEN
      OUTLYNE=&
      &'"INDEX", "VNUM" AND "DPART" COMMANDS REQUIRE EXPLICIT'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'NUMERIC WORD #1 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF

   IF(F5.EQ.1.AND.(SURF-1).LT.0) THEN
      OUTLYNE=&
      &'"INDEX", "VNUM" AND "DPART" COMMANDS'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'WORK ON THE PREVIOUS SURFACE IN LENS'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'INPUT MODE BUT THERE ARE NO SURFACES AS YET'
      CALL SHOWIT(1)
      OUTLYNE='NO ACTION TAKEN'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(F6.EQ.1) THEN
      IF(GLANAM(SURF,1).NE.'MODEL') THEN
         GLANAM(SURF,1)='MODEL'
         OUTLYNE=&
         &'GLASS CATALOG NAME CHANGED TO "MODEL"'
         CALL SHOWIT(1)
      ELSE
!     UPDATE LENS LEVEL
         IF(SQ.EQ.0) THEN
            IF(WC.EQ.'INDEX   ')  ALENS(86,SURF)=W1
            IF(WC.EQ.'VNUM    ')  ALENS(87,SURF)=W1
            IF(WC.EQ.'DPART   ')  ALENS(89,SURF)=W1
         END IF
         IF(WQ.EQ.'DELT') THEN
            IF(WC.EQ.'INDEX   ')  ALENS(86,SURF)=ALENS(86,SURF)+W1
            IF(WC.EQ.'VNUM    ')  ALENS(87,SURF)=ALENS(87,SURF)+W1
            IF(WC.EQ.'DPART   ')  ALENS(89,SURF)=ALENS(87,SURF)+W1
         END IF
         IF(WQ.EQ.'CENT') THEN
            IF(WC.EQ.'INDEX   ')&
            &ALENS(86,SURF)=ALENS(86,SURF)+(W1*0.01D0*ALENS(86,SURF))
            IF(WC.EQ.'VNUM    ')&
            &ALENS(87,SURF)=ALENS(87,SURF)+(W1*0.01D0*ALENS(87,SURF))
            IF(WC.EQ.'DPART   ')&
            &ALENS(89,SURF)=ALENS(89,SURF)+(W1*0.01D0*ALENS(89,SURF))
         END IF
         F22=1
      END IF
   END IF
   IF(F5.EQ.1) THEN
!     LENS INPUT LEVEL
      IF(GLANAM(SURF-1,1).NE.'MODEL') THEN
         OUTLYNE=&
         &'"INDEX","VNUM" AND "DPART" COMMANDS NOT VALID'
         CALL SHOWIT(1)
         OUTLYNE=&
         &'WITH CURRENT GLASS TYPE'
         CALL SHOWIT(1)
         OUTLYNE=&
         &'AT THE LENS INPUT LEVEL'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      ELSE
!     UPDATE LENS LEVEL
         IF(SQ.EQ.0) THEN
            IF(WC.EQ.'INDEX   ')  ALENS(86,SURF-1)=W1
            IF(WC.EQ.'VNUM    ')  ALENS(87,SURF-1)=W1
            IF(WC.EQ.'DPART   ')  ALENS(89,SURF-1)=W1
         END IF
         F22=1
      END IF
   END IF
!       NOW HANDEL EXISTING PIKUPS OF GLASS TYPE, I E DELETE THEM.
!       CHECK FOR CC PIKUPS AND DELETE IF FOUND
!
!
   IF(F6.EQ.1) THEN
      IF(PIKUP(1,SURF,20).EQ.0.0D0) THEN
!
!       NO GLASS PIKUPS, JUST RETURN
!
         RETURN
      ELSE
      END IF
!
!       DELETE THE PIKUP
      PIKUP(1:6,SURF,20)=0.0D0
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
!
!
!       PRINT MESSAGE
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GLASS) DELETED'
      CALL SHOWIT(1)
!
!       ARE THERE MORE PIKUPS? IF NOT SET ALENS(32,SURF) TO ZERO.
!
      PIKCNT=0
      DO I=1,PSIZ
         IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
            PIKCNT=PIKCNT+1
         ELSE
         END IF
      END DO
!
      IF(PIKCNT.EQ.0) ALENS(32,SURF)=0.0D0
   END IF
   IF(F5.EQ.1) THEN
      IF(PIKUP(1,SURF-1,20).EQ.0.0D0) THEN
!
!       NO GLASS PIKUPS, JUST RETURN
!
         RETURN
      ELSE
      END IF
!
!       DELETE THE PIKUP
      PIKUP(1:6,SURF-1,20)=0.0D0
      ALENS(32,SURF-1)=ALENS(32,SURF-1)-1.0D0
!
!
!       PRINT MESSAGE
      WRITE(OUTLYNE,*)'SURFACE',SURF-1,' :PIKUP (GLASS) DELETED'
      CALL SHOWIT(1)
!
!       ARE THERE MORE PIKUPS? IF NOT SET ALENS(32,SURF) TO ZERO.
!
      PIKCNT=0
      DO I=1,PSIZ
         IF(PIKUP(1,SURF-1,I).NE.0.0D0) THEN
            PIKCNT=PIKCNT+1
         ELSE
         END IF
      END DO
!
      IF(PIKCNT.EQ.0) ALENS(32,SURF-1)=0.0D0
   END IF
   RETURN
END
! SUB RNCHG.FOR
SUBROUTINE RNCHG
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE RNCHG WHICH IMPLEMENTS THE N1,N2,N3,N4,N5
!       N6,N7,N8,N9 AND N10 (N6 TO N10 ALSO WORK AT THE LENS LEVEL)
!       COMMANDS AT UPDATE LENS LEVEL. THIS IS USED TO CHG AN INDEX OF
!       REFRACTION IF THE GLASS NAME IS JUST GLASS (GLANAM(SURF,1) THAT IS)
!       IF THE GLASS IS A CATALOG GLASS, THE CAT NAME IS CHANGED TO "GLASS"
!
   INTEGER PIKCNT,I
!
!
!               CHECK FOR STRING OR QUALIFIER
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SST.EQ.1) THEN
      OUTLYNE=&
      &'INDEX CHANGE COMMANDS TAKE NO STRING INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(S2.EQ.1.OR.S3.EQ.1.OR.&
   &S4.EQ.1.OR.S5.EQ.1) THEN
      OUTLYNE=&
      &'INDEX CHANGE COMMANDS TAKE NO NUMERIC WORD #2 THROUGH #5 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.1.AND.F5.EQ.1) THEN
      OUTLYNE=&
      &'INDEX CHANGE COMMANDS TAKE NO QUALIFIER WORD IN LENS INPUT MODE'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(F6.EQ.1) THEN
      IF(WQ.NE.'        '.AND.WQ.NE.'CENT'.AND.WQ.NE.'DELT') THEN
         OUTLYNE=&
         &'INVALID QUALIFIER WORD USED WITH INDEX CHANGE COMMAND'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(DF1.EQ.1) THEN
      OUTLYNE=&
      &'INDEX CHANGE COMMANDS REQUIRE EXPLICIT NUMERIC WORD #1 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF

   IF(F5.EQ.1.AND.(SURF-1).LT.0) THEN
      OUTLYNE=&
      &'INDEX CHANGE COMMANDS WORK ON THE PREVIOUS SURFACE IN LENS'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'INPUT MODE BUT THERE ARE NO SURFACES AS YET'
      CALL SHOWIT(1)
      OUTLYNE='NO ACTION TAKEN'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(F6.EQ.1) THEN
      IF(GLANAM(SURF,1).NE.'GLASS') THEN
         GLANAM(SURF,1)='GLASS'
         OUTLYNE=&
         &'GLASS CATALOG NAME CHANGED TO "GLASS"'
         CALL SHOWIT(1)
      ELSE
!     UPDATE LENS LEVEL
         IF(SQ.EQ.0) THEN
            IF(WC.EQ.'N1')  ALENS(46,SURF)=W1
            IF(WC.EQ.'N2')  ALENS(47,SURF)=W1
            IF(WC.EQ.'N3')  ALENS(48,SURF)=W1
            IF(WC.EQ.'N4')  ALENS(49,SURF)=W1
            IF(WC.EQ.'N5')  ALENS(50,SURF)=W1
            IF(WC.EQ.'N6')  ALENS(71,SURF)=W1
            IF(WC.EQ.'N7')  ALENS(72,SURF)=W1
            IF(WC.EQ.'N8')  ALENS(73,SURF)=W1
            IF(WC.EQ.'N9')  ALENS(74,SURF)=W1
            IF(WC.EQ.'N10')  ALENS(75,SURF)=W1
         END IF
         IF(WQ.EQ.'DELT') THEN
            IF(WC.EQ.'N1')  ALENS(46,SURF)=ALENS(46,SURF)+W1
            IF(WC.EQ.'N2')  ALENS(47,SURF)=ALENS(47,SURF)+W1
            IF(WC.EQ.'N3')  ALENS(48,SURF)=ALENS(48,SURF)+W1
            IF(WC.EQ.'N4')  ALENS(49,SURF)=ALENS(49,SURF)+W1
            IF(WC.EQ.'N5')  ALENS(50,SURF)=ALENS(50,SURF)+W1
            IF(WC.EQ.'N6')  ALENS(71,SURF)=ALENS(71,SURF)+W1
            IF(WC.EQ.'N7')  ALENS(72,SURF)=ALENS(72,SURF)+W1
            IF(WC.EQ.'N8')  ALENS(73,SURF)=ALENS(73,SURF)+W1
            IF(WC.EQ.'N9')  ALENS(74,SURF)=ALENS(74,SURF)+W1
            IF(WC.EQ.'N10')  ALENS(75,SURF)=ALENS(75,SURF)+W1
         END IF
         IF(WQ.EQ.'CENT') THEN
            IF(WC.EQ.'N1')&
            &ALENS(46,SURF)=ALENS(46,SURF)+(W1*0.01D0*ALENS(46,SURF))
            IF(WC.EQ.'N2')&
            &ALENS(47,SURF)=ALENS(47,SURF)+(W1*0.01D0*ALENS(47,SURF))
            IF(WC.EQ.'N3')&
            &ALENS(48,SURF)=ALENS(48,SURF)+(W1*0.01D0*ALENS(48,SURF))
            IF(WC.EQ.'N4')&
            &ALENS(49,SURF)=ALENS(49,SURF)+(W1*0.01D0*ALENS(49,SURF))
            IF(WC.EQ.'N5')&
            &ALENS(49,SURF)=ALENS(50,SURF)+(W1*0.01D0*ALENS(50,SURF))
            IF(WC.EQ.'N6')&
            &ALENS(71,SURF)=ALENS(71,SURF)+(W1*0.01D0*ALENS(71,SURF))
            IF(WC.EQ.'N7')&
            &ALENS(72,SURF)=ALENS(72,SURF)+(W1*0.01D0*ALENS(72,SURF))
            IF(WC.EQ.'N8')&
            &ALENS(73,SURF)=ALENS(73,SURF)+(W1*0.01D0*ALENS(73,SURF))
            IF(WC.EQ.'N9')&
            &ALENS(74,SURF)=ALENS(74,SURF)+(W1*0.01D0*ALENS(74,SURF))
            IF(WC.EQ.'N10')&
            &ALENS(75,SURF)=ALENS(75,SURF)+(W1*0.01D0*ALENS(75,SURF))
         END IF
         F22=1
      END IF
   END IF
   IF(F5.EQ.1) THEN
!     LENS INPUT LEVEL
      IF(GLANAM(SURF-1,1).NE.'GLASS') THEN
         OUTLYNE=&
         &'INDEX CHANGE COMMAND NOT VALID WITH CURRENT GLASS TYPE'
         CALL SHOWIT(1)
         OUTLYNE=&
         &'AT THE LENS INPUT LEVEL'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      ELSE
!     UPDATE LENS LEVEL
         IF(SQ.EQ.0) THEN
            IF(WC.EQ.'N1')  ALENS(46,SURF-1)=W1
            IF(WC.EQ.'N2')  ALENS(47,SURF-1)=W1
            IF(WC.EQ.'N3')  ALENS(48,SURF-1)=W1
            IF(WC.EQ.'N4')  ALENS(49,SURF-1)=W1
            IF(WC.EQ.'N5')  ALENS(50,SURF-1)=W1
            IF(WC.EQ.'N6')  ALENS(71,SURF-1)=W1
            IF(WC.EQ.'N7')  ALENS(72,SURF-1)=W1
            IF(WC.EQ.'N8')  ALENS(73,SURF-1)=W1
            IF(WC.EQ.'N9')  ALENS(74,SURF-1)=W1
            IF(WC.EQ.'N10')  ALENS(75,SURF-1)=W1
         END IF
         F22=1
      END IF
   END IF
!       NOW HANDEL EXISTING PIKUPS OF GLASS TYPE, I E DELETE THEM.
!       CHECK FOR CC PIKUPS AND DELETE IF FOUND
!
!
   IF(F6.EQ.1) THEN
      IF(PIKUP(1,SURF,20).EQ.0.0D0) THEN
!
!       NO GLASS PIKUPS, JUST RETURN
!
         RETURN
      ELSE
      END IF
!
!       DELETE THE PIKUP
      PIKUP(1:6,SURF,20)=0.0D0
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
!
!
!       PRINT MESSAGE
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (GLASS) DELETED'
      CALL SHOWIT(1)
!
!       ARE THERE MORE PIKUPS? IF NOT SET ALENS(32,SURF) TO ZERO.
!
      PIKCNT=0
      DO I=1,PSIZ
         IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
            PIKCNT=PIKCNT+1
         ELSE
         END IF
      END DO
!
      IF(PIKCNT.EQ.0) ALENS(32,SURF)=0.0D0
   END IF
   IF(F5.EQ.1) THEN
      IF(PIKUP(1,SURF-1,20).EQ.0.0D0) THEN
!
!       NO GLASS PIKUPS, JUST RETURN
!
         RETURN
      ELSE
      END IF
!
!       DELETE THE PIKUP
      PIKUP(1:6,SURF-1,20)=0.0D0
      ALENS(32,SURF-1)=ALENS(32,SURF-1)-1.0D0
!
!
!       PRINT MESSAGE
      WRITE(OUTLYNE,*)'SURFACE',SURF-1,' :PIKUP (GLASS) DELETED'
      CALL SHOWIT(1)
!
!       ARE THERE MORE PIKUPS? IF NOT SET ALENS(32,SURF) TO ZERO.
!
      PIKCNT=0
      DO I=1,PSIZ
         IF(PIKUP(1,SURF-1,I).NE.0.0D0) THEN
            PIKCNT=PIKCNT+1
         ELSE
         END IF
      END DO
!
      IF(PIKCNT.EQ.0) ALENS(32,SURF-1)=0.0D0
   END IF
   RETURN
END
