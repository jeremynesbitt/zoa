!       THIRTEENTH FILE FOR LENS DATABASE MANAGER FILES

! SUB FNDGLS.FOR
SUBROUTINE FNDGLS
!
   use DATLEN
   use DATMAI
   use command_utils, only: is_command_query
   use mod_system, only: sys_last_surf, sys_wavelength, sys_wl_weight
   use mod_surface, only: surf_refractive_index
   use glass_manager, only: gdb
   use glass_catalog, only: gc_num_catalogs, gc_catalog_name, gc_catalog_size, &
                            gc_index, gc_glass, glass_record
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE FNDGLS WHICH IMPLEMENTS THE FINDGLAS
!       COMMAND AT THE CMD LEVEL: THE CATALOG GLASSES CLOSEST TO A
!       "MODEL" GLASS (RMS INDEX DIFFERENCE OVER THE WEIGHTED
!       WAVELENGTHS), FROM ONE CATALOG OR (ALL) EVERY CATALOG.
!
   INTEGER NUMSRCH,I,J,K,IC,IG,NTOT,ISURF,II
   real(real64) D,NMOD,NCAT
   real(real64), allocatable :: SORTIT(:)
   CHARACTER(len=13), allocatable :: ASORTIT(:,:)
   type(glass_record) :: g
!
   IF(is_command_query()) THEN
      OUTLYNE='"FINDGLAS" SEARCHES FOR GLASSES CLOSE TO A "MODEL" GLASS'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      RETURN
   END IF
   IF(SST.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL('"FINDGLAS" TAKES NO STRING INPUT'//'\n'//'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(SQ.EQ.0) THEN
      SQ=1
      WQ='ALL'
   END IF
   IC=0
   IF(WQ.NE.'ALL') THEN
      IF(.NOT.gdb%isNameInCatalog(trim(WQ), IC)) IC=-1
   END IF
   IF(IC.LT.0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'INVALID CATALOG NAME ISSUED WITH "FINDGLAS"'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"FINDGLAS" ONLY TAKES NUMERIC WORDS #1 AND #2'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(DF1.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"FINDGLAS" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(W1.LT.0.0D0.OR.W1.GT.sys_last_surf()) THEN
      CALL REPORT_ERROR_AND_FAIL('SURFACE NUMBER BEYOND LEGAL BOUNDS'//'\n'//'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(DF2.EQ.1) THEN
      DF2=0
      S2=1
      W2=5.0D0
   END IF
   IF(W2.LT.1.0D0) THEN
      CALL REPORT_ERROR_AND_FAIL('"N" MUST BE 1 GREATER'//'\n'//'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   NUMSRCH=INT(W2)
   ISURF=INT(W1)
   IF(GLANAM(ISURF,1)(1:5).NE.'MODEL') THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'GLASS IS NOT A "MODEL" GLASS'//'\n'//&
      & '"FINDGLAS" TOOK NO ACTION', 1)
      RETURN
   END IF
!
!     DISTANCE OF EVERY GLASS OF THE CATALOG(S) FROM THE MODEL GLASS
   NTOT=0
   DO I=1,gc_num_catalogs()
      IF(IC.EQ.0.OR.IC.EQ.I) NTOT=NTOT+gc_catalog_size(I)
   END DO
   ALLOCATE(SORTIT(max(NTOT,1)),ASORTIT(2,max(NTOT,1)))
   K=0
   DO I=1,gc_num_catalogs()
      IF(IC.NE.0.AND.IC.NE.I) CYCLE
      DO IG=1,gc_catalog_size(I)
         D=0.0D0
         DO J=1,10
            IF(sys_wl_weight(J).EQ.0.0D0) CYCLE
            NMOD=surf_refractive_index(ISURF,J)
            NCAT=gc_index(I,IG,sys_wavelength(J))
            D=D+((NMOD-NCAT)**2)
         END DO
         g=gc_glass(I,IG)
         K=K+1
         ASORTIT(1,K)=gc_catalog_name(I)
         ASORTIT(2,K)=g%name
         SORTIT(K)=DSQRT(D)
      END DO
   END DO
   IF(K.GT.0) CALL SORT_JK(K,SORTIT,ASORTIT)
10 FORMAT('THE CLOSEST ',I3,' CATALOG GLASSES')
20 FORMAT('FOUND IN THE LAST "FINDGLAS" SEARCH')
30 FORMAT('FOR SURFACE #',I3,' ARE:')
   WRITE(OUTLYNE,10) NUMSRCH
   CALL SHOWIT(0)
   WRITE(OUTLYNE,20)
   CALL SHOWIT(0)
   WRITE(OUTLYNE,30) ISURF
   CALL SHOWIT(0)
50 FORMAT('  # ',1X,'CAT. NAME    ',1X,'GLASS NAME   ',6X,'DISTANCE')
   WRITE(OUTLYNE,50)
   CALL SHOWIT(0)
   DO II=1,min(NUMSRCH,K,5000)
      WRITE(OUTLYNE,40) II,ASORTIT(1,II),ASORTIT(2,II),SORTIT(II)
      CALL SHOWIT(0)
   END DO
40 FORMAT(I4,1X,A13,1X,A13,1X,G23.15)
   DEALLOCATE(SORTIT,ASORTIT)
   RETURN
END
! SUB GLASSP.FOR
SUBROUTINE GLASSP
   use glass_manager, only: gdb
   use glass_catalog, only: gc_num_catalogs
!
   use DATLEN
   use DATMAI
   use command_utils, only: is_command_query
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE GLASSP WHICH IMPLEMENTS THE GLASSP
!       COMMAND AT THE CMD LEVEL:
!         GLASSP               THE CATALOG NAMES
!         GLASSP cat           THE GLASSES OF A CATALOG (CATALOG NAME NUMBER)
!         GLASSP GLAK|GLCAT    THE GLASSES OF EVERY CATALOG
!         GLASSP cat glass     THE DATA OF A GLASS (BY NAME OR GLASS CODE);
!                              GLAK|GLCAT SEARCHES EVERY CATALOG
!
   INTEGER i, IC
   LOGICAL FOUND
!
   IF(SN.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"GLASSP" TAKES NO NUMERIC WORD INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(SST.EQ.1.AND.SQ.EQ.0) THEN
      CALL REPORT_ERROR_AND_FAIL('"THE GLASS CATALOG NAME IS MISSING'//'\n'//'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(is_command_query()) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"GLASSP" INTERROGATES GLASS CATALOGS'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   call gdb%sync()
!
   IF(SST.EQ.0.AND.SQ.EQ.0) THEN
!     JUST GLASSP
      WRITE(OUTLYNE,100)
      CALL SHOWIT(0)
      do i=1,size(gdb%catalogDescriptions)
         WRITE(OUTLYNE,101) gdb%catalogs(i), trim(gdb%catalogDescriptions(i))
         CALL SHOWIT(0)
      end do
      WRITE(OUTLYNE,106)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,117)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,116)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,2161)
      CALL SHOWIT(0)
100   FORMAT('THE CURRENT PROGRAM GLASS CATALOG NAMES ARE:')
101   FORMAT(A13,1X,A)
106   FORMAT(10X,'"GLCAT     (Searches all catalogs)')
117   FORMAT(10X,'"GLASS     (Searches all catalogs)')
116   FORMAT(10X,'            [specified without giving catalog name]')
2161  FORMAT(10X,'              [but just by glass name or number]')
      RETURN
   END IF
!
   IF(SQ.EQ.1.AND.SST.EQ.0) THEN
!     THE GLASS NAMES AND NUMBERS OF A CATALOG
      IF(WQ.EQ.'GLCAT'.OR.WQ.EQ.'GLAK') THEN
         DO IC=1,gc_num_catalogs()
            CALL LISTAGFCATALOG(IC)
         END DO
         RETURN
      END IF
      IC=0
      IF(.NOT.gdb%isNameInCatalog(trim(WQ), IC)) THEN
         WRITE(OUTLYNE,*) WQ
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL(&
         & 'THE REQUESTED GLASS CATALOG WAS NOT RECOGNIZED'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      CALL LISTAGFCATALOG(IC)
      RETURN
   END IF
!
   IF(SQ.EQ.1.AND.SST.EQ.1) THEN
!     THE DATA OF THE SPECIFIED GLASS
      FOUND=.FALSE.
      IC=0
      IF(WQ.EQ.'GLCAT'.OR.WQ.EQ.'GLAK') THEN
         FOUND=gdb%isGlassInAnyCatalog(WS(1:32), IC, byCode=.TRUE.)
      ELSE IF(gdb%isNameInCatalog(trim(WQ), IC)) THEN
         FOUND=gdb%isGlassInCatalog(WS(1:32), IC, byCode=.TRUE.)
      ELSE
         CALL REPORT_ERROR_AND_FAIL(&
         & '2THE REQUESTED GLASS CATALOG WAS NOT RECOGNIZED'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(FOUND.AND.gdb%curCat.GT.0) CALL SHOWAGFGLASS(gdb%curCat, gdb%curGlass)
   END IF
END

! The glasses of catalog ic: catalog, name and glass code per line (the
! Glass Manager window reads this listing).
SUBROUTINE LISTAGFCATALOG(IC)
   use DATMAI
   use glass_catalog, only: gc_catalog_name, gc_catalog_size, gc_glass, glass_record
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: IC
   INTEGER IG
   type(glass_record) :: g
   CHARACTER(len=13) :: CATNAME, NAME, NUMBER
   CATNAME=gc_catalog_name(IC)
   DO IG=1,gc_catalog_size(IC)
      g=gc_glass(IC,IG)
      NAME=g%name
      NUMBER=g%code
      WRITE(OUTLYNE,110) CATNAME,NAME,NUMBER
      CALL SHOWIT(0)
   END DO
110 FORMAT(A13,1X,A13,1X,A13)
END SUBROUTINE

! The data of glass ig of catalog ic: wavelength range, name, dispersion
! formula and its coefficients.
SUBROUTINE SHOWAGFGLASS(IC, IG)
   use DATMAI
   use glass_catalog, only: gc_glass, glass_record, agf_formula_name, agf_formula_ncoef, &
                            FORMULA_TABLE
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: IC, IG
   type(glass_record) :: g
   INTEGER K
   CHARACTER(len=12) :: LBL
   g=gc_glass(IC,IG)
991 FORMAT('WAVELENGTH RANGE:')
992 FORMAT('LOWER WAVELENGTH = ',G15.8,' MICRONS')
993 FORMAT('UPPER WAVELENGTH = ',G15.8,' MICRONS')
120 FORMAT('GLASS NAME   = ',A)
121 FORMAT('GLASS NUMBER = ',A)
122 FORMAT(A12,' = ',G23.15)
   WRITE(OUTLYNE,991)
   CALL SHOWIT(0)
   WRITE(OUTLYNE,992) g%lmin
   CALL SHOWIT(0)
   WRITE(OUTLYNE,993) g%lmax
   CALL SHOWIT(0)
   WRITE(OUTLYNE,120) trim(g%name)
   CALL SHOWIT(0)
   WRITE(OUTLYNE,121) trim(g%code)
   CALL SHOWIT(0)
   IF(g%formula.EQ.FORMULA_TABLE) THEN
      WRITE(OUTLYNE,123) g%ntab
      CALL SHOWIT(0)
      DO K=1,g%ntab
         WRITE(OUTLYNE,124) g%tab(1,K), g%tab(2,K)
         CALL SHOWIT(0)
      END DO
123   FORMAT('TABLE GLASS (ZTG), ',I4,' POINTS:  WAVELENGTH (MICRONS)  INDEX')
124   FORMAT(20X,G15.8,4X,G15.8)
      RETURN
   END IF
   OUTLYNE=trim(agf_formula_name(g%formula))//' DISPERSION FORMULA (AGF FORMULA '// &
   &  CHAR(48+g%formula/10)//CHAR(48+MOD(g%formula,10))//')'
   CALL SHOWIT(0)
   DO K=1,MIN(g%ncd,agf_formula_ncoef(g%formula))
      SELECT CASE (g%formula)
      CASE (1)
         WRITE(LBL,'(10X,A1,I1)') 'A', K-1
      CASE (2)
         ! K1 L1 K2 L2 K3 L3, labelled B1 C1 B2 C2 B3 C3 (Schott's names)
         IF(MOD(K,2).EQ.1) THEN
            WRITE(LBL,'(10X,A1,I1)') 'B', (K+1)/2
         ELSE
            WRITE(LBL,'(10X,A1,I1)') 'C', K/2
         END IF
      CASE DEFAULT
         WRITE(LBL,'(7X,A2,I2.2)') 'CD', K
      END SELECT
      WRITE(OUTLYNE,122) LBL, g%cd(K)
      CALL SHOWIT(0)
   END DO
END SUBROUTINE



