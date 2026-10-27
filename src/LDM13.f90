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
      IF(IC.GT.gc_num_catalogs()) IC=-1          ! (SPECIAL)
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
SUBROUTINE GLASSP_RUSSIAN1
   use DATLEN
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   CHARACTER*13 RNAME(1:113)
   INTEGER I
   RNAME(1)=   'LK1'
   RNAME(2)=   'LK3'
   RNAME(3)=   'LK4'
   RNAME(4)=   'LK5'
   RNAME(5)=   'LK6'
   RNAME(6)=   'LK7'
   RNAME(7)=   'LK8'
   RNAME(8)=   'LK11'
   RNAME(9)=   'LK13'
   RNAME(10)=  'LK14'
   RNAME(11)=  'TFK1'
   RNAME(12)=  'K100'
   RNAME(13)=  'K2'
   RNAME(14)=  'K3'
   RNAME(15)=  'K8'
   RNAME(16)=  'K14'
   RNAME(17)=  'K15'
   RNAME(18)=  'K18'
   RNAME(19)=  'K19'
   RNAME(20)=  'K20'
   RNAME(21)=  'BK4'
   RNAME(22)=  'BK6'
   RNAME(23)=  'BK8'
   RNAME(24)=  'BK10'
   RNAME(25)=  'BK13'
   RNAME(26)=  'TK2'
   RNAME(27)=  'TK4'
   RNAME(28)=  'TK8'
   RNAME(29)=  'TK9'
   RNAME(30)=  'TK12'
   RNAME(31)=  'TK13'
   RNAME(32)=  'TK14'
   RNAME(33)=  'TK16'
   RNAME(34)=  'TK17'
   RNAME(35)=  'TK20'
   RNAME(36)=  'TK21'
   RNAME(37)=  'TK23'
   RNAME(38)=  'STK3'
   RNAME(39)=  'STK7'
   RNAME(40)=  'STK8'
   RNAME(41)=  'STK9'
   RNAME(42)=  'STK10'
   RNAME(43)=  'STK12'
   RNAME(44)=  'STK15'
   RNAME(45)=  'STK16'
   RNAME(46)=  'STK19'
   RNAME(47)=  'STK20'
   RNAME(48)=  'OK1'
   RNAME(49)=  'OK2'
   RNAME(50)=  'KF4'
   RNAME(51)=  'KF6'
   RNAME(52)=  'KF7'
   RNAME(53)=  'BF1'
   RNAME(54)=  'BF4'
   RNAME(55)=  'BF6'
   RNAME(56)=  'BF7'
   RNAME(57)=  'BF8'
   RNAME(58)=  'BF11'
   RNAME(59)=  'BF12'
   RNAME(60)=  'BF13'
   RNAME(61)=  'BF16'
   RNAME(62)=  'BF21'
   RNAME(63)=  'BF24'
   RNAME(64)=  'BF25'
   RNAME(65)=  'BF26'
   RNAME(66)=  'BF27'
   RNAME(67)=  'BF28'
   RNAME(68)=  'BF32'
   RNAME(69)=  'TBF3'
   RNAME(70)=  'TBF4'
   RNAME(71)=  'TBF8'
   RNAME(72)=  'TBF9'
   RNAME(73)=  'TBF25'
   RNAME(74)=  'TBF10'
   RNAME(75)=  'TBF11'
   RNAME(76)=  'LF5'
   RNAME(77)=  'LF7'
   RNAME(78)=  'LF8'
   RNAME(79)=  'LF9'
   RNAME(80)=  'LF10'
   RNAME(81)=  'LF11'
   RNAME(82)=  'LF12'
   RNAME(83)=  'F1'
   RNAME(84)=  'F2'
   RNAME(85)=  'F4'
   RNAME(86)=  'F6'
   RNAME(87)=  'F9'
   RNAME(88)=  'F13'
   RNAME(89)=  'F18'
   RNAME(90)=  'TF1'
   RNAME(91)=  'TF2'
   RNAME(92)=  'TF3'
   RNAME(93)=  'TF4'
   RNAME(94)=  'TF5'
   RNAME(95)=  'TF7'
   RNAME(96)=  'TF8'
   RNAME(97)=  'TF10'
   RNAME(98)=  'TF11'
   RNAME(99)=  'TF12'
   RNAME(100)= 'TF13'
   RNAME(101)= 'ST2'
   RNAME(102)= 'ST3'
   RNAME(103)= 'ST11'
   RNAME(104)= 'OF1'
   RNAME(105)= 'OF3'
   RNAME(106)= 'OF4'
   RNAME(107)= 'OF5'
   RNAME(108)= 'OF6'
   RNAME(109)= 'KY1'
   RNAME(110)= 'KY2'
   RNAME(111)= 'KB'
   RNAME(112)= 'KI'
   RNAME(113)= 'KB-P'
1  FORMAT('RUSSIAN       ',A13)
   DO I=1,113



      WRITE(OUTLYNE,1) RNAME(I)(1:13)
      CALL SHOWIT(0)
   END DO
   RETURN
END
SUBROUTINE GLASSP_RUSSIAN2
   use DATLEN
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   CHARACTER*13 RNAME(1:113),NAME1,NAME2
   CHARACTER*12 FLNAME
   real(real64) LAMUPP,LAMLOW
   INTEGER I
   RNAME(1)=   'LK1'
   RNAME(2)=   'LK3'
   RNAME(3)=   'LK4'
   RNAME(4)=   'LK5'
   RNAME(5)=   'LK6'
   RNAME(6)=   'LK7'
   RNAME(7)=   'LK8'
   RNAME(8)=   'LK11'
   RNAME(9)=   'LK13'
   RNAME(10)=  'LK14'
   RNAME(11)=  'TFK1'
   RNAME(12)=  'K100'
   RNAME(13)=  'K2'
   RNAME(14)=  'K3'
   RNAME(15)=  'K8'
   RNAME(16)=  'K14'
   RNAME(17)=  'K15'
   RNAME(18)=  'K18'
   RNAME(19)=  'K19'
   RNAME(20)=  'K20'
   RNAME(21)=  'BK4'
   RNAME(22)=  'BK6'
   RNAME(23)=  'BK8'
   RNAME(24)=  'BK10'
   RNAME(25)=  'BK13'
   RNAME(26)=  'TK2'
   RNAME(27)=  'TK4'
   RNAME(28)=  'TK8'
   RNAME(29)=  'TK9'
   RNAME(30)=  'TK12'
   RNAME(31)=  'TK13'
   RNAME(32)=  'TK14'
   RNAME(33)=  'TK16'
   RNAME(34)=  'TK17'
   RNAME(35)=  'TK20'
   RNAME(36)=  'TK21'
   RNAME(37)=  'TK23'
   RNAME(38)=  'STK3'
   RNAME(39)=  'STK7'
   RNAME(40)=  'STK8'
   RNAME(41)=  'STK9'
   RNAME(42)=  'STK10'
   RNAME(43)=  'STK12'
   RNAME(44)=  'STK15'
   RNAME(45)=  'STK16'
   RNAME(46)=  'STK19'
   RNAME(47)=  'STK20'
   RNAME(48)=  'OK1'
   RNAME(49)=  'OK2'
   RNAME(50)=  'KF4'
   RNAME(51)=  'KF6'
   RNAME(52)=  'KF7'
   RNAME(53)=  'BF1'
   RNAME(54)=  'BF4'
   RNAME(55)=  'BF6'
   RNAME(56)=  'BF7'
   RNAME(57)=  'BF8'
   RNAME(58)=  'BF11'
   RNAME(59)=  'BF12'
   RNAME(60)=  'BF13'
   RNAME(61)=  'BF16'
   RNAME(62)=  'BF21'
   RNAME(63)=  'BF24'
   RNAME(64)=  'BF25'
   RNAME(65)=  'BF26'
   RNAME(66)=  'BF27'
   RNAME(67)=  'BF28'
   RNAME(68)=  'BF32'
   RNAME(69)=  'TBF3'
   RNAME(70)=  'TBF4'
   RNAME(71)=  'TBF8'
   RNAME(72)=  'TBF9'
   RNAME(73)=  'TBF25'
   RNAME(74)=  'TBF10'
   RNAME(75)=  'TBF11'
   RNAME(76)=  'LF5'
   RNAME(77)=  'LF7'
   RNAME(78)=  'LF8'
   RNAME(79)=  'LF9'
   RNAME(80)=  'LF10'
   RNAME(81)=  'LF11'
   RNAME(82)=  'LF12'
   RNAME(83)=  'F1'
   RNAME(84)=  'F2'
   RNAME(85)=  'F4'
   RNAME(86)=  'F6'
   RNAME(87)=  'F9'
   RNAME(88)=  'F13'
   RNAME(89)=  'F18'
   RNAME(90)=  'TF1'
   RNAME(91)=  'TF2'
   RNAME(92)=  'TF3'
   RNAME(93)=  'TF4'
   RNAME(94)=  'TF5'
   RNAME(95)=  'TF7'
   RNAME(96)=  'TF8'
   RNAME(97)=  'TF10'
   RNAME(98)=  'TF11'
   RNAME(99)= 'TF12'
   RNAME(100)= 'TF13'
   RNAME(101)= 'ST2'
   RNAME(102)= 'ST3'
   RNAME(103)= 'ST11'
   RNAME(104)= 'OF1'
   RNAME(105)= 'OF3'
   RNAME(106)= 'OF4'
   RNAME(107)= 'OF5'
   RNAME(108)= 'OF6'
   RNAME(109)= 'KY1'
   RNAME(110)= 'KY2'
   RNAME(111)= 'KB'
   RNAME(112)= 'KI'
   RNAME(113)= 'KB-P'
1  FORMAT('RUSSIAN       ',A13)
10 FORMAT('Normal Cubic Spline Fit to SOVOPTIKS Data')
   DO I=1,113
      IF(WS(1:13).EQ.RNAME(I)(1:13)) THEN
991      FORMAT('WAVELENGTH RANGE:')
992      FORMAT('LOWER WAVELENGTH = ',G15.8,' MICRONS')
993      FORMAT('UPPER WAVELENGTH = ',G15.8,' MICRONS')
         NAME1='RUSSIAN      '
         NAME2=RNAME(I)(1:13)
         FLNAME=' '
         CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
         WRITE(OUTLYNE,991)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,992) LAMLOW
         CALL SHOWIT(0)
         WRITE(OUTLYNE,993) LAMUPP
         CALL SHOWIT(0)



         WRITE(OUTLYNE,1) RNAME(I)(1:13)
         CALL SHOWIT(0)



         WRITE(OUTLYNE,10)
         CALL SHOWIT(0)
         GO TO 3
      ELSE
      END IF
   END DO



   CALL REPORT_ERROR_AND_FAIL('THE REQUESTED GLASS NAME WAS NOT FOUND'//'\n'//'RE-ENTER COMMAND', 1)
3  CONTINUE
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
      WRITE(OUTLYNE,107)
      CALL SHOWIT(0)
100   FORMAT('THE CURRENT PROGRAM GLASS CATALOG NAMES ARE:')
101   FORMAT(A13,1X,A)
106   FORMAT(10X,'"GLCAT     (Searches all catalogs)')
117   FORMAT(10X,'"GLASS     (Searches all catalogs)')
116   FORMAT(10X,'            [specified without giving catalog name]')
2161  FORMAT(10X,'              [but just by glass name or number]')
107   FORMAT(10X,'"MATL"     (Special IR, UV, Visual Materials and Plastics)')
      RETURN
   END IF
!
   IF(SQ.EQ.1.AND.SST.EQ.0) THEN
!     THE GLASS NAMES AND NUMBERS OF A CATALOG
      IF(WQ.EQ.'SPECIAL') THEN
         call PRINTSPECIALCATALOGMATERIALS()
         return
      END IF
      IF(WQ.EQ.'MATL') THEN
         CALL PRINTMATLGLASSES()
         RETURN
      END IF
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
      IF(WQ.EQ.'RUSSIAN') THEN
         CALL GLASSP_RUSSIAN2
         RETURN
      END IF
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
   use glass_catalog, only: gc_glass, glass_record, agf_formula_name, agf_formula_ncoef
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


SUBROUTINE PRINTSPECIALCATALOGMATERIALS
   use zoa_output, only:  zoa_emit
   use glass_manager
   use DATLEN
   use DATMAI

   integer :: i

   do i=1,size(specCat%names)
      call zoa_emit('SPECIAL  '&
      &//specCat%names(i), "black")

   end do



END SUBROUTINE

SUBROUTINE PRINTMATLGLASSES
   use DATLEN
   use DATMAI


3000 FORMAT('MATL          ACRYLIC      ',&
   &'-   Acrylic (Lucite)')
   WRITE(OUTLYNE,3000)
   CALL SHOWIT(0)
3010 FORMAT('MATL          PLYSTY       ',&
   &'-   Polystyrene')
   WRITE(OUTLYNE,3010)
   CALL SHOWIT(0)
3020 FORMAT('MATL          POLYCARB     ',&
   &'-   Polycarbonate')
   WRITE(OUTLYNE,3020)
   CALL SHOWIT(0)
3030 FORMAT('MATL          SAN          ',&
   &'-   Copolymer Styrene-Acrylonitrile (SAN)')
   WRITE(OUTLYNE,3030)
   CALL SHOWIT(0)

200 FORMAT('MATL          GERMSC       ',&
   &'-   Single Crystal Germanium')
   WRITE(OUTLYNE,200)
   CALL SHOWIT(0)
201 FORMAT('MATL          GERMPC       ',&
   &'-   Poly-Crystalline Germanium')
   WRITE(OUTLYNE,201)
   CALL SHOWIT(0)
202 FORMAT('MATL          SILICON      ',&
   &'-   Silicon')
   WRITE(OUTLYNE,202)
   CALL SHOWIT(0)
203 FORMAT('MATL          IRG100       ',&
   &'-   Schott IRG100 Infrared Glass')
   WRITE(OUTLYNE,203)
   CALL SHOWIT(0)
204 FORMAT('MATL          ZNSE         ',&
   &'-   CVD Zinc Selenide (IRTRAN4)')
   WRITE(OUTLYNE,204)
   CALL SHOWIT(0)
205 FORMAT('MATL          ZNS          ',&
   &'-   CVD Zinc Sulfide (IRTRAN2)')
   WRITE(OUTLYNE,205)
   CALL SHOWIT(0)
1215 FORMAT('MATL          IRTRAN1      ',&
   &'-   IRTRAN1 (Magnesium Fluoride (o) ray')
   WRITE(OUTLYNE,1215)
   CALL SHOWIT(0)
1205 FORMAT('MATL          IRTRAN2      ',&
   &'-   IRTRAN2 (CVD Zinc Sulfide)')
   WRITE(OUTLYNE,1205)
   CALL SHOWIT(0)
1217 FORMAT('MATL          IRTRAN3      ',&
   &'-   IRTRAN3 (Calcium Fluoride)')
   WRITE(OUTLYNE,1217)
   CALL SHOWIT(0)
1204 FORMAT('MATL          IRTRAN4      ',&
   &'-   IRTRAN4 (CVD Zinc Selenide)')
   WRITE(OUTLYNE,1204)
   CALL SHOWIT(0)
1218 FORMAT('MATL          IRTRAN5      ',&
   &'-   IRTRAN5 (Magnesium Oxide)')
   WRITE(OUTLYNE,1218)
   CALL SHOWIT(0)
1214 FORMAT('MATL          IRTRAN6      ',&
   &'-   IRTRAN6 (Cadmium Telluride)')
   WRITE(OUTLYNE,1214)
   CALL SHOWIT(0)
206 FORMAT('MATL          CLRTRAN      ',&
   &'-   CVD CLEARTRAN')
   WRITE(OUTLYNE,206)
   CALL SHOWIT(0)
207 FORMAT('MATL          SILICA       ',&
   &'-   Fused Silica (SiO2)')
   WRITE(OUTLYNE,1207)
   CALL SHOWIT(0)
2071 FORMAT('MATL          SIO2         ',&
   &'-   Fused Silica (SiO2) (SILICA equiv.)')
   WRITE(OUTLYNE,1207)
   CALL SHOWIT(0)
1207 FORMAT('MATL          SUPRASIL     ',&
   &'-   Fused Silica (SiO2)')
   WRITE(OUTLYNE,2207)
   CALL SHOWIT(0)
2207 FORMAT('MATL          HOMOSIL      ',&
   &'-   Fused Silica (SiO2)')
   WRITE(OUTLYNE,207)
   CALL SHOWIT(0)
   WRITE(OUTLYNE,2071)
   CALL SHOWIT(0)
208 FORMAT('MATL          SAPPHIRE     ',&
   &'-  Sapphire (Al2O3) (o)rdinary ray')
   WRITE(OUTLYNE,208)
   CALL SHOWIT(0)
209 FORMAT('MATL          DYNASIL      ',&
   &'-   Synthetic Fused Silica')
   WRITE(OUTLYNE,209)
   CALL SHOWIT(0)
210 FORMAT('MATL          AMTIR1       ',&
   &'-   AMTIR-1')
   WRITE(OUTLYNE,210)
   CALL SHOWIT(0)
211 FORMAT('MATL          AMTIR3       ',&
   &'-   AMTIR-3 (TI 1173 Equivalent)')
   WRITE(OUTLYNE,211)
   CALL SHOWIT(0)
212 FORMAT('MATL          AS2S3        ',&
   &'-   Arsenic Trisulfide IR Glass')
   WRITE(OUTLYNE,212)
   CALL SHOWIT(0)
213 FORMAT('MATL          GAAS         ',&
   &'-   Gallium Arsenide')
   WRITE(OUTLYNE,213)
   CALL SHOWIT(0)
214 FORMAT('MATL          CDTE         ',&
   &'-   Cadmium Telluride (IRTRAN6)')
   WRITE(OUTLYNE,214)
   CALL SHOWIT(0)
215 FORMAT('MATL          MGF2(O)      ',&
   &'-   Magnesium Fluoride (IRTRAN1) (o) ray')
   WRITE(OUTLYNE,215)
   CALL SHOWIT(0)
2215 FORMAT('MATL          MGF2         ',&
   &'-   Magnesium Fluoride (IRTRAN1) (o) ray')
   WRITE(OUTLYNE,2215)
   CALL SHOWIT(0)
216 FORMAT('MATL          MGF2(E)      ',&
   &'-   Magnesium Fluoride (IRTRAN1) (e) ray')
   WRITE(OUTLYNE,216)
   CALL SHOWIT(0)
217 FORMAT('MATL          CAF2         ',&
   &'-   Calcium Fluoride (IRTRAN3)')
   WRITE(OUTLYNE,217)
   CALL SHOWIT(0)
218 FORMAT('MATL          MGO          ',&
   &'-   Magnesium Oxide (IRTRAN5)')
   WRITE(OUTLYNE,218)
   CALL SHOWIT(0)
219 FORMAT('MATL          BAF2         ',&
   &'-   Barium Fluoride')
   WRITE(OUTLYNE,219)
   CALL SHOWIT(0)
220 FORMAT('MATL          KBR          ',&
   &'-   Potassium Bromide')
   WRITE(OUTLYNE,220)
   CALL SHOWIT(0)
221 FORMAT('MATL          CSI          ',&
   &'-   Cesium Iodide')
   WRITE(OUTLYNE,221)
   CALL SHOWIT(0)
222 FORMAT('MATL         CSBR          ',&
   &'-   Cesium Bromide')
   WRITE(OUTLYNE,222)
   CALL SHOWIT(0)
223 FORMAT('MATL         KRS5          ',&
   &'-   Thallium Bromoiodide (KRS-5)')
   WRITE(OUTLYNE,223)
   CALL SHOWIT(0)
224 FORMAT('MATL         SIO2O         ',&
   &'-   Crystalline Quartz (SiO2) (o) ray')
   WRITE(OUTLYNE,224)
   CALL SHOWIT(0)
225 FORMAT('MATL         SIO2E         ',&
   &'-   Crystalline Quartz (SiO2) (e) ray')
   WRITE(OUTLYNE,225)
   CALL SHOWIT(0)
226 FORMAT('MATL         NACL          ',&
   &'-   Sodium Chloride')
   WRITE(OUTLYNE,226)
   CALL SHOWIT(0)
227 FORMAT('MATL         LIF           ',&
   &'-   Lithium Fluoride')
   WRITE(OUTLYNE,227)
   CALL SHOWIT(0)
228 FORMAT('MATL         VIR3          ',&
   &'-   Corning VIR 3 Infrared Glass')
   WRITE(OUTLYNE,228)
   CALL SHOWIT(0)
229 FORMAT('MATL         9754          ',&
   &'-   Corning 9754 Infrared Glass')
   WRITE(OUTLYNE,229)
   CALL SHOWIT(0)
230 FORMAT('MATL         ALON          ',&
   &'-   Raytheon Aluminum Oxynitride')
   WRITE(OUTLYNE,230)
   CALL SHOWIT(0)
231 FORMAT('MATL         SPINEL        ',&
   &'-   Magnesium Aluminate Spinel (MgAl2O4)')
   WRITE(OUTLYNE,231)
   CALL SHOWIT(0)
232 FORMAT('MATL         CALAL         ',&
   &'-   Calcium Aluminate Glass')
   WRITE(OUTLYNE,232)
   CALL SHOWIT(0)
233 FORMAT('MATL         B270          ',&
   &'-   Schott B270 Glass')
   WRITE(OUTLYNE,233)
   CALL SHOWIT(0)
234 FORMAT('MATL         IRG2          ',&
   &'-   Schott IRG2 Glass')
   WRITE(OUTLYNE,234)
   CALL SHOWIT(0)
235 FORMAT('MATL         IRG3          ',&
   &'-   Schott IRG3 Glass')
   WRITE(OUTLYNE,235)
   CALL SHOWIT(0)
236 FORMAT('MATL         IRGN6         ',&
   &'-   Schott IRGN6 Glass')
   WRITE(OUTLYNE,236)
   CALL SHOWIT(0)
237 FORMAT('MATL         IRG7          ',&
   &'-   Schott IRG7 Glass')
   WRITE(OUTLYNE,237)
   CALL SHOWIT(0)
238 FORMAT('MATL         IRG9          ',&
   &'-   Schott IRG9 Glass')
   WRITE(OUTLYNE,238)
   CALL SHOWIT(0)
239 FORMAT('MATL         IRG11         ',&
   &'-   Schott IRG11 Glass')
   WRITE(OUTLYNE,239)
   CALL SHOWIT(0)
240 FORMAT('MATL         IRG15         ',&
   &'-   Schott IRG15 Glass')
   WRITE(OUTLYNE,240)
   CALL SHOWIT(0)
241 FORMAT('MATL         WATER          ',&
   &'-   Distilled Water')
   WRITE(OUTLYNE,241)
   CALL SHOWIT(0)
242 FORMAT('MATL         VAC           ',&
   &'-   Vacuum')
   WRITE(OUTLYNE,242)
   CALL SHOWIT(0)
243 FORMAT('MATL         ZNS-MS        ',&
   &'-   II-VI Multi-Spectral ZNS')
   WRITE(OUTLYNE,243)
   CALL SHOWIT(0)
244 FORMAT('MATL         CEF3          ',&
   &'-   Cerium Flouride for coatings')
   WRITE(OUTLYNE,244)
   CALL SHOWIT(0)
245 FORMAT('MATL         LA2O3         ',&
   &'-   Lanthanum Oxide for coatings')
   WRITE(OUTLYNE,245)
   CALL SHOWIT(0)
246 FORMAT('MATL         THF4          ',&
   &'-   Thorium Flouride for coatings')
   WRITE(OUTLYNE,246)
   CALL SHOWIT(0)
247 FORMAT('MATL         ZRO2          ',&
   &'-   Zirconium Oxide for coatings')
   WRITE(OUTLYNE,247)
   CALL SHOWIT(0)
248 FORMAT('MATL         DIAMOND       ',&
   &'-   Herzberger formula')
   WRITE(OUTLYNE,248)
   CALL SHOWIT(0)
249 FORMAT('MATL         YAG - Yittrium Aluminum Garnet')
   WRITE(OUTLYNE,249)
   CALL SHOWIT(0)

END SUBROUTINE
