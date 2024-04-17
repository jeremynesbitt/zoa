! Currently most glass routines are in .FOR files like LDM13.
! Would like to eventually migrate functionality to this module
! to facilitate supporting different glasses

module glass_manager
  use iso_fortran_env, only: real64

type :: glassdb
  character(len=13), allocatable :: catalogs(:)
  character(len=19), allocatable :: catalogFileNames(:)
  character(len=200), allocatable :: catalogDescriptions(:)
  integer, allocatable :: catalogDataTypes(:)

  real(kind=real64), dimension(6) :: coeffs
  integer :: dataType

  contains
  procedure, public, pass(self) :: isNameInCatalog
  procedure, public, pass(self) :: isGlassInCatalog
  procedure, public, pass(self) :: isGlassInAnyCatalog
  procedure, public, pass(self) :: calcIndexForCurrentGlass
  procedure, private, pass(self) :: calcPN
  procedure, private, pass(self) :: calcPNSC

end type

interface glassdb
  module procedure glassdb_constructor
end interface

type(glassdb) :: gdb
integer, parameter ::  ID_PN   = 1
integer, parameter ::  ID_PNSC = 2



contains

 type(glassdb) function glassdb_constructor() result(self)
     integer, parameter :: numC = 10
     allocate(character(len=13) :: self%catalogs(numC))
     allocate(character(len=19) :: self%catalogFileNames(numC))
     allocate(character(len=200) :: self%catalogDescriptions(numC))
     allocate(self%catalogDataTypes(numC))


     self%catalogs(1) = 'SCHOTT'
     self%catalogFileNames(1) = 'SCHOTT.BIN'
     self%catalogDescriptions(1) = 'Schott Optical Glass Catalog'
     self%catalogDataTypes(1) = ID_PNSC

     self%catalogs(2) = 'SCH2000'
     self%catalogFileNames(2) = 'SCH2000.BIN'
     self%catalogDescriptions(2) = 'SCHOTT POST-2000 Optical Glass Catalog'
     self%catalogDataTypes(2) = ID_PNSC
     
          self%catalogs(3) = 'OHARA'
     self%catalogFileNames(3) = 'OHARA.BIN'
     self%catalogDescriptions(3) = 'OHara Optical Glass Catalog'
     self%catalogDataTypes(3) = ID_PNSC
     
     self%catalogs(4) = 'OHARA'
     self%catalogFileNames(4) = 'OHARA-O.BIN'
     self%catalogDescriptions(4) = 'OHara Optical Glass Catalog'
     self%catalogDataTypes(4) = ID_PNSC
     
     self%catalogs(5) = 'HOYA'
     self%catalogFileNames(5) = 'HOYA.BIN'
     self%catalogDescriptions(5) = 'Hoya Optical Glass Catalog'
     self%catalogDataTypes(5) = ID_PN
     
     self%catalogs(6) = 'CHANCE'
     self%catalogFileNames(6) = 'CHANCE.BIN'
     self%catalogDescriptions(6) = 'Chance-Pilkington Optical Glass Catalog'
     self%catalogDataTypes(6) = ID_PN
     
     self%catalogs(7) = 'CORNIN'
     self%catalogFileNames(7) = 'CORNIN.BIN'
     self%catalogDescriptions(7) = 'Corning-France Optical Glass Catalog'
     self%catalogDataTypes(7) = ID_PN
     
     self%catalogs(8) = 'HIKARI'
     self%catalogFileNames(8) = 'HIKARI.BIN'
     self%catalogDescriptions(8) = 'Hikara Optical Glass Catalog'
     self%catalogDataTypes(8) = ID_PN
     
     self%catalogs(9) = 'RADHARD'
     self%catalogFileNames(9) = 'RADHARD.BIN'
     self%catalogDescriptions(9) = 'Radiation Resistant Optical Materials'
     self%catalogDataTypes(9) = ID_PN
     
     self%catalogs(10) = 'SCH2023'
     self%catalogFileNames(10) = 'SCH2023.BIN'
     self%catalogDescriptions(10) = 'Schott 2023 Preferred Glass Catalog'
     self%catalogDataTypes(10) = ID_PNSC


 end function

 subroutine parseModelGlassEntry(modelGlass, nd, vd)
  use type_utils, only: str2int
  character(len=*) :: modelGlass
  integer :: dotLoc
  real*8, intent(inout) :: nd, vd
  character(len=3) :: nStr, vStr
  
  
  ! Format is:  MXYZ.ABC
  ! XYZ is index - 1
  ! ABC is abbe number *10
  dotLoc = INDEX(modelGlass, '.')
  nStr = modelGlass(1:dotLoc-1)
  call LogTermFOR("DEBUG: ModelGlass Start is "// modelGlass(1:dotLoc-1))
  vStr = modelGlass(dotLoc+1:len(modelGlass))

  
  ! Divide the number to the left of the . s.t. it is < 1.  Then add one to it to make the index
  PRINT *, "Num is ", REAL(str2int(nStr))
  PRINT *, "Dem is ", (10**(dotLoc-2))
  nd = 1.0+ REAL(str2int(nStr))/(10**(dotLoc-1))
  vd = REAL(str2int(vStr))/10.0
  PRINT *, "nd is ", nd
  PRINT *, "vd is ", vd



 end subroutine

 function calcPN(self, lambda) result(n)

  class(glassdb) :: self
  real(kind=real64), intent(in) :: lambda
  real(kind=real64) :: n

   n = DSQRT(self%coeffs(1)+ &
   &   (self%coeffs(2)*(lambda**2))+&
   &   (self%coeffs(3)*(1.0D0/(lambda**2)))+ &
   &   (self%coeffs(4)*(1.0D0/(lambda**4)))+ &
   &   (self%coeffs(5)*(1.0D0/(lambda**6)))+ &
   &   (self%coeffs(6)*(1.0D0/(lambda**8))))

 end function

 function calcPNSC(self, lambda) result(n)
  class(glassdb) :: self
  real(kind=real64), intent(in) :: lambda
  real(kind=real64) :: n

  n = DSQRT(((self%coeffs(1)*(lambda**2))/((lambda**2)-self%coeffs(4)))+ &
  &  ((self%coeffs(2)*(lambda**2))/((lambda**2)-self%coeffs(5)))+ &
  &  ((self%coeffs(3)*(lambda**2))/((lambda**2)-self%coeffs(6)))+1.0D0)

 end function
 
 function calcIndexForCurrentGlass(self, lambda) result(n_l)
  class(glassdb) :: self
  real(kind=real64), intent(in) :: lambda
  real(kind=real64) :: n_l

  if(lambda == 0 ) then
    n_l = 1.0
    return
  end if

  select case(self%dataType)
   case(ID_PN)
    n_l = self%calcPN(lambda)

   case(ID_PNSC)
    n_l = self%calcPNSC(lambda)

  end select

 end function

 function isGlassInCatalog(self, strName, gdb_loc) result(boolResult)
  ! Note.  Needs some refactoring to separate opening file
  use iso_fortran_env, only: real64
  use type_utils, only: real2str
  implicit none
  class(glassdb) :: self
  character(len=*) :: strName
  integer, intent(in) :: gdb_loc
  logical :: boolResult
  integer :: TOTAL, J, uG
  character(len=1024) :: candCatalog

  REAL(kind=real64) :: A0,A1,A2,A3,A4,A5
  CHARACTER(len=13) NAME,NUMBER
  logical :: EXIS36

  INCLUDE 'DATMAI.INC'

  boolResult = .FALSE.

  candCatalog = trim(LIBGLA)//self%catalogFileNames(gdb_loc)

  EXIS36=.FALSE.
  INQUIRE(FILE=trim(candCatalog),EXIST=EXIS36)
  IF(EXIS36) THEN

    call OPENGLASSFILE(trim(candCatalog), uG, TOTAL)
    READ(UNIT=36,REC=1) TOTAL
    DO J=2,TOTAL+1
      ! TODO:  Make this a type to simplify code
      READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
      IF(strName.EQ.NAME) THEN
        boolResult = .TRUE.
        call LogTermFOR("Found "//strName//" eq "//NAME)
        call LogTermFOR("Old Coeff is "//real2str(self%coeffs(1)))
        self%coeffs(1) = A0
        self%coeffs(2) = A1
        self%coeffs(3) = A2
        self%coeffs(4) = A3
        self%coeffs(5) = A4
        self%coeffs(6) = A5
        call LogTermFOR("New Coeff is "//real2str(self%coeffs(1)))
        
        self%dataType = self%catalogDataTypes(gdb_loc)

        close(unit=36)
        return

      END IF
    END DO
  ELSE
    call LogTermFOR("Warning:  Unable to open "//trim(candCatalog)//".  Moving to next catalog")
  END IF

 end function

 function isGlassInAnyCatalog(self, strName, gdb_loc) result(boolResult)
    
    implicit none
    class(glassdb) :: self
    character(len=*) :: strName
    integer, intent(inout), optional :: gdb_loc
    logical :: boolResult
    integer :: i
    

    boolResult = .FALSE.
 

    do i=1,size(self%catalogs)
      if (self%isGlassInCatalog(strName, i)) then
        boolResult = .TRUE.
        if(present(gdb_loc)) gdb_loc = i
        return 
      end if
    end do


 end function

 function isNameInCatalog(self, strName, gdb_loc) result(boolResult)

  implicit none
  class(glassdb) :: self
  character(len=*) :: strName
  integer, intent(inout), optional :: gdb_loc
  logical :: boolResult
  integer :: i

  boolResult = .FALSE.
  !gdb_loc = -1

  do i=1,size(self%catalogs)
    if (strName.eq.self%catalogs(i)) then
      boolResult = .TRUE.
      if (present(gdb_loc)) gdb_loc = i
      return
    end if
  end do

 end function

 subroutine findCatalogNameFromGlassName(glassName, catalogName)
      implicit none
      
      character(len=13), intent(in) :: glassName
      character(len=13), intent(inout) :: catalogName
      real*8 :: A0,A1,A2,A3,A4,A5
      character(len=13) :: Name, Number
      integer :: i, m, uG, TOTAL, J

      include "DATMAI.INC"

      

      m = size(gdb%catalogs, DIM=1)
      PRINT *, "m is ", m
      do i=1,m
        call OPENGLASSFILE(trim(LIBGLA)//gdb%catalogFileNames(i), uG, TOTAL)
        PRINT *, trim(LIBGLA)//gdb%catalogFileNames(i)

        DO J=2,TOTAL+1
          READ(UNIT=uG,REC=J)Name,Number,A0,A1,A2,A3,A4,A5
        if (trim(Name).eq.glassName) then
          PRINT *, "Found Catalog! "
          catalogName = gdb%catalogs(i)
          CLOSE(uG)
          return
        end if

      end DO
    end do

 end subroutine

end module
