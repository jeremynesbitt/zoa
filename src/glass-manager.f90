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
  procedure, private, pass(self) :: isGlassInSpecialCatalog

end type

interface glassdb
  module procedure glassdb_constructor
end interface

type specialCatalog
   character(len=40) :: names(9) ! TODO:  Define this in constructor     
   character(len=40) :: currName

   contains
   procedure, public, pass(self) :: calcSpecialIndex
   procedure, private, pass(self) :: interpolateIndexData

end type

interface specialCatalog
  module procedure specialCatalog_constructor
end interface

type(glassdb) :: gdb
integer, parameter ::  ID_PN   = 1
integer, parameter ::  ID_PNSC = 2
integer, parameter ::  ID_SPECIAL = 3
type(specialCatalog) :: specCat





contains

 type(glassdb) function glassdb_constructor() result(self)
     integer, parameter :: numC = 11
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

     self%catalogs(11) = 'SPECIAL'
     self%catalogFileNames(11) = ''
     self%catalogDescriptions(11) = 'Special Material Catalog'
     self%catalogDataTypes(11) = ID_SPECIAL

     specCat = specialCatalog_constructor()

 end function

 type(specialCatalog) function specialCatalog_constructor() result(self)
    
    ! I did not convert all the original KDP special glasses
    ! here.  I just did a few to test it out
    ! I am not sure this is the long term best way to store this info
    ! so didn't want to do a bunch of converting only to convert again
    self%names(1) = 'WATER'
    self%names(2) = 'ACRYLIC'
    self%names(3) = 'POLYSTYRENE'
    self%names(4) = 'POLYCARBONATE'
    self%names(5) = 'SAN'
    self%names(6) = 'SILICON'
    self%names(7) = 'SILICA'
    self%names(8) = 'SAPPHIRE'
    self%names(9) = 'DIAMOND'



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
  !call LogTermFOR("DEBUG: ModelGlass Start is "// modelGlass(1:dotLoc-1))
  vStr = modelGlass(dotLoc+1:len(modelGlass))

  
  ! Divide the number to the left of the . s.t. it is < 1.  Then add one to it to make the index
  !PRINT *, "Num is ", REAL(str2int(nStr))
  !PRINT *, "Dem is ", (10**(dotLoc-2))
  nd = 1.0+ REAL(str2int(nStr))/(10**(dotLoc-1))
  vd = REAL(str2int(vStr))/10.0
  !PRINT *, "nd is ", nd
  !PRINT *, "vd is ", vd



 end subroutine

 function interpolateIndexData(self, lambda, X,Y) result(n)
  class(specialCatalog) :: self
  real(kind=real64) :: lambda
  real(kind=real64) :: n
  real(kind=real64) :: X(:), Y(:)
  real(kind=real64) :: Y2(100)



  CALL SPLINE(X,Y,size(X),1.0D35,1.0D35,Y2)    

  if(lambda == 0.0) then
    n = 1.0
  else
     call SPLINT(X,Y,Y2,size(X),lambda,n)
  end if

 end function

 function calcSpecialIndex(self, lambda) result(n)
  ! This is to replace the original SPCGL function so all materials
  ! can be treated the same, simplifying the code and making it easier
  ! to add new glasses
  implicit none

  class(specialCatalog) :: self
  real(kind=real64), intent(in) :: lambda
  real(kind=real64) :: n

  REAL(kind=real64) :: X(1:100),Y(1:100)
  integer :: NUM, J


  select case(self%currName)
    ! Names must be defined in constructor first!

  case('ACRYLIC2')
    NUM = 13

    X(1:NUM) = (/0.36501D0,0.40466D0,0.43484D0 &
    &,0.47999D0,0.48613D0,0.54607D0,0.58756D0,0.58929D0,0.64385D0 &
    &,0.65627D0,0.70652D0,0.85211D0,1.01398D0/)

    Y(1:NUM) = (/1.513613D0,1.506607D0,1.502557D0 &
    &,1.498258D0,1.497760D0,1.493795D0,1.491757D0,1.491681D0 &
    &,1.489603D0,1.489201D0,1.487552D0,1.484965D0,1.483115D0/)   

    if(lambda == 0.0) then
      n = 1.0
    else
      n = self%interpolateIndexData(lambda, X(1:NUM), Y(1:NUM))
       !call SPLINT(X,Y,Y2,NUM,lambda,n)
    end if    

  case('POLYSTYRENE2')
    NUM = 13

    X(1:NUM) = (/0.36501D0,0.40466D0,0.43484D0 &
    &,0.47999D0,0.48613D0,0.54607D0,0.58756D0,0.58929D0,0.64385D0 &
    &,0.65627D0,0.70652D0,0.85211D0,1.01398D0/)

    Y(1:NUM) = (/1.643126D0,1.625341D0,1.615466D0 &
    ,1.605241D0,1.604079D0,1.595010D0,1.590481D0,1.590315D0 &
    ,1.585808D0,1.584949D0,1.581954D0,1.576196D0,1.572553D0/)    
      
    if(lambda == 0.0) then
        n = 1.0
      else
        n = self%interpolateIndexData(lambda, X(1:NUM), Y(1:NUM))
         !call SPLINT(X,Y,Y2,NUM,lambda,n)
      end if

  case('POLYCARBONATE2')

    NUM = 13
    
    X(1:NUM) = (/0.36501D0,0.40466D0,0.43484D0 &
    &,0.47999D0,0.48613D0,0.54607D0,0.58756D0,0.58929D0,0.64385D0 &
    &,0.65627D0,0.70652D0,0.85211D0,1.01398D0/) 

    Y(1:NUM) = (/1.643231D0,1.622447D0,1.611519D0 &
    ,1.600654D0,1.599439D0,1.590081D0,1.585470D0,1.585302D0 &
    ,1.580734D0,1.579864D0,1.576831D0,1.570981D0,1.567248D0/)  
  
    if(lambda == 0.0) then
      n = 1.0
    else
      n = self%interpolateIndexData(lambda, X(1:NUM), Y(1:NUM))
       !call SPLINT(X,Y,Y2,NUM,lambda,n)
    end if

  case('SAN2')

    NUM = 13

    X(1:NUM) = (/0.36501D0,0.40466D0,0.43484D0 &
    &,0.47999D0,0.48613D0,0.54607D0,0.58756D0,0.58929D0,0.64385D0 &
    &,0.65627D0,0.70652D0,0.85211D0,1.01398D0/)
    
    Y(1:NUM) = (/1.612490D0,1.597075D0,1.588640D0 &
    ,1.579985D0,1.579000D0,1.571300D0,1.567400D0,1.567298D0 &
    ,1.563438D0,1.562700D0,1.560119D0,1.555108D0,1.551870D0/)

    if(lambda == 0.0) then
      n = 1.0
    else
      n = self%interpolateIndexData(lambda, X(1:NUM), Y(1:NUM))
       !call SPLINT(X,Y,Y2,NUM,lambda,n)
    end if

  case('SILICON2')
    if(lambda == 0.0) then
      n = 1.0
    else
      ! Silicon interpolation formula
      n = &
      3.41696D0+(0.138497D0/((lambda**2)-0.028D0)) &
      +(0.013924D0/(((lambda**2)-0.028D0)**2))-(0.0000209D0*(lambda**2)) &
     +(0.000000148D0*(lambda**4))
    end if

  case('SILICA2')
    if(lambda == 0.0) then
      n = 1.0
    else
       !     SILICA/SIO2 INTERPOLATION FORMULA (HANDBOOK OF OPTICS 11/14/2000)
      n = DSQRT(1.0D0+ &
      ((0.6961663*(lambda**2))/((lambda**2)-((0.0684043)**2)))+ &
      ((0.4079426*(lambda**2))/((lambda**2)-((0.1162414)**2)))+ &
      ((0.8974794*(lambda**2))/((lambda**2)-((9.896161)**2))) &
      )
    end if    

  case('DIAMOND2')
    if(lambda == 0.0) then
      n = 1.0
    else
!     DIAMOND INTERPOLATION FORMULA
      n= &
      2.37837D0+ &
      ((1.18897E-2)*(1.0D0/((lambda**2)-0.028D0)))+ &
      ((-1.0083E-4)*((1.0D0/((lambda**2)-0.028D0))**2))+ &
      ((-2.3676E-5)*(lambda**2))+ &
      ((3.24263E-8)*(lambda**4))    
    end if      
    
  case('LIF')
    if(lambda == 0.0) then
      n = 1.0
    else
!     LiF INTERPOLATION FORMULA
      n = &
       1.38761D0+(0.001796D0/((lambda**2)-0.028D0)) &
       +(0.000041D0/(((lambda**2)-0.028D0)**2))-(0.0023045D0*(lambda**2)) &
      -(0.00000557D0*(lambda**4))
    end if         

  case('KBR')
    if(lambda == 0.0) then
      n = 1.0
    else
!     
      n =  DSQRT( &
      2.361323D0-(3.11497D-4*(lambda**2))-(5.8613D-8*(lambda**4))+ &
      (0.007676D0/(lambda**2))+(0.0156569D0/((lambda**2)-0.0324D0)))
    end if         

  case('SAPPHIRE')
    if(lambda == 0.0) then
      n = 1.0
    else
!     
      n =  DSQRT( &
      ((1.023798D0*(lambda**2))/((lambda**2)-(0.00377588D0)))+ &
      ((1.058264D0*(lambda**2))/((lambda**2)-(0.01225440D0)))+ &
      ((5.280792D0*(lambda**2))/((lambda**2)-(321.361600D0)))+1.0D0)
    end if         


  case('WATER')
!    INTERPOLATE THE H2O
               NUM=21

!     DATA FOR H2O

      X(1:NUM) = (/0.1829D0,0.20255D0,0.25020D0,0.30822D0, &
    & 10.35871D0,0.40466D0,0.44715D0,0.50157D0,0.54607D0,0.58926D0, &
    & 20.65628D0,0.70652D0,0.76820D0,0.808D0,0.871D0,0.943D0,1.028D0, &
    & 31.130D0,1.256D0,1.617D0,1.968D0/)

      Y(1:NUM) = (/1.46379D0,1.41993D0,1.37734D0,1.35671D0, &
    & 11.34795D0,1.342724D0,1.339423D0,1.336363D0,1.334466D0,1.332988D0, &
    & 21.331151D0,1.330019D0,1.32890D0,1.3286D0,1.3273D0,1.3262D0, &
    & 31.3250D0,1.3234D0,1.3215D0,1.3149D0,1.3078D0/)


      if(lambda == 0.0) then
        n = 1.0
      else
        n = self%interpolateIndexData(lambda, X(1:NUM), Y(1:NUM))
         !call SPLINT(X,Y,Y2,NUM,lambda,n)
      end if

  case default
    n = 1.0
    call LogTermFOR("Data for Glass Name "//trim(self%currName)//" not found!")

  end select


 end function

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

   case(ID_SPECIAL)
    n_l = specCat%calcSpecialIndex(lambda)

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
  
  ! If no file name, check if this is a special catalog
  if (self%catalogFileNames(gdb_loc) == ' ') then
     boolResult = self%isGlassInSpecialCatalog(strName, gdb_loc)
     if(boolResult) self%dataType = self%catalogDataTypes(gdb_loc)
  else

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
        !call LogTermFOR("Found "//strName//" eq "//NAME)
        self%coeffs(1) = A0
        self%coeffs(2) = A1
        self%coeffs(3) = A2
        self%coeffs(4) = A3
        self%coeffs(5) = A4
        self%coeffs(6) = A5

        self%dataType = self%catalogDataTypes(gdb_loc)

        close(unit=36)
        return

      END IF
    END DO
  ELSE
    call LogTermFOR("Warning:  Unable to open "//trim(candCatalog)//".  Moving to next catalog")
  END IF
  end if

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

 function isGlassInSpecialCatalog(self, strName, gdb_loc) result(boolResult)
  implicit none
  class(glassdb) :: self
  character(len=*) :: strName
  integer, intent(in) :: gdb_loc
  logical :: boolResult
  integer :: i

   boolResult = .FALSE.

   do i=1,size(specCat%names)
    if (strName == specCat%names(i)) then
      !call LogTermFOR("Found Special Glass!")
      specCat%currName = specCat%names(i)
      boolResult = .TRUE.
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
      logical :: EXIS36

      include "DATMAI.INC"

      

      m = size(gdb%catalogs, DIM=1)
      PRINT *, "m is ", m
      do i=1,m
        EXIS36=.FALSE.
        INQUIRE(FILE=trim(LIBGLA)//gdb%catalogFileNames(i),EXIST=EXIS36)
        IF(EXIS36) THEN        
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
    ELSE 
      call LogTermFOR("Unable to open "//trim(LIBGLA)//gdb%catalogFileNames(i))
    END IF
    end do

 end subroutine

end module
