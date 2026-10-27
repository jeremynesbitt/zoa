! The glass database the legacy code calls (gdb): catalog names and the
! lookup / index calculation of a catalog glass.  The catalogs themselves are
! the AGF files of module glass_catalog; this type keeps the names, files
! and descriptions in the arrays the legacy code indexes, rebuilt whenever
! glass_catalog reloads.  The SPECIAL materials (formulas and tables below)
! are still built in and come last in the search order.

module glass_manager
  use iso_fortran_env, only: real64
  private :: gm_upper

type :: glassdb
  character(len=32), allocatable :: catalogs(:)
  character(len=32), allocatable :: catalogFileNames(:)
  character(len=200), allocatable :: catalogDescriptions(:)
  integer, allocatable :: catalogDataTypes(:)

  ! the glass found by the last successful isGlassInCatalog/isGlassInAnyCatalog
  integer :: dataType = 0
  integer :: curCat = 0, curGlass = 0
  integer :: generation = -1

  contains
  procedure, public, pass(self) :: sync
  procedure, public, pass(self) :: isNameInCatalog
  procedure, public, pass(self) :: isGlassInCatalog
  procedure, public, pass(self) :: isGlassInAnyCatalog
  procedure, public, pass(self) :: calcIndexForCurrentGlass
  procedure, public, pass(self) :: currentWavelengthRange
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
integer, parameter ::  ID_AGF = 1
integer, parameter ::  ID_SPECIAL = 3
type(specialCatalog) :: specCat





contains

 type(glassdb) function glassdb_constructor() result(self)
   call self%sync()
   specCat = specialCatalog_constructor()
 end function

 ! Rebuild the catalog arrays if glass_catalog has (re)loaded.  Called by
 ! every method; legacy code that loops over gdb%catalogs directly calls it
 ! first.
 subroutine sync(self)
   use glass_catalog, only: gc_generation, gc_num_catalogs, gc_catalog_name, &
                            gc_catalog_comment, gc_catalog_path
   class(glassdb) :: self
   integer :: n, i, gen, k
   character(len=1024) :: path

   gen = gc_generation()
   if (gen == self%generation .and. allocated(self%catalogs)) return
   n = gc_num_catalogs()
   if (allocated(self%catalogs)) deallocate(self%catalogs, self%catalogFileNames, &
       self%catalogDescriptions, self%catalogDataTypes)
   allocate(self%catalogs(n+1), self%catalogFileNames(n+1), &
            self%catalogDescriptions(n+1), self%catalogDataTypes(n+1))
   do i = 1, n
     self%catalogs(i) = gc_catalog_name(i)
     path = gc_catalog_path(i)
     k = max(index(path, '/', back=.true.), index(path, '\', back=.true.))
     self%catalogFileNames(i) = path(k+1:)
     self%catalogDescriptions(i) = gc_catalog_comment(i)
     if (len_trim(self%catalogDescriptions(i)) == 0) &
       self%catalogDescriptions(i) = trim(self%catalogs(i))//' glass catalog'
     self%catalogDataTypes(i) = ID_AGF
   end do
   self%catalogs(n+1) = 'SPECIAL'
   self%catalogFileNames(n+1) = ''
   self%catalogDescriptions(n+1) = 'Special Material Catalog'
   self%catalogDataTypes(n+1) = ID_SPECIAL
   self%generation = gen
   self%curCat = 0; self%curGlass = 0
 end subroutine

 function calcIndexForCurrentGlass(self, lambda) result(n_l)
  use glass_catalog, only: gc_index
  class(glassdb) :: self
  real(kind=real64), intent(in) :: lambda
  real(kind=real64) :: n_l

  n_l = 1.0_real64
  if (lambda == 0) return
  select case (self%dataType)
  case (ID_AGF)
    if (self%curCat > 0) n_l = gc_index(self%curCat, self%curGlass, lambda)
  case (ID_SPECIAL)
    n_l = specCat%calcSpecialIndex(lambda)
  end select
 end function

 ! Wavelength range (um) of the current catalog glass; .false. for a
 ! SPECIAL material (its range is still LAMHILO's).
 logical function currentWavelengthRange(self, lmin, lmax) result(ok)
  use glass_catalog, only: glass_record, gc_glass
  class(glassdb) :: self
  real(kind=real64), intent(out) :: lmin, lmax
  type(glass_record) :: g
  ok = .false.; lmin = 0.0_real64; lmax = 1.0e10_real64
  if (self%dataType /= ID_AGF .or. self%curCat < 1) return
  g = gc_glass(self%curCat, self%curGlass)
  lmin = g%lmin; lmax = g%lmax
  ok = .true.
 end function

 ! Glass strName in catalog number gdb_loc; on success it becomes the
 ! current glass.  byCode also accepts the glass code (GLASSP, GLSRIN).
 function isGlassInCatalog(self, strName, gdb_loc, byCode) result(boolResult)
  use glass_catalog, only: gc_find_glass
  class(glassdb) :: self
  character(len=*) :: strName
  integer, intent(in) :: gdb_loc
  logical, intent(in), optional :: byCode
  logical :: boolResult
  integer :: ig

  call self%sync()
  boolResult = .false.
  if (gdb_loc < 1 .or. gdb_loc > size(self%catalogs)) return
  if (self%catalogDataTypes(gdb_loc) == ID_SPECIAL) then
    boolResult = self%isGlassInSpecialCatalog(strName, gdb_loc)
    if (boolResult) then
      self%dataType = ID_SPECIAL; self%curCat = 0; self%curGlass = 0
    end if
    return
  end if
  if (gc_find_glass(gdb_loc, trim(strName), ig, byCode)) then
    boolResult = .true.
    self%dataType = ID_AGF
    self%curCat = gdb_loc
    self%curGlass = ig
  end if
 end function

 function isGlassInAnyCatalog(self, strName, gdb_loc, byCode) result(boolResult)
    class(glassdb) :: self
    character(len=*) :: strName
    integer, intent(inout), optional :: gdb_loc
    logical, intent(in), optional :: byCode
    logical :: boolResult
    integer :: i

    call self%sync()
    boolResult = .FALSE.
    do i=1,size(self%catalogs)
      if (self%isGlassInCatalog(strName, i, byCode)) then
        boolResult = .TRUE.
        if(present(gdb_loc)) gdb_loc = i
        return
      end if
    end do
 end function

 function isGlassInSpecialCatalog(self, strName, gdb_loc) result(boolResult)
  class(glassdb) :: self
  character(len=*) :: strName
  integer, intent(in) :: gdb_loc
  logical :: boolResult
  integer :: i

   boolResult = .FALSE.
   do i=1,size(specCat%names)
    if (strName == specCat%names(i)) then
      specCat%currName = specCat%names(i)
      boolResult = .TRUE.
      return
    end if
   end do
 end function

 ! Is strName a catalog name (case-insensitive)?  gdb_loc = its number.
 function isNameInCatalog(self, strName, gdb_loc) result(boolResult)
  class(glassdb) :: self
  character(len=*) :: strName
  integer, intent(inout), optional :: gdb_loc
  logical :: boolResult
  integer :: i
  character(len=32) :: u

  call self%sync()
  boolResult = .FALSE.
  u = gm_upper(adjustl(strName))
  do i=1,size(self%catalogs)
    if (u == self%catalogs(i)) then
      boolResult = .TRUE.
      if (present(gdb_loc)) gdb_loc = i
      return
    end if
  end do
 end function

 ! Catalog of the first catalog holding glassName ('' if none).
 subroutine findCatalogNameFromGlassName(glassName, catalogName)
   character(len=*), intent(in) :: glassName
   character(len=*), intent(inout) :: catalogName
   integer :: loc
   loc = 0
   if (gdb%isGlassInAnyCatalog(trim(glassName), loc)) then
     catalogName = gdb%catalogs(loc)
   else
     catalogName = ''
   end if
 end subroutine

 function gm_upper(s) result(u)
   character(len=*), intent(in) :: s
   character(len=len(s)) :: u
   integer :: i, c
   u = s
   do i = 1, len(u)
     c = iachar(u(i:i))
     if (c >= 97 .and. c <= 122) u(i:i) = achar(c - 32)
   end do
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
  real(real64), intent(inout) :: nd, vd
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
   use iso_fortran_env, only: real64
  implicit none

  class(specialCatalog) :: self
  real(kind=real64), intent(in) :: lambda
  real(kind=real64) :: n

  REAL(kind=real64) :: X(1:100),Y(1:100)
  integer :: NUM, J


  select case(self%currName)
    ! Names must be defined in constructor first!

  case('ACRYLIC')
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

  case('POLYSTYRENE')
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

  case('POLYCARBONATE')

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

  case('SAN')

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

  case('SILICON')
    if(lambda == 0.0) then
      n = 1.0
    else
      ! Silicon interpolation formula
      n = &
      3.41696D0+(0.138497D0/((lambda**2)-0.028D0)) &
      +(0.013924D0/(((lambda**2)-0.028D0)**2))-(0.0000209D0*(lambda**2)) &
     +(0.000000148D0*(lambda**4))
    end if

  case('SILICA')
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

  case('DIAMOND')
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


  case('WATERKDP')
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

  case('WATER')


      if(lambda == 0.0) then
        n = 1.0
      else
      n =  DSQRT( &
      ((.7592504D0*(lambda**2))/((lambda**2)-(0.09568208D0)**2))+ &
      ((.02429797D0*(lambda**2))/((lambda**2)-(1.789668D0)**2))+1.0D0)
         !call SPLINT(X,Y,Y2,NUM,lambda,n)
      end if



  case default
    n = 1.0
    call LogTermUser("Data for Glass Name "//trim(self%currName)//" not found!")

  end select


 end function

end module
