! The glass database the legacy code calls (gdb): catalog names and the
! lookup / index calculation of a catalog glass.  The catalogs themselves are
! the AGF files of module glass_catalog; this type keeps the names, files
! and descriptions in the arrays the legacy code indexes, rebuilt whenever
! glass_catalog reloads.

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

end type

interface glassdb
  module procedure glassdb_constructor
end interface

type(glassdb) :: gdb
integer, parameter ::  ID_AGF = 1

! The last glass GLSRES found not defined across the system wavelengths
! (surface -1 if none since the last clear) -- for commands that update the
! lens silently (GLA) to report it.
integer :: rangeWarnSurf = -1
character(len=200) :: rangeWarnText = ''





contains

 type(glassdb) function glassdb_constructor() result(self)
   call self%sync()
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
   allocate(self%catalogs(n), self%catalogFileNames(n), &
            self%catalogDescriptions(n), self%catalogDataTypes(n))
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
  end select
 end function

 ! Wavelength range (um) of the current catalog glass (.false. if none).
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

 ! Is strName a catalog name or catalog alias (case-insensitive)?  gdb_loc =
 ! its number; gdb%catalogs(gdb_loc) is then the catalog's own name.
 function isNameInCatalog(self, strName, gdb_loc) result(boolResult)
  use glass_catalog, only: gc_find_catalog
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
  i = gc_find_catalog(u)       ! an alias (catalog numbers are the store's)
  if (i > 0) then
    boolResult = .TRUE.
    if (present(gdb_loc)) gdb_loc = i
  end if
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


 ! A glass code as a model glass: nnn.vvv or the 6-digit nnnvvv (e.g.
 ! 517642): nd = 1.nnn, vd = vv.v.  nd = vd = 0 if it is neither.
 recursive subroutine parseModelGlassEntry(modelGlass, nd, vd)
  use type_utils, only: str2int
  character(len=*) :: modelGlass
  integer :: dotLoc
  real(real64), intent(inout) :: nd, vd
  character(len=3) :: nStr, vStr
  character(len=len(modelGlass)) :: code

  dotLoc = INDEX(modelGlass, '.')
  if (dotLoc == 0) then
    nd = 0.0_real64; vd = 0.0_real64
    code = adjustl(modelGlass)
    if (len_trim(code) == 6 .and. verify(trim(code), '0123456789') == 0) &
      call parseModelGlassEntry(code(1:3)//'.'//code(4:6), nd, vd)
    return
  end if
  ! XYZ.ABC: XYZ is (index - 1)*1000, ABC is the Abbe number*10
  nStr = modelGlass(1:dotLoc-1)
  vStr = modelGlass(dotLoc+1:len(modelGlass))
  nd = 1.0+ REAL(str2int(nStr))/(10**(dotLoc-1))
  vd = REAL(str2int(vStr))/10.0
 end subroutine

end module
