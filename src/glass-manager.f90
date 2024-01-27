! Currently most clast routines are in .FOR files like LDM13.
! Would like to eventually migrate functionality to this module
! to facilitate supporting different glasses

module glass_manager

type :: glassdb
  character(len=13), allocatable :: catalogs(:)
  character(len=19), allocatable :: catalogFileNames(:)


end type

interface glassdb
  module procedure glassdb_constructor
end interface

contains

 type(glassdb) function glassdb_constructor() result(self)
     allocate(character(len=13) :: self%catalogs(9))
     allocate(character(len=19) :: self%catalogFileNames(9))

     self%catalogs(1) = 'SCHOTT'
     self%catalogFileNames(1) = 'SCHOTT.BIN'
     self%catalogs(2) = 'SCH2000'
     self%catalogFileNames(2) = 'SCHOTT2000.BIN'
     self%catalogs(3) = 'OHARA'
     self%catalogFileNames(3) = 'OHARA.BIN'
     self%catalogs(4) = 'OHARA'
     self%catalogFileNames(4) = 'OHARA-O.BIN'
     self%catalogs(5) = 'HOYA'
     self%catalogFileNames(5) = 'HOYA.BIN'
     self%catalogs(6) = 'CHANCE'
     self%catalogFileNames(6) = 'CHANCE.BIN'
     self%catalogs(7) = 'CORNIN'
     self%catalogFileNames(7) = 'CORNIN.BIN'
     self%catalogs(8) = 'HIKARI'
     self%catalogFileNames(8) = 'HIKARI.BIN'
     self%catalogs(9) = 'RADHARD'
     self%catalogFileNames(9) = 'RADHARD.BIN'


 end function

 subroutine parseModelGlassEntry(modelGlass, nd, vd)
  use type_utils, only: str2int
  character(len=*) :: modelGlass
  real*8, intent(inout) :: nd, vd
  character(len=3) :: nStr, vStr
  
  
  ! Format is:  MXYZ.ABC
  ! XYZ is index = 1
  ! ABC is abbe number *10
  nStr = modelGlass(2:5)
  vStr = modelGlass(6:8)

  nd = 1+ REAL(str2int(nStr))/1000.0
  vd = REAL(str2int(vStr))/10.0


 end subroutine

 subroutine findCatalogNameFromGlassName(glassName, catalogName)
      implicit none
      type(glassdb) :: gdb
      character(len=13), intent(in) :: glassName
      character(len=13), intent(inout) :: catalogName
      real*8 :: A0,A1,A2,A3,A4,A5
      character(len=13) :: Name, Number
      integer :: i, m, uG, TOTAL, J

      include "DATMAI.INC"

      gdb = glassdb()

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
