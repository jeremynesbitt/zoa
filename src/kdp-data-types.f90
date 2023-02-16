module kdp_data_types
     integer, parameter :: ASPH_NON_TORIC    = 1
     integer, parameter :: ASPH_TORIC_AXIS_Y = 2
     integer, parameter :: ASPH_TORIC_AXIS_X = 3


type sys_config

    integer :: imgSurface
  contains
    procedure, public, pass(self) :: getImageSurface
    procedure, public, pass(self) :: isFocalSystem
    procedure, public, pass(self) :: isTelecentric

end type

type lens_data

  integer num_surfaces, ref_stop
  real, allocatable :: radii(:), thicknesses(:), surf_index(:), surf_vnum(:), &
  & curvatures(:)
  character(:), allocatable :: glassnames(:)

contains
  procedure, public, pass(self) :: set_num_surfaces
  procedure, public, pass(self)  :: add_lens_data
  procedure, public, pass(self) :: imageSurfaceIsIdeal
  !procedure, private, pass(self) ::

end type lens_data

type, extends(lens_data) :: aspheric_surf_data
    integer, allocatable :: surface_type(:)
    logical, allocatable :: advancedSurf(:)
    real, dimension(:), allocatable :: conic_constant, toric_conic_constant
    !real :: conic_constant, toric_conic_constant
    real, dimension(:,:), allocatable :: asphereTerms !Second dim is 10
    real, dimension(:,:), allocatable  :: anamorphicTerms !Second term is 4

  contains
    procedure, public, pass(self) :: updateAsphereTable


end type

 type, extends(lens_data) :: paraxial_ray_trace_data
           !integer, allocatable :: surface(:)
           real, allocatable ::  marginal_ray_height(:), marginal_ray_angle(:), &
           & chief_ray_height(:), chief_ray_angle(:), marginal_ray_aoi(:), &
           & chief_ray_aoi(:)
           real :: t_mag = 0
    contains



end type paraxial_ray_trace_data



interface lens_data
  module procedure :: lens_data_constructor
end interface lens_data

contains

type(lens_data) function lens_data_constructor() result(self)
    self%num_surfaces = -1

end function lens_data_constructor

subroutine getImageSurface(self, intSurf)
    class(sys_config), intent(inout) :: self
    integer, intent(inout) :: intSurf
    include "DATMAI.INC"
    intSurf = NEWIMG

end subroutine

subroutine set_num_surfaces(self, input)

  class(lens_data), intent(inout) :: self
  integer, intent(in) :: input
  if (input.ne.self%num_surfaces.and.allocated(self%radii)) THEN
    PRINT *, "NEED TO DEALLOCATE?"
    DEALLOCATE(self%radii)
    DEALLOCATE(self%thicknesses)
    DEALLOCATE(self%curvatures)
    DEALLOCATE(self%surf_index)
    DEALLOCATE(self%surf_vnum)
    DEALLOCATE(self%glassnames)
  end if


  self%num_surfaces = input
  if (.not.allocated(self%radii)) THEN
    allocate(self%radii(self%num_surfaces), self%thicknesses(self%num_surfaces))
    allocate(self%curvatures(self%num_surfaces))

    allocate(self%surf_index(self%num_surfaces), self%surf_vnum(self%num_surfaces))
    allocate(character(40) :: self%glassnames(self%num_surfaces))
  END IF
end subroutine set_num_surfaces



subroutine add_lens_data(self, lens_data_obj)
   class(lens_data) :: self
   type(lens_data) lens_data_obj

   self%num_surfaces = lens_data_obj%num_surfaces
   self%ref_stop = lens_data_obj%ref_stop


select type (self)
type is (lens_data)
        ! no further initialization required
    class is (paraxial_ray_trace_data)
      !  PRINT *, "Adding to paraxial object!"
      if (.not. allocated(self%marginal_ray_height)) THEN
        allocate(self%marginal_ray_height(self%num_surfaces))
        allocate(self%marginal_ray_angle(self%num_surfaces))
        allocate(self%chief_ray_height(self%num_surfaces))
        allocate(self%chief_ray_angle(self%num_surfaces))
        allocate(self%chief_ray_aoi(self%num_surfaces))
        allocate(self%marginal_ray_aoi(self%num_surfaces))
      end if
    class default
      ! give error for unexpected/unsupported type
         stop 'initialize: unexpected type for sh object!'
    end select

 end subroutine add_lens_data

function imageSurfaceIsIdeal(self) result(boolResult)
    class(lens_data) :: self
    logical :: boolResult

    !  IF(GLANAM(INT(SYSTEM(20))-1,2).EQ.'PERFECT      ' &
    !  .OR.GLANAM(INT(SYSTEM(20))-1,2).EQ.'IDEAL        ') THEN

    boolResult = .FALSE.

  end function

function isFocalSystem(self) result(boolResult)

  class(sys_config) :: self
  logical :: boolResult

  include "DATLEN.INC"

  boolResult = .FALSE.
  IF(SYSTEM(30).EQ.1.0D0.OR.SYSTEM(30).EQ.2.0D0) boolResult = .TRUE.

end function

function isTelecentric(self) result(boolResult)
  class(sys_config) :: self
  logical :: boolResult

  boolResult = .FALSE.

end function

 subroutine updateAsphereTable(self, maxSurf)
   class(aspheric_surf_data), intent(inout) :: self

   integer :: I, maxSurf

   include "DATLEN.INC"

   if (allocated(self%conic_constant)) deallocate(self%conic_constant)
   allocate(self%conic_constant(maxSurf))
   if (allocated(self%asphereTerms)) deallocate(self%asphereTerms)
   allocate(self%asphereTerms(maxSurf,8))


   ! Logic taken from the LOADSHEET.INC routine.
   do I = 0,maxSurf

        IF(ALENS(1,I).EQ.0.0D0.AND.ALENS(2,I).NE.0.0D0) THEN
                        ALENS(2,I)=0.0D0
                        END IF
        IF(ALENS(1,I).NE.0.0D0.AND.ALENS(43,I).NE.0.0D0) THEN
                        ALENS(43,I)=0.0D0
                        END IF
        IF(ALENS(4,I).EQ.0.0D0.AND.ALENS(5,I).EQ.0.0D0.AND.     &
     &  ALENS(6,I).EQ.0.0D0.AND.ALENS(7,I).EQ.0.0D0.AND.        &
     &  ALENS(81,I).EQ.0.0D0.AND.ALENS(82,I).EQ.0.0D0.AND.      &
     &  ALENS(83,I).EQ.0.0D0.AND.ALENS(84,I).EQ.0.0D0.AND.      &
     &  ALENS(85,I).EQ.0.0D0.AND.ALENS(43,I).EQ.0.0D0) THEN
                        ALENS(8,I)=0.0D0
                        END IF
        IF(ALENS(23,I).NE.0.0D0.OR.ALENS(8,I).NE.0.0D0.OR.      &
     &  ALENS(2,I).NE.0.0D0.OR.ALENS(4,I).NE.0.0D0.OR.          &
     &  ALENS(5,I).NE.0.0D0.OR.ALENS(6,I).NE.0.0D0.OR.          &
     &  ALENS(7,I).NE.0.0D0.OR.ALENS(36,I).NE.0.0D0.OR.         &
     &  ALENS(37,I).NE.0.0D0.OR.ALENS(38,I).NE.0.0D0.OR.        &
     &  ALENS(39,I).NE.0.0D0.OR.ALENS(40,I).NE.0.0D0.OR.        &
     &  ALENS(41,I).NE.0.0D0.OR.ALENS(43,I).NE.0.0D0.OR.        &
     &  ALENS(81,I).NE.0.0D0.OR.ALENS(82,I).NE.0.0D0.OR.        &
     &  ALENS(83,I).NE.0.0D0.OR.ALENS(85,I).NE.0.0D0.OR.        &
     &  ALENS(85,I).NE.0.0D0) THEN
        PRINT *, "FOUND ASPHERE AT index ", I
        self%conic_constant(I+1) = ALENS(2,I)
        !PRINT *, ALENS(1:85,I)
        self%asphereTerms(I+1,1:4) = ALENS(4:7,I)
        self%asphereTerms(I+1,5:8) = ALENS(81:84,I)
        !PRINT *, "ASPHRE TErms ", self%asphereTerms(I+1,:)
        PRINT "(E10.4)", self%asphereTerms(I+1,1)
      else
        self%conic_constant(I+1) = 0.0
        self%asphereTerms(I+1,:) = 0.0
       END IF
     end do


     !PRINT *, "Conic Constant Array is ", self%conic_constant

   end subroutine

end module kdp_data_types
