module kdp_data_types

type lens_data

  integer num_surfaces, ref_stop
  real, allocatable :: radii(:), thicknesses(:), surf_index(:), surf_vnum(:), &
  & curvatures(:)
  character(:), allocatable :: glassnames(:)

contains
  procedure, public, pass(self) :: set_num_surfaces
  procedure, public, pass(self)  :: add_lens_data
  !procedure, private, pass(self) ::

end type lens_data

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

subroutine set_num_surfaces(self, input)

  class(lens_data), intent(inout) :: self
  integer, intent(in) :: input

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

end module kdp_data_types
