module clear_apertures
    use iso_fortran_env, only: real64
    use mod_surface, only: &
        surf_clap_type,     set_surf_clap_type,   &
        surf_clap_dim,      set_surf_clap_dim,    &
        surf_clap_tilt,     set_surf_clap_tilt,   &
        surf_spider_flag,   set_surf_spider_flag, &
        surf_spider_arms,   set_surf_spider_arms, &
        surf_spider_angle,  set_surf_spider_angle,&
        surf_spider_width,  set_surf_spider_width
    implicit none
    private
    public :: spider_obscuration, clear_aperture

    ! Spider vane overlay descriptor.
    ! surf_spider_flag -> integer; surf_spider_arms/angle/width -> real(real64)
    type :: spider_obscuration
        logical      :: present = .false.   ! surf_spider_flag(s) /= 0
        integer      :: arms    = 0         ! surf_spider_arms (stored as real in ALENS)
        real(real64) :: angle   = 0.0_real64
        real(real64) :: width   = 0.0_real64
    end type

    ! Typed clear-aperture descriptor, mirroring ALENS(9..15, s).
    type :: clear_aperture
        integer      :: shape      = 0           ! ALENS(9): 0=none,1=circ,2=rect,3=ellip,4=racetrack,5=poly,6=ipoly
        real(real64) :: dim1       = 0.0_real64  ! ALENS(10)
        real(real64) :: dim2       = 0.0_real64  ! ALENS(11)
        real(real64) :: decenter_y = 0.0_real64  ! ALENS(12)
        real(real64) :: decenter_x = 0.0_real64  ! ALENS(13)
        real(real64) :: dim5       = 0.0_real64  ! ALENS(14)
        real(real64) :: tilt       = 0.0_real64  ! ALENS(15)
        type(spider_obscuration) :: spider
        real(real64) :: auto_semi_x = 0.0_real64  ! ray-traced extent: DISPLAY ONLY
        real(real64) :: auto_semi_y = 0.0_real64
        ! Edge (physical/mechanical) semi-aperture in Y -- larger than the clear
        ! aperture to allow coating roll-off, polishing, and mounting.  0 => unset
        ! (use the system default edge scale factor).  Set via `CIR EDG Sk v`.
        ! Persistent owner is the ldm (edge apertures are NOT an ALENS quantity);
        ! load_surfaces_from_alens re-populates this field after each rebuild.
        real(real64) :: semi_edge_y = 0.0_real64
    contains
        procedure :: from_alens
        procedure :: to_alens
        procedure :: is_set
        procedure :: display_semi_y
    end type

contains

    subroutine from_alens(self, s)
        class(clear_aperture), intent(inout) :: self
        integer, intent(in) :: s
        self%shape      = surf_clap_type(s)
        self%dim1       = surf_clap_dim(s, 1)
        self%dim2       = surf_clap_dim(s, 2)
        self%decenter_y = surf_clap_dim(s, 3)
        self%decenter_x = surf_clap_dim(s, 4)
        self%dim5       = surf_clap_dim(s, 5)
        self%tilt       = surf_clap_tilt(s)
        self%spider%present = surf_spider_flag(s) /= 0
        self%spider%arms    = nint(surf_spider_arms(s))
        self%spider%angle   = surf_spider_angle(s)
        self%spider%width   = surf_spider_width(s)
        ! auto_semi_x and auto_semi_y are not loaded from ALENS
    end subroutine from_alens

    subroutine to_alens(self, s)
        class(clear_aperture), intent(in) :: self
        integer, intent(in) :: s
        call set_surf_clap_type(s, self%shape)
        call set_surf_clap_dim(s, 1, self%dim1)
        call set_surf_clap_dim(s, 2, self%dim2)
        call set_surf_clap_dim(s, 3, self%decenter_y)
        call set_surf_clap_dim(s, 4, self%decenter_x)
        call set_surf_clap_dim(s, 5, self%dim5)
        call set_surf_clap_tilt(s, self%tilt)
        if (self%spider%present) then
            call set_surf_spider_flag(s, 1)
        else
            call set_surf_spider_flag(s, 0)
        end if
        call set_surf_spider_arms(s, real(self%spider%arms, real64))
        call set_surf_spider_angle(s, self%spider%angle)
        call set_surf_spider_width(s, self%spider%width)
        ! auto_semi_x and auto_semi_y are never written to ALENS
    end subroutine to_alens

    logical function is_set(self)
        class(clear_aperture), intent(in) :: self
        is_set = self%shape /= 0
    end function is_set

    real(real64) function display_semi_y(self)
        class(clear_aperture), intent(in) :: self
        if (self%is_set()) then
            if (self%dim2 /= 0.0_real64) then
                display_semi_y = self%dim2
            else
                display_semi_y = self%dim1
            end if
        else
            display_semi_y = self%auto_semi_y
        end if
    end function display_semi_y

end module clear_apertures
