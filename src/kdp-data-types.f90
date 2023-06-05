module kdp_data_types
     integer, parameter :: ASPH_NON_TORIC    = 1
     integer, parameter :: ASPH_TORIC_AXIS_Y = 2
     integer, parameter :: ASPH_TORIC_AXIS_X = 3

     integer, parameter :: APER_ENTR_PUPIL_DIAMETER = 102
     integer, parameter :: APER_OBJECT_NA = 103
     integer, parameter :: APER_STOP_SURFACE = 104
     integer, PARAMETER :: APER_FNO = 105

     integer, parameter :: FIELD_OBJECT_HEIGHT = 202
     integer, parameter :: FIELD_OBJECT_ANGLE_DEG = 203
     integer, parameter :: FIELD_PARAX_IMAGE_HEIGHT = 204
     integer, parameter :: FIELD_PARAX_IMAGE_SLOPE_TAN = 205
     integer, PARAMETER :: FIELD_REAL_IMAGE_HEIGHT = 206
     integer, PARAMETER :: FIELD_REAL_IMAGE_HEIGHT_DEG = 207

     integer, parameter :: LENS_UNITS_INCHES = 1
     integer, parameter :: LENS_UNITS_CM = 2
     integer, parameter :: LENS_UNITS_MM = 3
     integer, parameter :: LENS_UNITS_M = 4

type idText
  integer :: ID
  character(len=40) :: text
end type
! Scratch for a better way of defining aperture
! type apertureOption
!   integer :: aperCode
!   character(len=20) :: aperCompactName
!   character(len=40) :: aperDisplayName
!  contains
!    functionToGetApertureFromSystem
! end type

! sys_config - try to collect all the SYSTEM variables into
! more human readable variables
type sys_config

    integer :: imgSurface
    character(len=40) :: currApertureName
    integer :: currApertureID, currFieldID, currLensUnitsID
    type(idText), allocatable :: aperOptions(:)
    type(idText), allocatable :: refFieldOptions(:)
    type(idText), allocatable :: lensUnits(:)
    real, dimension(2) :: refFieldValue
    real, dimension(2,10) :: relativeFields
    integer :: numFields

    ! Wavelength Data
    real, dimension(10) :: wavelengths
    real, dimension(10) :: spectralWeights
    integer :: refWavelengthIndex

  character(len=40), dimension(3) :: possibleApertureNames ! = ["ObjectHeight",&
    real, dimension(2) :: refApertureValue
  !TODO:  Where to define max field points instead of hard coded?
  integer, dimension(10) :: fieldColorCodes
  !  & "ObjectAngle", "ParaxialImageHeight"]
  contains
    procedure, public, pass(self) :: getImageSurface
    procedure, public, pass(self) :: isFocalSystem
    procedure, public, pass(self) :: isTelecentric
    !procedure, public, pass(self) :: getPossibleApertueSettings
    !procedure, public, pass(self) :: getCurrentApertueSetting
    procedure, public, pass(self) :: getApertureFromSystemArr
    procedure, public, pass(self) :: getFieldRefFromSystemArr
    procedure, public, pass(self) :: updateParameters
    procedure, public, pass(self) :: updateApertureSelectionByCode
    procedure, public, pass(self) :: setNumFields
    procedure, public, pass(self) :: setRefWavelengthIndex
    procedure, public, pass(self) :: setWavelengths
    procedure, public, pass(self) :: setSpectralWeights
    procedure, public, pass(self) :: setRelativeFields

end type

interface sys_config
  module procedure :: sys_config_constructor
end interface

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
           real :: EPD = 0 ! EPD = entrance pupil diameter
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
    PRINT *, "Allocating values in lens data to repopulate"
    allocate(self%radii(self%num_surfaces), self%thicknesses(self%num_surfaces))
    PRINT *, "About to allocate curvatures"
    allocate(self%curvatures(self%num_surfaces))
    PRINT *, "About to allocate indices"
    allocate(self%surf_index(self%num_surfaces), self%surf_vnum(self%num_surfaces))
    PRINT *, "About to allocate glass names"
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
   do I = 0,maxSurf-1

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

   type(sys_config) function sys_config_constructor() result(self)
     use zoa_ui

     include "DATLEN.INC"

     allocate(idText :: self%aperOptions(3))
     allocate(idText :: self%refFieldOptions(3))
     allocate(idText :: self%lensUnits(4))

     self%aperOptions(1)%text = "Entrance Pupil Diameter"
     self%aperOptions(1)%id = APER_ENTR_PUPIL_DIAMETER

     self%aperOptions(2)%text = "Object Space NA"
     self%aperOptions(2)%id = APER_OBJECT_NA

     self%aperOptions(3)%text = "Stop Surface Aperture"
     self%aperOptions(3)%id = APER_STOP_SURFACE

     self%refFieldOptions(1)%text = "Object Height"
     self%refFieldOptions(1)%id = FIELD_OBJECT_HEIGHT

     self%refFieldOptions(2)%text = "Object Angle (deg)"
     self%refFieldOptions(2)%id = FIELD_OBJECT_ANGLE_DEG

     self%refFieldOptions(3)%text = "Paraxial Image Height"
     self%refFieldOptions(3)%id = FIELD_PARAX_IMAGE_HEIGHT

     self%lensUnits(1)%text = "inches"
     self%lensUnits(1)%id = LENS_UNITS_INCHES

     self%lensUnits(2)%text = "cm"
     self%lensUnits(2)%id = LENS_UNITS_CM

     self%lensUnits(3)%text = "mm"
     self%lensUnits(3)%id = LENS_UNITS_MM

     self%lensUnits(4)%text = "meters"
     self%lensUnits(4)%id = LENS_UNITS_M



     self%numFields = CFLDCNT
     self%relativeFields = CFLDS

     self%fieldColorCodes = [ID_COLOR_RED, ID_COLOR_GREEN, ID_COLOR_BLUE, &
     & ID_COLOR_MAGENTA, ID_COLOR_BLACK, ID_COLOR_BLACK, ID_COLOR_BLACK, &
     & ID_COLOR_BLACK, ID_COLOR_BLACK, ID_COLOR_BLACK]


     call self%updateParameters()


   end function

   subroutine updateParameters(self)
     class(sys_config), intent(inout) :: self
     include "DATLEN.INC"

     call self%getApertureFromSystemArr()
     select case (self%currApertureID)

     case (APER_ENTR_PUPIL_DIAMETER)

       self%refApertureValue(1) = (2.0D0*SYSTEM(13))
       self%refApertureValue(2) = (2.0D0*SYSTEM(12))

     case (APER_OBJECT_NA)
        self%refApertureValue(1) = SYSTEM(66)
        self%refApertureValue(2) = SYSTEM(65)

     end select

    !TODO:  Split into separate proc?
     call self%getFieldRefFromSystemArr()


     select case (self%currFieldID)

     case (FIELD_OBJECT_HEIGHT)

       self%refFieldValue(1) = SYSTEM(16)
       self%refFieldValue(2) = SYSTEM(14)

     case (FIELD_OBJECT_ANGLE_DEG)

       self%refFieldValue(1) = SYSTEM(23) !SYSTEM(16)
       self%refFieldValue(2) = SYSTEM(21) !SYSTEM(14)

     case (FIELD_PARAX_IMAGE_HEIGHT)
           self%refFieldValue(1) = SYSTEM(92) !SYSTEM(16)
           self%refFieldValue(2) = SYSTEM(93) !SYSTEM(14)

     case (FIELD_PARAX_IMAGE_SLOPE_TAN)
           self%refFieldValue(1) = SYSTEM(92) !SYSTEM(16)
           self%refFieldValue(2) = SYSTEM(93) !SYSTEM(14)

     case (FIELD_REAL_IMAGE_HEIGHT)
           self%refFieldValue(1) = SYSTEM(96) !SYSTEM(16)
           self%refFieldValue(2) = SYSTEM(97) !SYSTEM(14)
     case (FIELD_REAL_IMAGE_HEIGHT_DEG)
           self%refFieldValue(1) = SYSTEM(96) !SYSTEM(16)
           self%refFieldValue(2) = SYSTEM(97) !SYSTEM(14)

     end select ! Reference Field

     PRINT *, "Ref Fields XY are ", self%refFieldValue

     self%numFields = CFLDCNT
     self%relativeFields = CFLDS

     ! Wavelengths
     self%wavelengths(1:5) = SYSTEM(1:5)
     self%wavelengths(6:10) = SYSTEM(71:75)
     self%spectralWeights(1:5) = SYSTEM(31:35)
     self%spectralWeights(6:10) = SYSTEM(76:80)
     self%refWavelengthIndex = INT(SYSTEM(11))

     self%currLensUnitsID = SYSTEM(6)

   end subroutine

   subroutine getApertureFromSystemArr(self)
     class(sys_config), intent(inout) :: self
     include "DATLEN.INC"

     if (SYSTEM(64).EQ.0.AND.SYSTEM(67).EQ.0.AND.SYSTEM(83).EQ.0) then
       self%currApertureID = APER_ENTR_PUPIL_DIAMETER
     else if (SYSTEM(64).EQ.1) then
       self%currApertureID = APER_OBJECT_NA
     else if (SYSTEM(83).EQ.1) then
       self%currApertureID = APER_STOP_SURFACE
     else if (SYSTEM(67).EQ.1) then
       self%currApertureID = APER_FNO
     end if

           ! PRINT *, "SYSTEM(64) is ", SYSTEM(64) ! NAOY
           ! PRINT *, "SYSTEM(67) is ", SYSTEM(67) ! F-number
           ! PRINT *, "SYSTEM(83) is ", SYSTEM(83) ! FLOAT
           !
   end subroutine

   subroutine getFieldRefFromSystemArr(self)
    !use handlers

     class(sys_config), intent(inout) :: self
     include "DATLEN.INC"

        PRINT *, "Getting Field Reference From System Array"
        if (SYSTEM(60).EQ.1.AND.SYSTEM(61).EQ.1.AND.SYSTEM(18).EQ.0) THEN
          PRINT *, "FIELD OBJECT HEIGHT SETTING FOUND"
          self%currFieldID = FIELD_OBJECT_HEIGHT
          !self%currApertureName = "Object Height"
        else if (SYSTEM(60).EQ.1.AND.SYSTEM(61).EQ.1.AND.SYSTEM(18).EQ.1) THEN
          PRINT *, "FIELD OBJECT ANGLE DEGREE SETTING FOUND"
            self%currFieldID = FIELD_OBJECT_ANGLE_DEG
           !self%currApertureName = "Object Angle"
        else if (SYSTEM(94).EQ.-1.AND.SYSTEM(95).EQ.-1) THEN
          self%currFieldID = FIELD_PARAX_IMAGE_HEIGHT
          !self%currApertureName = "Paraxial Image Height"
        else if (SYSTEM(98).EQ.-1.AND.SYSTEM(99).EQ.-1) THEN
          self%currFieldID = FIELD_REAL_IMAGE_HEIGHT
          !self%currApertureName = "Real Image Height"

        end if
        ! CASE (IDF_B1)
        ! SYSTEM(60)=1.0D0
        ! SYSTEM(61)=1.0D0
        ! SYSTEM(18)=0.0D0
        ! SYSTEM(94)=0.0D0
        ! SYSTEM(95)=0.0D0
        ! SYSTEM(98)=0.0D0
        ! SYSTEM(99)=0.0D0
        ! INCLUDE 'NONSURF3FRESH.INC'
        ! CASE (IDF_B2)
        ! SYSTEM(60)=1.0D0
        ! SYSTEM(61)=1.0D0
        ! SYSTEM(18)=1.0D0
        ! SYSTEM(94)=0.0D0
        ! SYSTEM(95)=0.0D0
        ! SYSTEM(98)=0.0D0
        ! SYSTEM(99)=0.0D0
        ! INCLUDE 'NONSURF3FRESH.INC'
        ! CASE (IDF_B3)
        ! SYSTEM(60)=0.0D0
        ! SYSTEM(61)=0.0D0
        ! SYSTEM(94)=-1.0D0
        ! SYSTEM(95)=-1.0D0
        ! SYSTEM(98)=0.0D0
        ! SYSTEM(99)=0.0D0
        ! INCLUDE 'NONSURF3FRESH.INC'
        ! CASE (IDF_B4)
        ! SYSTEM(60)=0.0D0
        ! SYSTEM(61)=0.0D0
        ! SYSTEM(94)=-1.0D0
        ! SYSTEM(95)=-1.0D0
        ! SYSTEM(98)=0.0D0
        ! SYSTEM(99)=0.0D0
        ! INCLUDE 'NONSURF3FRESH.INC'
        ! CASE (IDF_B5)
        ! SYSTEM(60)=0.0D0
        ! SYSTEM(61)=0.0D0
        ! SYSTEM(98)=-1.0D0
        ! SYSTEM(99)=-1.0D0
        ! SYSTEM(94)=0.0D0
        ! SYSTEM(95)=0.0D0
        ! INCLUDE 'NONSURF3FRESH.INC'
        ! CASE (IDF_B6)
        ! SYSTEM(60)=0.0D0
        ! SYSTEM(61)=0.0D0
        ! SYSTEM(98)=1.0D0
        ! SYSTEM(99)=1.0D0
        ! SYSTEM(94)=0.0D0
        ! SYSTEM(95)=0.0D0
        ! INCLUDE 'NONSURF3FRESH.INC'

   end subroutine

   subroutine setNumFields(self, numFields)
     class(sys_config), intent(inout) :: self
     integer, intent(in) :: numFields
     include "DATLEN.INC"

     CFLDCNT = numFields
     self%numFields = numFields
     PRINT *, "Updating Number of Fields to ", self%numFields

   end subroutine

   subroutine setRefWavelengthIndex(self, refWavelengthIdx)
     class(sys_config), intent(inout) :: self
     integer, intent(in) :: refWavelengthIdx
     include "DATLEN.INC"

     self%refWavelengthIndex = refWavelengthIdx
     SYSTEM(11) = REAL(self%refWavelengthIndex)

   end subroutine

   subroutine setWavelengths(self, index, wavelength)
     class(sys_config), intent(inout) :: self
     integer, intent(in) :: index
     real, intent(in) :: wavelength
     include "DATLEN.INC"

     self%wavelengths(index) = wavelength

     SYSTEM(1:5) = self%wavelengths(1:5)
     SYSTEM(71:75) = self%wavelengths(6:10)

   end subroutine

   subroutine setSpectralWeights(self, index, weight)
     class(sys_config), intent(inout) :: self
     integer, intent(in) :: index
     real, intent(in) :: weight
     include "DATLEN.INC"

     self%spectralWeights(index) = weight

     SYSTEM(31:35) = self%spectralWeights(1:5)
     SYSTEM(76:80) = self%spectralWeights(6:10)

   end subroutine

   subroutine setRelativeFields(self, col, row, value)
     implicit none
     class(sys_config), intent(inout) :: self
     integer, intent(in) :: col, row
     real, intent(in) :: value
     include "DATLEN.INC"


     self%relativeFields(col, row) = value
     CFLDS = self%relativeFields
   end subroutine


   subroutine updateApertureSelectionByCode(self, ID_SELECTION, xAp, yAp, xySame)
     class(sys_config), intent(inout) :: self
     integer, intent(in) :: ID_SELECTION
     real :: xAp, yAp
     integer :: xySame
     character(len=23) :: strXAp, strYAp

     include "DATLEN.INC"

     !self%currApertureID = ID_SELECTION

     select case (ID_SELECTION)

     case (APER_OBJECT_NA)
       PRINT *, "Define Aperture by Object NA"
       PRINT *, "xAp = ", xAp
       PRINT *, "yAp =  ", yAp

        CALL DTOA23(xAp,strXAp)
        CALL DTOA23(yAp,strYAp)
        call PROCESKDP('U L')
        IF(xySame.EQ.1) THEN
         call PROCESKDP('NAOY,'//strYAp)
        ELSE
         call PROCESKDP('NAOY,'//strYAp)
         call PROCESKDP('NAOX,'//strXAp)
       END IF
       call PROCESKDP('EOS')

     case (APER_ENTR_PUPIL_DIAMETER)
       PRINT *, "Define Aperture by Entrance Pupil Diameter"

!       SAY/SAX
!        CALL WDIALOGGETDOUBLE(IDF_X,DW1)
!        CALL WDIALOGGETDOUBLE(IDF_Y,DW2)
        xApertureRadius = xAp/2.0
        yApertureRadius = yAp/2.0

        PRINT *, "xApertureRadius is ", xApertureRadius

        CALL DTOA23(xApertureRadius,strXAp)
        CALL DTOA23(yApertureRadius,strYAp)
        call PROCESKDP('U L')
        IF(xySame.EQ.1) THEN
         call PROCESKDP('SAY,'//strYAp)
         call PROCESKDP('SAX,'//strYAp)
        ELSE
         call PROCESKDP('SAY,'//strYAp)
         call PROCESKDP('SAX,'//strXAp)
       END IF
       call PROCESKDP('EOS')

     case (APER_STOP_SURFACE)
       PRINT *, "Define Aperture by Stop Surface"
       call PROCESKDP('U L')
        IF(xySame.EQ.1) THEN
          CALL PROCESKDP('SAY FLOAT')
        ELSE
          CALL PROCESKDP('SAY FLOAT')
          CALL PROCESKDP('SAX FLOAT')
        END IF
        call PROCESKDP('EOS')


     end select

     PRINT *, "SYSTEM(64) is ", SYSTEM(64) ! NAOY
     PRINT *, "SYSTEM(67) is ", SYSTEM(67) ! F-number
     PRINT *, "SYSTEM(83) is ", SYSTEM(83) ! FLOAT

     ! Make sure we have the up to date values
     call self%updateParameters()


   end subroutine


end module kdp_data_types
