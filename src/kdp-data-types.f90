module kdp_data_types
     use iso_c_binding, only:  c_ptr
     use iso_fortran_env, only: real64
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

     integer, parameter :: RAYAIM_PARAX = 303
     integer, parameter :: RAYAIM_REAL = 304
     integer, parameter :: RAYAIM_APLANATIC = 305
     integer, PARAMETER :: RAYAIM_TELE = 306   

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
    integer :: currApertureID, currFieldID, currLensUnitsID, currRayAimID
    type(idText), allocatable :: aperOptions(:)
    type(idText), allocatable :: refFieldOptions(:)
    type(idText), allocatable :: lensUnits(:)
    type(idText), allocatable :: rayAimOptions(:)
    real, dimension(2) :: refFieldValue
    real, dimension(2,10) :: relativeFields
    integer :: numFields, numWavelengths
    integer, dimension(10) :: wavelengthIndices

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
    procedure, public, pass(self) :: isUSystem 
    procedure, public, pass(self) :: isTelecentric
    procedure, public, pass(self) :: isObjectAfInf
    !procedure, public, pass(self) :: getPossibleApertueSettings
    !procedure, public, pass(self) :: getCurrentApertueSetting
    procedure, public, pass(self) :: getApertureFromSystemArr
    procedure, public, pass(self) :: getRayAimFromSystemArr
    procedure, public, pass(self) :: getFieldRefFromSystemArr
    procedure, public, pass(self) :: updateParameters
    procedure, public, pass(self) :: updateApertureSelectionByCode
    procedure, public, pass(self) :: updateRayAimSelectionByCode
    procedure, public, pass(self) :: updateFieldSelectionByCode
    procedure, public, pass(self) :: setNumFields
    procedure, public, pass(self) :: setRefWavelengthIndex
    procedure, public, pass(self) :: setWavelengths
    procedure, public, pass(self) :: setSpectralWeights
    procedure, public, pass(self) :: setRelativeFields
    procedure, public, pass(self) :: getFieldText
    procedure :: getLensUnitsText
    procedure, private :: setNumberofWavelengths
    procedure, private, pass(self) :: setRefFieldKDP


end type



interface sys_config
  module procedure :: sys_config_constructor
end interface

type io_config
   type(c_ptr) :: textView
   type(c_ptr), allocatable :: allBuffers(:)

   contains
      procedure, public, pass(self) :: setTextView
      procedure, public, pass(self) :: registerTextView
end type

interface io_config
  module procedure :: io_config_constructor
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
           real(kind=real64), allocatable :: CSeidel(:,:), CXSeidel(:,:)
    contains




end type paraxial_ray_trace_data

type :: ray_fan_data

   real, allocatable :: relAper(:) 
   real, allocatable :: xyfan(:,:)

end type


interface lens_data
  module procedure :: lens_data_constructor
end interface lens_data

type opd_data
   real(kind=real64), allocatable :: X(:),Y(:),Z(:)
   integer :: numPts, numx, numy

end type

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
    !PRINT *, "About to allocate curvatures"
    allocate(self%curvatures(self%num_surfaces))
    !PRINT *, "About to allocate indices"
    allocate(self%surf_index(self%num_surfaces), self%surf_vnum(self%num_surfaces))
    !PRINT *, "About to allocate glass names"
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
 
 ! This may be sketchy.  Not sure if we ever need to distinguish between
 ! UFocal and UAFocal
 ! From the manual:  
 ! If the qualifier "UAFOCAL" is specified, these paraxial based
 ! aberrations will not be converted to transverse linear measure 
!(i.e., they remain the direct sum of surface contribution coefficients).  
function isUSystem(self) result(boolResult)
  implicit none
  class(sys_config) :: self
  logical :: boolResult

  include "DATLEN.INC"

  boolResult = .FALSE.  
  IF(SYSTEM(30).EQ.2.0D0.OR.SYSTEM(30).EQ.4.0D0) boolResult = .TRUE.

  
end function

function isFocalSystem(self) result(boolResult)

  class(sys_config) :: self
  logical :: boolResult

  include "DATLEN.INC"

  boolResult = .FALSE.
  IF(SYSTEM(30).EQ.1.0D0.OR.SYSTEM(30).EQ.2.0D0) boolResult = .TRUE.

end function

function isObjectAfInf(self) result(boolResult)
  class(sys_config) :: self
  logical :: boolResult
  include "DATLEN.INC"

  boolResult = .FALSE.

  if(DABS(ALENS(3,0)).GT.1E19) boolResult = .TRUE.
  PRINT *, "Value being compared is ", ALENS(3,0)

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
     allocate(idText :: self%rayAimOptions(4))

     self%aperOptions(1)%text = "Entrance Pupil Diameter"
     self%aperOptions(1)%id = APER_ENTR_PUPIL_DIAMETER

     self%aperOptions(2)%text = "Object Space NA"
     self%aperOptions(2)%id = APER_OBJECT_NA

     self%aperOptions(3)%text = "Stop Surface Aperture"
     self%aperOptions(3)%id = APER_STOP_SURFACE

     self%rayAimOptions(1)%text = "Paraxial Only"
     self%rayAimOptions(1)%id = RAYAIM_PARAX

     self%rayAimOptions(2)%text = "Real, Aplanatic Off (Default)"
     self%rayAimOptions(2)%id = RAYAIM_REAL

     self%rayAimOptions(3)%text = "Real, Aplanatic On"
     self%rayAimOptions(3)%id = RAYAIM_APLANATIC

     self%rayAimOptions(4)%text = "Telecentric"
     self%rayAimOptions(4)%id = RAYAIM_TELE     


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

type(io_config) function io_config_constructor() result(self)
  use zoa_ui
  use iso_c_binding, only: c_null_ptr
  !implicit none


  ! Size of array needs to match max # of ID_TERMINAL parameter in zoa-ui
    allocate(c_ptr :: self%allBuffers(4))


    self%textView = c_null_ptr


   end function

   subroutine registerTextView(self, textView, idTextView)
     use zoa_ui
     class(io_config) :: self
     type(c_ptr) :: textView
     integer :: idTextView

     ! TODO:  Add error checking here
     self%allBuffers(idTextView) = textView
     PRINT *, "Set Text View Buffer to ", idTextView

     !select case (idBuffer)
     !case (ID_TERMINAL_DEFAULT)

   end subroutine

   subroutine setTextView(self, idTextView)
     use zoa_ui
     class(io_config) :: self

     integer :: idTextView

     ! TODO:  Add error checking here
     self%textView = self%allBuffers(idTextView)

   end subroutine

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

     call self%setNumberofWavelengths()

   end subroutine

   subroutine setRefFieldKDP(self)
     ! hopefully temporary interface to set KDP system
     ! vars based on ref field value and field type
     implicit none
     class(sys_config) :: self
     include "DATLEN.INC"

     select case (self%currFieldID)

     case (FIELD_OBJECT_HEIGHT)

       SYSTEM(16) = self%refFieldValue(1)
       SYSTEM(14) = self%refFieldValue(2)

     case (FIELD_OBJECT_ANGLE_DEG)

       SYSTEM(23) = self%refFieldValue(1)
       SYSTEM(21) = self%refFieldValue(2)

     case (FIELD_PARAX_IMAGE_HEIGHT)
           SYSTEM(92) = self%refFieldValue(1)
           SYSTEM(93) = self%refFieldValue(2)

     case (FIELD_PARAX_IMAGE_SLOPE_TAN)
           SYSTEM(92) = self%refFieldValue(1)
           SYSTEM(93) = self%refFieldValue(2)

     case (FIELD_REAL_IMAGE_HEIGHT)
           SYSTEM(96) = self%refFieldValue(1)
           SYSTEM(97) = self%refFieldValue(2)
     case (FIELD_REAL_IMAGE_HEIGHT_DEG)
           SYSTEM(96) = self%refFieldValue(1)
           SYSTEM(97) = self%refFieldValue(2)

     end select ! Reference Field


   end subroutine

   subroutine getRayAimFromSystemArr(self)
    class(sys_config), intent(inout) :: self
    include "DATLEN.INC"

    ! Aplanatic on 
    !SYSTEM(70)=1.0D0
    !SYSTEM(62)=0.0D0
    !SYSTEM(63)=0.0D0

    !FROM LDM1.FOR

    !SET  RAY AIMING ON
    !SYSTEM(62)=1.0D0
    !SET TEL OFF
    !SYSTEM(63)=0.0D0
    !SET AIMAPL OFF
    !SYSTEM(70)=0.0D0

    if (SYSTEM(70).EQ.1.AND.SYSTEM(62).EQ.0.AND.SYSTEM(63).EQ.0) then
      self%currRayAimID = RAYAIM_APLANATIC
    else if (SYSTEM(62).EQ.1.AND.SYSTEM(63).EQ.0.AND.SYSTEM(70).EQ.0) then
      self%currRayAimID = RAYAIM_REAL
    else if (SYSTEM(62).EQ.0.AND.SYSTEM(63).EQ.0.AND.SYSTEM(70).EQ.0) then
      self%currRayAimID = RAYAIM_PARAX
    else if (SYSTEM(62).EQ.0.AND.SYSTEM(63).EQ.1.AND.SYSTEM(70).EQ.0) then
      self%currRayAimID = RAYAIM_TELE
    end if

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

   subroutine setNumberofWavelengths(self)
     ! record wavelngths with nonzero spectral weight.
     ! mainly used for plotting
     implicit none
     class(sys_config), intent(inout) :: self
     integer :: i
     integer, dimension(10) :: nonzerowavelengths
     !self%numWavelengths = 0
     PRINT *, "Pack operation"
     self%numWavelengths = size(pack(self%spectralWeights, self%spectralWeights /= 0))
     self%wavelengthIndices(1:self%numWavelengths) =  &
     & pack([(i,i=1,size(self%spectralWeights))],self%spectralWeights /= 0)

     PRINT *, "numWavelengths is ", self%numWavelengths
     PRINT *, "Wavelength indices is ", self%wavelengthIndices



     !PRINT *, findloc(self%spectralWeights, self%spectralWeights /= 0)

     ! do i=1,len(self%spectralWeights)
     !   if (self%spectralWeights(i) > 0) then
     !       self%numWavelengths = self%numWavelengths+1
     !
     !    end if
     !
     ! end do

   end subroutine

   subroutine setSpectralWeights(self, index, weight)
     class(sys_config), intent(inout) :: self
     integer, intent(in) :: index
     real, intent(in) :: weight
     include "DATLEN.INC"

     self%spectralWeights(index) = weight

     SYSTEM(31:35) = self%spectralWeights(1:5)
     SYSTEM(76:80) = self%spectralWeights(6:10)

     call self%setNumberOfWavelengths()

   end subroutine

   subroutine setRelativeFields(self, col, row, newval)
     implicit none
     class(sys_config), intent(inout) :: self
     integer, intent(in) :: col, row
     real, intent(in) :: newval
     include "DATLEN.INC"

     ! Need to convert from absolute to relative
     if (self%refFieldValue(col) < newval) then
        self%refFieldValue(col) = newval
        call self%setRefFieldKDP()
     end if

     self%relativeFields(col, row) = newval/self%refFieldValue(col)
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

   subroutine updateFieldSelectionByCode(self, ID_SELECTION)
    class(sys_config), intent(inout) :: self
    integer, intent(in) :: ID_SELECTION
    include "DATLEN.INC"

    select case (ID_SELECTION)
    case (FIELD_OBJECT_HEIGHT)

      SYSTEM(16) = self%refFieldValue(1)
      SYSTEM(14) = self%refFieldValue(2) 
      SYSTEM(60)=1.0D0
      SYSTEM(61)=1.0D0
      SYSTEM(18)=0.0D0
      SYSTEM(94)=0.0D0
      SYSTEM(95)=0.0D0
      SYSTEM(98)=0.0D0
      SYSTEM(99)=0.0D0
    case (FIELD_OBJECT_ANGLE_DEG)

      SYSTEM(23) = self%refFieldValue(1) 
      SYSTEM(21) = self%refFieldValue(2) 
      SYSTEM(60)=1.0D0
      SYSTEM(61)=1.0D0
      SYSTEM(18)=1.0D0
      SYSTEM(94)=0.0D0
      SYSTEM(95)=0.0D0
      SYSTEM(98)=0.0D0
      SYSTEM(99)=0.0D0      

    case (FIELD_PARAX_IMAGE_HEIGHT)
      SYSTEM(92) = self%refFieldValue(1)  
      SYSTEM(93) = self%refFieldValue(2)      
      SYSTEM(60)=0.0D0
      SYSTEM(61)=0.0D0
      SYSTEM(94)=-1.0D0
      SYSTEM(95)=-1.0D0
      SYSTEM(98)=0.0D0
      SYSTEM(99)=0.0D0

    case (FIELD_PARAX_IMAGE_SLOPE_TAN)
      SYSTEM(92) = self%refFieldValue(1) 
      SYSTEM(93) = self%refFieldValue(2)
      SYSTEM(14) = 0.0D0
      SYSTEM(16) = 0.0D0
      SYSTEM(21) = 0.0D0
      SYSTEM(23) = 0.0D0    
      SYSTEM(96:97) = 0.0D0  

    case (FIELD_REAL_IMAGE_HEIGHT)
      SYSTEM(96) = self%refFieldValue(1) 
      SYSTEM(97) =  self%refFieldValue(2) 
      SYSTEM(14) = 0.0D0
      SYSTEM(16) = 0.0D0
      SYSTEM(21) = 0.0D0
      SYSTEM(23) = 0.0D0    
      SYSTEM(92:93) = 0.0D0    
    case (FIELD_REAL_IMAGE_HEIGHT_DEG)
      SYSTEM(96) = self%refFieldValue(1) 
      SYSTEM(97) = self%refFieldValue(2) 
      SYSTEM(14) = 0.0D0
      SYSTEM(16) = 0.0D0
      SYSTEM(21) = 0.0D0
      SYSTEM(23) = 0.0D0    
      SYSTEM(92:93) = 0.0D0  
    end select 

   end subroutine

   subroutine updateRayAimSelectionByCode(self, ID_SELECTION)
    class(sys_config), intent(inout) :: self
    integer, intent(in) :: ID_SELECTION
    include "DATLEN.INC"

    select case (ID_SELECTION)

    ! !FROM LDM1.FOR

    ! !SET  RAY AIMING ON
    ! !SYSTEM(62)=1.0D0
    ! !SET TEL OFF
    ! !SYSTEM(63)=0.0D0
    ! !SET AIMAPL OFF
    ! !SYSTEM(70)=0.0D0

     case (RAYAIM_PARAX)
       SYSTEM(62) = 0.0D0
       SYSTEM(63) = 0.0D0
       SYSTEM(70) = 0.0D0
     case (RAYAIM_REAL)
      SYSTEM(62) = 1.0D0
      SYSTEM(63) = 0.0D0
      SYSTEM(70) = 0.0D0      
    case (RAYAIM_APLANATIC)
      SYSTEM(62) = 0.0D0
      SYSTEM(63) = 0.0D0
      SYSTEM(70) = 1.0D0      
     case (RAYAIM_TELE)
      SYSTEM(62) = 0.0D0
      SYSTEM(63) = 1.0D0
      SYSTEM(70) = 0.0D0 
     end select    

     self%currRayAimID = ID_SELECTION

   end subroutine


   function getFieldText(self) result(fldText)
     implicit none
     class(sys_config) :: self
     character(len=150) :: fldText

     fldText = self%lensUnits(self%currLensUnitsID)%text
     PRINT *, "currFieldID is ", self%currFieldID
     if (self%currFieldID.EQ.FIELD_OBJECT_ANGLE_DEG) then
       fldText = "Object Angle [deg]"
     end if

   end function

   function getLensUnitsText(self) result(lenText)
     implicit none
     class(sys_config) :: self
     character(len=10) :: lenText

     lenText = self%lensUnits(self%currLensUnitsID)%text

   end function




end module kdp_data_types
