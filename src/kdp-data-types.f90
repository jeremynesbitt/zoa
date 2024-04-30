
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

  integer, parameter :: ID_PICKUP_RAD = 1
  integer, parameter :: ID_PICKUP_THIC = 3  
  
  ! These are options for the solve adjustment drop down lists.  
  integer, parameter :: ID_SOLVE_NONE = 0
  integer, parameter :: ID_SOLVE_PY = 1 
  integer, parameter :: ID_SOLVE_PX = 4 
  integer, parameter :: ID_SOLVE_PCY = 2 
  integer, parameter :: ID_SOLVE_PCX = 5
  integer, parameter :: ID_SOLVE_CAY = 3
  integer, parameter :: ID_SOLVE_CAX = 6
  integer, parameter :: ID_SOLVE_MAG = 7


  integer, parameter :: ID_SOLVE_ = 1 
  integer, parameter :: ID_SOLVE_APY = 4 
  integer, parameter :: ID_SOLVE_APX = 2 
  integer, parameter :: ID_SOLVE_APCY = 5
  integer, parameter :: ID_SOLVE_APCX = 8
  integer, parameter :: ID_SOLVE_PIY = 6
  integer, parameter :: ID_SOLVE_PIX = 7
  integer, parameter :: ID_SOLVE_PUY = 3



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
 real(kind=real64), dimension(2) :: refFieldValue
 real(kind=real64), dimension(2,10) :: relativeFields
 integer :: numFields, numWavelengths
 integer, dimension(10) :: wavelengthIndices
 character(len=80) :: lensTitle

 ! Wavelength Data
 real(kind=real64), dimension(10) :: wavelengths
 real(kind=real64), dimension(10) :: spectralWeights
 integer :: refWavelengthIndex

character(len=40), dimension(3) :: possibleApertureNames ! = ["ObjectHeight",&
 real(kind=real64), dimension(2) :: refApertureValue
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
 procedure, public, pass(self) :: setAbsoluteFields
 procedure, public, pass(self) :: getFieldText
 procedure :: getLensUnitsText
 procedure, private :: setNumberofWavelengths
 procedure, private, pass(self) :: setRefFieldKDP
 procedure, public, pass(self) :: genSaveOutputText
 procedure, public, pass(self) :: setFieldTypeFromString
 procedure, public, pass(self) :: setMaxField




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



type pickup
integer :: surf, surf_ref, column, ID_type
real(kind=real64) :: scale, offset 
character(len=10) :: pickupTxt

contains 
  procedure, public, pass(self) :: setPickupText
  procedure, public, pass(self) :: genKDPCMD
  procedure, public, pass(self) :: genKDPCMDToRemovePickup


end type


! This is to store all the data needed for telling the user the 
! type of solves available and how to interface it with the CLI
type solve_options
 character(len=40) :: uiText
 integer :: id_solve, solve_type
 character(len=8) :: cmd_kdp 
 character(len=8) :: cmd_codeV 

 character(len=20) :: param1Name, param2Name 
end type

! I wanted this to be solve, but when I try to name it such,
! I get a weird, internal compiler error in gfortran.  So ksolve it is!
type, extends(solve_options) :: ksolve
integer :: surf 
real(kind=real64) :: param1, param2

contains 
 procedure, public, pass(self) :: updateSolveData
 procedure, public, pass(self) :: setSolveText
 procedure, public, pass(self) :: genKDPCMDToSetSolve
 procedure, public, pass(self) :: genCodeVCMDToSetSolve
 procedure, public, pass(self) :: genKDPCMDToRemoveSolve


end type





! Eventually want this to take over storing of surface data, with 
! lens data having gathering functions to create arrays

type surface
integer :: surf_num, surf_type
real :: radius, thickness
character(len=40) :: glass
logical :: pickup_radius, pickup_thic
!type(pickup) :: pickup_radius, pickup_thic



end type

type lens_data

integer num_surfaces, ref_stop
real, allocatable :: radii(:), thicknesses(:), surf_index(:), surf_vnum(:), &
& curvatures(:), pickups(:,:,:), solves(:,:), clearapertures(:)
character(:), allocatable :: glassnames(:)
character(:), allocatable :: catalognames(:)
type(ksolve), allocatable, dimension(:) :: thickSolves


contains
procedure, public, pass(self) :: set_num_surfaces
procedure, public, pass(self)  :: add_lens_data
procedure, public, pass(self) :: imageSurfaceIsIdeal
procedure, public, pass(self) :: update => updateLensData
procedure, public, pass(self) :: genSaveOutputText => genLensDataSaveOutputText
procedure, public, pass(self) :: isSolveOnSurface
procedure, public, pass(self) :: setSolveData


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

!TODO:  Not sure it makes sense for this to extend lens_Data.  perhaps change move all this to parax_calcs.f90?
type, extends(lens_data) :: paraxial_ray_trace_data
        !integer, allocatable :: surface(:)
        real(kind=real64), allocatable ::  marginal_ray_height(:), marginal_ray_angle(:), &
        & chief_ray_height(:), chief_ray_angle(:), marginal_ray_aoi(:), &
        & chief_ray_aoi(:)
        real(kind=real64) :: t_mag = 0
        real :: EPD = 0 ! EPD = entrance pupil diameter
        real(kind=real64) :: EFL, BFL, FFL, FNUM, imageDistance, OAL, ENPUPDIA, EXPUPDIA, ENPUPPOS, EXPUPPOS
        real(kind=real64) :: TT, objectDistance

        real(kind=real64), allocatable :: CSeidel(:,:), CXSeidel(:,:)
 contains
        procedure, public, pass(self) :: calculateFirstOrderParameters
        procedure, public, pass(self) :: getObjectThicknessToSetParaxialMag




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

! More definitions
  ! Solves
  type(solve_options), dimension(8) :: thick_solves
  type(solve_options), dimension(8) :: curv_solves


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
 DEALLOCATE(self%catalognames)
 DEALLOCATE(self%pickups)
 deallocate(self%solves)
 deallocate(self%clearapertures)
 deallocate(self%thickSolves)



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
 allocate(character(40) :: self%glassnames(self%num_surfaces), self%catalognames(self%num_surfaces))
 
 ! For now just copy what is in DATLEN.INC
 allocate(self%pickups(6,self%num_surfaces,45))
 allocate(self%solves(9,self%num_surfaces))
 allocate(self%clearapertures(self%num_surfaces))
 allocate(self%thickSolves(self%num_surfaces))


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
! TODO:  Where do document quantitative definition of infinity?
if(DABS(ALENS(3,0)).GT.1E11) boolResult = .TRUE.
!PRINT *, "Value being compared is ", ALENS(3,0)

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

 ! Initialize Solves
  call init_solves()

  call self%updateParameters()


end function

type(io_config) function io_config_constructor() result(self)
use zoa_ui
use iso_c_binding, only: c_null_ptr
!implicit none


! Size of array needs to match max # of ID_TERMINAL parameter in zoa-ui
 allocate(c_ptr :: self%allBuffers(5))


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
  implicit none
  class(sys_config), intent(inout) :: self
  include "DATLEN.INC"

  self%lensTitle = trim(LI)

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

! Was having some trouble with the setRefFieldKDP sub so wrote this one to figure out
! what is going on.  Only one should survive...
subroutine setMaxField(self)
  use type_utils, only: real2str
  implicit none
  class(sys_config) :: self

  select case (self%currFieldID)

  case (FIELD_OBJECT_HEIGHT)

    call PROCESKDP("U L; SCY "//trim(real2str(self%refFieldValue(2)))//';EOS')

  case (FIELD_OBJECT_ANGLE_DEG)
    call PROCESKDP("U L; SCY FANG"//trim(real2str(self%refFieldValue(2)))//';EOS')

  end select ! Reference Field  
  

end subroutine

subroutine setRefFieldKDP(self)
  ! hopefully temporary interface to set KDP system
  ! vars based on ref field value and field type
  use type_utils, only: real2str
  implicit none
  class(sys_config) :: self
  include "DATLEN.INC"

  select case (self%currFieldID)

  case (FIELD_OBJECT_HEIGHT)

    !SYSTEM(16) = self%refFieldValue(1)
    SYSTEM(16) = self%refFieldValue(1)
    SYSTEM(14) = self%refFieldValue(2)
    
    ! TODO:  For symmetric fields the value is the same.  If I want to support
    ! asymmetric field settings need to change this
    call PROCESKDP("U L; SCY "//trim(real2str(self%refFieldValue(2)))//';EOS')
    call PROCESKDP("U L; SCX "//trim(real2str(self%refFieldValue(2)))//';EOS')

  case (FIELD_OBJECT_ANGLE_DEG)

    SYSTEM(23) = self%refFieldValue(1)
    SYSTEM(21) = self%refFieldValue(2)

    call PROCESKDP("U L; SCY FANG "//trim(real2str(self%refFieldValue(2)))//';EOS')
    call PROCESKDP("U L; SCX FANG "//trim(real2str(self%refFieldValue(2)))//';EOS')

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

subroutine setFieldTypeFromString(self, iptStr)
  class(sys_config), intent(inout) :: self
  character(len=*) :: iptStr

select case(iptStr)
  case('YAN')
    self%currFieldID = FIELD_OBJECT_ANGLE_DEG
  case('XAN')
    self%currFieldID = FIELD_OBJECT_ANGLE_DEG
  case('YOB')
    self%currFieldID = FIELD_OBJECT_HEIGHT
  case('XOB')
    self%currFieldID = FIELD_OBJECT_HEIGHT
end select


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

subroutine init_solves()

 thick_solves(1)%solve_type = ID_PICKUP_THIC
 thick_solves(1)%id_solve = ID_SOLVE_NONE
 thick_solves(1)%param1Name = "Not Used"
 thick_solves(1)%param2Name = "Not Used"
 thick_solves(1)%uiText = "None"
 thick_solves(1)%cmd_kdp = ""
 thick_solves(1)%cmd_codeV = ""


 thick_solves(2)%solve_type = ID_PICKUP_THIC
 thick_solves(2)%id_solve = ID_SOLVE_PY
 thick_solves(2)%param1Name = "Axial Height"
 thick_solves(2)%param2Name = "Not Used"
 thick_solves(2)%uiText = "Paraxial Axial Height (PY)"
 thick_solves(2)%cmd_kdp = "PY"  
 thick_solves(2)%cmd_codeV = "PIM"  

 thick_solves(3)%solve_type = ID_PICKUP_THIC
 thick_solves(3)%id_solve = ID_SOLVE_PX
 thick_solves(3)%param1Name = "Axial Height"
 thick_solves(3)%param2Name = "Not Used"
 thick_solves(3)%uiText = "X Paraxial Axial Height (PX)"
 thick_solves(3)%cmd_kdp = "PX" 
 thick_solves(3)%cmd_codeV = ""   

 thick_solves(4)%solve_type = ID_PICKUP_THIC
 thick_solves(4)%id_solve = ID_SOLVE_PCY
 thick_solves(4)%param1Name = "Chief Ray Height"
 thick_solves(4)%param2Name = "Not Used"
 thick_solves(4)%uiText = "Paraxial Chief Ray Height (PCY)"
 thick_solves(4)%cmd_kdp = "PCY"
 thick_solves(4)%cmd_codeV = ""

 thick_solves(5)%solve_type = ID_PICKUP_THIC
 thick_solves(5)%id_solve = ID_SOLVE_PCX
 thick_solves(5)%param1Name = "Chief Ray Height"
 thick_solves(5)%param2Name = "Not Used"
 thick_solves(5)%uiText = "X Paraxial Chief Ray Height (PCX)"
 thick_solves(5)%cmd_kdp = "PCX" 
 thick_solves(5)%cmd_codeV = ""   

 thick_solves(6)%solve_type = ID_PICKUP_THIC
 thick_solves(6)%id_solve = ID_SOLVE_CAY
 thick_solves(6)%param1Name = "Clear Aperture"
 thick_solves(6)%param2Name = "Not Used"
 thick_solves(6)%uiText = "Clear Aperture Solve (CAY)"
 thick_solves(6)%cmd_kdp = "CAY" 
 thick_solves(6)%cmd_codeV = ""  

 thick_solves(7)%solve_type = ID_PICKUP_THIC
 thick_solves(7)%id_solve = ID_SOLVE_CAX
 thick_solves(7)%param1Name = "Clear Aperture"
 thick_solves(7)%param2Name = "Not Used"
 thick_solves(7)%uiText = "X Clear Aperture Solve (CAX)"
 thick_solves(7)%cmd_kdp = "CAX" 
 thick_solves(7)%cmd_codeV = ""  

 thick_solves(8)%solve_type = ID_PICKUP_THIC
 thick_solves(8)%id_solve = ID_SOLVE_MAG
 thick_solves(8)%param1Name = "Target Reduction"
 thick_solves(8)%param2Name = "Not Used"
 thick_solves(8)%uiText = "Mag Solve - Object Surf Only"
 thick_solves(8)%cmd_kdp = "REDSLV"
 thick_solves(8)%cmd_codeV = "RED"   

 curv_solves(1)%solve_type = ID_PICKUP_RAD
 curv_solves(1)%id_solve = ID_SOLVE_NONE
 curv_solves(1)%param1Name = "Not Used"
 curv_solves(1)%param2Name = "Not Used"
 curv_solves(1)%uiText = "None"
 curv_solves(1)%cmd_kdp = ""
 curv_solves(1)%cmd_codeV = ""

 curv_solves(2)%solve_type = ID_PICKUP_RAD
 curv_solves(2)%id_solve = ID_SOLVE_APY
 curv_solves(2)%param1Name = "Not Used"
 curv_solves(2)%param2Name = "Not Used"
 curv_solves(2)%uiText = "Aplanatic Paraxial Axial Ray (APY)"
 curv_solves(2)%cmd_kdp = "APY"  
 curv_solves(2)%cmd_codeV = ""  

 curv_solves(3)%solve_type = ID_PICKUP_RAD
 curv_solves(3)%id_solve = ID_SOLVE_APX
 curv_solves(3)%param1Name = "Not Used"
 curv_solves(3)%param2Name = "Not Used"
 curv_solves(3)%uiText = "X Aplanatic Paraxial Axial Ray (APX)"
 curv_solves(3)%cmd_kdp = "PX"   
 curv_solves(3)%cmd_codeV = ""   

 curv_solves(4)%solve_type = ID_PICKUP_RAD
 curv_solves(4)%id_solve = ID_SOLVE_APCY
 curv_solves(4)%param1Name = "Not Used"
 curv_solves(4)%param2Name = "Not Used"
 curv_solves(4)%uiText = "Paraxial Chief Ray Angle (APCY)"
 curv_solves(4)%cmd_kdp = "APCY"  
 curv_solves(5)%cmd_codeV = "APCY"  

 curv_solves(5)%solve_type = ID_PICKUP_RAD
 curv_solves(5)%id_solve = ID_SOLVE_APCX
 curv_solves(5)%param1Name = "Not Used"
 curv_solves(5)%param2Name = "Not Used"
 curv_solves(5)%uiText = "X Paraxial Chief Ray Angle (APCX)"
 curv_solves(5)%cmd_kdp = "APCX"   
 curv_solves(5)%cmd_codeV = ""   

 curv_solves(6)%solve_type = ID_PICKUP_RAD
 curv_solves(6)%id_solve = ID_SOLVE_PIY
 curv_solves(6)%param1Name = "Angle of Incidece"
 curv_solves(6)%param2Name = "Not Used"
 curv_solves(6)%uiText = "Paraxial Chief Ray AOI (PIY)"
 curv_solves(6)%cmd_kdp = "PIY"  
 curv_solves(6)%cmd_codeV = "PIY"  

 curv_solves(7)%solve_type = ID_PICKUP_RAD
 curv_solves(7)%id_solve = ID_SOLVE_PIX
 curv_solves(7)%param1Name = "Angle of Incidece"
 curv_solves(7)%param2Name = "Not Used"
 curv_solves(7)%uiText = "X Paraxial Chief Ray AOI (PIY)"
 curv_solves(7)%cmd_kdp = "PIX"  
 curv_solves(7)%cmd_codeV = "PIX" 

 curv_solves(8)%solve_type = ID_PICKUP_RAD
 curv_solves(8)%id_solve = ID_SOLVE_PUY
 curv_solves(8)%param1Name = "Paraxial Angle"
 curv_solves(8)%param2Name = "Not Used"
 curv_solves(8)%uiText = "Paraxial Axial Ray Slope Angle (HUY)"
 curv_solves(8)%cmd_kdp = "PUY"  
 curv_solves(8)%cmd_codeV = ""
 
 

    !thicSolves(1) = "None"
    !thicSolves(2) = "Paraxial Axial Height (PY)"
    !thicSolves(3) = "X Paraxial Axial Height (PX)"
    !thicSolves(4) = "Paraxial Chief Ray Height (PCY)"
    !thicSolves(5) = "X Paraxial Chief Ray Height (PCX)"
    !thicSolves(6) = "Clear Aperture Solve (CAY)"
    !thicSolves(7) = "X Clear Aperture Solve (CAX)"

end subroutine

subroutine setSolveText(self, solveOptions) 
  implicit none
  class(ksolve) :: self
  type(solve_options), dimension(:) :: solveOptions
  integer :: id
  integer :: i
  
  
  PRINT *, "ID is ", id
  do i=1,size(solveOptions)
    PRINT *, "solveOpptions id_solve is ", solveOptions(i)%id_solve
    if (solveOptions(i)%id_solve.EQ.self%id_solve) then
      PRINT *, "In correct Loop"
      ! Brute force it for now
      self%solve_type = solveOptions(i)%solve_type 
      self%uiText = solveOptions(i)%uiText
      self%param1Name =  solveOptions(i)%param1Name
      self%param2Name =  solveOptions(i)%param2Name
      self%cmd_kdp = solveOptions(i)%cmd_kdp
      self%cmd_codeV = solveOptions(i)%cmd_codeV


    end if

  end do


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

subroutine genSaveOutputText(self, fID)
  use type_utils, only: real2str, blankStr, int2str
  use GLOBALS, only: zoaVersion
  class(sys_config) :: self
  integer :: fID
  integer :: ii
  character(len=256) :: strOutLine
  character(len=256) :: strWL, strWLwgt, strXFLD, strYFLD, strFLDWGT

  write(fID, *) "! Zoa "//zoaVersion
  write(fID, *) "LEN NEW"
  write(fID, *) "TIT "//self%lensTitle
        ! Store dimensions
  select case(self%currLensUnitsID)
  case(LENS_UNITS_MM)
    strOutLine = "DIM M"
    PRINT *, "DIM M"
    
  case(LENS_UNITS_CM)
    strOutLine = "DIM C"
    PRINT *, "DIM C"
  case(LENS_UNITS_INCHES)
    strOutLine = "DIM I"
    PRINT *, "DIM I"
  end select
  write(fID, *) trim(strOutLine)

  ! Store Aperture
  select case(self%currApertureID)
  case(APER_ENTR_PUPIL_DIAMETER)
    strOutLine = "EPD "//real2str(self%refApertureValue(2),4)
     PRINT *, "EPD "//real2str(self%refApertureValue(2),4)
  end select
  write(fID, *) trim(strOutLine)

  !Store Wavelength Info
  strWL = 'WL'
  do ii=1,self%numWavelengths
    strWL = trim(strWL)//blankStr(1)//real2str(1000.0*self%wavelengths(ii),4)

  end do
  !PRINT *, trim(strWL)
  write(fID, *) trim(strWL)
  ! Spectral Weights

  if(self%numWavelengths > 1 ) then 
    strWLwgt = 'WTW'
    do ii=1,self%numWavelengths
      strWLwgt = trim(strWLwgt)//blankStr(1)//real2str(100.0*self%spectralWeights(ii),4)
    end do
    PRINT *, trim(strWLwgt)
    write(fID, *) trim(strWLwgt)
    ! Print Ref wavelength
    strOutLine = "REF "// int2str(self%refWavelengthIndex)
    write(fID, *) trim(strOutLine)
       
    PRINT *, "REF ", int2str(self%refWavelengthIndex)

  ! Store Field
    select case(self%currFieldID)
    case(FIELD_OBJECT_ANGLE_DEG)
       strXFLD = 'XAN'
       strYFLD = 'YAN'
       do ii=1,self%numFields
        strXFLD = trim(strXFLD)//blankStr(1)//real2str(self%refFieldValue(1)*self%relativeFields(1,ii),4)
        strYFLD = trim(strYFLD)//blankStr(1)//real2str(self%refFieldValue(2)*self%relativeFields(2,ii),4)
       end do
       write(fID, *) trim(strXFLD)
       write(fID, *) trim(strYFLD)
       
       PRINT *, trim(strXFLD)
       PRINT *, trim(strYFLD)

    end select

    ! Field weights not supported, so for now just output all 100s
    strFLDWGT = 'WTF'
    do ii=1,self%numFields
      strFLDWGT = trim(strFLDWGT)//blankStr(5)//'100'
    end do
    write(fID, *) trim(strFLDWGT)

    print *, strFLDWGT

  end if


 

end subroutine

subroutine setAbsoluteFields(self, absFields, fieldDir)
  ! fieldDir =1 : X
  ! fieldDir =2 : Y
  use type_utils, only: real2str, int2str
  use iso_fortran_env, only: real64
  implicit none

  class(sys_config), intent(inout) :: self
  real(kind=real64) :: absFields(:)
  real(kind=real64) :: maxField
  
  integer, intent(in) :: fieldDir
  integer :: i

  include "DATLEN.INC"

  ! Figure out which one is maximum
  do i=1,size(absFields)
    if (absFields(i) > maxField) maxField = absFields(i)
  end do
  ! NEEDS TESTING!
  self%refFieldValue(fieldDir) = maxField
  call self%updateFieldSelectionByCode(self%currFieldID)
  call self%setRefFieldKDP()
  !call self%setMaxField()


  ! TEMP FOR TESTING
  !self%currFieldID = FIELD_OBJECT_ANGLE_DEG
  ! Update zoa and kdp data structures
  ! call LogTermFOR("Max FIeld is "//real2str(maxField))
  ! self%refFieldValue(fieldDir) = maxField
  ! call self%setRefFieldKDP()
  ! call LogTermFOR("Max FIeld is "//real2str(self%refFieldValue(fieldDir)))

  ! ! Update number of fields based on size of absFields
  ! call LogTermFOR("Number of Fields is "//int2str(size(absFields)))
  ! call self%setNumFields(size(absFields))
  
  ! Update vals
  do i=1,size(absFields)
    !self%relativeFields(fieldDir, i) = absFields(i)
    self%relativeFields(fieldDir, i) = absFields(i)/maxField

  end do
  CFLDS = self%relativeFields  

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


subroutine setPickupText(self)
 class(pickup) :: self


 select case (self%ID_type)

 case(ID_PICKUP_RAD)
   self%pickupTxt = "RD"
 case(ID_PICKUP_THIC)
   self%pickupTxt = "TH"

 end select

end subroutine

function genKDPCMD(self) result(outTxt)
 use type_utils, only: int2str, real2str
 class(pickup) :: self
 character(len=280) :: outTxt

 outTxt = "PIKUP "//trim(self%pickupTxt)//","// &
 & trim(int2str(self%surf_ref))//","//trim(real2str(self%scale))// &
 & ","//trim(real2str(self%offset))//","//"0.0,"    
end function

function genKDPCMDToRemovePickup(self) result(outTxt)
 use type_utils, only: int2str, real2str
 class(pickup) :: self
 character(len=280) :: outTxt

 outTxt = "PIKD "//trim(self%pickupTxt)//","// &
 & trim(int2str(self%surf))//","    



end function  


subroutine updateSolveData(self, lData, row, solve_type) 

 class(ksolve) :: self
 type(lens_data) :: lData
 integer :: row, solve_type
 character(len=280) :: outTxt

 include "DATLEN.INC"

 ! From LDM1.FOR
 ! IF(WC.EQ.'PY') SOLVE(6,SURF)=1.0D0
 ! IF(WC.EQ.'PCY') SOLVE(6,SURF)=2.0D0
 ! IF(WC.EQ.'CAY') SOLVE(6,SURF)=3.0D0
 ! IF(WC.EQ.'PY'.OR.WC.EQ.'PCY'.OR.WC.EQ.'CAY') THEN
 ! SOLVE(7,SURF)=W1
 ! SOLVE(4,SURF)=0.0D0
 ! SOLVE(3,SURF)=0.0D0
 !         ELSE
 !         END IF    



 self%param1Name = "Not Used"
 self%param2Name = "Not Used"
 self%surf = row-1
 ! TODO:  Add a constant somewhere or do something to make it easier to 
 ! understand what is in solves array

 select case(solve_type)
 case(ID_PICKUP_THIC)
  self%id_solve = INT(lData%solves(6,row))
  self%param1 =  lData%solves(7,row)
 case(ID_PICKUP_RAD)
  PRINT *, "lData solves(8,row) is ", lData%solves(8,row)
  self%id_solve = INT(lData%solves(8,row))
  self%param1 =  lData%solves(9,row)
 end select 

 self%solve_type = solve_type
 !call self%setSolveFromType()
 select case(self%solve_type)

 case(ID_PICKUP_THIC)
  call self%setSolveText(thick_solves)
   !self%solveTxt = "PY"

 case(ID_PICKUP_RAD)
  call self%setSolveText(curv_solves)
  !self%solveTxt = getSolveTypeTxt(self%ID_type, curv_solves)

 end select

!  case (ID_SOLVE_PY)
!    self%param1 = lData%solves(7,row)
!    self%param2 = lData%solves(4,row)
!    self%param1Name = "Axial Height"
!    self%param2Name = "Not Used"

!  end select 

end subroutine    


function genKDPCMDToSetSolve(self) result(outTxt)
 use type_utils, only: int2str, real2str
 class(ksolve) :: self
 character(len=280) :: outTxt

 !PRINT *, "About to set ",trim(self%cmd_kdp)//", "//trim(real2str(self%param1,4))
 outTxt = trim(self%cmd_kdp)//" "//trim(real2str(self%param1,4))

end function

function genCodeVCMDToSetSolve(self) result(outTxt)
  use type_utils, only: int2str, real2str
  class(ksolve) :: self
  character(len=280) :: outTxt
 
  !PRINT *, "About to set ",trim(self%cmd_kdp)//", "//trim(real2str(self%param1,4))
  ! Special Case:  PIM 
  if (self%cmd_codeV == 'PIM') Then
    outTxt = trim(self%cmd_codeV)
  else 
     outTxt = trim(self%cmd_codeV)//" "//trim(real2str(self%param1,4))
  end if
 
 end function

function genKDPCMDToRemoveSolve(self, surf) result(outTxt)
 use type_utils, only: int2str, real2str
 class(ksolve) :: self
 integer :: surf
 character(len=280) :: outTxt

 PRINT *, "Removing solve from surface!"
 select case (self%solve_type)
 case (ID_PICKUP_THIC) 
     outTxt = "TSD, "//trim(int2str(surf)//","//trim(int2str(surf)))
 case (ID_PICKUP_RAD)
  outTxt = "CSDY, "//trim(int2str(surf)//","//trim(int2str(surf)))
 end select
     
end function  

! Call this from KDP side when data is updated for lens system
! Challenge is to find all the places in KDP where this happens
subroutine updateLensData(self)
  use iso_fortran_env, only: real64
  use command_utils, only: isInputNumber
  implicit none
  class(lens_data) :: self
  integer :: JJ
  real(kind=real64) :: INDEX, VNUM, RD
  include "DATMAI.INC"
  include "DATLEN.INC"

  JJ = 0
  call self%set_num_surfaces(INT(SYSTEM(20)) + 1)
  self%ref_stop = INT(SYSTEM(25)+1)
  DO JJ=0,self%num_surfaces-1
    CALL SINDEXJN(JJ, INDEX, VNUM)
    IF(ALENS(1,JJ).EQ.0.0D0) THEN
      RD=0.0D0
    ELSE
      RD=1.0D0/(ALENS(1,JJ))
    END IF

!     Dump data to interface
    self%radii(JJ+1) = RD
    self%curvatures(JJ+1) = ALENS(1,JJ)
    self%thicknesses(JJ+1) = ALENS(3,JJ)
    self%surf_index(JJ+1) = INDEX
    self%surf_vnum(JJ+1) = VNUM
    self%glassnames(JJ+1) = GLANAM(JJ,2)
    if (GLANAM(JJ,2).EQ.'AIR') self%glassnames(JJ+1) = ' '
    if (GLANAM(JJ,2).EQ.'LAST SURFACE') self%glassnames(JJ+1) = ' '
    ! The KDP code always puts a D in front of the model glass, but 
    ! for UI input and file saving I don't want it there, so remove it
    ! if it is there.  Figured this was less risky then trying to strip
    ! it in the original KDP code, but perhaps this is a mistake.
    if (GLANAM(JJ,2)(1:1).EQ.'D') then ! Check if this is a model glass
        if (isInputNumber(GLANAM(JJ,2)(2:13))) then
          !call LogTermFOR("Found Model Glas!")
          self%glassnames(JJ+1) = GLANAM(JJ,2)(2:13) 
        end if
    end if
    self%catalognames(JJ+1) = GLANAM(JJ,1)
    self%clearapertures(JJ+1) = ALENS(10,JJ)

    ! Pickup and Solvesstorage
    self%pickups(1:6,JJ+1,1:45) = PIKUP(1:6,JJ,1:45)
    self%solves(1:9,JJ+1) = SOLVE(1:9,JJ)

    self%thickSolves(JJ+1) = self%setSolveData(JJ+1,ID_PICKUP_THIC, thick_solves)


  END DO


end subroutine

subroutine calculateFirstOrderParameters(self, lData)
  class(paraxial_ray_trace_data) :: self
  type(lens_data) :: lData
  integer :: I,J
  include "DATLEN.INC"
  self%EFL = 0.0
  self%BFL = 0.0
  self%FFL = 0.0
  
  ! EFL, BFL, FFL
  I=0
  J=lData%num_surfaces-2

  !PRINT *, "J is ", J

   IF(((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J))).NE.0.0D0) THEN
   self%BFL=-(((PXTRAY(2,I)*PXTRAY(5,J))-(PXTRAY(6,I)*PXTRAY(1,J)))/ &
   & ((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J))))
                  ELSE
   self%BFL=1.0D20
                  END IF      
   


    IF(((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J))).NE.0.0D0) THEN
    self%FFL=-(((PXTRAY(1,I+1)*PXTRAY(6,J))-(PXTRAY(2,J)*PXTRAY(5,I+1)))/((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J))))
                    ELSE
    self%FFL=1.0D20
                    END IF       
                    
    IF(((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J))).NE.0.0D0) THEN
    self%EFL=-(((PXTRAY(2,I)*PXTRAY(5,I+1))-(PXTRAY(1,I+1)*PXTRAY(6,I)))/ &
    & ((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J))))
      ELSE
    self%EFL=1.0D20
      END IF                    

  I = 0
  J = lData%num_surfaces-1
  PRINT *, "PXTRAY(2,J) is ", PXTRAY(2,J)
  IF(DABS(PXTRAY(2,J)).GE.1.0D-15) THEN
    self%FNUM=-1.0D0/(2.0D0*PXTRAY(2,J))/lData%surf_index(J)
    PRINT *, "Object fnum ", -1.0D0/(2.0D0*PXTRAY(2,0))
                    ELSE
                    self%FNUM=0.0D0
                    END IF  

self%imageDistance = lData%thicknesses(lData%num_surfaces-1)
self%OAL = 0.0
do I=2, lData%num_surfaces-2
  self%OAL = self%OAL + lData%thicknesses(I)
end do

self%TT = 0.0
do I=1, lData%num_surfaces
  self%TT = self%TT + lData%thicknesses(I)
end do

self%objectDistance = lData%thicknesses(1)

!TODO:  move calcs of this here
self%ENPUPDIA = ENDIAY
self%EXPUPDIA = EXDIAY
self%ENPUPPOS = ENPUZ
self%EXPUPPOS = EXPUZ+self%imageDistance

! Paraxial Image Calcs
PRINT *, "PARAX IMAGE HT IS ", self%chief_ray_height(lData%num_surfaces)
PRINT *, "PARAX IMAGE ANGLE IS ", self%chief_ray_angle(lData%num_surfaces)

end subroutine

function getObjectThicknessToSetParaxialMag(self, magTgt, lData) result(t0)
  use type_utils, only: real2str
  implicit none
  class(paraxial_ray_trace_data) :: self
  type(lens_data) :: lData
  real(kind=real64) :: magTgt, t0, thick0, uk1, u01, uk2, u02
  real(kind=real64) :: mag, y, uTgt, uSlope, uOffset
  real(kind=real64), dimension(5) :: tempSolveData
  integer :: s1, s2

  include "DATLEN.INC"

  t0 = 10.0

  ! Here is a brief description of the logic here
  ! paraxial lateral magnification can be calculated as (nu)_k-1/(n0uo)
  ! Where k-1 is the surface before image surface
  ! Due to linearity of paraxial eqns, there is a linear relationship between
  ! u_k-1 and u0.  u_k-1 = slope*u0 + offset
  ! A way to figure out this slope and offset is to trace a ray with a slightly
  ! different object distance.  This will change both u0 and u_k-1 and from the
  ! original distance and the new distance can determine slope and offset.
  ! Once you know this, can figure out new thickness with the following equation
  ! magTgt = gamma/(slope*gamma+offset)
  ! magTgt = new mag
  ! gamma = y/t0, y is the marginal ray height on surface 2, and t0 is the new thicknesss
   ! You can solve this for t0 and thus figure out the new thickness
  ! JN 2/20/24


  s2 = lData%num_surfaces
  s1 = 1
  uk1 = self%marginal_ray_angle(s2)
  u01 = self%marginal_ray_angle(s1)

  ! I don't actually use this here 
  mag = lData%surf_index(s2)*self%marginal_ray_angle(s2) / &
  & (lData%surf_index(s1)*self%marginal_ray_angle(s1))

  ! Need to turn off the solve if it is on.
  ! TODO:  have a getter/setter fcn vs just having this global
  tempSolveData(1:5) = SOLVE(3:7,0)
  SOLVE(3:7,0) = 0.0D0


  ! Adjust object thickness by a bit and recalculate
  thick0 = lData%thicknesses(1)
  CALL PROCESKDP("THI SO "//real2str(thick0+1)) ! arbitrary to choose +1mm

  uk2 = self%marginal_ray_angle(s2)
  u02 = self%marginal_ray_angle(s1)

  ! Compute Slope and Offset
  uSlope = (uk2-uk1)/(u02-u01)
  uOffset = uk1-u01*uSlope


  CALL PROCESKDP("THI SO "//real2str(thick0))

  ! Finally ready to calculate new thickness
  t0 = -1*self%marginal_ray_height(s1+1)*(magTgt*uSlope-1)/(magTgt*uOffset)
  !call LogTermFOR("New Thickness is "//real2str(t0))

  !Restore Solve Data
  SOLVE(3:7,0) = tempSolveData(1:5)

end function

function isSolveOnSurface(self, surf) result(boolResult)
  class(lens_data) :: self
  integer :: surf
  logical :: boolResult

  boolResult = .FALSE.

  if (self%solves(6,surf).NE.0.OR.self%solves(8,surf).NE.0) then
    boolResult = .TRUE.

  end if

end function



subroutine genLensDataSaveOutputText(self, fID)
  use type_utils, only: real2str, blankStr, int2str
  use zoa_file_handler, only: genOutputLineWithSpacing
  class(lens_data) :: self
  integer :: fID
  integer :: ii
  character(len=1024) :: strSurfLine
  character(len=4) :: surfStr
  !character(len=80) :: glassStr
  logical :: rdmFlag

  rdmFlag = .TRUE.

  ! Do Object SUrface
  strSurfLine = 'SO'
  if (rdmFlag) then
    strSurfLine = genOutputLineWithSpacing(blankStr(1), 'SO', trim(real2str(self%radii(1),4)), &
    & trim(real2str(self%thicknesses(1),sci=.TRUE.)), trim(self%glassnames(1)))
    !strSurfLine = 'SO'//blankStr(1)//trim(real2str(self%radii(1),4))//blankStr(1)// &
    !& trim(real2str(self%thicknesses(1),sci=.TRUE.))//blankStr(1)//trim(self%glassnames(1))
  else
    strSurfLine = 'SO'//blankStr(3)//real2str(self%curvatures(1),4)//blankStr(5)//real2str(self%thicknesses(1))// &
    & blankStr(5)//self%glassnames(1)    
  end if
  write(fID, *) trim(strSurfLine)
  PRINT *, trim(strSurfLine)

  do ii=2,self%num_surfaces-1
    surfStr = 'S' !//trim(int2str(ii-1))
    if (ii==2) surfStr = 'S1'

    !glassStr = self%glassnames(ii)
    !if (isModelGlass(glassStr)) glassStr = set 
    
    if(rdmFlag) then
      strSurfLine = genOutputLineWithSpacing(blankStr(1), trim(surfStr), & 
      & trim(real2str(self%radii(ii),4)), trim(real2str(self%thicknesses(ii),4)), & 
      & trim(self%glassnames(ii)))      
      !strSurfLine = 'S'//int2str(ii-1)//blankStr(3)//real2str(self%radii(ii),4)// &
      !& blankStr(5)//real2str(self%thicknesses(ii),4)// &
      !& blankStr(5)//self%glassnames(ii)
    else
      strSurfLine = 'S'//int2str(ii-1)//blankStr(3)//real2str(self%curvatures(ii),4)// &
      & blankStr(5)//real2str(self%thicknesses(ii))// &
      & blankStr(5)//self%glassnames(ii)    
    end if      
    write(fID, *) trim(strSurfLine)
    PRINT *, trim(strSurfLine)
    ! Check for ref stop
    if (self%ref_stop == ii) then
      strSurfLine = blankStr(2)//'STO S'//trim(int2str(ii-1))
      write(fID, *) trim(strSurfLine)
    end if
    ! Check for user specified clear aperture.  TODO:  Need to implement a more sophisticated
    ! way to store CA info, as the geometry is not always circular.  But for now
    ! just support circular until I get some to mkae it more abstract
    if (self%clearapertures(ii).NE.0) then
      strSurfLine = blankStr(2)//'CIR '//trim(real2str(self%clearapertures(ii)))
      write(fID, *) trim(strSurfLine)
    end if

    ! pseudocode for now
    !if (self%hasPickup(ii)) then
    !  print *, self%getPickupTxt(ii)
      if (self%isSolveOnSurface(ii)) then
        print *, "Solve in surf ",ii
        strSurfLine = self%thickSolves(ii)%genCodeVCMDToSetSolve()
        write(fID, *) trim(strSurfLine)
        print *, "Thic Solve code is ",strSurfLine
      end if
             
    
  end do

  ! Do Image SUrface.  

  if (rdmFlag) then
    strSurfLine = 'SI'//blankStr(2)//trim(real2str(self%radii(self%num_surfaces),4))//blankStr(3)// &
    & trim(real2str(self%thicknesses(self%num_surfaces)))//blankStr(3)//self%glassnames(self%num_surfaces)
  else
    strSurfLine = 'SI'//blankStr(2)//trim(real2str(self%curvatures(self%num_surfaces),4))//blankStr(3)// &
    & trim(real2str(self%thicknesses(self%num_surfaces)))//blankStr(3)//self%glassnames(self%num_surfaces)  
  end if
  write(fID, *) trim(strSurfLine)
  PRINT *, trim(strSurfLine)  
  if (self%clearapertures(self%num_surfaces).NE.0) then
    strSurfLine = blankStr(2)//'CIR '//trim(real2str(self%clearapertures(self%num_surfaces)))
    write(fID, *) trim(strSurfLine)
  end if

  ! Now that we are done send GO cmd to leave lens update level
  write(fID, *) "GO"

end subroutine

function setSolveData(self, surf, solve_type, solveOptions) result(currSolve)
  implicit none
  class(lens_data) :: self
  type(solve_options), dimension(:) :: solveOptions
  type(ksolve) :: currSolve
  integer :: surf, solve_type, id, col
  integer :: i

  select case (solve_type)

  case (ID_PICKUP_THIC)
    col = 6
  case (ID_PICKUP_RAD)
    col = 8
  end select

  id = INT(self%solves(col,surf))
  
  
  do i=1,size(solveOptions)
    if (solveOptions(i)%id_solve.EQ.id) then
      ! Transfer data
      currSolve%solve_type = solveOptions(i)%solve_type
      currSolve%id_solve = id
      currSolve%param1 = self%solves(col+1,surf)
      currSolve%param1Name =  solveOptions(i)%param1Name
      currSolve%param2Name =  solveOptions(i)%param2Name
      currSolve%cmd_kdp = solveOptions(i)%cmd_kdp
      currSolve%cmd_codeV = solveOptions(i)%cmd_codeV


    end if

  end do


end function




end module kdp_data_types
