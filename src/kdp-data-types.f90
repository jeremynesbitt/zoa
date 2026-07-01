
module kdp_data_types
  use iso_c_binding, only:  c_ptr
  use iso_fortran_env, only: real64
  use zoa_ui
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

  ! Aperture types
  integer, parameter :: ID_APER_CIRCULAR = 1



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
 ! Per-field CODE V vignetting factors, column = field index.
 ! Rows: 1=VUY (upper-y), 2=VLY (lower-y), 3=VUX (+x), 4=VLX (-x). 0 = no vignetting.
 real(kind=real64), dimension(4,10) :: vignetting = 0.0_real64
 ! Default edge (physical) aperture scale factor for lens drawing: physical size
 ! = clear aperture * this, used when a surface has no explicit CIR EDG value.
 ! Exposed in the GUI as a margin percentage (factor = 1 + margin/100); default
 ! 1.0 = 0% margin (draw at the clear aperture).
 real(kind=real64) :: defaultEdgeScaleFactor = 1.0_real64
 integer :: numFields, numWavelengths
 integer, dimension(10) :: wavelengthIndices
 character(len=80) :: lensTitle
 ! List of Colors to store for UI selection
 character(len=8),dimension(9) :: colorNames
 integer, dimension(9) :: colorIDs

 ! Wavelength Data
 real(kind=real64), dimension(10) :: wavelengths
 real(kind=real64), dimension(10) :: spectralWeights
 integer :: refWavelengthIndex

character(len=40), dimension(3) :: possibleApertureNames ! = ["ObjectHeight",&
 real(kind=real64), dimension(2) :: refApertureValue
!TODO:  Where to define max field points instead of hard coded?
integer, dimension(10) :: fieldColorCodes
integer, dimension(10) :: wavelengthColorCodes

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
 procedure, public, pass(self) :: setVignetting
 procedure, public, pass(self) :: getVignetting
 procedure, public, pass(self) :: resetVignetting
 procedure, public, pass(self) :: setAbsoluteFields
 procedure, public, pass(self) :: getFieldText
 procedure :: getLensUnitsText
 procedure, private :: setNumberofWavelengths
 procedure, private, pass(self) :: setRefFieldKDP
 procedure, public, pass(self) :: genSaveOutputText
 procedure, public, pass(self) :: setFieldTypeFromString
 procedure, public, pass(self) :: setMaxField
 procedure, public, pass(self) :: getWavelength
 procedure, public, pass(self) :: getSpectralWeight
 procedure, public, pass(self) :: getRefWavelengthIndex
 procedure, public, pass(self) :: getNumWavelengths
 procedure, public, pass(self) :: getDefaultWavelength
 procedure, public, pass(self) :: getDimensions
 procedure :: getAbsYFieldText




end type



interface sys_config
module procedure :: sys_config_constructor
end interface

type io_config
type(c_ptr) :: textView, prev_textView
type(c_ptr), allocatable :: allBuffers(:)
logical :: dumpText

contains
   procedure, public, pass(self) :: setTextView
   procedure, public, pass(self) :: setTextViewFromPtr 
   procedure, public, pass(self) :: restoreTextView
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
& curvatures(:), pickups(:,:,:), solves(:,:)
character(:), allocatable :: glassnames(:)
character(:), allocatable :: catalognames(:)
type(ksolve), allocatable, dimension(:) :: thickSolves


contains
procedure, public, pass(self) :: set_num_surfaces
procedure, public, pass(self)  :: add_lens_data
procedure, public, pass(self) :: imageSurfaceIsIdeal
procedure, public, pass(self) :: update => updateLensData
procedure, public, pass(self) :: genAsphereSavOutputText
procedure, public, pass(self) :: isSolveOnSurface, isAsphereOnSurface
procedure, public, pass(self) :: isConicConstantOnSurface, setSolveData

!procedure, private, pass(self) ::

end type lens_data

!TODO:  Not sure it makes sense for this to extend lens_Data.  perhaps change move all this to parax_calcs.f90?
type, extends(lens_data) :: paraxial_ray_trace_data
        !integer, allocatable :: surface(:)
        real(kind=real64), allocatable ::  marginal_ray_height(:), marginal_ray_angle(:), &
        & chief_ray_height(:), chief_ray_angle(:), marginal_ray_aoi(:), &
        & chief_ray_aoi(:)
        real(kind=real64) :: t_mag = 0
        real :: EPD = 0 ! EPD = entrance pupil diameter
        real(kind=real64) :: EFL, BFL, FFL, FNUM, imageDistance, OAL, ENPUPDIA, EXPUPDIA, ENPUPPOS, EXPUPPOS
        real(kind=real64) :: nao, nai
        real(kind=real64) :: TT, objectDistance

        real(kind=real64), allocatable :: CSeidel(:,:), CXSeidel(:,:)
 contains
        procedure, public, pass(self) :: isFirstOrderDataValid 
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
 use DATLEN, only: NEWIMG
  class(sys_config), intent(inout) :: self
 integer, intent(inout) :: intSurf

 intSurf = NEWIMG

end subroutine

subroutine set_num_surfaces(self, input)
   use mod_surface

class(lens_data), intent(inout) :: self
integer, intent(in) :: input
if (input.ne.self%num_surfaces.and.allocated(self%radii)) THEN
 ! This is bad if I have somethign stored that I want to persist if the user changes
 ! the number of lenses.  need to fix this. 
 DEALLOCATE(self%radii)
 DEALLOCATE(self%thicknesses)
 DEALLOCATE(self%curvatures)
 DEALLOCATE(self%surf_index)
 DEALLOCATE(self%surf_vnum)
 DEALLOCATE(self%glassnames)
 DEALLOCATE(self%catalognames)
 DEALLOCATE(self%pickups)
 deallocate(self%solves)
 deallocate(self%thickSolves)



end if


self%num_surfaces = input
if (.not.allocated(self%radii)) THEN
 allocate(self%radii(self%num_surfaces), self%thicknesses(self%num_surfaces))
 allocate(self%curvatures(self%num_surfaces))
 allocate(self%surf_index(self%num_surfaces), self%surf_vnum(self%num_surfaces))
 allocate(character(40) :: self%glassnames(self%num_surfaces), self%catalognames(self%num_surfaces))
 
 ! For now just copy what is in DATLEN.INC
 allocate(self%pickups(6,self%num_surfaces,45))
 allocate(self%solves(9,self%num_surfaces))
 allocate(self%thickSolves(self%num_surfaces))
 !call check_clear_apertures(self)


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

 !  IF(GLANAM(INT(sys_last_surf())-1,2).EQ.'PERFECT      ' &
 !  .OR.GLANAM(INT(sys_last_surf())-1,2).EQ.'IDEAL        ') THEN

 boolResult = .FALSE.

end function

! This may be sketchy.  Not sure if we ever need to distinguish between
! UFocal and UAFocal
! From the manual:  
! If the qualifier "UAFOCAL" is specified, these paraxial based
! aberrations will not be converted to transverse linear measure 
!(i.e., they remain the direct sum of surface contribution coefficients).  
function isUSystem(self) result(boolResult)
   use mod_surface
   use mod_system, only: sys_mode
   use DATLEN
   use iso_fortran_env, only: real64
implicit none
class(sys_config) :: self
logical :: boolResult
boolResult = .FALSE.  
IF(sys_mode().EQ.2.0D0.OR.sys_mode().EQ.4.0D0) boolResult = .TRUE.


end function

function isFocalSystem(self) result(boolResult)
   use mod_system, only: sys_mode
   use DATLEN

class(sys_config) :: self
logical :: boolResult
boolResult = .FALSE.
IF(sys_mode().EQ.1.0D0.OR.sys_mode().EQ.2.0D0) boolResult = .TRUE.

end function

function isObjectAfInf(self) result(boolResult)
   use mod_surface
   use DATLEN
class(sys_config) :: self
logical :: boolResult
boolResult = .FALSE.
! TODO:  Where do document quantitative definition of infinity?
if(DABS(surf_thickness(0)).GT.1.0D11) boolResult = .TRUE.
!PRINT *, "Value being compared is ", surf_thickness(0)

end function

function isTelecentric(self) result(boolResult)
class(sys_config) :: self
logical :: boolResult

boolResult = .FALSE.

end function


type(sys_config) function sys_config_constructor() result(self)

  use DATLEN
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
  self%vignetting = 0.0_real64

  ! self%colorNames = [character(len=8) :: "Red", "Green", "Magenta", "Red", "Cyan", &
  ! & "Green", "Blue", "Grey", "Black"]
  ! self%colorIDs = [ID_COLOR_WHITE, ID_COLOR_YELLOW, &
  ! & ID_COLOR_MAGENTA, ID_COLOR_RED, ID_COLOR_CYAN, &
  ! & ID_COLOR_GREEN, ID_COLOR_BLUE, ID_COLOR_GREY, &
  ! & ID_COLOR_BLACK ]
  ! Defaults
  ! Field color codes here are really using the KDP colors cheme
  self%fieldColorCodes = [ID_COLOR_BLUE, ID_COLOR_RED, ID_COLOR_GREEN, &
  & ID_COLOR_MAGENTA, ID_COLOR_BLACK, ID_COLOR_BLACK, ID_COLOR_BLACK, &
  & ID_COLOR_BLACK, ID_COLOR_BLACK, ID_COLOR_BLACK]

  ! WL plots using the PL plot scheme
  self%wavelengthColorCodes = [PL_PLOT_RED, PL_PLOT_GREEN, PL_PLOT_BLUE, &
  & PL_PLOT_MAGENTA, ID_COLOR_BLACK, ID_COLOR_BLACK, ID_COLOR_BLACK, &
  & ID_COLOR_BLACK, ID_COLOR_BLACK, ID_COLOR_BLACK]



 ! Initialize Solves
  call init_solves()

  call self%updateParameters()


end function

type(io_config) function io_config_constructor() result(self)
use iso_c_binding, only: c_null_ptr
!implicit none


! Size of array needs to match max # of ID_TERMINAL parameter in zoa-ui
 allocate(c_ptr :: self%allBuffers(6))


 self%textView = c_null_ptr
 self%prev_textView = c_null_ptr


end function

subroutine registerTextView(self, textView, idTextView)
  class(io_config) :: self
  type(c_ptr) :: textView
  integer :: idTextView

  ! TODO:  Add error checking here
  self%allBuffers(idTextView) = textView

  !select case (idBuffer)
  !case (ID_TERMINAL_DEFAULT)

end subroutine

subroutine restoreTextView(self)
  class(io_config) :: self
  self%textView = self%prev_textView

  ! This is indeterminate but keep as a band aid for now.
  if (self%dumpText) self%dumpText = .FALSE.

end subroutine

subroutine setTextViewFromPtr(self, newtextview)
  class(io_config) :: self
  type(c_ptr) :: newtextview
  self%prev_textView = self%textView
  self%textView = newtextview
end subroutine



subroutine setTextView(self, idTextView)
  use GLOBALS, only: logger

  class(io_config) :: self

  integer :: idTextView

  call logger%logTextWithInt("setTextView id=", idTextView)

  ! TODO:  Add error checking here
  self%prev_textView = self%textView
  self%textView = self%allBuffers(idTextView)

  ! This is used as a flag for updateTerminalLog to stop trying to format text to
  ! prevent annoything gtk critical logs when writing to the dump text view
  if(idTextView == ID_TERMINAL_KDPDUMP) then
    self%dumpText = .TRUE.
  else
    self%dumpText = .FALSE.
  end if

end subroutine

subroutine updateParameters(self)
  use mod_system, only: sys_naox, sys_naoy, sys_pxim, sys_pyim, sys_rxim, sys_ryim, &
     & sys_sax, sys_say, sys_scx, sys_scx_fang, sys_scy, sys_scy_fang, sys_units, sys_wl_ref, &
     & sys_wavelength, sys_wl_weight
  use DATLEN
   use iso_fortran_env, only: real64
  implicit none
  class(sys_config), intent(inout) :: self
  integer :: n
  self%lensTitle = trim(LI)

  call self%getApertureFromSystemArr()
  select case (self%currApertureID)

  case (APER_ENTR_PUPIL_DIAMETER)

    self%refApertureValue(1) = (2.0D0*sys_sax())
    self%refApertureValue(2) = (2.0D0*sys_say())

  case (APER_OBJECT_NA)
     self%refApertureValue(1) = sys_naox()
     self%refApertureValue(2) = sys_naoy()

  end select

 

 call self%getFieldRefFromSystemArr()



  select case (self%currFieldID)

  case (FIELD_OBJECT_HEIGHT)

    self%refFieldValue(1) = sys_scx()
    self%refFieldValue(2) = sys_scy()

  case (FIELD_OBJECT_ANGLE_DEG)

    self%refFieldValue(1) = sys_scx_fang() !sys_scx()
    self%refFieldValue(2) = sys_scy_fang() !sys_scy()

  case (FIELD_PARAX_IMAGE_HEIGHT)
        self%refFieldValue(1) = sys_pxim() !sys_scx()
        self%refFieldValue(2) = sys_pyim() !sys_scy()

  case (FIELD_PARAX_IMAGE_SLOPE_TAN)
        self%refFieldValue(1) = sys_pxim() !sys_scx()
        self%refFieldValue(2) = sys_pyim() !sys_scy()

  case (FIELD_REAL_IMAGE_HEIGHT)
        self%refFieldValue(1) = sys_rxim() !sys_scx()
        self%refFieldValue(2) = sys_ryim() !sys_scy()
  case (FIELD_REAL_IMAGE_HEIGHT_DEG)
        self%refFieldValue(1) = sys_rxim() !sys_scx()
        self%refFieldValue(2) = sys_ryim() !sys_scy()

  end select ! Reference Field

  self%numFields = CFLDCNT
  self%relativeFields = CFLDS

  ! Wavelengths — read live from SYSTEM via mod_system
  do n = 1, 10
    self%wavelengths(n) = sys_wavelength(n)
    self%spectralWeights(n) = sys_wl_weight(n)
  end do
  self%refWavelengthIndex = INT(sys_wl_ref())


  self%currLensUnitsID = sys_units()


  call self%setNumberofWavelengths()

end subroutine

function getDimensions(self) result(dimStr)
   use iso_fortran_env, only: real64
  implicit none
  class(sys_config) :: self
  character(len=3) :: dimStr

  dimStr = " "
  select case(self%currLensUnitsID)
  case(LENS_UNITS_MM)
    dimStr = "mm" 
  case(LENS_UNITS_CM)
    dimStr = "cm"
  case(LENS_UNITS_INCHES)
    dimStr = "in"
  end select
  
end function

! Was having some trouble with the setRefFieldKDP sub so wrote this one to figure out
! what is going on.  Only one should survive...
subroutine setMaxField(self)
  use type_utils, only: real2str
   use iso_fortran_env, only: real64
  implicit none
  class(sys_config) :: self

  select case (self%currFieldID)

  case (FIELD_OBJECT_HEIGHT)
    if (self%refFieldValue(2) /= 0) then
      call processLensUpdateCommand("SCY "//trim(real2str(self%refFieldValue(2))))
    end if

  case (FIELD_OBJECT_ANGLE_DEG)
    call processLensUpdateCommand("SCY FANG "//trim(real2str(self%refFieldValue(2))))
  end select ! Reference Field  
  

end subroutine

! THis is a minicopy of updateSurfCommand in codeV-commands.  But to avoid circular 
! dependency I put it here for now
subroutine processLensUpdateCommand(strCMD)
  use DATMAI
  
  character(len=*) :: strCMD

  logical :: boolResult
 
  boolResult = .FALSE.
  !IF (F1.EQ.0.AND.F5.EQ.1) boolResult = .TRUE.
  IF(F6.EQ.1.AND.F10.EQ.0.OR.F6.EQ.1.AND.F11.EQ.0) boolResult = .TRUE.

  if (boolResult) then 
    call PROCESKDP(strCMD)
  else
    call PROCESKDP("U L; "//strCMD//"; EOS")
  end if

end subroutine

subroutine setRefFieldKDP(self)
  ! hopefully temporary interface to set KDP system
  ! vars based on ref field value and field type
  use type_utils, only: real2str
  use mod_system, only: sys_set_pxim, sys_set_pyim, sys_set_rxim, sys_set_ryim, &
     & sys_set_scx, sys_set_scx_fang, sys_set_scy, sys_set_scy_fang
  use DATLEN
   use iso_fortran_env, only: real64
  implicit none
  class(sys_config) :: self
  select case (self%currFieldID)

  case (FIELD_OBJECT_HEIGHT)

    !SYSTEM(16) = self%refFieldValue(1)
    call sys_set_scx( self%refFieldValue(1))
    call sys_set_scy( self%refFieldValue(2))
    
    ! TODO:  For symmetric fields the value is the same.  If I want to support
    ! asymmetric field settings need to change this
    if (self%refFieldValue(2) /= 0.0) then
    call processLensUpdateCommand("SCY "//trim(real2str(self%refFieldValue(2))))
    end if
    if (self%refFieldValue(1) /= 0.0) then
    call processLensUpdateCommand("SCX "//trim(real2str(self%refFieldValue(1))))
    end if

    !call PROCESKDP("U L; SCY "//trim(real2str(self%refFieldValue(2)))//';EOS')
    !call PROCESKDP("U L; SCX "//trim(real2str(self%refFieldValue(2)))//';EOS')

  case (FIELD_OBJECT_ANGLE_DEG)

    call sys_set_scx_fang( self%refFieldValue(1))
    call sys_set_scy_fang( self%refFieldValue(2))


    call processLensUpdateCommand("SCY FANG "//trim(real2str(self%refFieldValue(2))))
    call processLensUpdateCommand("SCX FANG "//trim(real2str(self%refFieldValue(1))))
    !call PROCESKDP("U L; SCY FANG "//trim(real2str(self%refFieldValue(2)))//';EOS')
    !call PROCESKDP("U L; SCX FANG "//trim(real2str(self%refFieldValue(2)))//';EOS')

  case (FIELD_PARAX_IMAGE_HEIGHT)
        call sys_set_pxim( self%refFieldValue(1))
        call sys_set_pyim( self%refFieldValue(2))

  case (FIELD_PARAX_IMAGE_SLOPE_TAN)
        call sys_set_pxim( self%refFieldValue(1))
        call sys_set_pyim( self%refFieldValue(2))

  case (FIELD_REAL_IMAGE_HEIGHT)
        call sys_set_rxim( self%refFieldValue(1))
        call sys_set_ryim( self%refFieldValue(2))
  case (FIELD_REAL_IMAGE_HEIGHT_DEG)
        call sys_set_rxim( self%refFieldValue(1))
        call sys_set_ryim( self%refFieldValue(2))

  end select ! Reference Field


end subroutine

subroutine getRayAimFromSystemArr(self)
 use mod_system, only: sys_aplanatic_aim, sys_ray_aiming, sys_telecentric
 use DATLEN
 class(sys_config), intent(inout) :: self
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

 if (sys_aplanatic_aim().EQ.1.AND.sys_ray_aiming().EQ.0.AND.sys_telecentric().EQ.0) then
   self%currRayAimID = RAYAIM_APLANATIC
 else if (sys_ray_aiming().EQ.1.AND.sys_telecentric().EQ.0.AND.sys_aplanatic_aim().EQ.0) then
   self%currRayAimID = RAYAIM_REAL
 else if (sys_ray_aiming().EQ.0.AND.sys_telecentric().EQ.0.AND.sys_aplanatic_aim().EQ.0) then
   self%currRayAimID = RAYAIM_PARAX
 else if (sys_ray_aiming().EQ.0.AND.sys_telecentric().EQ.1.AND.sys_aplanatic_aim().EQ.0) then
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
  case('YIM')
    self%currFieldID = FIELD_PARAX_IMAGE_HEIGHT
  case('XIM')
    self%currFieldID = FIELD_PARAX_IMAGE_HEIGHT    
end select


end subroutine

subroutine getApertureFromSystemArr(self)
  use mod_system, only: sys_fno_val_set, sys_na_set, sys_say_float
  use DATLEN
  class(sys_config), intent(inout) :: self
  if (sys_na_set().EQ.0.AND.sys_fno_val_set().EQ.0.AND.sys_say_float().EQ.0) then
    self%currApertureID = APER_ENTR_PUPIL_DIAMETER
  else if (sys_na_set().EQ.1) then
    self%currApertureID = APER_OBJECT_NA
  else if (sys_say_float().EQ.1) then
    self%currApertureID = APER_STOP_SURFACE
  else if (sys_fno_val_set().EQ.1) then
    self%currApertureID = APER_FNO
  end if

        ! PRINT *, "sys_na_set() is ", sys_na_set() ! NAOY
        ! PRINT *, "sys_fno_val_set() is ", sys_fno_val_set() ! F-number
        ! PRINT *, "sys_say_float() is ", sys_say_float() ! FLOAT
        !
end subroutine

subroutine getFieldRefFromSystemArr(self)
 !use handlers
 use mod_system, only: sys_pxim_fang_set, sys_pyim_fang_set, sys_rxim_fang_set, &
    & sys_ryim_fang_set, sys_scy_fang_set
  use DATLEN
  class(sys_config), intent(inout) :: self
     if (sys_scy_fang_set().EQ.0.AND.sys_pxim_fang_set().EQ.0.AND.sys_pyim_fang_set().EQ.0) THEN
       self%currFieldID = FIELD_OBJECT_HEIGHT
       !self%currApertureName = "Object Height"
     else if (sys_scy_fang_set().EQ.1.AND.sys_pxim_fang_set().EQ.0.AND.sys_pyim_fang_set().EQ.0) THEN
         self%currFieldID = FIELD_OBJECT_ANGLE_DEG
        !self%currApertureName = "Object Angle"
     else if (sys_pxim_fang_set().EQ.-1.AND.sys_pyim_fang_set().EQ.-1) THEN
       self%currFieldID = FIELD_PARAX_IMAGE_HEIGHT
       !self%currApertureName = "Paraxial Image Height"
     else if (sys_rxim_fang_set().EQ.-1.AND.sys_ryim_fang_set().EQ.-1) THEN
       self%currFieldID = FIELD_REAL_IMAGE_HEIGHT
       !self%currApertureName = "Real Image Height"

     end if
!      C       FIELDS
!      IF(SYSTEM(92).EQ.0.0D0.AND.SYSTEM(93).EQ.0.0D0.AND.
!   1  SYSTEM(96).EQ.0.0D0.AND.SYSTEM(97).EQ.0.0D0) THEN
! C       NOT PYIM,PXIM,RYIM OR RXIM
! C
! C       FIX SYSTEM(49)
!      IF(SYSTEM(18).EQ.0.0D0.AND.SYSTEM(18).EQ.SYSTEM(19)) THEN
! C       OBJ HT SAME MODE
! C       WRITE(OUTLYNE,*) SYSTEM(14),SYSTEM(16)
! C       CALL SHOWIT(1)
! C       WRITE(OUTLYNE,*) SYSTEM(15),SYSTEM(17)
! C       CALL SHOWIT(1)
!      IF(SYSTEM(14).EQ.SYSTEM(16).AND.SYSTEM(15).EQ.SYSTEM(17).AND.
!   1  SYSTEM(49).EQ.2.0D0.OR.
!   1  SYSTEM(14).EQ.SYSTEM(16).AND.SYSTEM(15).EQ.SYSTEM(17).AND.
!   1  SYSTEM(49).EQ.3.0D0) SYSTEM(49)=SYSTEM(49)-1.0D0
!                      END IF
!      IF(SYSTEM(18).EQ.1.0D0.AND.SYSTEM(18).EQ.SYSTEM(19)) THEN
! C       OBJ ANG SAME MODE
! C       WRITE(OUTLYNE,*) SYSTEM(21),SYSTEM(23)
! C       CALL SHOWIT(1)
! C       WRITE(OUTLYNE,*) SYSTEM(22),SYSTEM(24)
! C       CALL SHOWIT(1)
!      IF(SYSTEM(21).EQ.SYSTEM(23).AND.SYSTEM(22).EQ.SYSTEM(24).AND.
!   1  SYSTEM(49).EQ.2.0D0.OR.
!   1  SYSTEM(21).EQ.SYSTEM(23).AND.SYSTEM(22).EQ.SYSTEM(24).AND.
!   1  SYSTEM(49).EQ.3.0D0) SYSTEM(49)=SYSTEM(49)-1.0D0
!                      END IF
!                      ELSE
!      IF(SYSTEM(92).NE.0.0D0.AND.SYSTEM(93).NE.0.0D0.AND.
!   1SYSTEM(92).EQ.SYSTEM(93).AND.SYSTEM(49).EQ.2.0D0.OR.
!   1SYSTEM(92).NE.0.0D0.AND.SYSTEM(93).NE.0.0D0.AND.
!   1SYSTEM(92).EQ.SYSTEM(93).AND.SYSTEM(49).EQ.3.0D0)
!   4SYSTEM(49)=SYSTEM(49)-1.0D0
!      IF(SYSTEM(96).NE.0.0D0.AND.SYSTEM(97).NE.0.0D0.AND.
!   1SYSTEM(96).EQ.SYSTEM(97).AND.SYSTEM(49).EQ.2.0D0.OR.
!   1SYSTEM(96).NE.0.0D0.AND.SYSTEM(97).NE.0.0D0.AND.
!   1SYSTEM(96).EQ.SYSTEM(97).AND.SYSTEM(49).EQ.3.0D0)
!   4SYSTEM(49)=SYSTEM(49)-1.0D0
!                      END IF
! C
!      IF(SYSTEM(49).NE.2.0D0.AND.SYSTEM(49).NE.3.0D0) THEN
!      CALL WDIALOGPUTCHECKBOX(IDF_SAMEFIELD,1)
!                      ELSE
!      CALL WDIALOGPUTCHECKBOX(IDF_SAMEFIELD,0)
!                      END IF
!      IF(SYSTEM(60).EQ.1.0D0.OR.SYSTEM(61).EQ.1.0D0) THEN
!      SYSTEM(60)=1.0D0
!      SYSTEM(61)=1.0D0
!      END IF
! C       SCY/SCX OR SCY FANG/SCX FANG EXPLICITLY
!      IF(SYSTEM(18).EQ.0.0D0) THEN
! C       SCY/SCX
!      CALL WDIALOGPUTRADIOBUTTON(IDF_F1)
!      CALL WDIALOGPUTDOUBLE(IDF_XREF,SYSTEM(16))
!      CALL WDIALOGPUTDOUBLE(IDF_YREF,SYSTEM(14))
!      CALL WDIALOGFIELDSTATE(IDF_CX,1)
!      CALL WDIALOGFIELDSTATE(IDF_CY,1)
!      CALL WDIALOGPUTDOUBLE(IDF_CX,SYSTEM(17))
!      CALL WDIALOGPUTDOUBLE(IDF_CY,SYSTEM(15))
!                      ELSE
! C       SCY FANG/SCX FANG
!      CALL WDIALOGPUTRADIOBUTTON(IDF_F2)
!      CALL WDIALOGPUTDOUBLE(IDF_XREF,SYSTEM(23))
!      CALL WDIALOGPUTDOUBLE(IDF_YREF,SYSTEM(21))
!      CALL WDIALOGFIELDSTATE(IDF_CX,1)
!      CALL WDIALOGFIELDSTATE(IDF_CY,1)
!      CALL WDIALOGPUTDOUBLE(IDF_CX,SYSTEM(24))
!      CALL WDIALOGPUTDOUBLE(IDF_CY,SYSTEM(22))
!                      END IF
!      IF(SYSTEM(94).NE.0.0D0.OR.SYSTEM(95).NE.0.0D0) THEN
!      SYSTEM(94)=SYSTEM(95)
! C       PYIM AND PXIM
!      IF(SYSTEM(94).EQ.-1.0D0) THEN
! C       PXIM/PYIM
!      CALL WDIALOGPUTRADIOBUTTON(IDF_F3)
!      CALL WDIALOGPUTDOUBLE(IDF_XREF,SYSTEM(92))
!      CALL WDIALOGPUTDOUBLE(IDF_YREF,SYSTEM(93))
!      CALL WDIALOGCLEARFIELD(IDF_CX)
!      CALL WDIALOGCLEARFIELD(IDF_CY)
!      CALL WDIALOGFIELDSTATE(IDF_CX,2)
!      CALL WDIALOGFIELDSTATE(IDF_CY,2)
!                      ELSE
! C       PXIM FANG, PXIM FANG
!      CALL WDIALOGPUTRADIOBUTTON(IDF_F4)
!      CALL WDIALOGPUTDOUBLE(IDF_XREF,SYSTEM(92))
!      CALL WDIALOGPUTDOUBLE(IDF_YREF,SYSTEM(93))
!      CALL WDIALOGCLEARFIELD(IDF_CX)
!      CALL WDIALOGCLEARFIELD(IDF_CY)
!      CALL WDIALOGFIELDSTATE(IDF_CX,2)
!      CALL WDIALOGFIELDSTATE(IDF_CY,2)
!                      END IF
!                      END IF
!      IF(SYSTEM(98).NE.0.0D0.OR.SYSTEM(99).NE.0.0D0) THEN
!      SYSTEM(98)=SYSTEM(99)
! C       RYIM AND RXIM
!      IF(SYSTEM(98).EQ.-1.0D0) THEN
! C       RXIM/RYIM
!      CALL WDIALOGPUTRADIOBUTTON(IDF_F5)
!      CALL WDIALOGPUTDOUBLE(IDF_XREF,SYSTEM(96))
!      CALL WDIALOGPUTDOUBLE(IDF_YREF,SYSTEM(97))
!      CALL WDIALOGCLEARFIELD(IDF_CX)
!      CALL WDIALOGCLEARFIELD(IDF_CY)
!      CALL WDIALOGFIELDSTATE(IDF_CX,2)
!      CALL WDIALOGFIELDSTATE(IDF_CY,2)
!                      ELSE
! C       RXIM FANG, RXIM FANG
!      CALL WDIALOGPUTRADIOBUTTON(IDF_F6)
!      CALL WDIALOGPUTDOUBLE(IDF_XREF,SYSTEM(96))
!      CALL WDIALOGPUTDOUBLE(IDF_YREF,SYSTEM(97))
!      CALL WDIALOGCLEARFIELD(IDF_CX)
!      CALL WDIALOGCLEARFIELD(IDF_CY)
!      CALL WDIALOGFIELDSTATE(IDF_CX,2)
!      CALL WDIALOGFIELDSTATE(IDF_CY,2)
!                      END IF
!                      END IF
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
   use iso_fortran_env, only: real64
  implicit none
  class(ksolve) :: self
  type(solve_options), dimension(:) :: solveOptions
  integer :: id
  integer :: i
  
  
  do i=1,size(solveOptions)
    if (solveOptions(i)%id_solve.EQ.self%id_solve) then
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

function getAbsYFieldText(self, idxFld) result(strAbsFld)
  use type_utils, only: real2str
  class(sys_config), intent(in) :: self
  character(len=80) :: strAbsFld
  integer :: idxFld


  strAbsFld = trim(real2str(self%refFieldValue(2)*self%relativeFields(2,idxFld),2))
  select case(self%currFieldID)
  case(FIELD_OBJECT_ANGLE_DEG)  
       strAbsFld = trim(strAbsFld)//" deg"
  case(FIELD_OBJECT_HEIGHT)
    strAbsFld = trim(strAbsFld)//" "//self%getDimensions()
  case(FIELD_REAL_IMAGE_HEIGHT)
    strAbsFld = trim(strAbsFld)//" "//self%getDimensions()      
  end select

 
end function

subroutine setNumFields(self, numFields)
  use DATLEN
  class(sys_config), intent(inout) :: self
  integer, intent(in) :: numFields
  CFLDCNT = numFields
  self%numFields = numFields

end subroutine

subroutine setRefWavelengthIndex(self, refWavelengthIdx)
  use mod_system, only: sys_set_wl_ref
  use DATLEN
  class(sys_config), intent(inout) :: self
  integer, intent(in) :: refWavelengthIdx
  self%refWavelengthIndex = refWavelengthIdx
  call sys_set_wl_ref( DBLE(self%refWavelengthIndex))

end subroutine

function getWavelength(self, index)result (wavelength)
  use mod_system, only: sys_wavelength
  class(sys_config), intent(in) :: self
  integer, intent(in) :: index
  double precision :: wavelength
  wavelength = sys_wavelength(index)

end function

function getSpectralWeight(self, index) result(weight)
  use mod_system, only: sys_wl_weight
  use iso_fortran_env, only: real64
  class(sys_config), intent(in) :: self
  integer, intent(in) :: index
  real(real64) :: weight
  weight = sys_wl_weight(index)

end function

function getRefWavelengthIndex(self) result(idx)
  use mod_system, only: sys_wl_ref
  class(sys_config), intent(in) :: self
  integer :: idx
  idx = INT(sys_wl_ref())

end function

function getNumWavelengths(self) result(n)
  use mod_system, only: sys_wl_weight
  class(sys_config), intent(in) :: self
  integer :: n
  integer :: i
  n = 0
  do i = 1, 10
    if (sys_wl_weight(i) /= 0.0D0) n = n + 1
  end do

end function

function getDefaultWavelength(self, index) result(wl)
  use mod_system, only: sys_wv_default
  use iso_fortran_env, only: real64
  class(sys_config), intent(in) :: self
  integer, intent(in) :: index
  real(real64) :: wl
  wl = sys_wv_default(index)

end function

subroutine setWavelengths(self, index, wavelength)
  use mod_system, only: sys_set_wavelength
  use iso_fortran_env, only: real64
  class(sys_config), intent(inout) :: self
  integer, intent(in) :: index
  real(real64), intent(in) :: wavelength
  call sys_set_wavelength(index, wavelength)
  self%wavelengths(index) = wavelength

end subroutine

subroutine setNumberofWavelengths(self)
  ! record wavelngths with nonzero spectral weight.
  ! mainly used for plotting
   use iso_fortran_env, only: real64
  implicit none
  class(sys_config), intent(inout) :: self
  integer :: i
  integer, dimension(10) :: nonzerowavelengths
  !self%numWavelengths = 0
  self%numWavelengths = size(pack(self%spectralWeights, self%spectralWeights /= 0))
  self%wavelengthIndices(1:self%numWavelengths) =  &
  & pack([(i,i=1,size(self%spectralWeights))],self%spectralWeights /= 0)
  self%wavelengthIndices(self%numWavelengths+1:size(self%wavelengthIndices)) = 0

end subroutine

subroutine setSpectralWeights(self, index, weight)
  use mod_system, only: sys_set_wl_weight
  use iso_fortran_env, only: real64
  class(sys_config), intent(inout) :: self
  integer, intent(in) :: index
  real(real64), intent(in) :: weight
  call sys_set_wl_weight(index, weight)
  self%spectralWeights(index) = weight

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
  write(fID, *) "LEN"
  write(fID, *) "TIT "//"'"//trim(self%lensTitle)//"'"
        ! Store dimensions
  select case(self%currLensUnitsID)
  case(LENS_UNITS_MM)
    strOutLine = "DIM M" 
  case(LENS_UNITS_CM)
    strOutLine = "DIM C"
  case(LENS_UNITS_INCHES)
    strOutLine = "DIM I"
  end select
  write(fID, *) trim(strOutLine)

  ! Store Aperture
  select case(self%currApertureID)
  case(APER_ENTR_PUPIL_DIAMETER)
    strOutLine = "EPD "//real2str(self%refApertureValue(2),4)
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
    write(fID, *) trim(strWLwgt)
  end if

    ! Print Ref wavelength
    strOutLine = "REF "// int2str(self%refWavelengthIndex)
    write(fID, *) trim(strOutLine)
       
  ! Store Field
    select case(self%currFieldID)
    case(FIELD_OBJECT_ANGLE_DEG)
       strXFLD = 'XAN'
       strYFLD = 'YAN'
    case(FIELD_OBJECT_HEIGHT)
       strXFLD = 'XOB'
       strYFLD = 'YOB'

    end select


       do ii=1,self%numFields
        strXFLD = trim(strXFLD)//blankStr(1)//real2str(self%refFieldValue(1)*self%relativeFields(1,ii),4)
        strYFLD = trim(strYFLD)//blankStr(1)//real2str(self%refFieldValue(2)*self%relativeFields(2,ii),4)
       end do
       write(fID, *) trim(strXFLD)
       write(fID, *) trim(strYFLD)


    ! Field weights not supported, so for now just output all 100s
    strFLDWGT = 'WTF'
    do ii=1,self%numFields
      strFLDWGT = trim(strFLDWGT)//blankStr(5)//'100'
    end do
    write(fID, *) trim(strFLDWGT)

    ! Per-field vignetting factors (only emit fields that have any set, so a lens
    ! with no vignetting saves identically to before this feature).
    do ii=1,self%numFields
      if (any(self%vignetting(:,ii) /= 0.0_real64)) then
        write(fID, *) "SET VIG "//trim(int2str(ii))//blankStr(1)// &
          real2str(self%vignetting(1,ii),4)//blankStr(1)// &
          real2str(self%vignetting(2,ii),4)//blankStr(1)// &
          real2str(self%vignetting(3,ii),4)//blankStr(1)// &
          real2str(self%vignetting(4,ii),4)
      end if
    end do



 

end subroutine

subroutine setAbsoluteFields(self, absFields, fieldDir)
  ! fieldDir =1 : X
  ! fieldDir =2 : Y
  use type_utils, only: real2str, int2str
  use iso_fortran_env, only: real64
  use DATLEN
  implicit none

  class(sys_config), intent(inout) :: self
  real(kind=real64) :: absFields(:)
  real(kind=real64) :: maxField = 0.0_real64

  integer, intent(in) :: fieldDir
  integer :: i
  maxField = 0.0_real64
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
    if (maxField /= 0.0_real64) then
      self%relativeFields(fieldDir, i) = absFields(i)/maxField
    else
      self%relativeFields(fieldDir, i) = 0.0_real64
    end if

  end do
  CFLDS = self%relativeFields  

end subroutine

subroutine setRelativeFields(self, col, row, newval)
  use DATLEN
   use iso_fortran_env, only: real64
  implicit none
  class(sys_config), intent(inout) :: self
  integer, intent(in) :: col, row
  real, intent(in) :: newval
  ! Need to convert from absolute to relative
  if (self%refFieldValue(col) < newval) then
     self%refFieldValue(col) = newval
     call self%setRefFieldKDP()
  end if

  self%relativeFields(col, row) = newval/self%refFieldValue(col)
  CFLDS = self%relativeFields

end subroutine

! Set the four CODE V vignetting factors for one field point (1-based index).
subroutine setVignetting(self, field, vuy, vly, vux, vlx)
  use iso_fortran_env, only: real64
  implicit none
  class(sys_config), intent(inout) :: self
  integer, intent(in) :: field
  real(kind=real64), intent(in) :: vuy, vly, vux, vlx
  if (field < 1 .or. field > 10) return
  self%vignetting(1, field) = vuy
  self%vignetting(2, field) = vly
  self%vignetting(3, field) = vux
  self%vignetting(4, field) = vlx
end subroutine

! Clear all per-field vignetting factors. Called wherever the lens is replaced
! (new lens / file load), mirroring zoom_reset; the loaded file's own SET VIG lines
! (emitted by genSaveOutputText) then re-apply any that belong to that lens.
subroutine resetVignetting(self)
  use iso_fortran_env, only: real64
  implicit none
  class(sys_config), intent(inout) :: self
  self%vignetting = 0.0_real64
end subroutine

! Return the four vignetting factors (VUY,VLY,VUX,VLX) for one field point.
function getVignetting(self, field) result(vig)
  use iso_fortran_env, only: real64
  implicit none
  class(sys_config), intent(in) :: self
  integer, intent(in) :: field
  real(kind=real64) :: vig(4)
  if (field < 1 .or. field > 10) then
    vig = 0.0_real64
  else
    vig = self%vignetting(:, field)
  end if
end function


subroutine updateApertureSelectionByCode(self, ID_SELECTION, xAp, yAp, xySame)
  use DATLEN
   use iso_fortran_env, only: real64
  implicit none
  class(sys_config), intent(inout) :: self
  integer, intent(in) :: ID_SELECTION
  real :: xAp, yAp, xApertureRadius, yApertureRadius
  integer :: xySame
  character(len=23) :: strXAp, strYAp
  !self%currApertureID = ID_SELECTION

  select case (ID_SELECTION)

  case (APER_OBJECT_NA)

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

!       SAY/SAX
!        CALL WDIALOGGETDOUBLE(IDF_X,DW1)
!        CALL WDIALOGGETDOUBLE(IDF_Y,DW2)
     xApertureRadius = xAp/2.0
     yApertureRadius = yAp/2.0

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
    call PROCESKDP('U L')
     IF(xySame.EQ.1) THEN
       CALL PROCESKDP('SAY FLOAT')
     ELSE
       CALL PROCESKDP('SAY FLOAT')
       CALL PROCESKDP('SAX FLOAT')
     END IF
     call PROCESKDP('EOS')


  end select

  ! Make sure we have the up to date values
  call self%updateParameters()


end subroutine

subroutine updateFieldSelectionByCode(self, ID_SELECTION)
 use mod_system, only: sys_set_pxim, sys_set_pxim_fang_set, sys_set_pyim, sys_set_pyim_fang_set, &
    & sys_set_rxim, sys_set_rxim_fang_set, sys_set_ryim, sys_set_ryim_fang_set, &
    & sys_set_scx, sys_set_scx_fang, sys_set_scx_set, sys_set_scy, sys_set_scy_fang, &
    & sys_set_scy_fang_set, sys_set_scy_set
 use DATLEN
 class(sys_config), intent(inout) :: self
 integer, intent(in) :: ID_SELECTION
 select case (ID_SELECTION)
 case (FIELD_OBJECT_HEIGHT)

   call sys_set_scx( self%refFieldValue(1))
   call sys_set_scy( self%refFieldValue(2))
   call sys_set_scy_set(1.0D0)
   call sys_set_scx_set(1.0D0)
   call sys_set_scy_fang_set(0.0D0)
   call sys_set_pxim_fang_set(0.0D0)
   call sys_set_pyim_fang_set(0.0D0)
   call sys_set_rxim_fang_set(0.0D0)
   call sys_set_ryim_fang_set(0.0D0)
 case (FIELD_OBJECT_ANGLE_DEG)

   call sys_set_scx_fang( self%refFieldValue(1))
   call sys_set_scy_fang( self%refFieldValue(2))
   call sys_set_scy_set(1.0D0)
   call sys_set_scx_set(1.0D0)
   call sys_set_scy_fang_set(1.0D0)
   call sys_set_pxim_fang_set(0.0D0)
   call sys_set_pyim_fang_set(0.0D0)
   call sys_set_rxim_fang_set(0.0D0)
   call sys_set_ryim_fang_set(0.0D0)

 case (FIELD_PARAX_IMAGE_HEIGHT)
   call sys_set_pxim( self%refFieldValue(1))
   call sys_set_pyim( self%refFieldValue(2))
   call sys_set_scy_set(1.0D0)
   call sys_set_scx_set(1.0D0)
   call sys_set_pxim_fang_set(-1.0D0)
   call sys_set_pyim_fang_set(-1.0D0)
   SYSTEM(96:99)=0.0D0

 case (FIELD_PARAX_IMAGE_SLOPE_TAN)
   call sys_set_pxim( self%refFieldValue(1))
   call sys_set_pyim( self%refFieldValue(2))
   call sys_set_scy( 0.0D0)
   call sys_set_scx( 0.0D0)
   call sys_set_scy_fang( 0.0D0)
   call sys_set_scx_fang( 0.0D0)
   SYSTEM(96:97) = 0.0D0  

 case (FIELD_REAL_IMAGE_HEIGHT)
   call sys_set_rxim( self%refFieldValue(1))
   call sys_set_ryim(  self%refFieldValue(2))
   call sys_set_scy( 0.0D0)
   call sys_set_scx( 0.0D0)
   call sys_set_scy_fang( 0.0D0)
   call sys_set_scx_fang( 0.0D0)
   SYSTEM(92:93) = 0.0D0    
 case (FIELD_REAL_IMAGE_HEIGHT_DEG)
   call sys_set_rxim( self%refFieldValue(1))
   call sys_set_ryim( self%refFieldValue(2))
   call sys_set_scy( 0.0D0)
   call sys_set_scx( 0.0D0)
   call sys_set_scy_fang( 0.0D0)
   call sys_set_scx_fang( 0.0D0)
   SYSTEM(92:93) = 0.0D0  
 end select 

end subroutine

subroutine updateRayAimSelectionByCode(self, ID_SELECTION)
 use mod_system, only: sys_set_aplanatic_aim, sys_set_ray_aiming, sys_set_telecentric
 use DATLEN
 class(sys_config), intent(inout) :: self
 integer, intent(in) :: ID_SELECTION
 select case (ID_SELECTION)

 ! !FROM LDM1.FOR

 ! !SET  RAY AIMING ON
 ! !SYSTEM(62)=1.0D0
 ! !SET TEL OFF
 ! !SYSTEM(63)=0.0D0
 ! !SET AIMAPL OFF
 ! !SYSTEM(70)=0.0D0

  case (RAYAIM_PARAX)
    call sys_set_ray_aiming( 0.0D0)
    call sys_set_telecentric( 0.0D0)
    call sys_set_aplanatic_aim( 0.0D0)
  case (RAYAIM_REAL)
   call sys_set_ray_aiming( 1.0D0)
   call sys_set_telecentric( 0.0D0)
   call sys_set_aplanatic_aim( 0.0D0)
 case (RAYAIM_APLANATIC)
   call sys_set_ray_aiming( 0.0D0)
   call sys_set_telecentric( 0.0D0)
   call sys_set_aplanatic_aim( 1.0D0)
  case (RAYAIM_TELE)
   call sys_set_ray_aiming( 0.0D0)
   call sys_set_telecentric( 1.0D0)
   call sys_set_aplanatic_aim( 0.0D0)
  end select    

  self%currRayAimID = ID_SELECTION

end subroutine


function getFieldText(self) result(fldText)
   use iso_fortran_env, only: real64
  implicit none
  class(sys_config) :: self
  character(len=150) :: fldText

  fldText = self%lensUnits(self%currLensUnitsID)%text
  if (self%currFieldID.EQ.FIELD_OBJECT_ANGLE_DEG) then
    fldText = "Object Angle [deg]"
  end if
  if (self%currFieldID.EQ.FIELD_OBJECT_HEIGHT) then 
    fldText = "Object Height ["//trim(self%getLensUnitsText())//"]"
  end if
  if (self%currFieldID.EQ.FIELD_REAL_IMAGE_HEIGHT) then 
    fldText = "Image Height ["//trim(self%getLensUnitsText())//"]"
  end if 
  if (self%currFieldID.EQ.FIELD_PARAX_IMAGE_HEIGHT) then 
    fldText = "Paraxial Image Height ["//trim(self%getLensUnitsText())//"]"
  end if    

end function

function getLensUnitsText(self) result(lenText)
   use iso_fortran_env, only: real64
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
 use DATLEN
 class(ksolve) :: self
 type(lens_data) :: lData
 integer :: row, solve_type
 character(len=280) :: outTxt
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
  use mod_surface
  use mod_system, only: sys_last_surf, sys_ref_surf
  use DATMAI
  use DATLEN
  implicit none
  class(lens_data) :: self
  integer :: JJ
  real(kind=real64) :: INDEX, VNUM, RD
  JJ = 0
  call self%set_num_surfaces(INT(sys_last_surf()) + 1)
  self%ref_stop = INT(sys_ref_surf()+1)
  DO JJ=0,self%num_surfaces-1
    CALL SINDEXJN(JJ, INDEX, VNUM)
    IF(surf_curvature(JJ).EQ.0.0D0) THEN
      RD=0.0D0
    ELSE
      RD=1.0D0/(surf_curvature(JJ))
    END IF

!     Dump data to interface
    self%radii(JJ+1) = RD
    self%curvatures(JJ+1) = surf_curvature(JJ)
    self%thicknesses(JJ+1) = surf_thickness(JJ)
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

    ! Pickup and Solvesstorage
    self%pickups(1:6,JJ+1,1:45) = PIKUP(1:6,JJ,1:45)
    self%solves(1:9,JJ+1) = SOLVE(1:9,JJ)

    self%thickSolves(JJ+1) = self%setSolveData(JJ+1,ID_PICKUP_THIC, thick_solves)
    
   !call check_clear_apertures(self)


  END DO


end subroutine


function isFirstOrderDataValid(self) result(boolResult)
  class(paraxial_ray_trace_data) :: self
  logical :: boolResult

  boolResult = .TRUE.

  if (self%EFL == 0 .AND. self%BFL == 0 .AND. self%FFL == 0 ) then
    boolResult = .FALSE.
  end if

end function



subroutine calculateFirstOrderParameters(self, lData)
  use mod_surface
  use DATLEN
   use iso_fortran_env, only: real64
  implicit none
  class(paraxial_ray_trace_data) :: self
  type(lens_data) :: lData
  integer :: I,J
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
  IF(DABS(PXTRAY(2,J)).GE.1.0D-15) THEN
    self%FNUM=-1.0D0/(2.0D0*PXTRAY(2,J))/lData%surf_index(J)
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
! Entrance pupil position is where the unrefracted chief ray crosses the optical
! axis.   

self%ENPUPPOS = ENPUZ
self%EXPUPPOS = EXPUZ+self%imageDistance


end subroutine

function getObjectThicknessToSetParaxialMag(self, magTgt, lData) result(t0)
  use type_utils, only: real2str
  use mod_surface
  use DATLEN
   use iso_fortran_env, only: real64
  implicit none
  class(paraxial_ray_trace_data) :: self
  type(lens_data) :: lData
  real(kind=real64) :: magTgt, t0, thick0, uk1, u01, uk2, u02, dt
  real(kind=real64) :: mag, y, uTgt, uSlope, uOffset, errAllowed
  real(kind=real64), dimension(5) :: tempSolveData
  integer :: s1, s2, maxTries, i
  t0 = 10.0 ! Arbitrary initialization

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


  ! JN 12/5/24
  ! Add some logic here to deal with a larger range of initial thicknesses and improve convergence
  maxTries = 10
  errAllowed = .01


  s2 = lData%num_surfaces
  s1 = 1
  uk1 = self%marginal_ray_angle(s2)
  u01 = self%marginal_ray_angle(s1)

  
  mag = lData%surf_index(s2)*self%marginal_ray_angle(s2) / &
  & (lData%surf_index(s1)*self%marginal_ray_angle(s1))





    ! Need to turn off the solve if it is on.
    ! TODO:  have a getter/setter fcn vs just having this global
    tempSolveData(1:5) = SOLVE(3:7,0)
    SOLVE(3:7,0) = 0.0D0

    ! Adjust object thickness by a bit and recalculate
    thick0 = lData%thicknesses(1)
    ! Need to deal with the case that the thickness is at infinity.  Not sure this is the
    ! best way but this is what I will try for now
    if (thick0 > 1E11) then
      thick0 = 200.0

    end if

    do i=1,maxTries
      if (abs((mag-magTgt)/magTgt) < errAllowed) then 
        ! We are done here
        exit
      end if



    ! THis is also a bit of an arbitrary decision
    dt = max(.01*thick0,1.0) 


    CALL PROCESSILENT("THI SO "//real2str(thick0+dt)) 

    uk2 = self%marginal_ray_angle(s2)
    u02 = self%marginal_ray_angle(s1)

    ! Compute Slope and Offset
    uSlope = (uk2-uk1)/(u02-u01)
    uOffset = uk1-u01*uSlope


    CALL PROCESSILENT("THI SO "//real2str(thick0))

    ! Finally ready to calculate new thickness
    thick0 = -1*self%marginal_ray_height(s1+1)*(magTgt*uSlope-1)/(magTgt*uOffset)
    !call LogTermFOR("New Thickness is "//real2str(t0))
  end do

  ! Finally, send the best guess at the thickness to the caller and restore the solve data

  t0 = thick0
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

function isAsphereOnSurface(self, surf) result(boolResult)
  use DATLEN, only: ALENS
   use mod_surface
  class(lens_data) :: self
  integer :: surf
  logical :: boolResult

  boolResult = .FALSE.

  if (surf_is_asphere(surf)) then
    boolResult = .TRUE.
  end if

end function

function isConicConstantOnSurface(self, surf) result(boolResult)
  use DATLEN, only: ALENS
   use mod_surface
  class(lens_data) :: self
  integer :: surf
  logical :: boolResult

  boolResult = .FALSE.
  if (surf_conic(surf) .ne. 0.0d0) then
    boolResult = .TRUE.
  end if

end function

function genAsphereSavOutputText(self, surf, fID) result(strSurfLine)
  use type_utils, only: real2str, blankStr
  use DATLEN, only: ALENS
  class(lens_data) :: self
  integer :: surf, ii
  integer :: fID
  character(len=1024) :: strSurfLine
  character(len=1), dimension(4) :: lblsPart1, lblsPart2

  lblsPart1 = [character(len=1) :: 'A', 'B', 'C', 'D']
  lblsPart2 = [character(len=1) :: 'E', 'F', 'G', 'H']

  ! A conic-only surface (conic K but no aspheric coefficients) must NOT emit an
  ! ASP block: the conic is saved separately as 'K', and an all-zero ASP both
  ! bloats the file and -- because it spans two lines -- garbles on restore
  ! ("INVALID CMD LEVEL COMMAND / EOS").  surf_is_asphere() is true for any conic
  ! (ALENS(8)/=0), so guard on the actual coefficients here.
  if (ALENS(4,surf)==0.0_real64 .and. ALENS(5,surf)==0.0_real64 .and. &
      ALENS(6,surf)==0.0_real64 .and. ALENS(7,surf)==0.0_real64 .and. &
      ALENS(81,surf)==0.0_real64 .and. ALENS(82,surf)==0.0_real64 .and. &
      ALENS(83,surf)==0.0_real64 .and. ALENS(84,surf)==0.0_real64) then
    strSurfLine = ''
    return
  end if


      ! Gather terms and add to line
  strSurfLine = blankStr(2)//'ASP ;'
  ! due to how this is stored in ALENS, use two separate loops
  !ALENS(4,surf#) -- 4th order aspheric coefficient
  !ALENS(5,surf#) -- 6th order aspheric coefficient
  !ALENS(6,surf#) -- 8th order aspheric coefficient
  !ALENS(7,surf#) -- 10th order aspheric coefficient      
  do ii = 1,4
      strSurfLine = trim(strSurfLine)//blankStr(1)//lblsPart1(ii)//blankStr(1)// &
      & trim(real2str(ALENS(ii+3,surf),sci=.TRUE.))//' ;'
  end do
  write(fID, *) strSurfLine(1:len_trim(strSurfLine)-1)
  strSurfLine = ' '
  !     ALENS(81,surf#) -- 12th order aspheric coefficient
  !     ALENS(82,surf#) -- 14th order aspheric coefficient
  !     ALENS(83,surf#) -- 16th order aspheric coefficient
  !     ALENS(84,surf#) -- 18th order aspheric coefficient  
  do ii = 1,4
      strSurfLine = trim(strSurfLine)//blankStr(1)//lblsPart2(ii)//blankStr(1)// &
      & trim(real2str(ALENS(ii+80,surf),sci=.TRUE.))//' ;'
  end do
  ! Drop the trailing ' ;' (the -1 removes the semicolon, as for the A-D line).
  ! Previously this also blanked the semicolon first, so the -1 then truncated a
  ! real digit (e.g. "...D+00" -> "...D+0"), corrupting the value on restore.
  write(fID, *) strSurfLine(1:len_trim(strSurfLine)-1)
    

end function

function setSolveData(self, surf, solve_type, solveOptions) result(currSolve)
   use mod_surface
   use iso_fortran_env, only: real64
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

! This is based on SETCLAP, but does not update the ALENS array
! It is to serve as a band-aid until I have time to better abstract
! lens data management
! I want to be able to accurately report to users what the 
! clear apertures are, but not break existing KDP functionality,
! Which assumes that a stored value of 0 means it is automatically calculated
! So the purpose here is to calculate the clear aperture and if surf_clap_dim(i, 1) is
! 0 then I will report the values calculated here to the user.                            

SUBROUTINE check_clear_apertures(lData, surfaces)
  !use global_widgets, only: curr_lens_data

  use type_utils ! DEBUG
!
  use DATLEN
  use DATMAI
  use mod_surface
  use mod_surface_type, only: surf_slot
  use mod_system, only: sys_wl_ref
  use mod_system, only: sys_last_surf
   use iso_fortran_env, only: real64
  IMPLICIT NONE
  class(lens_data), intent(inout) :: lData
  ! Typed surface array, passed in by the caller (which can reach ldm without the
  ! kdp_data_types<->mod_lens_data_manager module cycle). Declared ALLOCATABLE so an
  ! unallocated ldm%surfaces is a legal actual argument (the writes below are guarded).
  ! We write ray-traced automatic extents into each surface's clap%auto_* (display only).
  type(surf_slot), allocatable, intent(inout) :: surfaces(:)

!
!       THIS IS SUBROUTINE SETCLAP
!
  INTEGER FWARN,RWARN,J,I,K,L,M,N,KK,ALLOERR
!
LOGICAL OLDLDIF,OLDLDIF2
!
real(real64) HY,HX,YF,XF,YR,XR,HXMAX,HXMIN,HYMAX,HYMIN &
,XFFIX,YFFIX,XRFIX,YRFIX,XCENPOS,YCENPOS,RMAX,XLO,XHI,YLO,YHI

real(real64) VERARRAY,XRAD,YRAD,RRAD
DIMENSION VERARRAY(:,:)
ALLOCATABLE :: VERARRAY
DEALLOCATE(VERARRAY,STAT=ALLOERR)
ALLOCATE(VERARRAY(1:220,0:MAXSUR),STAT=ALLOERR)
! Zero it: dummy surfaces store ray data only for the on-axis field, so the
! unwritten slots must read 0 (not ALLOCATE garbage) when we size them below.
VERARRAY = 0.0D0

10   FORMAT('          CLEAR APERTURE ASSIGNED TO SURFACE ',I3)
15   FORMAT('CLEAR APERTURE DIMENSIONS CHANGED AT SURFACE ',I3)

! Here we want to do all surfaces, 
W1 = 1.0
W2 = sys_last_surf()


!       HANDLE NO SURFACES
          IF(sys_last_surf().EQ.0.0) THEN
  WRITE(OUTLYNE,*)'LENS SYSTEM HAS NO SURFACES'
CALL SHOWIT(1)
  WRITE(OUTLYNE,*)'NO PARAXIAL DATA EXISTS'
CALL SHOWIT(1)
          CALL MACFAL
DEALLOCATE(VERARRAY,STAT=ALLOERR)
          RETURN
          END IF

  IF(SQ.EQ.0) THEN
             SQ=1
             WQ='REAL'
             END IF

!IF(WQ.EQ.'REAL'.OR.WQ.EQ.'VREAL') THEN

RWARN=0
FWARN=0      

!     EIGHT PLACES AROUND THE FULL FOV
                 KK=0
                 DO K=0,8
IF(surf_clap_type(NEWOBJ) /= 2.AND.surf_clap_type(NEWOBJ) /= 4 &
.AND.surf_array_parity(NEWOBJ) == 0) THEN
IF(K.EQ.0) THEN
YF=0.0D0
XF=1.0D0
END IF
IF(K.EQ.1) THEN
YF=DSQRT(2.0D0)/2.0D0
XF=DSQRT(2.0D0)/2.0D0
END IF
IF(K.EQ.2) THEN
YF=1.0D0
XF=0.0D0
END IF
IF(K.EQ.3) THEN
YF=DSQRT(2.0D0)/2.0D0
XF=-DSQRT(2.0D0)/2.0D0
END IF
IF(K.EQ.4) THEN
YF=0.0D0
XF=-1.0D0
END IF
IF(K.EQ.5) THEN
YF=-DSQRT(2.0D0)/2.0D0
XF=-DSQRT(2.0D0)/2.0D0
END IF
IF(K.EQ.6) THEN
YF=-1.0D0
XF=0.0D0
END IF
IF(K.EQ.7) THEN
YF=-DSQRT(2.0D0)/2.0D0
XF=DSQRT(2.0D0)/2.0D0
END IF
IF(K.EQ.8) THEN
YF=0.0D0
XF=0.0D0
END IF
                     ELSE
IF(surf_clap_type(NEWOBJ) == 2.AND.surf_array_parity(NEWOBJ) == 0) THEN
IF(K.EQ.0) THEN
YF=0.0D0
XF=1.0D0
         END IF
IF(K.EQ.1) THEN
YF=1.0D0
XF=1.0D0
         END IF
IF(K.EQ.2) THEN
YF=1.0D0
XF=0.0D0
         END IF
IF(K.EQ.3) THEN
YF=1.0D0
XF=-1.0D0
         END IF
IF(K.EQ.4) THEN
YF=0.0D0
XF=-1.0D0
         END IF
IF(K.EQ.5) THEN
YF=-1.0D0
XF=-1.0D0
         END IF
IF(K.EQ.6) THEN
YF=-1.0D0
XF=0.0D0
         END IF
IF(K.EQ.7) THEN
YF=-1.0D0
XF=1.0D0
         END IF
IF(K.EQ.8) THEN
YF=0.0D0
XF=0.0D0
END IF
         END IF
IF(surf_clap_type(NEWOBJ) == 4.AND.surf_array_parity(NEWOBJ) == 0) THEN
YFFIX=DABS(surf_clap_dim(NEWOBJ, 5)/surf_clap_dim(NEWOBJ, 1))
XFFIX=DABS(surf_clap_dim(NEWOBJ, 5)/surf_clap_dim(NEWOBJ, 2))
IF(K.EQ.0) THEN
YF=0.0D0
XF=1.0D0
         END IF
IF(K.EQ.1) THEN
YF=1.0D0-(0.02928D0*YFFIX)
XF=1.0D0-(0.02928D0*XFFIX)
         END IF
IF(K.EQ.2) THEN
YF=1.0D0
XF=0.0D0
         END IF
IF(K.EQ.3) THEN
YF=1.0D0-(0.02928D0*YFFIX)
XF=-1.0D0+(0.02928D0*XFFIX)
         END IF
IF(K.EQ.4) THEN
YF=0.0D0
XF=-1.0D0
         END IF
IF(K.EQ.5) THEN
YF=-1.0D0+(0.02928D0*YFFIX)
XF=-1.0D0+(0.02928D0*XFFIX)
         END IF
IF(K.EQ.6) THEN
YF=-1.0D0
XF=0.0D0
         END IF
IF(K.EQ.7) THEN
YF=-1.0D0+(0.02928D0*YFFIX)
XF=1.0D0-(0.02928D0*XFFIX)
         END IF
IF(K.EQ.8) THEN
YF=0.0D0
XF=0.0D0
END IF
         END IF
                     END IF
!     EIGHT PLACES AROUND THE APERTURE
!     DO THE FOB
SAVE_KDP(1)=SAVEINPT(1)
WC='FOB     '
WQ='        '
SQ=0
SST=0
STI=0
DF1=0
DF2=0
DF3=0
DF4=0
DF5=1
S1=1
S2=1
S3=1
S4=1
S5=0
SN=1
W1=YF
W2=XF
W3=0.0D0
W4=sys_wl_ref()
!     SET MSG TO FALSE
  MSG=.FALSE.
  CALL FFOB
IF(.NOT.REFEXT) FWARN=1
REST_KDP(1)=RESTINPT(1)
!
IF(WQ.NE.'VREAL') THEN
                 XLO=-1.0D0
                 XHI=1.0D0
                 YLO=-1.0D0
                 YHI=1.0D0
                 END IF
IF(WQ.EQ.'VREAL') THEN
CALL VIGCAL(N,XLO,XHI,1)
CALL VIGCAL(N,YLO,YHI,2)
  MSG=.FALSE.
                 END IF
                 DO L=0,7
                 KK=KK+1
                 M=(3*KK)-2
IF(surf_clap_type(NEWREF) /= 2.AND.surf_clap_type(NEWREF) /= 4 &
.AND.surf_array_parity(NEWREF) == 0) THEN
IF(L.EQ.0) THEN
YR=0.0D0
XR=XHI
END IF
IF(L.EQ.1) THEN
YR=YHI*DSQRT(2.0D0)/2.0D0
XR=XHI*DSQRT(2.0D0)/2.0D0
END IF
IF(L.EQ.2) THEN
YR=YHI
XR=0.0D0
END IF
IF(L.EQ.3) THEN
YR=YHI*DSQRT(2.0D0)/2.0D0
XR=XLO*DSQRT(2.0D0)/2.0D0
END IF
IF(L.EQ.4) THEN
YR=0.0D0
XR=XLO
END IF
IF(L.EQ.5) THEN
YR=YLO*DSQRT(2.0D0)/2.0D0
XR=XLO*DSQRT(2.0D0)/2.0D0
END IF
IF(L.EQ.6) THEN
YR=YLO
XR=0.0D0
END IF
IF(L.EQ.7) THEN
YR=YLO*DSQRT(2.0D0)/2.0D0
XR=XHI*DSQRT(2.0D0)/2.0D0
END IF
                     ELSE
IF(surf_clap_type(NEWREF) == 2.AND.surf_array_parity(NEWREF) == 0) THEN
IF(L.EQ.0) THEN
YR=0.0D0
XR=XHI
         END IF
IF(L.EQ.1) THEN
YR=YHI
XR=XHI
         END IF
IF(L.EQ.2) THEN
YR=YHI
XR=0.0D0
         END IF
IF(L.EQ.3) THEN
YR=YHI
XR=XLO
         END IF
IF(L.EQ.4) THEN
YR=0.0D0
XR=XLO
         END IF
IF(L.EQ.5) THEN
YR=YLO
XR=XLO
         END IF
IF(L.EQ.6) THEN
YR=YLO
XR=0.0D0
         END IF
IF(L.EQ.7) THEN
YR=YLO
XR=XHI
         END IF
         END IF
IF(surf_clap_type(NEWREF) == 4.AND.surf_array_parity(NEWREF) == 0) THEN
YRFIX=DABS(surf_clap_dim(NEWREF, 5)/surf_clap_dim(NEWREF, 1))
XRFIX=DABS(surf_clap_dim(NEWREF, 5)/surf_clap_dim(NEWREF, 2))
IF(L.EQ.0) THEN
YR=0.0D0
XR=XHI
         END IF
IF(L.EQ.1) THEN
YR=YHI-(0.02928D0*YRFIX*YHI)
XR=XHI-(0.02928D0*XRFIX*XHI)
         END IF
IF(L.EQ.2) THEN
YR=YHI
XR=0.0D0
         END IF
IF(L.EQ.3) THEN
YR=YHI-(0.02928D0*YRFIX*YHI)
XR=XLO-(0.02928D0*XRFIX*XLO)
         END IF
IF(L.EQ.4) THEN
YR=0.0D0
XR=-XLO
         END IF
IF(L.EQ.5) THEN
YR=YLO-(0.02928D0*YRFIX*YLO)
XR=XLO-(0.02928D0*XRFIX*XLO)
         END IF
IF(L.EQ.6) THEN
YR=-YLO
XR=0.0D0
         END IF
IF(L.EQ.7) THEN
YR=YLO-(0.02928D0*YFFIX*YLO)
XR=XHI-(0.02928D0*XFFIX*XHI)
         END IF
         END IF
         END IF
!     TRACE THE RAY
SAVE_KDP(1)=SAVEINPT(1)
  WQ='        '
  SQ=0
  SST=0
  STI=0
  DF1=0
  DF2=0
  DF3=0
  DF4=1
  DF5=1
  S1=1
  S2=1
  S3=1
  S4=0
  S5=0
  SN=1
  W1=YR
  W2=XR
  W3=sys_wl_ref()
  WC='RAY     '
  CALL RRAY
IF(.NOT.RAYEXT) RWARN=1
  REST_KDP(1)=RESTINPT(1)
!     SAVE RAY DATA
                 DO I=0,INT(sys_last_surf())
!     Store the ray footprint at EVERY surface, including dummies, so the
!     aperture stop (a flat air-air dummy) is sized from the beam that passes
!     through it instead of being left at 0.
          IF(.TRUE.) THEN
             VERARRAY(M,I)=RAYRAY(1,I)
             VERARRAY(M+1,I)=RAYRAY(2,I)
VERARRAY(M+2,I)=DSQRT((RAYRAY(1,I)**2)+(RAYRAY(2,I)**2))
                 END IF
                 END DO
                 END DO
                 END DO
LDIF2=OLDLDIF2
LDIF=OLDLDIF
!
!     PROCESS DATA
DO I=0,INT(sys_last_surf())
IF(I.GE.INT(W1).AND.I.LE.INT(W2)) THEN
          IF(.NOT.DUMMMY(I).OR.DF1.EQ.0.AND.DF2.EQ.0) THEN
HXMAX=-1.0D10
HYMAX=-1.0D10
HXMIN=1.0D10
HYMIN=1.0D10
DO J=1,214,3
IF(VERARRAY(J,I).GT.HXMAX) HXMAX=VERARRAY(J,I)
IF(VERARRAY(J,I).LT.HXMIN) HXMIN=VERARRAY(J,I)
END DO
DO J=2,215,3
IF(VERARRAY(J,I).GT.HYMAX) HYMAX=VERARRAY(J,I)
IF(VERARRAY(J,I).LT.HYMIN) HYMIN=VERARRAY(J,I)
END DO
                  END IF
                  END IF
         END DO
!
DO I=0,INT(sys_last_surf())
IF(I.GE.INT(W1).AND.I.LE.INT(W2)) THEN
          ! Size every surface from its ray-traced beam footprint, INCLUDING
          ! flat air-air "dummy" surfaces (e.g. an aperture stop marked with
          ! STO).  Dummies store ray data only for the on-axis field; the rest
          ! of VERARRAY is zeroed above, so a dummy with no footprint stays 0
          ! while one the beam passes through gets its true semi-diameter.
      IF(.TRUE.) THEN
  RMAX=-1.0D10
  HXMAX=-1.0D10
  HYMAX=-1.0D10
  HXMIN=1.0D10
  HYMIN=1.0D10
  DO J=1,214,3
      IF(VERARRAY(J,I).GT.HXMAX) HXMAX=VERARRAY(J,I)
      IF(VERARRAY(J,I).LT.HXMIN) HXMIN=VERARRAY(J,I)
      XCENPOS=(HXMAX+HXMIN)/2.0D0
  END DO
DO J=2,215,3
IF(VERARRAY(J,I).GT.HYMAX) HYMAX=VERARRAY(J,I)
IF(VERARRAY(J,I).LT.HYMIN) HYMIN=VERARRAY(J,I)
YCENPOS=(HYMAX+HYMIN)/2.0D0
END DO
DO J=3,216,3
IF(VERARRAY(J,I).GT.RMAX) RMAX=VERARRAY(J,I)
END DO
YRAD=DABS((HYMAX-HYMIN)/2.0D0)
XRAD=DABS((HXMAX-HXMIN)/2.0D0)
!call LogTermFOR("YRAD is "//trim(real2str(YRAD)))
!call LogTermFOR("XRAD is "//trim(real2str(XRAD)))
!call LogTermFOR("surf_clap_dim(I, 1) is "//trim(real2str(surf_clap_dim(I, 1))))

! Store the ray-traced automatic extent (display only) on the typed clap.
! User-set apertures live in ALENS and surface via display_semi_y's is_set path.
IF(allocated(surfaces)) THEN
  IF(I.GE.lbound(surfaces,1).AND.I.LE.ubound(surfaces,1)) THEN
    IF(allocated(surfaces(I)%s)) THEN
      surfaces(I)%s%clap%auto_semi_y = YRAD
      surfaces(I)%s%clap%auto_semi_x = XRAD
    END IF
  END IF
END IF

RRAD=YRAD


 ELSE
  ! Dummy surface: no ray-traced extent. clap%auto_* stay at their loaded
  ! defaults (0); any user aperture shows via display_semi_y's is_set path.
                  END IF
              END IF
         END DO
IF(FWARN.NE.0) WRITE(OUTLYNE,2004)
IF(RWARN.NE.0) WRITE(OUTLYNE,2005)
CALL SHOWIT(0)
IF(FWARN.NE.0.OR.RWARN.NE.0) THEN
WRITE(OUTLYNE,2006)
CALL SHOWIT(0)
WRITE(OUTLYNE,2007)
CALL SHOWIT(0)
             END IF
2004 FORMAT('WARNING: SOME CHIEF RAYS COULD NOT BE TRACED')
2005 FORMAT('WARNING: SOME MARGINAL RAYS COULD NOT BE TRACED')
2006 FORMAT('LIMIT RAY DATA MAY BE IN ERROR, CLAPS NOT ASSIGNED')
2007 FORMAT('CHANGE OBJECT HT. OR REF. AP. HT. AND RE-RUN')
DEALLOCATE(VERARRAY,STAT=ALLOERR)
                  !RETURN
                  !END IF
END !check_clear_apertures


end module kdp_data_types
