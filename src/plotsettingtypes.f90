module mod_lens_editor_settings
type lens_editor_settings
     integer changed
     integer numSurfaces
     !real, ALLOCATABLE radii(:), thicknesses(:)



contains
    procedure, public, pass(self) :: lens_editor_is_changed

  end type lens_editor_settings



interface lens_editor_settings
  module procedure :: lens_editor_settings_constructor
end interface lens_editor_settings

contains

type(lens_editor_settings) function lens_editor_settings_constructor() result(self)
    self%changed = 0
    call getOpticalSystemLastSurface(self%numSurfaces)

    self%numSurfaces = self%numSurfaces + 1 ! Since surface number starts with 022

end function lens_editor_settings_constructor


function lens_editor_is_changed(self) result(flag)
  class(lens_editor_settings), intent(in) :: self
  integer :: flag
  flag = self%changed
end function lens_editor_is_changed




end module mod_lens_editor_settings


module mod_lens_draw_settings
  use zoa_ui

type  lens_draw_settings
      integer plot_orientation
      integer field_symmetry
      integer changed
      integer start_surface
      integer end_surface
      real    scaleFactor
      integer autoScale
      real elevation
      real azimuth


contains
    procedure, public, pass(self) :: is_changed
    procedure, public, pass(self) :: set_plot_orientation
    procedure, public, pass(self) :: set_field_symmetry
    procedure, public, pass(self) :: set_start_surface
    procedure, public, pass(self) :: set_end_surface
    procedure, public, pass(self) :: set_autoScale
    procedure, public, pass(self) :: set_scaleFactor
    procedure, public, pass(self) :: set_elevation
    procedure, public, pass(self) :: set_azimuth
    procedure, public, pass(self) :: lens_draw_replot




end type lens_draw_settings


interface lens_draw_settings
    module procedure :: lens_draw_settings_constructor
end interface lens_draw_settings

contains
type(lens_draw_settings) function lens_draw_settings_constructor() result(self)

     self%plot_orientation = ID_LENSDRAW_YZ_PLOT_ORIENTATION
     self%changed= 0
     self%field_symmetry = ID_LENSDRAW_PLOT_WHOLE_FIELD
     self%start_surface = 1
     self%end_surface = 13
     call getOpticalSystemLastSurface(self%end_surface)
     PRINT *, "******************** END SURFACE CONSTRUCTOR ", self%end_surface
     !WRITE(OUTLYNE, *), "END SURFACE IN LD SETTINGS CONSTRUCTOR ", self%end_surface
     !CALL SHOWIT(19)

     self%scaleFactor = .045
     self%autoScale = ID_LENSDRAW_AUTOSCALE
     self%elevation = 26.2
     self%azimuth   = 232.2


end function lens_draw_settings_constructor

function is_changed(self) result(flag)
  class(lens_draw_settings), intent(in) :: self
  integer :: flag
  flag = self%changed
end function is_changed

subroutine set_plot_orientation(self, ID_SETTING)
  class(lens_draw_settings), intent(inout) :: self
  integer, intent(in) :: ID_SETTING

  if (self%plot_orientation.ne.ID_SETTING) THEN
     self%plot_orientation = ID_SETTING
     self%changed = 1
  end if


end subroutine set_plot_orientation

subroutine set_field_symmetry(self, ID_SETTING)
  class(lens_draw_settings), intent(inout) :: self
  integer, intent(in) :: ID_SETTING

  if (self%field_symmetry.ne.ID_SETTING) THEN
     self%field_symmetry = ID_SETTING
     self%changed = 1
  end if


end subroutine set_field_symmetry

subroutine set_start_surface(self, start_surface)
  class(lens_draw_settings), intent(inout) :: self
  integer, intent(in) :: start_surface

  if (self%start_surface.ne.start_surface) THEN

     self%start_surface = start_surface
     self%changed = 1
  end if


end subroutine set_start_surface

subroutine set_end_surface(self, end_surface)
  class(lens_draw_settings), intent(inout) :: self
  integer, intent(in) :: end_surface

  if (self%end_surface.ne.end_surface) THEN

     self%end_surface = end_surface
     self%changed = 1
  end if


end subroutine set_end_surface

subroutine set_elevation(self, elevation)
  class(lens_draw_settings), intent(inout) :: self
  real, intent(in) :: elevation

  if (self%elevation.ne.elevation) THEN

     self%elevation = elevation
     self%changed = 1
  end if


end subroutine set_elevation

subroutine set_azimuth(self, azimuth)
  class(lens_draw_settings), intent(inout) :: self
  real, intent(in) :: azimuth

  if (self%azimuth.ne.azimuth) THEN

     self%azimuth = azimuth
     self%changed = 1
  end if


end subroutine set_azimuth

subroutine set_autoScale(self, autoScale)
  class(lens_draw_settings), intent(inout) :: self
  integer, intent(in) :: autoScale

  if (self%autoScale.ne.autoScale) THEN

     self%autoScale = autoScale
     self%changed = 1
  end if


end subroutine set_autoScale

subroutine set_scaleFactor(self, scaleFactor)
  class(lens_draw_settings), intent(inout) :: self
  real, intent(in) :: scaleFactor

  if (self%scaleFactor.ne.scaleFactor) THEN

     self%scaleFactor = scaleFactor
     self%changed = 1
  end if


end subroutine set_scaleFactor

  subroutine update_zoa_ui_settings_and_replot(ID_SETTING)

    integer, intent(in) :: ID_SETTING

      !if (is_lens_draw_setting(ID_SETTING).EQ.1)
      !   call update_lens_draw_plot(ID_SETTING)
      !endif

  end subroutine update_zoa_ui_settings_and_replot

  subroutine is_lens_draw_setting(ID_SETTING)
      integer, intent(in) :: ID_SETTING


  end subroutine is_lens_draw_setting

subroutine lens_draw_replot(self)

  class(lens_draw_settings) :: self


  character(len=40) :: command, qual_word
  character(len=100) :: ftext
  character(len=3)   :: AJ, AK
  character(len=23) :: autoScale_text, AW2, AW3


  PRINT *, "LENS DRAW REPLOT INIITIATED"
  PRINT *, "LAST SURFACE IS LENS DRAW REPLOT", self%end_surface

  command = "VIECO"


  ! Original logic in LENSED.INC
  select case (self%field_symmetry)
  case (ID_LENSDRAW_PLOT_HALF_FIELD)
       ftext = 'VIESYM OFF'
       CALL PROCESKDP(ftext)
  case (ID_LENSDRAW_PLOT_WHOLE_FIELD)
       ftext = 'VIESYM ON'
       CALL PROCESKDP(ftext)
  end select

  select case (self%plot_orientation)

  case (ID_LENSDRAW_YZ_PLOT_ORIENTATION)
       qual_word = "YZ"
  case (ID_LENSDRAW_XZ_PLOT_ORIENTATION)
      qual_word = "XZ"
  case (ID_LENSDRAW_XY_PLOT_ORIENTATION)
      qual_word = "XY"
  case (ID_LENSDRAW_ORTHO_PLOT_ORIENTATION)
       qual_word = "ORTHO"
        !CALL DTOA23(ld_settings%elevation,AW2)
        !CALL DTOA23(ld_settings%azimuth,AW3)
        WRITE(AW2, *) self%elevation
        WRITE(AW3, *) self%azimuth

        ftext ='PLOT VIEW,'//AW2//','//AW3
        PRINT *, "ORTHO TEXT IS ", ftext
        !PRINT *, "LD Settings Elevation, Azimuth is ", ld_settings%elevation, ",", ld_settings%azimuth
        CALL PROCESKDP(ftext)
  case DEFAULT
      qual_word = " "
  end select


  !INPUT='VIECO,'//','//AJ//','//AK//',1'

!      AUTOSCALE
!        CALL ITOAA(ISTARTSURF,AJ)
!        CALL ITOAA(ISTOPSURF,AK)
!        INPUT='VIECO,'//','//AJ//','//AK//',1'
!        CALL PROCES

  ! Start and End Surface
  CALL ITOAA(self%start_surface, AJ)
  CALL ITOAA(self%end_surface, AK)

  PRINT *, "AJ = ", AJ, " AK = ", AK

    !ftext= trim('VIECO,'//','//AJ//','//AK//',1')
    !PRINT *, ftext

    ! Working
    !ftext = trim(command)//" "//trim(qual_word)

  if (self%autoScale.eq.ID_LENSDRAW_MANUALSCALE) THEN
      !write(autoScale_text, *), ",", ld_settings%scaleFactor, ","
      !autoScale_text = trim(",,")
      Call DTOA23(self%scaleFactor,autoScale_text)
      PRINT *, ",", self%scaleFactor, ","
      WRITE(autoScale_text, *) self%scaleFactor

  else
      autoScale_text = trim("")
  end if


    !ftext = trim(command)//" "//trim(qual_word)//autoScale_text//AJ//","//AK//",0"

    ftext = trim(command)//" "//trim(qual_word)//","//autoScale_text//","//AJ//","//AK//",0,1"
    !if (ld_settings%plot_orientation.eq.ID_LENSDRAW_ORTHO_PLOT_ORIENTATION) THEN
    !  ftext = ftext//",1"
    !end if


    PRINT *, "Command is ", ftext
    CALL PROCESKDP(ftext)

end subroutine lens_draw_replot


end module mod_lens_draw_settings

module mod_ray_fan_settings
 use zoa_ui

type ray_fan_settings
   integer fan_type
   integer fan_wfe
   integer changed
   real maxPupil
   real minPupil
   integer numRays
   integer wavelength
   logical autoplotupdate

 contains
    procedure, public, pass(self) :: ray_fan_is_changed
    procedure, public, pass(self) :: set_ray_fan
    procedure, public, pass(self) :: set_fan_wfe
    procedure, public, pass(self) :: set_max_pupil
    procedure, public, pass(self) :: set_min_pupil
    procedure, public, pass(self) :: set_num_rays
    procedure, public, pass(self) :: set_ray_fan_wavelength
    procedure, public :: replot => ray_fan_replot


end type ray_fan_settings

interface ray_fan_settings
  module procedure :: ray_fan_settings_constructor
end interface ray_fan_settings

contains

type(ray_fan_settings) function ray_fan_settings_constructor() result(self)
    self%fan_type = ID_RAYFAN_Y_FAN
    self%fan_wfe  = ID_RAYFAN_TRANSVERSE_RAY
    self%maxPupil = 1.0
    self%minPupil = -1.0
    self%numRays = 11
    self%autoplotupdate = .TRUE.
    self%wavelength = 2 ! TODO NEED TO GET DEFAULT WAVELENGTH FROM PRESCRIPTION

end function ray_fan_settings_constructor


function ray_fan_is_changed(self) result(flag)
  class(ray_fan_settings), intent(in) :: self
  integer :: flag
  flag = self%changed
end function ray_fan_is_changed

subroutine set_ray_fan(self, ID_SETTING)
  class(ray_fan_settings), intent(inout) :: self
  integer, intent(in) :: ID_SETTING

  if (self%fan_type.ne.ID_SETTING) THEN
     self%fan_type = ID_SETTING
     self%changed = 1
     if (self%autoplotupdate) call self%replot()
  end if


end subroutine set_ray_fan

subroutine set_max_pupil(self, ID_SETTING)
  class(ray_fan_settings), intent(inout) :: self
  real, intent(in) :: ID_SETTING

  if (self%maxPupil.ne.ID_SETTING) THEN
     self%maxPupil = ID_SETTING
     self%changed = 1
     if (self%autoplotupdate) call self%replot()
  end if


end subroutine set_max_pupil

subroutine set_min_pupil(self, ID_SETTING)
  class(ray_fan_settings), intent(inout) :: self
  real, intent(in) :: ID_SETTING

  if (self%minPupil.ne.ID_SETTING) THEN
     self%minPupil = ID_SETTING
     self%changed = 1
     if (self%autoplotupdate) call self%replot()
  end if


end subroutine set_min_pupil

subroutine set_num_rays(self, ID_SETTING)
  class(ray_fan_settings), intent(inout) :: self
  integer, intent(in) :: ID_SETTING

  if (self%numRays.ne.ID_SETTING) THEN
     self%numRays = ID_SETTING
     self%changed = 1
     if (self%autoplotupdate) call self%replot()
  end if


end subroutine set_num_rays

subroutine set_ray_fan_wavelength(self, ID_SETTING)
  class(ray_fan_settings), intent(inout) :: self
  integer, intent(in) :: ID_SETTING

  if (self%wavelength.ne.ID_SETTING) THEN
     self%wavelength = ID_SETTING
     self%changed = 1
     if (self%autoplotupdate) call self%replot()
  end if


end subroutine set_ray_fan_wavelength

subroutine set_fan_wfe(self, ID_SETTING)
  class(ray_fan_settings), intent(inout) :: self
  integer, intent(in) :: ID_SETTING

  if (self%fan_wfe.ne.ID_SETTING) THEN
     self%fan_wfe = ID_SETTING
     self%changed = 1
     if (self%autoplotupdate) call self%replot()
  end if


end subroutine set_fan_wfe

subroutine ray_fan_replot(self)
  class(ray_fan_settings), intent(inout) :: self

  character PART1*5, PART2*5, AJ*3, A6*3, AW1*23, AW2*23
  character(len=100) :: ftext

!
!       SIMPLE FANS
!
!
        !IF(MESSAGE%WIN.EQ.IDD_FAN1) THEN
        !CALL WDIALOGSELECT(IDD_FAN1)
        !SELECT CASE(MESSAGE%VALUE1)
        !CASE (IDOK)
!       MAX PUPIL HT
        !CALL WDIALOGGETDOUBLE(IDF_MIN,DW1)

!       MIN PUPIL HT
        !CALL WDIALOGGETDOUBLE(IDF_MAX,DW2)

!       PLOT FAN SCALE
        !CALL WDIALOGGETDOUBLE(IDF_SSI,SSI)
        !CALL DTOA23(SSI,ASSI)

!       NUMBER OF RAYS IN FAN
        !CALL WDIALOGGETINTEGER(IDF_NUM,INUM)

!       FAN WAVELENGTH
        !CALL WDIALOGGETINTEGER(IDF_WAV,IWAV)
!
!       FAN PRIMARY TYPE
!       1=YFAN
!       2=XFAN
!       3=PFAN
!       4=NFAN
        !CALL WDIALOGGETRADIOBUTTON(IDF_FT1,ISET)
!
!       FAN SECONDARY TYPE
!       1=TRANSVERSE
!       2=OPD
!       3=CD
!       4=LA
        !CALL WDIALOGGETRADIOBUTTON(IDF_Q1,JSET)
!
!       PLOTTING ON OR OFF
!       0=NO PLOT
!       1=PLOT
        !CALL WDIALOGGETCHECKBOX(IDF_PLOT,ISTATE)
!
!       PLOTTING AUTOSCALE FACTOR
!       1=AUTOSCALE
!       0=NO AUTOSCALE
        !CALL WDIALOGGETCHECKBOX(IDF_AUTOSSI,KSTATE)
        !IF(KSTATE.EQ.1.AND.SSI.EQ.0.0D0) THEN
        !KSTATE=1
        !CALL WDIALOGPUTCHECKBOX(IDF_AUTOSSI,KSTATE)
        !                END IF
!
!       TRACE THE CORRECT CHIEF RAY
        !CALL CHIEFTRACE
!
        select case (self%fan_type)
          case (ID_RAYFAN_Y_FAN)
                PART1='YFAN '
          case (ID_RAYFAN_X_FAN)
                PART1='XFAN '
          case (ID_RAYFAN_P_FAN)
                PART1='PFAN '
          case (ID_RAYFAN_N_FAN)
                PART1='NFAN '
          case DEFAULT
               PART1='YFAN '
        end select
!
        select case (self%fan_wfe)
        case (ID_RAYFAN_TRANSVERSE_RAY)
        PART2='     '
      case (ID_RAYFAN_TRANSVERSE_OPD)
        PART2='OPD  '
      case (ID_RAYFAN_CHROMATIC)
        PART2='CD   '
      case (ID_RAYFAN_LONGITUDINAL)
        PART2='LA   '
      case DEFAULT
        PART2 = '    '
      end select
!
        !CALL DTOA23(DW1,AW1)
        !CALL DTOA23(DW2,AW2)
        CALL ITOAA(self%numRays, A6)
        CALL ITOAA(self%wavelength, AJ)

        PRINT *, "Max Pupil ", self%maxPupil
        PRINT *, "Min Pupil ", self%minPupil
        PRINT *, "NumRays ", self%numRays
        PRINT *, "Wavelength ", self%wavelength

        WRITE(AW2, *) self%maxPupil
        WRITE(AW1, *) self%minPupil
        !WRITE(A6, *)  self%numRays
        !WRITE(AJ, *)  self%wavelength
        !CALL ITOAA(IWAV,AJ)
        !CALL ITOA6(INUM,A6)
!
        ftext=PART1//TRIM(PART2)//','//AW1//','//AW2//','//AJ//','//A6
        PRINT *, ftext
        CALL PROCESKDP(ftext)
        !                IF(ISTATE.EQ.1) THEN
!       PLOTTING
        !                IF(KSTATE.EQ.0) THEN
!       USE SCALE
        !INPUT='DRAWFAN,'//ASSI//',1'
        !CALL PROCES
        !                ELSE
!       AUTO SCALE
        ftext='DRAWFAN,,1'
        CALL PROCESKDP(ftext)
        ftext = 'DRAW'
        CALL PROCESKDP(ftext)
        !                END IF
        !CALL GRAPHOUTPUT
        !                END IF

        !END SELECT
        !                        END IF
!

end subroutine ray_fan_replot


end module mod_ray_fan_settings

module mod_ast_fc_dist_settings
 use zoa_ui

type ast_fc_dist_settings
   integer ast_field_dir
   integer ast_numRays
   integer changed
   integer wavelength
   logical autoplotupdate

 contains
    procedure, public :: is_changed => ast_fc_dist_is_changed
    procedure, public, pass(self) :: set_ast_field_dir
    procedure, public, pass(self) :: set_ast_num_rays
    !procedure, public, pass(self) :: set_ray_fan_wavelength
    procedure, public :: replot => ast_fc_dist_replot


end type ast_fc_dist_settings

interface ast_fc_dist_settings
  module procedure :: ast_fc_dist_settings_constructor
end interface ast_fc_dist_settings

contains

type(ast_fc_dist_settings) function ast_fc_dist_settings_constructor() result(self)
    self%ast_field_dir = ID_AST_FIELD_Y
    self%ast_numRays = 10
    self%autoplotupdate = .TRUE.
    self%wavelength = 2 ! TODO NEED TO GET DEFAULT WAVELENGTH FROM PRESCRIPTION

end function

function ast_fc_dist_is_changed(self) result(flag)
  class(ast_fc_dist_settings), intent(in) :: self
  integer :: flag
  flag = self%changed
end function

subroutine set_ast_field_dir(self, ID_SETTING)
  class(ast_fc_dist_settings), intent(inout) :: self
  integer, intent(in) :: ID_SETTING

  if (self%ast_field_dir.ne.ID_SETTING) THEN
     self%ast_field_dir = ID_SETTING
     self%changed = 1
     PRINT *, "AST FIELD DIR CHANGED!"
     if (self%autoplotupdate) call self%replot()
  end if

end subroutine

subroutine set_ast_num_rays(self, ID_SETTING)
  class(ast_fc_dist_settings), intent(inout) :: self
  integer, intent(in) :: ID_SETTING

  PRINT *, "NUM RAYS TO SET IS ", ID_SETTING
  PRINT *, "NUM OF CURRENT RAYS IS ", self%ast_numRays

  if (self%ast_numRays.ne.ID_SETTING) THEN
     self%ast_numRays = ID_SETTING
     self%changed = 1
     PRINT *, "NUM AST RAYS CHANGED!"
     if (self%autoplotupdate) call self%replot()
  end if

end subroutine

subroutine ast_fc_dist_replot(self)
  class(ast_fc_dist_settings), intent(inout) :: self

  character PART1*5, PART2*5, AJ*3, A6*3, AW1*23, AW2*23
  character(len=100) :: ftext

        ! CASE(IDF_AST)
        ! CALL WDIALOGGETRADIOBUTTON(IDF_PL3,ISET)
        ! CALL WDIALOGGETINTEGER(IDF_N2,JSET)
        ! CALL WDIALOGGETCHECKBOX(IDF_PLOTIT2,KSET)
        ! IF(JSET.LT.10) JSET=10
        ! IF(JSET.GT.50) JSET=50
        ! WRITE(OUTLYNE,*) ' '
        ! CALL SHOWIT(1)
        ! IF(ISET.EQ.1) WRITE(INPUT,*) 'AST,0,,',JSET
        ! IF(ISET.EQ.2) WRITE(INPUT,*) 'AST,90,,',JSET
        ! CALL PROCES
        ! IF(KSET.EQ.1) THEN
        ! INPUT='PLTAST,,1'
        ! CALL PROCES
        ! CALL GRAPHOUTPUT
        !
       select case (self%ast_field_dir)
       case (ID_AST_FIELD_Y)
         ftext = 'AST,0,,'
       case (ID_AST_FIELD_X)
         ftext = 'AST,90,,'
       end select
       CALL ITOAA(self%ast_numRays, A6)
       PRINT *, "COMMAND SENT TO KDP IN AST REPLOT IS ", trim(ftext)//A6
       CALL PROCESKDP(trim(ftext)//A6)


       CALL PROCESKDP('PLTAST 1')
!

end subroutine


end module
