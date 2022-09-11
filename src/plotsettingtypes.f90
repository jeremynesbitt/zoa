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

!module mod_ray_fan_settings
 !use zoa_ui



!contains

!end module mod_ray_fan_settings

module mod_ast_fc_dist_settings
 use zoa_ui

type ast_fc_dist_settings
   integer ast_field_dir
   integer ast_numRays
   integer changed
   integer wavelength
   logical autoplotupdate
   character(len=140) ::astcalccmd
   character(len=140) ::distcalccmd
   character(len=140) ::fccalccmd



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
    self%astcalccmd = "AST"
    self%distcalccmd = "DIST"
    self%fccalccmd = "FLDCV"


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
         ftext = ',0,,'
       case (ID_AST_FIELD_X)
         ftext = ',90,,'
       end select
       CALL ITOAA(self%ast_numRays, A6)
       self%astcalccmd = 'AST'//trim(ftext)//A6
       PRINT *, "COMMAND SENT TO KDP IN AST REPLOT IS ", self%astcalccmd
       CALL PROCESKDP(self%astcalccmd)

       self%distcalccmd = 'DIST'//trim(ftext)//A6
       self%fccalccmd   = 'FLDCV'//trim(ftext)//A6

       PRINT *, "ABOUT TO TRIGGER REPLOT!"
       CALL PROCESKDP('PLTAST 1')
!

end subroutine


end module
