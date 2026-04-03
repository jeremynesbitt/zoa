! surface-params.f90
! Named integer constants for ALENS and SYSTEM array column indices.
!
! Source of truth: Library/OPTMAN/LENSTORE.DOC
!
! Usage:  use surface_params
!
! These replace magic numbers like ALENS(9,I) with ALENS(A_APTYPE,I),
! and SYSTEM(62) with SYSTEM(SYS_RAY_AIMING), making code self-documenting.
!
! Note on refractive index access:
!   Wavelengths 1-5:  ALENS(A_N_OFFSET + wl, surf)  gives index at wavelength wl
!   Wavelengths 6-10: ALENS(A_N2_OFFSET + wl, surf) gives index at wavelength wl
!   e.g. wl=1 -> ALENS(46,surf), wl=6 -> ALENS(71,surf)
!
! Note on aperture parameters (ALENS 10-14):
!   Their meaning depends on the aperture type in ALENS(A_APTYPE, surf):
!     CIRC (1): A_CLAP_P1=radius, A_CLAP_P2=rad-to-flat, A_CLAP_YD=yd, A_CLAP_XD=xd, A_CLAP_P3=zd
!     RECT (2): A_CLAP_P1=y-half, A_CLAP_P2=x-half, A_CLAP_YD=yd, A_CLAP_XD=xd
!     ELLIP(3): A_CLAP_P1=y-semi, A_CLAP_P2=x-semi, A_CLAP_YD=yd, A_CLAP_XD=xd
!     RCTK (4): A_CLAP_P1=y-half, A_CLAP_P2=x-half, A_CLAP_YD=yd, A_CLAP_XD=xd, A_CLAP_P3=corner-rad
!     POLY (5): A_CLAP_P1=rad-to-corner, A_CLAP_P2=num-sides, A_CLAP_YD=yd, A_CLAP_XD=xd
!     IPOLY(6): A_CLAP_P1=poly-file-num, A_CLAP_P2=num-points, A_CLAP_YD=yd, A_CLAP_XD=xd, A_CLAP_P3=max-dim

module surface_params
  implicit none

  ! -----------------------------------------------------------------------
  ! ALENS column indices
  ! -----------------------------------------------------------------------

  ! Basic surface shape
  integer, parameter :: A_CURV       = 1    ! curvature (1/radius)
  integer, parameter :: A_CONIC      = 2    ! conic constant
  integer, parameter :: A_THI        = 3    ! thickness to next surface
  integer, parameter :: A_ASPH4      = 4    ! 4th order aspheric coefficient
  integer, parameter :: A_ASPH6      = 5    ! 6th order aspheric coefficient
  integer, parameter :: A_ASPH8      = 6    ! 8th order aspheric coefficient
  integer, parameter :: A_ASPH10     = 7    ! 10th order aspheric coefficient
  integer, parameter :: A_ASPH_FLAG  = 8    ! asphere flag (1=asphere, 0=none)

  ! Clear aperture (columns 9-15); see note above on type-dependent meanings
  integer, parameter :: A_APTYPE     = 9    ! aperture type (see AP_* constants below)
  integer, parameter :: A_CLAP_P1    = 10   ! primary aperture dimension (radius, y-half, etc.)
  integer, parameter :: A_CLAP_P2    = 11   ! secondary aperture dimension (rad-to-flat, x-half, etc.)
  integer, parameter :: A_CLAP_YD    = 12   ! y-decentration of aperture
  integer, parameter :: A_CLAP_XD    = 13   ! x-decentration of aperture
  integer, parameter :: A_CLAP_P3    = 14   ! tertiary param (zd / corner-rad / ipoly-max-dim)
  integer, parameter :: A_CLAP_TILT  = 15   ! aperture tilt angle (degrees)

  ! Obscuration (columns 16-22; same layout as clear aperture)
  integer, parameter :: A_OBSTYPE    = 16   ! obscuration type flag (same codes as A_APTYPE)

  ! Toric surface
  integer, parameter :: A_TORIC_FLAG = 23   ! toric flag (0=none, 1=Y-toric)
  integer, parameter :: A_TORIC_CURV = 24   ! toric curvature

  ! Tilts
  integer, parameter :: A_TILT_FLAG  = 25   ! tilt flag (0=none, 1=tilted)
  integer, parameter :: A_ALPHA      = 26   ! alpha tilt angle (degrees)
  integer, parameter :: A_BETA       = 27   ! beta tilt angle (degrees)
  integer, parameter :: A_GAMMA      = 28   ! gamma tilt angle (degrees)

  ! Decenters
  integer, parameter :: A_DEC_FLAG   = 29   ! decenter flag (0=none, 1=decentered)
  integer, parameter :: A_YDEC       = 30   ! y-decenter value
  integer, parameter :: A_XDEC       = 31   ! x-decenter value
  integer, parameter :: A_ZDEC       = 69   ! z-decenter value

  ! Pickups and solves
  integer, parameter :: A_PIKUP_FLAG = 32   ! pickup flag (0=none, >0=count)
  integer, parameter :: A_SOLVE_FLAG = 33   ! solves flag

  ! Surface type
  integer, parameter :: A_SURFTYPE   = 34   ! special surface type (negative = off)

  ! Anamorphic asphere
  integer, parameter :: A_ASI_FLAG   = 35   ! alternate surface intersection flag
  integer, parameter :: A_ANAM_FLAG  = 36   ! anamorphic asphere flag
  integer, parameter :: A_ANAM4      = 37   ! 4th order anamorphic aspheric term
  integer, parameter :: A_ANAM6      = 38   ! 6th order anamorphic aspheric term
  integer, parameter :: A_ANAM8      = 39   ! 8th order anamorphic aspheric term
  integer, parameter :: A_ANAM10     = 40   ! 10th order anamorphic aspheric term
  integer, parameter :: A_ANAM_CONIC = 41   ! anamorphic conic coefficient

  ! Config, label
  integer, parameter :: A_CFG_FLAG   = 42   ! config data flag
  integer, parameter :: A_ASPH2      = 43   ! 2nd order aspheric (plano only)
  integer, parameter :: A_LABEL_FLAG = 44   ! surface label flag

  ! Refractive index (see module header note)
  integer, parameter :: A_N_OFFSET   = 45   ! base for wavelengths 1-5: ALENS(A_N_OFFSET+wl, surf)
  !   ALENS(46,surf) = index at wl 1
  !   ALENS(47,surf) = index at wl 2
  !   ALENS(48,surf) = index at wl 3
  !   ALENS(49,surf) = index at wl 4
  !   ALENS(50,surf) = index at wl 5
  integer, parameter :: A_N2_OFFSET  = 65   ! base for wavelengths 6-10: ALENS(A_N2_OFFSET+wl, surf)
  !   ALENS(71,surf) = index at wl 6
  !   ALENS(72,surf) = index at wl 7
  !   ALENS(73,surf) = index at wl 8
  !   ALENS(74,surf) = index at wl 9
  !   ALENS(75,surf) = index at wl 10

  ! Aperture erase
  integer, parameter :: A_CLAP_ERASE = 51   ! clear aperture erase type flag

  ! Dummy surface
  integer, parameter :: A_DUMMY      = 68   ! dummy surface flag (0=dummy, 1=not dummy)

  ! Global coordinates
  integer, parameter :: A_GLOB_XDEC  = 90   ! global x-decenter
  integer, parameter :: A_GLOB_YDEC  = 91   ! global y-decenter
  integer, parameter :: A_GLOB_ZDEC  = 92   ! global z-decenter
  integer, parameter :: A_GLOB_ALPHA = 93   ! global alpha tilt
  integer, parameter :: A_GLOB_BETA  = 94   ! global beta tilt
  integer, parameter :: A_GLOB_GAMMA = 95   ! global gamma tilt

  ! Grating / diffractive
  integer, parameter :: A_GRATING    = 96   ! grating flag (0=none, 1=grating)
  integer, parameter :: A_GRAT_ORDER = 97   ! grating order
  integer, parameter :: A_GRAT_SPACE = 98   ! grating spacing (lens units)
  integer, parameter :: A_GRAT_NX    = 99   ! grating x-direction number
  integer, parameter :: A_GRAT_NY    = 100  ! grating y-direction number
  integer, parameter :: A_GRAT_NZ    = 101  ! grating z-direction number

  ! Deformable surface
  integer, parameter :: A_DEFORM     = 103  ! deformable surface flag
  integer, parameter :: A_DEFORM_ID  = 104  ! file ID for deformable surface
  integer, parameter :: A_DEFORM_N   = 105  ! total number of actuators
  integer, parameter :: A_DEFORM_Z   = 106  ! z-scale for deformable surface

  ! Coating
  integer, parameter :: A_COATING    = 112  ! coating file number (0=none)

  ! Higher-order aspherics
  integer, parameter :: A_ASPH12     = 81
  integer, parameter :: A_ASPH14     = 82
  integer, parameter :: A_ASPH16     = 83
  integer, parameter :: A_ASPH18     = 84
  integer, parameter :: A_ASPH20     = 85

  ! Ideal lens
  integer, parameter :: A_IDEAL_FL   = 121  ! focal length of ideal lens

  ! Multi-aperture / multi-obscuration counts
  integer, parameter :: A_MULTICLAP  = 127  ! number of multi-clap apertures (0=none)
  integer, parameter :: A_MULTICOBS  = 128  ! number of multi-cob obscurations (0=none)

  ! -----------------------------------------------------------------------
  ! Aperture type codes (value of ALENS(A_APTYPE, surf))
  ! -----------------------------------------------------------------------
  integer, parameter :: AP_NONE   = 0
  integer, parameter :: AP_CIRC   = 1   ! circular
  integer, parameter :: AP_RECT   = 2   ! rectangular
  integer, parameter :: AP_ELLIP  = 3   ! elliptical
  integer, parameter :: AP_RCTK   = 4   ! racetrack
  integer, parameter :: AP_POLY   = 5   ! regular polygon
  integer, parameter :: AP_IPOLY  = 6   ! irregular polygon

  ! -----------------------------------------------------------------------
  ! SYSTEM array indices
  ! -----------------------------------------------------------------------

  ! Wavelengths (microns)
  integer, parameter :: SYS_WL1    = 1    ! wavelength 1
  integer, parameter :: SYS_WL2    = 2    ! wavelength 2
  integer, parameter :: SYS_WL3    = 3    ! wavelength 3
  integer, parameter :: SYS_WL4    = 4    ! wavelength 4
  integer, parameter :: SYS_WL5    = 5    ! wavelength 5
  integer, parameter :: SYS_WL6    = 71   ! wavelength 6
  integer, parameter :: SYS_WL7    = 72   ! wavelength 7
  integer, parameter :: SYS_WL8    = 73   ! wavelength 8
  integer, parameter :: SYS_WL9    = 74   ! wavelength 9
  integer, parameter :: SYS_WL10   = 75   ! wavelength 10

  ! Units
  integer, parameter :: SYS_UNITS  = 6    ! units (1=in, 2=cm, 3=mm, 4=m)

  ! Wavelength pair indices
  integer, parameter :: SYS_WL_PRI1  = 7  ! first wavelength of primary pair
  integer, parameter :: SYS_WL_PRI2  = 8  ! second wavelength of primary pair
  integer, parameter :: SYS_WL_SEC1  = 9  ! first wavelength of secondary pair
  integer, parameter :: SYS_WL_SEC2  = 10 ! second wavelength of secondary pair
  integer, parameter :: SYS_WL_REF   = 11 ! reference (control) wavelength number

  ! Aperture / field
  integer, parameter :: SYS_SAY    = 12   ! semi-aperture Y
  integer, parameter :: SYS_SAX    = 13   ! semi-aperture X
  integer, parameter :: SYS_SCY    = 14   ! semi-field Y
  integer, parameter :: SYS_SCX    = 16   ! semi-field X
  integer, parameter :: SYS_FANG_Y = 21   ! SCY FANG value
  integer, parameter :: SYS_FANG_X = 23   ! SCX FANG value

  ! Surface counts / indices
  integer, parameter :: SYS_LAST_SURF = 20 ! surface number of the last surface
  integer, parameter :: SYS_REF_SURF  = 25 ! reference surface number (default=1)
  integer, parameter :: SYS_ASTOP     = 26 ! aperture stop surface (-99 if none)

  ! System mode
  integer, parameter :: SYS_MODE   = 30   ! mode (1=FOCAL,2=UFOCAL,3=AFOCAL,4=UAFOCAL)

  ! Spectral weights (wavelengths 1-5)
  integer, parameter :: SYS_WT1    = 31
  integer, parameter :: SYS_WT2    = 32
  integer, parameter :: SYS_WT3    = 33
  integer, parameter :: SYS_WT4    = 34
  integer, parameter :: SYS_WT5    = 35
  ! Spectral weights (wavelengths 6-10)
  integer, parameter :: SYS_WT6    = 76
  integer, parameter :: SYS_WT7    = 77
  integer, parameter :: SYS_WT8    = 78
  integer, parameter :: SYS_WT9    = 79
  integer, parameter :: SYS_WT10   = 80

  ! Ray aiming
  integer, parameter :: SYS_RAY_AIMING  = 62  ! ray aiming flag (0=off, 1=on)
  integer, parameter :: SYS_TELECENTRIC = 63  ! telecentric aiming flag (0=off, 1=on)
  integer, parameter :: SYS_APLANATIC   = 70  ! aplanatic ray aiming (0=off, 1=on)
  integer, parameter :: SYS_REF_ORIENT  = 59  ! reference surface orientation angle

  ! NSS / screen surface
  integer, parameter :: SYS_NSS         = 102 ! NSS surfaces present (0=no, 1=yes)
  integer, parameter :: SYS_SCREEN      = 103 ! screen surface flag (0=none, 1=present)
  integer, parameter :: SYS_SCREEN_SURF = 104 ! screen surface number
  integer, parameter :: SYS_SCREEN_D    = 105 ! screen hole diameter
  integer, parameter :: SYS_SCREEN_H    = 106 ! screen hole depth
  integer, parameter :: SYS_SCREEN_S    = 107 ! screen hole spacing

  ! Reference surface orientation flip flags
  integer, parameter :: SYS_FLIPREFX    = 128 ! flip reference X (0=off, 1=on)
  integer, parameter :: SYS_FLIPREFY    = 129 ! flip reference Y (0=off, 1=on)

  ! Alternate configurations
  integer, parameter :: SYS_CURR_CFG    = 50  ! current configuration (do not modify directly)
  integer, parameter :: SYS_HIGH_CFG    = 56  ! highest non-blank alternate config number

  ! Reverse trace
  integer, parameter :: SYS_REVERSE     = 100 ! reverse trace flag (0=no, 1=yes)

  ! Gaussian beam
  integer, parameter :: SYS_WRX  = 85
  integer, parameter :: SYS_WRY  = 86
  integer, parameter :: SYS_BDX  = 87
  integer, parameter :: SYS_BDY  = 88

end module surface_params
