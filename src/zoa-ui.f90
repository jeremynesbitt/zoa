module zoa_ui
    implicit none

! To extent this type to support easier plotting
type ui_settings
 logical :: useToolbar = .FALSE.

contains
  procedure :: replot => ui_settings_replot
end type

! DoNotDelete Unique Plot IDs
integer, parameter :: ID_NEWPLOT_LENSDRAW = 1001
integer, parameter :: ID_NEWPLOT_RAYFAN   = 1002
integer, parameter :: ID_PLOTTYPE_GENERIC = 1000
integer, parameter :: ID_PLOTTYPE_SPOT     = 1003
integer, parameter :: ID_PLOTTYPE_RMSFIELD     = 1011
integer, parameter :: ID_PLOTTYPE_SPOT_VS_FIELD     = 1012
integer, parameter :: ID_PLOTTYPE_ZERN_VS_FIELD     = 1013
integer, parameter :: ID_PLOTTYPE_POWSYM     = 1014
integer, parameter :: ID_PLOTTYPE_SEIDEL     = 1015
integer, parameter :: ID_PLOTTYPE_PLT3DTST     = 1016
integer, parameter :: ID_PLOTTYPE_OPD     = 1017
integer, parameter :: ID_PLOTTYPE_FAN     = 1018


! Plot Setting Types
integer, parameter :: UITYPE_SPINBUTTON = 1
integer, parameter :: UITYPE_ENTRY = 2






integer, parameter :: ID_LENSDRAW_PLOT_ORIENTATION = 1401
integer, parameter :: ID_LENSDRAW_YZ_PLOT_ORIENTATION = 1406
integer, parameter :: ID_LENSDRAW_XZ_PLOT_ORIENTATION = 1407
integer, parameter :: ID_LENSDRAW_XY_PLOT_ORIENTATION = 1408
integer, parameter :: ID_LENSDRAW_ORTHO_PLOT_ORIENTATION = 1405

integer, parameter :: ID_LENSDRAW_FIELD_SYMMETRY = 1402
integer, parameter :: ID_LENSDRAW_PLOT_WHOLE_FIELD    = 1409
integer, parameter :: ID_LENSDRAW_PLOT_HALF_FIELD     = 1410
integer, parameter :: ID_LENSDRAW_SCALE = 1403
integer, parameter :: ID_LENSDRAW_AUTOSCALE           = 1411
integer, parameter :: ID_LENSDRAW_MANUALSCALE         = 1412

integer, parameter :: ID_LENSDRAW_AZIMUTH         = 1413
integer, parameter :: ID_LENSDRAW_ELEVATION         = 1414
integer, parameter :: ID_LENSDRAW_AUTOSCALE_VALUE         = 1418
integer, parameter :: ID_LENSDRAW_NUM_FIELD_RAYS    = 1419

integer, parameter :: ID_LENS_FIRSTSURFACE         = 1415
integer, parameter :: ID_LENS_LASTSURFACE         = 1416



integer, parameter :: ID_RAYFAN_FANTYPE  =   1505
integer, parameter :: ID_RAYFAN_WFETYPE  =   1504


integer, parameter :: ID_RAYFAN_Y_FAN  =   1506
integer, parameter :: ID_RAYFAN_X_FAN  =   1507
integer, parameter :: ID_RAYFAN_P_FAN  =   1508
integer, parameter :: ID_RAYFAN_N_FAN  =   1509

integer, parameter :: ID_RAYFAN_TRANSVERSE_RAY  =   1510
integer, parameter :: ID_RAYFAN_TRANSVERSE_OPD  =   1511
integer, parameter :: ID_RAYFAN_CHROMATIC     =   1512
integer, parameter :: ID_RAYFAN_LONGITUDINAL  =   1513

integer, parameter :: ID_RAYFAN_NUMRAYS = 1514
integer, parameter :: ID_RAYFAN_WAVELENGTH = 1515

integer, parameter :: ID_RAYFAN_MAX_PUPIL = 1516
integer, parameter :: ID_RAYFAN_MIN_PUPIL = 1517
integer, parameter :: ID_RAYFAN_IMGSURF = 1518
integer, parameter :: ID_RAYFAN_FOB = 1519

integer, parameter :: ID_PLOTTYPE_AST = 1525
integer, parameter :: ID_AST_FIELDXY = 1531
integer, parameter :: ID_AST_FIELD_X = 1532
integer, parameter :: ID_AST_FIELD_Y = 1533

integer, parameter :: ID_SPOT_TRACE_ALGO = 1602
integer, parameter :: ID_SPOT_RECT = 1
integer, parameter :: ID_SPOT_RING = 2
integer, parameter :: ID_SPOT_RAND = 3
integer, parameter :: ID_SPOT_RECT_GRID = 1611
integer, parameter :: ID_SPOT_RAND_NUMRAYS = 1612
integer, parameter :: ID_SPOT_RING_NUMRINGS = 1613
integer, parameter :: ID_SPOT_FIELD = 1614
integer, parameter :: ID_SPOT_WAVELENGTH = 1615





integer :: active_plot = -1

! C               0 = WHITE
! C               1 = LIGHT YELLOW
! C               2 = LIGHT MAGENTA
! C               3 = LIGHT RED
! C               4 = LIGHT CYAN
! C               5 = LIGHT GREEN
! C               6 = LIGHT BLUE
! C               7 = DARK GREY
! C               8 = LIGHT GREY
! C               9 = DARK YELLOW
! C              10 = DARK MAGENTA
! C              11 = DARK RED
! C              12 = DARK CYAN
! C              13 = DARK GREEN
! C              14 = DARK BLUE
! C              15 = BLACK
integer, parameter :: ID_COLOR_WHITE = 0
integer, parameter :: ID_COLOR_YELLOW = 1
integer, parameter :: ID_COLOR_MAGENTA = 2
integer, parameter :: ID_COLOR_RED = 3
integer, parameter :: ID_COLOR_CYAN = 4
integer, parameter :: ID_COLOR_GREEN = 5
integer, parameter :: ID_COLOR_BLUE = 6
integer, parameter :: ID_COLOR_GREY = 7
integer, parameter :: ID_COLOR_BLACK = 15



! To redirect output console for some uis
integer, parameter :: ID_TERMINAL_DEFAULT = 1
integer, parameter :: ID_TERMINAL_GLASS = 2
integer, parameter :: ID_TERMINAL_MACRO = 3
integer, parameter :: ID_TERMINAL_LENSLIB  = 4
integer, parameter :: ID_TERMINAL_KDPDUMP  = 5








contains
subroutine ui_settings_replot(self)
  class(ui_settings) :: self

  PRINT *, "This is a dummy proc which should not be called AFAIK"

end subroutine

end module zoa_ui
