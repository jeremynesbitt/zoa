module zoa_ui
    implicit none

integer, parameter :: ID_NEWPLOT_LENSDRAW = 1001
integer, parameter :: ID_NEWPLOT_RAYFAN   = 1002
integer, parameter :: ID_PLOTTYPE_GENERIC = 1000

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

integer, parameter :: ID_PLOTTYPE_AST = 1525
integer, parameter :: ID_AST_FIELDXY = 1531
integer, parameter :: ID_AST_FIELD_X = 1532
integer, parameter :: ID_AST_FIELD_Y = 1533



integer :: active_plot = -1


end module zoa_ui
