! UI to allow user to edit surface data
! Rev 2
module lens_editor

  use handlers ! includes gtk and g
!  use gth_hl
  use hl_gtk_zoa
  use gtk_hl_tree

  use kdp_data_types

  implicit none

  interface
  ! append_lens_model(GListStore *store, int surfaceNo, 
  ! bool refSurf,
  ! const char *surfaceName,
  ! const char *surfaceType,
  ! double radius,
  ! int radiusMod,
  ! double thickness,
  ! int thickMod,
  ! const char *glass,
  ! double aperture,
  ! double index)
  function append_lens_model(store, surfaceNo, refSurf, surfaceName, surfaceType, &
    & radius, radiusMod, thickness, thickMod, glass, aperture, index, extraParams) bind(c)
    import c_ptr, c_char, c_int, c_double
    implicit none
    type(c_ptr), value    :: store
    integer(c_int), value :: surfaceNo, refSurf, radiusMod, thickMod
    character(kind=c_char), dimension(*) :: surfaceName, surfaceType, glass
    type(c_ptr)    :: append_lens_model
    real(c_double), value :: radius, thickness, aperture, index
    real(c_double), dimension(16) :: extraParams
  end function
  function lens_item_get_surface_name(item) bind(c)
    import :: c_ptr
    type(c_ptr), value :: item
    type(c_ptr) :: lens_item_get_surface_name
 end function  
 function lens_item_get_surface_type(item) bind(c)
  import :: c_ptr
  type(c_ptr), value :: item
  type(c_ptr) :: lens_item_get_surface_type
end function   
 function lens_item_get_surface_number(item) bind(c)
  import :: c_ptr, c_int
  type(c_ptr), value :: item
  integer(c_int) :: lens_item_get_surface_number
end function  
function lens_item_get_ref_surf(item) bind(c)
  import :: c_ptr, c_int
  type(c_ptr), value :: item
  integer(c_int) :: lens_item_get_ref_surf
end function  
function lens_item_get_surface_radius(item) bind(c)
  import :: c_ptr, c_double
  type(c_ptr), value :: item
  real(c_double) :: lens_item_get_surface_radius
end function  
function lens_item_get_radius_mod(item) bind(c)
  import :: c_ptr, c_int
  type(c_ptr), value :: item
  integer(c_int) :: lens_item_get_radius_mod
end function  
function lens_item_get_surface_thickness(item) bind(c)
  import :: c_ptr, c_double
  type(c_ptr), value :: item
  real(c_double) :: lens_item_get_surface_thickness
end function  
function lens_item_get_thickness_mod(item) bind(c)
  import :: c_ptr, c_int
  type(c_ptr), value :: item
  integer(c_int) :: lens_item_get_thickness_mod
end function  
function lens_item_get_glass(item) bind(c)
  import :: c_ptr
  type(c_ptr), value :: item
  type(c_ptr) :: lens_item_get_glass 
end function
function lens_item_get_aperture(item) bind(c)
  import :: c_ptr, c_double
  type(c_ptr), value :: item
  real(c_double) :: lens_item_get_aperture
end function  
function lens_item_get_extra_param(item, index) bind(c)
  import :: c_ptr, c_int, c_double
  type(c_ptr), value :: item
  integer(c_int), value :: index
  real(c_double) :: lens_item_get_extra_param

end function
  end interface

  type(c_ptr) :: ihscrollcontain,ihlist, &
       &  qbut, dbut, lbl, ibut, ihAsph, ihSolv, ihScrollAsph, ihScrollSolv

  integer(kind=c_int) :: numEditorRows
  type(c_ptr) :: sbScale, sbOffset, dropDown, lblScale, lblOffset

  ! Pickup Global.  Since the ui for this is modal pseudo safe to just store
  ! current pickup to be modded in this type.  Same idea with solve
  type(pickup) :: pData
  type(ksolve)  :: sData

  type(c_ptr) :: win_modal_solve
  type(c_ptr) :: refRadio

  logical :: mod_update
  type(c_ptr) :: cv, curr_btn
  
  integer, parameter :: extra_param_start = 9 

  integer, parameter :: ID_EDIT_ASPH_NONTORIC = 1001
  integer, parameter :: ID_EDIT_ASPH_TORIC_Y = 1002
  integer, parameter :: ID_EDIT_ASPH_TORIC_X = 1003

  integer, parameter :: ID_COMBO_SOLVE = 2000
  integer, parameter :: ID_EDIT_SOLVE = 3
  integer, parameter :: ID_EDIT_SOLVE_NONE = 2001
  integer, parameter :: ID_EDIT_SOLVE_CURV = 2002
  integer, parameter :: ID_EDIT_SOLVE_CENT_CURV = 2003
  integer, parameter :: ID_EDIT_SOLVE_THICK = 2004
  integer, parameter :: ID_EDIT_SOLVE_CA = 2005

  integer, parameter :: ID_MOD_NONE   = 3003
  integer, parameter :: ID_MOD_PICKUP = 3004
  integer, parameter :: ID_MOD_SOLVE  = 3005
  integer, parameter :: ID_MOD_VAR    = 3006

  integer, parameter :: ID_SURF_SPHERE = 1


  integer(kind=c_int), parameter :: ID_COL_SURFTYPE = 4
  integer(kind=c_int), parameter :: ID_COL_RADIUS = 5
  integer(kind=c_int), parameter :: ID_COL_THICKNESS = 6
  integer(kind=c_int), parameter :: ID_COL_GLASS = 7
  integer(kind=c_int), parameter :: ID_COL_CLAP = 10
  integer(kind=c_int), parameter :: ID_COL_INDEX = 11




contains


  subroutine lens_editor_destroy(widget, gdata) bind(c)

    type(c_ptr), value :: widget, gdata
    type(c_ptr) :: isurface
    print *, "Exit called"


    !call cairo_destroy(rf_cairo_drawing_area)
    !call gtk_widget_unparent(gdata)
    !call g_object_unref(rf_cairo_drawing_area)
    call gtk_window_destroy(gdata)

    lens_editor_window = c_null_ptr

  end subroutine lens_editor_destroy


  subroutine lens_editor_new(parent_window)

    type(c_ptr) :: parent_window
    !type(c_ptr), value :: lens_editor_window

    type(c_ptr) :: content, junk, gfilter
    integer(kind=c_int) :: icreate, idir, action, lval
    integer(kind=c_int) :: i, idx0, idx1, pageIdx
    !integer(c_int)  :: width, height

    type(c_ptr)  :: table, expander, box1, nbk, basicLabel, boxAperture, boxAsphere
    type(c_ptr)  :: boxSolve, SolveLabel
    type(c_ptr)  :: lblAperture, AsphLabel

    PRINT *, "ABOUT TO FIRE UP LENS EDITOR WINDOW!"

    ! Create a modal dialogue
    lens_editor_window = gtk_window_new()
    call gtk_window_set_title(lens_editor_window, "Lens Editor Window"//c_null_char)

    width = 700
    height = 400
    call gtk_window_set_default_size(lens_editor_window, width, height)

       call gtk_window_set_transient_for(lens_editor_window, parent_window)
       call gtk_window_set_destroy_with_parent(lens_editor_window, TRUE)

    boxSolve = lens_editor_create_table()

    call gtk_window_set_child(lens_editor_window, boxSolve)


    call g_signal_connect(lens_editor_window, "destroy"//c_null_char, c_funloc(lens_editor_destroy), lens_editor_window)


    call gtk_window_set_mnemonics_visible (lens_editor_window, TRUE)
    call gtk_widget_show(lens_editor_window)

end subroutine lens_editor_new

subroutine lens_editor_replot

  character PART1*5, PART2*5, AJ*3, A6*3, AW1*23, AW2*23
  character(len=100) :: ftext

!

end subroutine lens_editor_replot

 subroutine ins_row(but, gdata) bind(c)
  use type_utils, only: int2str
    type(c_ptr), value, intent(in) :: but, gdata
    integer(kind=c_int) :: currRow

    ! new code
    currRow = getCurrentLensEditorRow()
    call PROCESKDP('U L ; CHG '//trim(int2str(currRow))//' ; INSK ; EOS')
    call rebuildLensEditorTable()
    call zoatabMgr%rePlotIfNeeded()
    return
    
  end subroutine

 subroutine del_row(but, gdata) bind(c)
  use type_utils, only: int2str
  use mod_lens_data_manager
  type(c_ptr), value, intent(in) :: but, gdata
  integer(kind=c_int) :: currRow

  ! new code
  currRow = getCurrentLensEditorRow()
  IF(currRow == 0) THEN
    call updateTerminalLog('OBJECT SURFACE MAY NOT BE DELETED', "black")
    return
  END IF
  IF(currRow == ldm%getLastSurf()) THEN
    call updateTerminalLog('IMAGE SURFACE MAY NOT BE DELETED', "black")
    return
  END IF
  
  call PROCESKDP('U L ; CHG '//trim(int2str(currRow))//' ; DELK ; EOS')
  call rebuildLensEditorTable()
  call zoatabMgr%rePlotIfNeeded()

  end subroutine del_row

  function ITOC(intVal) result(output)
    integer :: intVal
    character(len=100) :: output
    character(len=80) :: B
    !PRINT *, "val is ", intVal
    write(output, *) intVal
 
  end function

  subroutine refreshLensEditorUI()

    implicit none

    call refreshLensDataStruct()
    call zoatabMgr%rePlotIfNeeded()

  end subroutine

  subroutine solv_edited(renderer, path, text, gdata) bind(c)

    type(c_ptr), value :: renderer, path, text, gdata
      character(len=200) :: ftext
      integer :: row, col
      integer(kind=c_int) :: ID_SETTING
  
      PRINT *, "CALLING SOLVE EDITED PROC!"
      call convert_c_string(text, ftext)
      call getRowAndColFromCallback(renderer, path, row, col)

      PRINT *,"Row is ", row
      PRINT *, "Col is ", col

      if (col == 1) then
        PRINT *, "Set by text"
        call hl_gtk_combo_set_by_text(ihSolv, row, col, trim(ftext), ID_SETTING)
        PRINT *, "ID_SETTING is ", ID_SETTING
      end if      

      ! How to update column names
      ! Store array of editable column names and current names
      ! Add callback to row being selected
      ! When row is selected, update column names based on value of master row
      

  
  
  end subroutine



function genPickupArr(ID_PICKUP_TYPE) result(pickupArr)
  use mod_lens_data_manager
  integer, dimension(curr_lens_data%num_surfaces) :: pickupArr
  integer :: ID_PICKUP_TYPE, surfType
  integer :: i
  !PRINT *, "About to set pickupArr"
  !if (ID_PICKUP_TYPE == ID_PICKUP_RAD) PRINT *, "Size of array is ", size(pickupArr)

  select case (ID_PICKUP_TYPE)
  case(ID_PICKUP_RAD)
    surfType = VAR_CURV
  case(ID_PICKUP_THIC)
    surfType = VAR_THI
  end select

  do i=1,size(pickupArr)
    !PRINT *, "i is ", i
    pickupArr(i) = ID_MOD_NONE
    ! Look for Pickups First
    if (curr_lens_data%pickups(1,i,ID_PICKUP_TYPE) == ID_PICKUP_RAD) then 
        pickupArr(i) = ID_MOD_PICKUP 
        
    end if
    if (curr_lens_data%pickups(1,i,ID_PICKUP_TYPE) == ID_PICKUP_THIC) then 
      pickupArr(i) = ID_MOD_PICKUP 
      
    end if
    ! If no pickups found, look for a solve
    select case (ID_PICKUP_TYPE)
    case(ID_PICKUP_RAD)
       !PRINT *, "Solve test is ", curr_lens_data%solves(8,:)
       !PRINT *, "i is ", i
       !PRINT *, "Solve RAD Tst is ", curr_lens_data%solves(8,i)
       if (curr_lens_data%solves(8,i).NE.0) then 
        !PRINT *, "Setting RAD SOlve pickup for row ", i
        pickupArr(i) = ID_MOD_SOLVE
        !PRINT *, " ", size(pickupArr)
        
       end if
    case(ID_PICKUP_THIC)
      !if (i == 1 ) then
      !PRINT *, "solves(6,i) is", curr_lens_data%solves(:,i)
      !end if
      if (curr_lens_data%solves(6,i).NE.0) then 
        pickupArr(i) = ID_MOD_SOLVE
        
      end if
    end select


    ! If no pickup or solve, look for a var

    if (ldm%isVarOnSurf(i-1,surfType)) then
      pickupArr(i) = ID_MOD_VAR 
    end if


  end do

end function

function genSurfaceTypes() result(typeStr)

  character(len=10), dimension(curr_lens_data%num_surfaces) :: typeStr
  integer :: i

  do i=1,curr_lens_data%num_surfaces
    typeStr(i) = "Sphere"
  end do



end function

! Eventually this will move to surf types once implemented, but for now put it here while building out new UI
function getExtraParams(surf, surfType) result(extraParams)
  use mod_lens_data_manager
  use DATLEN, only: ALENS
  integer, intent(in) :: surf
  character(len=*), intent(in) :: surfType
  real(kind=real64), dimension(16) :: extraParams

  select case(surfType)
  case('Sphere')
    extraParams = 0.0d0
  case('Asphere')
    extraParams(1) = ALENS(2,surf)
    extraParams(2:5) = ALENS(4:7,surf)
    extraParams(6:10) = ALENS(81:85,surf)
    extraParams(11:16) = 0.0d0

  end select

end function

! This func interfaces with the c struct that stores the data
! At some point I may migrate this to python but for now the
! main cost of this is a bunch of interfaces for each get which
! I can live with
function buildLensEditTable() result(store)
  use mod_lens_data_manager


  integer, allocatable, dimension(:) :: surfIdx
  integer, allocatable, dimension(:) :: isRefSurface, radPickups, thiPickups
  real(kind=real64), dimension(curr_lens_data%num_surfaces) :: clearApertures
  integer :: i

  integer, parameter :: numSurfTypes = 1
  character(kind=c_char, len=20),dimension(numSurfTypes) :: surfTypeNames
  character(kind=c_char, len=20), dimension(curr_lens_data%num_surfaces) :: surfaceLabels
  integer(c_int), dimension(numSurfTypes) :: surfTypeIDs
  real(kind=real64), dimension(16) :: extraParams
  type(c_ptr) :: store

 
    allocate(surfIdx(curr_lens_data%num_surfaces))
    surfIdx =  (/ (i,i=0,curr_lens_data%num_surfaces-1)/)

    allocate(isRefSurface(curr_lens_data%num_surfaces))
    isRefSurface = 0*isRefSurface
    isRefSurface(curr_lens_data%ref_stop) = 1

    clearApertures = curr_lens_data%clearAps(1:curr_lens_data%num_surfaces)%yRad   
    radPickups = genPickupArr(ID_PICKUP_RAD)
    thiPickups = genPickupArr(ID_PICKUP_THIC)

    surfaceLabels = getSurfaceNames()

    store = g_list_store_new(G_TYPE_OBJECT)
    do i=1,curr_lens_data%num_surfaces
      extraParams = getExtraParams(i-1,ldm%getSurfTypeName(i-1))
    store = append_lens_model(store, surfIdx(i), isRefSurface(i), trim(surfaceLabels(i))//c_null_char, trim(ldm%getSurfTypeName(i-1))//c_null_char, &
    & real(curr_lens_data%radii(i),8), radPickups(i), real(curr_lens_data%thicknesses(i),8), thiPickups(i), &
    & trim(curr_lens_data%glassnames(i))//c_null_char, clearApertures(i), real(curr_lens_data%surf_index(i),8), extraParams)
    end do


  end function

  subroutine converttoscientificnotationstring(fVal, strVal)
     real :: fVal
     character(len=23), intent(inout) :: strVal

     WRITE(strVal, "(E10.4)") fVal


  end subroutine


  function getSolvesAsCStringArray(solve_type, row) result (c_ptr_array)
    use type_utils, only : int2str
    
    implicit none
    integer :: solve_type, row
    integer :: i
    
    integer, dimension(curr_lens_data%num_surfaces) :: surfIdx
    character(kind=c_char), dimension(:), allocatable :: strTmp
    character(kind=c_char), pointer, dimension(:) :: ptrTmp
    integer, parameter :: numThickSolves = size(thick_solves)
    integer, parameter :: numCurvSolves = size(curv_solves)

    character(len=30), dimension(numThickSolves) :: thicSolves
    character(len=30), dimension(numCurvSolves) :: curvSolves

    !type(c_ptr), dimension(numThickSolves+1) :: c_ptr_array
    type(c_ptr), dimension(:), allocatable :: c_ptr_array

    PRINT *, "row is ", row




    !thicSolves(1) = "None"
    !thicSolves(2) = "Paraxial Axial Height (PY)"
    !thicSolves(3) = "X Paraxial Axial Height (PX)"
    !thicSolves(4) = "Paraxial Chief Ray Height (PCY)"
    !thicSolves(5) = "X Paraxial Chief Ray Height (PCX)"
    !thicSolves(6) = "Clear Aperture Solve (CAY)"
    !thicSolves(7) = "X Clear Aperture Solve (CAX)"
    
    ! TODO:  Implement RAD Solves once this is working

    select case (solve_type)

    case (ID_PICKUP_THIC)

    allocate(c_ptr_array(numThickSolves+1))    
    do i = 1, numThickSolves
      thicSolves(i) = trim(thick_solves(i)%uiText)
      call f_c_string(trim(thicSolves(i)), strTmp)
      allocate(ptrTmp(size(strTmp)))
      ! A Fortran pointer toward the Fortran string:
      ptrTmp(:) = strTmp(:)
      ! Store the C address in the array:
      c_ptr_array(i) = c_loc(ptrTmp(1))
      nullify(ptrTmp)
    end do
    ! The array must be null terminated:
    c_ptr_array(numThickSolves+1) = c_null_ptr

  case (ID_PICKUP_RAD)
    allocate(c_ptr_array(numCurvSolves+1))  
    do i = 1, numCurvSolves
      curvSolves(i) = trim(curv_solves(i)%uiText)
      call f_c_string(trim(curvSolves(i)), strTmp)
      allocate(ptrTmp(size(strTmp)))
      ! A Fortran pointer toward the Fortran string:
      ptrTmp(:) = strTmp(:)
      ! Store the C address in the array:
      c_ptr_array(i) = c_loc(ptrTmp(1))
      nullify(ptrTmp)
    end do
    ! The array must be null terminated:
    c_ptr_array(numCurvSolves+1) = c_null_ptr    

    end select

  end function

  function getSurfaceNames() result(surfName_array)
    include "DATLEN.INC"
    character(len=40), dimension(curr_lens_data%num_surfaces) :: surfName_array
    integer :: i
    do i=1,curr_lens_data%num_surfaces-1
      surfName_array(i) = LBL(i-1)(1:40)

    end do


  end function

  function getSurfaceTypesAsCStringArray() result(c_ptr_array)
    integer :: i
    type(c_ptr), dimension(3) :: c_ptr_array
    character(kind=c_char), dimension(:), allocatable :: strTmp
    character(kind=c_char), pointer, dimension(:) :: ptrTmp
    character(len=80), dimension(2) :: surfList 

    surfList(1) = "Sphere"
    surfList(2) = "Asphere"

    
    do i = 1, size(surfList)
      call f_c_string(surfList(i), strTmp)
      allocate(ptrTmp(size(strTmp)))
      ! A Fortran pointer toward the Fortran string:
      ptrTmp(:) = strTmp(:)
      ! Store the C address in the array:
      c_ptr_array(i) = c_loc(ptrTmp(1))
      nullify(ptrTmp)
    end do
    ! The array must be null terminated:
    c_ptr_array(size(surfList)+1) = c_null_ptr

  end function


  function getSurfacesAsCStringArray() result (c_ptr_array)
    use type_utils, only : int2str

    implicit none

    integer :: i
    type(c_ptr), dimension(curr_lens_data%num_surfaces+1) :: c_ptr_array
    integer, dimension(curr_lens_data%num_surfaces) :: surfIdx
    character(kind=c_char), dimension(:), allocatable :: strTmp
    character(kind=c_char), pointer, dimension(:) :: ptrTmp


    surfIdx =  (/ (i,i=0,curr_lens_data%num_surfaces-1)/)   
    
    
    do i = 1, size(surfIdx)
      call f_c_string(trim(int2str(surfIdx(i))), strTmp)
      allocate(ptrTmp(size(strTmp)))
      ! A Fortran pointer toward the Fortran string:
      ptrTmp(:) = strTmp(:)
      ! Store the C address in the array:
      c_ptr_array(i) = c_loc(ptrTmp(1))
      nullify(ptrTmp)
    end do
    ! The array must be null terminated:
    c_ptr_array(curr_lens_data%num_surfaces+1) = c_null_ptr

  end function


  subroutine destroy_solve(widget, btn)
    type(c_ptr), value :: widget, btn
    type(c_ptr) :: cStr
    character(len=20) ::  currIconName 
    integer(c_int) :: pageNum

    ! I had wanted to update the button icon  name here, but nothing I tried would work.
    ! If I passed btn as an object, it would crash
    ! If I added btn as an object to the widget, it would crash
    ! If I set a global variable curr_btn to this button, it would crash
    ! So I gave up and will rely on refreshing lens structure to 
    if (mod_update) then
      !call gtk_menu_button_set_icon_name(btn, 'letter-s'//c_null_char)
      call rebuildLensEditorTable()
    end if

    end subroutine

 
  ! I really don't need this if I can't get it to work I want.  See note in destroy_solve
  subroutine destroy_pickup(widget, btn) bind(c)
    type(c_ptr), value :: widget, btn
    integer(c_int) :: pageNum
    if (mod_update) then
      call rebuildLensEditorTable()
    end if

    end subroutine
  ! Goals
  ! Allow user to set pickup vals
  ! Check integrity of entry
  ! when closed, update kdp
  subroutine ui_pickup(row, pickup_type, btn)
      use mod_lens_data_manager
      use type_utils, only:  int2str

      integer :: row, pickup_type

     
      type(c_ptr), value, optional :: btn
      type(c_ptr) :: win,pUpdate, pCancel, boxWin, cBut, uBut
      type(c_ptr) :: table, lblSurf
      real(kind=c_double) :: pickupScale
      integer :: surfType

      ! Translation from UI to ldm.  Should merge these
      select case (pickup_type)
      case(ID_PICKUP_RAD)
        surfType = VAR_CURV
      case(ID_PICKUP_THIC)
        surfType = VAR_THI
      end select

      pData%ID_type = pickup_type
      call pData%setPickupText()
      pData%surf = row
      pData%surf_ref = INT(curr_lens_data%pickups(2,row+1,pickup_type))
      pData%scale = curr_lens_data%pickups(3,row+1,pickup_type)
      pData%offset = curr_lens_data%pickups(4,row+1,pickup_type)

      ! Create the window:
      win = gtk_window_new()

      call gtk_window_set_title(win, "Set Pickup"//c_null_char)

      ! I added this because I was using it in the pickup ui to find the surface we 
      ! are modifying, but I moved this info to the pickup derived type
      ! So this can be removed but I left it here for now
      call gtk_widget_set_name(win, trim(int2str(row))//c_null_char)

      call gtk_window_set_default_size(win, 300_c_int, 300_c_int)


      boxWin = hl_gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0_c_int)
      call gtk_window_set_child(win, boxWin)

      table = gtk_grid_new()
      call gtk_grid_set_column_homogeneous(table, TRUE)
      call gtk_grid_set_row_homogeneous(table, TRUE)      

      dropDown = gtk_drop_down_new_from_strings(getSurfacesAsCStringArray())
      call gtk_drop_down_set_selected(dropDown, INT(curr_lens_data%pickups(2,row+1,pickup_type)))

      lblSurf = gtk_label_new("Surface"//c_null_char)
      !gtk_grid_attach (GtkGrid *grid, GtkWidget *child, int column, int row, int width, int height);
      call gtk_grid_attach(table, lblSurf, 0_c_int, 0_c_int, 1_c_int, 1_c_int)
      call gtk_grid_attach(table, dropDown, 1_c_int, 0_c_int, 1_c_int, 1_c_int)

      lblScale = gtk_label_new("Scale"//c_null_char)
      lblOffset = gtk_label_new("Offset"//c_null_char)

      call gtk_grid_attach(table, lblScale, 0_c_int, 1_c_int, 1_c_int, 1_c_int)
      call gtk_grid_attach(table, lblOffset, 0_c_int, 2_c_int, 1_c_int, 1_c_int)

      if (ldm%isPikupOnSurf(row, surfType)) then
        pickupScale = curr_lens_data%pickups(3,row+1,pickup_type)*1d0
      else
        pickupScale = 1.0d0 
      end if
      sbScale = gtk_spin_button_new (gtk_adjustment_new( &
      & value=pickupScale, &
      & lower=-1000*1d0, &
      & upper=1000*1d0, &
      & step_increment=1d0, &
      & page_increment=1d0, &
      & page_size=0d0),climb_rate=2d0, &
      & digits=3_c_int)

      sbOffset = gtk_spin_button_new (gtk_adjustment_new( &
      & value=curr_lens_data%pickups(4,row+1,pickup_type)*1d0, &
      & lower=-1000*1d0, &
      & upper=1000*1d0, &
      & step_increment=1d0, &
      & page_increment=1d0, &
      & page_size=0d0),climb_rate=2d0, &
      & digits=3_c_int)

      call gtk_grid_attach(table, sbScale, 1_c_int, 1_c_int, 1_c_int, 1_c_int)
      call gtk_grid_attach(table, sbOffset, 1_c_int, 2_c_int, 1_c_int, 1_c_int)

    ! Now make a single column list with multiple selections enabled


    ! It is the scrollcontainer that is placed into the box.
    call hl_gtk_box_pack(boxWin, table)

    uBut = hl_gtk_button_new("Update"//c_null_char, &
        & clicked=c_funloc(pickupUpdate_click), &
        & data=win)
       
    cBut = hl_gtk_button_new("Cancel"//c_null_char, &
         & clicked=c_funloc(pickupCancel_click), &
         & data=win)         

    if(present(btn)) then 
      call g_signal_connect(win, "destroy"//c_null_char, c_funloc(destroy_pickup), btn)
      
    end if

    call hl_gtk_box_pack(boxWin, uBut)
    call hl_gtk_box_pack(boxWin, cBut)

    call gtk_widget_show(win)
    call gtk_window_set_modal(win, 1_c_int)
    call gtk_window_set_transient_for(win, lens_editor_window)
    

  end subroutine

  subroutine pickupUpdate_click(widget, gdata) bind(c)
    use type_utils, only: int2str
    type(c_ptr), value, intent(in) :: widget, gdata

    PRINT *, "Button clicked!"
    pData%scale = gtk_spin_button_get_value (sbScale)
    pData%offset = gtk_spin_button_get_value (sbOffset)
    pData%surf_ref = gtk_drop_down_get_selected(dropDown)

  
    ! It's time to update the pickup
    CALL PROCESKDP('U L')
    !CALL PROCESKDP("CHG, "//trim(choice))
    CALL PROCESKDP("CHG, "//trim(int2str(pData%surf)))

    call PROCESKDP(trim(pData%genKDPCMD()))

    !CALL PROCESKDP("PIKUP "//trim(pData%pickupTxt)//","//trim(int2str(surfNum))//","//trim(real2str(scale))// &
    !& ","//trim(real2str(offset))//","//"0.0,")

    PRINT *, "Update cmd tst is ", trim(pData%genKDPCMD())

    CALL PROCESKDP('EOS')
    mod_update = .TRUE.
    call refreshLensDataStruct()
    call zoatabMgr%rePlotIfNeeded()

    call gtk_window_destroy(gdata)



  end subroutine

  subroutine pickupCancel_click(widget, parentWin) bind(c)
    type(c_ptr), value, intent(in) :: widget, parentWin

    PRINT *, "Button clicked!"

    ! This is a bad design but not sure what else to do to get rid of the "P" entry without
    ! yet another global
    call refreshLensDataStruct()
    call zoatabMgr%rePlotIfNeeded()    
    call gtk_window_destroy(parentWin)

  end subroutine  

  !TODO:
  !Move thic_solves data to kdp-data-types
  !In kdp-data-types, when updateSolveDataIsCalled
  !call a new method that finds which solve matches the 
  !value found in the curr_lens_data and sets param names
  !etc accordingly

  subroutine ui_solve(row, solve_type, btn)

    use type_utils, only:  int2str
    type(c_ptr), value :: btn
    integer :: row, solve_type, ii
    ! This seems like an insane way of passing an integer but it works
    integer, target :: ID_TGT_THIC = ID_PICKUP_THIC
    integer, target :: ID_TGT_RAD = ID_PICKUP_RAD

    type(c_ptr) :: boxWin, cBut, uBut
    type(c_ptr) :: table, lblSurf

    !pData%ID_type = pickup_type
    !call pData%setPickupText()
    !pData%surf = row
    !pData%surf_ref = INT(curr_lens_data%pickups(2,row+1,pickup_type))
    !pData%scale = curr_lens_data%pickups(3,row+1,pickup_type)
    !pData%offset = curr_lens_data%pickups(4,row+1,pickup_type)

    PRINT *, "Call Update Solve Data from ui_solve"
    call sData%updateSolveData(curr_lens_data, row+1, solve_type)
    PRINT *, "After Call Update Solve Data from ui_solve"



    ! Create the window:
    win_modal_solve= gtk_window_new()
    mod_update = .fALSE.

    call gtk_window_set_title(win_modal_solve, "Set Solve"//c_null_char)

    ! I added this because I was using it in the pickup ui to find the surface we 
    ! are modifying, but I moved this info to the pickup derived type
    ! So this can be removed but I left it here for now
    call gtk_widget_set_name(win_modal_solve, trim(int2str(row))//c_null_char)

    call gtk_window_set_default_size(win_modal_solve, 300_c_int, 300_c_int)


    boxWin = hl_gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0_c_int)
    call gtk_window_set_child(win_modal_solve, boxWin)

    table = gtk_grid_new()
    call gtk_grid_set_column_homogeneous(table, TRUE)
    call gtk_grid_set_row_homogeneous(table, TRUE)      

    dropDown = gtk_drop_down_new_from_strings(getSolvesAsCStringArray(solve_type, row))

    ! TODO:  Need to set selection based on users current solve.  I asume this is going to require
    select case (solve_type)
    case (ID_PICKUP_THIC)
      do ii=1,size(thick_solves)
        if (thick_solves(ii)%id_solve == sData%id_solve) then
         call gtk_drop_down_set_selected(dropDown, ii-1)
        end if
      end do
      !Insanity part 2
      call g_signal_connect(dropDown, "notify::selected"//c_null_char, c_funloc(solveTypeChanged), c_loc(ID_TGT_THIC))
      uBut = hl_gtk_button_new("Update"//c_null_char, &
      & clicked=c_funloc(solveUpdate_click), &
      & data=c_loc(ID_TGT_THIC))
    case(ID_PICKUP_RAD)
      do ii=1,size(curv_solves)
        if (curv_solves(ii)%id_solve == sData%id_solve) then
         call gtk_drop_down_set_selected(dropDown, ii-1)
        end if
      end do
      !Insanity part 2
      call g_signal_connect(dropDown, "notify::selected"//c_null_char, c_funloc(solveTypeChanged), c_loc(ID_TGT_RAD))
      uBut = hl_gtk_button_new("Update"//c_null_char, &
      & clicked=c_funloc(solveUpdate_click), &
      & data=c_loc(ID_TGT_RAD))

    end select

    

    

    !dropDown = gtk_drop_down_new_from_strings(getSurfacesAsCStringArray())
    

    lblSurf = gtk_label_new("Solve Type"//c_null_char)
    !gtk_grid_attach (GtkGrid *grid, GtkWidget *child, int column, int row, int width, int height);
    call gtk_grid_attach(table, lblSurf, 0_c_int, 0_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, dropDown, 1_c_int, 0_c_int, 1_c_int, 1_c_int)

    lblScale = gtk_label_new(trim(sData%param1Name)//c_null_char)
    lblOffset = gtk_label_new(trim(sData%param2Name)//c_null_char)

    call gtk_grid_attach(table, lblScale, 0_c_int, 1_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, lblOffset, 0_c_int, 2_c_int, 1_c_int, 1_c_int)

    sbScale = gtk_spin_button_new (gtk_adjustment_new( &
    & value=sData%param1*1d0, &
    & lower=-1000*1d0, &
    & upper=1000*1d0, &
    & step_increment=1d0, &
    & page_increment=1d0, &
    & page_size=0d0),climb_rate=2d0, &
    & digits=3_c_int)

    sbOffset = gtk_spin_button_new (gtk_adjustment_new( &
    & value=sData%param2*1d0, &
    & lower=-1000*1d0, &
    & upper=1000*1d0, &
    & step_increment=1d0, &
    & page_increment=1d0, &
    & page_size=0d0),climb_rate=2d0, &
    & digits=3_c_int)

    call gtk_grid_attach(table, sbScale, 1_c_int, 1_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, sbOffset, 1_c_int, 2_c_int, 1_c_int, 1_c_int)

  ! Now make a single column list with multiple selections enabled


  ! It is the scrollcontainer that is placed into the box.
  call hl_gtk_box_pack(boxWin, table)


     
  cBut = hl_gtk_button_new("Cancel"//c_null_char, &
       & clicked=c_funloc(pickupCancel_click), &
       & data=win_modal_solve)         

  call hl_gtk_box_pack(boxWin, uBut)
  call hl_gtk_box_pack(boxWin, cBut)

  call gtk_widget_show(win_modal_solve)
  call gtk_window_set_modal(win_modal_solve, 1_c_int)
  call gtk_window_set_transient_for(win_modal_solve, lens_editor_window)
  
  call g_signal_connect(win_modal_solve, "destroy"//c_null_char, c_funloc(destroy_solve), btn)

end subroutine

! Prototype
function cStrToStr(cStr) result(fStr)
  type(c_ptr), value :: cStr 
  character(len=100) :: fStr
  call convert_c_string((cStr), fStr)

end function

subroutine solveTypeChanged(widget, gdata) bind(c)
  type(c_ptr), value, intent(in) :: widget, gdata
  integer :: selection
  integer(kind=c_int), pointer :: ID_SETTING

  call c_f_pointer(gdata, ID_SETTING)

  select case(ID_SETTING)

  case(ID_PICKUP_THIC)
    selection = gtk_drop_down_get_selected(dropDown)+1 ! Since it starts from 0 in gtk
    call gtk_label_set_label(lblScale, trim(thick_solves(selection)%param1Name)//c_null_char)
    call gtk_label_set_label(lblOffset, trim(thick_solves(selection)%param2Name)//c_null_char)

  case(ID_PICKUP_RAD)

    selection = gtk_drop_down_get_selected(dropDown)+1 ! Since it starts from 0 in gtk
    call gtk_label_set_label(lblScale, trim(curv_solves(selection)%param1Name)//c_null_char)
    call gtk_label_set_label(lblOffset, trim(curv_solves(selection)%param2Name)//c_null_char)

  end select


end subroutine  

subroutine solveUpdate_click(widget, gdata) bind(c)
  use type_utils, only: int2str
  type(c_ptr), value, intent(in) :: widget, gdata
  integer, pointer :: ID_SETTING
  integer :: selection

  call c_f_pointer(gdata, ID_SETTING)
  PRINT *, "Button clicked!"
  sData%param1 = gtk_spin_button_get_value (sbScale)
  sData%param2 = gtk_spin_button_get_value (sbOffset)
  selection = gtk_drop_down_get_selected(dropDown)+1

  select case (ID_SETTING)
  case(ID_PICKUP_THIC)

  sData%id_solve = thick_solves(selection)%id_solve
  call sData%setSolveText(thick_solves)
  case(ID_PICKUP_RAD)
    sData%id_solve = curv_solves(selection)%id_solve
    call sData%setSolveText(curv_solves)
  end select

  ! Update is either a solve or none.  If none, delete solve
  ! on surface
  if (sData%ID_solve == ID_SOLVE_NONE) then
    CALL PROCESKDP('U L ; '// &
    & trim(sData%genKDPCMDToRemoveSolve(sData%surf))//';EOS')
  else   
  ! It's time to update the pickup
    CALL PROCESKDP('U L ; CHG, '//trim(int2str(sData%surf)) &
    & //"; "//trim(sData%genKDPCMDToSetSolve())//";EOS")
  end if
  



  mod_update = .TRUE.
  call gtk_menu_button_set_icon_name(curr_btn, 'letter-s'//c_null_char)
  !call refreshLensDataStruct()
  !call zoatabMgr%rePlotIfNeeded()


  call gtk_window_close(win_modal_solve)
  !call gtk_window_destroy(win_modal_solve)



end subroutine

function getSurfaceIndexFromRowColumnCode(rcCode, colIdx) result(surfIdx)
  use type_utils
  character(len=*) :: rcCode
  integer :: surfIdx
  integer, optional, intent(inout) :: colIdx
  integer :: rL, cL

  rL = index(rcCode, 'R')
  cL = index(rcCode, 'C')
  !print *, "row is ", rcCode(rL+1:cL)
  surfIdx = str2int(rcCode(rL+1:cL-1))
  if (present(colIdx)) then 
    colIdx = str2int(rcCode(cL+1:len(rcCode)))
  end if
  !print *, "surfIdx is ", surfIdx

end function

! Assumption here is that data contains the cmd to change the value
! and the structure is CMD Sk Val will update the surface value
subroutine cell_changed(widget, data) bind(c)
  use type_utils

type(c_ptr), value :: widget, data
type(c_ptr) :: buff2, cStr
character(len=100) :: ftext, rcCode, cmd
integer :: surfIdx

buff2 = gtk_entry_get_buffer(widget)
call c_f_string_copy(gtk_entry_buffer_get_text(buff2), ftext)

print *, "Val is ", trim(ftext)
cStr = gtk_widget_get_name(gtk_widget_get_parent(widget))
call convert_c_string(cStr, rcCode)  
print *, "Val is ", trim(rcCode)

surfIdx = getSurfaceIndexFromRowColumnCode(trim(rcCode))

call convert_c_string(data, cmd)  
print *, "cmd is ", trim(cmd)
call PROCESSILENT(trim(cmd)//" S"//trim(int2str(surfIdx))//" "//trim(ftext))
end subroutine

! For extra param I can't store the command in the widget, so 
! look it up from row column index
subroutine extraParam_changed(widget, data) bind(c)
  use type_utils
  use mod_lens_data_manager

type(c_ptr), value :: widget, data
type(c_ptr) :: buff2, cStr
character(len=100) :: ftext, rcCode, cmd
integer :: surfIdx, colIdx

buff2 = gtk_entry_get_buffer(widget)
call c_f_string_copy(gtk_entry_buffer_get_text(buff2), ftext)

print *, "Val is ", trim(ftext)
cStr = gtk_widget_get_name(gtk_widget_get_parent(widget))
call convert_c_string(cStr, rcCode)  
print *, "Val is ", trim(rcCode)

surfIdx = getSurfaceIndexFromRowColumnCode(trim(rcCode), colIdx)
cmd = ldm%getExtraParamCmd(surfIdx, colIdx-extra_param_start+1)

print *, "cmd is ", cmd
if (len(trim(cmd)) > 0) then 
  call PROCESSILENT(trim(cmd)//" S"//trim(int2str(surfIdx))//" "//trim(ftext))
end if

end subroutine

subroutine removeSolve(act, avalue, btn) bind(c)
  type(c_ptr), value :: act, avalue, btn
  type(c_ptr) :: cStr
  character(len=100) :: rcCode
  type(ksolve)  :: sData
  integer :: surfIdx, colIdx

  cStr = gtk_widget_get_name(gtk_widget_get_parent(btn))
  call convert_c_string(cStr, rcCode)  
  print *, "Val is ", trim(rcCode)
  surfIdx = getSurfaceIndexFromRowColumnCode(trim(rcCode), colIdx)

  sData%surf = surfIdx
  select case (colIdx)
  case(ID_COL_RADIUS)
    sData%solve_type = ID_PICKUP_RAD
  case(ID_COL_THICKNESS)
    sData%solve_type = ID_PICKUP_THIC
  end select
  print *, "Hook working!"
  CALL PROCESKDP('U L ; '// &
  & trim(sData%genKDPCMDToRemoveSolve(sData%surf))//';EOS')
  call rebuildLensEditorTable()

end subroutine

subroutine removePickup(act, avalue, btn) bind(c)
  type(c_ptr), value :: act, avalue, btn
  type(c_ptr) :: cStr
  character(len=100) :: rcCode
  type(pickup)  :: pData
  integer :: surfIdx, colIdx

  cStr = gtk_widget_get_name(gtk_widget_get_parent(btn))
  call convert_c_string(cStr, rcCode)  
  print *, "Val is ", trim(rcCode)
  surfIdx = getSurfaceIndexFromRowColumnCode(trim(rcCode), colIdx)

  pData%surf = surfIdx
  select case (colIdx)
  case(ID_COL_RADIUS)
    pData%pickupTxt = "RD"
  case(ID_COL_THICKNESS)
    pData%pickupTxt = "TH"
  end select
  print *, "Hook working!"
  CALL PROCESKDP('U L ; '// &
  & trim(pData%genKDPCMDToRemovePickup())//';EOS')
  call rebuildLensEditorTable()

end subroutine


subroutine enableSolve(act, avalue, btn) bind(c)
  type(c_ptr), value :: act, avalue, btn
  type(c_ptr) :: cStr
  integer :: surfIdx, colIdx, colCode
  character(len=100) :: rcCode

  !cStr = gtk_widget_get_name(btn)
  cStr = gtk_widget_get_name(gtk_widget_get_parent(btn))
  call convert_c_string(cStr, rcCode)  
  print *, "Val is ", trim(rcCode)
  
  surfIdx = getSurfaceIndexFromRowColumnCode(trim(rcCode), colIdx)
  print *, "surfIdx is ", surfIdx  
  mod_update = .FALSE.  

  select case (colIdx)
  case(ID_COL_RADIUS)
    colCode = ID_PICKUP_RAD
  case(ID_COL_THICKNESS)
    colCode = ID_PICKUP_THIC
  end select


  call ui_solve(surfIdx, colCode, btn)

end subroutine


subroutine enablePickup(act, avalue, btn) bind(c)
  type(c_ptr), value, intent(in) :: act, avalue, btn
  type(c_ptr) :: cStr
  integer :: surfIdx, colIdx, colCode
  character(len=100) :: rcCode

  !cStr = gtk_widget_get_name(btn)
  cStr = gtk_widget_get_name(gtk_widget_get_parent(btn))
  call convert_c_string(cStr, rcCode)  
  print *, "Val is ", trim(rcCode)
  
  surfIdx = getSurfaceIndexFromRowColumnCode(trim(rcCode), colIdx)
  print *, "surfIdx is ", surfIdx  
  mod_update = .FALSE.  

  select case (colIdx)
  case(ID_COL_RADIUS)
    colCode = ID_PICKUP_RAD
  case(ID_COL_THICKNESS)
    colCode = ID_PICKUP_THIC
  end select

  call ui_pickup(surfIdx, colCode, btn)

end subroutine

subroutine enableVar(act, avalue, btn) bind(c)
  use type_utils
  type(c_ptr), value :: act, avalue, btn
  type(c_ptr) :: cStr
  integer :: surfIdx, colIdx, colCode
  character(len=100) :: rcCode
  character(len=4) :: cmd

  !cStr = gtk_widget_get_name(btn)
  cStr = gtk_widget_get_name(gtk_widget_get_parent(btn))
  call convert_c_string(cStr, rcCode)  
  print *, "Val is ", trim(rcCode)
  
  surfIdx = getSurfaceIndexFromRowColumnCode(trim(rcCode), colIdx)
  print *, "surfIdx is ", surfIdx  
  mod_update = .FALSE.  

  select case (colIdx)
  case(ID_COL_RADIUS)
    cmd = "CCY "
  case(ID_COL_THICKNESS)
    cmd = "THC "
  end select

  call PROCESSILENT(cmd//"S"//trim(int2str(surfIdx))//" 0")
  call rebuildLensEditorTable()

end subroutine

function createModMenu(btn, surf, colIdx) result(menuOptions)
  use mod_lens_data_manager
  use type_utils
  type(c_ptr), value :: btn
  type(c_ptr) :: menuOptions
  type(c_ptr) :: actGroup, actP, mPickup, mSolve, actS
  type(c_ptr) :: actRemoveSolve, mRemoveSolve, actRemovePickup, mRemovePickup
  type(c_ptr) :: actVar, mVar, actRemoveVar, mRemoveVar
  integer :: surf, colIdx, colType

  select case (colIdx)
  case(ID_COL_RADIUS)
    colType = VAR_CURV
  case(ID_COL_THICKNESS)
    colType = VAR_THI
  end select

  actGroup = g_simple_action_group_new()
  call gtk_widget_insert_action_group(btn, "mod"//c_null_char,actGroup)

  actP = g_simple_action_new("setPickup"//trim(int2str(surf))//c_null_char, c_null_ptr)
  call g_action_map_add_action(actGroup, actP)
  call g_signal_connect(actP, "activate"//c_null_char, c_funloc(enablePickup), btn)
  mPickup = g_menu_item_new("Set Pickup"//c_null_char, "mod.setPickup"//trim(int2str(surf))//c_null_char)

  actRemovePickup = g_simple_action_new("removePickup"//trim(int2str(surf))//c_null_char, c_null_ptr)
  if (ldm%isPikupOnSurf(surf, colType)) call g_action_map_add_action(actGroup, actRemovePickup)  
  call g_signal_connect(actRemovePickup, "activate"//c_null_char, c_funloc(removePickup), btn)
  mRemovePickup = g_menu_item_new("Remove Pickup"//c_null_char, "mod.removePickup"//trim(int2str(surf))//c_null_char)   

  actS = g_simple_action_new("setSolve"//trim(int2str(surf))//c_null_char, c_null_ptr)
  call g_action_map_add_action(actGroup, actS)  
  call g_signal_connect(actS, "activate"//c_null_char, c_funloc(enableSolve), btn)
  mSolve = g_menu_item_new("Set Solve"//c_null_char, "mod.setSolve"//trim(int2str(surf))//c_null_char)  

  actRemoveSolve = g_simple_action_new("removeSolve"//trim(int2str(surf))//c_null_char, c_null_ptr)
  if (ldm%isSolveOnSurf(surf, colType)) call g_action_map_add_action(actGroup, actRemoveSolve)  
  call g_signal_connect(actRemoveSolve, "activate"//c_null_char, c_funloc(removeSolve), btn)
  mRemoveSolve = g_menu_item_new("Remove Solve"//c_null_char, "mod.removeSolve"//trim(int2str(surf))//c_null_char) 
  
  actVar = g_simple_action_new("setVar"//trim(int2str(surf))//c_null_char, c_null_ptr)
  if (ldm%isVarOnSurf(surf, colType) .eqv. .FALSE.) call g_action_map_add_action(actGroup, actVar)  
  call g_signal_connect(actVar, "activate"//c_null_char, c_funloc(enableVar), btn)
  mVar = g_menu_item_new("Set Variable"//c_null_char, "mod.setVar"//trim(int2str(surf))//c_null_char)  

  actRemoveVar = g_simple_action_new("removeVar"//trim(int2str(surf))//c_null_char, c_null_ptr)
  if (ldm%isVarOnSurf(surf, colType)) call g_action_map_add_action(actGroup, actRemoveVar)  
  call g_signal_connect(actRemoveSolve, "activate"//c_null_char, c_funloc(removeSolve), btn)
  mRemoveVar = g_menu_item_new("Remove Variable"//c_null_char, "mod.removeVar"//trim(int2str(surf))//c_null_char)   

  menuOptions = g_menu_new()
  call g_menu_append_item(menuOptions, mPickup)
  call g_menu_append_item(menuOptions, mRemovePickup)
  call g_menu_append_item(menuOptions, mSolve)
  call g_menu_append_item(menuOptions, mRemoveSolve)
  call g_menu_append_item(menuOptions, mVar)
  call g_menu_append_item(menuOptions, mRemoveVar)  


end function

! TODO:  Needs updating when more surfaces are added
subroutine updateSurfaceType(widget, gdata) bind(c)
  use DATLEN, only: ALENS
  type(c_ptr), value, intent(in) :: widget, gdata
  integer :: selection, surfIdx, oldVal
  character(len=100) :: rcCode
  type(c_ptr) :: cStr 

  cStr = gtk_widget_get_name(widget)
  call convert_c_string(cStr, rcCode)  
  print *, "Val is ", trim(rcCode)
  
  surfIdx = getSurfaceIndexFromRowColumnCode(trim(rcCode))

  selection = gtk_drop_down_get_selected(widget)
  oldVal = ALENS(8,surfIdx)
  select case (selection)
  case(0) ! Sphere
    ALENS(8,surfIdx) = 0 
  case(1) ! Asphere
    ALENS(8,surfIdx) = 1 
  end select    
  
  if (oldVal - ALENS(8,surfIdx) .NE. 0) then 
    call rebuildLensEditorTable()
  end if


end subroutine


subroutine setup_cb(factory,listitem, gdata) bind(c)
  use gtk_hl_entry
  use gtk_hl_container
  
  type(c_ptr), value :: factory
  type(c_ptr), value :: listitem, gdata
  type(c_ptr) :: label, entryCB, menuB, boxS, dropDown
  integer(kind=c_int), pointer :: ID_COL
  character(len=3) :: cmd
  

  label =gtk_label_new(c_null_char)
  call gtk_list_item_set_child(listitem,label)

  call c_f_pointer(gdata, ID_COL)

  select case (ID_COL)

  case(2)
     label = gtk_check_button_new()
     if (.not.c_associated(refRadio)) refRadio = label
     call gtk_check_button_set_group(label, refRadio)
     call gtk_list_item_set_child(listitem,label)
  case(3) ! Surface Label   
    boxS = hl_gtk_box_new(horizontal=TRUE, spacing=0_c_int)
    entryCB = hl_gtk_entry_new(10_c_int, editable=TRUE, activate=c_funloc(cell_changed), data=g_strdup('SLB'))
    call gtk_box_append(boxS, entryCB)
    call gtk_list_item_set_child(listitem, boxS)         
  case(4)
    dropDown = gtk_drop_down_new_from_strings(getSurfaceTypesAsCStringArray())
    call gtk_list_item_set_child(listitem, dropDown)   

   case(5:6) ! Radius or Thickness + modifier
    boxS = hl_gtk_box_new(horizontal=TRUE, spacing=0_c_int)
    menuB = gtk_menu_button_new()
    cmd = 'RDY'
    if(ID_COL==6) cmd = 'THI'
    entryCB = hl_gtk_entry_new(10_c_int, editable=TRUE, activate=c_funloc(cell_changed), data=g_strdup(cmd))
    !call gtk_widget_set_name(entryCB, cmd) 
    call gtk_box_append(boxS, entryCB)
    call gtk_box_append(boxS, menuB)
    call gtk_list_item_set_child(listitem,boxS)
   case(7) ! Glass
    boxS = hl_gtk_box_new(horizontal=TRUE, spacing=0_c_int)
    entryCB = hl_gtk_entry_new(10_c_int, editable=TRUE, activate=c_funloc(cell_changed), data=g_strdup('GLA'))
    call gtk_box_append(boxS, entryCB)
    call gtk_list_item_set_child(listitem, boxS)       
  case(8) ! Aperture
    boxS = hl_gtk_box_new(horizontal=TRUE, spacing=0_c_int)
    entryCB = hl_gtk_entry_new(10_c_int, editable=TRUE, activate=c_funloc(cell_changed), data=g_strdup('CIR'))
    call gtk_box_append(boxS, entryCB)
    call gtk_list_item_set_child(listitem, boxS)      
    
  ! Extra params  
  case(9:18)
    ! Put it in a box to use the same callback for this entry and entries with menu buttons
    boxS = hl_gtk_box_new(horizontal=TRUE, spacing=0_c_int)
    entryCB = hl_gtk_entry_new(10_c_int, editable=TRUE, activate=c_funloc(extraParam_changed), data=c_null_ptr)
    call gtk_box_append(boxS, entryCB)
    call gtk_list_item_set_child(listitem, boxS)   
   case default 
    label =gtk_label_new(c_null_char)
    call gtk_list_item_set_child(listitem,label)
   end select 
end subroutine

subroutine bind_cb(factory,listitem, gdata) bind(c)
  use type_utils
  type(c_ptr), value :: factory
  type(c_ptr), value :: listitem, gdata
  type(c_ptr) :: widget, item, label, buffer, entryCB, menuCB
  type(c_ptr) :: cStr
  integer(kind=c_int), pointer :: ID_COL
  character(len=140) :: colName

  call c_f_pointer(gdata, ID_COL)
  label = gtk_list_item_get_child(listitem)
  item = gtk_list_item_get_item(listitem);

  select case (ID_COL)

  case(1)
    colName = trim(int2str(lens_item_get_surface_number(item)))//c_null_char
    call gtk_label_set_text(label, trim(colName)//c_null_char)
   case(2)
    if (lens_item_get_ref_surf(item) == 1_c_int) then
      call gtk_check_button_set_active(label, 1_c_int)
    end if
    !colName = trim(int2str(lens_item_get_ref_surf(item)))//c_null_char
    !call gtk_label_set_text(label, trim(colName)//c_null_char)
   case(3)
    cStr = lens_item_get_surface_name(item)
    call convert_c_string(cStr, colName)
    entryCB = gtk_widget_get_first_child(label)  
    buffer = gtk_entry_get_buffer(entryCB)    
    call gtk_entry_buffer_set_text(buffer, trim(colName),-1_c_int)
   case(4)
    cStr = lens_item_get_surface_type(item)
    call convert_c_string(cStr, colName)  
    ! Will need to abstract this when more types are added
    if(colName=='Sphere') then
      call gtk_drop_down_set_selected(label, 0_c_int)
    end if
    if(colName=='Asphere') then 
      call gtk_drop_down_set_selected(label, 1_c_int)
    end if
    !call gtk_label_set_text(label, trim(colName)//c_null_char)    
   case(5)
    colName = trim(real2str(lens_item_get_surface_radius(item)))//c_null_char  
    entryCB = gtk_widget_get_first_child(label)  
    buffer = gtk_entry_get_buffer(entryCB)
    !buffer = gtk_entry_get_buffer(label)
    call gtk_entry_buffer_set_text(buffer, trim(colName),-1_c_int)
    menuCB = gtk_widget_get_next_sibling(entryCB)
    colName = trim(int2str(lens_item_get_radius_mod(item)))//c_null_char   
    call gtk_menu_button_set_menu_model(menuCB, createModMenu(menuCB, lens_item_get_surface_number(item), ID_COL)) 
    select case (lens_item_get_radius_mod(item))
    case (ID_MOD_NONE)
      call gtk_menu_button_set_icon_name(menuCB, 'letter-blank'//c_null_char)      
    case (ID_MOD_PICKUP)
      call gtk_menu_button_set_icon_name(menuCB, 'letter-p'//c_null_char)
    case (ID_MOD_SOLVE)
      call gtk_menu_button_set_icon_name(menuCB, 'letter-s'//c_null_char)
    case (ID_MOD_VAR)
      call gtk_menu_button_set_icon_name(menuCB, 'letter-v'//c_null_char)      
    end select   


  case(6)
    colName = trim(real2str(lens_item_get_surface_thickness(item)))//c_null_char  
    entryCB = gtk_widget_get_first_child(label)  
    buffer = gtk_entry_get_buffer(entryCB)
    !buffer = gtk_entry_get_buffer(label)
    call gtk_entry_buffer_set_text(buffer, trim(colName),-1_c_int)
    menuCB = gtk_widget_get_next_sibling(entryCB)
    colName = trim(int2str(lens_item_get_thickness_mod(item)))//c_null_char   
    call gtk_menu_button_set_menu_model(menuCB, createModMenu(menuCB, lens_item_get_surface_number(item), ID_COL)) 
    select case (lens_item_get_thickness_mod(item))
    case (ID_MOD_NONE)
      call gtk_menu_button_set_icon_name(menuCB, 'letter-blank'//c_null_char)      
    case (ID_MOD_PICKUP)
      call gtk_menu_button_set_icon_name(menuCB, 'letter-p'//c_null_char)
    case (ID_MOD_SOLVE)
      call gtk_menu_button_set_icon_name(menuCB, 'letter-s'//c_null_char)  
    case (ID_MOD_VAR)
      call gtk_menu_button_set_icon_name(menuCB, 'letter-v'//c_null_char)            
    end select   
  case(7) ! Glass
    cStr = lens_item_get_glass(item)
    call convert_c_string(cStr, colName)
    entryCB = gtk_widget_get_first_child(label)  
    buffer = gtk_entry_get_buffer(entryCB)    
    call gtk_entry_buffer_set_text(buffer, trim(colName),-1_c_int)
  case(8) ! Clear Aperture
    colName = trim(real2str(lens_item_get_aperture(item)))//c_null_char  
    entryCB = gtk_widget_get_first_child(label)  
    buffer = gtk_entry_get_buffer(entryCB)    
    call gtk_entry_buffer_set_text(buffer, trim(colName),-1_c_int)
  case(9:18)
    entryCB = gtk_widget_get_first_child(label)  
    buffer = gtk_entry_get_buffer(entryCB)
    colName = trim(real2str(lens_item_get_extra_param(item, ID_COL-9),sci=.TRUE.))//c_null_char
    call gtk_entry_buffer_set_text(buffer, trim(colName),-1_c_int)     
   end select

    ! Encode row and column for later use  
     call gtk_widget_set_name(label,"R"//trim(int2str(lens_item_get_surface_number(item)))//"C"//trim(int2str(ID_COL))//c_null_char)

     if (ID_COL == 4) then 
     call g_signal_connect(label, "notify::selected"//c_null_char, c_funloc(updateSurfaceType), c_null_ptr)
     end if

     !print *, "Testing... ", lens_item_get_extra_param(item, 0_c_int)
     !print *, "Testing... ", lens_item_get_aperture(item)


end subroutine

subroutine clearColumnView()
  type(c_ptr) :: listModel, currCol
  integer(c_int) :: ii, numItems

  listmodel = gtk_column_view_get_columns(cv)
  numItems = g_list_model_get_n_items(listmodel) -1
  do ii=numItems,0,-1
    currCol = g_list_model_get_object(listmodel,ii)
    call gtk_column_view_remove_column(cv,currCol)
    call g_object_unref(currCol)

  end do

end subroutine


function getCurrentLensEditorRow() result(currPos)
  integer(c_int) :: currPos, numRows, isSelected, ii
  type(c_ptr) :: selection


  selection = gtk_column_view_get_model(cv)
  ! Seems there is no simple way to get selected row so brute force it
  numRows = g_list_model_get_n_items(selection)
  currPos=-1
  do ii=0,numRows-1
    isSelected = gtk_selection_model_is_selected(selection, ii)
    if (isSelected==1) then
         currPos = ii
         exit
    end if
  end do

end function

subroutine rebuildLensEditorTable()

  type(c_ptr) :: store, selection, model, column, vadj, hadj, swin
  integer(c_int) :: oldPos 
  real(c_double) :: vPos, hPos
  logical :: boolResult


  print *, "Rebuild lens table starting" 
 
  ! Get current position
  oldPos = getCurrentLensEditorRow()
  print *, "Old Position is ", oldPos

  ! Get location of horizontal and vertical scrollbars so we can recreate.  This seems crazy
  ! but I am not sure how else to make sure the lens editor data matches the lens "Database" 
  swin = gtk_widget_get_parent(cv) 
  vadj = gtk_scrolled_window_get_vadjustment(swin)
  vPos = gtk_adjustment_get_value(vadj)
  hadj = gtk_scrolled_window_get_hadjustment(swin)
  hPos = gtk_adjustment_get_value(hadj)

  print *, "Value is ", gtk_adjustment_get_value(vadj)


  call clearColumnView()
  refRadio = c_null_ptr ! Null this out for the rebuild

  ! Remake
  store = buildLensEditTable()
  !selection = gtk_multi_selection_new(store)
  selection = gtk_single_selection_new(store)
  call g_signal_connect(selection, 'selection-changed'//c_null_char, c_funloc(lens_edit_row_selected), c_null_ptr) 
  call gtk_single_selection_set_autoselect(selection,TRUE)    
  call gtk_column_view_set_model(cv, selection)
  call gtk_column_view_set_show_column_separators(cv, 1_c_int)
  call gtk_column_view_set_show_row_separators(cv, 1_c_int)

  !call gtk_box_append(box, cv)                            

  call setLensEditColumns(cv)

  ! Set selection to previous
  !model = gtk_column_view_get_model(cv)
  boolResult = gtk_selection_model_select_item(selection, oldPos, 1_c_int)
  print *, "boolResult is ", boolResult

  call pending_events() ! Critical for following to work!
  vadj = gtk_scrolled_window_get_vadjustment(swin)
  hadj = gtk_scrolled_window_get_hadjustment(swin)
  call gtk_adjustment_set_value(vadj, vPos)
  call gtk_adjustment_set_value(hadj, hPos)

end subroutine

subroutine setLensEditColumns(colView)
  use type_utils, only: int2str
  type(c_ptr), value :: colView

  integer :: ii
  integer, target :: colIDs(25) = [(ii,ii=1,25)]
  type(c_ptr) :: factory, column
  character(kind=c_char, len=20),dimension(10) :: colNames
  character(kind=c_char, len=20),dimension(16) :: extraParamColNames  

  colNames(1) = "Surface"
  colNames(2) = "Ref"
  colNames(3) = "Surface Name"
  colNames(4) = "Surface Type"
  colNames(5) = "Radius"
  colNames(6) = "Thickness"  
  colNames(7) = "Glass"  
  colNames(8) = "Aperture"  

  do ii=1,size(extraParamColNames)
    extraParamColNames(ii) = "Par "//trim(int2str(ii))
  end do



  do ii=1,8
    factory = gtk_signal_list_item_factory_new()
    call g_signal_connect(factory, "setup"//c_null_char, c_funloc(setup_cb),c_loc(colIDs(ii)))
    call g_signal_connect(factory, "bind"//c_null_char, c_funloc(bind_cb),c_loc(colIDs(ii)))
    column = gtk_column_view_column_new(trim(colNames(ii))//c_null_char, factory)
    call gtk_column_view_column_set_id(column, trim(int2str(colIDs(ii))))
    call gtk_column_view_column_set_resizable(column, 1_c_int)
    if (ii > 4) then
    call gtk_column_view_column_set_fixed_width(column, 135_c_int) ! THis shouldn't be hard coded but until I figure out a better way
    end if
    call gtk_column_view_append_column (colView, column)
    call g_object_unref (column)      
  end do

  ! Do extra params separately for now
  do ii=1,size(extraParamColNames)
    factory = gtk_signal_list_item_factory_new()
    call g_signal_connect(factory, "setup"//c_null_char, c_funloc(setup_cb),c_loc(colIDs(ii+extra_param_start-1)))
    call g_signal_connect(factory, "bind"//c_null_char, c_funloc(bind_cb),c_loc(colIDs(ii+extra_param_start-1)))
    column = gtk_column_view_column_new(trim(extraParamColNames(ii))//c_null_char, factory)
    call gtk_column_view_column_set_id(column, trim(int2str(colIDs(ii+extra_param_start-1))))
    call gtk_column_view_column_set_resizable(column, 1_c_int)
    call gtk_column_view_column_set_fixed_width(column, 80_c_int) ! THis shouldn't be hard coded but until I figure out a better way
    call gtk_column_view_append_column (colView, column)
    call g_object_unref (column)    
  end do


end subroutine

subroutine updateColumnHeadersIfNeeded(surfType)
  use type_utils, only: int2str

  character(len=*) :: surfType
  integer :: ii, numItems, refCol
  type(c_ptr) :: listmodel, cStr, currCol
  character(len=100) :: ftext
  logical :: foundCol
  character(kind=c_char, len=20),dimension(16) :: extraParamColNames  
  character(kind=c_char, len=20),dimension(16) :: extraParamAsphereColNames  
  integer :: extra_param_start = 9

  ! Hard code for the two types for now. Need to abstract this into types

  do ii=1,size(extraParamColNames)
    extraParamColNames(ii) = "Par "//trim(int2str(ii))
  end do

  extraParamAsphereColNames(1) = "Conic (K)"
  extraParamAsphereColNames(2) = "4th order (A)"
  extraParamAsphereColNames(3) = "6th order (B)"
  extraParamAsphereColNames(4) = "8th order (C)"
  extraParamAsphereColNames(5) = "10th order (D)"
  extraParamAsphereColNames(6) = "12th order (E)"
  extraParamAsphereColNames(7) = "14th order (F)"
  extraParamAsphereColNames(8) = "16th order (G)"
  extraParamAsphereColNames(9) = "18th order (H)"
  extraParamAsphereColNames(10) = "20th order (I)"
  extraParamAsphereColNames(11:16) = extraParamColNames(11:16)

  !Find First Extra column
  foundCol = .FALSE.
  listmodel = gtk_column_view_get_columns(cv)
  numItems = g_list_model_get_n_items(listmodel) -1
  do ii=0,numItems
    currCol = g_list_model_get_object(listmodel,ii)
    cStr = gtk_column_view_column_get_id(currCol)
    call convert_c_string(cStr, ftext) 
    if (trim(ftext) == '9') then 
         refCol = ii
         foundCol = .TRUE.
         exit
    end if
  end do

  if(foundCol) then
    cStr = gtk_column_view_column_get_title(currCol)
    call convert_c_string(cStr, ftext)   
    if (surfType == 'Sphere') then 
      do ii=1,16
          call gtk_column_view_column_set_title(currCol, trim(extraParamColNames(ii))//c_null_char)
          currCol = g_list_model_get_object(listmodel,refCol+ii)
      end do
    end if
    if (surfType == 'Asphere') then 
      do ii=1,16
        call gtk_column_view_column_set_title(currCol, trim(extraParamAsphereColNames(ii))//c_null_char)
        currCol = g_list_model_get_object(listmodel,refCol+ii)
    end do
    end if

  end if



end subroutine

subroutine lens_edit_row_selected(widget, position, n_items, userdata) bind(c)
  type(c_ptr), value ::  widget, userdata
  integer(c_int) :: position, n_items
  type(c_ptr) :: listitem, cStr
  character(len=100) :: ftext
  print *, "Row selected! "
  listitem = gtk_single_selection_get_selected_item(widget)
  cStr = lens_item_get_surface_type(listitem)
  call convert_c_string(cStr, ftext)   
  print *, "Surf type is ", trim(ftext)
  call updateColumnHeadersIfNeeded(trim(ftext))
  call gtk_widget_set_sensitive(dbut, TRUE)
  call gtk_widget_set_sensitive(ibut, TRUE)
end subroutine

  function lens_editor_create_table() result(boxNew)

    use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char

    type(integer) :: ID_TAB
    type(c_ptr) :: boxNew

    !Debug
    integer :: ii
    integer, target :: colIDs(10) = [(ii,ii=1,10)]

    type(c_ptr) :: store, cStrB, listitem, selection, factory, column, swin
    character(len=1024) :: debugName



    boxNew = hl_gtk_box_new()


      refRadio = c_null_ptr
      store = buildLensEditTable()
    
  
      selection = gtk_single_selection_new(store)
      call g_signal_connect(selection, 'selection-changed'//c_null_char, c_funloc(lens_edit_row_selected), c_null_ptr)      !selection = gtk_multi_selection_new(store)
      call gtk_single_selection_set_autoselect(selection,TRUE)    
      cv = gtk_column_view_new(selection)
      call gtk_column_view_set_show_column_separators(cv, 1_c_int)
      call gtk_column_view_set_show_row_separators(cv, 1_c_int)
      call gtk_column_view_set_reorderable(cv, 0_c_int)


      call setLensEditColumns(cv)

      swin = gtk_scrolled_window_new()
      call gtk_scrolled_window_set_child(swin, cv)
      call gtk_scrolled_window_set_min_content_height(swin, 300_c_int) !TODO:  Fix this properly 
      call gtk_box_append(boxNew, swin)

    ! Delete selected row
      ibut = hl_gtk_button_new("Insert row"//c_null_char, &
      & clicked=c_funloc(ins_row), &
      & tooltip="Insert new row above"//c_null_char, sensitive=FALSE)

      call hl_gtk_box_pack(boxNew, ibut)

      ! Delete selected row
      dbut = hl_gtk_button_new("Delete selected row"//c_null_char, &
            & clicked=c_funloc(del_row), &
            & tooltip="Delete the selected row"//c_null_char, sensitive=FALSE)

      call hl_gtk_box_pack(boxNew, dbut)

      ! Also a quit button
      qbut = hl_gtk_button_new("Quit"//c_null_char, clicked=c_funloc(lens_editor_destroy), data=lens_editor_window)
      call hl_gtk_box_pack(boxNew,qbut)


  end function


end module lens_editor
