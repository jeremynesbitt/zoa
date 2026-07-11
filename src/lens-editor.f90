! UI to allow user to edit surface data
! Rev 2
module lens_editor

  use gtk
  use g
  use iso_c_binding
  use globals, only: INFINITY_DISTANCE, INFINITY_DISPLAY_THRESHOLD
  use global_widgets
  use zoa_ui_callbacks, only: notify_replot, notify_replot_flush
  use zoa_output, only: zoa_emit
!  use gth_hl
  use hl_gtk_zoa
  use gtk_hl_button
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
    call notify_replot()
    call notify_replot_flush()
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
    call zoa_emit('OBJECT SURFACE MAY NOT BE DELETED', "black")
    return
  END IF
  IF(currRow == ldm%getLastSurf()) THEN
    call zoa_emit('IMAGE SURFACE MAY NOT BE DELETED', "black")
    return
  END IF
  
  call PROCESKDP('U L ; CHG '//trim(int2str(currRow))//' ; DELK ; EOS')
  call rebuildLensEditorTable()
  call notify_replot()
  call notify_replot_flush()

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
    call notify_replot()

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
  integer :: n

  extraParams = 0.0d0

  ! Read from typed surface if available (data(1:num_params) are the display values).
  if (allocated(ldm%surfaces)) then
    if (surf >= lbound(ldm%surfaces,1) .and. surf <= ubound(ldm%surfaces,1)) then
      if (allocated(ldm%surfaces(surf)%s)) then
        n = min(ldm%surfaces(surf)%s%num_params, 16)
        extraParams(1:n) = ldm%surfaces(surf)%s%data(1:n)
        return
      end if
    end if
  end if

  ! Fallback: read directly from ALENS.
  select case(surfType)
  case('Asphere')
    extraParams(1)    = ALENS(2,surf)        ! conic K
    extraParams(2:5)  = ALENS(4:7,surf)      ! A4-A10
    extraParams(6:10) = ALENS(81:85,surf)    ! A12-A20
  end select

end function

! This func interfaces with the c struct that stores the data
! At some point I may migrate this to fortran but for now the
! main cost of this is a bunch of interfaces for each get which
! I can live with
function buildLensEditTable() result(store)
  use mod_lens_data_manager
  use kdp_data_types, only: check_clear_apertures


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

    ! Refresh the typed clap before display:
    !  1) load_surfaces_from_alens picks up shape/dim from current ALENS so a
    !     user CIR/CLAP edit (which only wrote ALENS) is reflected via is_set/dim1;
    !  2) check_clear_apertures refills the ray-traced auto_* extents (load wipes them).
    ! Order matters: load first (resets auto_* to 0), then recompute auto.
    call ldm%load_surfaces_from_alens()
    if (allocated(ldm%surfaces)) call check_clear_apertures(curr_lens_data, ldm%surfaces)

    ! Show each surface's clap: the user/assigned value if set, else the
    ! ray-traced automatic semi-diameter (clap%auto_semi_y), via display_semi_y().
    do i = 1, curr_lens_data%num_surfaces
      if (allocated(ldm%surfaces) .and. i-1 <= ubound(ldm%surfaces,1)) then
        clearApertures(i) = ldm%surfaces(i-1)%s%clap%display_semi_y()
      else
        clearApertures(i) = 0.0_real64
      end if
    end do
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
      call convert_f_string(trim(thicSolves(i)), strTmp)
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
      call convert_f_string(trim(curvSolves(i)), strTmp)
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
    use DATLEN
    character(len=40), dimension(curr_lens_data%num_surfaces) :: surfName_array
    integer :: i
    do i=1,curr_lens_data%num_surfaces-1
      surfName_array(i) = LBL(i-1)(1:40)

    end do


  end function

  function getSurfaceTypesAsCStringArray() result(c_ptr_array)
    use mod_surface_type, only: NUM_SURFACE_TYPES, SURFACE_TYPE_NAMES
    integer :: i
    type(c_ptr), dimension(NUM_SURFACE_TYPES+1) :: c_ptr_array
    character(kind=c_char), dimension(:), allocatable :: strTmp
    character(kind=c_char), pointer, dimension(:) :: ptrTmp

    do i = 1, NUM_SURFACE_TYPES
      call convert_f_string(trim(SURFACE_TYPE_NAMES(i)), strTmp)
      allocate(ptrTmp(size(strTmp)))
      ptrTmp(:) = strTmp(:)
      c_ptr_array(i) = c_loc(ptrTmp(1))
      nullify(ptrTmp)
    end do
    c_ptr_array(NUM_SURFACE_TYPES+1) = c_null_ptr

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
      call convert_f_string(trim(int2str(surfIdx(i))), strTmp)
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
      use pickup_manager, only: pickup_get
      use type_utils, only:  int2str

      integer :: row, pickup_type

     
      type(c_ptr), value, optional :: btn
      type(c_ptr) :: win,pUpdate, pCancel, boxWin, cBut, uBut
      type(c_ptr) :: table, lblSurf
      real(kind=c_double) :: pickupScale
      integer :: pkSrc
      real(kind=c_double) :: pkScale, pkOffset
      logical :: pkFound

      ! pickup_type is the PIKUP array J index (RD=1, TH=3, CC=4, AD..AL=5-8,27-31)
      pkFound = pickup_get(row, pickup_type, pkSrc, pkScale, pkOffset)

      pData%ID_type = pickup_type
      call pData%setPickupText()
      pData%surf = row
      pData%surf_ref = pkSrc
      pData%scale = pkScale
      pData%offset = pkOffset

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
      call gtk_drop_down_set_selected(dropDown, pkSrc)

      lblSurf = gtk_label_new("Surface"//c_null_char)
      !gtk_grid_attach (GtkGrid *grid, GtkWidget *child, int column, int row, int width, int height);
      call gtk_grid_attach(table, lblSurf, 0_c_int, 0_c_int, 1_c_int, 1_c_int)
      call gtk_grid_attach(table, dropDown, 1_c_int, 0_c_int, 1_c_int, 1_c_int)

      lblScale = gtk_label_new("Scale"//c_null_char)
      lblOffset = gtk_label_new("Offset"//c_null_char)

      call gtk_grid_attach(table, lblScale, 0_c_int, 1_c_int, 1_c_int, 1_c_int)
      call gtk_grid_attach(table, lblOffset, 0_c_int, 2_c_int, 1_c_int, 1_c_int)

      if (pkFound) then
        pickupScale = pkScale
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
      & value=pkOffset, &
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
    use pickup_manager, only: pickup_set_cmd
    type(c_ptr), value, intent(in) :: widget, gdata

    PRINT *, "Button clicked!"
    pData%scale = gtk_spin_button_get_value (sbScale)
    pData%offset = gtk_spin_button_get_value (sbOffset)
    pData%surf_ref = gtk_drop_down_get_selected(dropDown)


    ! It's time to update the pickup (command built by the pickup manager)
    CALL PROCESKDP('U L')
    CALL PROCESKDP("CHG, "//trim(int2str(pData%surf)))

    call PROCESKDP(trim(pickup_set_cmd(pData%ID_type, pData%surf_ref, &
    &                                  pData%scale, pData%offset)))

    CALL PROCESKDP('EOS')
    mod_update = .TRUE.
    call refreshLensDataStruct()
    call notify_replot()
    call notify_replot_flush()

    call gtk_window_destroy(gdata)



  end subroutine

  subroutine pickupCancel_click(widget, parentWin) bind(c)
    type(c_ptr), value, intent(in) :: widget, parentWin

    PRINT *, "Button clicked!"

    ! This is a bad design but not sure what else to do to get rid of the "P" entry without
    ! yet another global
    call refreshLensDataStruct()
    call notify_replot()
    call notify_replot_flush()
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
  use solve_manager, only: solve_set_cmd, solve_remove_cmd, solve_kind_from_cmd, &
  &                        SLV_YZ_THI_SLOT, SLV_YZ_CURV_SLOT
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
    & trim(solve_remove_cmd(merge(SLV_YZ_THI_SLOT, SLV_YZ_CURV_SLOT, &
    &      ID_SETTING == ID_PICKUP_THIC), sData%surf))//';EOS')
  else
  ! It's time to update the pickup
    CALL PROCESKDP('U L ; CHG, '//trim(int2str(sData%surf)) &
    & //"; "//trim(solve_set_cmd(solve_kind_from_cmd(sData%cmd_kdp), sData%param1))//";EOS")
  end if
  



  mod_update = .TRUE.
  call gtk_menu_button_set_icon_name(curr_btn, 'letter-s'//c_null_char)
  !call refreshLensDataStruct()
  !call notify_replot()


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
  use strings

type(c_ptr), value :: widget, data
type(c_ptr) :: buff2, cStr
character(len=100) :: ftext, rcCode, cmd
integer :: surfIdx

buff2 = gtk_entry_get_buffer(widget)
call c_f_string_copy(gtk_entry_buffer_get_text(buff2), ftext)

print *, "Val is ", trim(ftext)
if (lowercase(ftext) == 'infinity') write(ftext, '(ES12.4E2)') INFINITY_DISTANCE
cStr = gtk_widget_get_name(gtk_widget_get_parent(widget))
call convert_c_string(cStr, rcCode)  
print *, "Val is ", trim(rcCode)

surfIdx = getSurfaceIndexFromRowColumnCode(trim(rcCode))

call convert_c_string(data, cmd)  
print *, "cmd is ", trim(cmd)
print *, "cmd to process is ", trim(cmd)//" S"//trim(int2str(surfIdx))//" "//trim(ftext)
call PROCESSILENT(trim(cmd)//" S"//trim(int2str(surfIdx))//" "//trim(ftext))
! The edit went through PROCESSILENT, not name_enter, so its deferred replot is
! never drained there. We are now back at top level (the command has returned),
! so flush any pending replot to refresh active plots.
call notify_replot_flush()
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
  call notify_replot_flush()
end if

end subroutine

subroutine removeSolve(act, avalue, btn) bind(c)
  use solve_manager, only: solve_remove_cmd, SLV_YZ_THI_SLOT, SLV_YZ_CURV_SLOT
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
  & trim(solve_remove_cmd(merge(SLV_YZ_THI_SLOT, SLV_YZ_CURV_SLOT, &
  &      sData%solve_type == ID_PICKUP_THIC), sData%surf))//';EOS')
  call rebuildLensEditorTable()

end subroutine

subroutine removePickup(act, avalue, btn) bind(c)
  use pickup_manager, only: pickup_remove_cmd
  type(c_ptr), value :: act, avalue, btn
  type(c_ptr) :: cStr
  character(len=100) :: rcCode
  integer :: surfIdx, colIdx, jIdx

  cStr = gtk_widget_get_name(gtk_widget_get_parent(btn))
  call convert_c_string(cStr, rcCode)
  print *, "Val is ", trim(rcCode)
  surfIdx = getSurfaceIndexFromRowColumnCode(trim(rcCode), colIdx)

  ! PIKD command built by the pickup manager from the column's kind
  jIdx = colToPikupJ(surfIdx, colIdx)
  if (jIdx == 0) return
  CALL PROCESKDP('U L ; '// &
  & trim(pickup_remove_cmd(jIdx, surfIdx))//';EOS')
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

  colCode = colToPikupJ(surfIdx, colIdx)
  if (colCode == 0) return

  call ui_pickup(surfIdx, colCode, btn)

end subroutine

subroutine enableVar(act, avalue, btn) bind(c)
  use type_utils
  type(c_ptr), value :: act, avalue, btn
  type(c_ptr) :: cStr
  integer :: surfIdx, colIdx
  character(len=100) :: rcCode
  character(len=5) :: cmd

  !cStr = gtk_widget_get_name(btn)
  cStr = gtk_widget_get_name(gtk_widget_get_parent(btn))
  call convert_c_string(cStr, rcCode)
  print *, "Val is ", trim(rcCode)

  surfIdx = getSurfaceIndexFromRowColumnCode(trim(rcCode), colIdx)
  print *, "surfIdx is ", surfIdx
  mod_update = .FALSE.

  cmd = colToVarCmd(surfIdx, colIdx)
  if (len_trim(cmd) == 0) return

  call PROCESSILENT(trim(cmd)//" S"//trim(int2str(surfIdx))//" 0")
  call rebuildLensEditorTable()

end subroutine

subroutine removeVar(act, avalue, btn) bind(c)
  use type_utils
  type(c_ptr), value :: act, avalue, btn
  type(c_ptr) :: cStr
  integer :: surfIdx, colIdx
  character(len=100) :: rcCode
  character(len=5) :: cmd

  !cStr = gtk_widget_get_name(btn)
  cStr = gtk_widget_get_name(gtk_widget_get_parent(btn))
  call convert_c_string(cStr, rcCode)
  print *, "Val is ", trim(rcCode)

  surfIdx = getSurfaceIndexFromRowColumnCode(trim(rcCode), colIdx)
  print *, "surfIdx is ", surfIdx
  mod_update = .FALSE.

  cmd = colToVarCmd(surfIdx, colIdx)
  if (len_trim(cmd) == 0) return

  call PROCESSILENT(trim(cmd)//" S"//trim(int2str(surfIdx))//" 100")
  call rebuildLensEditorTable()

end subroutine

! --- Column -> modifier-code helpers ------------------------------------------
! The lens editor columns map to backend codes as: Radius(5)=curvature,
! Thickness(6)=thickness, Glass(7)=glass (pickup + GLC variable; no solve),
! extra-param columns (9..18) = surface-type params (asphere: 1=K conic,
! 2..10=A4..A20).  These helpers centralize that mapping for variables
! (VAR_* code + CODE V var-code command) and pickups (PIKUP J).

! VAR_* code for a column (0 => column has no variable support on this surface)
function colToVarCode(surf, colIdx) result(varCode)
  use mod_lens_data_manager
  integer, intent(in) :: surf, colIdx
  integer :: varCode, i

  varCode = 0
  select case (colIdx)
  case(ID_COL_RADIUS)
    varCode = VAR_CURV
  case(ID_COL_THICKNESS)
    varCode = VAR_THI
  case(ID_COL_GLASS)
    varCode = VAR_GLA
  case(extra_param_start:extra_param_start+9)
    i = colIdx - extra_param_start + 1
    ! Only params with pickup/KDP plumbing (i.e. defined on the surface type)
    if (ldm%getExtraParamPikupIdx(surf, i) /= 0) then
      if (i == 1) then
        varCode = VAR_K
      else
        varCode = VAR_A4 + (i - 2)
      end if
    end if
  end select
end function

! CODE V variable-code command for a column (CCY/THC/KC/AC..IC; blank => none)
function colToVarCmd(surf, colIdx) result(cmd)
  use mod_lens_data_manager
  integer, intent(in) :: surf, colIdx
  character(len=5) :: cmd
  integer :: i

  cmd = ' '
  select case (colIdx)
  case(ID_COL_RADIUS)
    cmd = 'CCY'
  case(ID_COL_THICKNESS)
    cmd = 'THC'
  case(ID_COL_GLASS)
    cmd = 'GLC'
  case(extra_param_start:extra_param_start+9)
    i = colIdx - extra_param_start + 1
    if (ldm%getExtraParamPikupIdx(surf, i) /= 0) &
      cmd = trim(ldm%getExtraParamCmd(surf, i))//'C'   ! K->KC, A->AC, ...
  end select
end function

! PIKUP array J index for a column (0 => no pickup support)
function colToPikupJ(surf, colIdx) result(jIdx)
  use mod_lens_data_manager
  integer, intent(in) :: surf, colIdx
  integer :: jIdx

  jIdx = 0
  select case (colIdx)
  case(ID_COL_RADIUS)
    jIdx = ID_PICKUP_RAD
  case(ID_COL_THICKNESS)
    jIdx = ID_PICKUP_THIC
  case(ID_COL_GLASS)
    jIdx = ID_PICKUP_GLASS
  case(extra_param_start:extra_param_start+9)
    jIdx = ldm%getExtraParamPikupIdx(surf, colIdx - extra_param_start + 1)
  end select
end function

function createModMenu(btn, surf, colIdx) result(menuOptions)
  use mod_lens_data_manager
  use type_utils
  type(c_ptr), value :: btn
  type(c_ptr) :: menuOptions
  type(c_ptr) :: actGroup, actP, mPickup, mSolve, actS
  type(c_ptr) :: actRemoveSolve, mRemoveSolve, actRemovePickup, mRemovePickup
  type(c_ptr) :: actVar, mVar, actRemoveVar, mRemoveVar
  integer :: surf, colIdx, colType, pikJ
  logical :: hasSolve

  colType = colToVarCode(surf, colIdx)
  pikJ    = colToPikupJ(surf, colIdx)
  ! Solves exist only for curvature and thickness (SLVRS); the special-surface
  ! params (conic/asphere coefficients) offer Variable and Pickup only.
  hasSolve = (colIdx == ID_COL_RADIUS .or. colIdx == ID_COL_THICKNESS)

  actGroup = g_simple_action_group_new()
  call gtk_widget_insert_action_group(btn, "mod"//c_null_char,actGroup)

  actP = g_simple_action_new("setPickup"//trim(int2str(surf))//c_null_char, c_null_ptr)
  call g_action_map_add_action(actGroup, actP)
  call g_signal_connect(actP, "activate"//c_null_char, c_funloc(enablePickup), btn)
  mPickup = g_menu_item_new("Set Pickup"//c_null_char, "mod.setPickup"//trim(int2str(surf))//c_null_char)

  actRemovePickup = g_simple_action_new("removePickup"//trim(int2str(surf))//c_null_char, c_null_ptr)
  if (ldm%isPikupOnSurfJ(surf, pikJ)) call g_action_map_add_action(actGroup, actRemovePickup)
  call g_signal_connect(actRemovePickup, "activate"//c_null_char, c_funloc(removePickup), btn)
  mRemovePickup = g_menu_item_new("Remove Pickup"//c_null_char, "mod.removePickup"//trim(int2str(surf))//c_null_char)

  if (hasSolve) then
    actS = g_simple_action_new("setSolve"//trim(int2str(surf))//c_null_char, c_null_ptr)
    call g_action_map_add_action(actGroup, actS)
    call g_signal_connect(actS, "activate"//c_null_char, c_funloc(enableSolve), btn)
    mSolve = g_menu_item_new("Set Solve"//c_null_char, "mod.setSolve"//trim(int2str(surf))//c_null_char)

    actRemoveSolve = g_simple_action_new("removeSolve"//trim(int2str(surf))//c_null_char, c_null_ptr)
    if (ldm%isSolveOnSurf(surf, colType)) call g_action_map_add_action(actGroup, actRemoveSolve)
    call g_signal_connect(actRemoveSolve, "activate"//c_null_char, c_funloc(removeSolve), btn)
    mRemoveSolve = g_menu_item_new("Remove Solve"//c_null_char, "mod.removeSolve"//trim(int2str(surf))//c_null_char)
  end if
  
  actVar = g_simple_action_new("setVar"//trim(int2str(surf))//c_null_char, c_null_ptr)
  if (colType /= 0) then
    if (ldm%isVarOnSurf(surf, colType) .eqv. .FALSE.) call g_action_map_add_action(actGroup, actVar)
  end if
  call g_signal_connect(actVar, "activate"//c_null_char, c_funloc(enableVar), btn)
  mVar = g_menu_item_new("Set Variable"//c_null_char, "mod.setVar"//trim(int2str(surf))//c_null_char)

  actRemoveVar = g_simple_action_new("removeVar"//trim(int2str(surf))//c_null_char, c_null_ptr)
  if (colType /= 0) then
    if (ldm%isVarOnSurf(surf, colType)) call g_action_map_add_action(actGroup, actRemoveVar)
  end if
  call g_signal_connect(actRemoveVar, "activate"//c_null_char, c_funloc(removeVar), btn)
  mRemoveVar = g_menu_item_new("Remove Variable"//c_null_char, "mod.removeVar"//trim(int2str(surf))//c_null_char)

  menuOptions = g_menu_new()
  call g_menu_append_item(menuOptions, mPickup)
  call g_menu_append_item(menuOptions, mRemovePickup)
  if (hasSolve) then
    call g_menu_append_item(menuOptions, mSolve)
    call g_menu_append_item(menuOptions, mRemoveSolve)
  end if
  call g_menu_append_item(menuOptions, mVar)
  call g_menu_append_item(menuOptions, mRemoveVar)


end function

subroutine updateSurfaceType(widget, gdata) bind(c)
  use mod_surface, only: surf_is_asphere
  use type_utils, only: int2str
  type(c_ptr), value, intent(in) :: widget, gdata
  integer :: selection, surfIdx
  logical :: wasAsphere
  character(len=100) :: rcCode
  type(c_ptr) :: cStr

  cStr = gtk_widget_get_name(widget)
  call convert_c_string(cStr, rcCode)
  surfIdx = getSurfaceIndexFromRowColumnCode(trim(rcCode))

  selection = gtk_drop_down_get_selected(widget)
  wasAsphere = surf_is_asphere(surfIdx)

  select case (selection)
  case(0) ! Sphere
    if (wasAsphere) call PROCESSILENT("SPH S"//trim(int2str(surfIdx)))
  case(1) ! Asphere
    if (.not. wasAsphere) call PROCESSILENT("ASP S"//trim(int2str(surfIdx)))
  end select

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
    entryCB = hl_gtk_entry_new(10_c_int, editable=TRUE, activate=c_funloc(cell_changed), data=g_strdup(cmd//c_null_char))
    !call gtk_widget_set_name(entryCB, cmd) 
    call gtk_box_append(boxS, entryCB)
    call gtk_box_append(boxS, menuB)
    call gtk_list_item_set_child(listitem,boxS)
   case(7) ! Glass + modifier (Pickup/Variable menu; glass has no solve)
    boxS = hl_gtk_box_new(horizontal=TRUE, spacing=0_c_int)
    entryCB = hl_gtk_entry_new(10_c_int, editable=TRUE, activate=c_funloc(cell_changed), data=g_strdup('GLA'))
    menuB = gtk_menu_button_new()
    call gtk_box_append(boxS, entryCB)
    call gtk_box_append(boxS, menuB)
    call gtk_list_item_set_child(listitem, boxS)
  case(8) ! Aperture
    boxS = hl_gtk_box_new(horizontal=TRUE, spacing=0_c_int)
    entryCB = hl_gtk_entry_new(10_c_int, editable=TRUE, activate=c_funloc(cell_changed), data=g_strdup('CIR'))
    call gtk_box_append(boxS, entryCB)
    call gtk_list_item_set_child(listitem, boxS)      
    
  ! Extra params (surface-type-specific: conic + asphere coefficients).
  ! Same entry+menu-button pattern as Radius/Thickness; bind_cb hides the
  ! button for columns beyond the surface's defined params.
  case(9:18)
    boxS = hl_gtk_box_new(horizontal=TRUE, spacing=0_c_int)
    entryCB = hl_gtk_entry_new(10_c_int, editable=TRUE, activate=c_funloc(extraParam_changed), data=c_null_ptr)
    menuB = gtk_menu_button_new()
    call gtk_box_append(boxS, entryCB)
    call gtk_box_append(boxS, menuB)
    call gtk_list_item_set_child(listitem, boxS)
   case default 
    label =gtk_label_new(c_null_char)
    call gtk_list_item_set_child(listitem,label)
   end select 
end subroutine

subroutine bind_cb(factory,listitem, gdata) bind(c)
  use type_utils
  use mod_surface_type, only: surface_type_index
  use mod_lens_data_manager, only: ldm
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
    call gtk_entry_buffer_set_text(buffer, trim(colName)//c_null_char,-1_c_int)
   case(4)
    cStr = lens_item_get_surface_type(item)
    call convert_c_string(cStr, colName)
    call gtk_drop_down_set_selected(label, int(surface_type_index(trim(colName)), c_int))
    !call gtk_label_set_text(label, trim(colName)//c_null_char)    
   case(5)
    colName = trim(real2str(lens_item_get_surface_radius(item)))//c_null_char  
    entryCB = gtk_widget_get_first_child(label)  
    buffer = gtk_entry_get_buffer(entryCB)
    !buffer = gtk_entry_get_buffer(label)
    call gtk_entry_buffer_set_text(buffer, trim(colName)//c_null_char,-1_c_int)

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


  case(6) ! Thickness
    colName = trim(real2str(lens_item_get_surface_thickness(item)))//c_null_char  
    entryCB = gtk_widget_get_first_child(label)  
    buffer = gtk_entry_get_buffer(entryCB)
    !buffer = gtk_entry_get_buffer(label)
    if (abs(lens_item_get_surface_thickness(item)) > INFINITY_DISPLAY_THRESHOLD) then
      call gtk_entry_buffer_set_text(buffer, "Infinity"//c_null_char,-1_c_int)
    else
      call gtk_entry_buffer_set_text(buffer, trim(colName)//c_null_char,-1_c_int)
    end if
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

    ! Modifier menu button (Pickup/Variable; glass has no solve).  Same
    ! pattern as the extra-param columns: icon shows P for a glass pickup,
    ! V for a GLC variable, blank otherwise.
    block
      integer :: surfNo, varCode
      menuCB = gtk_widget_get_next_sibling(entryCB)
      surfNo = lens_item_get_surface_number(item)
      call gtk_menu_button_set_menu_model(menuCB, createModMenu(menuCB, surfNo, ID_COL))
      varCode = colToVarCode(surfNo, ID_COL)
      if (ldm%isPikupOnSurfJ(surfNo, ID_PICKUP_GLASS)) then
        call gtk_menu_button_set_icon_name(menuCB, 'letter-p'//c_null_char)
      else if (ldm%isVarOnSurf(surfNo, varCode)) then
        call gtk_menu_button_set_icon_name(menuCB, 'letter-v'//c_null_char)
      else
        call gtk_menu_button_set_icon_name(menuCB, 'letter-blank'//c_null_char)
      end if
    end block
  case(8) ! Clear Aperture
    colName = trim(real2str(lens_item_get_aperture(item)))//c_null_char  
    entryCB = gtk_widget_get_first_child(label)  
    buffer = gtk_entry_get_buffer(entryCB)    
    call gtk_entry_buffer_set_text(buffer, trim(colName),-1_c_int)
  case(9:18)
    entryCB = gtk_widget_get_first_child(label)
    buffer = gtk_entry_get_buffer(entryCB)
    if (abs(lens_item_get_extra_param(item, ID_COL-9)) < .01) then
        colName = trim(real2str(lens_item_get_extra_param(item, ID_COL-9),sci=.TRUE.))//c_null_char
    else
        colName = trim(real2str(lens_item_get_extra_param(item, ID_COL-9)))//c_null_char
    end if
    call gtk_entry_buffer_set_text(buffer, trim(colName),-1_c_int)

    ! Modifier menu button (Variable/Pickup): only for params this surface's
    ! type defines (pickup J index 0 => undefined => hide the button).
    block
      integer :: surfNo, pikJ, varCode
      menuCB = gtk_widget_get_next_sibling(entryCB)
      surfNo = lens_item_get_surface_number(item)
      pikJ   = colToPikupJ(surfNo, ID_COL)
      if (pikJ == 0) then
        call gtk_widget_set_visible(menuCB, FALSE)
      else
        call gtk_widget_set_visible(menuCB, TRUE)
        call gtk_menu_button_set_menu_model(menuCB, createModMenu(menuCB, surfNo, ID_COL))
        varCode = colToVarCode(surfNo, ID_COL)
        if (ldm%isPikupOnSurfJ(surfNo, pikJ)) then
          call gtk_menu_button_set_icon_name(menuCB, 'letter-p'//c_null_char)
        else if (varCode /= 0 .and. ldm%isVarOnSurf(surfNo, varCode)) then
          call gtk_menu_button_set_icon_name(menuCB, 'letter-v'//c_null_char)
        else
          call gtk_menu_button_set_icon_name(menuCB, 'letter-blank'//c_null_char)
        end if
      end if
    end block
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
  use handlers, only: pending_events

  type(c_ptr) :: store, selection, model, column, vadj, hadj, swin
  integer(c_int) :: oldPos
  real(c_double) :: vPos, hPos


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
  if (oldPos >= 0_c_int) then
    call gtk_single_selection_set_selected(selection, oldPos)
  end if

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
    ! Same default width as the Radius/Thickness columns: these now hold an
    ! entry plus the modifier menu button, which 80px was too narrow for.
    call gtk_column_view_column_set_fixed_width(column, 135_c_int) ! THis shouldn't be hard coded but until I figure out a better way
    call gtk_column_view_append_column (colView, column)
    call g_object_unref (column)    
  end do


end subroutine

subroutine updateColumnHeadersIfNeeded(surfIdx)
  use type_utils, only: int2str
  use mod_lens_data_manager, only: ldm

  integer, intent(in) :: surfIdx
  integer :: ii, numItems, refCol, nParams
  type(c_ptr) :: listmodel, cStr, currCol
  character(len=100) :: ftext
  logical :: foundCol, typedFound
  character(kind=c_char, len=24), dimension(16) :: colNames
  character(len=24) :: typeName

  ! Default: generic "Par N" labels.
  do ii = 1, 16
    colNames(ii) = "Par "//trim(int2str(ii))
  end do

  ! Prefer typed surface's param_names when the surface object is loaded.
  typedFound = .false.
  if (allocated(ldm%surfaces)) then
    if (surfIdx >= lbound(ldm%surfaces,1) .and. surfIdx <= ubound(ldm%surfaces,1)) then
      if (allocated(ldm%surfaces(surfIdx)%s)) then
        nParams = ldm%surfaces(surfIdx)%s%num_params
        do ii = 1, min(nParams, 16)
          colNames(ii) = trim(ldm%surfaces(surfIdx)%s%param_names(ii))
        end do
        typedFound = .true.
      end if
    end if
  end if

  ! Fallback: derive column names from the ALENS-based type name so that
  ! lenses entered manually (not via RES/CV2PRG) still show correct headers.
  if (.not. typedFound) then
    typeName = ldm%getSurfTypeName(surfIdx)
    if (trim(typeName) == 'Asphere') then
      colNames(1)  = "Conic (K)";      colNames(2)  = "4th order (A)"
      colNames(3)  = "6th order (B)";  colNames(4)  = "8th order (C)"
      colNames(5)  = "10th order (D)"; colNames(6)  = "12th order (E)"
      colNames(7)  = "14th order (F)"; colNames(8)  = "16th order (G)"
      colNames(9)  = "18th order (H)"; colNames(10) = "20th order (I)"
    end if
  end if

  ! Find the first extra-param column (ID = '9') and update all 16 headers.
  foundCol = .FALSE.
  listmodel = gtk_column_view_get_columns(cv)
  numItems = g_list_model_get_n_items(listmodel) - 1
  do ii = 0, numItems
    currCol = g_list_model_get_object(listmodel, ii)
    cStr = gtk_column_view_column_get_id(currCol)
    call convert_c_string(cStr, ftext)
    if (trim(ftext) == '9') then
      refCol = ii
      foundCol = .TRUE.
      exit
    end if
  end do

  if (foundCol) then
    do ii = 1, 16
      call gtk_column_view_column_set_title(currCol, trim(colNames(ii))//c_null_char)
      if (ii < 16) currCol = g_list_model_get_object(listmodel, refCol+ii)
    end do
  end if

end subroutine

subroutine lens_edit_row_selected(widget, position, n_items, userdata) bind(c)
  use DATLEN, only: HILITE_SURF
  type(c_ptr), value ::  widget, userdata
  integer(c_int) :: position, n_items
  type(c_ptr) :: listitem
  integer :: selSurf
  listitem = gtk_single_selection_get_selected_item(widget)
  selSurf = lens_item_get_surface_number(listitem)
  call updateColumnHeadersIfNeeded(selSurf)
  call gtk_widget_set_sensitive(dbut, TRUE)
  call gtk_widget_set_sensitive(ibut, TRUE)
  ! Zemax-style surface highlight: redraw any active lens plot (VIE) with the
  ! selected surface drawn in the highlight colour.
  HILITE_SURF = selSurf
  call notify_replot()
  call notify_replot_flush()
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
