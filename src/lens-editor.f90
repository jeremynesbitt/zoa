! TODO:
! make a single add_dialog function that works for every tab (using function pointer?)
! abstract the adding of label to notebook tab to avoid making more ptr variables (or 
! combine lines?)

! Notes on dynamic columns
! make a 2d array that has:  dim 1 all combo box IDs
!                            dim 2 all currently selected values (for each row)
! This should allow for seetings to be persisted, at least while editor is open
! does not solve the row problem
! Need to stop recreating data type and drawing UI during every update

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
    & radius, radiusMod, thickness, thickMod, glass, aperture, index) bind(c)
    import c_ptr, c_char, c_int, c_double
    implicit none
    type(c_ptr), value    :: store
    integer(c_int), value :: surfaceNo, refSurf, radiusMod, thickMod
    character(kind=c_char), dimension(*) :: surfaceName, surfaceType, glass
    type(c_ptr)    :: append_lens_model
    real(c_double), value :: radius, thickness, aperture, index
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
  end interface

  ! TODO:  Modifiy this so we can store values for combo entries
  type lens_edit_col
    integer(kind=type_kind) :: coltype
    character(len=20) :: coltitle
    integer(kind=c_int) :: sortable, editable
    integer :: dtype
    class(*), allocatable, dimension(:) :: data
    integer, allocatable, dimension(:)  :: dataInt
    real, allocatable, dimension(:)  :: dataFloat
    character(len=40), allocatable, dimension(:) :: dataString
    integer(c_int), allocatable, dimension(:) :: refsArray
    character(len=20), allocatable, dimension(:) :: valsArray

    integer :: colModel


  contains
    procedure, public, pass(self) :: initialize => le_col_init
    procedure, public, pass(self) :: getElementInt
    procedure, public, pass(self) :: getElementFloat
    procedure, public, pass(self) :: getElementString

  end type


  type(c_ptr) :: ihscrollcontain,ihlist, &
       &  qbut, dbut, lbl, ibut, ihAsph, ihSolv, ihScrollAsph, ihScrollSolv

  integer(kind=c_int) :: numEditorRows
  type(c_ptr) :: sbScale, sbOffset, dropDown, lblScale, lblOffset

  ! Pickup Global.  Since the ui for this is modal pseudo safe to just store
  ! current pickup to be modded in this type.  Same idea with solve
  type(pickup) :: pData
  type(ksolve)  :: sData

  type(c_ptr) :: win_modal_solve
  

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

  integer, parameter :: ID_SURF_SPHERE = 1



  integer, parameter :: DTYPE_INT = 1
  integer, parameter :: DTYPE_FLOAT = 2
  integer, parameter :: DTYPE_STRING = 3
  integer, parameter :: DTYPE_SCIENTIFIC = 4
  integer, parameter :: DTYPE_BOOLEAN = 5

  integer, parameter :: COL_MODEL_COMBO = 5

  integer(kind=c_int), parameter :: ID_COL_SURFTYPE = 4
  integer(kind=c_int), parameter :: ID_COL_RADIUS = 5
  integer(kind=c_int), parameter :: ID_COL_RADIUS_PICKUP = 6
  integer(kind=c_int), parameter :: ID_COL_THICKNESS = 7
  integer(kind=c_int), parameter :: ID_COL_THIC_PICKUP = 8
  integer(kind=c_int), parameter :: ID_COL_GLASS = 9
  integer(kind=c_int), parameter :: ID_COL_CLAP = 10
  integer(kind=c_int), parameter :: ID_COL_INDEX = 11



  integer(kind=c_int), parameter :: ID_COL_ASPH_A = 4
  integer(kind=c_int), parameter :: ID_COL_ASPH_B = 5
  integer(kind=c_int), parameter :: ID_COL_ASPH_C = 6
  integer(kind=c_int), parameter :: ID_COL_ASPH_D = 7
  integer(kind=c_int), parameter :: ID_COL_ASPH_E = 8
  integer(kind=c_int), parameter :: ID_COL_ASPH_F = 9
  integer(kind=c_int), parameter :: ID_COL_ASPH_G = 10
  integer(kind=c_int), parameter :: ID_COL_ASPH_H = 11
  integer(kind=c_int), parameter :: ID_COL_ASPH_I = 12


  ! FOr each tab, data structure to store values
  !integer, parameter :: ncols = 11
  type(lens_edit_col) :: asphereTypes(11)  


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

  subroutine lens_editor_basic_dialog(box1)

    use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char
    implicit none

    type(c_ptr), intent(inout) :: box1

    character(len=35) :: line
    integer(kind=c_int) :: i, ltr, j


    call buildBasicTable(.TRUE.)

    !call loadLensData()

    box1 = hl_gtk_box_new()
    ! It is the scrollcontainer that is placed into the box.
    call gtk_widget_set_vexpand (ihscrollcontain, FALSE)

    call hl_gtk_box_pack(box1, ihscrollcontain)

    PRINT *, "PACKING FIRST CONTAINER!"


    ! Delete selected row
    ibut = hl_gtk_button_new("Insert row"//c_null_char, &
         & clicked=c_funloc(ins_row), &
         & tooltip="Insert new row above"//c_null_char, sensitive=FALSE)

    call hl_gtk_box_pack(box1, ibut)

    ! Delete selected row
    dbut = hl_gtk_button_new("Delete selected row"//c_null_char, &
         & clicked=c_funloc(del_row), &
         & tooltip="Delete the selected row"//c_null_char, sensitive=FALSE)

    call hl_gtk_box_pack(box1, dbut)

    ! Also a quit button
    qbut = hl_gtk_button_new("Quit"//c_null_char, clicked=c_funloc(lens_editor_destroy), data=lens_editor_window)
    call hl_gtk_box_pack(box1,qbut)

    PRINT *, "END OF LENS EDITOR DIALOG SUB"

  end subroutine lens_editor_basic_dialog


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

        !PRINT *, "LENS EDITOR WINDOW PTR IS ", lens_editor_window

    !call gtk_window_set_modal(di, TRUE)
    !title = "Lens Draw Window"
    !if (present(title)) call gtk_window_set_title(dialog, title)
    call gtk_window_set_title(lens_editor_window, "Lens Editor Window"//c_null_char)
    !if (present(wsize)) then
    !   call gtk_window_set_default_size(dialog, wsize(1),&
    !        & wsize(2))
    !else

    width = 700
    height = 400
       call gtk_window_set_default_size(lens_editor_window, width, height)
    !end if

    !if (present(parent)) then
       call gtk_window_set_transient_for(lens_editor_window, parent_window)
       call gtk_window_set_destroy_with_parent(lens_editor_window, TRUE)
    !end if


    !call lens_editor_settings_dialog_2(box1)
    call lens_editor_basic_dialog(box1)


    call lens_editor_asphere_dialog(boxAsphere)

    boxSolve = lens_editor_add_dialog(ID_EDIT_SOLVE)


    !call lens_editor_aperture(boxAperture)

    !call gtk_scrolled_window_set_child(scrolled_tab, self%box1)
    !location = gtk_notebook_append_page(self%notebook, scrolled_tab, self%tab_label)
    nbk = gtk_notebook_new()
    basicLabel = gtk_label_new_with_mnemonic("_Basic"//c_null_char)
    pageIdx = gtk_notebook_append_page(nbk, box1, basicLabel)

    AsphLabel = gtk_label_new_with_mnemonic("_Asphere"//c_null_char)
    pageIdx = gtk_notebook_append_page(nbk, boxAsphere, AsphLabel)

    SolveLabel = gtk_label_new_with_mnemonic("_Solves"//c_null_char)
    pageIdx = gtk_notebook_append_page(nbk, boxSolve, SolveLabel)

    PRINT *, "FINISHED WITH LENS EDITOR"
    !call gtk_box_append(box1, rf_cairo_drawing_area)
    !call gtk_window_set_child(lens_editor_window, rf_cairo_drawing_area)
    call gtk_window_set_child(lens_editor_window, nbk)


    call g_signal_connect(lens_editor_window, "destroy"//c_null_char, c_funloc(lens_editor_destroy), lens_editor_window)


    call gtk_window_set_mnemonics_visible (lens_editor_window, TRUE)
    !call gtk_widget_queue_draw(my_drawing_area)
    call gtk_widget_show(lens_editor_window)


    PRINT *, "SHOULD SEE GRAPHICS NOW!"
end subroutine lens_editor_new

subroutine lens_editor_replot

  character PART1*5, PART2*5, AJ*3, A6*3, AW1*23, AW2*23
  character(len=100) :: ftext

!

end subroutine lens_editor_replot

 subroutine ins_row(but, gdata) bind(c)
    type(c_ptr), value, intent(in) :: but, gdata
    integer(kind=c_int), dimension(:), allocatable :: iset
    integer(kind=c_int), dimension(:), allocatable :: dep
    integer(kind=c_int) :: nsel
    character(len=40) :: kdptext

    nsel = hl_gtk_listn_get_selections(ihlist, iset)

    if (nsel /= 1) then
       print *, "Not a single selection"
       return
    end if

    PRINT *, "Insert Method!"

    !call hl_gtk_tree_rem(ihlist, selections(:dep(1),1))

    call PROCESKDP('U L')
    WRITE(kdptext,*) 'CHG,', iset(1)
        call PROCESKDP(kdptext)
        call PROCESKDP('INSK')
        call PROCESKDP('EOS')
        call PROCESKDP('OUT TP')

! C               CALL PROCES
!                 INPUT='U L'
!                 CALL PROCES
!                 WRITE(INPUT,*) 'CHG,',ISET
!                 CALL PROCES
!                 INPUT='INS'
!                 CALL PROCES
!                 INPUT='EOS'
!                 CALL PROCES
!                 INPUT='OUT TP'
!                 CALL PROCES


    !call hl_gtk_tree_ins(ihlist, absrow=iset(1,1))

    call gtk_widget_set_sensitive(but, FALSE)

    call refreshLensDataStruct() ! Is this really needed?  Does 'EOS' do this for free?
    call buildBasicTable(.FALSE.)
    !call loadLensData()
    call zoatabMgr%rePlotIfNeeded()


  end subroutine

 function ITOC(intVal) result(output)
   integer :: intVal
   character(len=100) :: output
   character(len=80) :: B
   !PRINT *, "val is ", intVal
   write(output, *) intVal

   !output = 'C'
   !PRINT *, "intVal is", intVal
   !WRITE(B,'(I2)') intVal
   !READ(B,'(A2)') output
   !write(output, '(I2)') intVal
   !PRINT *, "output is ", output

 end function


 subroutine del_row(but, gdata) bind(c)
    type(c_ptr), value, intent(in) :: but, gdata
    integer(kind=c_int), dimension(:), allocatable :: iset
    integer(kind=c_int), dimension(:), allocatable :: dep
    integer(kind=c_int) :: nsel
    integer(kind=c_int) :: lastSurface
    character(len=40) :: kdptext

    nsel = hl_gtk_listn_get_selections(ihlist, iset)

    if (nsel /= 1) then
       print *, "Not a single selection"
       print *, "end selection is ", iset(nsel)
       !return
    end if

    PRINT *, "iset = ", iset(1)
    call getOpticalSystemLastSurface(lastSurface)


        IF(iset(1) == 0) THEN
            call updateTerminalLog('OBJECT SURFACE MAY NOT BE DELETED', "black")
            return
        END IF
        IF(iset(1) == lastSurface) THEN
          call updateTerminalLog('IMAGE SURFACE MAY NOT BE DELETED', "black")
           return
        END IF
        ! IF(INT(SYSTEM(20)).LE.3) THEN
        !    WRITE(OUTLYNE,*) 'LENS IS OF MINIMUM SIZE, NO MORE SURFACES MAY BE DELETED'
        !    CALL SHOWIT(1)
        !    return
        ! END IF

        call PROCESKDP('OUT NULL')
        call PROCESKDP('U L')
        !WRITE(kdptext,*) 'CHG,', iset(1,1)

        !call PROCESKDP(kdptext)
        !PRINT *, "ITOC TEST "
        !PRINT *, "ITOC TEST ", ITOC(INT(1))
        !PRINT *, 'DEL, '//ITOC(iset(1,1))
        if (nsel.EQ.1) then
           PRINT *, 'DELK, '//ITOC(iset(1))
           call PROCESKDP('DELK, '//ITOC(iset(1)))
        else
          call PROCESKDP('DELK, '//ITOC(iset(1))//','//ITOC(iset(nsel)))
        end if
        !call PROCESKDP('DELK')
        call PROCESKDP('EOS')
        !call PROCESKDP('OUT TP')


    call hl_gtk_listn_rem(ihlist, iset(1))

    call gtk_widget_set_sensitive(but, FALSE)

    call refreshLensDataStruct()
    call buildBasicTable(.FALSE.)
    call buildAsphereTable(.FALSE.)
    call buildSolveTable(.FALSE.)

    !call loadLensData()
    PRINT *, "About to try replot"
    call zoatabMgr%rePlotIfNeeded()
    PRINT *, "Done with replot"

  end subroutine del_row


  subroutine lens_edited(renderer, path, text, gdata) bind(c)

    use glass_manager, only: parseModelGlassEntry, findCatalogNameFromGlassName
    use type_utils, only: real2str, int2str
    !type(c_ptr), value, intent(in) :: list, gdata

    type(c_ptr), value :: renderer, path, text, gdata
    character(len=40) :: kdptext
    character(len=13) :: catalogName
    integer(kind=c_int) :: ID_SETTING

    character(len=200) :: fpath, ftext
    integer(kind=c_int) :: irow
    integer(kind=c_int), pointer :: icol
    integer :: i, n
    type(c_ptr) :: tree, pcol

!! Temp
    character(len=7) :: rstring
    real(kind=c_double) :: dval
    type(gvalue), target :: svalue
    type(c_ptr) :: val_ptr
    real*8 :: nd, vd
    




    PRINT *, "CALLING LENS EDITED PROC!"

    call convert_c_string(path, fpath)
    pcol = g_object_get_data(renderer, "column-number"//c_null_char)
    call c_f_pointer(pcol, icol)
    call convert_c_string(text, ftext)

    !call getRowAndColFromCallback(renderer, path, row, col)


    n = 0
    do i = 1, len_trim(fpath)
       if (fpath(i:i) == ":") then
          n = n+1
          fpath(i:i) = ' '   ! : is not a separator for a Fortran read
       end if
    end do
    !allocate(irow(n+1))
    read(fpath, *) irow
    tree = g_object_get_data(renderer, "view"//c_null_char)

    call hl_gtk_listn_set_cell(tree, irow, icol, &
         & svalue=trim(ftext))

    PRINT *, "New value is ", trim(ftext)
    PRINT *, "Selected Row is ", irow

    ! Try to update lens system
    !select case (irow(1))
    select case (icol+1) ! start with index 1 to align with lens_edit_col arrays
  case (ID_COL_RADIUS)
        !call PROCESKDP('RDY S'//trim(int2str(irow))//' '//trim(real2str(trim(ftext))))
        call PROCESKDP('U L')
        WRITE(kdptext, *) 'CHG ' ,irow
        call PROCESKDP(kdptext)
        call PROCESKDP("RD "//trim(ftext)//"; EOS")
        !call PROCESKDP('EOS')
  case (ID_COL_RADIUS_PICKUP)
       call hl_gtk_combo_set_by_text(ihlist, irow, icol, trim(ftext), ID_SETTING)
      
      select case (ID_SETTING)

        case (ID_MOD_PICKUP)
           call ui_pickup(irow, ID_PICKUP_RAD)
        case (ID_MOD_NONE)
          pData%ID_type = ID_PICKUP_RAD
          pData%surf = irow
          call pData%setPickupText()
          CALL PROCESKDP('U L')
          CALL PROCESKDP('CHG, '//int2str(irow))
          call PROCESKDP(pData%genKDPCMDToRemovePickup())
          CALL PROCESKDP('EOS')

          call refreshLensEditorUI()
        case (ID_MOD_SOLVE)
          call ui_solve(irow, ID_PICKUP_RAD) 
        end select 

      case (ID_COL_CLAP)
        CALL PROCESKDP('CIR S'//trim(int2str(irow))//' '//trim(ftext))
        call refreshLensEditorUI()
        !call PROCESKDP('U L')
        !WRITE(kdptext, *) 'CHG ' ,irow
        !call PROCESKDP(kdptext)
        !call PROCESKDP("CLAP "//trim(ftext))
        !call PROCESKDP('EOS')        

    case (ID_COL_THICKNESS)
        PRINT *, "Thickness changed!"
        call PROCESKDP('U L')
        WRITE(kdptext, *) 'CHG ' ,irow
        call PROCESKDP(kdptext)
        call PROCESKDP("TH "//trim(ftext))
        call PROCESKDP('EOS')

    case (ID_COL_THIC_PICKUP)
          ! TODO:  Abstract this into a sub since it is essentially identical to _RAD
          call hl_gtk_combo_set_by_text(ihlist, irow, icol, trim(ftext), ID_SETTING)
         PRINT *, "ftext is ", trim(ftext)
         select case (ID_SETTING)
   
           case (ID_MOD_PICKUP)
              call ui_pickup(irow, ID_PICKUP_THIC)
           case (ID_MOD_NONE)
             pData%ID_type = ID_PICKUP_THIC
             pData%surf = irow
             call pData%setPickupText()
             CALL PROCESKDP('U L')
             CALL PROCESKDP('CHG, '//int2str(irow))
             call PROCESKDP(pData%genKDPCMDToRemovePickup())
             call PROCESKDP(sData%genKDPCMDToRemoveSolve(irow))
             CALL PROCESKDP('EOS')
   
             call refreshLensEditorUI()
           case (ID_MOD_SOLVE)
            PRINT *, "irow is", irow
             call ui_solve(irow, ID_PICKUP_THIC)   
           end select       

           ! Remove pickup
      

      ! if (ftext(1:1).NE."P".AND.ftext(1:1).NE." ") then
      !   ! Set it to blank
      !   call hl_gtk_listn_set_cell(tree, irow, icol, &
      !   & svalue=" ")       
      ! else
      !    call ui_pickup(irow, ID_PICKUP_RAD)
      ! end if 



      case (ID_COL_GLASS)
        if (len(trim(ftext)).EQ.8.AND.ftext(5:5).EQ.'.') then
          PRINT *, "Model Glass Entered!"
          call parseModelGlassEntry(trim(ftext), nd, vd)
          print *, "nd is ", real2str(nd)
          print *, "vd is ", vd
          call PROCESKDP('U L')
          WRITE(kdptext, *) 'CHG ' ,irow
          call PROCESKDP(kdptext)
          call PROCESKDP('MODEL D'//trim(ftext)//','//real2str(nd)//','//real2str(vd))
          call PROCESKDP('EOS')
        else
        call findCatalogNameFromGlassName(ftext, catalogName)
        PRINT *, "Glass entry request is ", ftext
        if (ftext(1:1) /= ' ') then
        call PROCESKDP('U L')
        WRITE(kdptext, *) 'CHG ' ,irow
        call PROCESKDP(kdptext)
        call PROCESKDP(catalogName//' '//ftext)
        call PROCESKDP('EOS')
        end if
        end if



  end select


    call zoatabMgr%rePlotIfNeeded()
        ! INPUT='U L'
        ! CALL PROCES
        ! WRITE(INPUT,*) 'CHG ',J
        ! CALL PROCES
        ! WRITE(INPUT,*) 'RD ',TEMPCV
        ! CALL PROCES
        ! INPUT='EOS'
        ! CALL PROCES

    !deallocate(irow)




  end subroutine

  subroutine refreshLensEditorUI()

    implicit none

    call refreshLensDataStruct()
    call buildBasicTable(.FALSE.)
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

! This sub is a mess.
! WHy?  I cannot seem to figure out how to go from whater object is
! passed to this callback to get to the model I created for the combo box
! So there are lots of fits and starts here
! To get around this, I wrote a fcn that takes the text (which I can get)
! To set it and get the ID, from a global variable I created when I created 
! the object.  There must be a better way, but I can't find it...
 ! By elimination, the widget is not
 ! gtk_tree_view
 ! gtk_tree_view_column
 ! TODO:  I think I may have found part of the problem.  I may not have been setting the
!  model properly.  I modified the attach method to set_data instead of just set_property
 ! and now I see it.  But this does not tell me what value is currently set, so I'm not 
 ! sure it helps me.       
 ! 
subroutine asph_edited(renderer, path, text, gdata) bind(c)

  type(c_ptr), value :: renderer, path, text, gdata
    character(len=200) :: fpath, ftext
    character(len=40) :: kdptext
    integer(kind=c_int), allocatable, dimension(:) :: irow
    integer(kind=c_int), pointer :: icol
    integer :: i, n
    type(c_ptr) :: tree, pcol, treeCol, widget, model
    real :: fVal
    integer :: ios
    integer :: ID_SETTING
    
    !integer(kind=c_int), pointer :: ID_SETTING
    type(c_ptr) :: col, rlist, rend2
    type(gtktreeiter), target :: tree_iter
    integer :: boolResult

    !call c_f_pointer(gdata, ID_SETTING)
    PRINT *, "CALLING ASPHERE EDITED PROC!"
    PRINT *, "widget ptr is ", LOC(renderer)
    !PRINT *, "ID_SETTING IS ", ID_SETTING

    call convert_c_string(path, fpath)
    pcol = g_object_get_data(renderer, "column-number"//c_null_char)
    call c_f_pointer(pcol, icol)
    call convert_c_string(text, ftext)

    PRINT *, "ftext is ", ftext

    PRINT *, "icol is ", icol

    ! This does not work because the data is not stored in a GtkListStore object
    model = g_object_get_data(renderer, "model"//c_null_char)
    PRINT *, "model is ", loc(model)    
    PRINT *, "before error?"
    boolResult = gtk_tree_model_get_iter_first(model, c_loc(tree_iter))

    PRINT *, "boolResult is ", boolResult



     !tree = g_object_get_data(renderer, "model"//c_null_char)
     !print *, "tree is ", LOC(tree)
    ! 
    ! widget = gtk_tree_view_column_get_widget(treeCol)

    if (icol == 1) then

            ! Find the renderer for the column
      !col = gtk_tree_view_get_column(renderer, icol)
      !rlist = gtk_cell_layout_get_cells(col)
      !rend2= g_list_nth_data(rlist, 0_c_int)
      !call g_list_free(rlist)

        !ID_SETTING = hl_zoa_combo_get_list2id_by_text(rend2, trim(ftext))

        !!!ID_SETTING = hl_zoa_combo_get_selected_list2_id(renderer)

       !PRINT *, "ID_SETTING is ", ID_SETTING
     end if
    !PRINT *, "IOS is ", ios

    
    read( ftext, *, iostat=ios)  fVal

    PRINT *, "ios is ", ios



    n = 0
    do i = 1, len_trim(fpath)
       if (fpath(i:i) == ":") then
          n = n+1
          fpath(i:i) = ' '   ! : is not a separator for a Fortran read
       end if
    end do
    print *, "n = ", n
    allocate(irow(n+1))
    read(fpath, *) irow
    PRINT *, "Selected Row is ", irow

    if (icol == 1) then
      PRINT *, "Set by text"
      call hl_gtk_combo_set_by_text(ihAsph, irow(1), icol, trim(ftext), ID_SETTING)
      PRINT *, "ID_SETTING is ", ID_SETTING
    end if



    if (ios == 0)  THEN !Valid number

      call updateAsphereCoefficient(irow(1),INT(icol),ftext)


      ! call PROCESKDP('U L')
      ! WRITE(kdptext, *) 'CHG ' ,irow
      ! PRINT *, "kdptext is ", kdptext
      ! call PROCESKDP(kdptext)
      !   !IF(ALENS(8,J).EQ.0.0D0) THEN
      ! call PROCESKDP('ASPH')
      ! call PROCESKDP('AD '//trim(ftext))
      ! call PROCESKDP('EOS')
      ! call buildAsphereTable(.FALSE.)
      ! call zoatabMgr%rePlotIfNeeded()

    end if

    !PRINT *, "fVal is ", fVal


end subroutine

subroutine updateAsphereCoefficient(irow,icol,ftext)

    integer(kind=c_int) :: irow, icol
    character(len=*) :: ftext
    character(len=1) :: asphVar
    character(len=40) :: kdptext

    PRINT *, "irow is ", irow
    PRINT *, "icol is ", icol
    PRINT *, "ftext is ", ftext

    select case (icol+1)

    case (ID_COL_ASPH_A)
      asphVar = 'D'
    case (ID_COL_ASPH_B)
      asphVar = 'E'
    case (ID_COL_ASPH_C)
      asphVar = 'F'
    case (ID_COL_ASPH_D)
      asphVar = 'G'
    case (ID_COL_ASPH_E)
      asphVar = 'H'
    case (ID_COL_ASPH_F)
      asphVar = 'I'
    case (ID_COL_ASPH_G)
      asphVar = 'J'
    case (ID_COL_ASPH_H)
      asphVar = 'K'
    case (ID_COL_ASPH_I)
      asphVar = 'L'

    end select


      call PROCESKDP('U L')
      WRITE(kdptext, *) 'CHG ' ,irow
      PRINT *, "kdptext is ", kdptext
      call PROCESKDP(kdptext)
        !IF(ALENS(8,J).EQ.0.0D0) THEN
      call PROCESKDP('ASPH')
      call PROCESKDP('A'//asphVar//' '//trim(ftext))
      call PROCESKDP('EOS')
      
      call buildAsphereTable(.FALSE.)
      call zoatabMgr%rePlotIfNeeded()


end subroutine

function genPickupArr(ID_PICKUP_TYPE) result(pickupArr)

  integer, dimension(curr_lens_data%num_surfaces) :: pickupArr
  integer :: ID_PICKUP_TYPE
  integer :: i
  !PRINT *, "About to set pickupArr"
  !if (ID_PICKUP_TYPE == ID_PICKUP_RAD) PRINT *, "Size of array is ", size(pickupArr)


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


  end do

end function

function genSurfaceTypes() result(typeStr)

  character(len=10), dimension(curr_lens_data%num_surfaces) :: typeStr
  integer :: i

  do i=1,curr_lens_data%num_surfaces
    typeStr(i) = "Sphere"
  end do



end function

function buildLensEditTable() result(store)

  logical :: firstTime
  integer(kind=c_int), PARAMETER :: ncols = 11
  character(len=10), dimension(ncols) :: colModel
  type(lens_edit_col), dimension(ncols) :: basicTypes
  integer(kind=type_kind), dimension(ncols) :: ctypes
  character(len=20), dimension(ncols) :: titles
  integer(kind=c_int), dimension(ncols) :: sortable, editable
  integer, allocatable, dimension(:) :: surfIdx
  integer, allocatable, dimension(:) :: isRefSurface
  real(kind=real64), dimension(curr_lens_data%num_surfaces) :: clearApertures

  integer, parameter :: numModTypes = 3
  character(kind=c_char, len=20),dimension(numModTypes) :: valsArray
  integer(c_int), dimension(numModTypes) :: refsArray
  integer :: i



  integer, parameter :: numSurfTypes = 1
  character(kind=c_char, len=20),dimension(numSurfTypes) :: surfTypeNames
  integer(c_int), dimension(numSurfTypes) :: surfTypeIDs

  type(c_ptr) :: boxNew, store


  

    allocate(surfIdx(curr_lens_data%num_surfaces))
    surfIdx =  (/ (i,i=0,curr_lens_data%num_surfaces-1)/)

    allocate(isRefSurface(curr_lens_data%num_surfaces))
    isRefSurface = 0*isRefSurface
    isRefSurface(curr_lens_data%ref_stop) = 1

    clearApertures = curr_lens_data%clearAps(1:curr_lens_data%num_surfaces)%yRad   

    boxNew = hl_gtk_box_new()
    store = g_list_store_new(G_TYPE_OBJECT)
    do i=1,curr_lens_data%num_surfaces
    store = append_lens_model(store, surfIdx(i), isRefSurface(i), ""//c_null_char, "Sphere"//c_null_char, &
    & real(curr_lens_data%radii(i),8), ID_MOD_NONE, real(curr_lens_data%thicknesses(i),8), ID_MOD_NONE, &
    & trim(curr_lens_data%glassnames(i))//c_null_char, clearApertures(i), real(curr_lens_data%surf_index(i),8))
    end do



  end function

subroutine buildBasicTable(firstTime)

    logical :: firstTime
    integer(kind=c_int), PARAMETER :: ncols = 11
    character(len=10), dimension(ncols) :: colModel
    type(lens_edit_col), dimension(ncols) :: basicTypes
    integer(kind=type_kind), dimension(ncols) :: ctypes
    character(len=20), dimension(ncols) :: titles
    integer(kind=c_int), dimension(ncols) :: sortable, editable
    integer, allocatable, dimension(:) :: surfIdx
    integer, allocatable, dimension(:) :: isRefSurface
    real(kind=real64), dimension(curr_lens_data%num_surfaces) :: clearApertures

    integer, parameter :: numModTypes = 3
    character(kind=c_char, len=20),dimension(numModTypes) :: valsArray
    integer(c_int), dimension(numModTypes) :: refsArray
    integer :: i

    integer, parameter :: numSurfTypes = 1
    character(kind=c_char, len=20),dimension(numSurfTypes) :: surfTypeNames
    integer(c_int), dimension(numSurfTypes) :: surfTypeIDs


  
    ! Notes:
    !  Need column titles to be a function of row
    ! Each time a row is selected, update column names
    valsArray(1) = " "
    valsArray(2) = "P"
    valsArray(3) = "S"

    refsArray(1) = ID_MOD_NONE
    refsArray(2) = ID_MOD_PICKUP
    refsArray(3) = ID_MOD_SOLVE
    
    surfTypeNames(1) = "Sphere"
    surfTypeIDs(1) = ID_SURF_SPHERE

   
    PRINT *, "Starting Basic Table Proc"

    allocate(surfIdx(curr_lens_data%num_surfaces))
    surfIdx =  (/ (i,i=0,curr_lens_data%num_surfaces-1)/)

    allocate(isRefSurface(curr_lens_data%num_surfaces))
    isRefSurface = 0*isRefSurface
    isRefSurface(curr_lens_data%ref_stop) = 1

    clearApertures = curr_lens_data%clearAps(1:curr_lens_data%num_surfaces)%yRad   
    PRINT *, "clear Apertures is ", clearApertures

    call basicTypes(1)%initialize("Surface"   , G_TYPE_INT,   FALSE, FALSE, surfIdx)
    call basicTypes(2)%initialize("Ref"    , G_TYPE_BOOLEAN, FALSE, TRUE, isRefSurface)
    !call basicTypes(3)%initialize("Surface Name"   , G_TYPE_STRING,   FALSE, FALSE, surfIdx)
    call basicTypes(3)%initialize("Surface Name"   , G_TYPE_STRING,   FALSE, FALSE, &
    & getSurfaceNames(), numRows=curr_lens_data%num_surfaces)

    call basicTypes(ID_COL_SURFTYPE)%initialize("Surface Type"   , G_TYPE_STRING,   FALSE, TRUE, genSurfaceTypes(), &
    & numRows=numSurfTypes, refsArray=surfTypeIDs, valsArray=surfTypeNames)
    call basicTypes(ID_COL_RADIUS)%initialize("Radius"    , G_TYPE_FLOAT, FALSE, TRUE, curr_lens_data%radii )
    call basicTypes(ID_COL_RADIUS_PICKUP)%initialize(" ", G_TYPE_STRING, FALSE, TRUE, genPickupArr(ID_PICKUP_RAD), &
    & numRows=numModTypes, refsArray=refsArray, valsArray=valsArray )

    !PRINT *, "just to confirm, for RAD pickupArr is ", genPickupArr(ID_PICKUP_RAD)
    
    !call basicTypes(4)%initialize("S", G_TYPE_STRING, FALSE, TRUE, tmpArray, &
    !&numRows=numModTypes , refsArray=refsArray, valsArray=valsArray)    

    call basicTypes(ID_COL_THICKNESS)%initialize("Thickness" , G_TYPE_FLOAT, FALSE, TRUE, curr_lens_data%thicknesses )
    call basicTypes(ID_COL_THIC_PICKUP)%initialize(" ", G_TYPE_STRING, FALSE, TRUE, genPickupArr(ID_PICKUP_THIC), &
    & numRows=numModTypes, refsArray=refsArray, valsArray=valsArray )  
   
    call basicTypes(ID_COL_GLASS)%initialize("Glass"     , G_TYPE_STRING, FALSE, TRUE, &
    & curr_lens_data%glassnames, curr_lens_data%num_surfaces )
    !call basicTypes(ID_COL_CLAP)%initialize("Aperture" , G_TYPE_FLOAT, FALSE, TRUE, &
    !& curr_lens_data%clearapertures) 
    call basicTypes(ID_COL_CLAP)%initialize("Aperture" , G_TYPE_FLOAT, FALSE, TRUE, &
    & REAL(clearApertures))     
    !PRINT *, "TST is ", curr_lens_data%clearAps(1:6)%yRad   
    call basicTypes(ID_COL_INDEX)%initialize("Index"     , G_TYPE_FLOAT, FALSE, FALSE, curr_lens_data%surf_index )

    ! Notes for adding aperture
    ! Start with circular aperture
    ! Looks like it is stored here ALENS(10,EDIT_SURFACE)
    ! Will need to call SETCLAP first as it seems like the default value is 0
    ! Whether this is a good idea is TBD
    ! Also would like to figure out CodeV command for this and get it supported

    colModel(1) = 'text'
    colModel(2) = 'radio'
    colModel(3) = 'text'
    colModel(4) = 'text'
    colModel(ID_COL_RADIUS) = 'text'
    colModel(ID_COL_RADIUS_PICKUP) = 'combo'
    !colModel(4) = 'text'
    colModel(ID_COL_THICKNESS) = 'text'
    colModel(ID_COL_THIC_PICKUP) = 'combo'
    colModel(ID_COL_GLASS) = 'text'
    colModel(ID_COL_CLAP) = 'text' 
    colModel(ID_COL_INDEX) = 'text'

    ! gathering step to be compabible with hl_gtk interface
    do i=1,ncols
      ctypes(i) = basicTypes(i)%coltype
      sortable(i) = basicTypes(i)%sortable
      editable(i) = basicTypes(i)%editable
      titles(i) = basicTypes(i)%coltitle

    end do

    if (firstTime) then
    ihlist = hl_gtk_listn_new(ihscrollcontain, types=ctypes, &
         & changed=c_funloc(list_select),&
         & edited=c_funloc(lens_edited),&
         &  multiple=TRUE, height=250_c_int, swidth=400_c_int, titles=titles, &
         & sortable=sortable, editable=editable, renderers=colModel, &
         & toggled_radio=c_funloc(refstop_clicked))

   !PRINT *, "ihlist created!  ", ihlist
    end if

    !call buildTree(ihList, basicTypes)
    call hl_gtk_listn_attach_combo_box_model(ihlist, (ID_COL_SURFTYPE-1), surfTypeNames, surfTypeIDs)
    call hl_gtk_listn_attach_combo_box_model(ihlist, (ID_COL_RADIUS_PICKUP-1), valsArray, refsArray)
    call hl_gtk_listn_attach_combo_box_model(ihlist, (ID_COL_THIC_PICKUP-1), valsArray, refsArray)


    ! Now put 10 top level rows into it
    PRINT *, "About to populate lens edit basic table"
    call populatelensedittable(ihlist, basicTypes,ncols)
    call set_listn_column_color(ihlist, 0_c_int, "orange")
end subroutine

subroutine buildAsphereTable(firstTime)

    logical :: firstTime
    integer(kind=type_kind), dimension(size(asphereTypes)) :: ctypes
    character(len=20), dimension(size(asphereTypes)) :: titles
    integer(kind=c_int), dimension(size(asphereTypes)) :: sortable, editable
    integer, allocatable, dimension(:) :: surfIdx
    character(len=10), dimension(size(asphereTypes)) :: colModel
    integer :: i
    integer, parameter :: numAsphTypes = 3
    character(kind=c_char, len=20),dimension(numAsphTypes) :: valsArray
    integer(c_int), dimension(numAsphTypes) :: refsArray
    type(c_ptr) :: colTmp

    integer, dimension(curr_lens_data%num_surfaces) :: defaultAsphereType


    allocate(surfIdx(curr_lens_data%num_surfaces))
    surfIdx =  (/ (i,i=0,curr_lens_data%num_surfaces-1)/)

    defaultAsphereType = (/ (ID_EDIT_ASPH_NONTORIC, i=1,size(defaultAsphereType))/)



    valsArray(1) = "Non-Toric"
    valsArray(2) = "Toric Parallel to Y"
    valsArray(3) = "Toric Parallel to X"

    refsArray(1) = ID_EDIT_ASPH_NONTORIC
    refsArray(2) = ID_EDIT_ASPH_TORIC_Y
    refsArray(3) = ID_EDIT_ASPH_TORIC_X




    PRINT *, "LENS EDITOR Asphere DIALOG SUB STARTING!"
    ! Now make a multi column list with multiple selections enabled
    call asphereTypes(1)%initialize("Surface", G_TYPE_INT, FALSE, FALSE, surfIdx)
    call asphereTypes(2)%initialize("Type", G_TYPE_STRING, FALSE, TRUE, defaultAsphereType, &
    & numRows=numAsphTypes , refsArray=refsArray, valsArray=valsArray)
    !call asphereTypes(2)%initialize("Type", G_TYPE_STRING, FALSE, TRUE, data=valsArray, numRows=numAsphTypes , refsArray=refsArray)
    call asphereTypes(3)%initialize("Conic Constant", G_TYPE_FLOAT, FALSE, TRUE, curr_asph_data%conic_constant)
    call asphereTypes(ID_COL_ASPH_A)%initialize("A (h^4)", G_TYPE_STRING, FALSE, TRUE, &
    & curr_asph_data%asphereTerms(:,1), dtype=DTYPE_SCIENTIFIC)
    call asphereTypes(ID_COL_ASPH_B)%initialize("B (h^6)", G_TYPE_STRING, FALSE, TRUE, &
    & curr_asph_data%asphereTerms(:,2), dtype=DTYPE_SCIENTIFIC)
    call asphereTypes(ID_COL_ASPH_C)%initialize("C (h^8)", G_TYPE_STRING, FALSE, TRUE, &
    & curr_asph_data%asphereTerms(:,3), dtype=DTYPE_SCIENTIFIC)
    call asphereTypes(ID_COL_ASPH_D)%initialize("D (h^10)", G_TYPE_STRING, FALSE, TRUE, &
    & curr_asph_data%asphereTerms(:,4), dtype=DTYPE_SCIENTIFIC)
    call asphereTypes(ID_COL_ASPH_E)%initialize("E (h^12)", G_TYPE_STRING, FALSE, TRUE, &
    & curr_asph_data%asphereTerms(:,5), dtype=DTYPE_SCIENTIFIC)
    call asphereTypes(ID_COL_ASPH_F)%initialize("F (h^14)", G_TYPE_STRING, FALSE, TRUE, &
    & curr_asph_data%asphereTerms(:,6), dtype=DTYPE_SCIENTIFIC)
    call asphereTypes(ID_COL_ASPH_G)%initialize("G (h^16)", G_TYPE_STRING, FALSE, TRUE, &
    & curr_asph_data%asphereTerms(:,7), dtype=DTYPE_SCIENTIFIC)
    call asphereTypes(ID_COL_ASPH_H)%initialize("H (h^18)", G_TYPE_STRING, FALSE, TRUE, &
    & curr_asph_data%asphereTerms(:,8), dtype=DTYPE_SCIENTIFIC)

    colModel(1) = 'text'
    colModel(2) = 'combo'
    colModel(3) = 'text'
    colModel(4) = 'text'
    colModel(5) = 'text'
    colModel(6) = 'text'
    colModel(7) = 'text'
    colModel(8) = 'text'
    colModel(9) = 'text'
    colModel(10) = 'text'
    colModel(11) = 'text'

    !asphereTypes(4)%dtype = DTYPE_SCIENTIFIC
    !asphereTypes(4)%coltype = G_TYPE_STRING



    do i=1,size(asphereTypes)
      ctypes(i) = asphereTypes(i)%coltype
      sortable(i) = asphereTypes(i)%sortable
      editable(i) = asphereTypes(i)%editable
      titles(i) = asphereTypes(i)%coltitle
    end do

    !PRINT *, "About to define ihAsph"
    !   PRINT *, "buildAsphere valsArray is ", valsArray
    !   PRINT *, "buildAsphere refsArray is ", refsArray
    if (firstTime) then
    ihAsph = hl_gtk_listn_new(scroll=ihScrollAsph, types=ctypes, &
         & changed=c_funloc(list_select),&
         & edited=c_funloc(asph_edited),&
         &  multiple=FALSE, height=250_c_int, swidth=400_c_int, titles=titles, &
         & sortable=sortable, editable=editable, renderers=colModel) !, &
         !& valsArray=valsArray, refsArray=refsArray)
         call hl_gtk_listn_attach_combo_box_model(ihAsph, 1_c_int, valsArray, refsArray)
        end if



    !PRINT *, "ihlist created!  ", ihlist
    !colTmp = gtk_tree_view_get_column(ihAsph, 3_c_int)
    !call gtk_column_view_column_set_fixed_width(colTmp, 0_c_int)
    !call gtk_tree_view_column_set_max_width(colTmp, 0_c_int)
    ! Did not test code of this,but in debug window this works.
    !call gtk_tree_view_column_set_visible(colTmp, 0_c_int)

    ! Now put 10 top level rows into it
    PRINT *, "About to Populate UI Table"
    call populatelensedittable(ihAsph, asphereTypes, size(asphereTypes))
    PRINT *, "Done Populating UI Table"
    !call hl_gtk_box_pack(ihScrollAsph, ihAsph)
    !call loadAphereDataIntoTable()
    !call loadLensData()

end subroutine

subroutine buildSolveTable(firstTime)

  logical :: firstTime
  integer, parameter :: ncols = 5
  type(lens_edit_col) :: solveTypes(ncols)
  integer(kind=type_kind), dimension(ncols) :: ctypes
  character(len=20), dimension(ncols) :: titles
  integer(kind=c_int), dimension(ncols) :: sortable, editable
  integer, allocatable, dimension(:) :: surfIdx
  character(len=10), dimension(ncols) :: colModel
  integer :: i
  integer, parameter :: numSolveTypes = 5
  character(kind=c_char, len=20),dimension(numSolveTypes) :: valsArray
  integer(c_int), dimension(numSolveTypes) :: refsArray
  integer, dimension(curr_lens_data%num_surfaces) :: defaultSolveType
  type(c_ptr) :: colTmp



  allocate(surfIdx(curr_lens_data%num_surfaces))
  surfIdx =  (/ (i,i=0,curr_lens_data%num_surfaces-1)/)

  defaultSolveType = (/ (ID_EDIT_SOLVE_NONE,i=1,curr_lens_data%num_surfaces)/)

  ! Notes:
  !  Need column titles to be a function of row
  ! Each time a row is selected, update column names
  valsArray(1) = "None"
  valsArray(2) = "Curvature"
  valsArray(3) = "Center of Curvature"
  valsArray(4) = "Thickness"
  valsArray(5) = "Clear Aperture"

  refsArray(1) = ID_EDIT_SOLVE_NONE
  refsArray(2) = ID_EDIT_SOLVE_CURV
  refsArray(3) = ID_EDIT_SOLVE_CENT_CURV
  refsArray(4) = ID_EDIT_SOLVE_THICK
  refsArray(5) = ID_EDIT_SOLVE_CA



  PRINT *, "LENS EDITOR Solve DIALOG SUB STARTING!"
  ! Now make a multi column list with multiple selections enabled
  call solveTypes(1)%initialize("Surface", G_TYPE_INT, FALSE, FALSE, surfIdx)
  !call solveTypes(2)%initialize("Solve", G_TYPE_STRING, FALSE, TRUE, data=valsArray, numRows=numSolveTypes , refsArray=refsArray)
  call solveTypes(2)%initialize("Solve", G_TYPE_STRING, FALSE, TRUE, defaultSolveType, &
  &numRows=numSolveTypes , refsArray=refsArray, valsArray=valsArray)
  
  call solveTypes(3)%initialize("Param 1", G_TYPE_FLOAT, FALSE, TRUE, curr_asph_data%conic_constant)
  call solveTypes(ID_COL_ASPH_A)%initialize("Param 2", G_TYPE_STRING, FALSE, TRUE, &
  & curr_asph_data%asphereTerms(:,1), dtype=DTYPE_SCIENTIFIC)
  call solveTypes(ID_COL_ASPH_B)%initialize("Param 3", G_TYPE_STRING, FALSE, TRUE, &
  & curr_asph_data%asphereTerms(:,2), dtype=DTYPE_SCIENTIFIC)

  colModel(1) = 'text'
  colModel(2) = 'combo'
  colModel(3) = 'text'
  colModel(4) = 'text'
  colModel(5) = 'text'


  !asphereTypes(4)%dtype = DTYPE_SCIENTIFIC
  !asphereTypes(4)%coltype = G_TYPE_STRING



  do i=1,ncols
    ctypes(i) = solveTypes(i)%coltype
    sortable(i) = solveTypes(i)%sortable
    editable(i) = solveTypes(i)%editable
    titles(i) = solveTypes(i)%coltitle
  end do

  !PRINT *, "About to define ihAsph"
  !   PRINT *, "buildAsphere valsArray is ", valsArray
  !   PRINT *, "buildAsphere refsArray is ", refsArray
  if (firstTime) then
  ihSolv = hl_gtk_listn_new(scroll=ihScrollSolv, types=ctypes, &
       & changed=c_funloc(list_select),&
       & edited=c_funloc(solv_edited),&
       &  multiple=FALSE, height=250_c_int, swidth=400_c_int, titles=titles, &
       & sortable=sortable, editable=editable, renderers=colModel) !, &

!         & valsArray=valsArray, refsArray=refsArray)
     end if

 call hl_gtk_listn_attach_combo_box_model(ihSolv, 1_c_int, valsArray, refsArray)

call gtk_tree_view_set_activate_on_single_click(ihSolv, 1_c_int) 
 call g_signal_connect(ihSolv, "row-activated"//c_null_char, c_funloc(row_selected))

  



  !PRINT *, "ihlist created!  ", ihlist
  !colTmp = gtk_tree_view_get_column(ihAsph, 3_c_int)
  !call gtk_column_view_column_set_fixed_width(colTmp, 0_c_int)
  !call gtk_tree_view_column_set_max_width(colTmp, 0_c_int)
  ! Did not test code of this,but in debug window this works.
  !call gtk_tree_view_column_set_visible(colTmp, 0_c_int)

  ! Now put 10 top level rows into it
  PRINT *, "About to Populate UI Table"
  call populatelensedittable(ihSolv, solveTypes, ncols, ID_COMBO_SOLVE)
  PRINT *, "Done Populating UI Table"
  !call hl_gtk_box_pack(ihScrollAsph, ihAsph)
  !call loadAphereDataIntoTable()
  !call loadLensData()

end subroutine

  subroutine row_selected(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata
    integer :: row, col
    integer(kind=c_int) :: nsel
    integer(kind=c_int), dimension(:), allocatable :: selections
    
    ! Probably needs to move?
    type(c_ptr) :: tree_col

    ! Vars to move to another sub when working
    type(c_ptr) :: store, cstr, sval
    type(gtktreeiter), target :: viter
    integer(kind=c_int) :: valid
    character(len=50) :: choice
    type(gvalue), target :: sresult

    
    PRINT *, "Signal activated!"
    nsel = hl_gtk_listn_get_selections(ihSolv, selections)
    PRINT *, "Selections(1) is ", selections(1)
  
   ! How to get column data
    
    ! Get list store
    store = gtk_tree_view_get_model(ihSolv)

    PRINT *, "Store is ", LOC(store)

    ! Get the iterator of the row
    call clear_gtktreeiter(viter)
    valid = gtk_tree_model_iter_nth_child(store, c_loc(viter), C_NULL_PTR, selections(1))
    if (.not. c_f_logical(valid)) return
    PRINT *, "About to get value"
    sval = c_loc(sresult)
    call gtk_tree_model_get_value(store, c_loc(viter), 1_c_int, sval)

    PRINT *, "About to get string"

    cstr = g_value_get_string(sval)
    call convert_c_string(cstr, choice)   

    PRINT *, "Choice is ", choice    

    tree_col = gtk_tree_view_get_column(ihSolv, 3_c_int)

    if (selections(1) < 5) then
      call gtk_tree_view_column_set_title(tree_col, "Tst"//c_null_char)
    else 
      call gtk_tree_view_column_set_title(tree_col, "Param1"//c_null_char)
    end if



    !// Modify a particular row
    !path = gtk_tree_path_new_from_string ("4");
    !gtk_tree_model_get_iter (GTK_TREE_MODEL (list_store),
    !                         &iter,
    !                         path);
    !gtk_tree_path_free (path);

    !gtk_tree_model_get_value (
    !  GtkTreeModel* tree_model,
    !  GtkTreeIter* iter,
    !  int column,
    !  GValue* value    



  end subroutine

  subroutine refstop_clicked(renderer, path, gdata) bind(c)
    type(c_ptr), value, intent(in) :: renderer, path, gdata


    ! Default callback for a toggle button in a list
    !
    ! RENDERER |  c_ptr |  required |  The renderer which sent the signal
    ! PATH |  c_ptr |  required |  The path at which to insert
    ! GDATA |  c_ptr |  required |  User data, Not used.
    !
    ! The column number is passed via the "column-number" gobject data value.
    ! The treeview containing the cell is passed via the "view" gobject
    ! data value.
    ! The row number is passed as a string in the PATH argument.
    ! This routine is not normally called by the application developer.
    !-
    character(len=200) :: fpath
    integer(c_int) :: irow
    integer(c_int), pointer :: icol
    integer(c_int) :: i
    type(c_ptr) :: pcol, list
    logical :: state
    integer(c_int) :: nrows
    character(len=40) :: kdptext

    call convert_c_string(path, fpath)
    read(fpath, *) irow

    pcol = g_object_get_data(renderer, "column-number"//c_null_char)
    call c_f_pointer(pcol, icol)

    list = g_object_get_data(renderer, "view"//c_null_char)

    state = c_f_logical(gtk_cell_renderer_toggle_get_active(renderer))
    print *, irow, state

    if (state) return ! Don't act on an unset

    ! Find the first iterator
    nrows = gtk_tree_model_iter_n_children (gtk_tree_view_get_model(list), &
         & c_null_ptr)
    do i = 0,nrows-1
       call hl_gtk_listn_set_cell(list, i, icol, &
            & logvalue= i == irow)
    end do
    CALL PROCESKDP('U L')
    WRITE(kdptext, *) 'CHG ' ,irow
    call PROCESKDP(kdptext)
    call PROCESKDP('ASTOP')
    call PROCESKDP('REFS')
    call PROCESKDP('EOS')


  end subroutine refstop_clicked

  subroutine list_select(list, gdata) bind(c)
    type(c_ptr), value, intent(in) :: list, gdata
    integer(kind=c_int) :: nsel
    integer(kind=c_int), dimension(:), allocatable :: selections
    integer(kind=c_int), dimension(:), allocatable :: dep
    integer(kind=c_int) :: n, n3
    integer(kind=c_int64_t) :: n4
    real(kind=c_float) :: nlog
    character(len=30) :: name
    character(len=10) :: nodd
    integer :: i
    nsel = hl_gtk_listn_get_selections(C_NULL_PTR, selections, list)
    if (nsel == 0) then
       print *, "No selection"
       return
    end if


    if (nsel == 1) then
       !call hl_gtk_listn_set_cell(ihlist, selections(1), 0_c_int, &
      !      & ivalue=n)

       call gtk_widget_set_sensitive(dbut, TRUE)
       call gtk_widget_set_sensitive(ibut, TRUE)
    else
       call gtk_widget_set_sensitive(dbut, TRUE)
       call gtk_widget_set_sensitive(ibut, FALSE)
    end if

    deallocate(selections)
  end subroutine list_select

  subroutine updateSurfaceNumbers()

    integer :: i

    do i = 1, numEditorRows

        call hl_gtk_listn_set_cell(ihlist, row=i-1_c_int, col=0_c_int, &
             & ivalue=(i-1))
        end do


  end subroutine

  subroutine populatelensedittable(ihObj, colObj, m, ID_COMBO)
    type(c_ptr) :: ihObj
    type(lens_edit_col), dimension(*) :: colObj
    character(len=23) :: AVAL
    !integer :: i,j
    integer(kind=c_int) :: i,j,m
    integer, optional :: ID_COMBO

    !m = size(colObj,DIM=1)

    call hl_gtk_listn_rem(ihObj)
    !PRINT *, "Number of entries in loop is ", curr_lens_data%num_surfaces
     do i=1,curr_lens_data%num_surfaces
        call hl_gtk_listn_ins(ihObj, count = 1_c_int)

        do j=1,m


          select case(colObj(j)%dtype)


          case(DTYPE_INT)
            if (colObj(j)%colModel.EQ.COL_MODEL_COMBO) then
                if (j==ID_COL_RADIUS_PICKUP) then
                  !PRINT *, "Element is ", colObj(j)%getElementInt(i)
                end if 
          
                call hl_gtk_listn_combo_set_by_list_id(ihObj, row=i-1_c_int, colno=j-1_c_int, &
                    & targetValue=colObj(j)%getElementInt(i))

            else
              call hl_gtk_listn_set_cell(ihObj, row=i-1_c_int, col=(j-1), &
              & ivalue=colObj(j)%getElementInt(i))
            end if            



         case (DTYPE_FLOAT)
          ! Special case - deal with infinity
          if (j.EQ.ID_COL_THICKNESS) then
            if (colObj(j)%getElementFloat(i).GT.(1e13)) then
              call hl_gtk_listn_set_cell(ihObj, row=i-1_c_int, col=(j-1), &
             & svalue="Infinity")
            else
            call hl_gtk_listn_set_cell(ihObj, row=i-1_c_int, col=(j-1), &
              & fvalue=colObj(j)%getElementFloat(i))
            end if
          else
            call hl_gtk_listn_set_cell(ihObj, row=i-1_c_int, col=(j-1), &
              & fvalue=colObj(j)%getElementFloat(i))
          end if            
          

        case (DTYPE_STRING)
          call hl_gtk_listn_set_cell(ihObj, row=i-1_c_int, col=(j-1), &
          & svalue=colObj(j)%getElementString(i))


        !  call hl_gtk_tree_set_cell(ihlist, absrow=i-1_c_int, col=(j-1), &
        !       & svalue="TMP")

      case (DTYPE_SCIENTIFIC) ! Actually float but need to convert to string for display
        !PRINT *, "Value to convert is ", colObj(j)%getElementFloat(i)
         call converttoscientificnotationstring(colObj(j)%getElementFloat(i), AVAL)
          call hl_gtk_listn_set_cell(ihObj, row=i-1_c_int, col=(j-1), &
               & svalue=AVAL)

      case (DTYPE_BOOLEAN)
          call hl_gtk_listn_set_cell(ihObj, row=i-1_c_int, col=(j-1), &
               & ivalue=colObj(j)%getElementInt(i))



        end select
      end do
     end do

  end subroutine

  subroutine converttoscientificnotationstring(fVal, strVal)
     real :: fVal
     character(len=23), intent(inout) :: strVal

     WRITE(strVal, "(E10.4)") fVal


  end subroutine

  subroutine lens_editor_asphere_dialog(boxAsph)

    use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char

    type(c_ptr), intent(inout) :: boxAsph

    character(len=35) :: line
    integer(kind=c_int) :: i, ltr, j


    call buildAsphereTable(.TRUE.)
   


    boxAsph = hl_gtk_box_new()
    ! It is the scrollcontainer that is placed into the box.
        call gtk_widget_set_vexpand (ihScrollAsph, FALSE)
        call gtk_widget_set_hexpand (ihScrollAsph, FALSE)


    call hl_gtk_box_pack(boxAsph, ihScrollAsph)

    PRINT *, "PACKING FIRST CONTAINER!"


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

  ! Goals
  ! Allow user to set pickup vals
  ! Check integrity of entry
  ! when closed, update kdp
  subroutine ui_pickup(row, pickup_type)

      use type_utils, only:  int2str

      integer :: row, pickup_type

     

      type(c_ptr) :: win,pUpdate, pCancel, boxWin, cBut, uBut
      type(c_ptr) :: table, lblSurf


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

      sbScale = gtk_spin_button_new (gtk_adjustment_new( &
      & value=curr_lens_data%pickups(3,row+1,pickup_type)*1d0, &
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

    call refreshLensDataStruct()
    call buildBasicTable(.FALSE.)
    call zoatabMgr%rePlotIfNeeded()

    call gtk_window_destroy(gdata)



  end subroutine

  subroutine pickupCancel_click(widget, parentWin) bind(c)
    type(c_ptr), value, intent(in) :: widget, parentWin

    PRINT *, "Button clicked!"

    ! This is a bad design but not sure what else to do to get rid of the "P" entry without
    ! yet another global
    call refreshLensDataStruct()
    call buildBasicTable(.FALSE.)
    call zoatabMgr%rePlotIfNeeded()    
    call gtk_window_destroy(parentWin)

  end subroutine  

  !TODO:
  !Move thic_solves data to kdp-data-types
  !In kdp-data-types, when updateSolveDataIsCalled
  !call a new method that finds which solve matches the 
  !value found in the curr_lens_data and sets param names
  !etc accordingly

  subroutine ui_solve(row, solve_type)

    use type_utils, only:  int2str

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
  

end subroutine

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
  




  call refreshLensDataStruct()
  call buildBasicTable(.FALSE.)
  call zoatabMgr%rePlotIfNeeded()

  call gtk_window_destroy(win_modal_solve)



end subroutine

subroutine setup_cb(factory,listitem, gdata) bind(c)
  
  type(c_ptr), value :: factory
  type(c_ptr), value :: listitem, gdata
  type(c_ptr) :: label, entryCB
  integer(kind=c_int), pointer :: ID_COL

  label =gtk_label_new(c_null_char)
  call gtk_list_item_set_child(listitem,label)

  call c_f_pointer(gdata, ID_COL)

  select case (ID_COL)

   case(5)
    entryCB = gtk_entry_new()
    call gtk_list_item_set_child(listitem,entryCB)
   case default 
    label =gtk_label_new(c_null_char)
    call gtk_list_item_set_child(listitem,label)
   end select 
end subroutine

subroutine bind_cb(factory,listitem, gdata) bind(c)
  use type_utils
  type(c_ptr), value :: factory
  type(c_ptr), value :: listitem, gdata
  type(c_ptr) :: widget, item, label, buffer
  type(c_ptr) :: cStr
  integer(kind=c_int), pointer :: ID_COL
  character(len=140) :: colName

  call c_f_pointer(gdata, ID_COL)
  label = gtk_list_item_get_child(listitem)
  item = gtk_list_item_get_item(listitem);

  select case (ID_COL)

  case(1)
    colName = trim(int2str(lens_item_get_surface_number(item)))//c_null_char
    cStr = lens_item_get_surface_name(item)
    call gtk_label_set_text(label, trim(colName)//c_null_char)
   case(2)
    colName = trim(int2str(lens_item_get_ref_surf(item)))//c_null_char
    call gtk_label_set_text(label, trim(colName)//c_null_char)
   case(3)
    cStr = lens_item_get_surface_name(item)
    call convert_c_string(cStr, colName)
    call gtk_label_set_text(label, trim(colName)//c_null_char)
   case(4)
    cStr = lens_item_get_surface_type(item)
    call convert_c_string(cStr, colName)   
    call gtk_label_set_text(label, trim(colName)//c_null_char)    
   case(5)
    colName = trim(real2str(lens_item_get_surface_radius(item)))//c_null_char    
    buffer = gtk_entry_get_buffer(label)
    call gtk_entry_buffer_set_text(buffer, trim(colName),-1_c_int)

   end select

     !call convert_c_string(cStr, colName)
     print *, "ID_COL is ", ID_COL
     print *, "colName is ", trim(colName)
    
    !call gtk_menu_button_set_label(label, trim(colName)//c_null_char)
     !call gtk_label_set_text(label, trim(int2str(lens_item_get_surface_number(item)))//c_null_char)
    
    !call gtk_label_set_text(label, "Test123"//c_null_char)
end subroutine


  function lens_editor_add_dialog(ID_TAB) result(boxNew)

    use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char

    type(integer) :: ID_TAB
    type(c_ptr) :: boxNew

    !Debug
    integer :: ii
    integer, target :: colIDs(10) = [(ii,ii=1,10)]

    type(c_ptr) :: store, cStrB, listitem, selection, factory, column, cv
    character(len=1024) :: debugName
    integer, target :: COL_ONE = 1

 
    character(kind=c_char, len=20),dimension(10) :: colNames


    colNames(1) = "Surface"
    colNames(2) = "Ref"
    colNames(3) = "Surface Name"
    colNames(4) = "Surface Type"
    colNames(5) = "Radius"

    do ii=1,10
      print *, "ColIDs(ii) ",colIDs(ii)

    end do
 
    !listitem = g_list_model_get_object(store, 0_c_int)
    !cStrB = lens_item_get_glass(listitem)
    !call convert_c_string(cStrB, debugName)
    !print *, "debugName is ", debugName    

    boxNew = c_null_ptr

    select case (ID_TAB)

    case (ID_EDIT_SOLVE)
      call buildSolveTable(.TRUE.)
      boxNew = hl_gtk_box_new()
      !store = g_list_store_new(G_TYPE_OBJECT)
      !store = append_lens_model(store, 1_c_int, 0_c_int, "Name"//c_null_char, "Sphere"//c_null_char, &
      !& 100.0d0, 1_c_int, 2.0d0, 1_c_int, "N-BK7"//c_null_char, 7.8d0, 1.51d0)

      store = buildLensEditTable()
    
  
      !selection = gtk_single_selection_new(store)
      selection = gtk_multi_selection_new(store)
      call gtk_single_selection_set_autoselect(selection,TRUE)    
      cv = gtk_column_view_new(selection)
      call gtk_column_view_set_show_column_separators(cv, 1_c_int)
      call gtk_column_view_set_show_row_separators(cv, 1_c_int)

      !call gtk_box_append(box, cv)                            
  
      
      do ii=1,5
        factory = gtk_signal_list_item_factory_new()
        call g_signal_connect(factory, "setup"//c_null_char, c_funloc(setup_cb),c_loc(colIDs(ii)))
        call g_signal_connect(factory, "bind"//c_null_char, c_funloc(bind_cb),c_loc(colIDs(ii)))
        column = gtk_column_view_column_new(trim(colNames(ii))//c_null_char, factory)
        call gtk_column_view_append_column (cv, column)
        call g_object_unref (column)      
      end do

      !call gtk_widget_set_vexpand (ihScrollSolv, FALSE)
      !call gtk_widget_set_hexpand (ihScrollSolv, FALSE)      
      call hl_gtk_box_pack(boxNew, cv)
    end select
   
    PRINT *, "PACKING FIRST CONTAINER!"


  end function


  subroutine le_col_init(self, title, coltype, sortable, editable, data, numRows, dtype, refsArray, valsArray)
    class(lens_edit_col), intent(inout) :: self
    integer(kind=type_kind) :: coltype
    integer, optional :: numRows
    integer, optional :: dtype
    character(len=*) :: title
    integer(kind=c_int) :: sortable, editable
    integer(c_int), dimension(:), optional :: refsArray
    character(len=20), dimension(:), optional :: valsArray
    class(*), dimension(:), intent(in) :: data
    integer :: m


    m = size(data)
  
    self%coltitle = title
    self%coltype = coltype
    self%sortable = sortable
    self%editable = editable

    self%colModel = 0 ! TODO:  Flesh out this more if it works post prototype


    select type(data)
    ! type is (logical)
    !   self%dtype = DTYPE_BOOLEAN
    !   allocate(integer::self%data(m))
    !   self%dataInt = data
    type is (real)
      if (present(dtype)) THEN
          if (dtype == DTYPE_SCIENTIFIC) THEN
        ! Store data as float but will be shown as string
          self%dtype = dtype
         !PRINT *, "Scientific Column!"
       end if

      Else
         self%dtype = DTYPE_FLOAT
       end if
       if (allocated(self%data)) deallocate(self%data)
       if (allocated(self%dataFloat)) deallocate(self%dataFloat)    
      allocate(real::self%data(m))
      allocate(self%dataFloat(m))
      self%dataFloat = data
    type is (integer)
      self%dtype = DTYPE_INT
      if (allocated(self%data)) deallocate(self%data)
      if (allocated(self%dataInt)) deallocate(self%dataInt)

      allocate(integer::self%data(m))
      allocate(self%dataInt(m))
      self%dataInt=data
      if (present(refsArray)) then
        if (allocated(self%refsArray)) deallocate(self%refsArray)
        if (allocated(self%valsArray)) deallocate(self%valsArray)
        
        allocate(integer(c_int) :: self%refsArray(numRows))
        allocate(character(len=20) :: self%valsArray(numRows))

        self%refsArray = refsArray
        self%valsArray = valsArray
        self%colModel = COL_MODEL_COMBO
        !PRINT *, "Set Col Model to ", self%colModel
      end if
    type is (character(*))
      self%dtype = DTYPE_STRING
      !PRINT *, "String length is ? ", size(self%data,1)
      !PRINT *, "m = ", m
      !m = len(self%data(1))
      if (allocated(self%data)) deallocate(self%data)
      if (allocated(self%dataString)) deallocate(self%dataString)    
      
      allocate(character(len=40)::self%data(numRows))
      allocate(character(len=40) :: self%dataString(numRows))
      self%dataString = data
   
    end select
    !self%data = data

  end subroutine

  function getElementInt(self, idx) result(res)
    class(lens_edit_col) :: self
    integer :: idx
    integer :: res
    res = self%dataInt(idx)

  end function

  function getElementFloat(self, idx) result(res)
    class(lens_edit_col) :: self
    integer :: idx
    real :: res
    res = self%dataFloat(idx)

  end function

  function getElementString(self, idx) result(res)
    class(lens_edit_col) :: self
    integer :: idx
    character(len=20) :: res
    res = self%dataString(idx)

  end function

  ! subroutine findGlassByName(cName)
  !
  !   implicit none
  !   character(len=*), intent(in) :: cName
  !   character(len=13) :: compStr
  !   integer :: JJ, maxVal, cLen
  !   include "DATLEN.INC"
  !
  !   maxVal = size(GLANAM,DIM=1)
  !
  !   compStr = "             "
  !   cLen = len(cName)
  !   if (cLen.LT.13) then
  !     compStr(1:cLen) = cName
  !   else
  !     compStr(1:13) = cName(1:13)
  !
  !   end if
  !
  !   do JJ=0,maxVal-1
  !     PRINT *, GLANAM(JJ,2)
  !     if (compStr.eq.GLANAM(JJ,2)) then
  !       PRINT *, "Found Glass in Catalog ", GLANAM(JJ,1)
  !       return
  !     end if
  !
  !   end do
  !
  ! end subroutine

end module lens_editor
