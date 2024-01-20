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


  use GLOBALS
  use global_widgets
  use handlers
  use cairo
!  use gth_hl
  use gtk_hl_container
  use gtk_hl_progress
  use gtk_hl_button
  use gtk_hl_tree
  !use hl_zoa_tree_tmp, only: hl_gtk_listn_new, hl_gtk_listn_get_selections, hl_gtk_listn_ins, &
  !& hl_gtk_listn_set_cell, hl_gtk_listn_rem
  use hl_gtk_zoa

  use gtk
  use gtk_hl_chooser

  use g
  use zoa_ui
  !use mod_lens_editor_settings

  implicit none


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



  integer, parameter :: DTYPE_INT = 1
  integer, parameter :: DTYPE_FLOAT = 2
  integer, parameter :: DTYPE_STRING = 3
  integer, parameter :: DTYPE_SCIENTIFIC = 4
  integer, parameter :: DTYPE_BOOLEAN = 5

  integer, parameter :: COL_MODEL_COMBO = 5

  integer(kind=c_int), parameter :: ID_COL_RADIUS = 3
  integer(kind=c_int), parameter :: ID_COL_THICKNESS = 4
  integer(kind=c_int), parameter :: ID_COL_GLASS = 5
  integer(kind=c_int), parameter :: ID_COL_INDEX = 6



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

    use hl_gtk_zoa
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
        call PROCESKDP('INS')
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
           PRINT *, 'DEL, '//ITOC(iset(1))
           call PROCESKDP('DEL, '//ITOC(iset(1)))
        else
          call PROCESKDP('DEL, '//ITOC(iset(1))//','//ITOC(iset(nsel)))
        end if
        !call PROCESKDP('DEL')
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
    use kdp_utils, only: real2str
    !type(c_ptr), value, intent(in) :: list, gdata

    type(c_ptr), value :: renderer, path, text, gdata
    character(len=40) :: kdptext
    character(len=13) :: catalogName



    ! Default callback for tree cell edited.
    !
    ! RENDERER: c_ptr: required: The renderer which sent the signal
    ! PATH: c_ptr: required: The path at which to insert
    ! TEXT: c_ptr: required: The text to insert
    ! GDATA: c_ptr: required: User data, not used.
    !
    ! The column number is passed via the "column-number" gobject data value.
    ! The treeview containing the cell is passed via the "view" gobject
    ! data value.
    ! The row number is passed as a string in the PATH argument.
    !
    ! This routine is not normally called by the application developer.
    !-

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
        call PROCESKDP('U L')
        WRITE(kdptext, *) 'CHG ' ,irow
        call PROCESKDP(kdptext)
        call PROCESKDP("RD "//trim(ftext))
        call PROCESKDP('EOS')

      case (ID_COL_THICKNESS)
        PRINT *, "Thickness changed!"
        call PROCESKDP('U L')
        WRITE(kdptext, *) 'CHG ' ,irow
        call PROCESKDP(kdptext)
        call PROCESKDP("TH "//trim(ftext))
        call PROCESKDP('EOS')
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
        call PROCESKDP('U L')
        WRITE(kdptext, *) 'CHG ' ,irow
        call PROCESKDP(kdptext)
        call PROCESKDP(catalogName//' '//ftext)
        call PROCESKDP('EOS')
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

  subroutine solv_edited(renderer, path, text, gdata) bind(c)
    use hl_gtk_zoa
    implicit none
    !type(c_ptr), value, intent(in) :: list, gdata
  
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

subroutine getRowAndColFromCallback(widget, path, row, col) 
  type(c_ptr), value :: widget, path
  integer :: row, col
  character(len=200) :: outStr
  character(len=200) :: fpath

  integer(kind=c_int), pointer :: icol
  integer :: i, n
  type(c_ptr) :: tree, pcol, treeCol, model
  integer(kind=c_int), allocatable, dimension(:) :: irow


  call convert_c_string(path, fpath)
  pcol = g_object_get_data(widget, "column-number"//c_null_char)
  call c_f_pointer(pcol, icol)

  PRINT *, "icol is ", icol  

  n = 0
  do i = 1, len_trim(fpath)
     if (fpath(i:i) == ":") then
        n = n+1
        fpath(i:i) = ' '   ! : is not a separator for a Fortran read
     end if
  end do
  allocate(irow(n+1))
  read(fpath, *) irow
  PRINT *, "Selected Row is ", irow

  ! Only return the first row if multiple are selected
  row = irow(1)
  

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
  use hl_gtk_zoa
  !type(c_ptr), value, intent(in) :: list, gdata

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

subroutine buildBasicTable(firstTime)

    logical :: firstTime
    integer(kind=c_int), PARAMETER :: ncols = 6
    character(len=10), dimension(ncols) :: colModel
    type(lens_edit_col), dimension(ncols) :: basicTypes
    integer(kind=type_kind), dimension(ncols) :: ctypes
    character(len=20), dimension(ncols) :: titles
    integer(kind=c_int), dimension(ncols) :: sortable, editable
    integer, allocatable, dimension(:) :: surfIdx
    integer, allocatable, dimension(:) :: isRefSurface

    integer :: i

    allocate(surfIdx(curr_lens_data%num_surfaces))
    surfIdx =  (/ (i,i=0,curr_lens_data%num_surfaces-1)/)

    allocate(isRefSurface(curr_lens_data%num_surfaces))
    isRefSurface = 0*isRefSurface
    isRefSurface(curr_lens_data%ref_stop) = 1

    call basicTypes(1)%initialize("Surface"   , G_TYPE_INT,   FALSE, FALSE, surfIdx)
    call basicTypes(2)%initialize("Ref"    , G_TYPE_BOOLEAN, FALSE, TRUE, isRefSurface)
    call basicTypes(ID_COL_RADIUS)%initialize("Radius"    , G_TYPE_FLOAT, FALSE, TRUE, curr_lens_data%radii )
    call basicTypes(ID_COL_THICKNESS)%initialize("Thickness" , G_TYPE_FLOAT, FALSE, TRUE, curr_lens_data%thicknesses )
    call basicTypes(ID_COL_GLASS)%initialize("Glass"     , G_TYPE_STRING, FALSE, TRUE, &
    & curr_lens_data%glassnames, curr_lens_data%num_surfaces )
    call basicTypes(ID_COL_INDEX)%initialize("Index"     , G_TYPE_FLOAT, FALSE, FALSE, curr_lens_data%surf_index )

    colModel(1) = 'text'
    colModel(2) = 'radio'
    colModel(3) = 'text'
    colModel(4) = 'text'
    colModel(5) = 'text'
    colModel(6) = 'text'

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

    ! Now put 10 top level rows into it
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
    PRINT *, "Number of entries in loop is ", curr_lens_data%num_surfaces
     do i=1,curr_lens_data%num_surfaces
        call hl_gtk_listn_ins(ihObj, count = 1_c_int)

        do j=1,m


          select case(colObj(j)%dtype)


          case(DTYPE_INT)
            if (colObj(j)%colModel.EQ.COL_MODEL_COMBO) then
          
                PRINT *, "ihObj is ", LOC(ihObj)
                call hl_gtk_listn_combo_set_by_list_id(ihObj, row=i-1_c_int, colno=j-1_c_int, &
                    & targetValue=colObj(j)%getElementInt(i))

            else
              call hl_gtk_listn_set_cell(ihObj, row=i-1_c_int, col=(j-1), &
              & ivalue=colObj(j)%getElementInt(i))
            end if            



         case (DTYPE_FLOAT)
        call hl_gtk_listn_set_cell(ihObj, row=i-1_c_int, col=(j-1), &
             & fvalue=colObj(j)%getElementFloat(i))

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

    use hl_gtk_zoa
    use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char
    implicit none

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

  function lens_editor_add_dialog(ID_TAB) result(boxNew)

    use hl_gtk_zoa
    use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char
    implicit none

    type(integer) :: ID_TAB

    type(c_ptr) :: boxNew

    boxNew = c_null_ptr

    select case (ID_TAB)

    case (ID_EDIT_SOLVE)
      call buildSolveTable(.TRUE.)
      boxNew = hl_gtk_box_new()
      call gtk_widget_set_vexpand (ihScrollSolv, FALSE)
      call gtk_widget_set_hexpand (ihScrollSolv, FALSE)      
      call hl_gtk_box_pack(boxNew, ihScrollSolv)
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
         PRINT *, "Scientific Column!"
       end if

      Else
         self%dtype = DTYPE_FLOAT
       end if
      allocate(real::self%data(m))
      allocate(self%dataFloat(m))
      self%dataFloat = data
    type is (integer)
      self%dtype = DTYPE_INT
      allocate(integer::self%data(m))
      allocate(self%dataInt(m))
      self%dataInt=data
      if (present(refsArray)) then
        allocate(integer(c_int) :: self%refsArray(numRows))
        allocate(character(len=20) :: self%valsArray(numRows))

        self%refsArray = refsArray
        self%valsArray = valsArray
        self%colModel = COL_MODEL_COMBO
        PRINT *, "Set Col Model to ", self%colModel
      end if
    type is (character(*))
      self%dtype = DTYPE_STRING
      !PRINT *, "String length is ? ", size(self%data,1)
      !PRINT *, "m = ", m
      !m = len(self%data(1))
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
