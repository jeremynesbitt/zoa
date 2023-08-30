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
  use hl_gtk_zoa
  !use hl_zoa_tree_tmp
  use gtk

  use gtk_hl_chooser

  use g
  use zoa_ui
  !use mod_lens_editor_settings

  implicit none

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
    integer :: colModel


  contains
    procedure, public, pass(self) :: initialize => le_col_init
    procedure, public, pass(self) :: getElementInt
    procedure, public, pass(self) :: getElementFloat
    procedure, public, pass(self) :: getElementString

  end type


  type(c_ptr) :: ihscrollcontain,ihlist, &
       &  qbut, dbut, lbl, ibut, ihAsph, ihScrollAsph

  integer(kind=c_int) :: numEditorRows

  integer, parameter :: ID_EDIT_ASPH_NONTORIC = 1001
  integer, parameter :: ID_EDIT_ASPH_TORIC_Y = 1002
  integer, parameter :: ID_EDIT_ASPH_TORIC_X = 1003



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


    !call lens_editor_aperture(boxAperture)

    !call gtk_scrolled_window_set_child(scrolled_tab, self%box1)
    !location = gtk_notebook_append_page(self%notebook, scrolled_tab, self%tab_label)
    nbk = gtk_notebook_new()
    basicLabel = gtk_label_new_with_mnemonic("_Basic"//c_null_char)
    pageIdx = gtk_notebook_append_page(nbk, box1, basicLabel)

    AsphLabel = gtk_label_new_with_mnemonic("_Asphere"//c_null_char)
    pageIdx = gtk_notebook_append_page(nbk, boxAsphere, AsphLabel)


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
    !call loadLensData()
    PRINT *, "About to try replot"
    call zoatabMgr%rePlotIfNeeded()
    PRINT *, "Done with replot"

  end subroutine del_row

  subroutine lens_edited(renderer, path, text, gdata) bind(c)

    use glass_manager
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
        call findCatalogNameFromGlassName(ftext, catalogName)
        PRINT *, "Glass entry request is ", ftext
        call PROCESKDP('U L')
        WRITE(kdptext, *) 'CHG ' ,irow
        call PROCESKDP(kdptext)
        call PROCESKDP(catalogName//' '//ftext)
        call PROCESKDP('EOS')



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

subroutine asph_edited(renderer, path, text, gdata) bind(c)
  use hl_gtk_zoa
  !type(c_ptr), value, intent(in) :: list, gdata

  type(c_ptr), value :: renderer, path, text, gdata
    character(len=200) :: fpath, ftext
    character(len=40) :: kdptext
    integer(kind=c_int), allocatable, dimension(:) :: irow
    integer(kind=c_int), pointer :: icol
    integer :: i, n
    type(c_ptr) :: tree, pcol
    real :: fVal
    integer :: ios
    integer(kind=c_int), pointer :: ID_SETTING

    call c_f_pointer(gdata, ID_SETTING)
    PRINT *, "CALLING ASPHERE EDITED PROC!"

    call convert_c_string(path, fpath)
    pcol = g_object_get_data(renderer, "column-number"//c_null_char)
    call c_f_pointer(pcol, icol)
    call convert_c_string(text, ftext)

    PRINT *, "ftext is ", ftext

    ID_SETTING = hl_zoa_combo_get_selected_list2_id(renderer)
    PRINT *, "ID_SETTING is ", ID_SETTING
    !PRINT *, "IOS is ", ios


    read( ftext, *, iostat=ios)  fVal

    PRINT *, "ios is ", ios

    if (ios == 0)  THEN !Valid number

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
    integer, parameter :: ncols = 11
    type(lens_edit_col) :: asphereTypes(ncols)
    integer(kind=type_kind), dimension(ncols) :: ctypes
    character(len=20), dimension(ncols) :: titles
    integer(kind=c_int), dimension(ncols) :: sortable, editable
    integer, allocatable, dimension(:) :: surfIdx
    character(len=10), dimension(ncols) :: colModel
    integer :: i
    integer, parameter :: numAsphTypes = 3
    character(kind=c_char, len=20),dimension(numAsphTypes) :: valsArray
    integer(c_int), dimension(numAsphTypes) :: refsArray
    type(c_ptr) :: colTmp


    allocate(surfIdx(curr_lens_data%num_surfaces))
    surfIdx =  (/ (i,i=0,curr_lens_data%num_surfaces-1)/)



    valsArray(1) = "Non-Toric"
    valsArray(2) = "Toric Parallel to Y"
    valsArray(3) = "Toric Parallel to X"

    refsArray(1) = ID_EDIT_ASPH_NONTORIC
    refsArray(2) = ID_EDIT_ASPH_TORIC_Y
    refsArray(3) = ID_EDIT_ASPH_TORIC_X




    PRINT *, "LENS EDITOR Asphere DIALOG SUB STARTING!"
    ! Now make a multi column list with multiple selections enabled
    call asphereTypes(1)%initialize("Surface", G_TYPE_INT, FALSE, FALSE, surfIdx)
    call asphereTypes(2)%initialize("Type", G_TYPE_STRING, FALSE, TRUE, data=valsArray, numRows=numAsphTypes , refsArray=refsArray)
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



    do i=1,ncols
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

!         & valsArray=valsArray, refsArray=refsArray)
       end if

   call hl_gtk_listn_attach_combo_box_model(ihAsph, 1_c_int, valsArray, refsArray)



    !PRINT *, "ihlist created!  ", ihlist
    !colTmp = gtk_tree_view_get_column(ihAsph, 3_c_int)
    !call gtk_column_view_column_set_fixed_width(colTmp, 0_c_int)
    !call gtk_tree_view_column_set_max_width(colTmp, 0_c_int)
    ! Did not test code of this,but in debug window this works.
    !call gtk_tree_view_column_set_visible(colTmp, 0_c_int)

    ! Now put 10 top level rows into it
    PRINT *, "About to Populate UI Table"
    call populatelensedittable(ihAsph, asphereTypes, ncols)
    PRINT *, "Done Populating UI Table"
    !call hl_gtk_box_pack(ihScrollAsph, ihAsph)
    !call loadAphereDataIntoTable()
    !call loadLensData()

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

  subroutine populatelensedittable(ihObj, colObj, m)
    type(c_ptr) :: ihObj
    type(lens_edit_col), dimension(*) :: colObj
    character(len=23) :: AVAL
    !integer :: i,j
    integer(kind=c_int) :: i,j,m

    !m = size(colObj,DIM=1)

    call hl_gtk_listn_rem(ihObj)
    PRINT *, "Number of entries in loop is ", curr_lens_data%num_surfaces
     do i=1,curr_lens_data%num_surfaces
        call hl_gtk_listn_ins(ihObj, count = 1_c_int)

        do j=1,m


          select case(colObj(j)%dtype)


          case(DTYPE_INT)

        call hl_gtk_listn_set_cell(ihObj, row=i-1_c_int, col=(j-1), &
             & ivalue=colObj(j)%getElementInt(i))

         case (DTYPE_FLOAT)
        call hl_gtk_listn_set_cell(ihObj, row=i-1_c_int, col=(j-1), &
             & fvalue=colObj(j)%getElementFloat(i))

        case (DTYPE_STRING)

        if (colObj(j)%colModel.EQ.COL_MODEL_COMBO) then
          call hl_gtk_listn_combo_set_by_list_id(ihObj, row=i-1_c_int, colno=j-1_c_int, &
               & targetValue=ID_EDIT_ASPH_NONTORIC)
        else
          call hl_gtk_listn_set_cell(ihObj, row=i-1_c_int, col=(j-1), &
             & svalue=colObj(j)%getElementString(i))
        end if

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


  subroutine le_col_init(self, title, coltype, sortable, editable, data, numRows, dtype, refsArray)
    class(lens_edit_col), intent(inout) :: self
    integer(kind=type_kind) :: coltype
    integer, optional :: numRows
    integer, optional :: dtype
    character(len=*) :: title
    integer(kind=c_int) :: sortable, editable
    integer(c_int), dimension(:), optional :: refsArray
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
    type is (character(*))
      self%dtype = DTYPE_STRING
      !PRINT *, "String length is ? ", size(self%data,1)
      !PRINT *, "m = ", m
      !m = len(self%data(1))
      allocate(character(len=40)::self%data(numRows))
      allocate(character(len=40) :: self%dataString(numRows))
      self%dataString = data
      if (present(refsArray)) then
        allocate(integer(c_int) :: self%refsArray(numRows))
        self%refsArray = refsArray
        self%colModel = COL_MODEL_COMBO
        PRINT *, "Set Col Model to ", self%colModel
      end if
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
