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


  contains
    procedure, public, pass(self) :: initialize => le_col_init
    procedure, public, pass(self) :: getElementInt
    procedure, public, pass(self) :: getElementFloat
    procedure, public, pass(self) :: getElementString
    final :: destructor




  end type


  type(c_ptr) :: ihscrollcontain,ihlist, &
       &  qbut, dbut, lbl, ibut, ihAsph, ihScrollAsph

  integer(kind=c_int) :: numEditorRows

  integer, parameter :: DTYPE_INT = 1
  integer, parameter :: DTYPE_FLOAT = 2
  integer, parameter :: DTYPE_STRING = 3


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



subroutine callback_lens_editor_settings (widget, gdata ) bind(c)
   use iso_c_binding
   use hl_gtk_zoa
   use zoa_ui
   implicit none
   type(c_ptr), value, intent(in) :: widget, gdata
   integer :: int_value

  integer(kind=c_int), pointer :: ID_SETTING

  call c_f_pointer(gdata, ID_SETTING)

  PRINT *, "Integer Passed is ", ID_SETTING

  PRINT *, "Pointer Passed is ", gdata


   !if (rf_settings%changed.eq.1) THEN
  !    rf_settings%changed = 0
  !    call lens_editor_replot()

   !end if

end subroutine callback_lens_editor_settings

  subroutine lens_editor_basic_dialog(box1)

    use hl_gtk_zoa
    use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char
    implicit none

    type(c_ptr), intent(inout) :: box1

    character(len=35) :: line
    integer(kind=c_int) :: i, ltr, j
    integer(kind=c_int), PARAMETER :: ncols = 5
    type(lens_edit_col), dimension(ncols) :: basicTypes
    integer(kind=type_kind), dimension(ncols) :: ctypes
    character(len=20), dimension(ncols) :: titles
    integer(kind=c_int), dimension(ncols) :: sortable, editable
    integer, allocatable, dimension(:) :: surfIdx

    allocate(surfIdx(curr_lens_data%num_surfaces))
    surfIdx =  (/ (i,i=0,curr_lens_data%num_surfaces-1)/)

    ctypes = (/ G_TYPE_INT, G_TYPE_FLOAT, G_TYPE_FLOAT, G_TYPE_STRING, &
         & G_TYPE_FLOAT /)
    !ctypes = (/ G_TYPE_STRING, G_TYPE_INT, G_TYPE_INT, G_TYPE_FLOAT, &
    !     & G_TYPE_UINT64, G_TYPE_BOOLEAN /)
    sortable = (/ FALSE, FALSE, FALSE, FALSE, FALSE /)
    editable = (/ FALSE, TRUE, TRUE, FALSE, FALSE /)

    titles(1) = "Surface"
    titles(2) = "Radius"
    titles(3) = "Thickness"
    titles(4) = "Glass"
    titles(5) = "Index"

    call basicTypes(1)%initialize("Surface"   , G_TYPE_INT,   FALSE, FALSE, surfIdx)
    call basicTypes(2)%initialize("Radius"    , G_TYPE_FLOAT, FALSE, TRUE, curr_lens_data%radii )
    call basicTypes(3)%initialize("Thickness" , G_TYPE_FLOAT, FALSE, TRUE, curr_lens_data%thicknesses )
    call basicTypes(4)%initialize("Glass"     , G_TYPE_STRING, FALSE, FALSE, &
    & curr_lens_data%glassnames, curr_lens_data%num_surfaces )  
    call basicTypes(5)%initialize("Index"     , G_TYPE_FLOAT, FALSE, FALSE, curr_lens_data%surf_index )


    ! do i=1,ncols
    !   ctypes(i) = basicTypes(i)%coltype
    !   sortable(i) = basicTypes(i)%sortable
    !   editable(i) = basicTypes(i)%editable
    !   titles(i) = basicTypes(i)%coltitle
    !
    ! end do


    PRINT *, "about to define ihlist"
    PRINT *, "cyptes is ", ctypes
    PRINT *, "titles is ", titles
    PRINT *, "sortable is ", sortable
    PRINT *, "editable is ", editable
    ihscrollcontain = c_null_ptr
    PRINT *, "ihlist= ", ihlist
    PRINT *, "ihscrollcontain= ", ihscrollcontain

    ihlist = hl_gtk_tree_new(ihscrollcontain, types=ctypes, &
         & changed=c_funloc(list_select),&
         & edited=c_funloc(lens_edited),&
         &  multiple=TRUE, height=250_c_int, swidth=400_c_int, titles=titles, &
         & sortable=sortable, editable=editable)

    PRINT *, "ihlist created!  ", ihlist

    ! Now put 10 top level rows into it
    call populatelensedittable(ihlist, basicTypes,ncols)
    !call loadLensData()

    box1 = hl_gtk_box_new()
    ! It is the scrollcontainer that is placed into the box.
        call gtk_widget_set_vexpand (ihscrollcontain, FALSE)
        call gtk_widget_set_hexpand (ihscrollcontain, FALSE)

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

  subroutine lens_editor_settings_dialog_2(box1)

    use hl_gtk_zoa
    use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char
    implicit none

    type(c_ptr), intent(inout) :: box1

    character(len=35) :: line
    integer(kind=c_int) :: i, ltr, j
    integer, parameter :: ncols = 5
    type(lens_edit_col), dimension(ncols) :: basicTypes
    integer(kind=type_kind), dimension(ncols) :: ctypes
    character(len=20), dimension(ncols) :: titles
    integer(kind=c_int), dimension(ncols) :: sortable, editable
    integer, allocatable, dimension(:) :: surfIdx

    allocate(surfIdx(curr_lens_data%num_surfaces))
    surfIdx =  (/ (i,i=0,curr_lens_data%num_surfaces-1)/)

    ctypes = (/ G_TYPE_INT, G_TYPE_FLOAT, G_TYPE_FLOAT, G_TYPE_STRING, &
         & G_TYPE_FLOAT /)

    call basicTypes(1)%initialize("Surface"   , G_TYPE_INT,   FALSE, FALSE, surfIdx)
    call basicTypes(2)%initialize("Radius"    , G_TYPE_FLOAT, FALSE, TRUE, curr_lens_data%radii )
    call basicTypes(3)%initialize("Thickness" , G_TYPE_FLOAT, FALSE, TRUE, curr_lens_data%thicknesses )
    call basicTypes(4)%initialize("Glass"     , G_TYPE_STRING, FALSE, FALSE, &
    & curr_lens_data%glassnames, curr_lens_data%num_surfaces )


    PRINT *, "size of glassnames is ", len(curr_lens_data%glassnames)
    !PRINT *, "glassnames array is ", curr_lens_data%glassnames
    !PRINT *, "basictypes array is ", basicTypes(4)%dataString
    PRINT *, "size of basicTypes is ", size(basicTypes)

    PRINT *, "LENS EDITOR DIALOG SUB STARTING!"
    ! Now make a multi column list with multiple selections enabled
    ctypes = (/ G_TYPE_INT, G_TYPE_FLOAT, G_TYPE_FLOAT, G_TYPE_STRING, &
         & G_TYPE_FLOAT /)
    !ctypes = (/ G_TYPE_STRING, G_TYPE_INT, G_TYPE_INT, G_TYPE_FLOAT, &
    !     & G_TYPE_UINT64, G_TYPE_BOOLEAN /)
    sortable = (/ FALSE, FALSE, FALSE, FALSE, FALSE /)
    editable = (/ FALSE, TRUE, TRUE, FALSE, FALSE /)

    titles(1) = "Surface"
    titles(2) = "Radius"
    titles(3) = "Thickness"
    titles(4) = "Glass"
    titles(5) = "Index"
    !titles(6) = "Abbe"

    PRINT *, "about to define ihlist"
    PRINT *, "cyptes is ", ctypes
    PRINT *, "titles is ", titles
    PRINT *, "sortable is ", sortable
    PRINT *, "editable is ", editable

    ihlist = hl_gtk_tree_new(ihscrollcontain, types=ctypes, &
         & changed=c_funloc(list_select),&
         & edited=c_funloc(lens_edited),&
         &  multiple=TRUE, height=250_c_int, swidth=400_c_int, titles=titles, &
         & sortable=sortable, editable=editable)

    PRINT *, "ihlist created!  ", ihlist

    ! Now put 10 top level rows into it

    !call loadLensData_2()
    call populatelensedittable(c_null_ptr,basicTypes,ncols)

    box1 = hl_gtk_box_new()
    ! It is the scrollcontainer that is placed into the box.
        call gtk_widget_set_vexpand (ihscrollcontain, FALSE)
        call gtk_widget_set_hexpand (ihscrollcontain, FALSE)

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

  end subroutine

  subroutine lens_editor_settings_dialog(box1)

    use hl_gtk_zoa
    use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char
    implicit none

    type(c_ptr), intent(inout) :: box1

    character(len=35) :: line
    integer(kind=c_int) :: i, ltr, j
    integer(kind=type_kind), dimension(6) :: ctypes
    character(len=20), dimension(6) :: titles
    integer(kind=c_int), dimension(6) :: sortable, editable

    PRINT *, "LENS EDITOR DIALOG SUB STARTING!"
    ! Now make a multi column list with multiple selections enabled
    ctypes = (/ G_TYPE_INT, G_TYPE_FLOAT, G_TYPE_FLOAT, G_TYPE_STRING, &
         & G_TYPE_FLOAT, G_TYPE_FLOAT /)
    !ctypes = (/ G_TYPE_STRING, G_TYPE_INT, G_TYPE_INT, G_TYPE_FLOAT, &
    !     & G_TYPE_UINT64, G_TYPE_BOOLEAN /)
    sortable = (/ FALSE, FALSE, FALSE, FALSE, FALSE, FALSE /)
    editable = (/ FALSE, TRUE, TRUE, FALSE, FALSE, FALSE /)

    titles(1) = "Surface"
    titles(2) = "Radius"
    titles(3) = "Thickness"
    titles(4) = "Glass"
    titles(5) = "Index"
    titles(6) = "Abbe"

    PRINT *, "about to define ihlist"
    PRINT *, "cyptes is ", ctypes
    PRINT *, "titles is ", titles
    PRINT *, "sortable is ", sortable
    PRINT *, "editable is ", editable

    ihlist = hl_gtk_tree_new(ihscrollcontain, types=ctypes, &
         & changed=c_funloc(list_select),&
         & edited=c_funloc(lens_edited),&
         &  multiple=TRUE, height=250_c_int, swidth=400_c_int, titles=titles, &
         & sortable=sortable, editable=editable)

    PRINT *, "ihlist created!  ", ihlist

    ! Now put 10 top level rows into it

    call loadLensData()

    box1 = hl_gtk_box_new()
    ! It is the scrollcontainer that is placed into the box.
        call gtk_widget_set_vexpand (ihscrollcontain, FALSE)
        call gtk_widget_set_hexpand (ihscrollcontain, FALSE)

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

  end subroutine lens_editor_settings_dialog


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

        PRINT *, "LENS EDITOR WINDOW PTR IS ", lens_editor_window

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
    integer(kind=c_int), dimension(:,:), allocatable :: iset
    integer(kind=c_int), dimension(:), allocatable :: dep
    integer(kind=c_int) :: nsel
    character(len=40) :: kdptext

    nsel = hl_gtk_tree_get_selections(ihlist, iset, &
         & depths=dep)

    if (nsel /= 1) then
       print *, "Not a single selection"
       return
    end if

    PRINT *, "Insert Method!"

    !call hl_gtk_tree_rem(ihlist, selections(:dep(1),1))

    call PROCESKDP('U L')
    WRITE(kdptext,*) 'CHG,', iset(1,1)
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

    call refreshLensDataStruct()
    call loadLensData()
    call zoatabMgr%rePlotIfNeeded()


  end subroutine

 subroutine del_row(but, gdata) bind(c)
    type(c_ptr), value, intent(in) :: but, gdata
    integer(kind=c_int), dimension(:,:), allocatable :: iset
    integer(kind=c_int), dimension(:), allocatable :: dep
    integer(kind=c_int) :: nsel
    integer(kind=c_int) :: lastSurface
    character(len=40) :: kdptext

    nsel = hl_gtk_tree_get_selections(ihlist, iset, &
         & depths=dep)

    if (nsel /= 1) then
       print *, "Not a single selection"
       return
    end if

    PRINT *, "iset = ", iset(1,1)
    call getOpticalSystemLastSurface(lastSurface)


        IF(iset(1,1) == 0) THEN
            call updateTerminalLog('OBJECT SURFACE MAY NOT BE DELETED', "black")
            return
        END IF
        IF(iset(1,1) == lastSurface) THEN
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
        WRITE(kdptext,*) 'CHG,', iset(1,1)
        call PROCESKDP(kdptext)
        call PROCESKDP('DEL')
        call PROCESKDP('EOS')
        call PROCESKDP('OUT TP')


    call hl_gtk_tree_rem(ihlist, iset(:dep(1),1))

    call gtk_widget_set_sensitive(but, FALSE)

    call refreshLensDataStruct()
    call loadLensData()
    call zoatabMgr%rePlotIfNeeded()

  end subroutine del_row

  subroutine lens_edited(renderer, path, text, gdata) bind(c)
    !type(c_ptr), value, intent(in) :: list, gdata

    type(c_ptr), value :: renderer, path, text, gdata
    character(len=40) :: kdptext



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
    integer(kind=c_int), allocatable, dimension(:) :: irow
    integer(kind=c_int), pointer :: icol
    integer :: i, n
    type(c_ptr) :: tree, pcol
    integer(kind=c_int), parameter :: ID_ROW_RADIUS = 1
    integer(kind=c_int), parameter :: ID_ROW_THICKNESS = 2

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
    allocate(irow(n+1))
    read(fpath, *) irow
    tree = g_object_get_data(renderer, "view"//c_null_char)

    call hl_gtk_tree_set_cell(tree, irow, icol, &
         & svalue=trim(ftext))

    PRINT *, "New value is ", trim(ftext)
    PRINT *, "Selected Row is ", irow

    ! Try to update lens system
    !select case (irow(1))
    select case (icol)
  case (ID_ROW_RADIUS)
        call PROCESKDP('U L')
        WRITE(kdptext, *) 'CHG ' ,irow
        call PROCESKDP(kdptext)
        call PROCESKDP("RD "//trim(ftext))
        call PROCESKDP('EOS')

      case (ID_ROW_THICKNESS)
        PRINT *, "Thickness changed!"
        call PROCESKDP('U L')
        WRITE(kdptext, *) 'CHG ' ,irow
        call PROCESKDP(kdptext)
        call PROCESKDP("TH "//trim(ftext))
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

    deallocate(irow)




  end subroutine

subroutine asph_edited(renderer, path, text, gdata) bind(c)
  !type(c_ptr), value, intent(in) :: list, gdata

  type(c_ptr), value :: renderer, path, text, gdata

end subroutine

subroutine asph_select(list, gdata) bind(c)
  type(c_ptr), value, intent(in) :: list, gdata

end subroutine

  subroutine list_select(list, gdata) bind(c)
    type(c_ptr), value, intent(in) :: list, gdata
    integer(kind=c_int) :: nsel
    integer(kind=c_int), dimension(:,:), allocatable :: selections
    integer(kind=c_int), dimension(:), allocatable :: dep
    integer(kind=c_int) :: n, n3
    integer(kind=c_int64_t) :: n4
    real(kind=c_float) :: nlog
    character(len=30) :: name
    character(len=10) :: nodd
    integer :: i
    nsel = hl_gtk_tree_get_selections(C_NULL_PTR, selections, selection=list, &
         & depths=dep)
    if (nsel == 0) then
       print *, "No selection"
       return
    end if

    ! Find and print the selected row(s)
    print *, nsel,"Rows selected"
    print *, "Depths", dep
    print *, "Rows"
    do i = 1, nsel
       print *, selections(:dep(i),i)
    end do

    if (nsel == 1) then
       call hl_gtk_tree_get_cell(ihlist, selections(:dep(1),1), 0_c_int, &
            & ivalue=n)
       ! call hl_gtk_tree_get_cell(ihlist, selections(:dep(1),1), 1_c_int, &
       !      & ivalue=n)
       ! call hl_gtk_tree_get_cell(ihlist, selections(:dep(1),1), 2_c_int, &
       !      & ivalue=n3)
       ! call hl_gtk_tree_get_cell(ihlist, selections(:dep(1),1), 4_c_int, &
       !      & l64value=n4)
       ! call hl_gtk_tree_get_cell(ihlist, selections(:dep(1),1), 3_c_int, &
       !      & fvalue=nlog)
       ! call hl_gtk_tree_get_cell(ihlist, selections(:dep(1),1), 5_c_int,&
       !      & svalue=nodd)
      print *, "first column is , ", n
       ! print "('Name: ',a,' N:',I3,' 3N:',I4,' N**4:',I7,&
       !      &' log(n):',F7.5,' Odd?: ',a)", trim(name), &
       !      & n, n3, n4, nlog, nodd
       call gtk_widget_set_sensitive(dbut, TRUE)
       call gtk_widget_set_sensitive(ibut, TRUE)
    else
       call gtk_widget_set_sensitive(dbut, FALSE)
       call gtk_widget_set_sensitive(ibut, FALSE)
    end if

    deallocate(selections)
  end subroutine list_select

  subroutine updateSurfaceNumbers()

    integer :: i

    do i = 1, numEditorRows

        call hl_gtk_tree_set_cell(ihlist, absrow=i-1_c_int, col=0_c_int, &
             & ivalue=(i-1))
        end do


  end subroutine

  subroutine populatelensedittable(ihObj, colObj, m)
    type(c_ptr) :: ihObj
    type(lens_edit_col), dimension(*) :: colObj
    integer :: i,j
    integer(kind=c_int) :: m

    !m = size(colObj,DIM=1)

    call hl_gtk_tree_rem(ihlist)

     do i=1,curr_lens_data%num_surfaces
        call hl_gtk_tree_ins(ihlist, row = (/ -1_c_int /))

        do j=1,m
          select case(colObj(j)%dtype)

          case(DTYPE_INT)

        call hl_gtk_tree_set_cell(ihlist, absrow=i-1_c_int, col=(j-1), &
             & ivalue=colObj(j)%getElementInt(i))

         case (DTYPE_FLOAT)
        call hl_gtk_tree_set_cell(ihlist, absrow=i-1_c_int, col=(j-1), &
             & fvalue=colObj(j)%getElementFloat(i))

        case (DTYPE_STRING)
             PRINT *, "j is ", j
             PRINT *, "i is ", i

             PRINT *, "Val is ", colObj(j)%getElementString(i)

        call hl_gtk_tree_set_cell(ihlist, absrow=i-1_c_int, col=(j-1), &
             & svalue=colObj(j)%getElementString(i))
        !  call hl_gtk_tree_set_cell(ihlist, absrow=i-1_c_int, col=(j-1), &
        !       & svalue="TMP")
        end select
      end do
     end do

  end subroutine

  subroutine loadLensData()

    integer :: i

    !Make sure we start froms scratch
    call hl_gtk_tree_rem(ihlist)

     do i=1,curr_lens_data%num_surfaces
        call hl_gtk_tree_ins(ihlist, row = (/ -1_c_int /))
    !    write(line,"('List entry number ',I0)") i
    !    ltr=len_trim(line)+1
    !    line(ltr:ltr)=c_null_char
        call hl_gtk_tree_set_cell(ihlist, absrow=i-1_c_int, col=0_c_int, &
             & ivalue=(i-1))
        call hl_gtk_tree_set_cell(ihlist, absrow=i-1_c_int, col=1_c_int, &
             & fvalue=curr_lens_data%radii(i))
        call hl_gtk_tree_set_cell(ihlist, absrow=i-1_c_int, col=2_c_int, &
             & fvalue=curr_lens_data%thicknesses(i))
        call hl_gtk_tree_set_cell(ihlist, absrow=i-1_c_int, col=3_c_int, &
             & svalue=curr_lens_data%glassnames(i))
        call hl_gtk_tree_set_cell(ihlist, absrow=i-1_c_int, col=4_c_int, &
             & fvalue=curr_lens_data%surf_index(i))
        call hl_gtk_tree_set_cell(ihlist, absrow=i-1_c_int, col=5_c_int, &
             & fvalue=curr_lens_data%surf_vnum(i))
     end do

  end subroutine


subroutine loadLensData_2()

  integer :: i

  !Make sure we start froms scratch
  call hl_gtk_tree_rem(ihlist)

   do i=1,curr_lens_data%num_surfaces
      call hl_gtk_tree_ins(ihlist, row = (/ -1_c_int /))
  !    write(line,"('List entry number ',I0)") i
  !    ltr=len_trim(line)+1
  !    line(ltr:ltr)=c_null_char
      call hl_gtk_tree_set_cell(ihlist, absrow=i-1_c_int, col=0_c_int, &
           & ivalue=(i-1))
      call hl_gtk_tree_set_cell(ihlist, absrow=i-1_c_int, col=1_c_int, &
           & fvalue=curr_lens_data%radii(i))
      call hl_gtk_tree_set_cell(ihlist, absrow=i-1_c_int, col=2_c_int, &
           & fvalue=curr_lens_data%thicknesses(i))
      call hl_gtk_tree_set_cell(ihlist, absrow=i-1_c_int, col=3_c_int, &
           & svalue=curr_lens_data%glassnames(i))
      call hl_gtk_tree_set_cell(ihlist, absrow=i-1_c_int, col=4_c_int, &
           & fvalue=curr_lens_data%surf_index(i))
   end do

end subroutine


  subroutine loadAphereDataIntoTable()

    integer :: i

    !Make sure we start froms scratch
    call hl_gtk_tree_rem(ihAsph)

     do i=1,curr_lens_data%num_surfaces
        call hl_gtk_tree_ins(ihAsph, row = (/ -1_c_int /))
    !    write(line,"('List entry number ',I0)") i
    !    ltr=len_trim(line)+1
    !    line(ltr:ltr)=c_null_char
        call hl_gtk_tree_set_cell(ihAsph, absrow=i-1_c_int, col=0_c_int, &
             & ivalue=(i-1))
        call hl_gtk_tree_set_cell(ihAsph, absrow=i-1_c_int, col=1_c_int, &
             & svalue="TMP"//c_null_char)
        call hl_gtk_tree_set_cell(ihAsph, absrow=i-1_c_int, col=2_c_int, &
             & fvalue=curr_asph_data%conic_constant(i))

        ! call hl_gtk_tree_set_cell(ihlist, absrow=i-1_c_int, col=3_c_int, &
        !      & svalue=curr_lens_data%glassnames(i))
        ! call hl_gtk_tree_set_cell(ihlist, absrow=i-1_c_int, col=4_c_int, &
        !      & fvalue=curr_lens_data%surf_index(i))
        ! call hl_gtk_tree_set_cell(ihlist, absrow=i-1_c_int, col=5_c_int, &
        !      & fvalue=curr_lens_data%surf_vnum(i))
     end do

  end subroutine

  subroutine lens_editor_asphere_dialog(boxAsph)

    use hl_gtk_zoa
    use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char
    implicit none

    type(c_ptr), intent(inout) :: boxAsph

    character(len=35) :: line
    integer(kind=c_int) :: i, ltr, j
    integer(kind=type_kind), dimension(3) :: ctypes
    character(len=20), dimension(3) :: titles
    integer(kind=c_int), dimension(3) :: sortable, editable

    PRINT *, "LENS EDITOR Asphere DIALOG SUB STARTING!"
    ! Now make a multi column list with multiple selections enabled
    ctypes = (/ G_TYPE_INT, &
                G_TYPE_STRING, &
                G_TYPE_FLOAT /)
    !ctypes = (/ G_TYPE_STRING, G_TYPE_INT, G_TYPE_INT, G_TYPE_FLOAT, &
    !     & G_TYPE_UINT64, G_TYPE_BOOLEAN /)
    sortable = (/ FALSE, FALSE, FALSE /)
    editable = (/ FALSE, FALSE, TRUE /)

    titles(1) = "Surface"
    titles(2) = "Type"
    titles(3) = "Conic Constant"
    !titles(4) = "Glass"
    !titles(5) = "Index"
    !titles(6) = "Abbe"

    ihAsph = hl_gtk_tree_new(ihScrollAsph, types=ctypes, &
         & changed=c_funloc(asph_select),&
         & edited=c_funloc(asph_edited),&
         &  multiple=TRUE, height=250_c_int, swidth=400_c_int, titles=titles, &
         & sortable=sortable, editable=editable)

    !PRINT *, "ihlist created!  ", ihlist

    ! Now put 10 top level rows into it
    call loadAphereDataIntoTable()
    !call loadLensData()

    boxAsph = hl_gtk_box_new()
    ! It is the scrollcontainer that is placed into the box.
        call gtk_widget_set_vexpand (ihScrollAsph, FALSE)
        call gtk_widget_set_hexpand (ihScrollAsph, FALSE)

    call hl_gtk_box_pack(boxAsph, ihScrollAsph)

    PRINT *, "PACKING FIRST CONTAINER!"


  end subroutine

  subroutine le_col_init(self, title, coltype, sortable, editable, data, numRows)
    class(lens_edit_col), intent(inout) :: self
    integer(kind=type_kind) :: coltype
    integer, optional :: numRows
    character(len=*) :: title
    integer(kind=c_int) :: sortable, editable
    class(*), dimension(:), intent(in) :: data
    integer :: m

    m = size(data)

    self%coltitle = title
    self%coltype = coltype
    self%sortable = sortable
    self%editable = editable


    select type(data)
    type is (real)
      self%dtype = DTYPE_FLOAT
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

  subroutine destructor(self)
    type(lens_edit_col) :: self
    PRINT *, "Destructor called!"

  end subroutine

end module lens_editor
