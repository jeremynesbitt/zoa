! This module is meant to augment some of the existing hl_gtk functions
! in gtk-fortran fo zoa.  Most functions revolve around supporting
! combo boxes with a model that has a unique integer attached to each user
! selectable string to avoid using string comparison when identifing ui choices
! TODO:  Some refactoring is needed here (eg model creation)
! Could probably also refactor some

module hl_gtk_zoa
    use iso_c_binding
    use gtk
    use gtk_sup
    use g

    implicit none

contains


subroutine hl_gtk_combo_box_list2_new(cbox, refsArray, valsArray, ID_SETTING)
  type(c_ptr), intent(inout) :: cbox
  integer(c_int), intent(in), target, optional :: ID_SETTING
  character(kind=c_char, len=*), intent(in) :: valsArray(:)
  integer(c_int), dimension(:) :: refsArray
  integer                      :: i
  type(c_ptr)  :: combo_list_store
  type(c_ptr)  :: data_callback

  !type(c_ptr)  :: val_ptr
  character(:), pointer :: val_ptr
  character(kind=c_char), dimension(:), allocatable :: textptr




  integer(kind=c_int), parameter :: ncols=2
  integer(kind=type_kind), dimension(ncols), target :: coltypes = &
       & [G_TYPE_INT, G_TYPE_STRING]

  type(gtktreeiter), target :: iter
  type(gvalue), target :: valt, vali
  type(c_ptr) :: val


  character, pointer                            :: fstring(:)
  !PRINT *, "Working up to Line 28"

  combo_list_store = gtk_list_store_newv(ncols, c_loc(coltypes))

  val = c_loc(vali)
  val = g_value_init(val, G_TYPE_INT)
  val = c_loc(valt)
  val = g_value_init(val, G_TYPE_STRING)



  do i = 1, size(valsArray)

    call gtk_list_store_append(combo_list_store, c_loc(iter))
    call g_value_set_int(c_loc(vali), refsArray(i))
    call gtk_list_store_set_value(combo_list_store, c_loc(iter), 0_c_int, c_loc(vali))


    call convert_f_string_s(trim(valsArray(i)), textptr)

    call g_value_set_static_string(c_loc(valt), textptr)

    !call g_value_set_static_string(c_loc(valt), val_ptr)

    call gtk_list_store_set_value(combo_list_store, c_loc(iter), 1_c_int, &
         & c_loc(valt))

    print *, "String Array Value is ", trim(valsArray(i))
      !print *, trim(akeys(i)), ": ", trim(avals(i))
  end do

  !call c_f_pointer(c_loc(ID_SETTING), data_callback)

  cbox = gtk_combo_box_new_with_model_and_entry(combo_list_store)

  call gtk_combo_box_set_entry_text_column(cbox, 1_c_int)
  call gtk_combo_box_set_active(cbox, 0_c_int)



end subroutine hl_gtk_combo_box_list2_new

function hl_zoa_combo_get_selected_list2_id (widget) result(ivalue)

  use, intrinsic :: iso_c_binding, only: c_double, c_f_pointer
  use gtk_sup, only: c_f_string_copy
  use global_widgets


  type(c_ptr), value, intent(in) :: widget
  real(c_double)                 :: x, y
  character(len=50)                        :: choice
  character(len=512)             :: my_string
  type(gtktreeiter), target :: tree_iter

  type(c_ptr)  :: model

  type(c_ptr)  :: val, cstr, ival

  type(gvalue), target :: result, iresult


  type(integer)  :: tmpresult, ivalue

  integer(c_int), pointer :: interfaceData

  ! For HL function
  !character(len=*) :: hl_string


  !tree_iter = c_null_ptr
  tmpresult = gtk_combo_box_get_active_iter(widget, c_loc(tree_iter))
  !tree_iter = gtk_combo_box_get_active_iter(gdata)

  !PRINT *, "tree_iter is ", tree_iter

  model = gtk_combo_box_get_model(widget)
  val = c_loc(result)
  call gtk_tree_model_get_value(model, c_loc(tree_iter), 1_c_int, val)

  !cstr = g_value_get_string(result)

  cstr = g_value_get_string(val)
  call convert_c_string(cstr, choice)

!        cstr = g_value_get_string(val)
!          call convert_c_string(cstr, svalue)
  PRINT *, "CHOICE is ", choice

  ! Get ING
  ival = c_loc(iresult)
  call gtk_tree_model_get_value(model, c_loc(tree_iter), 0_c_int, ival)
  ivalue = g_value_get_int(ival)

  PRINT *, "Integer Index is ", ivalue


  PRINT *, "Callback works!"



end function hl_zoa_combo_get_selected_list2_id

subroutine hl_zoa_combo_set_selected_by_list2_id(widget, targetValue)
  type (c_ptr) :: widget
  integer(kind=c_int) :: targetValue
  integer :: boolResult

  type(gtktreeiter), target :: tree_iter

  type(c_ptr)  :: model, ival
  type(gvalue), target :: iresult
  type(integer)  :: ivalue

  model = gtk_combo_box_get_model(widget)
  boolResult = gtk_tree_model_get_iter_first(model, c_loc(tree_iter))

  PRINT *, "targetValue is ", targetValue




  do while(boolResult.EQ.1)
    ival = c_loc(iresult)
    call gtk_tree_model_get_value(model, c_loc(tree_iter), 0_c_int, ival)
    ivalue = g_value_get_int(ival)
    PRINT *, "ivalue is ", ivalue
  if (ivalue.EQ.targetValue) then
    PRINT *, "Found correct combo entry to display!"
    call gtk_combo_box_set_active_iter(widget, c_loc(tree_iter))
    return
  else
    boolResult = gtk_tree_model_iter_next(model, c_loc(tree_iter))
    if (boolResult.EQ.0) then
      PRINT *, "Reached end of model and no suitable matches found"
      return
    end if
  end if
  end do

!do while not at end
!get value from iteration
!if value==targetvalue
! call gtk_combo_box_set_active_iter(widget, currIter)
! else
! call gtk_tree_model_iter_next(model, iter)

end subroutine



subroutine combo_setting_callback (widget, gdata) bind(c)

  use, intrinsic :: iso_c_binding, only: c_double, c_f_pointer
  use gtk_sup, only: c_f_string_copy
  use global_widgets



  type(c_ptr), value, intent(in) :: widget, gdata
  real(c_double)                 :: x, y
  character(len=50)                        :: choice
  character(len=512)             :: my_string
  type(gtktreeiter), target :: tree_iter

  type(c_ptr)  :: model

  type(c_ptr)  :: val, cstr, ival

  type(gvalue), target :: result, iresult


  type(integer)  :: tmpresult, ivalue

  integer(c_int), pointer :: interfaceData

  ! For HL function
  !character(len=*) :: hl_string


  !PRINT *, "In callback, pointer is ", gdata
  call c_f_pointer(gdata, interfaceData)

  !PRINT *, "In callback, data is ", interfaceData
  !tree_iter = c_null_ptr
  tmpresult = gtk_combo_box_get_active_iter(widget, c_loc(tree_iter))
  !tree_iter = gtk_combo_box_get_active_iter(gdata)

  !PRINT *, "tree_iter is ", tree_iter

  model = gtk_combo_box_get_model(widget)
  val = c_loc(result)
  call gtk_tree_model_get_value(model, c_loc(tree_iter), 1_c_int, val)

  !cstr = g_value_get_string(result)

  cstr = g_value_get_string(val)
  call convert_c_string(cstr, choice)

!        cstr = g_value_get_string(val)
!          call convert_c_string(cstr, svalue)
  PRINT *, "CHOICE is ", choice

  ! Get ING
  ival = c_loc(iresult)
  call gtk_tree_model_get_value(model, c_loc(tree_iter), 0_c_int, ival)
  ivalue = g_value_get_int(ival)

  PRINT *, "Integer Index is ", ivalue


  PRINT *, "Callback works!"

  !call update_zoa_ui_settings_and_replot(ivalue)
  !call ld_settings % set_plot_orientation(ivalue)
  !
  ! if (ld_settings%changed.eq.1) THEN
  !    ld_settings%changed = 0
  !    call lens_draw_replot()
  !
  ! end if

end subroutine combo_setting_callback

subroutine hl_gtk_listn_attach_combo_box_model(view, colno, valsArray, refsArray)

   type(c_ptr) :: view
   integer(kind=c_int) :: colno, icol
   character(kind=c_char, len=*),optional, intent(in), dimension(:) :: valsArray
   integer(c_int), optional, intent(in), dimension(:) :: refsArray

   integer :: i
   type(c_ptr) :: col, rlist, renderer
   integer(kind=c_int), parameter :: ncols=2
   integer(kind=type_kind), dimension(ncols), target :: coltypes = &
       & [G_TYPE_INT, G_TYPE_STRING]
   type(gvalue), target :: valt, vali
   type(c_ptr) :: val
   type(gtktreeiter), target :: iter
   character(kind=c_char), dimension(:), allocatable :: textptr
    type(gvalue), target :: modelv, columnv
    type(c_ptr) :: pmodel, pcolumn, model

    ! Find the renderer for the column
    col = gtk_tree_view_get_column(view, colno)
    rlist = gtk_cell_layout_get_cells(col)
    renderer = g_list_nth_data(rlist, 0_c_int)
    call g_list_free(rlist)

    ! Create Model
    model = gtk_list_store_newv(ncols, c_loc(coltypes))
    icol = 0

    val = c_loc(vali)
    val = g_value_init(val, G_TYPE_INT)
    val = c_loc(valt)
    val = g_value_init(val, G_TYPE_STRING)



  do i = 1, size(valsArray)

      call gtk_list_store_append(model, c_loc(iter))
      call g_value_set_int(c_loc(vali), refsArray(i))
      call gtk_list_store_set_value(model, c_loc(iter), 0_c_int, c_loc(vali))

      call convert_f_string_s(trim(valsArray(i)), textptr)

      call g_value_set_static_string(c_loc(valt), textptr)

      call gtk_list_store_set_value(model, c_loc(iter), 1_c_int, &
           & c_loc(valt))

    end do

    ! Attach it to the renderer.
    pmodel = c_loc(modelv)
    pmodel = g_value_init(pmodel, gtk_tree_model_get_type())
    call g_value_set_object(pmodel, model)
    call g_object_set_property(renderer, "model"//c_null_char, pmodel)
  !  call g_object_set_property(renderer, "model"//c_null_char, model)



    ! Tell the renderer that the text is in column 1
    pcolumn = c_loc(columnv)
    pcolumn = g_value_init(pcolumn, G_TYPE_INT)
    call g_value_set_int(pcolumn, 1)
    call g_object_set_property(renderer, "text-column"//c_null_char, &
         & pcolumn)


end subroutine


subroutine hl_gtk_listn_combo_set_by_list_id(view, row, colno, targetValue)
  type(c_ptr), intent(in) :: view
  integer(kind=c_int), intent(in) :: row, colno, targetValue
  integer(kind=c_int) :: ivalue
  type(gvalue), target :: iresult

  ! Set the selected item in a combo cell renderer.
  !
  ! VIEW: c_ptr: required: The list view containing the cell.
  ! ROW: int: required: The row number of the cell
  ! COLNO: int: required: The column number with the cell
  ! SELECTION: int: required: The element of the combo to set.
  !-

  type(c_ptr) :: store, pstring, col, rlist, renderer, pmodel, model, ival
  type(gvalue), target :: stringv, modelv
  type(gtktreeiter), target :: viter, citer
  integer(kind=c_int) :: valid
  integer :: boolResult


  ! Get list store
  store = gtk_tree_view_get_model(view)

  ! Get the iterator of the row
  call clear_gtktreeiter(viter)
  valid = gtk_tree_model_iter_nth_child(store, c_loc(viter), C_NULL_PTR, row)
  if (.not. c_f_logical(valid)) return

  ! Find the renderer for the column
  col = gtk_tree_view_get_column(view, colno)
  rlist = gtk_cell_layout_get_cells(col)
  renderer = g_list_nth_data(rlist, 0_c_int)
  call g_list_free(rlist)

  ! Find the model for the combobox
  pmodel = c_loc(modelv)
  pmodel = g_value_init(pmodel, gtk_tree_model_get_type())

  call g_object_get_property(renderer, "model"//c_null_char, pmodel)
  model = g_value_get_object(pmodel)

  boolResult = gtk_tree_model_get_iter_first(model, c_loc(citer))

do while(boolResult.EQ.1)
  PRINT *, "Before error?"
  ival = c_loc(iresult)
  PRINT *, "After ival"
  call gtk_tree_model_get_value(model, c_loc(citer), 0_c_int, ival)
  PRINT *, "after get valu"
  ivalue = g_value_get_int(ival)
  PRINT *, "after get_int"
if (ivalue.EQ.targetValue) then
     PRINT *, "Found correct combo entry to display!"
     pstring = c_loc(stringv)
     pstring = g_value_init(pstring, G_TYPE_STRING)
     call g_value_unset(pstring)

     call gtk_tree_model_get_value(model, c_loc(citer), 1_c_int, pstring)
     call gtk_list_store_set_value(store, c_loc(viter), colno, pstring)

  !call gtk_combo_box_set_active_iter(widget, c_loc(tree_iter))
  return
else
  call g_value_unset(ival)
  boolResult = gtk_tree_model_iter_next(model, c_loc(citer))
  if (boolResult.EQ.0) then
    PRINT *, "Reached end of model and no suitable matches found"
    return
  end if
end if
end do

  !
  ! call clear_gtktreeiter(citer)
  ! valid = gtk_tree_model_iter_nth_child(model, c_loc(citer), &
  !      & c_null_ptr, selection)
  ! if (c_f_logical(valid)) then
  !    pstring = c_loc(stringv)
  !    pstring = g_value_init(pstring, G_TYPE_STRING)
  !    call g_value_unset(pstring)
  !   ! call gtk_tree_model_get_value(model, c_loc(citer), 0_c_int, pstring)
  !   ! JN:  Hack to test my model.  TODO:  Do not leave it like this!!!
  !    call gtk_tree_model_get_value(model, c_loc(citer), 1_c_int, pstring)
  !    call gtk_list_store_set_value(store, c_loc(viter), colno, pstring)
  ! end if


end subroutine

subroutine set_listn_column_color(view, col, colorTxt)

   implicit none

   type(c_ptr) :: view
   integer(kind=c_int) :: col
   character(len=*) :: colorTxt

   integer :: i
   type(c_ptr) :: colptr, rlist, renderer
    type(gvalue), target :: svalue
    type(c_ptr) :: val_ptr



    ! Find the renderer for the column
    colptr = gtk_tree_view_get_column(view, col)
    rlist = gtk_cell_layout_get_cells(colptr)
    renderer = g_list_nth_data(rlist, 0_c_int)
    call g_list_free(rlist)
    !
    ! ! Find the model for the combobox
    ! pmodel = c_loc(modelv)
    ! pmodel = g_value_init(pmodel, gtk_tree_model_get_type())
    ! call g_object_get_property(renderer, "model"//c_null_char, pmodel)
    ! model = g_value_get_object(pmodel)
    !
    !
     !rstring = "orange"
    val_ptr = c_loc(svalue)
    val_ptr = g_value_init(val_ptr, G_TYPE_STRING)

    PRINT *, "Updating column with color ", colorTxt
    call g_value_set_string(val_ptr, trim(colorTxt)//c_null_char)
    call g_object_set_property(renderer, "background"//c_null_char, val_ptr)
    !
end subroutine

end module hl_gtk_zoa
