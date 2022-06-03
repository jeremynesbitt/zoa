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


  PRINT *, "In callback, pointer is ", gdata
  call c_f_pointer(gdata, interfaceData)

  PRINT *, "In callback, data is ", interfaceData
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
  call ld_settings % set_plot_orientation(ivalue)
  !
  ! if (ld_settings%changed.eq.1) THEN
  !    ld_settings%changed = 0
  !    call lens_draw_replot()
  !
  ! end if

end subroutine combo_setting_callback


end module hl_gtk_zoa
