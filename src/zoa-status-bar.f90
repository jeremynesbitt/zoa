! Status bar to info in current lens system
module zoa_status_bar
    use iso_c_binding
    use gtk

    !type status_bar_data
    !character(len=1024), dimension(4) :: textData
    !type(c_ptr), dimension(4) :: labels
    !end type

    !type(status_bar_data) :: curr_status_bar_data

    type(c_ptr), dimension(4) :: labels

    contains
    function createStatusBar() result (uiObj)
        
        implicit none
        type(c_ptr) :: uiObj, table, e1,e2,e3,e4

        ! Try to create a table with 4 columns of non-editable entries

        table = gtk_grid_new ()
        call gtk_grid_set_column_homogeneous(table, TRUE)
        call gtk_grid_set_row_homogeneous(table, TRUE)

        labels(1) = gtk_label_new("Test1"//c_null_char)
        labels(2) = gtk_label_new("Test2"//c_null_char)
        labels(3) = gtk_label_new("Test3"//c_null_char)
        labels(4) = gtk_label_new(" "//c_null_char)

        call gtk_grid_attach(table, labels(1), 0_c_int, 0_c_int, 1_c_int, 1_c_int)
        call gtk_grid_attach(table, labels(2), 1_c_int, 0_c_int, 1_c_int, 1_c_int)
        call gtk_grid_attach(table, labels(3), 2_c_int, 0_c_int, 1_c_int, 1_c_int)
        call gtk_grid_attach(table, labels(4), 3_c_int, 0_c_int, 1_c_int, 1_c_int)

        uiObj = gtk_box_new (GTK_ORIENTATION_VERTICAL, 5_c_int)
        call gtk_box_append(uiObj, table)
    

    end function


    subroutine updateStatusBar(idx, strVal)
        implicit none
        integer, intent(in) :: idx
        character(len=*), intent(in) :: strVal
        call gtk_label_set_label(labels(idx), strVal//c_null_char)

    end subroutine

end module