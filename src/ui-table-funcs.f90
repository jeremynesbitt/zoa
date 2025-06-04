! This module is to help set up and maintain tables used in lens editor and optimizer uis
module ui_table_funcs
    use iso_c_binding
    use gtk
    use g
    use handlers, only: pending_events

    ! For setting up ui table
    ! abstract interface
    ! subroutine setCoumns(cv)
    !     import c_ptr
    !     type(c_ptr), value :: cv

    ! end subroutine
    ! end interface

    contains

    subroutine setColumnViewDefault(cv, setColumnFunc) 
        interface setColumns 
        subroutine setColumns(cv)
            import c_ptr 
            type(c_ptr), value :: cv 
        end subroutine
        end interface
        type(c_ptr) :: cv
        type(c_ptr) :: store, selection
        procedure(setColumns) :: setColumnFunc

        call gtk_column_view_set_show_column_separators(cv, 1_c_int)
        call gtk_column_view_set_show_row_separators(cv, 1_c_int)
        call gtk_column_view_set_reorderable(cv, 0_c_int)
    
        call setColumnFunc(cv)
        !call setConstraintColumns(cv)

    end subroutine

    subroutine rebuildTable(cv, store, setColumnFunc)
        interface setColumns 
        subroutine setColumns(cv)
            import c_ptr 
            type(c_ptr), value :: cv 
        end subroutine
        end interface        
        procedure(setColumns) :: setColumnFunc
        type(c_ptr) :: cv ! column view
        type(c_ptr) :: store, selection, model, column, vadj, hadj, swin
        integer(c_int) :: oldPos 
        real(c_double) :: vPos, hPos
        logical :: boolResult
     
               
        ! Get current position
        oldPos = getCurrentTableRow(cv)
        print *, "Old Position is ", oldPos
      
        ! Get location of horizontal and vertical scrollbars so we can recreate.  This seems crazy
        swin = gtk_widget_get_parent(cv) 
        vadj = gtk_scrolled_window_get_vadjustment(swin)
        vPos = gtk_adjustment_get_value(vadj)
        hadj = gtk_scrolled_window_get_hadjustment(swin)
        hPos = gtk_adjustment_get_value(hadj)
      
        print *, "Value is ", gtk_adjustment_get_value(vadj)
      
        call clearColumnView_opt(cv)

        !store = buildConstraintTable()
        !selection = gtk_multi_selection_new(store)
        selection = gtk_single_selection_new(store)

        call gtk_column_view_set_model(cv, selection)

        call setColumnViewDefault(cv, setColumnFunc)
      
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

      function getCurrentTableRow(cv) result(currPos)
        integer(c_int) :: currPos, numRows, isSelected, ii
        type(c_ptr) :: cv
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

      subroutine clearColumnView_opt(cv)
        type(c_ptr) :: listModel, currCol
        type(c_ptr), intent(inout) :: cv
        integer(c_int) :: ii, numItems
      
        listmodel = gtk_column_view_get_columns(cv)
        numItems = g_list_model_get_n_items(listmodel) -1
        do ii=numItems,0,-1
          currCol = g_list_model_get_object(listmodel,ii)
          call gtk_column_view_remove_column(cv,currCol)
          call g_object_unref(currCol)
      
        end do
      
      end subroutine          


      subroutine getRowAndColumnFromStrPtr(cStr, rowIdx, colIdx) 
        use type_utils
        use gtk_sup, only: convert_c_string
        type(c_ptr), value :: cStr
        character(len=100) :: rcCode
        integer :: rowIdx
        integer, optional, intent(inout) :: colIdx
        integer :: rL, cL

        call convert_c_string(cStr, rcCode)  
      
        rL = index(rcCode, 'R')
        cL = index(rcCode, 'C')
        !print *, "row is ", rcCode(rL+1:cL)
        rowIdx = str2int(rcCode(rL+1:cL-1))
        if (present(colIdx)) then 
          colIdx = str2int(rcCode(cL+1:len(rcCode)))
        end if
        !print *, "surfIdx is ", surfIdx
      
    end subroutine      


end module