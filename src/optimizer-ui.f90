module optimizer_ui
    use handlers
    !use hl_gtk_zoa
    use gtk_hl_container
    use iso_c_binding
    use global_widgets
    use optim_types
    use zoa_ui
    use ui_table_funcs

    implicit none

    interface

    function append_operand_model(store, operandName, waveLength, fieldPos,  &
      & pupilX, pupilY, fieldX, fieldY, targ, currValue) bind(c)
      import c_ptr, c_char, c_int, c_double
      implicit none
      type(c_ptr), value    :: store
      integer(c_int), value :: waveLength, fieldPos
      character(kind=c_char), dimension(*) :: operandName
      type(c_ptr)    :: append_operand_model
      real(c_double), value :: pupilX, pupilY, fieldX, fieldY, targ, currValue
    end function    

    function append_blank_operand(store) bind(c)
        import c_ptr 
        implicit none
        type(c_ptr), value :: store 
        type(c_ptr)    :: append_blank_operand

    end function
    function operand_item_get_name(item) bind(c)
        import :: c_ptr
        type(c_ptr), value :: item
        type(c_ptr) :: operand_item_get_name
      end function   
       function operand_item_get_wavelength(item) bind(c)
        import :: c_ptr, c_int
        type(c_ptr), value :: item
        integer(c_int) :: operand_item_get_wavelength
      end function  
      function operand_item_get_fieldpos(item) bind(c)
        import :: c_ptr, c_int
        type(c_ptr), value :: item
        integer(c_int) :: operand_item_get_fieldPos
      end function        


! Constraints
      function append_constraint_model(store, constraintName, conValue, contype,  &
        & targ) bind(c)
        import c_ptr, c_char, c_int, c_double
        implicit none
        type(c_ptr), value    :: store
        character(kind=c_char), dimension(*) :: constraintName
        type(c_ptr)    :: append_constraint_model
        integer(c_int), value :: contype
        real(c_double), value :: conValue, targ
      end function    
  
      function append_blank_constraint(store) bind(c)
          import c_ptr 
          implicit none
          type(c_ptr), value :: store 
          type(c_ptr)    :: append_blank_constraint
      end function
      function constraint_item_get_name(item) bind(c)
        import :: c_ptr
        type(c_ptr), value :: item
        type(c_ptr) :: constraint_item_get_name
      end function   
       function constraint_item_get_value(item) bind(c)
        import :: c_ptr, c_double
        type(c_ptr), value :: item
        real(c_double) :: constraint_item_get_value
      end function  
      function constraint_item_get_target(item) bind(c)
        import :: c_ptr, c_double
        type(c_ptr), value :: item
        real(c_double) :: constraint_item_get_target
      end function   
      function constraint_item_get_contype(item) bind(c)
        import :: c_ptr, c_int
        type(c_ptr), value :: item
        integer(c_int) :: constraint_item_get_conType
      end function              

    end interface

    

    type uiTableColumnInfo
        character(len=20) :: colName
        integer :: colType ! combo, exit, etc
        integer :: dataType ! real, str, double
        procedure(getItemValue_int), pointer, nopass:: getFunc_int
        procedure(getItemValue_str), pointer, nopass:: getFunc_str
        procedure(getItemValue_dbl), pointer, nopass:: getFunc_dbl

        contains

    end type

    abstract interface
    function getItemValue_int(item) bind(c)
      import :: uiTableColumnInfo, c_ptr, c_int
     type(c_ptr), value :: item
     integer(c_int) :: getItemValue_int
    end function 
    function getItemValue_str(item) bind(c)
      import :: uiTableColumnInfo, c_ptr
     type(c_ptr), value :: item
     type(c_ptr) :: getItemValue_str
    end function    
    function getItemValue_dbl(item) bind(c)
      import :: uiTableColumnInfo, c_ptr, c_double
     type(c_ptr), value :: item
     real(c_double) :: getItemValue_dbl
    end function         
  end interface



    ! For now add some vars for column names and types.  WOuld like a more elegant solution
    integer, parameter :: ID_OPERAND_NAME_COL = 1
    integer, parameter :: ID_OPERAND_WL_COL = 2
    integer, parameter :: ID_OPERAND_FIELD_COL = 3


    integer, parameter :: ID_DATATYPE_STR = 1
    integer, parameter :: ID_DATATYPE_INT = 2
    integer, parameter :: ID_DATATYPE_DBL = 3

    integer, parameter :: ID_WIDGET_TYPE_LABEL = 4001
    integer, parameter :: ID_WIDGET_TYPE_DROPDOWN = 4002
    integer, parameter :: ID_WIDGET_TYPE_ENTRY = 4003


    integer, parameter :: ID_CONSTRAINT_NAME_COL = 1
    integer, parameter :: ID_CONSTRAINT_VALUE_COL = 3
    integer, parameter :: ID_CONSTRAINT_TYPE_COL = 2

    type(uiTableColumnInfo) :: operandColInfo(3)
    type(uiTableColumnInfo) :: constraintColInfo(3)    

    contains

    ! function tstWLInterface(item) result(intWL)
    !     type(c_ptr), value :: item 
    !     integer(c_int) :: intWL

    !     intWL =  operand_item_get_wavelength(0)


    ! end function

    subroutine optimizer_ui_new(parent_window)

        type(c_ptr) :: parent_window
        !type(c_ptr), value :: lens_editor_window
    
        type(c_ptr) :: content, junk, gfilter
        integer(kind=c_int) :: icreate, idir, action, lval
        integer(kind=c_int) :: i, idx0, idx1, pageIdx
        !integer(c_int)  :: width, height
    
        type(c_ptr)  :: table, expander, nbk, basicLabel, boxAperture, boxAsphere
        type(c_ptr)  :: box1, box2, box3
        type(c_ptr)  :: boxSolve, SolveLabel, conLabel
        type(c_ptr)  :: lblAperture, AsphLabel
    
        PRINT *, "ABOUT TO FIRE UP OPTIMIZIER WINDOW!"
    

        ! Build operaand info
        operandColInfo(ID_OPERAND_NAME_COL )%colName = "Name"
        operandColInfo(ID_OPERAND_NAME_COL)%colType = ID_WIDGET_TYPE_LABEL
        operandColInfo(ID_OPERAND_NAME_COL)%dataType = ID_DATATYPE_STR
        operandColInfo(ID_OPERAND_NAME_COL)%getFunc_str => operand_item_get_name
    
        operandColInfo(ID_OPERAND_WL_COL)%colName = "Wavelength"
        operandColInfo(ID_OPERAND_WL_COL)%colType = ID_WIDGET_TYPE_LABEL
        operandColInfo(ID_OPERAND_WL_COL)%dataType = ID_DATATYPE_INT
        !operandColInfo(ID_OPERAND_WL_COL)%getFunc => tstWLInterface
        operandColInfo(ID_OPERAND_WL_COL)%getFunc_int => operand_item_get_wavelength
    
        operandColInfo(ID_OPERAND_FIELD_COL)%colName = "Field"
        operandColInfo(ID_OPERAND_FIELD_COL)%colType = ID_WIDGET_TYPE_LABEL    
        operandColInfo(ID_OPERAND_FIELD_COL)%dataType = ID_DATATYPE_INT    
        operandColInfo(ID_OPERAND_FIELD_COL)%getFunc_int => operand_item_get_fieldpos

        call initConstraintColInfo()
        ! Create a modal dialogue
        optimizer_window = gtk_window_new()
    
            !PRINT *, "LENS EDITOR WINDOW PTR IS ", lens_editor_window
    
        !call gtk_window_set_modal(di, TRUE)
        !title = "Lens Draw Window"
        !if (present(title)) call gtk_window_set_title(dialog, title)
        call gtk_window_set_title(optimizer_window, "Optimizer Setup"//c_null_char)

    
        width = 700
        height = 400
           call gtk_window_set_default_size(optimizer_window, width, height)
        !end if
    
        !if (present(parent)) then
           call gtk_window_set_transient_for(optimizer_window, parent_window)
           call gtk_window_set_destroy_with_parent(optimizer_window, TRUE)
        !end if
    
        ! Temp
        box1 = hl_gtk_box_new()
        box2 = hl_gtk_box_new()
        box3 = hl_gtk_box_new()


        !call lens_editor_basic_dialog(box1)
    
    
        !call lens_editor_asphere_dialog(boxAsphere)
    
        !boxSolve = lens_editor_add_dialog(ID_EDIT_SOLVE)
    
        nbk = gtk_notebook_new()
        basicLabel = gtk_label_new_with_mnemonic("_General"//c_null_char)
        pageIdx = gtk_notebook_append_page(nbk, box1, basicLabel)
    
        AsphLabel = gtk_label_new_with_mnemonic("_Operands"//c_null_char)
        pageIdx = gtk_notebook_append_page(nbk, operands_create_table(), AsphLabel)

        conLabel = gtk_label_new_with_mnemonic("_Constraints"//c_null_char)
        pageIdx = gtk_notebook_append_page(nbk, constraints_create_table(), conLabel)        
    
        SolveLabel = gtk_label_new_with_mnemonic("_Merit Function"//c_null_char)
        pageIdx = gtk_notebook_append_page(nbk, box3, SolveLabel)
    
        PRINT *, "FINISHED WITH OPTIMIZER WINDOW"
        !call gtk_box_append(box1, rf_cairo_drawing_area)
        !call gtk_window_set_child(lens_editor_window, rf_cairo_drawing_area)
        call gtk_window_set_child(optimizer_window, nbk)
    
    
        !call g_signal_connect(lens_editor_window, "destroy"//c_null_char, c_funloc(lens_editor_destroy), lens_editor_window)
    
    
        call gtk_window_set_mnemonics_visible (optimizer_window, TRUE)
        !call gtk_widget_queue_draw(my_drawing_area)
        call gtk_widget_show(optimizer_window)

    end subroutine optimizer_ui_new

    subroutine del_optimizer_row(but, gdata) bind(c)
        use type_utils, only: int2str
        type(c_ptr), value, intent(in) :: but, gdata
        integer(kind=c_int) :: currRow
    end subroutine
 
    subroutine ins_optimizer_row(but, gdata) bind(c)
        use type_utils, only: int2str
          type(c_ptr), value, intent(in) :: but, gdata
          integer(kind=c_int) :: currRow
    end subroutine

    subroutine optimizer_ui_destroy(widget, gdata) bind(c)

        type(c_ptr), value :: widget, gdata
        type(c_ptr) :: isurface
        print *, "Exit called"
    
    
        !call cairo_destroy(rf_cairo_drawing_area)
        !call gtk_widget_unparent(gdata)
        !call g_object_unref(rf_cairo_drawing_area)
        call gtk_window_destroy(gdata)
    
        optimizer_window = c_null_ptr
    
      end subroutine 

    function operands_create_table() result(boxNew)
        ! Columns:
        ! Operand Name [dropdown]
        ! Operand Type (contraint or weighted) unedited text (fcn of name)
        ! Constraint -dropdown for > < = 
        ! Constraint target (also target for weighted operand?)
        ! Weight (not implemented yet?)  editable if weighted, greyed out and empty if contraint
        ! For future use:  Field X Field Y Pupil X Pupil Y Wavelength

        use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char
    
        type(integer) :: ID_TAB
        type(c_ptr) :: boxNew
    
        !Debug
        integer :: ii
        integer, target :: colIDs(10) = [(ii,ii=1,10)]
    
        type(c_ptr) :: store, cStrB, listitem, selection, factory, column, swin
        character(len=1024) :: debugName
    
        type(c_ptr) :: cv, dbut, ibut, qbut
    
    
        boxNew = hl_gtk_box_new()
    
    
          store = buildOperandTable()
        
      
          selection = gtk_multi_selection_new(store)
          !call g_signal_connect(selection, 'selection-changed'//c_null_char, c_funloc(lens_edit_row_selected), c_null_ptr)      !selection = gtk_multi_selection_new(store)
          call gtk_single_selection_set_autoselect(selection,TRUE)    
          cv = gtk_column_view_new(selection)
          call gtk_column_view_set_show_column_separators(cv, 1_c_int)
          call gtk_column_view_set_show_row_separators(cv, 1_c_int)
          call gtk_column_view_set_reorderable(cv, 0_c_int)
    
    
          call setOperandColumns(cv)
    
          swin = gtk_scrolled_window_new()
          call gtk_scrolled_window_set_child(swin, cv)
          call gtk_scrolled_window_set_min_content_height(swin, 300_c_int) !TODO:  Fix this properly 
          call gtk_box_append(boxNew, swin)
    
        ! Delete selected row
          ibut = hl_gtk_button_new("Insert row"//c_null_char, &
          & clicked=c_funloc(ins_optimizer_row), &
          & tooltip="Insert new row above"//c_null_char, sensitive=FALSE)
    
          call hl_gtk_box_pack(boxNew, ibut)
    
          ! Delete selected row
          dbut = hl_gtk_button_new("Delete selected row"//c_null_char, &
                & clicked=c_funloc(del_optimizer_row), &
                & tooltip="Delete the selected row"//c_null_char, sensitive=FALSE)
    
          call hl_gtk_box_pack(boxNew, dbut)
    
          ! Also a quit button
          qbut = hl_gtk_button_new("Quit"//c_null_char, clicked=c_funloc(optimizer_ui_destroy), data=optimizer_window)
          call hl_gtk_box_pack(boxNew,qbut)
    
    
      end function   

! This func interfaces with the c struct that stores the data
! At some point I may migrate this to fortran but for now the
! main cost of this is a bunch of interfaces for each get which
! I can live with
      function buildOperandTable() result(store)
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
        integer :: numOperands, numRows
      
       
        numOperands = nO

        ! Set some minimum amount 
        if (numOperands < 20) then 
            numRows = 20
        else
            numRows = numOperands
        end if
      

          print *, "number of Operands is ", getTotalNumberOfOperands()
          store = g_list_store_new(G_TYPE_OBJECT)
          do i=1,numRows
            if (i <= numOperands ) then
          store = append_operand_model(store, operandsInUse(i)%name, operandsInUse(i)%iW, operandsInUse(i)%iF, &
          & operandsInUse(i)%px, operandsInUse(i)%py, operandsInUse(i)%hx, operandsInUse(i)%hy, &
          & operandsInUse(i)%targ, operandsInUse(i)%op)
            else 
                print *, "Before list store blank error?"
                store = append_blank_operand(store)
            end if
          end do
      
      
        end function

        subroutine setOperandColumns(colView)
            use type_utils, only: int2str
            type(c_ptr), value :: colView
          
            integer :: ii
            integer, target :: colIDs(25) = [(ii,ii=1,25)]
            type(c_ptr) :: factory, column


            do ii=1,size(operandColInfo)
              factory = gtk_signal_list_item_factory_new()
              print *, "Before setup"
              call g_signal_connect(factory, "setup"//c_null_char, c_funloc(setup_operand_cb),c_loc(colIDs(ii)))
              print *, "Before bind"
              call g_signal_connect(factory, "bind"//c_null_char, c_funloc(bind_operand_cb),c_loc(colIDs(ii)))
              column = gtk_column_view_column_new(trim(operandColInfo(ii)%colName)//c_null_char, factory)
              call gtk_column_view_column_set_id(column, trim(int2str(colIDs(ii))))
              call gtk_column_view_column_set_resizable(column, 1_c_int)
              call gtk_column_view_append_column (colView, column)
              call g_object_unref (column)      
            end do
          
          end subroutine
          


        subroutine setup_operand_cb(factory,listitem, gdata) bind(c)
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

            select case (operandColInfo(ID_COL)%colType)
            case (ID_WIDGET_TYPE_LABEL)
                label =gtk_label_new(c_null_char)
                call gtk_list_item_set_child(listitem,label)                

            end select
          
            !select case (ID_COL)
          
            ! case(2)
            !    label = gtk_check_button_new()
            !    if (.not.c_associated(refRadio)) refRadio = label
            !    call gtk_check_button_set_group(label, refRadio)
            !    call gtk_list_item_set_child(listitem,label)
            ! case(3) ! Surface Label   
            !   boxS = hl_gtk_box_new(horizontal=TRUE, spacing=0_c_int)
            !   entryCB = hl_gtk_entry_new(10_c_int, editable=TRUE, activate=c_funloc(cell_changed), data=g_strdup('SLB'))
            !   call gtk_box_append(boxS, entryCB)
            !   call gtk_list_item_set_child(listitem, boxS)         
            ! case(4)
            !   dropDown = gtk_drop_down_new_from_strings(getSurfaceTypesAsCStringArray())
            !   call gtk_list_item_set_child(listitem, dropDown)   
          
            !  case(5:6) ! Radius or Thickness + modifier
            !   boxS = hl_gtk_box_new(horizontal=TRUE, spacing=0_c_int)
            !   menuB = gtk_menu_button_new()
            !   cmd = 'RDY'
            !   if(ID_COL==6) cmd = 'THI'
            !   entryCB = hl_gtk_entry_new(10_c_int, editable=TRUE, activate=c_funloc(cell_changed), data=g_strdup(cmd//c_null_char))
            !   !call gtk_widget_set_name(entryCB, cmd) 
            !   call gtk_box_append(boxS, entryCB)
            !   call gtk_box_append(boxS, menuB)
            !   call gtk_list_item_set_child(listitem,boxS)
            !  case(7) ! Glass
            !   boxS = hl_gtk_box_new(horizontal=TRUE, spacing=0_c_int)
            !   entryCB = hl_gtk_entry_new(10_c_int, editable=TRUE, activate=c_funloc(cell_changed), data=g_strdup('GLA'))
            !   call gtk_box_append(boxS, entryCB)
            !   call gtk_list_item_set_child(listitem, boxS)       
            ! case(8) ! Aperture
            !   boxS = hl_gtk_box_new(horizontal=TRUE, spacing=0_c_int)
            !   entryCB = hl_gtk_entry_new(10_c_int, editable=TRUE, activate=c_funloc(cell_changed), data=g_strdup('CIR'))
            !   call gtk_box_append(boxS, entryCB)
            !   call gtk_list_item_set_child(listitem, boxS)      
              
            !  case default 
            !   label =gtk_label_new(c_null_char)
            !   call gtk_list_item_set_child(listitem,label)
            !  end select 
          end subroutine  
          
          
          subroutine bind_operand_cb(factory,listitem, gdata) bind(c)
            use type_utils
            type(c_ptr), value :: factory
            type(c_ptr), value :: listitem, gdata
            type(c_ptr) :: widget, item, label, buffer, entryCB, menuCB
            type(c_ptr) :: cStr
            integer(kind=c_int), pointer :: ID_COL
            character(len=140) :: colName
            class(*), pointer :: tmpPtr
          
            call c_f_pointer(gdata, ID_COL)
            label = gtk_list_item_get_child(listitem)
            item = gtk_list_item_get_item(listitem);
          
            select case (operandColInfo(ID_COL)%colType)
            case (ID_WIDGET_TYPE_LABEL)

                select case (operandColInfo(ID_COL)%dataType)
                    case (ID_DATATYPE_INT)
                    colName = trim(int2str(operandColInfo(ID_COL)%getFunc_int(item)))
                       
                    case (ID_DATATYPE_STR)
                        cStr = operandColInfo(ID_COL)%getFunc_str(item)
                        call convert_c_string(cStr, colName)                        

                end select

                call gtk_label_set_text(label, trim(colName)//c_null_char)   
                ! colName = trim(int2str(operandColInfo(ID_COL)%getFunc(item)))//c_null_char
                ! print *, "colName is ", trim(colName)
                ! call gtk_label_set_text(label, trim(colName)//c_null_char)       

            end select


            ! select case (ID_COL)
          
            ! case(1)
            !   colName = trim(int2str(lens_item_get_surface_number(item)))//c_null_char
            !   call gtk_label_set_text(label, trim(colName)//c_null_char)
            !  case(2)
            !   if (lens_item_get_ref_surf(item) == 1_c_int) then
            !     call gtk_check_button_set_active(label, 1_c_int)
            !   end if
            !   !colName = trim(int2str(lens_item_get_ref_surf(item)))//c_null_char
            !   !call gtk_label_set_text(label, trim(colName)//c_null_char)
            !  case(3)
            !   cStr = lens_item_get_surface_name(item)
            !   call convert_c_string(cStr, colName)
            !   entryCB = gtk_widget_get_first_child(label)  
            !   buffer = gtk_entry_get_buffer(entryCB)    
            !   call gtk_entry_buffer_set_text(buffer, trim(colName)//c_null_char,-1_c_int)
            !  case(4)
            !   cStr = lens_item_get_surface_type(item)
            !   call convert_c_string(cStr, colName)  
            !   ! Will need to abstract this when more types are added
            !   if(colName=='Sphere') then
            !     call gtk_drop_down_set_selected(label, 0_c_int)
            !   end if
            !   if(colName=='Asphere') then 
            !     call gtk_drop_down_set_selected(label, 1_c_int)
            !   end if
            !   !call gtk_label_set_text(label, trim(colName)//c_null_char)    
            !  case(5)
            !   colName = trim(real2str(lens_item_get_surface_radius(item)))//c_null_char  
            !   entryCB = gtk_widget_get_first_child(label)  
            !   buffer = gtk_entry_get_buffer(entryCB)
            !   !buffer = gtk_entry_get_buffer(label)
            !   call gtk_entry_buffer_set_text(buffer, trim(colName)//c_null_char,-1_c_int)
          
            !   menuCB = gtk_widget_get_next_sibling(entryCB)
           
            !   colName = trim(int2str(lens_item_get_radius_mod(item)))//c_null_char   
            !   call gtk_menu_button_set_menu_model(menuCB, createModMenu(menuCB, lens_item_get_surface_number(item), ID_COL)) 
            !   select case (lens_item_get_radius_mod(item))
            !   case (ID_MOD_NONE)
            !     call gtk_menu_button_set_icon_name(menuCB, 'letter-blank'//c_null_char)      
            !   case (ID_MOD_PICKUP)
            !     call gtk_menu_button_set_icon_name(menuCB, 'letter-p'//c_null_char)
            !   case (ID_MOD_SOLVE)
            !     call gtk_menu_button_set_icon_name(menuCB, 'letter-s'//c_null_char)
            !   case (ID_MOD_VAR)
            !     call gtk_menu_button_set_icon_name(menuCB, 'letter-v'//c_null_char)      
            !   end select   
          
          
            ! case(6) ! Thickness
            !   colName = trim(real2str(lens_item_get_surface_thickness(item)))//c_null_char  
            !   entryCB = gtk_widget_get_first_child(label)  
            !   buffer = gtk_entry_get_buffer(entryCB)
            !   !buffer = gtk_entry_get_buffer(label)
            !   if (lens_item_get_surface_thickness(item) > 1e13) then 
            !     call gtk_entry_buffer_set_text(buffer, "Infinity"//c_null_char,-1_c_int)
            !   else
            !     call gtk_entry_buffer_set_text(buffer, trim(colName)//c_null_char,-1_c_int)
            !   end if    
            !   menuCB = gtk_widget_get_next_sibling(entryCB)
            !   colName = trim(int2str(lens_item_get_thickness_mod(item)))//c_null_char   
            !   call gtk_menu_button_set_menu_model(menuCB, createModMenu(menuCB, lens_item_get_surface_number(item), ID_COL)) 
            !   select case (lens_item_get_thickness_mod(item))
            !   case (ID_MOD_NONE)
            !     call gtk_menu_button_set_icon_name(menuCB, 'letter-blank'//c_null_char)      
            !   case (ID_MOD_PICKUP)
            !     call gtk_menu_button_set_icon_name(menuCB, 'letter-p'//c_null_char)
            !   case (ID_MOD_SOLVE)
            !     call gtk_menu_button_set_icon_name(menuCB, 'letter-s'//c_null_char)  
            !   case (ID_MOD_VAR)
            !     call gtk_menu_button_set_icon_name(menuCB, 'letter-v'//c_null_char)            
            !   end select   
            ! case(7) ! Glass
            !   cStr = lens_item_get_glass(item)
            !   call convert_c_string(cStr, colName)
            !   entryCB = gtk_widget_get_first_child(label)  
            !   buffer = gtk_entry_get_buffer(entryCB)    
            !   call gtk_entry_buffer_set_text(buffer, trim(colName),-1_c_int)
            ! case(8) ! Clear Aperture
            !   colName = trim(real2str(lens_item_get_aperture(item)))//c_null_char  
            !   entryCB = gtk_widget_get_first_child(label)  
            !   buffer = gtk_entry_get_buffer(entryCB)    
            !   call gtk_entry_buffer_set_text(buffer, trim(colName),-1_c_int)
            ! case(9:18)
            !   entryCB = gtk_widget_get_first_child(label)  
            !   buffer = gtk_entry_get_buffer(entryCB)
            !   if (abs(lens_item_get_extra_param(item, ID_COL-9)) < .01) then 
            !       colName = trim(real2str(lens_item_get_extra_param(item, ID_COL-9),sci=.TRUE.))//c_null_char
            !   else
            !       colName = trim(real2str(lens_item_get_extra_param(item, ID_COL-9)))//c_null_char
            !   end if
            !   call gtk_entry_buffer_set_text(buffer, trim(colName),-1_c_int)     
            !  end select
          
            !   ! Encode row and column for later use  
            !    call gtk_widget_set_name(label,"R"//trim(int2str(lens_item_get_surface_number(item)))//"C"//trim(int2str(ID_COL))//c_null_char)
          
            !    if (ID_COL == 4) then 
            !    call g_signal_connect(label, "notify::selected"//c_null_char, c_funloc(updateSurfaceType), c_null_ptr)
            !    end if
          
               !print *, "Testing... ", lens_item_get_extra_param(item, 0_c_int)
               !print *, "Testing... ", lens_item_get_aperture(item)
          
          
          end subroutine          
      

    subroutine initConstraintColInfo()

        ! Build constraint info
        constraintColInfo(ID_CONSTRAINT_NAME_COL)%colName = "Name"
        constraintColInfo(ID_CONSTRAINT_NAME_COL)%colType = ID_WIDGET_TYPE_DROPDOWN
        constraintColInfo(ID_CONSTRAINT_NAME_COL)%dataType = ID_DATATYPE_STR
        constraintColInfo(ID_CONSTRAINT_NAME_COL)%getFunc_str => constraint_item_get_name

        constraintColInfo(ID_CONSTRAINT_TYPE_COL)%colName = "Type"
        constraintColInfo(ID_CONSTRAINT_TYPE_COL)%colType = ID_WIDGET_TYPE_DROPDOWN
        constraintColInfo(ID_CONSTRAINT_TYPE_COL)%dataType = ID_DATATYPE_INT
        constraintColInfo(ID_CONSTRAINT_TYPE_COL)%getFunc_int => constraint_item_get_contype        

        constraintColInfo(ID_CONSTRAINT_VALUE_COL)%colName = "Target"
        constraintColInfo(ID_CONSTRAINT_VALUE_COL)%colType = ID_WIDGET_TYPE_ENTRY
        constraintColInfo(ID_CONSTRAINT_VALUE_COL)%dataType = ID_DATATYPE_DBL
        constraintColInfo(ID_CONSTRAINT_VALUE_COL)%getFunc_dbl => constraint_item_get_target

    end subroutine



    function constraints_create_table() result(boxNew)
        ! Columns:
        ! Constraint Name [dropdown]
        ! Constraint -dropdown for > < = 
        ! Constraint target 


        use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char
    
        type(integer) :: ID_TAB
        type(c_ptr) :: boxNew, toolBox
    
        !Debug
        integer :: ii
        integer, target :: colIDs(10) = [(ii,ii=1,10)]
    
        type(c_ptr) :: store, cStrB, listitem, selection, factory, column, swin
        character(len=1024) :: debugName
    
        type(c_ptr) :: cv, dbut, ibut, qbut
    
    
        boxNew = hl_gtk_box_new()

        !Create toolbar
        call createConstraintToolbar(toolBox)
        call gtk_box_append(boxNew, toolBox)        

        store = buildConstraintTable()
        selection = gtk_multi_selection_new(store)
              !selection = gtk_multi_selection_new(store)
        call gtk_single_selection_set_autoselect(selection,TRUE)    
        cv = gtk_column_view_new(selection)

        call gtk_widget_set_name(cv, "Constraint"//c_null_char)

        call setColumnViewDefault(cv, setConstraintColumns)
    
          swin = gtk_scrolled_window_new()
          call gtk_scrolled_window_set_child(swin, cv)
          call gtk_scrolled_window_set_min_content_height(swin, 300_c_int) !TODO:  Fix this properly 
          call gtk_box_append(boxNew, swin)
    
        ! Insert row a possible future feature
        !   ibut = hl_gtk_button_new("Insert row"//c_null_char, &
        !   & clicked=c_funloc(ins_constraint_row), &
        !   & tooltip="Insert new row above"//c_null_char, sensitive=FALSE)
    
        !   call hl_gtk_box_pack(boxNew, ibut)
    
          ! Delete selected row
          dbut = hl_gtk_button_new("Delete selected row"//c_null_char, &
                & clicked=c_funloc(del_constraint_row), &
                & data=cv, &
                & tooltip="Delete the selected row"//c_null_char, sensitive=FALSE)

          call g_signal_connect(selection, 'selection-changed'//c_null_char, c_funloc(constraint_row_selected), dbut)                
    
          call hl_gtk_box_pack(boxNew, dbut)
    
              ! Also a quit button
          qbut = hl_gtk_button_new("Quit"//c_null_char, clicked=c_funloc(optimizer_ui_destroy), data=optimizer_window)
          call hl_gtk_box_pack(boxNew,qbut)
    
      end function   


! This func interfaces with the c struct that stores the data
! At some point I may migrate this to fortran but for now the
! main cost of this is a bunch of interfaces for each get which
! I can live with
      function buildConstraintTable() result(store)
        use mod_lens_data_manager
      
        integer :: i
      
        type(c_ptr) :: store
        integer :: numConstraints, numRows
      
       
        numConstraints = nC

        print *, "Num Constraints is ", numConstraints

        ! Set some minimum amount 
        if (numConstraints < 20) then 
            numRows = 20
        else
            numRows = numConstraints
        end if
      
          store = g_list_store_new(G_TYPE_OBJECT)
          do i=1,numRows
            if (i <= numConstraints ) then
          store = append_constraint_model(store, constraintsInUse(i)%name, constraintsInUse(i)%con,  &
          & constraintsInUse(i)%conType, constraintsInUse(i)%targ)
          print *, "Targ is ", constraintsInUse(i)%targ
          print *, "conType is ", constraintsInUse(i)%conType

            else 
                print *, "i is ", i
                store = append_blank_constraint(store)
            end if
          end do
      
        end function      


        subroutine setConstraintColumns(colView)
            use type_utils, only: int2str
            type(c_ptr), value :: colView
          
            integer :: ii
            integer, target :: colIDs(25) = [(ii,ii=1,25)]
            type(c_ptr) :: factory, column


            do ii=1,size(constraintColInfo)
              factory = gtk_signal_list_item_factory_new()
              !call g_signal_connect(factory, "setup"//c_null_char, c_funloc(setup_constraint_cb),c_loc(colIDs(ii)))
              call g_signal_connect(factory, "setup"//c_null_char, c_funloc(setup_constraint_cb),g_strdup("R"//trim(int2str(ii))//"C"//trim(int2str(colIDs(ii)))//c_null_char))
              call g_signal_connect(factory, "bind"//c_null_char, c_funloc(bind_constraint_cb),c_loc(colIDs(ii)))
              !call g_signal_connect(factory, "bind"//c_null_char, c_funloc(bind_constraint_cb),g_strdup("R"//trim(int2str(ii))//"C"//trim(int2str(colIDs(ii)))))
              column = gtk_column_view_column_new(trim(constraintColInfo(ii)%colName)//c_null_char, factory)
              call gtk_column_view_column_set_id(column, trim(int2str(colIDs(ii))))
              call gtk_column_view_column_set_resizable(column, 1_c_int)
              call gtk_column_view_append_column (colView, column)
              call g_object_unref (column)      
            end do
          
          end subroutine

          function convertListtoCStringArray(iptStrList) result(c_ptr_array)
            integer :: ii
            character(len=*), dimension(:) :: iptStrList 
            type(c_ptr), dimension(size(iptStrList)+1) :: c_ptr_array
            character(kind=c_char), dimension(:), allocatable :: strTmp
            character(kind=c_char), pointer, dimension(:) :: ptrTmp

            
            do ii = 1, size(iptStrList)
              call f_c_string(iptStrList(ii), strTmp)
              allocate(ptrTmp(size(strTmp)))
              ! A Fortran pointer toward the Fortran string:
              ptrTmp(:) = strTmp(:)
              ! Store the C address in the array:
              c_ptr_array(ii) = c_loc(ptrTmp(1))
              nullify(ptrTmp)
            end do
            ! The array must be null terminated:
            c_ptr_array(size(iptStrList)+1) = c_null_ptr
        
          end function
        

          subroutine setup_constraint_cb(factory,listitem, gdata) bind(c)
            use gtk_hl_entry
            use gtk_hl_container
            use ui_table_funcs
            
            type(c_ptr), value :: factory
            type(c_ptr), value :: listitem, gdata
            type(c_ptr) :: label, entryCB, menuB, boxS, dropDown
            !integer(kind=c_int), pointer :: ID_COL
            integer :: ii
            integer, target :: colIDs(25) = [(ii,ii=1,25)]
            character(len=3) :: cmd
            integer :: row, ID_COL
            
          
            label =gtk_label_new(c_null_char)
            call gtk_list_item_set_child(listitem,label)
          
            call getRowAndColumnFromStrPtr(gdata, row, ID_COL)
            !call c_f_pointer(gdata, ID_COL)

            select case (constraintColInfo(ID_COL)%colType)
            case (ID_WIDGET_TYPE_LABEL)
                label =gtk_label_new(c_null_char)
                call gtk_list_item_set_child(listitem,label)    
            case (ID_WIDGET_TYPE_DROPDOWN)
                ! Data is column dependent
                select case (ID_COL)
                case(ID_CONSTRAINT_NAME_COL)
                 dropDown = gtk_drop_down_new_from_strings(convertListtoCStringArray(gatherConstraintNames()))
                case(ID_CONSTRAINT_TYPE_COL)
                 dropDown = gtk_drop_down_new_from_strings(convertListtoCStringArray(gatherConstraintTypeNames()))
                end select

                call gtk_list_item_set_child(listitem,dropDown)     
            case (ID_WIDGET_TYPE_ENTRY)
                boxS = hl_gtk_box_new(horizontal=TRUE, spacing=0_c_int)
                entryCB = hl_gtk_entry_new(4_c_int, editable=TRUE, activate=c_funloc(constraint_cell_changed), data=c_null_ptr)                                           
                !entryCB = hl_gtk_entry_new(10_c_int, editable=TRUE, activate=c_funloc(cell_changed), data=g_strdup('CIR'))                                           
                call gtk_box_append(boxS, entryCB)
                call gtk_list_item_set_child(listitem, boxS)  
                
            end select       
        end subroutine   


        subroutine setDropDownByString(dropDown, strCand)
            type(c_ptr) :: dropDown
            character(len=*) :: strCand

            type(c_ptr) :: model, cstr 
            character(len=140) :: strDD
            integer :: n_items, ii

            model = gtk_drop_down_get_model(dropDown)
            n_items = g_list_model_get_n_items(model)

            do ii=0,n_items
                cstr = gtk_string_list_get_string(model, ii)
                call convert_c_string(cStr, strDD)
                if (strDD == strCand) then 
                    call gtk_drop_down_set_selected(dropDown, ii)
                    exit
                end if
            end do


        end subroutine

        subroutine bind_constraint_cb(factory,listitem, gdata) bind(c)
            use type_utils
            type(c_ptr), value :: factory
            type(c_ptr), value :: listitem, gdata
            type(c_ptr) :: widget, item, label, buffer, entryCB, menuCB
            type(c_ptr) :: cStr
            real(kind=c_double) :: tmpDbl
            integer(kind=c_int) :: tmpInt
            character(len=140) :: colName
            character(len=1), dimension(:), allocatable :: conTypeNames
            class(*), pointer :: tmpPtr
            character(len=100) :: rcCode
            integer(kind=c_int), pointer :: ID_COL
            integer :: row

            call c_f_pointer(gdata, ID_COL)
            !call getRowAndColumnFromStrPtr(gdata, row, ID_COL)
            label = gtk_list_item_get_child(listitem)
            item = gtk_list_item_get_item(listitem);
          
            select case (constraintColInfo(ID_COL)%colType)
            case (ID_WIDGET_TYPE_LABEL)

                select case (constraintColInfo(ID_COL)%dataType)
                    case (ID_DATATYPE_INT)
                    colName = trim(int2str(constraintColInfo(ID_COL)%getFunc_int(item)))
                       
                    case (ID_DATATYPE_STR)
                        cStr = constraintColInfo(ID_COL)%getFunc_str(item)
                        call convert_c_string(cStr, colName)      

                    case (ID_DATATYPE_DBL)
                        tmpDbl = constraintColInfo(ID_COL)%getFunc_dbl(item)
                        write(colName, *) tmpDbl
                        
                        !colName = real2str(constraintColInfo(ID_COL)%getFunc_dbl(item))
                end select

                call gtk_label_set_text(label, trim(colName)//c_null_char)   
                ! colName = trim(int2str(operandColInfo(ID_COL)%getFunc(item)))//c_null_char
                ! print *, "colName is ", trim(colName)
                ! call gtk_label_set_text(label, trim(colName)//c_null_char)       

            case (ID_WIDGET_TYPE_DROPDOWN)    
                select case (ID_COL)
                    case(ID_CONSTRAINT_NAME_COL)
                        cStr = constraintColInfo(ID_COL)%getFunc_str(item)
                        call convert_c_string(cStr, colName)   
                        call setDropDownByString(label, colName)                
                    case (ID_CONSTRAINT_TYPE_COL)
                        tmpInt = constraintColInfo(ID_COL)%getFunc_int(item)
                        ! There is a bug where the getFunc is not returning a value ine
                        ! range.  I could not figure out why so for now just restrict values
                        if (tmpInt > 0 .AND. tmpInt < 5) then 
                        conTypeNames = gatherConstraintTypeNames()
                        call setDropDownByString(label, conTypeNames(tmpInt))
                        else 
                            conTypeNames = gatherConstraintTypeNames()
                            call setDropDownByString(label, conTypeNames(1))
                        end if
                    end select

                case (ID_WIDGET_TYPE_ENTRY)
                    select case (constraintColInfo(ID_COL)%dataType)
                    case (ID_DATATYPE_INT)
                    colName = trim(int2str(constraintColInfo(ID_COL)%getFunc_int(item)))
                       
                    case (ID_DATATYPE_STR)
                        cStr = constraintColInfo(ID_COL)%getFunc_str(item)
                        call convert_c_string(cStr, colName)      

                    case (ID_DATATYPE_DBL)
                        tmpDbl = constraintColInfo(ID_COL)%getFunc_dbl(item)
                        !write(colName, *) tmpDbl
                        colName = real2str(tmpDbl)
                        !if (tmpDbl == 0) colName = "0"
                        !colName = real2str(constraintColInfo(ID_COL)%getFunc_dbl(item))
                    end select
                    entryCB = gtk_widget_get_first_child(label)  
                    buffer = gtk_entry_get_buffer(entryCB)    
                    call gtk_entry_buffer_set_text(buffer, trim(colName)//c_null_char,-1_c_int)                    
                    

                
                
                !call gtk_drop_down_set_selected(label, 0_c_int)
                ! Encode row and column for later use    

                !call gtk_widget_set_name(label,trim(rcCode)//c_null_char)

            end select   
            row = gtk_list_item_get_position(listitem)   
            call gtk_widget_set_name(label,"R"//trim(int2str(row))//"C"//trim(int2str(ID_COL))//c_null_char)

            if (constraintColInfo(ID_COL)%colType == ID_WIDGET_TYPE_DROPDOWN) then
                call g_signal_connect(label, "notify::selected"//c_null_char, c_funloc(constraintDropDownChanged), c_null_ptr)
            end if            
 
        end subroutine

        function getRowFromColumnView(cv) result(currPos)
            type(c_ptr), value :: cv
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

        subroutine del_constraint_row(but, gdata) bind(c)
            use type_utils, only: int2str
            type(c_ptr), value, intent(in) :: but, gdata
            integer(kind=c_int) :: currRow

            currRow = getRowFromColumnView(gdata)

            print *, "currRow is ", currRow
            call PROCESKDP('DEL CON '//trim(int2str(currRow+1)))
            !call rebuildConstraintTable(gdata)
            call rebuildTable(gdata, buildConstraintTable(), setConstraintColumns)

        end subroutine
     
        subroutine ins_constraint_row(but, gdata) bind(c)
            use type_utils, only: int2str
              type(c_ptr), value, intent(in) :: but, gdata
              integer(kind=c_int) :: currRow
        end subroutine

        subroutine constraint_row_selected(widget, position, n_items, userdata) bind(c)
            type(c_ptr), value ::  widget, userdata
            integer(c_int) :: position, n_items
            type(c_ptr) :: listitem, cStr
            character(len=100) :: ftext
            print *, "Row selected! "
            !listitem = gtk_single_selection_get_selected_item(widget)
            ! cStr = lens_item_get_surface_type(listitem)
            ! call convert_c_string(cStr, ftext)   
            ! print *, "Surf type is ", trim(ftext)
            !call updateColumnHeadersIfNeeded(trim(ftext))
            call gtk_widget_set_sensitive(userdata, TRUE)
            !call gtk_widget_set_sensitive(ibut, TRUE)
          end subroutine      
          
        !   subroutine rebuildConstraintTable(cv)
        !     type(c_ptr), value :: cv ! column view
        !     type(c_ptr) :: store, selection, model, column, vadj, hadj, swin
        !     integer(c_int) :: oldPos 
        !     real(c_double) :: vPos, hPos
        !     logical :: boolResult
         
                   
        !     ! Get current position
        !     oldPos = getCurrentTableRow(cv)
        !     print *, "Old Position is ", oldPos
          
        !     ! Get location of horizontal and vertical scrollbars so we can recreate.  This seems crazy
        !     swin = gtk_widget_get_parent(cv) 
        !     vadj = gtk_scrolled_window_get_vadjustment(swin)
        !     vPos = gtk_adjustment_get_value(vadj)
        !     hadj = gtk_scrolled_window_get_hadjustment(swin)
        !     hPos = gtk_adjustment_get_value(hadj)
          
        !     print *, "Value is ", gtk_adjustment_get_value(vadj)
          
        !     call clearColumnView_opt(cv)

        !     store = buildConstraintTable()
        !     !selection = gtk_multi_selection_new(store)
        !     selection = gtk_single_selection_new(store)

        !     call gtk_column_view_set_model(cv, selection)
   
        !     call setColumnViewDefault(cv, setConstraintColumns)
          
        !     ! Set selection to previous
        !     !model = gtk_column_view_get_model(cv)
        !     boolResult = gtk_selection_model_select_item(selection, oldPos, 1_c_int)
        !     print *, "boolResult is ", boolResult
          
        !     call pending_events() ! Critical for following to work!
        !     vadj = gtk_scrolled_window_get_vadjustment(swin)
        !     hadj = gtk_scrolled_window_get_hadjustment(swin)
        !     call gtk_adjustment_set_value(vadj, vPos)
        !     call gtk_adjustment_set_value(hadj, hPos)
          
        !   end subroutine          

        !   function getCurrentTableRow(cv) result(currPos)
        !     integer(c_int) :: currPos, numRows, isSelected, ii
        !     type(c_ptr) :: cv
        !     type(c_ptr) :: selection
        
          
          
        !     selection = gtk_column_view_get_model(cv)
        !     ! Seems there is no simple way to get selected row so brute force it
        !     numRows = g_list_model_get_n_items(selection)
        !     currPos=-1
        !     do ii=0,numRows-1
        !       isSelected = gtk_selection_model_is_selected(selection, ii)
        !       if (isSelected==1) then
        !            currPos = ii
        !            exit
        !       end if
        !     end do
          
        !   end function          

        !   subroutine clearColumnView_opt(cv)
        !     type(c_ptr) :: listModel, currCol
        !     type(c_ptr), intent(inout) :: cv
        !     integer(c_int) :: ii, numItems
          
        !     listmodel = gtk_column_view_get_columns(cv)
        !     numItems = g_list_model_get_n_items(listmodel) -1
        !     do ii=numItems,0,-1
        !       currCol = g_list_model_get_object(listmodel,ii)
        !       call gtk_column_view_remove_column(cv,currCol)
        !       call g_object_unref(currCol)
          
        !     end do
          
        !   end subroutine          


          subroutine createConstraintToolbar(toolBox)
            use gdk

            type(c_ptr) :: toolBox ! Output
            type(c_ptr), dimension(2) :: btns
            type(c_ptr) :: theme
            integer :: i
    

      
            toolBox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0_c_int);
    
            ! Adding icons
            ! I used iconoir.com to find icons and download svg files
            ! converted them using magick from command line (eg magick zoom-out.svg zoom-out.png)
            ! Copy to /data folder
            ! add file to gresource.xml 
    
            btns(1) = gtk_button_new_from_icon_name ("view-refresh-symbolic"//c_null_char)
            btns(2) = gtk_button_new_from_icon_name ("view-help-symbolic"//c_null_char)
            
            call gtk_widget_set_tooltip_text(btns(1), "Update Database"//c_null_char)
            call gtk_widget_set_tooltip_text(btns(2), "Help on this Window"//c_null_char)
 
            do i=1,size(btns)
                call gtk_widget_set_valign(btns(i), GTK_ALIGN_START)
                call gtk_button_set_has_frame (btns(i), FALSE)
                call gtk_widget_set_focus_on_click (btns(i), FALSE)
                call hl_gtk_box_pack (toolBox, btns(i))
                call gtk_widget_set_has_tooltip(btns(i), 1_c_int)
            end do          
            call g_signal_connect(btns(1), 'clicked'//c_null_char, c_funloc(updateConstraints), c_null_ptr)
            call g_signal_connect(btns(2), 'clicked'//c_null_char, c_funloc(helpWindow), c_null_ptr)

        end subroutine

        subroutine updateConstraints(widget, event, gdata) bind(c)
            use gdk, only: gdk_cursor_new_from_name
            implicit none
            type(c_ptr), value :: widget, event, gdata
           
            !toolbarState = ID_NONE
            !call gtk_widget_set_cursor_from_name(plotArea, "default"//c_null_char)
        end subroutine      
        
        subroutine helpWindow(widget, event, gdata) bind(c)
            use gdk, only: gdk_cursor_new_from_name
            implicit none
            type(c_ptr), value :: widget, event, gdata
           
            !toolbarState = ID_NONE
            !call gtk_widget_set_cursor_from_name(plotArea, "default"//c_null_char)
        end subroutine              

        function getColumnViewFromWidget(widget, strName) result(cv)
            type(c_ptr), value :: widget 
            type(c_ptr) :: cv
            character(len=*) :: strName 
            character(len=100) :: ftext
            type(c_ptr) :: tmpPtr, cStr
            integer :: ii 

            cv = c_null_ptr
            tmpPtr = widget
            do ii=1,10
                tmpPtr = gtk_widget_get_parent(tmpPtr)
                if (c_associated(tmpPtr)) then 
                    cStr = gtk_widget_get_name(tmpPtr)
                    if (c_associated(cStr)) then 
                        call convert_c_string(cStr, ftext)
                        if (ftext == strName) then 
                            print *, "Found right widget!  It's a miracle"
                            cv= tmpPtr
                            return 
                        end if
                    end if
                end if
            end do

        end function

        function getModelFromWidget(widget, strName) result(model)
            type(c_ptr), value :: widget 
            type(c_ptr) :: model
            character(len=*) :: strName 
            character(len=100) :: ftext
            type(c_ptr) :: tmpPtr, cStr
            integer :: ii 

            model = c_null_ptr
            print *, "Trying to get model from widget"
            tmpPtr = widget
            do ii=1,10
                tmpPtr = gtk_widget_get_parent(tmpPtr)
                if (c_associated(tmpPtr)) then 
                    cStr = gtk_widget_get_name(tmpPtr)
                    if (c_associated(cStr)) then 
                        call convert_c_string(cStr, ftext)
                        if (ftext == strName) then 
                            print *, "Found right widget!  It's a miracle"
                            model = gtk_column_view_get_model(tmpPtr)
                            return 
                        end if
                    end if
                end if
            end do

        end function

        subroutine constraint_cell_changed(widget, data) bind(c)
            use type_utils
            use strings
            use ui_table_funcs
          
          type(c_ptr), value :: widget, data
          type(c_ptr) :: buff2, cStr, item, model
          character(len=100) :: rcCode, cmd, valTxt
          character(len=140) :: ftext
          character(len=1) :: conStr
          integer :: row,col
          integer(kind=c_int) :: conType
          
          model = getModelFromWidget(widget, "Constraint")

          buff2 = gtk_entry_get_buffer(widget)
          call c_f_string_copy(gtk_entry_buffer_get_text(buff2), valTxt)

          call getRowAndColumnFromStrPtr(gtk_widget_get_name(gtk_widget_get_parent(widget)),row,col)

          cmd = getConstraintChangeCommand(model, row, col, trim(valTxt))

          print *, "update cmd is ", trim(cmd)
          call PROCESKDP("UPD CON ; CHA "//trim(int2str(row+1))//" ; "//trim(cmd)//'; GO')
          call rebuildTable(getColumnViewFromWidget(widget, "Constraint"), buildConstraintTable(), setConstraintColumns)




        !   if(c_associated(model)) then 
        !     print *, "actually iun the right place?"
        !     print *, "row is ", row
        !     item = g_list_model_get_item(model, row) ! row 0 indexed
        !     print *, "item is ", loc(item)
        !     conType = constraint_item_get_contype(item)
        !     print *, "Contype is ", conType
        !     cStr = constraint_item_get_name(item)
        !     print *, "cStr is ", cStr
        !     call convert_c_string(cStr, ftext)
        !     select case (conType)
        !     case(ID_CON_EXACT)
        !         conStr = '='
        !     case(ID_CON_GREATER_THAN)
        !         conStr = '>'
        !     case(ID_CON_LESS_THAN)
        !         conStr = '<'
        !     end select
        !     print *, "Cmd is "//"UPD CON ; CHA "//trim(int2str(row+1))//" ; "//trim(ftext)//' '//conStr//' '//trim(valTxt)
        !     call PROCESKDP("UPD CON ; CHA "//trim(int2str(row+1))//" ; "//trim(ftext)//' '//conStr//' '//trim(valTxt))
        !   end if


          end subroutine       
          
          subroutine constraintDropDownChanged(widget, gdata) bind(c)
            use type_utils
            type(c_ptr), value :: widget, gdata
            type(c_ptr) :: buff2, cStr, item, model, currItem
            character(len=100) :: rcCode, cmd, valTxt
            character(len=140) :: ftext
            character(len=1) :: conStr
            integer :: row,col
            integer(kind=c_int) :: conType
            
            model = getModelFromWidget(widget, "Constraint")

            ! cStr = gtk_widget_get_name(widget)
            ! call convert_c_string(cStr, ftext)
            ! print *, "Dropdown widget name is ", trim(ftext)
  
            !call getRowAndColumnFromStrPtr(gtk_widget_get_name(gtk_widget_get_parent(widget)),row,col)


            call getRowAndColumnFromStrPtr(gtk_widget_get_name(widget),row,col)
            print *, "row is ", row 
            print *, "col is ", col
            currItem = gtk_drop_down_get_selected_item(widget)
            cStr = gtk_string_object_get_string(currItem)
            !cStr = g_value_get_string(cStr)
            call convert_c_string(cStr, ftext)            
            print *, "text is ", trim(ftext)

            cmd = getConstraintChangeCommand(model, row, col, trim(ftext))
            print *, "cmd is ", cmd
            call PROCESKDP("UPD CON ; CHA "//trim(int2str(row+1))//" ; "//trim(cmd)//'; GO')
            call rebuildTable(getColumnViewFromWidget(widget, "Constraint"), buildConstraintTable(), setConstraintColumns)
    
        end subroutine

        function getColValueAsStr(item, uiColInfo) result (outStr)
            use type_utils
            type(uiTableColumnInfo) :: uiColInfo 
            type(c_ptr), value :: item
            character(len=240) :: outStr 
            type(c_ptr) :: cStr
            real(kind=c_double) :: tmpDbl


            select case (uiColInfo%dataType)
                case (ID_DATATYPE_INT)
                    outStr= trim(int2str(uiColInfo%getFunc_int(item)))
                   
                case (ID_DATATYPE_STR)
                    cStr = uiColInfo%getFunc_str(item)
                    call convert_c_string(cStr, outStr)      

                case (ID_DATATYPE_DBL)
                    tmpDbl = uiColInfo%getFunc_dbl(item)
                    
                    outStr = real2str(tmpDbl)
                  !outStr = real2str(uiColInfo%getFunc_dbl(item))
            end select

        end function

        function getConstraintChangeCommand(model, row, col, colText) result(outStr)
            use type_utils, only: str2int
            type(c_ptr), value :: model
            integer :: row, col 
            character(len=*) :: colText
            character(len=200) :: outStr, tmpStr
            character(len=1) :: conStr
            type(c_ptr) :: item 
            integer :: ii, conType

            item = g_list_model_get_item(model, row) ! row 0 indexed

            outStr = ''
            do ii=1,size(constraintColInfo)
                if (ii .ne. col) then 
                    if (ii == ID_CONSTRAINT_TYPE_COL) then 
                        conType = str2int(trim(getColValueAsStr(item, constraintColInfo(ii))))
                        select case (conType)
                        case(ID_CON_EXACT)
                            conStr = '='
                        case(ID_CON_GREATER_THAN)
                            conStr = '>'
                        case(ID_CON_LESS_THAN)
                            conStr = '<'
                        end select   
                    outStr = trim(outStr)//' '//conStr
                    else
                    outStr = trim(outStr)//' '//trim(getColValueAsStr(item, constraintColInfo(ii)))
                    end if ! ID_CONSTRAINT_TYPE_COL check
                else
                    outStr = trim(outStr)//' '//colText
                end if
            end do

        end function

end module