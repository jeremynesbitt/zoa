! This module is a bit of a mess.  I had greater ambitions for this when I first made it.
! For now it just includes a table of operands and a button to run.  All of this can be done via 
! the CLI but this UI is nice to see operands and delete them if needed 
module optimizer_ui
    use gtk
    use g
    !use hl_gtk_zoa
    use gtk_hl_button
    use gtk_hl_entry
    use gtk_hl_container
    use iso_c_binding
    use global_widgets
    use optim_types
    use zoa_ui
    use ui_table_funcs

    implicit none

    interface

! Merit entries (historically "constraints"; the table now holds both
! objective terms and constraints, distinguished by role)
      function append_constraint_model(store, constraintName, conValue, role, contype,  &
        & targ, weight) bind(c)
        import c_ptr, c_char, c_int, c_double
        implicit none
        type(c_ptr), value    :: store
        character(kind=c_char), dimension(*) :: constraintName
        type(c_ptr)    :: append_constraint_model
        integer(c_int), value :: role, contype
        real(c_double), value :: conValue, targ, weight
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
       function constraint_item_get_contribution(item) bind(c)
        import :: c_ptr, c_double
        type(c_ptr), value :: item
        real(c_double) :: constraint_item_get_contribution
      end function
       subroutine set_contribution_total(t) bind(c)
        import :: c_double
        real(c_double), value :: t
      end subroutine
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
      function constraint_item_get_role(item) bind(c)
        import :: c_ptr, c_int
        type(c_ptr), value :: item
        integer(c_int) :: constraint_item_get_role
      end function
      function constraint_item_get_weight(item) bind(c)
        import :: c_ptr, c_double
        type(c_ptr), value :: item
        real(c_double) :: constraint_item_get_weight
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


    integer, parameter :: ID_DATATYPE_STR = 1
    integer, parameter :: ID_DATATYPE_INT = 2
    integer, parameter :: ID_DATATYPE_DBL = 3

    integer, parameter :: ID_WIDGET_TYPE_LABEL = 4001
    integer, parameter :: ID_WIDGET_TYPE_DROPDOWN = 4002
    integer, parameter :: ID_WIDGET_TYPE_ENTRY = 4003


    ! Unified merit table columns
    integer, parameter :: ID_CONSTRAINT_NAME_COL = 1
    integer, parameter :: ID_CONSTRAINT_ROLE_COL = 2
    integer, parameter :: ID_CONSTRAINT_TYPE_COL = 3
    integer, parameter :: ID_CONSTRAINT_TARGET_COL = 4
    integer, parameter :: ID_CONSTRAINT_WEIGHT_COL = 5
    integer, parameter :: ID_CONSTRAINT_VALUE_COL = 6
    integer, parameter :: ID_CONSTRAINT_CONTRIB_COL = 7

    type(uiTableColumnInfo) :: constraintColInfo(7)

    ! Guard: .TRUE. while bind_constraint_cb programmatically sets dropdown
    ! selections.  gtk fires 'notify::selected' on programmatic changes too,
    ! and the recycled-widget handlers would build phantom UPD CON edit
    ! commands from rows that were merely being (re)displayed.
    logical :: binding_merit_row = .FALSE.

    ! General Constraints tab: the GENCON checkbox and one value entry per
    ! constraint (MXT, MNT, MNE, MNA, MAE in that order).  genRefreshing
    ! blocks the toggled handler while the tab is being refreshed from optim.
    type(c_ptr) :: genCheck = c_null_ptr
    type(c_ptr) :: genEntries(5) = c_null_ptr
    logical :: genRefreshing = .FALSE.
    integer(c_int), target :: genIdx(5) = [1, 2, 3, 4, 5]
    character(len=3), parameter :: GEN_NAMES(5) = ['MXT', 'MNT', 'MNE', 'MNA', 'MAE']

    contains

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

    
        nbk = gtk_notebook_new()

        ! I had ambitions to make this more comprehensive but I give up on this for now

        !basicLabel = gtk_label_new_with_mnemonic("_General"//c_null_char)
        !pageIdx = gtk_notebook_append_page(nbk, box1, basicLabel)
    
        ! Unified merit table: objective terms (operands) and constraints in
        ! one table, distinguished by the Role column.
        conLabel = gtk_label_new_with_mnemonic("_Merit"//c_null_char)
        pageIdx = gtk_notebook_append_page(nbk, constraints_create_table(), conLabel)
    
        SolveLabel = gtk_label_new_with_mnemonic("_Optimize"//c_null_char)
        pageIdx = gtk_notebook_append_page(nbk, optimize_create_objects(), SolveLabel)

        conLabel = gtk_label_new_with_mnemonic("_General Constraints"//c_null_char)
        pageIdx = gtk_notebook_append_page(nbk, general_constraints_create(), conLabel)
        ! A GENCON/MXT/... typed at the command line while the window is open
        ! shows up the next time the tab is selected.
        call g_signal_connect(nbk, "switch-page"//c_null_char, c_funloc(gen_switch_page_cb))
    
        PRINT *, "FINISHED WITH OPTIMIZER WINDOW"
        !call gtk_box_append(box1, rf_cairo_drawing_area)
        !call gtk_window_set_child(lens_editor_window, rf_cairo_drawing_area)
        call gtk_window_set_child(optimizer_window, nbk)
    
    
        call gtk_window_set_mnemonics_visible (optimizer_window, TRUE)
        call g_signal_connect(optimizer_window, "destroy"//c_null_char, &
             & c_funloc(optimizer_ui_on_destroy), optimizer_window)
        call gtk_widget_show(optimizer_window)

    end subroutine optimizer_ui_new

    function optimize_create_objects() result(boxOptimize)
    ! For now have this contain a run button and an info table.
    ! Eventually some settings would be nice (eg optimization goal)
        type(c_ptr) :: boxOptimize,entry_widget, rbut
        character(len=1024) :: boxText


        boxOptimize = hl_gtk_box_new()

        boxText = "Use this button to run optimizer.  Results will show up in command window"
      
        ! Create an entry widget (text box)
        entry_widget =  hl_gtk_entry_new(editable=FALSE, value=trim(boxText))

        call gtk_box_append(boxOptimize, entry_widget)
      

          rbut = hl_gtk_button_new("Run Optimizer"//c_null_char, &
          & clicked=c_funloc(run_optimizer_cmd))

          call gtk_box_append(boxOptimize, rbut)


    end function

    ! General Constraints tab.  Every change goes through a command (GENCON,
    ! MXT, ...) in an UPD CON loop, which sets it without running the
    ! optimizer; the tab then re-reads optim so it always shows what is set.
    function general_constraints_create() result(boxGen)
        type(c_ptr) :: boxGen, grid, lbl, note
        integer :: i
        character(len=40) :: desc(5), units(5)
        real(long) :: defaults(5)

        desc = [character(len=40) :: 'Maximum element center thickness', &
            &   'Minimum element center thickness', 'Minimum element edge thickness', &
            &   'Minimum axial air spacing', 'Minimum air spacing at edge']
        defaults = [GEN_MXT_DEFAULT, GEN_MNT_DEFAULT, GEN_MNE_DEFAULT, &
            &       GEN_MNA_DEFAULT, GEN_MAE_DEFAULT]

        boxGen = gtk_box_new(GTK_ORIENTATION_VERTICAL, 8_c_int)
        call gtk_widget_set_margin_start(boxGen,  12_c_int)
        call gtk_widget_set_margin_end(boxGen,    12_c_int)
        call gtk_widget_set_margin_top(boxGen,    12_c_int)
        call gtk_widget_set_margin_bottom(boxGen, 12_c_int)

        genCheck = gtk_check_button_new_with_label( &
            & 'Use general constraints (GENCON)'//c_null_char)
        call gtk_widget_set_tooltip_text(genCheck, &
            & 'On: GENCON YES.  Off: GENCON NO (none of the limits below are applied).'//c_null_char)
        call g_signal_connect(genCheck, "toggled"//c_null_char, c_funloc(gen_toggled_cb))
        call gtk_box_append(boxGen, genCheck)

        grid = gtk_grid_new()
        call gtk_grid_set_column_spacing(grid, 12_c_int)
        call gtk_grid_set_row_spacing(grid, 6_c_int)

        call gen_header(grid, 'Command', 0)
        call gen_header(grid, 'Limit', 1)
        call gen_header(grid, 'Value', 2)
        call gen_header(grid, 'Default', 3)

        do i = 1, 5
            lbl = gtk_label_new(GEN_NAMES(i)//c_null_char)
            call gtk_label_set_xalign(lbl, 0.0_c_float)
            call gtk_grid_attach(grid, lbl, 0_c_int, int(i, c_int), 1_c_int, 1_c_int)

            lbl = gtk_label_new(trim(desc(i))//c_null_char)
            call gtk_label_set_xalign(lbl, 0.0_c_float)
            call gtk_grid_attach(grid, lbl, 1_c_int, int(i, c_int), 1_c_int, 1_c_int)

            genEntries(i) = hl_gtk_entry_new(editable=TRUE, &
                & activate=c_funloc(gen_value_entered_cb), data=c_loc(genIdx(i)))
            call gtk_widget_set_tooltip_text(genEntries(i), &
                & 'Press Enter to apply ('//GEN_NAMES(i)//' value)'//c_null_char)
            call gtk_grid_attach(grid, genEntries(i), 2_c_int, int(i, c_int), 1_c_int, 1_c_int)

            lbl = gtk_label_new(trim(gen_fmt(defaults(i)))//c_null_char)
            call gtk_label_set_xalign(lbl, 0.0_c_float)
            call gtk_grid_attach(grid, lbl, 3_c_int, int(i, c_int), 1_c_int, 1_c_int)
        end do
        call gtk_box_append(boxGen, grid)

        note = gtk_label_new('Applied at AUT; GO to variable thicknesses only (lens units).'// &
            & '  If MNE and MXT conflict, MNE wins.'//c_null_char)
        call gtk_label_set_wrap(note, TRUE)
        call gtk_label_set_xalign(note, 0.0_c_float)
        call gtk_box_append(boxGen, note)

        call gen_refresh()
    end function

    subroutine gen_header(grid, text, col)
        type(c_ptr), intent(in) :: grid
        character(len=*), intent(in) :: text
        integer, intent(in) :: col
        type(c_ptr) :: lbl
        lbl = gtk_label_new('<b>'//text//'</b>'//c_null_char)
        call gtk_label_set_use_markup(lbl, TRUE)
        call gtk_label_set_xalign(lbl, 0.0_c_float)
        call gtk_grid_attach(grid, lbl, int(col, c_int), 0_c_int, 1_c_int, 1_c_int)
    end subroutine

    function gen_fmt(v) result(s)
        use type_utils, only: real2str
        real(long), intent(in) :: v
        character(len=40) :: s
        s = adjustl(real2str(v))
    end function

    ! Show the current settings (optim) in the tab.
    subroutine gen_refresh()
        real(long) :: vals(5)
        integer :: i
        if (.not. c_associated(genCheck)) return
        genRefreshing = .TRUE.
        if (optim%genConOn) then
            call gtk_check_button_set_active(genCheck, TRUE)
        else
            call gtk_check_button_set_active(genCheck, FALSE)
        end if
        vals = [optim%mxt, optim%mnt, optim%mne, optim%mna, optim%mae]
        do i = 1, 5
            call gtk_entry_buffer_set_text(gtk_entry_get_buffer(genEntries(i)), &
                & trim(gen_fmt(vals(i)))//c_null_char, -1_c_int)
            if (optim%genConOn) then
                call gtk_widget_set_sensitive(genEntries(i), TRUE)
            else
                call gtk_widget_set_sensitive(genEntries(i), FALSE)
            end if
        end do
        genRefreshing = .FALSE.
    end subroutine

    subroutine gen_toggled_cb(widget, gdata) bind(c)
        type(c_ptr), value, intent(in) :: widget, gdata
        if (genRefreshing) return
        if (gtk_check_button_get_active(widget) /= 0) then
            call PROCESKDP('UPD CON; GENCON YES; GO')
        else
            call PROCESKDP('UPD CON; GENCON NO; GO')
        end if
        call gen_refresh()
    end subroutine

    subroutine gen_value_entered_cb(widget, gdata) bind(c)
        use gtk_sup, only: c_f_string_copy
        use command_utils, only: isInputNumber
        use zoa_output, only: zoa_emit
        type(c_ptr), value, intent(in) :: widget, gdata
        integer(c_int), pointer :: idx
        character(len=80) :: text
        call c_f_pointer(gdata, idx)
        call c_f_string_copy(gtk_entry_buffer_get_text(gtk_entry_get_buffer(widget)), text)
        if (isInputNumber(trim(adjustl(text)))) then
            call PROCESKDP('UPD CON; '//GEN_NAMES(idx)//' '//trim(adjustl(text))//'; GO')
        else
            call zoa_emit(GEN_NAMES(idx)//': "'//trim(adjustl(text))//'" is not a number', "red")
        end if
        call gen_refresh()
    end subroutine

    subroutine gen_switch_page_cb(notebook, page, pageNum, gdata) bind(c)
        type(c_ptr), value, intent(in) :: notebook, page, gdata
        integer(c_int), value, intent(in) :: pageNum
        call gen_refresh()
    end subroutine

    subroutine run_optimizer_cmd(but, gdata) bind(c)
        type(c_ptr), value, intent(in) :: but, gdata
        call PROCESKDP('AUT;GO')
    end subroutine

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

    subroutine optimizer_ui_on_destroy(widget, gdata) bind(c)
        type(c_ptr), value, intent(in) :: widget, gdata
        optimizer_window = c_null_ptr
        genCheck = c_null_ptr
        genEntries = c_null_ptr
    end subroutine

    subroutine initConstraintColInfo()

        ! Build merit-table info: Name | Role | Type | Target | Weight | Value.
        ! Type applies to constraints; Weight applies to objective terms
        ! (each is ignored for the other role when the edit command is built).
        constraintColInfo(ID_CONSTRAINT_NAME_COL)%colName = "Name"
        constraintColInfo(ID_CONSTRAINT_NAME_COL)%colType = ID_WIDGET_TYPE_DROPDOWN
        constraintColInfo(ID_CONSTRAINT_NAME_COL)%dataType = ID_DATATYPE_STR
        constraintColInfo(ID_CONSTRAINT_NAME_COL)%getFunc_str => constraint_item_get_name

        constraintColInfo(ID_CONSTRAINT_ROLE_COL)%colName = "Role"
        constraintColInfo(ID_CONSTRAINT_ROLE_COL)%colType = ID_WIDGET_TYPE_DROPDOWN
        constraintColInfo(ID_CONSTRAINT_ROLE_COL)%dataType = ID_DATATYPE_INT
        constraintColInfo(ID_CONSTRAINT_ROLE_COL)%getFunc_int => constraint_item_get_role

        constraintColInfo(ID_CONSTRAINT_TYPE_COL)%colName = "Type"
        constraintColInfo(ID_CONSTRAINT_TYPE_COL)%colType = ID_WIDGET_TYPE_DROPDOWN
        constraintColInfo(ID_CONSTRAINT_TYPE_COL)%dataType = ID_DATATYPE_INT
        constraintColInfo(ID_CONSTRAINT_TYPE_COL)%getFunc_int => constraint_item_get_contype

        constraintColInfo(ID_CONSTRAINT_TARGET_COL)%colName = "Target"
        constraintColInfo(ID_CONSTRAINT_TARGET_COL)%colType = ID_WIDGET_TYPE_ENTRY
        constraintColInfo(ID_CONSTRAINT_TARGET_COL)%dataType = ID_DATATYPE_DBL
        constraintColInfo(ID_CONSTRAINT_TARGET_COL)%getFunc_dbl => constraint_item_get_target

        constraintColInfo(ID_CONSTRAINT_WEIGHT_COL)%colName = "Weight"
        constraintColInfo(ID_CONSTRAINT_WEIGHT_COL)%colType = ID_WIDGET_TYPE_ENTRY
        constraintColInfo(ID_CONSTRAINT_WEIGHT_COL)%dataType = ID_DATATYPE_DBL
        constraintColInfo(ID_CONSTRAINT_WEIGHT_COL)%getFunc_dbl => constraint_item_get_weight

        constraintColInfo(ID_CONSTRAINT_VALUE_COL)%colName = "Value"
        constraintColInfo(ID_CONSTRAINT_VALUE_COL)%colType = ID_WIDGET_TYPE_LABEL
        constraintColInfo(ID_CONSTRAINT_VALUE_COL)%dataType = ID_DATATYPE_DBL
        constraintColInfo(ID_CONSTRAINT_VALUE_COL)%getFunc_dbl => constraint_item_get_value

        ! Contribution = weight*(value-target)^2, this operand's share of the
        ! objective f = sum weight*(value-target)^2 (0 for constraint rows).
        constraintColInfo(ID_CONSTRAINT_CONTRIB_COL)%colName = "Contribution %"
        constraintColInfo(ID_CONSTRAINT_CONTRIB_COL)%colType = ID_WIDGET_TYPE_LABEL
        constraintColInfo(ID_CONSTRAINT_CONTRIB_COL)%dataType = ID_DATATYPE_DBL
        constraintColInfo(ID_CONSTRAINT_CONTRIB_COL)%getFunc_dbl => constraint_item_get_contribution

    end subroutine

    ! Role names indexed by ID_ROLE_OBJECTIVE / ID_ROLE_CONSTRAINT
    function gatherRoleNames() result(strNameList)
        character(len=10), dimension(2) :: strNameList

        strNameList(ID_ROLE_OBJECTIVE)  = 'Operand'
        strNameList(ID_ROLE_CONSTRAINT) = 'Constraint'

    end function



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

  

        store = buildConstraintTable()
        selection = gtk_multi_selection_new(store)
        ! (was: gtk_single_selection_set_autoselect on a multi-selection -- a type
        !  mismatch that only logged a GTK assertion.  The delete button is now
        !  always enabled and guards against no selection, so it isn't needed.)
        cv = gtk_column_view_new(selection)

        call gtk_widget_set_name(cv, "Constraint"//c_null_char)
        call createConstraintToolbar(toolBox, cv)
        call gtk_box_append(boxNew, toolBox)      

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
                & tooltip="Delete the selected row"//c_null_char, sensitive=TRUE)

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
      ! Build the unified merit table: EVERY merit entry (objective terms and
      ! constraints), in definition order -- the row number matches the # that
      ! UPD CON; CHA n and LCON use.
      function buildConstraintTable() result(store)
        use mod_lens_data_manager

        integer :: i

        type(c_ptr) :: store
        integer :: numRows
        real(c_double) :: contribTotal, v

        print *, "Num merit entries is ", nM

        ! Total objective f = sum weight*(value-target)^2 over the operand rows,
        ! so the Contribution column can be shown as each operand's % of f (the
        ! per-row percentages then sum to 100).  Constraints don't contribute.
        contribTotal = 0.0_c_double
        do i=1,nM
          if (meritInUse(i)%role == ID_ROLE_OBJECTIVE) then
            v = meritInUse(i)%func()
            contribTotal = contribTotal + meritInUse(i)%weight * (v - meritInUse(i)%targ)**2
          end if
        end do
        call set_contribution_total(contribTotal)

        ! Set some minimum amount
        if (nM < 20) then
            numRows = 20
        else
            numRows = nM
        end if

          store = g_list_store_new(G_TYPE_OBJECT)
          do i=1,nM
            ! Name must be trimmed + null-terminated: C reads to the NUL, and a
            ! bare derived-type component has neither (the padding/overrun was
            ! the AUTUI name-dropdown crash).
            ! This did not work without () on the %func but did not throw and error.  Annoying
            store = append_constraint_model(store, trim(meritInUse(i)%name)//c_null_char, meritInUse(i)%func(),  &
            & meritInUse(i)%role, meritInUse(i)%conType, meritInUse(i)%targ, meritInUse(i)%weight)
          end do
          do i=nM+1,numRows
            store = append_blank_constraint(store)
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
              call convert_f_string(iptStrList(ii), strTmp)
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
            use hl_gtk_zoa, only: get_widget_name_f
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
            character(len=200) :: widgetName
            integer :: row, ID_COL
            type(c_ptr) :: cStr 
            character(len=200) :: fStr
            
          
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
                 dropDown = gtk_drop_down_new_from_strings(convertListtoCStringArray(gatherEvaluatorNames()))
                case(ID_CONSTRAINT_ROLE_COL)
                 dropDown = gtk_drop_down_new_from_strings(convertListtoCStringArray(gatherRoleNames()))
                case(ID_CONSTRAINT_TYPE_COL)
                 dropDown = gtk_drop_down_new_from_strings(convertListtoCStringArray(gatherConstraintTypeNames()))
                end select

                call gtk_list_item_set_child(listitem,dropDown)     
            case (ID_WIDGET_TYPE_ENTRY)
                boxS = hl_gtk_box_new(horizontal=TRUE, spacing=0_c_int)
                ! Max length 20 (was 4, too short for e.g. a 5-digit EFL target).
                entryCB = hl_gtk_entry_new(20_c_int, editable=TRUE, activate=c_funloc(constraint_cell_changed), data=c_null_ptr)
                !entryCB = hl_gtk_entry_new(10_c_int, editable=TRUE, activate=c_funloc(cell_changed), data=g_strdup('CIR'))                                           
                call gtk_box_append(boxS, entryCB)
                call gtk_list_item_set_child(listitem, boxS)  
                
            end select       
        end subroutine   


        ! Select the dropdown entry matching strCand.  Leaves the selection
        ! unchanged when no entry matches -- must never crash on a miss.
        ! (Historically the loop ran one past the end, where GTK returns NULL
        ! and convert_c_string segfaulted in strlen; the AUTUI crash.)
        subroutine setDropDownByString(dropDown, strCand)
            type(c_ptr) :: dropDown
            character(len=*) :: strCand

            type(c_ptr) :: model, cstr
            character(len=140) :: strDD
            integer :: n_items, ii

            model = gtk_drop_down_get_model(dropDown)
            if (.not. c_associated(model)) return
            n_items = g_list_model_get_n_items(model)

            do ii=0,n_items-1
                cstr = gtk_string_list_get_string(model, ii)
                if (.not. c_associated(cstr)) cycle
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

            binding_merit_row = .TRUE.
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
                        ! ~6 significant figures (was list-directed full precision).
                        write(colName, '(G0.6)') tmpDbl
                        
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
                    case (ID_CONSTRAINT_ROLE_COL)
                        block
                            character(len=10), dimension(2) :: roleNames
                            tmpInt = constraintColInfo(ID_COL)%getFunc_int(item)
                            roleNames = gatherRoleNames()
                            if (tmpInt == ID_ROLE_OBJECTIVE .OR. tmpInt == ID_ROLE_CONSTRAINT) then
                                call setDropDownByString(label, trim(roleNames(tmpInt)))
                            else
                                ! Blank row: default new entries to Constraint
                                ! (the table's historical content).
                                call setDropDownByString(label, trim(roleNames(ID_ROLE_CONSTRAINT)))
                            end if
                        end block
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
            binding_merit_row = .FALSE.

            ! Connect ONCE per widget: list items are recycled and bind runs
            ! repeatedly -- connecting every bind accumulated handlers, so a
            ! single user dropdown change fired N edit commands.
            if (constraintColInfo(ID_COL)%colType == ID_WIDGET_TYPE_DROPDOWN) then
                if (.not. c_associated(g_object_get_data(label, "zoa-dd-connected"//c_null_char))) then
                    call g_signal_connect(label, "notify::selected"//c_null_char, c_funloc(constraintDropDownChanged), c_null_ptr)
                    call g_object_set_data(label, "zoa-dd-connected"//c_null_char, label)
                end if
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
            use zoa_output, only: zoa_emit
            type(c_ptr), value, intent(in) :: but, gdata
            integer(kind=c_int) :: currRow

            currRow = getRowFromColumnView(gdata)

            ! getRowFromColumnView returns -1 when nothing is selected; do not
            ! issue "DEL CON 0" (invalid) in that case.
            if (currRow < 0) then
                call zoa_emit("Select a constraint row first, then Delete selected row", "black")
                return
            end if
            call PROCESKDP('DEL CON '//trim(int2str(currRow+1)))
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


          subroutine createConstraintToolbar(toolBox, cv)
            use gdk
            type(c_ptr), value :: cv

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
            btns(2) = gtk_button_new_from_icon_name ("dialog-question-symbolic"//c_null_char)
            
            call gtk_widget_set_tooltip_text(btns(1), "Update Database"//c_null_char)
            call gtk_widget_set_tooltip_text(btns(2), "Help on this Window"//c_null_char)
 
            do i=1,size(btns)
                call gtk_widget_set_valign(btns(i), GTK_ALIGN_START)
                call gtk_button_set_has_frame (btns(i), FALSE)
                call gtk_widget_set_focus_on_click (btns(i), FALSE)
                call hl_gtk_box_pack (toolBox, btns(i))
                call gtk_widget_set_has_tooltip(btns(i), 1_c_int)
            end do          
            call g_signal_connect(btns(1), 'clicked'//c_null_char, c_funloc(updateConstraints), cv)
            call g_signal_connect(btns(2), 'clicked'//c_null_char, c_funloc(helpWindow), c_null_ptr)

        end subroutine

        subroutine updateConstraints(widget, event, gdata) bind(c)
            use gdk, only: gdk_cursor_new_from_name
            implicit none
            type(c_ptr), value :: widget, event, gdata
           
            call rebuildTable(gdata, buildConstraintTable(), setConstraintColumns)
        end subroutine      
        
        subroutine helpWindow(widget, event, gdata) bind(c)
            use zoa_file_handler, only: openHelpFile
            use gdk, only: gdk_cursor_new_from_name
            implicit none
            type(c_ptr), value :: widget, event, gdata

            call openHelpFile('constraints_table.html')
           
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

            ! Programmatic selection changes during (re)binding are not edits.
            if (binding_merit_row) return

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

        ! Build the merit-entry line for UPD CON; CHA n from the row's current
        ! item, substituting colText for the edited column.  Role decides the
        ! form the unified parser expects:
        !   Constraint:  NAME <=|<|>> target
        !   Operand:     NAME target weight
        ! Editing the Role dropdown therefore converts the entry in place.
        function getConstraintChangeCommand(model, row, col, colText) result(outStr)
            use type_utils, only: str2int
            type(c_ptr), value :: model
            integer :: row, col
            character(len=*) :: colText
            character(len=200) :: outStr
            character(len=240) :: nameStr, targStr, weightStr
            character(len=10), dimension(2) :: roleNames
            character(len=1) :: conStr
            type(c_ptr) :: item
            integer :: conType, role

            item = g_list_model_get_item(model, row) ! row 0 indexed

            ! Gather every field, taking the edited column's new value from
            ! colText (dropdown edits arrive as display text).
            nameStr   = getColValueAsStr(item, constraintColInfo(ID_CONSTRAINT_NAME_COL))
            targStr   = getColValueAsStr(item, constraintColInfo(ID_CONSTRAINT_TARGET_COL))
            weightStr = getColValueAsStr(item, constraintColInfo(ID_CONSTRAINT_WEIGHT_COL))
            role      = constraint_item_get_role(item)
            conType   = constraint_item_get_contype(item)
            if (role /= ID_ROLE_OBJECTIVE .AND. role /= ID_ROLE_CONSTRAINT) role = ID_ROLE_CONSTRAINT

            select case (col)
            case (ID_CONSTRAINT_NAME_COL)
                nameStr = colText
            case (ID_CONSTRAINT_ROLE_COL)
                roleNames = gatherRoleNames()
                if (colText == trim(roleNames(ID_ROLE_OBJECTIVE))) then
                    role = ID_ROLE_OBJECTIVE
                else
                    role = ID_ROLE_CONSTRAINT
                end if
            case (ID_CONSTRAINT_TYPE_COL)
                select case (colText)
                case('='); conType = ID_CON_EXACT
                case('>'); conType = ID_CON_GREATER_THAN
                case('<'); conType = ID_CON_LESS_THAN
                end select
            case (ID_CONSTRAINT_TARGET_COL)
                targStr = colText
            case (ID_CONSTRAINT_WEIGHT_COL)
                weightStr = colText
            end select

            if (role == ID_ROLE_CONSTRAINT) then
                select case (conType)
                case(ID_CON_EXACT)
                    conStr = '='
                case(ID_CON_GREATER_THAN)
                    conStr = '>'
                case(ID_CON_LESS_THAN)
                    conStr = '<'
                case default
                    conStr = '='
                end select
                outStr = trim(nameStr)//' '//conStr//' '//trim(targStr)
            else
                outStr = trim(nameStr)//' '//trim(targStr)//' '//trim(weightStr)
            end if

        end function

end module
