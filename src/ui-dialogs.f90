! Save, open dialogs
module ui_dialogs

    implicit none

    contains

    function ui_open_file(parent_window, filename, cdir, startFileDir, iptFilter, iptFilterName) result(fileSelected)
        use iso_c_binding
        use gtk_hl_chooser

        type(c_ptr), value, intent(in) :: parent_window
        character(len=*), intent(inout) :: filename
        character(len=*), intent(inout) :: cdir
        character(len=*), intent(in) :: startFileDir
        character(len=*), intent(in) :: iptFilter
        character(len=*), intent(in) :: iptFilterName
      
        logical :: fileSelected
      
        integer(kind=c_int) :: isel
        character(len=120), dimension(:), allocatable :: chfile
        character(len=30), dimension(2) :: filters
        character(len=30), dimension(2) :: filtnames
        character(len=200) :: inln
        integer :: ios
        integer :: idxs
      
        filters(1) = iptFilter
        filters(2) = "*.txt"
        filtnames(1) = iptFilterName
        filtnames(2) = ".txt File"
        ! filters(1) = "*.seq"
        ! filters(2) = "*.txt"
        ! filtnames(1) = "CodeV .seq File"
        ! filtnames(2) = ".txt File"  
        fileSelected = .TRUE.
      
        isel = hl_gtk_file_chooser_show(chfile, cdir=cdir, create=FALSE,&
             & title="Select input file"//c_null_char, filter=filters, &
             & filter_name=filtnames, wsize=(/ 600_c_int, 400_c_int /), &
             & edit_filters=TRUE, initial_dir=startFileDir, &
             & parent=parent_window, all=TRUE)
        print *, "isel = hl_gtk_file_chooser_show=", isel
        if (isel == FALSE) then
          fileSelected = .FALSE.
          return
        else   ! No selection made
      
          filename = chfile(1)
          deallocate(chfile)
        end if
      
      end function    

      function ui_new_file(parent_window, filename, cdir, startFileDir, iptFilter, iptFilterName) result(fileSelected)
        use iso_c_binding
        use gtk_hl_chooser
        implicit none
        type(c_ptr), value, intent(in) :: parent_window
        character(len=*), intent(inout) :: filename
        character(len=*), intent(inout) :: cdir
        character(len=*), intent(in) :: startFileDir
        character(len=*), intent(in) :: iptFilter
        character(len=*), intent(in) :: iptFilterName
      
        logical :: fileSelected
      
        integer(kind=c_int) :: isel
        character(len=120), dimension(:), allocatable :: chfile
        character(len=30), dimension(2) :: filters
        character(len=30), dimension(2) :: filtnames
        character(len=200) :: inln
        integer :: ios
        integer :: idxs
      
        filters(1) = iptFilter
        filters(2) = "*.txt"
        filtnames(1) = iptFilterName
        filtnames(2) = ".txt File"
        ! filters(1) = "*.seq"
        ! filters(2) = "*.txt"
        ! filtnames(1) = "CodeV .seq File"
        ! filtnames(2) = ".txt File"  
        fileSelected = .TRUE.
      
        isel = hl_gtk_file_chooser_show(chfile, cdir=cdir, create=TRUE,&
             & title="Select input file"//c_null_char, filter=filters, &
             & filter_name=filtnames, wsize=(/ 600_c_int, 400_c_int /), &
             & edit_filters=TRUE, initial_dir=startFileDir, &
             & parent=parent_window, all=TRUE)
        print *, "isel = hl_gtk_file_chooser_show=", isel
        if (isel == FALSE) then
          fileSelected = .FALSE.
          return
        else   ! No selection made
      
          filename = chfile(1)
          deallocate(chfile)
        end if
      
      end function

end module