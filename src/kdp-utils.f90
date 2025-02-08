module kdp_utils

  interface
  subroutine updateTerminal(ftext, txtColor)

    character(len=*), intent(in) :: ftext
    character(len=*), intent(in)  :: txtColor
  end subroutine
  end interface

    contains
  
  subroutine OUTKDP(txt, code)
    character(len=*) :: txt
    integer, optional :: code

    include "DATMAI.INC"

    if(len(txt) > 140) then
      PRINT *, "WARNING:  output text truncated due to input character length longer than OUTLYNE"
      OUTLYNE_LONG = txt
      call SHOWIT(20)
    else
    

    OUTLYNE = txt

    if (present(code))  then 
        CALL SHOWIT(code)
    else
        CALL SHOWIT(0)
    end if
    
  end if

  end subroutine

  function inLensUpdateLevel() result(boolResult)
    logical :: boolResult
    include "DATMAI.INC"

    boolResult = .FALSE.
    IF (F1.EQ.0.AND.F5.EQ.1) boolResult = .TRUE.
    IF(F1.EQ.0.AND.F6.EQ.1) boolResult = .TRUE.
    IF(F6.EQ.1.AND.F10.EQ.0.OR.F6.EQ.1.AND.F11.EQ.0) boolResult = .TRUE.
    PRINT *, "Lens Update check is ", boolResult

  end function

  subroutine spaceDataForTable(xdata, ydata, xHeader, colHeaders, blankArray)
    implicit none
    real, dimension(:) :: xdata
    real, dimension(:,:) :: ydata
    character(len=*) :: xHeader
    character(len=*), dimension(:) :: colHeaders
    integer, dimension(:,:), intent(inout) :: blankArray
    integer, dimension(size(blankArray,1),size(blankArray,2)) :: lengthArray
    integer, dimension(size(blankArray,2)) :: maxPerColumn,  colStartPos
    integer :: i,j
    character(len=1024) :: lineStr
    character(len=230) :: entryStr 
    integer :: minBlankSpacing   

    
    !TODO:  This should be a parameter somwhere:
    minBlankSpacing = 2

    ! Need to figure out the size in character if each entry as printed
    ! TODO :  Should get print format from some common source as the logging fcns
    lengthArray(1,1) = len(xHeader)

    do j=2,size(colHeaders)+1
      lengthArray(1,j) = len(trim(colHeaders(j-1)))
    end do

    do i=1,size(xdata)
      !do i=0,curr_lens_data%num_surfaces   
          write(entryStr, '(F12.5)') xdata(i)
          lineStr = trim(adjustl(entryStr))
          lengthArray(i+1,1) = len(trim(lineStr))
          do j=1,size(colHeaders)
            write(entryStr, '(F12.5)') ydata(i,j)
            lineStr = trim(adjustl(entryStr))
            lengthArray(i+1,j+1) = len(trim(lineStr))            
          end do
    end do

    !Now figure out max value per column
    colStartPos(1) = 0
    do j=1,size(colHeaders)+1
      maxPerColumn(j) = maxval(lengthArray(:,j))
      if (j > 1) then
         colStartPos(j) = colStartPos(j-1) + maxPerColumn(j-1) + minBlankSpacing
      end if
    end do
    PRINT *, "maxPercolumn is ", maxPerColumn
    PRINT *, "colStartPos is ", colStartPos
    PRINT *, "lengthArray of first row is ", lengthArray(1,:)
    PRINT *, "lengthArray of fifth row is ", lengthArray(5,:)

    ! Finally we are ready to populate the blankArray
    ! size of blankArray is:
    ! rows:  the size of the data + 1 (one extra row for headers)
    ! cols:  the number of columns of dataArray +1 (xdata)
    ! Do not actually need the blankArray values in the last column as there is
    ! no spacing needed to next column
    do i=1,size(xdata)+1
          do j=1,size(blankArray,2)-1
            blankArray(i,j) = colStartPos(j+1)-lengthArray(i,j)-colStartPos(j)
         
          end do
    end do

    PRINT *, "blankArray of fifth row is ", blankArray(5,:)



  end subroutine

  subroutine logImageData(img)
    use globals, only: long
    implicit none

    real(long), intent(in) :: img(:,:)
    integer :: ii, jj
    character(len=20480) :: strData

    print *, "Size of img,2 is ", size(img,2)
    do ii=1,size(img,1)
        write(strData, *) (img(ii,jj), jj=1,256)!size(img,2))
        call updateTerminal(trim(adjustl(strData)), "black")
        !call OUTKDP(trim(strData))
    end do

  end subroutine

  subroutine log2DData(xData,yData, xHeader, yHeader)
    use globals, only: long
    use type_utils
    real(long), dimension(:) :: xData, yData
    character(len=*), optional :: xHeader, yHeader
    integer :: i
    character(len=1024) :: outStr

    
    ! Only output if both xHeader and yHeader are there
    if(present(xHeader)) then 
      if(present(yHeader)) then 
        ! TODO:  adjust blanks for xHeader and yHeader to align with data
        call OUTKDP(blankStr(5)//xHeader//blankStr(7)//yHeader)
      end if 
    end if
    do i=1,size(xData)
      write(outStr, '(F12.5,A5,F12.5)') xData(i),blankStr(5), yData(i)
       !call OUTKDP(trim(real2str(xData(i)))//blankStr(5)//trim(real2str(yData(i))))
       call OUTKDP(trim(outStr))
    end do


  end subroutine

  subroutine logDataVsField(fldPoints, dataArray, colHeaders, extraRowName, singleSurface)
    use global_widgets, only: curr_lens_data
    use iso_fortran_env, only: real64
    use iso_c_binding, only: c_null_char
    use type_utils, only: blankStr
    implicit none

    real, dimension(:) :: fldPoints
    real, dimension(:,:) :: dataArray
    character(len=*), dimension(:) :: colHeaders
    character(len=*), optional :: extraRowName
    integer, optional :: singleSurface
    integer :: i, j, sStart, sEnd
    character(len=1024) :: lineStr
    character(len=230) :: entryStr
    integer, allocatable :: blankArray(:,:)

    allocate(blankArray(size(fldPoints)+1,size(dataArray,2)+1))
    call spaceDataForTable(fldPoints, dataArray, 'Field', colHeaders, blankArray)
    print *, "blankArray for first row is ", blankArray(1,:)
    print *, "colHeaders is ", colHeaders


    ! Print header
    !lineStr = 'Field'//blankStr(blankArray(1,1))
    lineStr = 'Field'//blankStr(blankArray(1,1)+5)//trim(colHeaders(1))
    print *, "lineStry is ", trim(lineStr)
    do i=2,size(colHeaders)
      print *, "len of lineStr is ", len(trim(lineStr))
      lineStr = trim(lineStr)//blankStr(blankArray(1,i)+5)//trim(colHeaders(i))
        
        !lineStr = trim(lineStr)//blankStr(10)//trim(colHeaders(i))
    end do
    call OUTKDP("Data vs Field Position")
    
    !print *, "len of lineStr is ", len(trim(lineStr))
    !call updateTerminalLog(trim(lineStr), "black")
    call OUTKDP(trim(lineStr)//c_null_char)
   ! print *, "after OUTKDP call"
   ! print *, "len of blankstr(6) is ", len(blankStr(6))
    print *, trim(lineStr)
    if (present(singleSurface)) then
       ! PRINT *, "SingleSurface is ", singleSurface
        sStart = singleSurface
        sEnd = singleSurface
    else
        sStart = 1
        sEnd = size(fldPoints)

    end if
    !PRINT *, "sStart is ", sStart
    !PRINT *, "sEnd is ", sEnd


    do i=sStart,sEnd
    !do i=0,curr_lens_data%num_surfaces   
        write(entryStr, '(F12.5)') fldPoints(i)
        lineStr = trim(adjustl(entryStr))
        if (i == curr_lens_data%num_surfaces) then
            if (present(extraRowName)) then
                lineStr = extraRowName
            else
                lineStr = '   '
            end if
        end if
            
        do j=1,size(colHeaders)
            write(entryStr, '(F12.5)') dataArray(i,j)
            lineStr = trim(lineStr)//blankStr(blankArray(i+1,j))//trim(adjustl(entryStr))
            ! if (i.EQ.6) then
            !   PRINT *, "j is ", j
            !   PRINT *, "blankArray is ", blankArray(i+1,j)
            !   PRINT *, "len of blankStr is ", 
            ! end if
            ! if (dataArray(j,i) > 0.0) then
            !   !lineStr = trim(lineStr)//'    '//trim(entryStr)
            !   lineStr = trim(lineStr)//blankStr(6)//trim(entryStr)
            ! else
            !   lineStr = trim(lineStr)//'    '//' '//trim(entryStr)
            ! end if
        end do
        call OUTKDP(trim(lineStr))
        print *, trim(lineStr)
        !PRINT *, "len of lineStr is ", len(trim(lineStr))
    end do

  end subroutine

  subroutine logDataVsSurface(dataArray, colHeaders, extraRowName, singleSurface)
    use global_widgets, only: curr_lens_data
    use iso_fortran_env, only: real64
    use type_utils, only: blankStr
    use mod_lens_data_manager
    
    implicit none

    real(kind=real64), dimension(:,:) :: dataArray
    character(len=*), dimension(:) :: colHeaders
    character(len=*), optional :: extraRowName
    integer, optional :: singleSurface
    integer :: i, j, sStart, sEnd
    character(len=1024) :: lineStr
    character(len=230) :: entryStr

    integer :: numDataChars = 9 ! Linked to format which is hard coded below
    integer :: dataSpacing = 1
    integer :: headerSpacing 

    headerSpacing =  numDataChars-2*len(trim(colHeaders(1)))+1
    ! Print header
    lineStr = 'SRF'//blankStr(dataSpacing)//blankStr(headerSpacing)//colHeaders(1)
    do i=2,size(colHeaders)
        lineStr = trim(lineStr)//blankStr(dataSpacing)//blankStr(2*headerSpacing-&
        & len(trim(colHeaders(i-1)))+1)//colHeaders(i)
    end do
    call OUTKDP(trim(lineStr))
    
    if (present(singleSurface)) then
        PRINT *, "SingleSurface is ", singleSurface
        sStart = singleSurface
        sEnd = singleSurface
    else
        sStart = 0
        sEnd = curr_lens_data%num_surfaces

    end if
    PRINT *, "sStart is ", sStart
    PRINT *, "sEnd is ", sEnd

    if(.not.present(extraRowName)) sEnd = sEnd -1 

    do i=sStart,sEnd
    !do i=0,curr_lens_data%num_surfaces   
        entryStr = ldm%getSurfName(i)
        !write(entryStr, '(I0.3)')  i
        lineStr = trim(adjustl(entryStr))
        if (i == curr_lens_data%num_surfaces) then
            if (present(extraRowName)) then
                lineStr = extraRowName
            else
                lineStr = '   '
            end if
        end if
            
        do j=1,size(colHeaders)
            print *, "Processing Column Header ", colHeaders(j)
            print *, "surf is ", i
            write(entryStr, '(F9.5)') dataArray(j,i+1) ! Surface starts at 0, passed array starts at 1?
            PRINT *, "length of entryStry is ", len(trim(entryStr))
            if (dataArray(j,i+1) > 0.0) then
              !lineStr = trim(lineStr)//'    '//trim(entryStr)
              lineStr = trim(lineStr)//blankStr(dataSpacing)//trim(entryStr)
            else
              lineStr = trim(lineStr)//blankStr(dataSpacing)//trim(entryStr)
            end if
            PRINT *, "lineStr is "//trim(lineStr)
        end do
        call OUTKDP(trim(lineStr))
    end do

  end subroutine





end module