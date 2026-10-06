module kdp_utils

  interface
  subroutine updateTerminal(ftext, txtColor)

    character(len=*), intent(in) :: ftext
    character(len=*), intent(in)  :: txtColor
  end subroutine
  end interface

    contains
  
  subroutine OUTKDP(txt, code)
    use DATMAI, only: OUTLYNE, OUTLYNE_LONG
    character(len=*) :: txt
    integer, optional :: code

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
    use DATMAI, only: F1, F5, F6
    logical :: boolResult

    boolResult = .FALSE.
    IF (F1.EQ.0.AND.F5.EQ.1) boolResult = .TRUE.
    IF(F1.EQ.0.AND.F6.EQ.1) boolResult = .TRUE.
 
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
    integer :: ii
    character(len=:), allocatable :: strData

    ! Every value of the 2D image, one image row per line, so the Data tab
    ! pastes straight into MATLAB/Python as the matrix.  Explicit format
    ! (full double precision, fixed width): list-directed output laid a row
    ! out differently on each compiler.
    allocate(character(len=24*size(img,2)) :: strData)
    do ii=1,size(img,1)
        write(strData, '(*(ES24.16))') img(ii,:)
        call updateTerminal(trim(strData), "black")
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
    use iso_c_binding, only: c_null_char
    implicit none

    real, dimension(:) :: fldPoints
    real, dimension(:,:) :: dataArray
    character(len=*), dimension(:) :: colHeaders
    character(len=*), optional :: extraRowName
    integer, optional :: singleSurface
    integer :: i, j, c, sStart, sEnd, ncols, pos, tlen, lpad
    integer, parameter :: PAD = 3
    character(len=1024) :: lineStr
    character(len=32) :: entryStr
    integer, allocatable :: colW(:)

    ! Column layout: column 1 is the field value ("Field" header), then one
    ! column per data series.  Each column is wide enough for its header and
    ! every formatted value (F12.5) plus padding.  Headers are centered over
    ! their column; values are right-aligned within it.
    ncols = size(colHeaders) + 1
    allocate(colW(ncols))
    colW(1) = len('Field')
    do j=1,size(colHeaders)
      colW(j+1) = len_trim(colHeaders(j))
    end do
    do i=1,size(fldPoints)
      write(entryStr, '(F12.5)') fldPoints(i)
      colW(1) = max(colW(1), len_trim(adjustl(entryStr)))
      do j=1,size(colHeaders)
        write(entryStr, '(F12.5)') dataArray(i,j)
        colW(j+1) = max(colW(j+1), len_trim(adjustl(entryStr)))
      end do
    end do
    colW = colW + PAD

    call OUTKDP("Data vs Field Position")

    ! Header row -- each header centered in its column.
    lineStr = ' '
    pos = 1
    tlen = len('Field'); lpad = (colW(1)-tlen)/2
    lineStr(pos+lpad:pos+lpad+tlen-1) = 'Field'
    pos = pos + colW(1)
    do j=1,size(colHeaders)
      tlen = len_trim(colHeaders(j)); lpad = (colW(j+1)-tlen)/2
      lineStr(pos+lpad:pos+lpad+tlen-1) = trim(colHeaders(j))
      pos = pos + colW(j+1)
    end do
    call OUTKDP(trim(lineStr)//c_null_char)

    if (present(singleSurface)) then
      sStart = singleSurface
      sEnd = singleSurface
    else
      sStart = 1
      sEnd = size(fldPoints)
    end if

    ! Data rows -- each value right-aligned in its column.
    do i=sStart,sEnd
      lineStr = ' '
      pos = 1
      write(entryStr, '(F12.5)') fldPoints(i)
      entryStr = adjustl(entryStr); tlen = len_trim(entryStr)
      lineStr(pos+colW(1)-tlen:pos+colW(1)-1) = trim(entryStr)
      pos = pos + colW(1)
      do j=1,size(colHeaders)
        write(entryStr, '(F12.5)') dataArray(i,j)
        entryStr = adjustl(entryStr); tlen = len_trim(entryStr)
        lineStr(pos+colW(j+1)-tlen:pos+colW(j+1)-1) = trim(entryStr)
        pos = pos + colW(j+1)
      end do
      call OUTKDP(trim(lineStr))
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
            write(entryStr, '(F9.5)') dataArray(j,i+1) ! Surface starts at 0, passed array starts at 1?
            if (dataArray(j,i+1) > 0.0) then
              !lineStr = trim(lineStr)//'    '//trim(entryStr)
              lineStr = trim(lineStr)//blankStr(dataSpacing)//trim(entryStr)
            else
              lineStr = trim(lineStr)//blankStr(dataSpacing)//trim(entryStr)
            end if
        end do
        call OUTKDP(trim(lineStr))
    end do

  end subroutine




end module