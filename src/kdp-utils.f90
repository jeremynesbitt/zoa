module kdp_utils

    contains
  
  subroutine OUTKDP(txt, code)
    character(len=*) :: txt
    integer, optional :: code

    include "DATMAI.INC"

    OUTLYNE = txt

    if (present(code))  then 
        CALL SHOWIT(code)
    else
        CALL SHOWIT(0)
    end if

  end subroutine

  subroutine logDataVsSurface(dataArray, colHeaders, extraRowName, singleSurface)
    use global_widgets, only: curr_lens_data
    use iso_fortran_env, only: real64
    implicit none

    real(kind=real64), dimension(:,:) :: dataArray
    character(len=*), dimension(:) :: colHeaders
    character(len=*), optional :: extraRowName
    integer, optional :: singleSurface
    integer :: i, j, sStart, sEnd
    character(len=1024) :: lineStr
    character(len=230) :: entryStr

    ! Print header
    lineStr = 'Surface'
    do i=1,size(colHeaders)
        lineStr = trim(lineStr)//'     '//colHeaders(i)
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


    do i=sStart,sEnd
    !do i=0,curr_lens_data%num_surfaces   
        write(entryStr, '(I0.3)')  i
        lineStr = trim(adjustl(entryStr))
        if (i == curr_lens_data%num_surfaces) then
            if (present(extraRowName)) then
                lineStr = extraRowName
            else
                lineStr = '   '
            end if
        end if
            
        do j=1,size(colHeaders)
            write(entryStr, '(F12.5)') dataArray(j,i)
            if (dataArray(j,i) > 0.0) then
              !lineStr = trim(lineStr)//'    '//trim(entryStr)
              lineStr = trim(lineStr)//blankStr(6)//trim(entryStr)
            else
              lineStr = trim(lineStr)//'    '//' '//trim(entryStr)
            end if
        end do
        call OUTKDP(trim(lineStr))
    end do

  end subroutine

  function blankStr(strLen) result(blnk)
    implicit none
    character(:), allocatable :: blnk
    integer :: strLen, i

    allocate(character(len=strLen) :: blnk)
    do i=1,strLen
        blnk(i:i) = ' '
    end do 

  end function



end module