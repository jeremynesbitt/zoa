module optim_debug
    use GLOBALS, only: long
    use type_utils


    contains
subroutine simple_matlab_link()
    use DATSUB, only: OPERND
    implicit none
    integer, parameter :: max_rows = 1
    integer, parameter :: max_cols = 3
    integer :: num_rows, num_cols
    real(kind=long) :: inData(max_rows,max_cols)

    ! use current lens, hard code values for input/output
    call read_csv('optimInput.csv', inData, num_rows, num_cols)
    ! Hard coded data:  S3 Thickness, S1, S2 Curvature
    ! Print the read data
 
    print *, 'Data:', inData


    call PROCESKDP('U L')
    call PROCESKDP('CHG 3; TH '//real2str(inData(1,1)))
    call PROCESKDP('CHG 1; CV '//real2str(inData(1,2)))
    call PROCESKDP('CHG 2; CV '//real2str(inData(1,3)))
    call PROCESKDP('EOS')
    call PROCESKDP('SUR SA')

    ! Get updated merit data and write to output csv
    call PROCESKDP('OPRD CFG, 1') ! for operand calcs
    call write_csv('optimOutput.csv',OPERND(1:2,4), 1, 2)
    print *, "OPERND(1:2,4) is ", OPERND(1:2,4)


end subroutine

subroutine write_csv(filename, data, num_rows, num_cols)
    implicit none
    character(len=*), intent(in) :: filename
    real(kind=long) :: data(:)
    integer, intent(in) :: num_rows, num_cols

    integer :: i, j, ios
    integer :: unit

    unit = 10  ! File unit number

    open(unit, file=filename, status='replace', action='write', iostat=ios)
    if (ios /= 0) then
        print *, 'Error opening file for writing:', filename
        return
    end if

    ! Write data to CSV file
    do i = 1, num_rows
        do j = 1, num_cols
            if (j == num_cols) then
                !write(unit, '(F8.5)') data(j)  ! No comma for last element
                write(unit, *) data(j)  ! No comma for last element
            else
                !write(unit, '(F8.5,1X)', iostat=ios) data(j)  ! Comma and space
                write(unit, *, iostat=ios) data(j)  ! Comma and space
                !write(unit, *) ','
            end if
        end do
        write(unit, *)  ! New line after each row
    end do

    close(unit)
    end subroutine


subroutine read_csv(filename, data, num_rows, num_cols)
    use strings, only: parse
    implicit none
    character(len=*), intent(in) :: filename
    real(kind=long), intent(out) :: data(:,:)
    integer, intent(out) :: num_rows, num_cols

    integer :: i, j, ios
    character(len=100) :: line
    character(len=20) :: value
    integer :: unit, eof, max_cols
    character(len=80) :: tokens(40)
    integer :: numTokens


            

    num_rows = 0
    num_cols = 0
    unit = 10  ! File unit number

    open(unit, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, 'Error opening file:', filename
        return
    end if

    ! Read the first line to determine the number of columns
    read(unit, '(A)', iostat=ios) line
    if (ios == 0) then
        num_rows = num_rows + 1
        !num_cols = count_chars(line, ',') + 1
        call parse(trim(line), ',', tokens, numTokens) 
        do i=1,numTokens
            data(num_rows,i) = str2real8(tokens(i))
            
        end do
       
    end if

    ! Read the rest of the file
   ! do
   !     read(unit, '(A)', iostat=ios) line
   !     if (ios /= 0) exit
   !     num_rows = num_rows + 1
   !     call parse_line(line, data(num_rows, :), num_cols)
   ! end do

    close(unit)

end subroutine read_csv

subroutine parse_line(line, row, num_cols)
    implicit none
    character(len=*), intent(in) :: line
    real(kind=long), intent(out) :: row(:)
    integer, intent(in) :: num_cols

    integer :: i, ios
    character(len=20) :: value
    integer :: start, end

    start = 1
    do i = 1, num_cols
        end = index(line(start:), ',') + start - 1
        if (end == start - 1) end = len_trim(line) + 1
        read(line(start:end-1), *, iostat=ios) row(i)
        start = end + 1
    end do

end subroutine parse_line

function count_chars(line, char) result(count)
    implicit none
    character(len=*), intent(in) :: line, char
    integer :: count, pos

    count = 0
    pos = index(line, char)
    do while (pos > 0)
        count = count + 1
        pos = index(line(pos + 1:), char)
    end do

end function count_chars



end module