module optim_debug
    use GLOBALS, only: long
    use type_utils


    contains

! Notes for formal solution
! Create some struct that holds type information for each variable
! pass this to optimizer
! in optimizer func parse struct to figure out how to update lens
! for each var each iteration
 
! also make a type for constraints, that gives info on bounds or 
! target if exact
    
! Comptelely bypass existing optim struct?  Seems like I don't need it for variables
! For merit function I don't need it either - can store each separately
! Worry there are some locking or other benefits I will regret if I completely abandon
! HOw to deal with merit function?  all cmds?                


subroutine test_slsqp_new()

    use slsqp_module
    use slsqp_kinds

    implicit none

    integer,parameter               :: n = 3                    !! number of optimization variables
    integer,parameter               :: m = 2                    !! total number of constraints
    integer,parameter               :: meq = 1                  !! number of equality constraints
    integer,parameter               :: max_iter = 100           !! maximum number of allowed iterations
    real(long),dimension(n),parameter :: xl = [-10.0_long, -10.0_long, -10.0_long]  !! lower bounds
    real(long),dimension(n),parameter :: xu = [10.0_long,  10.0_long,  10.0_long]  !! upper bounds
    real(long),parameter              :: acc = 1.0e-7_long          !! tolerance
    integer,parameter               :: linesearch_mode = 1      !! use inexact linesearch.

    type(slsqp_solver)    :: solver      !! instantiate an slsqp solver
    real(long),dimension(n) :: x           !! optimization variable vector
    integer               :: istat       !! for solver status check
    logical               :: status_ok   !! for initialization status check
    integer               :: iterations  !! number of iterations by the solver

    x = [1.0_long, 2.0_long, 3.0_long] ! initial guess    
    
    call solver%initialize(n,m,meq,max_iter,acc,test_func,test_grad,&
                            xl,xu,linesearch_mode=linesearch_mode,status_ok=status_ok,&
                            report=report_iteration,&
                            alphamin=0.1_long, alphamax=0.5_long) !to limit search steps

    if (status_ok) then
        call solver%optimize(x,istat,iterations)
        write(*,*) ''
        write(*,*) 'solution   :', x
        write(*,*) 'istat      :', istat
        write(*,*) 'iterations :', iterations
        write(*,*) ''
    else
        error stop 'error calling slsqp.'
    end if                            

end subroutine


subroutine test_slsqp()

    use slsqp_module
    use slsqp_kinds

    implicit none

    integer,parameter               :: n = 3                    !! number of optimization variables
    integer,parameter               :: m = 1                    !! total number of constraints
    integer,parameter               :: meq = 1                  !! number of equality constraints
    integer,parameter               :: max_iter = 25          !! maximum number of allowed iterations
    real(long),dimension(n),parameter :: xl = [-100.0_long, -10.0_long, -10.0_long]  !! lower bounds
    real(long),dimension(n),parameter :: xu = [100.0_long,  10.0_long,  10.0_long]  !! upper bounds
    real(long),parameter              :: acc = 1.0e-2_long          !! tolerance
    real(long),parameter              :: gradient_delta = 1.0e-4_wp 
    integer,parameter               :: linesearch_mode = 1      !! use inexact linesearch.
    integer, parameter :: gradient_mode = 1

    type(slsqp_solver)    :: solver      !! instantiate an slsqp solver
    real(long),dimension(n) :: x           !! optimization variable vector
    integer               :: istat       !! for solver status check
    logical               :: status_ok   !! for initialization status check
    integer               :: iterations  !! number of iterations by the solver

    x = [0.0_long, 0.00833_long, -0.02899_long] ! initial guess    
    
    call solver%initialize(n,m,meq,max_iter,acc,test_func_spo_efl,test_grad,&
                            xl,xu,linesearch_mode=linesearch_mode,status_ok=status_ok,&
                            report=report_iteration,&
                            alphamin=0.1_long, alphamax=0.5_long, &
                            gradient_mode=gradient_mode, gradient_delta=gradient_delta, toldf=.05_long) !to limit search steps

    if (status_ok) then
        call solver%optimize(x,istat,iterations)
        write(*,*) ''
        write(*,*) 'solution   :', x
        write(*,*) 'istat      :', istat
        write(*,*) 'iterations :', iterations
        write(*,*) ''
    else
        error stop 'error calling slsqp.'
    end if                            

end subroutine


subroutine test_slsqp_comp_jac()

    use slsqp_module
    use slsqp_kinds

    implicit none

    integer,parameter               :: n = 3                    !! number of optimization variables
    integer,parameter               :: m = 2                    !! total number of constraints
    integer,parameter               :: meq = 1                  !! number of equality constraints
    integer,parameter               :: max_iter = 100           !! maximum number of allowed iterations
    real(long),dimension(n),parameter :: xl = [-100.0_long, -10.0_long, -10.0_long]  !! lower bounds
    real(long),dimension(n),parameter :: xu = [100.0_long,  10.0_long,  10.0_long]  !! upper bounds
    real(long),parameter              :: acc = 1.0e-7_long          !! tolerance
    real(long),parameter              :: gradient_delta = 1.0e-5_wp 
    integer,parameter               :: linesearch_mode = 1      !! use inexact linesearch.
    integer, parameter :: gradient_mode = 1

    type(slsqp_solver)    :: solver      !! instantiate an slsqp solver
    real(long),dimension(n) :: x           !! optimization variable vector
    integer               :: istat       !! for solver status check
    logical               :: status_ok   !! for initialization status check
    integer               :: iterations  !! number of iterations by the solver

    x = [0.0_long, 0.00833_long, -0.02899_long] ! initial guess    
    
    call solver%initialize(n,m,meq,max_iter,acc,test_func,test_grad,&
                            xl,xu,linesearch_mode=linesearch_mode,status_ok=status_ok,&
                            report=report_iteration,&
                            alphamin=0.1_long, alphamax=0.5_long, &
                            gradient_mode=gradient_mode, gradient_delta=gradient_delta) !to limit search steps

    if (status_ok) then
        call solver%optimize(x,istat,iterations)
        write(*,*) ''
        write(*,*) 'solution   :', x
        write(*,*) 'istat      :', istat
        write(*,*) 'iterations :', iterations
        write(*,*) ''
    else
        error stop 'error calling slsqp.'
    end if                            

end subroutine
    

subroutine test_slsqp_analytical_gradient()

    use slsqp_module
    use slsqp_kinds

    implicit none

    integer,parameter               :: n = 3                    !! number of optimization variables
    integer,parameter               :: m = 2                    !! total number of constraints
    integer,parameter               :: meq = 1                  !! number of equality constraints
    integer,parameter               :: max_iter = 100           !! maximum number of allowed iterations
    real(long),dimension(n),parameter :: xl = [-100.0_long, -10.0_long, -10.0_long]  !! lower bounds
    real(long),dimension(n),parameter :: xu = [100.0_long,  10.0_long,  10.0_long]  !! upper bounds
    real(long),parameter              :: acc = 1.0e-7_long          !! tolerance
    integer,parameter               :: linesearch_mode = 1      !! use inexact linesearch.

    type(slsqp_solver)    :: solver      !! instantiate an slsqp solver
    real(long),dimension(n) :: x           !! optimization variable vector
    integer               :: istat       !! for solver status check
    logical               :: status_ok   !! for initialization status check
    integer               :: iterations  !! number of iterations by the solver

    x = [0.0_long, 0.00833_long, -0.02899_long] ! initial guess    
    
    call solver%initialize(n,m,meq,max_iter,acc,test_func,test_grad,&
                            xl,xu,linesearch_mode=linesearch_mode,status_ok=status_ok,&
                            report=report_iteration,&
                            alphamin=0.1_long, alphamax=0.5_long) !to limit search steps

    if (status_ok) then
        call solver%optimize(x,istat,iterations)
        write(*,*) ''
        write(*,*) 'solution   :', x
        write(*,*) 'istat      :', istat
        write(*,*) 'iterations :', iterations
        write(*,*) ''
    else
        error stop 'error calling slsqp.'
    end if                            

end subroutine


! subroutine test_func_sandbox(me,x,f,c)
!     use DATSUB, only: OPERND
!     use slsqp_module
!     use slsqp_kinds
!     use zoa_ui
!     use global_widgets, only: ioConfig

!     implicit none

!     class(slsqp_solver),intent(inout) :: me
!     real(long),dimension(:),intent(in)  :: x   !! optimization variable vector
!     real(long),intent(out)              :: f   !! value of the objective function
!     real(long),dimension(:),intent(out) :: c   !! the constraint vector `dimension(m)`,
!                                              !! equality constraints (if any) first.

!     call ioConfig%setTextView(ID_TERMINAL_KDPDUMP) 
!     call PROCESKDP('U L')
!     do i=1,numVars
!         select case (vars(i)%type)

!         case(CURVATURE)
!             call PROCESKDP('CHG 1; CV '//real2str(x(i))) 
!             !Or directly update via ldm
!         case(THICKNESS)
!             call PROCESKDP('CHG 3; TH '//real2str(x(i)))
!             ! Or update directly via ldm
!         end select

!     end do
!     call PROCESKDP('CHG 3; TH '//real2str(x(1)))
!     call PROCESKDP('CHG 1; CV '//real2str(x(2)))
!     call PROCESKDP('CHG 2; CV '//real2str(x(3)))
!     call PROCESKDP('EOS')
!     call PROCESKDP('SUR SA')
!  ! Get updated merit data and write to output csv
!     call PROCESKDP('OPRD CFG, 1') ! for operand calcs
!     call ioConfig%setTextView(ID_TERMINAL_DEFAULT)  

   
!     !call write_csv('/Users/jeremy/Fortran/zoa/build/optimOutput.csv',OPERND(1:2,4), 1, 2)
!     f = OPERND(1,4)
!     print *, "OPERND(1:2,4) is ", OPERND(1:2,4)

!     c(1) = OPERND(2,4) - 50.0_long

!     !f = x(1)**2 + x(2)**2 + x(3)  !objective function

!     !c(1) = x(1)*x(2) - x(3)       !equality constraint (==0)
    
    
!     !c(2) = x(3) - 1.0_long          !inequality constraint (>=0)

! end subroutine test_func_sandbox


subroutine test_func_spo_efl(me,x,f,c)
    use DATSUB, only: OPERND
    use slsqp_module
    use slsqp_kinds
    use zoa_ui
    use global_widgets, only: ioConfig

    implicit none

    class(slsqp_solver),intent(inout) :: me
    real(long),dimension(:),intent(in)  :: x   !! optimization variable vector
    real(long),intent(out)              :: f   !! value of the objective function
    real(long),dimension(:),intent(out) :: c   !! the constraint vector `dimension(m)`,
                                             !! equality constraints (if any) first.

    call ioConfig%setTextView(ID_TERMINAL_KDPDUMP) 
    call PROCESKDP('U L')
    call PROCESKDP('CHG 3; TH '//real2str(x(1)))
    call PROCESKDP('CHG 1; CV '//real2str(x(2)))
    call PROCESKDP('CHG 2; CV '//real2str(x(3)))
    call PROCESKDP('EOS')
    call PROCESKDP('SUR SA')
 ! Get updated merit data and write to output csv
    call PROCESKDP('OPRD CFG, 1') ! for operand calcs
    call ioConfig%setTextView(ID_TERMINAL_DEFAULT)  

   
    !call write_csv('/Users/jeremy/Fortran/zoa/build/optimOutput.csv',OPERND(1:2,4), 1, 2)
    f = OPERND(1,4)
    print *, "OPERND(1:2,4) is ", OPERND(1:2,4)

    c(1) = OPERND(2,4) - 50.0_long

    !f = x(1)**2 + x(2)**2 + x(3)  !objective function

    !c(1) = x(1)*x(2) - x(3)       !equality constraint (==0)
    
    
    !c(2) = x(3) - 1.0_long          !inequality constraint (>=0)

end subroutine test_func_spo_efl

subroutine test_func(me,x,f,c)
    use slsqp_module
    use slsqp_kinds

    !!  Compute the objective function and constraints
    !!
    !!  Minimize:
    !!
    !!   * \( f = x_1^2 + x_2^2 + x_3 \)
    !!
    !!  Subject to:
    !!
    !!   * \( c_1 = x_1 x_2 - x_3 = 0 \)
    !!   * \( c_2 = x_3 - 1 \ge 0 \)

    implicit none

    class(slsqp_solver),intent(inout) :: me
    real(long),dimension(:),intent(in)  :: x   !! optimization variable vector
    real(long),intent(out)              :: f   !! value of the objective function
    real(long),dimension(:),intent(out) :: c   !! the constraint vector `dimension(m)`,
                                             !! equality constraints (if any) first.

    f = x(1)**2 + x(2)**2 + x(3)  !objective function

    c(1) = x(1)*x(2) - x(3)       !equality constraint (==0)
    c(2) = x(3) - 1.0_long          !inequality constraint (>=0)

end subroutine test_func

subroutine test_grad(me,x,g,a)
    use slsqp_module
    use slsqp_kinds

    !! compute the gradients.

    implicit none

    class(slsqp_solver),intent(inout)   :: me
    real(long),dimension(:),intent(in)    :: x    !! optimization variable vector
    real(long),dimension(:),intent(out)   :: g    !! objective function partials w.r.t x `dimension(n)`
    real(long),dimension(:,:),intent(out) :: a    !! gradient matrix of constraints w.r.t. x `dimension(m,n)`

    g(1) = 2.0_long*x(1)
    g(2) = 2.0_long*x(2)
    g(3) = 1.0_long

    a(1,1) = x(2)
    a(1,2) = x(1)
    a(1,3) = -1.0_long

    a(2,1) = 0.0_long
    a(2,2) = 0.0_long
    a(2,3) = 1.0_long

end subroutine test_grad

subroutine report_iteration(me,iter,x,f,c)
    use slsqp_module
    use slsqp_kinds
    use kdp_utils, only: OUTKDP

    !! report an iteration (print to the console).

    use, intrinsic :: iso_fortran_env, only: output_unit

    implicit none

    class(slsqp_solver),intent(inout) :: me
    integer,intent(in)                :: iter
    real(long),dimension(:),intent(in)  :: x
    real(long),intent(in)               :: f
    real(long),dimension(:),intent(in)  :: c
    character(len=1024) :: output_line

    !write a header:
    if (iter==0) then
        ! write(output_unit,'(*(A20,1X))') 'iteration', &
        !                                  'x(1)', 'x(2)', 'x(3)', &
        !                                  'f(1)', 'c(1)', 'c(2)'
        write(output_line,'(*(A20,1X))') 'iteration', &
                                         'x(1)', 'x(2)', 'x(3)', &
                                         'f(1)', 'c(1)', 'c(2)'                                         
        call OUTKDP(output_line)
    end if

    
    !write the iteration data:
    write(output_line,'(I20,1X,(*(F20.16,1X)))') iter,x,f,c
    call OUTKDP(output_line)


    !write(output_unit,'(I20,1X,(*(F20.16,1X)))') iter,x,f,c


end subroutine report_iteration


subroutine simple_matlab_link()
    use DATSUB, only: OPERND
    implicit none
    integer, parameter :: max_rows = 1
    integer, parameter :: max_cols = 3
    integer :: num_rows, num_cols
    real(kind=long) :: inData(max_rows,max_cols)

    ! use current lens, hard code values for input/output
    call read_csv('/Users/jeremy/Fortran/zoa/build/optimInput.csv', inData, num_rows, num_cols)
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
    call write_csv('/Users/jeremy/Fortran/zoa/build/optimOutput.csv',OPERND(1:2,4), 1, 2)
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
    print *, "line is ", line
    if (ios == 0) then
        num_rows = num_rows + 1
        !num_cols = count_chars(line, ',') + 1
        call parse(trim(line), ',', tokens, numTokens) 
        print *, "numTokens is ", numTokens
        do i=1,numTokens
            print *, "Data is ", tokens(i)
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