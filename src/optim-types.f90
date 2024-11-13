module optim_types
    use GLOBALS,only: long

    
    ! Try a similar design as the command parser - for each operand a function is supplied which will get the value.
    ! Some operands will have multiple inputs and this should handle it.   
    type :: operand
    real(long) :: op !Value - always a single valued result
    character(len=4) :: name
    integer :: iW, iF, density
    real(long) :: px, py, hx, hy, targ
    procedure (operandFunc), pointer :: func
    end type

    type :: constraint
       character(len=4) :: name
       real(long) :: con
       logical :: exact ! bound if false
       real(long) :: targ, ub, lb ! not all 3 are used
       procedure (constraintFunc), pointer :: func ! share same interface for func
    end type

    ! The interface will include all possible inputs as optional args.  This will limit number of possible inputs for better or worse.
    ! I may end up regretting this..
    abstract interface
    function operandFunc (self)
        import long
        import operand
        class(operand) :: self
        ! integer, optional :: iW, iF, density
        ! real(long), optional :: px, py, hx, hy
        real(long) :: operandFunc
    end function operandFunc
    function constraintFunc(self)
        import constraint
        import long
        class(constraint) :: self
        real(long) :: constraintFunc
    end function


 end interface        

    ! Type codes
    integer, parameter :: VAR_CURV = 1
    integer, parameter :: VAR_THI = 2


    type(operand), dimension(100) :: operands
    type(operand), dimension(100) :: operandsInUse
    type(constraint), dimension(100) :: constraints
    type(constraint), dimension(100) :: constraintsInUse


    integer :: nV !Number of variables
    integer :: VARS(1000,2) ! Hard code number of vars for now!  index 1 is surface, index 2 is var type


    integer :: nO, nC ! number of operands and constraints in use


    contains

    subroutine initializeOptimizer()

        nV = 0 ! Num variables is 0
        nO = 0 ! Initialize means 0 operands in use
        nC = 0 ! num constraints is 0

        operands(1)%name = 'SPO'
        operands(1)%func => getSPO
        constraints(1)%name = 'EFL'
        constraints(1)%func => getEFLConstraint     

    end subroutine


    function getSPO(self) result(res)
        use DATSPD, only: RMS
        implicit none
        class(operand) :: self
        !integer, optional :: iW, iF, density
        !real(long), optional :: px, py, hx, hy        
        real(long) :: res


        CALL PROCESKDP("SPD")
        res = RMS

    end function

    ! function getSPO(iW, iF, px,py, hx,hy, density) result(res)
    !     integer, optional :: iW, iF, density
    !     real(long), optional :: px, py, hx, hy        
    !     real(long) :: res

    !     res = 0.1

    ! end function

    function getEFLConstraint(self) result(res)
        use mod_lens_data_manager
        class(constraint) :: self
        real(long) :: res
        ! integer, optional :: iW, iF, density
        ! real(long), optional :: px, py, hx, hy        
        ! real(long) :: res

        res = ldm%getEFL() - self%targ

    end function

    subroutine addOptimVariable(surf, int_code)
        integer :: surf, int_code

        nV = nV + 1
        VARS(nV,1) = surf
        VARS(nV,2) = int_code
    end subroutine

    subroutine addOperand(name, targ)
        character(len=*) :: name
        real(long), optional :: targ
        integer :: idx
        
        idx = isNameInListNew(name, operands)
        if (idx.ne.0) then
            nO = nO +1
            operandsInUse(nO) = operands(idx)
            if(present(targ)) then
                operandsInUse(n0)%targ = targ
            else
                operandsInUse(n0)%targ = 0.0_long
            end if
            ! Continue for all other constraints
        end if

        ! if isNameInList ne 0 then
        !    operandsInUse(n0+1) = operand(i)
        !    add or init other vars


    end subroutine

    subroutine addConstraint(name, val, eq, lb, ub)
        character(len=*) :: name
        real(long) :: val
        logical, optional :: eq, lb, ub

        if (isNameInList(name, constraints)) then
            nC = nC +1
            constraintsInUse(nC)%name = name 
            if(present(eq)) then
                constraintsInUse(nc)%exact = .TRUE.
                constraintsInUse(nC)%targ = val
            end if
            if(present(lb)) then
                constraintsInUse(nc)%exact = .FALSE.
                constraintsInUse(nC)%lb = val
            end if
            if(present(ub)) then
                constraintsInUse(nc)%exact = .FALSE.
                constraintsInUse(nC)%ub = val
            end if
        end if
                        

    end subroutine

    function isNameInListNew(name, obj) result(idx)
        character(len=*) :: name
        integer :: i
        integer :: idx 
        class(*), dimension(:), target :: obj
   
        type (constraint), dimension(:), pointer :: tmp

        ok = 0
        select type (obj)
          type is (constraint)
          tmp => obj
          do i=1,size(tmp)
            if (name == tmp(i)%name) then
                ! Found value
                idx = i
                return
            end if
        end do
        end select
    end function        

    function isNameInList(name, obj) result(ok)

        character(len=*) :: name
        integer :: i
        logical :: ok
        class(*), dimension(:), target :: obj
   
        type (constraint), dimension(:), pointer :: tmp

        ok = .FALSE.
        select type (obj)
          type is (constraint)
          tmp => obj
          do i=1,size(tmp)
            if (name == tmp(i)%name) then
                ! Found value
                ok = .TRUE.
                return
            end if
        end do
        end select
    end function


    subroutine updateLensDuringOptimization(x)
        use type_utils
        real(long), dimension(:) :: x

        integer :: i

        call PROCESKDP('U L')

        do i=1,nV
            select case(VARS(i,2))
            case(VAR_CURV)
                call PROCESKDP('CHG '//int2str(VARS(i,1))//' ; TH '//real2str(x(i)))
            case(VAR_THI)
                call PROCESKDP('CHG '//int2str(VARS(i,1))//' ; CV '//real2str(x(i)))
            end select

        end do

        call PROCESKDP('EOS')

    end subroutine

    function getLowerBounds() result(lbArr)
        real(long), dimension(nV) :: lbArr
        integer :: i

        do i=1,nV
            lbArr(i) = -1*huge(0.0_long)
        end do
        
    end function

    function getUpperBounds() result(ubArr)
        implicit none
        real(long), dimension(nV) :: ubArr
        integer :: i

        do i=1,nV
            ubArr(i) = huge(0.0_long)
        end do
        
    end function  
    
    function getNumberofEqualityConstraints() result(neq)
        implicit none
        integer :: neq
        integer :: i
        
        neq = 0
        do i=1,nC
            if (constraintsInUse(i)%exact) neq = neq+1
        end do

    end function


    subroutine updateThiOptimVarsNew(s0, sf, intCode)
        use type_utils
        integer, intent(in) :: s0, sf, intCode
        integer :: i

        ! New Code
        select case (intCode)

        case(0) ! Make Variable
            if (s0==sf) then
                call addOptimVariable(s0,VAR_THI)
                !CALL PROCESKDP('UPDATE VARIABLE ; TH, '//trim(int2str(s0))//'; EOS ')
            else
                !call PROCESKDP('UPDATE VARIABLE')
                do i=s0,sf
                    call addOptimVariable(i,VAR_THI)
                    !CALL PROCESKDP('TH, '//trim(int2str(i)))
                end do
                !call PROCESKDP('EOS')
            end if

        end select

    end subroutine

    !TODO:  Refactor with updateThiOptimVars
    subroutine updateCurvOptimVarsNew(s0, sf, intCode)
        use type_utils
        integer, intent(in) :: s0, sf, intCode
        integer :: i

        ! New Code
        select case (intCode)

        case(0) ! Make Variable
            if (s0==sf) then
                call addOptimVariable(s0,VAR_CURV)
                !CALL PROCESKDP('UPDATE VARIABLE ; TH, '//trim(int2str(s0))//'; EOS ')
            else
                !call PROCESKDP('UPDATE VARIABLE')
                do i=s0,sf
                    call addOptimVariable(i,VAR_CURV)
                    !CALL PROCESKDP('TH, '//trim(int2str(i)))
                end do
                !call PROCESKDP('EOS')
            end if

        end select


    end subroutine    



end module