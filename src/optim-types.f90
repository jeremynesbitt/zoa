!TODO:  Get rid of exact/ub/lb for constraints and replace it with conType

module optim_types
    use GLOBALS,only: long
    use zoa_ui

    implicit none
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
       !logical :: exact, lb, ub ! bound if false.  To be depreciated
       integer :: conType ! Either exact, lb, or ub
       real(long) :: targ 
       procedure (constraintFunc), pointer :: func ! share same interface for func
       contains
           procedure :: getConstraintTypeAsText
    end type

    type optimizer
        real(kind=long) :: imp

    contains
        procedure ::  genSaveOutputText
        procedure ::  freezeAllSurfaces
        procedure ::  removeAllConstraints
        procedure ::  gatherVariableData

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

    interface
        module function getSPO(self) result(res)
            class(operand) :: self
            real(long) :: res
        end function
        module function getEFLConstraint(self) result(res)
            class(constraint) :: self
            real(long) :: res
        end function
        module function getTransverseComaConstraint(self) result(res)
            class(constraint) :: self
            real(long) :: res
        end function
        module function getSphericalConstraint(self) result(res)
            class(constraint) :: self
            real(long) :: res
        end function
        module function getTransverseAstigmatismConstraint(self) result(res)
            class(constraint) :: self
            real(long) :: res
        end function
        module function getPetzvalBlurConstraint(self) result(res)
            class(constraint) :: self
            real(long) :: res
        end function
        module function setDistanceToImagePlaneConstraint(self) result(res)
            class(constraint) :: self
            real(long) :: res
        end function
        module function getConstraintTypeAsText(self) result(strType)
            class(constraint) :: self
            character(len=1) :: strType
        end function
    end interface




    type(operand), dimension(100) :: operands
    type(operand), dimension(100) :: operandsInUse
    type(constraint), dimension(100) :: constraints
    type(constraint), dimension(100) :: constraintsInUse
    
    type(optimizer) :: optim


    integer :: nV !Number of variables
    integer :: VARS(1000,2) ! Hard code number of vars for now!  index 1 is surface, index 2 is var type
    !real(long) :: VARDATA(1000,3) ! Values of variables.  initial val, lb ub

    integer :: nO, nC ! number of operands and constraints in use

    integer :: idxConUpdate ! interface with CLI for updating constraintsInUse


    contains

    subroutine initializeOptimizer()

        nV = 0 ! Num variables is 0
        nO = 0 ! Initialize means 0 operands in use
        nC = 0 ! num constraints is 0

        !Initialize for checking later
        constraints(1:size(constraints))%name = ''
        operands(1:size(operands))%name = ''


        operands(1)%name = 'SPO'
        operands(1)%func => getSPO
        constraints(1)%name = 'EFL'
        constraints(1)%func => getEFLConstraint    
        constraints(2)%name = 'TCO'
        constraints(2)%func => getTransverseComaConstraint  
        constraints(3)%name = 'TAS'
        constraints(3)%func => getTransverseAstigmatismConstraint   
        constraints(4)%name = 'PTB'
        constraints(4)%func => getPetzvalBlurConstraint    
        constraints(5)%name = 'IMC'
        constraints(5)%func => setDistanceToImagePlaneConstraint          
        constraints(6)%name = 'SAS'
        constraints(6)%func => getSphericalConstraint                                             
        

    end subroutine

    function getTotalNumberOfOperands() result(nT)
        integer :: nT

        nT = nO + nC

    end function

    ! function getSPO(iW, iF, px,py, hx,hy, density) result(res)
    !     integer, optional :: iW, iF, density
    !     real(long), optional :: px, py, hx, hy        
    !     real(long) :: res

    !     res = 0.1

    ! end function

    subroutine addOptimVariable(surf, int_code)

        implicit none
        integer :: surf, int_code

        ! For better or worse, I changed this to increment the variable counter.
        ! If / when the variables are actually used, they are refound from where
        nV = nV + 1

    end subroutine

    subroutine addOperand(name, targ)
        character(len=*) :: name
        real(long), optional :: targ
        integer :: idx
        
        idx = isNameInOperandList(name)
        if (idx.ne.0) then
            nO = nO +1
            operandsInUse(nO) = operands(idx)
            if(present(targ)) then
                operandsInUse(nO)%targ = targ
            else
                operandsInUse(nO)%targ = 0.0_long
            end if
            ! Continue for all other constraints
        end if

        ! if isNameInList ne 0 then
        !    operandsInUse(n0+1) = operand(i)
        !    add or init other vars


    end subroutine


    subroutine addConstraint(name, val, strType, idxToUpdate)
        character(len=*) :: name
        real(long) :: val
        character(len=1) :: strType ! Either >, < =
        integer, optional :: idxToUpdate ! This is an interface if the user wants to update an existing constraint (or from ui)
        integer :: idx, conType, ii
        
        idx = isNameInConstraintList(name)

        select case (strType)

        case('=')
            conType = ID_CON_EXACT
        case('>')
            conType = ID_CON_GREATER_THAN
        case('<') 
            conType = ID_CON_LESS_THAN
        case default
            call LogTermFOR("Error in addConstraint type!  Only support =, >, < at this type")
            return 
        end select



        if (idx.ne.0) then
            if(present(idxToUpdate)) then 
                if (idxToUpdate > 0 .AND. idxToUpdate <= nC) then
                    ii = idxToUpdate
                else ! Add to end if the update index is not within current constraint list - whether this is a good decision is TBD
                    nC = nC +1
                    ii=nC 
                end if
            else
                nC = nC +1
                ii = nC
            end if
            constraintsInUse(ii) = constraints(idx)
            constraintsInUse(ii)%targ = val
            constraintsInUse(ii)%conType = conType
            !constraintsInUse(nC)%name = name 
        else
            call LogTermFOR("Error in addConstraint name!  Could not find "//name// " as a valid option")
            return             
        end if
                        

    end subroutine

    function isNameInListNew(name, obj) result(idx)
        character(len=*) :: name
        integer :: i
        integer :: idx 
        class(*), dimension(:), target :: obj
   
        type (constraint), dimension(:), pointer :: tmp

        idx = 0
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

    function isNameInOperandList(name) result(idx)
        character(len=*) :: name
        integer :: i
        integer :: idx 

        idx = 0
        do i=1,size(operands)
            if (name == operands(i)%name) then
                ! Found value
                idx = i
                return
            end if
        end do
    end function   
    
    function isNameInConstraintList(name) result(idx)
        character(len=*) :: name
        integer :: i
        integer :: idx 

        idx = 0
        do i=1,size(constraints)
            if (name == constraints(i)%name) then
                ! Found value
                idx = i
                return
            end if
        end do
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
        use mod_lens_data_manager
        real(long), dimension(:) :: x

        integer :: i

        call PROCESSILENT('U L')

        ! Using KDP commands because the new commands get into bad loops during optimization.
        ! obviously should fix this to directly update lens data but a project for another day..
        do i=1,nV
            if (VARS(i,2) >= VAR_A4 .and. VARS(i,2) <= VAR_A20) then
                ! Asphere coefficients live at 1e-4..1e-8: the default F9.5
                ! formatting of real2str would quantize them to zero, so apply
                ! them in scientific notation (D23.10).  CV/TH/CCK keep the
                ! original fixed format so existing optimization trajectories
                ! (and their golden refs) are unchanged.
                call PROCESSILENT('CHG '//int2str(VARS(i,1))//' ; '// &
                &  trim(getVarKdpCmd(VARS(i,2)))//' '//real2str(x(i), sci=.TRUE.))
            else
                call PROCESSILENT('CHG '//int2str(VARS(i,1))//' ; '// &
                &  trim(getVarKdpCmd(VARS(i,2)))//' '//real2str(x(i)))
            end if
        end do

        call PROCESSILENT('EOS')

    end subroutine

    function getLowerBounds() result(lbArr)
        real(long), dimension(nV) :: lbArr
        integer :: i

        do i=1,nV
            lbArr(i) = -.1*huge(0.0_long)
        end do
        
    end function

    function getUpperBounds() result(ubArr)
        implicit none
        real(long), dimension(nV) :: ubArr
        integer :: i

        do i=1,nV
            ubArr(i) = 0.1*huge(0.0_long)
        end do
        
    end function  
    
    function getNumberofEqualityConstraints() result(neq)
        implicit none
        integer :: neq
        integer :: i
        
        neq = 0
        do i=1,nC
            if (constraintsInUse(i)%conType == ID_CON_EXACT) neq = neq+1
        end do

    end function

    subroutine updateOptimVarsNew(varName, s0, sf, intCode)
        use type_utils
        character(len=*) :: varName
        integer, intent(in) :: s0, sf, intCode
        integer :: i, VAR_CODE

        select case (varName)
        case('THC')
            VAR_CODE = VAR_THI
        case('CCY')
            VAR_CODE = VAR_CURV
        case('KC')
            VAR_CODE = VAR_K
        case('AC')
            VAR_CODE = VAR_A4
        case('BC')
            VAR_CODE = VAR_A6
        case('CC')
            VAR_CODE = VAR_A8
        case('DC')
            VAR_CODE = VAR_A10
        case('EC')
            VAR_CODE = VAR_A12
        case('FC')
            VAR_CODE = VAR_A14
        case('GC')
            VAR_CODE = VAR_A16
        case('HC')
            VAR_CODE = VAR_A18
        case('IC')
            VAR_CODE = VAR_A20
        ! NOTE: 'GLC' (glass variable, VAR_GLA) is deliberately NOT mapped here:
        ! the optimizer has no glass-variable support yet, so GLC only updates
        ! the ldm%vars bookkeeping (lens-editor icon / GLC command) via
        ! ldm%updateOptimVars.  Add it here when glass optimization lands.
        case default
            return
        end select

        ! New Code
        select case (intCode)

        case(0) ! Make Variable
            if (s0==sf) then
                call addOptimVariable(s0,VAR_CODE)
            else
                do i=s0,sf
                    call addOptimVariable(i,VAR_CODE)
                end do
            end if

        end select
    end subroutine


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

    ! function gatherInitialValues() result(x)
    !     use mod_lens_data_manager

    !     implicit none
    !     real(long), dimension(nV) :: x
    !     integer :: i

    !     do i=1,nV

    !         ! Store current values
    !         select case(VARS(i,2))
    !         case(VAR_CURV)
    !             VARDATA(i,1) = ldm%getSurfCurv(VARS(i,1))

    !         case(VAR_THI)
    !             VARDATA(i,1) = ldm%getSurfThi(VARS(i,1))
            
    !         end select
    !     end do

    !     x = VARDATA(1:nV,1)

        

    ! end function

    function getVarCmd(int_code) result(outCmd)
        integer :: int_code
        character(len=4) :: outCmd
        ! CODE V variable-code commands for the asphere coefficients A4..A20
        character(len=2), parameter :: asphVarCmds(9) = &
            ['AC','BC','CC','DC','EC','FC','GC','HC','IC']

        outCmd = ''
        select case (int_code)
        case(VAR_CURV)
            outCmd = 'CCY'
        case(VAR_THI)
            outCmd = 'THC'
        case(VAR_K)
            outCmd = 'KC'
        case(VAR_A4:VAR_A20)
            outCmd = asphVarCmds(int_code - VAR_A4 + 1)
        end select

    end function

    ! KDP set-command used to apply a variable's value during optimization.
    function getVarKdpCmd(int_code) result(outCmd)
        integer :: int_code
        character(len=4) :: outCmd
        ! KDP asphere coefficient commands for A4..A20
        character(len=2), parameter :: asphKdpCmds(9) = &
            ['AD','AE','AF','AG','AH','AI','AJ','AK','AL']

        outCmd = ''
        select case (int_code)
        case(VAR_CURV)
            outCmd = 'CV'
        case(VAR_THI)
            outCmd = 'TH'
        case(VAR_K)
            outCmd = 'CCK'
        case(VAR_A4:VAR_A20)
            outCmd = asphKdpCmds(int_code - VAR_A4 + 1)
        end select

    end function

    ! TODO:  Constraints do not properly support > < 
    subroutine genSaveOutputText(self, fID)
        use type_utils
        
        implicit none
        class(optimizer) :: self
        integer :: fID
        integer :: i
        character(len=1) :: q
        real(long),dimension(nV,3) :: VARDATA

        if (nV > 0 .OR. nC > 0 .OR. nO > 0) then
            write(fID, *) "! Merit"
        if (nV > 0) then
                ! No guarantee that var data has been gathered so do this first.  Don't need
                ! VARDATA but I am kinda stuck with it so receive it here

                VARDATA = optim%gatherVariableData()
                ! Once VARS is properly populated, spit it out
                do i=1,nV
                    write(fID,*) trim(getVarCmd(VARS(i,2)))//" S"//trim(int2str(VARS(i,1)))//" 0"
                end do
            end if
            if (nO > 0 .OR. nC > 0) then
                write(fID, *) "TAR"
            if (nO > 0) then
                do i=1,nO 
                  write(fID, *) operandsInUse(i)%name//" "//real2str(operandsInUse(i)%targ)
                end do

            end if
            if (nC > 0 ) then
                do i=1,nC
                    q = constraintsInUse(i)%getConstraintTypeAsText()
                    write(fID,*) trim(constraintsInUse(i)%name)//" "//q//" "//real2str(constraintsInUse(i)%targ)
                end do
            end if
            write(fID, *) "GO"
        end if
        end if


    end subroutine

    subroutine freezeAllSurfaces(self)
        use mod_lens_data_manager
        implicit none
        class(optimizer) :: self
        integer :: i, j

        ! For now assume all variables are tied to surfaces.  Will need to revisit as more variables are supported

        nV = 0

        do i=0,ldm%getLastSurf()
            do j=1,ubound(ldm%vars, dim=2)
                if (ldm%vars(i,j) == 0 ) ldm%vars(i,j) = 100
            end do

        end do

        ! do i = nV,1,-1
        ! end do

    end subroutine freezeAllSurfaces

    subroutine removeAllConstraints(self)
        implicit none
        class(optimizer) :: self

        nC = 0 

    end subroutine removeAllConstraints

    function gatherVariableData(self) result(VARDATA)
        use mod_lens_data_manager
        use mod_surface, only: surf_asphere_coeff
        implicit none
        class(optimizer) :: self
        real(long), dimension(nV,3) :: VARDATA
        integer :: i, j, ctr

        !nV = nV + 1
        !VARS(nV,1) = surf
        !VARS(nV,2) = int_code

        ! I don't like this but for live with it for now
        ctr = 0

        do i=0,ldm%getLastSurf()
            do j=1,ubound(ldm%vars,dim=2)
                if (ldm%vars(i,j) == 0) then
                    ctr = ctr + 1
                    VARS(ctr,1) = i
                    VARS(ctr,2) = j
        ! Store initial value and bounds
                    select case(j)
                    case(VAR_CURV)
                        VARDATA(ctr,1) = ldm%getSurfCurv(i)
                        VARDATA(ctr,2) = -0.1*huge(0.0_long)
                        VARDATA(ctr,3) = 0.1*huge(0.0_long)
                    case(VAR_THI)
                        VARDATA(ctr,1) = ldm%getSurfThi(i)
                        VARDATA(ctr,2) = -0.1*huge(0.0_long)
                        VARDATA(ctr,3) = 0.1*huge(0.0_long)
                    case(VAR_K)
                        VARDATA(ctr,1) = ldm%getConicConstant(i)
                        VARDATA(ctr,2) = -0.1*huge(0.0_long)
                        VARDATA(ctr,3) = 0.1*huge(0.0_long)
                    case(VAR_A4:VAR_A20)
                        ! Asphere coefficient A(2n): order = 4,6,..,20
                        VARDATA(ctr,1) = surf_asphere_coeff(i, 4 + 2*(j - VAR_A4))
                        VARDATA(ctr,2) = -0.1*huge(0.0_long)
                        VARDATA(ctr,3) = 0.1*huge(0.0_long)
                end select
                    
                end if
            end do
        end do

    end function

    function gatherConstraintNames() result(strNameList)
        character(len=4), dimension(:), allocatable :: strNameList
        integer :: ii, n_c 


        do ii=1,size(constraints)
           if (constraints(ii)%name(1:2) == "") then
            n_c = ii
            exit 
           end if
        end do

        allocate(character(len=4) :: strNameList(n_c))

        !allocate(character(len=4), dimension(nC) :: strNameList)
        do ii=1,n_c
           strNameList(ii) = constraints(ii)%name
           !strNameList(ii) = "ABC"
        end do

    end function

    function gatherOperandNames() result(strNameList)
        character(len=4), dimension(:), allocatable :: strNameList
        integer :: ii, n_c 


        do ii=1,size(operands)
           if (operands(ii)%name(1:2) == "") then
            n_c = ii
            exit 
           end if
        end do

        allocate(character(len=4) :: strNameList(n_c))

        !allocate(character(len=4), dimension(nC) :: strNameList)
        do ii=1,n_c
           strNameList(ii) = operands(ii)%name
           !strNameList(ii) = "ABC"
        end do

    end function    

    function gatherConstraintTypeNames() result(strNameList)
        character(len=1), dimension(3) :: strNameList

        strNameList(ID_CON_EXACT) = '='
        strNameList(ID_CON_GREATER_THAN) = '>'
        strNameList(ID_CON_LESS_THAN) = '<'

    end function    

    subroutine deleteConstraint(idx)
        integer :: idx
        type(constraint), dimension(size(constraintsInUse)) :: tmpConstraints
        integer :: ii, jj

        print *, "Delete Constraint called!"
        print *, "idx is ", idx
        if(idx>0 .AND. idx<=nC) then


        do ii=1,idx
            print *, "ii is ", ii
           tmpConstraints(ii) = constraintsInUse(ii)
        end do
        do jj=idx+1,nC
           tmpConstraints(ii+jj) = constraintsInUse(ii+1+jj)
        end do
        constraintsInUse = tmpConstraints
        nC = nC -1

        end if

    end subroutine

end module
