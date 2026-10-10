module optim_types
    use GLOBALS,only: long
    use zoa_ui

    implicit none

    ! Unified merit entry: ONE type for what used to be separate "operand" and
    ! "constraint" types.  Every evaluator (SPO, EFL, TCO, ...) is role-agnostic;
    ! each USE of one is either an objective term (minimized) or a constraint
    ! (must hold at the solution).  slsqp problem statement:
    !     minimize f(x)  subject to  c_eq(x)=0, c_ineq(x)>=0, xl<=x<=xu
    ! Objective terms feed f; constraint entries feed c.
    type :: merit_entry
        character(len=4) :: name
        integer :: role = ID_ROLE_OBJECTIVE  ! ID_ROLE_OBJECTIVE / ID_ROLE_CONSTRAINT
        integer :: conType = ID_CON_EXACT    ! =, >, < (constraints only)
        real(long) :: targ = 0.0_long
        real(long) :: weight = 1.0_long      ! objective terms only
        real(long) :: val = 0.0_long         ! last computed value (display)
        ! Evaluator inputs (used by field/pupil-sampled evaluators like SPO)
        integer :: iW = 0, iF = 0, density = 0
        real(long) :: px = 0.0_long, py = 0.0_long, hx = 0.0_long, hy = 0.0_long
        procedure (meritFunc), pointer :: func
        contains
            procedure :: getConstraintTypeAsText
    end type

    type optimizer
        real(kind=long) :: imp
        ! General constraints (CODE V-style): global limits applied
        ! automatically to every VARIABLE thickness during AUT (set inside the
        ! loop, e.g. "AUT; MXT 14.0; GO").  Center-thickness limits become
        ! slsqp variable bounds; edge limits become internal inequality
        ! constraints evaluated via the typed surfaces' sag().
        real(kind=long) :: mxt = 12.0_long    ! max element center thickness
        real(kind=long) :: mnt = 2.0_long     ! min element center thickness
        real(kind=long) :: mne = 2.0_long     ! min element edge thickness
        real(kind=long) :: mna = 0.1_long     ! min axial air spacing
        real(kind=long) :: mae = 0.0025_long  ! min air spacing at edge

    contains
        procedure ::  genSaveOutputText
        procedure ::  freezeAllSurfaces
        procedure ::  removeAllConstraints
        procedure ::  gatherVariableData

    end type

    abstract interface
    function meritFunc (self)
        import long
        import merit_entry
        class(merit_entry) :: self
        real(long) :: meritFunc
    end function meritFunc
    end interface

    interface
        module function getSPO(self) result(res)
            class(merit_entry) :: self
            real(long) :: res
        end function
        module function getEFLConstraint(self) result(res)
            class(merit_entry) :: self
            real(long) :: res
        end function
        module function getTransverseComaConstraint(self) result(res)
            class(merit_entry) :: self
            real(long) :: res
        end function
        module function getSphericalConstraint(self) result(res)
            class(merit_entry) :: self
            real(long) :: res
        end function
        module function getPetzvalCurvatureConstraint(self) result(res)
            class(merit_entry) :: self
            real(long) :: res
        end function
        module function getTransverseAstigmatismConstraint(self) result(res)
            class(merit_entry) :: self
            real(long) :: res
        end function
        module function getPetzvalBlurConstraint(self) result(res)
            class(merit_entry) :: self
            real(long) :: res
        end function
        module function setDistanceToImagePlaneConstraint(self) result(res)
            class(merit_entry) :: self
            real(long) :: res
        end function
        module function getConstraintTypeAsText(self) result(strType)
            class(merit_entry) :: self
            character(len=1) :: strType
        end function
    end interface

    ! Internal edge-thickness constraints expanded from the general-constraint
    ! settings at AUT;GO (one per variable thickness: MNE for glass gaps, MAE
    ! for air gaps).  Deliberately NOT part of meritInUse: they are derived,
    ! not user entries, so LCON/AUTUI stay clean; the per-cycle report lists
    ! them separately.  rho is captured once at GO (fixed evaluation height
    ! keeps the constraint smooth for the SQP solver).
    type :: gen_constraint
        character(len=4) :: name = ' '   ! 'MNE' or 'MAE'
        integer :: surf = 0              ! gap between surf and surf+1
        real(long) :: limit = 0.0_long   ! minimum edge value
        real(long) :: rho = 0.0_long     ! evaluation height
        real(long) :: val = 0.0_long     ! last computed edge (report)
    end type

    type(gen_constraint) :: genConstraints(100)
    integer :: nGen = 0

    ! Role-agnostic evaluator registry (templates; role/target set per use).
    ! Order matters for the UI name dropdowns: SPO first, then the six
    ! quantities historically usable as constraints.
    type(merit_entry), dimension(100) :: evaluators
    ! The active merit function: objective terms + constraints, in the order
    ! the user defined them.
    type(merit_entry), dimension(100) :: meritInUse

    type(optimizer) :: optim


    integer :: nV !Number of variables
    integer :: VARS(1000,2) ! Hard code number of vars for now!  index 1 is surface, index 2 is var type

    integer :: nM ! number of merit entries in use (objectives + constraints)

    integer :: idxConUpdate ! interface with CLI for updating merit entries (UPD CON; CHA n)


    contains

    subroutine initializeOptimizer()

        nV = 0 ! Num variables is 0
        nM = 0 ! no merit entries in use

        !Initialize for checking later
        evaluators(1:size(evaluators))%name = ''

        ! Role-agnostic evaluator registry.  Any of these can be used as an
        ! objective term (NAME targ [weight]) or a constraint (NAME = targ).
        ! Keep SPO first, then the historical constraint six, so the UI name
        ! lists preserve their ordering.
        evaluators(1)%name = 'SPO'
        evaluators(1)%func => getSPO
        evaluators(2)%name = 'EFL'
        evaluators(2)%func => getEFLConstraint
        evaluators(3)%name = 'TCO'
        evaluators(3)%func => getTransverseComaConstraint
        evaluators(4)%name = 'TAS'
        evaluators(4)%func => getTransverseAstigmatismConstraint
        evaluators(5)%name = 'PTB'
        evaluators(5)%func => getPetzvalBlurConstraint
        evaluators(6)%name = 'IMC'
        evaluators(6)%func => setDistanceToImagePlaneConstraint
        evaluators(7)%name = 'SAS'
        evaluators(7)%func => getSphericalConstraint
        evaluators(8)%name = 'PTZ'
        evaluators(8)%func => getPetzvalCurvatureConstraint


    end subroutine

    ! True when any general-constraint setting differs from its default
    ! (drives whether the saved merit block needs a TAR section for them).
    function genSettingsModified() result(modified)
        logical :: modified
        modified = (optim%mxt /= 12.0_long) .OR. (optim%mnt /= 2.0_long) .OR. &
        &          (optim%mne /= 2.0_long)  .OR. (optim%mna /= 0.1_long) .OR. &
        &          (optim%mae /= 0.0025_long)
    end function

    ! Counts by role, derived from the merit list (no separate counters to drift).
    function numObjectives() result(n)
        integer :: n, i
        n = 0
        do i=1,nM
            if (meritInUse(i)%role == ID_ROLE_OBJECTIVE) n = n + 1
        end do
    end function

    function numConstraints() result(n)
        integer :: n, i
        n = 0
        do i=1,nM
            if (meritInUse(i)%role == ID_ROLE_CONSTRAINT) n = n + 1
        end do
    end function

    function getTotalNumberOfOperands() result(nT)
        integer :: nT

        nT = nM

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

    ! Add (or update, via idxToUpdate) a merit entry.  role selects objective
    ! term vs constraint; conType/weight apply to the matching role only.
    subroutine addMeritEntry(name, role, targ, conType, weight, idxToUpdate)
        character(len=*) :: name
        integer, intent(in) :: role
        real(long), intent(in) :: targ
        integer, intent(in), optional :: conType
        real(long), intent(in), optional :: weight
        integer, intent(in), optional :: idxToUpdate ! UPD CON; CHA n / UI edit path
        integer :: idx, ii

        idx = isNameInEvaluatorList(name)
        if (idx == 0) then
            call LogTermFOR("Error in addMeritEntry!  Could not find "//name//" as a valid option")
            return
        end if

        if (present(idxToUpdate)) then
            if (idxToUpdate > 0 .AND. idxToUpdate <= nM) then
                ii = idxToUpdate
            else ! Add to end if the update index is not within the current list
                nM = nM + 1
                ii = nM
            end if
        else
            nM = nM + 1
            ii = nM
        end if

        meritInUse(ii) = evaluators(idx)
        meritInUse(ii)%role = role
        meritInUse(ii)%targ = targ
        if (present(conType)) meritInUse(ii)%conType = conType
        if (present(weight))  meritInUse(ii)%weight  = weight

    end subroutine

    ! Back-compat wrapper: add an objective term (the old "operand").
    subroutine addOperand(name, targ)
        character(len=*) :: name
        real(long), optional :: targ
        real(long) :: t

        t = 0.0_long
        if (present(targ)) t = targ
        call addMeritEntry(name, ID_ROLE_OBJECTIVE, t)

    end subroutine

    ! Back-compat wrapper: add/update a constraint from its CLI spelling.
    subroutine addConstraint(name, val, strType, idxToUpdate)
        character(len=*) :: name
        real(long) :: val
        character(len=1) :: strType ! Either >, < =
        integer, optional :: idxToUpdate
        integer :: conType

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

        if (present(idxToUpdate)) then
            call addMeritEntry(name, ID_ROLE_CONSTRAINT, val, conType=conType, idxToUpdate=idxToUpdate)
        else
            call addMeritEntry(name, ID_ROLE_CONSTRAINT, val, conType=conType)
        end if

    end subroutine

    function isNameInEvaluatorList(name) result(idx)
        character(len=*) :: name
        integer :: i
        integer :: idx

        idx = 0
        do i=1,size(evaluators)
            if (name == evaluators(i)%name) then
                ! Found value
                idx = i
                return
            end if
        end do
    end function


    subroutine updateLensDuringOptimization(x)
        use type_utils
        use mod_kdp_api, only: kdp_lens_begin, kdp_chg, kdp_lens_cmd, kdp_lens_end, &
                               kdp_silent_begin, kdp_silent_end
        real(long), dimension(:) :: x

        integer :: i

        ! Suppress trace chatter (TIR/ray-failure spam from the EOS
        ! auto-aperture trace on intermediate geometries) exactly as the old
        ! per-command PROCESSILENT wrappers did.  Single set/restore pair --
        ! nothing inside redirects output itself.
        call kdp_silent_begin()

        ! Variables are applied through mod_kdp_api: the exact real64 value
        ! lands in W1 with no text round-trip.  (The old PROCESSILENT path
        ! formatted through real2str; its default F9.5 quantized at 1e-5 and
        ! the lens never changed -- the frozen-lens bug.)
        call kdp_lens_begin()
        do i=1,nV
            call kdp_chg(VARS(i,1))
            call kdp_lens_cmd(trim(getVarKdpCmd(VARS(i,2))), w1=x(i))
        end do

        ! CRITICAL: refreshAll rebuilds the typed surface store from the
        ! just-updated ALENS before the finalizing EOS traces.  The paraxial
        ! trace (EFL etc.) and the real-ray trace (SPO) refract through
        ! ldm%surfaces geometry, and the same-topology LNSEOS path
        ! deliberately does not rebuild it -- without this refresh every
        ! merit evaluation saw the ORIGINAL lens and slsqp aborted with
        ! "positive directional derivative" (the optimizer never worked for
        ! curvature/thickness variables).
        call kdp_lens_end(refreshAll=.TRUE.)

        call kdp_silent_end()

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
        do i=1,nM
            if (meritInUse(i)%role == ID_ROLE_CONSTRAINT .and. &
            &   meritInUse(i)%conType == ID_CON_EXACT) neq = neq+1
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

    subroutine genSaveOutputText(self, fID)
        use type_utils

        implicit none
        class(optimizer) :: self
        integer :: fID
        integer :: i
        character(len=1) :: q
        real(long),dimension(nV,3) :: VARDATA

        if (nV > 0 .OR. nM > 0 .OR. genSettingsModified()) then
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
            if (nM > 0 .OR. genSettingsModified()) then
                write(fID, *) "TAR"
            ! Objective terms first, then constraints (preserves the historical
            ! file order).  Objective line: NAME targ weight; the loader treats
            ! a bare numeric second token as an objective add (weight optional,
            ! so pre-weight files still load).
            do i=1,nM
                if (meritInUse(i)%role == ID_ROLE_OBJECTIVE) then
                  write(fID, *) meritInUse(i)%name//" "//real2str(meritInUse(i)%targ)// &
                  &  " "//real2str(meritInUse(i)%weight)
                end if
            end do
            do i=1,nM
                if (meritInUse(i)%role == ID_ROLE_CONSTRAINT) then
                    q = meritInUse(i)%getConstraintTypeAsText()
                    write(fID,*) trim(meritInUse(i)%name)//" "//q//" "//real2str(meritInUse(i)%targ)
                end if
            end do
            ! Non-default general-constraint settings (loop commands).
            if (optim%mxt /= 12.0_long)   write(fID,*) "MXT "//real2str(optim%mxt)
            if (optim%mnt /= 2.0_long)    write(fID,*) "MNT "//real2str(optim%mnt)
            if (optim%mne /= 2.0_long)    write(fID,*) "MNE "//real2str(optim%mne)
            if (optim%mna /= 0.1_long)    write(fID,*) "MNA "//real2str(optim%mna)
            if (optim%mae /= 0.0025_long) write(fID,*) "MAE "//real2str(optim%mae)
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

    ! Clears the WHOLE merit list (objective terms AND constraints).  Called by
    ! DCON ALL and therefore by the newlens.zoa lens-replacement reset -- this
    ! also fixes the historical stale-operand leak (nO was only ever reset in
    ! initializeOptimizer, so an old SPO target survived lens loads).
    ! The general-constraint settings also return to their defaults here so a
    ! modified MXT/MNE/... cannot leak across lens loads (a loaded lens's own
    ! values are re-applied by its saved merit block).
    subroutine removeAllConstraints(self)
        implicit none
        class(optimizer) :: self

        nM = 0
        self%mxt = 12.0_long
        self%mnt = 2.0_long
        self%mne = 2.0_long
        self%mna = 0.1_long
        self%mae = 0.0025_long

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

    ! All registered evaluator names (usable as either role).  The trailing
    ! blank entry is preserved for the UI dropdown convention (an empty final
    ! slot), matching the historical gatherConstraintNames behavior.
    function gatherEvaluatorNames() result(strNameList)
        character(len=4), dimension(:), allocatable :: strNameList
        integer :: ii, n_c

        do ii=1,size(evaluators)
           if (evaluators(ii)%name(1:2) == "") then
            n_c = ii
            exit
           end if
        end do

        allocate(character(len=4) :: strNameList(n_c))

        do ii=1,n_c
           strNameList(ii) = evaluators(ii)%name
        end do

    end function

    function gatherConstraintTypeNames() result(strNameList)
        character(len=1), dimension(3) :: strNameList

        strNameList(ID_CON_EXACT) = '='
        strNameList(ID_CON_GREATER_THAN) = '>'
        strNameList(ID_CON_LESS_THAN) = '<'

    end function    

    ! Delete merit entry idx (1-based position in the unified list) and shift
    ! the rest down.  Name kept for the DEL CON command path.
    subroutine deleteConstraint(idx)
        integer :: idx
        integer :: ii

        if(idx>0 .AND. idx<=nM) then
            do ii=idx,nM-1
                meritInUse(ii) = meritInUse(ii+1)
            end do
            nM = nM - 1
        end if

    end subroutine

end module
