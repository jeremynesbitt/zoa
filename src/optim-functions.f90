module optim_functions
    use GLOBALS, only: long
    use zoa_output, only: zoa_emit
    use type_utils
    use zoa_ui
    use global_widgets, only: ioConfig    
   use iso_fortran_env, only: real64
    implicit none

contains

subroutine aut_go()
    use kdp_utils, only: OUTKDP
    use DATLEN, only: PFAC
    use DATMAI
    use optim_types
    use slsqp_module
    use slsqp_kinds

    real(kind=long) :: fmtOld, fmtTst, fmtLow, m, iDamp, fmtDamp
    integer, parameter :: maxIter = 500
    real(kind=long), dimension(maxIter) :: fmtArr
    logical :: autConverge
    integer :: i, endCode
    integer :: meq               !! number of equality constraints

    type(slsqp_solver)    :: solver      !! instantiate an slsqp solver
    integer,parameter               :: max_iter = 25          !! maximum number of allowed iterations
    real(long),dimension(nV) :: xl 
    real(long),dimension(nV) :: xu 
    real(long),dimension(nV,3) :: VARDATA
 
    !real(long),parameter              :: acc = 1.0e-2_long          !! tolerance
    real(long)              :: acc           !! tolerance
    real(long),parameter              :: gradient_delta = 1.0e-4_wp 
    integer,parameter               :: linesearch_mode = 1      !! use inexact linesearch.
    integer, parameter :: gradient_mode = 1
    real(long),dimension(nV) :: x           !! optimization variable vector
    integer               :: istat       !! for solver status check
    logical               :: status_ok   !! for initialization status check
    integer               :: iterations  !! number of iterations by the solver

    !integer, parameter :: nV = 1000
    !real(kind=long), dimension(nV) :: oldVars


    ! No variables -> nothing to optimize.  Bail out with a clear message
    ! instead of handing the solver an empty variable vector (slsqp init
    ! fails and the old code hit an ERROR STOP, killing the whole program).
    if (nV < 1) then
        call zoa_emit("AUT: no variables are defined; nothing to optimize.", "red")
        call zoa_emit("Set at least one variable first (e.g. CCY S2 0, THC S3 0, "// &
        &  "KC S2 0), or use the lens editor's Set Variable menu.", "red")
        return
    end if

    ! Default merit: whenever the user defined no OBJECTIVE term, minimize
    ! spot size.  This also covers constraints-only setups ("AUT; EFL = 50;
    ! GO"): with a constant f = 0 the solver's |df| stopping test fired on
    ! the first iteration and the run ended before the constraints were even
    ! satisfied -- the user expectation is "minimize spot subject to the
    ! constraints".
    print *, "nM is ", nM
    if (numObjectives() == 0) then
        call zoa_emit("AUT: no operand defined; minimizing spot size (SPO)", "black")
        call addMeritEntry('SPO', ID_ROLE_OBJECTIVE, 0.0_long)
    end if


    ! This is the improvement factor during optimization
    acc = optim%imp
    ! Eventually add more vars to this.  For now use this interface to evaluate

    ! Move these to a type?  eg optimizer%getLowerBounds  Some type that does gathering.
    !xl = getLowerBounds()  !! lower bounds
    !x =  VARDATA(1:nV,1)
    VARDATA = optim%gatherVariableData()
    xl = VARDATA(1:nV,2)  !! lower bounds
    xu = VARDATA(1:nV,3)  !! upper bounds
    x =  VARDATA(1:nV,1)
    !x = [0.0_long, 0.00833_long, -0.02899_long] ! initial guess

    ! Expand the general constraints (MXT/MNT/MNE/MNA/MAE) over the variable
    ! thicknesses: center limits become slsqp variable bounds; edge limits
    ! become internal inequality constraints (genConstraints).
    call expandGeneralConstraints(x, xl, xu)

    meq = getNumberofEqualityConstraints()

    print *, "nV is ", nV
    ! NOTE: an earlier version passed toldf=0.05 ("to limit search steps") --
    ! that stopped every run after one iteration once the frozen-typed-store
    ! bug was fixed (per-iteration |df| is naturally small), so it is gone.
    call solver%initialize(nV,numConstraints()+nGen,meq,max_iter,acc,optimizerFunc,dummy_grad,&
                           xl,xu,linesearch_mode=linesearch_mode,status_ok=status_ok,&
                           report=report_iteration,&
                           alphamin=0.1_long, alphamax=0.5_long, &
                           gradient_mode=gradient_mode, gradient_delta=gradient_delta)

    if (status_ok) then
        ! Don't allow commands called during optimization to pollute output log

        call ioConfig%setTextView(ID_TERMINAL_KDPDUMP)
        block
            character(len=:), allocatable :: status_message
            call solver%optimize(x,istat,iterations,status_message)
            ! Apply the RETURNED solution to the lens: the last merit
            ! evaluation inside the solver is typically a gradient-probe or
            ! line-search point, not the solution, so without this the lens
            ! was left at an arbitrary nearby state.
            call updateLensDuringOptimization(x)
            ! Restore the terminal EXPLICITLY: ioConfig's set/restore is a
            ! single-slot save (prev := current), and both report_iteration's
            ! per-cycle redirects and PROCESSILENT leave prev == KDPDUMP, so
            ! restoreTextView() here stranded the terminal on the hidden dump
            ! view (the user needed the TERM backdoor to get output back).
            call ioConfig%setTextView(ID_TERMINAL_DEFAULT)
            write(*,*) ''
            write(*,*) 'solution   :', x
            write(*,*) 'istat      :', istat
            write(*,*) 'iterations :', iterations
            write(*,*) ''
            ! Report the solver's exit status to the user -- "silently
            ! stopped" is indistinguishable from "converged" otherwise.
            call zoa_emit("AUT: done after "//trim(int2str(iterations))// &
            &  " iterations, status: "//trim(status_message), "black")
        end block
    else
        ! Never ERROR STOP from a user command -- that terminates the whole
        ! program.  Report and return; the lens is untouched.
        call zoa_emit("AUT: optimizer initialization failed (slsqp); "// &
        &  "check variables and constraints.", "red")
    end if


end subroutine

! Expand the general-constraint settings over the variable thicknesses.
! For each THC variable at surface k:
!   glass gap: bounds [MNT, MXT] on the variable, plus edge >= MNE
!   air gap:   lower bound MNA, plus edge >= MAE
! Edge constraints get a FIXED evaluation height captured here (max of the two
! surfaces' semi-diameters after a fresh aperture trace).  MNE-beats-MXT rule
! (CODE V): if even at MXT the edge cannot reach MNE (using start-of-run
! sags), raise that variable's upper bound and warn -- approximate when
! curvatures are also variables.  A start value outside its bounds is clamped
! (the constraint takes effect immediately) with a warning.
subroutine expandGeneralConstraints(x, xl, xu)
    use optim_types
    use mod_lens_data_manager, only: ldm
    use global_widgets, only: curr_lens_data
    use kdp_data_types, only: check_clear_apertures
    use type_utils, only: int2str, real2str
    implicit none

    real(long), dimension(:), intent(inout) :: x, xl, xu
    integer :: i, k
    real(long) :: rho, dSag, xuNeeded
    logical :: isGlass

    nGen = 0

    ! GENCON NO: the user turned the general constraints off.
    if (.not. optim%genConOn) return

    ! Nothing to do without a thickness variable -- in particular, skip the
    ! aperture ray trace below (it can spam ray-failure messages on systems
    ! whose optimization has nothing to do with thicknesses).
    if (.not. any(VARS(1:nV,2) == VAR_THI)) return

    ! Fresh typed store + auto apertures for the evaluation heights.
    call ldm%load_surfaces_from_alens()
    call check_clear_apertures(curr_lens_data, ldm%surfaces)

    do i = 1, nV
        if (VARS(i,2) /= VAR_THI) cycle
        k = VARS(i,1)
        if (k < 1 .or. k+1 > ldm%getLastSurf()) cycle
        isGlass = ldm%isGlassSurf(k)

        if (isGlass) then
            xl(i) = optim%mnt
            xu(i) = optim%mxt
        else
            xl(i) = optim%mna
        end if

        ! Edge constraint for this gap.
        rho = max(ldm%getEvalSemiDia(k), ldm%getEvalSemiDia(k+1))
        nGen = nGen + 1
        genConstraints(nGen)%surf  = k
        genConstraints(nGen)%rho   = rho
        if (isGlass) then
            genConstraints(nGen)%name  = 'MNE'
            genConstraints(nGen)%limit = optim%mne
        else
            genConstraints(nGen)%name  = 'MAE'
            genConstraints(nGen)%limit = optim%mae
        end if

        ! MNE-beats-MXT: dSag = edge - center (fixed by the start-of-run
        ! sags at this rho).  If MXT + dSag < MNE the two conflict; relax MXT
        ! for this variable so MNE can be satisfied.
        if (isGlass) then
            dSag = ldm%edge_thickness(k, rho) - ldm%getSurfThi(k)
            xuNeeded = optim%mne - dSag
            if (xu(i) < xuNeeded) then
                call zoa_emit("AUT: MXT relaxed to "//trim(real2str(xuNeeded))// &
                &  " on S"//trim(int2str(k))//" so MNE can be satisfied", "black")
                xu(i) = xuNeeded
            end if
        end if

        ! Clamp the start value into its bounds (constraint applies now).
        if (x(i) < xl(i)) then
            call zoa_emit("AUT: variable thickness S"//trim(int2str(k))// &
            &  " raised to its lower limit "//trim(real2str(xl(i))), "black")
            x(i) = xl(i)
        else if (x(i) > xu(i)) then
            call zoa_emit("AUT: variable thickness S"//trim(int2str(k))// &
            &  " reduced to its upper limit "//trim(real2str(xu(i))), "black")
            x(i) = xu(i)
        end if
    end do

end subroutine

! To not change solver interface, using info from optim_types directly
! It makes this code harder to read, but for now seems better than alternative
subroutine optimizerFunc(me, x,f,c)
    use optim_types
    use slsqp_module
    use mod_lens_data_manager, only: ldm
   use iso_fortran_env, only: real64
    implicit none

    class(slsqp_solver),intent(inout) :: me
    real(long),dimension(:),intent(in)  :: x   !! optimization variable vector
    real(long),intent(out)              :: f   !! value of the objective function
    real(long),dimension(:),intent(out) :: c   !! the constraint vector `dimension(m)`,

    integer :: i, ieq, ineq
    real(long), dimension(nM) :: ceq, cneq
    real(long) :: val

    call updateLensDuringOptimization(x)

    ! Objective: weighted residuals over the objective-role merit entries.
    !     f = sum  weight * (value - target)**2
    ! With the default entry (SPO, targ 0, weight 1) this minimizes SPO**2;
    ! a soft target like "EFL 50 0.5" pulls EFL toward 50 with weight 0.5,
    ! trading against the other objective terms (unlike a hard constraint,
    ! which must hold exactly / one-sidedly at the solution).
    f = 0
    do i=1,nM
        if (meritInUse(i)%role /= ID_ROLE_OBJECTIVE) cycle
        val = meritInUse(i)%func()
        meritInUse(i)%val = val
        f = f + meritInUse(i)%weight * (val - meritInUse(i)%targ)**2
    end do

    ! Constraints: slsqp requires equality constraints first, then
    ! inequalities in the c(x) >= 0 convention.
    ieq  = 0
    ineq = 0
    do i=1,nM
        if (meritInUse(i)%role /= ID_ROLE_CONSTRAINT) cycle
        val = meritInUse(i)%func()
        meritInUse(i)%val = val
        select case (meritInUse(i)%conType)
        case (ID_CON_EXACT)
            ieq = ieq + 1
            ceq(ieq) = val - meritInUse(i)%targ
        case(ID_CON_GREATER_THAN)
            ineq = ineq + 1
            cneq(ineq) = val - meritInUse(i)%targ
        case(ID_CON_LESS_THAN)
            ineq = ineq + 1
            cneq(ineq) = -1*(val - meritInUse(i)%targ)
        end select
    end do

    c(1:ieq) = ceq(1:ieq)
    if (ineq.ne.0) c(ieq+1:ineq+ieq) = cneq(1:ineq)

    ! Internal edge-thickness constraints from the general-constraint
    ! expansion (all inequalities: edge - limit >= 0), appended after the
    ! user constraints.  refresh_typed_surf_geom brings the two surfaces'
    ! cv/thickness/conic current with the just-applied variables (the
    ! same-topology LNSEOS path deliberately does not rebuild the store).
    ! NOTE: asphere COEFFICIENT variables are not re-synced here, so an edge
    ! constraint on a gap whose asphere terms are being varied uses the
    ! start-of-run polynomial -- acceptable v1 approximation.
    block
        integer :: k
        do i = 1, nGen
            k = genConstraints(i)%surf
            call ldm%refresh_typed_surf_geom(k)
            call ldm%refresh_typed_surf_geom(k+1)
            val = ldm%edge_thickness(k, genConstraints(i)%rho)
            genConstraints(i)%val = val
            c(ieq+ineq+i) = val - genConstraints(i)%limit
        end do
    end block


end subroutine

subroutine report_iteration(me,iter,x,f,c)
    use slsqp_module
    use slsqp_kinds
    use kdp_utils, only: OUTKDP
    use optim_types

    !! report an iteration (print to the console).

    use, intrinsic :: iso_fortran_env, only: output_unit

   use iso_fortran_env, only: real64
    implicit none

    class(slsqp_solver),intent(inout) :: me
    integer,intent(in)                :: iter
    real(long),dimension(:),intent(in)  :: x
    real(long),intent(in)               :: f
    real(long),dimension(:),intent(in)  :: c
    character(len=1024) :: output_line
    integer :: i

    !write a header:
    call ioConfig%setTextView(ID_TERMINAL_DEFAULT)
    call OUTKDP("                                                          l")! Blank line 
    call OUTKDP("CYCLE NUMBER "//int2str(iter))
    call OUTKDP("   ERROR FUNCTION = "//real2str(f))
    call PROCESKDP("SUR SA")
    call OUTKDP("                                                          l")! Blank line 
     
    ! Per-cycle merit table: every entry (objective terms AND constraints)
    ! with its role.  Constraint values are reconstructed from the c(:) vector
    ! the solver reports at this iterate, using the eq-first packing order and
    ! the sign convention (fixes the historical row/packing mispairing, which
    ! also mis-signed '<' constraints).  Objective values use the entry's last
    ! evaluation (the scalar f is printed above).
    if (nM > 0) then
        call OUTKDP("Name  Role        Typ  Target              Value               diff")
        block
            integer :: meq_r, jeq, jineq, idx
            character(len=10) :: roleTxt
            character(len=1)  :: typTxt
            real(long) :: val

            meq_r = getNumberofEqualityConstraints()
            jeq   = 0
            jineq = 0
            do i=1,nM
                if (meritInUse(i)%role == ID_ROLE_CONSTRAINT) then
                    roleTxt = 'Constraint'
                    typTxt  = meritInUse(i)%getConstraintTypeAsText()
                    select case (meritInUse(i)%conType)
                    case (ID_CON_EXACT)
                        jeq = jeq + 1
                        idx = jeq
                        val = meritInUse(i)%targ + c(idx)
                    case (ID_CON_GREATER_THAN)
                        jineq = jineq + 1
                        idx = meq_r + jineq
                        val = meritInUse(i)%targ + c(idx)
                    case (ID_CON_LESS_THAN)
                        jineq = jineq + 1
                        idx = meq_r + jineq
                        val = meritInUse(i)%targ - c(idx)
                    end select
                else
                    roleTxt = 'Operand'
                    typTxt  = ' '
                    val = meritInUse(i)%val
                end if
                write(output_line,'(A4,2X,A10,2X,A1,2X,*(F20.16,1X))') &
                &  meritInUse(i)%name, roleTxt, typTxt, meritInUse(i)%targ, &
                &  val, val - meritInUse(i)%targ
                call OUTKDP(trim(output_line))
            end do

            ! Internal edge constraints from the general-constraint expansion
            ! (packed after the user inequalities in c(:)).
            do i = 1, nGen
                idx = meq_r + (numConstraints() - meq_r) + i
                val = genConstraints(i)%limit + c(idx)
                write(output_line,'(A4,2X,A10,2X,A1,2X,*(F20.16,1X))') &
                &  genConstraints(i)%name, 'S'//trim(int2str(genConstraints(i)%surf)), '>', &
                &  genConstraints(i)%limit, val, c(idx)
                call OUTKDP(trim(output_line))
            end do
        end block
    end if
    call ioConfig%restoreTextView()
    ! CALL OUTKDP("Constraint   target    value     diff ")
    ! write(output_line,'(*(A20,1X))') 'EFL', &
    ! 'x(1)', 'x(2)', 'x(3)', &
    ! 'f(1)', 'c(1)', 'c(2)'   


end subroutine report_iteration


subroutine report_iteration_old(me,iter,x,f,c)
    use slsqp_module
    use slsqp_kinds
    use kdp_utils, only: OUTKDP

    !! report an iteration (print to the console).

    use, intrinsic :: iso_fortran_env, only: output_unit

   use iso_fortran_env, only: real64
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


end subroutine report_iteration_old

subroutine dummy_grad(me,x,g,a)
    use slsqp_module
    use slsqp_kinds

    !! compute the gradients.

   use iso_fortran_env, only: real64
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

end subroutine dummy_grad

function testAutConvergence(fmtOld, fmtTst, i, endCode) result(autConverge)
    real(kind=long), intent(in) :: fmtOld, fmtTst
    integer, intent(in) :: i
    integer, intent(inout) :: endCode
    logical :: autConverge
    real(kind=long) :: tstVal, smallChange

    autConverge = .FALSE.
    smallChange = .0000001
    !tstVal = (fmtTst-fmtOld)/fmtOld
    tstVal = (fmtTst-fmtOld)/fmtOld

    ! For now force min 5 iterations.  will fix this with an inner loop after multi-var testing
    if (i <500) return

    !For now only support it being not too different.  Eventually add more options
    if (DABS(tstVal) < smallChange) then
        autConverge = .TRUE.
    end if

end function
end module