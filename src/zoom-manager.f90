! zoom-manager.f90
!
! Multi-configuration (zoom) support, modeled on Zemax's Multi-Configuration
! Editor and built in the typed layer (NOT the legacy KDP config database).
!
! A "zoom operand" is a parameter on a surface (e.g. THI on surface 3) holding
! one value per configuration. Config 1 is the base; others store the per-config
! value. The ACTIVE config is materialized into the live lens by re-issuing the
! corresponding CODE V command via PROCESKDP (e.g. "THI S3 7.0"), so everything
! that reads the live lens (plots, analyses, FIR/LIS) follows the active config.
!
! Materialization goes through PROCESKDP (a global entry point that routes CODE V
! verbs, as proven by .zoa restore) rather than calling codeV_commands directly,
! which would create a module cycle (codeV_commands' ZOO/POS handlers call here).
module zoom_manager
  use iso_fortran_env, only: real64
  use type_utils,      only: int2str, real2str
  use zoa_output,      only: zoa_emit
  implicit none
  private
  public :: zoom_set_count, zoom_define_operand, zoom_switch, zoom_reset
  public :: zoom_num_configs, zoom_current_config, zoom_genSaveOutputText
  public :: zoom_list

  ! Operand kinds
  integer, parameter :: ZK_NUMERIC = 1   ! THI/RDY/CUY/K
  integer, parameter :: ZK_GLASS   = 2   ! GLA
  integer, parameter :: ZK_SOLVE   = 3   ! PIM (no surface/value)

  type :: zoom_operand
    character(len=8)  :: param = ''      ! CODE V keyword: THI/RDY/CUY/GLA/K/PIM
    integer           :: surf  = 0       ! 0-based surface (unused for solve kind)
    integer           :: kind  = ZK_NUMERIC
    real(real64),      allocatable :: rvalues(:)   ! numeric per-config values
    character(len=40), allocatable :: svalues(:)   ! glass per-config values
  end type

  type(zoom_operand), allocatable :: operands(:)
  integer :: num_configs    = 1
  integer :: current_config = 1

contains

  integer function zoom_num_configs();    zoom_num_configs    = num_configs;    end function
  integer function zoom_current_config(); zoom_current_config = current_config; end function

  ! Clear all zoom state (a freshly loaded/created lens has a single config).
  subroutine zoom_reset()
    if (allocated(operands)) deallocate(operands)
    num_configs    = 1
    current_config = 1
  end subroutine zoom_reset

  ! Map a CODE V parameter keyword to an operand kind, or -1 if unsupported.
  integer function param_kind(param)
    character(len=*), intent(in) :: param
    select case (trim(param))
    case ('THI','RDY','CUY','K'); param_kind = ZK_NUMERIC
    case ('GLA');                 param_kind = ZK_GLASS
    case ('PIM');                 param_kind = ZK_SOLVE
    case default;                 param_kind = -1
    end select
  end function param_kind

  ! Set the number of configurations. Growing copies each operand's last value
  ! into the new slots; shrinking truncates. current_config is clamped.
  subroutine zoom_set_count(n)
    integer, intent(in) :: n
    integer :: i
    if (n < 1) then
      call zoa_emit("ZOO: number of configurations must be >= 1", "red")
      return
    end if
    if (allocated(operands)) then
      do i = 1, size(operands)
        call resize_operand(operands(i), n)
      end do
    end if
    num_configs = n
    if (current_config > n) current_config = n
    call zoa_emit("Number of zoom configurations set to "//trim(int2str(n)), "black")
  end subroutine zoom_set_count

  subroutine resize_operand(op, n)
    type(zoom_operand), intent(inout) :: op
    integer, intent(in) :: n
    real(real64),      allocatable :: rtmp(:)
    character(len=40), allocatable :: stmp(:)
    integer :: keep, j
    if (op%kind == ZK_NUMERIC) then
      allocate(rtmp(n))
      keep = min(n, size(op%rvalues))
      rtmp(1:keep) = op%rvalues(1:keep)
      do j = keep+1, n; rtmp(j) = op%rvalues(size(op%rvalues)); end do
      call move_alloc(rtmp, op%rvalues)
    else if (op%kind == ZK_GLASS) then
      allocate(stmp(n))
      keep = min(n, size(op%svalues))
      stmp(1:keep) = op%svalues(1:keep)
      do j = keep+1, n; stmp(j) = op%svalues(size(op%svalues)); end do
      call move_alloc(stmp, op%svalues)
    end if
    ! ZK_SOLVE has no per-config arrays
  end subroutine resize_operand

  ! Find an operand by (param, surf); 0 if not present.
  integer function find_operand(param, surf)
    character(len=*), intent(in) :: param
    integer, intent(in) :: surf
    integer :: i
    find_operand = 0
    if (.not. allocated(operands)) return
    do i = 1, size(operands)
      if (trim(operands(i)%param) == trim(param) .and. operands(i)%surf == surf) then
        find_operand = i; return
      end if
    end do
  end function find_operand

  ! Append an empty operand slot and return its index.
  integer function add_operand_slot()
    type(zoom_operand), allocatable :: tmp(:)
    integer :: n
    if (.not. allocated(operands)) then
      allocate(operands(1)); add_operand_slot = 1; return
    end if
    n = size(operands)
    allocate(tmp(n+1))
    tmp(1:n) = operands
    call move_alloc(tmp, operands)
    add_operand_slot = n+1
  end function add_operand_slot

  ! Define (or replace) a zoom operand from parsed command tokens.
  ! valTokens(1:nVals) are the per-config value strings (numeric or glass name);
  ! for the PIM solve kind, nVals may be 0.
  subroutine zoom_define_operand(param, surf, valTokens, nVals)
    use command_utils, only: isInputNumber
    character(len=*), intent(in) :: param
    integer,          intent(in) :: surf
    character(len=*), intent(in) :: valTokens(:)
    integer,          intent(in) :: nVals
    integer :: kind, idx, j
    real(real64) :: rv

    kind = param_kind(param)
    if (kind < 0) then
      call zoa_emit("ZOO: unsupported parameter '"//trim(param)// &
                    "' (use THI, RDY, CUY, GLA, K, or PIM)", "red")
      return
    end if

    if (kind == ZK_SOLVE) then
      ! PIM: a per-config solve, re-applied on every switch. No surface/values.
      idx = find_operand(param, surf)
      if (idx == 0) idx = add_operand_slot()
      operands(idx)%param = param
      operands(idx)%surf  = surf
      operands(idx)%kind  = ZK_SOLVE
      call materialize(current_config)
      return
    end if

    if (nVals /= num_configs) then
      call zoa_emit("ZOO: expected "//trim(int2str(num_configs))// &
                    " values (one per config), got "//trim(int2str(nVals)), "red")
      return
    end if
    if (kind == ZK_NUMERIC) then
      do j = 1, nVals
        if (.not. isInputNumber(trim(valTokens(j)))) then
          call zoa_emit("ZOO: '"//trim(valTokens(j))//"' is not a number", "red")
          return
        end if
      end do
    end if

    idx = find_operand(param, surf)
    if (idx == 0) idx = add_operand_slot()
    operands(idx)%param = param
    operands(idx)%surf  = surf
    operands(idx)%kind  = kind
    if (kind == ZK_NUMERIC) then
      if (allocated(operands(idx)%rvalues)) deallocate(operands(idx)%rvalues)
      allocate(operands(idx)%rvalues(num_configs))
      do j = 1, num_configs
        read(valTokens(j), *) rv
        operands(idx)%rvalues(j) = rv
      end do
    else  ! ZK_GLASS
      if (allocated(operands(idx)%svalues)) deallocate(operands(idx)%svalues)
      allocate(operands(idx)%svalues(num_configs))
      do j = 1, num_configs
        operands(idx)%svalues(j) = trim(valTokens(j))
      end do
    end if

    ! Apply the active config's value so the live lens stays consistent.
    call materialize(current_config)
  end subroutine zoom_define_operand

  ! Switch the active configuration and materialize it into the live lens.
  subroutine zoom_switch(n)
    integer, intent(in) :: n
    if (n < 1 .or. n > num_configs) then
      call zoa_emit("POS: configuration "//trim(int2str(n))// &
                    " out of range (1.."//trim(int2str(num_configs))//")", "red")
      return
    end if
    current_config = n
    call materialize(n)
    call zoa_emit("Active configuration: "//trim(int2str(n))//" of "// &
                  trim(int2str(num_configs)), "black")
  end subroutine zoom_switch

  ! Apply every operand's value for configuration n to the live lens by issuing
  ! the CODE V command via PROCESKDP. Solve operands (PIM) are applied last so
  ! they solve against the freshly materialized geometry.
  subroutine materialize(n)
    integer, intent(in) :: n
    integer :: i
    character(len=4) :: sTok
    if (.not. allocated(operands)) return
    ! value operands first
    do i = 1, size(operands)
      sTok = 'S'//trim(int2str(operands(i)%surf))
      select case (operands(i)%kind)
      case (ZK_NUMERIC)
        call PROCESKDP(trim(operands(i)%param)//' '//trim(sTok)//' '// &
                       trim(real2str(operands(i)%rvalues(n), 10)))
      case (ZK_GLASS)
        call PROCESKDP(trim(operands(i)%param)//' '//trim(sTok)//' '// &
                       trim(operands(i)%svalues(n)))
      end select
    end do
    ! solve operands last
    do i = 1, size(operands)
      if (operands(i)%kind == ZK_SOLVE) call PROCESKDP(trim(operands(i)%param))
    end do
  end subroutine materialize

  ! Print the current zoom table (for the bare ZOO command).
  subroutine zoom_list()
    integer :: i, j
    character(len=200) :: line
    call zoa_emit("Zoom configurations: "//trim(int2str(num_configs))// &
                  "   active: "//trim(int2str(current_config)), "black")
    if (.not. allocated(operands)) return
    do i = 1, size(operands)
      line = '  '//trim(operands(i)%param)
      if (operands(i)%kind /= ZK_SOLVE) line = trim(line)//' S'//trim(int2str(operands(i)%surf))
      select case (operands(i)%kind)
      case (ZK_NUMERIC)
        do j = 1, num_configs; line = trim(line)//'  '//trim(real2str(operands(i)%rvalues(j), 6)); end do
      case (ZK_GLASS)
        do j = 1, num_configs; line = trim(line)//'  '//trim(operands(i)%svalues(j)); end do
      end select
      call zoa_emit(trim(line), "black")
    end do
  end subroutine zoom_list

  ! Emit ZOO/POS commands to reproduce the current zoom state (for SAV/snapshots).
  subroutine zoom_genSaveOutputText(fID)
    integer, intent(in) :: fID
    integer :: i, j
    character(len=400) :: line
    if (num_configs <= 1 .and. .not. allocated(operands)) return
    write(fID, *) 'ZOO '//trim(int2str(num_configs))
    if (allocated(operands)) then
      do i = 1, size(operands)
        line = 'ZOO '//trim(operands(i)%param)
        if (operands(i)%kind /= ZK_SOLVE) line = trim(line)//' S'//trim(int2str(operands(i)%surf))
        select case (operands(i)%kind)
        case (ZK_NUMERIC)
          do j = 1, num_configs; line = trim(line)//' '//trim(real2str(operands(i)%rvalues(j), 10)); end do
        case (ZK_GLASS)
          do j = 1, num_configs; line = trim(line)//' '//trim(operands(i)%svalues(j)); end do
        end select
        write(fID, *) trim(line)
      end do
    end if
    write(fID, *) 'POS '//trim(int2str(current_config))
  end subroutine zoom_genSaveOutputText

end module zoom_manager
