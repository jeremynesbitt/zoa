! CODE V expressions in commands: wherever a number is expected, an
! expression may be given instead -- a word that starts with "(" (or a sign
! and "("), through its matching ")" and any arithmetic directly after it:
!
!   THI SO (THI SI-1)        CIR S1 (3.66/2)        THI S5 (THI S5)-1
!   XDE S33 (X R1 F1 SI)     EVA (EFL)              EVA (sqrtf(2)*(EFL))
!
! PROCESKDP replaces each such word by its value before the command runs
! (expand_expressions), so every command takes them.  An expression that
! cannot be evaluated stops its command, with the reason -- never a silent
! zero.
!
! Inside the parentheses: numbers, + - * / ** and parentheses, the CODE V
! functions SQRTF ABSF SINF COSF TANF ASINF ACOSF ATANF ATAN2F EXPF LOGF
! LOG10F INTF NINTF (radians), and database items in parentheses:
!   RDY | CUY | THI | K  Sk           (Sk: Sn, SO, SS, SI, with +-n: SI-1)
!   X Y Z L M N SRL SRM SRN  Rk Fj Sm [Wn]   (reference ray k of field j)
!   any optimizer operand with its qualifiers (EFL, IMD, OAL Si..j, ...)
module expr_eval
  use iso_fortran_env, only: real64
  implicit none
  private
  public :: expand_expressions, eval_expression

  character(len=*), parameter :: FUNCS(14) = [character(len=6) :: &
    'SQRTF', 'ABSF', 'SINF', 'COSF', 'TANF', 'ASINF', 'ACOSF', 'ATANF', &
    'ATAN2F', 'EXPF', 'LOGF', 'LOG10F', 'INTF', 'NINTF']

contains

  ! Replace every expression word in seg by its value.  ok false (with the
  ! error emitted) when one cannot be evaluated.  Quoted strings and a
  ! trailing ! comment are left alone.
  subroutine expand_expressions(seg, out, ok)
    use zoa_output, only: zoa_emit
    character(len=*), intent(in) :: seg
    character(len=:), allocatable, intent(out) :: out
    logical, intent(out) :: ok
    integer :: i, n, j, depth, startTok
    character :: q
    character(len=40) :: num
    real(real64) :: v
    character(len=200) :: msg

    ok = .true.
    n = len_trim(seg)
    ! @NAME == expression: a user-defined constraint (kept as text and
    ! evaluated whenever used, so not expanded now)
    if (defineUserConstraint(seg)) then
      out = ''
      return
    end if
    if (index(seg, '(') == 0) then
      out = seg(1:n)
      return
    end if
    out = ''
    q = ' '
    i = 1
    do while (i <= n)
      if (q /= ' ') then                       ! inside a quoted string
        out = out//seg(i:i)
        if (seg(i:i) == q) q = ' '
        i = i + 1
        cycle
      end if
      if (seg(i:i) == "'" .or. seg(i:i) == '"') then
        q = seg(i:i)
        out = out//seg(i:i)
        i = i + 1
        cycle
      end if
      if (seg(i:i) == '!') then                ! comment: the rest as is
        out = out//seg(i:n)
        exit
      end if
      ! an expression word: at a word start, "(" or a sign then "("
      startTok = i
      if (atWordStart(i)) then
        j = i
        if ((seg(j:j) == '-' .or. seg(j:j) == '+') .and. j < n) j = j + 1
        if (seg(j:j) == '(') then
          depth = 0
          do while (j <= n)
            if (seg(j:j) == '(') depth = depth + 1
            if (seg(j:j) == ')') depth = depth - 1
            if (depth == 0 .and. (isBlank(seg, j) .or. seg(j:j) == ';')) exit
            j = j + 1
          end do
          if (depth /= 0) then
            call zoa_emit("Error:  unbalanced parentheses in '"//trim(seg(startTok:n))//"'", "red")
            ok = .false.
            return
          end if
          call eval_expression(seg(startTok:j-1), v, ok, msg)
          if (.not. ok) then
            call zoa_emit("Error:  cannot evaluate "//seg(startTok:j-1)//": "//trim(msg), "red")
            return
          end if
          write(num, '(ES24.16E3)') v
          out = out//trim(adjustl(num))
          i = j
          cycle
        end if
      end if
      out = out//seg(i:i)
      i = i + 1
    end do
  contains
    ! (no short-circuit .or. in Fortran: seg(0:0) must not be touched)
    logical function atWordStart(k)
      integer, intent(in) :: k
      if (k == 1) then
        atWordStart = .true.
      else
        atWordStart = isBlank(seg, k-1)
      end if
    end function

    logical function isBlank(s, k)
      character(len=*), intent(in) :: s
      integer, intent(in) :: k
      isBlank = s(k:k) == ' ' .or. s(k:k) == achar(9) .or. s(k:k) == ','
    end function
  end subroutine expand_expressions

  ! "@NAME == expression": store it (optim_types) and answer true; any
  ! other text: false.  The expression is checked once now.
  logical function defineUserConstraint(seg) result(isDef)
    use strings, only: uppercase
    use zoa_output, only: zoa_emit
    use optim_types, only: defineUDC, udcEvaluator
    character(len=*), intent(in) :: seg
    character(len=:), allocatable :: t, nm, ex
    integer :: k
    logical :: ok
    real(real64) :: v
    character(len=200) :: msg

    isDef = .false.
    t = trim(adjustl(seg))
    if (len(t) < 4) return
    if (t(1:1) /= '@') return
    k = index(t, '==')
    if (k == 0) return
    isDef = .true.
    nm = uppercase(trim(t(1:k-1)))
    ex = trim(adjustl(t(k+2:)))
    if (len(nm) < 2 .or. index(nm, ' ') > 0 .or. len(nm) > 32) then
      call zoa_emit("Error:  a user-defined constraint is @NAME == expression", "red")
      return
    end if
    call eval_expression(ex, v, ok, msg)
    if (.not. ok) then
      call zoa_emit("Error:  "//nm//": cannot evaluate "//ex//": "//trim(msg), "red")
      return
    end if
    udcEvaluator => evalText
    call defineUDC(nm, ex, ok)
    if (.not. ok) call zoa_emit("Error:  too many user-defined constraints", "red")
  end function

  ! the optimizer's hook: an expression's value
  function evalText(text, ok) result(v)
    character(len=*), intent(in) :: text
    logical, intent(out) :: ok
    real(real64) :: v
    character(len=200) :: msg
    call eval_expression(text, v, ok, msg)
  end function

  ! Evaluate one expression (no surrounding text).
  subroutine eval_expression(text, v, ok, msg)
    use strings, only: uppercase
    character(len=*), intent(in) :: text
    real(real64), intent(out) :: v
    logical, intent(out) :: ok
    character(len=*), intent(out) :: msg
    character(len=:), allocatable :: s
    integer :: p

    s = uppercase(trim(adjustl(text)))
    p = 1
    ok = .true.
    msg = ''
    v = parseExpr(s, p, ok, msg)
    if (ok) then
      call skipBlanks(s, p)
      if (p <= len(s)) then
        ok = .false.
        msg = "unexpected '"//s(p:)//"'"
      end if
    end if
    if (.not. ok) v = 0.0_real64
  end subroutine eval_expression

  ! ---- recursive descent: expr := term {+|- term} ----------------------
  recursive function parseExpr(s, p, ok, msg) result(v)
    character(len=*), intent(in) :: s
    integer, intent(inout) :: p
    logical, intent(inout) :: ok
    character(len=*), intent(inout) :: msg
    real(real64) :: v, t
    character :: op
    v = parseTerm(s, p, ok, msg)
    do while (ok)
      call skipBlanks(s, p)
      if (p > len(s)) exit
      op = s(p:p)
      if (op /= '+' .and. op /= '-') exit
      p = p + 1
      t = parseTerm(s, p, ok, msg)
      if (op == '+') then
        v = v + t
      else
        v = v - t
      end if
    end do
  end function

  ! term := power {*|/ power}
  recursive function parseTerm(s, p, ok, msg) result(v)
    character(len=*), intent(in) :: s
    integer, intent(inout) :: p
    logical, intent(inout) :: ok
    character(len=*), intent(inout) :: msg
    real(real64) :: v, t
    character :: op
    v = parsePower(s, p, ok, msg)
    do while (ok)
      call skipBlanks(s, p)
      if (p > len(s)) exit
      op = s(p:p)
      if (op /= '*' .and. op /= '/') exit
      if (op == '*' .and. p < len(s)) then
        if (s(p+1:p+1) == '*') exit             ! ** is a power
      end if
      p = p + 1
      t = parsePower(s, p, ok, msg)
      if (op == '*') then
        v = v*t
      else if (t == 0.0_real64) then
        ok = .false.; msg = 'division by zero'
      else
        v = v/t
      end if
    end do
  end function

  ! power := unary [** power]
  recursive function parsePower(s, p, ok, msg) result(v)
    character(len=*), intent(in) :: s
    integer, intent(inout) :: p
    logical, intent(inout) :: ok
    character(len=*), intent(inout) :: msg
    real(real64) :: v, e
    v = parseUnary(s, p, ok, msg)
    if (.not. ok) return
    call skipBlanks(s, p)
    if (p + 1 <= len(s)) then
      if (s(p:p+1) == '**') then
        p = p + 2
        e = parsePower(s, p, ok, msg)
        if (ok) v = v**e
      end if
    end if
  end function

  ! unary := (+|-) unary | primary
  recursive function parseUnary(s, p, ok, msg) result(v)
    character(len=*), intent(in) :: s
    integer, intent(inout) :: p
    logical, intent(inout) :: ok
    character(len=*), intent(inout) :: msg
    real(real64) :: v
    call skipBlanks(s, p)
    if (p <= len(s)) then
      if (s(p:p) == '-') then
        p = p + 1
        v = -parseUnary(s, p, ok, msg)
        return
      else if (s(p:p) == '+') then
        p = p + 1
        v = parseUnary(s, p, ok, msg)
        return
      end if
    end if
    v = parsePrimary(s, p, ok, msg)
  end function

  ! primary := number | FUNC ( expr [, expr] ) | ( item ) | ( expr )
  recursive function parsePrimary(s, p, ok, msg) result(v)
    character(len=*), intent(in) :: s
    integer, intent(inout) :: p
    logical, intent(inout) :: ok
    character(len=*), intent(inout) :: msg
    real(real64) :: v, a2
    integer :: q, depth, ios
    character(len=:), allocatable :: word, inner

    v = 0.0_real64
    call skipBlanks(s, p)
    if (p > len(s)) then
      ok = .false.; msg = 'expression ends early'; return
    end if

    ! number
    if (index('0123456789.', s(p:p)) > 0) then
      q = p
      do while (q <= len(s))
        if (index('0123456789.', s(q:q)) > 0) then
          q = q + 1
        else if (s(q:q) == 'E' .or. s(q:q) == 'D') then
          q = q + 1
          if (q <= len(s)) then
            if (s(q:q) == '+' .or. s(q:q) == '-') q = q + 1
          end if
        else
          exit
        end if
      end do
      read(s(p:q-1), *, iostat=ios) v
      if (ios /= 0) then
        ok = .false.; msg = "bad number '"//s(p:q-1)//"'"
      end if
      p = q
      return
    end if

    ! function call
    if (isLetter(s(p:p))) then
      q = p
      do while (q <= len(s))
        if (.not. (isLetter(s(q:q)) .or. index('0123456789_', s(q:q)) > 0)) exit
        q = q + 1
      end do
      word = s(p:q-1)
      if (any(FUNCS == word)) then
        p = q
        call skipBlanks(s, p)
        if (p > len(s) .or. s(p:p) /= '(') then
          ok = .false.; msg = word//' needs (argument)'; return
        end if
        p = p + 1
        v = parseExpr(s, p, ok, msg)
        if (.not. ok) return
        call skipBlanks(s, p)
        a2 = 0.0_real64
        if (word == 'ATAN2F') then
          if (p > len(s) .or. s(p:p) /= ',') then
            ok = .false.; msg = 'ATAN2F needs two arguments'; return
          end if
          p = p + 1
          a2 = parseExpr(s, p, ok, msg)
          if (.not. ok) return
          call skipBlanks(s, p)
        end if
        if (p > len(s) .or. s(p:p) /= ')') then
          ok = .false.; msg = "missing ')' after "//word; return
        end if
        p = p + 1
        v = applyFunc(word, v, a2, ok, msg)
        return
      end if
      ok = .false.
      msg = "'"//word//"' must be in parentheses, as a database item: ("//word//" ...)"
      return
    end if

    ! parenthesised: a database item (starts with a word that is not a
    ! function) or a sub-expression
    if (s(p:p) == '(') then
      depth = 0
      q = p
      do while (q <= len(s))
        if (s(q:q) == '(') depth = depth + 1
        if (s(q:q) == ')') depth = depth - 1
        if (depth == 0) exit
        q = q + 1
      end do
      if (depth /= 0) then
        ok = .false.; msg = 'unbalanced parentheses'; return
      end if
      inner = trim(adjustl(s(p+1:q-1)))
      if (isItem(inner)) then
        v = evalItem(inner, ok, msg)
        p = q + 1
      else
        p = p + 1
        v = parseExpr(s, p, ok, msg)
        if (.not. ok) return
        call skipBlanks(s, p)
        if (p > len(s) .or. s(p:p) /= ')') then
          ok = .false.; msg = "missing ')'"; return
        end if
        p = p + 1
      end if
      return
    end if

    ok = .false.
    msg = "unexpected '"//s(p:p)//"'"
  end function parsePrimary

  ! A database item: starts with a letter word that is not a function name.
  logical function isItem(t)
    character(len=*), intent(in) :: t
    integer :: q
    isItem = .false.
    if (len(t) < 1) return
    if (t(1:1) == '@') then
      isItem = .true.; return
    end if
    if (.not. isLetter(t(1:1))) return
    q = 1
    do while (q <= len(t))
      if (.not. (isLetter(t(q:q)) .or. index('0123456789_', t(q:q)) > 0)) exit
      q = q + 1
    end do
    isItem = .not. any(FUNCS == t(1:q-1))
  end function

  function applyFunc(f, a, b, ok, msg) result(v)
    character(len=*), intent(in) :: f
    real(real64), intent(in) :: a, b
    logical, intent(inout) :: ok
    character(len=*), intent(inout) :: msg
    real(real64) :: v
    v = 0.0_real64
    select case (f)
    case ('SQRTF')
      if (a < 0) then
        ok = .false.; msg = 'SQRTF of a negative number'
      else
        v = sqrt(a)
      end if
    case ('ABSF');   v = abs(a)
    case ('SINF');   v = sin(a)
    case ('COSF');   v = cos(a)
    case ('TANF');   v = tan(a)
    case ('ASINF');  v = asin(max(-1.0_real64, min(1.0_real64, a)))
    case ('ACOSF');  v = acos(max(-1.0_real64, min(1.0_real64, a)))
    case ('ATANF');  v = atan(a)
    case ('ATAN2F'); v = atan2(a, b)
    case ('EXPF');   v = exp(a)
    case ('LOGF', 'LOG10F')
      if (a <= 0) then
        ok = .false.; msg = f//' of a number <= 0'
      else if (f == 'LOGF') then
        v = log(a)
      else
        v = log10(a)
      end if
    case ('INTF');   v = aint(a)
    case ('NINTF');  v = anint(a)
    end select
  end function

  ! ---- database items -------------------------------------------------
  function evalItem(item, ok, msg) result(v)
    use strings, only: parse
    character(len=*), intent(in) :: item
    logical, intent(inout) :: ok
    character(len=*), intent(inout) :: msg
    real(real64) :: v
    character(len=80) :: tk(40)
    integer :: nt

    v = 0.0_real64
    call parse(item, ' ', tk, nt)
    if (tk(1)(1:1) == '@') then                ! a user-defined constraint
      block
        use codeV_commands, only: evalFunc
        logical :: found
        v = evalFunc(item, okOut=found)
        if (.not. found) then
          ok = .false.; msg = trim(tk(1))//" is not defined (@NAME == expression)"
        end if
      end block
      return
    end if
    select case (trim(tk(1)))
    case ('RDY', 'CUY', 'THI', 'K')
      v = surfaceItem(tk, nt, ok, msg)
    case ('X', 'Y', 'Z', 'L', 'M', 'N', 'SRL', 'SRM', 'SRN')
      v = rayItem(tk, nt, ok, msg)
    case default
      block
        use codeV_commands, only: evalFunc
        logical :: found
        v = evalFunc(item, okOut=found)
        if (.not. found) then
          ok = .false.; msg = "unknown item ("//trim(item)//")"
        end if
      end block
    end select
  end function

  function surfaceItem(tk, nt, ok, msg) result(v)
    use mod_surface, only: surf_radius, surf_curvature, surf_thickness, surf_conic
    character(len=*), intent(in) :: tk(:)
    integer, intent(in) :: nt
    logical, intent(inout) :: ok
    character(len=*), intent(inout) :: msg
    real(real64) :: v
    integer :: s
    v = 0.0_real64
    if (nt /= 2) then
      ok = .false.; msg = trim(tk(1))//' needs one surface: ('//trim(tk(1))//' Sk)'; return
    end if
    s = surfaceIndex(tk(2), ok, msg)
    if (.not. ok) return
    select case (trim(tk(1)))
    case ('RDY'); v = surf_radius(s)
    case ('CUY'); v = surf_curvature(s)
    case ('THI'); v = surf_thickness(s)
    case ('K');   v = surf_conic(s)
    end select
  end function

  ! Ray data of reference ray Rk (default R1) of field Fj (default F1) at
  ! surface Sm (default SI), at wavelength Wn (default the reference one).
  function rayItem(tk, nt, ok, msg) result(v)
    use mod_reference_rays, only: refRays, R1_CHIEF, NUM_REF_RAYS
    use global_widgets, only: sysConfig
    character(len=*), intent(in) :: tk(:)
    integer, intent(in) :: nt
    logical, intent(inout) :: ok
    character(len=*), intent(inout) :: msg
    real(real64) :: v
    integer :: k, r, f, s, w, row, ios
    character(len=80) :: t

    v = 0.0_real64
    r = R1_CHIEF; f = 1; w = 0; s = -999
    do k = 2, nt
      t = tk(k)
      select case (t(1:1))
      case ('R')
        read(t(2:), *, iostat=ios) r
        if (ios /= 0 .or. r < 1 .or. r > NUM_REF_RAYS) then
          ok = .false.; msg = "bad ray '"//trim(t)//"' (R1..R5)"; return
        end if
      case ('F')
        if (trim(t) == 'FL') then
          f = sysConfig%numFields
        else
          read(t(2:), *, iostat=ios) f
          if (ios /= 0) f = -1
        end if
        if (f < 1 .or. f > sysConfig%numFields) then
          ok = .false.; msg = "bad field '"//trim(t)//"'"; return
        end if
      case ('W')
        read(t(2:), *, iostat=ios) w
        if (ios /= 0 .or. w < 1 .or. w > sysConfig%numWavelengths) then
          ok = .false.; msg = "bad wavelength '"//trim(t)//"'"; return
        end if
      case ('S')
        s = surfaceIndex(t, ok, msg)
        if (.not. ok) return
      case default
        ok = .false.; msg = "unexpected '"//trim(t)//"'"; return
      end select
    end do
    if (s == -999) s = surfaceIndex('SI', ok, msg)
    select case (trim(tk(1)))
    case ('X');   row = 1
    case ('Y');   row = 2
    case ('Z');   row = 3
    case ('L');   row = 4
    case ('M');   row = 5
    case ('N');   row = 6
    case ('SRL'); row = 13
    case ('SRM'); row = 14
    case ('SRN'); row = 15
    end select

    if (w == 0 .or. w == sysConfig%getRefWavelengthIndex()) then
      ! always retraced: the store is not marked stale by every lens change
      ! (after AUT it held the starting lens), as DIX/DIY do
      call refRays%populate()
      block
        use mod_reference_rays, only: ray_trace_record
        type(ray_trace_record) :: rec
        rec = refRays%getRay(f, r)
        if (.not. rec%traced_ok .or. .not. allocated(rec%data)) then
          ok = .false.; msg = 'the reference ray does not trace'; return
        end if
        if (s < lbound(rec%data, 2) .or. s > ubound(rec%data, 2)) then
          ok = .false.; msg = 'surface out of range'; return
        end if
        v = rec%data(row, s)
      end block
    else
      v = tracedRay(f, r, w, s, row, ok, msg)
    end if
  end function

  ! The reference ray traced at wavelength w (not the stored one).
  function tracedRay(f, r, w, s, row, ok, msg) result(v)
    use global_widgets, only: sysConfig
    use mod_kdp_api, only: kdp_fob, kdp_ray, kdp_silent_begin, kdp_silent_end
    use DATLEN, only: RAYRAY, RAYEXT, NEWOBJ, NEWIMG
    integer, intent(in) :: f, r, w, s, row
    logical, intent(inout) :: ok
    character(len=*), intent(inout) :: msg
    real(real64) :: v, vig(4), py, px
    v = 0.0_real64
    vig = sysConfig%getVignetting(f)
    py = 0; px = 0
    select case (r)
    case (2); py =  (1 - vig(1))
    case (3); py = -(1 - vig(2))
    case (4); px =  (1 - vig(3))
    case (5); px = -(1 - vig(4))
    end select
    call kdp_silent_begin()
    call kdp_fob(sysConfig%relativeFields(2,f), sysConfig%relativeFields(1,f))
    call kdp_ray(py, px, real(w, real64))
    call kdp_silent_end()
    if (.not. RAYEXT) then
      ok = .false.; msg = 'the ray does not trace'; return
    end if
    if (s < NEWOBJ .or. s > NEWIMG) then
      ok = .false.; msg = 'surface out of range'; return
    end if
    v = RAYRAY(row, s)
  end function

  ! Sn, SO, SS, SI, SL, with an optional +-n offset (SI-1, SS+2)
  integer function surfaceIndex(t, ok, msg) result(s)
    use mod_lens_data_manager, only: ldm
    character(len=*), intent(in) :: t
    logical, intent(inout) :: ok
    character(len=*), intent(inout) :: msg
    integer :: k, off, ios, base
    character(len=80) :: u
    s = 0
    u = adjustl(t)
    if (u(1:1) /= 'S' .or. len_trim(u) < 2) then
      ok = .false.; msg = "bad surface '"//trim(t)//"'"; return
    end if
    k = scan(u(2:), '+-')
    off = 0
    if (k > 1) then
      read(u(k+1:), *, iostat=ios) off
      if (ios /= 0) then
        ok = .false.; msg = "bad surface '"//trim(t)//"'"; return
      end if
      u = u(1:k)
    end if
    select case (trim(u(2:)))
    case ('O'); base = 0
    case ('S'); base = ldm%getStopSurf()
    case ('I', 'L'); base = ldm%getLastSurf()
    case default
      read(u(2:), *, iostat=ios) base
      if (ios /= 0) then
        ok = .false.; msg = "bad surface '"//trim(t)//"'"; return
      end if
    end select
    s = base + off
    if (s < 0 .or. s > ldm%getLastSurf()) then
      ok = .false.; msg = "surface '"//trim(t)//"' is not in the lens"
    end if
  end function

  ! ---- helpers ----
  subroutine skipBlanks(s, p)
    character(len=*), intent(in) :: s
    integer, intent(inout) :: p
    do while (p <= len(s))
      if (s(p:p) /= ' ') exit
      p = p + 1
    end do
  end subroutine

  logical function isLetter(c)
    character, intent(in) :: c
    isLetter = (c >= 'A' .and. c <= 'Z') .or. (c >= 'a' .and. c <= 'z')
  end function

end module expr_eval
