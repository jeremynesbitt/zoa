! Glass catalogs read from AGF files (the ANSI Glass Format that OpticStudio
! documents and glass makers publish).
!
! Every *.AGF file found is one catalog, named after the file (SCHOTT.AGF is
! catalog SCHOTT).  Folders are searched in this order, the first catalog of
! a name winning:
!   1. LIBGLA/ under each search-path folder (SEARCHPATH / ZOA_SEARCH_PATH;
!      the tests' own folders) -- where users add or override catalogs;
!   2. the program's glass folder (LIBGLA, or the GlassCatalogDir preference).
! The catalog search order (for a glass given without a catalog) is the
! first catalog-order.txt found in those folders -- one catalog name per
! line, '!' comments -- then any other catalogs alphabetically.
!
! The whole set is read on first use and again whenever those folders
! change.  Lookups are case-insensitive.
!
! AGF records used: CC (catalog comment), NM (name, formula, glass code,
! nd, vd, exclude, status), GC (comment), ED, CD (coefficients), TD, OD, LD
! (wavelength range, um), IT (internal transmittance: wavelength, tau,
! thickness mm).  Text files may be ASCII/UTF-8 or UTF-16 (with BOM).
module glass_catalog
  use iso_fortran_env, only: real64, int8
  use iso_c_binding, only: c_ptr, c_char, c_int, c_null_char, c_null_ptr, &
                           c_associated, c_f_pointer
  implicit none
  private

  public :: glass_record, gc_ensure_loaded, gc_reload, gc_num_catalogs, &
            gc_catalog_name, gc_catalog_comment, gc_catalog_path, gc_catalog_size, &
            gc_find_catalog, gc_find_glass, gc_find_glass_any, gc_glass, gc_index, &
            agf_index, agf_formula_name, gc_generation

  integer, parameter :: MAXCD = 10

  type :: glass_record
    character(len=32) :: name = ''
    character(len=16) :: code = '0'        ! NM glass code (MIL#)
    integer :: formula = 0
    real(real64) :: nd = 0.0_real64, vd = 0.0_real64
    integer :: status = 0
    integer :: ncd = 0
    real(real64) :: cd(MAXCD) = 0.0_real64
    real(real64) :: lmin = 0.0_real64, lmax = 1.0e10_real64
    real(real64) :: ed(5) = 0.0_real64
    real(real64) :: td(7) = 0.0_real64
    real(real64) :: od(6) = -1.0_real64
    integer :: nit = 0
    real(real64), allocatable :: it(:,:)   ! (3, nit): wavelength, tau, thickness
    character(len=140) :: comment = ''
  end type

  type :: catalog
    character(len=32) :: name = ''
    character(len=1024) :: path = ''
    character(len=140) :: comment = ''
    integer :: n = 0
    type(glass_record), allocatable :: g(:)
  end type

  type(catalog), allocatable, save :: cats(:)
  integer, save :: ncat = 0
  logical, save :: loaded = .false.
  integer, save :: generation = 0          ! bumped on every (re)load
  character(len=:), allocatable, save :: loadedKey

  interface
    function g_dir_open(path, flags, error) bind(c, name='g_dir_open')
      import :: c_ptr, c_char, c_int
      character(kind=c_char), dimension(*), intent(in) :: path
      integer(c_int), value :: flags
      type(c_ptr), value :: error
      type(c_ptr) :: g_dir_open
    end function
    function g_dir_read_name(dir) bind(c, name='g_dir_read_name')
      import :: c_ptr
      type(c_ptr), value :: dir
      type(c_ptr) :: g_dir_read_name
    end function
    subroutine g_dir_close(dir) bind(c, name='g_dir_close')
      import :: c_ptr
      type(c_ptr), value :: dir
    end subroutine
  end interface

contains

  ! ---------------------------------------------------------------- folders

  ! The folders searched, in order (each ends with a separator).
  subroutine glass_dirs(dirs, n)
    use DATMAI, only: LIBGLA
    use zoa_file_handler, only: searchDirs, nSearchDirs
    character(len=1024), intent(out) :: dirs(:)
    integer, intent(out) :: n
    integer :: i
    n = 0
    do i = 1, nSearchDirs
      if (n >= size(dirs) - 1) exit
      n = n + 1
      dirs(n) = trim(searchDirs(i))//'LIBGLA/'
    end do
    n = n + 1
    dirs(n) = trim(LIBGLA)
  end subroutine

  function current_key() result(key)
    character(len=:), allocatable :: key
    character(len=1024) :: dirs(40)
    integer :: n, i
    call glass_dirs(dirs, n)
    key = ''
    do i = 1, n
      key = key//trim(dirs(i))//'|'
    end do
  end function

  ! Load the catalogs if not loaded, or if the folders changed.
  subroutine gc_ensure_loaded()
    character(len=:), allocatable :: key
    key = current_key()
    if (loaded) then
      if (key == loadedKey) return
    end if
    call load_all(key)
  end subroutine

  ! Force a re-read (catalog files edited while running).
  subroutine gc_reload()
    call load_all(current_key())
  end subroutine

  subroutine load_all(key)
    character(len=*), intent(in) :: key
    character(len=1024) :: dirs(40), files(500), paths(500), order(200)
    character(len=32) :: names(500), nm
    integer :: nd, i, j, k, nf, nord, nfound, idx(500), nfirst, t
    logical :: used(500)

    if (allocated(cats)) deallocate(cats)
    ncat = 0
    call glass_dirs(dirs, nd)
    nfound = 0
    nord = 0
    do i = 1, nd
      if (nord == 0) call read_order(trim(dirs(i))//'catalog-order.txt', order, nord)
      call list_agf(trim(dirs(i)), files, nf)
      do j = 1, nf
        nm = upper(stem(files(j)))
        if (any(names(1:nfound) == nm)) cycle      ! an earlier folder wins
        if (nfound >= size(names)) exit
        nfound = nfound + 1
        names(nfound) = nm
        paths(nfound) = files(j)
      end do
    end do

    ! search order: catalog-order.txt first, then the rest alphabetically
    used = .false.
    k = 0
    do i = 1, nord
      do j = 1, nfound
        if (.not. used(j) .and. names(j) == upper(order(i))) then
          k = k + 1; idx(k) = j; used(j) = .true.
        end if
      end do
    end do
    nfirst = k
    do j = 1, nfound
      if (used(j)) cycle
      k = k + 1; idx(k) = j
    end do
    do i = nfirst + 2, k
      t = idx(i); j = i - 1
      do while (j > nfirst)
        if (names(idx(j)) <= names(t)) exit
        idx(j+1) = idx(j); j = j - 1
      end do
      idx(j+1) = t
    end do

    allocate(cats(nfound))
    do i = 1, nfound
      call read_agf(trim(paths(idx(i))), names(idx(i)), cats(i))
    end do
    ncat = nfound
    loadedKey = key
    loaded = .true.
    generation = generation + 1
  end subroutine

  ! *.AGF files in dir (full paths), sorted by name.
  subroutine list_agf(dir, files, n)
    character(len=*), intent(in) :: dir
    character(len=1024), intent(out) :: files(:)
    integer, intent(out) :: n
    type(c_ptr) :: d, p
    character(len=1024) :: f, t
    integer :: i, j, L

    n = 0
    d = g_dir_open(trim(dir)//c_null_char, 0_c_int, c_null_ptr)
    if (.not. c_associated(d)) return
    do
      p = g_dir_read_name(d)
      if (.not. c_associated(p)) exit
      f = c_string(p)
      L = len_trim(f)
      if (L < 5) cycle
      if (upper(f(L-3:L)) /= '.AGF') cycle
      if (n >= size(files)) exit
      n = n + 1
      files(n) = trim(dir)//trim(f)
    end do
    call g_dir_close(d)
    do i = 2, n
      t = files(i); j = i - 1
      do while (j >= 1)
        if (files(j) <= t) exit
        files(j+1) = files(j); j = j - 1
      end do
      files(j+1) = t
    end do
  end subroutine

  subroutine read_order(path, order, n)
    character(len=*), intent(in) :: path
    character(len=1024), intent(inout) :: order(:)
    integer, intent(inout) :: n
    character(len=:), allocatable :: text
    character(len=1024) :: line
    integer :: pos, ios
    logical :: ok
    call read_text(path, text, ok)
    if (.not. ok) return
    pos = 1
    do while (next_line(text, pos, line))
      line = adjustl(line)
      if (len_trim(line) == 0 .or. line(1:1) == '!') cycle
      if (n >= size(order)) exit
      n = n + 1
      read(line, *, iostat=ios) order(n)
      if (ios /= 0) n = n - 1
    end do
  end subroutine

  ! ---------------------------------------------------------------- AGF

  subroutine read_agf(path, name, cat)
    character(len=*), intent(in) :: path, name
    type(catalog), intent(out) :: cat
    character(len=:), allocatable :: text
    character(len=1024) :: line, rest
    character(len=8) :: key
    type(glass_record), allocatable :: tmp(:)
    real(real64) :: v(12)
    integer :: pos, nv, cur
    logical :: ok

    cat%name = name
    cat%path = path
    allocate(cat%g(64))
    cat%n = 0
    cur = 0
    call read_text(path, text, ok)
    if (.not. ok) return
    pos = 1
    do while (next_line(text, pos, line))
      line = adjustl(line)
      if (len_trim(line) < 2) cycle
      call split_key(line, key, rest)
      select case (key)
      case ('CC')
        cat%comment = rest
      case ('NM')
        if (cat%n >= size(cat%g)) then
          allocate(tmp(2*size(cat%g)))
          tmp(1:cat%n) = cat%g(1:cat%n)
          call move_alloc(tmp, cat%g)
        end if
        cat%n = cat%n + 1
        cur = cat%n
        call parse_nm(rest, cat%g(cur))
      case ('GC')
        if (cur > 0) cat%g(cur)%comment = rest
      case ('CD')
        if (cur > 0) then
          call reals(rest, v, nv)
          cat%g(cur)%ncd = min(nv, MAXCD)
          cat%g(cur)%cd(1:cat%g(cur)%ncd) = v(1:cat%g(cur)%ncd)
        end if
      case ('LD')
        if (cur > 0) then
          call reals(rest, v, nv)
          if (nv >= 2) then
            cat%g(cur)%lmin = v(1); cat%g(cur)%lmax = v(2)
          end if
        end if
      case ('ED')
        if (cur > 0) then
          call reals(rest, v, nv)
          cat%g(cur)%ed(1:min(nv,5)) = v(1:min(nv,5))
        end if
      case ('TD')
        if (cur > 0) then
          call reals(rest, v, nv)
          cat%g(cur)%td(1:min(nv,7)) = v(1:min(nv,7))
        end if
      case ('OD')
        if (cur > 0) then
          call reals(rest, v, nv)
          cat%g(cur)%od(1:min(nv,6)) = v(1:min(nv,6))
        end if
      case ('IT')
        if (cur > 0) then
          call reals(rest, v, nv)
          if (nv >= 2) call add_it(cat%g(cur), v(1), v(2), merge(v(3), 0.0_real64, nv >= 3))
        end if
      end select
    end do
  end subroutine

  subroutine parse_nm(rest, g)
    character(len=*), intent(in) :: rest
    type(glass_record), intent(inout) :: g
    character(len=64) :: tok(8)
    integer :: n, ios
    real(real64) :: x
    call tokens(rest, tok, n)
    if (n >= 1) g%name = tok(1)
    if (n >= 2) then
      read(tok(2), *, iostat=ios) x
      if (ios == 0) g%formula = nint(x)
    end if
    if (n >= 3) g%code = tok(3)
    if (n >= 4) read(tok(4), *, iostat=ios) g%nd
    if (n >= 5) read(tok(5), *, iostat=ios) g%vd
    if (n >= 7) then
      read(tok(7), *, iostat=ios) x
      if (ios == 0) g%status = nint(x)
    end if
  end subroutine

  subroutine add_it(g, lam, tau, thk)
    type(glass_record), intent(inout) :: g
    real(real64), intent(in) :: lam, tau, thk
    real(real64), allocatable :: t(:,:)
    if (.not. allocated(g%it)) allocate(g%it(3, 16))
    if (g%nit >= size(g%it, 2)) then
      allocate(t(3, 2*size(g%it, 2)))
      t(:, 1:g%nit) = g%it(:, 1:g%nit)
      call move_alloc(t, g%it)
    end if
    g%nit = g%nit + 1
    g%it(:, g%nit) = [lam, tau, thk]
  end subroutine

  ! ---------------------------------------------------------------- lookup

  ! Changes whenever the catalogs are (re)loaded, so callers holding copies
  ! of catalog data know to refresh them.
  integer function gc_generation()
    call gc_ensure_loaded()
    gc_generation = generation
  end function

  integer function gc_num_catalogs()
    call gc_ensure_loaded()
    gc_num_catalogs = ncat
  end function

  function gc_catalog_name(ic) result(s)
    integer, intent(in) :: ic
    character(len=32) :: s
    s = ''
    if (ic >= 1 .and. ic <= ncat) s = cats(ic)%name
  end function

  function gc_catalog_comment(ic) result(s)
    integer, intent(in) :: ic
    character(len=140) :: s
    s = ''
    if (ic >= 1 .and. ic <= ncat) s = cats(ic)%comment
  end function

  function gc_catalog_path(ic) result(s)
    integer, intent(in) :: ic
    character(len=1024) :: s
    s = ''
    if (ic >= 1 .and. ic <= ncat) s = cats(ic)%path
  end function

  integer function gc_catalog_size(ic)
    integer, intent(in) :: ic
    gc_catalog_size = 0
    if (ic >= 1 .and. ic <= ncat) gc_catalog_size = cats(ic)%n
  end function

  ! Catalog number of a name, 0 if none.
  integer function gc_find_catalog(name) result(ic)
    character(len=*), intent(in) :: name
    character(len=32) :: u
    call gc_ensure_loaded()
    u = upper(adjustl(name))
    do ic = 1, ncat
      if (cats(ic)%name == u) return
    end do
    ic = 0
  end function

  ! Glass of a name (or, with byCode, also of a glass code) in catalog ic.
  logical function gc_find_glass(ic, name, ig, byCode) result(found)
    integer, intent(in) :: ic
    character(len=*), intent(in) :: name
    integer, intent(out) :: ig
    logical, intent(in), optional :: byCode
    character(len=32) :: u
    logical :: codes
    found = .false.; ig = 0
    call gc_ensure_loaded()
    if (ic < 1 .or. ic > ncat) return
    codes = .false.
    if (present(byCode)) codes = byCode
    u = upper(adjustl(name))
    do ig = 1, cats(ic)%n
      if (upper(cats(ic)%g(ig)%name) == u) then
        found = .true.; return
      end if
      if (codes) then
        if (cats(ic)%g(ig)%code == adjustl(name)) then
          found = .true.; return
        end if
      end if
    end do
    ig = 0
  end function

  ! First catalog in search order holding the glass.
  logical function gc_find_glass_any(name, ic, ig, byCode) result(found)
    character(len=*), intent(in) :: name
    integer, intent(out) :: ic, ig
    logical, intent(in), optional :: byCode
    call gc_ensure_loaded()
    do ic = 1, ncat
      if (gc_find_glass(ic, name, ig, byCode)) then
        found = .true.; return
      end if
    end do
    found = .false.; ic = 0; ig = 0
  end function

  function gc_glass(ic, ig) result(g)
    integer, intent(in) :: ic, ig
    type(glass_record) :: g
    g = cats(ic)%g(ig)
  end function

  ! Index of glass ig of catalog ic at lambda (um); 1 at lambda = 0.
  real(real64) function gc_index(ic, ig, lambda) result(n)
    integer, intent(in) :: ic, ig
    real(real64), intent(in) :: lambda
    n = agf_index(cats(ic)%g(ig)%formula, cats(ic)%g(ig)%cd, lambda)
  end function

  ! ---------------------------------------------------------------- formulas

  ! The AGF dispersion formulas (lambda in um; 1 at lambda = 0 and for an
  ! unknown formula).  c are the CD coefficients in file order.
  real(real64) function agf_index(formula, c, lambda) result(n)
    integer, intent(in) :: formula
    real(real64), intent(in) :: c(MAXCD), lambda
    real(real64) :: L2, n2, Lh

    n = 1.0_real64
    if (lambda == 0.0_real64) return
    L2 = lambda*lambda
    select case (formula)
    ! Schott and Sellmeier 1 keep the legacy calcPN/calcPNSC evaluation
    ! order, so catalogs converted from the old binaries give bit-identical
    ! indices.
    case (1)    ! Schott
      n2 = c(1) + (c(2)*(lambda**2)) + (c(3)*(1.0_real64/(lambda**2))) + &
           (c(4)*(1.0_real64/(lambda**4))) + (c(5)*(1.0_real64/(lambda**6))) + &
           (c(6)*(1.0_real64/(lambda**8)))
    case (2)    ! Sellmeier 1: K1 L1 K2 L2 K3 L3
      n2 = ((c(1)*(lambda**2))/((lambda**2) - c(2))) + &
           ((c(3)*(lambda**2))/((lambda**2) - c(4))) + &
           ((c(5)*(lambda**2))/((lambda**2) - c(6))) + 1.0_real64
    case (3)    ! Herzberger
      Lh = 1.0_real64/(L2 - 0.028_real64)
      n = c(1) + c(2)*Lh + c(3)*Lh*Lh + c(4)*L2 + c(5)*L2**2 + c(6)*L2**3
      return
    case (4)    ! Sellmeier 2: A B1 lambda1 B2 lambda2
      n2 = 1.0_real64 + c(1) + c(2)*L2/(L2 - c(3)**2) + c(4)/(L2 - c(5)**2)
    case (5)    ! Conrady
      n = c(1) + c(2)/lambda + c(3)/lambda**3.5_real64
      return
    case (6)    ! Sellmeier 3: K1 L1 .. K4 L4
      n2 = 1.0_real64 + c(1)*L2/(L2 - c(2)) + c(3)*L2/(L2 - c(4)) + &
           c(5)*L2/(L2 - c(6)) + c(7)*L2/(L2 - c(8))
    case (7)    ! Handbook of Optics 1
      n2 = c(1) + c(2)/(L2 - c(3)) - c(4)*L2
    case (8)    ! Handbook of Optics 2
      n2 = c(1) + c(2)*L2/(L2 - c(3)) - c(4)*L2
    case (9)    ! Sellmeier 4
      n2 = c(1) + c(2)*L2/(L2 - c(3)) + c(4)*L2/(L2 - c(5))
    case (10)   ! Extended
      n2 = c(1) + c(2)*L2 + c(3)/L2 + c(4)/L2**2 + c(5)/L2**3 + c(6)/L2**4 + &
           c(7)/L2**5 + c(8)/L2**6
    case (11)   ! Sellmeier 5: K1 L1 .. K5 L5
      n2 = 1.0_real64 + c(1)*L2/(L2 - c(2)) + c(3)*L2/(L2 - c(4)) + &
           c(5)*L2/(L2 - c(6)) + c(7)*L2/(L2 - c(8)) + c(9)*L2/(L2 - c(10))
    case (12)   ! Extended 2
      n2 = c(1) + c(2)*L2 + c(3)/L2 + c(4)/L2**2 + c(5)/L2**3 + c(6)/L2**4 + &
           c(7)*L2**2 + c(8)*L2**3
    case (13)   ! Extended 3
      n2 = c(1) + c(2)*L2 + c(3)*L2**2 + c(4)/L2 + c(5)/L2**2 + c(6)/L2**3 + &
           c(7)/L2**4 + c(8)/L2**5 + c(9)/L2**6
    case default
      return
    end select
    n = sqrt(n2)
  end function

  function agf_formula_name(formula) result(s)
    integer, intent(in) :: formula
    character(len=20) :: s
    select case (formula)
    case (1);  s = 'Schott'
    case (2);  s = 'Sellmeier 1'
    case (3);  s = 'Herzberger'
    case (4);  s = 'Sellmeier 2'
    case (5);  s = 'Conrady'
    case (6);  s = 'Sellmeier 3'
    case (7);  s = 'Handbook of Optics 1'
    case (8);  s = 'Handbook of Optics 2'
    case (9);  s = 'Sellmeier 4'
    case (10); s = 'Extended'
    case (11); s = 'Sellmeier 5'
    case (12); s = 'Extended 2'
    case (13); s = 'Extended 3'
    case default; s = 'unknown'
    end select
  end function

  ! ---------------------------------------------------------------- text

  ! Whole file as text: UTF-16 (LE/BE, with BOM) is narrowed to one byte
  ! per character (non-ASCII becomes '?'); a UTF-8 BOM is dropped.
  subroutine read_text(path, text, ok)
    character(len=*), intent(in) :: path
    character(len=:), allocatable, intent(out) :: text
    logical, intent(out) :: ok
    integer(int8), allocatable :: b(:)
    integer :: u, sz, ios, i, k, lo, hi, cp
    logical :: ex

    ok = .false.
    inquire(file=path, exist=ex, size=sz)
    if (.not. ex .or. sz < 0) return
    open(newunit=u, file=path, access='stream', form='unformatted', status='old', &
         action='read', iostat=ios)
    if (ios /= 0) return
    allocate(b(max(sz, 1)))
    if (sz > 0) read(u, iostat=ios) b(1:sz)
    close(u)
    if (ios /= 0) return
    ok = .true.
    if (sz >= 2 .and. ((byte(b(1)) == 255 .and. byte(b(2)) == 254) .or. &
                       (byte(b(1)) == 254 .and. byte(b(2)) == 255))) then
      lo = merge(0, 1, byte(b(1)) == 255)     ! LE: low byte first
      hi = 1 - lo
      allocate(character(len=(sz-2)/2) :: text)
      k = 0
      do i = 3, sz - 1, 2
        cp = byte(b(i+lo)) + 256*byte(b(i+hi))
        k = k + 1
        if (cp < 128) then
          text(k:k) = achar(cp)
        else
          text(k:k) = '?'
        end if
      end do
      text = text(1:k)
    else
      i = 1
      if (sz >= 3) then
        if (byte(b(1)) == 239 .and. byte(b(2)) == 187 .and. byte(b(3)) == 191) i = 4
      end if
      allocate(character(len=max(sz-i+1, 0)) :: text)
      do k = i, sz
        text(k-i+1:k-i+1) = achar(byte(b(k)))
      end do
    end if
  contains
    integer function byte(x)
      integer(int8), intent(in) :: x
      byte = iand(int(x), 255)
    end function
  end subroutine

  ! Next line of text from pos (LF or CR LF ends a line).
  logical function next_line(text, pos, line) result(more)
    character(len=*), intent(in) :: text
    integer, intent(inout) :: pos
    character(len=*), intent(out) :: line
    integer :: e
    more = pos <= len(text)
    line = ''
    if (.not. more) return
    e = index(text(pos:), achar(10))
    if (e == 0) then
      e = len(text) + 1
    else
      e = pos + e - 1
    end if
    line = text(pos:e-1)
    pos = e + 1
    e = len_trim(line)
    if (e > 0) then
      if (line(e:e) == achar(13)) line(e:e) = ' '
    end if
    line = translate_tabs(line)
  end function

  function translate_tabs(s) result(t)
    character(len=*), intent(in) :: s
    character(len=len(s)) :: t
    integer :: i
    t = s
    do i = 1, len(t)
      if (t(i:i) == achar(9)) t(i:i) = ' '
    end do
  end function

  subroutine split_key(line, key, rest)
    character(len=*), intent(in) :: line
    character(len=*), intent(out) :: key, rest
    integer :: s
    s = index(line, ' ')
    if (s == 0) then
      key = line; rest = ''
    else
      key = line(1:s-1); rest = adjustl(line(s+1:))
    end if
  end subroutine

  subroutine tokens(s, tok, n)
    character(len=*), intent(in) :: s
    character(len=*), intent(out) :: tok(:)
    integer, intent(out) :: n
    integer :: i, j, L
    n = 0; i = 1; L = len_trim(s)
    do while (i <= L .and. n < size(tok))
      do while (i <= L)
        if (s(i:i) /= ' ') exit
        i = i + 1
      end do
      if (i > L) exit
      j = i
      do while (j <= L)
        if (s(j:j) == ' ') exit
        j = j + 1
      end do
      n = n + 1
      tok(n) = s(i:j-1)
      i = j
    end do
  end subroutine

  ! Numbers on a line (stops at the first token that is not one; '-' alone,
  ! as some files write for "no data", counts as 0).
  subroutine reals(s, v, n)
    character(len=*), intent(in) :: s
    real(real64), intent(out) :: v(:)
    integer, intent(out) :: n
    character(len=64) :: tok(size(v))
    integer :: nt, i, ios
    v = 0.0_real64
    call tokens(s, tok, nt)
    n = 0
    do i = 1, nt
      if (trim(tok(i)) == '-') then
        n = n + 1; cycle
      end if
      read(tok(i), *, iostat=ios) v(i)
      if (ios /= 0) exit
      n = n + 1
    end do
  end subroutine

  function c_string(p) result(s)
    type(c_ptr), intent(in) :: p
    character(len=1024) :: s
    character(kind=c_char), pointer :: a(:)
    integer :: i
    call c_f_pointer(p, a, [1024])
    s = ''
    do i = 1, 1024
      if (a(i) == c_null_char) exit
      s(i:i) = a(i)
    end do
  end function

  function stem(path) result(s)
    character(len=*), intent(in) :: path
    character(len=32) :: s
    integer :: a, b
    a = max(index(path, '/', back=.true.), index(path, '\', back=.true.)) + 1
    b = index(path, '.', back=.true.) - 1
    if (b < a) b = len_trim(path)
    s = path(a:b)
  end function

  function upper(s) result(u)
    character(len=*), intent(in) :: s
    character(len=len(s)) :: u
    integer :: i, c
    u = s
    do i = 1, len(u)
      c = iachar(u(i:i))
      if (c >= 97 .and. c <= 122) u(i:i) = achar(c - 32)
    end do
  end function

end module glass_catalog
