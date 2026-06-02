! result-builder.f90
! Module-level singleton that accumulates structured analysis results
! and serializes them to a hand-rolled JSON string.
!
! Public API:
!   result_begin(type_str)                      — reset and set result type
!   result_clear()                              — clear without setting type
!   result_set_meta(key, value_str)             — string metadata
!   result_set_scalar(name, value)              — named real scalar
!   result_add_table(name, row_labels, col_labels, data) — 2-D table
!   result_add_grid(name, data, dx)             — 2-D grid (base64-encoded)
!   result_add_message(msg)                     — warning/message string
!   result_to_json()   result(json_string)      — serialize to JSON
!   result_has_data    — logical flag, true after result_begin

module result_builder
    use iso_fortran_env, only: real64
    implicit none
    private

    ! Maximum sizes for static allocation
    integer, parameter :: MAX_META     = 16
    integer, parameter :: MAX_SCALARS  = 32
    integer, parameter :: MAX_TABLES   = 8
    integer, parameter :: MAX_GRIDS    = 4
    integer, parameter :: MAX_MESSAGES = 32
    integer, parameter :: MAX_STR      = 256
    integer, parameter :: MAX_LABELS   = 128

    ! ---- internal derived types ----

    type :: kv_string
        character(len=MAX_STR) :: key   = ''
        character(len=MAX_STR) :: value = ''
    end type

    type :: named_scalar
        character(len=MAX_STR) :: name  = ''
        real(real64)            :: value = 0.0_real64
    end type

    type :: named_table
        character(len=MAX_STR) :: name = ''
        integer :: nrows = 0
        integer :: ncols = 0
        character(len=MAX_STR) :: row_labels(MAX_LABELS)
        character(len=MAX_STR) :: col_labels(MAX_LABELS)
        real(real64), allocatable :: data(:,:)   ! (nrows, ncols)
    end type

    type :: named_grid
        character(len=MAX_STR) :: name = ''
        integer :: nx = 0, ny = 0
        real(real64) :: dx = 0.0_real64
        real(real64), allocatable :: data(:,:)   ! (nx, ny)
    end type

    ! ---- singleton state ----

    logical,           public  :: result_has_data = .false.
    character(len=MAX_STR)     :: rb_type         = ''

    integer :: n_meta     = 0
    integer :: n_scalars  = 0
    integer :: n_tables   = 0
    integer :: n_grids    = 0
    integer :: n_messages = 0

    type(kv_string)    :: meta_store(MAX_META)
    type(named_scalar) :: scalar_store(MAX_SCALARS)
    type(named_table)  :: table_store(MAX_TABLES)
    type(named_grid)   :: grid_store(MAX_GRIDS)
    character(len=MAX_STR) :: message_store(MAX_MESSAGES)

    public :: result_begin, result_clear
    public :: result_set_meta, result_set_scalar
    public :: result_add_table, result_add_grid, result_add_message
    public :: result_to_json

contains

    ! ------------------------------------------------------------------
    ! result_begin — reset state and set result type
    ! ------------------------------------------------------------------
    subroutine result_begin(type_str)
        character(len=*), intent(in) :: type_str
        call result_clear()
        rb_type        = trim(type_str)
        result_has_data = .true.
    end subroutine result_begin

    ! ------------------------------------------------------------------
    ! result_clear — reset all state
    ! ------------------------------------------------------------------
    subroutine result_clear()
        integer :: i
        result_has_data = .false.
        rb_type         = ''
        n_meta     = 0
        n_scalars  = 0
        n_messages = 0
        do i = 1, n_tables
            if (allocated(table_store(i)%data)) deallocate(table_store(i)%data)
        end do
        n_tables = 0
        do i = 1, n_grids
            if (allocated(grid_store(i)%data)) deallocate(grid_store(i)%data)
        end do
        n_grids = 0
    end subroutine result_clear

    ! ------------------------------------------------------------------
    ! result_set_meta
    ! ------------------------------------------------------------------
    subroutine result_set_meta(key, value_str)
        character(len=*), intent(in) :: key, value_str
        if (n_meta >= MAX_META) return
        n_meta = n_meta + 1
        meta_store(n_meta)%key   = trim(key)
        meta_store(n_meta)%value = trim(value_str)
    end subroutine result_set_meta

    ! ------------------------------------------------------------------
    ! result_set_scalar
    ! ------------------------------------------------------------------
    subroutine result_set_scalar(name, value)
        character(len=*), intent(in) :: name
        real(real64),     intent(in) :: value
        if (n_scalars >= MAX_SCALARS) return
        n_scalars = n_scalars + 1
        scalar_store(n_scalars)%name  = trim(name)
        scalar_store(n_scalars)%value = value
    end subroutine result_set_scalar

    ! ------------------------------------------------------------------
    ! result_add_table
    ! row_labels(1:nrows), col_labels(1:ncols), data(nrows,ncols)
    ! ------------------------------------------------------------------
    subroutine result_add_table(name, row_labels, col_labels, data)
        character(len=*),  intent(in) :: name
        character(len=*),  intent(in) :: row_labels(:)
        character(len=*),  intent(in) :: col_labels(:)
        real(real64),      intent(in) :: data(:,:)
        integer :: nr, nc, i, tidx

        if (n_tables >= MAX_TABLES) return
        n_tables = n_tables + 1
        tidx = n_tables

        nr = size(row_labels)
        nc = size(col_labels)
        table_store(tidx)%name  = trim(name)
        table_store(tidx)%nrows = nr
        table_store(tidx)%ncols = nc
        do i = 1, min(nr, MAX_LABELS)
            table_store(tidx)%row_labels(i) = trim(row_labels(i))
        end do
        do i = 1, min(nc, MAX_LABELS)
            table_store(tidx)%col_labels(i) = trim(col_labels(i))
        end do
        if (allocated(table_store(tidx)%data)) deallocate(table_store(tidx)%data)
        allocate(table_store(tidx)%data(nr, nc))
        table_store(tidx)%data = data(1:nr, 1:nc)
    end subroutine result_add_table

    ! ------------------------------------------------------------------
    ! result_add_grid
    ! data(nx, ny) — will be base64-encoded as raw little-endian float64
    ! ------------------------------------------------------------------
    subroutine result_add_grid(name, data, dx)
        character(len=*), intent(in) :: name
        real(real64),     intent(in) :: data(:,:)
        real(real64),     intent(in) :: dx
        integer :: gidx

        if (n_grids >= MAX_GRIDS) return
        n_grids = n_grids + 1
        gidx = n_grids

        grid_store(gidx)%name = trim(name)
        grid_store(gidx)%nx   = size(data, 1)
        grid_store(gidx)%ny   = size(data, 2)
        grid_store(gidx)%dx   = dx
        if (allocated(grid_store(gidx)%data)) deallocate(grid_store(gidx)%data)
        allocate(grid_store(gidx)%data(grid_store(gidx)%nx, grid_store(gidx)%ny))
        grid_store(gidx)%data = data
    end subroutine result_add_grid

    ! ------------------------------------------------------------------
    ! result_add_message
    ! ------------------------------------------------------------------
    subroutine result_add_message(msg)
        character(len=*), intent(in) :: msg
        if (n_messages >= MAX_MESSAGES) return
        n_messages = n_messages + 1
        message_store(n_messages) = trim(msg)
    end subroutine result_add_message

    ! ==================================================================
    ! result_to_json — serialize everything to a JSON string
    ! ==================================================================
    function result_to_json() result(json_string)
        character(len=:), allocatable :: json_string
        character(len=:), allocatable :: buf
        integer :: i, j

        buf = '{"schema":"zoa.result/1","type":' // json_str(trim(rb_type))

        ! --- meta ---
        if (n_meta > 0) then
            buf = buf // ',"meta":{'
            do i = 1, n_meta
                if (i > 1) buf = buf // ','
                buf = buf // json_str(trim(meta_store(i)%key)) // ':' // &
                      json_str(trim(meta_store(i)%value))
            end do
            buf = buf // '}'
        end if

        ! --- scalars ---
        if (n_scalars > 0) then
            buf = buf // ',"scalars":{'
            do i = 1, n_scalars
                if (i > 1) buf = buf // ','
                buf = buf // json_str(trim(scalar_store(i)%name)) // ':' // &
                      json_num(scalar_store(i)%value)
            end do
            buf = buf // '}'
        end if

        ! --- tables ---
        if (n_tables > 0) then
            buf = buf // ',"tables":{'
            do i = 1, n_tables
                if (i > 1) buf = buf // ','
                buf = buf // json_str(trim(table_store(i)%name)) // ':{'
                ! row_labels
                buf = buf // '"row_labels":['
                do j = 1, table_store(i)%nrows
                    if (j > 1) buf = buf // ','
                    buf = buf // json_str(trim(table_store(i)%row_labels(j)))
                end do
                buf = buf // '],'
                ! col_labels
                buf = buf // '"col_labels":['
                do j = 1, table_store(i)%ncols
                    if (j > 1) buf = buf // ','
                    buf = buf // json_str(trim(table_store(i)%col_labels(j)))
                end do
                buf = buf // '],'
                ! data: array-of-arrays, row-major (outer=rows, inner=cols)
                buf = buf // '"data":['
                call append_table_data(buf, table_store(i))
                buf = buf // ']}'
            end do
            buf = buf // '}'
        end if

        ! --- grids ---
        if (n_grids > 0) then
            buf = buf // ',"grids":{'
            do i = 1, n_grids
                if (i > 1) buf = buf // ','
                buf = buf // json_str(trim(grid_store(i)%name)) // ':{'
                buf = buf // '"nx":' // int_to_str(grid_store(i)%nx) // ','
                buf = buf // '"ny":' // int_to_str(grid_store(i)%ny) // ','
                buf = buf // '"dx":' // json_num(grid_store(i)%dx) // ','
                buf = buf // '"encoding":"base64-f64-le",'
                ! Base64 characters are all safe ASCII — no escaping needed.
                ! Bypass json_str (which uses O(n^2) concat) and wrap directly.
                buf = buf // '"data":"' // grid_base64(grid_store(i)) // '"'
                buf = buf // '}'
            end do
            buf = buf // '}'
        end if

        ! --- messages ---
        if (n_messages > 0) then
            buf = buf // ',"messages":['
            do i = 1, n_messages
                if (i > 1) buf = buf // ','
                buf = buf // json_str(trim(message_store(i)))
            end do
            buf = buf // ']'
        end if

        buf = buf // '}'
        json_string = buf
    end function result_to_json

    ! ==================================================================
    ! Private helpers
    ! ==================================================================

    ! JSON-escape a string and wrap in double quotes
    function json_str(s) result(out)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: out
        character(len=:), allocatable :: escaped
        integer :: i
        character(len=1) :: c

        escaped = ''
        do i = 1, len(s)
            c = s(i:i)
            if (c == '\') then
                escaped = escaped // '\\'
            else if (c == '"') then
                escaped = escaped // '\"'
            else if (ichar(c) == 8) then
                escaped = escaped // '\b'
            else if (ichar(c) == 9) then
                escaped = escaped // '\t'
            else if (ichar(c) == 10) then
                escaped = escaped // '\n'
            else if (ichar(c) == 12) then
                escaped = escaped // '\f'
            else if (ichar(c) == 13) then
                escaped = escaped // '\r'
            else
                escaped = escaped // c
            end if
        end do
        out = '"' // escaped // '"'
    end function json_str

    ! Serialize a real(real64) to JSON number string
    ! Uses ES24.16 then strips leading/trailing spaces.
    ! Guards against NaN/Infinity which are illegal in JSON.
    function json_num(v) result(out)
        real(real64), intent(in) :: v
        character(len=:), allocatable :: out
        character(len=40) :: buf

        ! Check for non-finite values
        if (v /= v) then
            ! NaN
            out = 'null'
            return
        end if
        if (v > huge(v)) then
            out = 'null'
            return
        end if
        if (v < -huge(v)) then
            out = 'null'
            return
        end if

        write(buf, '(ES24.16)') v
        out = trim(adjustl(buf))
    end function json_num

    ! Convert integer to string
    function int_to_str(n) result(out)
        integer, intent(in) :: n
        character(len=:), allocatable :: out
        character(len=20) :: buf
        write(buf, '(I0)') n
        out = trim(buf)
    end function int_to_str

    ! Serialize table data as JSON array-of-arrays (rows × cols)
    subroutine append_table_data(buf, tbl)
        character(len=:), allocatable, intent(inout) :: buf
        type(named_table), intent(in) :: tbl
        integer :: r, c

        do r = 1, tbl%nrows
            if (r > 1) buf = buf // ','
            buf = buf // '['
            do c = 1, tbl%ncols
                if (c > 1) buf = buf // ','
                buf = buf // json_num(tbl%data(r, c))
            end do
            buf = buf // ']'
        end do
    end subroutine append_table_data

    ! Base64-encode a grid's raw bytes
    function grid_base64(g) result(b64)
        type(named_grid), intent(in) :: g
        character(len=:), allocatable :: b64
        ! Reinterpret the real(real64) array as bytes
        integer(kind=1), allocatable :: raw(:)
        integer :: nbytes, i, j, k, nx, ny
        real(real64) :: val

        nx = g%nx
        ny = g%ny
        nbytes = nx * ny * 8   ! 8 bytes per real64

        allocate(raw(nbytes))

        ! Fill raw bytes column-major (Fortran default storage order),
        ! which matches MATLAB's reshape convention.
        k = 0
        do j = 1, ny
            do i = 1, nx
                val = g%data(i, j)
                call real64_to_bytes_le(val, raw(k+1:k+8))
                k = k + 8
            end do
        end do

        b64 = base64_encode(raw, nbytes)
        deallocate(raw)
    end function grid_base64

    ! Extract 8 little-endian bytes from a real64
    subroutine real64_to_bytes_le(val, bytes)
        real(real64), intent(in) :: val
        integer(kind=1), intent(out) :: bytes(8)
        ! Use equivalence via a local array
        integer(kind=1) :: tmp(8)
        real(real64) :: v_copy
        ! We need to extract the raw bytes from the IEEE 754 double.
        ! Fortran stores doubles in native endianness (little-endian on x86/ARM).
        ! We use a TRANSFER to get the integer representation.
        integer(kind=8) :: ibits
        ibits = transfer(val, ibits)
        tmp(1) = int(ibits,         kind=1)
        tmp(2) = int(ishft(ibits, -8),  kind=1)
        tmp(3) = int(ishft(ibits, -16), kind=1)
        tmp(4) = int(ishft(ibits, -24), kind=1)
        tmp(5) = int(ishft(ibits, -32), kind=1)
        tmp(6) = int(ishft(ibits, -40), kind=1)
        tmp(7) = int(ishft(ibits, -48), kind=1)
        tmp(8) = int(ishft(ibits, -56), kind=1)
        bytes = tmp
    end subroutine real64_to_bytes_le

    ! Standard Base64 encoder
    function base64_encode(bytes, nbytes) result(b64)
        integer(kind=1), intent(in) :: bytes(:)
        integer, intent(in) :: nbytes
        character(len=:), allocatable :: b64
        character(len=64), parameter :: ALPHA = &
            'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
        integer :: i, b64len, b0, b1, b2, idx
        integer(kind=1) :: bb0, bb1, bb2

        ! Output length: ceil(nbytes/3)*4
        b64len = ((nbytes + 2) / 3) * 4
        if (b64len <= 0 .or. nbytes <= 0) then
            b64 = ''
            return
        end if
        allocate(character(len=b64len) :: b64)
        b64 = repeat(' ', b64len)   ! pre-fill to guarantee length

        i = 1
        idx = 1
        do while (i <= nbytes)
            ! Get three bytes (pad with zero if at end)
            bb0 = bytes(i)
            if (i + 1 <= nbytes) then
                bb1 = bytes(i+1)
            else
                bb1 = 0_1
            end if
            if (i + 2 <= nbytes) then
                bb2 = bytes(i+2)
            else
                bb2 = 0_1
            end if

            ! Convert to unsigned
            b0 = iand(int(bb0, kind=4), 255)
            b1 = iand(int(bb1, kind=4), 255)
            b2 = iand(int(bb2, kind=4), 255)

            ! Encode 4 base64 characters
            b64(idx:idx)     = ALPHA(ishft(b0, -2) + 1 : ishft(b0, -2) + 1)
            b64(idx+1:idx+1) = ALPHA(iand(ishft(b0, 4), 63) + ishft(b1, -4) + 1 : &
                                     iand(ishft(b0, 4), 63) + ishft(b1, -4) + 1)
            if (i + 1 <= nbytes) then
                b64(idx+2:idx+2) = ALPHA(iand(ishft(b1, 2), 63) + ishft(b2, -6) + 1 : &
                                         iand(ishft(b1, 2), 63) + ishft(b2, -6) + 1)
            else
                b64(idx+2:idx+2) = '='
            end if
            if (i + 2 <= nbytes) then
                b64(idx+3:idx+3) = ALPHA(iand(b2, 63) + 1 : iand(b2, 63) + 1)
            else
                b64(idx+3:idx+3) = '='
            end if

            i   = i + 3
            idx = idx + 4
        end do
    end function base64_encode

end module result_builder
