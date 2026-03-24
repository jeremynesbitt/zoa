! Minimal Fortran binding to libzmq C API via iso_c_binding
! Only covers what's needed for REQ/REP string messaging
module zmq_binding
  use iso_c_binding
  implicit none

  integer(c_int), parameter :: ZMQ_REQ = 3
  integer(c_int), parameter :: ZMQ_REP = 4
  integer(c_int), parameter :: ZMQ_LINGER = 17

  interface
    type(c_ptr) function zmq_ctx_new() bind(c, name='zmq_ctx_new')
      import :: c_ptr
    end function

    type(c_ptr) function zmq_socket(ctx, typ) bind(c, name='zmq_socket')
      import :: c_ptr, c_int
      type(c_ptr), value :: ctx
      integer(c_int), value :: typ
    end function

    integer(c_int) function zmq_bind(sock, endpoint) bind(c, name='zmq_bind')
      import :: c_ptr, c_int, c_char
      type(c_ptr), value :: sock
      character(kind=c_char), intent(in) :: endpoint(*)
    end function

    integer(c_int) function zmq_connect(sock, endpoint) bind(c, name='zmq_connect')
      import :: c_ptr, c_int, c_char
      type(c_ptr), value :: sock
      character(kind=c_char), intent(in) :: endpoint(*)
    end function

    integer(c_int) function zmq_send_raw(sock, buf, len, flags) bind(c, name='zmq_send')
      import :: c_ptr, c_int, c_size_t, c_char
      type(c_ptr), value :: sock
      character(kind=c_char), intent(in) :: buf(*)
      integer(c_size_t), value :: len
      integer(c_int), value :: flags
    end function

    integer(c_int) function zmq_recv_raw(sock, buf, len, flags) bind(c, name='zmq_recv')
      import :: c_ptr, c_int, c_size_t, c_char
      type(c_ptr), value :: sock
      character(kind=c_char), intent(out) :: buf(*)
      integer(c_size_t), value :: len
      integer(c_int), value :: flags
    end function

    integer(c_int) function zmq_close(sock) bind(c, name='zmq_close')
      import :: c_ptr, c_int
      type(c_ptr), value :: sock
    end function

    integer(c_int) function zmq_ctx_term(ctx) bind(c, name='zmq_ctx_term')
      import :: c_ptr, c_int
      type(c_ptr), value :: ctx
    end function

    integer(c_int) function zmq_setsockopt(sock, option, optval, optvallen) &
        bind(c, name='zmq_setsockopt')
      import :: c_ptr, c_int, c_size_t
      type(c_ptr), value :: sock
      integer(c_int), value :: option
      type(*), intent(in) :: optval
      integer(c_size_t), value :: optvallen
    end function
  end interface

contains

  ! Send a Fortran string over ZMQ (handles null-termination)
  function zmq_send_string(sock, str, flags) result(rc)
    type(c_ptr), intent(in) :: sock
    character(len=*), intent(in) :: str
    integer(c_int), intent(in) :: flags
    integer(c_int) :: rc
    integer :: slen

    slen = len_trim(str)
    rc = zmq_send_raw(sock, trim(str), int(slen, c_size_t), flags)
  end function

  ! Receive a string from ZMQ into a Fortran character variable
  ! Returns the number of bytes received
  function zmq_recv_string(sock, buf, flags) result(nbytes)
    type(c_ptr), intent(in) :: sock
    character(len=*), intent(out) :: buf
    integer(c_int), intent(in) :: flags
    integer(c_int) :: nbytes

    buf = ' '
    nbytes = zmq_recv_raw(sock, buf, int(len(buf), c_size_t), flags)
    ! Null-terminate for safety
    if (nbytes > 0 .and. nbytes < len(buf)) then
      buf(nbytes+1:) = ' '
    end if
  end function

end module zmq_binding
