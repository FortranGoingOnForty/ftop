module ftop_terminal_io
  use, intrinsic :: iso_c_binding, only : c_char, c_int
  implicit none
  private

  integer, parameter :: INPUT_BUFFER_SIZE = 4096

  type, public :: terminal_read_result
    logical :: has_data = .false.
    logical :: interrupted = .false.
    logical :: failed = .false.
    integer :: error_code = 0
    character(len=:), allocatable :: bytes
  end type terminal_read_result

  public :: read_terminal_input
  public :: write_terminal_output

  interface
    integer(c_int) function c_ftop_poll_stdin(timeout_ms, sys_errno) bind(C, name="ftop_poll_stdin")
      import :: c_int
      integer(c_int), value :: timeout_ms
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_poll_stdin

    integer(c_int) function c_ftop_read_stdin(buffer, buffer_len, bytes_read, sys_errno) &
        bind(C, name="ftop_read_stdin")
      import :: c_char, c_int
      character(kind=c_char), intent(out) :: buffer(*)
      integer(c_int), value :: buffer_len
      integer(c_int), intent(out) :: bytes_read
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_read_stdin

    integer(c_int) function c_ftop_write_stdout(buffer, buffer_len, sys_errno) &
        bind(C, name="ftop_write_stdout")
      import :: c_char, c_int
      character(kind=c_char), intent(in) :: buffer(*)
      integer(c_int), value :: buffer_len
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_write_stdout
  end interface

contains

  function read_terminal_input(timeout_ms) result(read_result)
    integer, intent(in) :: timeout_ms
    type(terminal_read_result) :: read_result
    character(kind=c_char) :: buffer(INPUT_BUFFER_SIZE)
    integer(c_int) :: rc
    integer(c_int) :: sys_errno
    integer(c_int) :: bytes_read

    rc = c_ftop_poll_stdin(int(max(timeout_ms, 0), c_int), sys_errno)
    select case (rc)
    case (1_c_int)
      rc = c_ftop_read_stdin(buffer, int(size(buffer), c_int), bytes_read, sys_errno)
      call fill_read_result(read_result, rc, bytes_read, sys_errno, buffer)
    case (0_c_int)
      continue
    case (-2_c_int)
      read_result%interrupted = .true.
      read_result%error_code = int(sys_errno)
    case default
      read_result%failed = .true.
      read_result%error_code = int(sys_errno)
    end select
  end function read_terminal_input

  subroutine fill_read_result(read_result, rc, bytes_read, sys_errno, buffer)
    type(terminal_read_result), intent(inout) :: read_result
    integer(c_int), intent(in) :: rc
    integer(c_int), intent(in) :: bytes_read
    integer(c_int), intent(in) :: sys_errno
    character(kind=c_char), intent(in) :: buffer(:)
    integer :: i

    select case (rc)
    case (0_c_int)
      if (bytes_read <= 0_c_int) return
      read_result%has_data = .true.
      allocate(character(len=int(bytes_read)) :: read_result%bytes)
      do i = 1, int(bytes_read)
        read_result%bytes(i:i) = char(iachar(buffer(i)))
      end do
    case (-2_c_int)
      read_result%interrupted = .true.
      read_result%error_code = int(sys_errno)
    case default
      read_result%failed = .true.
      read_result%error_code = int(sys_errno)
    end select
  end subroutine fill_read_result

  subroutine write_terminal_output(text)
    character(len=*), intent(in) :: text
    character(kind=c_char), allocatable :: buffer(:)
    integer(c_int) :: sys_errno
    integer(c_int) :: rc
    integer :: i

    if (len(text) == 0) return
    allocate(buffer(len(text)))
    do i = 1, len(text)
      buffer(i) = char(iachar(text(i:i)), kind=c_char)
    end do
    rc = c_ftop_write_stdout(buffer, int(size(buffer), c_int), sys_errno)
    if (rc /= 0_c_int) then
      if (sys_errno /= 0_c_int) continue
    end if
  end subroutine write_terminal_output

end module ftop_terminal_io
