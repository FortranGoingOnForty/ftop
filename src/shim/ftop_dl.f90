module ftop_dl
  use, intrinsic :: iso_c_binding, only : &
    c_associated, &
    c_char, &
    c_funptr, &
    c_int, &
    c_null_char, &
    c_null_funptr, &
    c_null_ptr, &
    c_ptr
  implicit none
  private

  integer, parameter, public :: FTOP_DL_OK = 0

  type, public :: ftop_dl_handle
    type(c_ptr) :: handle = c_null_ptr
  end type ftop_dl_handle

  public :: ftop_dlclose
  public :: ftop_dlopen
  public :: ftop_dlsym

  interface
    function c_ftop_dlopen(path, sys_errno) bind(C, name="ftop_dlopen")
      import :: c_char, c_int, c_ptr
      character(kind=c_char), intent(in) :: path(*)
      integer(c_int), intent(out) :: sys_errno
      type(c_ptr) :: c_ftop_dlopen
    end function c_ftop_dlopen

    function c_ftop_dlsym(handle, symbol_name, sys_errno) bind(C, name="ftop_dlsym")
      import :: c_char, c_funptr, c_int, c_ptr
      type(c_ptr), value :: handle
      character(kind=c_char), intent(in) :: symbol_name(*)
      integer(c_int), intent(out) :: sys_errno
      type(c_funptr) :: c_ftop_dlsym
    end function c_ftop_dlsym

    integer(c_int) function c_ftop_dlclose(handle, sys_errno) bind(C, name="ftop_dlclose")
      import :: c_int, c_ptr
      type(c_ptr), value :: handle
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_dlclose
  end interface

contains

  logical function ftop_dlopen(path, handle, error_code) result(success)
    character(len=*), intent(in) :: path
    type(ftop_dl_handle), intent(out) :: handle
    integer, intent(out), optional :: error_code
    character(kind=c_char), allocatable :: c_path(:)
    integer(c_int) :: sys_errno

    call to_c_string(path, c_path)
    handle%handle = c_ftop_dlopen(c_path, sys_errno)
    success = c_associated(handle%handle)
    call assign_error(error_code, sys_errno)
  end function ftop_dlopen

  logical function ftop_dlsym(handle, symbol_name, symbol, error_code) result(success)
    type(ftop_dl_handle), intent(in) :: handle
    character(len=*), intent(in) :: symbol_name
    type(c_funptr), intent(out) :: symbol
    integer, intent(out), optional :: error_code
    character(kind=c_char), allocatable :: c_symbol_name(:)
    integer(c_int) :: sys_errno

    call to_c_string(symbol_name, c_symbol_name)
    symbol = c_ftop_dlsym(handle%handle, c_symbol_name, sys_errno)
    success = c_associated(symbol)
    if (.not. success) symbol = c_null_funptr
    call assign_error(error_code, sys_errno)
  end function ftop_dlsym

  logical function ftop_dlclose(handle, error_code) result(success)
    type(ftop_dl_handle), intent(inout) :: handle
    integer, intent(out), optional :: error_code
    integer(c_int) :: sys_errno

    success = c_ftop_dlclose(handle%handle, sys_errno) == 0_c_int
    if (success) handle%handle = c_null_ptr
    call assign_error(error_code, sys_errno)
  end function ftop_dlclose

  subroutine to_c_string(text, buffer)
    character(len=*), intent(in) :: text
    character(kind=c_char), allocatable, intent(out) :: buffer(:)
    integer :: i
    integer :: text_len

    text_len = len_trim(text)
    allocate(buffer(text_len + 1))
    do i = 1, text_len
      buffer(i) = char(iachar(text(i:i)), kind=c_char)
    end do
    buffer(text_len + 1) = c_null_char
  end subroutine to_c_string

  subroutine assign_error(error_code, sys_errno)
    integer, intent(out), optional :: error_code
    integer(c_int), intent(in) :: sys_errno

    if (present(error_code)) error_code = int(sys_errno)
  end subroutine assign_error

end module ftop_dl
