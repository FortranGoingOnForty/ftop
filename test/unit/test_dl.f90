program test_dl
  use, intrinsic :: iso_c_binding, only : c_f_procpointer, c_funptr, c_int
  use ftop_dl, only : ftop_dl_handle, ftop_dlclose, ftop_dlopen, ftop_dlsym
  implicit none

  abstract interface
    integer(c_int) function fixture_value_fn() bind(C)
      import :: c_int
    end function fixture_value_fn
  end interface

  character(len=512) :: fixture_path
  type(ftop_dl_handle) :: handle
  type(c_funptr) :: symbol
  procedure(fixture_value_fn), pointer :: fixture_value
  integer :: error_code

  call get_command_argument(1, fixture_path)
  if (len_trim(fixture_path) == 0) error stop "shared library fixture path is required"

  call require(ftop_dlopen(trim(fixture_path), handle, error_code), "dlopen fixture failed")
  call require(error_code == 0, "dlopen should clear error code")

  call require(ftop_dlsym(handle, "ftop_dl_fixture_value", symbol, error_code), "dlsym fixture symbol failed")
  call require(error_code == 0, "dlsym should clear error code")

  call c_f_procpointer(symbol, fixture_value)
  if (.not. associated(fixture_value)) error stop "dlsym did not produce a callable function pointer"
  if (fixture_value() /= 42_c_int) error stop "fixture function returned an unexpected value"

  call require(.not. ftop_dlsym(handle, "ftop_dl_missing_symbol", symbol, error_code), &
               "dlsym missing symbol should fail")
  if (error_code == 0) error stop "dlsym missing symbol should set an error code"

  call require(ftop_dlclose(handle, error_code), "dlclose fixture failed")
  call require(error_code == 0, "dlclose should clear error code")

contains

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

end program test_dl
