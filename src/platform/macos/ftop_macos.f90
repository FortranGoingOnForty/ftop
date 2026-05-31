module ftop_platform
  use, intrinsic :: iso_c_binding, only : c_int
  use ftop_platform_types, only : platform_backend
  implicit none
  private

  type, extends(platform_backend) :: macos_backend
  contains
    procedure :: get_cpu_count => macos_get_cpu_count
  end type macos_backend

  public :: create_platform
  public :: platform_backend

  interface
    integer(c_int) function c_ftop_macos_cpu_count(count, sys_errno) bind(C, name="ftop_macos_cpu_count")
      import :: c_int
      integer(c_int), intent(out) :: count
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_macos_cpu_count
  end interface

contains

  function create_platform() result(backend)
    class(platform_backend), allocatable :: backend

    allocate(macos_backend :: backend)
  end function create_platform

  integer function macos_get_cpu_count(self) result(count)
    class(macos_backend), intent(in) :: self
    integer(c_int) :: c_count
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    associate(unused => self)
    end associate

    rc = c_ftop_macos_cpu_count(c_count, sys_errno)
    if (rc == 0_c_int) then
      count = int(c_count)
    else
      count = 0
    end if
  end function macos_get_cpu_count

end module ftop_platform
