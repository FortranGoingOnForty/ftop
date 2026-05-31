module ftop_platform
  use, intrinsic :: iso_c_binding, only : c_int, c_long_long
  use, intrinsic :: iso_fortran_env, only : int64
  use ftop_platform_types, only : cpu_tick_sample, cpu_usage_percent, platform_backend
  implicit none
  private

  type, extends(platform_backend) :: linux_backend
  contains
    procedure :: get_cpu_count => linux_get_cpu_count
    procedure :: get_cpu_sample => linux_get_cpu_sample
  end type linux_backend

  public :: create_platform
  public :: cpu_tick_sample
  public :: cpu_usage_percent
  public :: platform_backend

  interface
    integer(c_int) function c_ftop_linux_cpu_count(count, sys_errno) bind(C, name="ftop_linux_cpu_count")
      import :: c_int
      integer(c_int), intent(out) :: count
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_cpu_count

    integer(c_int) function c_ftop_linux_cpu_ticks(total_ticks, idle_ticks, sys_errno) &
        bind(C, name="ftop_linux_cpu_ticks")
      import :: c_int, c_long_long
      integer(c_long_long), intent(out) :: total_ticks
      integer(c_long_long), intent(out) :: idle_ticks
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_cpu_ticks
  end interface

contains

  function create_platform() result(backend)
    class(platform_backend), allocatable :: backend

    allocate(linux_backend :: backend)
  end function create_platform

  integer function linux_get_cpu_count(self) result(count)
    class(linux_backend), intent(in) :: self
    integer(c_int) :: c_count
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    associate(unused => self)
    end associate

    rc = c_ftop_linux_cpu_count(c_count, sys_errno)
    if (rc == 0_c_int) then
      count = int(c_count)
    else
      count = 0
    end if
  end function linux_get_cpu_count

  function linux_get_cpu_sample(self) result(sample)
    class(linux_backend), intent(in) :: self
    type(cpu_tick_sample) :: sample
    integer(c_long_long) :: total_ticks
    integer(c_long_long) :: idle_ticks
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    associate(unused => self)
    end associate

    rc = c_ftop_linux_cpu_ticks(total_ticks, idle_ticks, sys_errno)
    if (rc == 0_c_int .and. total_ticks > 0_c_long_long) then
      sample%valid = .true.
      sample%total = int(total_ticks, int64)
      sample%idle = int(max(0_c_long_long, idle_ticks), int64)
    end if
  end function linux_get_cpu_sample

end module ftop_platform
