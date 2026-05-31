module ftop_platform
  use, intrinsic :: iso_c_binding, only : c_char, c_int, c_long, c_long_long, c_null_char, c_size_t
  use, intrinsic :: iso_fortran_env, only : int64
  use ftop_platform_types, only : cpu_tick_sample, cpu_usage_percent, memory_info, platform_backend
  implicit none
  private

  type, extends(platform_backend) :: freebsd_backend
  contains
    procedure :: get_cpu_count => freebsd_get_cpu_count
    procedure :: get_cpu_sample => freebsd_get_cpu_sample
    procedure :: get_memory_info => freebsd_get_memory_info
  end type freebsd_backend

  public :: create_platform
  public :: cpu_tick_sample
  public :: cpu_usage_percent
  public :: freebsd_sysctl_bytes
  public :: freebsd_sysctl_int
  public :: freebsd_sysctl_long
  public :: freebsd_sysctl_string
  public :: memory_info
  public :: platform_backend

  interface
    integer(c_int) function c_ftop_freebsd_cpu_count(count, sys_errno) bind(C, name="ftop_freebsd_cpu_count")
      import :: c_int
      integer(c_int), intent(out) :: count
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_freebsd_cpu_count

    integer(c_int) function c_ftop_freebsd_cpu_ticks(total_ticks, idle_ticks, sys_errno) &
        bind(C, name="ftop_freebsd_cpu_ticks")
      import :: c_int, c_long_long
      integer(c_long_long), intent(out) :: total_ticks
      integer(c_long_long), intent(out) :: idle_ticks
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_freebsd_cpu_ticks

    integer(c_int) function c_ftop_freebsd_memory_info(total_bytes, available_bytes, sys_errno) &
        bind(C, name="ftop_freebsd_memory_info")
      import :: c_int, c_long_long
      integer(c_long_long), intent(out) :: total_bytes
      integer(c_long_long), intent(out) :: available_bytes
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_freebsd_memory_info

    integer(c_int) function c_ftop_freebsd_sysctl_int(name, value, sys_errno) &
        bind(C, name="ftop_freebsd_sysctl_int")
      import :: c_char, c_int
      character(kind=c_char), intent(in) :: name(*)
      integer(c_int), intent(out) :: value
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_freebsd_sysctl_int

    integer(c_int) function c_ftop_freebsd_sysctl_long(name, value, sys_errno) &
        bind(C, name="ftop_freebsd_sysctl_long")
      import :: c_char, c_int, c_long
      character(kind=c_char), intent(in) :: name(*)
      integer(c_long), intent(out) :: value
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_freebsd_sysctl_long

    integer(c_int) function c_ftop_freebsd_sysctl_string(name, value, value_capacity, value_len, sys_errno) &
        bind(C, name="ftop_freebsd_sysctl_string")
      import :: c_char, c_int, c_size_t
      character(kind=c_char), intent(in) :: name(*)
      character(kind=c_char), intent(out) :: value(*)
      integer(c_size_t), value :: value_capacity
      integer(c_size_t), intent(out) :: value_len
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_freebsd_sysctl_string

    integer(c_int) function c_ftop_freebsd_sysctl_bytes(name, value, value_capacity, value_len, sys_errno) &
        bind(C, name="ftop_freebsd_sysctl_bytes")
      import :: c_char, c_int, c_size_t
      character(kind=c_char), intent(in) :: name(*)
      character(kind=c_char), intent(out) :: value(*)
      integer(c_size_t), value :: value_capacity
      integer(c_size_t), intent(out) :: value_len
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_freebsd_sysctl_bytes
  end interface

contains

  function create_platform() result(backend)
    class(platform_backend), allocatable :: backend

    allocate(freebsd_backend :: backend)
  end function create_platform

  integer function freebsd_get_cpu_count(self) result(count)
    class(freebsd_backend), intent(in) :: self
    integer(c_int) :: c_count
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    associate(unused => self)
    end associate

    rc = c_ftop_freebsd_cpu_count(c_count, sys_errno)
    if (rc == 0_c_int) then
      count = int(c_count)
    else
      count = 0
    end if
  end function freebsd_get_cpu_count

  function freebsd_get_cpu_sample(self) result(sample)
    class(freebsd_backend), intent(in) :: self
    type(cpu_tick_sample) :: sample
    integer(c_long_long) :: total_ticks
    integer(c_long_long) :: idle_ticks
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    associate(unused => self)
    end associate

    rc = c_ftop_freebsd_cpu_ticks(total_ticks, idle_ticks, sys_errno)
    if (rc == 0_c_int .and. total_ticks > 0_c_long_long) then
      sample%valid = .true.
      sample%total = int(total_ticks, int64)
      sample%idle = int(max(0_c_long_long, idle_ticks), int64)
    end if
  end function freebsd_get_cpu_sample

  function freebsd_get_memory_info(self) result(info)
    class(freebsd_backend), intent(in) :: self
    type(memory_info) :: info
    integer(c_long_long) :: total_bytes
    integer(c_long_long) :: available_bytes
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    associate(unused => self)
    end associate

    rc = c_ftop_freebsd_memory_info(total_bytes, available_bytes, sys_errno)
    if (rc == 0_c_int .and. total_bytes > 0_c_long_long) then
      info%valid = .true.
      info%total_bytes = int(total_bytes, int64)
      info%available_bytes = int(max(0_c_long_long, min(total_bytes, available_bytes)), int64)
    end if
  end function freebsd_get_memory_info

  logical function freebsd_sysctl_int(name, value, error_code) result(success)
    character(len=*), intent(in) :: name
    integer, intent(out) :: value
    integer, intent(out), optional :: error_code
    character(kind=c_char), allocatable :: c_name(:)
    integer(c_int) :: c_value
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    call to_c_string(name, c_name)
    rc = c_ftop_freebsd_sysctl_int(c_name, c_value, sys_errno)
    success = rc == 0_c_int
    if (success) then
      value = int(c_value)
    else
      value = 0
    end if
    call assign_error(error_code, sys_errno)
  end function freebsd_sysctl_int

  logical function freebsd_sysctl_long(name, value, error_code) result(success)
    character(len=*), intent(in) :: name
    integer(c_long), intent(out) :: value
    integer, intent(out), optional :: error_code
    character(kind=c_char), allocatable :: c_name(:)
    integer(c_long) :: c_value
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    call to_c_string(name, c_name)
    rc = c_ftop_freebsd_sysctl_long(c_name, c_value, sys_errno)
    success = rc == 0_c_int
    if (success) then
      value = c_value
    else
      value = 0_c_long
    end if
    call assign_error(error_code, sys_errno)
  end function freebsd_sysctl_long

  logical function freebsd_sysctl_string(name, value, value_len, error_code) result(success)
    character(len=*), intent(in) :: name
    character(len=*), intent(out) :: value
    integer, intent(out), optional :: value_len
    integer, intent(out), optional :: error_code
    character(kind=c_char), allocatable :: c_name(:)
    character(kind=c_char), allocatable :: c_value(:)
    integer(c_size_t) :: c_value_len
    integer(c_int) :: sys_errno
    integer(c_int) :: rc
    integer :: copied_len
    integer :: i
    integer :: limit

    value = ""
    if (present(value_len)) value_len = 0
    if (len(value) <= 0) then
      call assign_error(error_code, 0_c_int)
      success = .false.
      return
    end if

    call to_c_string(name, c_name)
    allocate(c_value(len(value) + 1))
    rc = c_ftop_freebsd_sysctl_string(c_name, c_value, int(size(c_value), c_size_t), c_value_len, sys_errno)
    success = rc == 0_c_int
    if (success) then
      copied_len = 0
      limit = min(len(value), int(c_value_len))
      do i = 1, limit
        if (c_value(i) == c_null_char) exit
        value(i:i) = achar(iachar(c_value(i)))
        copied_len = copied_len + 1
      end do
      if (present(value_len)) value_len = copied_len
    end if
    call assign_error(error_code, sys_errno)
  end function freebsd_sysctl_string

  logical function freebsd_sysctl_bytes(name, value, value_len, error_code) result(success)
    character(len=*), intent(in) :: name
    character(kind=c_char), intent(out) :: value(:)
    integer, intent(out), optional :: value_len
    integer, intent(out), optional :: error_code
    character(kind=c_char), allocatable :: c_name(:)
    integer(c_size_t) :: c_value_len
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    if (present(value_len)) value_len = 0
    if (size(value) <= 0) then
      call assign_error(error_code, 0_c_int)
      success = .false.
      return
    end if

    call to_c_string(name, c_name)
    rc = c_ftop_freebsd_sysctl_bytes(c_name, value, int(size(value), c_size_t), c_value_len, sys_errno)
    success = rc == 0_c_int
    if (success .and. present(value_len)) value_len = int(c_value_len)
    call assign_error(error_code, sys_errno)
  end function freebsd_sysctl_bytes

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

end module ftop_platform
