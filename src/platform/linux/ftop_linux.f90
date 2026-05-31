module ftop_platform
  use, intrinsic :: iso_c_binding, only : c_char, c_int, c_long_long, c_null_char, c_size_t
  use, intrinsic :: iso_fortran_env, only : int64
  use ftop_platform_types, only : cpu_tick_sample, cpu_usage_percent, memory_info, platform_backend
  implicit none
  private

  integer, parameter, public :: LINUX_HWMON_NAME_LEN = 128
  integer, parameter, public :: LINUX_HWMON_PATH_LEN = 256

  type, bind(C), public :: linux_hwmon_sensor
    character(kind=c_char) :: path(LINUX_HWMON_PATH_LEN)
    character(kind=c_char) :: name(LINUX_HWMON_NAME_LEN)
  end type linux_hwmon_sensor

  type, extends(platform_backend) :: linux_backend
  contains
    procedure :: get_cpu_count => linux_get_cpu_count
    procedure :: get_cpu_sample => linux_get_cpu_sample
    procedure :: get_memory_info => linux_get_memory_info
  end type linux_backend

  public :: create_platform
  public :: cpu_tick_sample
  public :: cpu_usage_percent
  public :: linux_cpuinfo_field
  public :: linux_cpuinfo_field_count
  public :: linux_hwmon_discover
  public :: memory_info
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

    integer(c_int) function c_ftop_linux_memory_info(total_bytes, available_bytes, sys_errno) &
        bind(C, name="ftop_linux_memory_info")
      import :: c_int, c_long_long
      integer(c_long_long), intent(out) :: total_bytes
      integer(c_long_long), intent(out) :: available_bytes
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_memory_info

    integer(c_int) function c_ftop_linux_cpuinfo_field(name, value, value_capacity, value_len, sys_errno) &
        bind(C, name="ftop_linux_cpuinfo_field")
      import :: c_char, c_int, c_size_t
      character(kind=c_char), intent(in) :: name(*)
      character(kind=c_char), intent(out) :: value(*)
      integer(c_size_t), value :: value_capacity
      integer(c_size_t), intent(out) :: value_len
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_cpuinfo_field

    integer(c_int) function c_ftop_linux_cpuinfo_field_count(name, count, sys_errno) &
        bind(C, name="ftop_linux_cpuinfo_field_count")
      import :: c_char, c_int
      character(kind=c_char), intent(in) :: name(*)
      integer(c_int), intent(out) :: count
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_cpuinfo_field_count

    integer(c_int) function c_ftop_linux_hwmon_discover(sensors, capacity, sensor_count, sys_errno) &
        bind(C, name="ftop_linux_hwmon_discover")
      import :: c_int, c_size_t, linux_hwmon_sensor
      type(linux_hwmon_sensor), intent(out) :: sensors(*)
      integer(c_size_t), value :: capacity
      integer(c_size_t), intent(out) :: sensor_count
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_hwmon_discover
  end interface

contains

  function create_platform() result(backend)
    class(platform_backend), allocatable :: backend

    allocate(backend, source=linux_backend())
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

  function linux_get_memory_info(self) result(info)
    class(linux_backend), intent(in) :: self
    type(memory_info) :: info
    integer(c_long_long) :: total_bytes
    integer(c_long_long) :: available_bytes
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    associate(unused => self)
    end associate

    rc = c_ftop_linux_memory_info(total_bytes, available_bytes, sys_errno)
    if (rc == 0_c_int .and. total_bytes > 0_c_long_long) then
      info%valid = .true.
      info%total_bytes = int(total_bytes, int64)
      info%available_bytes = int(max(0_c_long_long, min(total_bytes, available_bytes)), int64)
    end if
  end function linux_get_memory_info

  logical function linux_cpuinfo_field(name, value, value_len, error_code) result(success)
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
    rc = c_ftop_linux_cpuinfo_field(c_name, c_value, int(size(c_value), c_size_t), c_value_len, sys_errno)
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
  end function linux_cpuinfo_field

  logical function linux_cpuinfo_field_count(name, count, error_code) result(success)
    character(len=*), intent(in) :: name
    integer, intent(out) :: count
    integer, intent(out), optional :: error_code
    character(kind=c_char), allocatable :: c_name(:)
    integer(c_int) :: c_count
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    call to_c_string(name, c_name)
    rc = c_ftop_linux_cpuinfo_field_count(c_name, c_count, sys_errno)
    success = rc == 0_c_int
    if (success) then
      count = int(c_count)
    else
      count = 0
    end if
    call assign_error(error_code, sys_errno)
  end function linux_cpuinfo_field_count

  logical function linux_hwmon_discover(sensors, sensor_count, error_code) result(success)
    type(linux_hwmon_sensor), intent(out) :: sensors(:)
    integer, intent(out) :: sensor_count
    integer, intent(out), optional :: error_code
    integer(c_size_t) :: c_sensor_count
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    sensor_count = 0
    if (size(sensors) <= 0) then
      call assign_error(error_code, 0_c_int)
      success = .false.
      return
    end if

    rc = c_ftop_linux_hwmon_discover(sensors, int(size(sensors), c_size_t), c_sensor_count, sys_errno)
    success = rc == 0_c_int
    if (success) sensor_count = int(c_sensor_count)
    call assign_error(error_code, sys_errno)
  end function linux_hwmon_discover

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
