module ftop_platform
  use, intrinsic :: iso_c_binding, only : c_char, c_double, c_int, c_long_long, c_null_char, c_size_t
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use ftop_cpu_data, only : cpu_core_info, cpu_state_ticks, cpu_state_total_ticks
  use ftop_platform_types, only : cpu_tick_sample, cpu_usage_percent, load_average_info, memory_info, platform_backend
  implicit none
  private

  type, bind(C), public :: macos_processor_ticks
    integer(c_long_long) :: user
    integer(c_long_long) :: system
    integer(c_long_long) :: idle
    integer(c_long_long) :: nice
  end type macos_processor_ticks

  type, extends(platform_backend) :: macos_backend
  contains
    procedure :: get_cpu_count => macos_get_cpu_count
    procedure :: get_cpu_sample => macos_get_cpu_sample
    procedure :: get_cpu_state_snapshot => macos_get_cpu_state_snapshot
    procedure :: get_cpu_metadata => macos_get_cpu_metadata
    procedure :: get_memory_info => macos_get_memory_info
    procedure :: get_load_average => macos_get_load_average
  end type macos_backend

  public :: create_platform
  public :: cpu_tick_sample
  public :: cpu_usage_percent
  public :: load_average_info
  public :: macos_iokit_disk_count
  public :: macos_iokit_gpu_count
  public :: macos_processor_tick_samples
  public :: macos_sysctl_bytes
  public :: macos_sysctl_int
  public :: macos_sysctl_int64
  public :: macos_sysctl_string
  public :: memory_info
  public :: platform_backend

  interface
    integer(c_int) function c_ftop_macos_cpu_count(count, sys_errno) bind(C, name="ftop_macos_cpu_count")
      import :: c_int
      integer(c_int), intent(out) :: count
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_macos_cpu_count

    integer(c_int) function c_ftop_macos_cpu_ticks(total_ticks, idle_ticks, sys_errno) &
        bind(C, name="ftop_macos_cpu_ticks")
      import :: c_int, c_long_long
      integer(c_long_long), intent(out) :: total_ticks
      integer(c_long_long), intent(out) :: idle_ticks
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_macos_cpu_ticks

    integer(c_int) function c_ftop_macos_memory_info(total_bytes, used_bytes, free_bytes, available_bytes, &
        swap_total_bytes, swap_used_bytes, sys_errno) &
        bind(C, name="ftop_macos_memory_info")
      import :: c_int, c_long_long
      integer(c_long_long), intent(out) :: total_bytes
      integer(c_long_long), intent(out) :: used_bytes
      integer(c_long_long), intent(out) :: free_bytes
      integer(c_long_long), intent(out) :: available_bytes
      integer(c_long_long), intent(out) :: swap_total_bytes
      integer(c_long_long), intent(out) :: swap_used_bytes
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_macos_memory_info

    integer(c_int) function c_ftop_macos_load_average(loads, sys_errno) bind(C, name="ftop_macos_load_average")
      import :: c_double, c_int
      real(c_double), intent(out) :: loads(3)
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_macos_load_average

    integer(c_int) function c_ftop_macos_processor_ticks(ticks, capacity, processor_count, sys_errno) &
        bind(C, name="ftop_macos_processor_ticks")
      import :: c_int, c_size_t, macos_processor_ticks
      type(macos_processor_ticks), intent(out) :: ticks(*)
      integer(c_size_t), value :: capacity
      integer(c_size_t), intent(out) :: processor_count
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_macos_processor_ticks

    integer(c_int) function c_ftop_macos_sysctl_int(name, value, sys_errno) &
        bind(C, name="ftop_macos_sysctl_int")
      import :: c_char, c_int
      character(kind=c_char), intent(in) :: name(*)
      integer(c_int), intent(out) :: value
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_macos_sysctl_int

    integer(c_int) function c_ftop_macos_sysctl_long_long(name, value, sys_errno) &
        bind(C, name="ftop_macos_sysctl_long_long")
      import :: c_char, c_int, c_long_long
      character(kind=c_char), intent(in) :: name(*)
      integer(c_long_long), intent(out) :: value
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_macos_sysctl_long_long

    integer(c_int) function c_ftop_macos_sysctl_string(name, value, value_capacity, value_len, sys_errno) &
        bind(C, name="ftop_macos_sysctl_string")
      import :: c_char, c_int, c_size_t
      character(kind=c_char), intent(in) :: name(*)
      character(kind=c_char), intent(out) :: value(*)
      integer(c_size_t), value :: value_capacity
      integer(c_size_t), intent(out) :: value_len
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_macos_sysctl_string

    integer(c_int) function c_ftop_macos_sysctl_bytes(name, value, value_capacity, value_len, sys_errno) &
        bind(C, name="ftop_macos_sysctl_bytes")
      import :: c_char, c_int, c_size_t
      character(kind=c_char), intent(in) :: name(*)
      character(kind=c_char), intent(out) :: value(*)
      integer(c_size_t), value :: value_capacity
      integer(c_size_t), intent(out) :: value_len
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_macos_sysctl_bytes

    integer(c_int) function c_ftop_macos_iokit_gpu_count(count, sys_errno) bind(C, name="ftop_macos_iokit_gpu_count")
      import :: c_int
      integer(c_int), intent(out) :: count
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_macos_iokit_gpu_count

    integer(c_int) function c_ftop_macos_iokit_disk_count(count, sys_errno) bind(C, name="ftop_macos_iokit_disk_count")
      import :: c_int
      integer(c_int), intent(out) :: count
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_macos_iokit_disk_count
  end interface

contains

  function create_platform() result(backend)
    class(platform_backend), allocatable :: backend

    allocate(backend, source=macos_backend())
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

  function macos_get_cpu_sample(self) result(sample)
    class(macos_backend), intent(in) :: self
    type(cpu_tick_sample) :: sample
    integer(c_long_long) :: total_ticks
    integer(c_long_long) :: idle_ticks
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    associate(unused => self)
    end associate

    rc = c_ftop_macos_cpu_ticks(total_ticks, idle_ticks, sys_errno)
    if (rc == 0_c_int .and. total_ticks > 0_c_long_long) then
      sample%valid = .true.
      sample%total = int(total_ticks, int64)
      sample%idle = int(max(0_c_long_long, idle_ticks), int64)
    end if
  end function macos_get_cpu_sample

  logical function macos_get_cpu_state_snapshot(self, total, cores) result(success)
    class(macos_backend), intent(in) :: self
    type(cpu_state_ticks), intent(out) :: total
    type(cpu_state_ticks), allocatable, intent(out) :: cores(:)
    type(macos_processor_ticks), allocatable :: c_ticks(:)
    integer :: capacity
    integer :: processor_count
    integer :: i

    total = cpu_state_ticks()
    if (allocated(cores)) deallocate(cores)
    capacity = max(1, macos_get_cpu_count(self))
    allocate(c_ticks(capacity))

    success = macos_processor_tick_samples(c_ticks, processor_count)
    if (.not. success .or. processor_count <= 0) then
      allocate(cores(0))
      success = .false.
      return
    end if

    allocate(cores(processor_count))
    do i = 1, size(cores)
      cores(i) = macos_cpu_state_from_c(c_ticks(i))
      call add_cpu_state(total, cores(i))
    end do
    total%valid = cpu_state_total_ticks(total) > 0_int64
  end function macos_get_cpu_state_snapshot

  logical function macos_get_cpu_metadata(self, cores) result(success)
    class(macos_backend), intent(in) :: self
    type(cpu_core_info), allocatable, intent(out) :: cores(:)
    integer :: cpu_count
    integer :: cpu_index
    integer(int64) :: frequency_hz

    cpu_count = max(0, macos_get_cpu_count(self))
    allocate(cores(cpu_count))
    if (macos_sysctl_int64("hw.cpufrequency", frequency_hz) .and. frequency_hz > 0_int64) then
      do cpu_index = 1, cpu_count
        cores(cpu_index)%freq_valid = .true.
        cores(cpu_index)%freq_mhz = real(frequency_hz, real64) / 1000000.0_real64
      end do
    end if
    success = cpu_count > 0
  end function macos_get_cpu_metadata

  function macos_get_memory_info(self) result(info)
    class(macos_backend), intent(in) :: self
    type(memory_info) :: info
    integer(c_long_long) :: total_bytes
    integer(c_long_long) :: used_bytes
    integer(c_long_long) :: free_bytes
    integer(c_long_long) :: available_bytes
    integer(c_long_long) :: swap_total_bytes
    integer(c_long_long) :: swap_used_bytes
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    associate(unused => self)
    end associate

    rc = c_ftop_macos_memory_info(total_bytes, used_bytes, free_bytes, available_bytes, swap_total_bytes, &
                                  swap_used_bytes, sys_errno)
    if (rc == 0_c_int .and. total_bytes > 0_c_long_long) then
      info%valid = .true.
      info%total_bytes = int(total_bytes, int64)
      info%used_bytes = int(max(0_c_long_long, min(total_bytes, used_bytes)), int64)
      info%free_bytes = int(max(0_c_long_long, min(total_bytes, free_bytes)), int64)
      info%available_bytes = int(max(0_c_long_long, min(total_bytes, available_bytes)), int64)
      info%cached_bytes = max(0_int64, info%available_bytes - info%free_bytes)
      swap_total_bytes = max(0_c_long_long, swap_total_bytes)
      info%swap_total_bytes = int(swap_total_bytes, int64)
      info%swap_used_bytes = int(max(0_c_long_long, min(swap_total_bytes, swap_used_bytes)), int64)
    end if
  end function macos_get_memory_info

  function macos_get_load_average(self) result(info)
    class(macos_backend), intent(in) :: self
    type(load_average_info) :: info
    real(c_double) :: loads(3)
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    associate(unused => self)
    end associate

    rc = c_ftop_macos_load_average(loads, sys_errno)
    if (rc == 0_c_int .and. all(loads >= 0.0_c_double)) then
      info%valid = .true.
      info%values = real(loads, real64)
    end if
  end function macos_get_load_average

  function macos_cpu_state_from_c(c_ticks) result(ticks)
    type(macos_processor_ticks), intent(in) :: c_ticks
    type(cpu_state_ticks) :: ticks

    ticks%user = int(max(0_c_long_long, c_ticks%user), int64)
    ticks%nice = int(max(0_c_long_long, c_ticks%nice), int64)
    ticks%system = int(max(0_c_long_long, c_ticks%system), int64)
    ticks%idle = int(max(0_c_long_long, c_ticks%idle), int64)
    ticks%valid = cpu_state_total_ticks(ticks) > 0_int64
  end function macos_cpu_state_from_c

  subroutine add_cpu_state(total, core)
    type(cpu_state_ticks), intent(inout) :: total
    type(cpu_state_ticks), intent(in) :: core

    if (.not. core%valid) return
    total%user = total%user + core%user
    total%nice = total%nice + core%nice
    total%system = total%system + core%system
    total%idle = total%idle + core%idle
    total%iowait = total%iowait + core%iowait
    total%irq = total%irq + core%irq
    total%softirq = total%softirq + core%softirq
    total%steal = total%steal + core%steal
  end subroutine add_cpu_state

  logical function macos_processor_tick_samples(ticks, processor_count, error_code) result(success)
    type(macos_processor_ticks), intent(out) :: ticks(:)
    integer, intent(out) :: processor_count
    integer, intent(out), optional :: error_code
    integer(c_size_t) :: c_processor_count
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    processor_count = 0
    if (size(ticks) <= 0) then
      call assign_error(error_code, 0_c_int)
      success = .false.
      return
    end if

    rc = c_ftop_macos_processor_ticks(ticks, int(size(ticks), c_size_t), c_processor_count, sys_errno)
    success = rc == 0_c_int
    if (success) processor_count = int(c_processor_count)
    call assign_error(error_code, sys_errno)
  end function macos_processor_tick_samples

  logical function macos_sysctl_int(name, value, error_code) result(success)
    character(len=*), intent(in) :: name
    integer, intent(out) :: value
    integer, intent(out), optional :: error_code
    character(kind=c_char), allocatable :: c_name(:)
    integer(c_int) :: c_value
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    call to_c_string(name, c_name)
    rc = c_ftop_macos_sysctl_int(c_name, c_value, sys_errno)
    success = rc == 0_c_int
    if (success) then
      value = int(c_value)
    else
      value = 0
    end if
    call assign_error(error_code, sys_errno)
  end function macos_sysctl_int

  logical function macos_sysctl_int64(name, value, error_code) result(success)
    character(len=*), intent(in) :: name
    integer(int64), intent(out) :: value
    integer, intent(out), optional :: error_code
    character(kind=c_char), allocatable :: c_name(:)
    integer(c_long_long) :: c_value
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    call to_c_string(name, c_name)
    rc = c_ftop_macos_sysctl_long_long(c_name, c_value, sys_errno)
    success = rc == 0_c_int
    if (success) then
      value = int(c_value, int64)
    else
      value = 0_int64
    end if
    call assign_error(error_code, sys_errno)
  end function macos_sysctl_int64

  logical function macos_sysctl_string(name, value, value_len, error_code) result(success)
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
    rc = c_ftop_macos_sysctl_string(c_name, c_value, int(size(c_value), c_size_t), c_value_len, sys_errno)
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
  end function macos_sysctl_string

  logical function macos_sysctl_bytes(name, value, value_len, error_code) result(success)
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
    rc = c_ftop_macos_sysctl_bytes(c_name, value, int(size(value), c_size_t), c_value_len, sys_errno)
    success = rc == 0_c_int
    if (success .and. present(value_len)) value_len = int(c_value_len)
    call assign_error(error_code, sys_errno)
  end function macos_sysctl_bytes

  logical function macos_iokit_gpu_count(count, error_code) result(success)
    integer, intent(out) :: count
    integer, intent(out), optional :: error_code
    integer(c_int) :: c_count
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    rc = c_ftop_macos_iokit_gpu_count(c_count, sys_errno)
    success = rc == 0_c_int
    if (success) then
      count = int(c_count)
    else
      count = 0
    end if
    call assign_error(error_code, sys_errno)
  end function macos_iokit_gpu_count

  logical function macos_iokit_disk_count(count, error_code) result(success)
    integer, intent(out) :: count
    integer, intent(out), optional :: error_code
    integer(c_int) :: c_count
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    rc = c_ftop_macos_iokit_disk_count(c_count, sys_errno)
    success = rc == 0_c_int
    if (success) then
      count = int(c_count)
    else
      count = 0
    end if
    call assign_error(error_code, sys_errno)
  end function macos_iokit_disk_count

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
