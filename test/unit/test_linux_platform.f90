program test_linux_platform
  use, intrinsic :: iso_c_binding, only : c_null_char
  use, intrinsic :: iso_fortran_env, only : real64
  use ftop_cpu_data, only : cpu_state_ticks, cpu_state_total_ticks
  use ftop_mem_data, only : metric_memory_info => memory_info, memory_usage_percent
  use ftop_platform, only : &
    linux_cpuinfo_field, &
    linux_cpuinfo_field_count, &
    linux_cpu_state_snapshot, &
    linux_hwmon_discover, &
    linux_hwmon_sensor, &
    linux_memory_snapshot
  implicit none

  type(linux_hwmon_sensor), allocatable :: sensors(:)
  type(cpu_state_ticks) :: total_cpu
  type(cpu_state_ticks), allocatable :: cores(:)
  type(metric_memory_info) :: memory
  character(len=128) :: processor_id
  integer :: i
  integer :: processor_count
  integer :: processor_id_len
  integer :: sensor_count

  if (.not. linux_cpuinfo_field_count("processor", processor_count)) error stop "processor count failed"
  if (processor_count <= 0) error stop "processor count must be positive"

  if (.not. linux_cpuinfo_field("processor", processor_id, processor_id_len)) error stop "processor field failed"
  if (processor_id_len <= 0) error stop "processor field must not be empty"

  allocate(sensors(64))
  if (.not. linux_hwmon_discover(sensors, sensor_count)) error stop "hwmon discovery failed"
  if (sensor_count < 0) error stop "hwmon sensor count must not be negative"

  do i = 1, sensor_count
    if (sensors(i)%path(1) == c_null_char) error stop "hwmon sensor path must not be empty"
    if (sensors(i)%name(1) == c_null_char) error stop "hwmon sensor name must not be empty"
  end do

  if (.not. linux_cpu_state_snapshot(total_cpu, cores)) error stop "Linux CPU state snapshot failed"
  if (.not. total_cpu%valid) error stop "Linux total CPU state must be valid"
  if (cpu_state_total_ticks(total_cpu) <= 0) error stop "Linux total CPU ticks must be positive"
  if (size(cores) <= 0) error stop "Linux core CPU states must not be empty"
  do i = 1, size(cores)
    if (.not. cores(i)%valid) error stop "Linux core CPU state must be valid"
    if (cpu_state_total_ticks(cores(i)) <= 0) error stop "Linux core CPU ticks must be positive"
  end do

  if (.not. linux_memory_snapshot(memory)) error stop "Linux memory snapshot failed"
  if (.not. memory%valid) error stop "Linux memory snapshot must be valid"
  if (memory%total_bytes <= 0) error stop "Linux memory total must be positive"
  if (memory%available_bytes < 0) error stop "Linux available memory must not be negative"
  if (memory%available_bytes > memory%total_bytes) error stop "Linux available memory must not exceed total"
  if (memory%used_bytes < 0) error stop "Linux used memory must not be negative"
  if (memory%used_bytes > memory%total_bytes) error stop "Linux used memory must not exceed total"
  if (memory_usage_percent(memory) < 0.0_real64) error stop "Linux memory usage must not be negative"
  if (memory_usage_percent(memory) > 100.0_real64) error stop "Linux memory usage must not exceed 100"
end program test_linux_platform
