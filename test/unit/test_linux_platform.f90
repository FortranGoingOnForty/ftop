program test_linux_platform
  use, intrinsic :: iso_c_binding, only : c_null_char
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use ftop_cpu_data, only : cpu_state_ticks, cpu_state_total_ticks
  use ftop_mem_data, only : metric_memory_info => memory_info, memory_usage_percent
  use ftop_platform, only : &
    linux_cpuinfo_field, &
    linux_cpuinfo_field_count, &
    linux_cpu_state_snapshot, &
    linux_hwmon_discover, &
    linux_hwmon_sensor, &
    load_average_info, &
    linux_load_average_snapshot, &
    linux_memory_snapshot, &
    linux_network_snapshot, &
    linux_process_snapshot, &
    network_table, &
    process_table
  use ftop_signal, only : ftop_current_pid
  implicit none

  type(linux_hwmon_sensor), allocatable :: sensors(:)
  type(cpu_state_ticks) :: total_cpu
  type(cpu_state_ticks), allocatable :: cores(:)
  type(load_average_info) :: load_average
  type(metric_memory_info) :: memory
  type(network_table) :: network
  type(process_table) :: processes
  character(len=128) :: processor_id
  integer :: current_pid
  integer :: i
  integer :: processor_count
  integer :: processor_id_len
  integer :: sensor_count
  logical :: found_current_process

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

  if (.not. linux_load_average_snapshot(load_average)) error stop "Linux load average snapshot failed"
  if (.not. load_average%valid) error stop "Linux load average snapshot must be valid"
  if (any(load_average%values < 0.0_real64)) error stop "Linux load averages must not be negative"

  if (.not. linux_network_snapshot(network)) error stop "Linux network snapshot failed"
  if (.not. network%valid) error stop "Linux network snapshot must be valid"
  if (.not. allocated(network%interfaces)) error stop "Linux network interfaces must be allocated"
  if (.not. allocated(network%connections)) error stop "Linux network connections must be allocated"
  if (.not. allocated(network%processes)) error stop "Linux network processes must be allocated"
  do i = 1, size(network%processes)
    if (.not. network%processes(i)%valid) cycle
    if (network%processes(i)%pid <= 0) error stop "Linux network process pid must be positive"
    if (network%processes(i)%start_time <= 0_int64) error stop "Linux network process start time must be positive"
    if (network%processes(i)%rx_bytes < 0_int64) error stop "Linux network process rx bytes must not be negative"
    if (network%processes(i)%tx_bytes < 0_int64) error stop "Linux network process tx bytes must not be negative"
    if (network%processes(i)%rx_bytes_per_sec < 0.0_real64) error stop "Linux network process rx rate must not be negative"
    if (network%processes(i)%tx_bytes_per_sec < 0.0_real64) error stop "Linux network process tx rate must not be negative"
  end do

  if (.not. linux_process_snapshot(processes, memory%total_bytes)) error stop "Linux process snapshot failed"
  if (.not. processes%valid) error stop "Linux process snapshot must be valid"
  if (.not. allocated(processes%items)) error stop "Linux process snapshot items must be allocated"
  if (size(processes%items) <= 0) error stop "Linux process snapshot must include processes"
  if (any(processes%items%io_read_bytes < 0_int64)) error stop "Linux process read bytes must not be negative"
  if (any(processes%items%io_write_bytes < 0_int64)) error stop "Linux process write bytes must not be negative"
  if (.not. any(len_trim(processes%items%cgroup) > 0)) error stop "Linux process snapshot must include cgroups"

  current_pid = ftop_current_pid()
  found_current_process = .false.
  do i = 1, size(processes%items)
    if (processes%items(i)%pid /= current_pid) cycle
    found_current_process = .true.
    if (len_trim(processes%items(i)%cgroup) <= 0) error stop "current Linux process cgroup must not be empty"
    if (processes%items(i)%io_read_bytes < 0_int64) error stop "current Linux process read bytes must not be negative"
    if (processes%items(i)%io_write_bytes < 0_int64) error stop "current Linux process write bytes must not be negative"
  end do
  if (.not. found_current_process) error stop "Linux process snapshot must include current process"
end program test_linux_platform
