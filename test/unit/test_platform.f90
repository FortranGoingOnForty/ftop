program test_platform
  use, intrinsic :: iso_fortran_env, only : real64
  use ftop_cpu_data, only : cpu_core_info, cpu_state_ticks, cpu_state_total_ticks
  use ftop_platform, only : &
    cpu_tick_sample, &
    cpu_topology_info, &
    cpu_usage_percent, &
    create_platform, &
    load_average_info, &
    memory_info, &
    platform_backend, &
    system_uptime_info
  use ftop_proc_data, only : process_table
  use ftop_signal, only : ftop_current_pid
  implicit none

  class(platform_backend), allocatable :: backend
  type(cpu_tick_sample) :: first_sample
  type(cpu_tick_sample) :: second_sample
  type(cpu_topology_info) :: topology
  type(cpu_state_ticks) :: total_cpu_state
  type(cpu_state_ticks), allocatable :: core_cpu_states(:)
  type(cpu_core_info), allocatable :: cpu_metadata(:)
  type(memory_info) :: memory
  type(load_average_info) :: load_average
  type(system_uptime_info) :: uptime
  type(process_table) :: processes
  integer :: cpu_count
  integer :: current_pid
  integer :: process_index
  real(real64) :: usage
  logical :: found_current_process

  backend = create_platform()
  if (.not. allocated(backend)) error stop "platform factory did not allocate a backend"

  cpu_count = backend%get_cpu_count()
  if (cpu_count <= 0) error stop "platform CPU count must be positive"

  topology = backend%get_cpu_topology()
  if (.not. topology%valid) error stop "platform CPU topology must be valid"
  if (topology%core_count <= 0) error stop "platform CPU topology core count must be positive"
  if (topology%thread_count <= 0) error stop "platform CPU topology thread count must be positive"
  if (topology%core_count > topology%thread_count) error stop "platform CPU topology core count exceeds threads"
  if (topology%thread_count /= cpu_count) error stop "platform CPU topology thread count mismatch"
  if (.not. topology%model_name_valid) error stop "platform CPU model name must be valid"
  if (len_trim(topology%model_name) <= 0) error stop "platform CPU model name must not be empty"

  first_sample = backend%get_cpu_sample()
  if (.not. first_sample%valid) error stop "first CPU tick sample must be valid"
  if (first_sample%total <= 0) error stop "first CPU tick total must be positive"
  if (first_sample%idle < 0) error stop "first CPU idle ticks must not be negative"

  second_sample = wait_for_next_sample(backend, first_sample)
  if (.not. second_sample%valid) error stop "second CPU tick sample must be valid"

  usage = cpu_usage_percent(first_sample, second_sample)
  if (usage < 0.0_real64 .or. usage > 100.0_real64) error stop "CPU usage must be in range"

  if (.not. backend%get_cpu_state_snapshot(total_cpu_state, core_cpu_states)) error stop "CPU state snapshot failed"
  if (.not. total_cpu_state%valid) error stop "total CPU state must be valid"
  if (size(core_cpu_states) <= 0) error stop "CPU state snapshot must include cores"
  if (.not. core_cpu_states(1)%valid) error stop "first CPU core state must be valid"
  if (cpu_state_total_ticks(total_cpu_state) <= 0) error stop "total CPU state ticks must be positive"
  if (cpu_state_total_ticks(core_cpu_states(1)) <= 0) error stop "first CPU core ticks must be positive"

  if (.not. backend%get_cpu_metadata(cpu_metadata)) error stop "CPU metadata snapshot failed"
  if (size(cpu_metadata) /= cpu_count) error stop "CPU metadata count mismatch"
  if (any(cpu_metadata%freq_valid .and. cpu_metadata%freq_mhz <= 0.0_real64)) error stop "CPU frequency must be positive"
  if (any(cpu_metadata%temp_valid .and. cpu_metadata%temp_c < -100.0_real64)) error stop "CPU temperature too low"
  if (any(cpu_metadata%temp_valid .and. cpu_metadata%temp_c > 150.0_real64)) error stop "CPU temperature too high"

  memory = backend%get_memory_info()
  if (.not. memory%valid) error stop "memory info must be valid"
  if (memory%total_bytes <= 0) error stop "total memory must be positive"
  if (memory%used_bytes < 0) error stop "used memory must not be negative"
  if (memory%free_bytes < 0) error stop "free memory must not be negative"
  if (memory%available_bytes < 0) error stop "available memory must not be negative"
  if (memory%cached_bytes < 0) error stop "cached memory must not be negative"
  if (memory%buffers_bytes < 0) error stop "buffer memory must not be negative"
  if (memory%swap_total_bytes < 0) error stop "swap total memory must not be negative"
  if (memory%swap_used_bytes < 0) error stop "swap used memory must not be negative"
  if (memory%used_bytes > memory%total_bytes) error stop "used memory must not exceed total"
  if (memory%free_bytes > memory%total_bytes) error stop "free memory must not exceed total"
  if (memory%available_bytes > memory%total_bytes) error stop "available memory must not exceed total"
  if (memory%swap_used_bytes > memory%swap_total_bytes) error stop "swap used memory must not exceed swap total"

  load_average = backend%get_load_average()
  if (.not. load_average%valid) error stop "load average info must be valid"
  if (any(load_average%values < 0.0_real64)) error stop "load averages must not be negative"

  uptime = backend%get_system_uptime()
  if (.not. uptime%valid) error stop "system uptime must be valid"
  if (uptime%seconds < 0) error stop "system uptime must not be negative"

  processes = backend%get_process_table()
  if (.not. processes%valid) error stop "process table must be valid"
  if (.not. allocated(processes%items)) error stop "process table items must be allocated"
  if (size(processes%items) <= 0) error stop "process table must include processes"
  if (.not. any(processes%items%valid)) error stop "process table must include valid processes"
  if (any(processes%items%valid .and. processes%items%pid <= 0)) error stop "valid process pid must be positive"
  if (any(processes%items%mem_rss_bytes < 0)) error stop "process RSS must not be negative"
  current_pid = ftop_current_pid()
  found_current_process = .false.
  do process_index = 1, size(processes%items)
    if (processes%items(process_index)%pid /= current_pid) cycle
    found_current_process = .true.
    if (.not. processes%items(process_index)%user_valid) error stop "current process user must be valid"
    if (len_trim(processes%items(process_index)%user) <= 0) error stop "current process user must not be empty"
  end do
  if (.not. found_current_process) error stop "process table must include current process"

contains

  function wait_for_next_sample(backend, previous) result(sample)
    class(platform_backend), intent(in) :: backend
    type(cpu_tick_sample), intent(in) :: previous
    type(cpu_tick_sample) :: sample
    integer :: start_count
    integer :: current_count
    integer :: rate
    integer :: elapsed_ms

    call system_clock(start_count, rate)
    do
      sample = backend%get_cpu_sample()
      if (sample%valid .and. sample%total > previous%total) return

      call system_clock(current_count)
      if (rate <= 0) return
      elapsed_ms = int((real(current_count - start_count) / real(rate)) * 1000.0)
      if (elapsed_ms > 1000) return
    end do
  end function wait_for_next_sample
end program test_platform
