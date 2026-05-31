program test_platform
  use, intrinsic :: iso_fortran_env, only : real64
  use ftop_cpu_data, only : cpu_state_ticks, cpu_state_total_ticks
  use ftop_platform, only : &
    cpu_tick_sample, &
    cpu_usage_percent, &
    create_platform, &
    load_average_info, &
    memory_info, &
    platform_backend
  implicit none

  class(platform_backend), allocatable :: backend
  type(cpu_tick_sample) :: first_sample
  type(cpu_tick_sample) :: second_sample
  type(cpu_state_ticks) :: total_cpu_state
  type(cpu_state_ticks), allocatable :: core_cpu_states(:)
  type(memory_info) :: memory
  type(load_average_info) :: load_average
  integer :: cpu_count
  real(real64) :: usage

  backend = create_platform()
  if (.not. allocated(backend)) error stop "platform factory did not allocate a backend"

  cpu_count = backend%get_cpu_count()
  if (cpu_count <= 0) error stop "platform CPU count must be positive"

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
