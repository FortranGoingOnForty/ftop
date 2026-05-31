module ftop_collector
  use, intrinsic :: iso_c_binding, only : &
    c_double, &
    c_f_pointer, &
    c_funloc, &
    c_int, &
    c_loc, &
    c_long_long, &
    c_null_ptr, &
    c_ptr
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use ftop_cpu_data, only : &
    cpu_core_info, &
    cpu_state_delta_info, &
    cpu_state_ticks, &
    cpu_total_info
  use ftop_mem_data, only : metric_memory_info => memory_info
  use ftop_platform, only : &
    create_platform, &
    cpu_topology_info, &
    load_average_info, &
    platform_backend, &
    platform_memory_info => memory_info
  use ftop_pthread, only : &
    ftop_mutex_destroy, &
    ftop_mutex_handle, &
    ftop_mutex_init, &
    ftop_mutex_lock, &
    ftop_mutex_unlock, &
    ftop_thread_create, &
    ftop_thread_handle, &
    ftop_thread_join
  implicit none
  private

  integer, parameter, public :: FTOP_COLLECTOR_DEFAULT_INTERVAL_MS = 1000
  integer, parameter, public :: FTOP_COLLECTOR_MIN_INTERVAL_MS = 1
  integer, parameter, public :: FTOP_COLLECTOR_MAX_INTERVAL_MS = 60000
  integer, parameter, public :: FTOP_COLLECTOR_HISTORY_CAPACITY = 300
  integer, parameter, public :: FTOP_COLLECTOR_MAX_CPU_CORES = 512

  type, bind(C) :: collector_shared_state
    integer(c_int) :: stop_requested
    integer(c_int) :: running
    integer(c_int) :: warming_up
    integer(c_int) :: sample_count
    integer(c_int) :: interval_ms
    integer(c_int) :: cpu_count
    integer(c_int) :: cpu_physical_core_count
    integer(c_int) :: cpu_valid
    real(c_double) :: cpu_usage_percent
    real(c_double) :: cpu_user_percent
    real(c_double) :: cpu_system_percent
    real(c_double) :: cpu_iowait_percent
    integer(c_int) :: cpu_core_count
    integer(c_int) :: cpu_core_valid(FTOP_COLLECTOR_MAX_CPU_CORES)
    real(c_double) :: cpu_core_usage_percent(FTOP_COLLECTOR_MAX_CPU_CORES)
    real(c_double) :: cpu_core_user_percent(FTOP_COLLECTOR_MAX_CPU_CORES)
    real(c_double) :: cpu_core_system_percent(FTOP_COLLECTOR_MAX_CPU_CORES)
    real(c_double) :: cpu_core_iowait_percent(FTOP_COLLECTOR_MAX_CPU_CORES)
    integer(c_int) :: cpu_core_freq_valid(FTOP_COLLECTOR_MAX_CPU_CORES)
    real(c_double) :: cpu_core_freq_mhz(FTOP_COLLECTOR_MAX_CPU_CORES)
    integer(c_int) :: cpu_core_temp_valid(FTOP_COLLECTOR_MAX_CPU_CORES)
    real(c_double) :: cpu_core_temp_c(FTOP_COLLECTOR_MAX_CPU_CORES)
    integer(c_int) :: load_valid
    real(c_double) :: load_average(3)
    integer(c_int) :: memory_valid
    integer(c_long_long) :: memory_total_bytes
    integer(c_long_long) :: memory_used_bytes
    integer(c_long_long) :: memory_free_bytes
    integer(c_long_long) :: memory_available_bytes
    integer(c_long_long) :: memory_cached_bytes
    integer(c_long_long) :: memory_buffers_bytes
    integer(c_long_long) :: memory_swap_total_bytes
    integer(c_long_long) :: memory_swap_used_bytes
    integer(c_int) :: history_start
    integer(c_int) :: history_count
    real(c_double) :: cpu_usage_history(FTOP_COLLECTOR_HISTORY_CAPACITY)
    real(c_double) :: cpu_core_usage_history(FTOP_COLLECTOR_MAX_CPU_CORES, FTOP_COLLECTOR_HISTORY_CAPACITY)
    real(c_double) :: memory_usage_history(FTOP_COLLECTOR_HISTORY_CAPACITY)
    type(c_ptr) :: mutex
  end type collector_shared_state

  type, public :: collector_snapshot
    logical :: running = .false.
    logical :: warming_up = .true.
    integer :: sample_count = 0
    type(cpu_total_info) :: cpu_total
    type(cpu_core_info), allocatable :: cpu_cores(:)
    type(metric_memory_info) :: memory
    real(real64), allocatable :: cpu_usage_history(:)
    real(real64), allocatable :: cpu_core_usage_history(:, :)
    real(real64), allocatable :: memory_usage_history(:)
  end type collector_snapshot

  type, public :: collector
    private
    type(collector_shared_state) :: state
    type(ftop_mutex_handle) :: mutex
    type(ftop_thread_handle) :: thread
    logical :: ready = .false.
    logical :: thread_started = .false.
  contains
    procedure :: init => collector_init
    procedure :: destroy => collector_destroy
    procedure :: start => collector_start
    procedure :: stop => collector_stop
    procedure :: snapshot => collector_snapshot_copy
    procedure :: running => collector_running
    procedure :: initialized => collector_initialized
    final :: collector_finalize
  end type collector

  interface
    integer(c_int) function c_ftop_collector_monotonic_ms(milliseconds, sys_errno) &
        bind(C, name="ftop_collector_monotonic_ms")
      import :: c_int, c_long_long
      integer(c_long_long), intent(out) :: milliseconds
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_collector_monotonic_ms

    integer(c_int) function c_ftop_collector_sleep_until_ms(deadline_ms, sys_errno) &
        bind(C, name="ftop_collector_sleep_until_ms")
      import :: c_int, c_long_long
      integer(c_long_long), value :: deadline_ms
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_collector_sleep_until_ms
  end interface

contains

  logical function collector_init(self, interval_ms) result(success)
    class(collector), intent(inout) :: self
    integer, intent(in), optional :: interval_ms

    success = .false.
    if (self%thread_started) return
    if (self%ready) then
      if (.not. self%destroy()) return
    end if

    call clear_state(self%state, bounded_interval_ms(interval_ms))
    if (.not. ftop_mutex_init(self%mutex)) return
    self%state%mutex = self%mutex%handle
    self%ready = .true.
    success = .true.
  end function collector_init

  logical function collector_destroy(self) result(success)
    class(collector), intent(inout) :: self

    success = .true.
    if (self%thread_started) success = self%stop()
    if (.not. success) return

    if (self%ready) success = ftop_mutex_destroy(self%mutex)
    if (.not. success) return

    call clear_state(self%state, FTOP_COLLECTOR_DEFAULT_INTERVAL_MS)
    self%ready = .false.
    self%thread_started = .false.
  end function collector_destroy

  logical function collector_start(self, interval_ms) result(success)
    class(collector), intent(inout), target :: self
    integer, intent(in), optional :: interval_ms

    success = .false.
    if (self%thread_started) then
      success = .true.
      return
    end if

    if (.not. self%ready) then
      if (.not. self%init(interval_ms)) return
    else if (present(interval_ms)) then
      if (.not. set_interval(self, interval_ms)) return
    end if

    if (.not. ftop_mutex_lock(self%mutex)) return
    call clear_runtime_state(self%state)
    self%state%running = 1_c_int
    if (.not. ftop_mutex_unlock(self%mutex)) return

    if (ftop_thread_create(self%thread, c_funloc(collector_thread_main), c_loc(self%state))) then
      self%thread_started = .true.
      success = .true.
    else
      call mark_not_running(self%state)
    end if
  end function collector_start

  logical function collector_stop(self) result(success)
    class(collector), intent(inout) :: self

    success = .true.
    if (.not. self%thread_started) return

    success = request_stop(self%state)
    if (.not. success) return

    success = ftop_thread_join(self%thread)
    if (success) self%thread_started = .false.
  end function collector_stop

  function collector_snapshot_copy(self) result(snapshot)
    class(collector), intent(in) :: self
    type(collector_snapshot) :: snapshot

    if (.not. self%ready) return
    if (.not. ftop_mutex_lock(self%mutex)) return

    snapshot%running = self%state%running /= 0_c_int
    snapshot%warming_up = self%state%warming_up /= 0_c_int
    snapshot%sample_count = int(self%state%sample_count)
    snapshot%cpu_total%valid = self%state%cpu_valid /= 0_c_int
    snapshot%cpu_total%usage_percent = real(self%state%cpu_usage_percent, real64)
    snapshot%cpu_total%user_percent = real(self%state%cpu_user_percent, real64)
    snapshot%cpu_total%system_percent = real(self%state%cpu_system_percent, real64)
    snapshot%cpu_total%iowait_percent = real(self%state%cpu_iowait_percent, real64)
    snapshot%cpu_total%load_valid = self%state%load_valid /= 0_c_int
    snapshot%cpu_total%load_avg = real(self%state%load_average, real64)
    snapshot%cpu_total%core_count = int(self%state%cpu_physical_core_count)
    snapshot%cpu_total%thread_count = int(self%state%cpu_count)
    snapshot%memory%valid = self%state%memory_valid /= 0_c_int
    snapshot%memory%total_bytes = int(self%state%memory_total_bytes, int64)
    snapshot%memory%used_bytes = int(self%state%memory_used_bytes, int64)
    snapshot%memory%free_bytes = int(self%state%memory_free_bytes, int64)
    snapshot%memory%available_bytes = int(self%state%memory_available_bytes, int64)
    snapshot%memory%cached_bytes = int(self%state%memory_cached_bytes, int64)
    snapshot%memory%buffers_bytes = int(self%state%memory_buffers_bytes, int64)
    snapshot%memory%swap_total_bytes = int(self%state%memory_swap_total_bytes, int64)
    snapshot%memory%swap_used_bytes = int(self%state%memory_swap_used_bytes, int64)
    if (.not. copy_cpu_cores(self%state, snapshot)) then
      call clear_snapshot(snapshot)
      call ignore_mutex_unlock(self%mutex)
      return
    end if
    if (.not. copy_history(self%state, snapshot)) then
      call clear_snapshot(snapshot)
      call ignore_mutex_unlock(self%mutex)
      return
    end if

    if (.not. ftop_mutex_unlock(self%mutex)) then
      call clear_snapshot(snapshot)
    end if
  end function collector_snapshot_copy

  logical function collector_running(self) result(is_running)
    class(collector), intent(in) :: self

    is_running = .false.
    if (.not. self%ready) return
    if (.not. ftop_mutex_lock(self%mutex)) return
    is_running = self%state%running /= 0_c_int
    if (.not. ftop_mutex_unlock(self%mutex)) is_running = .false.
  end function collector_running

  logical function collector_initialized(self) result(is_initialized)
    class(collector), intent(in) :: self

    is_initialized = self%ready
  end function collector_initialized

  subroutine collector_finalize(self)
    type(collector), intent(inout) :: self
    logical :: ignored

    ignored = collector_destroy(self)
  end subroutine collector_finalize

  function collector_thread_main(arg) bind(C) result(result)
    type(c_ptr), value :: arg
    type(c_ptr) :: result
    type(collector_shared_state), pointer :: state
    class(platform_backend), allocatable :: backend
    type(cpu_state_ticks) :: previous_total_cpu
    type(cpu_state_ticks) :: current_total_cpu
    type(cpu_state_ticks), allocatable :: previous_core_cpus(:)
    type(cpu_state_ticks), allocatable :: current_core_cpus(:)
    type(cpu_core_info) :: total_cpu
    type(cpu_core_info), allocatable :: core_cpus(:)
    type(cpu_core_info), allocatable :: cpu_metadata(:)
    type(cpu_topology_info) :: topology
    type(platform_memory_info) :: memory
    type(load_average_info) :: load_average
    integer(c_long_long) :: deadline_ms
    integer(c_long_long) :: metadata_refresh_ms
    logical :: warming_up

    result = c_null_ptr
    nullify(state)
    call c_f_pointer(arg, state)
    if (.not. associated(state)) return

    backend = create_platform()
    if (.not. backend%get_cpu_state_snapshot(previous_total_cpu, previous_core_cpus)) then
      previous_total_cpu = cpu_state_ticks()
      allocate(previous_core_cpus(0))
    end if
    call make_invalid_cpu_infos(size(previous_core_cpus), total_cpu, core_cpus)
    if (.not. backend%get_cpu_metadata(cpu_metadata)) allocate(cpu_metadata(0))
    call merge_cpu_metadata(core_cpus, cpu_metadata)
    topology = backend%get_cpu_topology()
    memory = backend%get_memory_info()
    load_average = backend%get_load_average()
    call publish_sample(state, topology, total_cpu, core_cpus, .true., memory, load_average)

    deadline_ms = monotonic_ms()
    metadata_refresh_ms = deadline_ms + 1000_c_long_long
    do
      if (sleep_until_or_stop(state, deadline_ms + interval_ms(state))) exit
      deadline_ms = deadline_ms + interval_ms(state)

      memory = backend%get_memory_info()
      load_average = backend%get_load_average()
      topology = backend%get_cpu_topology()
      warming_up = .true.
      call make_invalid_cpu_infos(size(previous_core_cpus), total_cpu, core_cpus)
      if (backend%get_cpu_state_snapshot(current_total_cpu, current_core_cpus)) then
        call make_cpu_delta_infos(previous_total_cpu, previous_core_cpus, current_total_cpu, current_core_cpus, &
                                  total_cpu, core_cpus)
        warming_up = .not. total_cpu%valid
        if (current_total_cpu%valid) then
          previous_total_cpu = current_total_cpu
          if (allocated(previous_core_cpus)) deallocate(previous_core_cpus)
          allocate(previous_core_cpus(size(current_core_cpus)))
          previous_core_cpus = current_core_cpus
        end if
      end if
      if (deadline_ms >= metadata_refresh_ms) then
        if (.not. backend%get_cpu_metadata(cpu_metadata)) then
          if (allocated(cpu_metadata)) deallocate(cpu_metadata)
          allocate(cpu_metadata(0))
        end if
        metadata_refresh_ms = deadline_ms + 1000_c_long_long
      end if
      call merge_cpu_metadata(core_cpus, cpu_metadata)

      call publish_sample(state, topology, total_cpu, core_cpus, warming_up, memory, load_average)
      if (should_stop(state)) exit
    end do

    call mark_not_running(state)
  end function collector_thread_main

  subroutine make_invalid_cpu_infos(core_count, total_cpu, core_cpus)
    integer, intent(in) :: core_count
    type(cpu_core_info), intent(out) :: total_cpu
    type(cpu_core_info), allocatable, intent(out) :: core_cpus(:)

    total_cpu = cpu_core_info()
    allocate(core_cpus(bounded_core_count(core_count)))
  end subroutine make_invalid_cpu_infos

  subroutine make_cpu_delta_infos(previous_total, previous_cores, current_total, current_cores, total_cpu, core_cpus)
    type(cpu_state_ticks), intent(in) :: previous_total
    type(cpu_state_ticks), intent(in) :: previous_cores(:)
    type(cpu_state_ticks), intent(in) :: current_total
    type(cpu_state_ticks), intent(in) :: current_cores(:)
    type(cpu_core_info), intent(out) :: total_cpu
    type(cpu_core_info), allocatable, intent(out) :: core_cpus(:)
    integer :: core_count
    integer :: core_index

    total_cpu = cpu_state_delta_info(previous_total, current_total)
    core_count = bounded_core_count(size(current_cores))
    allocate(core_cpus(core_count))
    do core_index = 1, min(core_count, size(previous_cores))
      core_cpus(core_index) = cpu_state_delta_info(previous_cores(core_index), current_cores(core_index))
    end do
  end subroutine make_cpu_delta_infos

  subroutine merge_cpu_metadata(core_cpus, metadata)
    type(cpu_core_info), intent(inout) :: core_cpus(:)
    type(cpu_core_info), intent(in) :: metadata(:)
    integer :: core_index

    do core_index = 1, min(size(core_cpus), size(metadata))
      core_cpus(core_index)%freq_valid = metadata(core_index)%freq_valid
      core_cpus(core_index)%freq_mhz = metadata(core_index)%freq_mhz
      core_cpus(core_index)%temp_valid = metadata(core_index)%temp_valid
      core_cpus(core_index)%temp_c = metadata(core_index)%temp_c
    end do
  end subroutine merge_cpu_metadata

  subroutine clear_state(state, interval_ms)
    type(collector_shared_state), intent(out) :: state
    integer, intent(in) :: interval_ms

    state%stop_requested = 0_c_int
    state%running = 0_c_int
    state%warming_up = 1_c_int
    state%sample_count = 0_c_int
    state%interval_ms = int(interval_ms, c_int)
    state%cpu_count = 0_c_int
    state%cpu_physical_core_count = 0_c_int
    state%cpu_valid = 0_c_int
    state%cpu_usage_percent = 0.0_c_double
    state%cpu_user_percent = 0.0_c_double
    state%cpu_system_percent = 0.0_c_double
    state%cpu_iowait_percent = 0.0_c_double
    state%cpu_core_count = 0_c_int
    state%cpu_core_valid = 0_c_int
    state%cpu_core_usage_percent = 0.0_c_double
    state%cpu_core_user_percent = 0.0_c_double
    state%cpu_core_system_percent = 0.0_c_double
    state%cpu_core_iowait_percent = 0.0_c_double
    state%cpu_core_freq_valid = 0_c_int
    state%cpu_core_freq_mhz = 0.0_c_double
    state%cpu_core_temp_valid = 0_c_int
    state%cpu_core_temp_c = 0.0_c_double
    state%load_valid = 0_c_int
    state%load_average = 0.0_c_double
    state%memory_valid = 0_c_int
    state%memory_total_bytes = 0_c_long_long
    state%memory_used_bytes = 0_c_long_long
    state%memory_free_bytes = 0_c_long_long
    state%memory_available_bytes = 0_c_long_long
    state%memory_cached_bytes = 0_c_long_long
    state%memory_buffers_bytes = 0_c_long_long
    state%memory_swap_total_bytes = 0_c_long_long
    state%memory_swap_used_bytes = 0_c_long_long
    state%history_start = 1_c_int
    state%history_count = 0_c_int
    state%cpu_usage_history = 0.0_c_double
    state%cpu_core_usage_history = 0.0_c_double
    state%memory_usage_history = 0.0_c_double
    state%mutex = c_null_ptr
  end subroutine clear_state

  subroutine clear_runtime_state(state)
    type(collector_shared_state), intent(inout) :: state

    state%stop_requested = 0_c_int
    state%warming_up = 1_c_int
    state%sample_count = 0_c_int
    state%cpu_count = 0_c_int
    state%cpu_physical_core_count = 0_c_int
    state%cpu_valid = 0_c_int
    state%cpu_usage_percent = 0.0_c_double
    state%cpu_user_percent = 0.0_c_double
    state%cpu_system_percent = 0.0_c_double
    state%cpu_iowait_percent = 0.0_c_double
    state%cpu_core_count = 0_c_int
    state%cpu_core_valid = 0_c_int
    state%cpu_core_usage_percent = 0.0_c_double
    state%cpu_core_user_percent = 0.0_c_double
    state%cpu_core_system_percent = 0.0_c_double
    state%cpu_core_iowait_percent = 0.0_c_double
    state%cpu_core_freq_valid = 0_c_int
    state%cpu_core_freq_mhz = 0.0_c_double
    state%cpu_core_temp_valid = 0_c_int
    state%cpu_core_temp_c = 0.0_c_double
    state%load_valid = 0_c_int
    state%load_average = 0.0_c_double
    state%memory_valid = 0_c_int
    state%memory_total_bytes = 0_c_long_long
    state%memory_used_bytes = 0_c_long_long
    state%memory_free_bytes = 0_c_long_long
    state%memory_available_bytes = 0_c_long_long
    state%memory_cached_bytes = 0_c_long_long
    state%memory_buffers_bytes = 0_c_long_long
    state%memory_swap_total_bytes = 0_c_long_long
    state%memory_swap_used_bytes = 0_c_long_long
    state%history_start = 1_c_int
    state%history_count = 0_c_int
    state%cpu_usage_history = 0.0_c_double
    state%cpu_core_usage_history = 0.0_c_double
    state%memory_usage_history = 0.0_c_double
  end subroutine clear_runtime_state

  subroutine clear_snapshot(snapshot)
    type(collector_snapshot), intent(out) :: snapshot

    snapshot%running = .false.
    snapshot%warming_up = .true.
    snapshot%sample_count = 0
    snapshot%cpu_total%valid = .false.
    snapshot%cpu_total%usage_percent = 0.0_real64
    snapshot%cpu_total%user_percent = 0.0_real64
    snapshot%cpu_total%system_percent = 0.0_real64
    snapshot%cpu_total%iowait_percent = 0.0_real64
    snapshot%cpu_total%load_avg = 0.0_real64
    snapshot%cpu_total%load_valid = .false.
    snapshot%cpu_total%core_count = 0
    snapshot%cpu_total%thread_count = 0
    snapshot%memory%valid = .false.
    snapshot%memory%total_bytes = 0_int64
    snapshot%memory%used_bytes = 0_int64
    snapshot%memory%free_bytes = 0_int64
    snapshot%memory%available_bytes = 0_int64
    snapshot%memory%cached_bytes = 0_int64
    snapshot%memory%buffers_bytes = 0_int64
    snapshot%memory%swap_total_bytes = 0_int64
    snapshot%memory%swap_used_bytes = 0_int64
  end subroutine clear_snapshot

  logical function copy_cpu_cores(state, snapshot) result(success)
    type(collector_shared_state), intent(in) :: state
    type(collector_snapshot), intent(inout) :: snapshot
    integer :: allocation_status
    integer :: core_count
    integer :: core_index

    success = .false.
    core_count = bounded_core_count(int(state%cpu_core_count))
    allocate(snapshot%cpu_cores(core_count), stat=allocation_status)
    if (allocation_status /= 0) return

    do core_index = 1, core_count
      snapshot%cpu_cores(core_index)%valid = state%cpu_core_valid(core_index) /= 0_c_int
      snapshot%cpu_cores(core_index)%usage_percent = real(state%cpu_core_usage_percent(core_index), real64)
      snapshot%cpu_cores(core_index)%user_percent = real(state%cpu_core_user_percent(core_index), real64)
      snapshot%cpu_cores(core_index)%system_percent = real(state%cpu_core_system_percent(core_index), real64)
      snapshot%cpu_cores(core_index)%iowait_percent = real(state%cpu_core_iowait_percent(core_index), real64)
      snapshot%cpu_cores(core_index)%freq_valid = state%cpu_core_freq_valid(core_index) /= 0_c_int
      snapshot%cpu_cores(core_index)%freq_mhz = real(state%cpu_core_freq_mhz(core_index), real64)
      snapshot%cpu_cores(core_index)%temp_valid = state%cpu_core_temp_valid(core_index) /= 0_c_int
      snapshot%cpu_cores(core_index)%temp_c = real(state%cpu_core_temp_c(core_index), real64)
    end do

    success = .true.
  end function copy_cpu_cores

  logical function copy_history(state, snapshot) result(success)
    type(collector_shared_state), intent(in) :: state
    type(collector_snapshot), intent(inout) :: snapshot
    integer :: allocation_status
    integer :: core_count
    integer :: core_index
    integer :: item_index
    integer :: source_index
    integer :: history_count

    success = .false.
    history_count = max(0, min(int(state%history_count), FTOP_COLLECTOR_HISTORY_CAPACITY))
    core_count = bounded_core_count(int(state%cpu_core_count))
    allocate(snapshot%cpu_usage_history(history_count), stat=allocation_status)
    if (allocation_status /= 0) return
    allocate(snapshot%cpu_core_usage_history(core_count, history_count), stat=allocation_status)
    if (allocation_status /= 0) return
    allocate(snapshot%memory_usage_history(history_count), stat=allocation_status)
    if (allocation_status /= 0) return

    do item_index = 1, history_count
      source_index = history_index(state, item_index)
      snapshot%cpu_usage_history(item_index) = real(state%cpu_usage_history(source_index), real64)
      do core_index = 1, core_count
        snapshot%cpu_core_usage_history(core_index, item_index) = &
          real(state%cpu_core_usage_history(core_index, source_index), real64)
      end do
      snapshot%memory_usage_history(item_index) = real(state%memory_usage_history(source_index), real64)
    end do

    success = .true.
  end function copy_history

  integer function history_index(state, item_index) result(index)
    type(collector_shared_state), intent(in) :: state
    integer, intent(in) :: item_index

    index = modulo(int(state%history_start) + item_index - 2, FTOP_COLLECTOR_HISTORY_CAPACITY) + 1
  end function history_index

  integer function bounded_core_count(core_count) result(bounded)
    integer, intent(in) :: core_count

    bounded = max(0, min(FTOP_COLLECTOR_MAX_CPU_CORES, core_count))
  end function bounded_core_count

  integer function bounded_interval_ms(interval_ms) result(bounded)
    integer, intent(in), optional :: interval_ms

    bounded = FTOP_COLLECTOR_DEFAULT_INTERVAL_MS
    if (present(interval_ms)) bounded = interval_ms
    bounded = max(FTOP_COLLECTOR_MIN_INTERVAL_MS, min(FTOP_COLLECTOR_MAX_INTERVAL_MS, bounded))
  end function bounded_interval_ms

  logical function set_interval(self, interval_ms) result(success)
    class(collector), intent(inout) :: self
    integer, intent(in) :: interval_ms

    success = .false.
    if (.not. self%ready) return
    if (.not. ftop_mutex_lock(self%mutex)) return
    self%state%interval_ms = int(bounded_interval_ms(interval_ms), c_int)
    success = .true.
    if (.not. ftop_mutex_unlock(self%mutex)) success = .false.
  end function set_interval

  integer(c_long_long) function monotonic_ms() result(milliseconds)
    integer(c_int) :: rc
    integer(c_int) :: sys_errno

    rc = c_ftop_collector_monotonic_ms(milliseconds, sys_errno)
    if (rc /= 0_c_int) milliseconds = 0_c_long_long
  end function monotonic_ms

  integer(c_long_long) function interval_ms(state) result(interval)
    type(collector_shared_state), intent(in) :: state

    interval = int(max(FTOP_COLLECTOR_MIN_INTERVAL_MS, int(state%interval_ms)), c_long_long)
  end function interval_ms

  logical function sleep_until_or_stop(state, deadline_ms) result(stopped)
    type(collector_shared_state), intent(inout) :: state
    integer(c_long_long), intent(in) :: deadline_ms
    integer(c_long_long) :: now_ms
    integer(c_long_long) :: next_sleep_ms
    integer(c_int) :: rc
    integer(c_int) :: sys_errno

    stopped = .false.
    do
      if (should_stop(state)) then
        stopped = .true.
        return
      end if

      now_ms = monotonic_ms()
      if (now_ms >= deadline_ms) return
      next_sleep_ms = min(deadline_ms, now_ms + 10_c_long_long)
      rc = c_ftop_collector_sleep_until_ms(next_sleep_ms, sys_errno)
      if (rc /= 0_c_int) return
    end do
  end function sleep_until_or_stop

  logical function request_stop(state) result(success)
    type(collector_shared_state), intent(inout) :: state
    type(ftop_mutex_handle) :: mutex

    success = .false.
    mutex%handle = state%mutex
    if (.not. ftop_mutex_lock(mutex)) return
    state%stop_requested = 1_c_int
    success = .true.
    if (.not. ftop_mutex_unlock(mutex)) success = .false.
  end function request_stop

  logical function should_stop(state) result(stop_requested)
    type(collector_shared_state), intent(inout) :: state
    type(ftop_mutex_handle) :: mutex

    stop_requested = .true.
    mutex%handle = state%mutex
    if (.not. ftop_mutex_lock(mutex)) return
    stop_requested = state%stop_requested /= 0_c_int
    if (.not. ftop_mutex_unlock(mutex)) stop_requested = .true.
  end function should_stop

  subroutine publish_sample(state, topology, total_cpu, core_cpus, warming_up, memory, load_average)
    type(collector_shared_state), intent(inout) :: state
    type(cpu_topology_info), intent(in) :: topology
    type(cpu_core_info), intent(in) :: total_cpu
    type(cpu_core_info), intent(in) :: core_cpus(:)
    logical, intent(in) :: warming_up
    type(platform_memory_info), intent(in) :: memory
    type(load_average_info), intent(in) :: load_average
    type(ftop_mutex_handle) :: mutex
    integer :: core_count
    integer :: physical_core_count
    integer :: thread_count

    mutex%handle = state%mutex
    if (.not. ftop_mutex_lock(mutex)) return

    thread_count = max(0, topology%thread_count)
    if (.not. topology%valid .or. thread_count <= 0) thread_count = bounded_core_count(size(core_cpus))
    physical_core_count = max(0, topology%core_count)
    if (.not. topology%valid .or. physical_core_count <= 0) physical_core_count = thread_count
    if (thread_count > 0) physical_core_count = min(physical_core_count, thread_count)

    state%cpu_count = int(thread_count, c_int)
    state%cpu_physical_core_count = int(physical_core_count, c_int)
    state%cpu_valid = merge(1_c_int, 0_c_int, total_cpu%valid .and. .not. warming_up)
    state%cpu_usage_percent = real(clamp_percent(total_cpu%usage_percent), c_double)
    state%cpu_user_percent = real(clamp_percent(total_cpu%user_percent), c_double)
    state%cpu_system_percent = real(clamp_percent(total_cpu%system_percent), c_double)
    state%cpu_iowait_percent = real(clamp_percent(total_cpu%iowait_percent), c_double)
    core_count = bounded_core_count(size(core_cpus))
    state%cpu_core_count = int(core_count, c_int)
    call publish_cpu_cores(state, core_cpus, core_count, warming_up)
    state%load_valid = merge(1_c_int, 0_c_int, load_average%valid)
    state%load_average = real(max(0.0_real64, load_average%values), c_double)
    state%warming_up = merge(1_c_int, 0_c_int, warming_up)
    state%sample_count = state%sample_count + 1_c_int
    state%memory_valid = merge(1_c_int, 0_c_int, memory%valid)
    state%memory_total_bytes = int(max(0_int64, memory%total_bytes), c_long_long)
    state%memory_used_bytes = int(clamp_memory_value(memory%used_bytes, memory%total_bytes), c_long_long)
    state%memory_free_bytes = int(clamp_memory_value(memory%free_bytes, memory%total_bytes), c_long_long)
    state%memory_available_bytes = int(max(0_int64, min(memory%total_bytes, memory%available_bytes)), c_long_long)
    state%memory_cached_bytes = int(clamp_memory_value(memory%cached_bytes, memory%total_bytes), c_long_long)
    state%memory_buffers_bytes = int(clamp_memory_value(memory%buffers_bytes, memory%total_bytes), c_long_long)
    state%memory_swap_total_bytes = int(max(0_int64, memory%swap_total_bytes), c_long_long)
    state%memory_swap_used_bytes = int(clamp_memory_value(memory%swap_used_bytes, memory%swap_total_bytes), c_long_long)
    call append_history(state, real(state%cpu_usage_percent, real64), memory_usage_percent(memory), core_cpus, core_count)

    if (.not. ftop_mutex_unlock(mutex)) return
  end subroutine publish_sample

  subroutine publish_cpu_cores(state, core_cpus, core_count, warming_up)
    type(collector_shared_state), intent(inout) :: state
    type(cpu_core_info), intent(in) :: core_cpus(:)
    integer, intent(in) :: core_count
    logical, intent(in) :: warming_up
    integer :: core_index

    state%cpu_core_valid = 0_c_int
    state%cpu_core_usage_percent = 0.0_c_double
    state%cpu_core_user_percent = 0.0_c_double
    state%cpu_core_system_percent = 0.0_c_double
    state%cpu_core_iowait_percent = 0.0_c_double
    state%cpu_core_freq_valid = 0_c_int
    state%cpu_core_freq_mhz = 0.0_c_double
    state%cpu_core_temp_valid = 0_c_int
    state%cpu_core_temp_c = 0.0_c_double
    do core_index = 1, core_count
      state%cpu_core_valid(core_index) = merge(1_c_int, 0_c_int, core_cpus(core_index)%valid .and. .not. warming_up)
      state%cpu_core_usage_percent(core_index) = real(clamp_percent(core_cpus(core_index)%usage_percent), c_double)
      state%cpu_core_user_percent(core_index) = real(clamp_percent(core_cpus(core_index)%user_percent), c_double)
      state%cpu_core_system_percent(core_index) = real(clamp_percent(core_cpus(core_index)%system_percent), c_double)
      state%cpu_core_iowait_percent(core_index) = real(clamp_percent(core_cpus(core_index)%iowait_percent), c_double)
      state%cpu_core_freq_valid(core_index) = merge(1_c_int, 0_c_int, core_cpus(core_index)%freq_valid)
      state%cpu_core_freq_mhz(core_index) = real(max(0.0_real64, core_cpus(core_index)%freq_mhz), c_double)
      state%cpu_core_temp_valid(core_index) = merge(1_c_int, 0_c_int, core_cpus(core_index)%temp_valid)
      state%cpu_core_temp_c(core_index) = real(core_cpus(core_index)%temp_c, c_double)
    end do
  end subroutine publish_cpu_cores

  subroutine append_history(state, cpu_usage_percent, memory_usage_percent, core_cpus, core_count)
    type(collector_shared_state), intent(inout) :: state
    real(real64), intent(in) :: cpu_usage_percent
    real(real64), intent(in) :: memory_usage_percent
    type(cpu_core_info), intent(in) :: core_cpus(:)
    integer, intent(in) :: core_count
    integer :: core_index
    integer :: index

    if (state%history_count < FTOP_COLLECTOR_HISTORY_CAPACITY) then
      state%history_count = state%history_count + 1_c_int
      index = history_index(state, int(state%history_count))
    else
      index = int(state%history_start)
      state%history_start = int(modulo(int(state%history_start), FTOP_COLLECTOR_HISTORY_CAPACITY) + 1, c_int)
    end if

    state%cpu_usage_history(index) = real(clamp_percent(cpu_usage_percent), c_double)
    state%cpu_core_usage_history(:, index) = 0.0_c_double
    do core_index = 1, core_count
      state%cpu_core_usage_history(core_index, index) = real(clamp_percent(core_cpus(core_index)%usage_percent), c_double)
    end do
    state%memory_usage_history(index) = real(clamp_percent(memory_usage_percent), c_double)
  end subroutine append_history

  pure real(real64) function clamp_percent(value) result(clamped)
    real(real64), intent(in) :: value

    clamped = max(0.0_real64, min(100.0_real64, value))
  end function clamp_percent

  real(real64) function memory_usage_percent(memory) result(usage_percent)
    type(platform_memory_info), intent(in) :: memory
    integer(int64) :: used_bytes

    usage_percent = 0.0_real64
    if (.not. memory%valid) return
    if (memory%total_bytes <= 0_int64) return
    used_bytes = clamp_memory_value(memory%used_bytes, memory%total_bytes)
    usage_percent = 100.0_real64 * real(used_bytes, real64) / real(memory%total_bytes, real64)
    usage_percent = max(0.0_real64, min(100.0_real64, usage_percent))
  end function memory_usage_percent

  integer(int64) function clamp_memory_value(value, limit) result(clamped)
    integer(int64), intent(in) :: value
    integer(int64), intent(in) :: limit

    clamped = max(0_int64, min(max(0_int64, limit), value))
  end function clamp_memory_value

  subroutine ignore_mutex_unlock(mutex)
    type(ftop_mutex_handle), intent(in) :: mutex
    logical :: ignored

    ignored = ftop_mutex_unlock(mutex)
  end subroutine ignore_mutex_unlock

  subroutine mark_not_running(state)
    type(collector_shared_state), intent(inout) :: state
    type(ftop_mutex_handle) :: mutex

    mutex%handle = state%mutex
    if (.not. ftop_mutex_lock(mutex)) return
    state%running = 0_c_int
    state%stop_requested = 0_c_int
    if (.not. ftop_mutex_unlock(mutex)) return
  end subroutine mark_not_running

end module ftop_collector
