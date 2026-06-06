program test_collector
  use, intrinsic :: iso_c_binding, only : c_int
  use, intrinsic :: iso_fortran_env, only : real64
  use ftop_collector, only : FTOP_COLLECTOR_HISTORY_CAPACITY, collector, collector_snapshot
  implicit none

  interface
    integer(c_int) function c_usleep(useconds) bind(C, name="usleep")
      import :: c_int
      integer(c_int), value :: useconds
    end function c_usleep
  end interface

  call test_collector_lifecycle()
  call test_collector_restart()
  call test_collector_history_capacity()
  call test_collector_clean_shutdown_deadline()
  call test_collector_rapid_start_stop()
  call test_collector_five_second_plausibility()

contains

  subroutine test_collector_lifecycle()
    type(collector) :: metrics
    type(collector_snapshot) :: snapshot

    call require(.not. metrics%initialized(), "collector must start uninitialized")
    call require(metrics%init(20), "collector init failed")
    call require(metrics%initialized(), "collector must report initialized")
    call require(metrics%start(), "collector start failed")
    call require(metrics%running(), "collector must report running after start")

    snapshot = wait_for_snapshot(metrics)
    call require(snapshot%sample_count > 0, "collector must publish at least one sample")
    call require(snapshot%cpu_total%core_count > 0, "collector CPU core count must be positive")
    call require(snapshot%cpu_total%thread_count > 0, "collector CPU thread count must be positive")
    call require(snapshot%cpu_total%core_count <= snapshot%cpu_total%thread_count, &
                 "collector CPU core count must not exceed threads")
    call require(snapshot%cpu_total%model_name_valid, "collector CPU model name must be valid")
    call require(len_trim(snapshot%cpu_total%model_name) > 0, "collector CPU model name must not be empty")
    call require(snapshot%cpu_total%valid, "collector CPU total must be valid")
    call require(allocated(snapshot%cpu_cores), "collector CPU cores must be allocated")
    call require(size(snapshot%cpu_cores) > 0, "collector must publish CPU cores")
    call require(all(snapshot%cpu_cores%valid), "collector CPU cores must be valid")
    call require(snapshot%memory%valid, "collector memory snapshot must be valid")
    call require(snapshot%memory%total_bytes > 0, "collector memory total must be positive")
    call require(snapshot%memory%used_bytes >= 0, "collector used memory must not be negative")
    call require(snapshot%memory%used_bytes <= snapshot%memory%total_bytes, "collector used memory must not exceed total")
    call require(snapshot%memory%free_bytes >= 0, "collector free memory must not be negative")
    call require(snapshot%memory%free_bytes <= snapshot%memory%total_bytes, "collector free memory must not exceed total")
    call require(snapshot%memory%available_bytes >= 0, "collector available memory must not be negative")
    call require(snapshot%memory%available_bytes <= snapshot%memory%total_bytes, &
                 "collector available memory must not exceed total")
    call require(snapshot%memory%cached_bytes >= 0, "collector cached memory must not be negative")
    call require(snapshot%memory%buffers_bytes >= 0, "collector buffer memory must not be negative")
    call require(snapshot%memory%swap_total_bytes >= 0, "collector swap total must not be negative")
    call require(snapshot%memory%swap_used_bytes >= 0, "collector swap used must not be negative")
    call require(snapshot%memory%swap_used_bytes <= snapshot%memory%swap_total_bytes, &
                 "collector swap used must not exceed swap total")
    call require(allocated(snapshot%cpu_usage_history), "collector CPU history must be allocated")
    call require(allocated(snapshot%cpu_core_usage_history), "collector CPU core history must be allocated")
    call require(allocated(snapshot%memory_usage_history), "collector memory history must be allocated")
    call require(size(snapshot%cpu_usage_history) == snapshot%sample_count, "collector CPU history size mismatch")
    call require(size(snapshot%cpu_core_usage_history, 1) == size(snapshot%cpu_cores), &
                 "collector CPU core history core size mismatch")
    call require(size(snapshot%cpu_core_usage_history, 2) == snapshot%sample_count, &
                 "collector CPU core history sample size mismatch")
    call require(size(snapshot%memory_usage_history) == snapshot%sample_count, "collector memory history size mismatch")
    call require(all(snapshot%cpu_usage_history >= 0.0_real64), "collector CPU history must not be negative")
    call require(all(snapshot%cpu_usage_history <= 100.0_real64), "collector CPU history must not exceed 100")
    call require(all(snapshot%cpu_core_usage_history >= 0.0_real64), "collector CPU core history must not be negative")
    call require(all(snapshot%cpu_core_usage_history <= 100.0_real64), "collector CPU core history must not exceed 100")
    call require(all(snapshot%memory_usage_history >= 0.0_real64), "collector memory history must not be negative")
    call require(all(snapshot%memory_usage_history <= 100.0_real64), "collector memory history must not exceed 100")
    call require(snapshot%cpu_total%usage_percent >= 0.0_real64, "collector CPU usage must not be negative")
    call require(snapshot%cpu_total%usage_percent <= 100.0_real64, "collector CPU usage must not exceed 100")
    call require(snapshot%cpu_total%user_percent >= 0.0_real64, "collector CPU user must not be negative")
    call require(snapshot%cpu_total%system_percent >= 0.0_real64, "collector CPU system must not be negative")
    call require(snapshot%cpu_total%iowait_percent >= 0.0_real64, "collector CPU iowait must not be negative")
    call require(all(snapshot%cpu_cores%usage_percent >= 0.0_real64), "collector core usage must not be negative")
    call require(all(snapshot%cpu_cores%usage_percent <= 100.0_real64), "collector core usage must not exceed 100")
    call require(all(snapshot%cpu_cores%user_percent >= 0.0_real64), "collector core user must not be negative")
    call require(all(snapshot%cpu_cores%system_percent >= 0.0_real64), "collector core system must not be negative")
    call require(all(snapshot%cpu_cores%iowait_percent >= 0.0_real64), "collector core iowait must not be negative")
    call require(all(.not. snapshot%cpu_cores%freq_valid .or. snapshot%cpu_cores%freq_mhz > 0.0_real64), &
                 "collector valid core frequency must be positive")
    call require(all(.not. snapshot%cpu_cores%temp_valid .or. snapshot%cpu_cores%temp_c > -100.0_real64), &
                 "collector valid core temperature must not be too low")
    call require(all(.not. snapshot%cpu_cores%temp_valid .or. snapshot%cpu_cores%temp_c < 150.0_real64), &
                 "collector valid core temperature must not be too high")
    call require(snapshot%cpu_total%load_valid, "collector load average must be valid")
    call require(all(snapshot%cpu_total%load_avg >= 0.0_real64), "collector load average must not be negative")
    call require(snapshot%system_uptime_valid, "collector uptime must be valid")
    call require(snapshot%system_uptime_seconds >= 0, "collector uptime must not be negative")
    call require(snapshot%processes%valid, "collector process table must be valid")
    call require(allocated(snapshot%processes%items), "collector process table must be allocated")
    call require(size(snapshot%processes%items) > 0, "collector process table must include processes")
    call require(any(snapshot%processes%items%valid), "collector process table must include valid processes")
    call require(any(snapshot%processes%items%pid > 0), "collector process table must include positive pids")
    call require(any(snapshot%processes%items%history_count > 0), "collector process histories must include samples")
    call require(all(snapshot%processes%items%history_count >= 0), "collector process history counts must not be negative")
    call require(all(snapshot%processes%items%history_count <= FTOP_COLLECTOR_HISTORY_CAPACITY), &
                 "collector process history counts must stay capped")
    call validate_network_snapshot(snapshot)
    call validate_disk_snapshot(snapshot)

    call require(metrics%stop(), "collector stop failed")
    call require(.not. metrics%running(), "collector must stop running")
    call require(metrics%destroy(), "collector destroy failed")
    call require(.not. metrics%initialized(), "collector must report destroyed")
  end subroutine test_collector_lifecycle

  subroutine test_collector_restart()
    type(collector) :: metrics
    type(collector_snapshot) :: snapshot
    type(collector_snapshot) :: later_snapshot
    integer :: first_history_size

    call require(metrics%start(10), "collector initial start failed")
    snapshot = wait_for_snapshot(metrics)
    call require(snapshot%sample_count > 0, "collector initial run did not publish")
    first_history_size = size(snapshot%cpu_usage_history)
    later_snapshot = wait_for_later_snapshot(metrics, snapshot%sample_count)
    call require(size(snapshot%cpu_usage_history) == first_history_size, "collector snapshot history must be deep copied")
    call require(later_snapshot%sample_count > snapshot%sample_count, "collector did not publish a later snapshot")
    call require(size(later_snapshot%cpu_usage_history) > first_history_size, "collector CPU history did not grow")
    call require(size(later_snapshot%cpu_core_usage_history, 2) > first_history_size, &
                 "collector CPU core history did not grow")
    call require(size(later_snapshot%memory_usage_history) > first_history_size, "collector memory history did not grow")
    call require(metrics%stop(), "collector initial stop failed")
    call require(.not. metrics%running(), "collector still running after initial stop")

    call require(metrics%start(10), "collector restart failed")
    snapshot = wait_for_snapshot(metrics)
    call require(snapshot%sample_count > 0, "collector restarted run did not publish")
    call require(size(snapshot%cpu_usage_history) == snapshot%sample_count, "collector CPU history did not reset on restart")
    call require(size(snapshot%cpu_core_usage_history, 2) == snapshot%sample_count, &
                 "collector CPU core history did not reset on restart")
    call require(size(snapshot%memory_usage_history) == snapshot%sample_count, "collector memory history did not reset on restart")
    call require(metrics%stop(), "collector restarted stop failed")
    call require(metrics%destroy(), "collector restarted destroy failed")
  end subroutine test_collector_restart

  subroutine test_collector_history_capacity()
    type(collector) :: metrics
    type(collector_snapshot) :: snapshot

    call require(metrics%start(1), "collector capacity start failed")
    snapshot = wait_for_sample_count(metrics, FTOP_COLLECTOR_HISTORY_CAPACITY + 5)
    call require(snapshot%sample_count >= FTOP_COLLECTOR_HISTORY_CAPACITY, "collector did not reach history capacity")
    call require(size(snapshot%cpu_usage_history) == FTOP_COLLECTOR_HISTORY_CAPACITY, &
                 "collector CPU history must cap at configured depth")
    call require(size(snapshot%cpu_core_usage_history, 2) == FTOP_COLLECTOR_HISTORY_CAPACITY, &
                 "collector CPU core history must cap at configured depth")
    call require(size(snapshot%memory_usage_history) == FTOP_COLLECTOR_HISTORY_CAPACITY, &
                 "collector memory history must cap at configured depth")
    call require(all(snapshot%cpu_usage_history >= 0.0_real64), "capped CPU history must not be negative")
    call require(all(snapshot%cpu_usage_history <= 100.0_real64), "capped CPU history must not exceed 100")
    call require(all(snapshot%cpu_core_usage_history >= 0.0_real64), "capped CPU core history must not be negative")
    call require(all(snapshot%cpu_core_usage_history <= 100.0_real64), "capped CPU core history must not exceed 100")
    call require(all(snapshot%memory_usage_history >= 0.0_real64), "capped memory history must not be negative")
    call require(all(snapshot%memory_usage_history <= 100.0_real64), "capped memory history must not exceed 100")
    call require(metrics%stop(), "collector capacity stop failed")
    call require(metrics%destroy(), "collector capacity destroy failed")
  end subroutine test_collector_history_capacity

  subroutine test_collector_clean_shutdown_deadline()
    type(collector) :: metrics
    integer :: start_count
    integer :: rate
    integer :: elapsed_ms

    call require(metrics%start(60000), "collector shutdown deadline start failed")
    call system_clock(start_count, rate)
    call require(metrics%stop(), "collector shutdown deadline stop failed")
    elapsed_ms = elapsed_milliseconds(start_count, rate)
    call require(elapsed_ms >= 0, "collector shutdown elapsed time failed")
    call require(elapsed_ms < 1000, "collector shutdown exceeded 1 second")
    call require(metrics%destroy(), "collector shutdown deadline destroy failed")
  end subroutine test_collector_clean_shutdown_deadline

  subroutine test_collector_rapid_start_stop()
    type(collector) :: metrics
    integer :: i

    do i = 1, 25
      call require(metrics%start(5), "collector rapid start failed")
      call require(metrics%running(), "collector rapid start must report running")
      call require(metrics%stop(), "collector rapid stop failed")
      call require(.not. metrics%running(), "collector rapid stop must report stopped")
    end do
    call require(metrics%destroy(), "collector rapid destroy failed")
  end subroutine test_collector_rapid_start_stop

  subroutine test_collector_five_second_plausibility()
    type(collector) :: metrics
    type(collector_snapshot) :: snapshot

    call require(metrics%start(50), "collector plausibility start failed")
    snapshot = wait_for_elapsed_snapshot(metrics, 5000)
    call require(snapshot%sample_count >= 5, "collector plausibility sample count too low")
    call require(snapshot%cpu_total%valid, "collector plausibility CPU total invalid")
    call require(snapshot%memory%valid, "collector plausibility memory invalid")
    call require(snapshot%memory%used_bytes > 0, "collector plausibility memory used must be positive")
    call require(size(snapshot%cpu_usage_history) >= 5, "collector plausibility CPU history too small")
    call require(any(snapshot%cpu_usage_history > 0.0_real64), "collector plausibility CPU history must be non-zero")
    call require(any(snapshot%cpu_core_usage_history > 0.0_real64), &
                 "collector plausibility CPU core history must be non-zero")
    call require(metrics%stop(), "collector plausibility stop failed")
    call require(metrics%destroy(), "collector plausibility destroy failed")
  end subroutine test_collector_five_second_plausibility

  function wait_for_snapshot(metrics) result(snapshot)
    type(collector), intent(in) :: metrics
    type(collector_snapshot) :: snapshot
    integer :: start_count
    integer :: current_count
    integer :: rate
    integer :: elapsed_ms

    call system_clock(start_count, rate)
    do
      snapshot = metrics%snapshot()
      if (snapshot%memory%valid .and. snapshot%cpu_total%valid .and. snapshot%sample_count >= 2) return

      call system_clock(current_count)
      if (rate <= 0) return
      elapsed_ms = int((real(current_count - start_count) / real(rate)) * 1000.0)
      if (elapsed_ms > 2000) return
      call wait_poll_interval()
    end do
  end function wait_for_snapshot

  subroutine validate_network_snapshot(snapshot)
    type(collector_snapshot), intent(in) :: snapshot
    integer :: interface_index
    integer :: process_index
    logical :: have_valid_interface

    call require(snapshot%network%valid, "collector network table must be valid")
    call require(allocated(snapshot%network%interfaces), "collector network interfaces must be allocated")
    call require(allocated(snapshot%network%connections), "collector network connections must be allocated")
    call require(allocated(snapshot%network%processes), "collector network processes must be allocated")
    call require(size(snapshot%network%interfaces) > 0, "collector network table must include interfaces")

    have_valid_interface = .false.
    do interface_index = 1, size(snapshot%network%interfaces)
      if (.not. snapshot%network%interfaces(interface_index)%valid) cycle
      have_valid_interface = .true.
      call require(len_trim(snapshot%network%interfaces(interface_index)%name) > 0, &
                   "collector network interface name must not be empty")
      call require(snapshot%network%interfaces(interface_index)%rx_bytes >= 0, &
                   "collector network rx bytes must not be negative")
      call require(snapshot%network%interfaces(interface_index)%tx_bytes >= 0, &
                   "collector network tx bytes must not be negative")
      call require(snapshot%network%interfaces(interface_index)%rx_bytes_per_sec >= 0.0_real64, &
                   "collector network rx rate must not be negative")
      call require(snapshot%network%interfaces(interface_index)%tx_bytes_per_sec >= 0.0_real64, &
                   "collector network tx rate must not be negative")
      call require(snapshot%network%interfaces(interface_index)%history_count > 0, &
                   "collector network histories must include samples")
      call require(snapshot%network%interfaces(interface_index)%history_count <= FTOP_COLLECTOR_HISTORY_CAPACITY, &
                   "collector network history counts must stay capped")
    end do
    call require(have_valid_interface, "collector network table must include valid interfaces")

    do process_index = 1, size(snapshot%network%processes)
      if (.not. snapshot%network%processes(process_index)%valid) cycle
      call require(snapshot%network%processes(process_index)%pid > 0, "network process pid must be positive")
      call require(snapshot%network%processes(process_index)%start_time > 0, &
                   "network process start time must be positive")
      call require(snapshot%network%processes(process_index)%rx_bytes >= 0, &
                   "network process rx bytes must not be negative")
      call require(snapshot%network%processes(process_index)%tx_bytes >= 0, &
                   "network process tx bytes must not be negative")
      call require(snapshot%network%processes(process_index)%rx_bytes_per_sec >= 0.0_real64, &
                   "network process rx rate must not be negative")
      call require(snapshot%network%processes(process_index)%tx_bytes_per_sec >= 0.0_real64, &
                   "network process tx rate must not be negative")
    end do
  end subroutine validate_network_snapshot

  subroutine validate_disk_snapshot(snapshot)
    type(collector_snapshot), intent(in) :: snapshot
    integer :: io_index

    call require(snapshot%disk%valid, "collector disk table must be valid")
    call require(allocated(snapshot%disk%filesystems), "collector disk filesystems must be allocated")
    call require(allocated(snapshot%disk%io), "collector disk IO must be allocated")
    call require(allocated(snapshot%disk%io_rates), "collector disk IO rates must be allocated")
    call require(allocated(snapshot%disk%latencies), "collector disk latencies must be allocated")
    call require(size(snapshot%disk%io_rates) == size(snapshot%disk%io), "collector disk IO rate size mismatch")
    call require(size(snapshot%disk%latencies) == size(snapshot%disk%io), "collector disk latency size mismatch")
    do io_index = 1, size(snapshot%disk%io)
      if (.not. snapshot%disk%io(io_index)%valid) cycle
      call require(len_trim(snapshot%disk%io(io_index)%device) > 0, "collector disk IO device must not be empty")
      call require(snapshot%disk%io(io_index)%read_bytes >= 0, "collector disk IO read bytes must not be negative")
      call require(snapshot%disk%io(io_index)%write_bytes >= 0, "collector disk IO write bytes must not be negative")
      call require(snapshot%disk%io(io_index)%read_ops >= 0, "collector disk IO read ops must not be negative")
      call require(snapshot%disk%io(io_index)%write_ops >= 0, "collector disk IO write ops must not be negative")
      call require(snapshot%disk%io(io_index)%sector_size_bytes > 0, "collector disk IO sector size must be positive")
      call require(trim(snapshot%disk%io_rates(io_index)%device) == trim(snapshot%disk%io(io_index)%device), &
                   "collector disk IO rate device mismatch")
      call require(trim(snapshot%disk%latencies(io_index)%device) == trim(snapshot%disk%io(io_index)%device), &
                   "collector disk latency device mismatch")
      if (snapshot%disk%io_rates(io_index)%valid) then
        call require(snapshot%disk%io_rates(io_index)%read_bytes_per_sec >= 0.0_real64, &
                     "collector disk read rate must not be negative")
        call require(snapshot%disk%io_rates(io_index)%write_bytes_per_sec >= 0.0_real64, &
                     "collector disk write rate must not be negative")
        call require(snapshot%disk%io_rates(io_index)%read_ops_per_sec >= 0.0_real64, &
                     "collector disk read op rate must not be negative")
        call require(snapshot%disk%io_rates(io_index)%write_ops_per_sec >= 0.0_real64, &
                     "collector disk write op rate must not be negative")
        call require(snapshot%disk%io_rates(io_index)%busy_percent >= 0.0_real64, &
                     "collector disk busy percent must not be negative")
        call require(snapshot%disk%io_rates(io_index)%busy_percent <= 100.0_real64, &
                     "collector disk busy percent must not exceed 100")
      end if
      if (snapshot%disk%latencies(io_index)%valid) then
        if (snapshot%disk%latencies(io_index)%read_valid) then
          call require(snapshot%disk%latencies(io_index)%avg_read_latency_us >= 0.0_real64, &
                       "collector disk read latency must not be negative")
        end if
        if (snapshot%disk%latencies(io_index)%write_valid) then
          call require(snapshot%disk%latencies(io_index)%avg_write_latency_us >= 0.0_real64, &
                       "collector disk write latency must not be negative")
        end if
        call require(snapshot%disk%latencies(io_index)%p99_latency_us >= 0.0_real64, &
                     "collector disk latency p99 proxy must not be negative")
      end if
    end do
  end subroutine validate_disk_snapshot

  function wait_for_later_snapshot(metrics, previous_sample_count) result(snapshot)
    type(collector), intent(in) :: metrics
    integer, intent(in) :: previous_sample_count
    type(collector_snapshot) :: snapshot
    integer :: start_count
    integer :: current_count
    integer :: rate
    integer :: elapsed_ms

    call system_clock(start_count, rate)
    do
      snapshot = metrics%snapshot()
      if (snapshot%sample_count > previous_sample_count) return

      call system_clock(current_count)
      if (rate <= 0) return
      elapsed_ms = int((real(current_count - start_count) / real(rate)) * 1000.0)
      if (elapsed_ms > 2000) return
      call wait_poll_interval()
    end do
  end function wait_for_later_snapshot

  function wait_for_sample_count(metrics, target_sample_count) result(snapshot)
    type(collector), intent(in) :: metrics
    integer, intent(in) :: target_sample_count
    type(collector_snapshot) :: snapshot
    integer :: start_count
    integer :: current_count
    integer :: rate
    integer :: elapsed_ms

    call system_clock(start_count, rate)
    do
      snapshot = metrics%snapshot()
      if (snapshot%sample_count >= target_sample_count) return

      call system_clock(current_count)
      if (rate <= 0) return
      elapsed_ms = int((real(current_count - start_count) / real(rate)) * 1000.0)
      if (elapsed_ms > 10000) return
      call wait_poll_interval()
    end do
  end function wait_for_sample_count

  function wait_for_elapsed_snapshot(metrics, target_elapsed_ms) result(snapshot)
    type(collector), intent(in) :: metrics
    integer, intent(in) :: target_elapsed_ms
    type(collector_snapshot) :: snapshot
    integer :: start_count
    integer :: current_count
    integer :: rate

    call system_clock(start_count, rate)
    do
      snapshot = metrics%snapshot()
      call system_clock(current_count)
      if (elapsed_milliseconds(start_count, rate, current_count) >= target_elapsed_ms) return
      if (rate <= 0) return
      call wait_poll_interval()
    end do
  end function wait_for_elapsed_snapshot

  subroutine wait_poll_interval()
    integer(c_int) :: rc

    rc = c_usleep(1000_c_int)
  end subroutine wait_poll_interval

  integer function elapsed_milliseconds(start_count, rate, current_count) result(elapsed_ms)
    integer, intent(in) :: start_count
    integer, intent(in) :: rate
    integer, intent(in), optional :: current_count
    integer :: end_count

    elapsed_ms = -1
    if (rate <= 0) return
    if (present(current_count)) then
      end_count = current_count
    else
      call system_clock(end_count)
    end if
    elapsed_ms = int((real(end_count - start_count) / real(rate)) * 1000.0)
  end function elapsed_milliseconds

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

end program test_collector
