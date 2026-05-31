program test_collector
  use, intrinsic :: iso_fortran_env, only : real64
  use ftop_collector, only : FTOP_COLLECTOR_HISTORY_CAPACITY, collector, collector_snapshot
  implicit none

  call test_collector_lifecycle()
  call test_collector_restart()
  call test_collector_history_capacity()

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
    call require(snapshot%cpu_total%core_count > 0, "collector CPU count must be positive")
    call require(snapshot%memory%valid, "collector memory snapshot must be valid")
    call require(snapshot%memory%total_bytes > 0, "collector memory total must be positive")
    call require(snapshot%memory%available_bytes >= 0, "collector available memory must not be negative")
    call require(snapshot%memory%available_bytes <= snapshot%memory%total_bytes, &
                 "collector available memory must not exceed total")
    call require(allocated(snapshot%cpu_usage_history), "collector CPU history must be allocated")
    call require(allocated(snapshot%memory_usage_history), "collector memory history must be allocated")
    call require(size(snapshot%cpu_usage_history) == snapshot%sample_count, "collector CPU history size mismatch")
    call require(size(snapshot%memory_usage_history) == snapshot%sample_count, "collector memory history size mismatch")
    call require(all(snapshot%cpu_usage_history >= 0.0_real64), "collector CPU history must not be negative")
    call require(all(snapshot%cpu_usage_history <= 100.0_real64), "collector CPU history must not exceed 100")
    call require(all(snapshot%memory_usage_history >= 0.0_real64), "collector memory history must not be negative")
    call require(all(snapshot%memory_usage_history <= 100.0_real64), "collector memory history must not exceed 100")
    if (snapshot%cpu_total%valid) then
      call require(snapshot%cpu_total%usage_percent >= 0.0_real64, "collector CPU usage must not be negative")
      call require(snapshot%cpu_total%usage_percent <= 100.0_real64, "collector CPU usage must not exceed 100")
    end if

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
    call require(size(later_snapshot%memory_usage_history) > first_history_size, "collector memory history did not grow")
    call require(metrics%stop(), "collector initial stop failed")
    call require(.not. metrics%running(), "collector still running after initial stop")

    call require(metrics%start(10), "collector restart failed")
    snapshot = wait_for_snapshot(metrics)
    call require(snapshot%sample_count > 0, "collector restarted run did not publish")
    call require(size(snapshot%cpu_usage_history) == snapshot%sample_count, "collector CPU history did not reset on restart")
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
    call require(size(snapshot%memory_usage_history) == FTOP_COLLECTOR_HISTORY_CAPACITY, &
                 "collector memory history must cap at configured depth")
    call require(all(snapshot%cpu_usage_history >= 0.0_real64), "capped CPU history must not be negative")
    call require(all(snapshot%cpu_usage_history <= 100.0_real64), "capped CPU history must not exceed 100")
    call require(all(snapshot%memory_usage_history >= 0.0_real64), "capped memory history must not be negative")
    call require(all(snapshot%memory_usage_history <= 100.0_real64), "capped memory history must not exceed 100")
    call require(metrics%stop(), "collector capacity stop failed")
    call require(metrics%destroy(), "collector capacity destroy failed")
  end subroutine test_collector_history_capacity

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
      if (snapshot%memory%valid .and. snapshot%sample_count >= 2) return

      call system_clock(current_count)
      if (rate <= 0) return
      elapsed_ms = int((real(current_count - start_count) / real(rate)) * 1000.0)
      if (elapsed_ms > 2000) return
    end do
  end function wait_for_snapshot

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
      if (elapsed_ms > 4000) return
    end do
  end function wait_for_sample_count

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

end program test_collector
