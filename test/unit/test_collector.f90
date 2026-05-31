program test_collector
  use, intrinsic :: iso_fortran_env, only : real64
  use ftop_collector, only : collector, collector_snapshot
  implicit none

  call test_collector_lifecycle()
  call test_collector_restart()

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

    call require(metrics%start(10), "collector initial start failed")
    snapshot = wait_for_snapshot(metrics)
    call require(snapshot%sample_count > 0, "collector initial run did not publish")
    call require(metrics%stop(), "collector initial stop failed")
    call require(.not. metrics%running(), "collector still running after initial stop")

    call require(metrics%start(10), "collector restart failed")
    snapshot = wait_for_snapshot(metrics)
    call require(snapshot%sample_count > 0, "collector restarted run did not publish")
    call require(metrics%stop(), "collector restarted stop failed")
    call require(metrics%destroy(), "collector restarted destroy failed")
  end subroutine test_collector_restart

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

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

end program test_collector
