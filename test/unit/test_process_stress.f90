program test_process_stress
  use, intrinsic :: iso_c_binding, only : c_int
  use ftop_platform, only : create_platform, platform_backend
  use ftop_proc_data, only : process_lookup_index, process_table
  implicit none

  integer(c_int), parameter :: REQUESTED_CHILDREN = 1000_c_int
  integer, parameter :: VISIBILITY_TIMEOUT_MS = 5000
  integer, parameter :: ABSENCE_TIMEOUT_MS = 5000

  interface
    integer(c_int) function c_ftop_process_stress_spawn(requested, pids, capacity, spawned, sys_errno) &
        bind(C, name="ftop_process_stress_spawn")
      import :: c_int
      integer(c_int), value :: requested
      integer(c_int), intent(inout) :: pids(*)
      integer(c_int), value :: capacity
      integer(c_int), intent(out) :: spawned
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_process_stress_spawn

    integer(c_int) function c_ftop_process_stress_cleanup(pids, count, sys_errno) &
        bind(C, name="ftop_process_stress_cleanup")
      import :: c_int
      integer(c_int), intent(in) :: pids(*)
      integer(c_int), value :: count
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_process_stress_cleanup

    subroutine c_ftop_process_stress_sleep_ms(milliseconds) bind(C, name="ftop_process_stress_sleep_ms")
      import :: c_int
      integer(c_int), value :: milliseconds
    end subroutine c_ftop_process_stress_sleep_ms
  end interface

  class(platform_backend), allocatable :: backend
  type(process_table) :: baseline
  integer(c_int) :: child_pids(REQUESTED_CHILDREN)
  integer(c_int) :: rc
  integer(c_int) :: spawned
  integer(c_int) :: sys_errno
  integer :: found_count

  backend = create_platform()
  if (.not. allocated(backend)) error stop "process stress platform allocation failed"

  baseline = backend%get_process_table()
  call require(baseline%valid, "process stress baseline snapshot must be valid")
  call require(allocated(baseline%items), "process stress baseline items must be allocated")

  child_pids = 0_c_int
  rc = c_ftop_process_stress_spawn(REQUESTED_CHILDREN, child_pids, REQUESTED_CHILDREN, spawned, sys_errno)
  if (rc /= 0_c_int) call fail_after_cleanup(child_pids, spawned, "process stress spawn failed")
  if (spawned /= REQUESTED_CHILDREN) then
    call fail_after_cleanup(child_pids, spawned, "process stress did not spawn requested child count")
  end if

  found_count = wait_for_child_count(backend, child_pids, int(spawned), int(spawned), VISIBILITY_TIMEOUT_MS)
  if (found_count /= int(spawned)) then
    call fail_after_cleanup(child_pids, spawned, "process stress snapshot did not include every child")
  end if

  rc = c_ftop_process_stress_cleanup(child_pids, spawned, sys_errno)
  if (rc /= 0_c_int) error stop "process stress cleanup failed"

  found_count = wait_for_child_count(backend, child_pids, int(spawned), 0, ABSENCE_TIMEOUT_MS)
  call require(found_count == 0, "process stress children should disappear after cleanup")

contains

  integer function wait_for_child_count(backend, pids, pid_count, target_count, timeout_ms) result(found_count)
    class(platform_backend), intent(in) :: backend
    integer(c_int), intent(in) :: pids(:)
    integer, intent(in) :: pid_count
    integer, intent(in) :: target_count
    integer, intent(in) :: timeout_ms
    type(process_table) :: snapshot
    integer :: start_count
    integer :: current_count
    integer :: rate

    found_count = 0
    call system_clock(start_count, rate)
    do
      snapshot = backend%get_process_table()
      if (snapshot%valid .and. allocated(snapshot%items)) then
        found_count = count_child_processes(snapshot, pids, pid_count)
        if (found_count == target_count) return
      end if

      call system_clock(current_count)
      if (elapsed_ms(start_count, current_count, rate) >= timeout_ms) return
      call c_ftop_process_stress_sleep_ms(50_c_int)
    end do
  end function wait_for_child_count

  integer function count_child_processes(snapshot, pids, pid_count) result(found_count)
    type(process_table), intent(in) :: snapshot
    integer(c_int), intent(in) :: pids(:)
    integer, intent(in) :: pid_count
    integer :: pid_index

    found_count = 0
    do pid_index = 1, pid_count
      if (pids(pid_index) <= 0_c_int) cycle
      if (process_lookup_index(snapshot, int(pids(pid_index))) > 0) found_count = found_count + 1
    end do
  end function count_child_processes

  integer function elapsed_ms(start_count, current_count, rate) result(milliseconds)
    integer, intent(in) :: start_count
    integer, intent(in) :: current_count
    integer, intent(in) :: rate

    if (rate <= 0) then
      milliseconds = 0
    else
      milliseconds = int(1000.0 * real(max(0, current_count - start_count)) / real(rate))
    end if
  end function elapsed_ms

  subroutine fail_after_cleanup(pids, count, message)
    integer(c_int), intent(in) :: pids(:)
    integer(c_int), intent(in) :: count
    character(len=*), intent(in) :: message
    integer(c_int) :: cleanup_errno
    integer(c_int) :: cleanup_rc

    if (count > 0_c_int) then
      cleanup_rc = c_ftop_process_stress_cleanup(pids, count, cleanup_errno)
      if (cleanup_rc /= 0_c_int) error stop "process stress cleanup failed after test failure"
    end if
    error stop message
  end subroutine fail_after_cleanup

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

end program test_process_stress
