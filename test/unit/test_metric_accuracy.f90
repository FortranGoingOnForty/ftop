program test_metric_accuracy
  use, intrinsic :: iso_c_binding, only : c_double, c_int, c_long_long
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use ftop_platform, only : cpu_tick_sample, cpu_usage_percent, create_platform, memory_info, platform_backend
  use ftop_proc_data, only : assign_process_cpu_percent, process_lookup_index, process_table
  use ftop_signal, only : ftop_current_pid
  implicit none

  real(real64), parameter :: CPU_USAGE_TOLERANCE = 20.0_real64
  real(real64), parameter :: MEMORY_TOTAL_TOLERANCE = 2.0_real64
  real(real64), parameter :: MEMORY_USED_TOLERANCE = 15.0_real64
  real(real64), parameter :: PROCESS_CPU_PERCENT_TOLERANCE = 10.0_real64
  real(real64), parameter :: PROCESS_MEMORY_PERCENT_TOLERANCE = 2.0_real64
  integer(c_int), parameter :: BUSY_PROCESS_COUNT = 5_c_int
  real(real64), parameter :: BUSY_PROCESS_CPU_TOLERANCE = 8.0_real64
  real(real64), parameter :: BUSY_PROCESS_MEMORY_TOLERANCE = 1.0_real64

  interface
    integer(c_int) function c_ftop_accuracy_reference_cpu(usage_percent, sys_errno) &
        bind(C, name="ftop_accuracy_reference_cpu")
      import :: c_double, c_int
      real(c_double), intent(out) :: usage_percent
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_accuracy_reference_cpu

    integer(c_int) function c_ftop_accuracy_reference_memory(total_bytes, used_bytes, sys_errno) &
        bind(C, name="ftop_accuracy_reference_memory")
      import :: c_int, c_long_long
      integer(c_long_long), intent(out) :: total_bytes
      integer(c_long_long), intent(out) :: used_bytes
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_accuracy_reference_memory

    integer(c_int) function c_ftop_accuracy_memory_used_is_comparable() &
        bind(C, name="ftop_accuracy_memory_used_is_comparable")
      import :: c_int
    end function c_ftop_accuracy_memory_used_is_comparable

    integer(c_int) function c_ftop_accuracy_reference_process(pid, cpu_percent, mem_percent, sys_errno) &
        bind(C, name="ftop_accuracy_reference_process")
      import :: c_double, c_int
      integer(c_int), value :: pid
      real(c_double), intent(out) :: cpu_percent
      real(c_double), intent(out) :: mem_percent
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_accuracy_reference_process

    integer(c_int) function c_ftop_accuracy_reference_processes(pids, count, cpu_percents, mem_percents, &
                                                               matched_count, sys_errno) &
        bind(C, name="ftop_accuracy_reference_processes")
      import :: c_double, c_int
      integer(c_int), intent(in) :: pids(*)
      integer(c_int), value :: count
      real(c_double), intent(out) :: cpu_percents(*)
      real(c_double), intent(out) :: mem_percents(*)
      integer(c_int), intent(out) :: matched_count
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_accuracy_reference_processes

    integer(c_int) function c_ftop_process_stress_spawn_busy(requested, pids, capacity, spawned, sys_errno) &
        bind(C, name="ftop_process_stress_spawn_busy")
      import :: c_int
      integer(c_int), value :: requested
      integer(c_int), intent(inout) :: pids(*)
      integer(c_int), value :: capacity
      integer(c_int), intent(out) :: spawned
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_process_stress_spawn_busy

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

  backend = create_platform()
  if (.not. allocated(backend)) error stop "platform backend allocation failed"

  call test_cpu_accuracy(backend)
  call test_memory_accuracy(backend)
  call test_process_accuracy(backend)
  call test_busy_process_accuracy(backend)

contains

  subroutine test_cpu_accuracy(backend)
    class(platform_backend), intent(in) :: backend
    type(cpu_tick_sample) :: before
    type(cpu_tick_sample) :: after
    real(c_double) :: reference_usage
    real(real64) :: ftop_usage
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    before = backend%get_cpu_sample()
    if (.not. before%valid) error stop "accuracy CPU first sample invalid"

    rc = c_ftop_accuracy_reference_cpu(reference_usage, sys_errno)
    if (rc /= 0_c_int) error stop "accuracy reference CPU command failed"

    after = backend%get_cpu_sample()
    if (.not. after%valid) error stop "accuracy CPU second sample invalid"
    ftop_usage = cpu_usage_percent(before, after)

    call require_range(ftop_usage, 0.0_real64, 100.0_real64, "accuracy ftop CPU out of range")
    call require_range(real(reference_usage, real64), 0.0_real64, 100.0_real64, "accuracy reference CPU out of range")
    if (abs(ftop_usage - real(reference_usage, real64)) > CPU_USAGE_TOLERANCE) then
      error stop "accuracy CPU usage exceeded tolerance"
    end if
  end subroutine test_cpu_accuracy

  subroutine test_memory_accuracy(backend)
    class(platform_backend), intent(in) :: backend
    type(memory_info) :: memory
    integer(c_long_long) :: reference_total
    integer(c_long_long) :: reference_used
    integer(c_int) :: sys_errno
    integer(c_int) :: rc
    real(real64) :: ftop_used_percent
    real(real64) :: reference_used_percent

    memory = backend%get_memory_info()
    if (.not. memory%valid) error stop "accuracy ftop memory invalid"

    rc = c_ftop_accuracy_reference_memory(reference_total, reference_used, sys_errno)
    if (rc /= 0_c_int) error stop "accuracy reference memory command failed"
    if (reference_total <= 0_c_long_long) error stop "accuracy reference memory total invalid"
    if (reference_used < 0_c_long_long) error stop "accuracy reference memory used invalid"

    if (percent_difference(real(memory%total_bytes, real64), real(reference_total, real64)) > MEMORY_TOTAL_TOLERANCE) then
      error stop "accuracy memory total exceeded tolerance"
    end if

    ftop_used_percent = 100.0_real64 * real(memory%used_bytes, real64) / real(memory%total_bytes, real64)
    reference_used_percent = 100.0_real64 * real(reference_used, real64) / real(reference_total, real64)
    if (c_ftop_accuracy_memory_used_is_comparable() /= 0_c_int) then
      if (abs(ftop_used_percent - reference_used_percent) > MEMORY_USED_TOLERANCE) then
        error stop "accuracy memory used exceeded tolerance"
      end if
    end if
  end subroutine test_memory_accuracy

  subroutine test_process_accuracy(backend)
    class(platform_backend), intent(in) :: backend
    type(process_table) :: before
    type(process_table) :: after
    integer :: target_pid
    integer :: before_index
    integer :: after_index
    integer(int64) :: target_start_time
    integer(int64) :: elapsed_ms
    real(c_double) :: reference_cpu_percent
    real(c_double) :: reference_mem_percent
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    target_pid = ftop_current_pid()
    before = backend%get_process_table()
    if (.not. before%valid) error stop "accuracy process first table invalid"
    before_index = process_lookup_index(before, target_pid)
    if (before_index <= 0) error stop "accuracy process first sample missing target"
    target_start_time = before%items(before_index)%start_time

    call time_reference_process(target_pid, reference_cpu_percent, reference_mem_percent, elapsed_ms, rc, sys_errno)
    if (rc /= 0_c_int) error stop "accuracy reference process command failed"

    after = backend%get_process_table()
    if (.not. after%valid) error stop "accuracy process second table invalid"
    call assign_process_cpu_percent(after, before, elapsed_ms)
    after_index = process_lookup_index(after, target_pid, target_start_time)
    if (after_index <= 0) error stop "accuracy process second sample missing target"

    call require_range(real(reference_cpu_percent, real64), 0.0_real64, 10000.0_real64, &
                       "accuracy reference process CPU out of range")
    call require_range(real(reference_mem_percent, real64), 0.0_real64, 100.0_real64, &
                       "accuracy reference process memory out of range")
    if (abs(after%items(after_index)%cpu_percent - real(reference_cpu_percent, real64)) > &
        PROCESS_CPU_PERCENT_TOLERANCE) then
      error stop "accuracy process CPU exceeded tolerance"
    end if
    if (abs(after%items(after_index)%mem_percent - real(reference_mem_percent, real64)) > &
        PROCESS_MEMORY_PERCENT_TOLERANCE) then
      error stop "accuracy process memory exceeded tolerance"
    end if
  end subroutine test_process_accuracy

  subroutine test_busy_process_accuracy(backend)
    class(platform_backend), intent(in) :: backend
    type(process_table) :: before
    type(process_table) :: after
    integer(c_int) :: child_pids(BUSY_PROCESS_COUNT)
    integer(c_int) :: spawned
    integer(c_int) :: matched_count
    integer(c_int) :: sys_errno
    integer(c_int) :: rc
    integer(int64) :: elapsed_ms
    real(c_double) :: reference_cpu_percent(BUSY_PROCESS_COUNT)
    real(c_double) :: reference_mem_percent(BUSY_PROCESS_COUNT)

    child_pids = 0_c_int
    rc = c_ftop_process_stress_spawn_busy(BUSY_PROCESS_COUNT, child_pids, BUSY_PROCESS_COUNT, spawned, sys_errno)
    if (rc /= 0_c_int) call fail_busy_after_cleanup(child_pids, spawned, "accuracy busy process spawn failed")
    if (spawned /= BUSY_PROCESS_COUNT) then
      call fail_busy_after_cleanup(child_pids, spawned, "accuracy busy process spawn count mismatch")
    end if

    call c_ftop_process_stress_sleep_ms(2000_c_int)
    before = backend%get_process_table()
    if (.not. before%valid) call fail_busy_after_cleanup(child_pids, spawned, "accuracy busy process first table invalid")
    call require_busy_children_visible(before, child_pids, int(spawned), child_pids, spawned, &
                                       "accuracy busy process first sample missing target")

    call time_reference_processes(child_pids, spawned, reference_cpu_percent, reference_mem_percent, matched_count, &
                                  elapsed_ms, rc, sys_errno)
    if (rc /= 0_c_int) call fail_busy_after_cleanup(child_pids, spawned, "accuracy busy process reference failed")
    if (matched_count /= spawned) call fail_busy_after_cleanup(child_pids, spawned, "accuracy busy process match count failed")

    after = backend%get_process_table()
    if (.not. after%valid) call fail_busy_after_cleanup(child_pids, spawned, "accuracy busy process second table invalid")
    call assign_process_cpu_percent(after, before, elapsed_ms)
    call require_busy_accuracy(after, child_pids, int(spawned), reference_cpu_percent, reference_mem_percent, &
                               child_pids, spawned)

    rc = c_ftop_process_stress_cleanup(child_pids, spawned, sys_errno)
    if (rc /= 0_c_int) error stop "accuracy busy process cleanup failed"
  end subroutine test_busy_process_accuracy

  subroutine require_busy_children_visible(table, target_pids, target_count, child_pids, child_count, message)
    type(process_table), intent(in) :: table
    integer(c_int), intent(in) :: target_pids(:)
    integer, intent(in) :: target_count
    integer(c_int), intent(in) :: child_pids(:)
    integer(c_int), intent(in) :: child_count
    character(len=*), intent(in) :: message
    integer :: target_index

    do target_index = 1, target_count
      if (process_lookup_index(table, int(target_pids(target_index))) <= 0) then
        call fail_busy_after_cleanup(child_pids, child_count, message)
      end if
    end do
  end subroutine require_busy_children_visible

  subroutine require_busy_accuracy(table, target_pids, target_count, reference_cpu_percent, reference_mem_percent, &
                                   child_pids, child_count)
    type(process_table), intent(in) :: table
    integer(c_int), intent(in) :: target_pids(:)
    integer, intent(in) :: target_count
    real(c_double), intent(in) :: reference_cpu_percent(:)
    real(c_double), intent(in) :: reference_mem_percent(:)
    integer(c_int), intent(in) :: child_pids(:)
    integer(c_int), intent(in) :: child_count
    integer :: process_index
    integer :: target_index
    character(len=256) :: message

    do target_index = 1, target_count
      process_index = process_lookup_index(table, int(target_pids(target_index)))
      if (process_index <= 0) then
        call fail_busy_after_cleanup(child_pids, child_count, "accuracy busy process second sample missing target")
      end if

      call require_busy_range(real(reference_cpu_percent(target_index), real64), 0.0_real64, 10000.0_real64, &
                              child_pids, child_count, "accuracy busy process reference CPU out of range")
      call require_busy_range(real(reference_mem_percent(target_index), real64), 0.0_real64, 100.0_real64, &
                              child_pids, child_count, "accuracy busy process reference memory out of range")
      if (abs(table%items(process_index)%cpu_percent - real(reference_cpu_percent(target_index), real64)) > &
          BUSY_PROCESS_CPU_TOLERANCE) then
        write(message, '(a,i0,a,f0.2,a,f0.2)') "accuracy busy process CPU exceeded tolerance pid ", &
          int(target_pids(target_index)), " ftop ", table%items(process_index)%cpu_percent, " ref ", &
          real(reference_cpu_percent(target_index), real64)
        call fail_busy_after_cleanup(child_pids, child_count, trim(message))
      end if
      if (abs(table%items(process_index)%mem_percent - real(reference_mem_percent(target_index), real64)) > &
          BUSY_PROCESS_MEMORY_TOLERANCE) then
        write(message, '(a,i0,a,f0.2,a,f0.2)') "accuracy busy process memory exceeded tolerance pid ", &
          int(target_pids(target_index)), " ftop ", table%items(process_index)%mem_percent, " ref ", &
          real(reference_mem_percent(target_index), real64)
        call fail_busy_after_cleanup(child_pids, child_count, trim(message))
      end if
    end do
  end subroutine require_busy_accuracy

  subroutine time_reference_process(pid, cpu_percent, mem_percent, elapsed_ms, rc, sys_errno)
    integer, intent(in) :: pid
    real(c_double), intent(out) :: cpu_percent
    real(c_double), intent(out) :: mem_percent
    integer(int64), intent(out) :: elapsed_ms
    integer(c_int), intent(out) :: rc
    integer(c_int), intent(out) :: sys_errno
    integer(int64) :: count_rate
    integer(int64) :: end_count
    integer(int64) :: start_count

    call system_clock(start_count, count_rate)
    rc = c_ftop_accuracy_reference_process(int(pid, c_int), cpu_percent, mem_percent, sys_errno)
    call system_clock(end_count)
    if (count_rate <= 0_int64) then
      elapsed_ms = 0_int64
    else
      elapsed_ms = int(1000.0_real64 * real(max(0_int64, end_count - start_count), real64) / &
                       real(count_rate, real64), int64)
    end if
    if (elapsed_ms <= 0_int64) elapsed_ms = 1_int64
  end subroutine time_reference_process

  subroutine time_reference_processes(pids, pid_count, cpu_percents, mem_percents, matched_count, elapsed_ms, rc, &
                                      sys_errno)
    integer(c_int), intent(in) :: pids(*)
    integer(c_int), intent(in) :: pid_count
    real(c_double), intent(out) :: cpu_percents(*)
    real(c_double), intent(out) :: mem_percents(*)
    integer(c_int), intent(out) :: matched_count
    integer(int64), intent(out) :: elapsed_ms
    integer(c_int), intent(out) :: rc
    integer(c_int), intent(out) :: sys_errno
    integer(int64) :: count_rate
    integer(int64) :: end_count
    integer(int64) :: start_count

    call system_clock(start_count, count_rate)
    rc = c_ftop_accuracy_reference_processes(pids, pid_count, cpu_percents, mem_percents, matched_count, sys_errno)
    call system_clock(end_count)
    if (count_rate <= 0_int64) then
      elapsed_ms = 0_int64
    else
      elapsed_ms = int(1000.0_real64 * real(max(0_int64, end_count - start_count), real64) / &
                       real(count_rate, real64), int64)
    end if
    if (elapsed_ms <= 0_int64) elapsed_ms = 1_int64
  end subroutine time_reference_processes

  real(real64) function percent_difference(left, right) result(percent)
    real(real64), intent(in) :: left
    real(real64), intent(in) :: right
    real(real64) :: denominator

    denominator = max(abs(left), abs(right))
    if (denominator <= 0.0_real64) then
      percent = 0.0_real64
    else
      percent = 100.0_real64 * abs(left - right) / denominator
    end if
  end function percent_difference

  subroutine require_range(value, low, high, message)
    real(real64), intent(in) :: value
    real(real64), intent(in) :: low
    real(real64), intent(in) :: high
    character(len=*), intent(in) :: message

    if (value < low .or. value > high) error stop message
  end subroutine require_range

  subroutine require_busy_range(value, low, high, child_pids, child_count, message)
    real(real64), intent(in) :: value
    real(real64), intent(in) :: low
    real(real64), intent(in) :: high
    integer(c_int), intent(in) :: child_pids(:)
    integer(c_int), intent(in) :: child_count
    character(len=*), intent(in) :: message

    if (value < low .or. value > high) call fail_busy_after_cleanup(child_pids, child_count, message)
  end subroutine require_busy_range

  subroutine fail_busy_after_cleanup(child_pids, child_count, message)
    integer(c_int), intent(in) :: child_pids(:)
    integer(c_int), intent(in) :: child_count
    character(len=*), intent(in) :: message
    integer(c_int) :: cleanup_errno
    integer(c_int) :: cleanup_rc

    if (child_count > 0_c_int) then
      cleanup_rc = c_ftop_process_stress_cleanup(child_pids, child_count, cleanup_errno)
      if (cleanup_rc /= 0_c_int) error stop "accuracy busy process cleanup failed after test failure"
    end if
    error stop message
  end subroutine fail_busy_after_cleanup

end program test_metric_accuracy
