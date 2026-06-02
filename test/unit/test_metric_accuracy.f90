program test_metric_accuracy
  use, intrinsic :: iso_c_binding, only : c_double, c_int, c_long_long
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use ftop_platform, only : cpu_tick_sample, cpu_usage_percent, create_platform, memory_info, platform_backend
  use ftop_proc_data, only : assign_process_cpu_percent, process_lookup_index, process_table
  use ftop_signal, only : ftop_current_pid
  implicit none

  real(real64), parameter :: CPU_USAGE_TOLERANCE = 20.0_real64
  real(real64), parameter :: MEMORY_TOTAL_TOLERANCE = 2.0_real64
  real(real64), parameter :: MEMORY_USED_TOLERANCE = 8.0_real64
  real(real64), parameter :: PROCESS_PERCENT_TOLERANCE = 2.0_real64

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

    integer(c_int) function c_ftop_accuracy_reference_process(pid, cpu_percent, mem_percent, sys_errno) &
        bind(C, name="ftop_accuracy_reference_process")
      import :: c_double, c_int
      integer(c_int), value :: pid
      real(c_double), intent(out) :: cpu_percent
      real(c_double), intent(out) :: mem_percent
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_accuracy_reference_process
  end interface

  class(platform_backend), allocatable :: backend

  backend = create_platform()
  if (.not. allocated(backend)) error stop "platform backend allocation failed"

  call test_cpu_accuracy(backend)
  call test_memory_accuracy(backend)
  call test_process_accuracy(backend)

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
    if (abs(ftop_used_percent - reference_used_percent) > MEMORY_USED_TOLERANCE) then
      error stop "accuracy memory used exceeded tolerance"
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
    if (abs(after%items(after_index)%cpu_percent - real(reference_cpu_percent, real64)) > PROCESS_PERCENT_TOLERANCE) then
      error stop "accuracy process CPU exceeded tolerance"
    end if
    if (abs(after%items(after_index)%mem_percent - real(reference_mem_percent, real64)) > PROCESS_PERCENT_TOLERANCE) then
      error stop "accuracy process memory exceeded tolerance"
    end if
  end subroutine test_process_accuracy

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

end program test_metric_accuracy
