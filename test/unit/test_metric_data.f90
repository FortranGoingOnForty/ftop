program test_metric_data
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use ftop_cpu_data, only : cpu_core_info, cpu_history, cpu_total_info
  use ftop_mem_data, only : memory_history, memory_info, memory_usage_percent
  implicit none

  call test_cpu_info_defaults()
  call test_cpu_history()
  call test_memory_usage_percent()
  call test_memory_history()

contains

  subroutine test_cpu_info_defaults()
    type(cpu_core_info) :: core
    type(cpu_total_info) :: total

    call require(.not. core%valid, "CPU core must default invalid")
    call require_close(core%usage_percent, 0.0_real64, "CPU core usage default mismatch")
    call require_close(core%user_percent, 0.0_real64, "CPU core user default mismatch")
    call require_close(core%system_percent, 0.0_real64, "CPU core system default mismatch")
    call require_close(core%iowait_percent, 0.0_real64, "CPU core iowait default mismatch")
    call require(.not. core%freq_valid, "CPU core frequency must default invalid")
    call require(.not. core%temp_valid, "CPU core temperature must default invalid")

    call require(.not. total%valid, "CPU total must default invalid")
    call require_close(total%usage_percent, 0.0_real64, "CPU total usage default mismatch")
    call require_close(total%user_percent, 0.0_real64, "CPU total user default mismatch")
    call require_close(total%system_percent, 0.0_real64, "CPU total system default mismatch")
    call require_close(total%iowait_percent, 0.0_real64, "CPU total iowait default mismatch")
    call require_close(total%load_avg(1), 0.0_real64, "CPU load average default mismatch")
    call require(.not. total%load_valid, "CPU load must default invalid")
    call require(total%core_count == 0, "CPU total core count default mismatch")
    call require(total%thread_count == 0, "CPU total thread count default mismatch")
    call require(.not. total%model_name_valid, "CPU model name must default invalid")
    call require(len_trim(total%model_name) == 0, "CPU model name must default empty")
  end subroutine test_cpu_info_defaults

  subroutine test_cpu_history()
    type(cpu_history) :: history
    real(real64), allocatable :: samples(:)

    call require(.not. history%initialized(), "CPU history must start uninitialized")
    call require(.not. history%init(-1, 3), "negative core count must fail")
    call require(history%init(2, 3), "CPU history init failed")
    call require(history%initialized(), "CPU history must report initialized")
    call require(history%core_count() == 2, "CPU history core count mismatch")
    call require(history%capacity() == 3, "CPU history capacity mismatch")

    call require(history%push_total_usage(-1.0_real64), "CPU total low push failed")
    call require(history%push_total_usage(50.0_real64), "CPU total middle push failed")
    call require(history%push_total_usage(150.0_real64), "CPU total high push failed")
    call require(history%push_total_usage(75.0_real64), "CPU total wrap push failed")
    call require(history%snapshot_total_usage(samples), "CPU total snapshot failed")
    call require(size(samples) == 3, "CPU total snapshot size mismatch")
    call require_close(samples(1), 50.0_real64, "CPU total oldest sample mismatch")
    call require_close(samples(2), 100.0_real64, "CPU total clamped sample mismatch")
    call require_close(samples(3), 75.0_real64, "CPU total newest sample mismatch")

    call require(history%push_core_usage(1, 10.0_real64), "CPU core 1 push failed")
    call require(history%push_core_usage(2, 20.0_real64), "CPU core 2 push failed")
    call require(.not. history%push_core_usage(3, 30.0_real64), "out-of-range CPU core push must fail")
    call require(history%snapshot_core_usage(1, samples), "CPU core 1 snapshot failed")
    call require(size(samples) == 1, "CPU core 1 snapshot size mismatch")
    call require_close(samples(1), 10.0_real64, "CPU core 1 sample mismatch")
    call require(history%snapshot_core_usage(2, samples), "CPU core 2 snapshot failed")
    call require_close(samples(1), 20.0_real64, "CPU core 2 sample mismatch")
    call require(.not. history%snapshot_core_usage(0, samples), "out-of-range CPU core snapshot must fail")

    call require(history%destroy(), "CPU history destroy failed")
  end subroutine test_cpu_history

  subroutine test_memory_usage_percent()
    type(memory_info) :: info

    call require_close(memory_usage_percent(info), 0.0_real64, "invalid memory must have zero usage")

    info%valid = .true.
    info%total_bytes = 1000_int64
    info%used_bytes = 250_int64
    call require_close(memory_usage_percent(info), 25.0_real64, "memory usage percent mismatch")

    info%used_bytes = 1500_int64
    call require_close(memory_usage_percent(info), 100.0_real64, "memory usage percent must clamp high")

    info%used_bytes = -10_int64
    call require_close(memory_usage_percent(info), 0.0_real64, "memory usage percent must clamp low")
  end subroutine test_memory_usage_percent

  subroutine test_memory_history()
    type(memory_history) :: history
    type(memory_info) :: info
    real(real64), allocatable :: samples(:)

    call require(.not. history%initialized(), "memory history must start uninitialized")
    call require(history%init(2), "memory history init failed")
    call require(history%initialized(), "memory history must report initialized")
    call require(history%capacity() == 2, "memory history capacity mismatch")
    call require(history%size() == 0, "memory history must start empty")

    info%valid = .true.
    info%total_bytes = 1000_int64
    info%used_bytes = 250_int64
    call require(history%push_info(info), "memory info push failed")
    call require(history%push_usage(90.0_real64), "memory usage push failed")
    call require(history%push_usage(110.0_real64), "memory usage wrap push failed")
    call require(history%size() == 2, "memory history size mismatch")
    call require(history%snapshot_usage(samples), "memory history snapshot failed")
    call require(size(samples) == 2, "memory history snapshot size mismatch")
    call require_close(samples(1), 90.0_real64, "memory history oldest sample mismatch")
    call require_close(samples(2), 100.0_real64, "memory history newest sample mismatch")

    call require(history%destroy(), "memory history destroy failed")
  end subroutine test_memory_history

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

  subroutine require_close(actual, expected, message)
    real(real64), intent(in) :: actual
    real(real64), intent(in) :: expected
    character(len=*), intent(in) :: message

    if (abs(actual - expected) > 0.000000001_real64) error stop message
  end subroutine require_close

end program test_metric_data
