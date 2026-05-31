module test_threaded_platform_worker
  use, intrinsic :: iso_c_binding, only : c_f_pointer, c_int, c_long_long, c_null_ptr, c_ptr
  use ftop_platform, only : cpu_tick_sample, create_platform, platform_backend
  use ftop_pthread, only : &
    ftop_cond_handle, &
    ftop_cond_signal, &
    ftop_mutex_handle, &
    ftop_mutex_lock, &
    ftop_mutex_unlock
  implicit none
  private

  type, bind(C), public :: collector_state
    integer(c_int) :: ready
    integer(c_int) :: cpu_count
    integer(c_int) :: sample_valid
    integer(c_long_long) :: total_ticks
    integer(c_long_long) :: idle_ticks
    type(c_ptr) :: mutex
    type(c_ptr) :: cond
  end type collector_state

  public :: collect_cpu_sample

contains

  function collect_cpu_sample(arg) bind(C) result(result)
    type(c_ptr), value :: arg
    type(c_ptr) :: result
    type(collector_state), pointer :: state
    class(platform_backend), allocatable :: backend
    type(cpu_tick_sample) :: sample
    type(ftop_mutex_handle) :: mutex
    type(ftop_cond_handle) :: cond

    result = c_null_ptr
    call c_f_pointer(arg, state)
    if (.not. associated(state)) return

    mutex%handle = state%mutex
    cond%handle = state%cond

    backend = create_platform()
    sample = backend%get_cpu_sample()

    if (.not. ftop_mutex_lock(mutex)) return
    state%cpu_count = int(backend%get_cpu_count(), c_int)
    state%sample_valid = merge(1_c_int, 0_c_int, sample%valid)
    state%total_ticks = int(sample%total, c_long_long)
    state%idle_ticks = int(sample%idle, c_long_long)
    state%ready = 1_c_int
    if (.not. ftop_cond_signal(cond)) state%ready = -1_c_int
    if (.not. ftop_mutex_unlock(mutex)) state%ready = -2_c_int
  end function collect_cpu_sample

end module test_threaded_platform_worker

program test_threaded_platform
  use, intrinsic :: iso_c_binding, only : c_funloc, c_loc, c_long_long
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use ftop_platform, only : cpu_tick_sample, cpu_usage_percent, create_platform, platform_backend
  use ftop_pthread, only : &
    ftop_cond_destroy, &
    ftop_cond_handle, &
    ftop_cond_init, &
    ftop_cond_wait, &
    ftop_mutex_destroy, &
    ftop_mutex_handle, &
    ftop_mutex_init, &
    ftop_mutex_lock, &
    ftop_mutex_unlock, &
    ftop_thread_create, &
    ftop_thread_handle, &
    ftop_thread_join
  use test_threaded_platform_worker, only : collect_cpu_sample, collector_state
  implicit none

  class(platform_backend), allocatable :: backend
  type(cpu_tick_sample) :: first_sample
  type(cpu_tick_sample) :: collected_sample
  type(ftop_thread_handle) :: thread
  type(ftop_mutex_handle) :: mutex
  type(ftop_cond_handle) :: cond
  type(collector_state), target :: state
  real(real64) :: usage

  backend = create_platform()
  first_sample = backend%get_cpu_sample()
  if (.not. first_sample%valid) error stop "initial CPU sample must be valid"
  call wait_for_tick_advance()

  state%ready = 0
  state%cpu_count = 0
  state%sample_valid = 0
  state%total_ticks = 0_c_long_long
  state%idle_ticks = 0_c_long_long
  call require(ftop_mutex_init(mutex), "mutex init failed")
  call require(ftop_cond_init(cond), "cond init failed")
  state%mutex = mutex%handle
  state%cond = cond%handle

  call require(ftop_mutex_lock(mutex), "main mutex lock failed")
  call require(ftop_thread_create(thread, c_funloc(collect_cpu_sample), c_loc(state)), "thread create failed")
  do while (state%ready == 0)
    call require(ftop_cond_wait(cond, mutex), "cond wait failed")
  end do

  call require(state%ready == 1, "collector failed before publishing data")
  call require(state%cpu_count > 0, "collector CPU count must be positive")
  call require(state%sample_valid == 1, "collector CPU sample must be valid")
  call require(state%total_ticks > int(first_sample%total, c_long_long), "collector total ticks did not advance")
  call require(ftop_mutex_unlock(mutex), "main mutex unlock failed")
  call require(ftop_thread_join(thread), "thread join failed")

  collected_sample%valid = .true.
  collected_sample%total = int(state%total_ticks, int64)
  collected_sample%idle = int(max(0_c_long_long, state%idle_ticks), int64)
  usage = cpu_usage_percent(first_sample, collected_sample)
  call require(usage >= 0.0_real64 .and. usage <= 100.0_real64, "threaded CPU usage must be in range")

  call require(ftop_cond_destroy(cond), "cond destroy failed")
  call require(ftop_mutex_destroy(mutex), "mutex destroy failed")

contains

  subroutine wait_for_tick_advance()
    type(cpu_tick_sample) :: sample
    integer :: start_count
    integer :: current_count
    integer :: rate
    integer :: elapsed_ms

    call system_clock(start_count, rate)
    do
      sample = backend%get_cpu_sample()
      if (sample%valid .and. sample%total > first_sample%total) return

      call system_clock(current_count)
      if (rate <= 0) return
      elapsed_ms = int((real(current_count - start_count) / real(rate)) * 1000.0)
      if (elapsed_ms > 1000) return
    end do
  end subroutine wait_for_tick_advance

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

end program test_threaded_platform
