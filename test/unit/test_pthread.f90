module test_pthread_worker
  use, intrinsic :: iso_c_binding, only : c_f_pointer, c_int, c_null_ptr, c_ptr
  use ftop_pthread, only : &
    ftop_cond_handle, &
    ftop_cond_signal, &
    ftop_mutex_handle, &
    ftop_mutex_lock, &
    ftop_mutex_unlock
  implicit none
  private

  type, bind(C), public :: worker_state
    integer(c_int) :: ready
    type(c_ptr) :: mutex
    type(c_ptr) :: cond
  end type worker_state

  public :: signal_worker

contains

  function signal_worker(arg) bind(C) result(result)
    type(c_ptr), value :: arg
    type(c_ptr) :: result
    type(worker_state), pointer :: state
    type(ftop_mutex_handle) :: mutex
    type(ftop_cond_handle) :: cond

    result = c_null_ptr
    call c_f_pointer(arg, state)
    if (.not. associated(state)) return

    mutex%handle = state%mutex
    cond%handle = state%cond
    if (.not. ftop_mutex_lock(mutex)) return

    state%ready = 1_c_int
    if (.not. ftop_cond_signal(cond)) state%ready = -1_c_int
    if (.not. ftop_mutex_unlock(mutex)) state%ready = -2_c_int
  end function signal_worker

end module test_pthread_worker

program test_pthread
  use, intrinsic :: iso_c_binding, only : c_funloc, c_loc
  use ftop_pthread, only : &
    ftop_cond_broadcast, &
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
  use test_pthread_worker, only : signal_worker, worker_state
  implicit none

  type(ftop_thread_handle) :: thread
  type(ftop_mutex_handle) :: mutex
  type(ftop_cond_handle) :: cond
  type(worker_state), target :: state

  state%ready = 0
  call require(ftop_mutex_init(mutex), "mutex init failed")
  call require(ftop_cond_init(cond), "cond init failed")
  state%mutex = mutex%handle
  state%cond = cond%handle

  call require(ftop_mutex_lock(mutex), "main mutex lock failed")
  call require(ftop_thread_create(thread, c_funloc(signal_worker), c_loc(state)), "thread create failed")

  do while (state%ready == 0)
    call require(ftop_cond_wait(cond, mutex), "cond wait failed")
  end do

  call require(state%ready == 1, "worker did not signal readiness")
  call require(ftop_mutex_unlock(mutex), "main mutex unlock failed")
  call require(ftop_thread_join(thread), "thread join failed")
  call require(ftop_cond_broadcast(cond), "cond broadcast failed")
  call require(ftop_cond_destroy(cond), "cond destroy failed")
  call require(ftop_mutex_destroy(mutex), "mutex destroy failed")

contains

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

end program test_pthread
