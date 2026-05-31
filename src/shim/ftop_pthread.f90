module ftop_pthread
  use, intrinsic :: iso_c_binding, only : c_associated, c_funptr, c_int, c_null_ptr, c_ptr
  implicit none
  private

  integer, parameter, public :: FTOP_PTHREAD_OK = 0

  type, public :: ftop_thread_handle
    type(c_ptr) :: handle = c_null_ptr
  end type ftop_thread_handle

  type, public :: ftop_mutex_handle
    type(c_ptr) :: handle = c_null_ptr
  end type ftop_mutex_handle

  type, public :: ftop_cond_handle
    type(c_ptr) :: handle = c_null_ptr
  end type ftop_cond_handle

  abstract interface
    function ftop_thread_callback(arg) bind(C) result(result)
      import :: c_ptr
      type(c_ptr), value :: arg
      type(c_ptr) :: result
    end function ftop_thread_callback
  end interface

  public :: ftop_thread_callback
  public :: ftop_thread_create
  public :: ftop_thread_join
  public :: ftop_mutex_init
  public :: ftop_mutex_lock
  public :: ftop_mutex_unlock
  public :: ftop_mutex_destroy
  public :: ftop_cond_init
  public :: ftop_cond_wait
  public :: ftop_cond_signal
  public :: ftop_cond_broadcast
  public :: ftop_cond_destroy

  interface
    function c_ftop_thread_create(start_routine, arg, sys_errno) bind(C, name="ftop_thread_create")
      import :: c_funptr, c_int, c_ptr
      type(c_funptr), value :: start_routine
      type(c_ptr), value :: arg
      integer(c_int), intent(out) :: sys_errno
      type(c_ptr) :: c_ftop_thread_create
    end function c_ftop_thread_create

    integer(c_int) function c_ftop_thread_join(thread_handle, sys_errno) bind(C, name="ftop_thread_join")
      import :: c_int, c_ptr
      type(c_ptr), value :: thread_handle
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_thread_join

    function c_ftop_mutex_init(sys_errno) bind(C, name="ftop_mutex_init")
      import :: c_int, c_ptr
      integer(c_int), intent(out) :: sys_errno
      type(c_ptr) :: c_ftop_mutex_init
    end function c_ftop_mutex_init

    integer(c_int) function c_ftop_mutex_lock(mutex_handle, sys_errno) bind(C, name="ftop_mutex_lock")
      import :: c_int, c_ptr
      type(c_ptr), value :: mutex_handle
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_mutex_lock

    integer(c_int) function c_ftop_mutex_unlock(mutex_handle, sys_errno) bind(C, name="ftop_mutex_unlock")
      import :: c_int, c_ptr
      type(c_ptr), value :: mutex_handle
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_mutex_unlock

    integer(c_int) function c_ftop_mutex_destroy(mutex_handle, sys_errno) bind(C, name="ftop_mutex_destroy")
      import :: c_int, c_ptr
      type(c_ptr), value :: mutex_handle
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_mutex_destroy

    function c_ftop_cond_init(sys_errno) bind(C, name="ftop_cond_init")
      import :: c_int, c_ptr
      integer(c_int), intent(out) :: sys_errno
      type(c_ptr) :: c_ftop_cond_init
    end function c_ftop_cond_init

    integer(c_int) function c_ftop_cond_wait(cond_handle, mutex_handle, sys_errno) bind(C, name="ftop_cond_wait")
      import :: c_int, c_ptr
      type(c_ptr), value :: cond_handle
      type(c_ptr), value :: mutex_handle
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_cond_wait

    integer(c_int) function c_ftop_cond_signal(cond_handle, sys_errno) bind(C, name="ftop_cond_signal")
      import :: c_int, c_ptr
      type(c_ptr), value :: cond_handle
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_cond_signal

    integer(c_int) function c_ftop_cond_broadcast(cond_handle, sys_errno) bind(C, name="ftop_cond_broadcast")
      import :: c_int, c_ptr
      type(c_ptr), value :: cond_handle
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_cond_broadcast

    integer(c_int) function c_ftop_cond_destroy(cond_handle, sys_errno) bind(C, name="ftop_cond_destroy")
      import :: c_int, c_ptr
      type(c_ptr), value :: cond_handle
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_cond_destroy
  end interface

contains

  logical function ftop_thread_create(thread, start_routine, arg, error_code) result(success)
    type(ftop_thread_handle), intent(out) :: thread
    type(c_funptr), intent(in) :: start_routine
    type(c_ptr), intent(in), optional :: arg
    integer, intent(out), optional :: error_code
    type(c_ptr) :: thread_arg
    integer(c_int) :: sys_errno

    thread_arg = c_null_ptr
    if (present(arg)) thread_arg = arg

    thread%handle = c_ftop_thread_create(start_routine, thread_arg, sys_errno)
    success = c_associated(thread%handle)
    call assign_error(error_code, sys_errno)
  end function ftop_thread_create

  logical function ftop_thread_join(thread, error_code) result(success)
    type(ftop_thread_handle), intent(inout) :: thread
    integer, intent(out), optional :: error_code
    integer(c_int) :: sys_errno

    success = c_ftop_thread_join(thread%handle, sys_errno) == 0_c_int
    if (success) thread%handle = c_null_ptr
    call assign_error(error_code, sys_errno)
  end function ftop_thread_join

  logical function ftop_mutex_init(mutex, error_code) result(success)
    type(ftop_mutex_handle), intent(out) :: mutex
    integer, intent(out), optional :: error_code
    integer(c_int) :: sys_errno

    mutex%handle = c_ftop_mutex_init(sys_errno)
    success = c_associated(mutex%handle)
    call assign_error(error_code, sys_errno)
  end function ftop_mutex_init

  logical function ftop_mutex_lock(mutex, error_code) result(success)
    type(ftop_mutex_handle), intent(in) :: mutex
    integer, intent(out), optional :: error_code
    integer(c_int) :: sys_errno

    success = c_ftop_mutex_lock(mutex%handle, sys_errno) == 0_c_int
    call assign_error(error_code, sys_errno)
  end function ftop_mutex_lock

  logical function ftop_mutex_unlock(mutex, error_code) result(success)
    type(ftop_mutex_handle), intent(in) :: mutex
    integer, intent(out), optional :: error_code
    integer(c_int) :: sys_errno

    success = c_ftop_mutex_unlock(mutex%handle, sys_errno) == 0_c_int
    call assign_error(error_code, sys_errno)
  end function ftop_mutex_unlock

  logical function ftop_mutex_destroy(mutex, error_code) result(success)
    type(ftop_mutex_handle), intent(inout) :: mutex
    integer, intent(out), optional :: error_code
    integer(c_int) :: sys_errno

    success = c_ftop_mutex_destroy(mutex%handle, sys_errno) == 0_c_int
    if (success) mutex%handle = c_null_ptr
    call assign_error(error_code, sys_errno)
  end function ftop_mutex_destroy

  logical function ftop_cond_init(cond, error_code) result(success)
    type(ftop_cond_handle), intent(out) :: cond
    integer, intent(out), optional :: error_code
    integer(c_int) :: sys_errno

    cond%handle = c_ftop_cond_init(sys_errno)
    success = c_associated(cond%handle)
    call assign_error(error_code, sys_errno)
  end function ftop_cond_init

  logical function ftop_cond_wait(cond, mutex, error_code) result(success)
    type(ftop_cond_handle), intent(in) :: cond
    type(ftop_mutex_handle), intent(in) :: mutex
    integer, intent(out), optional :: error_code
    integer(c_int) :: sys_errno

    success = c_ftop_cond_wait(cond%handle, mutex%handle, sys_errno) == 0_c_int
    call assign_error(error_code, sys_errno)
  end function ftop_cond_wait

  logical function ftop_cond_signal(cond, error_code) result(success)
    type(ftop_cond_handle), intent(in) :: cond
    integer, intent(out), optional :: error_code
    integer(c_int) :: sys_errno

    success = c_ftop_cond_signal(cond%handle, sys_errno) == 0_c_int
    call assign_error(error_code, sys_errno)
  end function ftop_cond_signal

  logical function ftop_cond_broadcast(cond, error_code) result(success)
    type(ftop_cond_handle), intent(in) :: cond
    integer, intent(out), optional :: error_code
    integer(c_int) :: sys_errno

    success = c_ftop_cond_broadcast(cond%handle, sys_errno) == 0_c_int
    call assign_error(error_code, sys_errno)
  end function ftop_cond_broadcast

  logical function ftop_cond_destroy(cond, error_code) result(success)
    type(ftop_cond_handle), intent(inout) :: cond
    integer, intent(out), optional :: error_code
    integer(c_int) :: sys_errno

    success = c_ftop_cond_destroy(cond%handle, sys_errno) == 0_c_int
    if (success) cond%handle = c_null_ptr
    call assign_error(error_code, sys_errno)
  end function ftop_cond_destroy

  subroutine assign_error(error_code, sys_errno)
    integer, intent(out), optional :: error_code
    integer(c_int), intent(in) :: sys_errno

    if (present(error_code)) error_code = int(sys_errno)
  end subroutine assign_error

end module ftop_pthread
