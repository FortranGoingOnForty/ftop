program test_ring_buffer
  use, intrinsic :: iso_fortran_env, only : real64
  use ftop_ring_buffer, only : FTOP_RING_BUFFER_DEFAULT_CAPACITY, ring_buffer
  implicit none

  call test_default_capacity()
  call test_invalid_capacity()
  call test_push_and_read()
  call test_wrap_around()
  call test_snapshot_copy()

contains

  subroutine test_default_capacity()
    type(ring_buffer) :: buffer
    real(real64), allocatable :: samples(:)

    call require(.not. buffer%initialized(), "new buffer must start uninitialized")
    call require(buffer%capacity() == 0, "uninitialized buffer capacity must be zero")
    call require(buffer%init(), "default ring buffer init failed")
    call require(buffer%initialized(), "ring buffer must report initialized")
    call require(buffer%capacity() == FTOP_RING_BUFFER_DEFAULT_CAPACITY, "default capacity mismatch")
    call require(buffer%size() == 0, "new ring buffer must be empty")
    call require(buffer%snapshot(samples), "empty snapshot failed")
    call require(allocated(samples), "empty snapshot must allocate output")
    call require(size(samples) == 0, "empty snapshot must contain no samples")
    call require(buffer%destroy(), "default ring buffer destroy failed")
  end subroutine test_default_capacity

  subroutine test_invalid_capacity()
    type(ring_buffer) :: buffer

    call require(.not. buffer%init(0), "zero capacity init must fail")
    call require(.not. buffer%initialized(), "failed init must leave buffer uninitialized")
    call require(buffer%capacity() == 0, "failed init must not allocate storage")
  end subroutine test_invalid_capacity

  subroutine test_push_and_read()
    type(ring_buffer) :: buffer
    real(real64) :: value

    call require(buffer%init(3), "ring buffer init failed")
    call require(buffer%push(10.0_real64), "first push failed")
    call require(buffer%push(20.0_real64), "second push failed")
    call require(buffer%size() == 2, "ring buffer size mismatch after pushes")

    call require(buffer%get(1, value), "oldest read failed")
    call require_close(value, 10.0_real64, "oldest value mismatch")
    call require(buffer%get(2, value), "newest read failed")
    call require_close(value, 20.0_real64, "newest value mismatch")
    call require(.not. buffer%get(0, value), "zero index read must fail")
    call require(.not. buffer%get(3, value), "out-of-range read must fail")

    call require(buffer%destroy(), "ring buffer destroy failed")
  end subroutine test_push_and_read

  subroutine test_wrap_around()
    type(ring_buffer) :: buffer
    real(real64), allocatable :: samples(:)
    real(real64) :: value
    integer :: item_index

    call require(buffer%init(3), "wrap ring buffer init failed")
    do item_index = 1, 5
      call require(buffer%push(real(item_index, real64)), "wrap push failed")
    end do

    call require(buffer%size() == 3, "wrapped ring buffer size must stay at capacity")
    call require(buffer%snapshot(samples), "wrapped snapshot failed")
    call require(size(samples) == 3, "wrapped snapshot size mismatch")
    call require_close(samples(1), 3.0_real64, "wrapped first sample mismatch")
    call require_close(samples(2), 4.0_real64, "wrapped second sample mismatch")
    call require_close(samples(3), 5.0_real64, "wrapped third sample mismatch")

    call require(buffer%get(1, value), "wrapped oldest read failed")
    call require_close(value, 3.0_real64, "wrapped oldest value mismatch")
    call require(buffer%get(3, value), "wrapped newest read failed")
    call require_close(value, 5.0_real64, "wrapped newest value mismatch")

    call require(buffer%destroy(), "wrap ring buffer destroy failed")
  end subroutine test_wrap_around

  subroutine test_snapshot_copy()
    type(ring_buffer) :: buffer
    real(real64), allocatable :: first_snapshot(:)
    real(real64), allocatable :: second_snapshot(:)

    call require(buffer%init(2), "snapshot ring buffer init failed")
    call require(buffer%push(1.0_real64), "snapshot first push failed")
    call require(buffer%snapshot(first_snapshot), "first snapshot failed")
    call require(buffer%push(2.0_real64), "snapshot second push failed")
    call require(buffer%snapshot(second_snapshot), "second snapshot failed")

    call require(size(first_snapshot) == 1, "first snapshot size changed")
    call require_close(first_snapshot(1), 1.0_real64, "first snapshot value changed")
    call require(size(second_snapshot) == 2, "second snapshot size mismatch")
    call require_close(second_snapshot(1), 1.0_real64, "second snapshot oldest mismatch")
    call require_close(second_snapshot(2), 2.0_real64, "second snapshot newest mismatch")

    call require(buffer%destroy(), "snapshot ring buffer destroy failed")
  end subroutine test_snapshot_copy

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

end program test_ring_buffer
