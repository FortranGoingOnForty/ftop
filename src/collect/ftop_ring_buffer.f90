module ftop_ring_buffer
  use, intrinsic :: iso_fortran_env, only : real64
  use ftop_pthread, only : &
    ftop_mutex_destroy, &
    ftop_mutex_handle, &
    ftop_mutex_init, &
    ftop_mutex_lock, &
    ftop_mutex_unlock
  implicit none
  private

  integer, parameter, public :: FTOP_RING_BUFFER_DEFAULT_CAPACITY = 300

  type, public :: ring_buffer
    private
    real(real64), allocatable :: values(:)
    integer :: start = 1
    integer :: count = 0
    type(ftop_mutex_handle) :: mutex
    logical :: ready = .false.
  contains
    procedure :: init => ring_buffer_init
    procedure :: destroy => ring_buffer_destroy
    procedure :: push => ring_buffer_push
    procedure :: get => ring_buffer_get
    procedure :: snapshot => ring_buffer_snapshot
    procedure :: size => ring_buffer_size
    procedure :: capacity => ring_buffer_capacity
    procedure :: initialized => ring_buffer_initialized
    final :: ring_buffer_finalize
    final :: ring_buffer_finalize_rank1
  end type ring_buffer

contains

  logical function ring_buffer_init(self, capacity) result(success)
    class(ring_buffer), intent(inout) :: self
    integer, intent(in), optional :: capacity
    integer :: allocation_status
    integer :: requested_capacity

    success = .false.
    requested_capacity = FTOP_RING_BUFFER_DEFAULT_CAPACITY
    if (present(capacity)) requested_capacity = capacity
    if (requested_capacity <= 0) return

    if (self%ready) then
      if (.not. self%destroy()) return
    else if (allocated(self%values)) then
      deallocate(self%values)
    end if

    allocate(self%values(requested_capacity), stat=allocation_status)
    if (allocation_status /= 0) return

    self%values = 0.0_real64
    self%start = 1
    self%count = 0

    if (.not. ftop_mutex_init(self%mutex)) then
      deallocate(self%values)
      return
    end if

    self%ready = .true.
    success = .true.
  end function ring_buffer_init

  logical function ring_buffer_destroy(self) result(success)
    class(ring_buffer), intent(inout) :: self

    success = .true.
    if (self%ready) success = ftop_mutex_destroy(self%mutex)
    if (.not. success) return

    if (allocated(self%values)) deallocate(self%values)
    self%start = 1
    self%count = 0
    self%ready = .false.
  end function ring_buffer_destroy

  logical function ring_buffer_push(self, value) result(success)
    class(ring_buffer), intent(inout) :: self
    real(real64), intent(in) :: value
    integer :: buffer_capacity
    integer :: index

    success = .false.
    if (.not. self%ready) return
    if (.not. ftop_mutex_lock(self%mutex)) return

    buffer_capacity = size(self%values)
    if (self%count < buffer_capacity) then
      self%count = self%count + 1
      index = physical_index(self, self%count)
    else
      index = self%start
      self%start = modulo(self%start, buffer_capacity) + 1
    end if

    self%values(index) = value
    success = .true.
    if (.not. ftop_mutex_unlock(self%mutex)) success = .false.
  end function ring_buffer_push

  logical function ring_buffer_get(self, item_index, value) result(success)
    class(ring_buffer), intent(in) :: self
    integer, intent(in) :: item_index
    real(real64), intent(out) :: value
    integer :: index

    value = 0.0_real64
    success = .false.
    if (.not. self%ready) return
    if (.not. ftop_mutex_lock(self%mutex)) return

    if (item_index >= 1 .and. item_index <= self%count) then
      index = physical_index(self, item_index)
      value = self%values(index)
      success = .true.
    end if

    if (.not. ftop_mutex_unlock(self%mutex)) success = .false.
  end function ring_buffer_get

  logical function ring_buffer_snapshot(self, samples) result(success)
    class(ring_buffer), intent(in) :: self
    real(real64), allocatable, intent(out) :: samples(:)
    integer :: allocation_status
    integer :: item_index

    success = .false.
    if (.not. self%ready) return
    if (.not. ftop_mutex_lock(self%mutex)) return

    allocate(samples(self%count), stat=allocation_status)
    if (allocation_status == 0) then
      do item_index = 1, self%count
        samples(item_index) = self%values(physical_index(self, item_index))
      end do
      success = .true.
    end if

    if (.not. ftop_mutex_unlock(self%mutex)) success = .false.
  end function ring_buffer_snapshot

  integer function ring_buffer_size(self) result(current_size)
    class(ring_buffer), intent(in) :: self

    current_size = 0
    if (.not. self%ready) return
    if (.not. ftop_mutex_lock(self%mutex)) return

    current_size = self%count
    if (.not. ftop_mutex_unlock(self%mutex)) current_size = 0
  end function ring_buffer_size

  integer function ring_buffer_capacity(self) result(current_capacity)
    class(ring_buffer), intent(in) :: self

    current_capacity = 0
    if (allocated(self%values)) current_capacity = size(self%values)
  end function ring_buffer_capacity

  logical function ring_buffer_initialized(self) result(is_initialized)
    class(ring_buffer), intent(in) :: self

    is_initialized = self%ready
  end function ring_buffer_initialized

  subroutine ring_buffer_finalize(self)
    type(ring_buffer), intent(inout) :: self
    logical :: ignored

    ignored = ring_buffer_destroy(self)
  end subroutine ring_buffer_finalize

  subroutine ring_buffer_finalize_rank1(self)
    type(ring_buffer), intent(inout) :: self(:)
    integer :: item_index
    logical :: ignored

    do item_index = 1, size(self)
      ignored = ring_buffer_destroy(self(item_index))
    end do
  end subroutine ring_buffer_finalize_rank1

  integer function physical_index(self, item_index) result(index)
    class(ring_buffer), intent(in) :: self
    integer, intent(in) :: item_index

    index = modulo(self%start + item_index - 2, size(self%values)) + 1
  end function physical_index

end module ftop_ring_buffer
