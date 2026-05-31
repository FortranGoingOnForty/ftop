module ftop_cpu_data
  use, intrinsic :: iso_fortran_env, only : real64
  use ftop_ring_buffer, only : ring_buffer
  implicit none
  private

  type, public :: cpu_core_info
    logical :: valid = .false.
    real(real64) :: usage_percent = 0.0_real64
    real(real64) :: user_percent = 0.0_real64
    real(real64) :: system_percent = 0.0_real64
    real(real64) :: iowait_percent = 0.0_real64
    logical :: freq_valid = .false.
    real(real64) :: freq_mhz = 0.0_real64
    logical :: temp_valid = .false.
    real(real64) :: temp_c = 0.0_real64
  end type cpu_core_info

  type, public :: cpu_total_info
    logical :: valid = .false.
    real(real64) :: usage_percent = 0.0_real64
    real(real64) :: load_avg(3) = 0.0_real64
    logical :: load_valid = .false.
    integer :: core_count = 0
    integer :: thread_count = 0
  end type cpu_total_info

  type, public :: cpu_history
    private
    type(ring_buffer) :: total_usage
    type(ring_buffer), allocatable :: core_usage(:)
    integer :: cores = 0
    logical :: ready = .false.
  contains
    procedure :: init => cpu_history_init
    procedure :: destroy => cpu_history_destroy
    procedure :: push_total_usage => cpu_history_push_total_usage
    procedure :: push_core_usage => cpu_history_push_core_usage
    procedure :: snapshot_total_usage => cpu_history_snapshot_total_usage
    procedure :: snapshot_core_usage => cpu_history_snapshot_core_usage
    procedure :: core_count => cpu_history_core_count
    procedure :: capacity => cpu_history_capacity
    procedure :: initialized => cpu_history_initialized
    final :: cpu_history_finalize
  end type cpu_history

contains

  logical function cpu_history_init(self, core_count, history_capacity) result(success)
    class(cpu_history), intent(inout) :: self
    integer, intent(in) :: core_count
    integer, intent(in), optional :: history_capacity
    integer :: allocation_status
    integer :: core_index

    success = .false.
    if (core_count < 0) return
    if (self%ready .or. allocated(self%core_usage) .or. self%total_usage%initialized()) then
      if (.not. self%destroy()) return
    end if

    allocate(self%core_usage(core_count), stat=allocation_status)
    if (allocation_status /= 0) return

    if (.not. self%total_usage%init(history_capacity)) then
      call cleanup_failed_init(self)
      return
    end if

    do core_index = 1, core_count
      if (.not. self%core_usage(core_index)%init(history_capacity)) then
        call cleanup_failed_init(self)
        return
      end if
    end do

    self%cores = core_count
    self%ready = .true.
    success = .true.
  end function cpu_history_init

  logical function cpu_history_destroy(self) result(success)
    class(cpu_history), intent(inout) :: self
    integer :: core_index

    success = .true.
    if (self%total_usage%initialized()) then
      if (.not. self%total_usage%destroy()) success = .false.
    end if

    if (allocated(self%core_usage)) then
      do core_index = 1, size(self%core_usage)
        if (self%core_usage(core_index)%initialized()) then
          if (.not. self%core_usage(core_index)%destroy()) success = .false.
        end if
      end do
      if (success) deallocate(self%core_usage)
    end if

    if (.not. success) return
    self%cores = 0
    self%ready = .false.
  end function cpu_history_destroy

  logical function cpu_history_push_total_usage(self, usage_percent) result(success)
    class(cpu_history), intent(inout) :: self
    real(real64), intent(in) :: usage_percent

    success = .false.
    if (.not. self%ready) return
    success = self%total_usage%push(clamp_percent(usage_percent))
  end function cpu_history_push_total_usage

  logical function cpu_history_push_core_usage(self, core_index, usage_percent) result(success)
    class(cpu_history), intent(inout) :: self
    integer, intent(in) :: core_index
    real(real64), intent(in) :: usage_percent

    success = .false.
    if (.not. self%ready) return
    if (core_index < 1 .or. core_index > self%cores) return
    success = self%core_usage(core_index)%push(clamp_percent(usage_percent))
  end function cpu_history_push_core_usage

  logical function cpu_history_snapshot_total_usage(self, samples) result(success)
    class(cpu_history), intent(in) :: self
    real(real64), allocatable, intent(out) :: samples(:)

    success = .false.
    if (.not. self%ready) return
    success = self%total_usage%snapshot(samples)
  end function cpu_history_snapshot_total_usage

  logical function cpu_history_snapshot_core_usage(self, core_index, samples) result(success)
    class(cpu_history), intent(in) :: self
    integer, intent(in) :: core_index
    real(real64), allocatable, intent(out) :: samples(:)

    success = .false.
    if (.not. self%ready) return
    if (core_index < 1 .or. core_index > self%cores) return
    success = self%core_usage(core_index)%snapshot(samples)
  end function cpu_history_snapshot_core_usage

  integer function cpu_history_core_count(self) result(count)
    class(cpu_history), intent(in) :: self

    count = self%cores
  end function cpu_history_core_count

  integer function cpu_history_capacity(self) result(history_capacity)
    class(cpu_history), intent(in) :: self

    history_capacity = self%total_usage%capacity()
  end function cpu_history_capacity

  logical function cpu_history_initialized(self) result(is_initialized)
    class(cpu_history), intent(in) :: self

    is_initialized = self%ready
  end function cpu_history_initialized

  subroutine cpu_history_finalize(self)
    type(cpu_history), intent(inout) :: self
    logical :: ignored

    ignored = cpu_history_destroy(self)
  end subroutine cpu_history_finalize

  subroutine cleanup_failed_init(self)
    class(cpu_history), intent(inout) :: self
    logical :: ignored

    ignored = self%destroy()
  end subroutine cleanup_failed_init

  real(real64) function clamp_percent(value) result(clamped)
    real(real64), intent(in) :: value

    clamped = max(0.0_real64, min(100.0_real64, value))
  end function clamp_percent

end module ftop_cpu_data
