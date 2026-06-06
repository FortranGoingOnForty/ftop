module ftop_mem_data
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use ftop_ring_buffer, only : ring_buffer
  implicit none
  private

  type, public :: memory_info
    logical :: valid = .false.
    integer(int64) :: total_bytes = 0_int64
    integer(int64) :: used_bytes = 0_int64
    integer(int64) :: free_bytes = 0_int64
    integer(int64) :: available_bytes = 0_int64
    integer(int64) :: cached_bytes = 0_int64
    integer(int64) :: buffers_bytes = 0_int64
    integer(int64) :: swap_total_bytes = 0_int64
    integer(int64) :: swap_used_bytes = 0_int64
  end type memory_info

  type, public :: memory_history
    private
    type(ring_buffer) :: usage
    logical :: ready = .false.
  contains
    procedure :: init => memory_history_init
    procedure :: destroy => memory_history_destroy
    procedure :: push_usage => memory_history_push_usage
    procedure :: push_info => memory_history_push_info
    procedure :: snapshot_usage => memory_history_snapshot_usage
    procedure :: size => memory_history_size
    procedure :: capacity => memory_history_capacity
    procedure :: initialized => memory_history_initialized
    final :: memory_history_finalize
  end type memory_history

  public :: memory_pressure_label
  public :: memory_pressure_percent
  public :: memory_reclaimable_bytes
  public :: memory_usage_percent
  public :: memory_used_bytes

contains

  real(real64) function memory_usage_percent(info) result(usage_percent)
    type(memory_info), intent(in) :: info

    usage_percent = 0.0_real64
    if (.not. info%valid) return
    if (info%total_bytes <= 0_int64) return
    usage_percent = 100.0_real64 * real(memory_used_bytes(info), real64) / real(info%total_bytes, real64)
    usage_percent = max(0.0_real64, min(100.0_real64, usage_percent))
  end function memory_usage_percent

  integer(int64) function memory_used_bytes(info) result(bytes)
    type(memory_info), intent(in) :: info
    integer(int64) :: available_bytes

    bytes = 0_int64
    if (.not. info%valid) return
    if (info%total_bytes <= 0_int64) return
    available_bytes = bounded_bytes(info%available_bytes, info%total_bytes)
    bytes = max(0_int64, info%total_bytes - available_bytes)
  end function memory_used_bytes

  integer(int64) function memory_reclaimable_bytes(info) result(bytes)
    type(memory_info), intent(in) :: info
    integer(int64) :: available_bytes
    integer(int64) :: free_bytes

    bytes = 0_int64
    if (.not. info%valid) return
    if (info%total_bytes <= 0_int64) return
    available_bytes = bounded_bytes(info%available_bytes, info%total_bytes)
    free_bytes = bounded_bytes(info%free_bytes, available_bytes)
    bytes = max(0_int64, available_bytes - free_bytes)
  end function memory_reclaimable_bytes

  real(real64) function memory_pressure_percent(info) result(pressure_percent)
    type(memory_info), intent(in) :: info

    pressure_percent = memory_usage_percent(info)
  end function memory_pressure_percent

  function memory_pressure_label(info) result(label)
    type(memory_info), intent(in) :: info
    character(len=:), allocatable :: label
    real(real64) :: available_fraction
    real(real64) :: swap_fraction

    label = "unknown"
    if (.not. info%valid) return
    if (info%total_bytes <= 0_int64) return

    available_fraction = real(bounded_bytes(info%available_bytes, info%total_bytes), real64) / &
                         real(info%total_bytes, real64)
    swap_fraction = 0.0_real64
    if (info%swap_total_bytes > 0_int64) then
      swap_fraction = real(max(0_int64, min(info%swap_total_bytes, info%swap_used_bytes)), real64) / &
                      real(info%swap_total_bytes, real64)
    end if

    if (available_fraction <= 0.05_real64 .or. swap_fraction >= 0.80_real64) then
      label = "critical"
    else if (available_fraction <= 0.10_real64 .or. swap_fraction >= 0.50_real64) then
      label = "high"
    else if (available_fraction <= 0.20_real64 .or. swap_fraction >= 0.25_real64) then
      label = "medium"
    else
      label = "low"
    end if
  end function memory_pressure_label

  logical function memory_history_init(self, history_capacity) result(success)
    class(memory_history), intent(inout) :: self
    integer, intent(in), optional :: history_capacity

    success = .false.
    if (self%ready .or. self%usage%initialized()) then
      if (.not. self%destroy()) return
    end if

    if (.not. self%usage%init(history_capacity)) return
    self%ready = .true.
    success = .true.
  end function memory_history_init

  logical function memory_history_destroy(self) result(success)
    class(memory_history), intent(inout) :: self

    success = .true.
    if (self%usage%initialized()) success = self%usage%destroy()
    if (.not. success) return
    self%ready = .false.
  end function memory_history_destroy

  logical function memory_history_push_usage(self, usage_percent) result(success)
    class(memory_history), intent(inout) :: self
    real(real64), intent(in) :: usage_percent

    success = .false.
    if (.not. self%ready) return
    success = self%usage%push(clamp_percent(usage_percent))
  end function memory_history_push_usage

  logical function memory_history_push_info(self, info) result(success)
    class(memory_history), intent(inout) :: self
    type(memory_info), intent(in) :: info

    success = .false.
    if (.not. self%ready) return
    success = self%usage%push(memory_usage_percent(info))
  end function memory_history_push_info

  logical function memory_history_snapshot_usage(self, samples) result(success)
    class(memory_history), intent(in) :: self
    real(real64), allocatable, intent(out) :: samples(:)

    success = .false.
    if (.not. self%ready) return
    success = self%usage%snapshot(samples)
  end function memory_history_snapshot_usage

  integer function memory_history_size(self) result(current_size)
    class(memory_history), intent(in) :: self

    current_size = 0
    if (.not. self%ready) return
    current_size = self%usage%size()
  end function memory_history_size

  integer function memory_history_capacity(self) result(history_capacity)
    class(memory_history), intent(in) :: self

    history_capacity = self%usage%capacity()
  end function memory_history_capacity

  logical function memory_history_initialized(self) result(is_initialized)
    class(memory_history), intent(in) :: self

    is_initialized = self%ready
  end function memory_history_initialized

  subroutine memory_history_finalize(self)
    type(memory_history), intent(inout) :: self
    logical :: ignored

    ignored = memory_history_destroy(self)
  end subroutine memory_history_finalize

  real(real64) function clamp_percent(value) result(clamped)
    real(real64), intent(in) :: value

    clamped = max(0.0_real64, min(100.0_real64, value))
  end function clamp_percent

  integer(int64) function bounded_bytes(bytes, limit) result(bounded)
    integer(int64), intent(in) :: bytes
    integer(int64), intent(in) :: limit

    bounded = max(0_int64, min(max(0_int64, limit), bytes))
  end function bounded_bytes

end module ftop_mem_data
