module ftop_platform_types
  use, intrinsic :: iso_fortran_env, only : int64, real64
  implicit none
  private

  public :: cpu_tick_sample
  public :: cpu_usage_percent
  public :: memory_info
  public :: platform_backend

  type :: cpu_tick_sample
    logical :: valid = .false.
    integer(int64) :: total = 0_int64
    integer(int64) :: idle = 0_int64
  end type cpu_tick_sample

  type :: memory_info
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

  type, abstract :: platform_backend
  contains
    procedure(get_cpu_count_interface), deferred :: get_cpu_count
    procedure(get_cpu_sample_interface), deferred :: get_cpu_sample
    procedure(get_memory_info_interface), deferred :: get_memory_info
  end type platform_backend

  abstract interface
    integer function get_cpu_count_interface(self) result(count)
      import :: platform_backend
      class(platform_backend), intent(in) :: self
    end function get_cpu_count_interface

    function get_cpu_sample_interface(self) result(sample)
      import :: cpu_tick_sample, platform_backend
      class(platform_backend), intent(in) :: self
      type(cpu_tick_sample) :: sample
    end function get_cpu_sample_interface

    function get_memory_info_interface(self) result(info)
      import :: memory_info, platform_backend
      class(platform_backend), intent(in) :: self
      type(memory_info) :: info
    end function get_memory_info_interface
  end interface

contains

  real(real64) function cpu_usage_percent(previous, current) result(usage)
    type(cpu_tick_sample), intent(in) :: previous
    type(cpu_tick_sample), intent(in) :: current
    integer(int64) :: total_delta
    integer(int64) :: idle_delta
    real(real64) :: busy_delta

    usage = 0.0_real64
    if (.not. previous%valid .or. .not. current%valid) return

    total_delta = current%total - previous%total
    idle_delta = current%idle - previous%idle
    if (total_delta <= 0_int64) return
    if (idle_delta < 0_int64) idle_delta = 0_int64
    if (idle_delta > total_delta) idle_delta = total_delta

    busy_delta = real(total_delta - idle_delta, real64)
    usage = 100.0_real64 * busy_delta / real(total_delta, real64)
    usage = max(0.0_real64, min(100.0_real64, usage))
  end function cpu_usage_percent

end module ftop_platform_types
