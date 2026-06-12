module ftop_platform_types
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use ftop_cpu_data, only : CPU_MODEL_NAME_LEN, cpu_core_info, cpu_state_ticks
  use ftop_disk_data, only : disk_table
  use ftop_gpu_data, only : gpu_process_table, gpu_table
  use ftop_net_data, only : network_table
  use ftop_proc_data, only : process_table
  implicit none
  private

  public :: cpu_tick_sample
  public :: cpu_topology_info
  public :: cpu_usage_percent
  public :: disk_table
  public :: gpu_table
  public :: gpu_process_table
  public :: load_average_info
  public :: memory_info
  public :: network_table
  public :: platform_backend
  public :: process_table
  public :: system_uptime_info

  type :: cpu_tick_sample
    logical :: valid = .false.
    integer(int64) :: total = 0_int64
    integer(int64) :: idle = 0_int64
  end type cpu_tick_sample

  type :: cpu_topology_info
    logical :: valid = .false.
    integer :: core_count = 0
    integer :: thread_count = 0
    logical :: model_name_valid = .false.
    character(len=CPU_MODEL_NAME_LEN) :: model_name = ""
  end type cpu_topology_info

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

  type :: load_average_info
    logical :: valid = .false.
    real(real64) :: values(3) = 0.0_real64
  end type load_average_info

  type :: system_uptime_info
    logical :: valid = .false.
    integer(int64) :: seconds = 0_int64
  end type system_uptime_info

  type, abstract :: platform_backend
  contains
    procedure(get_cpu_count_interface), deferred :: get_cpu_count
    procedure(get_cpu_topology_interface), deferred :: get_cpu_topology
    procedure(get_cpu_sample_interface), deferred :: get_cpu_sample
    procedure(get_cpu_state_snapshot_interface), deferred :: get_cpu_state_snapshot
    procedure(get_cpu_metadata_interface), deferred :: get_cpu_metadata
    procedure(get_memory_info_interface), deferred :: get_memory_info
    procedure(get_load_average_interface), deferred :: get_load_average
    procedure(get_system_uptime_interface), deferred :: get_system_uptime
    procedure(get_process_table_interface), deferred :: get_process_table
    procedure(get_network_table_interface), deferred :: get_network_table
    procedure(get_disk_table_interface), deferred :: get_disk_table
    procedure(get_gpu_table_interface), deferred :: get_gpu_table
    procedure(get_gpu_process_table_interface), deferred :: get_gpu_process_table
  end type platform_backend

  abstract interface
    integer function get_cpu_count_interface(self) result(count)
      import :: platform_backend
      class(platform_backend), intent(in) :: self
    end function get_cpu_count_interface

    function get_cpu_topology_interface(self) result(info)
      import :: cpu_topology_info, platform_backend
      class(platform_backend), intent(in) :: self
      type(cpu_topology_info) :: info
    end function get_cpu_topology_interface

    function get_cpu_sample_interface(self) result(sample)
      import :: cpu_tick_sample, platform_backend
      class(platform_backend), intent(in) :: self
      type(cpu_tick_sample) :: sample
    end function get_cpu_sample_interface

    logical function get_cpu_state_snapshot_interface(self, total, cores) result(success)
      import :: cpu_state_ticks, platform_backend
      class(platform_backend), intent(in) :: self
      type(cpu_state_ticks), intent(out) :: total
      type(cpu_state_ticks), allocatable, intent(out) :: cores(:)
    end function get_cpu_state_snapshot_interface

    logical function get_cpu_metadata_interface(self, cores) result(success)
      import :: cpu_core_info, platform_backend
      class(platform_backend), intent(in) :: self
      type(cpu_core_info), allocatable, intent(out) :: cores(:)
    end function get_cpu_metadata_interface

    function get_memory_info_interface(self) result(info)
      import :: memory_info, platform_backend
      class(platform_backend), intent(in) :: self
      type(memory_info) :: info
    end function get_memory_info_interface

    function get_load_average_interface(self) result(info)
      import :: load_average_info, platform_backend
      class(platform_backend), intent(in) :: self
      type(load_average_info) :: info
    end function get_load_average_interface

    function get_system_uptime_interface(self) result(info)
      import :: platform_backend, system_uptime_info
      class(platform_backend), intent(in) :: self
      type(system_uptime_info) :: info
    end function get_system_uptime_interface

    function get_process_table_interface(self) result(table)
      import :: platform_backend, process_table
      class(platform_backend), intent(in) :: self
      type(process_table) :: table
    end function get_process_table_interface

    function get_network_table_interface(self) result(table)
      import :: network_table, platform_backend
      class(platform_backend), intent(in) :: self
      type(network_table) :: table
    end function get_network_table_interface

    function get_disk_table_interface(self) result(table)
      import :: disk_table, platform_backend
      class(platform_backend), intent(in) :: self
      type(disk_table) :: table
    end function get_disk_table_interface

    function get_gpu_table_interface(self) result(table)
      import :: gpu_table, platform_backend
      class(platform_backend), intent(in) :: self
      type(gpu_table) :: table
    end function get_gpu_table_interface

    function get_gpu_process_table_interface(self) result(table)
      import :: gpu_process_table, platform_backend
      class(platform_backend), intent(in) :: self
      type(gpu_process_table) :: table
    end function get_gpu_process_table_interface
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
