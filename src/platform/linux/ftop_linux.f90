module ftop_platform
  use, intrinsic :: iso_c_binding, only : c_char, c_double, c_int, c_long_long, c_null_char, c_size_t
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use ftop_cpu_data, only : cpu_core_info, cpu_state_ticks, cpu_state_total_ticks
  use ftop_disk_data, only : DISK_FILESYSTEM_CAPACITY, c_filesystem_info, disk_io_info, disk_table, disk_table_from_c
  use ftop_gpu_data, only : GPU_CAPACITY, empty_gpu_table, gpu_info, gpu_table
  use ftop_linux_amdgpu, only : linux_amdgpu_snapshot
  use ftop_linux_diskstats, only : linux_diskstats_filter_whole_devices, linux_diskstats_parse
  use ftop_linux_intelgpu, only : linux_intelgpu_snapshot
  use ftop_linux_loadavg, only : linux_loadavg_parse
  use ftop_linux_meminfo, only : linux_meminfo_parse
  use ftop_linux_nvml, only : linux_nvml_gpu_snapshot
  use ftop_linux_proc_stat, only : linux_proc_stat_parse
  use ftop_mem_data, only : metric_memory_info => memory_info
  use ftop_net_data, only : &
    NET_PROCESS_NAME_LEN, &
    interface_info, &
    net_connection, &
    network_table, &
    parse_linux_proc_net_connections, &
    parse_linux_proc_net_dev, &
    process_bandwidth
  use ftop_platform_types, only : &
    cpu_tick_sample, &
    cpu_topology_info, &
    cpu_usage_percent, &
    load_average_info, &
    memory_info, &
    platform_backend, &
    process_table, &
    system_uptime_info
  use ftop_log, only : log_warn
  use ftop_proc_data, only : &
    PROCESS_CGROUP_LEN, &
    PROCESS_COMMAND_LEN, &
    PROCESS_NAME_LEN, &
    PROCESS_USER_LEN, &
    process_info
  implicit none
  private

  integer, parameter :: LINUX_PROC_STAT_BUFFER_LEN = 1048576
  integer, parameter :: LINUX_PROC_MEMINFO_BUFFER_LEN = 65536
  integer, parameter :: LINUX_PROC_LOADAVG_BUFFER_LEN = 256
  integer, parameter :: LINUX_PROC_UPTIME_BUFFER_LEN = 128
  integer, parameter :: LINUX_PROC_NET_DEV_BUFFER_LEN = 262144
  integer, parameter :: LINUX_PROC_NET_CONNECTION_BUFFER_LEN = 1048576
  integer, parameter :: LINUX_PROC_DISKSTATS_BUFFER_LEN = 1048576
  integer, parameter :: LINUX_NET_IFACE_FIELD_BUFFER_LEN = 128
  integer, parameter :: LINUX_SOCKET_OWNER_CAPACITY = 16384
  integer, parameter :: LINUX_SOCKET_TRAFFIC_CAPACITY = 16384
  integer, parameter :: LINUX_PROCESS_CAPACITY = 4096
  integer, parameter :: LINUX_PROCESS_STAT_LEN = 512
  integer, parameter :: LINUX_PROCESS_STATUS_LEN = 2048
  integer, parameter :: LINUX_PROCESS_IO_LEN = 512
  integer, parameter :: LINUX_PROCESS_CGROUP_RAW_LEN = 1024
  integer, parameter :: LINUX_CPU_FREQ_BUFFER_LEN = 64
  integer, parameter, public :: LINUX_HWMON_NAME_LEN = 128
  integer, parameter, public :: LINUX_HWMON_PATH_LEN = 256

  integer, allocatable, save :: user_cache_uids(:)
  character(len=PROCESS_USER_LEN), allocatable, save :: user_cache_names(:)
  logical, save :: linux_hwmon_warned = .false.

  type, bind(C), public :: linux_hwmon_sensor
    character(kind=c_char) :: path(LINUX_HWMON_PATH_LEN)
    character(kind=c_char) :: name(LINUX_HWMON_NAME_LEN)
  end type linux_hwmon_sensor

  type, bind(C), public :: linux_process_raw
    integer(c_int) :: pid
    character(kind=c_char) :: stat(LINUX_PROCESS_STAT_LEN)
    integer(c_size_t) :: stat_len
    character(kind=c_char) :: status(LINUX_PROCESS_STATUS_LEN)
    integer(c_size_t) :: status_len
    character(kind=c_char) :: cmdline(PROCESS_COMMAND_LEN)
    integer(c_size_t) :: cmdline_len
    character(kind=c_char) :: io(LINUX_PROCESS_IO_LEN)
    integer(c_size_t) :: io_len
    character(kind=c_char) :: cgroup(LINUX_PROCESS_CGROUP_RAW_LEN)
    integer(c_size_t) :: cgroup_len
  end type linux_process_raw

  type, bind(C), public :: linux_socket_owner
    integer(c_long_long) :: inode
    integer(c_long_long) :: start_time
    integer(c_int) :: pid
    character(kind=c_char) :: process_name(NET_PROCESS_NAME_LEN)
  end type linux_socket_owner

  type, bind(C), public :: linux_socket_traffic
    integer(c_long_long) :: inode
    integer(c_long_long) :: rx_bytes
    integer(c_long_long) :: tx_bytes
  end type linux_socket_traffic

  type, extends(platform_backend) :: linux_backend
  contains
    procedure :: get_cpu_count => linux_get_cpu_count
    procedure :: get_cpu_topology => linux_get_cpu_topology
    procedure :: get_cpu_sample => linux_get_cpu_sample
    procedure :: get_cpu_state_snapshot => linux_get_cpu_state_snapshot
    procedure :: get_cpu_metadata => linux_get_cpu_metadata
    procedure :: get_memory_info => linux_get_memory_info
    procedure :: get_load_average => linux_get_load_average
    procedure :: get_system_uptime => linux_get_system_uptime
    procedure :: get_process_table => linux_get_process_table
    procedure :: get_network_table => linux_get_network_table
    procedure :: get_disk_table => linux_get_disk_table
    procedure :: get_gpu_table => linux_get_gpu_table
  end type linux_backend

  public :: create_platform
  public :: cpu_tick_sample
  public :: cpu_topology_info
  public :: cpu_usage_percent
  public :: linux_cpuinfo_field
  public :: linux_cpuinfo_field_count
  public :: linux_cpu_state_snapshot
  public :: linux_hwmon_discover
  public :: linux_load_average_snapshot
  public :: linux_memory_snapshot
  public :: linux_disk_snapshot
  public :: linux_network_snapshot
  public :: linux_process_bandwidth_snapshot
  public :: linux_process_snapshot
  public :: load_average_info
  public :: memory_info
  public :: network_table
  public :: platform_backend
  public :: process_table
  public :: system_uptime_info

  interface
    integer(c_int) function c_ftop_linux_cpu_count(count, sys_errno) bind(C, name="ftop_linux_cpu_count")
      import :: c_int
      integer(c_int), intent(out) :: count
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_cpu_count

    integer(c_int) function c_ftop_linux_filesystems(filesystems, capacity, filesystem_count, sys_errno) &
        bind(C, name="ftop_linux_filesystems")
      import :: c_filesystem_info, c_int
      type(c_filesystem_info), intent(inout) :: filesystems(*)
      integer(c_int), value :: capacity
      integer(c_int), intent(out) :: filesystem_count
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_filesystems

    integer(c_int) function c_ftop_linux_read_proc_stat(buffer, buffer_capacity, value_len, sys_errno) &
        bind(C, name="ftop_linux_read_proc_stat")
      import :: c_char, c_int, c_size_t
      character(kind=c_char), intent(out) :: buffer(*)
      integer(c_size_t), value :: buffer_capacity
      integer(c_size_t), intent(out) :: value_len
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_read_proc_stat

    integer(c_int) function c_ftop_linux_read_proc_meminfo(buffer, buffer_capacity, value_len, sys_errno) &
        bind(C, name="ftop_linux_read_proc_meminfo")
      import :: c_char, c_int, c_size_t
      character(kind=c_char), intent(out) :: buffer(*)
      integer(c_size_t), value :: buffer_capacity
      integer(c_size_t), intent(out) :: value_len
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_read_proc_meminfo

    integer(c_int) function c_ftop_linux_read_proc_loadavg(buffer, buffer_capacity, value_len, sys_errno) &
        bind(C, name="ftop_linux_read_proc_loadavg")
      import :: c_char, c_int, c_size_t
      character(kind=c_char), intent(out) :: buffer(*)
      integer(c_size_t), value :: buffer_capacity
      integer(c_size_t), intent(out) :: value_len
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_read_proc_loadavg

    integer(c_int) function c_ftop_linux_read_proc_uptime(buffer, buffer_capacity, value_len, sys_errno) &
        bind(C, name="ftop_linux_read_proc_uptime")
      import :: c_char, c_int, c_size_t
      character(kind=c_char), intent(out) :: buffer(*)
      integer(c_size_t), value :: buffer_capacity
      integer(c_size_t), intent(out) :: value_len
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_read_proc_uptime

    integer(c_int) function c_ftop_linux_read_proc_net_dev(buffer, buffer_capacity, value_len, sys_errno) &
        bind(C, name="ftop_linux_read_proc_net_dev")
      import :: c_char, c_int, c_size_t
      character(kind=c_char), intent(out) :: buffer(*)
      integer(c_size_t), value :: buffer_capacity
      integer(c_size_t), intent(out) :: value_len
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_read_proc_net_dev

    integer(c_int) function c_ftop_linux_read_proc_diskstats(buffer, buffer_capacity, value_len, sys_errno) &
        bind(C, name="ftop_linux_read_proc_diskstats")
      import :: c_char, c_int, c_size_t
      character(kind=c_char), intent(out) :: buffer(*)
      integer(c_size_t), value :: buffer_capacity
      integer(c_size_t), intent(out) :: value_len
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_read_proc_diskstats

    integer(c_int) function c_ftop_linux_read_proc_net_tcp(buffer, buffer_capacity, value_len, sys_errno) &
        bind(C, name="ftop_linux_read_proc_net_tcp")
      import :: c_char, c_int, c_size_t
      character(kind=c_char), intent(out) :: buffer(*)
      integer(c_size_t), value :: buffer_capacity
      integer(c_size_t), intent(out) :: value_len
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_read_proc_net_tcp

    integer(c_int) function c_ftop_linux_read_proc_net_udp(buffer, buffer_capacity, value_len, sys_errno) &
        bind(C, name="ftop_linux_read_proc_net_udp")
      import :: c_char, c_int, c_size_t
      character(kind=c_char), intent(out) :: buffer(*)
      integer(c_size_t), value :: buffer_capacity
      integer(c_size_t), intent(out) :: value_len
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_read_proc_net_udp

    integer(c_int) function c_ftop_linux_read_proc_net_tcp6(buffer, buffer_capacity, value_len, sys_errno) &
        bind(C, name="ftop_linux_read_proc_net_tcp6")
      import :: c_char, c_int, c_size_t
      character(kind=c_char), intent(out) :: buffer(*)
      integer(c_size_t), value :: buffer_capacity
      integer(c_size_t), intent(out) :: value_len
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_read_proc_net_tcp6

    integer(c_int) function c_ftop_linux_read_proc_net_udp6(buffer, buffer_capacity, value_len, sys_errno) &
        bind(C, name="ftop_linux_read_proc_net_udp6")
      import :: c_char, c_int, c_size_t
      character(kind=c_char), intent(out) :: buffer(*)
      integer(c_size_t), value :: buffer_capacity
      integer(c_size_t), intent(out) :: value_len
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_read_proc_net_udp6

    integer(c_int) function c_ftop_linux_socket_owners(owners, capacity, owner_count, sys_errno) &
        bind(C, name="ftop_linux_socket_owners")
      import :: c_int, c_size_t, linux_socket_owner
      type(linux_socket_owner), intent(out) :: owners(*)
      integer(c_size_t), value :: capacity
      integer(c_size_t), intent(out) :: owner_count
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_socket_owners

    integer(c_int) function c_ftop_linux_socket_traffic(traffic, capacity, traffic_count, sys_errno) &
        bind(C, name="ftop_linux_socket_traffic")
      import :: c_int, c_size_t, linux_socket_traffic
      type(linux_socket_traffic), intent(out) :: traffic(*)
      integer(c_size_t), value :: capacity
      integer(c_size_t), intent(out) :: traffic_count
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_socket_traffic

    integer(c_int) function c_ftop_linux_read_net_interface_file(interface_name, field_name, buffer, &
        buffer_capacity, value_len, sys_errno) bind(C, name="ftop_linux_read_net_interface_file")
      import :: c_char, c_int, c_size_t
      character(kind=c_char), intent(in) :: interface_name(*)
      character(kind=c_char), intent(in) :: field_name(*)
      character(kind=c_char), intent(out) :: buffer(*)
      integer(c_size_t), value :: buffer_capacity
      integer(c_size_t), intent(out) :: value_len
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_read_net_interface_file

    integer(c_int) function c_ftop_linux_read_cpu_frequency(cpu_index, buffer, buffer_capacity, value_len, sys_errno) &
        bind(C, name="ftop_linux_read_cpu_frequency")
      import :: c_char, c_int, c_size_t
      integer(c_int), value :: cpu_index
      character(kind=c_char), intent(out) :: buffer(*)
      integer(c_size_t), value :: buffer_capacity
      integer(c_size_t), intent(out) :: value_len
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_read_cpu_frequency

    integer(c_int) function c_ftop_linux_read_cpu_temperature(temperature_c, sys_errno) &
        bind(C, name="ftop_linux_read_cpu_temperature")
      import :: c_double, c_int
      real(c_double), intent(out) :: temperature_c
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_read_cpu_temperature

    integer(c_int) function c_ftop_linux_cpuinfo_field(name, value, value_capacity, value_len, sys_errno) &
        bind(C, name="ftop_linux_cpuinfo_field")
      import :: c_char, c_int, c_size_t
      character(kind=c_char), intent(in) :: name(*)
      character(kind=c_char), intent(out) :: value(*)
      integer(c_size_t), value :: value_capacity
      integer(c_size_t), intent(out) :: value_len
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_cpuinfo_field

    integer(c_int) function c_ftop_linux_cpuinfo_field_count(name, count, sys_errno) &
        bind(C, name="ftop_linux_cpuinfo_field_count")
      import :: c_char, c_int
      character(kind=c_char), intent(in) :: name(*)
      integer(c_int), intent(out) :: count
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_cpuinfo_field_count

    integer(c_int) function c_ftop_linux_hwmon_discover(sensors, capacity, sensor_count, sys_errno) &
        bind(C, name="ftop_linux_hwmon_discover")
      import :: c_int, c_size_t, linux_hwmon_sensor
      type(linux_hwmon_sensor), intent(out) :: sensors(*)
      integer(c_size_t), value :: capacity
      integer(c_size_t), intent(out) :: sensor_count
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_hwmon_discover

    integer(c_int) function c_ftop_linux_process_snapshot(processes, capacity, process_count, sys_errno) &
        bind(C, name="ftop_linux_process_snapshot")
      import :: c_int, c_size_t, linux_process_raw
      type(linux_process_raw), intent(out) :: processes(*)
      integer(c_size_t), value :: capacity
      integer(c_size_t), intent(out) :: process_count
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_process_snapshot

    integer(c_int) function c_ftop_linux_page_size(page_size, sys_errno) bind(C, name="ftop_linux_page_size")
      import :: c_int, c_long_long
      integer(c_long_long), intent(out) :: page_size
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_page_size

    integer(c_int) function c_ftop_linux_clock_ticks_per_second(clock_ticks, sys_errno) &
        bind(C, name="ftop_linux_clock_ticks_per_second")
      import :: c_int, c_long_long
      integer(c_long_long), intent(out) :: clock_ticks
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_clock_ticks_per_second

    integer(c_int) function c_ftop_linux_user_name(uid, value, value_capacity, value_len, sys_errno) &
        bind(C, name="ftop_linux_user_name")
      import :: c_char, c_int, c_size_t
      integer(c_int), value :: uid
      character(kind=c_char), intent(out) :: value(*)
      integer(c_size_t), value :: value_capacity
      integer(c_size_t), intent(out) :: value_len
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_user_name
  end interface

contains

  function create_platform() result(backend)
    class(platform_backend), allocatable :: backend

    allocate(backend, source=linux_backend())
  end function create_platform

  integer function linux_get_cpu_count(self) result(count)
    class(linux_backend), intent(in) :: self
    integer(c_int) :: c_count
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    associate(unused => self)
    end associate

    rc = c_ftop_linux_cpu_count(c_count, sys_errno)
    if (rc == 0_c_int) then
      count = int(c_count)
    else
      count = 0
    end if
  end function linux_get_cpu_count

  function linux_get_cpu_topology(self) result(info)
    class(linux_backend), intent(in) :: self
    type(cpu_topology_info) :: info
    integer :: core_count
    integer :: cores_per_package
    integer :: model_name_len
    integer :: package_count
    integer :: siblings_per_package
    integer :: thread_count

    thread_count = linux_get_cpu_count(self)
    if (thread_count <= 0) return

    core_count = thread_count
    if (linux_cpuinfo_int_field("cpu cores", cores_per_package) .and. cores_per_package > 0) then
      if (linux_cpuinfo_int_field("siblings", siblings_per_package) .and. siblings_per_package > 0) then
        package_count = max(1, (thread_count + siblings_per_package - 1) / siblings_per_package)
        core_count = cores_per_package * package_count
      else
        core_count = cores_per_package
      end if
    end if

    info%valid = .true.
    info%thread_count = thread_count
    info%core_count = max(1, min(thread_count, core_count))
    info%model_name_valid = linux_cpuinfo_field("model name", info%model_name, model_name_len) .and. model_name_len > 0
  end function linux_get_cpu_topology

  function linux_get_cpu_sample(self) result(sample)
    class(linux_backend), intent(in) :: self
    type(cpu_tick_sample) :: sample
    type(cpu_state_ticks) :: total
    type(cpu_state_ticks), allocatable :: cores(:)

    associate(unused => self)
    end associate

    if (linux_cpu_state_snapshot(total, cores) .and. total%valid) then
      sample%valid = .true.
      sample%total = cpu_state_total_ticks(total)
      sample%idle = max(0_int64, total%idle + total%iowait)
    end if
  end function linux_get_cpu_sample

  logical function linux_get_cpu_state_snapshot(self, total, cores) result(success)
    class(linux_backend), intent(in) :: self
    type(cpu_state_ticks), intent(out) :: total
    type(cpu_state_ticks), allocatable, intent(out) :: cores(:)

    associate(unused => self)
    end associate

    success = linux_cpu_state_snapshot(total, cores)
  end function linux_get_cpu_state_snapshot

  logical function linux_get_cpu_metadata(self, cores) result(success)
    class(linux_backend), intent(in) :: self
    type(cpu_core_info), allocatable, intent(out) :: cores(:)
    character(kind=c_char) :: c_buffer(LINUX_CPU_FREQ_BUFFER_LEN)
    character(len=:), allocatable :: buffer
    integer(c_size_t) :: value_len
    integer(c_int) :: sys_errno
    integer(c_int) :: rc
    real(c_double) :: temperature_c
    integer(int64) :: khz
    integer :: cpu_count
    integer :: cpu_index
    integer :: read_status

    cpu_count = max(0, linux_get_cpu_count(self))
    allocate(cores(cpu_count))
    do cpu_index = 1, cpu_count
      rc = c_ftop_linux_read_cpu_frequency(int(cpu_index - 1, c_int), c_buffer, int(size(c_buffer), c_size_t), &
                                           value_len, sys_errno)
      if (rc /= 0_c_int) cycle
      call c_chars_to_string(c_buffer, int(value_len), buffer)
      read(buffer, *, iostat=read_status) khz
      if (read_status /= 0 .or. khz <= 0_int64) cycle
      cores(cpu_index)%freq_valid = .true.
      cores(cpu_index)%freq_mhz = real(khz, real64) / 1000.0_real64
    end do
    rc = c_ftop_linux_read_cpu_temperature(temperature_c, sys_errno)
    if (rc == 0_c_int) then
      do cpu_index = 1, cpu_count
        cores(cpu_index)%temp_valid = .true.
        cores(cpu_index)%temp_c = real(temperature_c, real64)
      end do
    else
      call log_warn_once(linux_hwmon_warned, "hwmon sensor not found: errno=" // integer_text(int(sys_errno)))
    end if
    success = cpu_count > 0
  end function linux_get_cpu_metadata

  function linux_get_memory_info(self) result(info)
    class(linux_backend), intent(in) :: self
    type(memory_info) :: info
    type(metric_memory_info) :: metric_memory

    associate(unused => self)
    end associate

    if (linux_memory_snapshot(metric_memory) .and. metric_memory%valid) then
      info%valid = .true.
      info%total_bytes = metric_memory%total_bytes
      info%used_bytes = metric_memory%used_bytes
      info%free_bytes = metric_memory%free_bytes
      info%available_bytes = metric_memory%available_bytes
      info%cached_bytes = metric_memory%cached_bytes
      info%buffers_bytes = metric_memory%buffers_bytes
      info%swap_total_bytes = metric_memory%swap_total_bytes
      info%swap_used_bytes = metric_memory%swap_used_bytes
    end if
  end function linux_get_memory_info

  function linux_get_load_average(self) result(info)
    class(linux_backend), intent(in) :: self
    type(load_average_info) :: info

    associate(unused => self)
    end associate

    if (.not. linux_load_average_snapshot(info)) info = load_average_info()
  end function linux_get_load_average

  function linux_get_system_uptime(self) result(info)
    class(linux_backend), intent(in) :: self
    type(system_uptime_info) :: info
    character(kind=c_char) :: c_buffer(LINUX_PROC_UPTIME_BUFFER_LEN)
    character(len=:), allocatable :: buffer
    integer(c_size_t) :: value_len
    integer(c_int) :: sys_errno
    integer(c_int) :: rc
    integer :: read_status
    real(real64) :: seconds

    associate(unused => self)
    end associate

    rc = c_ftop_linux_read_proc_uptime(c_buffer, int(size(c_buffer), c_size_t), value_len, sys_errno)
    if (rc /= 0_c_int) return

    call c_chars_to_string(c_buffer, int(value_len), buffer)
    read(buffer, *, iostat=read_status) seconds
    if (read_status /= 0 .or. seconds < 0.0_real64) return

    info%valid = .true.
    info%seconds = int(seconds, int64)
  end function linux_get_system_uptime

  function linux_get_process_table(self) result(table)
    class(linux_backend), intent(in) :: self
    type(process_table) :: table
    type(memory_info) :: memory

    memory = linux_get_memory_info(self)
    if (.not. linux_process_snapshot(table, memory%total_bytes)) table = process_table()
  end function linux_get_process_table

  function linux_get_network_table(self) result(table)
    class(linux_backend), intent(in) :: self
    type(network_table) :: table

    associate(unused => self)
    end associate

    if (.not. linux_network_snapshot(table)) table = network_table()
  end function linux_get_network_table

  function linux_get_disk_table(self) result(table)
    class(linux_backend), intent(in) :: self
    type(disk_table) :: table

    associate(unused => self)
    end associate

    if (.not. linux_disk_snapshot(table)) table = disk_table()
  end function linux_get_disk_table

  function linux_get_gpu_table(self) result(table)
    class(linux_backend), intent(in) :: self
    type(gpu_table) :: table
    type(gpu_table) :: backend_table

    associate(unused => self)
    end associate

    table = empty_gpu_table()
    if (linux_nvml_gpu_snapshot(backend_table)) call append_gpu_table(table, backend_table)
    if (linux_amdgpu_snapshot(backend_table)) call append_gpu_table(table, backend_table)
    if (linux_intelgpu_snapshot(backend_table)) call append_gpu_table(table, backend_table)
  end function linux_get_gpu_table

  subroutine append_gpu_table(destination, source)
    type(gpu_table), intent(inout) :: destination
    type(gpu_table), intent(in) :: source
    type(gpu_info), allocatable :: combined(:)
    integer :: append_count
    integer :: destination_count
    integer :: source_index
    integer :: write_index

    if (.not. source%valid) return
    if (.not. allocated(source%gpus)) return

    destination_count = 0
    if (allocated(destination%gpus)) destination_count = size(destination%gpus)
    append_count = 0
    do source_index = 1, size(source%gpus)
      if (.not. source%gpus(source_index)%valid) cycle
      if (destination_count + append_count >= GPU_CAPACITY) exit
      append_count = append_count + 1
    end do
    if (append_count <= 0) return

    allocate(combined(destination_count + append_count))
    if (destination_count > 0) combined(1:destination_count) = destination%gpus
    write_index = destination_count
    do source_index = 1, size(source%gpus)
      if (.not. source%gpus(source_index)%valid) cycle
      if (write_index >= size(combined)) exit
      write_index = write_index + 1
      combined(write_index) = source%gpus(source_index)
    end do
    call move_alloc(combined, destination%gpus)
    destination%valid = .true.
  end subroutine append_gpu_table

  logical function linux_disk_snapshot(table, error_code) result(success)
    type(disk_table), intent(out) :: table
    integer, intent(out), optional :: error_code
    type(disk_io_info), allocatable :: io_devices(:)
    type(c_filesystem_info) :: filesystems(DISK_FILESYSTEM_CAPACITY)
    integer(c_int) :: filesystem_count
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    table = disk_table()
    filesystems = c_filesystem_info()
    rc = c_ftop_linux_filesystems(filesystems, int(DISK_FILESYSTEM_CAPACITY, c_int), filesystem_count, sys_errno)
    success = rc == 0_c_int
    if (success) then
      table = disk_table_from_c(filesystems, int(filesystem_count))
      if (linux_disk_io_snapshot(io_devices)) call move_alloc(io_devices, table%io)
    else
      if (present(error_code)) error_code = int(sys_errno)
    end if
  end function linux_disk_snapshot

  logical function linux_disk_io_snapshot(devices, error_code) result(success)
    type(disk_io_info), allocatable, intent(out) :: devices(:)
    integer, intent(out), optional :: error_code
    character(kind=c_char), allocatable :: c_buffer(:)
    character(len=:), allocatable :: buffer
    type(disk_io_info), allocatable :: parsed(:)
    integer(c_size_t) :: value_len
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    allocate(devices(0))
    allocate(c_buffer(LINUX_PROC_DISKSTATS_BUFFER_LEN))
    rc = c_ftop_linux_read_proc_diskstats(c_buffer, int(size(c_buffer), c_size_t), value_len, sys_errno)
    success = rc == 0_c_int
    if (.not. success) then
      if (present(error_code)) error_code = int(sys_errno)
      return
    end if

    call c_chars_to_string(c_buffer, int(value_len), buffer)
    success = linux_diskstats_parse(buffer, parsed)
    if (success) call linux_diskstats_filter_whole_devices(parsed, devices)
  end function linux_disk_io_snapshot

  logical function linux_network_snapshot(table, error_code) result(success)
    type(network_table), intent(out) :: table
    integer, intent(out), optional :: error_code
    character(kind=c_char), allocatable :: c_buffer(:)
    character(len=:), allocatable :: buffer
    type(process_bandwidth), allocatable :: bandwidth(:)
    type(net_connection), allocatable :: connections(:)
    integer(c_size_t) :: value_len
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    table = network_table()
    allocate(c_buffer(LINUX_PROC_NET_DEV_BUFFER_LEN))
    rc = c_ftop_linux_read_proc_net_dev(c_buffer, int(size(c_buffer), c_size_t), value_len, sys_errno)
    success = rc == 0_c_int
    if (success) then
      call c_chars_to_string(c_buffer, int(value_len), buffer)
      table = parse_linux_proc_net_dev(buffer)
      call assign_linux_interface_metadata(table)
      if (linux_connection_snapshot(connections)) then
        if (allocated(table%connections)) deallocate(table%connections)
        call move_alloc(connections, table%connections)
      end if
      if (linux_process_bandwidth_snapshot(bandwidth)) then
        if (allocated(table%processes)) deallocate(table%processes)
        call move_alloc(bandwidth, table%processes)
      end if
      success = table%valid
    end if
    call assign_error(error_code, sys_errno)
  end function linux_network_snapshot

  logical function linux_process_bandwidth_snapshot(processes, error_code) result(success)
    type(process_bandwidth), allocatable, intent(out) :: processes(:)
    integer, intent(out), optional :: error_code
    type(linux_socket_owner), allocatable :: owners(:)
    type(linux_socket_traffic), allocatable :: traffic(:)
    integer(c_size_t) :: owner_count
    integer(c_size_t) :: traffic_count
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    allocate(processes(0))
    allocate(owners(LINUX_SOCKET_OWNER_CAPACITY))
    rc = c_ftop_linux_socket_owners(owners, int(size(owners), c_size_t), owner_count, sys_errno)
    if (rc /= 0_c_int) then
      call assign_error(error_code, sys_errno)
      success = .false.
      return
    end if

    allocate(traffic(LINUX_SOCKET_TRAFFIC_CAPACITY))
    rc = c_ftop_linux_socket_traffic(traffic, int(size(traffic), c_size_t), traffic_count, sys_errno)
    if (rc /= 0_c_int) then
      call assign_error(error_code, sys_errno)
      success = .false.
      return
    end if

    processes = aggregate_linux_process_bandwidth(owners, int(owner_count), traffic, int(traffic_count))
    call assign_error(error_code, 0_c_int)
    success = .true.
  end function linux_process_bandwidth_snapshot

  logical function linux_connection_snapshot(connections, error_code) result(success)
    type(net_connection), allocatable, intent(out) :: connections(:)
    integer, intent(out), optional :: error_code
    character(kind=c_char), allocatable :: c_tcp6_buffer(:)
    character(kind=c_char), allocatable :: c_tcp_buffer(:)
    character(kind=c_char), allocatable :: c_udp6_buffer(:)
    character(kind=c_char), allocatable :: c_udp_buffer(:)
    character(len=:), allocatable :: tcp6_buffer
    character(len=:), allocatable :: tcp_buffer
    character(len=:), allocatable :: udp6_buffer
    character(len=:), allocatable :: udp_buffer
    integer(c_size_t) :: tcp6_value_len
    integer(c_size_t) :: tcp_value_len
    integer(c_size_t) :: udp6_value_len
    integer(c_size_t) :: udp_value_len
    integer(c_int) :: sys_errno
    integer(c_int) :: tcp6_rc
    integer(c_int) :: tcp_rc
    integer(c_int) :: udp6_rc
    integer(c_int) :: udp_rc

    allocate(connections(0))
    allocate(c_tcp_buffer(LINUX_PROC_NET_CONNECTION_BUFFER_LEN))
    allocate(c_udp_buffer(LINUX_PROC_NET_CONNECTION_BUFFER_LEN))
    allocate(c_tcp6_buffer(LINUX_PROC_NET_CONNECTION_BUFFER_LEN))
    allocate(c_udp6_buffer(LINUX_PROC_NET_CONNECTION_BUFFER_LEN))
    tcp6_value_len = 0_c_size_t
    udp6_value_len = 0_c_size_t
    tcp_rc = c_ftop_linux_read_proc_net_tcp(c_tcp_buffer, int(size(c_tcp_buffer), c_size_t), &
                                           tcp_value_len, sys_errno)
    if (tcp_rc /= 0_c_int) then
      call assign_error(error_code, sys_errno)
      success = .false.
      return
    end if
    udp_rc = c_ftop_linux_read_proc_net_udp(c_udp_buffer, int(size(c_udp_buffer), c_size_t), &
                                           udp_value_len, sys_errno)
    if (udp_rc /= 0_c_int) then
      call assign_error(error_code, sys_errno)
      success = .false.
      return
    end if
    tcp6_rc = c_ftop_linux_read_proc_net_tcp6(c_tcp6_buffer, int(size(c_tcp6_buffer), c_size_t), &
                                             tcp6_value_len, sys_errno)
    if (tcp6_rc /= 0_c_int) tcp6_value_len = 0_c_size_t
    udp6_rc = c_ftop_linux_read_proc_net_udp6(c_udp6_buffer, int(size(c_udp6_buffer), c_size_t), &
                                             udp6_value_len, sys_errno)
    if (udp6_rc /= 0_c_int) udp6_value_len = 0_c_size_t

    call c_chars_to_string(c_tcp_buffer, int(tcp_value_len), tcp_buffer)
    call c_chars_to_string(c_udp_buffer, int(udp_value_len), udp_buffer)
    call c_chars_to_string(c_tcp6_buffer, int(tcp6_value_len), tcp6_buffer)
    call c_chars_to_string(c_udp6_buffer, int(udp6_value_len), udp6_buffer)
    connections = parse_linux_proc_net_connections(tcp_buffer, udp_buffer, tcp6_buffer, udp6_buffer)
    call assign_linux_connection_owners(connections)
    call assign_error(error_code, 0_c_int)
    success = .true.
  end function linux_connection_snapshot

  subroutine assign_linux_connection_owners(connections)
    type(net_connection), intent(inout) :: connections(:)
    type(linux_socket_owner), allocatable :: owners(:)
    integer(c_size_t) :: owner_count
    integer(c_int) :: sys_errno
    integer(c_int) :: rc
    integer :: connection_index
    integer :: owner_index

    if (size(connections) <= 0) return
    allocate(owners(LINUX_SOCKET_OWNER_CAPACITY))
    rc = c_ftop_linux_socket_owners(owners, int(size(owners), c_size_t), owner_count, sys_errno)
    if (rc /= 0_c_int) return

    do connection_index = 1, size(connections)
      if (.not. connections(connection_index)%valid) cycle
      owner_index = matching_socket_owner(owners, int(owner_count), connections(connection_index)%inode)
      if (owner_index <= 0) cycle
      connections(connection_index)%pid = int(max(0_c_int, owners(owner_index)%pid))
      call copy_socket_owner_name(owners(owner_index), connections(connection_index)%process_name)
    end do
  end subroutine assign_linux_connection_owners

  function aggregate_linux_process_bandwidth(owners, owner_count, traffic, traffic_count) result(processes)
    type(linux_socket_owner), intent(in) :: owners(:)
    integer, intent(in) :: owner_count
    type(linux_socket_traffic), intent(in) :: traffic(:)
    integer, intent(in) :: traffic_count
    type(process_bandwidth), allocatable :: processes(:)
    integer :: owner_index
    integer :: process_index
    integer :: row_count
    integer :: traffic_index

    allocate(processes(max(0, min(traffic_count, size(traffic)))))
    row_count = 0
    do traffic_index = 1, max(0, min(traffic_count, size(traffic)))
      if (traffic(traffic_index)%inode <= 0_c_long_long) cycle
      if (traffic(traffic_index)%rx_bytes <= 0_c_long_long .and. traffic(traffic_index)%tx_bytes <= 0_c_long_long) cycle
      owner_index = matching_socket_owner(owners, owner_count, int(traffic(traffic_index)%inode, int64))
      if (owner_index <= 0) cycle
      if (owners(owner_index)%pid <= 0_c_int .or. owners(owner_index)%start_time <= 0_c_long_long) cycle

      process_index = matching_linux_process_bandwidth(processes, row_count, int(owners(owner_index)%pid), &
                                                       int(owners(owner_index)%start_time, int64))
      if (process_index <= 0) then
        if (row_count >= size(processes)) cycle
        row_count = row_count + 1
        process_index = row_count
        processes(process_index)%valid = .true.
        processes(process_index)%pid = int(owners(owner_index)%pid)
        processes(process_index)%start_time = int(owners(owner_index)%start_time, int64)
        call copy_socket_owner_name(owners(owner_index), processes(process_index)%process_name)
      end if
      processes(process_index)%rx_bytes = saturating_int64_add(processes(process_index)%rx_bytes, &
                                                              int(max(0_c_long_long, traffic(traffic_index)%rx_bytes), int64))
      processes(process_index)%tx_bytes = saturating_int64_add(processes(process_index)%tx_bytes, &
                                                              int(max(0_c_long_long, traffic(traffic_index)%tx_bytes), int64))
    end do
    call trim_process_bandwidth_rows(processes, row_count)
  end function aggregate_linux_process_bandwidth

  integer function matching_linux_process_bandwidth(processes, process_count, pid, start_time) result(process_index)
    type(process_bandwidth), intent(in) :: processes(:)
    integer, intent(in) :: process_count
    integer, intent(in) :: pid
    integer(int64), intent(in) :: start_time
    integer :: candidate

    process_index = 0
    if (pid <= 0 .or. start_time <= 0_int64) return
    do candidate = 1, max(0, min(process_count, size(processes)))
      if (.not. processes(candidate)%valid) cycle
      if (processes(candidate)%pid == pid .and. processes(candidate)%start_time == start_time) then
        process_index = candidate
        return
      end if
    end do
  end function matching_linux_process_bandwidth

  subroutine trim_process_bandwidth_rows(processes, row_count)
    type(process_bandwidth), allocatable, intent(inout) :: processes(:)
    integer, intent(in) :: row_count
    type(process_bandwidth), allocatable :: trimmed(:)
    integer :: kept_count

    kept_count = max(0, min(row_count, size(processes)))
    allocate(trimmed(kept_count))
    if (kept_count > 0) trimmed = processes(:kept_count)
    call move_alloc(trimmed, processes)
  end subroutine trim_process_bandwidth_rows

  integer(int64) function saturating_int64_add(left, right) result(value)
    integer(int64), intent(in) :: left
    integer(int64), intent(in) :: right

    if (right > huge(value) - max(0_int64, left)) then
      value = huge(value)
    else
      value = max(0_int64, left) + max(0_int64, right)
    end if
  end function saturating_int64_add

  integer function matching_socket_owner(owners, owner_count, inode) result(owner_index)
    type(linux_socket_owner), intent(in) :: owners(:)
    integer, intent(in) :: owner_count
    integer(int64), intent(in) :: inode
    integer :: candidate
    integer :: limit

    owner_index = 0
    if (inode <= 0_int64) return
    limit = max(0, min(owner_count, size(owners)))
    do candidate = 1, limit
      if (int(max(0_c_long_long, owners(candidate)%inode), int64) == inode) then
        owner_index = candidate
        return
      end if
    end do
  end function matching_socket_owner

  subroutine copy_socket_owner_name(owner, process_name)
    type(linux_socket_owner), intent(in) :: owner
    character(len=*), intent(out) :: process_name
    integer :: name_len

    call copy_c_line_value(owner%process_name, size(owner%process_name), process_name, name_len)
  end subroutine copy_socket_owner_name

  subroutine assign_linux_interface_metadata(table)
    type(network_table), intent(inout) :: table
    integer :: interface_index

    if (.not. allocated(table%interfaces)) return
    do interface_index = 1, size(table%interfaces)
      if (.not. table%interfaces(interface_index)%valid) cycle
      call assign_linux_interface_state(table%interfaces(interface_index))
      call assign_linux_interface_integer_field(table%interfaces(interface_index), "speed", &
                                                table%interfaces(interface_index)%speed_mbps)
      call assign_linux_interface_integer_field(table%interfaces(interface_index), "mtu", &
                                                table%interfaces(interface_index)%mtu)
    end do
  end subroutine assign_linux_interface_metadata

  subroutine assign_linux_interface_state(interface)
    type(interface_info), intent(inout) :: interface
    character(len=LINUX_NET_IFACE_FIELD_BUFFER_LEN) :: value
    integer :: value_len

    if (.not. linux_net_interface_field(interface%name, "operstate", value, value_len)) return
    if (value_len <= 0) return
    interface%state = bounded_text(value(:value_len), len(interface%state))
  end subroutine assign_linux_interface_state

  subroutine assign_linux_interface_integer_field(interface, field_name, destination)
    type(interface_info), intent(in) :: interface
    character(len=*), intent(in) :: field_name
    integer, intent(inout) :: destination
    character(len=LINUX_NET_IFACE_FIELD_BUFFER_LEN) :: value
    integer :: read_status
    integer :: parsed_value
    integer :: value_len

    if (.not. linux_net_interface_field(interface%name, field_name, value, value_len)) return
    if (value_len <= 0) return
    read(value(:value_len), *, iostat=read_status) parsed_value
    if (read_status == 0 .and. parsed_value > 0) destination = parsed_value
  end subroutine assign_linux_interface_integer_field

  logical function linux_net_interface_field(interface_name, field_name, value, value_len, error_code) result(success)
    character(len=*), intent(in) :: interface_name
    character(len=*), intent(in) :: field_name
    character(len=*), intent(out) :: value
    integer, intent(out) :: value_len
    integer, intent(out), optional :: error_code
    character(kind=c_char), allocatable :: c_field_name(:)
    character(kind=c_char), allocatable :: c_interface_name(:)
    character(kind=c_char), allocatable :: c_value(:)
    integer(c_size_t) :: c_value_len
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    value = ""
    value_len = 0
    if (len(value) <= 0) then
      call assign_error(error_code, 0_c_int)
      success = .false.
      return
    end if

    call to_c_string(interface_name, c_interface_name)
    call to_c_string(field_name, c_field_name)
    allocate(c_value(len(value) + 1))
    rc = c_ftop_linux_read_net_interface_file(c_interface_name, c_field_name, c_value, &
                                              int(size(c_value), c_size_t), c_value_len, sys_errno)
    success = rc == 0_c_int
    if (success) call copy_c_line_value(c_value, int(c_value_len), value, value_len)
    call assign_error(error_code, sys_errno)
  end function linux_net_interface_field

  logical function linux_process_snapshot(table, memory_total_bytes, error_code) result(success)
    type(process_table), intent(out) :: table
    integer(int64), intent(in), optional :: memory_total_bytes
    integer, intent(out), optional :: error_code
    type(linux_process_raw), allocatable :: raw_processes(:)
    integer(c_long_long) :: clock_ticks_per_second
    integer(c_long_long) :: page_size
    integer(c_size_t) :: c_process_count
    integer(c_int) :: sys_errno
    integer(c_int) :: rc
    integer :: process_count
    integer :: process_index

    table = process_table()
    allocate(raw_processes(LINUX_PROCESS_CAPACITY))
    rc = c_ftop_linux_process_snapshot(raw_processes, int(size(raw_processes), c_size_t), c_process_count, sys_errno)
    success = rc == 0_c_int
    if (.not. success) then
      allocate(table%items(0))
      call assign_error(error_code, sys_errno)
      return
    end if

    page_size = linux_page_size()
    clock_ticks_per_second = linux_clock_ticks_per_second()
    process_count = max(0, min(int(c_process_count), size(raw_processes)))
    allocate(table%items(process_count))
    table%valid = .true.
    do process_index = 1, process_count
      table%items(process_index) = linux_process_from_raw(raw_processes(process_index), page_size, &
                                                          clock_ticks_per_second, memory_total_bytes)
    end do
    call assign_error(error_code, sys_errno)
  end function linux_process_snapshot

  function linux_process_from_raw(raw, page_size, clock_ticks_per_second, memory_total_bytes) result(process)
    type(linux_process_raw), intent(in) :: raw
    integer(c_long_long), intent(in) :: page_size
    integer(c_long_long), intent(in) :: clock_ticks_per_second
    integer(int64), intent(in), optional :: memory_total_bytes
    type(process_info) :: process
    character(len=:), allocatable :: cmdline
    character(len=:), allocatable :: cgroup_text
    character(len=:), allocatable :: io_text
    character(len=:), allocatable :: stat_text
    character(len=:), allocatable :: status_text

    process%pid = int(raw%pid)
    process%valid = process%pid > 0
    call c_chars_to_string(raw%stat, int(raw%stat_len), stat_text)
    call c_chars_to_string(raw%status, int(raw%status_len), status_text)
    call c_cmdline_to_string(raw%cmdline, int(raw%cmdline_len), cmdline)
    call c_chars_to_string(raw%io, int(raw%io_len), io_text)
    call c_chars_to_string(raw%cgroup, int(raw%cgroup_len), cgroup_text)

    call parse_linux_process_stat(stat_text, page_size, clock_ticks_per_second, process)
    call parse_linux_process_status(status_text, process)
    call parse_linux_process_io(io_text, process)
    call assign_linux_process_cgroup(cgroup_text, process)
    call assign_process_user(process)
    if (len_trim(cmdline) > 0) process%command = bounded_text(cmdline, len(process%command))
    if (len_trim(process%command) == 0) process%command = process%name
    if (present(memory_total_bytes)) call assign_memory_percent(process, memory_total_bytes)
  end function linux_process_from_raw

  subroutine parse_linux_process_stat(stat_text, page_size, clock_ticks_per_second, process)
    character(len=*), intent(in) :: stat_text
    integer(c_long_long), intent(in) :: page_size
    integer(c_long_long), intent(in) :: clock_ticks_per_second
    type(process_info), intent(inout) :: process
    character(len=:), allocatable :: remainder
    character(len=1) :: state
    integer :: close_paren
    integer :: open_paren
    integer :: read_status
    integer(int64) :: cmajflt
    integer(int64) :: cminflt
    integer(int64) :: cstime
    integer(int64) :: cutime
    integer(int64) :: flags
    integer(int64) :: itrealvalue
    integer(int64) :: majflt
    integer(int64) :: minflt
    integer(int64) :: nice
    integer(int64) :: num_threads
    integer(int64) :: pgrp
    integer(int64) :: ppid
    integer(int64) :: priority
    integer(int64) :: rss_pages
    integer(int64) :: session
    integer(int64) :: starttime
    integer(int64) :: stime
    integer(int64) :: tpgid
    integer(int64) :: tty_nr
    integer(int64) :: utime
    integer(int64) :: vsize

    open_paren = index(stat_text, "(")
    close_paren = last_index(stat_text, ")")
    if (open_paren <= 0 .or. close_paren <= open_paren) return

    process%name = bounded_text(stat_text(open_paren + 1:close_paren - 1), len(process%name))
    if (close_paren + 2 > len(stat_text)) return

    remainder = stat_text(close_paren + 2:)
    state = "?"
    read(remainder, *, iostat=read_status) state, ppid, pgrp, session, tty_nr, tpgid, flags, &
      minflt, cminflt, majflt, cmajflt, utime, stime, cutime, cstime, priority, nice, num_threads, &
      itrealvalue, starttime, vsize, rss_pages
    if (read_status /= 0) return

    process%state = linux_state_label(state)
    process%ppid = int(max(0_int64, ppid))
    process%threads = int(max(0_int64, num_threads))
    process%nice = int(nice)
    process%priority = int(priority)
    process%start_time = max(0_int64, starttime)
    process%cpu_time = ticks_to_milliseconds(max(0_int64, utime + stime), clock_ticks_per_second)
    process%mem_virt_bytes = max(0_int64, vsize)
    process%mem_rss_bytes = max(0_int64, rss_pages) * int(max(0_c_long_long, page_size), int64)
  end subroutine parse_linux_process_stat

  subroutine parse_linux_process_status(status_text, process)
    character(len=*), intent(in) :: status_text
    type(process_info), intent(inout) :: process
    integer :: value

    if (linux_status_int(status_text, "Uid:", value)) process%uid = max(0, value)
    if (linux_status_int(status_text, "Threads:", value)) process%threads = max(0, value)
    if (linux_status_int(status_text, "RssShmem:", value)) then
      process%mem_shared_bytes = int(max(0, value), int64) * 1024_int64
    end if
  end subroutine parse_linux_process_status

  subroutine parse_linux_process_io(io_text, process)
    character(len=*), intent(in) :: io_text
    type(process_info), intent(inout) :: process
    integer(int64) :: value

    if (linux_text_int64(io_text, "read_bytes:", value)) process%io_read_bytes = max(0_int64, value)
    if (linux_text_int64(io_text, "write_bytes:", value)) process%io_write_bytes = max(0_int64, value)
  end subroutine parse_linux_process_io

  subroutine assign_linux_process_cgroup(cgroup_text, process)
    character(len=*), intent(in) :: cgroup_text
    type(process_info), intent(inout) :: process
    character(len=:), allocatable :: line
    character(len=:), allocatable :: path
    integer :: line_end
    integer :: line_start
    integer :: separator

    line_start = 1
    do while (line_start <= len(cgroup_text))
      line_end = line_start
      do while (line_end <= len(cgroup_text) .and. cgroup_text(line_end:line_end) /= new_line("a"))
        line_end = line_end + 1
      end do
      line = cgroup_text(line_start:max(line_start, line_end - 1))
      separator = last_index(line, ":")
      if (separator > 0 .and. separator < len_trim(line)) then
        path = trim(line(separator + 1:))
        if (len_trim(path) > 0) then
          process%cgroup = bounded_text(path, PROCESS_CGROUP_LEN)
          return
        end if
      end if
      line_start = line_end + 1
    end do
  end subroutine assign_linux_process_cgroup

  logical function linux_status_int(status_text, key, value) result(found)
    character(len=*), intent(in) :: status_text
    character(len=*), intent(in) :: key
    integer, intent(out) :: value
    character(len=:), allocatable :: line
    integer :: line_end
    integer :: line_start
    integer :: read_status

    value = 0
    found = .false.
    line_start = 1
    do while (line_start <= len(status_text))
      line_end = line_start
      do while (line_end <= len(status_text) .and. status_text(line_end:line_end) /= new_line("a"))
        line_end = line_end + 1
      end do
      line = status_text(line_start:max(line_start, line_end - 1))
      if (len_trim(line) > len(key)) then
        if (line(1:len(key)) == key) then
          read(line(len(key) + 1:), *, iostat=read_status) value
          found = read_status == 0
          return
        end if
      end if
      line_start = line_end + 1
    end do
  end function linux_status_int

  logical function linux_text_int64(text, key, value) result(found)
    character(len=*), intent(in) :: text
    character(len=*), intent(in) :: key
    integer(int64), intent(out) :: value
    character(len=:), allocatable :: line
    integer :: line_end
    integer :: line_start
    integer :: read_status

    value = 0_int64
    found = .false.
    line_start = 1
    do while (line_start <= len(text))
      line_end = line_start
      do while (line_end <= len(text) .and. text(line_end:line_end) /= new_line("a"))
        line_end = line_end + 1
      end do
      line = text(line_start:max(line_start, line_end - 1))
      if (len_trim(line) > len(key)) then
        if (line(1:len(key)) == key) then
          read(line(len(key) + 1:), *, iostat=read_status) value
          found = read_status == 0
          return
        end if
      end if
      line_start = line_end + 1
    end do
  end function linux_text_int64

  function linux_state_label(state) result(label)
    character(len=1), intent(in) :: state
    character(len=:), allocatable :: label

    select case (state)
    case ("R", "S", "D", "T", "Z", "I")
      label = state
    case default
      label = "?"
    end select
  end function linux_state_label

  subroutine assign_memory_percent(process, memory_total_bytes)
    type(process_info), intent(inout) :: process
    integer(int64), intent(in) :: memory_total_bytes

    if (memory_total_bytes <= 0_int64) return
    process%mem_percent = 100.0_real64 * real(max(0_int64, process%mem_rss_bytes), real64) / &
                          real(memory_total_bytes, real64)
  end subroutine assign_memory_percent

  subroutine assign_process_user(process)
    type(process_info), intent(inout) :: process
    character(kind=c_char) :: c_value(PROCESS_USER_LEN + 1)
    character(len=:), allocatable :: user_name
    integer(c_size_t) :: value_len
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    if (cached_process_user(process%uid, process%user)) then
      process%user_valid = .true.
      return
    end if

    rc = c_ftop_linux_user_name(int(process%uid, c_int), c_value, int(size(c_value), c_size_t), value_len, sys_errno)
    if (rc /= 0_c_int .or. value_len <= 0_c_size_t) return
    call c_chars_to_string(c_value, int(value_len), user_name)
    process%user = bounded_text(user_name, len(process%user))
    process%user_valid = len_trim(process%user) > 0
    if (process%user_valid) call cache_process_user(process%uid, process%user)
  end subroutine assign_process_user

  logical function cached_process_user(uid, user) result(found)
    integer, intent(in) :: uid
    character(len=*), intent(out) :: user
    integer :: cache_index

    user = ""
    found = .false.
    if (.not. allocated(user_cache_uids)) return
    do cache_index = 1, size(user_cache_uids)
      if (user_cache_uids(cache_index) == uid) then
        user = user_cache_names(cache_index)
        found = len_trim(user) > 0
        return
      end if
    end do
  end function cached_process_user

  subroutine cache_process_user(uid, user)
    integer, intent(in) :: uid
    character(len=*), intent(in) :: user
    integer, allocatable :: next_uids(:)
    character(len=PROCESS_USER_LEN), allocatable :: next_names(:)
    integer :: cache_size

    if (len_trim(user) <= 0) return
    if (allocated(user_cache_uids)) then
      if (any(user_cache_uids == uid)) return
      cache_size = size(user_cache_uids)
    else
      cache_size = 0
    end if

    allocate(next_uids(cache_size + 1))
    allocate(next_names(cache_size + 1))
    if (cache_size > 0) then
      next_uids(1:cache_size) = user_cache_uids
      next_names(1:cache_size) = user_cache_names
    end if
    next_uids(cache_size + 1) = uid
    next_names(cache_size + 1) = bounded_text(user, PROCESS_USER_LEN)
    call move_alloc(next_uids, user_cache_uids)
    call move_alloc(next_names, user_cache_names)
  end subroutine cache_process_user

  integer(c_long_long) function linux_page_size() result(page_size)
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    rc = c_ftop_linux_page_size(page_size, sys_errno)
    if (rc /= 0_c_int .or. page_size <= 0_c_long_long) page_size = 4096_c_long_long
  end function linux_page_size

  integer(c_long_long) function linux_clock_ticks_per_second() result(clock_ticks)
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    rc = c_ftop_linux_clock_ticks_per_second(clock_ticks, sys_errno)
    if (rc /= 0_c_int .or. clock_ticks <= 0_c_long_long) clock_ticks = 100_c_long_long
  end function linux_clock_ticks_per_second

  integer(int64) function ticks_to_milliseconds(ticks, clock_ticks_per_second) result(milliseconds)
    integer(int64), intent(in) :: ticks
    integer(c_long_long), intent(in) :: clock_ticks_per_second

    milliseconds = 0_int64
    if (ticks <= 0_int64 .or. clock_ticks_per_second <= 0_c_long_long) return
    milliseconds = (ticks * 1000_int64) / int(clock_ticks_per_second, int64)
  end function ticks_to_milliseconds

  logical function linux_cpu_state_snapshot(total, cores, error_code) result(success)
    type(cpu_state_ticks), intent(out) :: total
    type(cpu_state_ticks), allocatable, intent(out) :: cores(:)
    integer, intent(out), optional :: error_code
    character(kind=c_char), allocatable :: c_buffer(:)
    character(len=:), allocatable :: buffer
    integer(c_size_t) :: value_len
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    total = cpu_state_ticks()
    if (allocated(cores)) deallocate(cores)
    allocate(c_buffer(LINUX_PROC_STAT_BUFFER_LEN))
    rc = c_ftop_linux_read_proc_stat(c_buffer, int(size(c_buffer), c_size_t), value_len, sys_errno)
    success = rc == 0_c_int
    if (success) then
      call c_chars_to_string(c_buffer, int(value_len), buffer)
      success = linux_proc_stat_parse(buffer, total, cores)
    end if
    call assign_error(error_code, sys_errno)
  end function linux_cpu_state_snapshot

  logical function linux_cpuinfo_int_field(name, value) result(success)
    character(len=*), intent(in) :: name
    integer, intent(out) :: value
    character(len=64) :: buffer
    integer :: read_status
    integer :: value_len

    value = 0
    success = linux_cpuinfo_field(name, buffer, value_len)
    if (.not. success .or. value_len <= 0) then
      success = .false.
      return
    end if

    read(buffer(1:value_len), *, iostat=read_status) value
    success = read_status == 0
    if (.not. success) value = 0
  end function linux_cpuinfo_int_field

  logical function linux_memory_snapshot(info, error_code) result(success)
    type(metric_memory_info), intent(out) :: info
    integer, intent(out), optional :: error_code
    character(kind=c_char), allocatable :: c_buffer(:)
    character(len=:), allocatable :: buffer
    integer(c_size_t) :: value_len
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    info = metric_memory_info()
    allocate(c_buffer(LINUX_PROC_MEMINFO_BUFFER_LEN))
    rc = c_ftop_linux_read_proc_meminfo(c_buffer, int(size(c_buffer), c_size_t), value_len, sys_errno)
    success = rc == 0_c_int
    if (success) then
      call c_chars_to_string(c_buffer, int(value_len), buffer)
      success = linux_meminfo_parse(buffer, info)
    end if
    call assign_error(error_code, sys_errno)
  end function linux_memory_snapshot

  logical function linux_load_average_snapshot(info, error_code) result(success)
    type(load_average_info), intent(out) :: info
    integer, intent(out), optional :: error_code
    character(kind=c_char), allocatable :: c_buffer(:)
    character(len=:), allocatable :: buffer
    integer(c_size_t) :: value_len
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    info = load_average_info()
    allocate(c_buffer(LINUX_PROC_LOADAVG_BUFFER_LEN))
    rc = c_ftop_linux_read_proc_loadavg(c_buffer, int(size(c_buffer), c_size_t), value_len, sys_errno)
    success = rc == 0_c_int
    if (success) then
      call c_chars_to_string(c_buffer, int(value_len), buffer)
      success = linux_loadavg_parse(buffer, info)
    end if
    call assign_error(error_code, sys_errno)
  end function linux_load_average_snapshot

  logical function linux_cpuinfo_field(name, value, value_len, error_code) result(success)
    character(len=*), intent(in) :: name
    character(len=*), intent(out) :: value
    integer, intent(out), optional :: value_len
    integer, intent(out), optional :: error_code
    character(kind=c_char), allocatable :: c_name(:)
    character(kind=c_char), allocatable :: c_value(:)
    integer(c_size_t) :: c_value_len
    integer(c_int) :: sys_errno
    integer(c_int) :: rc
    integer :: copied_len
    integer :: i
    integer :: limit

    value = ""
    if (present(value_len)) value_len = 0
    if (len(value) <= 0) then
      call assign_error(error_code, 0_c_int)
      success = .false.
      return
    end if

    call to_c_string(name, c_name)
    allocate(c_value(len(value) + 1))
    rc = c_ftop_linux_cpuinfo_field(c_name, c_value, int(size(c_value), c_size_t), c_value_len, sys_errno)
    success = rc == 0_c_int
    if (success) then
      copied_len = 0
      limit = min(len(value), int(c_value_len))
      do i = 1, limit
        if (c_value(i) == c_null_char) exit
        value(i:i) = achar(iachar(c_value(i)))
        copied_len = copied_len + 1
      end do
      if (present(value_len)) value_len = copied_len
    end if
    call assign_error(error_code, sys_errno)
  end function linux_cpuinfo_field

  logical function linux_cpuinfo_field_count(name, count, error_code) result(success)
    character(len=*), intent(in) :: name
    integer, intent(out) :: count
    integer, intent(out), optional :: error_code
    character(kind=c_char), allocatable :: c_name(:)
    integer(c_int) :: c_count
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    call to_c_string(name, c_name)
    rc = c_ftop_linux_cpuinfo_field_count(c_name, c_count, sys_errno)
    success = rc == 0_c_int
    if (success) then
      count = int(c_count)
    else
      count = 0
    end if
    call assign_error(error_code, sys_errno)
  end function linux_cpuinfo_field_count

  logical function linux_hwmon_discover(sensors, sensor_count, error_code) result(success)
    type(linux_hwmon_sensor), intent(out) :: sensors(:)
    integer, intent(out) :: sensor_count
    integer, intent(out), optional :: error_code
    integer(c_size_t) :: c_sensor_count
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    sensor_count = 0
    if (size(sensors) <= 0) then
      call assign_error(error_code, 0_c_int)
      success = .false.
      return
    end if

    rc = c_ftop_linux_hwmon_discover(sensors, int(size(sensors), c_size_t), c_sensor_count, sys_errno)
    success = rc == 0_c_int
    if (success) sensor_count = int(c_sensor_count)
    call assign_error(error_code, sys_errno)
  end function linux_hwmon_discover

  subroutine to_c_string(text, buffer)
    character(len=*), intent(in) :: text
    character(kind=c_char), allocatable, intent(out) :: buffer(:)
    integer :: i
    integer :: text_len

    text_len = len_trim(text)
    allocate(buffer(text_len + 1))
    do i = 1, text_len
      buffer(i) = char(iachar(text(i:i)), kind=c_char)
    end do
    buffer(text_len + 1) = c_null_char
  end subroutine to_c_string

  subroutine c_chars_to_string(c_buffer, value_len, text)
    character(kind=c_char), intent(in) :: c_buffer(:)
    integer, intent(in) :: value_len
    character(len=:), allocatable, intent(out) :: text
    integer :: i
    integer :: copied_len

    copied_len = max(0, min(value_len, size(c_buffer)))
    allocate(character(len=copied_len) :: text)
    do i = 1, copied_len
      text(i:i) = achar(iachar(c_buffer(i)))
    end do
  end subroutine c_chars_to_string

  subroutine copy_c_line_value(c_buffer, c_value_len, value, value_len)
    character(kind=c_char), intent(in) :: c_buffer(:)
    integer, intent(in) :: c_value_len
    character(len=*), intent(out) :: value
    integer, intent(out) :: value_len
    integer :: i
    integer :: limit

    value = ""
    value_len = 0
    limit = min(len(value), max(0, min(c_value_len, size(c_buffer))))
    do i = 1, limit
      if (c_buffer(i) == c_null_char) exit
      if (achar(iachar(c_buffer(i))) == new_line("a")) exit
      if (achar(iachar(c_buffer(i))) == achar(13)) exit
      value(i:i) = achar(iachar(c_buffer(i)))
      value_len = value_len + 1
    end do
  end subroutine copy_c_line_value

  subroutine c_cmdline_to_string(c_buffer, value_len, text)
    character(kind=c_char), intent(in) :: c_buffer(:)
    integer, intent(in) :: value_len
    character(len=:), allocatable, intent(out) :: text
    integer :: copied_len
    integer :: i

    copied_len = max(0, min(value_len, size(c_buffer)))
    allocate(character(len=copied_len) :: text)
    do i = 1, copied_len
      if (c_buffer(i) == c_null_char) then
        text(i:i) = " "
      else
        text(i:i) = achar(iachar(c_buffer(i)))
      end if
    end do
    text = trim(adjustl(text))
  end subroutine c_cmdline_to_string

  integer function last_index(text, needle) result(position)
    character(len=*), intent(in) :: text
    character(len=*), intent(in) :: needle
    integer :: candidate
    integer :: offset

    position = 0
    if (len(needle) <= 0) return
    offset = 0
    do
      candidate = index(text(offset + 1:), needle)
      if (candidate <= 0) exit
      position = offset + candidate
      offset = position
      if (offset >= len(text)) exit
    end do
  end function last_index

  function bounded_text(source, capacity) result(text)
    character(len=*), intent(in) :: source
    integer, intent(in) :: capacity
    character(len=:), allocatable :: text
    integer :: copied_len

    copied_len = max(0, min(len_trim(source), capacity))
    if (copied_len <= 0) then
      text = ""
    else
      text = source(1:copied_len)
    end if
  end function bounded_text

  subroutine assign_error(error_code, sys_errno)
    integer, intent(out), optional :: error_code
    integer(c_int), intent(in) :: sys_errno

    if (present(error_code)) error_code = int(sys_errno)
  end subroutine assign_error

  subroutine log_warn_once(warned, message)
    logical, intent(inout) :: warned
    character(len=*), intent(in) :: message

    if (warned) return
    call log_warn(message)
    warned = .true.
  end subroutine log_warn_once

  function integer_text(value) result(text)
    integer, intent(in) :: value
    character(len=:), allocatable :: text
    character(len=32) :: buffer

    write(buffer, '(i0)') value
    text = trim(buffer)
  end function integer_text

end module ftop_platform
