module ftop_platform
  use, intrinsic :: iso_c_binding, only : &
    c_associated, &
    c_char, &
    c_double, &
    c_int, &
    c_long, &
    c_long_long, &
    c_null_char, &
    c_null_ptr, &
    c_ptr, &
    c_size_t
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use ftop_cpu_data, only : cpu_core_info, cpu_state_ticks, cpu_state_total_ticks
  use ftop_net_data, only : &
    NET_ADDRESS_LEN, &
    NET_INTERFACE_NAME_LEN, &
    NET_PROCESS_NAME_LEN, &
    NET_PROTOCOL_LEN, &
    NET_STATE_LEN, &
    interface_info, &
    net_connection, &
    network_table
  use ftop_platform_types, only : &
    cpu_tick_sample, &
    cpu_topology_info, &
    cpu_usage_percent, &
    load_average_info, &
    memory_info, &
    platform_backend, &
    process_table, &
    system_uptime_info
  use ftop_proc_data, only : &
    PROCESS_NAME_LEN, &
    PROCESS_USER_LEN, &
    process_info
  implicit none
  private

  integer, parameter, public :: FREEBSD_PROCESS_COMMAND_LEN = 32
  integer, parameter, public :: FREEBSD_DEVSTAT_NAME_LEN = 16
  integer, parameter :: FREEBSD_PROCESS_CAPACITY = 4096
  integer, parameter :: FREEBSD_NETWORK_INTERFACE_CAPACITY = 128
  integer, parameter :: FREEBSD_NETWORK_CONNECTION_CAPACITY = 2048

  integer, allocatable, save :: user_cache_uids(:)
  character(len=PROCESS_USER_LEN), allocatable, save :: user_cache_names(:)

  type, bind(C), public :: freebsd_process_info
    integer(c_int) :: pid
    integer(c_int) :: ppid
    integer(c_int) :: uid
    integer(c_int) :: state
    integer(c_int) :: jid
    character(kind=c_char) :: command(FREEBSD_PROCESS_COMMAND_LEN)
    integer(c_long_long) :: mem_rss_bytes
    integer(c_long_long) :: mem_virt_bytes
    integer(c_int) :: threads
    integer(c_int) :: nice
    integer(c_int) :: priority
    integer(c_long_long) :: start_time
    integer(c_long_long) :: cpu_time
  end type freebsd_process_info

  type, public :: freebsd_kvm_handle
    type(c_ptr) :: handle = c_null_ptr
  end type freebsd_kvm_handle

  type, bind(C), public :: freebsd_devstat_info
    integer(c_int) :: device_number
    integer(c_int) :: unit_number
    integer(c_int) :: device_type
    integer(c_int) :: priority
    integer(c_int) :: block_size
    integer(c_long_long) :: bytes_read
    integer(c_long_long) :: bytes_written
    integer(c_long_long) :: bytes_freed
    integer(c_long_long) :: transfers_read
    integer(c_long_long) :: transfers_written
    integer(c_long_long) :: transfers_freed
    character(kind=c_char) :: name(FREEBSD_DEVSTAT_NAME_LEN)
  end type freebsd_devstat_info

  type, bind(C), public :: freebsd_net_interface_info
    integer(c_int) :: valid
    character(kind=c_char) :: name(NET_INTERFACE_NAME_LEN)
    integer(c_long_long) :: rx_bytes
    integer(c_long_long) :: tx_bytes
    integer(c_long_long) :: rx_packets
    integer(c_long_long) :: tx_packets
    character(kind=c_char) :: state(NET_STATE_LEN)
    integer(c_int) :: speed_mbps
    integer(c_int) :: mtu
  end type freebsd_net_interface_info

  type, bind(C), public :: freebsd_net_connection_info
    integer(c_int) :: valid
    character(kind=c_char) :: protocol(NET_PROTOCOL_LEN)
    character(kind=c_char) :: local_addr(NET_ADDRESS_LEN)
    integer(c_int) :: local_port
    character(kind=c_char) :: remote_addr(NET_ADDRESS_LEN)
    integer(c_int) :: remote_port
    character(kind=c_char) :: state(NET_STATE_LEN)
    integer(c_long_long) :: socket_id
    integer(c_int) :: pid
    character(kind=c_char) :: process_name(NET_PROCESS_NAME_LEN)
  end type freebsd_net_connection_info

  type, bind(C) :: freebsd_cpu_state_tick_sample
    integer(c_long_long) :: user
    integer(c_long_long) :: nice
    integer(c_long_long) :: system
    integer(c_long_long) :: idle
    integer(c_long_long) :: irq
  end type freebsd_cpu_state_tick_sample

  type, extends(platform_backend) :: freebsd_backend
  contains
    procedure :: get_cpu_count => freebsd_get_cpu_count
    procedure :: get_cpu_topology => freebsd_get_cpu_topology
    procedure :: get_cpu_sample => freebsd_get_cpu_sample
    procedure :: get_cpu_state_snapshot => freebsd_get_cpu_state_snapshot
    procedure :: get_cpu_metadata => freebsd_get_cpu_metadata
    procedure :: get_memory_info => freebsd_get_memory_info
    procedure :: get_load_average => freebsd_get_load_average
    procedure :: get_system_uptime => freebsd_get_system_uptime
    procedure :: get_process_table => freebsd_get_process_table
    procedure :: get_network_table => freebsd_get_network_table
  end type freebsd_backend

  public :: create_platform
  public :: cpu_tick_sample
  public :: cpu_topology_info
  public :: cpu_usage_percent
  public :: freebsd_devstat_getdevs
  public :: freebsd_kvm_close
  public :: freebsd_kvm_getprocs
  public :: freebsd_kvm_open
  public :: freebsd_network_interfaces
  public :: freebsd_network_connections
  public :: freebsd_network_snapshot
  public :: freebsd_sysctl_bytes
  public :: freebsd_sysctl_int
  public :: freebsd_sysctl_long
  public :: freebsd_sysctl_string
  public :: load_average_info
  public :: memory_info
  public :: network_table
  public :: platform_backend
  public :: process_table
  public :: system_uptime_info

  interface
    integer(c_int) function c_ftop_freebsd_cpu_count(count, sys_errno) bind(C, name="ftop_freebsd_cpu_count")
      import :: c_int
      integer(c_int), intent(out) :: count
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_freebsd_cpu_count

    integer(c_int) function c_ftop_freebsd_cpu_ticks(total_ticks, idle_ticks, sys_errno) &
        bind(C, name="ftop_freebsd_cpu_ticks")
      import :: c_int, c_long_long
      integer(c_long_long), intent(out) :: total_ticks
      integer(c_long_long), intent(out) :: idle_ticks
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_freebsd_cpu_ticks

    integer(c_int) function c_ftop_freebsd_cpu_state_ticks(ticks, capacity, cpu_count, sys_errno) &
        bind(C, name="ftop_freebsd_cpu_state_ticks")
      import :: c_int, c_size_t, freebsd_cpu_state_tick_sample
      type(freebsd_cpu_state_tick_sample), intent(out) :: ticks(*)
      integer(c_size_t), value :: capacity
      integer(c_size_t), intent(out) :: cpu_count
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_freebsd_cpu_state_ticks

    integer(c_int) function c_ftop_freebsd_cpu_frequency(cpu_index, freq_mhz, sys_errno) &
        bind(C, name="ftop_freebsd_cpu_frequency")
      import :: c_int
      integer(c_int), value :: cpu_index
      integer(c_int), intent(out) :: freq_mhz
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_freebsd_cpu_frequency

    integer(c_int) function c_ftop_freebsd_cpu_temperature(cpu_index, temperature_c, sys_errno) &
        bind(C, name="ftop_freebsd_cpu_temperature")
      import :: c_double, c_int
      integer(c_int), value :: cpu_index
      real(c_double), intent(out) :: temperature_c
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_freebsd_cpu_temperature

    integer(c_int) function c_ftop_freebsd_acpi_temperature(temperature_c, sys_errno) &
        bind(C, name="ftop_freebsd_acpi_temperature")
      import :: c_double, c_int
      real(c_double), intent(out) :: temperature_c
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_freebsd_acpi_temperature

    integer(c_int) function c_ftop_freebsd_memory_info(total_bytes, available_bytes, swap_total_bytes, &
        swap_used_bytes, sys_errno) &
        bind(C, name="ftop_freebsd_memory_info")
      import :: c_int, c_long_long
      integer(c_long_long), intent(out) :: total_bytes
      integer(c_long_long), intent(out) :: available_bytes
      integer(c_long_long), intent(out) :: swap_total_bytes
      integer(c_long_long), intent(out) :: swap_used_bytes
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_freebsd_memory_info

    integer(c_int) function c_ftop_freebsd_load_average(loads, sys_errno) bind(C, name="ftop_freebsd_load_average")
      import :: c_double, c_int
      real(c_double), intent(out) :: loads(3)
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_freebsd_load_average

    integer(c_int) function c_ftop_freebsd_uptime_seconds(seconds, sys_errno) &
        bind(C, name="ftop_freebsd_uptime_seconds")
      import :: c_int, c_long_long
      integer(c_long_long), intent(out) :: seconds
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_freebsd_uptime_seconds

    integer(c_int) function c_ftop_freebsd_sysctl_int(name, value, sys_errno) &
        bind(C, name="ftop_freebsd_sysctl_int")
      import :: c_char, c_int
      character(kind=c_char), intent(in) :: name(*)
      integer(c_int), intent(out) :: value
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_freebsd_sysctl_int

    integer(c_int) function c_ftop_freebsd_sysctl_long(name, value, sys_errno) &
        bind(C, name="ftop_freebsd_sysctl_long")
      import :: c_char, c_int, c_long
      character(kind=c_char), intent(in) :: name(*)
      integer(c_long), intent(out) :: value
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_freebsd_sysctl_long

    integer(c_int) function c_ftop_freebsd_sysctl_string(name, value, value_capacity, value_len, sys_errno) &
        bind(C, name="ftop_freebsd_sysctl_string")
      import :: c_char, c_int, c_size_t
      character(kind=c_char), intent(in) :: name(*)
      character(kind=c_char), intent(out) :: value(*)
      integer(c_size_t), value :: value_capacity
      integer(c_size_t), intent(out) :: value_len
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_freebsd_sysctl_string

    integer(c_int) function c_ftop_freebsd_sysctl_bytes(name, value, value_capacity, value_len, sys_errno) &
        bind(C, name="ftop_freebsd_sysctl_bytes")
      import :: c_char, c_int, c_size_t
      character(kind=c_char), intent(in) :: name(*)
      character(kind=c_char), intent(out) :: value(*)
      integer(c_size_t), value :: value_capacity
      integer(c_size_t), intent(out) :: value_len
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_freebsd_sysctl_bytes

    type(c_ptr) function c_ftop_freebsd_kvm_open(sys_errno) bind(C, name="ftop_freebsd_kvm_open")
      import :: c_int, c_ptr
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_freebsd_kvm_open

    integer(c_int) function c_ftop_freebsd_kvm_close(handle, sys_errno) bind(C, name="ftop_freebsd_kvm_close")
      import :: c_int, c_ptr
      type(c_ptr), value :: handle
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_freebsd_kvm_close

    integer(c_int) function c_ftop_freebsd_kvm_getprocs(handle, processes, capacity, process_count, sys_errno) &
        bind(C, name="ftop_freebsd_kvm_getprocs")
      import :: c_int, c_ptr, c_size_t, freebsd_process_info
      type(c_ptr), value :: handle
      type(freebsd_process_info), intent(out) :: processes(*)
      integer(c_size_t), value :: capacity
      integer(c_size_t), intent(out) :: process_count
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_freebsd_kvm_getprocs

    integer(c_int) function c_ftop_freebsd_devstat_getdevs(devices, capacity, device_count, generation, sys_errno) &
        bind(C, name="ftop_freebsd_devstat_getdevs")
      import :: c_int, c_long_long, c_size_t, freebsd_devstat_info
      type(freebsd_devstat_info), intent(out) :: devices(*)
      integer(c_size_t), value :: capacity
      integer(c_size_t), intent(out) :: device_count
      integer(c_long_long), intent(out) :: generation
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_freebsd_devstat_getdevs

    integer(c_int) function c_ftop_freebsd_network_interfaces(interfaces, capacity, interface_count, sys_errno) &
        bind(C, name="ftop_freebsd_network_interfaces")
      import :: c_int, c_size_t, freebsd_net_interface_info
      type(freebsd_net_interface_info), intent(out) :: interfaces(*)
      integer(c_size_t), value :: capacity
      integer(c_size_t), intent(out) :: interface_count
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_freebsd_network_interfaces

    integer(c_int) function c_ftop_freebsd_network_connections(connections, capacity, connection_count, sys_errno) &
        bind(C, name="ftop_freebsd_network_connections")
      import :: c_int, c_size_t, freebsd_net_connection_info
      type(freebsd_net_connection_info), intent(out) :: connections(*)
      integer(c_size_t), value :: capacity
      integer(c_size_t), intent(out) :: connection_count
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_freebsd_network_connections

    integer(c_int) function c_ftop_freebsd_user_name(uid, value, value_capacity, value_len, sys_errno) &
        bind(C, name="ftop_freebsd_user_name")
      import :: c_char, c_int, c_size_t
      integer(c_int), value :: uid
      character(kind=c_char), intent(out) :: value(*)
      integer(c_size_t), value :: value_capacity
      integer(c_size_t), intent(out) :: value_len
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_freebsd_user_name
  end interface

contains

  function create_platform() result(backend)
    class(platform_backend), allocatable :: backend

    allocate(backend, source=freebsd_backend())
  end function create_platform

  integer function freebsd_get_cpu_count(self) result(count)
    class(freebsd_backend), intent(in) :: self
    integer(c_int) :: c_count
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    associate(unused => self)
    end associate

    rc = c_ftop_freebsd_cpu_count(c_count, sys_errno)
    if (rc == 0_c_int) then
      count = int(c_count)
    else
      count = 0
    end if
  end function freebsd_get_cpu_count

  function freebsd_get_cpu_topology(self) result(info)
    class(freebsd_backend), intent(in) :: self
    type(cpu_topology_info) :: info
    integer :: core_count
    integer :: model_name_len
    integer :: thread_count
    integer :: threads_per_core

    thread_count = freebsd_get_cpu_count(self)
    if (thread_count <= 0) return

    core_count = thread_count
    if (.not. freebsd_sysctl_int("kern.smp.cores", core_count) .or. core_count <= 0) then
      if (freebsd_sysctl_int("kern.smp.threads_per_core", threads_per_core) .and. threads_per_core > 0) then
        core_count = max(1, (thread_count + threads_per_core - 1) / threads_per_core)
      else
        core_count = thread_count
      end if
    end if

    info%valid = .true.
    info%thread_count = thread_count
    info%core_count = max(1, min(thread_count, core_count))
    info%model_name_valid = freebsd_sysctl_string("hw.model", info%model_name, model_name_len) .and. model_name_len > 0
  end function freebsd_get_cpu_topology

  function freebsd_get_cpu_sample(self) result(sample)
    class(freebsd_backend), intent(in) :: self
    type(cpu_tick_sample) :: sample
    integer(c_long_long) :: total_ticks
    integer(c_long_long) :: idle_ticks
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    associate(unused => self)
    end associate

    rc = c_ftop_freebsd_cpu_ticks(total_ticks, idle_ticks, sys_errno)
    if (rc == 0_c_int .and. total_ticks > 0_c_long_long) then
      sample%valid = .true.
      sample%total = int(total_ticks, int64)
      sample%idle = int(max(0_c_long_long, idle_ticks), int64)
    end if
  end function freebsd_get_cpu_sample

  logical function freebsd_get_cpu_state_snapshot(self, total, cores) result(success)
    class(freebsd_backend), intent(in) :: self
    type(cpu_state_ticks), intent(out) :: total
    type(cpu_state_ticks), allocatable, intent(out) :: cores(:)
    type(freebsd_cpu_state_tick_sample), allocatable :: c_ticks(:)
    integer(c_size_t) :: cpu_count
    integer(c_int) :: sys_errno
    integer(c_int) :: rc
    integer :: capacity
    integer :: i

    total = cpu_state_ticks()
    if (allocated(cores)) deallocate(cores)
    capacity = max(1, freebsd_get_cpu_count(self))
    allocate(c_ticks(capacity))

    rc = c_ftop_freebsd_cpu_state_ticks(c_ticks, int(size(c_ticks), c_size_t), cpu_count, sys_errno)
    success = rc == 0_c_int .and. cpu_count > 0_c_size_t
    if (.not. success) then
      allocate(cores(0))
      return
    end if

    allocate(cores(int(cpu_count)))
    do i = 1, size(cores)
      cores(i) = freebsd_cpu_state_from_c(c_ticks(i))
      call add_cpu_state(total, cores(i))
    end do
    total%valid = cpu_state_total_ticks(total) > 0_int64
  end function freebsd_get_cpu_state_snapshot

  logical function freebsd_get_cpu_metadata(self, cores) result(success)
    class(freebsd_backend), intent(in) :: self
    type(cpu_core_info), allocatable, intent(out) :: cores(:)
    real(c_double) :: acpi_temperature_c
    real(c_double) :: cpu_temperature_c
    integer(c_int) :: c_freq_mhz
    integer(c_int) :: sys_errno
    integer :: cpu_count
    integer :: cpu_index
    logical :: have_acpi_temperature

    cpu_count = max(0, freebsd_get_cpu_count(self))
    allocate(cores(cpu_count))
    have_acpi_temperature = c_ftop_freebsd_acpi_temperature(acpi_temperature_c, sys_errno) == 0_c_int
    do cpu_index = 1, cpu_count
      if (c_ftop_freebsd_cpu_frequency(int(cpu_index - 1, c_int), c_freq_mhz, sys_errno) == 0_c_int .and. &
          c_freq_mhz > 0_c_int) then
        cores(cpu_index)%freq_valid = .true.
        cores(cpu_index)%freq_mhz = real(c_freq_mhz, real64)
      end if

      if (c_ftop_freebsd_cpu_temperature(int(cpu_index - 1, c_int), cpu_temperature_c, sys_errno) == 0_c_int) then
        cores(cpu_index)%temp_valid = .true.
        cores(cpu_index)%temp_c = real(cpu_temperature_c, real64)
      else if (have_acpi_temperature) then
        cores(cpu_index)%temp_valid = .true.
        cores(cpu_index)%temp_c = real(acpi_temperature_c, real64)
      end if
    end do
    success = cpu_count > 0
  end function freebsd_get_cpu_metadata

  function freebsd_get_memory_info(self) result(info)
    class(freebsd_backend), intent(in) :: self
    type(memory_info) :: info
    integer(c_long_long) :: total_bytes
    integer(c_long_long) :: available_bytes
    integer(c_long_long) :: swap_total_bytes
    integer(c_long_long) :: swap_used_bytes
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    associate(unused => self)
    end associate

    rc = c_ftop_freebsd_memory_info(total_bytes, available_bytes, swap_total_bytes, swap_used_bytes, sys_errno)
    if (rc == 0_c_int .and. total_bytes > 0_c_long_long) then
      info%valid = .true.
      info%total_bytes = int(total_bytes, int64)
      info%available_bytes = int(max(0_c_long_long, min(total_bytes, available_bytes)), int64)
      info%used_bytes = max(0_int64, info%total_bytes - info%available_bytes)
      info%free_bytes = info%available_bytes
      swap_total_bytes = max(0_c_long_long, swap_total_bytes)
      info%swap_total_bytes = int(swap_total_bytes, int64)
      info%swap_used_bytes = int(max(0_c_long_long, min(swap_total_bytes, swap_used_bytes)), int64)
    end if
  end function freebsd_get_memory_info

  function freebsd_get_load_average(self) result(info)
    class(freebsd_backend), intent(in) :: self
    type(load_average_info) :: info
    real(c_double) :: loads(3)
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    associate(unused => self)
    end associate

    rc = c_ftop_freebsd_load_average(loads, sys_errno)
    if (rc == 0_c_int .and. all(loads >= 0.0_c_double)) then
      info%valid = .true.
      info%values = real(loads, real64)
    end if
  end function freebsd_get_load_average

  function freebsd_get_system_uptime(self) result(info)
    class(freebsd_backend), intent(in) :: self
    type(system_uptime_info) :: info
    integer(c_long_long) :: seconds
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    associate(unused => self)
    end associate

    rc = c_ftop_freebsd_uptime_seconds(seconds, sys_errno)
    if (rc == 0_c_int .and. seconds >= 0_c_long_long) then
      info%valid = .true.
      info%seconds = int(seconds, int64)
    end if
  end function freebsd_get_system_uptime

  function freebsd_get_process_table(self) result(table)
    class(freebsd_backend), intent(in) :: self
    type(process_table) :: table
    type(freebsd_kvm_handle) :: handle
    type(freebsd_process_info), allocatable :: raw_processes(:)
    type(memory_info) :: memory
    integer :: process_count
    integer :: process_index
    logical :: ignored

    table = process_table()
    allocate(raw_processes(FREEBSD_PROCESS_CAPACITY))
    if (.not. freebsd_kvm_open(handle)) then
      allocate(table%items(0))
      return
    end if

    if (.not. freebsd_kvm_getprocs(handle, raw_processes, process_count)) then
      ignored = freebsd_kvm_close(handle)
      allocate(table%items(0))
      return
    end if
    ignored = freebsd_kvm_close(handle)

    memory = freebsd_get_memory_info(self)
    process_count = max(0, min(process_count, size(raw_processes)))
    allocate(table%items(process_count))
    table%valid = .true.
    do process_index = 1, process_count
      table%items(process_index) = freebsd_process_from_c(raw_processes(process_index), memory%total_bytes)
    end do
  end function freebsd_get_process_table

  function freebsd_get_network_table(self) result(table)
    class(freebsd_backend), intent(in) :: self
    type(network_table) :: table

    associate(unused => self)
    end associate

    if (.not. freebsd_network_snapshot(table)) table = network_table()
  end function freebsd_get_network_table

  logical function freebsd_network_snapshot(table, error_code) result(success)
    type(network_table), intent(out) :: table
    integer, intent(out), optional :: error_code
    type(freebsd_net_connection_info), allocatable :: raw_connections(:)
    type(freebsd_net_interface_info), allocatable :: raw_interfaces(:)
    integer :: connection_count
    integer :: connection_index
    integer :: interface_count
    integer :: interface_index

    table = network_table()
    allocate(raw_interfaces(FREEBSD_NETWORK_INTERFACE_CAPACITY))
    allocate(raw_connections(FREEBSD_NETWORK_CONNECTION_CAPACITY))
    success = freebsd_network_interfaces(raw_interfaces, interface_count, error_code)
    if (.not. success) return

    interface_count = max(0, min(interface_count, size(raw_interfaces)))
    allocate(table%interfaces(interface_count))
    ! Public FreeBSD APIs used here expose socket ownership, not reliable byte counters.
    allocate(table%processes(0))
    table%valid = .true.
    do interface_index = 1, interface_count
      table%interfaces(interface_index) = freebsd_interface_from_c(raw_interfaces(interface_index))
    end do

    if (freebsd_network_connections(raw_connections, connection_count)) then
      connection_count = max(0, min(connection_count, size(raw_connections)))
      allocate(table%connections(connection_count))
      do connection_index = 1, connection_count
        table%connections(connection_index) = freebsd_connection_from_c(raw_connections(connection_index))
      end do
    else
      allocate(table%connections(0))
    end if
  end function freebsd_network_snapshot

  function freebsd_interface_from_c(raw) result(interface)
    type(freebsd_net_interface_info), intent(in) :: raw
    type(interface_info) :: interface
    character(len=:), allocatable :: name
    character(len=:), allocatable :: state

    interface%valid = raw%valid /= 0_c_int
    call c_chars_to_string(raw%name, name)
    call c_chars_to_string(raw%state, state)
    interface%name = bounded_text(name, len(interface%name))
    interface%rx_bytes = int(max(0_c_long_long, raw%rx_bytes), int64)
    interface%tx_bytes = int(max(0_c_long_long, raw%tx_bytes), int64)
    interface%rx_packets = int(max(0_c_long_long, raw%rx_packets), int64)
    interface%tx_packets = int(max(0_c_long_long, raw%tx_packets), int64)
    interface%state = bounded_text(state, len(interface%state))
    interface%speed_mbps = int(max(0_c_int, raw%speed_mbps))
    interface%mtu = int(max(0_c_int, raw%mtu))
  end function freebsd_interface_from_c

  function freebsd_connection_from_c(raw) result(connection)
    type(freebsd_net_connection_info), intent(in) :: raw
    type(net_connection) :: connection
    character(len=:), allocatable :: local_addr
    character(len=:), allocatable :: process_name
    character(len=:), allocatable :: protocol
    character(len=:), allocatable :: remote_addr
    character(len=:), allocatable :: state

    connection%valid = raw%valid /= 0_c_int
    call c_chars_to_string(raw%protocol, protocol)
    call c_chars_to_string(raw%local_addr, local_addr)
    call c_chars_to_string(raw%remote_addr, remote_addr)
    call c_chars_to_string(raw%state, state)
    call c_chars_to_string(raw%process_name, process_name)
    connection%protocol = bounded_text(protocol, len(connection%protocol))
    connection%local_addr = bounded_text(local_addr, len(connection%local_addr))
    connection%local_port = int(max(0_c_int, raw%local_port))
    connection%remote_addr = bounded_text(remote_addr, len(connection%remote_addr))
    connection%remote_port = int(max(0_c_int, raw%remote_port))
    connection%state = bounded_text(state, len(connection%state))
    connection%inode = int(max(0_c_long_long, raw%socket_id), int64)
    connection%pid = int(max(0_c_int, raw%pid))
    connection%process_name = bounded_text(process_name, len(connection%process_name))
  end function freebsd_connection_from_c

  function freebsd_process_from_c(raw, memory_total_bytes) result(process)
    type(freebsd_process_info), intent(in) :: raw
    integer(int64), intent(in) :: memory_total_bytes
    type(process_info) :: process
    character(len=:), allocatable :: command

    process%valid = raw%pid > 0_c_int
    process%pid = int(max(0_c_int, raw%pid))
    process%ppid = int(max(0_c_int, raw%ppid))
    process%uid = int(max(0_c_int, raw%uid))
    process%jid = int(max(0_c_int, raw%jid))
    call assign_process_user(process)
    process%state = freebsd_state_label(int(raw%state))
    call c_chars_to_string(raw%command, command)
    process%name = bounded_text(command, len(process%name))
    process%command = bounded_text(command, len(process%command))
    process%mem_rss_bytes = int(max(0_c_long_long, raw%mem_rss_bytes), int64)
    process%mem_virt_bytes = int(max(0_c_long_long, raw%mem_virt_bytes), int64)
    process%threads = int(max(0_c_int, raw%threads))
    process%nice = int(raw%nice)
    process%priority = int(raw%priority)
    process%start_time = int(max(0_c_long_long, raw%start_time), int64)
    process%cpu_time = int(max(0_c_long_long, raw%cpu_time), int64)
    call assign_memory_percent(process, memory_total_bytes)
  end function freebsd_process_from_c

  function freebsd_state_label(state) result(label)
    integer, intent(in) :: state
    character(len=:), allocatable :: label

    select case (state)
    case (1)
      label = "S"
    case (2)
      label = "R"
    case (3)
      label = "T"
    case (4)
      label = "Z"
    case (5)
      label = "W"
    case (6)
      label = "L"
    case default
      label = "?"
    end select
  end function freebsd_state_label

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

    rc = c_ftop_freebsd_user_name(int(process%uid, c_int), c_value, int(size(c_value), c_size_t), value_len, sys_errno)
    if (rc /= 0_c_int .or. value_len <= 0_c_size_t) return
    call c_chars_to_string(c_value, user_name)
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

  function freebsd_cpu_state_from_c(c_ticks) result(ticks)
    type(freebsd_cpu_state_tick_sample), intent(in) :: c_ticks
    type(cpu_state_ticks) :: ticks

    ticks%user = int(max(0_c_long_long, c_ticks%user), int64)
    ticks%nice = int(max(0_c_long_long, c_ticks%nice), int64)
    ticks%system = int(max(0_c_long_long, c_ticks%system), int64)
    ticks%idle = int(max(0_c_long_long, c_ticks%idle), int64)
    ticks%irq = int(max(0_c_long_long, c_ticks%irq), int64)
    ticks%valid = cpu_state_total_ticks(ticks) > 0_int64
  end function freebsd_cpu_state_from_c

  subroutine add_cpu_state(total, core)
    type(cpu_state_ticks), intent(inout) :: total
    type(cpu_state_ticks), intent(in) :: core

    if (.not. core%valid) return
    total%user = total%user + core%user
    total%nice = total%nice + core%nice
    total%system = total%system + core%system
    total%idle = total%idle + core%idle
    total%iowait = total%iowait + core%iowait
    total%irq = total%irq + core%irq
    total%softirq = total%softirq + core%softirq
    total%steal = total%steal + core%steal
  end subroutine add_cpu_state

  logical function freebsd_sysctl_int(name, value, error_code) result(success)
    character(len=*), intent(in) :: name
    integer, intent(out) :: value
    integer, intent(out), optional :: error_code
    character(kind=c_char), allocatable :: c_name(:)
    integer(c_int) :: c_value
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    call to_c_string(name, c_name)
    rc = c_ftop_freebsd_sysctl_int(c_name, c_value, sys_errno)
    success = rc == 0_c_int
    if (success) then
      value = int(c_value)
    else
      value = 0
    end if
    call assign_error(error_code, sys_errno)
  end function freebsd_sysctl_int

  logical function freebsd_sysctl_long(name, value, error_code) result(success)
    character(len=*), intent(in) :: name
    integer(c_long), intent(out) :: value
    integer, intent(out), optional :: error_code
    character(kind=c_char), allocatable :: c_name(:)
    integer(c_long) :: c_value
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    call to_c_string(name, c_name)
    rc = c_ftop_freebsd_sysctl_long(c_name, c_value, sys_errno)
    success = rc == 0_c_int
    if (success) then
      value = c_value
    else
      value = 0_c_long
    end if
    call assign_error(error_code, sys_errno)
  end function freebsd_sysctl_long

  logical function freebsd_sysctl_string(name, value, value_len, error_code) result(success)
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
    rc = c_ftop_freebsd_sysctl_string(c_name, c_value, int(size(c_value), c_size_t), c_value_len, sys_errno)
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
  end function freebsd_sysctl_string

  logical function freebsd_sysctl_bytes(name, value, value_len, error_code) result(success)
    character(len=*), intent(in) :: name
    character(kind=c_char), intent(out) :: value(:)
    integer, intent(out), optional :: value_len
    integer, intent(out), optional :: error_code
    character(kind=c_char), allocatable :: c_name(:)
    integer(c_size_t) :: c_value_len
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    if (present(value_len)) value_len = 0
    if (size(value) <= 0) then
      call assign_error(error_code, 0_c_int)
      success = .false.
      return
    end if

    call to_c_string(name, c_name)
    rc = c_ftop_freebsd_sysctl_bytes(c_name, value, int(size(value), c_size_t), c_value_len, sys_errno)
    success = rc == 0_c_int
    if (success .and. present(value_len)) value_len = int(c_value_len)
    call assign_error(error_code, sys_errno)
  end function freebsd_sysctl_bytes

  logical function freebsd_kvm_open(handle, error_code) result(success)
    type(freebsd_kvm_handle), intent(out) :: handle
    integer, intent(out), optional :: error_code
    integer(c_int) :: sys_errno

    handle%handle = c_ftop_freebsd_kvm_open(sys_errno)
    success = c_associated(handle%handle)
    call assign_error(error_code, sys_errno)
  end function freebsd_kvm_open

  logical function freebsd_kvm_close(handle, error_code) result(success)
    type(freebsd_kvm_handle), intent(inout) :: handle
    integer, intent(out), optional :: error_code
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    sys_errno = 0_c_int
    success = .false.
    if (c_associated(handle%handle)) then
      rc = c_ftop_freebsd_kvm_close(handle%handle, sys_errno)
      success = rc == 0_c_int
    end if
    if (success) handle%handle = c_null_ptr
    call assign_error(error_code, sys_errno)
  end function freebsd_kvm_close

  logical function freebsd_kvm_getprocs(handle, processes, process_count, error_code) result(success)
    type(freebsd_kvm_handle), intent(in) :: handle
    type(freebsd_process_info), intent(out) :: processes(:)
    integer, intent(out) :: process_count
    integer, intent(out), optional :: error_code
    integer(c_size_t) :: c_process_count
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    process_count = 0
    if (.not. c_associated(handle%handle) .or. size(processes) <= 0) then
      call assign_error(error_code, 0_c_int)
      success = .false.
      return
    end if

    rc = c_ftop_freebsd_kvm_getprocs(handle%handle, processes, int(size(processes), c_size_t), c_process_count, sys_errno)
    success = rc == 0_c_int
    if (success) process_count = int(c_process_count)
    call assign_error(error_code, sys_errno)
  end function freebsd_kvm_getprocs

  logical function freebsd_devstat_getdevs(devices, device_count, generation, error_code) result(success)
    type(freebsd_devstat_info), intent(out) :: devices(:)
    integer, intent(out) :: device_count
    integer(int64), intent(out), optional :: generation
    integer, intent(out), optional :: error_code
    integer(c_long_long) :: c_generation
    integer(c_size_t) :: c_device_count
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    device_count = 0
    if (present(generation)) generation = 0_int64
    if (size(devices) <= 0) then
      call assign_error(error_code, 0_c_int)
      success = .false.
      return
    end if

    rc = c_ftop_freebsd_devstat_getdevs(devices, int(size(devices), c_size_t), c_device_count, c_generation, sys_errno)
    success = rc == 0_c_int
    if (success) then
      device_count = int(c_device_count)
      if (present(generation)) generation = int(c_generation, int64)
    end if
    call assign_error(error_code, sys_errno)
  end function freebsd_devstat_getdevs

  logical function freebsd_network_interfaces(interfaces, interface_count, error_code) result(success)
    type(freebsd_net_interface_info), intent(out) :: interfaces(:)
    integer, intent(out) :: interface_count
    integer, intent(out), optional :: error_code
    integer(c_size_t) :: c_interface_count
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    interface_count = 0
    if (size(interfaces) <= 0) then
      call assign_error(error_code, 0_c_int)
      success = .false.
      return
    end if

    rc = c_ftop_freebsd_network_interfaces(interfaces, int(size(interfaces), c_size_t), c_interface_count, sys_errno)
    success = rc == 0_c_int
    if (success) interface_count = int(c_interface_count)
    call assign_error(error_code, sys_errno)
  end function freebsd_network_interfaces

  logical function freebsd_network_connections(connections, connection_count, error_code) result(success)
    type(freebsd_net_connection_info), intent(out) :: connections(:)
    integer, intent(out) :: connection_count
    integer, intent(out), optional :: error_code
    integer(c_size_t) :: c_connection_count
    integer(c_int) :: sys_errno
    integer(c_int) :: rc

    connection_count = 0
    if (size(connections) <= 0) then
      call assign_error(error_code, 0_c_int)
      success = .false.
      return
    end if

    rc = c_ftop_freebsd_network_connections(connections, int(size(connections), c_size_t), &
                                            c_connection_count, sys_errno)
    success = rc == 0_c_int
    if (success) connection_count = int(c_connection_count)
    call assign_error(error_code, sys_errno)
  end function freebsd_network_connections

  subroutine c_chars_to_string(c_buffer, text)
    character(kind=c_char), intent(in) :: c_buffer(:)
    character(len=:), allocatable, intent(out) :: text
    integer :: copied_len
    integer :: i

    copied_len = 0
    do i = 1, size(c_buffer)
      if (c_buffer(i) == c_null_char) exit
      copied_len = copied_len + 1
    end do

    allocate(character(len=copied_len) :: text)
    do i = 1, copied_len
      text(i:i) = achar(iachar(c_buffer(i)))
    end do
  end subroutine c_chars_to_string

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

  subroutine assign_error(error_code, sys_errno)
    integer, intent(out), optional :: error_code
    integer(c_int), intent(in) :: sys_errno

    if (present(error_code)) error_code = int(sys_errno)
  end subroutine assign_error

end module ftop_platform
