program test_macos_platform
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use ftop_cpu_data, only : cpu_core_info
  use ftop_platform, only : &
    create_platform, &
    macos_iokit_disk_count, &
    macos_iokit_gpu_count, &
    macos_net_connection_info, &
    macos_net_interface_info, &
    macos_network_connections, &
    macos_network_interfaces, &
    macos_network_snapshot, &
    macos_processor_tick_samples, &
    macos_processor_ticks, &
    macos_sysctl_int, &
    macos_sysctl_int64, &
    macos_sysctl_string, &
    network_table, &
    platform_backend
  implicit none

  class(platform_backend), allocatable :: backend
  type(macos_net_connection_info), allocatable :: connections(:)
  type(macos_net_interface_info), allocatable :: interfaces(:)
  type(network_table) :: network
  type(macos_processor_ticks), allocatable :: ticks(:)
  type(cpu_core_info), allocatable :: cpu_metadata(:)
  integer :: connection_count
  character(len=64) :: os_type
  integer :: disk_count
  integer :: gpu_count
  integer :: interface_count
  integer :: interface_index
  integer :: logical_cpu_count
  integer :: os_type_len
  integer :: processor_count
  integer(int64) :: memory_bytes
  integer(int64) :: first_total

  if (.not. macos_sysctl_int("hw.logicalcpu", logical_cpu_count)) error stop "hw.logicalcpu sysctl failed"
  if (logical_cpu_count <= 0) error stop "hw.logicalcpu must be positive"

  if (.not. macos_sysctl_int64("hw.memsize", memory_bytes)) error stop "hw.memsize sysctl failed"
  if (memory_bytes <= 0_int64) error stop "hw.memsize must be positive"

  if (.not. macos_sysctl_string("kern.ostype", os_type, os_type_len)) error stop "kern.ostype sysctl failed"
  if (os_type_len <= 0) error stop "kern.ostype must not be empty"
  if (index(os_type, "Darwin") /= 1) error stop "kern.ostype must start with Darwin"

  allocate(ticks(256))
  if (.not. macos_processor_tick_samples(ticks, processor_count)) error stop "host_processor_info failed"
  if (processor_count <= 0) error stop "processor tick count must be positive"
  first_total = ticks(1)%user + ticks(1)%system + ticks(1)%idle + ticks(1)%nice
  if (first_total <= 0_int64) error stop "processor ticks must be positive"

  if (.not. macos_iokit_gpu_count(gpu_count)) error stop "IOKit GPU stub failed"
  if (.not. macos_iokit_disk_count(disk_count)) error stop "IOKit disk stub failed"
  if (gpu_count < 0 .or. disk_count < 0) error stop "IOKit stub counts must not be negative"

  allocate(interfaces(256))
  if (.not. macos_network_interfaces(interfaces, interface_count)) error stop "macOS interface snapshot failed"
  if (interface_count <= 0 .or. interface_count > size(interfaces)) error stop "macOS interface count out of range"
  do interface_index = 1, interface_count
    if (interfaces(interface_index)%valid == 0) error stop "macOS network interface must be valid"
    if (c_string_len(interfaces(interface_index)%name) <= 0) error stop "macOS interface name must not be empty"
    if (c_string_len(interfaces(interface_index)%state) <= 0) error stop "macOS interface state must not be empty"
    if (interfaces(interface_index)%mtu <= 0) error stop "macOS interface MTU must be positive"
    if (interfaces(interface_index)%speed_mbps < 0) error stop "macOS interface speed must not be negative"
  end do

  allocate(connections(256))
  if (.not. macos_network_connections(connections, connection_count)) error stop "macOS connection snapshot failed"
  if (connection_count < 0 .or. connection_count > size(connections)) error stop "macOS connection count out of range"
  if (connection_count > 0 .and. all(connections(1:connection_count)%valid == 0)) then
    error stop "macOS connection snapshot must mark returned rows valid"
  end if

  if (.not. macos_network_snapshot(network)) error stop "macOS network snapshot failed"
  if (.not. allocated(network%interfaces)) error stop "macOS network interfaces must be allocated"
  if (.not. allocated(network%connections)) error stop "macOS network connections must be allocated"

  backend = create_platform()
  if (.not. allocated(backend)) error stop "macOS platform backend allocation failed"
  if (.not. backend%get_cpu_metadata(cpu_metadata)) error stop "macOS CPU metadata failed"
  if (any(cpu_metadata%temp_valid .and. cpu_metadata%temp_c < -100.0_real64)) error stop "macOS CPU temperature too low"
  if (any(cpu_metadata%temp_valid .and. cpu_metadata%temp_c > 150.0_real64)) error stop "macOS CPU temperature too high"

contains

  integer function c_string_len(buffer) result(length)
    use, intrinsic :: iso_c_binding, only : c_char, c_null_char
    character(kind=c_char), intent(in) :: buffer(:)
    integer :: index_value

    length = 0
    do index_value = 1, size(buffer)
      if (buffer(index_value) == c_null_char) return
      length = length + 1
    end do
  end function c_string_len
end program test_macos_platform
