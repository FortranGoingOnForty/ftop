program test_linux_platform
  use, intrinsic :: iso_c_binding, only : c_char, c_int, c_long_long, c_null_char
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use ftop_cpu_data, only : cpu_state_ticks, cpu_state_total_ticks
  use ftop_mem_data, only : metric_memory_info => memory_info, memory_usage_percent
  use ftop_net_data, only : process_bandwidth
  use ftop_platform, only : &
    linux_cpuinfo_field, &
    linux_cpuinfo_field_count, &
    linux_cpu_state_snapshot, &
    linux_hwmon_discover, &
    linux_hwmon_sensor, &
    load_average_info, &
    linux_load_average_snapshot, &
    linux_memory_snapshot, &
    linux_network_snapshot, &
    linux_process_bandwidth_snapshot, &
    linux_process_snapshot, &
    network_table, &
    process_table
  use ftop_signal, only : ftop_current_pid
  implicit none

  interface
    integer(c_int) function ftop_test_linux_network_open(tcp_port, udp_port, sys_errno) &
        bind(C, name="ftop_test_linux_network_open")
      import :: c_int
      integer(c_int), intent(out) :: tcp_port
      integer(c_int), intent(out) :: udp_port
      integer(c_int), intent(out) :: sys_errno
    end function ftop_test_linux_network_open

    subroutine ftop_test_linux_network_close() bind(C, name="ftop_test_linux_network_close")
    end subroutine ftop_test_linux_network_close

    integer(c_int) function ftop_test_linux_network_open_traffic(sys_errno) &
        bind(C, name="ftop_test_linux_network_open_traffic")
      import :: c_int
      integer(c_int), intent(out) :: sys_errno
    end function ftop_test_linux_network_open_traffic

    integer(c_int) function ftop_test_linux_ip_link_counters(interface_name, rx_bytes, tx_bytes, sys_errno) &
        bind(C, name="ftop_test_linux_ip_link_counters")
      import :: c_char, c_int, c_long_long
      character(kind=c_char), intent(in) :: interface_name(*)
      integer(c_long_long), intent(out) :: rx_bytes
      integer(c_long_long), intent(out) :: tx_bytes
      integer(c_int), intent(out) :: sys_errno
    end function ftop_test_linux_ip_link_counters
  end interface

  integer(int64), parameter :: INTERFACE_COUNTER_ABSOLUTE_TOLERANCE = 1024_int64 * 1024_int64

  type(linux_hwmon_sensor), allocatable :: sensors(:)
  type(cpu_state_ticks) :: total_cpu
  type(cpu_state_ticks), allocatable :: cores(:)
  type(load_average_info) :: load_average
  type(metric_memory_info) :: memory
  type(network_table) :: network
  type(process_table) :: processes
  character(len=128) :: processor_id
  integer :: current_pid
  integer :: i
  integer :: processor_count
  integer :: processor_id_len
  integer :: sensor_count
  logical :: found_current_process

  if (.not. linux_cpuinfo_field_count("processor", processor_count)) error stop "processor count failed"
  if (processor_count <= 0) error stop "processor count must be positive"

  if (.not. linux_cpuinfo_field("processor", processor_id, processor_id_len)) error stop "processor field failed"
  if (processor_id_len <= 0) error stop "processor field must not be empty"

  allocate(sensors(64))
  if (.not. linux_hwmon_discover(sensors, sensor_count)) error stop "hwmon discovery failed"
  if (sensor_count < 0) error stop "hwmon sensor count must not be negative"

  do i = 1, sensor_count
    if (sensors(i)%path(1) == c_null_char) error stop "hwmon sensor path must not be empty"
    if (sensors(i)%name(1) == c_null_char) error stop "hwmon sensor name must not be empty"
  end do

  if (.not. linux_cpu_state_snapshot(total_cpu, cores)) error stop "Linux CPU state snapshot failed"
  if (.not. total_cpu%valid) error stop "Linux total CPU state must be valid"
  if (cpu_state_total_ticks(total_cpu) <= 0) error stop "Linux total CPU ticks must be positive"
  if (size(cores) <= 0) error stop "Linux core CPU states must not be empty"
  do i = 1, size(cores)
    if (.not. cores(i)%valid) error stop "Linux core CPU state must be valid"
    if (cpu_state_total_ticks(cores(i)) <= 0) error stop "Linux core CPU ticks must be positive"
  end do

  if (.not. linux_memory_snapshot(memory)) error stop "Linux memory snapshot failed"
  if (.not. memory%valid) error stop "Linux memory snapshot must be valid"
  if (memory%total_bytes <= 0) error stop "Linux memory total must be positive"
  if (memory%available_bytes < 0) error stop "Linux available memory must not be negative"
  if (memory%available_bytes > memory%total_bytes) error stop "Linux available memory must not exceed total"
  if (memory%used_bytes < 0) error stop "Linux used memory must not be negative"
  if (memory%used_bytes > memory%total_bytes) error stop "Linux used memory must not exceed total"
  if (memory_usage_percent(memory) < 0.0_real64) error stop "Linux memory usage must not be negative"
  if (memory_usage_percent(memory) > 100.0_real64) error stop "Linux memory usage must not exceed 100"

  if (.not. linux_load_average_snapshot(load_average)) error stop "Linux load average snapshot failed"
  if (.not. load_average%valid) error stop "Linux load average snapshot must be valid"
  if (any(load_average%values < 0.0_real64)) error stop "Linux load averages must not be negative"

  if (.not. linux_network_snapshot(network)) error stop "Linux network snapshot failed"
  if (.not. network%valid) error stop "Linux network snapshot must be valid"
  if (.not. allocated(network%interfaces)) error stop "Linux network interfaces must be allocated"
  if (.not. allocated(network%connections)) error stop "Linux network connections must be allocated"
  if (.not. allocated(network%processes)) error stop "Linux network processes must be allocated"
  call test_interface_counter_accuracy(network)
  do i = 1, size(network%processes)
    if (.not. network%processes(i)%valid) cycle
    if (network%processes(i)%pid <= 0) error stop "Linux network process pid must be positive"
    if (network%processes(i)%start_time <= 0_int64) error stop "Linux network process start time must be positive"
    if (network%processes(i)%rx_bytes < 0_int64) error stop "Linux network process rx bytes must not be negative"
    if (network%processes(i)%tx_bytes < 0_int64) error stop "Linux network process tx bytes must not be negative"
    if (network%processes(i)%rx_bytes_per_sec < 0.0_real64) error stop "Linux network process rx rate must not be negative"
    if (network%processes(i)%tx_bytes_per_sec < 0.0_real64) error stop "Linux network process tx rate must not be negative"
  end do
  call test_network_socket_ownership()
  call test_network_process_bandwidth()

  if (.not. linux_process_snapshot(processes, memory%total_bytes)) error stop "Linux process snapshot failed"
  if (.not. processes%valid) error stop "Linux process snapshot must be valid"
  if (.not. allocated(processes%items)) error stop "Linux process snapshot items must be allocated"
  if (size(processes%items) <= 0) error stop "Linux process snapshot must include processes"
  if (any(processes%items%io_read_bytes < 0_int64)) error stop "Linux process read bytes must not be negative"
  if (any(processes%items%io_write_bytes < 0_int64)) error stop "Linux process write bytes must not be negative"
  if (.not. any(len_trim(processes%items%cgroup) > 0)) error stop "Linux process snapshot must include cgroups"

  current_pid = ftop_current_pid()
  found_current_process = .false.
  do i = 1, size(processes%items)
    if (processes%items(i)%pid /= current_pid) cycle
    found_current_process = .true.
    if (len_trim(processes%items(i)%cgroup) <= 0) error stop "current Linux process cgroup must not be empty"
    if (processes%items(i)%io_read_bytes < 0_int64) error stop "current Linux process read bytes must not be negative"
    if (processes%items(i)%io_write_bytes < 0_int64) error stop "current Linux process write bytes must not be negative"
  end do
  if (.not. found_current_process) error stop "Linux process snapshot must include current process"

contains

  subroutine test_network_socket_ownership()
    type(network_table) :: owned_network
    integer(c_int) :: sys_errno
    integer(c_int) :: tcp_port
    integer(c_int) :: udp_port
    integer :: current_pid

    if (ftop_test_linux_network_open(tcp_port, udp_port, sys_errno) /= 0_c_int) then
      error stop "Linux network test sockets failed"
    end if
    if (.not. linux_network_snapshot(owned_network)) error stop "Linux network ownership snapshot failed"
    call ftop_test_linux_network_close()

    current_pid = ftop_current_pid()
    call require_owned_connection(owned_network, "tcp", int(tcp_port), current_pid, "LISTEN", &
                                  "Linux TCP socket ownership must map to current process")
    call require_owned_connection(owned_network, "udp", int(udp_port), current_pid, "OPEN", &
                                  "Linux UDP socket ownership must map to current process")
  end subroutine test_network_socket_ownership

  subroutine test_network_process_bandwidth()
    type(process_bandwidth), allocatable :: bandwidth(:)
    integer(c_int) :: sys_errno

    if (ftop_test_linux_network_open_traffic(sys_errno) /= 0_c_int) then
      error stop "Linux network traffic test sockets failed"
    end if
    if (.not. linux_process_bandwidth_snapshot(bandwidth)) then
      call ftop_test_linux_network_close()
      error stop "Linux process bandwidth snapshot failed"
    end if
    call ftop_test_linux_network_close()

    call require_process_bandwidth(bandwidth, ftop_current_pid())
  end subroutine test_network_process_bandwidth

  subroutine require_process_bandwidth(bandwidth, pid)
    type(process_bandwidth), intent(in) :: bandwidth(:)
    integer, intent(in) :: pid
    integer :: process_index

    do process_index = 1, size(bandwidth)
      if (.not. bandwidth(process_index)%valid) cycle
      if (bandwidth(process_index)%pid /= pid) cycle
      if (bandwidth(process_index)%start_time <= 0_int64) error stop "Linux bandwidth start time must be positive"
      if (len_trim(bandwidth(process_index)%process_name) <= 0) then
        error stop "Linux bandwidth process name must not be empty"
      end if
      if (bandwidth(process_index)%rx_bytes <= 0_int64) error stop "Linux bandwidth rx bytes must be positive"
      if (bandwidth(process_index)%tx_bytes <= 0_int64) error stop "Linux bandwidth tx bytes must be positive"
      return
    end do
    error stop "Linux process bandwidth must include current process traffic"
  end subroutine require_process_bandwidth

  subroutine test_interface_counter_accuracy(network)
    type(network_table), intent(in) :: network
    character(kind=c_char) :: interface_name(64)
    integer(c_long_long) :: reference_rx_bytes
    integer(c_long_long) :: reference_tx_bytes
    integer(c_int) :: sys_errno
    integer :: interface_index

    if (.not. allocated(network%interfaces)) return
    do interface_index = 1, size(network%interfaces)
      if (.not. network%interfaces(interface_index)%valid) cycle
      call copy_c_string(trim(network%interfaces(interface_index)%name), interface_name)
      if (ftop_test_linux_ip_link_counters(interface_name, reference_rx_bytes, reference_tx_bytes, sys_errno) /= 0_c_int) then
        cycle
      end if
      call require_counter_close(network%interfaces(interface_index)%rx_bytes, int(reference_rx_bytes, int64), &
                                 "Linux interface rx byte counter exceeded tolerance")
      call require_counter_close(network%interfaces(interface_index)%tx_bytes, int(reference_tx_bytes, int64), &
                                 "Linux interface tx byte counter exceeded tolerance")
      return
    end do
  end subroutine test_interface_counter_accuracy

  subroutine require_counter_close(ftop_value, reference_value, message)
    integer(int64), intent(in) :: ftop_value
    integer(int64), intent(in) :: reference_value
    character(len=*), intent(in) :: message
    integer(int64) :: difference
    integer(int64) :: tolerance

    if (ftop_value < 0_int64 .or. reference_value < 0_int64) error stop "Linux interface counter must be non-negative"
    difference = abs(reference_value - ftop_value)
    tolerance = max(INTERFACE_COUNTER_ABSOLUTE_TOLERANCE, int(0.05_real64 * real(max(reference_value, 1_int64), real64), int64))
    if (difference > tolerance) error stop message
  end subroutine require_counter_close

  subroutine copy_c_string(value, buffer)
    character(len=*), intent(in) :: value
    character(kind=c_char), intent(out) :: buffer(:)
    integer :: character_index
    integer :: copy_len

    buffer = c_null_char
    copy_len = min(len_trim(value), size(buffer) - 1)
    do character_index = 1, max(0, copy_len)
      buffer(character_index) = value(character_index:character_index)
    end do
  end subroutine copy_c_string

  subroutine require_owned_connection(network, protocol, local_port, pid, state, message)
    type(network_table), intent(in) :: network
    character(len=*), intent(in) :: protocol
    integer, intent(in) :: local_port
    integer, intent(in) :: pid
    character(len=*), intent(in) :: state
    character(len=*), intent(in) :: message
    integer :: connection_index
    logical :: found

    if (.not. allocated(network%connections)) error stop "Linux network connections must be allocated"
    found = .false.
    do connection_index = 1, size(network%connections)
      if (.not. network%connections(connection_index)%valid) cycle
      if (trim(network%connections(connection_index)%protocol) /= protocol) cycle
      if (network%connections(connection_index)%local_port /= local_port) cycle
      if (network%connections(connection_index)%pid /= pid) cycle
      if (trim(network%connections(connection_index)%state) /= state) error stop "Linux owned socket state mismatch"
      if (len_trim(network%connections(connection_index)%process_name) <= 0) then
        error stop "Linux owned socket process name must not be empty"
      end if
      found = .true.
      exit
    end do
    if (.not. found) error stop message
  end subroutine require_owned_connection
end program test_linux_platform
