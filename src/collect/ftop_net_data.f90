module ftop_net_data
  use, intrinsic :: iso_fortran_env, only : int64, real64
  implicit none
  private

  integer, parameter, public :: NET_INTERFACE_NAME_LEN = 32
  integer, parameter, public :: NET_STATE_LEN = 16
  integer, parameter, public :: NET_PROTOCOL_LEN = 8
  integer, parameter, public :: NET_ADDRESS_LEN = 64
  integer, parameter, public :: NET_PROCESS_NAME_LEN = 64
  integer, parameter, public :: NET_HISTORY_CAPACITY = 300

  type, public :: interface_info
    logical :: valid = .false.
    character(len=NET_INTERFACE_NAME_LEN) :: name = ""
    integer(int64) :: rx_bytes = 0_int64
    integer(int64) :: tx_bytes = 0_int64
    integer(int64) :: rx_packets = 0_int64
    integer(int64) :: tx_packets = 0_int64
    character(len=NET_STATE_LEN) :: state = ""
    integer :: speed_mbps = 0
    integer :: mtu = 0
    real(real64) :: rx_bytes_per_sec = 0.0_real64
    real(real64) :: tx_bytes_per_sec = 0.0_real64
    integer :: history_count = 0
    real(real64) :: rx_history(NET_HISTORY_CAPACITY) = 0.0_real64
    real(real64) :: tx_history(NET_HISTORY_CAPACITY) = 0.0_real64
  end type interface_info

  type, public :: net_connection
    logical :: valid = .false.
    character(len=NET_PROTOCOL_LEN) :: protocol = ""
    character(len=NET_ADDRESS_LEN) :: local_addr = ""
    integer :: local_port = 0
    character(len=NET_ADDRESS_LEN) :: remote_addr = ""
    integer :: remote_port = 0
    character(len=NET_STATE_LEN) :: state = ""
    integer(int64) :: inode = 0_int64
    integer :: pid = 0
    character(len=NET_PROCESS_NAME_LEN) :: process_name = ""
  end type net_connection

  type, public :: process_bandwidth
    logical :: valid = .false.
    integer :: pid = 0
    character(len=NET_PROCESS_NAME_LEN) :: process_name = ""
    real(real64) :: rx_bytes_per_sec = 0.0_real64
    real(real64) :: tx_bytes_per_sec = 0.0_real64
  end type process_bandwidth

  type, public :: network_table
    logical :: valid = .false.
    type(interface_info), allocatable :: interfaces(:)
    type(net_connection), allocatable :: connections(:)
    type(process_bandwidth), allocatable :: processes(:)
  end type network_table

  public :: append_interface_histories
  public :: assign_interface_rates
  public :: decode_linux_ipv4_endpoint
  public :: format_byte_rate
  public :: parse_linux_proc_net_connections
  public :: parse_linux_proc_net_dev

contains

  function parse_linux_proc_net_dev(text, include_loopback) result(table)
    character(len=*), intent(in) :: text
    logical, intent(in), optional :: include_loopback
    type(network_table) :: table
    type(interface_info), allocatable :: parsed(:)
    integer :: cursor
    integer :: line_end
    integer :: line_start
    integer :: match_count
    logical :: keep_loopback

    keep_loopback = .false.
    if (present(include_loopback)) keep_loopback = include_loopback

    match_count = 0
    cursor = 1
    do while (cursor <= len(text))
      line_start = cursor
      line_end = next_line_end(text, line_start)
      if (linux_net_dev_line_matches(text(line_start:line_end), keep_loopback)) match_count = match_count + 1
      cursor = line_end + 2
    end do

    allocate(parsed(match_count))
    match_count = 0
    table%valid = .true.
    cursor = 1
    do while (cursor <= len(text))
      line_start = cursor
      line_end = next_line_end(text, line_start)
      if (parse_linux_net_dev_line(text(line_start:line_end), keep_loopback, parsed, match_count)) table%valid = .true.
      cursor = line_end + 2
    end do

    call move_alloc(parsed, table%interfaces)
    allocate(table%connections(0))
    allocate(table%processes(0))
  end function parse_linux_proc_net_dev

  function parse_linux_proc_net_connections(tcp_text, udp_text) result(connections)
    character(len=*), intent(in) :: tcp_text
    character(len=*), intent(in) :: udp_text
    type(net_connection), allocatable :: connections(:)
    integer :: connection_count

    connection_count = count_linux_proc_net_connections(tcp_text, "tcp") + &
                       count_linux_proc_net_connections(udp_text, "udp")
    allocate(connections(connection_count))
    connection_count = 0
    call append_linux_proc_net_connections(tcp_text, "tcp", connections, connection_count)
    call append_linux_proc_net_connections(udp_text, "udp", connections, connection_count)
  end function parse_linux_proc_net_connections

  integer function count_linux_proc_net_connections(text, protocol) result(connection_count)
    character(len=*), intent(in) :: text
    character(len=*), intent(in) :: protocol
    integer :: cursor
    integer :: line_end
    integer :: line_start
    type(net_connection) :: connection

    connection_count = 0
    cursor = 1
    do while (cursor <= len(text))
      line_start = cursor
      line_end = next_line_end(text, line_start)
      if (parse_linux_proc_net_connection_line(text(line_start:line_end), protocol, connection)) then
        connection_count = connection_count + 1
      end if
      cursor = line_end + 2
    end do
  end function count_linux_proc_net_connections

  subroutine append_linux_proc_net_connections(text, protocol, connections, connection_count)
    character(len=*), intent(in) :: text
    character(len=*), intent(in) :: protocol
    type(net_connection), intent(inout) :: connections(:)
    integer, intent(inout) :: connection_count
    integer :: cursor
    integer :: line_end
    integer :: line_start
    type(net_connection) :: connection

    cursor = 1
    do while (cursor <= len(text))
      line_start = cursor
      line_end = next_line_end(text, line_start)
      if (parse_linux_proc_net_connection_line(text(line_start:line_end), protocol, connection)) then
        if (connection_count < size(connections)) then
          connection_count = connection_count + 1
          connections(connection_count) = connection
        end if
      end if
      cursor = line_end + 2
    end do
  end subroutine append_linux_proc_net_connections

  logical function parse_linux_proc_net_connection_line(line, protocol, connection) result(parsed)
    character(len=*), intent(in) :: line
    character(len=*), intent(in) :: protocol
    type(net_connection), intent(out) :: connection
    character(len=64) :: local_endpoint
    character(len=64) :: remote_endpoint
    character(len=16) :: slot
    character(len=16) :: state_hex
    character(len=32) :: retransmit_timeout
    character(len=32) :: timer_when
    character(len=32) :: timeout
    character(len=32) :: transmit_receive_queue
    integer :: local_separator
    integer :: read_status
    integer :: remote_separator
    integer(int64) :: inode
    integer(int64) :: uid

    parsed = .false.
    connection = net_connection()
    local_endpoint = ""
    remote_endpoint = ""
    state_hex = ""
    read(line, *, iostat=read_status) slot, local_endpoint, remote_endpoint, state_hex, &
      transmit_receive_queue, timer_when, retransmit_timeout, uid, timeout, inode
    if (read_status /= 0) return

    local_separator = index(local_endpoint, ":")
    remote_separator = index(remote_endpoint, ":")
    if (local_separator <= 1 .or. remote_separator <= 1) return
    if (.not. decode_linux_ipv4_endpoint(local_endpoint(:local_separator - 1), &
                                         local_endpoint(local_separator + 1:), &
                                         connection%local_addr, connection%local_port)) return
    if (.not. decode_linux_ipv4_endpoint(remote_endpoint(:remote_separator - 1), &
                                         remote_endpoint(remote_separator + 1:), &
                                         connection%remote_addr, connection%remote_port)) return

    connection%valid = .true.
    connection%protocol = bounded_text(protocol, len(connection%protocol))
    connection%state = bounded_text(linux_connection_state_label(protocol, state_hex), len(connection%state))
    connection%inode = max(0_int64, inode)
    parsed = connection%inode > 0_int64
  end function parse_linux_proc_net_connection_line

  logical function linux_net_dev_line_matches(line, include_loopback) result(matches)
    character(len=*), intent(in) :: line
    logical, intent(in) :: include_loopback
    character(len=:), allocatable :: interface_name
    character(len=:), allocatable :: values
    integer :: colon
    integer :: status
    integer(int64) :: fields(16)

    matches = .false.
    colon = index(line, ":")
    if (colon <= 1) return
    interface_name = trim(adjustl(line(:colon - 1)))
    if (len(interface_name) <= 0) return
    if (.not. include_loopback .and. interface_name == "lo") return
    values = line(colon + 1:)
    fields = 0_int64
    read(values, *, iostat=status) fields
    if (status /= 0) return
    matches = .true.
  end function linux_net_dev_line_matches

  logical function parse_linux_net_dev_line(line, include_loopback, interfaces, count) result(parsed)
    character(len=*), intent(in) :: line
    logical, intent(in) :: include_loopback
    type(interface_info), intent(inout) :: interfaces(:)
    integer, intent(inout) :: count
    character(len=:), allocatable :: interface_name
    character(len=:), allocatable :: values
    integer :: colon
    integer :: status
    integer(int64) :: fields(16)

    parsed = .false.
    colon = index(line, ":")
    if (colon <= 1) return
    interface_name = trim(adjustl(line(:colon - 1)))
    if (len(interface_name) <= 0) return
    if (.not. include_loopback .and. interface_name == "lo") return

    values = line(colon + 1:)
    fields = 0_int64
    read(values, *, iostat=status) fields
    if (status /= 0) return
    if (count >= size(interfaces)) return

    count = count + 1
    interfaces(count)%valid = .true.
    interfaces(count)%name = bounded_text(interface_name, len(interfaces(count)%name))
    interfaces(count)%rx_bytes = max(0_int64, fields(1))
    interfaces(count)%rx_packets = max(0_int64, fields(2))
    interfaces(count)%tx_bytes = max(0_int64, fields(9))
    interfaces(count)%tx_packets = max(0_int64, fields(10))
    parsed = .true.
  end function parse_linux_net_dev_line

  subroutine assign_interface_rates(current, previous, elapsed_ms)
    type(network_table), intent(inout) :: current
    type(network_table), intent(in) :: previous
    integer(int64), intent(in) :: elapsed_ms
    integer :: current_index
    integer :: previous_index

    if (.not. allocated(current%interfaces)) return
    current%interfaces%rx_bytes_per_sec = 0.0_real64
    current%interfaces%tx_bytes_per_sec = 0.0_real64
    if (.not. current%valid .or. .not. previous%valid) return
    if (.not. allocated(previous%interfaces)) return
    if (elapsed_ms <= 0_int64) return

    do current_index = 1, size(current%interfaces)
      if (.not. current%interfaces(current_index)%valid) cycle
      previous_index = matching_previous_interface(previous, current%interfaces(current_index)%name)
      if (previous_index <= 0) cycle
      call assign_interface_rate(current%interfaces(current_index), previous%interfaces(previous_index), elapsed_ms)
    end do
  end subroutine assign_interface_rates

  subroutine append_interface_histories(current, previous)
    type(network_table), intent(inout) :: current
    type(network_table), intent(in) :: previous
    integer :: current_index
    integer :: previous_index

    if (.not. allocated(current%interfaces)) return
    do current_index = 1, size(current%interfaces)
      current%interfaces(current_index)%history_count = 0
      current%interfaces(current_index)%rx_history = 0.0_real64
      current%interfaces(current_index)%tx_history = 0.0_real64
      if (.not. current%interfaces(current_index)%valid) cycle

      previous_index = 0
      if (previous%valid .and. allocated(previous%interfaces)) then
        previous_index = matching_previous_interface(previous, current%interfaces(current_index)%name)
      end if
      if (previous_index > 0) call copy_interface_history(current%interfaces(current_index), previous%interfaces(previous_index))
      call append_interface_history_sample(current%interfaces(current_index), &
                                           current%interfaces(current_index)%rx_bytes_per_sec, &
                                           current%interfaces(current_index)%tx_bytes_per_sec)
    end do
  end subroutine append_interface_histories

  subroutine assign_interface_rate(current, previous, elapsed_ms)
    type(interface_info), intent(inout) :: current
    type(interface_info), intent(in) :: previous
    integer(int64), intent(in) :: elapsed_ms
    integer(int64) :: rx_delta
    integer(int64) :: tx_delta

    if (.not. current%valid .or. .not. previous%valid) return
    rx_delta = current%rx_bytes - previous%rx_bytes
    tx_delta = current%tx_bytes - previous%tx_bytes
    if (rx_delta > 0_int64) current%rx_bytes_per_sec = bytes_per_second(rx_delta, elapsed_ms)
    if (tx_delta > 0_int64) current%tx_bytes_per_sec = bytes_per_second(tx_delta, elapsed_ms)
  end subroutine assign_interface_rate

  real(real64) function bytes_per_second(byte_delta, elapsed_ms) result(rate)
    integer(int64), intent(in) :: byte_delta
    integer(int64), intent(in) :: elapsed_ms

    rate = 0.0_real64
    if (byte_delta <= 0_int64 .or. elapsed_ms <= 0_int64) return
    rate = 1000.0_real64 * real(byte_delta, real64) / real(elapsed_ms, real64)
  end function bytes_per_second

  integer function matching_previous_interface(table, interface_name) result(item_index)
    type(network_table), intent(in) :: table
    character(len=*), intent(in) :: interface_name
    integer :: candidate

    item_index = 0
    if (.not. allocated(table%interfaces)) return
    do candidate = 1, size(table%interfaces)
      if (.not. table%interfaces(candidate)%valid) cycle
      if (trim(table%interfaces(candidate)%name) == trim(interface_name)) then
        item_index = candidate
        return
      end if
    end do
  end function matching_previous_interface

  subroutine copy_interface_history(current, previous)
    type(interface_info), intent(inout) :: current
    type(interface_info), intent(in) :: previous
    integer :: history_count

    history_count = bounded_history_count(previous%history_count)
    if (history_count <= 0) return
    current%history_count = history_count
    current%rx_history(:history_count) = previous%rx_history(:history_count)
    current%tx_history(:history_count) = previous%tx_history(:history_count)
  end subroutine copy_interface_history

  subroutine append_interface_history_sample(interface, rx_rate, tx_rate)
    type(interface_info), intent(inout) :: interface
    real(real64), intent(in) :: rx_rate
    real(real64), intent(in) :: tx_rate
    integer :: history_index

    if (.not. interface%valid) return
    interface%history_count = bounded_history_count(interface%history_count)
    if (interface%history_count < NET_HISTORY_CAPACITY) then
      interface%history_count = interface%history_count + 1
    else
      do history_index = 1, NET_HISTORY_CAPACITY - 1
        interface%rx_history(history_index) = interface%rx_history(history_index + 1)
        interface%tx_history(history_index) = interface%tx_history(history_index + 1)
      end do
    end if

    interface%rx_history(interface%history_count) = max(0.0_real64, rx_rate)
    interface%tx_history(interface%history_count) = max(0.0_real64, tx_rate)
  end subroutine append_interface_history_sample

  integer function bounded_history_count(history_count) result(bounded)
    integer, intent(in) :: history_count

    bounded = max(0, min(NET_HISTORY_CAPACITY, history_count))
  end function bounded_history_count

  function format_byte_rate(bytes_per_sec) result(text)
    real(real64), intent(in) :: bytes_per_sec
    character(len=:), allocatable :: text
    character(len=32) :: buffer
    real(real64) :: value

    value = max(0.0_real64, bytes_per_sec)
    if (value < 1024.0_real64) then
      write(buffer, '(I0, A)') int(value), " B/s"
    else if (value < 1024.0_real64 * 1024.0_real64) then
      write(buffer, '(F0.1, A)') value / 1024.0_real64, " KB/s"
    else if (value < 1024.0_real64 * 1024.0_real64 * 1024.0_real64) then
      write(buffer, '(F0.1, A)') value / (1024.0_real64 * 1024.0_real64), " MB/s"
    else
      write(buffer, '(F0.1, A)') value / (1024.0_real64 * 1024.0_real64 * 1024.0_real64), " GB/s"
    end if
    text = trim(adjustl(buffer))
  end function format_byte_rate

  logical function decode_linux_ipv4_endpoint(hex_addr, hex_port, address, port) result(success)
    character(len=*), intent(in) :: hex_addr
    character(len=*), intent(in) :: hex_port
    character(len=*), intent(out) :: address
    integer, intent(out) :: port
    integer :: bytes(4)
    integer :: parsed_port

    success = .false.
    address = ""
    port = 0
    if (len_trim(hex_addr) /= 8 .or. len_trim(hex_port) /= 4) return
    if (.not. parse_hex_byte(hex_addr(7:8), bytes(1))) return
    if (.not. parse_hex_byte(hex_addr(5:6), bytes(2))) return
    if (.not. parse_hex_byte(hex_addr(3:4), bytes(3))) return
    if (.not. parse_hex_byte(hex_addr(1:2), bytes(4))) return
    if (.not. parse_hex_int(hex_port(:4), parsed_port)) return

    write(address, '(I0, A, I0, A, I0, A, I0)') bytes(1), ".", bytes(2), ".", bytes(3), ".", bytes(4)
    port = parsed_port
    success = .true.
  end function decode_linux_ipv4_endpoint

  function linux_connection_state_label(protocol, state_hex) result(label)
    character(len=*), intent(in) :: protocol
    character(len=*), intent(in) :: state_hex
    character(len=:), allocatable :: label
    integer :: state_value

    if (.not. parse_hex_int(trim(state_hex), state_value)) then
      label = "UNKNOWN"
      return
    end if

    if (trim(protocol) == "udp" .and. state_value == 7) then
      label = "OPEN"
      return
    end if

    select case (state_value)
    case (1)
      label = "ESTABLISHED"
    case (2)
      label = "SYN_SENT"
    case (3)
      label = "SYN_RECV"
    case (4)
      label = "FIN_WAIT1"
    case (5)
      label = "FIN_WAIT2"
    case (6)
      label = "TIME_WAIT"
    case (7)
      label = "CLOSE"
    case (8)
      label = "CLOSE_WAIT"
    case (9)
      label = "LAST_ACK"
    case (10)
      label = "LISTEN"
    case (11)
      label = "CLOSING"
    case default
      label = "UNKNOWN"
    end select
  end function linux_connection_state_label

  logical function parse_hex_byte(text, value) result(success)
    character(len=*), intent(in) :: text
    integer, intent(out) :: value

    success = parse_hex_int(text, value)
    if (.not. success) return
    success = value >= 0 .and. value <= 255
  end function parse_hex_byte

  logical function parse_hex_int(text, value) result(success)
    character(len=*), intent(in) :: text
    integer, intent(out) :: value
    integer :: digit
    integer :: index_value

    success = .false.
    value = 0
    if (len_trim(text) <= 0) return
    do index_value = 1, len_trim(text)
      digit = hex_digit_value(text(index_value:index_value))
      if (digit < 0) return
      value = value * 16 + digit
    end do
    success = .true.
  end function parse_hex_int

  integer function hex_digit_value(char) result(value)
    character(len=1), intent(in) :: char
    integer :: code

    code = iachar(char)
    select case (char)
    case ("0":"9")
      value = code - iachar("0")
    case ("A":"F")
      value = code - iachar("A") + 10
    case ("a":"f")
      value = code - iachar("a") + 10
    case default
      value = -1
    end select
  end function hex_digit_value

  integer function next_line_end(text, line_start) result(line_end)
    character(len=*), intent(in) :: text
    integer, intent(in) :: line_start
    integer :: newline

    newline = index(text(line_start:), new_line("a"))
    if (newline <= 0) then
      line_end = len(text)
    else
      line_end = line_start + newline - 2
    end if
  end function next_line_end

  function bounded_text(value, capacity) result(text)
    character(len=*), intent(in) :: value
    integer, intent(in) :: capacity
    character(len=capacity) :: text
    integer :: copy_len

    text = ""
    copy_len = min(len_trim(value), capacity)
    if (copy_len > 0) text(:copy_len) = value(:copy_len)
  end function bounded_text

end module ftop_net_data
