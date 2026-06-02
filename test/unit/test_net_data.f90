program test_net_data
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use ftop_net_data, only : append_interface_histories, assign_interface_rates, assign_process_bandwidth_rates, &
                             default_network_interface_filters, &
                             decode_linux_ipv4_endpoint, decode_linux_ipv6_endpoint, format_byte_rate, &
                             net_connection, network_interface_filters, network_table, &
                             parse_linux_proc_net_connections, parse_linux_proc_net_dev, set_network_interface_filters
  use ftop_services, only : parse_services, service_entry, service_name_for
  implicit none

  call test_linux_proc_net_dev_parser()
  call test_linux_proc_net_connection_parser()
  call test_large_linux_proc_net_connection_parser()
  call test_interface_rates()
  call test_process_bandwidth_rates()
  call test_interface_histories()
  call test_services_parser()
  call test_byte_rate_formatting()
  call test_linux_ipv4_endpoint_decoder()
  call test_linux_ipv6_endpoint_decoder()

contains

  subroutine test_linux_proc_net_dev_parser()
    type(network_table) :: table
    type(network_interface_filters) :: filters
    type(network_table) :: with_filters
    type(network_table) :: with_loopback
    character(len=*), parameter :: text = &
      "Inter-|   Receive                                                |  Transmit" // new_line("a") // &
      " face |bytes    packets errs drop fifo frame compressed multicast|bytes    packets errs drop fifo colls carrier compressed" // new_line("a") // &
      "    lo: 100 1 0 0 0 0 0 0 200 2 0 0 0 0 0 0" // new_line("a") // &
      "  eth0: 123456 789 1 2 3 4 5 6 654321 987 6 5 4 3 2 1" // new_line("a") // &
      " wlan0: 12 3 0 0 0 0 0 0 45 6 0 0 0 0 0 0"

    table = parse_linux_proc_net_dev(text)
    call require(table%valid, "network table should be valid")
    call require(allocated(table%interfaces), "network interfaces should be allocated")
    call require(size(table%interfaces) == 2, "network parser should hide loopback by default")
    call require(trim(table%interfaces(1)%name) == "eth0", "network parser should parse interface name")
    call require(table%interfaces(1)%rx_bytes == 123456_int64, "network parser should parse rx bytes")
    call require(table%interfaces(1)%rx_packets == 789_int64, "network parser should parse rx packets")
    call require(table%interfaces(1)%rx_errs == 1_int64, "network parser should parse rx errors")
    call require(table%interfaces(1)%rx_drop == 2_int64, "network parser should parse rx drops")
    call require(table%interfaces(1)%rx_fifo == 3_int64, "network parser should parse rx fifo errors")
    call require(table%interfaces(1)%rx_frame == 4_int64, "network parser should parse rx frame errors")
    call require(table%interfaces(1)%rx_compressed == 5_int64, "network parser should parse rx compressed packets")
    call require(table%interfaces(1)%rx_multicast == 6_int64, "network parser should parse rx multicast packets")
    call require(table%interfaces(1)%tx_bytes == 654321_int64, "network parser should parse tx bytes")
    call require(table%interfaces(1)%tx_packets == 987_int64, "network parser should parse tx packets")
    call require(table%interfaces(1)%tx_errs == 6_int64, "network parser should parse tx errors")
    call require(table%interfaces(1)%tx_drop == 5_int64, "network parser should parse tx drops")
    call require(table%interfaces(1)%tx_fifo == 4_int64, "network parser should parse tx fifo errors")
    call require(table%interfaces(1)%tx_colls == 3_int64, "network parser should parse tx collisions")
    call require(table%interfaces(1)%tx_carrier == 2_int64, "network parser should parse tx carrier errors")
    call require(table%interfaces(1)%tx_compressed == 1_int64, "network parser should parse tx compressed packets")
    call require(trim(table%interfaces(2)%name) == "wlan0", "network parser should parse second interface")

    with_loopback = parse_linux_proc_net_dev(text, include_loopback=.true.)
    call require(size(with_loopback%interfaces) == 3, "network parser should optionally include loopback")
    call require(trim(with_loopback%interfaces(1)%name) == "lo", "network parser should keep loopback when requested")

    filters%include_loopback = .true.
    filters%exclude_count = 1
    filters%exclude_patterns(1) = "wlan*"
    call set_network_interface_filters(filters)
    with_filters = parse_linux_proc_net_dev(text)
    call set_network_interface_filters(default_network_interface_filters())
    call require(size(with_filters%interfaces) == 2, "network parser should apply configured interface filters")
    call require(trim(with_filters%interfaces(1)%name) == "lo", "network filters should preserve configured loopback")
    call require(trim(with_filters%interfaces(2)%name) == "eth0", "network filters should exclude matching patterns")
  end subroutine test_linux_proc_net_dev_parser

  subroutine test_linux_proc_net_connection_parser()
    type(net_connection), allocatable :: connections(:)
    character(len=*), parameter :: tcp_text = &
      "  sl  local_address rem_address   st tx_queue rx_queue tr tm->when retrnsmt   uid  timeout inode" // &
      new_line("a") // &
      "   0: 0100007F:1F90 0200000A:01BB 01 00000000:00000000 00:00000000 00000000 1000 0 12345" // &
      new_line("a") // &
      "   1: 00000000:0016 00000000:0000 0A 00000000:00000000 00:00000000 00000000 0 0 54321"
    character(len=*), parameter :: udp_text = &
      "  sl  local_address rem_address   st tx_queue rx_queue tr tm->when retrnsmt   uid  timeout inode" // &
      new_line("a") // &
      "   0: 00000000:0035 00000000:0000 07 00000000:00000000 00:00000000 00000000 0 0 67890"
    character(len=*), parameter :: tcp6_text = &
      "  sl  local_address                         remote_address                        st tx_queue rx_queue tr tm->when retrnsmt   uid  timeout inode" // &
      new_line("a") // &
      "   0: B80D0120000000000000000001000000:1F90 00000000000000000000000001000000:01BB 01 00000000:00000000 00:00000000 00000000 1000 0 98765"
    character(len=*), parameter :: udp6_text = &
      "  sl  local_address                         remote_address                        st tx_queue rx_queue tr tm->when retrnsmt   uid  timeout inode" // &
      new_line("a") // &
      "   0: 00000000000000000000000000000000:0035 00000000000000000000000000000000:0000 07 00000000:00000000 00:00000000 00000000 0 0 87654"

    connections = parse_linux_proc_net_connections(tcp_text, udp_text, tcp6_text, udp6_text)
    call require(size(connections) == 5, "network connection parser should parse tcp, udp, tcp6, and udp6 rows")
    call require(connections(1)%valid, "network connection parser should mark tcp row valid")
    call require(trim(connections(1)%protocol) == "tcp", "network connection parser should set tcp protocol")
    call require(trim(connections(1)%local_addr) == "127.0.0.1", "network connection parser local address mismatch")
    call require(connections(1)%local_port == 8080, "network connection parser local port mismatch")
    call require(trim(connections(1)%remote_addr) == "10.0.0.2", "network connection parser remote address mismatch")
    call require(connections(1)%remote_port == 443, "network connection parser remote port mismatch")
    call require(trim(connections(1)%state) == "ESTABLISHED", "network connection parser state mismatch")
    call require(connections(1)%inode == 12345_int64, "network connection parser inode mismatch")
    call require(trim(connections(2)%state) == "LISTEN", "network connection parser should decode listen state")
    call require(trim(connections(3)%protocol) == "udp", "network connection parser should set udp protocol")
    call require(trim(connections(3)%state) == "OPEN", "network connection parser should label udp state")
    call require(connections(3)%local_port == 53, "network connection parser udp port mismatch")
    call require(trim(connections(4)%local_addr) == "2001:db8::1", "network connection parser tcp6 local address mismatch")
    call require(trim(connections(4)%remote_addr) == "::1", "network connection parser tcp6 remote address mismatch")
    call require(connections(4)%inode == 98765_int64, "network connection parser tcp6 inode mismatch")
    call require(trim(connections(5)%local_addr) == "::", "network connection parser udp6 wildcard address mismatch")
    call require(trim(connections(5)%state) == "OPEN", "network connection parser should label udp6 state")
  end subroutine test_linux_proc_net_connection_parser

  subroutine test_large_linux_proc_net_connection_parser()
    integer, parameter :: connection_count = 2048
    type(net_connection), allocatable :: connections(:)

    connections = parse_linux_proc_net_connections(linux_proc_net_tcp_text(connection_count), "", "", "")
    call require(size(connections) == connection_count, "network parser should handle large connection tables")
    call require(all(connections%valid), "large connection table rows should be valid")
    call require(trim(connections(connection_count)%protocol) == "tcp", "large connection parser protocol mismatch")
    call require(connections(connection_count)%local_port == 10000 + mod(connection_count, 40000), &
                 "large connection parser local port mismatch")
    call require(connections(connection_count)%inode == 500000_int64 + int(connection_count, int64), &
                 "large connection parser inode mismatch")
  end subroutine test_large_linux_proc_net_connection_parser

  subroutine test_interface_rates()
    type(network_table) :: previous
    type(network_table) :: current

    previous = sample_table([1000_int64, 5000_int64], [2000_int64, 7000_int64])
    current = sample_table([2500_int64, 4500_int64], [5000_int64, 7600_int64])
    call assign_interface_rates(current, previous, 500_int64)

    call require(near(current%interfaces(1)%rx_bytes_per_sec, 3000.0_real64), &
                 "network rx rate should use elapsed delta")
    call require(near(current%interfaces(1)%tx_bytes_per_sec, 6000.0_real64), &
                 "network tx rate should use elapsed delta")
    call require(near(current%interfaces(2)%rx_bytes_per_sec, 0.0_real64), &
                 "network rx rate should ignore counter reset")
    call require(near(current%interfaces(2)%tx_bytes_per_sec, 1200.0_real64), &
                 "network tx rate should preserve matching interface order")

    current%interfaces%rx_bytes_per_sec = 99.0_real64
    current%interfaces%tx_bytes_per_sec = 99.0_real64
    call assign_interface_rates(current, previous, 0_int64)
    call require(all(current%interfaces%rx_bytes_per_sec == 0.0_real64), "network invalid elapsed should reset rx rates")
    call require(all(current%interfaces%tx_bytes_per_sec == 0.0_real64), "network invalid elapsed should reset tx rates")
  end subroutine test_interface_rates

  subroutine test_process_bandwidth_rates()
    type(network_table) :: previous
    type(network_table) :: current

    previous = sample_process_bandwidth_table([101, 202], [11_int64, 22_int64], &
                                              [1000_int64, 5000_int64], [2000_int64, 7000_int64])
    current = sample_process_bandwidth_table([101, 202], [11_int64, 33_int64], &
                                             [2500_int64, 9000_int64], [5000_int64, 12000_int64])
    call assign_process_bandwidth_rates(current, previous, 500_int64)

    call require(near(current%processes(1)%rx_bytes_per_sec, 3000.0_real64), &
                 "process bandwidth rx rate should use pid and start time")
    call require(near(current%processes(1)%tx_bytes_per_sec, 6000.0_real64), &
                 "process bandwidth tx rate should use pid and start time")
    call require(near(current%processes(2)%rx_bytes_per_sec, 0.0_real64), &
                 "process bandwidth should ignore reused pid start time mismatch")
    call require(near(current%processes(2)%tx_bytes_per_sec, 0.0_real64), &
                 "process bandwidth should leave unmatched tx rate at zero")

    current%processes%rx_bytes_per_sec = 99.0_real64
    current%processes%tx_bytes_per_sec = 99.0_real64
    call assign_process_bandwidth_rates(current, previous, 0_int64)
    call require(all(current%processes%rx_bytes_per_sec == 0.0_real64), &
                 "process bandwidth invalid elapsed should reset rx rates")
    call require(all(current%processes%tx_bytes_per_sec == 0.0_real64), &
                 "process bandwidth invalid elapsed should reset tx rates")
  end subroutine test_process_bandwidth_rates

  subroutine test_interface_histories()
    type(network_table) :: previous
    type(network_table) :: current
    type(network_table) :: empty

    previous = sample_table([100_int64], [200_int64])
    previous%interfaces(1)%rx_bytes_per_sec = 10.0_real64
    previous%interfaces(1)%tx_bytes_per_sec = 20.0_real64
    call append_interface_histories(previous, empty)

    current = sample_table([300_int64], [500_int64])
    current%interfaces(1)%rx_bytes_per_sec = 30.0_real64
    current%interfaces(1)%tx_bytes_per_sec = 40.0_real64
    call append_interface_histories(current, previous)

    call require(current%interfaces(1)%history_count == 2, "network histories should append samples")
    call require(near(current%interfaces(1)%rx_history(1), 10.0_real64), "network histories should copy rx history")
    call require(near(current%interfaces(1)%tx_history(1), 20.0_real64), "network histories should copy tx history")
    call require(near(current%interfaces(1)%rx_history(2), 30.0_real64), "network histories should append rx rate")
    call require(near(current%interfaces(1)%tx_history(2), 40.0_real64), "network histories should append tx rate")
  end subroutine test_interface_histories

  subroutine test_services_parser()
    type(service_entry), allocatable :: services(:)
    character(len=*), parameter :: text = &
      "# service aliases are ignored" // new_line("a") // &
      "ssh             22/tcp" // new_line("a") // &
      "domain          53/udp" // new_line("a") // &
      "https           443/TCP" // new_line("a") // &
      "bad             nope/tcp" // new_line("a") // &
      "too-high        70000/tcp" // new_line("a") // &
      "custom-http     8080/tcp  webcache # comment"

    services = parse_services(text)
    call require(size(services) == 4, "services parser should keep valid service rows")
    call require(trim(services(1)%name) == "ssh", "services parser should parse service name")
    call require(services(1)%port == 22, "services parser should parse service port")
    call require(trim(services(1)%protocol) == "tcp", "services parser should parse service protocol")
    call require(service_name_for(services, 443, "tcp") == "https", "service lookup should match tcp port")
    call require(service_name_for(services, 443, "TCP") == "https", "service lookup should normalize protocol")
    call require(service_name_for(services, 53, "udp") == "domain", "service lookup should match udp port")
    call require(service_name_for(services, 53, "tcp") == "", "service lookup should respect protocol")
    call require(service_name_for(services, 8080, "tcp") == "custom-http", &
                 "service lookup should keep non-standard services")
  end subroutine test_services_parser

  subroutine test_byte_rate_formatting()
    call require(format_byte_rate(0.0_real64) == "0 B/s", "zero byte rate format mismatch")
    call require(format_byte_rate(512.0_real64) == "512 B/s", "byte rate format mismatch")
    call require(format_byte_rate(1536.0_real64) == "1.5 KB/s", "kilobyte rate format mismatch")
    call require(format_byte_rate(2.0_real64 * 1024.0_real64 * 1024.0_real64) == "2.0 MB/s", &
                 "megabyte rate format mismatch")
    call require(format_byte_rate(3.0_real64 * 1024.0_real64 * 1024.0_real64 * 1024.0_real64) == "3.0 GB/s", &
                 "gigabyte rate format mismatch")
  end subroutine test_byte_rate_formatting

  subroutine test_linux_ipv4_endpoint_decoder()
    character(len=64) :: address
    integer :: port

    call require(decode_linux_ipv4_endpoint("0100007F", "1F90", address, port), &
                 "linux ipv4 endpoint decoder should accept valid endpoint")
    call require(trim(address) == "127.0.0.1", "linux ipv4 endpoint address mismatch")
    call require(port == 8080, "linux ipv4 endpoint port mismatch")
    call require(decode_linux_ipv4_endpoint("00000000", "0016", address, port), &
                 "linux ipv4 endpoint decoder should parse wildcard")
    call require(trim(address) == "0.0.0.0", "linux ipv4 wildcard address mismatch")
    call require(port == 22, "linux ipv4 wildcard port mismatch")
    call require(.not. decode_linux_ipv4_endpoint("not-hex!", "0016", address, port), &
                 "linux ipv4 endpoint decoder should reject bad address")
  end subroutine test_linux_ipv4_endpoint_decoder

  subroutine test_linux_ipv6_endpoint_decoder()
    character(len=64) :: address
    integer :: port

    call require(decode_linux_ipv6_endpoint("00000000000000000000000001000000", "1F90", address, port), &
                 "linux ipv6 endpoint decoder should accept loopback endpoint")
    call require(trim(address) == "::1", "linux ipv6 loopback address mismatch")
    call require(port == 8080, "linux ipv6 loopback port mismatch")
    call require(decode_linux_ipv6_endpoint("B80D0120000000000000000001000000", "01BB", address, port), &
                 "linux ipv6 endpoint decoder should accept global endpoint")
    call require(trim(address) == "2001:db8::1", "linux ipv6 global address mismatch")
    call require(port == 443, "linux ipv6 global port mismatch")
    call require(decode_linux_ipv6_endpoint("00000000000000000000000000000000", "0035", address, port), &
                 "linux ipv6 endpoint decoder should parse wildcard")
    call require(trim(address) == "::", "linux ipv6 wildcard address mismatch")
    call require(port == 53, "linux ipv6 wildcard port mismatch")
    call require(.not. decode_linux_ipv6_endpoint("not-hex", "0016", address, port), &
                 "linux ipv6 endpoint decoder should reject bad address")
  end subroutine test_linux_ipv6_endpoint_decoder

  function sample_table(rx_bytes, tx_bytes) result(table)
    integer(int64), intent(in) :: rx_bytes(:)
    integer(int64), intent(in) :: tx_bytes(:)
    type(network_table) :: table
    integer :: index_value

    table%valid = .true.
    allocate(table%interfaces(size(rx_bytes)))
    allocate(table%connections(0))
    allocate(table%processes(0))
    do index_value = 1, size(rx_bytes)
      table%interfaces(index_value)%valid = .true.
      write(table%interfaces(index_value)%name, '(A, I0)') "eth", index_value
      table%interfaces(index_value)%rx_bytes = rx_bytes(index_value)
      table%interfaces(index_value)%tx_bytes = tx_bytes(index_value)
    end do
  end function sample_table

  function sample_process_bandwidth_table(pids, start_times, rx_bytes, tx_bytes) result(table)
    integer, intent(in) :: pids(:)
    integer(int64), intent(in) :: start_times(:)
    integer(int64), intent(in) :: rx_bytes(:)
    integer(int64), intent(in) :: tx_bytes(:)
    type(network_table) :: table
    integer :: index_value

    table%valid = .true.
    allocate(table%interfaces(0))
    allocate(table%connections(0))
    allocate(table%processes(size(pids)))
    do index_value = 1, size(pids)
      table%processes(index_value)%valid = .true.
      table%processes(index_value)%pid = pids(index_value)
      table%processes(index_value)%start_time = start_times(index_value)
      table%processes(index_value)%rx_bytes = rx_bytes(index_value)
      table%processes(index_value)%tx_bytes = tx_bytes(index_value)
      write(table%processes(index_value)%process_name, '(A, I0)') "proc", index_value
    end do
  end function sample_process_bandwidth_table

  function linux_proc_net_tcp_text(connection_count) result(text)
    integer, intent(in) :: connection_count
    character(len=:), allocatable :: text
    character(len=192) :: line
    integer :: row

    text = "  sl  local_address rem_address   st tx_queue rx_queue tr tm->when retrnsmt   uid  timeout inode" // &
           new_line("a")
    do row = 1, connection_count
      write(line, '(" ", I0, ": 0100007F:", Z4.4, " 0200000A:01BB 01 00000000:00000000 00:00000000 00000000 1000 0 ", I0)') &
        row - 1, 10000 + mod(row, 40000), 500000 + row
      text = text // trim(line) // new_line("a")
    end do
  end function linux_proc_net_tcp_text

  logical function near(left, right) result(matches)
    real(real64), intent(in) :: left
    real(real64), intent(in) :: right

    matches = abs(left - right) <= 0.001_real64
  end function near

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

end program test_net_data
