program test_net_data
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use ftop_net_data, only : append_interface_histories, assign_interface_rates, decode_linux_ipv4_endpoint, &
                            format_byte_rate, network_table, parse_linux_proc_net_dev
  implicit none

  call test_linux_proc_net_dev_parser()
  call test_interface_rates()
  call test_interface_histories()
  call test_byte_rate_formatting()
  call test_linux_ipv4_endpoint_decoder()

contains

  subroutine test_linux_proc_net_dev_parser()
    type(network_table) :: table
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
    call require(table%interfaces(1)%tx_bytes == 654321_int64, "network parser should parse tx bytes")
    call require(table%interfaces(1)%tx_packets == 987_int64, "network parser should parse tx packets")
    call require(trim(table%interfaces(2)%name) == "wlan0", "network parser should parse second interface")

    with_loopback = parse_linux_proc_net_dev(text, include_loopback=.true.)
    call require(size(with_loopback%interfaces) == 3, "network parser should optionally include loopback")
    call require(trim(with_loopback%interfaces(1)%name) == "lo", "network parser should keep loopback when requested")
  end subroutine test_linux_proc_net_dev_parser

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
