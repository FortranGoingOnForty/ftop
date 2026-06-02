program test_freebsd_network
  use ftop_platform, only : &
    freebsd_net_connection_info, &
    freebsd_net_interface_info, &
    freebsd_network_connections, &
    freebsd_network_interfaces
  implicit none

  type(freebsd_net_connection_info), allocatable :: connections(:)
  type(freebsd_net_interface_info), allocatable :: interfaces(:)
  integer :: connection_count
  integer :: connection_index
  integer :: interface_count
  integer :: interface_index

  allocate(interfaces(128))
  if (.not. freebsd_network_interfaces(interfaces, interface_count)) error stop "FreeBSD network interfaces failed"
  if (interface_count <= 0) error stop "FreeBSD network interfaces must not be empty"
  if (interface_count > size(interfaces)) error stop "FreeBSD network interface count exceeded capacity"
  do interface_index = 1, interface_count
    if (interfaces(interface_index)%valid == 0) error stop "FreeBSD network interface must be valid"
    if (c_string_len(interfaces(interface_index)%name) <= 0) error stop "FreeBSD interface name must not be empty"
    if (c_string_len(interfaces(interface_index)%state) <= 0) error stop "FreeBSD interface state must not be empty"
    if (interfaces(interface_index)%mtu <= 0) error stop "FreeBSD interface MTU must be positive"
    if (interfaces(interface_index)%speed_mbps < 0) error stop "FreeBSD interface speed must not be negative"
  end do

  allocate(connections(2048))
  if (.not. freebsd_network_connections(connections, connection_count)) error stop "FreeBSD network connections failed"
  if (connection_count < 0) error stop "FreeBSD network connection count must not be negative"
  if (connection_count > size(connections)) error stop "FreeBSD network connection count exceeded capacity"

  do connection_index = 1, connection_count
    if (connections(connection_index)%valid == 0) error stop "FreeBSD network connection must be valid"
    if (.not. c_string_equals(connections(connection_index)%protocol, "tcp") .and. &
        .not. c_string_equals(connections(connection_index)%protocol, "udp")) then
      error stop "FreeBSD network connection protocol must be tcp or udp"
    end if
    if (c_string_len(connections(connection_index)%local_addr) <= 0) then
      error stop "FreeBSD network connection local address must not be empty"
    end if
    if (c_string_len(connections(connection_index)%remote_addr) <= 0) then
      error stop "FreeBSD network connection remote address must not be empty"
    end if
    if (connections(connection_index)%local_port < 0) error stop "FreeBSD network local port must not be negative"
    if (connections(connection_index)%remote_port < 0) error stop "FreeBSD network remote port must not be negative"
    if (c_string_len(connections(connection_index)%state) <= 0) error stop "FreeBSD network state must not be empty"
    if (connections(connection_index)%pid <= 0) error stop "FreeBSD network pid must be positive"
    if (c_string_len(connections(connection_index)%process_name) <= 0) then
      error stop "FreeBSD network process name must not be empty"
    end if
  end do

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

  logical function c_string_equals(buffer, expected) result(matches)
    use, intrinsic :: iso_c_binding, only : c_char, c_null_char
    character(kind=c_char), intent(in) :: buffer(:)
    character(len=*), intent(in) :: expected
    integer :: index_value

    matches = .false.
    if (len_trim(expected) > size(buffer)) return
    do index_value = 1, len_trim(expected)
      if (buffer(index_value) == c_null_char) return
      if (achar(iachar(buffer(index_value))) /= expected(index_value:index_value)) return
    end do
    if (len_trim(expected) < size(buffer)) then
      if (buffer(len_trim(expected) + 1) /= c_null_char) return
    end if
    matches = .true.
  end function c_string_equals
end program test_freebsd_network
