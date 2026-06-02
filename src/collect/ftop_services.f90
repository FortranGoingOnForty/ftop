module ftop_services
  implicit none
  private

  integer, parameter :: SERVICE_NAME_LEN = 32
  integer, parameter :: SERVICE_PROTOCOL_LEN = 8
  character(len=*), parameter :: SERVICES_PATH = "/etc/services"

  type, public :: service_entry
    logical :: valid = .false.
    character(len=SERVICE_NAME_LEN) :: name = ""
    integer :: port = 0
    character(len=SERVICE_PROTOCOL_LEN) :: protocol = ""
  end type service_entry

  type(service_entry), allocatable, save :: service_cache(:)
  logical, save :: service_cache_loaded = .false.

  public :: cached_service_name
  public :: load_service_cache_from_text
  public :: parse_services
  public :: reset_service_cache
  public :: service_name_for

contains

  function parse_services(text) result(services)
    character(len=*), intent(in) :: text
    type(service_entry), allocatable :: services(:)
    integer :: cursor
    integer :: line_end
    integer :: line_start
    integer :: service_count
    type(service_entry) :: service

    service_count = 0
    cursor = 1
    do while (cursor <= len(text))
      line_start = cursor
      line_end = next_line_end(text, line_start)
      if (parse_service_line(text(line_start:line_end), service)) service_count = service_count + 1
      cursor = line_end + 2
    end do

    allocate(services(service_count))
    service_count = 0
    cursor = 1
    do while (cursor <= len(text))
      line_start = cursor
      line_end = next_line_end(text, line_start)
      if (parse_service_line(text(line_start:line_end), service)) then
        service_count = service_count + 1
        services(service_count) = service
      end if
      cursor = line_end + 2
    end do
  end function parse_services

  function service_name_for(services, port, protocol) result(name)
    type(service_entry), intent(in) :: services(:)
    integer, intent(in) :: port
    character(len=*), intent(in) :: protocol
    character(len=:), allocatable :: name
    character(len=:), allocatable :: normalized_protocol
    integer :: service_index

    name = ""
    if (port <= 0) return
    normalized_protocol = trim(protocol)
    call lowercase_ascii(normalized_protocol)
    if (len_trim(normalized_protocol) <= 0) return

    do service_index = 1, size(services)
      if (.not. services(service_index)%valid) cycle
      if (services(service_index)%port /= port) cycle
      if (trim(services(service_index)%protocol) /= normalized_protocol) cycle
      name = trim(services(service_index)%name)
      return
    end do
  end function service_name_for

  function cached_service_name(port, protocol) result(name)
    integer, intent(in) :: port
    character(len=*), intent(in) :: protocol
    character(len=:), allocatable :: name

    name = ""
    if (port <= 0 .or. len_trim(protocol) <= 0) return
    if (.not. service_cache_loaded) call load_system_service_cache()
    if (.not. allocated(service_cache)) return
    name = service_name_for(service_cache, port, protocol)
  end function cached_service_name

  subroutine load_service_cache_from_text(text)
    character(len=*), intent(in) :: text

    if (allocated(service_cache)) deallocate(service_cache)
    service_cache = parse_services(text)
    service_cache_loaded = .true.
  end subroutine load_service_cache_from_text

  subroutine reset_service_cache()
    if (allocated(service_cache)) deallocate(service_cache)
    service_cache_loaded = .false.
  end subroutine reset_service_cache

  subroutine load_system_service_cache()
    character(len=:), allocatable :: text

    text = read_text_file(SERVICES_PATH)
    call load_service_cache_from_text(text)
  end subroutine load_system_service_cache

  function read_text_file(path) result(text)
    character(len=*), intent(in) :: path
    character(len=:), allocatable :: text
    character(len=1024) :: line
    integer :: io_status
    integer :: unit

    text = ""
    open(newunit=unit, file=path, status="old", action="read", iostat=io_status)
    if (io_status /= 0) return

    do
      read(unit, '(A)', iostat=io_status) line
      if (io_status /= 0) exit
      text = text // trim(line) // new_line("a")
    end do
    close(unit, iostat=io_status)
  end function read_text_file

  logical function parse_service_line(raw_line, service) result(parsed)
    character(len=*), intent(in) :: raw_line
    type(service_entry), intent(out) :: service
    character(len=:), allocatable :: endpoint_token
    character(len=:), allocatable :: line
    character(len=:), allocatable :: name_token
    character(len=:), allocatable :: port_token
    character(len=:), allocatable :: protocol_token
    integer :: comment_start
    integer :: cursor
    integer :: next_start
    integer :: parsed_port
    integer :: slash

    parsed = .false.
    service = service_entry()
    line = raw_line
    comment_start = index(line, "#")
    if (comment_start > 0) line = line(:comment_start - 1)
    if (len_trim(line) <= 0) return

    cursor = 1
    if (.not. next_token(line, cursor, name_token, next_start)) return
    cursor = next_start
    if (.not. next_token(line, cursor, endpoint_token, next_start)) return

    slash = index(endpoint_token, "/")
    if (slash <= 1 .or. slash >= len_trim(endpoint_token)) return
    port_token = endpoint_token(:slash - 1)
    protocol_token = endpoint_token(slash + 1:len_trim(endpoint_token))
    call lowercase_ascii(protocol_token)
    if (.not. parse_decimal_port(port_token, parsed_port)) return
    if (len_trim(name_token) <= 0 .or. len_trim(protocol_token) <= 0) return

    service%valid = .true.
    service%name = bounded_text(name_token, len(service%name))
    service%port = parsed_port
    service%protocol = bounded_text(protocol_token, len(service%protocol))
    parsed = .true.
  end function parse_service_line

  logical function next_token(text, start, token, next_start) result(found)
    character(len=*), intent(in) :: text
    integer, intent(in) :: start
    character(len=:), allocatable, intent(out) :: token
    integer, intent(out) :: next_start
    integer :: cursor
    integer :: token_end

    found = .false.
    token = ""
    cursor = max(1, start)
    do while (cursor <= len(text) .and. is_space(text(cursor:cursor)))
      cursor = cursor + 1
    end do
    if (cursor > len(text)) then
      next_start = cursor
      return
    end if

    token_end = cursor
    do while (token_end <= len(text) .and. .not. is_space(text(token_end:token_end)))
      token_end = token_end + 1
    end do
    token = text(cursor:token_end - 1)
    next_start = token_end
    found = .true.
  end function next_token

  logical function is_space(char) result(matches)
    character(len=1), intent(in) :: char

    matches = char == " " .or. char == achar(9)
  end function is_space

  logical function parse_decimal_port(text, value) result(success)
    character(len=*), intent(in) :: text
    integer, intent(out) :: value
    integer :: char_code
    integer :: index_value

    success = .false.
    value = 0
    if (len_trim(text) <= 0) return
    do index_value = 1, len_trim(text)
      char_code = iachar(text(index_value:index_value))
      if (char_code < iachar("0") .or. char_code > iachar("9")) return
      value = value * 10 + char_code - iachar("0")
      if (value > 65535) return
    end do
    success = .true.
  end function parse_decimal_port

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

  subroutine lowercase_ascii(text)
    character(len=*), intent(inout) :: text
    integer :: char_code
    integer :: index_value

    do index_value = 1, len(text)
      char_code = iachar(text(index_value:index_value))
      if (char_code >= iachar("A") .and. char_code <= iachar("Z")) then
        text(index_value:index_value) = achar(char_code - iachar("A") + iachar("a"))
      end if
    end do
  end subroutine lowercase_ascii

end module ftop_services
