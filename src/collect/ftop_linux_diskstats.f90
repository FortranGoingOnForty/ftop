module ftop_linux_diskstats
  use, intrinsic :: iso_fortran_env, only : int64
  use ftop_disk_data, only : disk_io_info
  implicit none
  private

  public :: linux_diskstats_parse
  public :: linux_diskstats_parse_line

contains

  logical function linux_diskstats_parse(buffer, devices, sector_size_bytes) result(success)
    character(len=*), intent(in) :: buffer
    type(disk_io_info), allocatable, intent(out) :: devices(:)
    integer(int64), intent(in), optional :: sector_size_bytes
    type(disk_io_info) :: device
    character(len=:), allocatable :: line
    integer :: device_count
    integer :: device_index
    integer :: line_start
    integer :: next_start

    device_count = 0
    line_start = 1
    do while (line_start <= len(buffer))
      call next_line(buffer, line_start, line, next_start)
      line_start = next_start
      if (linux_diskstats_parse_line(line, device, sector_size_bytes)) device_count = device_count + 1
    end do

    allocate(devices(device_count))
    device_index = 0
    line_start = 1
    do while (line_start <= len(buffer))
      call next_line(buffer, line_start, line, next_start)
      line_start = next_start
      if (.not. linux_diskstats_parse_line(line, device, sector_size_bytes)) cycle
      device_index = device_index + 1
      devices(device_index) = device
    end do

    success = device_count > 0
  end function linux_diskstats_parse

  logical function linux_diskstats_parse_line(line, device, sector_size_bytes) result(success)
    character(len=*), intent(in) :: line
    type(disk_io_info), intent(out) :: device
    integer(int64), intent(in), optional :: sector_size_bytes
    character(len=:), allocatable :: name
    integer :: cursor
    integer :: value_count
    integer(int64) :: major
    integer(int64) :: minor
    integer(int64) :: sector_size
    integer(int64) :: values(11)

    device = disk_io_info()
    values = 0_int64
    cursor = 1
    sector_size = 512_int64
    if (present(sector_size_bytes)) sector_size = max(1_int64, sector_size_bytes)
    success = .false.

    if (.not. next_integer_token(line, cursor, major)) return
    if (.not. next_integer_token(line, cursor, minor)) return
    if (.not. next_text_token(line, cursor, name)) return
    if (major < 0_int64 .or. minor < 0_int64) return
    if (len_trim(name) <= 0) return

    value_count = parse_integer_fields(line, cursor, values)
    if (value_count < size(values)) return
    if (any(values < 0_int64)) return

    device%valid = .true.
    device%device = trim(name)
    device%read_ops = values(1)
    device%reads_merged = values(2)
    device%read_bytes = values(3) * sector_size
    device%read_time_ms = values(4)
    device%write_ops = values(5)
    device%writes_merged = values(6)
    device%write_bytes = values(7) * sector_size
    device%write_time_ms = values(8)
    device%ios_in_progress = values(9)
    device%io_time_ms = values(10)
    device%weighted_io_time_ms = values(11)
    device%sector_size_bytes = sector_size
    success = .true.
  end function linux_diskstats_parse_line

  subroutine next_line(buffer, start_index, line, next_start)
    character(len=*), intent(in) :: buffer
    integer, intent(in) :: start_index
    character(len=:), allocatable, intent(out) :: line
    integer, intent(out) :: next_start
    integer :: newline_offset
    integer :: end_index

    newline_offset = index(buffer(start_index:), new_line("a"))
    if (newline_offset == 0) then
      end_index = len(buffer)
      next_start = len(buffer) + 1
    else
      end_index = start_index + newline_offset - 2
      next_start = start_index + newline_offset
    end if

    if (end_index < start_index) then
      line = ""
    else
      line = buffer(start_index:end_index)
    end if
  end subroutine next_line

  logical function next_text_token(line, cursor, token) result(success)
    character(len=*), intent(in) :: line
    integer, intent(inout) :: cursor
    character(len=:), allocatable, intent(out) :: token
    integer :: token_start
    integer :: token_end

    success = .false.
    token = ""
    call skip_whitespace(line, cursor)
    if (cursor > len(line)) return

    token_start = cursor
    do while (cursor <= len(line) .and. .not. is_whitespace(line(cursor:cursor)))
      cursor = cursor + 1
    end do
    token_end = cursor - 1
    if (token_end < token_start) return

    token = line(token_start:token_end)
    success = .true.
  end function next_text_token

  logical function next_integer_token(line, cursor, value) result(success)
    character(len=*), intent(in) :: line
    integer, intent(inout) :: cursor
    integer(int64), intent(out) :: value
    character(len=:), allocatable :: token
    integer :: read_status

    value = 0_int64
    success = .false.
    if (.not. next_text_token(line, cursor, token)) return
    read(token, *, iostat=read_status) value
    success = read_status == 0
  end function next_integer_token

  integer function parse_integer_fields(line, start_cursor, values) result(value_count)
    character(len=*), intent(in) :: line
    integer, intent(in) :: start_cursor
    integer(int64), intent(out) :: values(:)
    integer :: cursor
    integer :: read_status
    character(len=:), allocatable :: token

    values = 0_int64
    value_count = 0
    cursor = start_cursor
    do while (cursor <= len(line) .and. value_count < size(values))
      if (.not. next_text_token(line, cursor, token)) exit
      value_count = value_count + 1
      read(token, *, iostat=read_status) values(value_count)
      if (read_status /= 0) then
        value_count = value_count - 1
        return
      end if
    end do
  end function parse_integer_fields

  subroutine skip_whitespace(line, cursor)
    character(len=*), intent(in) :: line
    integer, intent(inout) :: cursor

    do while (cursor <= len(line) .and. is_whitespace(line(cursor:cursor)))
      cursor = cursor + 1
    end do
  end subroutine skip_whitespace

  logical function is_whitespace(character_value) result(is_space)
    character(len=1), intent(in) :: character_value

    is_space = character_value == " " .or. character_value == achar(9) .or. character_value == achar(13)
  end function is_whitespace

end module ftop_linux_diskstats
