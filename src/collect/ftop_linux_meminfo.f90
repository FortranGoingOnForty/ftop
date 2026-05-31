module ftop_linux_meminfo
  use, intrinsic :: iso_fortran_env, only : int64
  use ftop_mem_data, only : memory_info
  implicit none
  private

  public :: linux_meminfo_parse

contains

  logical function linux_meminfo_parse(buffer, info) result(success)
    character(len=*), intent(in) :: buffer
    type(memory_info), intent(out) :: info
    integer(int64) :: mem_total
    integer(int64) :: mem_free
    integer(int64) :: mem_available
    integer(int64) :: buffers
    integer(int64) :: cached
    integer(int64) :: swap_total
    integer(int64) :: swap_free

    info = memory_info()
    mem_total = meminfo_value(buffer, "MemTotal")
    mem_free = meminfo_value(buffer, "MemFree")
    mem_available = meminfo_value(buffer, "MemAvailable")
    buffers = meminfo_value(buffer, "Buffers")
    cached = meminfo_value(buffer, "Cached")
    swap_total = meminfo_value(buffer, "SwapTotal")
    swap_free = meminfo_value(buffer, "SwapFree")

    success = .false.
    if (mem_total <= 0_int64) return
    if (mem_free < 0_int64 .or. mem_available < 0_int64 .or. buffers < 0_int64 .or. cached < 0_int64) return
    if (swap_total < 0_int64 .or. swap_free < 0_int64) return

    if (mem_available <= 0_int64) mem_available = mem_free + buffers + cached
    mem_available = max(0_int64, min(mem_total, mem_available))
    mem_free = max(0_int64, min(mem_total, mem_free))
    swap_free = max(0_int64, min(swap_total, swap_free))

    info%valid = .true.
    info%total_bytes = mem_total
    info%free_bytes = mem_free
    info%available_bytes = mem_available
    info%used_bytes = max(0_int64, mem_total - mem_available)
    info%cached_bytes = cached
    info%buffers_bytes = buffers
    info%swap_total_bytes = swap_total
    info%swap_used_bytes = max(0_int64, swap_total - swap_free)
    success = .true.
  end function linux_meminfo_parse

  integer(int64) function meminfo_value(buffer, key) result(value)
    character(len=*), intent(in) :: buffer
    character(len=*), intent(in) :: key
    character(len=:), allocatable :: line
    character(len=:), allocatable :: label
    integer :: line_start
    integer :: next_start
    integer :: value_start

    value = 0_int64
    line_start = 1
    do while (line_start <= len(buffer))
      call next_line(buffer, line_start, line, next_start)
      line_start = next_start
      if (.not. meminfo_label(line, label, value_start)) cycle
      if (label == key) then
        value = parse_kibibytes(line, value_start)
        return
      end if
    end do
  end function meminfo_value

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

  logical function meminfo_label(line, label, value_start) result(success)
    character(len=*), intent(in) :: line
    character(len=:), allocatable, intent(out) :: label
    integer, intent(out) :: value_start
    integer :: cursor
    integer :: label_start
    integer :: label_end

    success = .false.
    label = ""
    value_start = 1
    cursor = 1
    call skip_whitespace(line, cursor)
    if (cursor > len(line)) return

    label_start = cursor
    do while (cursor <= len(line) .and. line(cursor:cursor) /= ":")
      cursor = cursor + 1
    end do
    if (cursor > len(line)) return

    label_end = cursor - 1
    do while (label_end >= label_start .and. is_whitespace(line(label_end:label_end)))
      label_end = label_end - 1
    end do
    if (label_end < label_start) return

    label = line(label_start:label_end)
    value_start = cursor + 1
    success = .true.
  end function meminfo_label

  integer(int64) function parse_kibibytes(line, start_index) result(bytes)
    character(len=*), intent(in) :: line
    integer, intent(in) :: start_index
    integer :: cursor
    integer :: token_start
    integer :: token_end
    integer :: read_status
    integer(int64) :: value

    bytes = 0_int64
    cursor = start_index
    call skip_whitespace(line, cursor)
    if (cursor > len(line)) return


    token_start = cursor
    do while (cursor <= len(line) .and. .not. is_whitespace(line(cursor:cursor)))
      cursor = cursor + 1
    end do
    token_end = cursor - 1

    read(line(token_start:token_end), *, iostat=read_status) value
    if (read_status /= 0) return
    bytes = value * 1024_int64
  end function parse_kibibytes

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

end module ftop_linux_meminfo
