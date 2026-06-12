module ftop_linux_gpu_fdinfo
  use, intrinsic :: iso_fortran_env, only : int64
  use ftop_gpu_data, only : GPU_ENGINE_LEN, GPU_PROCESS_NAME_LEN, gpu_process_info
  implicit none
  private

  public :: linux_gpu_fdinfo_parse

contains

  logical function linux_gpu_fdinfo_parse(text, pid, start_time, process_name, process) result(parsed)
    character(len=*), intent(in) :: text
    integer, intent(in) :: pid
    integer(int64), intent(in) :: start_time
    character(len=*), intent(in) :: process_name
    type(gpu_process_info), intent(out) :: process
    character(len=:), allocatable :: engine
    character(len=:), allocatable :: key
    character(len=:), allocatable :: line
    character(len=:), allocatable :: value
    integer :: line_start
    integer :: next_start
    integer(int64) :: engine_time_ns
    integer(int64) :: engine_total_ns
    integer(int64) :: max_engine_time_ns
    integer(int64) :: memory_bytes
    integer(int64) :: memory_total_bytes

    parsed = .false.
    process = gpu_process_info()
    if (pid <= 0) return

    engine = ""
    engine_total_ns = 0_int64
    max_engine_time_ns = 0_int64
    memory_total_bytes = 0_int64
    line_start = 1
    do while (line_start <= len(text))
      call next_line(text, line_start, line, next_start)
      line_start = next_start
      if (.not. line_key_value(line, key, value)) cycle
      if (parse_engine_counter(key, value, engine, engine_time_ns)) then
        engine_total_ns = saturating_add_int64(engine_total_ns, engine_time_ns)
        if (engine_time_ns > max_engine_time_ns) then
          max_engine_time_ns = engine_time_ns
          process%engine = bounded_text(engine, GPU_ENGINE_LEN)
        end if
      else if (parse_memory_counter(key, value, memory_bytes)) then
        memory_total_bytes = max(memory_total_bytes, memory_bytes)
      end if
    end do

    if (engine_total_ns <= 0_int64) return
    process%valid = .true.
    process%pid = pid
    process%start_time = max(0_int64, start_time)
    process%process_name = bounded_text(process_name, GPU_PROCESS_NAME_LEN)
    if (len_trim(process%process_name) <= 0) process%process_name = "[unknown]"
    if (len_trim(process%engine) <= 0) process%engine = "gpu"
    process%engine_time_ns = engine_total_ns
    if (memory_total_bytes > 0_int64) then
      process%memory_valid = .true.
      process%memory_bytes = memory_total_bytes
    end if
    parsed = .true.
  end function linux_gpu_fdinfo_parse

  subroutine next_line(buffer, start_index, line, next_start)
    character(len=*), intent(in) :: buffer
    integer, intent(in) :: start_index
    character(len=:), allocatable, intent(out) :: line
    integer, intent(out) :: next_start
    integer :: end_index
    integer :: newline_offset

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

  logical function line_key_value(line, key, value) result(parsed)
    character(len=*), intent(in) :: line
    character(len=:), allocatable, intent(out) :: key
    character(len=:), allocatable, intent(out) :: value
    integer :: colon

    parsed = .false.
    key = ""
    value = ""
    colon = index(line, ":")
    if (colon <= 1) return
    key = lower_ascii(trim(adjustl(line(:colon - 1))))
    value = adjustl(line(colon + 1:))
    parsed = len(key) > 0
  end function line_key_value

  logical function parse_engine_counter(key, value, engine, time_ns) result(parsed)
    character(len=*), intent(in) :: key
    character(len=*), intent(in) :: value
    character(len=:), allocatable, intent(out) :: engine
    integer(int64), intent(out) :: time_ns
    character(len=:), allocatable :: unit
    integer(int64) :: amount

    parsed = .false.
    engine = ""
    time_ns = 0_int64
    if (.not. starts_with(key, "drm-engine-")) return
    if (.not. parse_count_with_unit(value, amount, unit)) return
    time_ns = convert_time_to_ns(amount, unit)
    if (time_ns <= 0_int64) return
    engine = key(len("drm-engine-") + 1:)
    parsed = len_trim(engine) > 0
  end function parse_engine_counter

  logical function parse_memory_counter(key, value, bytes) result(parsed)
    character(len=*), intent(in) :: key
    character(len=*), intent(in) :: value
    integer(int64), intent(out) :: bytes
    character(len=:), allocatable :: unit
    integer(int64) :: amount

    parsed = .false.
    bytes = 0_int64
    if (.not. starts_with(key, "drm-memory-")) return
    if (.not. parse_count_with_unit(value, amount, unit)) return
    bytes = convert_memory_to_bytes(amount, unit)
    parsed = bytes > 0_int64
  end function parse_memory_counter

  logical function parse_count_with_unit(text, amount, unit) result(parsed)
    character(len=*), intent(in) :: text
    integer(int64), intent(out) :: amount
    character(len=:), allocatable, intent(out) :: unit
    character(len=:), allocatable :: amount_token
    integer :: read_status

    amount = 0_int64
    unit = ""
    call token_at(text, 1, amount_token)
    if (len(amount_token) <= 0) then
      parsed = .false.
      return
    end if

    read(amount_token, *, iostat=read_status) amount
    parsed = read_status == 0 .and. amount >= 0_int64
    if (.not. parsed) then
      amount = 0_int64
      return
    end if
    call token_at(text, 2, unit)
    unit = lower_ascii(unit)
  end function parse_count_with_unit

  subroutine token_at(text, target_index, token)
    character(len=*), intent(in) :: text
    integer, intent(in) :: target_index
    character(len=:), allocatable, intent(out) :: token
    integer :: cursor
    integer :: current_index
    integer :: token_end
    integer :: token_start

    token = ""
    if (target_index <= 0) return
    cursor = 1
    current_index = 0
    do while (cursor <= len(text))
      call skip_whitespace(text, cursor)
      if (cursor > len(text)) exit
      token_start = cursor
      do while (cursor <= len(text) .and. .not. is_whitespace(text(cursor:cursor)))
        cursor = cursor + 1
      end do
      token_end = cursor - 1
      current_index = current_index + 1
      if (current_index == target_index) then
        token = text(token_start:token_end)
        return
      end if
    end do
  end subroutine token_at

  subroutine skip_whitespace(text, cursor)
    character(len=*), intent(in) :: text
    integer, intent(inout) :: cursor

    do while (cursor <= len(text) .and. is_whitespace(text(cursor:cursor)))
      cursor = cursor + 1
    end do
  end subroutine skip_whitespace

  logical function is_whitespace(character_value) result(is_space)
    character(len=1), intent(in) :: character_value

    is_space = character_value == " " .or. character_value == achar(9) .or. character_value == achar(13)
  end function is_whitespace

  integer(int64) function convert_time_to_ns(amount, unit) result(value)
    integer(int64), intent(in) :: amount
    character(len=*), intent(in) :: unit

    select case (trim(unit))
    case ("", "ns", "nsec", "nsecs")
      value = amount
    case ("us", "usec", "usecs")
      value = saturating_multiply_int64(amount, 1000_int64)
    case ("ms", "msec", "msecs")
      value = saturating_multiply_int64(amount, 1000000_int64)
    case ("s", "sec", "secs")
      value = saturating_multiply_int64(amount, 1000000000_int64)
    case default
      value = 0_int64
    end select
  end function convert_time_to_ns

  integer(int64) function convert_memory_to_bytes(amount, unit) result(value)
    integer(int64), intent(in) :: amount
    character(len=*), intent(in) :: unit

    select case (trim(unit))
    case ("", "b", "byte", "bytes")
      value = amount
    case ("k", "kb", "kib")
      value = saturating_multiply_int64(amount, 1024_int64)
    case ("m", "mb", "mib")
      value = saturating_multiply_int64(amount, 1024_int64 * 1024_int64)
    case ("g", "gb", "gib")
      value = saturating_multiply_int64(amount, 1024_int64 * 1024_int64 * 1024_int64)
    case default
      value = 0_int64
    end select
  end function convert_memory_to_bytes

  integer(int64) function saturating_add_int64(left, right) result(value)
    integer(int64), intent(in) :: left
    integer(int64), intent(in) :: right

    if (left < 0_int64 .or. right < 0_int64) then
      value = max(0_int64, left) + max(0_int64, right)
    else if (right > huge(value) - left) then
      value = huge(value)
    else
      value = left + right
    end if
  end function saturating_add_int64

  integer(int64) function saturating_multiply_int64(left, right) result(value)
    integer(int64), intent(in) :: left
    integer(int64), intent(in) :: right

    if (left <= 0_int64 .or. right <= 0_int64) then
      value = 0_int64
    else if (left > huge(value) / right) then
      value = huge(value)
    else
      value = left * right
    end if
  end function saturating_multiply_int64

  logical function starts_with(text, prefix) result(matches)
    character(len=*), intent(in) :: text
    character(len=*), intent(in) :: prefix

    matches = .false.
    if (len(text) < len(prefix)) return
    matches = text(:len(prefix)) == prefix
  end function starts_with

  function lower_ascii(text) result(lower)
    character(len=*), intent(in) :: text
    character(len=:), allocatable :: lower
    integer :: char_code
    integer :: index_value

    lower = text
    do index_value = 1, len(lower)
      char_code = iachar(lower(index_value:index_value))
      if (char_code >= iachar("A") .and. char_code <= iachar("Z")) then
        lower(index_value:index_value) = achar(char_code - iachar("A") + iachar("a"))
      end if
    end do
  end function lower_ascii

  function bounded_text(source, capacity) result(text)
    character(len=*), intent(in) :: source
    integer, intent(in) :: capacity
    character(len=:), allocatable :: text
    integer :: copied_len

    copied_len = max(0, min(len_trim(source), capacity))
    if (copied_len <= 0) then
      text = ""
    else
      text = source(:copied_len)
    end if
  end function bounded_text
end module ftop_linux_gpu_fdinfo
