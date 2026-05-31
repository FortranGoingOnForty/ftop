module ftop_linux_proc_stat
  use, intrinsic :: iso_fortran_env, only : int64
  use ftop_cpu_data, only : cpu_state_ticks
  implicit none
  private

  public :: linux_proc_stat_parse
  public :: linux_proc_stat_parse_line

contains

  logical function linux_proc_stat_parse(buffer, total, cores) result(success)
    character(len=*), intent(in) :: buffer
    type(cpu_state_ticks), intent(out) :: total
    type(cpu_state_ticks), allocatable, intent(out) :: cores(:)
    character(len=:), allocatable :: line
    character(len=:), allocatable :: label
    integer :: core_count
    integer :: core_index
    integer :: line_start
    integer :: line_end
    integer :: next_start

    total = cpu_state_ticks()
    core_count = 0
    line_start = 1
    do while (line_start <= len(buffer))
      call next_line(buffer, line_start, line, next_start)
      line_start = next_start
      if (.not. line_label(line, label)) cycle
      if (is_core_label(label)) core_count = core_count + 1
    end do

    allocate(cores(core_count))
    core_index = 0
    line_start = 1
    success = .true.
    do while (line_start <= len(buffer))
      call next_line(buffer, line_start, line, line_end)
      next_start = line_end
      line_start = next_start
      if (.not. line_label(line, label)) cycle

      if (label == "cpu") then
        if (.not. linux_proc_stat_parse_line(line, total)) success = .false.
      else if (is_core_label(label)) then
        core_index = core_index + 1
        if (.not. linux_proc_stat_parse_line(line, cores(core_index))) success = .false.
      end if
    end do

    if (.not. total%valid) success = .false.
  end function linux_proc_stat_parse

  logical function linux_proc_stat_parse_line(line, sample) result(success)
    character(len=*), intent(in) :: line
    type(cpu_state_ticks), intent(out) :: sample
    character(len=:), allocatable :: label
    integer :: cursor
    integer :: value_count
    integer(int64) :: values(8)

    sample = cpu_state_ticks()
    values = 0_int64
    success = .false.

    if (.not. line_label(line, label, cursor)) return
    if (label /= "cpu" .and. .not. is_core_label(label)) return

    value_count = parse_integer_fields(line, cursor, values)
    if (value_count < 4) return
    if (any(values < 0_int64)) return

    sample%valid = .true.
    sample%user = values(1)
    sample%nice = values(2)
    sample%system = values(3)
    sample%idle = values(4)
    sample%iowait = values(5)
    sample%irq = values(6)
    sample%softirq = values(7)
    sample%steal = values(8)
    success = .true.
  end function linux_proc_stat_parse_line

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

  logical function line_label(line, label, next_cursor) result(success)
    character(len=*), intent(in) :: line
    character(len=:), allocatable, intent(out) :: label
    integer, intent(out), optional :: next_cursor
    integer :: cursor
    integer :: label_start
    integer :: label_end

    success = .false.
    label = ""
    cursor = 1
    call skip_whitespace(line, cursor)
    if (cursor > len(line)) return

    label_start = cursor
    do while (cursor <= len(line) .and. .not. is_whitespace(line(cursor:cursor)))
      cursor = cursor + 1
    end do
    label_end = cursor - 1
    if (label_end < label_start) return

    label = line(label_start:label_end)
    if (present(next_cursor)) next_cursor = cursor
    success = .true.
  end function line_label

  integer function parse_integer_fields(line, start_cursor, values) result(value_count)
    character(len=*), intent(in) :: line
    integer, intent(in) :: start_cursor
    integer(int64), intent(out) :: values(:)
    integer :: cursor
    integer :: token_start
    integer :: token_end
    integer :: read_status

    values = 0_int64
    value_count = 0
    cursor = start_cursor
    do while (cursor <= len(line) .and. value_count < size(values))
      call skip_whitespace(line, cursor)
      if (cursor > len(line)) exit

      token_start = cursor
      do while (cursor <= len(line) .and. .not. is_whitespace(line(cursor:cursor)))
        cursor = cursor + 1
      end do
      token_end = cursor - 1

      value_count = value_count + 1
      read(line(token_start:token_end), *, iostat=read_status) values(value_count)
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

  logical function is_core_label(label) result(is_core)
    character(len=*), intent(in) :: label
    integer :: i

    is_core = .false.
    if (len(label) <= 3) return
    if (label(1:3) /= "cpu") return

    do i = 4, len(label)
      if (label(i:i) < "0" .or. label(i:i) > "9") return
    end do
    is_core = .true.
  end function is_core_label

  logical function is_whitespace(character_value) result(is_space)
    character(len=1), intent(in) :: character_value

    is_space = character_value == " " .or. character_value == achar(9) .or. character_value == achar(13)
  end function is_whitespace

end module ftop_linux_proc_stat
