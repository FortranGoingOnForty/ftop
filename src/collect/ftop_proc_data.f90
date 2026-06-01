module ftop_proc_data
  use, intrinsic :: iso_fortran_env, only : int64, real64
  implicit none
  private

  integer, parameter, public :: PROCESS_NAME_LEN = 64
  integer, parameter, public :: PROCESS_USER_LEN = 32
  integer, parameter, public :: PROCESS_COMMAND_LEN = 256
  integer, parameter, public :: PROCESS_STATE_LEN = 16
  integer, parameter, public :: PROCESS_CGROUP_LEN = 128

  integer, parameter, public :: PROCESS_SORT_PID = 1
  integer, parameter, public :: PROCESS_SORT_USER = 2
  integer, parameter, public :: PROCESS_SORT_CPU = 3
  integer, parameter, public :: PROCESS_SORT_MEMORY = 4
  integer, parameter, public :: PROCESS_SORT_RSS = 5
  integer, parameter, public :: PROCESS_SORT_COMMAND = 6

  type, public :: process_info
    logical :: valid = .false.
    integer :: pid = 0
    integer :: ppid = 0
    integer :: uid = 0
    logical :: user_valid = .false.
    character(len=PROCESS_USER_LEN) :: user = ""
    character(len=PROCESS_NAME_LEN) :: name = ""
    character(len=PROCESS_COMMAND_LEN) :: command = ""
    character(len=PROCESS_STATE_LEN) :: state = ""
    real(real64) :: cpu_percent = 0.0_real64
    real(real64) :: mem_percent = 0.0_real64
    integer(int64) :: mem_rss_bytes = 0_int64
    integer(int64) :: mem_virt_bytes = 0_int64
    integer :: threads = 0
    integer :: nice = 0
    integer :: priority = 0
    integer(int64) :: io_read_bytes = 0_int64
    integer(int64) :: io_write_bytes = 0_int64
    integer(int64) :: start_time = 0_int64
    integer(int64) :: cpu_time = 0_int64
    character(len=PROCESS_CGROUP_LEN) :: cgroup = ""
    integer :: jid = 0
  end type process_info

  type, public :: process_table
    logical :: valid = .false.
    type(process_info), allocatable :: items(:)
  end type process_table

  public :: process_count
  public :: process_display_command
  public :: process_state_label
  public :: process_user_label
  public :: sort_process_table

contains

  integer function process_count(table) result(count)
    type(process_table), intent(in) :: table

    count = 0
    if (allocated(table%items)) count = size(table%items)
  end function process_count

  function process_display_command(process) result(text)
    type(process_info), intent(in) :: process
    character(len=:), allocatable :: text

    if (len_trim(process%command) > 0) then
      text = trim(process%command)
    else if (len_trim(process%name) > 0) then
      text = trim(process%name)
    else
      text = "[unknown]"
    end if
  end function process_display_command

  function process_user_label(process) result(text)
    type(process_info), intent(in) :: process
    character(len=:), allocatable :: text

    if (process%user_valid .and. len_trim(process%user) > 0) then
      text = trim(process%user)
    else
      text = integer_text(process%uid)
    end if
  end function process_user_label

  function process_state_label(process) result(text)
    type(process_info), intent(in) :: process
    character(len=:), allocatable :: text

    if (len_trim(process%state) > 0) then
      text = trim(process%state)
    else
      text = "?"
    end if
  end function process_state_label

  subroutine sort_process_table(table, sort_key, descending)
    type(process_table), intent(inout) :: table
    integer, intent(in) :: sort_key
    logical, intent(in), optional :: descending
    type(process_info) :: current
    integer :: item_index
    integer :: scan_index
    logical :: use_descending

    if (.not. allocated(table%items)) return
    if (size(table%items) <= 1) return
    use_descending = .false.
    if (present(descending)) use_descending = descending

    do item_index = 2, size(table%items)
      current = table%items(item_index)
      scan_index = item_index - 1
      do while (scan_index >= 1)
        if (.not. process_out_of_order(table%items(scan_index), current, sort_key, use_descending)) exit
        table%items(scan_index + 1) = table%items(scan_index)
        scan_index = scan_index - 1
      end do
      table%items(scan_index + 1) = current
    end do
  end subroutine sort_process_table

  logical function process_out_of_order(left, right, sort_key, descending) result(out_of_order)
    type(process_info), intent(in) :: left
    type(process_info), intent(in) :: right
    integer, intent(in) :: sort_key
    logical, intent(in) :: descending
    integer :: comparison

    comparison = compare_processes(left, right, sort_key)
    if (descending) then
      out_of_order = comparison < 0
    else
      out_of_order = comparison > 0
    end if
  end function process_out_of_order

  integer function compare_processes(left, right, sort_key) result(comparison)
    type(process_info), intent(in) :: left
    type(process_info), intent(in) :: right
    integer, intent(in) :: sort_key

    select case (sort_key)
    case (PROCESS_SORT_USER)
      comparison = compare_text(process_user_label(left), process_user_label(right))
    case (PROCESS_SORT_CPU)
      comparison = compare_real(left%cpu_percent, right%cpu_percent)
    case (PROCESS_SORT_MEMORY)
      comparison = compare_real(left%mem_percent, right%mem_percent)
    case (PROCESS_SORT_RSS)
      comparison = compare_int64(left%mem_rss_bytes, right%mem_rss_bytes)
    case (PROCESS_SORT_COMMAND)
      comparison = compare_text(process_display_command(left), process_display_command(right))
    case default
      comparison = compare_integer(left%pid, right%pid)
    end select
  end function compare_processes

  integer function compare_integer(left, right) result(comparison)
    integer, intent(in) :: left
    integer, intent(in) :: right

    if (left < right) then
      comparison = -1
    else if (left > right) then
      comparison = 1
    else
      comparison = 0
    end if
  end function compare_integer

  integer function compare_int64(left, right) result(comparison)
    integer(int64), intent(in) :: left
    integer(int64), intent(in) :: right

    if (left < right) then
      comparison = -1
    else if (left > right) then
      comparison = 1
    else
      comparison = 0
    end if
  end function compare_int64

  integer function compare_real(left, right) result(comparison)
    real(real64), intent(in) :: left
    real(real64), intent(in) :: right

    if (left < right) then
      comparison = -1
    else if (left > right) then
      comparison = 1
    else
      comparison = 0
    end if
  end function compare_real

  integer function compare_text(left, right) result(comparison)
    character(len=*), intent(in) :: left
    character(len=*), intent(in) :: right
    character(len=:), allocatable :: left_trimmed
    character(len=:), allocatable :: right_trimmed

    left_trimmed = trim(left)
    right_trimmed = trim(right)
    if (left_trimmed < right_trimmed) then
      comparison = -1
    else if (left_trimmed > right_trimmed) then
      comparison = 1
    else
      comparison = 0
    end if
  end function compare_text

  function integer_text(value) result(text)
    integer, intent(in) :: value
    character(len=:), allocatable :: text
    character(len=32) :: scratch

    write(scratch, '(i0)') value
    text = trim(scratch)
  end function integer_text

end module ftop_proc_data
