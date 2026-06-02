module ftop_proc_data
  use, intrinsic :: iso_fortran_env, only : int64, real64
  implicit none
  private

  integer, parameter, public :: PROCESS_NAME_LEN = 64
  integer, parameter, public :: PROCESS_USER_LEN = 32
  integer, parameter, public :: PROCESS_COMMAND_LEN = 256
  integer, parameter, public :: PROCESS_STATE_LEN = 16
  integer, parameter, public :: PROCESS_CGROUP_LEN = 128
  integer, parameter, public :: PROCESS_TREE_PREFIX_LEN = 128
  integer, parameter, public :: PROCESS_HISTORY_CAPACITY = 300
  integer, parameter :: PROCESS_TREE_MAX_DEPTH = 24

  integer, parameter, public :: PROCESS_SORT_PID = 1
  integer, parameter, public :: PROCESS_SORT_USER = 2
  integer, parameter, public :: PROCESS_SORT_PRIORITY = 3
  integer, parameter, public :: PROCESS_SORT_NICE = 4
  integer, parameter, public :: PROCESS_SORT_VIRT = 5
  integer, parameter, public :: PROCESS_SORT_RSS = 6
  integer, parameter, public :: PROCESS_SORT_SHARED = 7
  integer, parameter, public :: PROCESS_SORT_STATE = 8
  integer, parameter, public :: PROCESS_SORT_CPU = 9
  integer, parameter, public :: PROCESS_SORT_MEMORY = 10
  integer, parameter, public :: PROCESS_SORT_TIME = 11
  integer, parameter, public :: PROCESS_SORT_COMMAND = 12

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
    integer(int64) :: mem_shared_bytes = 0_int64
    integer :: threads = 0
    integer :: nice = 0
    integer :: priority = 0
    integer(int64) :: io_read_bytes = 0_int64
    integer(int64) :: io_write_bytes = 0_int64
    integer(int64) :: start_time = 0_int64
    integer(int64) :: cpu_time = 0_int64 ! Cumulative CPU time in milliseconds.
    integer :: history_count = 0
    real(real64) :: cpu_history(PROCESS_HISTORY_CAPACITY) = 0.0_real64
    real(real64) :: mem_history(PROCESS_HISTORY_CAPACITY) = 0.0_real64
    character(len=PROCESS_CGROUP_LEN) :: cgroup = ""
    integer :: jid = 0
    integer :: tree_depth = 0
    character(len=PROCESS_TREE_PREFIX_LEN) :: tree_prefix = ""
  end type process_info

  type, public :: process_table
    logical :: valid = .false.
    type(process_info), allocatable :: items(:)
  end type process_table

  public :: process_count
  public :: process_display_command
  public :: process_state_label
  public :: process_user_label
  public :: append_process_histories
  public :: assign_process_cpu_percent
  public :: build_process_tree
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

  subroutine assign_process_cpu_percent(current, previous, elapsed_ms)
    type(process_table), intent(inout) :: current
    type(process_table), intent(in) :: previous
    integer(int64), intent(in) :: elapsed_ms
    integer :: current_index
    integer :: previous_index
    integer(int64) :: cpu_delta_ms

    if (.not. allocated(current%items)) return
    current%items%cpu_percent = 0.0_real64
    if (.not. current%valid .or. .not. previous%valid) return
    if (.not. allocated(previous%items)) return
    if (elapsed_ms <= 0_int64) return

    do current_index = 1, size(current%items)
      previous_index = matching_previous_process(previous, current%items(current_index))
      if (previous_index <= 0) cycle

      cpu_delta_ms = current%items(current_index)%cpu_time - previous%items(previous_index)%cpu_time
      if (cpu_delta_ms <= 0_int64) cycle
      current%items(current_index)%cpu_percent = 100.0_real64 * real(cpu_delta_ms, real64) / real(elapsed_ms, real64)
    end do
  end subroutine assign_process_cpu_percent

  subroutine append_process_histories(current, previous)
    type(process_table), intent(inout) :: current
    type(process_table), intent(in) :: previous
    integer :: current_index
    integer :: previous_index

    if (.not. allocated(current%items)) return

    do current_index = 1, size(current%items)
      current%items(current_index)%history_count = 0
      current%items(current_index)%cpu_history = 0.0_real64
      current%items(current_index)%mem_history = 0.0_real64
      if (.not. current%items(current_index)%valid) cycle

      previous_index = 0
      if (previous%valid .and. allocated(previous%items)) then
        previous_index = matching_previous_process(previous, current%items(current_index))
      end if
      if (previous_index > 0) call copy_process_history(current%items(current_index), previous%items(previous_index))
      call append_process_history_sample(current%items(current_index), current%items(current_index)%cpu_percent, &
                                         current%items(current_index)%mem_percent)
    end do
  end subroutine append_process_histories

  subroutine copy_process_history(current, previous)
    type(process_info), intent(inout) :: current
    type(process_info), intent(in) :: previous
    integer :: history_count

    history_count = bounded_history_count(previous%history_count)
    if (history_count <= 0) return
    current%history_count = history_count
    current%cpu_history(:history_count) = previous%cpu_history(:history_count)
    current%mem_history(:history_count) = previous%mem_history(:history_count)
  end subroutine copy_process_history

  subroutine append_process_history_sample(process, cpu_percent, mem_percent)
    type(process_info), intent(inout) :: process
    real(real64), intent(in) :: cpu_percent
    real(real64), intent(in) :: mem_percent
    integer :: history_index

    if (.not. process%valid) return
    process%history_count = bounded_history_count(process%history_count)
    if (process%history_count < PROCESS_HISTORY_CAPACITY) then
      process%history_count = process%history_count + 1
    else
      do history_index = 1, PROCESS_HISTORY_CAPACITY - 1
        process%cpu_history(history_index) = process%cpu_history(history_index + 1)
        process%mem_history(history_index) = process%mem_history(history_index + 1)
      end do
    end if

    process%cpu_history(process%history_count) = clamp_percent(cpu_percent)
    process%mem_history(process%history_count) = clamp_percent(mem_percent)
  end subroutine append_process_history_sample

  integer function bounded_history_count(history_count) result(bounded)
    integer, intent(in) :: history_count

    bounded = max(0, min(PROCESS_HISTORY_CAPACITY, history_count))
  end function bounded_history_count

  subroutine build_process_tree(table)
    type(process_table), intent(inout) :: table
    type(process_info), allocatable :: ordered(:)
    logical, allocatable :: emitted(:)
    integer :: item_count
    integer :: item_index
    integer :: output_count

    if (.not. allocated(table%items)) return
    item_count = size(table%items)
    if (item_count <= 0) return

    table%items%tree_depth = 0
    table%items%tree_prefix = ""
    allocate(ordered(item_count))
    ordered = process_info()
    allocate(emitted(item_count))
    emitted = .false.
    output_count = 0

    do item_index = 1, item_count
      if (emitted(item_index)) cycle
      if (.not. table%items(item_index)%valid) cycle
      if (.not. process_is_tree_root(table, item_index)) cycle
      call append_tree_process(table, item_index, 0, "", .true., emitted, ordered, output_count)
    end do

    do item_index = 1, item_count
      if (emitted(item_index)) cycle
      if (.not. table%items(item_index)%valid) cycle
      call append_tree_process(table, item_index, 0, "", .true., emitted, ordered, output_count)
    end do

    do item_index = 1, item_count
      if (emitted(item_index)) cycle
      output_count = output_count + 1
      ordered(output_count) = table%items(item_index)
      emitted(item_index) = .true.
    end do

    table%items = ordered
  end subroutine build_process_tree

  recursive subroutine append_tree_process(table, item_index, depth, ancestor_prefix, is_last, emitted, ordered, &
                                           output_count)
    type(process_table), intent(in) :: table
    integer, intent(in) :: item_index
    integer, intent(in) :: depth
    character(len=*), intent(in) :: ancestor_prefix
    logical, intent(in) :: is_last
    logical, intent(inout) :: emitted(:)
    type(process_info), intent(inout) :: ordered(:)
    integer, intent(inout) :: output_count
    character(len=:), allocatable :: child_ancestor_prefix
    integer :: child_index
    integer :: item_count

    if (item_index < 1 .or. item_index > size(table%items)) return
    if (emitted(item_index)) return
    if (.not. table%items(item_index)%valid) return

    output_count = output_count + 1
    ordered(output_count) = table%items(item_index)
    ordered(output_count)%tree_depth = min(depth, PROCESS_TREE_MAX_DEPTH)
    ordered(output_count)%tree_prefix = tree_branch_prefix(depth, ancestor_prefix, is_last)
    emitted(item_index) = .true.

    if (depth >= PROCESS_TREE_MAX_DEPTH) then
      child_ancestor_prefix = ancestor_prefix
    else
      child_ancestor_prefix = next_ancestor_prefix(depth, ancestor_prefix, is_last)
    end if
    item_count = size(table%items)
    do child_index = 1, item_count
      if (.not. process_is_child_of(table, child_index, item_index, emitted)) cycle
      call append_tree_process(table, child_index, depth + 1, child_ancestor_prefix, &
                               process_is_last_child(table, child_index, item_index, emitted), emitted, ordered, &
                               output_count)
    end do
  end subroutine append_tree_process

  logical function process_is_tree_root(table, item_index) result(is_root)
    type(process_table), intent(in) :: table
    integer, intent(in) :: item_index

    is_root = .false.
    if (item_index < 1 .or. item_index > size(table%items)) return
    if (.not. table%items(item_index)%valid) return
    if (table%items(item_index)%ppid <= 0) then
      is_root = .true.
    else if (table%items(item_index)%ppid == table%items(item_index)%pid) then
      is_root = .true.
    else
      is_root = .not. process_parent_exists(table, item_index)
    end if
  end function process_is_tree_root

  logical function process_parent_exists(table, item_index) result(found)
    type(process_table), intent(in) :: table
    integer, intent(in) :: item_index
    integer :: candidate_index
    integer :: parent_pid

    found = .false.
    parent_pid = table%items(item_index)%ppid
    do candidate_index = 1, size(table%items)
      if (candidate_index == item_index) cycle
      if (.not. table%items(candidate_index)%valid) cycle
      if (table%items(candidate_index)%pid == parent_pid) then
        found = .true.
        return
      end if
    end do
  end function process_parent_exists

  logical function process_is_child_of(table, child_index, parent_index, emitted) result(is_child)
    type(process_table), intent(in) :: table
    integer, intent(in) :: child_index
    integer, intent(in) :: parent_index
    logical, intent(in) :: emitted(:)

    is_child = .false.
    if (child_index < 1 .or. child_index > size(table%items)) return
    if (parent_index < 1 .or. parent_index > size(table%items)) return
    if (emitted(child_index)) return
    if (.not. table%items(child_index)%valid) return
    if (.not. table%items(parent_index)%valid) return
    if (table%items(child_index)%pid == table%items(parent_index)%pid) return
    is_child = table%items(child_index)%ppid == table%items(parent_index)%pid
  end function process_is_child_of

  logical function process_is_last_child(table, child_index, parent_index, emitted) result(is_last)
    type(process_table), intent(in) :: table
    integer, intent(in) :: child_index
    integer, intent(in) :: parent_index
    logical, intent(in) :: emitted(:)
    integer :: candidate_index

    is_last = .true.
    do candidate_index = child_index + 1, size(table%items)
      if (process_is_child_of(table, candidate_index, parent_index, emitted)) then
        is_last = .false.
        return
      end if
    end do
  end function process_is_last_child

  function tree_branch_prefix(depth, ancestor_prefix, is_last) result(prefix)
    integer, intent(in) :: depth
    character(len=*), intent(in) :: ancestor_prefix
    logical, intent(in) :: is_last
    character(len=:), allocatable :: prefix

    if (depth <= 0) then
      prefix = ""
    else if (is_last) then
      prefix = bounded_prefix(ancestor_prefix // "└─ ")
    else
      prefix = bounded_prefix(ancestor_prefix // "├─ ")
    end if
  end function tree_branch_prefix

  function next_ancestor_prefix(depth, ancestor_prefix, is_last) result(prefix)
    integer, intent(in) :: depth
    character(len=*), intent(in) :: ancestor_prefix
    logical, intent(in) :: is_last
    character(len=:), allocatable :: prefix

    if (depth <= 0) then
      prefix = ""
    else if (is_last) then
      prefix = bounded_prefix(ancestor_prefix // "   ")
    else
      prefix = bounded_prefix(ancestor_prefix // "│  ")
    end if
  end function next_ancestor_prefix

  function bounded_prefix(source) result(prefix)
    character(len=*), intent(in) :: source
    character(len=:), allocatable :: prefix
    integer :: copied_len

    copied_len = min(len(source), PROCESS_TREE_PREFIX_LEN)
    prefix = source(1:copied_len)
  end function bounded_prefix

  integer function matching_previous_process(previous, current) result(match_index)
    type(process_table), intent(in) :: previous
    type(process_info), intent(in) :: current
    integer :: previous_index

    match_index = 0
    if (.not. current%valid) return
    if (current%pid <= 0 .or. current%start_time <= 0_int64) return

    do previous_index = 1, size(previous%items)
      if (.not. previous%items(previous_index)%valid) cycle
      if (previous%items(previous_index)%pid /= current%pid) cycle
      if (previous%items(previous_index)%start_time /= current%start_time) cycle
      match_index = previous_index
      return
    end do
  end function matching_previous_process

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
    case (PROCESS_SORT_PRIORITY)
      comparison = compare_integer(left%priority, right%priority)
    case (PROCESS_SORT_NICE)
      comparison = compare_integer(left%nice, right%nice)
    case (PROCESS_SORT_VIRT)
      comparison = compare_int64(left%mem_virt_bytes, right%mem_virt_bytes)
    case (PROCESS_SORT_RSS)
      comparison = compare_int64(left%mem_rss_bytes, right%mem_rss_bytes)
    case (PROCESS_SORT_SHARED)
      comparison = compare_int64(left%mem_shared_bytes, right%mem_shared_bytes)
    case (PROCESS_SORT_STATE)
      comparison = compare_text(process_state_label(left), process_state_label(right))
    case (PROCESS_SORT_CPU)
      comparison = compare_real(left%cpu_percent, right%cpu_percent)
    case (PROCESS_SORT_MEMORY)
      comparison = compare_real(left%mem_percent, right%mem_percent)
    case (PROCESS_SORT_TIME)
      comparison = compare_int64(left%cpu_time, right%cpu_time)
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

  pure real(real64) function clamp_percent(value) result(clamped)
    real(real64), intent(in) :: value

    clamped = max(0.0_real64, min(100.0_real64, value))
  end function clamp_percent

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
