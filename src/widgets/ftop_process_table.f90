module ftop_process_table
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_box, only : BOX_STYLE_ROUNDED, box_content_rect, draw_box
  use ftop_collector, only : collector_snapshot
  use ftop_proc_data, only : &
    PROCESS_SORT_COMMAND, &
    PROCESS_SORT_CPU, &
    PROCESS_SORT_MEMORY, &
    PROCESS_SORT_NICE, &
    PROCESS_SORT_PID, &
    PROCESS_SORT_PRIORITY, &
    PROCESS_SORT_RSS, &
    PROCESS_SORT_SHARED, &
    PROCESS_SORT_STATE, &
    PROCESS_SORT_TIME, &
    PROCESS_SORT_VIRT, &
    PROCESS_SORT_USER, &
    build_process_tree, &
    process_display_command, &
    process_info, &
    process_lookup_index, &
    process_state_label, &
    process_table, &
    process_user_label, &
    rebuild_process_index, &
    sort_process_table
  use ftop_sparkline, only : sparkline_glyph
  use ftop_table, only : &
    TABLE_SORT_ASCENDING, &
    TABLE_SORT_DESCENDING, &
    TABLE_SORT_NONE, &
    TABLE_SEPARATOR_SPACE, &
    TABLE_WIDTH_FIXED, &
    TABLE_WIDTH_WEIGHT, &
    calculate_column_widths, &
    make_table_cell, &
    render_table, &
    table_cell, &
    table_column, &
    table_viewport_row_count
  use ftop_text, only : TEXT_ALIGN_RIGHT, format_percent, render_text
  use ftop_widgets, only : widget_rect, widget_size
  implicit none
  private

  integer, parameter :: PROCESS_TABLE_COLUMNS = 12
  integer, parameter :: PROCESS_COLUMN_PID = 1
  integer, parameter :: PROCESS_COLUMN_USER = 2
  integer, parameter :: PROCESS_COLUMN_PRIORITY = 3
  integer, parameter :: PROCESS_COLUMN_NICE = 4
  integer, parameter :: PROCESS_COLUMN_VIRT = 5
  integer, parameter :: PROCESS_COLUMN_RSS = 6
  integer, parameter :: PROCESS_COLUMN_SHARED = 7
  integer, parameter :: PROCESS_COLUMN_STATE = 8
  integer, parameter :: PROCESS_COLUMN_CPU = 9
  integer, parameter :: PROCESS_COLUMN_MEMORY = 10
  integer, parameter :: PROCESS_COLUMN_TIME = 11
  integer, parameter :: PROCESS_COLUMN_COMMAND = 12
  integer, parameter :: DEFAULT_PROCESS_COLUMN_IDS(PROCESS_TABLE_COLUMNS) = [ &
    PROCESS_COLUMN_PID, &
    PROCESS_COLUMN_USER, &
    PROCESS_COLUMN_PRIORITY, &
    PROCESS_COLUMN_NICE, &
    PROCESS_COLUMN_VIRT, &
    PROCESS_COLUMN_RSS, &
    PROCESS_COLUMN_SHARED, &
    PROCESS_COLUMN_STATE, &
    PROCESS_COLUMN_CPU, &
    PROCESS_COLUMN_MEMORY, &
    PROCESS_COLUMN_TIME, &
    PROCESS_COLUMN_COMMAND &
  ]
  integer, parameter :: PROCESS_COLLAPSED_CAPACITY = 256
  integer, parameter :: PROCESS_SIGNAL_FEEDBACK_FRAMES = 6
  integer, parameter :: PROCESS_METRIC_SPARKLINE_WIDTH = 6
  integer, parameter, public :: PROCESS_FILTER_LEN = 96
  integer, parameter :: PROCESS_SIGNAL_NAME_LEN = 16
  integer, parameter :: PROCESS_SIGNAL_INPUT_LEN = 4

  type, public :: process_table_state
    integer :: selected_row = 1
    integer :: scroll_row = 1
    integer :: sort_key = PROCESS_SORT_PID
    integer :: sort_direction = TABLE_SORT_ASCENDING
    integer :: row_count = 0
    integer :: total_row_count = 0
    integer :: viewport_rows = 0
    integer :: selected_pid = 0
    integer(int64) :: selected_start_time = 0_int64
    logical :: selected_has_children = .false.
    integer :: collapsed_count = 0
    integer :: collapsed_pid(PROCESS_COLLAPSED_CAPACITY) = 0
    integer(int64) :: collapsed_start_time(PROCESS_COLLAPSED_CAPACITY) = 0_int64
    logical :: signal_pending = .false.
    integer :: signal_pid = 0
    integer(int64) :: signal_start_time = 0_int64
    integer :: signal_number = 0
    character(len=PROCESS_SIGNAL_NAME_LEN) :: signal_name = ""
    logical :: signal_confirm_required = .false.
    logical :: signal_confirmed = .false.
    integer :: signal_input_length = 0
    character(len=PROCESS_SIGNAL_INPUT_LEN) :: signal_input_text = ""
    integer :: signal_feedback_pid = 0
    integer(int64) :: signal_feedback_start_time = 0_int64
    integer :: signal_feedback_frames = 0
    integer :: filter_length = 0
    character(len=PROCESS_FILTER_LEN) :: filter_text = ""
    logical :: filter_active = .false.
    logical :: metric_sparklines = .true.
    logical :: tree_view = .true.
    integer :: column_count = PROCESS_TABLE_COLUMNS
    integer :: column_ids(PROCESS_TABLE_COLUMNS) = DEFAULT_PROCESS_COLUMN_IDS
  end type process_table_state

  public :: process_panel_min_size
  public :: process_table_append_filter_text
  public :: process_table_begin_signal
  public :: process_table_begin_filter
  public :: process_table_cancel_signal
  public :: process_table_clear_filter
  public :: process_table_clear_signal_input
  public :: process_table_confirm_signal
  public :: process_table_cycle_sort_key
  public :: process_table_delete_filter_char
  public :: process_table_delete_signal_digit
  public :: process_table_page_delta
  public :: process_table_mark_signal_feedback
  public :: process_table_scroll_delta
  public :: process_table_select_at
  public :: process_table_select_delta
  public :: process_table_set_signal
  public :: process_table_set_columns
  public :: process_table_signal_status
  public :: process_table_sort_at
  public :: process_table_append_signal_digit
  public :: process_table_sort_direction_label
  public :: process_table_sort_key_label
  public :: process_table_status
  public :: process_table_finish_filter
  public :: process_table_toggle_metric_sparklines
  public :: process_table_toggle_selected_node
  public :: process_table_toggle_sort_direction
  public :: process_table_toggle_tree
  public :: render_process_panel

contains

  function process_panel_min_size() result(size_value)
    type(widget_size) :: size_value

    size_value%width = 76
    size_value%height = 8
  end function process_panel_min_size

  subroutine render_process_panel(buffer, panel, snapshot, border_style, title_style, dim_style, state)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: panel
    type(collector_snapshot), intent(in) :: snapshot
    type(screen_style), intent(in) :: border_style
    type(screen_style), intent(in) :: title_style
    type(screen_style), intent(in) :: dim_style
    type(process_table_state), intent(inout), optional :: state
    type(table_cell), allocatable :: cells(:, :)
    type(table_column), allocatable :: columns(:)
    type(process_table_state) :: active_state
    type(process_table) :: sorted_processes
    type(process_table) :: tree_processes
    type(widget_rect) :: content
    type(widget_rect) :: table_content
    logical :: show_filter_bar

    active_state = process_table_state()
    if (present(state)) active_state = state
    call normalize_filter_state(active_state)
    call normalize_process_columns(active_state)
    call draw_box(buffer, panel, BOX_STYLE_ROUNDED, border_style, "Processes", title_style)
    content = box_content_rect(panel)
    if (content%height <= 0 .or. content%width <= 0) return
    show_filter_bar = process_table_filter_visible(active_state)
    table_content = process_table_content_rect(content, show_filter_bar)

    if (.not. snapshot%processes%valid) then
      call render_text(buffer, content_line_rect(table_content, 1), "processes unavailable", dim_style)
      if (present(state)) then
        active_state%total_row_count = 0
        call clear_signal_feedback_state(active_state)
        call normalize_process_table_state(active_state, 0, 0)
        state = active_state
      end if
      if (show_filter_bar) call render_process_filter_bar(buffer, content, active_state, title_style, dim_style)
      return
    end if
    if (.not. allocated(snapshot%processes%items) .or. size(snapshot%processes%items) <= 0) then
      call render_text(buffer, content_line_rect(table_content, 1), "no processes", dim_style)
      if (present(state)) then
        active_state%total_row_count = 0
        call clear_signal_feedback_state(active_state)
        call normalize_process_table_state(active_state, 0, 0)
        state = active_state
      end if
      if (show_filter_bar) call render_process_filter_bar(buffer, content, active_state, title_style, dim_style)
      return
    end if

    columns = process_columns(active_state)
    sorted_processes = snapshot%processes
    active_state%total_row_count = count_valid_processes(sorted_processes)
    call prune_collapsed_nodes(active_state, sorted_processes)
    call prune_signal_feedback_state(active_state, sorted_processes)
    call sort_process_table(sorted_processes, active_state%sort_key, &
                            descending=active_state%sort_direction == TABLE_SORT_DESCENDING)
    call filter_process_table(sorted_processes, active_state)
    if (active_state%tree_view) then
      call build_process_tree(sorted_processes)
      tree_processes = sorted_processes
      call apply_collapsed_nodes(sorted_processes, active_state)
    else
      tree_processes = sorted_processes
    end if
    cells = process_cells(sorted_processes, active_state, signal_feedback_style(title_style))
    if (size(cells, 1) <= 0) then
      if (active_state%filter_length > 0) then
        call render_text(buffer, content_line_rect(table_content, 1), "no matching processes", dim_style)
      else
        call render_text(buffer, content_line_rect(table_content, 1), "no processes", dim_style)
      end if
      if (present(state)) then
        call normalize_process_table_state(active_state, 0, table_viewport_row_count(table_content, .true.))
        call advance_signal_feedback_state(active_state)
        state = active_state
      end if
      if (show_filter_bar) call render_process_filter_bar(buffer, content, active_state, title_style, dim_style)
      return
    end if

    call normalize_process_table_state(active_state, size(cells, 1), table_viewport_row_count(table_content, .true.))
    call update_selected_process_state(active_state, sorted_processes, tree_processes)

    call render_table(buffer, table_content, columns, cells, separator=TABLE_SEPARATOR_SPACE, &
                      show_header=.true., striped=.false., style=dim_style, &
                      header_style=title_style, selected_style=title_style, separator_style=dim_style, &
                      scroll_row=active_state%scroll_row, selected_row=active_state%selected_row)
    call advance_signal_feedback_state(active_state)
    if (show_filter_bar) call render_process_filter_bar(buffer, content, active_state, title_style, dim_style)
    if (present(state)) state = active_state
  end subroutine render_process_panel

  function process_columns(state) result(columns)
    type(process_table_state), intent(in) :: state
    type(table_column), allocatable :: columns(:)
    integer :: column_index
    integer :: column_count

    column_count = process_table_column_count(state)
    allocate(columns(column_count))
    do column_index = 1, column_count
      call process_column_definition(process_table_column_id(state, column_index), columns(column_index))
    end do
    call mark_sort_column(columns, state)
  end function process_columns

  subroutine process_column_definition(column_id, column)
    integer, intent(in) :: column_id
    type(table_column), intent(out) :: column

    select case (column_id)
    case (PROCESS_COLUMN_USER)
      column%name = "USER"
      column%width_mode = TABLE_WIDTH_FIXED
      column%width = 6
    case (PROCESS_COLUMN_PRIORITY)
      column%name = "PRI"
      column%width_mode = TABLE_WIDTH_FIXED
      column%width = 3
      column%alignment = TEXT_ALIGN_RIGHT
    case (PROCESS_COLUMN_NICE)
      column%name = "NI"
      column%width_mode = TABLE_WIDTH_FIXED
      column%width = 3
      column%alignment = TEXT_ALIGN_RIGHT
    case (PROCESS_COLUMN_VIRT)
      column%name = "VIRT"
      column%width_mode = TABLE_WIDTH_FIXED
      column%width = 5
      column%alignment = TEXT_ALIGN_RIGHT
    case (PROCESS_COLUMN_RSS)
      column%name = "RES"
      column%width_mode = TABLE_WIDTH_FIXED
      column%width = 5
      column%alignment = TEXT_ALIGN_RIGHT
    case (PROCESS_COLUMN_SHARED)
      column%name = "SHR"
      column%width_mode = TABLE_WIDTH_FIXED
      column%width = 5
      column%alignment = TEXT_ALIGN_RIGHT
    case (PROCESS_COLUMN_STATE)
      column%name = "S"
      column%width_mode = TABLE_WIDTH_FIXED
      column%width = 1
    case (PROCESS_COLUMN_CPU)
      column%name = "CPU%"
      column%width_mode = TABLE_WIDTH_FIXED
      column%width = 6
      column%alignment = TEXT_ALIGN_RIGHT
    case (PROCESS_COLUMN_MEMORY)
      column%name = "MEM%"
      column%width_mode = TABLE_WIDTH_FIXED
      column%width = 6
      column%alignment = TEXT_ALIGN_RIGHT
    case (PROCESS_COLUMN_TIME)
      column%name = "TIME+"
      column%width_mode = TABLE_WIDTH_FIXED
      column%width = 7
      column%alignment = TEXT_ALIGN_RIGHT
    case (PROCESS_COLUMN_COMMAND)
      column%name = "COMMAND"
      column%width_mode = TABLE_WIDTH_WEIGHT
      column%weight = 1
    case default
      column%name = "PID"
      column%width_mode = TABLE_WIDTH_FIXED
      column%width = 5
      column%alignment = TEXT_ALIGN_RIGHT
    end select
  end subroutine process_column_definition

  subroutine mark_sort_column(columns, state)
    type(table_column), intent(inout) :: columns(:)
    type(process_table_state), intent(in) :: state
    integer :: sort_column

    columns%sort_direction = TABLE_SORT_NONE
    sort_column = process_sort_column(state%sort_key, state)
    if (sort_column >= 1 .and. sort_column <= size(columns)) columns(sort_column)%sort_direction = state%sort_direction
  end subroutine mark_sort_column

  integer function process_sort_column(sort_key, state) result(column)
    integer, intent(in) :: sort_key
    type(process_table_state), intent(in) :: state
    integer :: column_index

    column = 0
    do column_index = 1, process_table_column_count(state)
      if (process_column_sort_key(process_table_column_id(state, column_index)) == sort_key) then
        column = column_index
        return
      end if
    end do
  end function process_sort_column

  function process_cells(table, state, feedback_style) result(cells)
    type(process_table), intent(in) :: table
    type(process_table_state), intent(in) :: state
    type(screen_style), intent(in) :: feedback_style
    type(table_cell), allocatable :: cells(:, :)
    integer :: column_count
    integer :: process_index
    integer :: row
    integer :: valid_count

    valid_count = 0
    do process_index = 1, size(table%items)
      if (table%items(process_index)%valid) valid_count = valid_count + 1
    end do

    column_count = process_table_column_count(state)
    allocate(cells(valid_count, column_count))
    row = 0
    do process_index = 1, size(table%items)
      if (.not. table%items(process_index)%valid) cycle
      row = row + 1
      if (process_signal_feedback_matches(state, table%items(process_index))) then
        call fill_process_row(cells, row, table%items(process_index), state, feedback_style)
      else
        call fill_process_row(cells, row, table%items(process_index), state)
      end if
    end do
  end function process_cells

  subroutine filter_process_table(table, state)
    type(process_table), intent(inout) :: table
    type(process_table_state), intent(inout) :: state
    type(process_info), allocatable :: filtered(:)
    character(len=:), allocatable :: query
    integer :: match_count
    integer :: process_index
    integer :: row

    call normalize_filter_state(state)
    if (state%filter_length <= 0) then
      state%row_count = count_valid_processes(table)
      return
    end if
    if (.not. allocated(table%items)) then
      state%row_count = 0
      return
    end if

    query = process_table_filter_text(state)
    match_count = 0
    do process_index = 1, size(table%items)
      if (process_matches_filter(table%items(process_index), query)) match_count = match_count + 1
    end do

    allocate(filtered(match_count))
    row = 0
    do process_index = 1, size(table%items)
      if (.not. process_matches_filter(table%items(process_index), query)) cycle
      row = row + 1
      filtered(row) = table%items(process_index)
    end do
    call move_alloc(filtered, table%items)
    call rebuild_process_index(table)
    state%row_count = match_count
  end subroutine filter_process_table

  integer function count_valid_processes(table) result(count)
    type(process_table), intent(in) :: table
    integer :: process_index

    count = 0
    if (.not. allocated(table%items)) return
    do process_index = 1, size(table%items)
      if (table%items(process_index)%valid) count = count + 1
    end do
  end function count_valid_processes

  logical function process_matches_filter(process, query) result(matches)
    type(process_info), intent(in) :: process
    character(len=*), intent(in) :: query
    character(len=:), allocatable :: needle

    if (.not. process%valid) then
      matches = .false.
      return
    end if
    if (len(query) <= 0) then
      matches = .true.
      return
    end if

    needle = ascii_lower(query)
    matches = index(ascii_lower(process_display_command(process)), needle) > 0
    if (matches) return
    matches = index(ascii_lower(trim(process%name)), needle) > 0
    if (matches) return
    matches = index(ascii_lower(process_user_label(process)), needle) > 0
  end function process_matches_filter

  subroutine apply_collapsed_nodes(table, state)
    type(process_table), intent(inout) :: table
    type(process_table_state), intent(in) :: state
    type(process_info), allocatable :: visible(:)
    integer :: hidden_depth
    integer :: process_index
    integer :: row

    if (.not. allocated(table%items)) return
    if (state%collapsed_count <= 0) return

    allocate(visible(size(table%items)))
    visible = process_info()
    hidden_depth = -1
    row = 0
    do process_index = 1, size(table%items)
      if (.not. table%items(process_index)%valid) cycle
      if (hidden_depth >= 0) then
        if (table%items(process_index)%tree_depth > hidden_depth) cycle
        hidden_depth = -1
      end if

      row = row + 1
      visible(row) = table%items(process_index)
      if (process_collapsed(state, table%items(process_index))) hidden_depth = table%items(process_index)%tree_depth
    end do

    call replace_process_items(table, visible, row)
  end subroutine apply_collapsed_nodes

  subroutine prune_collapsed_nodes(state, table)
    type(process_table_state), intent(inout) :: state
    type(process_table), intent(in) :: table
    integer :: collapsed_index
    integer :: kept_count

    call normalize_collapsed_state(state)
    if (state%collapsed_count <= 0) return

    kept_count = 0
    do collapsed_index = 1, state%collapsed_count
      if (.not. process_identity_exists(table, state%collapsed_pid(collapsed_index), &
                                        state%collapsed_start_time(collapsed_index))) cycle
      kept_count = kept_count + 1
      state%collapsed_pid(kept_count) = state%collapsed_pid(collapsed_index)
      state%collapsed_start_time(kept_count) = state%collapsed_start_time(collapsed_index)
    end do

    state%collapsed_count = kept_count
    if (kept_count < PROCESS_COLLAPSED_CAPACITY) then
      state%collapsed_pid(kept_count + 1:) = 0
      state%collapsed_start_time(kept_count + 1:) = 0_int64
    end if
  end subroutine prune_collapsed_nodes

  subroutine replace_process_items(table, items, item_count)
    type(process_table), intent(inout) :: table
    type(process_info), intent(in) :: items(:)
    integer, intent(in) :: item_count
    type(process_info), allocatable :: compact(:)

    allocate(compact(max(0, item_count)))
    if (item_count > 0) compact = items(:item_count)
    call move_alloc(compact, table%items)
    call rebuild_process_index(table)
  end subroutine replace_process_items

  subroutine update_selected_process_state(state, visible_table, full_tree_table)
    type(process_table_state), intent(inout) :: state
    type(process_table), intent(in) :: visible_table
    type(process_table), intent(in) :: full_tree_table
    integer :: process_index
    integer :: row

    state%selected_pid = 0
    state%selected_start_time = 0_int64
    state%selected_has_children = .false.
    if (state%selected_row <= 0) return
    if (.not. allocated(visible_table%items)) return

    row = 0
    do process_index = 1, size(visible_table%items)
      if (.not. visible_table%items(process_index)%valid) cycle
      row = row + 1
      if (row /= state%selected_row) cycle
      state%selected_pid = visible_table%items(process_index)%pid
      state%selected_start_time = visible_table%items(process_index)%start_time
      state%selected_has_children = process_has_child(full_tree_table, visible_table%items(process_index))
      return
    end do
  end subroutine update_selected_process_state

  logical function process_has_child(table, parent) result(has_child)
    type(process_table), intent(in) :: table
    type(process_info), intent(in) :: parent
    integer :: process_index

    has_child = .false.
    if (.not. allocated(table%items)) return
    do process_index = 1, size(table%items)
      if (.not. table%items(process_index)%valid) cycle
      if (table%items(process_index)%pid == parent%pid) cycle
      if (table%items(process_index)%ppid == parent%pid) then
        has_child = .true.
        return
      end if
    end do
  end function process_has_child

  logical function process_collapsed(state, process) result(collapsed)
    type(process_table_state), intent(in) :: state
    type(process_info), intent(in) :: process

    collapsed = collapsed_node_index(state, process%pid, process%start_time) > 0
  end function process_collapsed

  integer function collapsed_node_index(state, pid, start_time) result(node_index)
    type(process_table_state), intent(in) :: state
    integer, intent(in) :: pid
    integer(int64), intent(in) :: start_time
    integer :: collapsed_index

    node_index = 0
    if (pid <= 0) return
    do collapsed_index = 1, max(0, min(PROCESS_COLLAPSED_CAPACITY, state%collapsed_count))
      if (process_identity_matches(state%collapsed_pid(collapsed_index), state%collapsed_start_time(collapsed_index), &
                                   pid, start_time)) then
        node_index = collapsed_index
        return
      end if
    end do
  end function collapsed_node_index

  logical function process_identity_exists(table, pid, start_time) result(exists)
    type(process_table), intent(in) :: table
    integer, intent(in) :: pid
    integer(int64), intent(in) :: start_time

    exists = process_lookup_index(table, pid, start_time) > 0
  end function process_identity_exists

  subroutine prune_signal_feedback_state(state, table)
    type(process_table_state), intent(inout) :: state
    type(process_table), intent(in) :: table

    if (.not. signal_feedback_active(state)) return
    if (process_identity_exists(table, state%signal_feedback_pid, state%signal_feedback_start_time)) return
    call clear_signal_feedback_state(state)
  end subroutine prune_signal_feedback_state

  logical function process_identity_matches(left_pid, left_start_time, right_pid, right_start_time) result(matches)
    integer, intent(in) :: left_pid
    integer(int64), intent(in) :: left_start_time
    integer, intent(in) :: right_pid
    integer(int64), intent(in) :: right_start_time

    matches = .false.
    if (left_pid /= right_pid) return
    if (left_start_time > 0_int64 .and. right_start_time > 0_int64) then
      matches = left_start_time == right_start_time
    else
      matches = .true.
    end if
  end function process_identity_matches

  logical function process_signal_feedback_matches(state, process) result(matches)
    type(process_table_state), intent(in) :: state
    type(process_info), intent(in) :: process

    matches = .false.
    if (.not. signal_feedback_active(state)) return
    matches = process_identity_matches(process%pid, process%start_time, &
                                       state%signal_feedback_pid, state%signal_feedback_start_time)
  end function process_signal_feedback_matches

  logical function signal_feedback_active(state) result(active)
    type(process_table_state), intent(in) :: state

    active = state%signal_feedback_pid > 0 .and. state%signal_feedback_frames > 0
  end function signal_feedback_active

  function signal_feedback_style(base_style) result(style)
    type(screen_style), intent(in) :: base_style
    type(screen_style) :: style

    style = base_style
    style%bold = .true.
    style%inverse = .true.
  end function signal_feedback_style

  subroutine fill_process_row(cells, row_index, process, state, row_style)
    type(table_cell), intent(inout) :: cells(:, :)
    integer, intent(in) :: row_index
    type(process_info), intent(in) :: process
    type(process_table_state), intent(in) :: state
    type(screen_style), intent(in), optional :: row_style
    integer :: column_index

    if (row_index < 1 .or. row_index > size(cells, 1)) return
    do column_index = 1, size(cells, 2)
      cells(row_index, column_index) = process_column_cell(process, state, column_index, row_style)
    end do
  end subroutine fill_process_row

  function process_column_cell(process, state, column_index, row_style) result(cell)
    type(process_info), intent(in) :: process
    type(process_table_state), intent(in) :: state
    integer, intent(in) :: column_index
    type(screen_style), intent(in), optional :: row_style
    type(table_cell) :: cell

    select case (process_table_column_id(state, column_index))
    case (PROCESS_COLUMN_USER)
      cell = process_cell(process_user_label(process), row_style)
    case (PROCESS_COLUMN_PRIORITY)
      cell = process_cell(integer_text(process%priority), row_style)
    case (PROCESS_COLUMN_NICE)
      cell = process_cell(integer_text(process%nice), row_style)
    case (PROCESS_COLUMN_VIRT)
      cell = process_cell(process_size_text(process%mem_virt_bytes), row_style)
    case (PROCESS_COLUMN_RSS)
      cell = process_cell(process_size_text(process%mem_rss_bytes), row_style)
    case (PROCESS_COLUMN_SHARED)
      cell = process_cell(process_size_text(process%mem_shared_bytes), row_style)
    case (PROCESS_COLUMN_STATE)
      cell = process_cell(process_state_label(process), row_style)
    case (PROCESS_COLUMN_CPU)
      cell = process_cell(process_metric_text(process%cpu_percent, process%cpu_history, &
                                              process%history_count, state%metric_sparklines), row_style)
    case (PROCESS_COLUMN_MEMORY)
      cell = process_cell(process_metric_text(process%mem_percent, process%mem_history, &
                                              process%history_count, state%metric_sparklines), row_style)
    case (PROCESS_COLUMN_TIME)
      cell = process_cell(process_time_text(process%cpu_time), row_style)
    case (PROCESS_COLUMN_COMMAND)
      cell = process_cell(process_tree_display_command(process), row_style)
    case default
      cell = process_cell(integer_text(process%pid), row_style)
    end select
  end function process_column_cell

  function process_cell(text, row_style) result(cell)
    character(len=*), intent(in) :: text
    type(screen_style), intent(in), optional :: row_style
    type(table_cell) :: cell

    if (present(row_style)) then
      cell = make_table_cell(text, row_style)
    else
      cell = make_table_cell(text)
    end if
  end function process_cell

  function process_metric_text(value, history, history_count, show_sparklines) result(text)
    real(real64), intent(in) :: value
    real(real64), intent(in) :: history(:)
    integer, intent(in) :: history_count
    logical, intent(in) :: show_sparklines
    character(len=:), allocatable :: text

    if (show_sparklines .and. history_count >= 2 .and. size(history) > 0) then
      text = process_sparkline_text(history, history_count)
    else
      text = format_percent(real(clamp_percent(value)))
    end if
  end function process_metric_text

  function process_size_text(bytes) result(text)
    integer(int64), intent(in) :: bytes
    character(len=:), allocatable :: text
    character(len=32) :: scratch
    real :: value

    if (bytes < 1024_int64) then
      write(scratch, '(i0,a)') max(0_int64, bytes), "B"
    else if (bytes < 1024_int64 ** 2) then
      value = real(max(0_int64, bytes)) / 1024.0
      write(scratch, '(f5.1,a)') value, "K"
    else if (bytes < 1024_int64 ** 3) then
      value = real(max(0_int64, bytes)) / real(1024_int64 ** 2)
      write(scratch, '(f5.1,a)') value, "M"
    else
      value = real(max(0_int64, bytes)) / real(1024_int64 ** 3)
      write(scratch, '(f5.1,a)') value, "G"
    end if
    text = trim(adjustl(scratch))
  end function process_size_text

  function process_time_text(cpu_time_ms) result(text)
    integer(int64), intent(in) :: cpu_time_ms
    character(len=:), allocatable :: text
    character(len=32) :: scratch
    integer(int64) :: centiseconds
    integer(int64) :: hours
    integer(int64) :: minutes
    integer(int64) :: seconds
    integer(int64) :: total_ms

    total_ms = max(0_int64, cpu_time_ms)
    hours = total_ms / 3600000_int64
    minutes = mod(total_ms / 60000_int64, 60_int64)
    seconds = mod(total_ms / 1000_int64, 60_int64)
    centiseconds = mod(total_ms / 10_int64, 100_int64)

    if (hours > 0_int64) then
      write(scratch, '(i0,":",i2.2,":",i2.2)') hours, minutes, seconds
    else
      write(scratch, '(i0,":",i2.2,".",i2.2)') minutes, seconds, centiseconds
    end if
    text = trim(scratch)
  end function process_time_text

  function process_sparkline_text(history, history_count) result(text)
    real(real64), intent(in) :: history(:)
    integer, intent(in) :: history_count
    character(len=:), allocatable :: text
    integer :: count
    integer :: first
    integer :: sample_index

    count = max(0, min(min(size(history), history_count), PROCESS_METRIC_SPARKLINE_WIDTH))
    if (count <= 0) then
      text = ""
      return
    end if

    text = ""
    first = max(1, min(size(history), history_count) - count + 1)
    do sample_index = first, first + count - 1
      text = text // sparkline_glyph(real(clamp_percent(history(sample_index)) / 100.0_real64))
    end do
  end function process_sparkline_text

  function process_tree_display_command(process) result(text)
    type(process_info), intent(in) :: process
    character(len=:), allocatable :: text

    if (len_trim(process%tree_prefix) > 0) then
      text = trim(process%tree_prefix) // " " // process_display_command(process)
    else
      text = process_display_command(process)
    end if
  end function process_tree_display_command

  logical function process_table_set_columns(state, names, count, error_message) result(applied)
    type(process_table_state), intent(inout) :: state
    character(len=*), intent(in) :: names(:)
    integer, intent(in) :: count
    character(len=:), allocatable, intent(out) :: error_message
    integer :: column_index
    integer :: column_id
    integer :: ids(PROCESS_TABLE_COLUMNS)

    applied = .false.
    error_message = ""
    ids = 0

    if (count <= 0) then
      error_message = "process columns must contain at least one column"
      return
    end if
    if (count > size(names) .or. count > PROCESS_TABLE_COLUMNS) then
      error_message = "process columns cannot contain more than " // integer_text(PROCESS_TABLE_COLUMNS) // " columns"
      return
    end if

    do column_index = 1, count
      column_id = process_column_id_for_name(names(column_index))
      if (column_id == 0) then
        error_message = "unknown process column " // trim(names(column_index))
        return
      end if
      if (column_id_exists(ids, column_index - 1, column_id)) then
        error_message = "duplicate process column " // trim(names(column_index))
        return
      end if
      ids(column_index) = column_id
    end do

    state%column_count = count
    state%column_ids = 0
    state%column_ids(:count) = ids(:count)
    call normalize_process_columns(state)
    applied = .true.
  end function process_table_set_columns

  integer function process_column_id_for_name(name) result(column_id)
    character(len=*), intent(in) :: name
    character(len=:), allocatable :: normalized

    normalized = ascii_lower(trim(name))
    select case (normalized)
    case ("pid")
      column_id = PROCESS_COLUMN_PID
    case ("user")
      column_id = PROCESS_COLUMN_USER
    case ("pri", "priority")
      column_id = PROCESS_COLUMN_PRIORITY
    case ("ni", "nice")
      column_id = PROCESS_COLUMN_NICE
    case ("virt", "virtual", "vsz")
      column_id = PROCESS_COLUMN_VIRT
    case ("res", "rss")
      column_id = PROCESS_COLUMN_RSS
    case ("shr", "shared")
      column_id = PROCESS_COLUMN_SHARED
    case ("s", "state")
      column_id = PROCESS_COLUMN_STATE
    case ("cpu", "cpu%")
      column_id = PROCESS_COLUMN_CPU
    case ("mem", "mem%", "memory")
      column_id = PROCESS_COLUMN_MEMORY
    case ("time", "time+")
      column_id = PROCESS_COLUMN_TIME
    case ("cmd", "command")
      column_id = PROCESS_COLUMN_COMMAND
    case default
      column_id = 0
    end select
  end function process_column_id_for_name

  logical function column_id_exists(ids, count, column_id) result(exists)
    integer, intent(in) :: ids(:)
    integer, intent(in) :: count
    integer, intent(in) :: column_id
    integer :: index_value

    exists = .false.
    do index_value = 1, max(0, min(count, size(ids)))
      if (ids(index_value) == column_id) then
        exists = .true.
        return
      end if
    end do
  end function column_id_exists

  subroutine process_table_begin_filter(state)
    type(process_table_state), intent(inout) :: state

    call normalize_filter_state(state)
    state%filter_active = .true.
  end subroutine process_table_begin_filter

  subroutine process_table_finish_filter(state)
    type(process_table_state), intent(inout) :: state

    state%filter_active = .false.
    call normalize_filter_state(state)
  end subroutine process_table_finish_filter

  subroutine process_table_clear_filter(state)
    type(process_table_state), intent(inout) :: state

    state%filter_active = .false.
    state%filter_length = 0
    state%filter_text = ""
    state%selected_row = 1
    state%scroll_row = 1
    call normalize_process_table_state(state, state%row_count, state%viewport_rows)
  end subroutine process_table_clear_filter

  subroutine process_table_append_filter_text(state, text)
    type(process_table_state), intent(inout) :: state
    character(len=*), intent(in) :: text
    integer :: char_index

    call normalize_filter_state(state)
    state%filter_active = .true.
    do char_index = 1, len(text)
      if (state%filter_length >= PROCESS_FILTER_LEN) exit
      state%filter_length = state%filter_length + 1
      state%filter_text(state%filter_length:state%filter_length) = text(char_index:char_index)
    end do
    state%selected_row = 1
    state%scroll_row = 1
  end subroutine process_table_append_filter_text

  subroutine process_table_delete_filter_char(state)
    type(process_table_state), intent(inout) :: state

    call normalize_filter_state(state)
    state%filter_active = .true.
    if (state%filter_length <= 0) return
    state%filter_text(state%filter_length:state%filter_length) = " "
    state%filter_length = state%filter_length - 1
    state%selected_row = 1
    state%scroll_row = 1
  end subroutine process_table_delete_filter_char

  logical function process_table_begin_signal(state, signal_number, signal_name, confirm_required) result(started)
    type(process_table_state), intent(inout) :: state
    integer, intent(in) :: signal_number
    character(len=*), intent(in) :: signal_name
    logical, intent(in), optional :: confirm_required

    started = .false.
    if (state%selected_pid <= 0) return
    if (signal_number <= 0) return

    state%signal_pending = .true.
    state%signal_pid = state%selected_pid
    state%signal_start_time = state%selected_start_time
    call process_table_clear_signal_input(state)
    started = process_table_set_signal(state, signal_number, signal_name, confirm_required)
  end function process_table_begin_signal

  logical function process_table_set_signal(state, signal_number, signal_name, confirm_required) result(started)
    type(process_table_state), intent(inout) :: state
    integer, intent(in) :: signal_number
    character(len=*), intent(in) :: signal_name
    logical, intent(in), optional :: confirm_required

    started = .false.
    if (.not. state%signal_pending) return
    if (signal_number <= 0) return
    state%signal_number = signal_number
    state%signal_name = signal_name
    state%signal_confirm_required = .false.
    if (present(confirm_required)) state%signal_confirm_required = confirm_required
    state%signal_confirmed = .not. state%signal_confirm_required
    started = .true.
  end function process_table_set_signal

  subroutine process_table_confirm_signal(state)
    type(process_table_state), intent(inout) :: state

    if (.not. state%signal_pending) return
    if (.not. state%signal_confirm_required) return
    state%signal_confirmed = .true.
  end subroutine process_table_confirm_signal

  subroutine process_table_append_signal_digit(state, text, signal_number, changed)
    type(process_table_state), intent(inout) :: state
    character(len=*), intent(in) :: text
    integer, intent(out) :: signal_number
    logical, intent(out) :: changed
    integer :: char_index

    signal_number = state%signal_number
    changed = .false.
    if (.not. state%signal_pending) return
    do char_index = 1, len(text)
      if (.not. ascii_digit(text(char_index:char_index))) cycle
      if (state%signal_input_length == 0 .and. text(char_index:char_index) == "0") cycle
      if (state%signal_input_length >= PROCESS_SIGNAL_INPUT_LEN) exit
      state%signal_input_length = state%signal_input_length + 1
      state%signal_input_text(state%signal_input_length:state%signal_input_length) = text(char_index:char_index)
      changed = .true.
    end do
    if (.not. changed) return
    signal_number = signal_input_number(state)
  end subroutine process_table_append_signal_digit

  subroutine process_table_delete_signal_digit(state, signal_number, changed)
    type(process_table_state), intent(inout) :: state
    integer, intent(out) :: signal_number
    logical, intent(out) :: changed

    signal_number = state%signal_number
    changed = .false.
    if (.not. state%signal_pending) return
    if (state%signal_input_length <= 0) return
    state%signal_input_text(state%signal_input_length:state%signal_input_length) = " "
    state%signal_input_length = state%signal_input_length - 1
    changed = .true.
    signal_number = signal_input_number(state)
  end subroutine process_table_delete_signal_digit

  subroutine process_table_clear_signal_input(state)
    type(process_table_state), intent(inout) :: state

    state%signal_input_length = 0
    state%signal_input_text = ""
  end subroutine process_table_clear_signal_input

  subroutine process_table_cancel_signal(state)
    type(process_table_state), intent(inout) :: state

    state%signal_pending = .false.
    state%signal_pid = 0
    state%signal_start_time = 0_int64
    state%signal_number = 0
    state%signal_name = ""
    state%signal_confirm_required = .false.
    state%signal_confirmed = .false.
    call process_table_clear_signal_input(state)
  end subroutine process_table_cancel_signal

  subroutine process_table_mark_signal_feedback(state, pid, start_time)
    type(process_table_state), intent(inout) :: state
    integer, intent(in) :: pid
    integer(int64), intent(in) :: start_time

    if (pid <= 0) return
    state%signal_feedback_pid = pid
    state%signal_feedback_start_time = start_time
    state%signal_feedback_frames = PROCESS_SIGNAL_FEEDBACK_FRAMES
  end subroutine process_table_mark_signal_feedback

  subroutine clear_signal_feedback_state(state)
    type(process_table_state), intent(inout) :: state

    state%signal_feedback_pid = 0
    state%signal_feedback_start_time = 0_int64
    state%signal_feedback_frames = 0
  end subroutine clear_signal_feedback_state

  subroutine advance_signal_feedback_state(state)
    type(process_table_state), intent(inout) :: state

    if (.not. signal_feedback_active(state)) then
      call clear_signal_feedback_state(state)
      return
    end if
    state%signal_feedback_frames = state%signal_feedback_frames - 1
    if (state%signal_feedback_frames <= 0) call clear_signal_feedback_state(state)
  end subroutine advance_signal_feedback_state

  subroutine process_table_select_delta(state, delta)
    type(process_table_state), intent(inout) :: state
    integer, intent(in) :: delta

    state%selected_row = state%selected_row + delta
    call normalize_process_table_state(state, state%row_count, state%viewport_rows)
  end subroutine process_table_select_delta

  subroutine process_table_page_delta(state, delta_pages)
    type(process_table_state), intent(inout) :: state
    integer, intent(in) :: delta_pages
    integer :: step

    step = max(1, state%viewport_rows)
    call process_table_select_delta(state, delta_pages * step)
  end subroutine process_table_page_delta

  subroutine process_table_scroll_delta(state, delta_rows)
    type(process_table_state), intent(inout) :: state
    integer, intent(in) :: delta_rows

    call process_table_select_delta(state, delta_rows)
  end subroutine process_table_scroll_delta

  logical function process_table_select_at(state, panel, row, col) result(selected)
    type(process_table_state), intent(inout) :: state
    type(widget_rect), intent(in) :: panel
    integer, intent(in) :: row
    integer, intent(in) :: col
    type(widget_rect) :: content
    type(widget_rect) :: table_content
    integer :: data_row

    selected = .false.
    content = box_content_rect(panel)
    table_content = process_table_content_rect(content, process_table_filter_visible(state))
    if (.not. point_in_rect(table_content, row, col)) return
    if (row <= table_content%row) return

    data_row = state%scroll_row + row - table_content%row - 1
    if (data_row < 1 .or. data_row > state%row_count) return
    state%selected_row = data_row
    call normalize_process_table_state(state, state%row_count, state%viewport_rows)
    selected = .true.
  end function process_table_select_at

  logical function process_table_sort_at(state, panel, row, col) result(sorted)
    type(process_table_state), intent(inout) :: state
    type(widget_rect), intent(in) :: panel
    integer, intent(in) :: row
    integer, intent(in) :: col
    type(widget_rect) :: content
    type(widget_rect) :: table_content
    integer :: column
    integer :: sort_key

    sorted = .false.
    call normalize_process_columns(state)
    content = box_content_rect(panel)
    table_content = process_table_content_rect(content, process_table_filter_visible(state))
    if (.not. point_in_rect(table_content, row, col)) return
    if (row /= table_content%row) return

    column = process_table_column_at(state, table_content, col)
    sort_key = process_sort_key_for_column(state, column)
    if (sort_key == 0) return

    if (state%sort_key == sort_key) then
      call process_table_toggle_sort_direction(state)
    else
      state%sort_key = sort_key
      state%sort_direction = TABLE_SORT_ASCENDING
    end if
    sorted = .true.
  end function process_table_sort_at

  subroutine process_table_cycle_sort_key(state, direction)
    type(process_table_state), intent(inout) :: state
    integer, intent(in) :: direction
    integer :: column_index

    call normalize_process_columns(state)
    column_index = process_sort_column(state%sort_key, state)
    if (column_index <= 0) column_index = 1
    column_index = modulo(column_index - 1 + direction, state%column_count) + 1
    state%sort_key = process_column_sort_key(state%column_ids(column_index))
  end subroutine process_table_cycle_sort_key

  subroutine process_table_toggle_sort_direction(state)
    type(process_table_state), intent(inout) :: state

    if (state%sort_direction == TABLE_SORT_DESCENDING) then
      state%sort_direction = TABLE_SORT_ASCENDING
    else
      state%sort_direction = TABLE_SORT_DESCENDING
    end if
  end subroutine process_table_toggle_sort_direction

  subroutine process_table_toggle_tree(state)
    type(process_table_state), intent(inout) :: state

    state%tree_view = .not. state%tree_view
    call normalize_process_table_state(state, state%row_count, state%viewport_rows)
  end subroutine process_table_toggle_tree

  subroutine process_table_toggle_metric_sparklines(state)
    type(process_table_state), intent(inout) :: state

    state%metric_sparklines = .not. state%metric_sparklines
  end subroutine process_table_toggle_metric_sparklines

  logical function process_table_toggle_selected_node(state) result(toggled)
    type(process_table_state), intent(inout) :: state
    integer :: node_index

    toggled = .false.
    call normalize_collapsed_state(state)
    if (.not. state%tree_view) return
    if (.not. state%selected_has_children) return
    if (state%selected_pid <= 0) return

    node_index = collapsed_node_index(state, state%selected_pid, state%selected_start_time)
    if (node_index > 0) then
      call remove_collapsed_node(state, node_index)
    else
      if (state%collapsed_count >= PROCESS_COLLAPSED_CAPACITY) return
      state%collapsed_count = state%collapsed_count + 1
      state%collapsed_pid(state%collapsed_count) = state%selected_pid
      state%collapsed_start_time(state%collapsed_count) = state%selected_start_time
    end if
    toggled = .true.
  end function process_table_toggle_selected_node

  function process_table_status(state) result(text)
    type(process_table_state), intent(in) :: state
    character(len=:), allocatable :: text
    character(len=:), allocatable :: view_text

    if (state%tree_view) then
      view_text = "tree"
    else
      view_text = "flat"
    end if
    text = "process " // view_text // " row " // integer_text(max(0, state%selected_row)) // "/" // &
           integer_text(max(0, state%row_count)) // " sort " // process_table_sort_key_label(state) // " " // &
           process_table_sort_direction_label(state)
    if (state%tree_view .and. state%collapsed_count > 0) then
      text = text // " collapsed " // integer_text(max(0, state%collapsed_count))
    end if
    if (state%metric_sparklines) then
      text = text // " history spark"
    else
      text = text // " history numeric"
    end if
    if (state%signal_pending) text = text // " " // process_table_signal_status(state)
    if (process_table_filter_visible(state)) then
      text = text // " filter " // process_table_filter_text(state) // " showing " // &
             integer_text(max(0, state%row_count)) // "/" // integer_text(max(0, state%total_row_count))
    end if
  end function process_table_status

  function process_table_signal_status(state) result(text)
    type(process_table_state), intent(in) :: state
    character(len=:), allocatable :: text

    if (state%signal_pending) then
      if (state%signal_confirm_required .and. .not. state%signal_confirmed) then
        text = "signal " // process_signal_name_text(state) // " pid " // integer_text(max(0, state%signal_pid)) // &
               " enter to confirm escape to cancel"
      else if (state%signal_confirm_required) then
        text = "signal " // process_signal_name_text(state) // " pid " // integer_text(max(0, state%signal_pid)) // &
               " confirmed enter to send escape to cancel"
      else
        text = "signal " // process_signal_name_text(state) // " pid " // integer_text(max(0, state%signal_pid)) // &
               " enter to send arrows cycle number to set escape to cancel"
      end if
    else
      text = "signal inactive"
    end if
  end function process_table_signal_status

  function process_table_sort_key_label(state) result(label)
    type(process_table_state), intent(in) :: state
    character(len=:), allocatable :: label

    select case (state%sort_key)
    case (PROCESS_SORT_USER)
      label = "user"
    case (PROCESS_SORT_PRIORITY)
      label = "priority"
    case (PROCESS_SORT_NICE)
      label = "nice"
    case (PROCESS_SORT_VIRT)
      label = "virt"
    case (PROCESS_SORT_RSS)
      label = "rss"
    case (PROCESS_SORT_SHARED)
      label = "shared"
    case (PROCESS_SORT_STATE)
      label = "state"
    case (PROCESS_SORT_CPU)
      label = "cpu"
    case (PROCESS_SORT_MEMORY)
      label = "mem"
    case (PROCESS_SORT_TIME)
      label = "time"
    case (PROCESS_SORT_COMMAND)
      label = "command"
    case default
      label = "pid"
    end select
  end function process_table_sort_key_label

  function process_table_sort_direction_label(state) result(label)
    type(process_table_state), intent(in) :: state
    character(len=:), allocatable :: label

    if (state%sort_direction == TABLE_SORT_DESCENDING) then
      label = "desc"
    else
      label = "asc"
    end if
  end function process_table_sort_direction_label

  function process_signal_name_text(state) result(text)
    type(process_table_state), intent(in) :: state
    character(len=:), allocatable :: text

    if (state%signal_input_length > 0) then
      text = signal_input_text(state)
    else if (len_trim(state%signal_name) > 0) then
      text = trim(state%signal_name)
    else
      text = integer_text(max(0, state%signal_number))
    end if
  end function process_signal_name_text

  function signal_input_text(state) result(text)
    type(process_table_state), intent(in) :: state
    character(len=:), allocatable :: text
    integer :: length

    length = max(0, min(PROCESS_SIGNAL_INPUT_LEN, state%signal_input_length))
    if (length <= 0) then
      text = ""
    else
      text = state%signal_input_text(:length)
    end if
  end function signal_input_text

  integer function signal_input_number(state) result(signal_number)
    type(process_table_state), intent(in) :: state
    character(len=:), allocatable :: scratch
    integer :: status

    signal_number = 0
    if (state%signal_input_length <= 0) return
    scratch = signal_input_text(state)
    read(scratch, *, iostat=status) signal_number
    if (status /= 0) signal_number = 0
  end function signal_input_number

  logical function ascii_digit(text) result(is_digit)
    character(len=*), intent(in) :: text
    integer :: code

    is_digit = .false.
    if (len(text) <= 0) return
    code = iachar(text(1:1))
    is_digit = code >= iachar("0") .and. code <= iachar("9")
  end function ascii_digit

  integer function process_table_column_count(state) result(count)
    type(process_table_state), intent(in) :: state

    count = state%column_count
    if (count < 1 .or. count > PROCESS_TABLE_COLUMNS) count = PROCESS_TABLE_COLUMNS
  end function process_table_column_count

  integer function process_table_column_id(state, column_index) result(column_id)
    type(process_table_state), intent(in) :: state
    integer, intent(in) :: column_index
    integer :: count

    column_id = 0
    count = process_table_column_count(state)
    if (column_index < 1 .or. column_index > count) return

    if (state%column_count >= 1 .and. state%column_count <= PROCESS_TABLE_COLUMNS) then
      column_id = state%column_ids(column_index)
      if (valid_process_column_id(column_id)) return
    end if
    column_id = DEFAULT_PROCESS_COLUMN_IDS(column_index)
  end function process_table_column_id

  integer function process_column_sort_key(column_id) result(sort_key)
    integer, intent(in) :: column_id

    select case (column_id)
    case (PROCESS_COLUMN_USER)
      sort_key = PROCESS_SORT_USER
    case (PROCESS_COLUMN_PRIORITY)
      sort_key = PROCESS_SORT_PRIORITY
    case (PROCESS_COLUMN_NICE)
      sort_key = PROCESS_SORT_NICE
    case (PROCESS_COLUMN_VIRT)
      sort_key = PROCESS_SORT_VIRT
    case (PROCESS_COLUMN_RSS)
      sort_key = PROCESS_SORT_RSS
    case (PROCESS_COLUMN_SHARED)
      sort_key = PROCESS_SORT_SHARED
    case (PROCESS_COLUMN_STATE)
      sort_key = PROCESS_SORT_STATE
    case (PROCESS_COLUMN_CPU)
      sort_key = PROCESS_SORT_CPU
    case (PROCESS_COLUMN_MEMORY)
      sort_key = PROCESS_SORT_MEMORY
    case (PROCESS_COLUMN_TIME)
      sort_key = PROCESS_SORT_TIME
    case (PROCESS_COLUMN_COMMAND)
      sort_key = PROCESS_SORT_COMMAND
    case default
      sort_key = PROCESS_SORT_PID
    end select
  end function process_column_sort_key

  logical function valid_process_column_id(column_id) result(valid)
    integer, intent(in) :: column_id

    valid = column_id >= PROCESS_COLUMN_PID .and. column_id <= PROCESS_COLUMN_COMMAND
  end function valid_process_column_id

  subroutine normalize_process_columns(state)
    type(process_table_state), intent(inout) :: state
    integer :: column_index
    integer :: column_id
    integer :: count
    integer :: ids(PROCESS_TABLE_COLUMNS)

    ids = 0
    count = 0
    do column_index = 1, max(0, min(PROCESS_TABLE_COLUMNS, state%column_count))
      column_id = state%column_ids(column_index)
      if (.not. valid_process_column_id(column_id)) cycle
      if (column_id_exists(ids, count, column_id)) cycle
      count = count + 1
      ids(count) = column_id
    end do

    if (count <= 0) then
      count = PROCESS_TABLE_COLUMNS
      ids = DEFAULT_PROCESS_COLUMN_IDS
    end if

    state%column_count = count
    state%column_ids = 0
    state%column_ids(:count) = ids(:count)
    if (process_sort_column(state%sort_key, state) == 0) then
      state%sort_key = process_column_sort_key(state%column_ids(1))
    end if
  end subroutine normalize_process_columns

  subroutine normalize_process_table_state(state, row_count, viewport_rows)
    type(process_table_state), intent(inout) :: state
    integer, intent(in) :: row_count
    integer, intent(in) :: viewport_rows
    integer :: max_scroll_row

    state%row_count = max(0, row_count)
    state%total_row_count = max(state%row_count, state%total_row_count)
    state%viewport_rows = max(0, viewport_rows)
    call normalize_process_columns(state)
    call normalize_filter_state(state)
    call normalize_collapsed_state(state)
    if (state%sort_direction /= TABLE_SORT_DESCENDING) state%sort_direction = TABLE_SORT_ASCENDING
    if (state%row_count <= 0) then
      state%selected_row = 0
      state%scroll_row = 1
      state%selected_pid = 0
      state%selected_start_time = 0_int64
      state%selected_has_children = .false.
      return
    end if

    if (state%selected_row <= 0) state%selected_row = 1
    state%selected_row = max(1, min(state%row_count, state%selected_row))
    max_scroll_row = max(1, state%row_count - max(1, state%viewport_rows) + 1)
    state%scroll_row = max(1, min(max_scroll_row, state%scroll_row))
    if (state%viewport_rows <= 0) return
    if (state%selected_row < state%scroll_row) state%scroll_row = state%selected_row
    if (state%selected_row > state%scroll_row + state%viewport_rows - 1) then
      state%scroll_row = state%selected_row - state%viewport_rows + 1
    end if
    state%scroll_row = max(1, min(max_scroll_row, state%scroll_row))
  end subroutine normalize_process_table_state

  integer function process_sort_key_for_column(state, column) result(sort_key)
    type(process_table_state), intent(in) :: state
    integer, intent(in) :: column

    if (column < 1 .or. column > process_table_column_count(state)) then
      sort_key = 0
    else
      sort_key = process_column_sort_key(process_table_column_id(state, column))
    end if
  end function process_sort_key_for_column

  integer function process_table_column_at(state, rect, target_col) result(column)
    type(process_table_state), intent(in) :: state
    type(widget_rect), intent(in) :: rect
    integer, intent(in) :: target_col
    integer, allocatable :: widths(:)
    integer :: draw_col
    integer :: item

    column = 0
    if (target_col < rect%col .or. target_col >= rect%col + rect%width) return

    call process_table_column_widths(state, rect%width, widths)
    draw_col = rect%col
    do item = 1, size(widths)
      if (target_col >= draw_col .and. target_col < draw_col + widths(item)) then
        column = item
        return
      end if
      draw_col = draw_col + widths(item)
      if (item < size(widths)) then
        if (target_col == draw_col) return
        draw_col = draw_col + 1
      end if
    end do
  end function process_table_column_at

  subroutine process_table_column_widths(state, available_width, widths)
    type(process_table_state), intent(in) :: state
    integer, intent(in) :: available_width
    integer, allocatable, intent(out) :: widths(:)
    type(table_cell), allocatable :: empty_cells(:, :)
    type(table_column), allocatable :: columns(:)
    integer :: column

    allocate(columns(process_table_column_count(state)))
    do column = 1, size(columns)
      call process_column_definition(process_table_column_id(state, column), columns(column))
    end do
    call mark_sort_column(columns, state)
    allocate(empty_cells(0, size(columns)))
    call calculate_column_widths(columns, empty_cells, available_width, TABLE_SEPARATOR_SPACE, widths)
  end subroutine process_table_column_widths

  logical function point_in_rect(rect, row, col) result(inside)
    type(widget_rect), intent(in) :: rect
    integer, intent(in) :: row
    integer, intent(in) :: col

    inside = rect%width > 0 .and. rect%height > 0 .and. &
             row >= rect%row .and. row < rect%row + rect%height .and. &
             col >= rect%col .and. col < rect%col + rect%width
  end function point_in_rect

  subroutine normalize_filter_state(state)
    type(process_table_state), intent(inout) :: state

    state%filter_length = max(0, min(PROCESS_FILTER_LEN, state%filter_length))
    if (state%filter_length <= 0) then
      state%filter_text = ""
    else if (state%filter_length < PROCESS_FILTER_LEN) then
      state%filter_text(state%filter_length + 1:) = ""
    end if
  end subroutine normalize_filter_state

  subroutine normalize_collapsed_state(state)
    type(process_table_state), intent(inout) :: state

    state%collapsed_count = max(0, min(PROCESS_COLLAPSED_CAPACITY, state%collapsed_count))
    if (state%collapsed_count < PROCESS_COLLAPSED_CAPACITY) then
      state%collapsed_pid(state%collapsed_count + 1:) = 0
      state%collapsed_start_time(state%collapsed_count + 1:) = 0_int64
    end if
  end subroutine normalize_collapsed_state

  subroutine remove_collapsed_node(state, node_index)
    type(process_table_state), intent(inout) :: state
    integer, intent(in) :: node_index

    if (node_index < 1 .or. node_index > state%collapsed_count) return
    if (node_index < state%collapsed_count) then
      state%collapsed_pid(node_index:state%collapsed_count - 1) = &
        state%collapsed_pid(node_index + 1:state%collapsed_count)
      state%collapsed_start_time(node_index:state%collapsed_count - 1) = &
        state%collapsed_start_time(node_index + 1:state%collapsed_count)
    end if
    state%collapsed_pid(state%collapsed_count) = 0
    state%collapsed_start_time(state%collapsed_count) = 0_int64
    state%collapsed_count = state%collapsed_count - 1
  end subroutine remove_collapsed_node

  logical function process_table_filter_visible(state) result(visible)
    type(process_table_state), intent(in) :: state

    visible = state%filter_active .or. state%filter_length > 0
  end function process_table_filter_visible

  function process_table_filter_text(state) result(text)
    type(process_table_state), intent(in) :: state
    character(len=:), allocatable :: text
    integer :: length

    length = max(0, min(PROCESS_FILTER_LEN, state%filter_length))
    if (length <= 0) then
      text = ""
    else
      text = state%filter_text(:length)
    end if
  end function process_table_filter_text

  function process_table_content_rect(content, show_filter_bar) result(table_content)
    type(widget_rect), intent(in) :: content
    logical, intent(in) :: show_filter_bar
    type(widget_rect) :: table_content

    table_content = content
    if (show_filter_bar .and. table_content%height > 0) table_content%height = table_content%height - 1
  end function process_table_content_rect

  subroutine render_process_filter_bar(buffer, content, state, active_style, dim_style)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: content
    type(process_table_state), intent(in) :: state
    type(screen_style), intent(in) :: active_style
    type(screen_style), intent(in) :: dim_style
    type(screen_style) :: filter_style

    filter_style = dim_style
    if (state%filter_active) filter_style = active_style
    call render_text(buffer, content_line_rect(content, content%height), process_filter_bar_text(state), filter_style)
  end subroutine render_process_filter_bar

  function process_filter_bar_text(state) result(text)
    type(process_table_state), intent(in) :: state
    character(len=:), allocatable :: text
    character(len=:), allocatable :: cursor

    if (state%filter_active) then
      cursor = "_"
    else
      cursor = ""
    end if
    text = "filter: " // process_table_filter_text(state) // cursor // " (showing " // &
           integer_text(max(0, state%row_count)) // " of " // &
           integer_text(max(0, state%total_row_count)) // " processes)"
  end function process_filter_bar_text

  function ascii_lower(text) result(lower)
    character(len=*), intent(in) :: text
    character(len=:), allocatable :: lower
    integer :: code
    integer :: i

    lower = trim(text)
    do i = 1, len(lower)
      code = iachar(lower(i:i))
      if (code >= iachar("A") .and. code <= iachar("Z")) lower(i:i) = achar(code + 32)
    end do
  end function ascii_lower

  function content_line_rect(content, line_index) result(line)
    type(widget_rect), intent(in) :: content
    integer, intent(in) :: line_index
    type(widget_rect) :: line

    line = widget_rect(0, 0, 0, 0)
    if (line_index < 1 .or. line_index > content%height) return
    line = widget_rect(content%row + line_index - 1, content%col, content%width, 1)
    if (content%width > 2) then
      line%col = content%col + 1
      line%width = content%width - 2
    end if
  end function content_line_rect

  pure real(real64) function clamp_percent(value) result(clamped)
    real(real64), intent(in) :: value

    clamped = max(0.0_real64, min(100.0_real64, value))
  end function clamp_percent

  function integer_text(value) result(text)
    integer, intent(in) :: value
    character(len=:), allocatable :: text
    character(len=32) :: scratch

    write(scratch, '(i0)') value
    text = trim(scratch)
  end function integer_text

end module ftop_process_table
