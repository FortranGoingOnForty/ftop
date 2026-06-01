module ftop_process_table
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_box, only : BOX_STYLE_ROUNDED, box_content_rect, draw_box
  use ftop_collector, only : collector_snapshot
  use ftop_proc_data, only : &
    PROCESS_SORT_COMMAND, &
    PROCESS_SORT_CPU, &
    PROCESS_SORT_MEMORY, &
    PROCESS_SORT_PID, &
    PROCESS_SORT_RSS, &
    PROCESS_SORT_USER, &
    build_process_tree, &
    process_display_command, &
    process_info, &
    process_state_label, &
    process_table, &
    process_user_label, &
    sort_process_table
  use ftop_table, only : &
    TABLE_SORT_ASCENDING, &
    TABLE_SORT_DESCENDING, &
    TABLE_SORT_NONE, &
    TABLE_SEPARATOR_SPACE, &
    TABLE_WIDTH_FIXED, &
    TABLE_WIDTH_WEIGHT, &
    make_table_cell, &
    render_table, &
    table_cell, &
    table_column, &
    table_viewport_row_count
  use ftop_text, only : TEXT_ALIGN_RIGHT, format_bytes, format_percent, render_text
  use ftop_widgets, only : widget_rect, widget_size
  implicit none
  private

  integer, parameter :: PROCESS_TABLE_COLUMNS = 7
  integer, parameter :: PROCESS_SORT_KEY_COUNT = 6
  integer, parameter, public :: PROCESS_FILTER_LEN = 96

  type, public :: process_table_state
    integer :: selected_row = 1
    integer :: scroll_row = 1
    integer :: sort_key = PROCESS_SORT_PID
    integer :: sort_direction = TABLE_SORT_ASCENDING
    integer :: row_count = 0
    integer :: total_row_count = 0
    integer :: viewport_rows = 0
    integer :: filter_length = 0
    character(len=PROCESS_FILTER_LEN) :: filter_text = ""
    logical :: filter_active = .false.
    logical :: tree_view = .true.
  end type process_table_state

  public :: process_panel_min_size
  public :: process_table_append_filter_text
  public :: process_table_begin_filter
  public :: process_table_clear_filter
  public :: process_table_cycle_sort_key
  public :: process_table_delete_filter_char
  public :: process_table_page_delta
  public :: process_table_select_delta
  public :: process_table_sort_direction_label
  public :: process_table_sort_key_label
  public :: process_table_status
  public :: process_table_finish_filter
  public :: process_table_toggle_sort_direction
  public :: process_table_toggle_tree
  public :: render_process_panel

contains

  function process_panel_min_size() result(size_value)
    type(widget_size) :: size_value

    size_value%width = 44
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
    type(widget_rect) :: content
    type(widget_rect) :: table_content
    logical :: show_filter_bar

    active_state = process_table_state()
    if (present(state)) active_state = state
    call normalize_filter_state(active_state)
    call draw_box(buffer, panel, BOX_STYLE_ROUNDED, border_style, "Processes", title_style)
    content = box_content_rect(panel)
    if (content%height <= 0 .or. content%width <= 0) return
    show_filter_bar = process_table_filter_visible(active_state)
    table_content = process_table_content_rect(content, show_filter_bar)

    if (.not. snapshot%processes%valid) then
      call render_text(buffer, content_line_rect(table_content, 1), "processes unavailable", dim_style)
      if (present(state)) then
        active_state%total_row_count = 0
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
        call normalize_process_table_state(active_state, 0, 0)
        state = active_state
      end if
      if (show_filter_bar) call render_process_filter_bar(buffer, content, active_state, title_style, dim_style)
      return
    end if

    columns = process_columns(active_state)
    sorted_processes = snapshot%processes
    active_state%total_row_count = count_valid_processes(sorted_processes)
    call sort_process_table(sorted_processes, active_state%sort_key, &
                            descending=active_state%sort_direction == TABLE_SORT_DESCENDING)
    call filter_process_table(sorted_processes, active_state)
    if (active_state%tree_view) call build_process_tree(sorted_processes)
    cells = process_cells(sorted_processes)
    if (size(cells, 1) <= 0) then
      if (active_state%filter_length > 0) then
        call render_text(buffer, content_line_rect(table_content, 1), "no matching processes", dim_style)
      else
        call render_text(buffer, content_line_rect(table_content, 1), "no processes", dim_style)
      end if
      if (present(state)) then
        call normalize_process_table_state(active_state, 0, table_viewport_row_count(table_content, .true.))
        state = active_state
      end if
      if (show_filter_bar) call render_process_filter_bar(buffer, content, active_state, title_style, dim_style)
      return
    end if

    call normalize_process_table_state(active_state, size(cells, 1), table_viewport_row_count(table_content, .true.))

    call render_table(buffer, table_content, columns, cells, separator=TABLE_SEPARATOR_SPACE, &
                      show_header=.true., striped=.false., style=dim_style, &
                      header_style=title_style, selected_style=title_style, separator_style=dim_style, &
                      scroll_row=active_state%scroll_row, selected_row=active_state%selected_row)
    if (show_filter_bar) call render_process_filter_bar(buffer, content, active_state, title_style, dim_style)
    if (present(state)) state = active_state
  end subroutine render_process_panel

  function process_columns(state) result(columns)
    type(process_table_state), intent(in) :: state
    type(table_column), allocatable :: columns(:)

    allocate(columns(PROCESS_TABLE_COLUMNS))
    columns(1)%name = "PID"
    columns(1)%width_mode = TABLE_WIDTH_FIXED
    columns(1)%width = 6
    columns(1)%alignment = TEXT_ALIGN_RIGHT
    columns(2)%name = "USER"
    columns(2)%width_mode = TABLE_WIDTH_FIXED
    columns(2)%width = 8
    columns(3)%name = "CPU%"
    columns(3)%width_mode = TABLE_WIDTH_FIXED
    columns(3)%width = 6
    columns(3)%alignment = TEXT_ALIGN_RIGHT
    columns(4)%name = "MEM%"
    columns(4)%width_mode = TABLE_WIDTH_FIXED
    columns(4)%width = 6
    columns(4)%alignment = TEXT_ALIGN_RIGHT
    columns(5)%name = "RSS"
    columns(5)%width_mode = TABLE_WIDTH_FIXED
    columns(5)%width = 8
    columns(5)%alignment = TEXT_ALIGN_RIGHT
    columns(6)%name = "S"
    columns(6)%width_mode = TABLE_WIDTH_FIXED
    columns(6)%width = 2
    columns(7)%name = "COMMAND"
    columns(7)%width_mode = TABLE_WIDTH_WEIGHT
    columns(7)%weight = 1
    call mark_sort_column(columns, state)
  end function process_columns

  subroutine mark_sort_column(columns, state)
    type(table_column), intent(inout) :: columns(:)
    type(process_table_state), intent(in) :: state
    integer :: sort_column

    columns%sort_direction = TABLE_SORT_NONE
    sort_column = process_sort_column(state%sort_key)
    if (sort_column >= 1 .and. sort_column <= size(columns)) columns(sort_column)%sort_direction = state%sort_direction
  end subroutine mark_sort_column

  integer function process_sort_column(sort_key) result(column)
    integer, intent(in) :: sort_key

    select case (sort_key)
    case (PROCESS_SORT_USER)
      column = 2
    case (PROCESS_SORT_CPU)
      column = 3
    case (PROCESS_SORT_MEMORY)
      column = 4
    case (PROCESS_SORT_RSS)
      column = 5
    case (PROCESS_SORT_COMMAND)
      column = 7
    case default
      column = 1
    end select
  end function process_sort_column

  function process_cells(table) result(cells)
    type(process_table), intent(in) :: table
    type(table_cell), allocatable :: cells(:, :)
    integer :: process_index
    integer :: row
    integer :: valid_count

    valid_count = 0
    do process_index = 1, size(table%items)
      if (table%items(process_index)%valid) valid_count = valid_count + 1
    end do

    allocate(cells(valid_count, PROCESS_TABLE_COLUMNS))
    row = 0
    do process_index = 1, size(table%items)
      if (.not. table%items(process_index)%valid) cycle
      row = row + 1
      call fill_process_row(cells, row, table%items(process_index))
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

  subroutine fill_process_row(cells, row_index, process)
    type(table_cell), intent(inout) :: cells(:, :)
    integer, intent(in) :: row_index
    type(process_info), intent(in) :: process

    if (size(cells, 2) < PROCESS_TABLE_COLUMNS) return
    if (row_index < 1 .or. row_index > size(cells, 1)) return
    cells(row_index, 1) = make_table_cell(integer_text(process%pid))
    cells(row_index, 2) = make_table_cell(process_user_label(process))
    cells(row_index, 3) = make_table_cell(format_percent(real(clamp_percent(process%cpu_percent))))
    cells(row_index, 4) = make_table_cell(format_percent(real(clamp_percent(process%mem_percent))))
    cells(row_index, 5) = make_table_cell(format_bytes(max(0_int64, process%mem_rss_bytes)))
    cells(row_index, 6) = make_table_cell(process_state_label(process))
    cells(row_index, 7) = make_table_cell(process_tree_display_command(process))
  end subroutine fill_process_row

  function process_tree_display_command(process) result(text)
    type(process_info), intent(in) :: process
    character(len=:), allocatable :: text

    if (len_trim(process%tree_prefix) > 0) then
      text = trim(process%tree_prefix) // " " // process_display_command(process)
    else
      text = process_display_command(process)
    end if
  end function process_tree_display_command

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

  subroutine process_table_cycle_sort_key(state, direction)
    type(process_table_state), intent(inout) :: state
    integer, intent(in) :: direction
    integer :: key_index

    key_index = process_sort_key_index(state%sort_key)
    key_index = modulo(key_index - 1 + direction, PROCESS_SORT_KEY_COUNT) + 1
    state%sort_key = process_sort_key_at(key_index)
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
    if (process_table_filter_visible(state)) then
      text = text // " filter " // process_table_filter_text(state) // " showing " // &
             integer_text(max(0, state%row_count)) // "/" // integer_text(max(0, state%total_row_count))
    end if
  end function process_table_status

  function process_table_sort_key_label(state) result(label)
    type(process_table_state), intent(in) :: state
    character(len=:), allocatable :: label

    select case (state%sort_key)
    case (PROCESS_SORT_USER)
      label = "user"
    case (PROCESS_SORT_CPU)
      label = "cpu"
    case (PROCESS_SORT_MEMORY)
      label = "mem"
    case (PROCESS_SORT_RSS)
      label = "rss"
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

  subroutine normalize_process_table_state(state, row_count, viewport_rows)
    type(process_table_state), intent(inout) :: state
    integer, intent(in) :: row_count
    integer, intent(in) :: viewport_rows
    integer :: max_scroll_row

    state%row_count = max(0, row_count)
    state%total_row_count = max(state%row_count, state%total_row_count)
    state%viewport_rows = max(0, viewport_rows)
    call normalize_filter_state(state)
    if (state%sort_direction /= TABLE_SORT_DESCENDING) state%sort_direction = TABLE_SORT_ASCENDING
    if (state%row_count <= 0) then
      state%selected_row = 0
      state%scroll_row = 1
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

  integer function process_sort_key_index(sort_key) result(key_index)
    integer, intent(in) :: sort_key
    integer :: i

    key_index = 1
    do i = 1, PROCESS_SORT_KEY_COUNT
      if (process_sort_key_at(i) == sort_key) then
        key_index = i
        return
      end if
    end do
  end function process_sort_key_index

  integer function process_sort_key_at(key_index) result(sort_key)
    integer, intent(in) :: key_index

    select case (key_index)
    case (2)
      sort_key = PROCESS_SORT_USER
    case (3)
      sort_key = PROCESS_SORT_CPU
    case (4)
      sort_key = PROCESS_SORT_MEMORY
    case (5)
      sort_key = PROCESS_SORT_RSS
    case (6)
      sort_key = PROCESS_SORT_COMMAND
    case default
      sort_key = PROCESS_SORT_PID
    end select
  end function process_sort_key_at

  subroutine normalize_filter_state(state)
    type(process_table_state), intent(inout) :: state

    state%filter_length = max(0, min(PROCESS_FILTER_LEN, state%filter_length))
    if (state%filter_length <= 0) then
      state%filter_text = ""
    else if (state%filter_length < PROCESS_FILTER_LEN) then
      state%filter_text(state%filter_length + 1:) = ""
    end if
  end subroutine normalize_filter_state

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
