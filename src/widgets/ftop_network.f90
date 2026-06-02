module ftop_network
  use, intrinsic :: iso_fortran_env, only : real64
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_box, only : BOX_STYLE_ROUNDED, box_content_rect, draw_box
  use ftop_collector, only : collector_snapshot
  use ftop_color, only : &
    COLOR_BRIGHT_WHITE, &
    color_gradient, &
    gradient_blue_cyan, &
    style_from_rgb
  use ftop_net_data, only : NET_STATE_LEN, format_byte_rate, net_connection, process_bandwidth
  use ftop_sparkline, only : render_sparkline
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
  use ftop_text, only : TEXT_ALIGN_CENTER, TEXT_ALIGN_RIGHT, render_text
  use ftop_widgets, only : widget_rect, widget_size
  implicit none
  private

  integer, parameter, public :: NETWORK_SORT_PROTOCOL = 1
  integer, parameter, public :: NETWORK_SORT_LOCAL = 2
  integer, parameter, public :: NETWORK_SORT_REMOTE = 3
  integer, parameter, public :: NETWORK_SORT_STATE = 4
  integer, parameter, public :: NETWORK_SORT_PID = 5
  integer, parameter, public :: NETWORK_SORT_PROCESS = 6
  integer, parameter :: NETWORK_TABLE_COLUMNS = 6
  integer, parameter :: NETWORK_FILTER_COUNT = 6
  integer, parameter :: NETWORK_PROCESS_BANDWIDTH_MAX_ROWS = 5
  character(len=NET_STATE_LEN), parameter :: NETWORK_FILTERS(NETWORK_FILTER_COUNT) = [ &
    character(len=NET_STATE_LEN) :: "", "ESTABLISHED", "LISTEN", "OPEN", "TIME_WAIT", "CLOSE_WAIT" &
  ]

  type, public :: network_table_state
    integer :: selected_row = 1
    integer :: scroll_row = 1
    integer :: sort_key = NETWORK_SORT_STATE
    integer :: sort_direction = TABLE_SORT_ASCENDING
    integer :: row_count = 0
    integer :: total_row_count = 0
    integer :: viewport_rows = 0
    character(len=NET_STATE_LEN) :: state_filter = ""
  end type network_table_state

  public :: network_panel_min_size
  public :: network_table_clear_state_filter
  public :: network_table_cycle_sort_key
  public :: network_table_cycle_state_filter
  public :: network_table_page_delta
  public :: network_table_select_delta
  public :: network_table_sort_direction_label
  public :: network_table_sort_key_label
  public :: network_table_status
  public :: network_table_toggle_sort_direction
  public :: render_network_panel

contains

  function network_panel_min_size() result(size_value)
    type(widget_size) :: size_value

    size_value%width = 28
    size_value%height = 8
  end function network_panel_min_size

  subroutine render_network_panel(buffer, panel, snapshot, border_style, title_style, dim_style, state, expanded)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: panel
    type(collector_snapshot), intent(in) :: snapshot
    type(screen_style), intent(in) :: border_style
    type(screen_style), intent(in) :: title_style
    type(screen_style), intent(in) :: dim_style
    type(network_table_state), intent(inout), optional :: state
    logical, intent(in), optional :: expanded
    type(color_gradient) :: network_gradient
    type(screen_style) :: text_style
    type(widget_rect) :: content
    integer :: interface_index
    integer :: line_index
    logical :: actual_expanded

    network_gradient = gradient_blue_cyan()
    text_style = style_from_rgb(fg=COLOR_BRIGHT_WHITE)
    actual_expanded = .false.
    if (present(expanded)) actual_expanded = expanded

    call draw_box(buffer, panel, BOX_STYLE_ROUNDED, border_style, "Network", title_style)
    content = box_content_rect(panel)
    if (content%height <= 0 .or. content%width <= 0) return

    if (.not. snapshot%network%valid) then
      call render_text(buffer, content_line_rect(content, 1), "network unavailable", dim_style, TEXT_ALIGN_CENTER)
      return
    end if
    if (actual_expanded) then
      if (present(state)) then
        call render_network_table_panel(buffer, content, snapshot, text_style, title_style, dim_style, state)
      else
        call render_network_table_panel(buffer, content, snapshot, text_style, title_style, dim_style)
      end if
      return
    end if

    if (.not. allocated(snapshot%network%interfaces)) then
      call render_text(buffer, content_line_rect(content, 1), "network unavailable", dim_style, TEXT_ALIGN_CENTER)
      return
    end if
    if (size(snapshot%network%interfaces) <= 0) then
      call render_text(buffer, content_line_rect(content, 1), "no network interfaces", dim_style, TEXT_ALIGN_CENTER)
      return
    end if

    call render_text(buffer, content_line_rect(content, 1), network_summary_text(snapshot), text_style)
    line_index = 2
    do interface_index = 1, size(snapshot%network%interfaces)
      if (line_index > content%height) exit
      if (.not. snapshot%network%interfaces(interface_index)%valid) cycle
      call render_text(buffer, content_line_rect(content, line_index), &
                       network_interface_text(snapshot, interface_index), dim_style)
      line_index = line_index + 1
      if (line_index > content%height) cycle
      call render_interface_sparkline(buffer, content_line_rect(content, line_index), snapshot, interface_index, &
                                      network_gradient, dim_style)
      line_index = line_index + 1
    end do

    call render_connection_rows(buffer, content, line_index, snapshot, dim_style)
  end subroutine render_network_panel

  subroutine render_connection_rows(buffer, content, line_index, snapshot, dim_style)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: content
    integer, intent(inout) :: line_index
    type(collector_snapshot), intent(in) :: snapshot
    type(screen_style), intent(in) :: dim_style
    integer :: connection_index

    if (line_index > content%height) return
    if (.not. allocated(snapshot%network%connections)) return
    if (valid_connection_count(snapshot) <= 0) return

    call render_text(buffer, content_line_rect(content, line_index), &
                     "Connections " // integer_text(valid_connection_count(snapshot)), dim_style)
    line_index = line_index + 1
    do connection_index = 1, size(snapshot%network%connections)
      if (line_index > content%height) exit
      if (.not. snapshot%network%connections(connection_index)%valid) cycle
      call render_text(buffer, content_line_rect(content, line_index), &
                       connection_text(snapshot, connection_index), dim_style)
      line_index = line_index + 1
    end do
  end subroutine render_connection_rows

  subroutine render_network_table_panel(buffer, content, snapshot, text_style, title_style, dim_style, state)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: content
    type(collector_snapshot), intent(in) :: snapshot
    type(screen_style), intent(in) :: text_style
    type(screen_style), intent(in) :: title_style
    type(screen_style), intent(in) :: dim_style
    type(network_table_state), intent(inout), optional :: state
    type(network_table_state) :: active_state
    type(net_connection), allocatable :: connections(:)
    type(table_cell), allocatable :: cells(:, :)
    type(table_column) :: columns(NETWORK_TABLE_COLUMNS)
    type(widget_rect) :: table_rect
    integer :: process_row_count
    integer :: table_start_line

    active_state = network_table_state()
    if (present(state)) active_state = state
    call normalize_network_table_state(active_state, active_state%row_count, active_state%viewport_rows)

    call render_text(buffer, content_line_rect(content, 1), network_expanded_summary_text(snapshot, active_state), text_style)
    if (content%height <= 1) then
      if (present(state)) state = active_state
      return
    end if

    process_row_count = network_process_bandwidth_row_count(snapshot, content%height)
    if (process_row_count > 0) call render_process_bandwidth_rows(buffer, content, snapshot, dim_style, process_row_count)
    table_start_line = 2 + process_row_count
    if (content%height < table_start_line) then
      if (present(state)) state = active_state
      return
    end if

    table_rect = widget_rect(content%row + table_start_line - 1, content%col, content%width, &
                             content%height - table_start_line + 1)
    if (content%width > 2) then
      table_rect%col = content%col + 1
      table_rect%width = content%width - 2
    end if

    active_state%total_row_count = valid_connection_count(snapshot)
    connections = visible_connections(snapshot, active_state)
    if (size(connections) <= 0) then
      if (active_state%total_row_count > 0 .and. len_trim(active_state%state_filter) > 0) then
        call render_text(buffer, content_line_rect(content, table_start_line), &
                         "no matching connections", dim_style, TEXT_ALIGN_CENTER)
      else
        call render_text(buffer, content_line_rect(content, table_start_line), &
                         "no network connections", dim_style, TEXT_ALIGN_CENTER)
      end if
      call normalize_network_table_state(active_state, 0, table_viewport_row_count(table_rect, .true.))
      if (present(state)) state = active_state
      return
    end if

    call sort_connections(connections, active_state)
    call network_columns(columns, active_state)
    cells = network_connection_cells(connections)
    call normalize_network_table_state(active_state, size(cells, 1), table_viewport_row_count(table_rect, .true.))
    call render_table(buffer, table_rect, columns, cells, separator=TABLE_SEPARATOR_SPACE, &
                      show_header=.true., striped=.false., style=dim_style, header_style=title_style, &
                      selected_style=title_style, separator_style=dim_style, scroll_row=active_state%scroll_row, &
                      selected_row=active_state%selected_row)
    if (present(state)) state = active_state
  end subroutine render_network_table_panel

  function network_expanded_summary_text(snapshot, state) result(text)
    type(collector_snapshot), intent(in) :: snapshot
    type(network_table_state), intent(in) :: state
    character(len=:), allocatable :: text

    text = network_summary_text(snapshot) // "  Connections " // integer_text(valid_connection_count(snapshot)) // &
           "  filter " // network_state_filter_label(state)
  end function network_expanded_summary_text

  function network_state_filter_label(state) result(text)
    type(network_table_state), intent(in) :: state
    character(len=:), allocatable :: text

    if (len_trim(state%state_filter) > 0) then
      text = trim(state%state_filter)
    else
      text = "all"
    end if
  end function network_state_filter_label

  function visible_connections(snapshot, state) result(connections)
    type(collector_snapshot), intent(in) :: snapshot
    type(network_table_state), intent(in) :: state
    type(net_connection), allocatable :: connections(:)
    integer :: connection_index
    integer :: match_count
    integer :: row

    match_count = 0
    if (.not. allocated(snapshot%network%connections)) then
      allocate(connections(0))
      return
    end if

    do connection_index = 1, size(snapshot%network%connections)
      if (connection_matches(snapshot%network%connections(connection_index), state)) match_count = match_count + 1
    end do

    allocate(connections(match_count))
    row = 0
    do connection_index = 1, size(snapshot%network%connections)
      if (.not. connection_matches(snapshot%network%connections(connection_index), state)) cycle
      row = row + 1
      connections(row) = snapshot%network%connections(connection_index)
    end do
  end function visible_connections

  logical function connection_matches(connection, state) result(matches)
    type(net_connection), intent(in) :: connection
    type(network_table_state), intent(in) :: state

    matches = .false.
    if (.not. connection%valid) return
    if (len_trim(state%state_filter) <= 0) then
      matches = .true.
    else
      matches = trim(connection%state) == trim(state%state_filter)
    end if
  end function connection_matches

  subroutine sort_connections(connections, state)
    type(net_connection), intent(inout) :: connections(:)
    type(network_table_state), intent(in) :: state
    type(net_connection) :: temp
    integer :: i
    integer :: j

    do i = 1, size(connections) - 1
      do j = i + 1, size(connections)
        if (connections_out_of_order(connections(i), connections(j), state)) then
          temp = connections(i)
          connections(i) = connections(j)
          connections(j) = temp
        end if
      end do
    end do
  end subroutine sort_connections

  logical function connections_out_of_order(left, right, state) result(out_of_order)
    type(net_connection), intent(in) :: left
    type(net_connection), intent(in) :: right
    type(network_table_state), intent(in) :: state
    integer :: order

    select case (state%sort_key)
    case (NETWORK_SORT_PROTOCOL)
      order = compare_text(trim(left%protocol), trim(right%protocol))
    case (NETWORK_SORT_LOCAL)
      order = compare_text(endpoint_text(left%local_addr, left%local_port), endpoint_text(right%local_addr, right%local_port))
    case (NETWORK_SORT_REMOTE)
      order = compare_text(endpoint_text(left%remote_addr, left%remote_port), endpoint_text(right%remote_addr, right%remote_port))
    case (NETWORK_SORT_PID)
      order = compare_integer(left%pid, right%pid)
    case (NETWORK_SORT_PROCESS)
      order = compare_text(trim(left%process_name), trim(right%process_name))
    case default
      order = compare_text(trim(left%state), trim(right%state))
    end select
    if (order == 0) order = compare_text(endpoint_text(left%local_addr, left%local_port), &
                                         endpoint_text(right%local_addr, right%local_port))
    if (state%sort_direction == TABLE_SORT_DESCENDING) then
      out_of_order = order < 0
    else
      out_of_order = order > 0
    end if
  end function connections_out_of_order

  integer function compare_text(left, right) result(order)
    character(len=*), intent(in) :: left
    character(len=*), intent(in) :: right

    if (left < right) then
      order = -1
    else if (left > right) then
      order = 1
    else
      order = 0
    end if
  end function compare_text

  integer function compare_integer(left, right) result(order)
    integer, intent(in) :: left
    integer, intent(in) :: right

    if (left < right) then
      order = -1
    else if (left > right) then
      order = 1
    else
      order = 0
    end if
  end function compare_integer

  subroutine network_columns(columns, state)
    type(table_column), intent(out) :: columns(:)
    type(network_table_state), intent(in) :: state

    if (size(columns) < NETWORK_TABLE_COLUMNS) return
    columns(1)%name = "PROTO"
    columns(1)%width_mode = TABLE_WIDTH_FIXED
    columns(1)%width = 5
    columns(2)%name = "LOCAL"
    columns(2)%width_mode = TABLE_WIDTH_WEIGHT
    columns(2)%weight = 2
    columns(3)%name = "REMOTE"
    columns(3)%width_mode = TABLE_WIDTH_WEIGHT
    columns(3)%weight = 2
    columns(4)%name = "STATE"
    columns(4)%width_mode = TABLE_WIDTH_FIXED
    columns(4)%width = 12
    columns(5)%name = "PID"
    columns(5)%width_mode = TABLE_WIDTH_FIXED
    columns(5)%width = 6
    columns(5)%alignment = TEXT_ALIGN_RIGHT
    columns(6)%name = "PROCESS"
    columns(6)%width_mode = TABLE_WIDTH_WEIGHT
    columns(6)%weight = 1
    call mark_network_sort_column(columns, state)
  end subroutine network_columns

  subroutine mark_network_sort_column(columns, state)
    type(table_column), intent(inout) :: columns(:)
    type(network_table_state), intent(in) :: state
    integer :: column

    columns%sort_direction = TABLE_SORT_NONE
    column = network_sort_column(state%sort_key)
    if (column >= 1 .and. column <= size(columns)) columns(column)%sort_direction = state%sort_direction
  end subroutine mark_network_sort_column

  function network_connection_cells(connections) result(cells)
    type(net_connection), intent(in) :: connections(:)
    type(table_cell), allocatable :: cells(:, :)
    integer :: row

    allocate(cells(size(connections), NETWORK_TABLE_COLUMNS))
    do row = 1, size(connections)
      cells(row, 1) = make_table_cell(trim(connections(row)%protocol))
      cells(row, 2) = make_table_cell(endpoint_text(connections(row)%local_addr, connections(row)%local_port))
      cells(row, 3) = make_table_cell(endpoint_text(connections(row)%remote_addr, connections(row)%remote_port))
      cells(row, 4) = make_table_cell(trim(connections(row)%state))
      cells(row, 5) = make_table_cell(integer_text(max(0, connections(row)%pid)))
      cells(row, 6) = make_table_cell(trim(connections(row)%process_name))
    end do
  end function network_connection_cells

  subroutine render_interface_sparkline(buffer, rect, snapshot, interface_index, network_gradient, dim_style)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    type(collector_snapshot), intent(in) :: snapshot
    integer, intent(in) :: interface_index
    type(color_gradient), intent(in) :: network_gradient
    type(screen_style), intent(in) :: dim_style
    real(real64), allocatable :: total_history(:)
    integer :: history_count

    if (rect%width <= 0 .or. rect%height <= 0) return
    history_count = max(0, snapshot%network%interfaces(interface_index)%history_count)
    if (history_count <= 0) then
      call render_text(buffer, rect, "no traffic history", dim_style)
      return
    end if

    allocate(total_history(history_count))
    total_history = snapshot%network%interfaces(interface_index)%rx_history(:history_count) + &
                    snapshot%network%interfaces(interface_index)%tx_history(:history_count)
    call render_sparkline(buffer, rect, real(total_history), gradient=network_gradient, min_value=0.0, style=dim_style)
  end subroutine render_interface_sparkline

  function network_summary_text(snapshot) result(text)
    type(collector_snapshot), intent(in) :: snapshot
    character(len=:), allocatable :: text

    text = "Interfaces " // integer_text(valid_interface_count(snapshot)) // &
           "  rx " // format_byte_rate(total_rx_rate(snapshot)) // &
           "  tx " // format_byte_rate(total_tx_rate(snapshot))
  end function network_summary_text

  function network_interface_text(snapshot, interface_index) result(text)
    type(collector_snapshot), intent(in) :: snapshot
    integer, intent(in) :: interface_index
    character(len=:), allocatable :: text
    character(len=:), allocatable :: state

    state = trim(snapshot%network%interfaces(interface_index)%state)
    if (len(state) <= 0) state = "unknown"
    text = trim(snapshot%network%interfaces(interface_index)%name) // " " // state // &
           "  rx " // format_byte_rate(snapshot%network%interfaces(interface_index)%rx_bytes_per_sec) // &
           "  tx " // format_byte_rate(snapshot%network%interfaces(interface_index)%tx_bytes_per_sec)
  end function network_interface_text

  integer function valid_interface_count(snapshot) result(count)
    type(collector_snapshot), intent(in) :: snapshot
    integer :: interface_index

    count = 0
    if (.not. allocated(snapshot%network%interfaces)) return
    do interface_index = 1, size(snapshot%network%interfaces)
      if (snapshot%network%interfaces(interface_index)%valid) count = count + 1
    end do
  end function valid_interface_count

  integer function valid_connection_count(snapshot) result(count)
    type(collector_snapshot), intent(in) :: snapshot
    integer :: connection_index

    count = 0
    if (.not. allocated(snapshot%network%connections)) return
    do connection_index = 1, size(snapshot%network%connections)
      if (snapshot%network%connections(connection_index)%valid) count = count + 1
    end do
  end function valid_connection_count

  integer function valid_bandwidth_process_count(snapshot) result(count)
    type(collector_snapshot), intent(in) :: snapshot
    integer :: process_index

    count = 0
    if (.not. allocated(snapshot%network%processes)) return
    do process_index = 1, size(snapshot%network%processes)
      if (snapshot%network%processes(process_index)%valid) count = count + 1
    end do
  end function valid_bandwidth_process_count

  integer function network_process_bandwidth_row_count(snapshot, content_height) result(row_count)
    type(collector_snapshot), intent(in) :: snapshot
    integer, intent(in) :: content_height

    row_count = min(valid_bandwidth_process_count(snapshot), NETWORK_PROCESS_BANDWIDTH_MAX_ROWS)
    row_count = min(row_count, max(0, content_height - 4))
  end function network_process_bandwidth_row_count

  subroutine render_process_bandwidth_rows(buffer, content, snapshot, dim_style, row_count)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: content
    type(collector_snapshot), intent(in) :: snapshot
    type(screen_style), intent(in) :: dim_style
    integer, intent(in) :: row_count
    type(process_bandwidth), allocatable :: processes(:)
    integer :: row

    call collect_sorted_process_bandwidth(snapshot, processes)
    do row = 1, min(row_count, size(processes))
      call render_text(buffer, content_line_rect(content, row + 1), network_process_bandwidth_text(processes(row)), dim_style)
    end do
  end subroutine render_process_bandwidth_rows

  subroutine collect_sorted_process_bandwidth(snapshot, processes)
    type(collector_snapshot), intent(in) :: snapshot
    type(process_bandwidth), allocatable, intent(out) :: processes(:)
    integer :: match_count
    integer :: process_index
    integer :: row

    match_count = 0
    if (.not. allocated(snapshot%network%processes)) then
      allocate(processes(0))
      return
    end if

    do process_index = 1, size(snapshot%network%processes)
      if (snapshot%network%processes(process_index)%valid) match_count = match_count + 1
    end do

    allocate(processes(match_count))
    row = 0
    do process_index = 1, size(snapshot%network%processes)
      if (.not. snapshot%network%processes(process_index)%valid) cycle
      row = row + 1
      processes(row) = snapshot%network%processes(process_index)
    end do
    call sort_process_bandwidth(processes)
  end subroutine collect_sorted_process_bandwidth

  subroutine sort_process_bandwidth(processes)
    type(process_bandwidth), intent(inout) :: processes(:)
    type(process_bandwidth) :: temp
    integer :: i
    integer :: j

    do i = 1, size(processes) - 1
      do j = i + 1, size(processes)
        if (process_bandwidth_rate(processes(i)) < process_bandwidth_rate(processes(j))) then
          temp = processes(i)
          processes(i) = processes(j)
          processes(j) = temp
        end if
      end do
    end do
  end subroutine sort_process_bandwidth

  function network_process_bandwidth_text(process) result(text)
    type(process_bandwidth), intent(in) :: process
    character(len=:), allocatable :: text
    character(len=:), allocatable :: label

    if (len_trim(process%process_name) > 0) then
      label = trim(process%process_name)
    else
      label = "pid " // integer_text(max(0, process%pid))
    end if
    text = "Process " // label // &
           "  rx " // format_byte_rate(process%rx_bytes_per_sec) // &
           "  tx " // format_byte_rate(process%tx_bytes_per_sec)
  end function network_process_bandwidth_text

  real(real64) function process_bandwidth_rate(process) result(rate)
    type(process_bandwidth), intent(in) :: process

    rate = max(0.0_real64, process%rx_bytes_per_sec) + max(0.0_real64, process%tx_bytes_per_sec)
  end function process_bandwidth_rate

  function connection_text(snapshot, connection_index) result(text)
    type(collector_snapshot), intent(in) :: snapshot
    integer, intent(in) :: connection_index
    character(len=:), allocatable :: text
    character(len=:), allocatable :: owner

    owner = connection_owner_text(snapshot, connection_index)
    text = trim(snapshot%network%connections(connection_index)%protocol) // " " // &
           endpoint_text(snapshot%network%connections(connection_index)%local_addr, &
                         snapshot%network%connections(connection_index)%local_port) // &
           " -> " // endpoint_text(snapshot%network%connections(connection_index)%remote_addr, &
                                    snapshot%network%connections(connection_index)%remote_port) // &
           " " // trim(snapshot%network%connections(connection_index)%state) // owner
  end function connection_text

  function endpoint_text(address, port) result(text)
    character(len=*), intent(in) :: address
    integer, intent(in) :: port
    character(len=:), allocatable :: text

    if (index(trim(address), ":") > 0) then
      text = "[" // trim(address) // "]:" // integer_text(max(0, port))
    else
      text = trim(address) // ":" // integer_text(max(0, port))
    end if
  end function endpoint_text

  function connection_owner_text(snapshot, connection_index) result(text)
    type(collector_snapshot), intent(in) :: snapshot
    integer, intent(in) :: connection_index
    character(len=:), allocatable :: text

    text = ""
    if (snapshot%network%connections(connection_index)%pid <= 0) return
    text = " pid " // integer_text(snapshot%network%connections(connection_index)%pid)
    if (len_trim(snapshot%network%connections(connection_index)%process_name) > 0) then
      text = text // " " // trim(snapshot%network%connections(connection_index)%process_name)
    end if
  end function connection_owner_text

  subroutine network_table_select_delta(state, delta)
    type(network_table_state), intent(inout) :: state
    integer, intent(in) :: delta

    state%selected_row = state%selected_row + delta
    call normalize_network_table_state(state, state%row_count, state%viewport_rows)
  end subroutine network_table_select_delta

  subroutine network_table_page_delta(state, delta_pages)
    type(network_table_state), intent(inout) :: state
    integer, intent(in) :: delta_pages
    integer :: step

    step = max(1, state%viewport_rows)
    call network_table_select_delta(state, delta_pages * step)
  end subroutine network_table_page_delta

  subroutine network_table_cycle_sort_key(state, direction)
    type(network_table_state), intent(inout) :: state
    integer, intent(in) :: direction

    state%sort_key = modulo(network_sort_column(state%sort_key) - 1 + direction, NETWORK_TABLE_COLUMNS) + 1
  end subroutine network_table_cycle_sort_key

  subroutine network_table_toggle_sort_direction(state)
    type(network_table_state), intent(inout) :: state

    if (state%sort_direction == TABLE_SORT_DESCENDING) then
      state%sort_direction = TABLE_SORT_ASCENDING
    else
      state%sort_direction = TABLE_SORT_DESCENDING
    end if
  end subroutine network_table_toggle_sort_direction

  subroutine network_table_cycle_state_filter(state)
    type(network_table_state), intent(inout) :: state
    integer :: filter_index

    filter_index = network_filter_index(state%state_filter)
    filter_index = modulo(filter_index, NETWORK_FILTER_COUNT) + 1
    state%state_filter = NETWORK_FILTERS(filter_index)
    state%selected_row = 1
    state%scroll_row = 1
    call normalize_network_table_state(state, state%row_count, state%viewport_rows)
  end subroutine network_table_cycle_state_filter

  subroutine network_table_clear_state_filter(state)
    type(network_table_state), intent(inout) :: state

    state%state_filter = ""
    state%selected_row = 1
    state%scroll_row = 1
    call normalize_network_table_state(state, state%row_count, state%viewport_rows)
  end subroutine network_table_clear_state_filter

  function network_table_status(state) result(text)
    type(network_table_state), intent(in) :: state
    character(len=:), allocatable :: text

    text = "network row " // integer_text(max(0, state%selected_row)) // "/" // &
           integer_text(max(0, state%row_count)) // " sort " // network_table_sort_key_label(state) // " " // &
           network_table_sort_direction_label(state) // " filter " // network_state_filter_label(state)
    if (state%total_row_count > state%row_count .or. len_trim(state%state_filter) > 0) then
      text = text // " showing " // integer_text(max(0, state%row_count)) // "/" // &
             integer_text(max(0, state%total_row_count))
    end if
  end function network_table_status

  function network_table_sort_key_label(state) result(label)
    type(network_table_state), intent(in) :: state
    character(len=:), allocatable :: label

    select case (state%sort_key)
    case (NETWORK_SORT_PROTOCOL)
      label = "proto"
    case (NETWORK_SORT_LOCAL)
      label = "local"
    case (NETWORK_SORT_REMOTE)
      label = "remote"
    case (NETWORK_SORT_PID)
      label = "pid"
    case (NETWORK_SORT_PROCESS)
      label = "process"
    case default
      label = "state"
    end select
  end function network_table_sort_key_label

  function network_table_sort_direction_label(state) result(label)
    type(network_table_state), intent(in) :: state
    character(len=:), allocatable :: label

    if (state%sort_direction == TABLE_SORT_DESCENDING) then
      label = "desc"
    else
      label = "asc"
    end if
  end function network_table_sort_direction_label

  subroutine normalize_network_table_state(state, row_count, viewport_rows)
    type(network_table_state), intent(inout) :: state
    integer, intent(in) :: row_count
    integer, intent(in) :: viewport_rows
    integer :: max_scroll_row

    state%row_count = max(0, row_count)
    state%total_row_count = max(state%row_count, state%total_row_count)
    state%viewport_rows = max(0, viewport_rows)
    if (state%sort_key < NETWORK_SORT_PROTOCOL .or. state%sort_key > NETWORK_SORT_PROCESS) state%sort_key = NETWORK_SORT_STATE
    if (state%sort_direction /= TABLE_SORT_DESCENDING) state%sort_direction = TABLE_SORT_ASCENDING
    if (network_filter_index(state%state_filter) <= 0) state%state_filter = ""
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
  end subroutine normalize_network_table_state

  integer function network_sort_column(sort_key) result(column)
    integer, intent(in) :: sort_key

    if (sort_key >= NETWORK_SORT_PROTOCOL .and. sort_key <= NETWORK_SORT_PROCESS) then
      column = sort_key
    else
      column = NETWORK_SORT_STATE
    end if
  end function network_sort_column

  integer function network_filter_index(filter) result(index_value)
    character(len=*), intent(in) :: filter
    integer :: index

    index_value = 0
    do index = 1, NETWORK_FILTER_COUNT
      if (trim(filter) == trim(NETWORK_FILTERS(index))) then
        index_value = index
        return
      end if
    end do
  end function network_filter_index

  real(real64) function total_rx_rate(snapshot) result(rate)
    type(collector_snapshot), intent(in) :: snapshot
    integer :: interface_index

    rate = 0.0_real64
    if (.not. allocated(snapshot%network%interfaces)) return
    do interface_index = 1, size(snapshot%network%interfaces)
      if (snapshot%network%interfaces(interface_index)%valid) then
        rate = rate + max(0.0_real64, snapshot%network%interfaces(interface_index)%rx_bytes_per_sec)
      end if
    end do
  end function total_rx_rate

  real(real64) function total_tx_rate(snapshot) result(rate)
    type(collector_snapshot), intent(in) :: snapshot
    integer :: interface_index

    rate = 0.0_real64
    if (.not. allocated(snapshot%network%interfaces)) return
    do interface_index = 1, size(snapshot%network%interfaces)
      if (snapshot%network%interfaces(interface_index)%valid) then
        rate = rate + max(0.0_real64, snapshot%network%interfaces(interface_index)%tx_bytes_per_sec)
      end if
    end do
  end function total_tx_rate

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

  function integer_text(value) result(text)
    integer, intent(in) :: value
    character(len=:), allocatable :: text
    character(len=32) :: scratch

    write(scratch, '(i0)') value
    text = trim(scratch)
  end function integer_text

end module ftop_network
