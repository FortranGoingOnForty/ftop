module ftop_disk
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_box, only : BOX_STYLE_ROUNDED, box_content_rect, draw_box
  use ftop_collector, only : collector_snapshot
  use ftop_color, only : COLOR_BRIGHT_WHITE, gradient_green_yellow_red, style_from_rgb
  use ftop_disk_data, only : disk_latency_info, disk_table, filesystem_info, filesystem_usage_percent, real_filesystem_count
  use ftop_meter, only : METER_FILL_SHADED, render_meter
  use ftop_table, only : &
    TABLE_SEPARATOR_SPACE, &
    TABLE_WIDTH_AUTO, &
    TABLE_WIDTH_FIXED, &
    TABLE_WIDTH_WEIGHT, &
    make_table_cell, &
    render_table, &
    table_cell, &
    table_column, &
    table_viewport_row_count
  use ftop_text, only : &
    TEXT_ALIGN_CENTER, &
    TEXT_ALIGN_RIGHT, &
    format_bytes, &
    format_percent, &
    horizontal_text_state, &
    render_horizontal_text
  use ftop_widgets, only : widget_rect, widget_size
  implicit none
  private

  integer, parameter :: DISK_TABLE_COLUMNS = 5
  integer, parameter :: COMPACT_ROW_GAP_WIDTH = 4
  integer, parameter :: COMPACT_PERCENT_WIDTH = 6
  real(real64), parameter :: RATE_KIB = 1024.0_real64
  real(real64), parameter :: RATE_MIB = RATE_KIB * 1024.0_real64
  real(real64), parameter :: RATE_GIB = RATE_MIB * 1024.0_real64

  type, public :: disk_table_state
    integer :: selected_row = 1
    integer :: scroll_row = 1
    integer :: row_count = 0
    integer :: viewport_rows = 0
  end type disk_table_state

  public :: disk_panel_min_size
  public :: disk_table_page_delta
  public :: disk_table_select_delta
  public :: disk_table_status
  public :: render_disk_panel

contains

  function disk_panel_min_size() result(size_value)
    type(widget_size) :: size_value

    size_value%width = 28
    size_value%height = 6
  end function disk_panel_min_size

  subroutine render_disk_panel(buffer, panel, snapshot, border_style, title_style, dim_style, state, expanded, &
                               horizontal_state, table_active)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: panel
    type(collector_snapshot), intent(in) :: snapshot
    type(screen_style), intent(in) :: border_style
    type(screen_style), intent(in) :: title_style
    type(screen_style), intent(in) :: dim_style
    type(disk_table_state), intent(inout), optional :: state
    logical, intent(in), optional :: expanded
    type(horizontal_text_state), intent(inout), optional :: horizontal_state
    logical, intent(in), optional :: table_active
    type(screen_style) :: text_style
    type(widget_rect) :: content
    logical :: actual_expanded
    logical :: actual_table_active

    text_style = style_from_rgb(fg=COLOR_BRIGHT_WHITE)
    actual_expanded = .false.
    if (present(expanded)) actual_expanded = expanded
    actual_table_active = actual_expanded
    if (present(table_active)) actual_table_active = actual_table_active .or. table_active

    call draw_box(buffer, panel, BOX_STYLE_ROUNDED, border_style, "Disk", title_style)
    content = box_content_rect(panel)
    if (content%height <= 0 .or. content%width <= 0) return

    if (.not. snapshot%disk%valid) then
      if (present(state)) call clear_disk_table_state(state)
      call render_horizontal_text(buffer, content_line_rect(content, 1), "disk unavailable", dim_style, horizontal_state, &
                                  TEXT_ALIGN_CENTER)
      return
    end if

    if (actual_expanded) then
      if (present(state)) then
        call render_disk_table_panel(buffer, content, snapshot%disk, text_style, title_style, dim_style, state, &
                                     horizontal_state, actual_table_active)
      else
        call render_disk_table_panel(buffer, content, snapshot%disk, text_style, title_style, dim_style, &
                                     horizontal_state=horizontal_state, table_active=actual_table_active)
      end if
    else
      if (present(state)) then
        call render_disk_compact_panel(buffer, content, snapshot%disk, text_style, title_style, dim_style, state, &
                                       horizontal_state, actual_table_active)
      else
        call render_disk_compact_panel(buffer, content, snapshot%disk, text_style, title_style, dim_style, &
                                       horizontal_state=horizontal_state, table_active=actual_table_active)
      end if
    end if
  end subroutine render_disk_panel

  subroutine render_disk_compact_panel(buffer, content, table, text_style, selected_style, dim_style, state, horizontal_state, &
                                       table_active)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: content
    type(disk_table), intent(in) :: table
    type(screen_style), intent(in) :: text_style
    type(screen_style), intent(in) :: selected_style
    type(screen_style), intent(in) :: dim_style
    type(disk_table_state), intent(inout), optional :: state
    type(horizontal_text_state), intent(inout), optional :: horizontal_state
    logical, intent(in), optional :: table_active
    type(filesystem_info), allocatable :: filesystems(:)
    type(screen_style) :: row_style
    character(len=:), allocatable :: io_text
    integer :: actual_index
    integer :: line_index
    integer :: preview_row
    integer :: start_index
    integer :: viewport_rows
    logical :: stateful
    logical :: active_rows

    if (.not. allocated(table%filesystems)) then
      if (present(state)) call clear_disk_table_state(state)
      call render_horizontal_text(buffer, content_line_rect(content, 1), "no filesystems", dim_style, horizontal_state, &
                                  TEXT_ALIGN_CENTER)
      return
    end if

    filesystems = sorted_filesystems(table)
    if (size(filesystems) <= 0) then
      if (present(state)) call clear_disk_table_state(state)
      call render_horizontal_text(buffer, content_line_rect(content, 1), "no filesystems", dim_style, horizontal_state, &
                                  TEXT_ALIGN_CENTER)
      return
    end if

    stateful = present(state)
    active_rows = .false.
    if (present(table_active)) active_rows = table_active
    viewport_rows = max(0, content%height - 2)
    if (stateful) then
      state%row_count = size(filesystems)
      state%viewport_rows = viewport_rows
      call normalize_disk_table_state(state)
      start_index = state%scroll_row
    else
      start_index = 1
    end if

    call render_horizontal_text(buffer, content_line_rect(content, 1), disk_summary_text(table, filesystems), text_style, &
                                horizontal_state)
    call render_disk_usage_strip(buffer, content_line_rect(content, 2), filesystems, dim_style)
    line_index = 3
    io_text = disk_io_summary_text(table)
    if (len_trim(io_text) > 0 .and. line_index <= content%height) then
      call render_horizontal_text(buffer, content_line_rect(content, line_index), io_text, dim_style, horizontal_state)
      line_index = line_index + 1
    end if
    do preview_row = 1, max(0, content%height - line_index + 1)
      actual_index = start_index + preview_row - 1
      if (actual_index > size(filesystems)) exit
      row_style = dim_style
      if (stateful .and. actual_index == state%selected_row) row_style = selected_style
      call render_compact_filesystem_row(buffer, content_line_rect(content, line_index), &
                                         filesystems(actual_index), row_style, horizontal_state, &
                                         stateful .and. active_rows .and. actual_index == state%selected_row)
      line_index = line_index + 1
    end do
  end subroutine render_disk_compact_panel

  subroutine render_disk_usage_strip(buffer, rect, filesystems, dim_style)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    type(filesystem_info), intent(in) :: filesystems(:)
    type(screen_style), intent(in) :: dim_style
    real(real64) :: hottest

    if (rect%width <= 0 .or. rect%height <= 0) return
    hottest = hottest_usage_percent(filesystems)
    call render_meter(buffer, rect, real(max(0.0_real64, min(1.0_real64, hottest / 100.0_real64))), &
                      gradient=gradient_green_yellow_red(), fill_mode=METER_FILL_SHADED, &
                      empty_style=dim_style, label_style=dim_style)
  end subroutine render_disk_usage_strip

  subroutine render_disk_table_panel(buffer, content, table, text_style, title_style, dim_style, state, horizontal_state, &
                                     table_active)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: content
    type(disk_table), intent(in) :: table
    type(screen_style), intent(in) :: text_style
    type(screen_style), intent(in) :: title_style
    type(screen_style), intent(in) :: dim_style
    type(disk_table_state), intent(inout), optional :: state
    type(horizontal_text_state), intent(inout), optional :: horizontal_state
    logical, intent(in), optional :: table_active
    type(filesystem_info), allocatable :: filesystems(:)
    type(table_cell), allocatable :: cells(:, :)
    type(table_column) :: columns(DISK_TABLE_COLUMNS)
    type(disk_table_state) :: active_state
    type(widget_rect) :: table_rect
    character(len=:), allocatable :: io_text
    integer :: table_start_line
    logical :: active_rows

    filesystems = sorted_filesystems(table)
    active_rows = .false.
    if (present(table_active)) active_rows = table_active
    call render_horizontal_text(buffer, content_line_rect(content, 1), disk_summary_text(table, filesystems), text_style, &
                                horizontal_state)
    if (content%height <= 1) return
    table_start_line = 2
    io_text = disk_io_summary_text(table)
    if (len_trim(io_text) > 0) then
      call render_horizontal_text(buffer, content_line_rect(content, 2), io_text, dim_style, horizontal_state)
      table_start_line = 3
    end if
    if (content%height < table_start_line) return
    if (size(filesystems) <= 0) then
      if (present(state)) call clear_disk_table_state(state)
      call render_horizontal_text(buffer, content_line_rect(content, table_start_line), "no filesystems", dim_style, &
                                  horizontal_state, TEXT_ALIGN_CENTER)
      return
    end if

    active_state = disk_table_state()
    if (present(state)) active_state = state
    table_rect = widget_rect(content%row + table_start_line - 1, content%col, content%width, &
                             content%height - table_start_line + 1)
    active_state%row_count = size(filesystems)
    active_state%viewport_rows = table_viewport_row_count(table_rect, .true.)
    call normalize_disk_table_state(active_state)

    call disk_columns(columns)
    cells = filesystem_cells(filesystems)
    call render_table(buffer, table_rect, columns, cells, separator=TABLE_SEPARATOR_SPACE, show_header=.true., &
                      striped=.false., style=dim_style, header_style=title_style, selected_style=title_style, &
                      separator_style=dim_style, scroll_row=active_state%scroll_row, &
                      selected_row=active_state%selected_row, horizontal_state=horizontal_state, &
                      active_selected_row=active_rows)
    if (present(state)) state = active_state
  end subroutine render_disk_table_panel

  subroutine disk_columns(columns)
    type(table_column), intent(out) :: columns(:)

    if (size(columns) < DISK_TABLE_COLUMNS) return
    columns(1)%name = "MOUNT"
    columns(1)%width_mode = TABLE_WIDTH_WEIGHT
    columns(1)%weight = 2
    columns(2)%name = "USE"
    columns(2)%width_mode = TABLE_WIDTH_FIXED
    columns(2)%width = 6
    columns(2)%alignment = TEXT_ALIGN_RIGHT
    columns(3)%name = "USED"
    columns(3)%width_mode = TABLE_WIDTH_AUTO
    columns(3)%alignment = TEXT_ALIGN_RIGHT
    columns(4)%name = "AVAIL"
    columns(4)%width_mode = TABLE_WIDTH_AUTO
    columns(4)%alignment = TEXT_ALIGN_RIGHT
    columns(5)%name = "TYPE"
    columns(5)%width_mode = TABLE_WIDTH_AUTO
  end subroutine disk_columns

  function filesystem_cells(filesystems) result(cells)
    type(filesystem_info), intent(in) :: filesystems(:)
    type(table_cell), allocatable :: cells(:, :)
    integer :: row

    allocate(cells(size(filesystems), DISK_TABLE_COLUMNS))
    do row = 1, size(filesystems)
      cells(row, 1) = make_table_cell(trim(filesystems(row)%mountpoint))
      cells(row, 2) = make_table_cell(format_percent(real(filesystem_usage_percent(filesystems(row)))))
      cells(row, 3) = make_table_cell(format_bytes(filesystems(row)%used_bytes))
      cells(row, 4) = make_table_cell(format_bytes(filesystems(row)%available_bytes))
      cells(row, 5) = make_table_cell(trim(filesystems(row)%fstype))
    end do
  end function filesystem_cells

  function disk_summary_text(table, filesystems) result(text)
    type(disk_table), intent(in) :: table
    type(filesystem_info), intent(in) :: filesystems(:)
    character(len=:), allocatable :: text

    text = "Filesystems " // integer_text(real_filesystem_count(table))
    if (size(filesystems) > 0) then
      text = text // " hottest " // trim(filesystems(1)%mountpoint) // " " // &
             format_percent(real(filesystem_usage_percent(filesystems(1))))
    end if
  end function disk_summary_text

  function disk_io_summary_text(table) result(text)
    type(disk_table), intent(in) :: table
    character(len=:), allocatable :: text
    integer :: rate_index

    text = ""
    rate_index = busiest_disk_rate_index(table)
    if (rate_index <= 0) return

    text = "I/O " // trim(table%io_rates(rate_index)%device) // &
           " R: " // format_disk_rate(table%io_rates(rate_index)%read_bytes_per_sec) // &
           " W: " // format_disk_rate(table%io_rates(rate_index)%write_bytes_per_sec)
    if (allocated(table%latencies) .and. rate_index <= size(table%latencies)) then
      if (trim(table%latencies(rate_index)%device) == trim(table%io_rates(rate_index)%device)) then
        text = text // disk_latency_suffix(table%latencies(rate_index))
      end if
    end if
  end function disk_io_summary_text

  integer function busiest_disk_rate_index(table) result(rate_index)
    type(disk_table), intent(in) :: table
    real(real64) :: activity
    real(real64) :: best_activity
    integer :: index

    rate_index = 0
    best_activity = -1.0_real64
    if (.not. allocated(table%io_rates)) return
    do index = 1, size(table%io_rates)
      if (.not. table%io_rates(index)%valid) cycle
      if (len_trim(table%io_rates(index)%device) <= 0) cycle
      activity = max(0.0_real64, table%io_rates(index)%read_bytes_per_sec) + &
                 max(0.0_real64, table%io_rates(index)%write_bytes_per_sec)
      if (rate_index <= 0 .or. activity > best_activity) then
        rate_index = index
        best_activity = activity
      end if
    end do
  end function busiest_disk_rate_index

  function disk_latency_suffix(latency) result(text)
    type(disk_latency_info), intent(in) :: latency
    character(len=:), allocatable :: text

    text = ""
    if (.not. latency%valid) return
    if (latency%read_valid) text = text // " rlat " // format_latency_us(latency%avg_read_latency_us)
    if (latency%write_valid) text = text // " wlat " // format_latency_us(latency%avg_write_latency_us)
  end function disk_latency_suffix

  function format_disk_rate(bytes_per_sec) result(text)
    real(real64), intent(in) :: bytes_per_sec
    character(len=:), allocatable :: text
    character(len=32) :: buffer
    real(real64) :: value

    value = max(0.0_real64, bytes_per_sec)
    if (value < RATE_KIB) then
      write(buffer, '(I0, A)') int(value), " B/s"
    else if (value < RATE_MIB) then
      write(buffer, '(F0.1, A)') value / RATE_KIB, " KiB/s"
    else if (value < RATE_GIB) then
      write(buffer, '(F0.1, A)') value / RATE_MIB, " MiB/s"
    else
      write(buffer, '(F0.1, A)') value / RATE_GIB, " GiB/s"
    end if
    text = trim(adjustl(buffer))
  end function format_disk_rate

  function format_latency_us(latency_us) result(text)
    real(real64), intent(in) :: latency_us
    character(len=:), allocatable :: text
    character(len=32) :: buffer
    real(real64) :: value

    value = max(0.0_real64, latency_us)
    if (value < 1000.0_real64) then
      write(buffer, '(I0, A)') int(value), " us"
    else if (value < 1000000.0_real64) then
      write(buffer, '(F0.1, A)') value / 1000.0_real64, " ms"
    else
      write(buffer, '(F0.1, A)') value / 1000000.0_real64, " s"
    end if
    text = trim(adjustl(buffer))
  end function format_latency_us

  subroutine render_compact_filesystem_row(buffer, rect, filesystem, style, horizontal_state, active_row)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    type(filesystem_info), intent(in) :: filesystem
    type(screen_style), intent(in) :: style
    type(horizontal_text_state), intent(inout), optional :: horizontal_state
    logical, intent(in) :: active_row
    type(widget_rect) :: mount_rect
    type(widget_rect) :: percent_rect
    character(len=:), allocatable :: percent_text
    integer :: gap_width
    integer :: mount_width
    integer :: percent_width

    if (rect%width <= 0 .or. rect%height <= 0) return
    percent_text = format_percent(real(filesystem_usage_percent(filesystem)))
    percent_width = min(rect%width, max(COMPACT_PERCENT_WIDTH, len_trim(percent_text)))
    gap_width = min(COMPACT_ROW_GAP_WIDTH, max(0, rect%width - percent_width))
    mount_width = max(0, rect%width - percent_width - gap_width)

    if (mount_width > 0) then
      mount_rect = widget_rect(rect%row, rect%col, mount_width, 1)
      call render_horizontal_text(buffer, mount_rect, trim(filesystem%mountpoint), style, horizontal_state, &
                                  active=active_row, ticker=.false.)
    end if
    percent_rect = widget_rect(rect%row, rect%col + rect%width - percent_width, percent_width, 1)
    call render_horizontal_text(buffer, percent_rect, percent_text, style, horizontal_state, TEXT_ALIGN_RIGHT, &
                                active=active_row, ticker=.false.)
  end subroutine render_compact_filesystem_row

  function sorted_filesystems(table) result(filesystems)
    type(disk_table), intent(in) :: table
    type(filesystem_info), allocatable :: filesystems(:)
    integer :: filesystem_index
    integer :: output_index

    allocate(filesystems(real_filesystem_count(table)))
    output_index = 0
    if (.not. allocated(table%filesystems)) return
    do filesystem_index = 1, size(table%filesystems)
      if (.not. filesystem_visible_for_render(table%filesystems(filesystem_index))) cycle
      output_index = output_index + 1
      filesystems(output_index) = table%filesystems(filesystem_index)
    end do
    call sort_filesystems_by_usage(filesystems)
  end function sorted_filesystems

  logical function filesystem_visible_for_render(filesystem) result(visible)
    type(filesystem_info), intent(in) :: filesystem

    visible = filesystem%valid .and. filesystem%total_bytes > 0_int64
  end function filesystem_visible_for_render

  subroutine sort_filesystems_by_usage(filesystems)
    type(filesystem_info), intent(inout) :: filesystems(:)
    type(filesystem_info) :: current
    integer :: index
    integer :: scan

    do index = 2, size(filesystems)
      current = filesystems(index)
      scan = index - 1
      do while (scan >= 1 .and. filesystem_usage_percent(filesystems(scan)) < filesystem_usage_percent(current))
        filesystems(scan + 1) = filesystems(scan)
        scan = scan - 1
      end do
      filesystems(scan + 1) = current
    end do
  end subroutine sort_filesystems_by_usage

  real(real64) function hottest_usage_percent(filesystems) result(percent)
    type(filesystem_info), intent(in) :: filesystems(:)
    integer :: filesystem_index

    percent = 0.0_real64
    do filesystem_index = 1, size(filesystems)
      percent = max(percent, filesystem_usage_percent(filesystems(filesystem_index)))
    end do
  end function hottest_usage_percent

  subroutine disk_table_select_delta(state, delta)
    type(disk_table_state), intent(inout) :: state
    integer, intent(in) :: delta

    state%selected_row = state%selected_row + delta
    call normalize_disk_table_state(state)
  end subroutine disk_table_select_delta

  subroutine disk_table_page_delta(state, delta_pages)
    type(disk_table_state), intent(inout) :: state
    integer, intent(in) :: delta_pages
    integer :: step

    step = max(1, state%viewport_rows)
    state%selected_row = state%selected_row + delta_pages * step
    call normalize_disk_table_state(state)
  end subroutine disk_table_page_delta

  function disk_table_status(state) result(text)
    type(disk_table_state), intent(in) :: state
    character(len=:), allocatable :: text

    text = "disk row " // integer_text(max(0, state%selected_row)) // "/" // integer_text(max(0, state%row_count))
  end function disk_table_status

  subroutine normalize_disk_table_state(state)
    type(disk_table_state), intent(inout) :: state
    integer :: max_scroll

    state%row_count = max(0, state%row_count)
    state%viewport_rows = max(0, state%viewport_rows)
    if (state%row_count <= 0) then
      state%selected_row = 0
      state%scroll_row = 1
      return
    end if

    state%selected_row = max(1, min(state%row_count, state%selected_row))
    max_scroll = max(1, state%row_count - max(1, state%viewport_rows) + 1)
    state%scroll_row = max(1, min(max_scroll, state%scroll_row))
    if (state%viewport_rows > 0) then
      if (state%selected_row < state%scroll_row) state%scroll_row = state%selected_row
      if (state%selected_row >= state%scroll_row + state%viewport_rows) then
        state%scroll_row = state%selected_row - state%viewport_rows + 1
      end if
      state%scroll_row = max(1, min(max_scroll, state%scroll_row))
    end if
  end subroutine normalize_disk_table_state

  subroutine clear_disk_table_state(state)
    type(disk_table_state), intent(inout) :: state

    state%row_count = 0
    state%viewport_rows = 0
    call normalize_disk_table_state(state)
  end subroutine clear_disk_table_state

  function content_line_rect(content, line_index) result(line)
    type(widget_rect), intent(in) :: content
    integer, intent(in) :: line_index
    type(widget_rect) :: line

    line = widget_rect(content%row + line_index - 1, content%col, content%width, 1)
  end function content_line_rect

  function integer_text(value) result(text)
    integer, intent(in) :: value
    character(len=:), allocatable :: text
    character(len=32) :: scratch

    write(scratch, '(i0)') value
    text = trim(scratch)
  end function integer_text

end module ftop_disk
