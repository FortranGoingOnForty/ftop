module ftop_process_table
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_box, only : BOX_STYLE_ROUNDED, box_content_rect, draw_box
  use ftop_collector, only : collector_snapshot
  use ftop_proc_data, only : &
    process_display_command, &
    process_info, &
    process_state_label, &
    process_table, &
    process_user_label
  use ftop_table, only : &
    TABLE_SEPARATOR_SPACE, &
    TABLE_WIDTH_FIXED, &
    TABLE_WIDTH_WEIGHT, &
    make_table_cell, &
    render_table, &
    table_cell, &
    table_column
  use ftop_text, only : TEXT_ALIGN_RIGHT, format_bytes, format_percent, render_text
  use ftop_widgets, only : widget_rect, widget_size
  implicit none
  private

  integer, parameter :: PROCESS_TABLE_COLUMNS = 7

  public :: process_panel_min_size
  public :: render_process_panel

contains

  function process_panel_min_size() result(size_value)
    type(widget_size) :: size_value

    size_value%width = 44
    size_value%height = 8
  end function process_panel_min_size

  subroutine render_process_panel(buffer, panel, snapshot, border_style, title_style, dim_style)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: panel
    type(collector_snapshot), intent(in) :: snapshot
    type(screen_style), intent(in) :: border_style
    type(screen_style), intent(in) :: title_style
    type(screen_style), intent(in) :: dim_style
    type(table_cell), allocatable :: cells(:, :)
    type(table_column), allocatable :: columns(:)
    type(widget_rect) :: content

    call draw_box(buffer, panel, BOX_STYLE_ROUNDED, border_style, "Processes", title_style)
    content = box_content_rect(panel)
    if (content%height <= 0 .or. content%width <= 0) return

    if (.not. snapshot%processes%valid) then
      call render_text(buffer, content_line_rect(content, 1), "processes unavailable", dim_style)
      return
    end if
    if (.not. allocated(snapshot%processes%items) .or. size(snapshot%processes%items) <= 0) then
      call render_text(buffer, content_line_rect(content, 1), "no processes", dim_style)
      return
    end if

    columns = process_columns()
    cells = process_cells(snapshot%processes)
    if (size(cells, 1) <= 0) then
      call render_text(buffer, content_line_rect(content, 1), "no processes", dim_style)
      return
    end if

    call render_table(buffer, content, columns, cells, separator=TABLE_SEPARATOR_SPACE, &
                      show_header=.true., striped=.false., style=dim_style, &
                      header_style=title_style, separator_style=dim_style)
  end subroutine render_process_panel

  function process_columns() result(columns)
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
  end function process_columns

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
      call fill_process_row(cells(row, :), table%items(process_index))
    end do
  end function process_cells

  subroutine fill_process_row(row, process)
    type(table_cell), intent(out) :: row(:)
    type(process_info), intent(in) :: process

    if (size(row) < PROCESS_TABLE_COLUMNS) return
    row(1) = make_table_cell(integer_text(process%pid))
    row(2) = make_table_cell(process_user_label(process))
    row(3) = make_table_cell(format_percent(real(clamp_percent(process%cpu_percent))))
    row(4) = make_table_cell(format_percent(real(clamp_percent(process%mem_percent))))
    row(5) = make_table_cell(format_bytes(max(0_int64, process%mem_rss_bytes)))
    row(6) = make_table_cell(process_state_label(process))
    row(7) = make_table_cell(process_display_command(process))
  end subroutine fill_process_row

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
