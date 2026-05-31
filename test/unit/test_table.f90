program test_table
  use fgof_screen, only : allocate_screen, clear_screen_style
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_table, only : &
    TABLE_SEPARATOR_HEAVY, &
    TABLE_SEPARATOR_THIN, &
    TABLE_SORT_ASCENDING, &
    TABLE_WIDTH_AUTO, &
    TABLE_WIDTH_FIXED, &
    TABLE_WIDTH_WEIGHT, &
    calculate_column_widths, &
    make_table_cell, &
    render_table, &
    sort_table_cells, &
    table_cell, &
    table_column, &
    table_sort_indicator, &
    table_viewport_row_count, &
    table_widget
  use ftop_text, only : TEXT_ALIGN_RIGHT
  use ftop_widgets, only : widget_rect, widget_size
  implicit none

  call test_width_calculation()
  call test_header_and_alignment_rendering()
  call test_zero_width_column_rendering()
  call test_scroll_selection_and_striping()
  call test_sorting_rows()
  call test_table_widget_type()

contains

  subroutine test_width_calculation()
    type(table_cell), allocatable :: cells(:, :)
    type(table_column) :: columns(3)
    integer, allocatable :: widths(:)

    columns(1)%name = "PID"
    columns(1)%width_mode = TABLE_WIDTH_FIXED
    columns(1)%width = 4
    columns(2)%name = "CPU"
    columns(2)%width_mode = TABLE_WIDTH_AUTO
    columns(3)%name = "Command"
    columns(3)%width_mode = TABLE_WIDTH_WEIGHT
    columns(3)%weight = 1

    allocate(cells(2, 3))
    cells(1, 1) = make_table_cell("1")
    cells(1, 2) = make_table_cell("7%")
    cells(1, 3) = make_table_cell("ftop")
    cells(2, 1) = make_table_cell("22")
    cells(2, 2) = make_table_cell("100%")
    cells(2, 3) = make_table_cell("collector")

    call calculate_column_widths(columns, cells, 15, TABLE_SEPARATOR_THIN, widths)
    call require(size(widths) == 3, "table width count mismatch")
    call require(all(widths == [4, 4, 5]), "table mixed width calculation mismatch")
    call require(table_viewport_row_count(widget_rect(row=1, col=1, width=5, height=3), .true.) == 2, &
                 "table viewport row count mismatch")
  end subroutine test_width_calculation

  subroutine test_header_and_alignment_rendering()
    type(screen_buffer) :: buffer
    type(table_cell), allocatable :: cells(:, :)
    type(table_column) :: columns(2)

    buffer = allocate_screen(10, 2)
    columns(1)%name = "Name"
    columns(1)%width_mode = TABLE_WIDTH_FIXED
    columns(1)%width = 6
    columns(1)%sort_direction = TABLE_SORT_ASCENDING
    columns(2)%name = "CPU"
    columns(2)%width_mode = TABLE_WIDTH_FIXED
    columns(2)%width = 3
    columns(2)%alignment = TEXT_ALIGN_RIGHT

    allocate(cells(1, 2))
    cells(1, 1) = make_table_cell("ftop")
    cells(1, 2) = make_table_cell("42")

    call render_table(buffer, widget_rect(row=1, col=1, width=10, height=2), columns, cells, &
                      separator=TABLE_SEPARATOR_THIN)

    call require(table_sort_indicator(TABLE_SORT_ASCENDING) == "▲", "sort indicator mismatch")
    call require_glyph(buffer, 1, 6, "▲", "table sort glyph mismatch")
    call require_glyph(buffer, 1, 7, "│", "table thin separator mismatch")
    call require_glyph(buffer, 2, 1, "f", "table first cell mismatch")
    call require_glyph(buffer, 2, 9, "4", "table right-aligned cell first glyph mismatch")
    call require_glyph(buffer, 2, 10, "2", "table right-aligned cell final glyph mismatch")
  end subroutine test_header_and_alignment_rendering

  subroutine test_zero_width_column_rendering()
    type(screen_buffer) :: buffer
    type(table_cell), allocatable :: cells(:, :)
    type(table_column) :: columns(2)
    integer, allocatable :: widths(:)

    buffer = allocate_screen(2, 1)
    columns(1)%name = "Hidden"
    columns(1)%width_mode = TABLE_WIDTH_FIXED
    columns(1)%width = 0
    columns(2)%name = "B"
    columns(2)%width_mode = TABLE_WIDTH_FIXED
    columns(2)%width = 1

    allocate(cells(1, 2))
    cells(1, 1) = make_table_cell("skip")
    cells(1, 2) = make_table_cell("x")

    call calculate_column_widths(columns, cells, 2, TABLE_SEPARATOR_THIN, widths)
    call require(all(widths == [0, 1]), "zero-width table column calculation mismatch")
    call render_table(buffer, widget_rect(row=1, col=1, width=2, height=1), columns, cells, &
                      separator=TABLE_SEPARATOR_THIN)
    call require_glyph(buffer, 1, 1, "│", "zero-width table separator mismatch")
    call require_glyph(buffer, 1, 2, "B", "zero-width table visible header mismatch")
  end subroutine test_zero_width_column_rendering

  subroutine test_scroll_selection_and_striping()
    type(screen_buffer) :: buffer
    type(screen_style) :: alternate_style
    type(screen_style) :: selected_style
    type(table_cell), allocatable :: cells(:, :)
    type(table_column) :: columns(1)

    buffer = allocate_screen(4, 2)
    alternate_style = clear_screen_style()
    alternate_style%dim = .true.
    selected_style = clear_screen_style()
    selected_style%inverse = .true.
    columns(1)%name = "Row"
    columns(1)%width_mode = TABLE_WIDTH_FIXED
    columns(1)%width = 4

    allocate(cells(3, 1))
    cells(1, 1) = make_table_cell("r1")
    cells(2, 1) = make_table_cell("r2")
    cells(3, 1) = make_table_cell("r3")

    call render_table(buffer, widget_rect(row=1, col=1, width=4, height=2), columns, cells, &
                      scroll_row=2, selected_row=3, show_header=.false., striped=.true., &
                      selected_style=selected_style, alternate_style=alternate_style)

    call require_glyph(buffer, 1, 1, "r", "table scrolled first row mismatch")
    call require_glyph(buffer, 1, 2, "2", "table scrolled first value mismatch")
    call require_glyph(buffer, 2, 2, "3", "table selected row value mismatch")
    call require(buffer%cells(2, 1)%style%inverse, "table selected row style mismatch")
  end subroutine test_scroll_selection_and_striping

  subroutine test_sorting_rows()
    type(table_cell), allocatable :: cells(:, :)

    allocate(cells(3, 2))
    cells(1, 1) = make_table_cell("b")
    cells(1, 2) = make_table_cell("2")
    cells(2, 1) = make_table_cell("a")
    cells(2, 2) = make_table_cell("1")
    cells(3, 1) = make_table_cell("c")
    cells(3, 2) = make_table_cell("3")

    call sort_table_cells(cells, 1)
    call require(cell_text(cells(1, 1)) == "a", "ascending table sort first row mismatch")
    call require(cell_text(cells(2, 1)) == "b", "ascending table sort second row mismatch")
    call require(cell_text(cells(3, 2)) == "3", "ascending table sort row data mismatch")

    call sort_table_cells(cells, 1, descending=.true.)
    call require(cell_text(cells(1, 1)) == "c", "descending table sort first row mismatch")
    call require(cell_text(cells(3, 1)) == "a", "descending table sort final row mismatch")
  end subroutine test_sorting_rows

  subroutine test_table_widget_type()
    type(screen_buffer) :: buffer
    type(screen_style) :: selected_style
    type(table_cell), allocatable :: cells(:, :)
    type(table_column) :: columns(2)
    type(table_widget) :: table
    type(widget_size) :: size_value

    buffer = allocate_screen(8, 2)
    selected_style = clear_screen_style()
    selected_style%inverse = .true.
    columns(1)%name = "Name"
    columns(1)%width_mode = TABLE_WIDTH_FIXED
    columns(1)%width = 4
    columns(2)%name = "CPU"
    columns(2)%width_mode = TABLE_WIDTH_FIXED
    columns(2)%width = 3
    allocate(cells(1, 2))
    cells(1, 1) = make_table_cell("ftop")
    cells(1, 2) = make_table_cell("9")

    table%columns = columns
    table%cells = cells
    table%selected_row = 1
    table%separator = TABLE_SEPARATOR_HEAVY
    table%selected_style = selected_style

    size_value = table%min_size()
    call require(size_value%width == 8, "table widget min width mismatch")
    call require(size_value%height == 2, "table widget min height mismatch")

    call table%render(buffer, widget_rect(row=1, col=1, width=8, height=2))
    call require_glyph(buffer, 1, 5, "┃", "table widget heavy separator mismatch")
    call require_glyph(buffer, 2, 1, "f", "table widget first cell mismatch")
    call require(buffer%cells(2, 1)%style%inverse, "table widget selected style mismatch")
  end subroutine test_table_widget_type

  function cell_text(cell) result(text)
    type(table_cell), intent(in) :: cell
    character(len=:), allocatable :: text

    text = ""
    if (allocated(cell%text)) text = cell%text
  end function cell_text

  subroutine require_glyph(buffer, row, col, expected, message)
    type(screen_buffer), intent(in) :: buffer
    integer, intent(in) :: row
    integer, intent(in) :: col
    character(len=*), intent(in) :: expected
    character(len=*), intent(in) :: message

    call require(allocated(buffer%cells(row, col)%glyph), message)
    call require(buffer%cells(row, col)%glyph == expected, message)
  end subroutine require_glyph

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

end program test_table
