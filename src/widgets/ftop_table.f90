module ftop_table
  use fgof_screen, only : clear_screen_style, put_glyph
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_text, only : &
    TEXT_ALIGN_LEFT, &
    TEXT_ALIGN_RIGHT, &
    render_text, &
    text_cell_width
  use ftop_widgets, only : widget, widget_rect, widget_size
  implicit none
  private

  integer, parameter, public :: TABLE_WIDTH_FIXED = 1
  integer, parameter, public :: TABLE_WIDTH_AUTO = 2
  integer, parameter, public :: TABLE_WIDTH_WEIGHT = 3

  integer, parameter, public :: TABLE_SORT_DESCENDING = -1
  integer, parameter, public :: TABLE_SORT_NONE = 0
  integer, parameter, public :: TABLE_SORT_ASCENDING = 1

  integer, parameter, public :: TABLE_SEPARATOR_SPACE = 1
  integer, parameter, public :: TABLE_SEPARATOR_THIN = 2
  integer, parameter, public :: TABLE_SEPARATOR_HEAVY = 3

  type, public :: table_column
    character(len=:), allocatable :: name
    integer :: width_mode = TABLE_WIDTH_AUTO
    integer :: width = 0
    integer :: weight = 1
    integer :: alignment = TEXT_ALIGN_LEFT
    integer :: sort_direction = TABLE_SORT_NONE
  end type table_column

  type, public :: table_cell
    character(len=:), allocatable :: text
    logical :: style_set = .false.
    type(screen_style) :: style
  end type table_cell

  type, extends(widget), public :: table_widget
    type(table_column), allocatable :: columns(:)
    type(table_cell), allocatable :: cells(:, :)
    integer :: scroll_row = 1
    integer :: selected_row = 0
    integer :: separator = TABLE_SEPARATOR_SPACE
    logical :: show_header = .true.
    logical :: striped = .false.
    type(screen_style) :: style
    type(screen_style) :: header_style
    type(screen_style) :: selected_style
    type(screen_style) :: alternate_style
    type(screen_style) :: separator_style
  contains
    procedure :: render => table_widget_render
    procedure :: min_size => table_widget_min_size
  end type table_widget

  public :: calculate_column_widths
  public :: make_table_cell
  public :: render_table
  public :: sort_table_cells
  public :: table_sort_indicator
  public :: table_viewport_row_count

contains

  function make_table_cell(text, style) result(cell)
    character(len=*), intent(in) :: text
    type(screen_style), intent(in), optional :: style
    type(table_cell) :: cell

    cell%text = text
    if (present(style)) then
      cell%style = style
      cell%style_set = .true.
    end if
  end function make_table_cell

  subroutine render_table(buffer, rect, columns, cells, scroll_row, selected_row, separator, &
                          show_header, striped, style, header_style, selected_style, &
                          alternate_style, separator_style)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    type(table_column), intent(in) :: columns(:)
    type(table_cell), intent(in) :: cells(:, :)
    integer, intent(in), optional :: scroll_row
    integer, intent(in), optional :: selected_row
    integer, intent(in), optional :: separator
    logical, intent(in), optional :: show_header
    logical, intent(in), optional :: striped
    type(screen_style), intent(in), optional :: style
    type(screen_style), intent(in), optional :: header_style
    type(screen_style), intent(in), optional :: selected_style
    type(screen_style), intent(in), optional :: alternate_style
    type(screen_style), intent(in), optional :: separator_style
    integer, allocatable :: widths(:)
    type(screen_style) :: active_alternate_style
    type(screen_style) :: active_header_style
    type(screen_style) :: active_selected_style
    type(screen_style) :: active_separator_style
    type(screen_style) :: active_style
    type(screen_style) :: row_style
    integer :: actual_scroll_row
    integer :: actual_selected_row
    integer :: actual_separator
    integer :: col_count
    integer :: data_row
    integer :: draw_row
    integer :: max_rows
    integer :: row_count
    integer :: table_row
    logical :: actual_show_header
    logical :: actual_striped

    if (rect%width <= 0 .or. rect%height <= 0) return
    if (buffer%size%width <= 0 .or. buffer%size%height <= 0) return
    if (size(columns) <= 0) return

    actual_scroll_row = 1
    actual_selected_row = 0
    actual_separator = TABLE_SEPARATOR_SPACE
    actual_show_header = .true.
    actual_striped = .false.
    if (present(scroll_row)) actual_scroll_row = max(1, scroll_row)
    if (present(selected_row)) actual_selected_row = selected_row
    if (present(separator)) actual_separator = separator
    if (present(show_header)) actual_show_header = show_header
    if (present(striped)) actual_striped = striped

    active_style = clear_screen_style()
    active_header_style = clear_screen_style()
    active_selected_style = clear_screen_style()
    active_alternate_style = clear_screen_style()
    active_separator_style = clear_screen_style()
    if (present(style)) active_style = style
    if (present(header_style)) active_header_style = header_style
    if (present(selected_style)) active_selected_style = selected_style
    if (present(alternate_style)) active_alternate_style = alternate_style
    if (present(separator_style)) active_separator_style = separator_style

    call calculate_column_widths(columns, cells, rect%width, actual_separator, widths)
    col_count = size(widths)
    if (col_count <= 0) return

    draw_row = rect%row
    if (actual_show_header) then
      call render_header(buffer, widget_rect(draw_row, rect%col, rect%width, 1), columns, widths, &
                         actual_separator, active_header_style, active_separator_style)
      draw_row = draw_row + 1
    end if

    max_rows = table_viewport_row_count(rect, actual_show_header)
    if (max_rows <= 0) return

    row_count = size(cells, 1)
    do table_row = 1, max_rows
      data_row = actual_scroll_row + table_row - 1
      if (data_row > row_count) exit
      row_style = active_style
      if (actual_striped .and. mod(table_row, 2) == 0) row_style = active_alternate_style
      if (data_row == actual_selected_row) row_style = active_selected_style
      call render_data_row(buffer, widget_rect(draw_row + table_row - 1, rect%col, rect%width, 1), &
                           columns, cells, data_row, widths, actual_separator, row_style, &
                           active_separator_style)
    end do
  end subroutine render_table

  subroutine calculate_column_widths(columns, cells, available_width, separator, widths)
    type(table_column), intent(in) :: columns(:)
    type(table_cell), intent(in) :: cells(:, :)
    integer, intent(in) :: available_width
    integer, intent(in) :: separator
    integer, allocatable, intent(out) :: widths(:)
    integer :: col
    integer :: content_width
    integer :: leftover
    integer :: remaining
    integer :: separator_cells
    integer :: total_weight
    integer :: used

    allocate(widths(size(columns)))
    widths = 0
    if (size(columns) <= 0 .or. available_width <= 0) return

    associate(unused_separator => separator)
    end associate
    separator_cells = max(0, size(columns) - 1)
    content_width = max(0, available_width - separator_cells)
    total_weight = 0

    do col = 1, size(columns)
      select case (columns(col)%width_mode)
      case (TABLE_WIDTH_FIXED)
        widths(col) = max(0, columns(col)%width)
      case (TABLE_WIDTH_WEIGHT)
        total_weight = total_weight + max(1, columns(col)%weight)
      case default
        widths(col) = preferred_column_width(columns(col), cells, col)
      end select
    end do

    used = sum(widths)
    remaining = max(0, content_width - used)
    if (total_weight > 0) then
      do col = 1, size(columns)
        if (columns(col)%width_mode == TABLE_WIDTH_WEIGHT) then
          widths(col) = remaining * max(1, columns(col)%weight) / total_weight
        end if
      end do
      leftover = content_width - sum(widths)
      do col = 1, size(columns)
        if (leftover <= 0) exit
        if (columns(col)%width_mode == TABLE_WIDTH_WEIGHT) then
          widths(col) = widths(col) + 1
          leftover = leftover - 1
        end if
      end do
    end if

    call shrink_widths_to_fit(widths, content_width)
  end subroutine calculate_column_widths

  integer function table_viewport_row_count(rect, show_header) result(row_count)
    type(widget_rect), intent(in) :: rect
    logical, intent(in) :: show_header

    row_count = max(0, rect%height)
    if (show_header) row_count = max(0, row_count - 1)
  end function table_viewport_row_count

  function table_sort_indicator(sort_direction) result(indicator)
    integer, intent(in) :: sort_direction
    character(len=:), allocatable :: indicator

    select case (sort_direction)
    case (TABLE_SORT_ASCENDING)
      indicator = "▲"
    case (TABLE_SORT_DESCENDING)
      indicator = "▼"
    case default
      indicator = ""
    end select
  end function table_sort_indicator

  subroutine sort_table_cells(cells, column_index, descending)
    type(table_cell), intent(inout) :: cells(:, :)
    integer, intent(in) :: column_index
    logical, intent(in), optional :: descending
    type(table_cell), allocatable :: temp(:)
    integer :: i
    integer :: j
    logical :: actual_descending

    if (column_index < 1 .or. column_index > size(cells, 2)) return
    actual_descending = .false.
    if (present(descending)) actual_descending = descending

    allocate(temp(size(cells, 2)))
    do i = 1, size(cells, 1) - 1
      do j = i + 1, size(cells, 1)
        if (rows_out_of_order(cells, i, j, column_index, actual_descending)) then
          temp = cells(i, :)
          cells(i, :) = cells(j, :)
          cells(j, :) = temp
        end if
      end do
    end do
  end subroutine sort_table_cells

  subroutine table_widget_render(self, buffer, rect)
    class(table_widget), intent(inout) :: self
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect

    if (.not. self%visible) return
    if (.not. allocated(self%columns)) return
    if (.not. allocated(self%cells)) return

    call render_table(buffer, rect, self%columns, self%cells, scroll_row=self%scroll_row, &
                      selected_row=self%selected_row, separator=self%separator, &
                      show_header=self%show_header, striped=self%striped, style=self%style, &
                      header_style=self%header_style, selected_style=self%selected_style, &
                      alternate_style=self%alternate_style, separator_style=self%separator_style)
  end subroutine table_widget_render

  function table_widget_min_size(self) result(size_value)
    class(table_widget), intent(in) :: self
    type(widget_size) :: size_value
    integer :: col

    size_value%width = 1
    size_value%height = 1
    if (.not. allocated(self%columns)) return

    size_value%width = max(1, size(self%columns) - 1)
    do col = 1, size(self%columns)
      if (allocated(self%cells)) then
        size_value%width = size_value%width + max(1, preferred_column_width(self%columns(col), self%cells, col))
      else
        size_value%width = size_value%width + max(1, preferred_column_width(self%columns(col), col=col))
      end if
    end do
    if (self%show_header) size_value%height = 1
    if (allocated(self%cells)) size_value%height = size_value%height + min(1, size(self%cells, 1))
  end function table_widget_min_size

  subroutine render_header(buffer, rect, columns, widths, separator, header_style, separator_style)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    type(table_column), intent(in) :: columns(:)
    integer, intent(in) :: widths(:)
    integer, intent(in) :: separator
    type(screen_style), intent(in) :: header_style
    type(screen_style), intent(in) :: separator_style
    integer :: col
    integer :: draw_col

    draw_col = rect%col
    do col = 1, size(columns)
      if (widths(col) > 0) then
        call render_text(buffer, widget_rect(rect%row, draw_col, widths(col), 1), &
                         column_header_text(columns(col)), header_style, columns(col)%alignment)
      end if
      draw_col = draw_col + widths(col)
      if (col < size(columns)) call render_separator(buffer, rect%row, draw_col, separator, separator_style)
      draw_col = draw_col + 1
    end do
  end subroutine render_header

  subroutine render_data_row(buffer, rect, columns, cells, row_index, widths, separator, row_style, separator_style)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    type(table_column), intent(in) :: columns(:)
    type(table_cell), intent(in) :: cells(:, :)
    integer, intent(in) :: row_index
    integer, intent(in) :: widths(:)
    integer, intent(in) :: separator
    type(screen_style), intent(in) :: row_style
    type(screen_style), intent(in) :: separator_style
    type(screen_style) :: cell_style
    integer :: col
    integer :: draw_col

    draw_col = rect%col
    do col = 1, size(columns)
      if (widths(col) > 0) then
        cell_style = row_style
        if (cells(row_index, col)%style_set) cell_style = cells(row_index, col)%style
        call render_text(buffer, widget_rect(rect%row, draw_col, widths(col), 1), &
                         table_cell_text(cells, row_index, col), cell_style, columns(col)%alignment)
      end if
      draw_col = draw_col + widths(col)
      if (col < size(columns)) call render_separator(buffer, rect%row, draw_col, separator, separator_style)
      draw_col = draw_col + 1
    end do
  end subroutine render_data_row

  subroutine render_separator(buffer, row, col, separator, style)
    type(screen_buffer), intent(inout) :: buffer
    integer, intent(in) :: row
    integer, intent(in) :: col
    integer, intent(in) :: separator
    type(screen_style), intent(in) :: style

    call put_glyph(buffer, row, col, table_separator_glyph(separator), style)
  end subroutine render_separator

  function column_header_text(column) result(text)
    type(table_column), intent(in) :: column
    character(len=:), allocatable :: text
    character(len=:), allocatable :: indicator

    if (allocated(column%name)) then
      text = column%name
    else
      text = ""
    end if

    indicator = table_sort_indicator(column%sort_direction)
    if (len(indicator) > 0) text = text // " " // indicator
  end function column_header_text

  function table_separator_glyph(separator) result(glyph)
    integer, intent(in) :: separator
    character(len=:), allocatable :: glyph

    select case (separator)
    case (TABLE_SEPARATOR_THIN)
      glyph = "│"
    case (TABLE_SEPARATOR_HEAVY)
      glyph = "┃"
    case default
      glyph = " "
    end select
  end function table_separator_glyph

  function table_cell_text(cells, row, col) result(text)
    type(table_cell), intent(in) :: cells(:, :)
    integer, intent(in) :: row
    integer, intent(in) :: col
    character(len=:), allocatable :: text

    text = ""
    if (row < 1 .or. row > size(cells, 1)) return
    if (col < 1 .or. col > size(cells, 2)) return
    if (allocated(cells(row, col)%text)) text = cells(row, col)%text
  end function table_cell_text

  integer function preferred_column_width(column, cells, col) result(width)
    type(table_column), intent(in) :: column
    type(table_cell), intent(in), optional :: cells(:, :)
    integer, intent(in) :: col
    integer :: row

    width = text_cell_width(column_header_text(column))
    if (column%width_mode == TABLE_WIDTH_FIXED) width = max(width, column%width)
    if (present(cells)) then
      if (col >= 1 .and. col <= size(cells, 2)) then
        do row = 1, size(cells, 1)
          width = max(width, text_cell_width(table_cell_text(cells, row, col)))
        end do
      end if
    end if
  end function preferred_column_width

  subroutine shrink_widths_to_fit(widths, available_width)
    integer, intent(inout) :: widths(:)
    integer, intent(in) :: available_width
    integer :: largest

    if (available_width < 0) then
      widths = 0
      return
    end if

    do while (sum(widths) > available_width .and. any(widths > 0))
      largest = maxloc(widths, dim=1)
      widths(largest) = widths(largest) - 1
    end do
  end subroutine shrink_widths_to_fit

  logical function rows_out_of_order(cells, left_row, right_row, column_index, descending) result(out_of_order)
    type(table_cell), intent(in) :: cells(:, :)
    integer, intent(in) :: left_row
    integer, intent(in) :: right_row
    integer, intent(in) :: column_index
    logical, intent(in) :: descending
    character(len=:), allocatable :: left_text
    character(len=:), allocatable :: right_text

    left_text = table_cell_text(cells, left_row, column_index)
    right_text = table_cell_text(cells, right_row, column_index)
    if (descending) then
      out_of_order = left_text < right_text
    else
      out_of_order = left_text > right_text
    end if
  end function rows_out_of_order

end module ftop_table
