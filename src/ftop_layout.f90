module ftop_layout
  use, intrinsic :: iso_fortran_env, only : int64
  use fgof_toml, only : &
    TOML_KIND_ARRAY, &
    TOML_KIND_INTEGER, &
    TOML_KIND_STRING, &
    TOML_KIND_TABLE, &
    parse_file, &
    parse_string, &
    toml_array, &
    toml_document, &
    toml_error, &
    toml_value
  use ftop_widgets, only : widget_rect, widget_size
  implicit none
  private

  character(len=*), parameter, public :: LAYOUT_WIDGET_CPU = "cpu"
  character(len=*), parameter, public :: LAYOUT_WIDGET_MEMORY = "memory"
  character(len=*), parameter, public :: LAYOUT_WIDGET_PROCESS = "process"

  type, public :: layout_column
    character(len=:), allocatable :: widget
    integer :: weight = 1
    type(widget_size) :: min_size
  end type layout_column

  type, public :: layout_row
    integer :: weight = 1
    type(layout_column), allocatable :: columns(:)
  end type layout_row

  type, public :: layout_grid
    type(layout_row), allocatable :: rows(:)
  end type layout_grid

  type, public :: layout_assignment
    character(len=:), allocatable :: widget
    type(widget_rect) :: rect
  end type layout_assignment

  type, public :: dashboard_layout
    type(widget_rect) :: frame
    type(widget_rect) :: cpu_panel
    type(widget_rect) :: memory_panel
    type(widget_rect) :: process_panel
    type(widget_rect) :: footer
  end type dashboard_layout

  type, public :: layout_error
    logical :: failed = .false.
    integer :: line = 0
    integer :: column = 0
    character(len=:), allocatable :: message
  end type layout_error

  public :: clear_layout_error
  public :: dashboard_layout_from_grid
  public :: default_dashboard_layout
  public :: default_dashboard_grid
  public :: distribute_weighted_space
  public :: layout_focus_count
  public :: layout_focus_widget
  public :: layout_widget_registered
  public :: layout_widget_renderable
  public :: make_layout_column
  public :: make_layout_row
  public :: parse_layout_file
  public :: parse_layout_toml
  public :: resolve_layout

contains

  function default_dashboard_layout(width, height) result(layout)
    integer, intent(in) :: width
    integer, intent(in) :: height
    type(dashboard_layout) :: layout
    type(layout_grid) :: grid

    grid = default_dashboard_grid(stacked=width < 72)
    layout = dashboard_layout_from_grid(width, height, grid)
  end function default_dashboard_layout

  function dashboard_layout_from_grid(width, height, grid) result(layout)
    integer, intent(in) :: width
    integer, intent(in) :: height
    type(layout_grid), intent(in) :: grid
    type(dashboard_layout) :: layout
    type(layout_assignment), allocatable :: assignments(:)
    type(widget_rect) :: body
    integer :: item

    layout = empty_dashboard_layout(width, height)

    if (width < 8 .or. height < 6) return

    body = widget_rect(3, 3, max(0, width - 4), max(0, height - 5))
    if (body%width <= 0 .or. body%height <= 0) return

    assignments = resolve_layout(grid, body)
    do item = 1, size(assignments)
      select case (assignments(item)%widget)
      case (LAYOUT_WIDGET_CPU)
        layout%cpu_panel = assignments(item)%rect
      case (LAYOUT_WIDGET_MEMORY)
        layout%memory_panel = assignments(item)%rect
      case (LAYOUT_WIDGET_PROCESS)
        layout%process_panel = assignments(item)%rect
      end select
    end do
  end function dashboard_layout_from_grid

  function empty_dashboard_layout(width, height) result(layout)
    integer, intent(in) :: width
    integer, intent(in) :: height
    type(dashboard_layout) :: layout

    layout%frame = widget_rect(1, 1, max(0, width), max(0, height))
    layout%cpu_panel = widget_rect(0, 0, 0, 0)
    layout%memory_panel = widget_rect(0, 0, 0, 0)
    layout%process_panel = widget_rect(0, 0, 0, 0)
    layout%footer = widget_rect(max(1, height - 2), 3, max(0, width - 4), &
                                min(2, max(0, height - 2)))
  end function empty_dashboard_layout

  function default_dashboard_grid(stacked) result(grid)
    logical, intent(in), optional :: stacked
    type(layout_grid) :: grid
    logical :: use_stacked

    use_stacked = .false.
    if (present(stacked)) use_stacked = stacked

    if (use_stacked) then
      allocate(grid%rows(2))
      grid%rows(1) = make_layout_row(1, [make_layout_column(LAYOUT_WIDGET_CPU, 1, 24, 6)])
      grid%rows(2) = make_layout_row(1, [make_layout_column(LAYOUT_WIDGET_MEMORY, 1, 24, 6)])
    else
      allocate(grid%rows(1))
      grid%rows(1) = make_layout_row(1, [ &
        make_layout_column(LAYOUT_WIDGET_CPU, 1, 28, 9), &
        make_layout_column(LAYOUT_WIDGET_MEMORY, 1, 28, 8) &
      ])
    end if
  end function default_dashboard_grid

  function make_layout_column(widget, weight, min_width, min_height) result(column)
    character(len=*), intent(in) :: widget
    integer, intent(in), optional :: weight
    integer, intent(in), optional :: min_width
    integer, intent(in), optional :: min_height
    type(layout_column) :: column

    column%widget = trim(widget)
    if (present(weight)) column%weight = max(1, weight)
    if (present(min_width)) column%min_size%width = max(0, min_width)
    if (present(min_height)) column%min_size%height = max(0, min_height)
  end function make_layout_column

  function make_layout_row(weight, columns) result(row)
    integer, intent(in) :: weight
    type(layout_column), intent(in) :: columns(:)
    type(layout_row) :: row

    row%weight = max(1, weight)
    allocate(row%columns(size(columns)))
    row%columns = columns
  end function make_layout_row

  subroutine parse_layout_file(path, grid, error)
    character(len=*), intent(in) :: path
    type(layout_grid), intent(out) :: grid
    type(layout_error), intent(out) :: error
    type(toml_document) :: document
    type(toml_error) :: toml_status

    call clear_layout_error(error)
    call parse_file(path, document, toml_status)
    if (toml_status%failed) then
      call set_layout_error(error, toml_status%message, toml_status%line, toml_status%column)
      return
    end if
    call parse_layout_document(document, grid, error)
  end subroutine parse_layout_file

  subroutine parse_layout_toml(content, grid, error)
    character(len=*), intent(in) :: content
    type(layout_grid), intent(out) :: grid
    type(layout_error), intent(out) :: error
    type(toml_document) :: document
    type(toml_error) :: toml_status

    call clear_layout_error(error)
    call parse_string(content, document, toml_status)
    if (toml_status%failed) then
      call set_layout_error(error, toml_status%message, toml_status%line, toml_status%column)
      return
    end if
    call parse_layout_document(document, grid, error)
  end subroutine parse_layout_toml

  subroutine parse_layout_document(document, grid, error)
    type(toml_document), intent(in) :: document
    type(layout_grid), intent(out) :: grid
    type(layout_error), intent(inout) :: error
    type(toml_array) :: rows
    integer :: row_index

    rows = document%get_array("row")
    if (rows%length() <= 0) then
      call set_layout_error(error, "layout must contain at least one [[row]] table")
      return
    end if

    allocate(grid%rows(rows%length()))
    do row_index = 1, rows%length()
      call parse_row(rows%values(row_index), row_index, grid%rows(row_index), error)
      if (error%failed) return
    end do
  end subroutine parse_layout_document

  subroutine parse_row(row_value, row_index, row, error)
    type(toml_value), intent(in) :: row_value
    integer, intent(in) :: row_index
    type(layout_row), intent(out) :: row
    type(layout_error), intent(inout) :: error
    integer :: column_index
    integer :: columns_index

    if (row_value%kind /= TOML_KIND_TABLE) then
      call set_layout_error(error, "layout row " // integer_text(row_index) // " must be a table")
      return
    end if

    row%weight = integer_field(row_value, "weight", 1, error)
    if (error%failed) return

    columns_index = table_entry_index(row_value, "column")
    if (columns_index == 0) then
      call set_layout_error(error, "layout row " // integer_text(row_index) // " must contain [[row.column]]")
      return
    end if
    if (row_value%table_values(columns_index)%kind /= TOML_KIND_ARRAY) then
      call set_layout_error(error, "layout row " // integer_text(row_index) // " column must be an array")
      return
    end if
    if (size(row_value%table_values(columns_index)%array_values) <= 0) then
      call set_layout_error(error, "layout row " // integer_text(row_index) // " must contain columns")
      return
    end if

    allocate(row%columns(size(row_value%table_values(columns_index)%array_values)))
    do column_index = 1, size(row%columns)
      call parse_column(row_value%table_values(columns_index)%array_values(column_index), &
                        row_index, column_index, row%columns(column_index), error)
      if (error%failed) return
    end do
  end subroutine parse_row

  subroutine parse_column(column_value, row_index, column_index, column, error)
    type(toml_value), intent(in) :: column_value
    integer, intent(in) :: row_index
    integer, intent(in) :: column_index
    type(layout_column), intent(out) :: column
    type(layout_error), intent(inout) :: error
    integer :: widget_index

    if (column_value%kind /= TOML_KIND_TABLE) then
      call set_layout_error(error, "layout row " // integer_text(row_index) // &
                            " column " // integer_text(column_index) // " must be a table")
      return
    end if

    widget_index = table_entry_index(column_value, "widget")
    if (widget_index == 0) then
      call set_layout_error(error, "layout row " // integer_text(row_index) // &
                            " column " // integer_text(column_index) // " missing widget")
      return
    end if
    if (column_value%table_values(widget_index)%kind /= TOML_KIND_STRING) then
      call set_layout_error(error, "layout row " // integer_text(row_index) // &
                            " column " // integer_text(column_index) // " widget must be a string")
      return
    end if

    column%widget = trim(column_value%table_values(widget_index)%string_value%text)
    if (len(column%widget) == 0) then
      call set_layout_error(error, "layout row " // integer_text(row_index) // &
                            " column " // integer_text(column_index) // " widget cannot be empty")
      return
    end if
    if (.not. layout_widget_registered(column%widget)) then
      call set_layout_error(error, "layout row " // integer_text(row_index) // &
                            " column " // integer_text(column_index) // &
                            " unknown widget " // column%widget)
      return
    end if
    column%weight = integer_field(column_value, "weight", 1, error)
    if (error%failed) return
    column%min_size%width = integer_field(column_value, "min_width", 0, error)
    if (error%failed) return
    column%min_size%height = integer_field(column_value, "min_height", 0, error)
  end subroutine parse_column

  function resolve_layout(grid, rect) result(assignments)
    type(layout_grid), intent(in) :: grid
    type(widget_rect), intent(in) :: rect
    type(layout_assignment), allocatable :: assignments(:)
    integer, allocatable :: column_mins(:)
    integer, allocatable :: column_weights(:)
    integer, allocatable :: column_widths(:)
    integer, allocatable :: row_heights(:)
    integer, allocatable :: row_mins(:)
    integer, allocatable :: row_weights(:)
    integer :: assignment_count
    integer :: assignment_index
    integer :: col
    integer :: col_left
    integer :: row
    integer :: row_top

    assignment_count = layout_assignment_count(grid)
    allocate(assignments(assignment_count))
    if (assignment_count <= 0 .or. rect%width <= 0 .or. rect%height <= 0) return
    if (.not. allocated(grid%rows)) return

    allocate(row_weights(size(grid%rows)))
    allocate(row_mins(size(grid%rows)))
    do row = 1, size(grid%rows)
      row_weights(row) = max(1, grid%rows(row)%weight)
      row_mins(row) = row_min_height(grid%rows(row))
    end do
    row_heights = distribute_weighted_space(rect%height, row_weights, row_mins)

    row_top = rect%row
    assignment_index = 0
    do row = 1, size(grid%rows)
      if (.not. allocated(grid%rows(row)%columns)) cycle
      allocate(column_weights(size(grid%rows(row)%columns)))
      allocate(column_mins(size(grid%rows(row)%columns)))
      do col = 1, size(grid%rows(row)%columns)
        column_weights(col) = max(1, grid%rows(row)%columns(col)%weight)
        column_mins(col) = max(0, grid%rows(row)%columns(col)%min_size%width)
      end do
      column_widths = distribute_weighted_space(rect%width, column_weights, column_mins)

      col_left = rect%col
      do col = 1, size(grid%rows(row)%columns)
        assignment_index = assignment_index + 1
        assignments(assignment_index)%widget = grid%rows(row)%columns(col)%widget
        assignments(assignment_index)%rect = widget_rect(row_top, col_left, column_widths(col), row_heights(row))
        col_left = col_left + column_widths(col)
      end do

      row_top = row_top + row_heights(row)
      deallocate(column_weights)
      deallocate(column_mins)
      if (allocated(column_widths)) deallocate(column_widths)
    end do
  end function resolve_layout

  function distribute_weighted_space(total, weights, minimums) result(sizes)
    integer, intent(in) :: total
    integer, intent(in) :: weights(:)
    integer, intent(in) :: minimums(:)
    integer, allocatable :: sizes(:)
    logical, allocatable :: used_remainder(:)
    real, allocatable :: fractions(:)
    real :: share
    integer :: best_index
    integer :: item
    integer :: remaining
    integer :: total_weight

    allocate(sizes(size(weights)))
    if (size(weights) == 0) return
    if (size(minimums) /= size(weights)) then
      sizes = 0
      return
    end if

    sizes = max(0, minimums)
    if (total <= 0) then
      sizes = 0
      return
    end if
    if (sum(sizes) >= total) return

    remaining = total - sum(sizes)
    total_weight = sum(max(1, weights))
    allocate(fractions(size(weights)))
    allocate(used_remainder(size(weights)))
    fractions = 0.0
    used_remainder = .false.

    do item = 1, size(weights)
      share = real(remaining * max(1, weights(item))) / real(total_weight)
      sizes(item) = sizes(item) + int(share)
      fractions(item) = share - real(int(share))
    end do

    remaining = total - sum(sizes)
    do while (remaining > 0)
      best_index = best_remainder_index(fractions, used_remainder)
      sizes(best_index) = sizes(best_index) + 1
      used_remainder(best_index) = .true.
      remaining = remaining - 1
    end do
  end function distribute_weighted_space

  integer function best_remainder_index(fractions, used_remainder) result(index)
    real, intent(in) :: fractions(:)
    logical, intent(in) :: used_remainder(:)
    integer :: item

    index = 1
    do item = 1, size(fractions)
      if (used_remainder(item)) cycle
      if (used_remainder(index) .or. fractions(item) > fractions(index)) index = item
    end do
  end function best_remainder_index

  integer function layout_assignment_count(grid) result(count)
    type(layout_grid), intent(in) :: grid
    integer :: row

    count = 0
    if (.not. allocated(grid%rows)) return
    do row = 1, size(grid%rows)
      if (allocated(grid%rows(row)%columns)) count = count + size(grid%rows(row)%columns)
    end do
  end function layout_assignment_count

  integer function row_min_height(row) result(height)
    type(layout_row), intent(in) :: row
    integer :: col

    height = 0
    if (.not. allocated(row%columns)) return
    do col = 1, size(row%columns)
      height = max(height, row%columns(col)%min_size%height)
    end do
  end function row_min_height

  logical function layout_widget_registered(widget) result(registered)
    character(len=*), intent(in) :: widget

    select case (trim(widget))
    case (LAYOUT_WIDGET_CPU, LAYOUT_WIDGET_MEMORY, LAYOUT_WIDGET_PROCESS)
      registered = .true.
    case default
      registered = .false.
    end select
  end function layout_widget_registered

  logical function layout_widget_renderable(widget) result(renderable)
    character(len=*), intent(in) :: widget

    select case (trim(widget))
    case (LAYOUT_WIDGET_CPU, LAYOUT_WIDGET_MEMORY, LAYOUT_WIDGET_PROCESS)
      renderable = .true.
    case default
      renderable = .false.
    end select
  end function layout_widget_renderable

  integer function layout_focus_count(grid) result(count)
    type(layout_grid), intent(in) :: grid
    integer :: col
    integer :: row

    count = 0
    if (.not. allocated(grid%rows)) return
    do row = 1, size(grid%rows)
      if (.not. allocated(grid%rows(row)%columns)) cycle
      do col = 1, size(grid%rows(row)%columns)
        if (layout_widget_renderable(grid%rows(row)%columns(col)%widget)) count = count + 1
      end do
    end do
  end function layout_focus_count

  function layout_focus_widget(grid, focus_index) result(widget)
    type(layout_grid), intent(in) :: grid
    integer, intent(in) :: focus_index
    character(len=:), allocatable :: widget
    integer :: col
    integer :: current
    integer :: row
    integer :: target

    widget = ""
    target = max(1, focus_index)
    current = 0
    if (.not. allocated(grid%rows)) return
    do row = 1, size(grid%rows)
      if (.not. allocated(grid%rows(row)%columns)) cycle
      do col = 1, size(grid%rows(row)%columns)
        if (.not. layout_widget_renderable(grid%rows(row)%columns(col)%widget)) cycle
        current = current + 1
        if (current == target) then
          widget = grid%rows(row)%columns(col)%widget
          return
        end if
      end do
    end do
  end function layout_focus_widget

  integer function integer_field(table_value, key, default_value, error) result(value)
    type(toml_value), intent(in) :: table_value
    character(len=*), intent(in) :: key
    integer, intent(in) :: default_value
    type(layout_error), intent(inout) :: error
    integer :: index_value

    value = default_value
    index_value = table_entry_index(table_value, key)
    if (index_value == 0) return
    if (table_value%table_values(index_value)%kind /= TOML_KIND_INTEGER) then
      call set_layout_error(error, "layout field " // trim(key) // " must be an integer")
      return
    end if
    value = max(0, int(min(table_value%table_values(index_value)%integer_value, int(huge(value), int64))))
    if (key == "weight") value = max(1, value)
  end function integer_field

  integer function table_entry_index(table_value, key) result(index_value)
    type(toml_value), intent(in) :: table_value
    character(len=*), intent(in) :: key
    integer :: item

    index_value = 0
    if (table_value%kind /= TOML_KIND_TABLE) return
    if (.not. allocated(table_value%table_keys)) return
    do item = 1, size(table_value%table_keys)
      if (allocated(table_value%table_keys(item)%text)) then
        if (table_value%table_keys(item)%text == key) then
          index_value = item
          return
        end if
      end if
    end do
  end function table_entry_index

  subroutine clear_layout_error(error)
    type(layout_error), intent(out) :: error

    error%failed = .false.
    error%line = 0
    error%column = 0
    error%message = ""
  end subroutine clear_layout_error

  subroutine set_layout_error(error, message, line, column)
    type(layout_error), intent(inout) :: error
    character(len=*), intent(in) :: message
    integer, intent(in), optional :: line
    integer, intent(in), optional :: column

    error%failed = .true.
    error%line = 0
    error%column = 0
    if (present(line)) error%line = line
    if (present(column)) error%column = column
    error%message = trim(message)
  end subroutine set_layout_error

  function integer_text(value) result(text)
    integer, intent(in) :: value
    character(len=:), allocatable :: text
    character(len=32) :: scratch

    write(scratch, '(i0)') value
    text = trim(scratch)
  end function integer_text

end module ftop_layout
