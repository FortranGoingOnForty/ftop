module ftop_layout
  use, intrinsic :: iso_fortran_env, only : int64
  use fgof_toml, only : &
    TOML_KIND_ARRAY, &
    TOML_KIND_BOOLEAN, &
    TOML_KIND_INTEGER, &
    TOML_KIND_STRING, &
    TOML_KIND_TABLE, &
    parse_file, &
    parse_string, &
    toml_array, &
    toml_document, &
    toml_error, &
    toml_value
  use ftop_net_data, only : &
    NET_INTERFACE_FILTER_CAPACITY, &
    NET_INTERFACE_PATTERN_LEN, &
    network_interface_filters
  use ftop_widgets, only : widget_rect, widget_size
  implicit none
  private

  character(len=*), parameter, public :: LAYOUT_WIDGET_CPU = "cpu"
  character(len=*), parameter, public :: LAYOUT_WIDGET_MEMORY = "memory"
  character(len=*), parameter, public :: LAYOUT_WIDGET_NETWORK = "network"
  character(len=*), parameter, public :: LAYOUT_WIDGET_DISK = "disk"
  character(len=*), parameter, public :: LAYOUT_WIDGET_PROCESS = "process"
  integer, parameter, public :: LAYOUT_DIRECTION_UP = 1
  integer, parameter, public :: LAYOUT_DIRECTION_DOWN = 2
  integer, parameter, public :: LAYOUT_DIRECTION_LEFT = 3
  integer, parameter, public :: LAYOUT_DIRECTION_RIGHT = 4
  integer, parameter, public :: LAYOUT_PROCESS_COLUMN_CAPACITY = 12
  integer, parameter, public :: LAYOUT_PROCESS_COLUMN_NAME_LEN = 16

  type, public :: layout_process_config
    integer :: column_count = 0
    character(len=LAYOUT_PROCESS_COLUMN_NAME_LEN) :: columns(LAYOUT_PROCESS_COLUMN_CAPACITY) = ""
  end type layout_process_config

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
    type(network_interface_filters) :: network
    type(layout_process_config) :: process
  end type layout_grid

  type, public :: layout_assignment
    character(len=:), allocatable :: widget
    type(widget_rect) :: rect
  end type layout_assignment

  type, public :: dashboard_layout
    type(widget_rect) :: frame
    type(widget_rect) :: cpu_panel
    type(widget_rect) :: memory_panel
    type(widget_rect) :: network_panel
    type(widget_rect) :: disk_panel
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
  public :: layout_directional_focus_widget
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

    grid = default_dashboard_grid(stacked=width < 96)
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
      case (LAYOUT_WIDGET_NETWORK)
        layout%network_panel = assignments(item)%rect
      case (LAYOUT_WIDGET_DISK)
        layout%disk_panel = assignments(item)%rect
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
    layout%network_panel = widget_rect(0, 0, 0, 0)
    layout%disk_panel = widget_rect(0, 0, 0, 0)
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
      allocate(grid%rows(5))
      grid%rows(1) = make_layout_row(1, [make_layout_column(LAYOUT_WIDGET_CPU, 1, 24, 3)])
      grid%rows(2) = make_layout_row(1, [make_layout_column(LAYOUT_WIDGET_MEMORY, 1, 24, 3)])
      grid%rows(3) = make_layout_row(1, [make_layout_column(LAYOUT_WIDGET_NETWORK, 1, 24, 3)])
      grid%rows(4) = make_layout_row(1, [make_layout_column(LAYOUT_WIDGET_DISK, 1, 24, 3)])
      grid%rows(5) = make_layout_row(2, [make_layout_column(LAYOUT_WIDGET_PROCESS, 1, 40, 5)])
    else
      allocate(grid%rows(2))
      grid%rows(1) = make_layout_row(1, [ &
        make_layout_column(LAYOUT_WIDGET_CPU, 1, 28, 9), &
        make_layout_column(LAYOUT_WIDGET_MEMORY, 1, 28, 8), &
        make_layout_column(LAYOUT_WIDGET_NETWORK, 1, 28, 8), &
        make_layout_column(LAYOUT_WIDGET_DISK, 1, 28, 6) &
      ])
      grid%rows(2) = make_layout_row(2, [make_layout_column(LAYOUT_WIDGET_PROCESS, 1, 40, 8)])
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

    call parse_network_config(document, grid%network, error)
    if (error%failed) return
    call parse_process_config(document, grid%process, error)
  end subroutine parse_layout_document

  subroutine parse_network_config(document, config, error)
    type(toml_document), intent(in) :: document
    type(network_interface_filters), intent(out) :: config
    type(layout_error), intent(inout) :: error
    integer :: exclude_index
    integer :: network_index
    integer :: pattern_index

    config = network_interface_filters()
    network_index = table_entry_index(document%root, "network")
    if (network_index == 0) return
    if (document%root%table_values(network_index)%kind /= TOML_KIND_TABLE) then
      call set_layout_error(error, "network config must be a table")
      return
    end if

    associate (network_value => document%root%table_values(network_index))
      config%include_loopback = boolean_field(network_value, "include_loopback", .false., error)
      if (error%failed) return

      exclude_index = table_entry_index(network_value, "exclude_interfaces")
      if (exclude_index == 0) return
      if (network_value%table_values(exclude_index)%kind /= TOML_KIND_ARRAY) then
        call set_layout_error(error, "network exclude_interfaces must be an array")
        return
      end if
      if (.not. allocated(network_value%table_values(exclude_index)%array_values)) return
      if (size(network_value%table_values(exclude_index)%array_values) > NET_INTERFACE_FILTER_CAPACITY) then
        call set_layout_error(error, "network exclude_interfaces cannot contain more than " // &
                              integer_text(NET_INTERFACE_FILTER_CAPACITY) // " patterns")
        return
      end if

      config%exclude_count = size(network_value%table_values(exclude_index)%array_values)
      do pattern_index = 1, config%exclude_count
        if (network_value%table_values(exclude_index)%array_values(pattern_index)%kind /= TOML_KIND_STRING) then
          call set_layout_error(error, "network exclude_interfaces pattern " // integer_text(pattern_index) // &
                                " must be a string")
          return
        end if
        config%exclude_patterns(pattern_index) = bounded_text(&
          network_value%table_values(exclude_index)%array_values(pattern_index)%string_value%text, &
          NET_INTERFACE_PATTERN_LEN)
        if (len_trim(config%exclude_patterns(pattern_index)) == 0) then
          call set_layout_error(error, "network exclude_interfaces pattern " // integer_text(pattern_index) // &
                                " cannot be empty")
          return
        end if
      end do
    end associate
  end subroutine parse_network_config

  subroutine parse_process_config(document, config, error)
    type(toml_document), intent(in) :: document
    type(layout_process_config), intent(out) :: config
    type(layout_error), intent(inout) :: error
    integer :: column_index
    integer :: columns_index
    integer :: process_index

    config = layout_process_config()
    process_index = table_entry_index(document%root, "process")
    if (process_index == 0) return
    if (document%root%table_values(process_index)%kind /= TOML_KIND_TABLE) then
      call set_layout_error(error, "process config must be a table")
      return
    end if

    associate (process_value => document%root%table_values(process_index))
      columns_index = table_entry_index(process_value, "columns")
      if (columns_index == 0) return
      if (process_value%table_values(columns_index)%kind /= TOML_KIND_ARRAY) then
        call set_layout_error(error, "process columns must be an array")
        return
      end if
      if (.not. allocated(process_value%table_values(columns_index)%array_values)) then
        call set_layout_error(error, "process columns must contain at least one column")
        return
      end if
      if (size(process_value%table_values(columns_index)%array_values) <= 0) then
        call set_layout_error(error, "process columns must contain at least one column")
        return
      end if
      if (size(process_value%table_values(columns_index)%array_values) > LAYOUT_PROCESS_COLUMN_CAPACITY) then
        call set_layout_error(error, "process columns cannot contain more than " // &
                              integer_text(LAYOUT_PROCESS_COLUMN_CAPACITY) // " columns")
        return
      end if

      config%column_count = size(process_value%table_values(columns_index)%array_values)
      do column_index = 1, config%column_count
        if (process_value%table_values(columns_index)%array_values(column_index)%kind /= TOML_KIND_STRING) then
          call set_layout_error(error, "process column " // integer_text(column_index) // " must be a string")
          return
        end if
        config%columns(column_index) = &
          trim(process_value%table_values(columns_index)%array_values(column_index)%string_value%text)
        if (len_trim(config%columns(column_index)) == 0) then
          call set_layout_error(error, "process column " // integer_text(column_index) // " cannot be empty")
          return
        end if
      end do
    end associate
  end subroutine parse_process_config

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
    case (LAYOUT_WIDGET_CPU, LAYOUT_WIDGET_MEMORY, LAYOUT_WIDGET_NETWORK, LAYOUT_WIDGET_DISK, LAYOUT_WIDGET_PROCESS)
      registered = .true.
    case default
      registered = .false.
    end select
  end function layout_widget_registered

  logical function layout_widget_renderable(widget) result(renderable)
    character(len=*), intent(in) :: widget

    select case (trim(widget))
    case (LAYOUT_WIDGET_CPU, LAYOUT_WIDGET_MEMORY, LAYOUT_WIDGET_NETWORK, LAYOUT_WIDGET_DISK, LAYOUT_WIDGET_PROCESS)
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

  function layout_directional_focus_widget(grid, viewport, current_widget, direction) result(widget)
    type(layout_grid), intent(in) :: grid
    type(widget_rect), intent(in) :: viewport
    character(len=*), intent(in) :: current_widget
    integer, intent(in) :: direction
    character(len=:), allocatable :: widget
    type(layout_assignment), allocatable :: assignments(:)
    integer :: candidate
    integer :: best_candidate
    integer :: best_center_delta
    integer :: best_cross_gap
    integer :: best_primary_gap
    integer :: center_delta
    integer :: cross_gap
    integer :: primary_gap
    integer :: source

    widget = ""
    if (viewport%width <= 0 .or. viewport%height <= 0) return

    assignments = resolve_layout(grid, viewport)
    if (.not. allocated(assignments)) return
    source = layout_assignment_index(assignments, current_widget)
    if (source <= 0) return

    best_candidate = 0
    best_cross_gap = huge(best_cross_gap)
    best_primary_gap = huge(best_primary_gap)
    best_center_delta = huge(best_center_delta)
    do candidate = 1, size(assignments)
      if (candidate == source) cycle
      if (assignments(candidate)%rect%width <= 0 .or. assignments(candidate)%rect%height <= 0) cycle
      if (.not. layout_direction_candidate(assignments(source)%rect, assignments(candidate)%rect, direction)) cycle

      primary_gap = layout_primary_gap(assignments(source)%rect, assignments(candidate)%rect, direction)
      cross_gap = layout_cross_gap(assignments(source)%rect, assignments(candidate)%rect, direction)
      center_delta = layout_cross_center_delta(assignments(source)%rect, assignments(candidate)%rect, direction)
      if (layout_direction_candidate_is_better(primary_gap, cross_gap, center_delta, &
                                               best_primary_gap, best_cross_gap, best_center_delta)) then
        best_candidate = candidate
        best_primary_gap = primary_gap
        best_cross_gap = cross_gap
        best_center_delta = center_delta
      end if
    end do

    if (best_candidate > 0) widget = assignments(best_candidate)%widget
  end function layout_directional_focus_widget

  integer function layout_assignment_index(assignments, widget) result(index_value)
    type(layout_assignment), intent(in) :: assignments(:)
    character(len=*), intent(in) :: widget
    integer :: item

    index_value = 0
    if (len_trim(widget) <= 0) return
    do item = 1, size(assignments)
      if (assignments(item)%widget == trim(widget)) then
        index_value = item
        return
      end if
    end do
  end function layout_assignment_index

  logical function layout_direction_candidate(source, candidate, direction) result(candidate_valid)
    type(widget_rect), intent(in) :: source
    type(widget_rect), intent(in) :: candidate
    integer, intent(in) :: direction

    select case (direction)
    case (LAYOUT_DIRECTION_UP)
      candidate_valid = layout_rect_bottom(candidate) < source%row
    case (LAYOUT_DIRECTION_DOWN)
      candidate_valid = candidate%row > layout_rect_bottom(source)
    case (LAYOUT_DIRECTION_LEFT)
      candidate_valid = layout_rect_right(candidate) < source%col
    case (LAYOUT_DIRECTION_RIGHT)
      candidate_valid = candidate%col > layout_rect_right(source)
    case default
      candidate_valid = .false.
    end select
  end function layout_direction_candidate

  integer function layout_primary_gap(source, candidate, direction) result(gap)
    type(widget_rect), intent(in) :: source
    type(widget_rect), intent(in) :: candidate
    integer, intent(in) :: direction

    select case (direction)
    case (LAYOUT_DIRECTION_UP)
      gap = max(0, source%row - layout_rect_bottom(candidate))
    case (LAYOUT_DIRECTION_DOWN)
      gap = max(0, candidate%row - layout_rect_bottom(source))
    case (LAYOUT_DIRECTION_LEFT)
      gap = max(0, source%col - layout_rect_right(candidate))
    case (LAYOUT_DIRECTION_RIGHT)
      gap = max(0, candidate%col - layout_rect_right(source))
    case default
      gap = huge(gap)
    end select
  end function layout_primary_gap

  integer function layout_cross_gap(source, candidate, direction) result(gap)
    type(widget_rect), intent(in) :: source
    type(widget_rect), intent(in) :: candidate
    integer, intent(in) :: direction

    select case (direction)
    case (LAYOUT_DIRECTION_UP, LAYOUT_DIRECTION_DOWN)
      gap = layout_span_gap(source%col, layout_rect_right(source), candidate%col, layout_rect_right(candidate))
    case (LAYOUT_DIRECTION_LEFT, LAYOUT_DIRECTION_RIGHT)
      gap = layout_span_gap(source%row, layout_rect_bottom(source), candidate%row, layout_rect_bottom(candidate))
    case default
      gap = huge(gap)
    end select
  end function layout_cross_gap

  integer function layout_cross_center_delta(source, candidate, direction) result(delta)
    type(widget_rect), intent(in) :: source
    type(widget_rect), intent(in) :: candidate
    integer, intent(in) :: direction

    select case (direction)
    case (LAYOUT_DIRECTION_UP, LAYOUT_DIRECTION_DOWN)
      delta = abs(layout_rect_center_col2(source) - layout_rect_center_col2(candidate))
    case (LAYOUT_DIRECTION_LEFT, LAYOUT_DIRECTION_RIGHT)
      delta = abs(layout_rect_center_row2(source) - layout_rect_center_row2(candidate))
    case default
      delta = huge(delta)
    end select
  end function layout_cross_center_delta

  logical function layout_direction_candidate_is_better(primary_gap, cross_gap, center_delta, &
                                                        best_primary_gap, best_cross_gap, &
                                                        best_center_delta) result(better)
    integer, intent(in) :: primary_gap
    integer, intent(in) :: cross_gap
    integer, intent(in) :: center_delta
    integer, intent(in) :: best_primary_gap
    integer, intent(in) :: best_cross_gap
    integer, intent(in) :: best_center_delta

    better = .false.
    if (cross_gap < best_cross_gap) then
      better = .true.
    else if (cross_gap == best_cross_gap .and. primary_gap < best_primary_gap) then
      better = .true.
    else if (cross_gap == best_cross_gap .and. primary_gap == best_primary_gap .and. &
             center_delta < best_center_delta) then
      better = .true.
    end if
  end function layout_direction_candidate_is_better

  integer function layout_span_gap(left_start, left_end, right_start, right_end) result(gap)
    integer, intent(in) :: left_start
    integer, intent(in) :: left_end
    integer, intent(in) :: right_start
    integer, intent(in) :: right_end

    if (left_end < right_start) then
      gap = right_start - left_end
    else if (right_end < left_start) then
      gap = left_start - right_end
    else
      gap = 0
    end if
  end function layout_span_gap

  integer function layout_rect_right(rect) result(col)
    type(widget_rect), intent(in) :: rect

    col = rect%col + max(0, rect%width) - 1
  end function layout_rect_right

  integer function layout_rect_bottom(rect) result(row)
    type(widget_rect), intent(in) :: rect

    row = rect%row + max(0, rect%height) - 1
  end function layout_rect_bottom

  integer function layout_rect_center_col2(rect) result(col2)
    type(widget_rect), intent(in) :: rect

    col2 = 2 * rect%col + max(0, rect%width) - 1
  end function layout_rect_center_col2

  integer function layout_rect_center_row2(rect) result(row2)
    type(widget_rect), intent(in) :: rect

    row2 = 2 * rect%row + max(0, rect%height) - 1
  end function layout_rect_center_row2

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

  logical function boolean_field(table_value, key, default_value, error) result(value)
    type(toml_value), intent(in) :: table_value
    character(len=*), intent(in) :: key
    logical, intent(in) :: default_value
    type(layout_error), intent(inout) :: error
    integer :: index_value

    value = default_value
    index_value = table_entry_index(table_value, key)
    if (index_value == 0) return
    if (table_value%table_values(index_value)%kind /= TOML_KIND_BOOLEAN) then
      call set_layout_error(error, "layout field " // trim(key) // " must be a boolean")
      return
    end if
    value = table_value%table_values(index_value)%boolean_value
  end function boolean_field

  function bounded_text(value, capacity) result(text)
    character(len=*), intent(in) :: value
    integer, intent(in) :: capacity
    character(len=capacity) :: text
    integer :: copy_len

    text = ""
    copy_len = min(len_trim(value), capacity)
    if (copy_len > 0) text(:copy_len) = value(:copy_len)
  end function bounded_text

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
