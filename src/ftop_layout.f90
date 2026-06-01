module ftop_layout
  use ftop_widgets, only : widget_rect, widget_size
  implicit none
  private

  character(len=*), parameter, public :: LAYOUT_WIDGET_CPU = "cpu"
  character(len=*), parameter, public :: LAYOUT_WIDGET_MEMORY = "memory"

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
    type(widget_rect) :: footer
  end type dashboard_layout

  public :: default_dashboard_layout
  public :: default_dashboard_grid
  public :: distribute_weighted_space
  public :: make_layout_column
  public :: make_layout_row
  public :: resolve_layout

contains

  function default_dashboard_layout(width, height) result(layout)
    integer, intent(in) :: width
    integer, intent(in) :: height
    type(dashboard_layout) :: layout
    type(layout_assignment), allocatable :: assignments(:)
    type(layout_grid) :: grid
    type(widget_rect) :: body
    integer :: item

    layout%frame = widget_rect(1, 1, max(0, width), max(0, height))
    layout%cpu_panel = widget_rect(0, 0, 0, 0)
    layout%memory_panel = widget_rect(0, 0, 0, 0)
    layout%footer = widget_rect(max(1, height - 2), 3, max(0, width - 4), &
                                min(2, max(0, height - 2)))

    if (width < 8 .or. height < 6) return

    body = widget_rect(3, 3, max(0, width - 4), max(0, height - 5))
    if (body%width <= 0 .or. body%height <= 0) return

    grid = default_dashboard_grid(stacked=width < 72)
    assignments = resolve_layout(grid, body)
    do item = 1, size(assignments)
      select case (assignments(item)%widget)
      case (LAYOUT_WIDGET_CPU)
        layout%cpu_panel = assignments(item)%rect
      case (LAYOUT_WIDGET_MEMORY)
        layout%memory_panel = assignments(item)%rect
      end select
    end do
  end function default_dashboard_layout

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
        make_layout_column(LAYOUT_WIDGET_CPU, 1, 28, 8), &
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

end module ftop_layout
