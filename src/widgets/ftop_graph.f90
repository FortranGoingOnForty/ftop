module ftop_graph
  use fgof_screen, only : clear_screen_style, put_glyph
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_color, only : color_gradient, style_with_gradient
  use ftop_text, only : TEXT_ALIGN_RIGHT, render_text
  use ftop_widgets, only : widget, widget_rect, widget_size
  implicit none
  private

  integer, parameter :: BRAILLE_BASE = int(z'2800')
  integer, parameter :: GRAPH_AXIS_WIDTH = 7

  type, public :: graph_series
    real, allocatable :: values(:)
    type(screen_style) :: style
    type(color_gradient) :: gradient
  end type graph_series

  type, extends(widget), public :: graph_widget
    type(graph_series), allocatable :: series(:)
    integer :: width = 0
    integer :: height = 0
    logical :: autoscale = .true.
    real :: min_value = 0.0
    real :: max_value = 1.0
    logical :: area_fill = .true.
    logical :: show_grid = .false.
    logical :: show_y_axis = .false.
    type(screen_style) :: grid_style
    type(screen_style) :: axis_style
  contains
    procedure :: render => graph_widget_render
    procedure :: min_size => graph_widget_min_size
  end type graph_widget

  public :: braille_dot_bit
  public :: braille_pattern_glyph
  public :: graph_value_dot_row
  public :: render_graph
  public :: render_graph_series

contains

  subroutine render_graph(buffer, rect, values, gradient, min_value, max_value, area_fill, &
                          show_grid, show_y_axis, style)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    real, intent(in) :: values(:)
    type(color_gradient), intent(in), optional :: gradient
    real, intent(in), optional :: min_value
    real, intent(in), optional :: max_value
    logical, intent(in), optional :: area_fill
    logical, intent(in), optional :: show_grid
    logical, intent(in), optional :: show_y_axis
    type(screen_style), intent(in), optional :: style
    type(graph_series) :: series(1)

    series(1)%values = values
    if (present(style)) series(1)%style = style
    if (present(gradient)) series(1)%gradient = gradient
    call render_graph_series(buffer, rect, series, min_value=min_value, max_value=max_value, &
                             area_fill=area_fill, show_grid=show_grid, show_y_axis=show_y_axis)
  end subroutine render_graph

  subroutine render_graph_series(buffer, rect, series, min_value, max_value, area_fill, &
                                 show_grid, show_y_axis, grid_style, axis_style)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    type(graph_series), intent(in) :: series(:)
    real, intent(in), optional :: min_value
    real, intent(in), optional :: max_value
    logical, intent(in), optional :: area_fill
    logical, intent(in), optional :: show_grid
    logical, intent(in), optional :: show_y_axis
    type(screen_style), intent(in), optional :: grid_style
    type(screen_style), intent(in), optional :: axis_style
    integer, allocatable :: patterns(:, :)
    type(screen_style), allocatable :: styles(:, :)
    type(screen_style) :: active_axis_style
    type(screen_style) :: active_grid_style
    type(widget_rect) :: plot_rect
    integer :: col
    integer :: row
    logical :: actual_area_fill
    logical :: actual_show_grid
    logical :: actual_show_y_axis
    real :: actual_max
    real :: actual_min

    if (rect%width <= 0 .or. rect%height <= 0) return
    if (buffer%size%width <= 0 .or. buffer%size%height <= 0) return

    actual_area_fill = .true.
    actual_show_grid = .false.
    actual_show_y_axis = .false.
    if (present(area_fill)) actual_area_fill = area_fill
    if (present(show_grid)) actual_show_grid = show_grid
    if (present(show_y_axis)) actual_show_y_axis = show_y_axis

    active_grid_style = default_grid_style()
    active_axis_style = clear_screen_style()
    if (present(grid_style)) active_grid_style = grid_style
    if (present(axis_style)) active_axis_style = axis_style

    plot_rect = graph_plot_rect(rect, actual_show_y_axis)
    if (plot_rect%width <= 0 .or. plot_rect%height <= 0) return

    call graph_bounds(series, actual_min, actual_max)
    if (present(min_value)) actual_min = min_value
    if (present(max_value)) actual_max = max_value

    allocate(patterns(plot_rect%height, plot_rect%width))
    allocate(styles(plot_rect%height, plot_rect%width))
    patterns = 0
    styles = clear_screen_style()

    if (actual_show_y_axis) call draw_y_axis(buffer, rect, actual_min, actual_max, active_axis_style)
    if (actual_show_grid) call seed_grid(patterns, styles, active_grid_style)

    do row = 1, size(series)
      call add_series(patterns, styles, series(row), actual_min, actual_max, actual_area_fill)
    end do

    do row = 1, plot_rect%height
      do col = 1, plot_rect%width
        if (patterns(row, col) == 0) then
          call put_glyph(buffer, plot_rect%row + row - 1, plot_rect%col + col - 1, " ", styles(row, col))
        else
          call put_glyph(buffer, plot_rect%row + row - 1, plot_rect%col + col - 1, &
                         braille_pattern_glyph(patterns(row, col)), styles(row, col))
        end if
      end do
    end do
  end subroutine render_graph_series

  integer function braille_dot_bit(dot_col, dot_row) result(bit)
    integer, intent(in) :: dot_col
    integer, intent(in) :: dot_row

    if (dot_col == 1) then
      select case (dot_row)
      case (1)
        bit = 0
      case (2)
        bit = 1
      case (3)
        bit = 2
      case (4)
        bit = 6
      case default
        bit = -1
      end select
    else if (dot_col == 2) then
      select case (dot_row)
      case (1)
        bit = 3
      case (2)
        bit = 4
      case (3)
        bit = 5
      case (4)
        bit = 7
      case default
        bit = -1
      end select
    else
      bit = -1
    end if
  end function braille_dot_bit

  function braille_pattern_glyph(pattern) result(glyph)
    integer, intent(in) :: pattern
    character(len=:), allocatable :: glyph

    glyph = utf8_codepoint(BRAILLE_BASE + max(0, min(255, pattern)))
  end function braille_pattern_glyph

  integer function graph_value_dot_row(value, min_value, max_value, total_dots) result(dot_row)
    real, intent(in) :: value
    real, intent(in) :: min_value
    real, intent(in) :: max_value
    integer, intent(in) :: total_dots
    real :: normalized

    if (total_dots <= 1) then
      dot_row = 1
    else if (max_value <= min_value) then
      dot_row = total_dots
    else
      normalized = max(0.0, min(1.0, (value - min_value) / (max_value - min_value)))
      dot_row = total_dots - nint(normalized * real(total_dots - 1))
      dot_row = max(1, min(total_dots, dot_row))
    end if
  end function graph_value_dot_row

  subroutine graph_widget_render(self, buffer, rect)
    class(graph_widget), intent(inout) :: self
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    type(widget_rect) :: target_rect

    if (.not. self%visible) return
    if (.not. allocated(self%series)) return

    target_rect = rect
    if (self%width > 0) target_rect%width = min(rect%width, self%width)
    if (self%height > 0) target_rect%height = min(rect%height, self%height)

    if (self%autoscale) then
      call render_graph_series(buffer, target_rect, self%series, area_fill=self%area_fill, &
                               show_grid=self%show_grid, show_y_axis=self%show_y_axis, &
                               grid_style=self%grid_style, axis_style=self%axis_style)
    else
      call render_graph_series(buffer, target_rect, self%series, min_value=self%min_value, &
                               max_value=self%max_value, area_fill=self%area_fill, &
                               show_grid=self%show_grid, show_y_axis=self%show_y_axis, &
                               grid_style=self%grid_style, axis_style=self%axis_style)
    end if
  end subroutine graph_widget_render

  function graph_widget_min_size(self) result(size_value)
    class(graph_widget), intent(in) :: self
    type(widget_size) :: size_value

    size_value%width = max(1, self%width)
    size_value%height = max(1, self%height)
    if (allocated(self%series) .and. self%width <= 0) then
      size_value%width = max(1, max_series_samples(self%series) / 2)
    end if
    if (self%show_y_axis) size_value%width = size_value%width + GRAPH_AXIS_WIDTH
  end function graph_widget_min_size

  subroutine add_series(patterns, styles, series, min_value, max_value, area_fill)
    integer, intent(inout) :: patterns(:, :)
    type(screen_style), intent(inout) :: styles(:, :)
    type(graph_series), intent(in) :: series
    real, intent(in) :: min_value
    real, intent(in) :: max_value
    logical, intent(in) :: area_fill
    integer :: dot_col
    integer :: dot_row
    integer :: first
    integer :: plot_width
    integer :: sample_count
    integer :: sample_index
    integer :: total_dots

    if (.not. allocated(series%values)) return
    if (size(series%values) <= 0) return

    plot_width = size(patterns, 2)
    total_dots = size(patterns, 1) * 4
    sample_count = min(size(series%values), plot_width * 2)
    first = size(series%values) - sample_count + 1

    do sample_index = 1, sample_count
      dot_col = mod(sample_index - 1, 2) + 1
      dot_row = graph_value_dot_row(series%values(first + sample_index - 1), &
                                    min_value, max_value, total_dots)
      if (area_fill) then
        call set_area_column(patterns, styles, series, sample_index, dot_col, dot_row, total_dots)
      else
        call set_graph_dot(patterns, styles, series, sample_index, dot_col, dot_row)
      end if
    end do
  end subroutine add_series

  subroutine set_area_column(patterns, styles, series, sample_index, dot_col, first_dot_row, total_dots)
    integer, intent(inout) :: patterns(:, :)
    type(screen_style), intent(inout) :: styles(:, :)
    type(graph_series), intent(in) :: series
    integer, intent(in) :: sample_index
    integer, intent(in) :: dot_col
    integer, intent(in) :: first_dot_row
    integer, intent(in) :: total_dots
    integer :: fill_dot_row

    do fill_dot_row = first_dot_row, total_dots
      call set_graph_dot(patterns, styles, series, sample_index, dot_col, fill_dot_row)
    end do
  end subroutine set_area_column

  subroutine set_graph_dot(patterns, styles, series, sample_index, dot_col, global_dot_row)
    integer, intent(inout) :: patterns(:, :)
    type(screen_style), intent(inout) :: styles(:, :)
    type(graph_series), intent(in) :: series
    integer, intent(in) :: sample_index
    integer, intent(in) :: dot_col
    integer, intent(in) :: global_dot_row
    integer :: cell_col
    integer :: cell_row
    integer :: dot_bit
    integer :: local_dot_row

    cell_col = (sample_index - 1) / 2 + 1
    cell_row = (global_dot_row - 1) / 4 + 1
    local_dot_row = mod(global_dot_row - 1, 4) + 1
    if (cell_row < 1 .or. cell_row > size(patterns, 1)) return
    if (cell_col < 1 .or. cell_col > size(patterns, 2)) return

    dot_bit = braille_dot_bit(dot_col, local_dot_row)
    if (dot_bit < 0) return
    patterns(cell_row, cell_col) = ior(patterns(cell_row, cell_col), ishft(1, dot_bit))
    styles(cell_row, cell_col) = graph_cell_style(series, cell_row, size(patterns, 1))
  end subroutine set_graph_dot

  subroutine seed_grid(patterns, styles, grid_style)
    integer, intent(inout) :: patterns(:, :)
    type(screen_style), intent(inout) :: styles(:, :)
    type(screen_style), intent(in) :: grid_style
    integer :: col
    integer :: grid_row

    grid_row = max(1, min(4, 2))
    do col = 1, size(patterns, 2)
      patterns(1, col) = ior(patterns(1, col), ishft(1, braille_dot_bit(1, grid_row)))
      patterns(1, col) = ior(patterns(1, col), ishft(1, braille_dot_bit(2, grid_row)))
      styles(1, col) = grid_style
    end do
  end subroutine seed_grid

  function graph_cell_style(series, cell_row, height) result(style)
    type(graph_series), intent(in) :: series
    integer, intent(in) :: cell_row
    integer, intent(in) :: height
    type(screen_style) :: style
    real :: vertical_position

    style = series%style
    if (.not. allocated(series%gradient%stops)) return

    if (height <= 1) then
      vertical_position = 1.0
    else
      vertical_position = 1.0 - real(cell_row - 1) / real(height - 1)
    end if
    style = style_with_gradient(style, series%gradient, vertical_position)
  end function graph_cell_style

  subroutine draw_y_axis(buffer, rect, min_value, max_value, style)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    real, intent(in) :: min_value
    real, intent(in) :: max_value
    type(screen_style), intent(in) :: style
    integer :: row

    if (rect%width < GRAPH_AXIS_WIDTH .or. rect%height <= 0) return
    call render_text(buffer, widget_rect(rect%row, rect%col, GRAPH_AXIS_WIDTH - 1, 1), &
                     axis_label(max_value), style, TEXT_ALIGN_RIGHT)
    call render_text(buffer, widget_rect(rect%row + rect%height - 1, rect%col, GRAPH_AXIS_WIDTH - 1, 1), &
                     axis_label(min_value), style, TEXT_ALIGN_RIGHT)
    do row = rect%row, rect%row + rect%height - 1
      call put_glyph(buffer, row, rect%col + GRAPH_AXIS_WIDTH - 1, "│", style)
    end do
  end subroutine draw_y_axis

  function graph_plot_rect(rect, show_y_axis) result(plot_rect)
    type(widget_rect), intent(in) :: rect
    logical, intent(in) :: show_y_axis
    type(widget_rect) :: plot_rect

    plot_rect = rect
    if (show_y_axis) then
      plot_rect%col = rect%col + GRAPH_AXIS_WIDTH
      plot_rect%width = max(0, rect%width - GRAPH_AXIS_WIDTH)
    end if
  end function graph_plot_rect

  subroutine graph_bounds(series, min_value, max_value)
    type(graph_series), intent(in) :: series(:)
    real, intent(out) :: min_value
    real, intent(out) :: max_value
    integer :: i
    logical :: found

    min_value = 0.0
    max_value = 1.0
    found = .false.
    do i = 1, size(series)
      if (.not. allocated(series(i)%values)) cycle
      if (size(series(i)%values) <= 0) cycle
      if (.not. found) then
        min_value = minval(series(i)%values)
        max_value = maxval(series(i)%values)
        found = .true.
      else
        min_value = min(min_value, minval(series(i)%values))
        max_value = max(max_value, maxval(series(i)%values))
      end if
    end do
  end subroutine graph_bounds

  integer function max_series_samples(series) result(sample_count)
    type(graph_series), intent(in) :: series(:)
    integer :: i

    sample_count = 0
    do i = 1, size(series)
      if (allocated(series(i)%values)) sample_count = max(sample_count, size(series(i)%values))
    end do
  end function max_series_samples

  function default_grid_style() result(style)
    type(screen_style) :: style

    style = clear_screen_style()
    style%dim = .true.
  end function default_grid_style

  function axis_label(value) result(label)
    real, intent(in) :: value
    character(len=:), allocatable :: label
    character(len=16) :: scratch

    write(scratch, '(f6.1)') value
    label = trim(adjustl(scratch))
  end function axis_label

  function utf8_codepoint(codepoint) result(text)
    integer, intent(in) :: codepoint
    character(len=:), allocatable :: text

    if (codepoint <= int(z'7f')) then
      text = achar(codepoint)
    else if (codepoint <= int(z'7ff')) then
      text = achar(ior(int(z'c0'), ishft(codepoint, -6))) // &
             achar(ior(int(z'80'), iand(codepoint, int(z'3f'))))
    else if (codepoint <= int(z'ffff')) then
      text = achar(ior(int(z'e0'), ishft(codepoint, -12))) // &
             achar(ior(int(z'80'), iand(ishft(codepoint, -6), int(z'3f')))) // &
             achar(ior(int(z'80'), iand(codepoint, int(z'3f'))))
    else
      text = achar(ior(int(z'f0'), ishft(codepoint, -18))) // &
             achar(ior(int(z'80'), iand(ishft(codepoint, -12), int(z'3f')))) // &
             achar(ior(int(z'80'), iand(ishft(codepoint, -6), int(z'3f')))) // &
             achar(ior(int(z'80'), iand(codepoint, int(z'3f'))))
    end if
  end function utf8_codepoint

end module ftop_graph
