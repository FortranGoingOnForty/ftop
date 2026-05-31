module ftop_sparkline
  use fgof_screen, only : clear_screen_style, put_glyph
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_color, only : color_gradient, gradient_color
  use ftop_widgets, only : widget, widget_rect, widget_size
  implicit none
  private

  type, extends(widget), public :: sparkline_widget
    real, allocatable :: values(:)
    integer :: width = 0
    logical :: autoscale = .true.
    real :: min_value = 0.0
    real :: max_value = 1.0
    type(color_gradient) :: gradient
    type(screen_style) :: style
  contains
    procedure :: render => sparkline_widget_render
    procedure :: min_size => sparkline_widget_min_size
  end type sparkline_widget

  public :: render_sparkline
  public :: sparkline_bucket
  public :: sparkline_glyph

contains

  subroutine render_sparkline(buffer, rect, values, gradient, min_value, max_value, style)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    real, intent(in) :: values(:)
    type(color_gradient), intent(in), optional :: gradient
    real, intent(in), optional :: min_value
    real, intent(in), optional :: max_value
    type(screen_style), intent(in), optional :: style
    type(screen_style) :: active_style
    character(len=:), allocatable :: glyph
    integer :: count
    integer :: first
    integer :: i
    real :: actual_max
    real :: actual_min
    real :: normalized

    if (rect%width <= 0 .or. rect%height <= 0) return
    if (buffer%size%width <= 0 .or. buffer%size%height <= 0) return
    if (rect%row < 1 .or. rect%row > buffer%size%height) return
    if (size(values) <= 0) return

    count = min(rect%width, size(values))
    first = size(values) - count + 1
    actual_min = minval(values(first:size(values)))
    actual_max = maxval(values(first:size(values)))
    if (present(min_value)) actual_min = min_value
    if (present(max_value)) actual_max = max_value

    do i = 1, count
      normalized = normalized_value(values(first + i - 1), actual_min, actual_max)
      glyph = sparkline_glyph(normalized)
      active_style = clear_screen_style()
      if (present(style)) active_style = style
      if (present(gradient)) call apply_gradient(active_style, gradient, normalized)
      call put_glyph(buffer, rect%row, rect%col + i - 1, glyph, active_style)
    end do
  end subroutine render_sparkline

  integer function sparkline_bucket(value) result(bucket)
    real, intent(in) :: value
    real :: clamped_value

    clamped_value = max(0.0, min(1.0, value))
    bucket = int(clamped_value * 7.0) + 1
    bucket = max(1, min(8, bucket))
  end function sparkline_bucket

  function sparkline_glyph(value) result(glyph)
    real, intent(in) :: value
    character(len=:), allocatable :: glyph

    select case (sparkline_bucket(value))
    case (1)
      glyph = "▁"
    case (2)
      glyph = "▂"
    case (3)
      glyph = "▃"
    case (4)
      glyph = "▄"
    case (5)
      glyph = "▅"
    case (6)
      glyph = "▆"
    case (7)
      glyph = "▇"
    case default
      glyph = "█"
    end select
  end function sparkline_glyph

  subroutine sparkline_widget_render(self, buffer, rect)
    class(sparkline_widget), intent(inout) :: self
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    type(widget_rect) :: target_rect

    if (.not. self%visible) return
    if (.not. allocated(self%values)) return

    target_rect = rect
    if (self%width > 0) target_rect%width = min(rect%width, self%width)
    if (self%autoscale) then
      call render_sparkline(buffer, target_rect, self%values, gradient=self%gradient, style=self%style)
    else
      call render_sparkline(buffer, target_rect, self%values, gradient=self%gradient, &
                            min_value=self%min_value, max_value=self%max_value, style=self%style)
    end if
  end subroutine sparkline_widget_render

  function sparkline_widget_min_size(self) result(size_value)
    class(sparkline_widget), intent(in) :: self
    type(widget_size) :: size_value

    size_value%height = 1
    if (self%width > 0) then
      size_value%width = self%width
    else if (allocated(self%values)) then
      size_value%width = max(1, size(self%values))
    else
      size_value%width = 1
    end if
  end function sparkline_widget_min_size

  subroutine apply_gradient(style, gradient, value)
    type(screen_style), intent(inout) :: style
    type(color_gradient), intent(in) :: gradient
    real, intent(in) :: value

    if (.not. allocated(gradient%stops)) return
    style%fg_truecolor = .true.
    associate(color => gradient_color(gradient, value))
      style%fg_rgb = [color%r, color%g, color%b]
    end associate
  end subroutine apply_gradient

  real function normalized_value(value, min_value, max_value) result(normalized)
    real, intent(in) :: value
    real, intent(in) :: min_value
    real, intent(in) :: max_value

    if (max_value <= min_value) then
      normalized = 0.0
    else
      normalized = max(0.0, min(1.0, (value - min_value) / (max_value - min_value)))
    end if
  end function normalized_value

end module ftop_sparkline
