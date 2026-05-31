module ftop_meter
  use fgof_screen, only : clear_screen_style, put_glyph
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_color, only : color_gradient, style_with_gradient
  use ftop_text, only : TEXT_ALIGN_CENTER, render_text
  use ftop_widgets, only : widget, widget_rect, widget_size
  implicit none
  private

  integer, parameter, public :: METER_FILL_BLOCK = 1
  integer, parameter, public :: METER_FILL_SHADED = 2

  type, extends(widget), public :: meter_widget
    real :: value = 0.0
    integer :: fill_mode = METER_FILL_BLOCK
    logical :: compact = .true.
    character(len=:), allocatable :: label
    type(color_gradient) :: gradient
    type(screen_style) :: fill_style
    type(screen_style) :: empty_style
    type(screen_style) :: label_style
  contains
    procedure :: render => meter_widget_render
    procedure :: min_size => meter_widget_min_size
  end type meter_widget

  public :: meter_fill_count
  public :: meter_shade_glyph
  public :: render_meter

contains

  subroutine render_meter(buffer, rect, value, gradient, label, compact, fill_mode, &
                          fill_style, empty_style, label_style)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    real, intent(in) :: value
    type(color_gradient), intent(in), optional :: gradient
    character(len=*), intent(in), optional :: label
    logical, intent(in), optional :: compact
    integer, intent(in), optional :: fill_mode
    type(screen_style), intent(in), optional :: fill_style
    type(screen_style), intent(in), optional :: empty_style
    type(screen_style), intent(in), optional :: label_style
    type(screen_style) :: active_empty_style
    type(screen_style) :: active_fill_style
    type(screen_style) :: active_label_style
    type(widget_rect) :: label_rect
    character(len=:), allocatable :: glyph
    integer :: actual_fill_mode
    integer :: bar_col
    integer :: bar_width
    integer :: fill_count
    integer :: i
    integer :: target_col
    logical :: actual_compact
    real :: cell_fraction
    real :: clamped_value

    if (rect%width <= 0 .or. rect%height <= 0) return
    if (buffer%size%width <= 0 .or. buffer%size%height <= 0) return
    if (rect%row < 1 .or. rect%row > buffer%size%height) return

    clamped_value = clamp_fraction(value)
    actual_compact = .true.
    if (present(compact)) actual_compact = compact
    actual_fill_mode = METER_FILL_BLOCK
    if (present(fill_mode)) actual_fill_mode = fill_mode

    active_fill_style = clear_screen_style()
    active_empty_style = clear_screen_style()
    active_label_style = clear_screen_style()
    if (present(fill_style)) active_fill_style = fill_style
    if (present(empty_style)) active_empty_style = empty_style
    if (present(label_style)) active_label_style = label_style
    if (present(gradient)) active_fill_style = style_with_gradient(active_fill_style, gradient, clamped_value)

    bar_col = rect%col
    bar_width = rect%width
    if (.not. actual_compact .and. rect%width > 2) then
      call put_glyph(buffer, rect%row, rect%col, "[", active_empty_style)
      call put_glyph(buffer, rect%row, rect%col + rect%width - 1, "]", active_empty_style)
      bar_col = rect%col + 1
      bar_width = rect%width - 2
    end if

    if (bar_width <= 0) return
    fill_count = meter_fill_count(clamped_value, bar_width)
    do i = 1, bar_width
      target_col = bar_col + i - 1
      select case (actual_fill_mode)
      case (METER_FILL_SHADED)
        cell_fraction = shaded_cell_fraction(clamped_value, i, bar_width)
        glyph = meter_shade_glyph(cell_fraction)
        if (cell_fraction > 0.0) then
          call put_glyph(buffer, rect%row, target_col, glyph, active_fill_style)
        else
          call put_glyph(buffer, rect%row, target_col, glyph, active_empty_style)
        end if
      case default
        if (i <= fill_count) then
          call put_glyph(buffer, rect%row, target_col, "█", active_fill_style)
        else
          call put_glyph(buffer, rect%row, target_col, "░", active_empty_style)
        end if
      end select
    end do

    if (present(label)) then
      label_rect = widget_rect(rect%row, rect%col, rect%width, 1)
      call render_text(buffer, label_rect, label, active_label_style, TEXT_ALIGN_CENTER)
    end if
  end subroutine render_meter

  integer function meter_fill_count(value, width) result(count)
    real, intent(in) :: value
    integer, intent(in) :: width

    if (width <= 0) then
      count = 0
    else
      count = nint(clamp_fraction(value) * real(width))
      count = max(0, min(width, count))
    end if
  end function meter_fill_count

  function meter_shade_glyph(value) result(glyph)
    real, intent(in) :: value
    character(len=:), allocatable :: glyph
    real :: clamped_value

    clamped_value = clamp_fraction(value)
    if (clamped_value <= 0.25) then
      glyph = "░"
    else if (clamped_value <= 0.50) then
      glyph = "▒"
    else if (clamped_value <= 0.75) then
      glyph = "▓"
    else
      glyph = "█"
    end if
  end function meter_shade_glyph

  subroutine meter_widget_render(self, buffer, rect)
    class(meter_widget), intent(inout) :: self
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect

    if (.not. self%visible) return
    if (allocated(self%label)) then
      call render_meter(buffer, rect, self%value, gradient=self%gradient, label=self%label, &
                        compact=self%compact, fill_mode=self%fill_mode, &
                        fill_style=self%fill_style, empty_style=self%empty_style, &
                        label_style=self%label_style)
    else
      call render_meter(buffer, rect, self%value, gradient=self%gradient, &
                        compact=self%compact, fill_mode=self%fill_mode, &
                        fill_style=self%fill_style, empty_style=self%empty_style, &
                        label_style=self%label_style)
    end if
  end subroutine meter_widget_render

  function meter_widget_min_size(self) result(size_value)
    class(meter_widget), intent(in) :: self
    type(widget_size) :: size_value
    integer :: label_width

    label_width = 0
    if (allocated(self%label)) label_width = len(self%label)
    size_value%height = 1
    if (self%compact) then
      size_value%width = max(1, label_width)
    else
      size_value%width = max(2, label_width + 2)
    end if
  end function meter_widget_min_size

  real function shaded_cell_fraction(value, cell_index, width) result(fraction)
    real, intent(in) :: value
    integer, intent(in) :: cell_index
    integer, intent(in) :: width

    if (width <= 0) then
      fraction = 0.0
    else
      fraction = clamp_fraction(clamp_fraction(value) * real(width) - real(cell_index - 1))
    end if
  end function shaded_cell_fraction

  real function clamp_fraction(value) result(clamped)
    real, intent(in) :: value

    clamped = max(0.0, min(1.0, value))
  end function clamp_fraction

end module ftop_meter
