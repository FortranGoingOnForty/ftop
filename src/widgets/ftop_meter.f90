module ftop_meter
  use fgof_screen, only : clear_screen_style, put_glyph
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_color, only : color_gradient, style_with_gradient
  use ftop_text, only : text_cell_width, truncated_text, utf8_glyph_bytes
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
  public :: meter_empty_cell_style
  public :: meter_label_cell_style
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
    type(screen_style) :: active_empty_cell_style
    type(screen_style) :: active_fill_style
    type(screen_style) :: active_label_style
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
    active_empty_cell_style = meter_empty_cell_style(active_empty_style)

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
          call put_glyph(buffer, rect%row, target_col, glyph, active_empty_cell_style)
        end if
      case default
        if (i <= fill_count) then
          call put_glyph(buffer, rect%row, target_col, "█", active_fill_style)
        else
          call put_glyph(buffer, rect%row, target_col, "░", active_empty_cell_style)
        end if
      end select
    end do

    if (present(label)) then
      call render_meter_label(buffer, rect, label, active_label_style, active_fill_style, active_empty_cell_style, &
                              actual_fill_mode, clamped_value, bar_col, bar_width)
    end if
  end subroutine render_meter

  subroutine render_meter_label(buffer, rect, label, label_style, fill_style, empty_style, fill_mode, &
                                value, bar_col, bar_width)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    character(len=*), intent(in) :: label
    type(screen_style), intent(in) :: label_style
    type(screen_style), intent(in) :: fill_style
    type(screen_style), intent(in) :: empty_style
    integer, intent(in) :: fill_mode
    real, intent(in) :: value
    integer, intent(in) :: bar_col
    integer, intent(in) :: bar_width
    character(len=:), allocatable :: clipped
    integer :: bar_index
    integer :: draw_col
    integer :: glyph_bytes
    integer :: glyph_index
    integer :: i
    integer :: label_width
    integer :: target_col
    logical :: filled

    clipped = truncated_text(label, rect%width)
    label_width = text_cell_width(clipped)
    if (label_width <= 0) return

    draw_col = rect%col + max(0, (rect%width - label_width) / 2)
    i = 1
    glyph_index = 0
    do while (i <= len(clipped))
      glyph_bytes = utf8_glyph_bytes(clipped, i)
      target_col = draw_col + glyph_index
      if (target_col > rect%col + rect%width - 1) exit
      if (target_col >= rect%col) then
        bar_index = target_col - bar_col + 1
        filled = meter_label_cell_filled(fill_mode, value, bar_index, bar_width)
        if (filled) then
          call put_glyph(buffer, rect%row, target_col, clipped(i:i + glyph_bytes - 1), &
                         meter_label_cell_style(label_style, fill_style, muted_background=.false.))
        else
          call put_glyph(buffer, rect%row, target_col, clipped(i:i + glyph_bytes - 1), &
                         meter_label_cell_style(label_style, empty_style, muted_background=.true.))
        end if
      end if
      i = i + glyph_bytes
      glyph_index = glyph_index + 1
    end do
  end subroutine render_meter_label

  logical function meter_label_cell_filled(fill_mode, value, bar_index, bar_width) result(filled)
    integer, intent(in) :: fill_mode
    real, intent(in) :: value
    integer, intent(in) :: bar_index
    integer, intent(in) :: bar_width

    filled = .false.
    if (bar_index < 1 .or. bar_index > bar_width) return

    select case (fill_mode)
    case (METER_FILL_SHADED)
      filled = shaded_cell_fraction(value, bar_index, bar_width) > 0.0
    case default
      filled = bar_index <= meter_fill_count(value, bar_width)
    end select
  end function meter_label_cell_filled

  function meter_label_cell_style(label_style, bar_style, muted_background) result(style)
    type(screen_style), intent(in) :: label_style
    type(screen_style), intent(in) :: bar_style
    logical, intent(in), optional :: muted_background
    type(screen_style) :: style
    logical :: has_background
    logical :: muted

    style = label_style
    muted = .false.
    if (present(muted_background)) muted = muted_background
    has_background = apply_bar_background(style, bar_style, muted)
    if (has_background) call apply_contrast_foreground(style)
  end function meter_label_cell_style

  function meter_empty_cell_style(empty_style) result(style)
    type(screen_style), intent(in) :: empty_style
    type(screen_style) :: style

    style = empty_style
    if (style%bg_truecolor .or. style%bg >= 0) return

    if (style%fg_truecolor) then
      style%bg_truecolor = .true.
      style%bg_rgb = muted_rgb(style%fg_rgb)
      style%bg = -1
    else if (style%fg >= 0) then
      style%bg = style%fg
      style%bg_truecolor = .false.
    end if
  end function meter_empty_cell_style

  logical function apply_bar_background(style, bar_style, muted) result(applied)
    type(screen_style), intent(inout) :: style
    type(screen_style), intent(in) :: bar_style
    logical, intent(in) :: muted

    applied = .true.
    if (bar_style%bg_truecolor) then
      style%bg_truecolor = .true.
      style%bg_rgb = bar_style%bg_rgb
      style%bg = -1
    else if (bar_style%fg_truecolor) then
      style%bg_truecolor = .true.
      if (muted) then
        style%bg_rgb = muted_rgb(bar_style%fg_rgb)
      else
        style%bg_rgb = bar_style%fg_rgb
      end if
      style%bg = -1
    else if (bar_style%bg >= 0) then
      style%bg = bar_style%bg
      style%bg_truecolor = .false.
    else if (bar_style%fg >= 0) then
      style%bg = bar_style%fg
      style%bg_truecolor = .false.
    else
      applied = .false.
    end if
  end function apply_bar_background

  function muted_rgb(color) result(muted)
    integer, intent(in) :: color(3)
    integer :: muted(3)
    integer :: channel

    do channel = 1, 3
      muted(channel) = max(0, min(255, nint(real(color(channel)) * 0.20)))
    end do
  end function muted_rgb

  subroutine apply_contrast_foreground(style)
    type(screen_style), intent(inout) :: style
    real :: luminance

    if (style%bg_truecolor) then
      luminance = 0.2126 * real(style%bg_rgb(1)) + 0.7152 * real(style%bg_rgb(2)) + &
                  0.0722 * real(style%bg_rgb(3))
      style%fg_truecolor = .true.
      style%fg = -1
      if (luminance >= 140.0) then
        style%fg_rgb = [0, 0, 0]
      else
        style%fg_rgb = [255, 255, 255]
      end if
    else if (style%bg >= 0 .and. .not. style%fg_truecolor .and. style%fg < 0) then
      style%inverse = .true.
    end if
  end subroutine apply_contrast_foreground

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
