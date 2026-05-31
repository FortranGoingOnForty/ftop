program test_sparkline
  use fgof_screen, only : allocate_screen, clear_screen_style
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_color, only : gradient_blue_cyan
  use ftop_sparkline, only : &
    render_sparkline, &
    sparkline_bucket, &
    sparkline_glyph, &
    sparkline_widget
  use ftop_widgets, only : widget_rect, widget_size
  implicit none

  call test_glyph_helpers()
  call test_fixed_range_rendering()
  call test_autoscale_rendering()
  call test_recent_sample_clipping()
  call test_gradient_styles()
  call test_sparkline_widget_type()

contains

  subroutine test_glyph_helpers()
    call require(sparkline_bucket(-1.0) == 1, "sparkline bucket must clamp low")
    call require(sparkline_bucket(0.50) == 4, "sparkline middle bucket mismatch")
    call require(sparkline_bucket(2.0) == 8, "sparkline bucket must clamp high")
    call require(sparkline_glyph(0.0) == "▁", "sparkline low glyph mismatch")
    call require(sparkline_glyph(0.50) == "▄", "sparkline middle glyph mismatch")
    call require(sparkline_glyph(1.0) == "█", "sparkline high glyph mismatch")
  end subroutine test_glyph_helpers

  subroutine test_fixed_range_rendering()
    type(screen_buffer) :: buffer
    type(screen_style) :: style
    real :: values(5)

    buffer = allocate_screen(5, 1)
    style = clear_screen_style()
    style%bold = .true.
    values = [0.0, 0.25, 0.50, 0.75, 1.0]

    call render_sparkline(buffer, widget_rect(row=1, col=1, width=5, height=1), values, &
                          min_value=0.0, max_value=1.0, style=style)

    call require_glyph(buffer, 1, 1, "▁", "fixed sparkline first glyph mismatch")
    call require_glyph(buffer, 1, 2, "▂", "fixed sparkline second glyph mismatch")
    call require_glyph(buffer, 1, 3, "▄", "fixed sparkline middle glyph mismatch")
    call require_glyph(buffer, 1, 4, "▆", "fixed sparkline fourth glyph mismatch")
    call require_glyph(buffer, 1, 5, "█", "fixed sparkline final glyph mismatch")
    call require(buffer%cells(1, 3)%style%bold, "sparkline style must be applied")
  end subroutine test_fixed_range_rendering

  subroutine test_autoscale_rendering()
    type(screen_buffer) :: buffer
    real :: values(3)

    buffer = allocate_screen(3, 1)
    values = [10.0, 15.0, 20.0]
    call render_sparkline(buffer, widget_rect(row=1, col=1, width=3, height=1), values)

    call require_glyph(buffer, 1, 1, "▁", "autoscaled sparkline first glyph mismatch")
    call require_glyph(buffer, 1, 2, "▄", "autoscaled sparkline middle glyph mismatch")
    call require_glyph(buffer, 1, 3, "█", "autoscaled sparkline final glyph mismatch")
  end subroutine test_autoscale_rendering

  subroutine test_recent_sample_clipping()
    type(screen_buffer) :: buffer
    real :: values(5)

    buffer = allocate_screen(3, 1)
    values = [1.0, 2.0, 3.0, 4.0, 5.0]
    call render_sparkline(buffer, widget_rect(row=1, col=1, width=3, height=1), values, &
                          min_value=1.0, max_value=5.0)

    call require_glyph(buffer, 1, 1, "▄", "clipped sparkline first recent glyph mismatch")
    call require_glyph(buffer, 1, 2, "▆", "clipped sparkline second recent glyph mismatch")
    call require_glyph(buffer, 1, 3, "█", "clipped sparkline final recent glyph mismatch")
  end subroutine test_recent_sample_clipping

  subroutine test_gradient_styles()
    type(screen_buffer) :: buffer
    real :: values(2)

    buffer = allocate_screen(2, 1)
    values = [0.0, 1.0]
    call render_sparkline(buffer, widget_rect(row=1, col=1, width=2, height=1), values, &
                          gradient=gradient_blue_cyan(), min_value=0.0, max_value=1.0)

    call require(buffer%cells(1, 1)%style%fg_truecolor, "sparkline gradient must set truecolor")
    call require(all(buffer%cells(1, 1)%style%fg_rgb == [52, 152, 219]), &
                 "sparkline first gradient color mismatch")
    call require(all(buffer%cells(1, 2)%style%fg_rgb == [26, 188, 156]), &
                 "sparkline final gradient color mismatch")
  end subroutine test_gradient_styles

  subroutine test_sparkline_widget_type()
    type(screen_buffer) :: buffer
    type(sparkline_widget) :: spark
    type(widget_size) :: size_value

    buffer = allocate_screen(4, 1)
    spark%values = [0.0, 0.50, 1.0]
    spark%width = 2
    spark%autoscale = .false.
    spark%min_value = 0.0
    spark%max_value = 1.0

    size_value = spark%min_size()
    call require(size_value%width == 2, "sparkline widget min width mismatch")
    call require(size_value%height == 1, "sparkline widget min height mismatch")

    call spark%render(buffer, widget_rect(row=1, col=1, width=4, height=1))
    call require_glyph(buffer, 1, 1, "▄", "sparkline widget first glyph mismatch")
    call require_glyph(buffer, 1, 2, "█", "sparkline widget second glyph mismatch")
    call require_glyph(buffer, 1, 3, " ", "sparkline widget width should clip output")
  end subroutine test_sparkline_widget_type

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

end program test_sparkline
