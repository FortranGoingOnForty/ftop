program test_meter
  use fgof_screen, only : allocate_screen, clear_screen_style
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_color, only : gradient_green_yellow_red
  use ftop_meter, only : &
    METER_FILL_BLOCK, &
    METER_FILL_SHADED, &
    meter_fill_count, &
    meter_shade_glyph, &
    meter_widget, &
    render_meter
  use ftop_widgets, only : widget_rect, widget_size
  implicit none

  call test_fill_helpers()
  call test_block_meter_rendering()
  call test_shaded_meter_rendering()
  call test_label_overlay()
  call test_tiny_meter_edges()
  call test_meter_widget_type()

contains

  subroutine test_fill_helpers()
    call require(meter_fill_count(-1.0, 10) == 0, "meter fill must clamp low")
    call require(meter_fill_count(0.50, 10) == 5, "meter fill count mismatch")
    call require(meter_fill_count(2.0, 10) == 10, "meter fill must clamp high")
    call require(meter_fill_count(0.50, 0) == 0, "meter fill must handle zero width")
    call require(meter_shade_glyph(0.0) == "░", "empty shade glyph mismatch")
    call require(meter_shade_glyph(0.50) == "▒", "middle shade glyph mismatch")
    call require(meter_shade_glyph(0.75) == "▓", "high shade glyph mismatch")
    call require(meter_shade_glyph(1.0) == "█", "full shade glyph mismatch")
  end subroutine test_fill_helpers

  subroutine test_block_meter_rendering()
    type(screen_buffer) :: buffer
    type(screen_style) :: empty_style
    type(screen_style) :: fill_style

    buffer = allocate_screen(8, 1)
    fill_style = clear_screen_style()
    fill_style%bold = .true.
    empty_style = clear_screen_style()
    empty_style%dim = .true.

    call render_meter(buffer, widget_rect(row=1, col=1, width=8, height=1), 0.50, &
                      compact=.false., fill_mode=METER_FILL_BLOCK, &
                      fill_style=fill_style, empty_style=empty_style)

    call require_glyph(buffer, 1, 1, "[", "meter opening bracket mismatch")
    call require_glyph(buffer, 1, 2, "█", "meter first fill glyph mismatch")
    call require_glyph(buffer, 1, 4, "█", "meter final fill glyph mismatch")
    call require_glyph(buffer, 1, 5, "░", "meter first empty glyph mismatch")
    call require_glyph(buffer, 1, 8, "]", "meter closing bracket mismatch")
    call require(buffer%cells(1, 2)%style%bold, "meter fill style must be applied")
    call require(buffer%cells(1, 5)%style%dim, "meter empty style must be applied")
  end subroutine test_block_meter_rendering

  subroutine test_shaded_meter_rendering()
    type(screen_buffer) :: buffer

    buffer = allocate_screen(4, 1)
    call render_meter(buffer, widget_rect(row=1, col=1, width=4, height=1), 0.625, &
                      compact=.true., fill_mode=METER_FILL_SHADED)

    call require_glyph(buffer, 1, 1, "█", "shaded meter first full glyph mismatch")
    call require_glyph(buffer, 1, 2, "█", "shaded meter second full glyph mismatch")
    call require_glyph(buffer, 1, 3, "▒", "shaded meter partial glyph mismatch")
    call require_glyph(buffer, 1, 4, "░", "shaded meter empty glyph mismatch")
  end subroutine test_shaded_meter_rendering

  subroutine test_label_overlay()
    type(screen_buffer) :: buffer
    type(screen_style) :: label_style

    buffer = allocate_screen(7, 1)
    label_style = clear_screen_style()
    label_style%underline = .true.

    call render_meter(buffer, widget_rect(row=1, col=1, width=7, height=1), 1.0, &
                      label="47%", label_style=label_style)

    call require_glyph(buffer, 1, 3, "4", "meter label first glyph mismatch")
    call require_glyph(buffer, 1, 4, "7", "meter label second glyph mismatch")
    call require_glyph(buffer, 1, 5, "%", "meter label third glyph mismatch")
    call require(buffer%cells(1, 3)%style%underline, "meter label style must be applied")
  end subroutine test_label_overlay

  subroutine test_tiny_meter_edges()
    type(screen_buffer) :: buffer

    buffer = allocate_screen(1, 1)
    call render_meter(buffer, widget_rect(row=1, col=1, width=1, height=1), 1.0, compact=.false.)
    call require_glyph(buffer, 1, 1, "█", "single-cell meter must render fill")

    buffer = allocate_screen(1, 1)
    call render_meter(buffer, widget_rect(row=1, col=1, width=0, height=1), 1.0)
    call require_glyph(buffer, 1, 1, " ", "zero-width meter must not render")

    call render_meter(buffer, widget_rect(row=1, col=1, width=1, height=0), 1.0)
    call require_glyph(buffer, 1, 1, " ", "zero-height meter must not render")
  end subroutine test_tiny_meter_edges

  subroutine test_meter_widget_type()
    type(screen_buffer) :: buffer
    type(meter_widget) :: meter
    type(widget_size) :: size_value

    buffer = allocate_screen(8, 1)
    meter%value = 0.50
    meter%compact = .false.
    meter%label = "CPU"
    meter%gradient = gradient_green_yellow_red()

    size_value = meter%min_size()
    call require(size_value%width == 5, "meter widget min width mismatch")
    call require(size_value%height == 1, "meter widget min height mismatch")

    call meter%render(buffer, widget_rect(row=1, col=1, width=8, height=1))
    call require(buffer%cells(1, 2)%style%fg_truecolor, "meter gradient must set truecolor foreground")
    call require(all(buffer%cells(1, 2)%style%fg_rgb == [241, 196, 15]), &
                 "meter gradient foreground mismatch")
    call require_glyph(buffer, 1, 3, "C", "meter widget label first glyph mismatch")
    call require_glyph(buffer, 1, 5, "U", "meter widget label final glyph mismatch")
  end subroutine test_meter_widget_type

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

end program test_meter
