program test_widgets
  use, intrinsic :: iso_fortran_env, only : int64
  use fgof_screen, only : allocate_screen, clear_screen_style
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_box, only : &
    BOX_STYLE_HEAVY, &
    BOX_STYLE_ROUNDED, &
    box_content_rect, &
    box_widget, &
    draw_box
  use ftop_text, only : &
    TEXT_ALIGN_CENTER, &
    TEXT_ALIGN_RIGHT, &
    format_bytes, &
    format_percent, &
    render_text, &
    text_widget, &
    truncated_text
  use ftop_widgets, only : &
    clamp_rect_to_screen, &
    inner_rect, &
    rect_valid, &
    widget_rect, &
    widget_size
  implicit none

  call test_rect_helpers()
  call test_text_helpers()
  call test_text_rendering()
  call test_text_widget_type()
  call test_box_rendering()
  call test_tiny_box_rendering()
  call test_box_helpers()

contains

  subroutine test_rect_helpers()
    type(screen_buffer) :: buffer
    type(widget_rect) :: clamped
    type(widget_rect) :: inner

    buffer = allocate_screen(5, 4)
    call require(rect_valid(widget_rect(row=1, col=1, width=2, height=3)), &
                 "positive rect must be valid")
    call require(.not. rect_valid(widget_rect(row=1, col=1, width=0, height=3)), &
                 "zero-width rect must be invalid")

    inner = inner_rect(widget_rect(row=2, col=3, width=10, height=6), 2)
    call require_rect(inner, 4, 5, 6, 2, "inner rect mismatch")

    clamped = clamp_rect_to_screen(widget_rect(row=0, col=-1, width=4, height=3), buffer)
    call require_rect(clamped, 1, 1, 2, 2, "partially visible clamp mismatch")

    clamped = clamp_rect_to_screen(widget_rect(row=10, col=1, width=2, height=2), buffer)
    call require_rect(clamped, 10, 1, 0, 0, "offscreen clamp mismatch")
  end subroutine test_rect_helpers

  subroutine test_text_helpers()
    call require(truncated_text("abcdef", 4) == "a...", "text truncation mismatch")
    call require(truncated_text("abcdef", 2) == "..", "narrow text truncation mismatch")
    call require(truncated_text("abcdef", 0) == "", "zero-width text truncation mismatch")
    call require(format_percent(47.3) == "47.3%", "percent formatting mismatch")
    call require(format_bytes(1536_int64) == "1.5 KiB", "KiB formatting mismatch")
    call require(format_bytes(1073741824_int64) == "1.0 GiB", "GiB formatting mismatch")
  end subroutine test_text_helpers

  subroutine test_text_rendering()
    type(screen_buffer) :: buffer
    type(screen_style) :: style

    buffer = allocate_screen(10, 3)
    style = clear_screen_style()
    style%bold = .true.

    call render_text(buffer, widget_rect(row=2, col=2, width=5, height=1), &
                     "abc", style, TEXT_ALIGN_CENTER)
    call require_glyph(buffer, 2, 2, " ", "centered text must leave left padding")
    call require_glyph(buffer, 2, 3, "a", "centered text first glyph mismatch")
    call require_glyph(buffer, 2, 4, "b", "centered text second glyph mismatch")
    call require_glyph(buffer, 2, 5, "c", "centered text third glyph mismatch")
    call require(buffer%cells(2, 3)%style%bold, "text style must be applied")

    buffer = allocate_screen(5, 1)
    call render_text(buffer, widget_rect(row=1, col=1, width=4, height=1), &
                     "abcdef", alignment=TEXT_ALIGN_RIGHT)
    call require_glyph(buffer, 1, 1, "a", "truncated text first glyph mismatch")
    call require_glyph(buffer, 1, 2, ".", "truncated text first dot mismatch")
    call require_glyph(buffer, 1, 3, ".", "truncated text second dot mismatch")
    call require_glyph(buffer, 1, 4, ".", "truncated text third dot mismatch")

    buffer = allocate_screen(2, 1)
    call render_text(buffer, widget_rect(row=1, col=1, width=2, height=1), "A▲")
    call require_glyph(buffer, 1, 1, "A", "UTF-8 text first glyph mismatch")
    call require_glyph(buffer, 1, 2, "▲", "UTF-8 text second glyph mismatch")
  end subroutine test_text_rendering

  subroutine test_text_widget_type()
    type(screen_buffer) :: buffer
    type(text_widget) :: label
    type(widget_size) :: size_value

    buffer = allocate_screen(6, 1)
    label%text = "load"
    label%alignment = TEXT_ALIGN_RIGHT

    size_value = label%min_size()
    call require(size_value%width == 4, "text widget min width mismatch")
    call require(size_value%height == 1, "text widget min height mismatch")

    call label%render(buffer, widget_rect(row=1, col=1, width=6, height=1))
    call require_glyph(buffer, 1, 3, "l", "text widget right alignment mismatch")
    call require_glyph(buffer, 1, 6, "d", "text widget final glyph mismatch")
  end subroutine test_text_widget_type

  subroutine test_box_rendering()
    type(screen_buffer) :: buffer
    type(screen_style) :: style

    buffer = allocate_screen(8, 4)
    style = clear_screen_style()
    style%underline = .true.

    call draw_box(buffer, widget_rect(row=1, col=1, width=8, height=4), &
                  BOX_STYLE_ROUNDED, style, "CPU", style, TEXT_ALIGN_CENTER)

    call require_glyph(buffer, 1, 1, "╭", "rounded top-left mismatch")
    call require_glyph(buffer, 1, 8, "╮", "rounded top-right mismatch")
    call require_glyph(buffer, 4, 1, "╰", "rounded bottom-left mismatch")
    call require_glyph(buffer, 4, 8, "╯", "rounded bottom-right mismatch")
    call require_glyph(buffer, 2, 1, "│", "rounded vertical mismatch")
    call require_glyph(buffer, 1, 3, "C", "box title first glyph mismatch")
    call require_glyph(buffer, 1, 5, "U", "box title final glyph mismatch")
    call require(buffer%cells(1, 1)%style%underline, "box border style must be applied")

    buffer = allocate_screen(8, 3)
    call draw_box(buffer, widget_rect(row=1, col=1, width=8, height=3), &
                  BOX_STYLE_HEAVY, title="IO", title_alignment=TEXT_ALIGN_RIGHT)
    call require_glyph(buffer, 1, 1, "┏", "heavy top-left mismatch")
    call require_glyph(buffer, 3, 8, "┛", "heavy bottom-right mismatch")
    call require_glyph(buffer, 1, 5, "I", "right-aligned title first glyph mismatch")
    call require_glyph(buffer, 1, 6, "O", "right-aligned title final glyph mismatch")
  end subroutine test_box_rendering

  subroutine test_tiny_box_rendering()
    type(screen_buffer) :: buffer

    buffer = allocate_screen(1, 1)
    call draw_box(buffer, widget_rect(row=1, col=1, width=1, height=1), BOX_STYLE_ROUNDED)
    call require_glyph(buffer, 1, 1, "─", "single-cell box glyph mismatch")

    buffer = allocate_screen(1, 3)
    call draw_box(buffer, widget_rect(row=1, col=1, width=1, height=3), BOX_STYLE_ROUNDED)
    call require_glyph(buffer, 1, 1, "│", "single-column box top glyph mismatch")
    call require_glyph(buffer, 3, 1, "│", "single-column box bottom glyph mismatch")
  end subroutine test_tiny_box_rendering

  subroutine test_box_helpers()
    type(box_widget) :: box
    type(widget_rect) :: content
    type(widget_size) :: size_value

    content = box_content_rect(widget_rect(row=2, col=3, width=10, height=6), 1)
    call require_rect(content, 4, 5, 6, 2, "box content rect mismatch")

    box%title = "CPU"
    box%border = BOX_STYLE_HEAVY
    box%padding = 1
    size_value = box%min_size()
    call require(size_value%width == 7, "box min width mismatch")
    call require(size_value%height == 4, "box min height mismatch")
  end subroutine test_box_helpers

  subroutine require_rect(rect, row, col, width, height, message)
    type(widget_rect), intent(in) :: rect
    integer, intent(in) :: row
    integer, intent(in) :: col
    integer, intent(in) :: width
    integer, intent(in) :: height
    character(len=*), intent(in) :: message

    call require(rect%row == row .and. rect%col == col .and. &
                 rect%width == width .and. rect%height == height, message)
  end subroutine require_rect

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

end program test_widgets
