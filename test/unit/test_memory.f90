program test_memory
  use, intrinsic :: iso_fortran_env, only : int64
  use fgof_screen, only : allocate_screen, clear_screen_style
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_collector, only : collector_snapshot
  use ftop_memory, only : render_memory_panel
  use ftop_widgets, only : widget_rect
  implicit none

  call test_memory_label_backgrounds()

contains

  subroutine test_memory_label_backgrounds()
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot
    type(screen_style) :: style

    buffer = allocate_screen(32, 8)
    style = clear_screen_style()
    snapshot%memory%valid = .true.
    snapshot%memory%total_bytes = 1000_int64
    snapshot%memory%used_bytes = 500_int64
    snapshot%memory%free_bytes = 500_int64
    snapshot%memory%available_bytes = 500_int64

    call render_memory_panel(buffer, widget_rect(row=1, col=1, width=32, height=8), &
                             snapshot, style, style, style)

    call require_glyph(buffer, 3, 14, "5", "memory label first glyph mismatch")
    call require_glyph(buffer, 3, 18, "%", "memory label final glyph mismatch")
    call require(all(buffer%cells(3, 14)%style%bg_rgb == [231, 76, 60]), &
                 "memory used label background should use used segment color")
    call require(all(buffer%cells(3, 14)%style%fg_rgb == [255, 255, 255]), &
                 "memory used label background should use light text")
    call require(all(buffer%cells(3, 18)%style%bg_rgb == [38, 39, 45]), &
                 "memory free label background should use muted free segment color")
    call require(all(buffer%cells(3, 18)%style%fg_rgb == [255, 255, 255]), &
                 "memory free label background should use light text")
    call require(buffer%cells(3, 14)%style%bg_truecolor, "memory used label background should be truecolor")
    call require(buffer%cells(3, 18)%style%bg_truecolor, "memory free label background should be truecolor")
  end subroutine test_memory_label_backgrounds

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

end program test_memory
