program test_memory
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use fgof_screen, only : allocate_screen, clear_screen_style
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_collector, only : collector_snapshot
  use ftop_memory, only : render_memory_panel
  use ftop_widgets, only : widget_rect
  implicit none

  call test_memory_label_backgrounds()
  call test_memory_pressure_rows()
  call test_compact_memory_history_autoscales()
  call test_tall_memory_preview_autoscales()

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
    call require(all(buffer%cells(3, 18)%style%bg_rgb == [30, 31, 36]), &
                 "memory free label background should use muted free segment color")
    call require(all(buffer%cells(3, 18)%style%fg_rgb == [255, 255, 255]), &
                 "memory free label background should use light text")
    call require_glyph(buffer, 3, 24, "░", "memory free segment should use empty glyph")
    call require(all(buffer%cells(3, 24)%style%bg_rgb == [30, 31, 36]), &
                 "memory free segment background should match free label background")
    call require(buffer%cells(3, 14)%style%bg_truecolor, "memory used label background should be truecolor")
    call require(buffer%cells(3, 18)%style%bg_truecolor, "memory free label background should be truecolor")
  end subroutine test_memory_label_backgrounds

  subroutine test_memory_pressure_rows()
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot
    type(screen_style) :: style
    character(len=:), allocatable :: text

    buffer = allocate_screen(42, 10)
    style = clear_screen_style()
    snapshot%memory%valid = .true.
    snapshot%memory%total_bytes = 1000_int64
    snapshot%memory%available_bytes = 600_int64
    snapshot%memory%free_bytes = 200_int64
    snapshot%memory%swap_total_bytes = 0_int64

    call render_memory_panel(buffer, widget_rect(row=1, col=1, width=42, height=10), &
                             snapshot, style, style, style)
    text = buffer_text(buffer)

    call require(index(text, "pressure low") > 0, "memory panel should render pressure line")
    call require(index(text, "headroom 600 B  free 200 B") > 0, "memory panel should render headroom line")
    call require(index(text, "reclaim 400 B") > 0, "memory panel should render reclaimable line")
    call require(index(text, "cache 0 B") == 0, "memory panel should hide unsupported zero cache")
    call require(index(text, "used 400 B  free") == 0, "memory panel should not duplicate used/free row")
  end subroutine test_memory_pressure_rows

  subroutine test_compact_memory_history_autoscales()
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot
    type(screen_style) :: style

    buffer = allocate_screen(24, 9)
    style = clear_screen_style()
    snapshot%memory%valid = .true.
    snapshot%memory%total_bytes = 1000_int64
    snapshot%memory%used_bytes = 750_int64
    snapshot%memory%free_bytes = 250_int64
    snapshot%memory%available_bytes = 250_int64
    snapshot%memory_usage_history = [70.0_real64, 71.0_real64, 73.0_real64, 75.0_real64]

    call render_memory_panel(buffer, widget_rect(row=1, col=1, width=24, height=9), &
                             snapshot, style, style, style)

    call require(allocated(buffer%cells(8, 2)%glyph), "compact memory graph first glyph should render")
    call require(allocated(buffer%cells(8, 3)%glyph), "compact memory graph second glyph should render")
    call require(buffer%cells(8, 2)%glyph /= buffer%cells(8, 3)%glyph, &
                 "compact memory graph should preserve small history changes")
  end subroutine test_compact_memory_history_autoscales

  subroutine test_tall_memory_preview_autoscales()
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot
    type(screen_style) :: style

    buffer = allocate_screen(24, 12)
    style = clear_screen_style()
    snapshot%memory%valid = .true.
    snapshot%memory%total_bytes = 1000_int64
    snapshot%memory%available_bytes = 360_int64
    snapshot%memory%free_bytes = 100_int64
    snapshot%memory_usage_history = [64.0_real64, 64.1_real64, 64.2_real64, 64.3_real64]

    call render_memory_panel(buffer, widget_rect(row=1, col=1, width=24, height=12), &
                             snapshot, style, style, style, expanded=.false.)

    call require(graph_has_varied_glyphs(buffer, 8, 11, 2, 23), &
                 "tall memory preview should preserve minute history changes")
  end subroutine test_tall_memory_preview_autoscales

  logical function graph_has_varied_glyphs(buffer, first_row, last_row, first_col, last_col) result(varied)
    type(screen_buffer), intent(in) :: buffer
    integer, intent(in) :: first_row
    integer, intent(in) :: last_row
    integer, intent(in) :: first_col
    integer, intent(in) :: last_col
    character(len=:), allocatable :: first_glyph
    integer :: col
    integer :: row

    varied = .false.
    first_glyph = ""
    do row = first_row, last_row
      do col = first_col, last_col
        if (.not. allocated(buffer%cells(row, col)%glyph)) cycle
        if (buffer%cells(row, col)%glyph == " ") cycle
        if (len(first_glyph) == 0) then
          first_glyph = buffer%cells(row, col)%glyph
        else if (buffer%cells(row, col)%glyph /= first_glyph) then
          varied = .true.
          return
        end if
      end do
    end do
  end function graph_has_varied_glyphs

  function buffer_text(buffer) result(text)
    type(screen_buffer), intent(in) :: buffer
    character(len=:), allocatable :: text
    integer :: row

    text = ""
    do row = 1, buffer%size%height
      text = text // row_text(buffer, row) // new_line("a")
    end do
  end function buffer_text

  function row_text(buffer, row) result(text)
    type(screen_buffer), intent(in) :: buffer
    integer, intent(in) :: row
    character(len=:), allocatable :: text
    integer :: col

    text = ""
    do col = 1, buffer%size%width
      if (allocated(buffer%cells(row, col)%glyph)) then
        text = text // buffer%cells(row, col)%glyph
      else
        text = text // " "
      end if
    end do
  end function row_text

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
