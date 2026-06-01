module ftop_dashboard
  use fgof_screen, only : clear_screen
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_box, only : BOX_STYLE_DOUBLE, draw_box
  use ftop_collector, only : collector_snapshot
  use ftop_color, only : &
    COLOR_UI_ACCENT, &
    COLOR_UI_BORDER, &
    COLOR_UI_DIM, &
    COLOR_UI_PANEL, &
    style_from_rgb
  use ftop_cpu, only : render_cpu_panel
  use ftop_layout, only : dashboard_layout, dashboard_layout_from_grid, default_dashboard_layout, layout_grid
  use ftop_memory, only : render_memory_panel
  use ftop_text, only : TEXT_ALIGN_CENTER, render_text
  use ftop_widgets, only : widget_rect
  implicit none
  private

  public :: dashboard_layout
  public :: default_dashboard_layout
  public :: render_dashboard

contains

  subroutine render_dashboard(buffer, snapshot, refresh_ms, frame_count, status_text, grid)
    type(screen_buffer), intent(inout) :: buffer
    type(collector_snapshot), intent(in) :: snapshot
    integer, intent(in) :: refresh_ms
    integer, intent(in) :: frame_count
    character(len=*), intent(in) :: status_text
    type(layout_grid), intent(in), optional :: grid
    type(dashboard_layout) :: layout
    type(screen_style) :: border_style
    type(screen_style) :: title_style
    type(screen_style) :: dim_style
    type(widget_rect) :: title_rect
    integer :: height
    integer :: title_col
    integer :: width

    width = buffer%size%width
    height = buffer%size%height
    if (width <= 0 .or. height <= 0) return

    border_style = style_from_rgb(fg=COLOR_UI_BORDER)
    title_style = style_from_rgb(fg=COLOR_UI_ACCENT, bg=COLOR_UI_PANEL, bold=.true.)
    dim_style = style_from_rgb(fg=COLOR_UI_DIM)

    call clear_screen(buffer)
    buffer%cursor_visible = .false.

    if (width < 8 .or. height < 4) then
      call render_text(buffer, widget_rect(1, 1, width, 1), "ftop", title_style, TEXT_ALIGN_CENTER)
      return
    end if

    if (present(grid)) then
      layout = dashboard_layout_from_grid(width, height, grid)
    else
      layout = default_dashboard_layout(width, height)
    end if
    call draw_box(buffer, layout%frame, BOX_STYLE_DOUBLE, border_style, "ftop", title_style, TEXT_ALIGN_CENTER)

    if (layout%cpu_panel%height >= 3) then
      call render_cpu_panel(buffer, layout%cpu_panel, snapshot, border_style, title_style, dim_style)
    end if
    if (layout%memory_panel%height >= 3) then
      call render_memory_panel(buffer, layout%memory_panel, snapshot, border_style, title_style, dim_style)
    end if

    if (layout%cpu_panel%height < 3 .or. layout%memory_panel%height < 3) then
      title_col = max(2, (width - len_trim("CPU / Memory")) / 2 + 1)
      title_rect = widget_rect(max(2, height / 2), title_col, width - title_col, 1)
      call render_text(buffer, title_rect, "CPU / Memory", title_style)
    end if

    call render_footer(buffer, layout%footer, snapshot, refresh_ms, frame_count, status_text, dim_style)
  end subroutine render_dashboard

  subroutine render_footer(buffer, footer, snapshot, refresh_ms, frame_count, status_text, dim_style)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: footer
    type(collector_snapshot), intent(in) :: snapshot
    integer, intent(in) :: refresh_ms
    integer, intent(in) :: frame_count
    character(len=*), intent(in) :: status_text
    type(screen_style), intent(in) :: dim_style
    type(widget_rect) :: line
    character(len=:), allocatable :: status

    if (footer%width <= 0 .or. footer%height <= 0) return

    line = widget_rect(footer%row, footer%col, footer%width, 1)
    call render_text(buffer, line, footer_text(snapshot, refresh_ms, frame_count), dim_style)

    if (footer%height < 2) return
    status = trim(status_text)
    if (len(status) == 0) status = "ready"
    line = widget_rect(footer%row + 1, footer%col, footer%width, 1)
    call render_text(buffer, line, status, dim_style)
  end subroutine render_footer

  function footer_text(snapshot, refresh_ms, frame_count) result(text)
    type(collector_snapshot), intent(in) :: snapshot
    integer, intent(in) :: refresh_ms
    integer, intent(in) :: frame_count
    character(len=:), allocatable :: text
    character(len=:), allocatable :: running_text

    if (snapshot%running) then
      running_text = "collector running"
    else
      running_text = "collector idle"
    end if
    text = "refresh " // integer_text(refresh_ms) // "ms frame " // integer_text(frame_count) // &
           " samples " // integer_text(snapshot%sample_count) // "  " // running_text // &
           "  q/Ctrl+C quit Ctrl+Z suspend"
  end function footer_text

  function integer_text(value) result(text)
    integer, intent(in) :: value
    character(len=:), allocatable :: text
    character(len=32) :: scratch

    write(scratch, '(i0)') value
    text = trim(scratch)
  end function integer_text

end module ftop_dashboard
