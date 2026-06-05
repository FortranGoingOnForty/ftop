module ftop_help
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_box, only : BOX_STYLE_DOUBLE, draw_box
  use ftop_color, only : COLOR_UI_ACCENT, COLOR_UI_BORDER, COLOR_UI_DIM, COLOR_UI_PANEL, style_from_rgb
  use ftop_text, only : TEXT_ALIGN_CENTER, render_text
  use ftop_widgets, only : widget_rect
  implicit none
  private

  public :: render_help_overlay

contains

  subroutine render_help_overlay(buffer, focused_widget)
    type(screen_buffer), intent(inout) :: buffer
    character(len=*), intent(in) :: focused_widget
    type(screen_style) :: border_style
    type(screen_style) :: dim_style
    type(screen_style) :: focus_style
    type(screen_style) :: title_style
    type(widget_rect) :: overlay
    integer :: content_col
    integer :: row

    if (buffer%size%width < 20 .or. buffer%size%height < 8) return

    border_style = style_from_rgb(fg=COLOR_UI_BORDER, bg=COLOR_UI_PANEL)
    dim_style = style_from_rgb(fg=COLOR_UI_DIM, bg=COLOR_UI_PANEL)
    focus_style = style_from_rgb(fg=COLOR_UI_ACCENT, bg=COLOR_UI_PANEL, bold=.true.)
    title_style = style_from_rgb(fg=COLOR_UI_ACCENT, bg=COLOR_UI_PANEL, bold=.true.)

    overlay = widget_rect(2, 4, max(10, buffer%size%width - 6), max(6, buffer%size%height - 3))
    call fill_rect(buffer, overlay, dim_style)
    call draw_box(buffer, overlay, BOX_STYLE_DOUBLE, border_style, "ftop help", title_style, TEXT_ALIGN_CENTER)

    row = overlay%row + 2
    content_col = overlay%col + 2
    call render_section(buffer, row, content_col, overlay%width - 4, "Global", focused_widget == "", focus_style, dim_style)
    call render_line(buffer, row, content_col, overlay%width - 4, "c/m/n/p: focus CPU/Memory/Network/Process", dim_style)
    call render_line(buffer, row, content_col, overlay%width - 4, "d/g: disk/GPU placeholder status", dim_style)
    call render_line(buffer, row, content_col, overlay%width - 4, "P: cycle layout   1-4: jump layouts", dim_style)
    call render_line(buffer, row, content_col, overlay%width - 4, "Tab: next focus   Shift+Tab: last focus", dim_style)
    call render_line(buffer, row, content_col, overlay%width - 4, "z/Enter: zoom   +/-: refresh rate", dim_style)
    call render_line(buffer, row, content_col, overlay%width - 4, "Ctrl+L: redraw   Ctrl+R: refresh", dim_style)

    call render_section(buffer, row, content_col, overlay%width - 4, "Process", focused_widget == "process", focus_style, dim_style)
    call render_line(buffer, row, content_col, overlay%width - 4, "F2: signal   F3: tree   F4: filter   F9: signal tags", dim_style)
    call render_line(buffer, row, content_col, overlay%width - 4, &
                     "F5: sort direction   F6: sparklines   F10: command detail", dim_style)
    call render_line(buffer, row, content_col, overlay%width - 4, &
                     "type: fuzzy find   arrows/Page/Home/End: navigate", dim_style)

    call render_section(buffer, row, content_col, overlay%width - 4, "Network", focused_widget == "network", focus_style, dim_style)
    call render_line(buffer, row, content_col, overlay%width - 4, "f: state filter   s: sort direction", dim_style)
    call render_line(buffer, row, content_col, overlay%width - 4, &
                     "arrows/Page/Home/End: navigate connections", dim_style)

    call render_text(buffer, widget_rect(overlay%row + overlay%height - 1, overlay%col + 2, overlay%width - 4, 1), &
                     "Press ? or Escape to close", title_style, TEXT_ALIGN_CENTER)
  end subroutine render_help_overlay

  subroutine render_section(buffer, row, col, width, title, active, focus_style, dim_style)
    type(screen_buffer), intent(inout) :: buffer
    integer, intent(inout) :: row
    integer, intent(in) :: col
    integer, intent(in) :: width
    character(len=*), intent(in) :: title
    logical, intent(in) :: active
    type(screen_style), intent(in) :: focus_style
    type(screen_style), intent(in) :: dim_style
    type(widget_rect) :: rect

    row = row + 1
    rect = widget_rect(row, col, width, 1)
    if (active) then
      call render_text(buffer, rect, "[ " // trim(title) // " ]", focus_style)
    else
      call render_text(buffer, rect, trim(title), dim_style)
    end if
    row = row + 1
  end subroutine render_section

  subroutine render_line(buffer, row, col, width, text, style)
    type(screen_buffer), intent(inout) :: buffer
    integer, intent(inout) :: row
    integer, intent(in) :: col
    integer, intent(in) :: width
    character(len=*), intent(in) :: text
    type(screen_style), intent(in) :: style

    call render_text(buffer, widget_rect(row, col, width, 1), text, style)
    row = row + 1
  end subroutine render_line

  subroutine fill_rect(buffer, rect, style)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    type(screen_style), intent(in) :: style
    integer :: col
    integer :: row

    do row = max(1, rect%row), min(buffer%size%height, rect%row + rect%height - 1)
      do col = max(1, rect%col), min(buffer%size%width, rect%col + rect%width - 1)
        call render_text(buffer, widget_rect(row, col, 1, 1), " ", style)
      end do
    end do
  end subroutine fill_rect

end module ftop_help
