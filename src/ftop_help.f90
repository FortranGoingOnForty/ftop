module ftop_help
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_box, only : BOX_STYLE_DOUBLE, draw_box
  use ftop_color, only : COLOR_UI_ACCENT, COLOR_UI_BORDER, COLOR_UI_DIM, COLOR_UI_PANEL, style_from_rgb
  use ftop_text, only : TEXT_ALIGN_CENTER, render_text
  use ftop_widgets, only : widget_rect
  implicit none
  private

  public :: render_help_overlay

  integer, parameter :: HELP_TITLE_LEN = 16
  integer, parameter :: HELP_FOCUS_LEN = 16
  integer, parameter :: HELP_LINE_LEN = 78
  integer, parameter :: HELP_MAX_LINES = 8

  type :: help_section
    character(len=HELP_TITLE_LEN) :: title
    character(len=HELP_FOCUS_LEN) :: focus_widget
    integer :: line_count
    character(len=HELP_LINE_LEN) :: lines(HELP_MAX_LINES)
  end type help_section

  type(help_section), parameter :: HELP_SECTIONS(3) = [ &
    help_section("Global", "", 8, [character(len=HELP_LINE_LEN) :: &
      "c/m/n/p: focus CPU/Memory/Network/Process", &
      "d/g: disk/GPU placeholder status", &
      "P: cycle layout   1-4: jump layouts", &
      "arrows: focus panes; Enter activates CPU/network modes", &
      "Tab: next focus   Shift+Tab: last focus", &
      "z: zoom   Enter: activate/zoom focus", &
      "Ctrl+L: redraw   Ctrl+R: refresh", &
      "?/F1: help   Esc: leave zoom/help"]), &
    help_section("Process", "process", 7, [character(len=HELP_LINE_LEN) :: &
      "F2: signal   F3: tree   F4: filter   F5: sort dir", &
      "F6: sparklines   F7: pause   F8: follow", &
      "F9: signal tags   F10: command detail", &
      "Space: tag   U: clear tags", &
      "Enter/Down: tree active   Enter again: zoom", &
      "Page/Home/End/j/k/g/G: navigate", &
      "Esc: return arrows/leave zoom", &
      ""]), &
    help_section("Network", "network", 2, [character(len=HELP_LINE_LEN) :: &
      "f: state filter   s: sort direction", &
      "Page/Home/End/j/k/g/G: navigate connections", &
      "", &
      "", &
      "", &
      "", &
      "", &
      ""]) &
  ]

contains

  subroutine render_help_overlay(buffer, focused_widget)
    type(screen_buffer), intent(inout) :: buffer
    character(len=*), intent(in) :: focused_widget
    type(screen_style) :: border_style
    type(screen_style) :: dim_style
    type(screen_style) :: active_section_style
    type(screen_style) :: title_style
    type(widget_rect) :: overlay
    integer :: content_col
    integer :: row
    integer :: section_index

    if (buffer%size%width < 20 .or. buffer%size%height < 8) return

    border_style = style_from_rgb(fg=COLOR_UI_BORDER, bg=COLOR_UI_PANEL)
    dim_style = style_from_rgb(fg=COLOR_UI_DIM, bg=COLOR_UI_PANEL)
    active_section_style = style_from_rgb(fg=COLOR_UI_PANEL, bg=COLOR_UI_ACCENT, bold=.true.)
    title_style = style_from_rgb(fg=COLOR_UI_ACCENT, bg=COLOR_UI_PANEL, bold=.true.)

    overlay = widget_rect(1, 2, max(10, buffer%size%width - 2), max(6, buffer%size%height))
    call fill_rect(buffer, overlay, dim_style)
    call draw_box(buffer, overlay, BOX_STYLE_DOUBLE, border_style, "ftop help", title_style, TEXT_ALIGN_CENTER)

    row = overlay%row + 2
    content_col = overlay%col + 2
    do section_index = 1, size(HELP_SECTIONS)
      call render_help_section(buffer, row, content_col, overlay%width - 4, HELP_SECTIONS(section_index), &
                               is_focused_section(HELP_SECTIONS(section_index), focused_widget), active_section_style, dim_style)
    end do

    call render_text(buffer, widget_rect(overlay%row + overlay%height - 1, overlay%col + 2, overlay%width - 4, 1), &
                     "Press ? or Escape to close", title_style, TEXT_ALIGN_CENTER)
  end subroutine render_help_overlay

  subroutine render_help_section(buffer, row, col, width, section, active, active_section_style, dim_style)
    type(screen_buffer), intent(inout) :: buffer
    integer, intent(inout) :: row
    integer, intent(in) :: col
    integer, intent(in) :: width
    type(help_section), intent(in) :: section
    logical, intent(in) :: active
    type(screen_style), intent(in) :: active_section_style
    type(screen_style), intent(in) :: dim_style
    integer :: line_index

    call render_section_heading(buffer, row, col, width, trim(section%title), active, active_section_style, dim_style)
    do line_index = 1, section%line_count
      call render_line(buffer, row, col, width, trim(section%lines(line_index)), dim_style)
    end do
  end subroutine render_help_section

  subroutine render_section_heading(buffer, row, col, width, title, active, active_section_style, dim_style)
    type(screen_buffer), intent(inout) :: buffer
    integer, intent(inout) :: row
    integer, intent(in) :: col
    integer, intent(in) :: width
    character(len=*), intent(in) :: title
    logical, intent(in) :: active
    type(screen_style), intent(in) :: active_section_style
    type(screen_style), intent(in) :: dim_style
    type(widget_rect) :: rect

    rect = widget_rect(row, col, width, 1)
    if (active) then
      call fill_rect(buffer, rect, active_section_style)
      call render_text(buffer, rect, "[ " // trim(title) // " ]", active_section_style)
    else
      call render_text(buffer, rect, trim(title), dim_style)
    end if
    row = row + 1
  end subroutine render_section_heading

  logical function is_focused_section(section, focused_widget) result(focused)
    type(help_section), intent(in) :: section
    character(len=*), intent(in) :: focused_widget

    focused = trim(section%focus_widget) == trim(focused_widget)
  end function is_focused_section

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
