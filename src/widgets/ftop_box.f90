module ftop_box
  use fgof_screen, only : clear_screen_style, put_glyph
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_text, only : TEXT_ALIGN_CENTER, TEXT_ALIGN_LEFT, TEXT_ALIGN_RIGHT, render_text
  use ftop_widgets, only : widget, widget_rect, widget_size
  implicit none
  private

  integer, parameter, public :: BOX_STYLE_SINGLE = 1
  integer, parameter, public :: BOX_STYLE_DOUBLE = 2
  integer, parameter, public :: BOX_STYLE_ROUNDED = 3
  integer, parameter, public :: BOX_STYLE_HEAVY = 4

  type, extends(widget), public :: box_widget
    character(len=:), allocatable :: title
    integer :: border = BOX_STYLE_SINGLE
    integer :: title_alignment = TEXT_ALIGN_LEFT
    integer :: padding = 0
    type(screen_style) :: border_style
    type(screen_style) :: title_style
  contains
    procedure :: render => box_widget_render
    procedure :: min_size => box_widget_min_size
  end type box_widget

  public :: box_content_rect
  public :: draw_box

contains

  subroutine draw_box(buffer, rect, border, border_style, title, title_style, title_alignment)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    integer, intent(in), optional :: border
    type(screen_style), intent(in), optional :: border_style
    character(len=*), intent(in), optional :: title
    type(screen_style), intent(in), optional :: title_style
    integer, intent(in), optional :: title_alignment
    character(len=4) :: bottom_left
    character(len=4) :: bottom_right
    character(len=4) :: horizontal
    character(len=4) :: top_left
    character(len=4) :: top_right
    character(len=4) :: vertical
    type(screen_style) :: box_style
    type(screen_style) :: caption_style
    type(widget_rect) :: title_rect
    integer :: actual_border
    integer :: bottom
    integer :: col
    integer :: right
    integer :: row

    if (rect%width <= 0 .or. rect%height <= 0) return
    actual_border = BOX_STYLE_SINGLE
    if (present(border)) actual_border = border
    call box_glyphs(actual_border, top_left, top_right, bottom_left, bottom_right, horizontal, vertical)

    box_style = clear_screen_style()
    if (present(border_style)) box_style = border_style
    caption_style = box_style
    if (present(title_style)) caption_style = title_style

    right = rect%col + rect%width - 1
    bottom = rect%row + rect%height - 1

    if (rect%height == 1) then
      do col = rect%col, right
        call put_glyph(buffer, rect%row, col, horizontal, box_style)
      end do
    else if (rect%width == 1) then
      do row = rect%row, bottom
        call put_glyph(buffer, row, rect%col, vertical, box_style)
      end do
    else
      call put_glyph(buffer, rect%row, rect%col, top_left, box_style)
      call put_glyph(buffer, rect%row, right, top_right, box_style)
      call put_glyph(buffer, bottom, rect%col, bottom_left, box_style)
      call put_glyph(buffer, bottom, right, bottom_right, box_style)
      do col = rect%col + 1, right - 1
        call put_glyph(buffer, rect%row, col, horizontal, box_style)
        call put_glyph(buffer, bottom, col, horizontal, box_style)
      end do
      do row = rect%row + 1, bottom - 1
        call put_glyph(buffer, row, rect%col, vertical, box_style)
        call put_glyph(buffer, row, right, vertical, box_style)
      end do
    end if

    if (present(title) .and. rect%width > 2 .and. rect%height > 0) then
      title_rect = widget_rect(rect%row, rect%col + 1, rect%width - 2, 1)
      call render_text(buffer, title_rect, " " // trim(title) // " ", caption_style, alignment_or_default(title_alignment))
    end if
  end subroutine draw_box

  function box_content_rect(rect, padding) result(content)
    type(widget_rect), intent(in) :: rect
    integer, intent(in), optional :: padding
    type(widget_rect) :: content
    integer :: pad

    pad = 0
    if (present(padding)) pad = max(0, padding)
    content%row = rect%row + 1 + pad
    content%col = rect%col + 1 + pad
    content%width = max(0, rect%width - 2 - 2 * pad)
    content%height = max(0, rect%height - 2 - 2 * pad)
  end function box_content_rect

  subroutine box_widget_render(self, buffer, rect)
    class(box_widget), intent(inout) :: self
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect

    if (.not. self%visible) return
    if (allocated(self%title)) then
      call draw_box(buffer, rect, self%border, self%border_style, self%title, self%title_style, self%title_alignment)
    else
      call draw_box(buffer, rect, self%border, self%border_style)
    end if
  end subroutine box_widget_render

  function box_widget_min_size(self) result(size_value)
    class(box_widget), intent(in) :: self
    type(widget_size) :: size_value
    integer :: pad
    integer :: title_width

    pad = max(0, self%padding)
    title_width = 0
    if (allocated(self%title)) title_width = len_trim(self%title) + 2
    size_value%width = max(2 + 2 * pad, title_width + 2)
    size_value%height = 2 + 2 * pad
  end function box_widget_min_size

  subroutine box_glyphs(border, top_left, top_right, bottom_left, bottom_right, horizontal, vertical)
    integer, intent(in) :: border
    character(len=*), intent(out) :: top_left
    character(len=*), intent(out) :: top_right
    character(len=*), intent(out) :: bottom_left
    character(len=*), intent(out) :: bottom_right
    character(len=*), intent(out) :: horizontal
    character(len=*), intent(out) :: vertical

    select case (border)
    case (BOX_STYLE_DOUBLE)
      top_left = "╔"
      top_right = "╗"
      bottom_left = "╚"
      bottom_right = "╝"
      horizontal = "═"
      vertical = "║"
    case (BOX_STYLE_ROUNDED)
      top_left = "╭"
      top_right = "╮"
      bottom_left = "╰"
      bottom_right = "╯"
      horizontal = "─"
      vertical = "│"
    case (BOX_STYLE_HEAVY)
      top_left = "┏"
      top_right = "┓"
      bottom_left = "┗"
      bottom_right = "┛"
      horizontal = "━"
      vertical = "┃"
    case default
      top_left = "┌"
      top_right = "┐"
      bottom_left = "└"
      bottom_right = "┘"
      horizontal = "─"
      vertical = "│"
    end select
  end subroutine box_glyphs

  integer function alignment_or_default(alignment) result(value)
    integer, intent(in), optional :: alignment

    value = TEXT_ALIGN_LEFT
    if (present(alignment)) then
      select case (alignment)
      case (TEXT_ALIGN_LEFT, TEXT_ALIGN_CENTER, TEXT_ALIGN_RIGHT)
        value = alignment
      case default
        value = TEXT_ALIGN_LEFT
      end select
    end if
  end function alignment_or_default

end module ftop_box
