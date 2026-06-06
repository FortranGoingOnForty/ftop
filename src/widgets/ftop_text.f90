module ftop_text
  use fgof_screen, only : clear_screen_style, put_glyph
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_widgets, only : widget, widget_rect, widget_size
  implicit none
  private

  integer, parameter, public :: TEXT_ALIGN_LEFT = 1
  integer, parameter, public :: TEXT_ALIGN_CENTER = 2
  integer, parameter, public :: TEXT_ALIGN_RIGHT = 3

  type, extends(widget), public :: text_widget
    character(len=:), allocatable :: text
    type(screen_style) :: style
    integer :: alignment = TEXT_ALIGN_LEFT
  contains
    procedure :: render => text_widget_render
    procedure :: min_size => text_widget_min_size
  end type text_widget

  public :: format_bytes
  public :: format_percent
  public :: render_text
  public :: text_cell_width
  public :: truncated_text
  public :: utf8_glyph_bytes

contains

  subroutine render_text(buffer, rect, text, style, alignment)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    character(len=*), intent(in) :: text
    type(screen_style), intent(in), optional :: style
    integer, intent(in), optional :: alignment
    character(len=:), allocatable :: clipped
    type(screen_style) :: draw_style
    integer :: draw_col
    integer :: draw_row
    integer :: glyph_bytes
    integer :: glyph_index
    integer :: i
    integer :: text_len

    if (rect%width <= 0 .or. rect%height <= 0) return
    if (buffer%size%width <= 0 .or. buffer%size%height <= 0) return

    clipped = truncated_text(text, rect%width)
    text_len = text_cell_width(clipped)
    if (text_len <= 0) return

    draw_style = clear_screen_style()
    if (present(style)) draw_style = style
    draw_row = rect%row
    draw_col = aligned_col(rect, text_len, alignment_or_default(alignment))
    if (draw_row < 1 .or. draw_row > buffer%size%height) return

    i = 1
    glyph_index = 0
    do while (i <= len(clipped))
      glyph_bytes = utf8_glyph_bytes(clipped, i)
      if (draw_col + glyph_index > rect%col + rect%width - 1) exit
      if (draw_col + glyph_index >= rect%col) then
        call put_glyph(buffer, draw_row, draw_col + glyph_index, clipped(i:i + glyph_bytes - 1), draw_style)
      end if
      i = i + glyph_bytes
      glyph_index = glyph_index + 1
    end do
  end subroutine render_text

  integer function text_cell_width(text) result(width)
    character(len=*), intent(in) :: text
    integer :: glyph_bytes
    integer :: i

    width = 0
    i = 1
    do while (i <= len(text))
      glyph_bytes = utf8_glyph_bytes(text, i)
      width = width + 1
      i = i + glyph_bytes
    end do
  end function text_cell_width

  function truncated_text(text, width) result(clipped)
    character(len=*), intent(in) :: text
    integer, intent(in) :: width
    character(len=:), allocatable :: clipped
    character(len=:), allocatable :: original
    integer :: content_width

    if (width <= 0) then
      clipped = ""
      return
    end if

    original = text
    if (text_cell_width(original) <= width) then
      clipped = original
    else if (width == 1) then
      clipped = "."
    else if (width == 2) then
      clipped = ".."
    else
      content_width = width - 3
      clipped = first_cells(original, content_width) // "..."
    end if
  end function truncated_text

  function format_percent(value) result(text)
    real, intent(in) :: value
    character(len=:), allocatable :: text
    character(len=32) :: buffer

    write(buffer, '(f6.1,a)') value, "%"
    text = trim(adjustl(buffer))
  end function format_percent

  function format_bytes(bytes) result(text)
    integer(kind=8), intent(in) :: bytes
    character(len=:), allocatable :: text
    character(len=32) :: buffer
    real :: value

    if (bytes < 1024_8) then
      write(buffer, '(i0,a)') bytes, " B"
    else if (bytes < 1024_8 ** 2) then
      value = real(bytes) / 1024.0
      write(buffer, '(f5.1,a)') value, " KiB"
    else if (bytes < 1024_8 ** 3) then
      value = real(bytes) / real(1024_8 ** 2)
      write(buffer, '(f5.1,a)') value, " MiB"
    else
      value = real(bytes) / real(1024_8 ** 3)
      write(buffer, '(f5.1,a)') value, " GiB"
    end if
    text = trim(adjustl(buffer))
  end function format_bytes

  subroutine text_widget_render(self, buffer, rect)
    class(text_widget), intent(inout) :: self
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect

    if (.not. self%visible) return
    if (allocated(self%text)) then
      call render_text(buffer, rect, self%text, self%style, self%alignment)
    end if
  end subroutine text_widget_render

  function text_widget_min_size(self) result(size_value)
    class(text_widget), intent(in) :: self
    type(widget_size) :: size_value

    size_value%height = 1
    if (allocated(self%text)) size_value%width = max(1, text_cell_width(trim(self%text)))
  end function text_widget_min_size

  integer function alignment_or_default(alignment) result(value)
    integer, intent(in), optional :: alignment

    value = TEXT_ALIGN_LEFT
    if (present(alignment)) value = alignment
  end function alignment_or_default

  integer function aligned_col(rect, text_len, alignment) result(col)
    type(widget_rect), intent(in) :: rect
    integer, intent(in) :: text_len
    integer, intent(in) :: alignment

    select case (alignment)
    case (TEXT_ALIGN_CENTER)
      col = rect%col + max(0, (rect%width - text_len) / 2)
    case (TEXT_ALIGN_RIGHT)
      col = rect%col + max(0, rect%width - text_len)
    case default
      col = rect%col
    end select
  end function aligned_col

  function first_cells(text, width) result(clipped)
    character(len=*), intent(in) :: text
    integer, intent(in) :: width
    character(len=:), allocatable :: clipped
    integer :: glyph_bytes
    integer :: glyph_count
    integer :: i

    clipped = ""
    if (width <= 0) return

    glyph_count = 0
    i = 1
    do while (i <= len(text) .and. glyph_count < width)
      glyph_bytes = utf8_glyph_bytes(text, i)
      clipped = clipped // text(i:i + glyph_bytes - 1)
      i = i + glyph_bytes
      glyph_count = glyph_count + 1
    end do
  end function first_cells

  integer function utf8_glyph_bytes(text, start) result(byte_count)
    character(len=*), intent(in) :: text
    integer, intent(in) :: start
    integer :: first_byte

    if (start < 1 .or. start > len(text)) then
      byte_count = 1
      return
    end if

    first_byte = iachar(text(start:start))
    if (first_byte < 128) then
      byte_count = 1
    else if (first_byte >= 192 .and. first_byte <= 223) then
      byte_count = 2
    else if (first_byte >= 224 .and. first_byte <= 239) then
      byte_count = 3
    else if (first_byte >= 240 .and. first_byte <= 247) then
      byte_count = 4
    else
      byte_count = 1
    end if
    byte_count = max(1, min(byte_count, len(text) - start + 1))
  end function utf8_glyph_bytes

end module ftop_text
