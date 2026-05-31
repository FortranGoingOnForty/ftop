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
  public :: truncated_text

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
    integer :: i
    integer :: text_len

    if (rect%width <= 0 .or. rect%height <= 0) return
    if (buffer%size%width <= 0 .or. buffer%size%height <= 0) return

    clipped = truncated_text(text, rect%width)
    text_len = len(clipped)
    if (text_len <= 0) return

    draw_style = clear_screen_style()
    if (present(style)) draw_style = style
    draw_row = rect%row
    draw_col = aligned_col(rect, text_len, alignment_or_default(alignment))
    if (draw_row < 1 .or. draw_row > buffer%size%height) return

    do i = 1, text_len
      if (draw_col + i - 1 > rect%col + rect%width - 1) exit
      if (draw_col + i - 1 >= rect%col) then
        call put_glyph(buffer, draw_row, draw_col + i - 1, clipped(i:i), draw_style)
      end if
    end do
  end subroutine render_text

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
    if (len(original) <= width) then
      clipped = original
    else if (width == 1) then
      clipped = "."
    else if (width == 2) then
      clipped = ".."
    else
      content_width = width - 3
      clipped = original(1:content_width) // "..."
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
    if (allocated(self%text)) size_value%width = max(1, len_trim(self%text))
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

end module ftop_text
