module ftop_widgets
  use fgof_screen_types, only : screen_buffer
  implicit none
  private

  type, public :: widget_rect
    integer :: row = 1
    integer :: col = 1
    integer :: width = 0
    integer :: height = 0
  end type widget_rect

  type, public :: widget_size
    integer :: width = 0
    integer :: height = 0
  end type widget_size

  type, public, abstract :: widget
    type(widget_rect) :: bounds
    logical :: visible = .true.
    logical :: focused = .false.
  contains
    procedure(widget_render_interface), deferred :: render
    procedure(widget_min_size_interface), deferred :: min_size
  end type widget

  public :: clamp_rect_to_screen
  public :: inner_rect
  public :: rect_valid

  abstract interface
    subroutine widget_render_interface(self, buffer, rect)
      import :: screen_buffer, widget, widget_rect
      class(widget), intent(inout) :: self
      type(screen_buffer), intent(inout) :: buffer
      type(widget_rect), intent(in) :: rect
    end subroutine widget_render_interface

    function widget_min_size_interface(self) result(size_value)
      import :: widget, widget_size
      class(widget), intent(in) :: self
      type(widget_size) :: size_value
    end function widget_min_size_interface
  end interface

contains

  logical function rect_valid(rect) result(valid)
    type(widget_rect), intent(in) :: rect

    valid = rect%width > 0 .and. rect%height > 0
  end function rect_valid

  function inner_rect(rect, padding) result(inner)
    type(widget_rect), intent(in) :: rect
    integer, intent(in), optional :: padding
    type(widget_rect) :: inner
    integer :: pad

    pad = 0
    if (present(padding)) pad = max(0, padding)
    inner%row = rect%row + pad
    inner%col = rect%col + pad
    inner%width = max(0, rect%width - 2 * pad)
    inner%height = max(0, rect%height - 2 * pad)
  end function inner_rect

  function clamp_rect_to_screen(rect, buffer) result(clamped)
    type(widget_rect), intent(in) :: rect
    type(screen_buffer), intent(in) :: buffer
    type(widget_rect) :: clamped
    integer :: bottom
    integer :: right

    clamped = rect
    if (buffer%size%width <= 0 .or. buffer%size%height <= 0 .or. .not. rect_valid(rect)) then
      clamped%width = 0
      clamped%height = 0
      return
    end if

    bottom = min(buffer%size%height, rect%row + rect%height - 1)
    right = min(buffer%size%width, rect%col + rect%width - 1)
    clamped%row = max(1, rect%row)
    clamped%col = max(1, rect%col)
    clamped%width = max(0, right - clamped%col + 1)
    clamped%height = max(0, bottom - clamped%row + 1)
    if (clamped%width == 0 .or. clamped%height == 0) then
      clamped%width = 0
      clamped%height = 0
    end if
  end function clamp_rect_to_screen

end module ftop_widgets
