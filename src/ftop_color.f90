module ftop_color
  use fgof_screen, only : clear_screen_style
  use fgof_screen_types, only : screen_style
  implicit none
  private

  integer, parameter :: ANSI256_COLORS = 256

  type, public :: rgb_color
    integer :: r = 0
    integer :: g = 0
    integer :: b = 0
  end type rgb_color

  type, public :: color_gradient
    type(rgb_color), allocatable :: stops(:)
  end type color_gradient

  type(rgb_color), parameter, public :: COLOR_BLACK = rgb_color(0, 0, 0)
  type(rgb_color), parameter, public :: COLOR_RED = rgb_color(205, 49, 49)
  type(rgb_color), parameter, public :: COLOR_GREEN = rgb_color(13, 188, 121)
  type(rgb_color), parameter, public :: COLOR_YELLOW = rgb_color(229, 229, 16)
  type(rgb_color), parameter, public :: COLOR_BLUE = rgb_color(36, 114, 200)
  type(rgb_color), parameter, public :: COLOR_MAGENTA = rgb_color(188, 63, 188)
  type(rgb_color), parameter, public :: COLOR_CYAN = rgb_color(17, 168, 205)
  type(rgb_color), parameter, public :: COLOR_WHITE = rgb_color(229, 229, 229)
  type(rgb_color), parameter, public :: COLOR_BRIGHT_BLACK = rgb_color(102, 102, 102)
  type(rgb_color), parameter, public :: COLOR_BRIGHT_RED = rgb_color(241, 76, 76)
  type(rgb_color), parameter, public :: COLOR_BRIGHT_GREEN = rgb_color(35, 209, 139)
  type(rgb_color), parameter, public :: COLOR_BRIGHT_YELLOW = rgb_color(245, 245, 67)
  type(rgb_color), parameter, public :: COLOR_BRIGHT_BLUE = rgb_color(59, 142, 234)
  type(rgb_color), parameter, public :: COLOR_BRIGHT_MAGENTA = rgb_color(214, 112, 214)
  type(rgb_color), parameter, public :: COLOR_BRIGHT_CYAN = rgb_color(41, 184, 219)
  type(rgb_color), parameter, public :: COLOR_BRIGHT_WHITE = rgb_color(255, 255, 255)
  type(rgb_color), parameter, public :: COLOR_UI_BG = rgb_color(12, 10, 20)
  type(rgb_color), parameter, public :: COLOR_UI_PANEL = rgb_color(32, 22, 56)
  type(rgb_color), parameter, public :: COLOR_UI_DIM = rgb_color(150, 156, 178)
  type(rgb_color), parameter, public :: COLOR_UI_BORDER = rgb_color(126, 87, 255)
  type(rgb_color), parameter, public :: COLOR_UI_ACCENT = rgb_color(164, 240, 255)

  public :: ansi_bg_truecolor
  public :: ansi_fg_truecolor
  public :: gradient_blue_cyan
  public :: gradient_color
  public :: gradient_green_blue
  public :: gradient_green_yellow_red
  public :: make_gradient
  public :: rgb
  public :: rgb_to_ansi256
  public :: style_from_rgb
  public :: style_with_gradient

contains

  elemental function rgb(r, g, b) result(color)
    integer, intent(in) :: r
    integer, intent(in) :: g
    integer, intent(in) :: b
    type(rgb_color) :: color

    color%r = clamp_channel(r)
    color%g = clamp_channel(g)
    color%b = clamp_channel(b)
  end function rgb

  function make_gradient(stops) result(gradient)
    type(rgb_color), intent(in) :: stops(:)
    type(color_gradient) :: gradient

    allocate(gradient%stops(size(stops)))
    gradient%stops = stops
  end function make_gradient

  function gradient_green_yellow_red() result(gradient)
    type(color_gradient) :: gradient
    type(rgb_color) :: stops(3)

    stops = [rgb(46, 204, 113), rgb(241, 196, 15), rgb(231, 76, 60)]
    gradient = make_gradient(stops)
  end function gradient_green_yellow_red

  function gradient_blue_cyan() result(gradient)
    type(color_gradient) :: gradient
    type(rgb_color) :: stops(2)

    stops = [rgb(52, 152, 219), rgb(26, 188, 156)]
    gradient = make_gradient(stops)
  end function gradient_blue_cyan

  function gradient_green_blue() result(gradient)
    type(color_gradient) :: gradient
    type(rgb_color) :: stops(2)

    stops = [rgb(46, 204, 113), rgb(52, 152, 219)]
    gradient = make_gradient(stops)
  end function gradient_green_blue

  function gradient_color(gradient, value) result(color)
    type(color_gradient), intent(in) :: gradient
    real, intent(in) :: value
    type(rgb_color) :: color
    real :: clamped
    real :: position
    real :: fraction
    integer :: left_index
    integer :: stop_count

    color = COLOR_BLACK
    if (.not. allocated(gradient%stops)) return
    stop_count = size(gradient%stops)
    if (stop_count <= 0) return
    if (stop_count == 1) then
      color = gradient%stops(1)
      return
    end if

    clamped = max(0.0, min(1.0, value))
    position = clamped * real(stop_count - 1)
    left_index = min(stop_count - 1, int(position) + 1)
    fraction = position - real(left_index - 1)
    color = interpolate_rgb(gradient%stops(left_index), gradient%stops(left_index + 1), fraction)
  end function gradient_color

  function ansi_fg_truecolor(color) result(sequence)
    type(rgb_color), intent(in) :: color
    character(len=:), allocatable :: sequence

    sequence = achar(27) // "[38;2;" // integer_text(clamp_channel(color%r)) // ";" // &
               integer_text(clamp_channel(color%g)) // ";" // integer_text(clamp_channel(color%b)) // "m"
  end function ansi_fg_truecolor

  function ansi_bg_truecolor(color) result(sequence)
    type(rgb_color), intent(in) :: color
    character(len=:), allocatable :: sequence

    sequence = achar(27) // "[48;2;" // integer_text(clamp_channel(color%r)) // ";" // &
               integer_text(clamp_channel(color%g)) // ";" // integer_text(clamp_channel(color%b)) // "m"
  end function ansi_bg_truecolor

  integer function rgb_to_ansi256(color) result(index)
    type(rgb_color), intent(in) :: color
    type(rgb_color) :: candidate
    integer :: candidate_index
    integer :: candidate_distance
    integer :: best_distance

    index = 0
    best_distance = huge(best_distance)
    do candidate_index = 0, ANSI256_COLORS - 1
      candidate = ansi256_rgb(candidate_index)
      candidate_distance = color_distance_squared(color, candidate)
      if (candidate_distance < best_distance) then
        best_distance = candidate_distance
        index = candidate_index
      end if
    end do
  end function rgb_to_ansi256

  function style_from_rgb(fg, bg, bold) result(style)
    type(rgb_color), intent(in), optional :: fg
    type(rgb_color), intent(in), optional :: bg
    logical, intent(in), optional :: bold
    type(screen_style) :: style

    style = clear_screen_style()
    if (present(fg)) then
      style%fg_truecolor = .true.
      style%fg_rgb = [clamp_channel(fg%r), clamp_channel(fg%g), clamp_channel(fg%b)]
    end if
    if (present(bg)) then
      style%bg_truecolor = .true.
      style%bg_rgb = [clamp_channel(bg%r), clamp_channel(bg%g), clamp_channel(bg%b)]
    end if
    if (present(bold)) style%bold = bold
  end function style_from_rgb

  function style_with_gradient(base_style, gradient, value) result(style)
    type(screen_style), intent(in) :: base_style
    type(color_gradient), intent(in) :: gradient
    real, intent(in) :: value
    type(screen_style) :: style

    style = base_style
    if (.not. allocated(gradient%stops)) return
    associate(color => gradient_color(gradient, value))
      style%fg_truecolor = .true.
      style%fg_rgb = [color%r, color%g, color%b]
    end associate
  end function style_with_gradient

  elemental integer function clamp_channel(value) result(clamped)
    integer, intent(in) :: value

    clamped = max(0, min(255, value))
  end function clamp_channel

  elemental function interpolate_rgb(left, right, fraction) result(color)
    type(rgb_color), intent(in) :: left
    type(rgb_color), intent(in) :: right
    real, intent(in) :: fraction
    type(rgb_color) :: color
    real :: bounded

    bounded = max(0.0, min(1.0, fraction))
    color = rgb(nint(real(left%r) + real(right%r - left%r) * bounded), &
                nint(real(left%g) + real(right%g - left%g) * bounded), &
                nint(real(left%b) + real(right%b - left%b) * bounded))
  end function interpolate_rgb

  type(rgb_color) function ansi256_rgb(index) result(color)
    integer, intent(in) :: index
    integer, parameter :: standard(3, 16) = reshape([ &
      0, 0, 0, 128, 0, 0, 0, 128, 0, 128, 128, 0, &
      0, 0, 128, 128, 0, 128, 0, 128, 128, 192, 192, 192, &
      128, 128, 128, 255, 0, 0, 0, 255, 0, 255, 255, 0, &
      0, 0, 255, 255, 0, 255, 0, 255, 255, 255, 255, 255], [3, 16])
    integer, parameter :: cube_levels(6) = [0, 95, 135, 175, 215, 255]
    integer :: cube_index
    integer :: gray
    integer :: red_index
    integer :: green_index
    integer :: blue_index

    if (index < 16) then
      color = rgb(standard(1, index + 1), standard(2, index + 1), standard(3, index + 1))
    else if (index < 232) then
      cube_index = index - 16
      red_index = cube_index / 36
      green_index = mod(cube_index, 36) / 6
      blue_index = mod(cube_index, 6)
      color = rgb(cube_levels(red_index + 1), cube_levels(green_index + 1), cube_levels(blue_index + 1))
    else
      gray = 8 + (index - 232) * 10
      color = rgb(gray, gray, gray)
    end if
  end function ansi256_rgb

  integer function color_distance_squared(left, right) result(distance)
    type(rgb_color), intent(in) :: left
    type(rgb_color), intent(in) :: right

    distance = (clamp_channel(left%r) - clamp_channel(right%r)) ** 2 + &
               (clamp_channel(left%g) - clamp_channel(right%g)) ** 2 + &
               (clamp_channel(left%b) - clamp_channel(right%b)) ** 2
  end function color_distance_squared

  function integer_text(value) result(text)
    integer, intent(in) :: value
    character(len=:), allocatable :: text
    character(len=32) :: buffer

    write(buffer, '(i0)') value
    text = trim(buffer)
  end function integer_text

end module ftop_color
