program test_color
  use fgof_screen_types, only : screen_style
  use ftop_color, only : &
    COLOR_BRIGHT_WHITE, &
    COLOR_UI_PANEL, &
    ansi_bg_truecolor, &
    ansi_fg_truecolor, &
    color_gradient, &
    gradient_blue_cyan, &
    gradient_color, &
    gradient_green_blue, &
    gradient_green_yellow_red, &
    make_gradient, &
    rgb, &
    rgb_color, &
    rgb_to_ansi256, &
    style_from_rgb, &
    style_with_gradient
  implicit none

  call test_rgb_clamps_channels()
  call test_truecolor_sequences()
  call test_gradient_interpolation()
  call test_gradient_presets()
  call test_ansi256_mapping()
  call test_style_from_rgb()
  call test_style_with_gradient()
  call test_named_colors()

contains

  subroutine test_rgb_clamps_channels()
    type(rgb_color) :: color

    color = rgb(-4, 300, 42)
    call require_color(color, 0, 255, 42, "rgb must clamp channels")
  end subroutine test_rgb_clamps_channels

  subroutine test_truecolor_sequences()
    call require(ansi_fg_truecolor(rgb(1, 2, 3)) == achar(27) // "[38;2;1;2;3m", &
                 "foreground truecolor sequence mismatch")
    call require(ansi_bg_truecolor(rgb(-1, 256, 3)) == achar(27) // "[48;2;0;255;3m", &
                 "background truecolor sequence mismatch")
  end subroutine test_truecolor_sequences

  subroutine test_gradient_interpolation()
    type(rgb_color) :: color
    type(rgb_color) :: stops(3)
    type(color_gradient) :: gradient

    stops = [rgb(0, 0, 0), rgb(100, 50, 0), rgb(200, 100, 0)]
    gradient = make_gradient(stops)

    color = gradient_color(gradient, -1.0)
    call require_color(color, 0, 0, 0, "gradient must clamp low")

    color = gradient_color(gradient, 0.75)
    call require_color(color, 150, 75, 0, "gradient midpoint mismatch")

    color = gradient_color(gradient, 2.0)
    call require_color(color, 200, 100, 0, "gradient must clamp high")
  end subroutine test_gradient_interpolation

  subroutine test_gradient_presets()
    type(rgb_color) :: color
    type(color_gradient) :: gradient

    gradient = gradient_green_yellow_red()
    color = gradient_color(gradient, 0.0)
    call require_color(color, 46, 204, 113, "CPU gradient first stop mismatch")
    color = gradient_color(gradient, 1.0)
    call require_color(color, 231, 76, 60, "CPU gradient last stop mismatch")

    gradient = gradient_blue_cyan()
    color = gradient_color(gradient, 1.0)
    call require_color(color, 26, 188, 156, "network gradient last stop mismatch")

    gradient = gradient_green_blue()
    color = gradient_color(gradient, 1.0)
    call require_color(color, 52, 152, 219, "disk gradient last stop mismatch")
  end subroutine test_gradient_presets

  subroutine test_ansi256_mapping()
    call require(rgb_to_ansi256(rgb(0, 0, 0)) == 0, "black ANSI-256 index mismatch")
    call require(rgb_to_ansi256(rgb(255, 0, 0)) == 9, "red ANSI-256 index mismatch")
    call require(rgb_to_ansi256(rgb(0, 95, 255)) == 27, "color cube ANSI-256 index mismatch")
  end subroutine test_ansi256_mapping

  subroutine test_style_from_rgb()
    type(screen_style) :: style

    style = style_from_rgb(fg=rgb(1, 2, 3), bg=rgb(4, 5, 6), bold=.true.)
    call require(style%fg_truecolor, "style must use truecolor foreground")
    call require(style%bg_truecolor, "style must use truecolor background")
    call require(all(style%fg_rgb == [1, 2, 3]), "style foreground RGB mismatch")
    call require(all(style%bg_rgb == [4, 5, 6]), "style background RGB mismatch")
    call require(style%bold, "style bold flag mismatch")
  end subroutine test_style_from_rgb

  subroutine test_style_with_gradient()
    type(color_gradient) :: gradient
    type(screen_style) :: base_style
    type(screen_style) :: style

    base_style%bold = .true.
    gradient = gradient_green_yellow_red()
    style = style_with_gradient(base_style, gradient, 0.50)

    call require(style%bold, "gradient style must preserve base style")
    call require(style%fg_truecolor, "gradient style must set truecolor foreground")
    call require(all(style%fg_rgb == [241, 196, 15]), "gradient style RGB mismatch")
  end subroutine test_style_with_gradient

  subroutine test_named_colors()
    call require_color(COLOR_BRIGHT_WHITE, 255, 255, 255, "bright white mismatch")
    call require_color(COLOR_UI_PANEL, 32, 22, 56, "UI panel mismatch")
  end subroutine test_named_colors

  subroutine require_color(color, r, g, b, message)
    type(rgb_color), intent(in) :: color
    integer, intent(in) :: r
    integer, intent(in) :: g
    integer, intent(in) :: b
    character(len=*), intent(in) :: message

    call require(color%r == r .and. color%g == g .and. color%b == b, message)
  end subroutine require_color

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

end program test_color
