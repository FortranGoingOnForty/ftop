program test_terminal_frame
  use ftop_app, only : render_test_frame
  implicit none

  character(len=:), allocatable :: rendered

  rendered = render_test_frame(80, 24, 1000, 7, "snapshot-ok")

  if (index(rendered, "╔") == 0) error stop "frame should render a Unicode top-left border"
  if (index(rendered, "╚") == 0) error stop "frame should render a Unicode bottom-left border"
  if (index(rendered, "CPU") == 0) error stop "frame should render a CPU panel"
  if (index(rendered, "Memory") == 0) error stop "frame should render a memory panel"
  if (index(rendered, "refresh 1000ms frame 7 fps 1.0") == 0) error stop "frame should render refresh timing"
  if (index(rendered, "snapshot-ok") == 0) error stop "frame should render status text"
  if (index(rendered, achar(27) // "[38;2;126;87;255m") == 0) error stop "frame should use truecolor border"
  if (index(rendered, achar(27) // "[48;2;32;22;56m") == 0) error stop "frame should use truecolor title background"
end program test_terminal_frame
