program test_graph
  use fgof_screen, only : allocate_screen, clear_screen_style
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_color, only : gradient_green_blue
  use ftop_graph, only : &
    braille_dot_bit, &
    braille_pattern_glyph, &
    graph_series, &
    graph_value_dot_row, &
    graph_widget, &
    render_graph, &
    render_graph_series
  use ftop_widgets, only : widget_rect, widget_size
  implicit none

  call test_braille_helpers()
  call test_line_rendering()
  call test_area_fill_rendering()
  call test_gradient_style()
  call test_axis_and_grid_rendering()
  call test_graph_widget_type()

contains

  subroutine test_braille_helpers()
    call require(braille_dot_bit(1, 1) == 0, "top-left braille bit mismatch")
    call require(braille_dot_bit(2, 1) == 3, "top-right braille bit mismatch")
    call require(braille_dot_bit(1, 4) == 6, "bottom-left braille bit mismatch")
    call require(braille_dot_bit(2, 4) == 7, "bottom-right braille bit mismatch")
    call require(braille_pattern_glyph(255) == "⣿", "full braille glyph mismatch")
    call require(graph_value_dot_row(1.0, 0.0, 1.0, 8) == 1, "graph high dot row mismatch")
    call require(graph_value_dot_row(0.0, 0.0, 1.0, 8) == 8, "graph low dot row mismatch")
    call require(graph_value_dot_row(0.5, 0.0, 1.0, 8) == 4, "graph middle dot row mismatch")
  end subroutine test_braille_helpers

  subroutine test_line_rendering()
    type(screen_buffer) :: buffer
    real :: values(2)

    buffer = allocate_screen(2, 1)
    values = [0.0, 1.0]
    call render_graph(buffer, widget_rect(row=1, col=1, width=2, height=1), values, &
                      min_value=0.0, max_value=1.0, area_fill=.false.)

    call require_glyph(buffer, 1, 1, braille_pattern_glyph(72), "line graph first cell mismatch")
    call require_glyph(buffer, 1, 2, " ", "line graph unused cell mismatch")
  end subroutine test_line_rendering

  subroutine test_area_fill_rendering()
    type(screen_buffer) :: buffer
    real :: values(1)

    buffer = allocate_screen(1, 1)
    values = [0.5]
    call render_graph(buffer, widget_rect(row=1, col=1, width=1, height=1), values, &
                      min_value=0.0, max_value=1.0, area_fill=.true.)

    call require_glyph(buffer, 1, 1, braille_pattern_glyph(70), "area graph fill mismatch")
  end subroutine test_area_fill_rendering

  subroutine test_gradient_style()
    type(screen_buffer) :: buffer
    real :: values(1)

    buffer = allocate_screen(1, 2)
    values = [1.0]
    call render_graph(buffer, widget_rect(row=1, col=1, width=1, height=2), values, &
                      gradient=gradient_green_blue(), min_value=0.0, max_value=1.0, &
                      area_fill=.false.)

    call require(buffer%cells(1, 1)%style%fg_truecolor, "graph gradient must set truecolor")
    call require(all(buffer%cells(1, 1)%style%fg_rgb == [52, 152, 219]), &
                 "graph top gradient color mismatch")
  end subroutine test_gradient_style

  subroutine test_axis_and_grid_rendering()
    type(screen_buffer) :: buffer
    real :: values(2)

    buffer = allocate_screen(10, 2)
    values = [0.0, 1.0]
    call render_graph(buffer, widget_rect(row=1, col=1, width=10, height=2), values, &
                      min_value=0.0, max_value=1.0, area_fill=.false., &
                      show_grid=.true., show_y_axis=.true.)

    call require_glyph(buffer, 1, 4, "1", "graph axis max label mismatch")
    call require_glyph(buffer, 2, 4, "0", "graph axis min label mismatch")
    call require_glyph(buffer, 1, 7, "│", "graph axis line mismatch")
    call require(buffer%cells(1, 9)%style%dim, "graph grid style must be dim")
  end subroutine test_axis_and_grid_rendering

  subroutine test_graph_widget_type()
    type(graph_series) :: series(2)
    type(graph_widget) :: graph
    type(screen_buffer) :: buffer
    type(screen_style) :: second_style
    type(widget_size) :: size_value

    buffer = allocate_screen(2, 1)
    second_style = clear_screen_style()
    second_style%underline = .true.
    series(1)%values = [0.0]
    series(2)%values = [1.0]
    series(2)%style = second_style
    graph%series = series
    graph%width = 2
    graph%height = 1
    graph%autoscale = .false.
    graph%min_value = 0.0
    graph%max_value = 1.0
    graph%area_fill = .false.

    size_value = graph%min_size()
    call require(size_value%width == 2, "graph widget min width mismatch")
    call require(size_value%height == 1, "graph widget min height mismatch")

    call graph%render(buffer, widget_rect(row=1, col=1, width=2, height=1))
    call require_glyph(buffer, 1, 1, braille_pattern_glyph(65), "graph widget overlay glyph mismatch")
    call require(buffer%cells(1, 1)%style%underline, "graph overlay style mismatch")
  end subroutine test_graph_widget_type

  subroutine require_glyph(buffer, row, col, expected, message)
    type(screen_buffer), intent(in) :: buffer
    integer, intent(in) :: row
    integer, intent(in) :: col
    character(len=*), intent(in) :: expected
    character(len=*), intent(in) :: message

    call require(allocated(buffer%cells(row, col)%glyph), message)
    call require(buffer%cells(row, col)%glyph == expected, message)
  end subroutine require_glyph

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

end program test_graph
