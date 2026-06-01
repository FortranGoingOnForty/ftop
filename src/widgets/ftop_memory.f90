module ftop_memory
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use fgof_screen, only : put_glyph
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_box, only : BOX_STYLE_ROUNDED, box_content_rect, draw_box
  use ftop_collector, only : collector_snapshot
  use ftop_color, only : &
    COLOR_BRIGHT_WHITE, &
    COLOR_UI_DIM, &
    color_gradient, &
    gradient_blue_cyan, &
    rgb, &
    style_from_rgb
  use ftop_graph, only : render_graph
  use ftop_mem_data, only : memory_info, memory_usage_percent
  use ftop_meter, only : METER_FILL_SHADED, render_meter
  use ftop_text, only : TEXT_ALIGN_CENTER, format_bytes, format_percent, render_text
  use ftop_widgets, only : widget_rect, widget_size
  implicit none
  private

  public :: memory_panel_min_size
  public :: render_memory_panel

contains

  function memory_panel_min_size() result(size_value)
    type(widget_size) :: size_value

    size_value%width = 28
    size_value%height = 8
  end function memory_panel_min_size

  subroutine render_memory_panel(buffer, panel, snapshot, border_style, title_style, dim_style)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: panel
    type(collector_snapshot), intent(in) :: snapshot
    type(screen_style), intent(in) :: border_style
    type(screen_style), intent(in) :: title_style
    type(screen_style), intent(in) :: dim_style
    type(color_gradient) :: memory_gradient
    type(screen_style) :: text_style
    type(widget_rect) :: content
    integer :: graph_height
    integer :: graph_line

    memory_gradient = gradient_blue_cyan()
    text_style = style_from_rgb(fg=COLOR_BRIGHT_WHITE)

    call draw_box(buffer, panel, BOX_STYLE_ROUNDED, border_style, "Memory", title_style)
    content = box_content_rect(panel)
    if (content%height <= 0 .or. content%width <= 0) return

    call render_text(buffer, content_line_rect(content, 1), memory_summary_text(snapshot), text_style)
    call render_memory_bar(buffer, content_line_rect(content, 2), snapshot%memory, text_style)
    call render_swap_line(buffer, content_line_rect(content, 3), snapshot, memory_gradient, dim_style, text_style)
    call render_text(buffer, content_line_rect(content, 4), memory_available_text(snapshot), dim_style)
    call render_text(buffer, content_line_rect(content, 5), memory_cache_text(snapshot), dim_style)
    call render_text(buffer, content_line_rect(content, 6), memory_breakdown_text(snapshot), dim_style)

    graph_line = 7
    graph_height = max(0, content%height - graph_line + 1)
    if (graph_height > 0 .and. allocated(snapshot%memory_usage_history)) then
      call render_graph(buffer, content_block_rect(content, graph_line, graph_height), &
                        real(snapshot%memory_usage_history), gradient=memory_gradient, &
                        min_value=0.0, max_value=100.0, area_fill=.true., style=dim_style)
    end if
  end subroutine render_memory_panel

  subroutine render_memory_bar(buffer, rect, info, label_style)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    type(memory_info), intent(in) :: info
    type(screen_style), intent(in) :: label_style
    integer(int64) :: cached_bytes
    integer(int64) :: free_bytes
    integer(int64) :: buffer_bytes
    integer(int64) :: used_bytes
    integer :: col
    real(real64) :: position

    if (rect%width <= 0 .or. rect%height <= 0) return
    if (.not. info%valid .or. info%total_bytes <= 0_int64) then
      call render_text(buffer, rect, "memory unavailable", label_style, TEXT_ALIGN_CENTER)
      return
    end if

    free_bytes = bounded_bytes(info%free_bytes, info%total_bytes)
    cached_bytes = bounded_bytes(info%cached_bytes, info%total_bytes - free_bytes)
    buffer_bytes = bounded_bytes(info%buffers_bytes, info%total_bytes - free_bytes - cached_bytes)
    used_bytes = max(0_int64, info%total_bytes - free_bytes - cached_bytes - buffer_bytes)

    do col = 1, rect%width
      position = real(info%total_bytes, real64) * (real(col, real64) - 0.5_real64) / real(rect%width, real64)
      call put_glyph(buffer, rect%row, rect%col + col - 1, "█", &
                     memory_segment_style(position, used_bytes, buffer_bytes, cached_bytes))
    end do
    call render_text(buffer, rect, format_percent(real(memory_usage_percent(info))), label_style, TEXT_ALIGN_CENTER)
  end subroutine render_memory_bar

  function memory_segment_style(position, used_bytes, buffer_bytes, cached_bytes) result(style)
    real(real64), intent(in) :: position
    integer(int64), intent(in) :: used_bytes
    integer(int64), intent(in) :: buffer_bytes
    integer(int64), intent(in) :: cached_bytes
    type(screen_style) :: style
    real(real64) :: used_limit
    real(real64) :: buffers_limit
    real(real64) :: cached_limit

    used_limit = real(used_bytes, real64)
    buffers_limit = used_limit + real(buffer_bytes, real64)
    cached_limit = buffers_limit + real(cached_bytes, real64)
    if (position <= used_limit) then
      style = style_from_rgb(fg=rgb(231, 76, 60))
    else if (position <= buffers_limit) then
      style = style_from_rgb(fg=rgb(241, 196, 15))
    else if (position <= cached_limit) then
      style = style_from_rgb(fg=rgb(52, 152, 219))
    else
      style = style_from_rgb(fg=COLOR_UI_DIM)
    end if
  end function memory_segment_style

  subroutine render_swap_line(buffer, rect, snapshot, memory_gradient, dim_style, text_style)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    type(collector_snapshot), intent(in) :: snapshot
    type(color_gradient), intent(in) :: memory_gradient
    type(screen_style), intent(in) :: dim_style
    type(screen_style), intent(in) :: text_style

    if (rect%width <= 0 .or. rect%height <= 0) return
    if (.not. snapshot%memory%valid .or. snapshot%memory%swap_total_bytes <= 0_int64) then
      call render_text(buffer, rect, swap_text(snapshot), dim_style)
      return
    end if

    call render_meter(buffer, rect, swap_usage_fraction(snapshot%memory), gradient=memory_gradient, &
                      label=swap_text(snapshot), fill_mode=METER_FILL_SHADED, &
                      empty_style=dim_style, label_style=text_style)
  end subroutine render_swap_line

  function content_line_rect(content, line_index) result(line)
    type(widget_rect), intent(in) :: content
    integer, intent(in) :: line_index
    type(widget_rect) :: line

    line = widget_rect(0, 0, 0, 0)
    if (line_index < 1 .or. line_index > content%height) return
    line = widget_rect(content%row + line_index - 1, content%col, content%width, 1)
    if (content%width > 2) then
      line%col = content%col + 1
      line%width = content%width - 2
    end if
  end function content_line_rect

  function content_block_rect(content, start_line, height) result(block)
    type(widget_rect), intent(in) :: content
    integer, intent(in) :: start_line
    integer, intent(in) :: height
    type(widget_rect) :: block

    block = content_line_rect(content, start_line)
    block%height = min(max(0, height), max(0, content%height - start_line + 1))
  end function content_block_rect

  function memory_summary_text(snapshot) result(text)
    type(collector_snapshot), intent(in) :: snapshot
    character(len=:), allocatable :: text

    if (snapshot%memory%valid) then
      text = "Memory " // format_bytes(snapshot%memory%used_bytes) // " / " // &
             format_bytes(snapshot%memory%total_bytes) // " (" // &
             format_percent(real(memory_usage_percent(snapshot%memory))) // ")"
    else
      text = "Memory unavailable"
    end if
  end function memory_summary_text

  function memory_available_text(snapshot) result(text)
    type(collector_snapshot), intent(in) :: snapshot
    character(len=:), allocatable :: text

    if (snapshot%memory%valid) then
      text = "available " // format_bytes(snapshot%memory%available_bytes) // &
             "  free " // format_bytes(snapshot%memory%free_bytes)
    else
      text = "available unknown"
    end if
  end function memory_available_text

  function memory_cache_text(snapshot) result(text)
    type(collector_snapshot), intent(in) :: snapshot
    character(len=:), allocatable :: text

    if (snapshot%memory%valid) then
      text = "cache " // format_bytes(snapshot%memory%cached_bytes) // &
             "  buffers " // format_bytes(snapshot%memory%buffers_bytes)
    else
      text = "cache unknown"
    end if
  end function memory_cache_text

  function memory_breakdown_text(snapshot) result(text)
    type(collector_snapshot), intent(in) :: snapshot
    character(len=:), allocatable :: text

    if (snapshot%memory%valid) then
      text = "used " // format_bytes(snapshot%memory%used_bytes) // &
             "  free " // format_bytes(snapshot%memory%free_bytes)
    else
      text = "used unknown"
    end if
  end function memory_breakdown_text

  function swap_text(snapshot) result(text)
    type(collector_snapshot), intent(in) :: snapshot
    character(len=:), allocatable :: text

    if (.not. snapshot%memory%valid) then
      text = "swap unknown"
    else if (snapshot%memory%swap_total_bytes <= 0_int64) then
      text = "swap none"
    else
      text = "swap " // format_bytes(snapshot%memory%swap_used_bytes) // &
             " / " // format_bytes(snapshot%memory%swap_total_bytes)
    end if
  end function swap_text

  real function swap_usage_fraction(info) result(fraction)
    type(memory_info), intent(in) :: info

    fraction = 0.0
    if (info%swap_total_bytes <= 0_int64) return
    fraction = real(max(0.0_real64, min(1.0_real64, &
                    real(info%swap_used_bytes, real64) / real(info%swap_total_bytes, real64))))
  end function swap_usage_fraction

  integer(int64) function bounded_bytes(bytes, remaining) result(bounded)
    integer(int64), intent(in) :: bytes
    integer(int64), intent(in) :: remaining

    bounded = max(0_int64, min(bytes, max(0_int64, remaining)))
  end function bounded_bytes

end module ftop_memory
