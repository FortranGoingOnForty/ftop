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
  use ftop_mem_data, only : &
    memory_info, &
    memory_pressure_label, &
    memory_pressure_percent, &
    memory_reclaimable_bytes, &
    memory_usage_percent, &
    memory_used_bytes
  use ftop_meter, only : METER_FILL_SHADED, meter_empty_cell_style, meter_label_cell_style, render_meter
  use ftop_text, only : &
    TEXT_ALIGN_CENTER, &
    format_bytes, &
    format_percent, &
    horizontal_text_state, &
    render_horizontal_text, &
    text_cell_width, &
    truncated_text, &
    utf8_glyph_bytes
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

  subroutine render_memory_panel(buffer, panel, snapshot, border_style, title_style, dim_style, expanded, horizontal_state)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: panel
    type(collector_snapshot), intent(in) :: snapshot
    type(screen_style), intent(in) :: border_style
    type(screen_style), intent(in) :: title_style
    type(screen_style), intent(in) :: dim_style
    logical, intent(in), optional :: expanded
    type(horizontal_text_state), intent(inout), optional :: horizontal_state
    type(color_gradient) :: memory_gradient
    type(screen_style) :: text_style
    type(widget_rect) :: content
    integer :: graph_height
    integer :: graph_line
    logical :: expanded_view

    memory_gradient = gradient_blue_cyan()
    text_style = style_from_rgb(fg=COLOR_BRIGHT_WHITE)
    expanded_view = .false.
    if (present(expanded)) expanded_view = expanded

    call draw_box(buffer, panel, BOX_STYLE_ROUNDED, border_style, "Memory", title_style)
    content = box_content_rect(panel)
    if (content%height <= 0 .or. content%width <= 0) return

    call render_horizontal_text(buffer, content_line_rect(content, 1), memory_summary_text(snapshot), text_style, &
                                horizontal_state)
    call render_memory_bar(buffer, content_line_rect(content, 2), snapshot%memory, text_style, horizontal_state)
    call render_horizontal_text(buffer, content_line_rect(content, 3), memory_pressure_text(snapshot), dim_style, &
                                horizontal_state)
    call render_horizontal_text(buffer, content_line_rect(content, 4), memory_headroom_text(snapshot), dim_style, &
                                horizontal_state)
    call render_horizontal_text(buffer, content_line_rect(content, 5), memory_reclaimable_text(snapshot), dim_style, &
                                horizontal_state)
    call render_swap_line(buffer, content_line_rect(content, 6), snapshot, memory_gradient, dim_style, text_style, &
                          horizontal_state)

    graph_line = 7
    graph_height = max(0, content%height - graph_line + 1)
    if (graph_height > 0 .and. allocated(snapshot%memory_usage_history)) then
      call render_memory_history_graph(buffer, content_block_rect(content, graph_line, graph_height), &
                                       real(snapshot%memory_usage_history), memory_gradient, dim_style, expanded_view)
    end if
  end subroutine render_memory_panel

  subroutine render_memory_history_graph(buffer, rect, values, memory_gradient, dim_style, expanded)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    real, intent(in) :: values(:)
    type(color_gradient), intent(in) :: memory_gradient
    type(screen_style), intent(in) :: dim_style
    logical, intent(in) :: expanded

    if (expanded) then
      call render_graph(buffer, rect, values, gradient=memory_gradient, &
                        min_value=0.0, max_value=100.0, area_fill=.true., style=dim_style)
    else
      call render_graph(buffer, rect, values, gradient=memory_gradient, area_fill=.true., style=dim_style)
    end if
  end subroutine render_memory_history_graph

  subroutine render_memory_bar(buffer, rect, info, label_style, horizontal_state)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    type(memory_info), intent(in) :: info
    type(screen_style), intent(in) :: label_style
    type(horizontal_text_state), intent(inout), optional :: horizontal_state
    integer(int64) :: free_bytes
    integer(int64) :: reclaimable_bytes
    integer(int64) :: used_bytes
    integer :: col
    logical :: free_position
    type(screen_style) :: segment_style
    real(real64) :: position

    if (rect%width <= 0 .or. rect%height <= 0) return
    if (.not. info%valid .or. info%total_bytes <= 0_int64) then
      call render_horizontal_text(buffer, rect, "memory unavailable", label_style, horizontal_state, TEXT_ALIGN_CENTER)
      return
    end if

    used_bytes = memory_used_bytes(info)
    reclaimable_bytes = memory_reclaimable_bytes(info)
    free_bytes = bounded_bytes(info%free_bytes, max(0_int64, info%total_bytes - used_bytes - reclaimable_bytes))

    do col = 1, rect%width
      position = real(info%total_bytes, real64) * (real(col, real64) - 0.5_real64) / real(rect%width, real64)
      segment_style = memory_segment_style(position, used_bytes, reclaimable_bytes)
      free_position = memory_position_is_free(position, used_bytes, reclaimable_bytes)
      if (free_position) then
        call put_glyph(buffer, rect%row, rect%col + col - 1, "░", meter_empty_cell_style(segment_style))
      else
        call put_glyph(buffer, rect%row, rect%col + col - 1, "█", segment_style)
      end if
    end do
    call render_memory_bar_label(buffer, rect, format_percent(real(memory_usage_percent(info))), label_style, &
                                 info%total_bytes, used_bytes, reclaimable_bytes)
  end subroutine render_memory_bar

  subroutine render_memory_bar_label(buffer, rect, label, label_style, total_bytes, used_bytes, reclaimable_bytes)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    character(len=*), intent(in) :: label
    type(screen_style), intent(in) :: label_style
    integer(int64), intent(in) :: total_bytes
    integer(int64), intent(in) :: used_bytes
    integer(int64), intent(in) :: reclaimable_bytes
    character(len=:), allocatable :: clipped
    type(screen_style) :: segment_style
    integer :: draw_col
    integer :: glyph_bytes
    integer :: glyph_index
    integer :: i
    integer :: label_width
    integer :: segment_col
    integer :: target_col
    logical :: free_position
    real(real64) :: position

    if (rect%width <= 0 .or. total_bytes <= 0_int64) return

    clipped = truncated_text(label, rect%width)
    label_width = text_cell_width(clipped)
    if (label_width <= 0) return

    draw_col = rect%col + max(0, (rect%width - label_width) / 2)
    i = 1
    glyph_index = 0
    do while (i <= len(clipped))
      glyph_bytes = utf8_glyph_bytes(clipped, i)
      target_col = draw_col + glyph_index
      if (target_col > rect%col + rect%width - 1) exit
      if (target_col >= rect%col) then
        segment_col = target_col - rect%col + 1
        position = real(total_bytes, real64) * (real(segment_col, real64) - 0.5_real64) / real(rect%width, real64)
        segment_style = memory_segment_style(position, used_bytes, reclaimable_bytes)
        free_position = memory_position_is_free(position, used_bytes, reclaimable_bytes)
        if (free_position) segment_style = meter_empty_cell_style(segment_style)
        call put_glyph(buffer, rect%row, target_col, clipped(i:i + glyph_bytes - 1), &
                       meter_label_cell_style(label_style, segment_style))
      end if
      i = i + glyph_bytes
      glyph_index = glyph_index + 1
    end do
  end subroutine render_memory_bar_label

  function memory_segment_style(position, used_bytes, reclaimable_bytes) result(style)
    real(real64), intent(in) :: position
    integer(int64), intent(in) :: used_bytes
    integer(int64), intent(in) :: reclaimable_bytes
    type(screen_style) :: style
    real(real64) :: used_limit
    real(real64) :: reclaimable_limit

    used_limit = real(used_bytes, real64)
    reclaimable_limit = used_limit + real(reclaimable_bytes, real64)
    if (position <= used_limit) then
      style = style_from_rgb(fg=rgb(231, 76, 60))
    else if (position <= reclaimable_limit) then
      style = style_from_rgb(fg=rgb(52, 152, 219))
    else
      style = style_from_rgb(fg=COLOR_UI_DIM)
    end if
  end function memory_segment_style

  logical function memory_position_is_free(position, used_bytes, reclaimable_bytes) result(is_free)
    real(real64), intent(in) :: position
    integer(int64), intent(in) :: used_bytes
    integer(int64), intent(in) :: reclaimable_bytes
    real(real64) :: reclaimable_limit

    reclaimable_limit = real(used_bytes + reclaimable_bytes, real64)
    is_free = position > reclaimable_limit
  end function memory_position_is_free

  subroutine render_swap_line(buffer, rect, snapshot, memory_gradient, dim_style, text_style, horizontal_state)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    type(collector_snapshot), intent(in) :: snapshot
    type(color_gradient), intent(in) :: memory_gradient
    type(screen_style), intent(in) :: dim_style
    type(screen_style), intent(in) :: text_style
    type(horizontal_text_state), intent(inout), optional :: horizontal_state

    if (rect%width <= 0 .or. rect%height <= 0) return
    if (.not. snapshot%memory%valid .or. snapshot%memory%swap_total_bytes <= 0_int64) then
      call render_horizontal_text(buffer, rect, swap_text(snapshot), dim_style, horizontal_state)
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
      text = "Memory " // format_bytes(memory_used_bytes(snapshot%memory)) // " / " // &
             format_bytes(snapshot%memory%total_bytes) // " (" // &
             format_percent(real(memory_usage_percent(snapshot%memory))) // ")"
    else
      text = "Memory unavailable"
    end if
  end function memory_summary_text

  function memory_pressure_text(snapshot) result(text)
    type(collector_snapshot), intent(in) :: snapshot
    character(len=:), allocatable :: text

    if (snapshot%memory%valid) then
      text = "pressure " // memory_pressure_label(snapshot%memory)
    else
      text = "pressure unknown"
    end if
  end function memory_pressure_text

  function memory_headroom_text(snapshot) result(text)
    type(collector_snapshot), intent(in) :: snapshot
    character(len=:), allocatable :: text
    integer(int64) :: free_bytes
    integer(int64) :: headroom_bytes

    if (snapshot%memory%valid) then
      headroom_bytes = bounded_bytes(snapshot%memory%available_bytes, snapshot%memory%total_bytes)
      free_bytes = bounded_bytes(snapshot%memory%free_bytes, headroom_bytes)
      text = "headroom " // format_bytes(headroom_bytes)
      if (free_bytes < headroom_bytes) text = text // "  free " // format_bytes(free_bytes)
    else
      text = "headroom unknown"
    end if
  end function memory_headroom_text

  function memory_reclaimable_text(snapshot) result(text)
    type(collector_snapshot), intent(in) :: snapshot
    character(len=:), allocatable :: text
    integer(int64) :: cache_bytes
    integer(int64) :: reclaimable_bytes

    if (snapshot%memory%valid) then
      reclaimable_bytes = memory_reclaimable_bytes(snapshot%memory)
      cache_bytes = max(0_int64, snapshot%memory%cached_bytes + snapshot%memory%buffers_bytes)
      if (reclaimable_bytes > 0_int64) then
        text = "reclaim " // format_bytes(reclaimable_bytes)
        if (cache_bytes > 0_int64) text = text // "  cache " // format_bytes(cache_bytes)
      else
        text = "reclaim none"
      end if
    else
      text = "reclaim unknown"
    end if
  end function memory_reclaimable_text

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
