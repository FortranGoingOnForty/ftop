module ftop_dashboard
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use fgof_screen, only : clear_screen
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_box, only : BOX_STYLE_DOUBLE, BOX_STYLE_ROUNDED, box_content_rect, draw_box
  use ftop_collector, only : collector_snapshot
  use ftop_color, only : &
    COLOR_BRIGHT_WHITE, &
    COLOR_UI_ACCENT, &
    COLOR_UI_BORDER, &
    COLOR_UI_DIM, &
    COLOR_UI_PANEL, &
    color_gradient, &
    gradient_blue_cyan, &
    gradient_green_yellow_red, &
    style_from_rgb
  use ftop_mem_data, only : memory_usage_percent
  use ftop_meter, only : METER_FILL_SHADED, render_meter
  use ftop_sparkline, only : render_sparkline
  use ftop_text, only : TEXT_ALIGN_CENTER, format_bytes, format_percent, render_text
  use ftop_widgets, only : widget_rect
  implicit none
  private

  type, public :: dashboard_layout
    type(widget_rect) :: frame
    type(widget_rect) :: cpu_panel
    type(widget_rect) :: memory_panel
    type(widget_rect) :: footer
  end type dashboard_layout

  public :: default_dashboard_layout
  public :: render_dashboard

contains

  subroutine render_dashboard(buffer, snapshot, refresh_ms, frame_count, status_text)
    type(screen_buffer), intent(inout) :: buffer
    type(collector_snapshot), intent(in) :: snapshot
    integer, intent(in) :: refresh_ms
    integer, intent(in) :: frame_count
    character(len=*), intent(in) :: status_text
    type(dashboard_layout) :: layout
    type(screen_style) :: border_style
    type(screen_style) :: title_style
    type(screen_style) :: dim_style
    type(widget_rect) :: title_rect
    integer :: height
    integer :: title_col
    integer :: width

    width = buffer%size%width
    height = buffer%size%height
    if (width <= 0 .or. height <= 0) return

    border_style = style_from_rgb(fg=COLOR_UI_BORDER)
    title_style = style_from_rgb(fg=COLOR_UI_ACCENT, bg=COLOR_UI_PANEL, bold=.true.)
    dim_style = style_from_rgb(fg=COLOR_UI_DIM)

    call clear_screen(buffer)
    buffer%cursor_visible = .false.

    if (width < 8 .or. height < 4) then
      call render_text(buffer, widget_rect(1, 1, width, 1), "ftop", title_style, TEXT_ALIGN_CENTER)
      return
    end if

    layout = default_dashboard_layout(width, height)
    call draw_box(buffer, layout%frame, BOX_STYLE_DOUBLE, border_style, "ftop", title_style, TEXT_ALIGN_CENTER)

    if (layout%cpu_panel%height >= 3) then
      call render_cpu_panel(buffer, layout%cpu_panel, snapshot, border_style, title_style, dim_style)
    end if
    if (layout%memory_panel%height >= 3) then
      call render_memory_panel(buffer, layout%memory_panel, snapshot, border_style, title_style, dim_style)
    end if

    if (layout%cpu_panel%height < 3 .or. layout%memory_panel%height < 3) then
      title_col = max(2, (width - len_trim("CPU / Memory")) / 2 + 1)
      title_rect = widget_rect(max(2, height / 2), title_col, width - title_col, 1)
      call render_text(buffer, title_rect, "CPU / Memory", title_style)
    end if

    call render_footer(buffer, layout%footer, snapshot, refresh_ms, frame_count, status_text, dim_style)
  end subroutine render_dashboard

  function default_dashboard_layout(width, height) result(layout)
    integer, intent(in) :: width
    integer, intent(in) :: height
    type(dashboard_layout) :: layout
    integer :: body_height
    integer :: body_row
    integer :: body_width
    integer :: gap
    integer :: left_width
    integer :: panel_height
    integer :: right_width

    layout%frame = widget_rect(1, 1, max(0, width), max(0, height))
    layout%cpu_panel = widget_rect(0, 0, 0, 0)
    layout%memory_panel = widget_rect(0, 0, 0, 0)
    layout%footer = widget_rect(max(1, height - 2), 3, max(0, width - 4), min(2, max(0, height - 2)))

    if (width < 8 .or. height < 6) return

    body_row = 3
    body_width = max(0, width - 4)
    body_height = max(0, height - 5)
    if (body_width <= 0 .or. body_height <= 0) return

    if (width >= 72) then
      gap = 2
      left_width = max(0, (body_width - gap) / 2)
      right_width = max(0, body_width - gap - left_width)
      layout%cpu_panel = widget_rect(body_row, 3, left_width, body_height)
      layout%memory_panel = widget_rect(body_row, 3 + left_width + gap, right_width, body_height)
    else
      gap = 1
      panel_height = min(body_height, max(3, (body_height - gap) / 2))
      layout%cpu_panel = widget_rect(body_row, 3, body_width, panel_height)
      layout%memory_panel = widget_rect(body_row + panel_height + gap, 3, body_width, &
                                        max(0, body_height - panel_height - gap))
    end if
  end function default_dashboard_layout

  subroutine render_cpu_panel(buffer, panel, snapshot, border_style, title_style, dim_style)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: panel
    type(collector_snapshot), intent(in) :: snapshot
    type(screen_style), intent(in) :: border_style
    type(screen_style), intent(in) :: title_style
    type(screen_style), intent(in) :: dim_style
    type(color_gradient) :: usage_gradient
    type(screen_style) :: text_style
    type(widget_rect) :: content
    type(widget_rect) :: line

    usage_gradient = gradient_green_yellow_red()
    text_style = style_from_rgb(fg=COLOR_BRIGHT_WHITE)

    call draw_box(buffer, panel, BOX_STYLE_ROUNDED, border_style, "CPU", title_style)
    content = box_content_rect(panel)
    if (content%height <= 0 .or. content%width <= 0) return

    line = content_line_rect(content, 1)
    call render_text(buffer, line, cpu_summary_text(snapshot), text_style)

    line = content_line_rect(content, 2)
    call render_meter(buffer, line, cpu_usage_fraction(snapshot), gradient=usage_gradient, &
                      label=cpu_meter_label(snapshot), fill_mode=METER_FILL_SHADED, &
                      empty_style=dim_style, label_style=text_style)

    line = content_line_rect(content, 3)
    if (allocated(snapshot%cpu_usage_history)) then
      call render_sparkline(buffer, line, real(snapshot%cpu_usage_history), gradient=usage_gradient, &
                            min_value=0.0, max_value=100.0, style=dim_style)
    end if

    line = content_line_rect(content, 4)
    call render_text(buffer, line, cpu_detail_text(snapshot), dim_style)

    line = content_line_rect(content, 5)
    call render_text(buffer, line, cpu_model_text(snapshot), dim_style)

    line = content_line_rect(content, 6)
    call render_core_meters(buffer, line, snapshot, usage_gradient, dim_style, text_style)
  end subroutine render_cpu_panel

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
    type(widget_rect) :: line

    memory_gradient = gradient_blue_cyan()
    text_style = style_from_rgb(fg=COLOR_BRIGHT_WHITE)

    call draw_box(buffer, panel, BOX_STYLE_ROUNDED, border_style, "Memory", title_style)
    content = box_content_rect(panel)
    if (content%height <= 0 .or. content%width <= 0) return

    line = content_line_rect(content, 1)
    call render_text(buffer, line, memory_summary_text(snapshot), text_style)

    line = content_line_rect(content, 2)
    call render_meter(buffer, line, memory_usage_fraction(snapshot), gradient=memory_gradient, &
                      label=memory_meter_label(snapshot), fill_mode=METER_FILL_SHADED, &
                      empty_style=dim_style, label_style=text_style)

    line = content_line_rect(content, 3)
    if (allocated(snapshot%memory_usage_history)) then
      call render_sparkline(buffer, line, real(snapshot%memory_usage_history), gradient=memory_gradient, &
                            min_value=0.0, max_value=100.0, style=dim_style)
    end if

    line = content_line_rect(content, 4)
    call render_text(buffer, line, memory_available_text(snapshot), dim_style)

    line = content_line_rect(content, 5)
    call render_text(buffer, line, memory_cache_text(snapshot), dim_style)

    line = content_line_rect(content, 6)
    call render_text(buffer, line, swap_text(snapshot), dim_style)
  end subroutine render_memory_panel

  subroutine render_core_meters(buffer, line, snapshot, usage_gradient, dim_style, text_style)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: line
    type(collector_snapshot), intent(in) :: snapshot
    type(color_gradient), intent(in) :: usage_gradient
    type(screen_style), intent(in) :: dim_style
    type(screen_style), intent(in) :: text_style
    type(widget_rect) :: label_rect
    type(widget_rect) :: meter_rect
    integer :: available_width
    integer :: core_count
    integer :: core_index
    integer :: label_width
    integer :: meter_col
    integer :: meter_width
    integer :: visible_count

    if (line%width <= 0 .or. line%height <= 0) return
    if (.not. allocated(snapshot%cpu_cores)) return
    core_count = size(snapshot%cpu_cores)
    if (core_count <= 0) return

    label_width = min(6, line%width)
    label_rect = widget_rect(line%row, line%col, label_width, 1)
    call render_text(buffer, label_rect, "cores", dim_style)

    available_width = line%width - label_width - 1
    if (available_width <= 0) return

    meter_width = 3
    visible_count = min(core_count, max(1, (available_width + 1) / (meter_width + 1)))
    meter_col = line%col + label_width + 1
    do core_index = 1, visible_count
      meter_rect = widget_rect(line%row, meter_col, min(meter_width, line%col + line%width - meter_col), 1)
      call render_meter(buffer, meter_rect, core_usage_fraction(snapshot, core_index), gradient=usage_gradient, &
                        fill_mode=METER_FILL_SHADED, empty_style=dim_style, label_style=text_style)
      meter_col = meter_col + meter_width + 1
    end do

    if (visible_count < core_count .and. meter_col < line%col + line%width) then
      call render_text(buffer, widget_rect(line%row, meter_col, line%col + line%width - meter_col, 1), &
                       "+" // integer_text(core_count - visible_count), dim_style)
    end if
  end subroutine render_core_meters

  subroutine render_footer(buffer, footer, snapshot, refresh_ms, frame_count, status_text, dim_style)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: footer
    type(collector_snapshot), intent(in) :: snapshot
    integer, intent(in) :: refresh_ms
    integer, intent(in) :: frame_count
    character(len=*), intent(in) :: status_text
    type(screen_style), intent(in) :: dim_style
    type(widget_rect) :: line
    character(len=:), allocatable :: status

    if (footer%width <= 0 .or. footer%height <= 0) return

    line = widget_rect(footer%row, footer%col, footer%width, 1)
    call render_text(buffer, line, footer_text(snapshot, refresh_ms, frame_count), dim_style)

    if (footer%height < 2) return
    status = trim(status_text)
    if (len(status) == 0) status = "ready"
    line = widget_rect(footer%row + 1, footer%col, footer%width, 1)
    call render_text(buffer, line, status, dim_style)
  end subroutine render_footer

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

  function cpu_summary_text(snapshot) result(text)
    type(collector_snapshot), intent(in) :: snapshot
    character(len=:), allocatable :: text

    if (snapshot%cpu_total%valid) then
      text = "CPU " // format_percent(real(snapshot%cpu_total%usage_percent))
    else if (snapshot%warming_up) then
      text = "CPU warming up"
    else
      text = "CPU unavailable"
    end if
  end function cpu_summary_text

  function cpu_meter_label(snapshot) result(text)
    type(collector_snapshot), intent(in) :: snapshot
    character(len=:), allocatable :: text

    if (snapshot%cpu_total%valid) then
      text = format_percent(real(snapshot%cpu_total%usage_percent))
    else if (snapshot%warming_up) then
      text = "warming"
    else
      text = "no cpu"
    end if
  end function cpu_meter_label

  function cpu_detail_text(snapshot) result(text)
    type(collector_snapshot), intent(in) :: snapshot
    character(len=:), allocatable :: text

    text = "cores " // integer_text(snapshot%cpu_total%core_count) // &
           " threads " // integer_text(snapshot%cpu_total%thread_count)
    if (snapshot%cpu_total%load_valid) text = text // "  " // format_load(snapshot%cpu_total%load_avg)
  end function cpu_detail_text

  function cpu_model_text(snapshot) result(text)
    type(collector_snapshot), intent(in) :: snapshot
    character(len=:), allocatable :: text

    if (snapshot%cpu_total%model_name_valid .and. len_trim(snapshot%cpu_total%model_name) > 0) then
      text = trim(snapshot%cpu_total%model_name)
    else
      text = "model unknown"
    end if
  end function cpu_model_text

  real function cpu_usage_fraction(snapshot) result(fraction)
    type(collector_snapshot), intent(in) :: snapshot

    fraction = 0.0
    if (snapshot%cpu_total%valid) fraction = percent_fraction(snapshot%cpu_total%usage_percent)
  end function cpu_usage_fraction

  real function core_usage_fraction(snapshot, core_index) result(fraction)
    type(collector_snapshot), intent(in) :: snapshot
    integer, intent(in) :: core_index

    fraction = 0.0
    if (.not. allocated(snapshot%cpu_cores)) return
    if (core_index < 1 .or. core_index > size(snapshot%cpu_cores)) return
    if (snapshot%cpu_cores(core_index)%valid) then
      fraction = percent_fraction(snapshot%cpu_cores(core_index)%usage_percent)
    end if
  end function core_usage_fraction

  function memory_summary_text(snapshot) result(text)
    type(collector_snapshot), intent(in) :: snapshot
    character(len=:), allocatable :: text

    if (snapshot%memory%valid) then
      text = "Memory " // format_percent(real(memory_usage_percent(snapshot%memory))) // "  " // &
             format_bytes(snapshot%memory%used_bytes) // " / " // format_bytes(snapshot%memory%total_bytes)
    else
      text = "Memory unavailable"
    end if
  end function memory_summary_text

  function memory_meter_label(snapshot) result(text)
    type(collector_snapshot), intent(in) :: snapshot
    character(len=:), allocatable :: text

    if (snapshot%memory%valid) then
      text = format_percent(real(memory_usage_percent(snapshot%memory)))
    else
      text = "no memory"
    end if
  end function memory_meter_label

  real function memory_usage_fraction(snapshot) result(fraction)
    type(collector_snapshot), intent(in) :: snapshot

    fraction = 0.0
    if (snapshot%memory%valid) fraction = percent_fraction(memory_usage_percent(snapshot%memory))
  end function memory_usage_fraction

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

  function footer_text(snapshot, refresh_ms, frame_count) result(text)
    type(collector_snapshot), intent(in) :: snapshot
    integer, intent(in) :: refresh_ms
    integer, intent(in) :: frame_count
    character(len=:), allocatable :: text
    character(len=:), allocatable :: running_text

    if (snapshot%running) then
      running_text = "collector running"
    else
      running_text = "collector idle"
    end if
    text = "refresh " // integer_text(refresh_ms) // "ms frame " // integer_text(frame_count) // &
           " samples " // integer_text(snapshot%sample_count) // "  " // running_text // &
           "  q/Ctrl+C quit Ctrl+Z suspend"
  end function footer_text

  function format_load(load_avg) result(text)
    real(real64), intent(in) :: load_avg(3)
    character(len=:), allocatable :: text

    text = "load " // real_text(load_avg(1)) // " " // real_text(load_avg(2)) // " " // real_text(load_avg(3))
  end function format_load

  function real_text(value) result(text)
    real(real64), intent(in) :: value
    character(len=:), allocatable :: text
    character(len=32) :: scratch

    write(scratch, '(f6.2)') value
    text = trim(adjustl(scratch))
  end function real_text

  real function percent_fraction(value) result(fraction)
    real(real64), intent(in) :: value

    fraction = real(max(0.0_real64, min(100.0_real64, value)) / 100.0_real64)
  end function percent_fraction

  function integer_text(value) result(text)
    integer, intent(in) :: value
    character(len=:), allocatable :: text
    character(len=32) :: scratch

    write(scratch, '(i0)') value
    text = trim(scratch)
  end function integer_text

end module ftop_dashboard
