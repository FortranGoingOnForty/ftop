module ftop_cpu
  use, intrinsic :: iso_fortran_env, only : real64
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_box, only : BOX_STYLE_ROUNDED, box_content_rect, draw_box
  use ftop_collector, only : collector_snapshot
  use ftop_color, only : &
    COLOR_BRIGHT_WHITE, &
    color_gradient, &
    gradient_green_yellow_red, &
    style_from_rgb
  use ftop_graph, only : render_graph
  use ftop_meter, only : METER_FILL_SHADED, render_meter
  use ftop_sparkline, only : render_sparkline
  use ftop_text, only : format_percent, render_text
  use ftop_widgets, only : widget_rect, widget_size
  implicit none
  private

  public :: cpu_panel_min_size
  public :: render_cpu_panel

contains

  function cpu_panel_min_size() result(size_value)
    type(widget_size) :: size_value

    size_value%width = 28
    size_value%height = 8
  end function cpu_panel_min_size

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
    integer :: core_start_line
    integer :: graph_height
    integer :: graph_line

    usage_gradient = gradient_green_yellow_red()
    text_style = style_from_rgb(fg=COLOR_BRIGHT_WHITE)

    call draw_box(buffer, panel, BOX_STYLE_ROUNDED, border_style, "CPU", title_style)
    content = box_content_rect(panel)
    if (content%height <= 0 .or. content%width <= 0) return

    call render_text(buffer, content_line_rect(content, 1), cpu_summary_text(snapshot), text_style)
    call render_meter(buffer, content_line_rect(content, 2), cpu_usage_fraction(snapshot), &
                      gradient=usage_gradient, label=cpu_meter_label(snapshot), &
                      fill_mode=METER_FILL_SHADED, empty_style=dim_style, label_style=text_style)
    call render_text(buffer, content_line_rect(content, 3), cpu_detail_text(snapshot), dim_style)
    call render_text(buffer, content_line_rect(content, 4), cpu_model_text(snapshot), dim_style)
    call render_text(buffer, content_line_rect(content, 5), cpu_frequency_temp_text(snapshot), dim_style)

    graph_line = 6
    graph_height = min(4, max(0, content%height - graph_line - 1))
    if (graph_height > 0 .and. allocated(snapshot%cpu_usage_history)) then
      call render_cpu_history(buffer, content, graph_line, graph_height, snapshot, usage_gradient, dim_style)
      core_start_line = graph_line + graph_height
    else
      core_start_line = graph_line
    end if

    call render_core_sparklines(buffer, content, core_start_line, snapshot, usage_gradient, dim_style)
  end subroutine render_cpu_panel

  subroutine render_cpu_history(buffer, content, start_line, height, snapshot, usage_gradient, dim_style)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: content
    integer, intent(in) :: start_line
    integer, intent(in) :: height
    type(collector_snapshot), intent(in) :: snapshot
    type(color_gradient), intent(in) :: usage_gradient
    type(screen_style), intent(in) :: dim_style
    type(widget_rect) :: graph_rect

    graph_rect = content_block_rect(content, start_line, height)
    if (graph_rect%width <= 0 .or. graph_rect%height <= 0) return
    if (height == 1) then
      call render_sparkline(buffer, graph_rect, real(snapshot%cpu_usage_history), gradient=usage_gradient, &
                            min_value=0.0, max_value=100.0, style=dim_style)
    else
      call render_graph(buffer, graph_rect, real(snapshot%cpu_usage_history), gradient=usage_gradient, &
                        min_value=0.0, max_value=100.0, area_fill=.true., style=dim_style)
    end if
  end subroutine render_cpu_history

  subroutine render_core_sparklines(buffer, content, start_line, snapshot, usage_gradient, dim_style)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: content
    integer, intent(in) :: start_line
    type(collector_snapshot), intent(in) :: snapshot
    type(color_gradient), intent(in) :: usage_gradient
    type(screen_style), intent(in) :: dim_style
    type(widget_rect) :: label_rect
    type(widget_rect) :: line
    type(widget_rect) :: spark_rect
    integer :: core_count
    integer :: core_index
    integer :: label_width
    integer :: visible_count

    if (.not. allocated(snapshot%cpu_cores)) return
    core_count = size(snapshot%cpu_cores)
    if (core_count <= 0 .or. start_line > content%height) return

    label_width = min(5, content%width)
    visible_count = min(core_count, content%height - start_line + 1)
    do core_index = 1, visible_count
      line = content_line_rect(content, start_line + core_index - 1)
      if (line%width <= 0) cycle
      label_rect = widget_rect(line%row, line%col, label_width, 1)
      call render_text(buffer, label_rect, "c" // integer_text(core_index - 1), dim_style)
      if (line%width <= label_width + 1) cycle

      spark_rect = widget_rect(line%row, line%col + label_width + 1, &
                               line%width - label_width - 1, 1)
      if (allocated(snapshot%cpu_core_usage_history)) then
        if (core_index <= size(snapshot%cpu_core_usage_history, 1)) then
          call render_sparkline(buffer, spark_rect, &
                                real(snapshot%cpu_core_usage_history(core_index, :)), &
                                gradient=usage_gradient, min_value=0.0, max_value=100.0, &
                                style=dim_style)
        end if
      else
        call render_meter(buffer, spark_rect, core_usage_fraction(snapshot, core_index), &
                          gradient=usage_gradient, fill_mode=METER_FILL_SHADED, &
                          empty_style=dim_style, label_style=dim_style)
      end if
    end do
  end subroutine render_core_sparklines

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

  function cpu_frequency_temp_text(snapshot) result(text)
    type(collector_snapshot), intent(in) :: snapshot
    character(len=:), allocatable :: text
    real(real64) :: freq_sum
    real(real64) :: temp_sum
    integer :: core_index
    integer :: freq_count
    integer :: temp_count

    freq_sum = 0.0_real64
    temp_sum = 0.0_real64
    freq_count = 0
    temp_count = 0
    if (allocated(snapshot%cpu_cores)) then
      do core_index = 1, size(snapshot%cpu_cores)
        if (snapshot%cpu_cores(core_index)%freq_valid) then
          freq_sum = freq_sum + snapshot%cpu_cores(core_index)%freq_mhz
          freq_count = freq_count + 1
        end if
        if (snapshot%cpu_cores(core_index)%temp_valid) then
          temp_sum = temp_sum + snapshot%cpu_cores(core_index)%temp_c
          temp_count = temp_count + 1
        end if
      end do
    end if

    text = "freq " // average_text(freq_sum, freq_count, "MHz") // &
           " temp " // average_text(temp_sum, temp_count, "C")
  end function cpu_frequency_temp_text

  function average_text(total, count, unit) result(text)
    real(real64), intent(in) :: total
    integer, intent(in) :: count
    character(len=*), intent(in) :: unit
    character(len=:), allocatable :: text

    if (count <= 0) then
      text = "n/a"
    else
      text = real_text(total / real(count, real64)) // " " // unit
    end if
  end function average_text

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

  function format_load(load_avg) result(text)
    real(real64), intent(in) :: load_avg(3)
    character(len=:), allocatable :: text

    text = "load " // real_text(load_avg(1)) // " " // real_text(load_avg(2)) // &
           " " // real_text(load_avg(3))
  end function format_load

  function real_text(value) result(text)
    real(real64), intent(in) :: value
    character(len=:), allocatable :: text
    character(len=32) :: scratch

    write(scratch, '(f6.1)') value
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

end module ftop_cpu
