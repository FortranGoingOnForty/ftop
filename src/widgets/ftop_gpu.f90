module ftop_gpu
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_box, only : BOX_STYLE_ROUNDED, box_content_rect, draw_box
  use ftop_collector, only : collector_snapshot
  use ftop_color, only : &
    COLOR_BRIGHT_WHITE, &
    color_gradient, &
    gradient_green_yellow_red, &
    style_from_rgb
  use ftop_gpu_data, only : gpu_info, gpu_process_info, gpu_process_table, gpu_table
  use ftop_meter, only : METER_FILL_SHADED, render_meter
  use ftop_sparkline, only : render_sparkline
  use ftop_text, only : &
    TEXT_ALIGN_CENTER, &
    format_bytes, &
    format_percent, &
    horizontal_text_begin_frame, &
    horizontal_text_end_frame, &
    horizontal_text_state, &
    render_horizontal_text
  use ftop_widgets, only : widget_rect, widget_size
  implicit none
  private

  type, public :: gpu_process_state
    integer :: selected_row = 1
    integer :: scroll_row = 1
    integer :: row_count = 0
    integer :: viewport_rows = 0
  end type gpu_process_state

  public :: gpu_panel_min_size
  public :: gpu_process_page_delta
  public :: gpu_process_select_delta
  public :: gpu_process_status
  public :: render_gpu_panel

contains

  function gpu_panel_min_size() result(size_value)
    type(widget_size) :: size_value

    size_value%width = 24
    size_value%height = 5
  end function gpu_panel_min_size

  subroutine render_gpu_panel(buffer, panel, snapshot, border_style, title_style, dim_style, expanded, state, process_active, &
                              horizontal_state)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: panel
    type(collector_snapshot), intent(in) :: snapshot
    type(screen_style), intent(in) :: border_style
    type(screen_style), intent(in) :: title_style
    type(screen_style), intent(in) :: dim_style
    logical, intent(in), optional :: expanded
    type(gpu_process_state), intent(inout), optional :: state
    logical, intent(in), optional :: process_active
    type(horizontal_text_state), intent(inout), optional :: horizontal_state
    type(screen_style) :: text_style
    type(widget_rect) :: content
    integer :: gpu_index
    integer :: line_index
    logical :: actual_expanded
    logical :: actual_process_active

    actual_expanded = .false.
    if (present(expanded)) actual_expanded = expanded
    actual_process_active = .false.
    if (present(process_active)) actual_process_active = process_active

    text_style = style_from_rgb(fg=COLOR_BRIGHT_WHITE)
    call draw_box(buffer, panel, BOX_STYLE_ROUNDED, border_style, "GPU", title_style)
    if (present(horizontal_state)) call horizontal_text_begin_frame(horizontal_state)
    content = box_content_rect(panel)
    if (content%height <= 0 .or. content%width <= 0) then
      if (present(horizontal_state)) call horizontal_text_end_frame(horizontal_state)
      return
    end if

    if (.not. snapshot%gpu%valid) then
      call render_horizontal_text(buffer, content_line_rect(content, 1), "gpu unavailable", dim_style, horizontal_state, &
                                  TEXT_ALIGN_CENTER)
      if (present(horizontal_state)) call horizontal_text_end_frame(horizontal_state)
      return
    end if

    if (.not. allocated(snapshot%gpu%gpus) .or. valid_gpu_count(snapshot%gpu) <= 0) then
      call render_horizontal_text(buffer, content_line_rect(content, 1), "No GPUs detected", dim_style, horizontal_state, &
                                  TEXT_ALIGN_CENTER)
      if (present(horizontal_state)) call horizontal_text_end_frame(horizontal_state)
      return
    end if

    call render_horizontal_text(buffer, content_line_rect(content, 1), gpu_summary_text(snapshot%gpu), text_style, &
                                horizontal_state)
    line_index = 2
    do gpu_index = 1, size(snapshot%gpu%gpus)
      if (.not. snapshot%gpu%gpus(gpu_index)%valid) cycle
      if (line_index > content%height) exit
      call render_horizontal_text(buffer, content_line_rect(content, line_index), gpu_title_text(snapshot%gpu%gpus(gpu_index)), &
                                  text_style, horizontal_state)
      line_index = line_index + 1
      if (line_index > content%height) cycle
      call render_gpu_utilization(buffer, content_line_rect(content, line_index), snapshot%gpu%gpus(gpu_index), dim_style, &
                                  horizontal_state)
      line_index = line_index + 1
      if (line_index > content%height) cycle
      if (gpu_history_available(snapshot, gpu_index)) then
        call render_gpu_history(buffer, content_line_rect(content, line_index), snapshot, gpu_index, dim_style, &
                                horizontal_state)
        line_index = line_index + 1
      end if
      if (line_index > content%height) cycle
      call render_horizontal_text(buffer, content_line_rect(content, line_index), gpu_metric_text(snapshot%gpu%gpus(gpu_index)), &
                                  dim_style, horizontal_state)
      line_index = line_index + 1
    end do
    if (line_index <= content%height) then
      if (present(state)) then
        call render_gpu_processes(buffer, content, line_index, snapshot%gpu_processes, text_style, title_style, dim_style, &
                                  actual_expanded, state, actual_process_active, horizontal_state)
      else
        call render_gpu_processes(buffer, content, line_index, snapshot%gpu_processes, text_style, title_style, dim_style, &
                                  actual_expanded, process_active=actual_process_active, horizontal_state=horizontal_state)
      end if
    end if
    if (present(horizontal_state)) call horizontal_text_end_frame(horizontal_state)
  end subroutine render_gpu_panel

  subroutine render_gpu_processes(buffer, content, line_index, processes, text_style, selected_style, dim_style, expanded, &
                                  state, process_active, horizontal_state)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: content
    integer, intent(inout) :: line_index
    type(gpu_process_table), intent(in) :: processes
    type(screen_style), intent(in) :: text_style
    type(screen_style), intent(in) :: selected_style
    type(screen_style), intent(in) :: dim_style
    logical, intent(in) :: expanded
    type(gpu_process_state), intent(inout), optional :: state
    logical, intent(in), optional :: process_active
    type(horizontal_text_state), intent(inout), optional :: horizontal_state
    type(gpu_process_state) :: active_state
    type(gpu_process_info), allocatable :: visible_processes(:)
    type(screen_style) :: row_style
    type(widget_rect) :: row_rect
    integer :: actual_index
    integer :: preview_row
    integer :: remaining_lines
    integer :: start_index
    integer :: viewport_rows
    character(len=:), allocatable :: process_line
    logical :: active
    logical :: stateful

    remaining_lines = content%height - line_index + 1
    if (remaining_lines <= 0) return
    if (.not. expanded .and. remaining_lines < 3) then
      if (present(state)) call clear_gpu_process_state(state)
      return
    end if

    if (.not. processes%valid) then
      if (present(state)) call clear_gpu_process_state(state)
      if (expanded) call render_horizontal_text(buffer, content_line_rect(content, line_index), "GPU processes unsupported", &
                                               dim_style, horizontal_state)
      return
    end if
    if (.not. allocated(processes%processes) .or. valid_gpu_process_count(processes) <= 0) then
      if (present(state)) call clear_gpu_process_state(state)
      if (expanded) call render_horizontal_text(buffer, content_line_rect(content, line_index), "No GPU process activity", &
                                               dim_style, horizontal_state)
      return
    end if

    visible_processes = valid_gpu_processes(processes)
    active = .false.
    if (present(process_active)) active = process_active
    stateful = present(state)

    call render_horizontal_text(buffer, content_line_rect(content, line_index), "Hot GPU processes", text_style, horizontal_state)
    line_index = line_index + 1
    viewport_rows = max(0, content%height - line_index + 1)
    active_state = gpu_process_state()
    if (stateful) then
      active_state = state
      active_state%row_count = size(visible_processes)
      active_state%viewport_rows = viewport_rows
      call normalize_gpu_process_state(active_state)
      state = active_state
      start_index = active_state%scroll_row
    else
      start_index = 1
    end if

    do preview_row = 1, viewport_rows
      actual_index = start_index + preview_row - 1
      if (actual_index > size(visible_processes)) exit
      if (line_index > content%height) exit
      row_style = dim_style
      row_rect = content_line_rect(content, line_index)
      process_line = gpu_process_text(visible_processes(actual_index))
      if (stateful .and. active .and. actual_index == active_state%selected_row) row_style = selected_style
      call render_horizontal_text(buffer, row_rect, process_line, row_style, horizontal_state, &
                                  active=stateful .and. active .and. actual_index == active_state%selected_row, &
                                  ticker=.false.)
      line_index = line_index + 1
    end do
  end subroutine render_gpu_processes

  subroutine render_gpu_utilization(buffer, rect, gpu, dim_style, horizontal_state)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    type(gpu_info), intent(in) :: gpu
    type(screen_style), intent(in) :: dim_style
    type(horizontal_text_state), intent(inout), optional :: horizontal_state
    real :: usage_fraction

    if (rect%width <= 0 .or. rect%height <= 0) return
    if (.not. gpu%utilization_valid) then
      call render_horizontal_text(buffer, rect, "GPU util n/a", dim_style, horizontal_state)
      return
    end if

    usage_fraction = real(max(0.0_real64, min(100.0_real64, gpu%utilization_percent)) / 100.0_real64)
    call render_meter(buffer, rect, usage_fraction, gradient=gradient_green_yellow_red(), &
                      label="GPU " // format_percent(real(gpu%utilization_percent)), &
                      fill_mode=METER_FILL_SHADED, empty_style=dim_style, label_style=dim_style)
  end subroutine render_gpu_utilization

  subroutine render_gpu_history(buffer, rect, snapshot, gpu_index, dim_style, horizontal_state)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    type(collector_snapshot), intent(in) :: snapshot
    integer, intent(in) :: gpu_index
    type(screen_style), intent(in) :: dim_style
    type(horizontal_text_state), intent(inout), optional :: horizontal_state
    type(color_gradient) :: gpu_gradient
    type(widget_rect) :: label_rect
    type(widget_rect) :: spark_rect
    type(widget_rect) :: temp_label_rect
    type(widget_rect) :: temp_rect
    integer :: half_width
    integer :: label_width
    logical :: has_temp_history
    logical :: has_util_history

    if (rect%width <= 0 .or. rect%height <= 0) return
    has_util_history = gpu_utilization_history_available(snapshot, gpu_index)
    has_temp_history = gpu_temperature_history_available(snapshot, gpu_index)
    if (.not. has_util_history .and. .not. has_temp_history) return

    gpu_gradient = gradient_green_yellow_red()
    if (has_util_history .and. has_temp_history .and. rect%width >= 18) then
      half_width = rect%width / 2
      label_width = min(5, half_width)
      label_rect = widget_rect(rect%row, rect%col, label_width, 1)
      spark_rect = widget_rect(rect%row, rect%col + label_width, half_width - label_width, 1)
      temp_label_rect = widget_rect(rect%row, rect%col + half_width, label_width, 1)
      temp_rect = widget_rect(rect%row, rect%col + half_width + label_width, &
                              rect%width - half_width - label_width, 1)
      call render_horizontal_text(buffer, label_rect, "util", dim_style, horizontal_state)
      call render_sparkline(buffer, spark_rect, real(snapshot%gpu_utilization_history(gpu_index, :)), &
                            gradient=gpu_gradient, min_value=0.0, max_value=100.0, style=dim_style)
        call render_horizontal_text(buffer, temp_label_rect, "temp", dim_style, horizontal_state)
      call render_sparkline(buffer, temp_rect, real(snapshot%gpu_temperature_history(gpu_index, :)), &
                            gradient=gpu_gradient, min_value=0.0, &
                            max_value=gpu_temperature_history_max(snapshot%gpu%gpus(gpu_index)), style=dim_style)
      return
    end if

    label_width = min(5, rect%width)
    label_rect = widget_rect(rect%row, rect%col, label_width, 1)
    spark_rect = widget_rect(rect%row, rect%col + label_width, rect%width - label_width, 1)
    if (has_util_history) then
      call render_horizontal_text(buffer, label_rect, "util", dim_style, horizontal_state)
      call render_sparkline(buffer, spark_rect, real(snapshot%gpu_utilization_history(gpu_index, :)), &
                            gradient=gpu_gradient, min_value=0.0, max_value=100.0, style=dim_style)
    else
      call render_horizontal_text(buffer, label_rect, "temp", dim_style, horizontal_state)
      call render_sparkline(buffer, spark_rect, real(snapshot%gpu_temperature_history(gpu_index, :)), &
                            gradient=gpu_gradient, min_value=0.0, &
                            max_value=gpu_temperature_history_max(snapshot%gpu%gpus(gpu_index)), style=dim_style)
    end if
  end subroutine render_gpu_history

  function gpu_summary_text(table) result(text)
    type(gpu_table), intent(in) :: table
    character(len=:), allocatable :: text

    text = "GPUs " // integer_text(valid_gpu_count(table))
  end function gpu_summary_text

  function gpu_title_text(gpu) result(text)
    type(gpu_info), intent(in) :: gpu
    character(len=:), allocatable :: text

    if (len_trim(gpu%vendor) > 0 .and. len_trim(gpu%name) > 0) then
      text = trim(gpu%vendor) // " " // trim(gpu%name)
    else if (len_trim(gpu%name) > 0) then
      text = trim(gpu%name)
    else if (len_trim(gpu%vendor) > 0) then
      text = trim(gpu%vendor) // " GPU"
    else
      text = "GPU"
    end if
  end function gpu_title_text

  function gpu_metric_text(gpu) result(text)
    type(gpu_info), intent(in) :: gpu
    character(len=:), allocatable :: text

    text = vram_text(gpu)
    if (gpu%temperature_valid) text = text // "  " // temperature_text(gpu)
    if (gpu%power_valid) text = text // "  " // power_text(gpu)
  end function gpu_metric_text

  function vram_text(gpu) result(text)
    type(gpu_info), intent(in) :: gpu
    character(len=:), allocatable :: text

    if (gpu%memory_valid) then
      text = "VRAM " // format_bytes(max(0_int64, gpu%memory_used_bytes)) // " / " // &
             format_bytes(max(0_int64, gpu%memory_total_bytes))
    else if (gpu%memory_utilization_valid) then
      text = "VRAM " // format_percent(real(gpu%memory_utilization_percent))
    else
      text = "VRAM n/a"
    end if
  end function vram_text

  function temperature_text(gpu) result(text)
    type(gpu_info), intent(in) :: gpu
    character(len=:), allocatable :: text
    character(len=32) :: scratch

    write(scratch, '(f5.1,a)') gpu%temp_celsius, " C"
    text = "temp " // trim(adjustl(scratch))
  end function temperature_text

  function power_text(gpu) result(text)
    type(gpu_info), intent(in) :: gpu
    character(len=:), allocatable :: text
    character(len=32) :: scratch

    write(scratch, '(f5.1,a)') gpu%power_watts, " W"
    text = "power " // trim(adjustl(scratch))
    if (gpu%power_limit_watts > 0.0_real64) then
      write(scratch, '(f5.1,a)') gpu%power_limit_watts, " W"
      text = text // " / " // trim(adjustl(scratch))
    end if
  end function power_text

  integer function valid_gpu_count(table) result(count)
    type(gpu_table), intent(in) :: table
    integer :: gpu_index

    count = 0
    if (.not. allocated(table%gpus)) return
    do gpu_index = 1, size(table%gpus)
      if (table%gpus(gpu_index)%valid) count = count + 1
    end do
  end function valid_gpu_count

  integer function valid_gpu_process_count(table) result(count)
    type(gpu_process_table), intent(in) :: table
    integer :: process_index

    count = 0
    if (.not. allocated(table%processes)) return
    do process_index = 1, size(table%processes)
      if (table%processes(process_index)%valid) count = count + 1
    end do
  end function valid_gpu_process_count

  function valid_gpu_processes(table) result(processes)
    type(gpu_process_table), intent(in) :: table
    type(gpu_process_info), allocatable :: processes(:)
    integer :: process_index
    integer :: row

    allocate(processes(valid_gpu_process_count(table)))
    row = 0
    if (.not. allocated(table%processes)) return
    do process_index = 1, size(table%processes)
      if (.not. table%processes(process_index)%valid) cycle
      row = row + 1
      processes(row) = table%processes(process_index)
    end do
  end function valid_gpu_processes

  function gpu_process_text(process) result(text)
    type(gpu_process_info), intent(in) :: process
    character(len=:), allocatable :: text

    text = integer_text(process%pid) // " " // gpu_process_name_text(process) // &
           " " // gpu_process_engine_text(process) // " " // gpu_process_busy_text(process)
    if (process%memory_valid) text = text // " " // format_bytes(process%memory_bytes)
  end function gpu_process_text

  function gpu_process_name_text(process) result(text)
    type(gpu_process_info), intent(in) :: process
    character(len=:), allocatable :: text

    if (len_trim(process%process_name) > 0) then
      text = trim(process%process_name)
    else
      text = "[unknown]"
    end if
  end function gpu_process_name_text

  function gpu_process_engine_text(process) result(text)
    type(gpu_process_info), intent(in) :: process
    character(len=:), allocatable :: text

    if (len_trim(process%engine) > 0) then
      text = trim(process%engine)
    else
      text = "gpu"
    end if
  end function gpu_process_engine_text

  function gpu_process_busy_text(process) result(text)
    type(gpu_process_info), intent(in) :: process
    character(len=:), allocatable :: text

    if (process%busy_percent_valid) then
      text = format_percent(real(process%busy_percent))
    else
      text = "warming"
    end if
  end function gpu_process_busy_text

  subroutine gpu_process_select_delta(state, delta)
    type(gpu_process_state), intent(inout) :: state
    integer, intent(in) :: delta

    state%selected_row = state%selected_row + delta
    call normalize_gpu_process_state(state)
  end subroutine gpu_process_select_delta

  subroutine gpu_process_page_delta(state, delta_pages)
    type(gpu_process_state), intent(inout) :: state
    integer, intent(in) :: delta_pages
    integer :: step

    step = max(1, state%viewport_rows)
    call gpu_process_select_delta(state, delta_pages * step)
  end subroutine gpu_process_page_delta

  function gpu_process_status(state) result(text)
    type(gpu_process_state), intent(in) :: state
    character(len=:), allocatable :: text

    text = "gpu process row " // integer_text(max(0, state%selected_row)) // "/" // &
           integer_text(max(0, state%row_count))
  end function gpu_process_status

  subroutine normalize_gpu_process_state(state)
    type(gpu_process_state), intent(inout) :: state
    integer :: max_scroll

    state%row_count = max(0, state%row_count)
    state%viewport_rows = max(0, state%viewport_rows)
    if (state%row_count <= 0) then
      state%selected_row = 0
      state%scroll_row = 1
      return
    end if

    if (state%selected_row <= 0) state%selected_row = 1
    state%selected_row = max(1, min(state%row_count, state%selected_row))
    max_scroll = max(1, state%row_count - max(1, state%viewport_rows) + 1)
    state%scroll_row = max(1, min(max_scroll, state%scroll_row))
    if (state%viewport_rows > 0) then
      if (state%selected_row < state%scroll_row) state%scroll_row = state%selected_row
      if (state%selected_row >= state%scroll_row + state%viewport_rows) then
        state%scroll_row = state%selected_row - state%viewport_rows + 1
      end if
      state%scroll_row = max(1, min(max_scroll, state%scroll_row))
    end if
  end subroutine normalize_gpu_process_state

  subroutine clear_gpu_process_state(state)
    type(gpu_process_state), intent(inout) :: state

    state%row_count = 0
    state%viewport_rows = 0
    call normalize_gpu_process_state(state)
  end subroutine clear_gpu_process_state

  logical function gpu_history_available(snapshot, gpu_index) result(available)
    type(collector_snapshot), intent(in) :: snapshot
    integer, intent(in) :: gpu_index

    available = gpu_utilization_history_available(snapshot, gpu_index) .or. &
                gpu_temperature_history_available(snapshot, gpu_index)
  end function gpu_history_available

  logical function gpu_utilization_history_available(snapshot, gpu_index) result(available)
    type(collector_snapshot), intent(in) :: snapshot
    integer, intent(in) :: gpu_index

    available = .false.
    if (.not. allocated(snapshot%gpu%gpus)) return
    if (gpu_index < 1 .or. gpu_index > size(snapshot%gpu%gpus)) return
    if (.not. snapshot%gpu%gpus(gpu_index)%utilization_valid) return
    if (.not. allocated(snapshot%gpu_utilization_history)) return
    if (gpu_index > size(snapshot%gpu_utilization_history, 1)) return
    if (size(snapshot%gpu_utilization_history, 2) <= 0) return

    available = .true.
  end function gpu_utilization_history_available

  logical function gpu_temperature_history_available(snapshot, gpu_index) result(available)
    type(collector_snapshot), intent(in) :: snapshot
    integer, intent(in) :: gpu_index

    available = .false.
    if (.not. allocated(snapshot%gpu%gpus)) return
    if (gpu_index < 1 .or. gpu_index > size(snapshot%gpu%gpus)) return
    if (.not. snapshot%gpu%gpus(gpu_index)%temperature_valid) return
    if (.not. allocated(snapshot%gpu_temperature_history)) return
    if (gpu_index > size(snapshot%gpu_temperature_history, 1)) return
    if (size(snapshot%gpu_temperature_history, 2) <= 0) return

    available = .true.
  end function gpu_temperature_history_available

  real function gpu_temperature_history_max(gpu) result(max_value)
    type(gpu_info), intent(in) :: gpu

    max_value = 100.0
    if (gpu%temp_max_celsius > 0.0_real64) max_value = real(gpu%temp_max_celsius)
    max_value = max(1.0, max_value)
  end function gpu_temperature_history_max

  function content_line_rect(content, line_index) result(rect)
    type(widget_rect), intent(in) :: content
    integer, intent(in) :: line_index
    type(widget_rect) :: rect

    rect = widget_rect(content%row + line_index - 1, content%col, content%width, 1)
  end function content_line_rect

  function integer_text(value) result(text)
    integer, intent(in) :: value
    character(len=:), allocatable :: text
    character(len=32) :: scratch

    write(scratch, '(i0)') value
    text = trim(scratch)
  end function integer_text
end module ftop_gpu
