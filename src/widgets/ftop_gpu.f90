module ftop_gpu
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_box, only : BOX_STYLE_ROUNDED, box_content_rect, draw_box
  use ftop_collector, only : collector_snapshot
  use ftop_color, only : COLOR_BRIGHT_WHITE, gradient_green_yellow_red, style_from_rgb
  use ftop_gpu_data, only : gpu_info, gpu_table
  use ftop_meter, only : METER_FILL_SHADED, render_meter
  use ftop_text, only : TEXT_ALIGN_CENTER, format_bytes, format_percent, render_text
  use ftop_widgets, only : widget_rect, widget_size
  implicit none
  private

  public :: gpu_panel_min_size
  public :: render_gpu_panel

contains

  function gpu_panel_min_size() result(size_value)
    type(widget_size) :: size_value

    size_value%width = 24
    size_value%height = 5
  end function gpu_panel_min_size

  subroutine render_gpu_panel(buffer, panel, snapshot, border_style, title_style, dim_style, expanded)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: panel
    type(collector_snapshot), intent(in) :: snapshot
    type(screen_style), intent(in) :: border_style
    type(screen_style), intent(in) :: title_style
    type(screen_style), intent(in) :: dim_style
    logical, intent(in), optional :: expanded
    type(screen_style) :: text_style
    type(widget_rect) :: content
    integer :: gpu_index
    integer :: line_index
    logical :: actual_expanded

    actual_expanded = .false.
    if (present(expanded)) actual_expanded = expanded
    associate(unused_expanded => actual_expanded)
    end associate

    text_style = style_from_rgb(fg=COLOR_BRIGHT_WHITE)
    call draw_box(buffer, panel, BOX_STYLE_ROUNDED, border_style, "GPU", title_style)
    content = box_content_rect(panel)
    if (content%height <= 0 .or. content%width <= 0) return

    if (.not. snapshot%gpu%valid) then
      call render_text(buffer, content_line_rect(content, 1), "gpu unavailable", dim_style, TEXT_ALIGN_CENTER)
      return
    end if

    if (.not. allocated(snapshot%gpu%gpus) .or. valid_gpu_count(snapshot%gpu) <= 0) then
      call render_text(buffer, content_line_rect(content, 1), "No GPUs detected", dim_style, TEXT_ALIGN_CENTER)
      return
    end if

    call render_text(buffer, content_line_rect(content, 1), gpu_summary_text(snapshot%gpu), text_style)
    line_index = 2
    do gpu_index = 1, size(snapshot%gpu%gpus)
      if (.not. snapshot%gpu%gpus(gpu_index)%valid) cycle
      if (line_index > content%height) exit
      call render_text(buffer, content_line_rect(content, line_index), gpu_title_text(snapshot%gpu%gpus(gpu_index)), text_style)
      line_index = line_index + 1
      if (line_index > content%height) cycle
      call render_gpu_utilization(buffer, content_line_rect(content, line_index), snapshot%gpu%gpus(gpu_index), dim_style)
      line_index = line_index + 1
      if (line_index > content%height) cycle
      call render_text(buffer, content_line_rect(content, line_index), gpu_metric_text(snapshot%gpu%gpus(gpu_index)), dim_style)
      line_index = line_index + 1
    end do
  end subroutine render_gpu_panel

  subroutine render_gpu_utilization(buffer, rect, gpu, dim_style)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    type(gpu_info), intent(in) :: gpu
    type(screen_style), intent(in) :: dim_style
    real :: usage_fraction

    if (rect%width <= 0 .or. rect%height <= 0) return
    if (.not. gpu%utilization_valid) then
      call render_text(buffer, rect, "GPU util n/a", dim_style)
      return
    end if

    usage_fraction = real(max(0.0_real64, min(100.0_real64, gpu%utilization_percent)) / 100.0_real64)
    call render_meter(buffer, rect, usage_fraction, gradient=gradient_green_yellow_red(), &
                      label="GPU " // format_percent(real(gpu%utilization_percent)), &
                      fill_mode=METER_FILL_SHADED, empty_style=dim_style, label_style=dim_style)
  end subroutine render_gpu_utilization

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
