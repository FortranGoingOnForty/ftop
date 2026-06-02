module ftop_network
  use, intrinsic :: iso_fortran_env, only : real64
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_box, only : BOX_STYLE_ROUNDED, box_content_rect, draw_box
  use ftop_collector, only : collector_snapshot
  use ftop_color, only : &
    COLOR_BRIGHT_WHITE, &
    color_gradient, &
    gradient_blue_cyan, &
    style_from_rgb
  use ftop_net_data, only : format_byte_rate
  use ftop_sparkline, only : render_sparkline
  use ftop_text, only : TEXT_ALIGN_CENTER, render_text
  use ftop_widgets, only : widget_rect, widget_size
  implicit none
  private

  public :: network_panel_min_size
  public :: render_network_panel

contains

  function network_panel_min_size() result(size_value)
    type(widget_size) :: size_value

    size_value%width = 28
    size_value%height = 8
  end function network_panel_min_size

  subroutine render_network_panel(buffer, panel, snapshot, border_style, title_style, dim_style)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: panel
    type(collector_snapshot), intent(in) :: snapshot
    type(screen_style), intent(in) :: border_style
    type(screen_style), intent(in) :: title_style
    type(screen_style), intent(in) :: dim_style
    type(color_gradient) :: network_gradient
    type(screen_style) :: text_style
    type(widget_rect) :: content
    integer :: interface_index
    integer :: line_index

    network_gradient = gradient_blue_cyan()
    text_style = style_from_rgb(fg=COLOR_BRIGHT_WHITE)

    call draw_box(buffer, panel, BOX_STYLE_ROUNDED, border_style, "Network", title_style)
    content = box_content_rect(panel)
    if (content%height <= 0 .or. content%width <= 0) return

    if (.not. snapshot%network%valid .or. .not. allocated(snapshot%network%interfaces)) then
      call render_text(buffer, content_line_rect(content, 1), "network unavailable", dim_style, TEXT_ALIGN_CENTER)
      return
    end if
    if (size(snapshot%network%interfaces) <= 0) then
      call render_text(buffer, content_line_rect(content, 1), "no network interfaces", dim_style, TEXT_ALIGN_CENTER)
      return
    end if

    call render_text(buffer, content_line_rect(content, 1), network_summary_text(snapshot), text_style)
    line_index = 2
    do interface_index = 1, size(snapshot%network%interfaces)
      if (line_index > content%height) exit
      if (.not. snapshot%network%interfaces(interface_index)%valid) cycle
      call render_text(buffer, content_line_rect(content, line_index), &
                       network_interface_text(snapshot, interface_index), dim_style)
      line_index = line_index + 1
      if (line_index > content%height) cycle
      call render_interface_sparkline(buffer, content_line_rect(content, line_index), snapshot, interface_index, &
                                      network_gradient, dim_style)
      line_index = line_index + 1
    end do
  end subroutine render_network_panel

  subroutine render_interface_sparkline(buffer, rect, snapshot, interface_index, network_gradient, dim_style)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    type(collector_snapshot), intent(in) :: snapshot
    integer, intent(in) :: interface_index
    type(color_gradient), intent(in) :: network_gradient
    type(screen_style), intent(in) :: dim_style
    real(real64), allocatable :: total_history(:)
    integer :: history_count

    if (rect%width <= 0 .or. rect%height <= 0) return
    history_count = max(0, snapshot%network%interfaces(interface_index)%history_count)
    if (history_count <= 0) then
      call render_text(buffer, rect, "no traffic history", dim_style)
      return
    end if

    allocate(total_history(history_count))
    total_history = snapshot%network%interfaces(interface_index)%rx_history(:history_count) + &
                    snapshot%network%interfaces(interface_index)%tx_history(:history_count)
    call render_sparkline(buffer, rect, real(total_history), gradient=network_gradient, min_value=0.0, style=dim_style)
  end subroutine render_interface_sparkline

  function network_summary_text(snapshot) result(text)
    type(collector_snapshot), intent(in) :: snapshot
    character(len=:), allocatable :: text

    text = "Interfaces " // integer_text(valid_interface_count(snapshot)) // &
           "  rx " // format_byte_rate(total_rx_rate(snapshot)) // &
           "  tx " // format_byte_rate(total_tx_rate(snapshot))
  end function network_summary_text

  function network_interface_text(snapshot, interface_index) result(text)
    type(collector_snapshot), intent(in) :: snapshot
    integer, intent(in) :: interface_index
    character(len=:), allocatable :: text
    character(len=:), allocatable :: state

    state = trim(snapshot%network%interfaces(interface_index)%state)
    if (len(state) <= 0) state = "unknown"
    text = trim(snapshot%network%interfaces(interface_index)%name) // " " // state // &
           "  rx " // format_byte_rate(snapshot%network%interfaces(interface_index)%rx_bytes_per_sec) // &
           "  tx " // format_byte_rate(snapshot%network%interfaces(interface_index)%tx_bytes_per_sec)
  end function network_interface_text

  integer function valid_interface_count(snapshot) result(count)
    type(collector_snapshot), intent(in) :: snapshot
    integer :: interface_index

    count = 0
    if (.not. allocated(snapshot%network%interfaces)) return
    do interface_index = 1, size(snapshot%network%interfaces)
      if (snapshot%network%interfaces(interface_index)%valid) count = count + 1
    end do
  end function valid_interface_count

  real(real64) function total_rx_rate(snapshot) result(rate)
    type(collector_snapshot), intent(in) :: snapshot
    integer :: interface_index

    rate = 0.0_real64
    if (.not. allocated(snapshot%network%interfaces)) return
    do interface_index = 1, size(snapshot%network%interfaces)
      if (snapshot%network%interfaces(interface_index)%valid) then
        rate = rate + max(0.0_real64, snapshot%network%interfaces(interface_index)%rx_bytes_per_sec)
      end if
    end do
  end function total_rx_rate

  real(real64) function total_tx_rate(snapshot) result(rate)
    type(collector_snapshot), intent(in) :: snapshot
    integer :: interface_index

    rate = 0.0_real64
    if (.not. allocated(snapshot%network%interfaces)) return
    do interface_index = 1, size(snapshot%network%interfaces)
      if (snapshot%network%interfaces(interface_index)%valid) then
        rate = rate + max(0.0_real64, snapshot%network%interfaces(interface_index)%tx_bytes_per_sec)
      end if
    end do
  end function total_tx_rate

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

  function integer_text(value) result(text)
    integer, intent(in) :: value
    character(len=:), allocatable :: text
    character(len=32) :: scratch

    write(scratch, '(i0)') value
    text = trim(scratch)
  end function integer_text

end module ftop_network
