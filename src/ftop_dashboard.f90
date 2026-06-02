module ftop_dashboard
  use fgof_screen, only : clear_screen
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_box, only : BOX_STYLE_DOUBLE, draw_box
  use ftop_collector, only : collector_snapshot
  use ftop_color, only : &
    COLOR_UI_ACCENT, &
    COLOR_UI_BORDER, &
    COLOR_UI_DIM, &
    COLOR_UI_PANEL, &
    style_from_rgb
  use ftop_cpu, only : render_cpu_panel
  use ftop_layout, only : dashboard_layout, dashboard_layout_from_grid, default_dashboard_layout, layout_grid
  use ftop_memory, only : render_memory_panel
  use ftop_network, only : network_table_state, render_network_panel
  use ftop_process_table, only : process_table_state, render_process_panel
  use ftop_text, only : TEXT_ALIGN_CENTER, render_text
  use ftop_widgets, only : widget_rect
  implicit none
  private

  public :: dashboard_layout
  public :: default_dashboard_layout
  public :: render_dashboard

contains

  subroutine render_dashboard(buffer, snapshot, refresh_ms, frame_count, status_text, grid, focused_widget, zoomed, render_fps, &
                              process_state, network_state, layout_name)
    type(screen_buffer), intent(inout) :: buffer
    type(collector_snapshot), intent(in) :: snapshot
    integer, intent(in) :: refresh_ms
    integer, intent(in) :: frame_count
    character(len=*), intent(in) :: status_text
    type(layout_grid), intent(in), optional :: grid
    character(len=*), intent(in), optional :: focused_widget
    logical, intent(in), optional :: zoomed
    real, intent(in), optional :: render_fps
    type(process_table_state), intent(inout), optional :: process_state
    type(network_table_state), intent(inout), optional :: network_state
    character(len=*), intent(in), optional :: layout_name
    type(dashboard_layout) :: layout
    type(screen_style) :: border_style
    type(screen_style) :: cpu_border_style
    type(screen_style) :: title_style
    type(screen_style) :: dim_style
    type(screen_style) :: focus_style
    type(screen_style) :: memory_border_style
    type(screen_style) :: network_border_style
    type(screen_style) :: process_border_style
    type(widget_rect) :: title_rect
    type(widget_rect) :: zoom_rect
    character(len=:), allocatable :: focus
    integer :: height
    integer :: title_col
    integer :: width
    logical :: is_zoomed

    width = buffer%size%width
    height = buffer%size%height
    if (width <= 0 .or. height <= 0) return

    border_style = style_from_rgb(fg=COLOR_UI_BORDER)
    focus_style = style_from_rgb(fg=COLOR_UI_ACCENT, bold=.true.)
    title_style = style_from_rgb(fg=COLOR_UI_ACCENT, bg=COLOR_UI_PANEL, bold=.true.)
    dim_style = style_from_rgb(fg=COLOR_UI_DIM)
    focus = ""
    if (present(focused_widget)) focus = trim(focused_widget)
    is_zoomed = .false.
    if (present(zoomed)) is_zoomed = zoomed .and. len(focus) > 0

    call clear_screen(buffer)
    buffer%cursor_visible = .false.

    if (width < 8 .or. height < 4) then
      call render_text(buffer, widget_rect(1, 1, width, 1), "ftop", title_style, TEXT_ALIGN_CENTER)
      return
    end if

    if (present(grid)) then
      layout = dashboard_layout_from_grid(width, height, grid)
    else
      layout = default_dashboard_layout(width, height)
    end if
    call draw_box(buffer, layout%frame, BOX_STYLE_DOUBLE, border_style, "ftop", title_style, TEXT_ALIGN_CENTER)

    if (is_zoomed) then
      zoom_rect = widget_rect(3, 3, max(0, width - 4), max(0, height - 5))
      if (present(process_state) .and. present(network_state)) then
        call render_dashboard_panel(buffer, zoom_rect, focus, snapshot, focus_style, title_style, dim_style, .true., &
                                    process_state, network_state)
      else if (present(process_state)) then
        call render_dashboard_panel(buffer, zoom_rect, focus, snapshot, focus_style, title_style, dim_style, .true., &
                                    process_state=process_state)
      else if (present(network_state)) then
        call render_dashboard_panel(buffer, zoom_rect, focus, snapshot, focus_style, title_style, dim_style, .true., &
                                    network_state=network_state)
      else
        call render_dashboard_panel(buffer, zoom_rect, focus, snapshot, focus_style, title_style, dim_style, .true.)
      end if
    else
      cpu_border_style = border_style
      memory_border_style = border_style
      network_border_style = border_style
      process_border_style = border_style
      if (focus == "cpu") cpu_border_style = focus_style
      if (focus == "memory") memory_border_style = focus_style
      if (focus == "network") network_border_style = focus_style
      if (focus == "process") process_border_style = focus_style
      if (layout%cpu_panel%height >= 3) then
        call render_cpu_panel(buffer, layout%cpu_panel, snapshot, cpu_border_style, title_style, dim_style)
      end if
      if (layout%memory_panel%height >= 3) then
        call render_memory_panel(buffer, layout%memory_panel, snapshot, memory_border_style, title_style, dim_style)
      end if
      if (layout%network_panel%height >= 3) then
        call render_network_panel(buffer, layout%network_panel, snapshot, network_border_style, title_style, dim_style, &
                                  expanded=.false.)
      end if
      if (layout%process_panel%height >= 3) then
        if (present(process_state)) then
          call render_process_panel(buffer, layout%process_panel, snapshot, process_border_style, title_style, dim_style, &
                                    process_state)
        else
          call render_process_panel(buffer, layout%process_panel, snapshot, process_border_style, title_style, dim_style)
        end if
      end if
    end if

    if (.not. is_zoomed .and. layout%cpu_panel%height < 3 .and. layout%memory_panel%height < 3 .and. &
        layout%network_panel%height < 3 .and. layout%process_panel%height < 3) then
      title_col = max(2, (width - len_trim("CPU / Memory / Network / Processes")) / 2 + 1)
      title_rect = widget_rect(max(2, height / 2), title_col, width - title_col, 1)
      call render_text(buffer, title_rect, "CPU / Memory / Network / Processes", title_style)
    end if

    call render_footer(buffer, layout%footer, snapshot, refresh_ms, frame_count, status_text, &
                       dim_style, focus, is_zoomed, render_fps=render_fps, layout_name=layout_name)
  end subroutine render_dashboard

  subroutine render_dashboard_panel(buffer, rect, widget, snapshot, border_style, title_style, dim_style, expanded, &
                                    process_state, network_state)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    character(len=*), intent(in) :: widget
    type(collector_snapshot), intent(in) :: snapshot
    type(screen_style), intent(in) :: border_style
    type(screen_style), intent(in) :: title_style
    type(screen_style), intent(in) :: dim_style
    logical, intent(in) :: expanded
    type(process_table_state), intent(inout), optional :: process_state
    type(network_table_state), intent(inout), optional :: network_state

    if (rect%height < 3 .or. rect%width <= 0) return
    select case (trim(widget))
    case ("cpu")
      call render_cpu_panel(buffer, rect, snapshot, border_style, title_style, dim_style)
    case ("memory")
      call render_memory_panel(buffer, rect, snapshot, border_style, title_style, dim_style)
    case ("network")
      if (present(network_state)) then
        call render_network_panel(buffer, rect, snapshot, border_style, title_style, dim_style, network_state, expanded)
      else
        call render_network_panel(buffer, rect, snapshot, border_style, title_style, dim_style, expanded=expanded)
      end if
    case ("process")
      if (present(process_state)) then
        call render_process_panel(buffer, rect, snapshot, border_style, title_style, dim_style, process_state)
      else
        call render_process_panel(buffer, rect, snapshot, border_style, title_style, dim_style)
      end if
    end select
  end subroutine render_dashboard_panel

  subroutine render_footer(buffer, footer, snapshot, refresh_ms, frame_count, status_text, dim_style, focused_widget, &
                           zoomed, render_fps, layout_name)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: footer
    type(collector_snapshot), intent(in) :: snapshot
    integer, intent(in) :: refresh_ms
    integer, intent(in) :: frame_count
    character(len=*), intent(in) :: status_text
    type(screen_style), intent(in) :: dim_style
    character(len=*), intent(in) :: focused_widget
    logical, intent(in) :: zoomed
    real, intent(in), optional :: render_fps
    character(len=*), intent(in), optional :: layout_name
    type(widget_rect) :: line
    character(len=:), allocatable :: status

    if (footer%width <= 0 .or. footer%height <= 0) return

    line = widget_rect(footer%row, footer%col, footer%width, 1)
    call render_text(buffer, line, footer_text(snapshot, refresh_ms, frame_count, focused_widget, zoomed, &
                                             render_fps=render_fps, layout_name=layout_name), dim_style)

    if (footer%height < 2) return
    status = trim(status_text)
    if (len(status) == 0) status = "ready"
    line = widget_rect(footer%row + 1, footer%col, footer%width, 1)
    call render_text(buffer, line, status, dim_style)
  end subroutine render_footer

  function footer_text(snapshot, refresh_ms, frame_count, focused_widget, zoomed, render_fps, layout_name) result(text)
    type(collector_snapshot), intent(in) :: snapshot
    integer, intent(in) :: refresh_ms
    integer, intent(in) :: frame_count
    character(len=*), intent(in) :: focused_widget
    logical, intent(in) :: zoomed
    real, intent(in), optional :: render_fps
    character(len=*), intent(in), optional :: layout_name
    character(len=:), allocatable :: text
    character(len=:), allocatable :: focus_text
    character(len=:), allocatable :: fps
    character(len=:), allocatable :: layout_text
    character(len=:), allocatable :: running_text

    if (snapshot%running) then
      running_text = "collector running"
    else
      running_text = "collector idle"
    end if
    if (len_trim(focused_widget) > 0) then
      if (zoomed) then
        focus_text = " zoom " // trim(focused_widget)
      else
        focus_text = " focus " // trim(focused_widget)
      end if
    else
      focus_text = ""
    end if
    if (present(layout_name)) then
      if (len_trim(layout_name) > 0) then
        layout_text = " layout " // trim(layout_name)
      else
        layout_text = ""
      end if
    else
      layout_text = ""
    end if
    fps = real_text(footer_fps(refresh_ms, render_fps))
    text = "refresh " // integer_text(refresh_ms) // "ms frame " // integer_text(frame_count) // &
            " fps " // fps // " samples " // integer_text(snapshot%sample_count) // "  " // running_text // &
           focus_text // layout_text // "  Tab focus P layout 1-4 presets z zoom q quit Ctrl+Z suspend"
  end function footer_text

  real function footer_fps(refresh_ms, render_fps) result(fps)
    integer, intent(in) :: refresh_ms
    real, intent(in), optional :: render_fps

    if (present(render_fps)) then
      fps = max(0.0, render_fps)
    else if (refresh_ms > 0) then
      fps = 1000.0 / real(refresh_ms)
    else
      fps = 0.0
    end if
  end function footer_fps

  function real_text(value) result(text)
    real, intent(in) :: value
    character(len=:), allocatable :: text
    character(len=32) :: scratch

    write(scratch, '(f8.1)') value
    text = trim(adjustl(scratch))
  end function real_text

  function integer_text(value) result(text)
    integer, intent(in) :: value
    character(len=:), allocatable :: text
    character(len=32) :: scratch

    write(scratch, '(i0)') value
    text = trim(scratch)
  end function integer_text

end module ftop_dashboard
