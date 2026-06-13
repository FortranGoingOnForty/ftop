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
  use ftop_disk, only : disk_table_state, render_disk_panel
  use ftop_gpu, only : gpu_process_state, render_gpu_panel
  use ftop_layout, only : dashboard_layout, dashboard_layout_from_grid, default_dashboard_layout, layout_grid
  use ftop_memory, only : render_memory_panel
  use ftop_network, only : &
    network_table_sort_direction_label, &
    network_table_sort_key_label, &
    network_table_state, &
    render_network_panel
  use ftop_process_table, only : &
    process_table_sort_direction_label, &
    process_table_sort_key_label, &
    process_table_state, &
    render_process_panel
  use ftop_text, only : &
    TEXT_ALIGN_CENTER, &
    horizontal_text_begin_frame, &
    horizontal_text_end_frame, &
    horizontal_text_state, &
    render_text
  use ftop_widgets, only : widget_rect
  implicit none
  private

  public :: dashboard_layout
  public :: default_dashboard_layout
  public :: render_dashboard

contains

  subroutine render_dashboard(buffer, snapshot, refresh_ms, frame_count, status_text, grid, focused_widget, zoomed, render_fps, &
                               process_state, network_state, disk_state, layout_name, paused, cpu_core_scroll_offset, &
                               cpu_core_active, process_tree_active, network_table_active, disk_table_active, gpu_state, &
                               gpu_process_active, horizontal_state)
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
    type(disk_table_state), intent(inout), optional :: disk_state
    character(len=*), intent(in), optional :: layout_name
    logical, intent(in), optional :: paused
    integer, intent(in), optional :: cpu_core_scroll_offset
    logical, intent(in), optional :: cpu_core_active
    logical, intent(in), optional :: process_tree_active
    logical, intent(in), optional :: network_table_active
    logical, intent(in), optional :: disk_table_active
    type(gpu_process_state), intent(inout), optional :: gpu_state
    logical, intent(in), optional :: gpu_process_active
    type(horizontal_text_state), intent(inout), optional :: horizontal_state
    type(dashboard_layout) :: layout
    type(screen_style) :: border_style
    type(screen_style) :: cpu_border_style
    type(screen_style) :: disk_border_style
    type(screen_style) :: title_style
    type(screen_style) :: dim_style
    type(screen_style) :: focus_style
    type(screen_style) :: gpu_border_style
    type(screen_style) :: memory_border_style
    type(screen_style) :: network_border_style
    type(screen_style) :: process_border_style
    type(widget_rect) :: title_rect
    type(widget_rect) :: zoom_rect
    character(len=:), allocatable :: focus
    integer :: cpu_offset
    integer :: height
    integer :: title_col
    integer :: width
    logical :: is_zoomed
    logical :: is_paused
    logical :: cpu_active
    logical :: disk_active
    logical :: gpu_active
    logical :: network_active
    logical :: process_active

    if (present(horizontal_state)) call horizontal_text_begin_frame(horizontal_state, frame_count)
    width = buffer%size%width
    height = buffer%size%height
    if (width <= 0 .or. height <= 0) then
      if (present(horizontal_state)) call horizontal_text_end_frame(horizontal_state)
      return
    end if

    border_style = style_from_rgb(fg=COLOR_UI_BORDER)
    focus_style = style_from_rgb(fg=COLOR_UI_ACCENT, bold=.true.)
    title_style = style_from_rgb(fg=COLOR_UI_ACCENT, bg=COLOR_UI_PANEL, bold=.true.)
    dim_style = style_from_rgb(fg=COLOR_UI_DIM)
    focus = ""
    if (present(focused_widget)) focus = trim(focused_widget)
    is_zoomed = .false.
    if (present(zoomed)) is_zoomed = zoomed .and. len(focus) > 0
    is_paused = .false.
    if (present(paused)) is_paused = paused
    cpu_offset = 0
    if (present(cpu_core_scroll_offset)) cpu_offset = cpu_core_scroll_offset
    cpu_active = .false.
    if (present(cpu_core_active)) cpu_active = cpu_core_active
    process_active = .false.
    if (present(process_tree_active)) process_active = process_tree_active
    network_active = .false.
    if (present(network_table_active)) network_active = network_table_active
    disk_active = .false.
    if (present(disk_table_active)) disk_active = disk_table_active
    gpu_active = .false.
    if (present(gpu_process_active)) gpu_active = gpu_process_active

    call clear_screen(buffer)
    buffer%cursor_visible = .false.

    if (width < 8 .or. height < 4) then
      call render_text(buffer, widget_rect(1, 1, width, 1), "ftop", title_style, TEXT_ALIGN_CENTER)
      if (present(horizontal_state)) call horizontal_text_end_frame(horizontal_state)
      return
    end if

    if (present(grid)) then
      layout = dashboard_layout_from_grid(width, height, grid)
    else
      layout = default_dashboard_layout(width, height)
    end if
    if (is_paused) then
      call draw_box(buffer, layout%frame, BOX_STYLE_DOUBLE, border_style, "ftop PAUSED", title_style, TEXT_ALIGN_CENTER)
    else
      call draw_box(buffer, layout%frame, BOX_STYLE_DOUBLE, border_style, "ftop", title_style, TEXT_ALIGN_CENTER)
    end if

    if (is_zoomed) then
      zoom_rect = widget_rect(3, 3, max(0, width - 4), max(0, height - 5))
      call render_dashboard_panel(buffer, zoom_rect, focus, snapshot, focus_style, title_style, dim_style, .true., &
                                    process_state=process_state, network_state=network_state, disk_state=disk_state, &
                                    cpu_core_scroll_offset=cpu_offset, gpu_state=gpu_state, gpu_process_active=.true., &
                                    horizontal_state=horizontal_state)
    else
      cpu_border_style = border_style
      disk_border_style = border_style
      gpu_border_style = border_style
      memory_border_style = border_style
      network_border_style = border_style
      process_border_style = border_style
      if (focus == "cpu") cpu_border_style = focus_style
      if (focus == "disk") disk_border_style = focus_style
      if (focus == "gpu") gpu_border_style = focus_style
      if (focus == "memory") memory_border_style = focus_style
      if (focus == "network") network_border_style = focus_style
      if (focus == "process") process_border_style = focus_style
      if (layout%cpu_panel%height >= 3) then
        if (focus == "cpu") then
          call render_cpu_panel(buffer, layout%cpu_panel, snapshot, cpu_border_style, title_style, dim_style, &
                                core_scroll_offset=cpu_offset, horizontal_state=horizontal_state)
        else
          call render_cpu_panel(buffer, layout%cpu_panel, snapshot, cpu_border_style, title_style, dim_style, &
                                core_scroll_offset=cpu_offset)
        end if
      end if
      if (layout%memory_panel%height >= 3) then
        if (focus == "memory") then
          call render_memory_panel(buffer, layout%memory_panel, snapshot, memory_border_style, title_style, dim_style, &
                                   expanded=.false., horizontal_state=horizontal_state)
        else
          call render_memory_panel(buffer, layout%memory_panel, snapshot, memory_border_style, title_style, dim_style, &
                                   expanded=.false.)
        end if
      end if
      if (layout%network_panel%height >= 3) then
        if (focus == "network") then
          if (present(network_state)) then
            call render_network_panel(buffer, layout%network_panel, snapshot, network_border_style, title_style, dim_style, &
                                      network_state, expanded=.false., horizontal_state=horizontal_state, &
                                      table_active=network_active)
          else
            call render_network_panel(buffer, layout%network_panel, snapshot, network_border_style, title_style, dim_style, &
                                      expanded=.false., horizontal_state=horizontal_state, table_active=network_active)
          end if
        else
          if (present(network_state)) then
            call render_network_panel(buffer, layout%network_panel, snapshot, network_border_style, title_style, dim_style, &
                                      network_state, expanded=.false.)
          else
            call render_network_panel(buffer, layout%network_panel, snapshot, network_border_style, title_style, dim_style, &
                                      expanded=.false.)
          end if
        end if
      end if
      if (layout%disk_panel%height >= 3) then
        if (focus == "disk") then
          call render_disk_panel(buffer, layout%disk_panel, snapshot, disk_border_style, title_style, dim_style, &
                                 state=disk_state, expanded=.false., horizontal_state=horizontal_state, &
                                 table_active=disk_active)
        else
          call render_disk_panel(buffer, layout%disk_panel, snapshot, disk_border_style, title_style, dim_style, &
                                 state=disk_state, expanded=.false.)
        end if
      end if
      if (layout%gpu_panel%height >= 3) then
        if (focus == "gpu") then
          if (present(gpu_state)) then
            call render_gpu_panel(buffer, layout%gpu_panel, snapshot, gpu_border_style, title_style, dim_style, &
                                  expanded=.false., state=gpu_state, process_active=gpu_active, &
                                  horizontal_state=horizontal_state)
          else
            call render_gpu_panel(buffer, layout%gpu_panel, snapshot, gpu_border_style, title_style, dim_style, &
                                  expanded=.false., process_active=gpu_active, horizontal_state=horizontal_state)
          end if
        else
          if (present(gpu_state)) then
            call render_gpu_panel(buffer, layout%gpu_panel, snapshot, gpu_border_style, title_style, dim_style, &
                                  expanded=.false., state=gpu_state, process_active=gpu_active)
          else
            call render_gpu_panel(buffer, layout%gpu_panel, snapshot, gpu_border_style, title_style, dim_style, &
                                  expanded=.false., process_active=gpu_active)
          end if
        end if
      end if
      if (layout%process_panel%height >= 3) then
        if (focus == "process") then
          if (present(process_state)) then
            call render_process_panel(buffer, layout%process_panel, snapshot, process_border_style, title_style, dim_style, &
                                      process_state, horizontal_state=horizontal_state, rows_active=process_active)
          else
            call render_process_panel(buffer, layout%process_panel, snapshot, process_border_style, title_style, dim_style, &
                                      horizontal_state=horizontal_state, rows_active=process_active)
          end if
        else
          if (present(process_state)) then
            call render_process_panel(buffer, layout%process_panel, snapshot, process_border_style, title_style, dim_style, &
                                      process_state)
          else
            call render_process_panel(buffer, layout%process_panel, snapshot, process_border_style, title_style, dim_style)
          end if
        end if
      end if
    end if

    if (.not. is_zoomed .and. layout%cpu_panel%height < 3 .and. layout%memory_panel%height < 3 .and. &
        layout%network_panel%height < 3 .and. layout%disk_panel%height < 3 .and. layout%gpu_panel%height < 3 .and. &
        layout%process_panel%height < 3) then
      title_col = max(2, (width - len_trim("CPU / Memory / Network / Disk / GPU / Processes")) / 2 + 1)
      title_rect = widget_rect(max(2, height / 2), title_col, width - title_col, 1)
      call render_text(buffer, title_rect, "CPU / Memory / Network / Disk / GPU / Processes", title_style)
    end if

    call render_footer(buffer, layout%footer, snapshot, refresh_ms, frame_count, status_text, &
                        dim_style, focus_style, focus, is_zoomed, render_fps=render_fps, layout_name=layout_name, &
                        paused=is_paused, cpu_core_active=cpu_active, process_tree_active=process_active, &
                        network_table_active=network_active, disk_table_active=disk_active, &
                        gpu_process_active=gpu_active, process_state=process_state, network_state=network_state, &
                         disk_state=disk_state, gpu_state=gpu_state)
    if (present(horizontal_state)) call horizontal_text_end_frame(horizontal_state)
  end subroutine render_dashboard

  subroutine render_dashboard_panel(buffer, rect, widget, snapshot, border_style, title_style, dim_style, expanded, &
                                    process_state, network_state, disk_state, cpu_core_scroll_offset, gpu_state, &
                                    gpu_process_active, horizontal_state)
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
    type(disk_table_state), intent(inout), optional :: disk_state
    integer, intent(in), optional :: cpu_core_scroll_offset
    type(gpu_process_state), intent(inout), optional :: gpu_state
    logical, intent(in), optional :: gpu_process_active
    type(horizontal_text_state), intent(inout), optional :: horizontal_state
    logical :: active_gpu_processes

    if (rect%height < 3 .or. rect%width <= 0) return
    active_gpu_processes = expanded
    if (present(gpu_process_active)) active_gpu_processes = gpu_process_active
    select case (trim(widget))
    case ("cpu")
      call render_cpu_panel(buffer, rect, snapshot, border_style, title_style, dim_style, &
                            core_scroll_offset=cpu_core_scroll_offset, horizontal_state=horizontal_state)
    case ("memory")
      call render_memory_panel(buffer, rect, snapshot, border_style, title_style, dim_style, expanded=expanded, &
                               horizontal_state=horizontal_state)
    case ("network")
      if (present(network_state)) then
        call render_network_panel(buffer, rect, snapshot, border_style, title_style, dim_style, network_state, expanded, &
                                  horizontal_state, table_active=expanded)
      else
        call render_network_panel(buffer, rect, snapshot, border_style, title_style, dim_style, expanded=expanded, &
                                  horizontal_state=horizontal_state, table_active=expanded)
      end if
    case ("disk")
      call render_disk_panel(buffer, rect, snapshot, border_style, title_style, dim_style, state=disk_state, &
                             expanded=expanded, horizontal_state=horizontal_state, table_active=expanded)
    case ("gpu")
      if (present(gpu_state)) then
        call render_gpu_panel(buffer, rect, snapshot, border_style, title_style, dim_style, expanded=expanded, &
                              state=gpu_state, process_active=active_gpu_processes, horizontal_state=horizontal_state)
      else
        call render_gpu_panel(buffer, rect, snapshot, border_style, title_style, dim_style, expanded=expanded, &
                              process_active=active_gpu_processes, horizontal_state=horizontal_state)
      end if
    case ("process")
      if (present(process_state)) then
        call render_process_panel(buffer, rect, snapshot, border_style, title_style, dim_style, process_state, &
                                  horizontal_state=horizontal_state, rows_active=expanded)
      else
        call render_process_panel(buffer, rect, snapshot, border_style, title_style, dim_style, &
                                  horizontal_state=horizontal_state, rows_active=expanded)
      end if
    end select
  end subroutine render_dashboard_panel

  subroutine render_footer(buffer, footer, snapshot, refresh_ms, frame_count, status_text, dim_style, label_style, &
                            focused_widget, zoomed, render_fps, layout_name, paused, cpu_core_active, process_tree_active, &
                            network_table_active, disk_table_active, gpu_process_active, process_state, network_state, &
                            disk_state, gpu_state)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: footer
    type(collector_snapshot), intent(in) :: snapshot
    integer, intent(in) :: refresh_ms
    integer, intent(in) :: frame_count
    character(len=*), intent(in) :: status_text
    type(screen_style), intent(in) :: dim_style
    type(screen_style), intent(in) :: label_style
    character(len=*), intent(in) :: focused_widget
    logical, intent(in) :: zoomed
    real, intent(in), optional :: render_fps
    character(len=*), intent(in), optional :: layout_name
    logical, intent(in), optional :: paused
    logical, intent(in), optional :: cpu_core_active
    logical, intent(in), optional :: process_tree_active
    logical, intent(in), optional :: network_table_active
    logical, intent(in), optional :: disk_table_active
    logical, intent(in), optional :: gpu_process_active
    type(process_table_state), intent(in), optional :: process_state
    type(network_table_state), intent(in), optional :: network_state
    type(disk_table_state), intent(in), optional :: disk_state
    type(gpu_process_state), intent(in), optional :: gpu_state
    type(widget_rect) :: line
    character(len=:), allocatable :: status
    logical :: is_paused
    logical :: cpu_active
    logical :: disk_active
    logical :: gpu_active
    logical :: network_active
    logical :: process_active

    if (footer%width <= 0 .or. footer%height <= 0) return
    is_paused = .false.
    if (present(paused)) is_paused = paused
    cpu_active = .false.
    if (present(cpu_core_active)) cpu_active = cpu_core_active
    process_active = .false.
    if (present(process_tree_active)) process_active = process_tree_active
    network_active = .false.
    if (present(network_table_active)) network_active = network_table_active
    disk_active = .false.
    if (present(disk_table_active)) disk_active = disk_table_active
    gpu_active = .false.
    if (present(gpu_process_active)) gpu_active = gpu_process_active

    line = widget_rect(footer%row, footer%col, footer%width, 1)
    call render_footer_segment(buffer, line, "KEYS", footer_actions_text(focused_widget, zoomed, is_paused, cpu_active, &
                                                                         process_active, network_active, disk_active, gpu_active, &
                                                                         process_state=process_state, &
                                                                         network_state=network_state), &
                               label_style, dim_style)

    if (footer%height < 2) return
    status = trim(status_text)
    if (len(status) == 0) status = "ready"
    line = widget_rect(footer%row + 1, footer%col, footer%width, 1)
    call render_footer_segment(buffer, line, "STAT", footer_status_text(status, snapshot, refresh_ms, frame_count, &
                                                                        focused_widget, zoomed, is_paused, render_fps, &
                                                                         layout_name, cpu_active, process_active, &
                                                                         network_active, disk_active, gpu_active, &
                                                                         process_state=process_state, network_state=network_state, &
                                                                         disk_state=disk_state, gpu_state=gpu_state), &
                               label_style, dim_style)
  end subroutine render_footer

  subroutine render_footer_segment(buffer, rect, label, value, label_style, value_style)
    type(screen_buffer), intent(inout) :: buffer
    type(widget_rect), intent(in) :: rect
    character(len=*), intent(in) :: label
    character(len=*), intent(in) :: value
    type(screen_style), intent(in) :: label_style
    type(screen_style), intent(in) :: value_style
    type(widget_rect) :: label_rect
    type(widget_rect) :: value_rect
    character(len=:), allocatable :: label_text
    integer :: label_width

    if (rect%width <= 0 .or. rect%height <= 0) return
    label_text = trim(label) // " "
    label_width = min(rect%width, len(label_text))
    if (label_width > 0) then
      label_rect = widget_rect(rect%row, rect%col, label_width, 1)
      call render_text(buffer, label_rect, label_text, label_style)
    end if
    if (rect%width <= label_width) return
    value_rect = widget_rect(rect%row, rect%col + label_width, rect%width - label_width, 1)
    call render_text(buffer, value_rect, trim(value), value_style)
  end subroutine render_footer_segment

  function footer_actions_text(focused_widget, zoomed, paused, cpu_core_active, process_tree_active, network_table_active, &
                               disk_table_active, gpu_process_active, process_state, network_state) result(text)
    character(len=*), intent(in) :: focused_widget
    logical, intent(in) :: zoomed
    logical, intent(in) :: paused
    logical, intent(in) :: cpu_core_active
    logical, intent(in) :: process_tree_active
    logical, intent(in) :: network_table_active
    logical, intent(in) :: disk_table_active
    logical, intent(in) :: gpu_process_active
    type(process_table_state), intent(in), optional :: process_state
    type(network_table_state), intent(in), optional :: network_state
    character(len=:), allocatable :: text

    if (paused) then
      text = "F7 resume  q quit  Ctrl+Z suspend"
      return
    end if

    select case (trim(focused_widget))
    case ("cpu")
      if (zoomed) then
        text = "Up/Down cores  Enter grid  Esc grid  q quit"
      else if (cpu_core_active) then
        text = "Up/Down cores  Enter zoom  Esc focus  q quit"
      else
        text = "Enter cores  z zoom  arrows panes  Tab focus  q quit"
      end if
    case ("process")
      if (present(process_state)) then
        text = process_footer_actions(zoomed, process_tree_active, process_state)
      else if (zoomed .or. process_tree_active) then
        text = "Up/Down rows  Left/Right reveal  h/l  F5 flip  Esc grid"
      else
        text = "Enter tree  Down tree  z zoom  F4 filter  F9 signal"
      end if
    case ("network")
      if (present(network_state)) then
        text = network_footer_actions(zoomed, network_table_active, network_state)
      else if (zoomed) then
        text = "Up/Down rows  :port/ip jump  Left/Right reveal  h/l  f state  Esc grid"
      else if (network_table_active) then
        text = "Up/Down rows  :port/ip jump  Left/Right reveal  h/l  Enter zoom  Esc focus"
      else
        text = "f state  s sort  z zoom  arrows panes  q quit"
      end if
    case ("memory")
      if (zoomed) then
        text = "Esc grid  Enter grid  q quit"
      else
        text = "z/Enter zoom  arrows panes  Tab focus  q quit"
      end if
    case ("disk")
      if (zoomed) then
        text = "Up/Down rows  Page rows  Left/Right reveal  h/l  Enter grid  Esc grid"
      else if (disk_table_active) then
        text = "Up/Down rows  Page rows  Left/Right reveal  h/l  Enter zoom  Esc focus"
      else
        text = "Enter table  z zoom  arrows panes  Tab focus  q quit"
      end if
    case ("gpu")
      if (zoomed) then
        text = "Up/Down rows  Left/Right reveal  h/l  Enter grid  Esc grid"
      else if (gpu_process_active) then
        text = "Up/Down rows  Left/Right reveal  h/l  Enter zoom  Esc focus"
      else
        text = "Enter processes  z zoom  arrows panes  Tab focus  q quit"
      end if
    case default
      text = "Tab focus  arrows panes  z/Enter zoom  P layout  ? help  q quit"
    end select
  end function footer_actions_text

  function process_footer_actions(zoomed, process_tree_active, state) result(text)
    logical, intent(in) :: zoomed
    logical, intent(in) :: process_tree_active
    type(process_table_state), intent(in) :: state
    character(len=:), allocatable :: text

    if (state%signal_pending) then
      text = "digits signal  Left/Right choose  Enter send  Esc cancel"
    else if (state%filter_active) then
      text = "type filter  Left/Right cursor  Enter apply  Esc clear"
    else if (state%fuzzy_query_length > 0) then
      text = "type find  Up/Down match  Backspace edit  Esc clear"
    else if (zoomed) then
      text = "Up/Down rows  Left/Right reveal  h/l  F5 flip  F3 tree  F4 filter  Esc grid"
    else if (process_tree_active) then
      text = "Up/Down rows  Left/Right reveal  h/l  F5 flip  Enter zoom  Esc focus"
    else
      text = "Enter tree  Down tree  z zoom  F4 filter  F9 signal"
    end if
  end function process_footer_actions

  function network_footer_actions(zoomed, network_table_active, state) result(text)
    logical, intent(in) :: zoomed
    logical, intent(in) :: network_table_active
    type(network_table_state), intent(in) :: state
    character(len=:), allocatable :: text

    associate(unused_state => state)
    end associate
    if (zoomed) then
      text = "Up/Down rows  :port/ip jump  Left/Right reveal  h/l  f filter  Esc grid"
    else if (network_table_active) then
      text = "Up/Down rows  :port/ip jump  Left/Right reveal  h/l  Enter zoom  Esc focus"
    else
      text = "Enter table  z zoom  s sort  f filter  arrows panes  q quit"
    end if
  end function network_footer_actions

  function footer_status_text(status, snapshot, refresh_ms, frame_count, focused_widget, zoomed, paused, render_fps, &
                               layout_name, cpu_core_active, process_tree_active, network_table_active, disk_table_active, &
                               gpu_process_active, process_state, network_state, disk_state, gpu_state) result(text)
    type(collector_snapshot), intent(in) :: snapshot
    integer, intent(in) :: refresh_ms
    integer, intent(in) :: frame_count
    character(len=*), intent(in) :: status
    character(len=*), intent(in) :: focused_widget
    logical, intent(in) :: zoomed
    logical, intent(in) :: paused
    real, intent(in), optional :: render_fps
    character(len=*), intent(in), optional :: layout_name
    logical, intent(in) :: cpu_core_active
    logical, intent(in) :: process_tree_active
    logical, intent(in) :: network_table_active
    logical, intent(in) :: disk_table_active
    logical, intent(in) :: gpu_process_active
    type(process_table_state), intent(in), optional :: process_state
    type(network_table_state), intent(in), optional :: network_state
    type(disk_table_state), intent(in), optional :: disk_state
    type(gpu_process_state), intent(in), optional :: gpu_state
    character(len=:), allocatable :: text

    text = trim(status) // " | " // footer_mode_text(focused_widget, zoomed, paused, cpu_core_active, &
                                                      process_tree_active, network_table_active, disk_table_active, &
                                                      gpu_process_active, &
                                                      process_state=process_state, network_state=network_state, &
                                                      disk_state=disk_state, gpu_state=gpu_state)
    text = text // footer_layout_text(layout_name) // " | " // footer_timing_text(snapshot, refresh_ms, frame_count, render_fps)
  end function footer_status_text

  function footer_mode_text(focused_widget, zoomed, paused, cpu_core_active, process_tree_active, network_table_active, &
                            disk_table_active, gpu_process_active, process_state, network_state, disk_state, gpu_state) &
                            result(text)
    character(len=*), intent(in) :: focused_widget
    logical, intent(in) :: zoomed
    logical, intent(in) :: paused
    logical, intent(in) :: cpu_core_active
    logical, intent(in) :: process_tree_active
    logical, intent(in) :: network_table_active
    logical, intent(in) :: disk_table_active
    logical, intent(in) :: gpu_process_active
    type(process_table_state), intent(in), optional :: process_state
    type(network_table_state), intent(in), optional :: network_state
    type(disk_table_state), intent(in), optional :: disk_state
    type(gpu_process_state), intent(in), optional :: gpu_state
    character(len=:), allocatable :: text

    if (paused) then
      text = "paused"
      return
    end if

    select case (trim(focused_widget))
    case ("cpu")
      if (zoomed) then
        text = "cpu zoom"
      else if (cpu_core_active) then
        text = "cpu cores"
      else
        text = "cpu focus"
      end if
    case ("process")
      if (present(process_state)) then
        text = process_footer_mode(zoomed, process_tree_active, process_state)
      else if (zoomed) then
        text = "process zoom"
      else if (process_tree_active) then
        text = "process tree"
      else
        text = "process focus"
      end if
    case ("network")
      if (present(network_state)) then
        text = network_footer_mode(zoomed, network_table_active, network_state)
      else if (zoomed) then
        text = "network zoom"
      else if (network_table_active) then
        text = "network table"
      else
        text = "network focus"
      end if
    case ("memory")
      if (zoomed) then
        text = "memory zoom"
      else
        text = "memory focus"
      end if
    case ("disk")
      if (present(disk_state)) then
        text = disk_footer_mode(zoomed, disk_table_active, disk_state)
      else if (zoomed) then
        text = "disk zoom"
      else if (disk_table_active) then
        text = "disk table"
      else
        text = "disk focus"
      end if
    case ("gpu")
      if (present(gpu_state)) then
        text = gpu_footer_mode(zoomed, gpu_process_active, gpu_state)
      else if (zoomed) then
        text = "gpu zoom"
      else if (gpu_process_active) then
        text = "gpu processes"
      else
        text = "gpu focus"
      end if
    case default
      if (zoomed) then
        text = "zoom"
      else
        text = "grid"
      end if
    end select
  end function footer_mode_text

  function process_footer_mode(zoomed, process_tree_active, state) result(text)
    logical, intent(in) :: zoomed
    logical, intent(in) :: process_tree_active
    type(process_table_state), intent(in) :: state
    character(len=:), allocatable :: text

    if (state%signal_pending) then
      text = "process signal"
    else if (state%filter_active) then
      text = "process filter"
    else if (state%fuzzy_query_length > 0) then
      text = "process find"
    else if (zoomed) then
      text = "process zoom"
    else if (process_tree_active) then
      text = "process tree"
    else
      text = "process focus"
    end if
    text = text // " | row " // integer_text(max(0, state%selected_row)) // "/" // integer_text(max(0, state%row_count)) // &
           " | sort " // process_table_sort_key_label(state) // " " // process_table_sort_direction_label(state)
  end function process_footer_mode

  function network_footer_mode(zoomed, network_table_active, state) result(text)
    logical, intent(in) :: zoomed
    logical, intent(in) :: network_table_active
    type(network_table_state), intent(in) :: state
    character(len=:), allocatable :: text

    if (zoomed) then
      text = "network zoom"
    else if (network_table_active) then
      text = "network table"
    else
      text = "network focus"
    end if
    text = text // " | row " // integer_text(max(0, state%selected_row)) // "/" // integer_text(max(0, state%row_count)) // &
           " | sort " // network_table_sort_key_label(state) // " " // network_table_sort_direction_label(state)
    if (len_trim(state%state_filter) > 0) text = text // " | state " // trim(state%state_filter)
  end function network_footer_mode

  function disk_footer_mode(zoomed, disk_table_active, state) result(text)
    logical, intent(in) :: zoomed
    logical, intent(in) :: disk_table_active
    type(disk_table_state), intent(in) :: state
    character(len=:), allocatable :: text

    if (zoomed) then
      text = "disk zoom"
    else if (disk_table_active) then
      text = "disk table"
    else
      text = "disk focus"
    end if
    text = text // " | row " // integer_text(max(0, state%selected_row)) // "/" // &
           integer_text(max(0, state%row_count))
  end function disk_footer_mode

  function gpu_footer_mode(zoomed, gpu_process_active, state) result(text)
    logical, intent(in) :: zoomed
    logical, intent(in) :: gpu_process_active
    type(gpu_process_state), intent(in) :: state
    character(len=:), allocatable :: text

    if (zoomed) then
      text = "gpu zoom"
    else if (gpu_process_active) then
      text = "gpu processes"
    else
      text = "gpu focus"
    end if
    text = text // " | row " // integer_text(max(0, state%selected_row)) // "/" // &
           integer_text(max(0, state%row_count))
  end function gpu_footer_mode

  function footer_layout_text(layout_name) result(text)
    character(len=*), intent(in), optional :: layout_name
    character(len=:), allocatable :: text

    if (present(layout_name)) then
      if (len_trim(layout_name) > 0) then
        text = " | " // trim(layout_name)
      else
        text = ""
      end if
    else
      text = ""
    end if
  end function footer_layout_text

  function footer_timing_text(snapshot, refresh_ms, frame_count, render_fps) result(text)
    type(collector_snapshot), intent(in) :: snapshot
    integer, intent(in) :: refresh_ms
    integer, intent(in) :: frame_count
    real, intent(in), optional :: render_fps
    character(len=:), allocatable :: text
    character(len=:), allocatable :: running_text

    if (snapshot%running) then
      running_text = "running"
    else
      running_text = "idle"
    end if
    associate(unused_frame_count => frame_count)
    end associate
    if (present(render_fps)) then
      associate(unused_render_fps => render_fps)
      end associate
    end if
    text = integer_text(refresh_ms) // "ms | samples " // integer_text(snapshot%sample_count) // " | " // running_text
  end function footer_timing_text

  function integer_text(value) result(text)
    integer, intent(in) :: value
    character(len=:), allocatable :: text
    character(len=32) :: scratch

    write(scratch, '(i0)') value
    text = trim(scratch)
  end function integer_text

end module ftop_dashboard
