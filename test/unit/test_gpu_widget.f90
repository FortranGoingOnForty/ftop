program test_gpu_widget
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use fgof_screen, only : allocate_screen, clear_screen_style
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_collector, only : collector_snapshot
  use ftop_color, only : rgb, style_from_rgb
  use ftop_gpu, only : gpu_process_page_delta, gpu_process_select_delta, &
    gpu_process_state, gpu_process_status, render_gpu_panel
  use ftop_gpu_data, only : empty_gpu_table, gpu_process_info
  use ftop_text, only : horizontal_text_scroll, horizontal_text_state
  use ftop_widgets, only : widget_rect
  implicit none

  integer(int64), parameter :: GIB = 1024_int64 * 1024_int64 * 1024_int64

  call test_gpu_unavailable_state()
  call test_gpu_empty_state()
  call test_gpu_metrics_render()
  call test_gpu_process_rows_render()
  call test_gpu_process_state_navigation()
  call test_gpu_process_rows_scroll_and_highlight()
  call test_gpu_process_horizontal_scroll_reveals_long_row()
  call test_gpu_process_horizontal_scroll_noop_for_short_row()
  call test_gpu_temperature_history_render()

contains

  subroutine test_gpu_unavailable_state()
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot
    type(screen_style) :: style
    character(len=:), allocatable :: text

    style = clear_screen_style()
    buffer = allocate_screen(48, 8)
    call render_gpu_panel(buffer, widget_rect(1, 1, 48, 8), snapshot, style, style, style)
    text = buffer_text(buffer)

    call require(index(text, "GPU") > 0, "gpu panel should draw title")
    call require(index(text, "gpu unavailable") > 0, "invalid GPU table should render unavailable state")
  end subroutine test_gpu_unavailable_state

  subroutine test_gpu_empty_state()
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot
    type(screen_style) :: style
    character(len=:), allocatable :: text

    style = clear_screen_style()
    snapshot%gpu = empty_gpu_table()
    buffer = allocate_screen(48, 8)
    call render_gpu_panel(buffer, widget_rect(1, 1, 48, 8), snapshot, style, style, style)
    text = buffer_text(buffer)

    call require(index(text, "No GPUs detected") > 0, "empty GPU table should render no-GPU state")
  end subroutine test_gpu_empty_state

  subroutine test_gpu_metrics_render()
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot
    type(screen_style) :: style
    character(len=:), allocatable :: text

    style = clear_screen_style()
    call populate_gpu_snapshot(snapshot)
    buffer = allocate_screen(64, 9)
    call render_gpu_panel(buffer, widget_rect(1, 1, 64, 9), snapshot, style, style, style)
    text = buffer_text(buffer)

    call require(index(text, "GPUs 1") > 0, "gpu panel should render GPU count")
    call require(index(text, "nvidia Test RTX") > 0, "gpu panel should render vendor and name")
    call require(index(text, "GPU 72.5%") > 0, "gpu panel should render utilization label")
    call require(index(text, "util") > 0, "gpu panel should render utilization history label")
    call require(index(text, "VRAM 6.0 GiB / 12.0 GiB") > 0, "gpu panel should render VRAM usage")
    call require(index(text, "temp 63.5 C") > 0, "gpu panel should render temperature")
    call require(index(text, "power 120.0 W / 250.0 W") > 0, "gpu panel should render power draw")
  end subroutine test_gpu_metrics_render

  subroutine test_gpu_process_rows_render()
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot
    type(screen_style) :: style
    character(len=:), allocatable :: text

    style = clear_screen_style()
    call populate_gpu_snapshot(snapshot)
    snapshot%gpu_processes%valid = .true.
    allocate(snapshot%gpu_processes%processes(1))
    snapshot%gpu_processes%processes(1) = gpu_process_info(valid=.true., pid=4242, start_time=99_int64, &
                                                           process_name="game", engine="render", &
                                                           engine_time_ns=2000000000_int64, &
                                                           busy_percent_valid=.true., busy_percent=50.0_real64, &
                                                           memory_valid=.true., memory_bytes=512_int64 * 1024_int64 * 1024_int64)
    buffer = allocate_screen(72, 12)
    call render_gpu_panel(buffer, widget_rect(1, 1, 72, 12), snapshot, style, style, style, expanded=.true.)
    text = buffer_text(buffer)

    call require(index(text, "Hot GPU processes") > 0, "gpu panel should render process section")
    call require(index(text, "4242 game render 50.0%") > 0, "gpu panel should render hot GPU process row")
    call require(index(text, "512.0 MiB") > 0, "gpu panel should render hot GPU process memory")
  end subroutine test_gpu_process_rows_render

  subroutine test_gpu_process_state_navigation()
    type(gpu_process_state) :: state
    character(len=:), allocatable :: status

    state%row_count = 5
    state%viewport_rows = 2
    call gpu_process_select_delta(state, 10)
    call require(state%selected_row == 5, "gpu process selection should clamp at last row")
    call require(state%scroll_row == 4, "gpu process scroll should follow selected row")

    call gpu_process_page_delta(state, -1)
    call require(state%selected_row == 3, "gpu process page up should move by viewport")
    call require(state%scroll_row == 3, "gpu process page up should keep selection visible")
    status = gpu_process_status(state)
    call require(index(status, "gpu process row 3/5") > 0, "gpu process status should include selected row")
  end subroutine test_gpu_process_state_navigation

  subroutine test_gpu_process_rows_scroll_and_highlight()
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot
    type(screen_style) :: selected_style
    type(screen_style) :: style
    type(gpu_process_state) :: state
    character(len=:), allocatable :: text

    style = clear_screen_style()
    selected_style = style_from_rgb(fg=rgb(1, 2, 3))
    call populate_gpu_snapshot(snapshot)
    call populate_gpu_processes(snapshot, 5)
    state%selected_row = 4

    buffer = allocate_screen(72, 10)
    call render_gpu_panel(buffer, widget_rect(1, 1, 72, 10), snapshot, style, selected_style, style, expanded=.true., &
                          state=state, process_active=.true.)
    text = buffer_text(buffer)

    call require(state%row_count == 5, "gpu process state should track hot process rows")
    call require(state%viewport_rows == 2, "gpu process state should track visible process rows")
    call require(state%scroll_row == 3, "gpu process state should scroll selected row into view")
    call require(index(text, "1003 proc3 render 30.0%") > 0, "gpu process viewport should render first visible row")
    call require(index(text, "1004 proc4 render 40.0%") > 0, "gpu process viewport should render selected row")
    call require(index(text, "1001 proc1") == 0, "gpu process viewport should hide scrolled rows")
    call require(text_cell_has_fg(buffer, "proc4"), "gpu process viewport should highlight selected row")
  end subroutine test_gpu_process_rows_scroll_and_highlight

  subroutine test_gpu_process_horizontal_scroll_reveals_long_row()
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot
    type(screen_style) :: selected_style
    type(screen_style) :: style
    type(gpu_process_state) :: state
    type(horizontal_text_state) :: horizontal_state
    character(len=:), allocatable :: text

    style = clear_screen_style()
    selected_style = style_from_rgb(fg=rgb(1, 2, 3))
    call populate_gpu_snapshot(snapshot)
    call populate_gpu_process(snapshot, 6725, "/usr/local/libexec/gpu-render-worker")
    state%selected_row = 1

    buffer = allocate_screen(22, 10)
    call render_gpu_panel(buffer, widget_rect(1, 1, 22, 10), snapshot, style, selected_style, style, expanded=.true., &
                          state=state, process_active=.true., horizontal_state=horizontal_state)
    text = buffer_text(buffer)

    call require(horizontal_state%limit > 0, "long GPU process row should expose horizontal scroll")
    call require(index(text, "libexec") == 0, "initial GPU process row should be end-ellipsized")

    call horizontal_text_scroll(horizontal_state, 12)
    buffer = allocate_screen(22, 10)
    call render_gpu_panel(buffer, widget_rect(1, 1, 22, 10), snapshot, style, selected_style, style, expanded=.true., &
                          state=state, process_active=.true., horizontal_state=horizontal_state)
    text = buffer_text(buffer)

    call require(horizontal_state%offset > 0, "GPU process horizontal scroll should advance")
    call require(index(text, "libexec") > 0, "GPU process horizontal scroll should reveal hidden text")
  end subroutine test_gpu_process_horizontal_scroll_reveals_long_row

  subroutine test_gpu_process_horizontal_scroll_noop_for_short_row()
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot
    type(screen_style) :: selected_style
    type(screen_style) :: style
    type(gpu_process_state) :: state
    type(horizontal_text_state) :: horizontal_state

    style = clear_screen_style()
    selected_style = style_from_rgb(fg=rgb(1, 2, 3))
    call populate_gpu_snapshot(snapshot)
    call populate_gpu_process(snapshot, 12, "short")
    state%selected_row = 1

    buffer = allocate_screen(72, 10)
    call render_gpu_panel(buffer, widget_rect(1, 1, 72, 10), snapshot, style, selected_style, style, expanded=.true., &
                          state=state, process_active=.true., horizontal_state=horizontal_state)

    call require(horizontal_state%limit == 0, "short GPU process row should not expose horizontal scroll")
    call horizontal_text_scroll(horizontal_state, 1)
    call require(horizontal_state%offset == 0, "short GPU process row horizontal scroll should be a no-op")
  end subroutine test_gpu_process_horizontal_scroll_noop_for_short_row

  subroutine test_gpu_temperature_history_render()
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot
    type(screen_style) :: style
    character(len=:), allocatable :: text

    style = clear_screen_style()
    snapshot%gpu%valid = .true.
    allocate(snapshot%gpu%gpus(1))
    snapshot%gpu%gpus(1)%valid = .true.
    snapshot%gpu%gpus(1)%name = "Thermal GPU"
    snapshot%gpu%gpus(1)%temperature_valid = .true.
    snapshot%gpu%gpus(1)%temp_celsius = 61.0_real64
    allocate(snapshot%gpu_temperature_history(1, 3))
    snapshot%gpu_temperature_history(1, :) = [48.0_real64, 55.0_real64, 61.0_real64]
    buffer = allocate_screen(48, 6)
    call render_gpu_panel(buffer, widget_rect(1, 1, 48, 6), snapshot, style, style, style)
    text = buffer_text(buffer)

    call require(index(text, "temp") > 0, "gpu panel should render temperature history label")
  end subroutine test_gpu_temperature_history_render

  subroutine populate_gpu_snapshot(snapshot)
    type(collector_snapshot), intent(out) :: snapshot

    snapshot%gpu%valid = .true.
    allocate(snapshot%gpu%gpus(1))
    snapshot%gpu%gpus(1)%valid = .true.
    snapshot%gpu%gpus(1)%vendor = "nvidia"
    snapshot%gpu%gpus(1)%name = "Test RTX"
    snapshot%gpu%gpus(1)%utilization_valid = .true.
    snapshot%gpu%gpus(1)%utilization_percent = 72.5_real64
    snapshot%gpu%gpus(1)%memory_valid = .true.
    snapshot%gpu%gpus(1)%memory_used_bytes = 6_int64 * GIB
    snapshot%gpu%gpus(1)%memory_total_bytes = 12_int64 * GIB
    snapshot%gpu%gpus(1)%temperature_valid = .true.
    snapshot%gpu%gpus(1)%temp_celsius = 63.5_real64
    snapshot%gpu%gpus(1)%power_valid = .true.
    snapshot%gpu%gpus(1)%power_watts = 120.0_real64
    snapshot%gpu%gpus(1)%power_limit_watts = 250.0_real64
    allocate(snapshot%gpu_utilization_history(1, 4))
    snapshot%gpu_utilization_history(1, :) = [10.0_real64, 35.0_real64, 55.0_real64, 72.5_real64]
    allocate(snapshot%gpu_temperature_history(1, 4))
    snapshot%gpu_temperature_history(1, :) = [42.0_real64, 51.0_real64, 58.0_real64, 63.5_real64]
  end subroutine populate_gpu_snapshot

  subroutine populate_gpu_processes(snapshot, process_count)
    type(collector_snapshot), intent(inout) :: snapshot
    integer, intent(in) :: process_count
    integer :: process_index

    snapshot%gpu_processes%valid = .true.
    allocate(snapshot%gpu_processes%processes(process_count))
    do process_index = 1, process_count
      snapshot%gpu_processes%processes(process_index) = gpu_process_info(valid=.true., pid=1000 + process_index, &
        start_time=int(process_index, int64), process_name="proc" // integer_text(process_index), engine="render", &
        engine_time_ns=int(process_index, int64) * 1000000000_int64, busy_percent_valid=.true., &
        busy_percent=real(process_index * 10, real64))
    end do
  end subroutine populate_gpu_processes

  subroutine populate_gpu_process(snapshot, pid, process_name)
    type(collector_snapshot), intent(inout) :: snapshot
    integer, intent(in) :: pid
    character(len=*), intent(in) :: process_name

    snapshot%gpu_processes%valid = .true.
    allocate(snapshot%gpu_processes%processes(1))
    snapshot%gpu_processes%processes(1) = gpu_process_info(valid=.true., pid=pid, start_time=1_int64, &
      process_name=process_name, engine="render", engine_time_ns=1000000000_int64, busy_percent_valid=.true., &
      busy_percent=50.0_real64)
  end subroutine populate_gpu_process

  function buffer_text(buffer) result(text)
    type(screen_buffer), intent(in) :: buffer
    character(len=:), allocatable :: text
    integer :: col
    integer :: row

    text = ""
    do row = 1, buffer%size%height
      do col = 1, buffer%size%width
        if (allocated(buffer%cells(row, col)%glyph)) then
          text = text // buffer%cells(row, col)%glyph
        else
          text = text // " "
        end if
      end do
    end do
  end function buffer_text

  logical function text_cell_has_fg(buffer, needle) result(found)
    type(screen_buffer), intent(in) :: buffer
    character(len=*), intent(in) :: needle
    character(len=:), allocatable :: line
    integer :: col
    integer :: row

    found = .false.
    do row = 1, buffer%size%height
      line = row_text(buffer, row)
      col = index(line, needle)
      if (col <= 0) cycle
      found = buffer%cells(row, col)%style%fg_truecolor
      return
    end do
  end function text_cell_has_fg

  function row_text(buffer, row) result(text)
    type(screen_buffer), intent(in) :: buffer
    integer, intent(in) :: row
    character(len=:), allocatable :: text
    integer :: col

    text = ""
    do col = 1, buffer%size%width
      if (allocated(buffer%cells(row, col)%glyph)) then
        text = text // buffer%cells(row, col)%glyph
      else
        text = text // " "
      end if
    end do
  end function row_text

  function integer_text(value) result(text)
    integer, intent(in) :: value
    character(len=:), allocatable :: text
    character(len=32) :: scratch

    write(scratch, '(i0)') value
    text = trim(scratch)
  end function integer_text

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require
end program test_gpu_widget
