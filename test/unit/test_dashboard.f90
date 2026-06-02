program test_dashboard
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use fgof_screen, only : allocate_screen
  use fgof_screen_types, only : screen_buffer
  use ftop_collector, only : collector_snapshot
  use ftop_dashboard, only : dashboard_layout, default_dashboard_layout, render_dashboard
  implicit none

  integer(int64), parameter :: GIB = 1024_int64 * 1024_int64 * 1024_int64

  call test_default_layout()
  call test_dashboard_renders_metrics()
  call test_dashboard_renders_zoomed_widget()
  call test_tiny_dashboard()

contains

  subroutine test_default_layout()
    type(dashboard_layout) :: layout

    layout = default_dashboard_layout(120, 24)
    call require(layout%frame%width == 120 .and. layout%frame%height == 24, &
                 "wide layout should cover the screen")
    call require(layout%cpu_panel%row == 3, "wide layout should place cpu panel below title")
    call require(layout%memory_panel%row == layout%cpu_panel%row, "wide layout should use side-by-side panels")
    call require(layout%memory_panel%col > layout%cpu_panel%col, "wide layout memory panel should be right of cpu")
    call require(layout%network_panel%row == layout%cpu_panel%row, "wide layout should place network beside cpu")
    call require(layout%network_panel%col > layout%memory_panel%col, "wide layout network panel should be right of memory")
    call require(layout%footer%row == 22, "wide layout footer should stay above bottom border")

    layout = default_dashboard_layout(80, 24)
    call require(layout%cpu_panel%col == layout%memory_panel%col, "narrow layout should stack panels")
    call require(layout%memory_panel%row > layout%cpu_panel%row, "narrow layout memory panel should follow cpu")
    call require(layout%memory_panel%width == layout%cpu_panel%width, "narrow panels should share width")
    call require(layout%network_panel%row > layout%memory_panel%row, "narrow layout network panel should follow memory")
    call require(layout%network_panel%width == layout%cpu_panel%width, "narrow network panel should share width")
  end subroutine test_default_layout

  subroutine test_dashboard_renders_metrics()
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot
    character(len=:), allocatable :: text

    snapshot = sample_snapshot()
    buffer = allocate_screen(120, 24)
    call render_dashboard(buffer, snapshot, 1000, 7, "ready")
    text = buffer_text(buffer)

    call require(index(text, "CPU") > 0, "dashboard should render cpu panel")
    call require(index(text, "Memory") > 0, "dashboard should render memory panel")
    call require(index(text, "Network") > 0, "dashboard should render network panel")
    call require(index(text, "42.5%") > 0, "dashboard should render cpu usage")
    call require(index(text, "37.5%") > 0, "dashboard should render memory usage")
    call require(index(text, "6.0 GiB / 16.0 GiB") > 0, "dashboard should render memory bytes")
    call require(index(text, "cores 4 threads 8") > 0, "dashboard should render cpu topology")
    call require(index(text, "Test CPU") > 0, "dashboard should render cpu model")
    call require(index(text, "uptime 1d 02:03:04") > 0, "dashboard should render uptime")
    call require(index(text, "c0") > 0, "dashboard should render core labels")
    call require(index(text, "available 8.0 GiB") > 0, "dashboard should render available memory")
    call require(index(text, "swap 1.0 GiB / 2.0 GiB") > 0, "dashboard should render swap")
    call require(index(text, "Interfaces 1") > 0, "dashboard should render network summary")
    call require(index(text, "eth0 up") > 0, "dashboard should render network interface")
    call require(index(text, "refresh 1000ms frame 7 fps 1.0 samples 3") > 0, &
                 "dashboard should render footer")
    call require(index(text, "ready") > 0, "dashboard should render status")
  end subroutine test_dashboard_renders_metrics

  subroutine test_dashboard_renders_zoomed_widget()
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot
    character(len=:), allocatable :: text

    snapshot = sample_snapshot()
    buffer = allocate_screen(80, 24)
    call render_dashboard(buffer, snapshot, 1000, 7, "zoom-test", focused_widget="memory", zoomed=.true.)
    text = buffer_text(buffer)

    call require(index(text, "Memory") > 0, "zoomed dashboard should render focused memory widget")
    call require(index(text, "CPU 42.5%") == 0, "zoomed dashboard should hide unfocused cpu widget")
    call require(index(text, "zoom memory") > 0, "zoomed dashboard should render zoom status")
  end subroutine test_dashboard_renders_zoomed_widget

  subroutine test_tiny_dashboard()
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot

    buffer = allocate_screen(7, 3)
    call render_dashboard(buffer, snapshot, 1000, 0, "")
    call require(index(buffer_text(buffer), "ftop") > 0, "tiny dashboard should render app name")
  end subroutine test_tiny_dashboard

  function sample_snapshot() result(snapshot)
    type(collector_snapshot) :: snapshot

    snapshot%running = .true.
    snapshot%warming_up = .false.
    snapshot%sample_count = 3
    snapshot%system_uptime_valid = .true.
    snapshot%system_uptime_seconds = 93784_int64
    snapshot%cpu_total%valid = .true.
    snapshot%cpu_total%usage_percent = 42.5_real64
    snapshot%cpu_total%load_valid = .true.
    snapshot%cpu_total%load_avg = [1.25_real64, 0.75_real64, 0.50_real64]
    snapshot%cpu_total%core_count = 4
    snapshot%cpu_total%thread_count = 8
    snapshot%cpu_total%model_name_valid = .true.
    snapshot%cpu_total%model_name = "Test CPU"
    allocate(snapshot%cpu_cores(2))
    snapshot%cpu_cores(1)%valid = .true.
    snapshot%cpu_cores(1)%usage_percent = 25.0_real64
    snapshot%cpu_cores(1)%freq_valid = .true.
    snapshot%cpu_cores(1)%freq_mhz = 2400.0_real64
    snapshot%cpu_cores(1)%temp_valid = .true.
    snapshot%cpu_cores(1)%temp_c = 45.0_real64
    snapshot%cpu_cores(2)%valid = .true.
    snapshot%cpu_cores(2)%usage_percent = 75.0_real64
    snapshot%cpu_cores(2)%freq_valid = .true.
    snapshot%cpu_cores(2)%freq_mhz = 2600.0_real64
    snapshot%cpu_cores(2)%temp_valid = .true.
    snapshot%cpu_cores(2)%temp_c = 55.0_real64
    snapshot%cpu_usage_history = [10.0_real64, 20.0_real64, 30.0_real64, 42.5_real64]
    allocate(snapshot%cpu_core_usage_history(2, 4))
    snapshot%cpu_core_usage_history(1, :) = [10.0_real64, 15.0_real64, 20.0_real64, 25.0_real64]
    snapshot%cpu_core_usage_history(2, :) = [40.0_real64, 50.0_real64, 65.0_real64, 75.0_real64]

    snapshot%memory%valid = .true.
    snapshot%memory%total_bytes = 16_int64 * GIB
    snapshot%memory%used_bytes = 6_int64 * GIB
    snapshot%memory%free_bytes = 4_int64 * GIB
    snapshot%memory%available_bytes = 8_int64 * GIB
    snapshot%memory%cached_bytes = 2_int64 * GIB
    snapshot%memory%buffers_bytes = GIB / 2_int64
    snapshot%memory%swap_total_bytes = 2_int64 * GIB
    snapshot%memory%swap_used_bytes = GIB
    snapshot%memory_usage_history = [20.0_real64, 30.0_real64, 37.5_real64]

    snapshot%network%valid = .true.
    allocate(snapshot%network%interfaces(1))
    allocate(snapshot%network%connections(0))
    allocate(snapshot%network%processes(0))
    snapshot%network%interfaces(1)%valid = .true.
    snapshot%network%interfaces(1)%name = "eth0"
    snapshot%network%interfaces(1)%state = "up"
    snapshot%network%interfaces(1)%rx_bytes_per_sec = 1536.0_real64
    snapshot%network%interfaces(1)%tx_bytes_per_sec = 512.0_real64
    snapshot%network%interfaces(1)%history_count = 3
    snapshot%network%interfaces(1)%rx_history(:3) = [128.0_real64, 512.0_real64, 1536.0_real64]
    snapshot%network%interfaces(1)%tx_history(:3) = [64.0_real64, 256.0_real64, 512.0_real64]
  end function sample_snapshot

  function buffer_text(buffer) result(text)
    type(screen_buffer), intent(in) :: buffer
    character(len=:), allocatable :: text
    integer :: row

    text = ""
    do row = 1, buffer%size%height
      text = text // row_text(buffer, row) // new_line("a")
    end do
  end function buffer_text

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

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

end program test_dashboard
