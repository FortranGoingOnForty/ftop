program test_dashboard
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use fgof_screen, only : allocate_screen
  use fgof_screen_types, only : screen_buffer
  use ftop_collector, only : collector_snapshot
  use ftop_dashboard, only : dashboard_layout, default_dashboard_layout, render_dashboard
  use ftop_network, only : network_table_state
  use ftop_services, only : load_service_cache_from_text
  implicit none

  integer(int64), parameter :: GIB = 1024_int64 * 1024_int64 * 1024_int64

  call load_service_cache_from_text(test_services_text())

  call test_default_layout()
  call test_dashboard_renders_metrics()
  call test_dashboard_renders_zoomed_widget()
  call test_dashboard_renders_zoomed_network_table()
  call test_dashboard_renders_network_process_bandwidth()
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
    call require(index(text, "Connections 1") > 0, "dashboard should render network connection count")
    call require(index(text, "127.0.0.1:8080") > 0, "dashboard should render network endpoint")
    call require(index(text, "8080(web)") > 0, "dashboard should render service names")
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

  subroutine test_dashboard_renders_zoomed_network_table()
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot
    type(network_table_state) :: network_state
    character(len=:), allocatable :: text

    snapshot = network_table_snapshot()
    network_state%state_filter = "ESTABLISHED"
    buffer = allocate_screen(100, 28)
    call render_dashboard(buffer, snapshot, 1000, 7, "network-test", focused_widget="network", zoomed=.true., &
                          network_state=network_state)
    text = buffer_text(buffer)

    call require(index(text, "Network") > 0, "zoomed network should render network panel")
    call require(index(text, "PROTO") > 0, "zoomed network should render protocol column")
    call require(index(text, "LOCAL") > 0, "zoomed network should render local column")
    call require(index(text, "REMOTE") > 0, "zoomed network should render remote column")
    call require(index(text, "STATE") > 0, "zoomed network should render state column")
    call require(index(text, "PROCESS") > 0, "zoomed network should render process column")
    call require(index(text, "filter ESTABLISHED") > 0, "zoomed network should render active state filter")
    call require(index(text, "curl") > 0, "zoomed network should render matching connection")
    call require(index(text, "443(https)") > 0, "zoomed network should render remote service names")
    call require(index(text, "sshd") == 0, "zoomed network should hide filtered listen connection")
    call require(index(text, "dnsmasq") == 0, "zoomed network should hide filtered udp connection")
    call require(network_state%row_count == 1, "network table state should track filtered rows")
    call require(network_state%total_row_count == 3, "network table state should track total rows")
    call require(network_state%viewport_rows > 0, "network table state should track viewport rows")
  end subroutine test_dashboard_renders_zoomed_network_table

  subroutine test_dashboard_renders_network_process_bandwidth()
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot
    type(network_table_state) :: network_state
    character(len=:), allocatable :: text

    snapshot = network_table_snapshot()
    if (allocated(snapshot%network%processes)) deallocate(snapshot%network%processes)
    allocate(snapshot%network%processes(3))
    snapshot%network%processes(1)%valid = .true.
    snapshot%network%processes(1)%pid = 1234
    snapshot%network%processes(1)%start_time = 42_int64
    snapshot%network%processes(1)%process_name = "curl"
    snapshot%network%processes(1)%rx_bytes_per_sec = 2048.0_real64
    snapshot%network%processes(1)%tx_bytes_per_sec = 512.0_real64
    snapshot%network%processes(2)%valid = .true.
    snapshot%network%processes(2)%pid = 2222
    snapshot%network%processes(2)%start_time = 43_int64
    snapshot%network%processes(2)%process_name = "wget"
    snapshot%network%processes(2)%rx_bytes_per_sec = 4096.0_real64
    snapshot%network%processes(2)%tx_bytes_per_sec = 1024.0_real64
    snapshot%network%processes(3)%valid = .true.
    snapshot%network%processes(3)%pid = 22
    snapshot%network%processes(3)%start_time = 44_int64
    snapshot%network%processes(3)%process_name = "sshd"
    snapshot%network%processes(3)%rx_bytes_per_sec = 128.0_real64
    snapshot%network%processes(3)%tx_bytes_per_sec = 128.0_real64

    buffer = allocate_screen(100, 28)
    call render_dashboard(buffer, snapshot, 1000, 7, "network-test", focused_widget="network", zoomed=.true., &
                          network_state=network_state)
    text = buffer_text(buffer)

    call require(index(text, "Process wget") > 0, "zoomed network should render top bandwidth process")
    call require(index(text, "Process curl") > 0, "zoomed network should render additional process bandwidth")
    call require(index(text, "Process sshd") > 0, "zoomed network should render lower bandwidth process")
    call require(index(text, "Process wget") < index(text, "Process curl"), &
                 "zoomed network should sort process bandwidth by total rate")
    call require(index(text, "rx 2.0 KB/s") > 0, "zoomed network should render process rx bandwidth")
    call require(index(text, "tx 512 B/s") > 0, "zoomed network should render process tx bandwidth")
    call require(index(text, "PROTO") > 0, "zoomed network should keep connection table below process bandwidth")
  end subroutine test_dashboard_renders_network_process_bandwidth

  subroutine test_tiny_dashboard()
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot

    buffer = allocate_screen(7, 3)
    call render_dashboard(buffer, snapshot, 1000, 0, "")
    call require(index(buffer_text(buffer), "ftop") > 0, "tiny dashboard should render app name")
  end subroutine test_tiny_dashboard

  function test_services_text() result(text)
    character(len=:), allocatable :: text

    text = "web 8080/tcp" // new_line("a") // &
           "https 443/tcp" // new_line("a") // &
           "domain 53/udp" // new_line("a") // &
           "ssh 22/tcp"
  end function test_services_text

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
    allocate(snapshot%network%connections(1))
    allocate(snapshot%network%processes(0))
    snapshot%network%interfaces(1)%valid = .true.
    snapshot%network%interfaces(1)%name = "eth0"
    snapshot%network%interfaces(1)%state = "up"
    snapshot%network%interfaces(1)%rx_bytes_per_sec = 1536.0_real64
    snapshot%network%interfaces(1)%tx_bytes_per_sec = 512.0_real64
    snapshot%network%interfaces(1)%history_count = 3
    snapshot%network%interfaces(1)%rx_history(:3) = [128.0_real64, 512.0_real64, 1536.0_real64]
    snapshot%network%interfaces(1)%tx_history(:3) = [64.0_real64, 256.0_real64, 512.0_real64]
    snapshot%network%connections(1)%valid = .true.
    snapshot%network%connections(1)%protocol = "tcp"
    snapshot%network%connections(1)%local_addr = "127.0.0.1"
    snapshot%network%connections(1)%local_port = 8080
    snapshot%network%connections(1)%remote_addr = "10.0.0.2"
    snapshot%network%connections(1)%remote_port = 443
    snapshot%network%connections(1)%state = "ESTABLISHED"
    snapshot%network%connections(1)%pid = 1234
    snapshot%network%connections(1)%process_name = "curl"
  end function sample_snapshot

  function network_table_snapshot() result(snapshot)
    type(collector_snapshot) :: snapshot

    snapshot = sample_snapshot()
    if (allocated(snapshot%network%connections)) deallocate(snapshot%network%connections)
    allocate(snapshot%network%connections(3))
    snapshot%network%connections(1)%valid = .true.
    snapshot%network%connections(1)%protocol = "tcp"
    snapshot%network%connections(1)%local_addr = "0.0.0.0"
    snapshot%network%connections(1)%local_port = 22
    snapshot%network%connections(1)%remote_addr = "0.0.0.0"
    snapshot%network%connections(1)%remote_port = 0
    snapshot%network%connections(1)%state = "LISTEN"
    snapshot%network%connections(1)%pid = 100
    snapshot%network%connections(1)%process_name = "sshd"

    snapshot%network%connections(2)%valid = .true.
    snapshot%network%connections(2)%protocol = "tcp"
    snapshot%network%connections(2)%local_addr = "127.0.0.1"
    snapshot%network%connections(2)%local_port = 8080
    snapshot%network%connections(2)%remote_addr = "10.0.0.2"
    snapshot%network%connections(2)%remote_port = 443
    snapshot%network%connections(2)%state = "ESTABLISHED"
    snapshot%network%connections(2)%pid = 1234
    snapshot%network%connections(2)%process_name = "curl"

    snapshot%network%connections(3)%valid = .true.
    snapshot%network%connections(3)%protocol = "udp"
    snapshot%network%connections(3)%local_addr = "192.0.2.10"
    snapshot%network%connections(3)%local_port = 53
    snapshot%network%connections(3)%remote_addr = "0.0.0.0"
    snapshot%network%connections(3)%remote_port = 0
    snapshot%network%connections(3)%state = "OPEN"
    snapshot%network%connections(3)%pid = 5353
    snapshot%network%connections(3)%process_name = "dnsmasq"
  end function network_table_snapshot

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
