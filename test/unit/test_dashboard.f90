program test_dashboard
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use fgof_screen, only : allocate_screen, clear_screen_style
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_collector, only : collector_snapshot
  use ftop_color, only : rgb, style_from_rgb
  use ftop_dashboard, only : dashboard_layout, default_dashboard_layout, render_dashboard
  use ftop_disk, only : disk_table_state
  use ftop_net_data, only : net_connection
  use ftop_network, only : NETWORK_SORT_LOCAL, NETWORK_SORT_PID, NETWORK_SORT_PROCESS, NETWORK_SORT_PROTOCOL, &
    network_table_quick_clear, network_table_quick_input, network_table_state, network_table_toggle_sort_direction, &
    render_network_panel
  use ftop_services, only : load_service_cache_from_text
  use ftop_table, only : TABLE_SORT_ASCENDING, table_sort_indicator
  use ftop_widgets, only : widget_rect
  implicit none

  integer(int64), parameter :: GIB = 1024_int64 * 1024_int64 * 1024_int64

  call load_service_cache_from_text(test_services_text())

  call test_default_layout()
  call test_dashboard_renders_metrics()
  call test_dashboard_scrolls_cpu_cores()
  call test_dashboard_renders_paused_indicator()
  call test_dashboard_renders_zoomed_widget()
  call test_dashboard_renders_active_disk_table()
  call test_dashboard_renders_zoomed_network_table()
  call test_network_table_column_sizing()
  call test_network_preview_uses_sort_state()
  call test_network_quick_jumps()
  call test_dashboard_handles_large_network_table()
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
    call require(layout%disk_panel%row == layout%cpu_panel%row, "wide layout should place disk beside network")
    call require(layout%disk_panel%col > layout%network_panel%col, "wide layout disk panel should be right of network")
    call require(layout%process_panel%row > layout%cpu_panel%row, "wide layout should place process below metrics")
    call require(layout%process_panel%height >= 8, "wide layout should reserve process table height")
    call require(layout%footer%row == 22, "wide layout footer should stay above bottom border")

    layout = default_dashboard_layout(80, 24)
    call require(layout%cpu_panel%col == layout%memory_panel%col, "narrow layout should stack panels")
    call require(layout%memory_panel%row > layout%cpu_panel%row, "narrow layout memory panel should follow cpu")
    call require(layout%memory_panel%width == layout%cpu_panel%width, "narrow panels should share width")
    call require(layout%network_panel%row > layout%memory_panel%row, "narrow layout network panel should follow memory")
    call require(layout%network_panel%width == layout%cpu_panel%width, "narrow network panel should share width")
    call require(layout%disk_panel%row > layout%network_panel%row, "narrow layout disk panel should follow network")
    call require(layout%disk_panel%width == layout%cpu_panel%width, "narrow disk panel should share width")
    call require(layout%process_panel%row > layout%disk_panel%row, "narrow layout process panel should follow disk")
    call require(layout%process_panel%width == layout%cpu_panel%width, "narrow process panel should share width")
    call require(layout%process_panel%row + layout%process_panel%height <= layout%footer%row, &
                 "narrow process panel should stay above footer")
  end subroutine test_default_layout

  subroutine test_dashboard_renders_metrics()
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot
    character(len=:), allocatable :: text

    snapshot = sample_snapshot()
    buffer = allocate_screen(160, 24)
    call render_dashboard(buffer, snapshot, 1000, 7, "ready")
    text = buffer_text(buffer)

    call require(index(text, "CPU") > 0, "dashboard should render cpu panel")
    call require(index(text, "Memory") > 0, "dashboard should render memory panel")
    call require(index(text, "Network") > 0, "dashboard should render network panel")
    call require(index(text, "Disk") > 0, "dashboard should render disk panel")
    call require(index(text, "Processes") > 0, "dashboard should render process panel by default")
    call require(index(text, "42.5%") > 0, "dashboard should render cpu usage")
    call require(index(text, "50.0%") > 0, "dashboard should render memory usage")
    call require(index(text, "8.0 GiB / 16.0 GiB") > 0, "dashboard should render memory bytes")
    call require(index(text, "cores 4 threads 8") > 0, "dashboard should render cpu topology")
    call require(index(text, "1m/5m/15m") > 0, "dashboard should label cpu load averages")
    call require(index(text, "Test CPU") > 0, "dashboard should render cpu model")
    call require(index(text, "uptime 1d 02:03:04") > 0, "dashboard should render uptime")
    call require(index(text, "c0") > 0, "dashboard should render core labels")
    call require(index(text, "headroom 8.0 GiB") > 0, "dashboard should render memory headroom")
    call require(index(text, "pressure high") > 0, "dashboard should render memory pressure")
    call require(index(text, "swap 1.0 GiB / 2.0 GiB") > 0, "dashboard should render swap")
    call require(index(text, "Interfaces 1") > 0, "dashboard should render network summary")
    call require(index(text, "eth0 ^ up") > 0, "dashboard should render network interface")
    call require(index(text, "Connections 1") > 0, "dashboard should render network connection count")
    call require(index(text, "127.0.0.1:8080") > 0, "dashboard should render network endpoint")
    call require(index(text, "8080(web)") > 0, "dashboard should render service names")
    call require(index(text, "KEYS") > 0, "dashboard should render footer keybar")
    call require(index(text, "STAT ready") > 0, "dashboard should render footer status")
    call require(index(text, "1000ms | samples 3") > 0, "dashboard should render compact timing")
    call require(index(text, "z/Enter zoom") > 0, "dashboard should render mode-aware key hints")
    call require(index(text, "ready") > 0, "dashboard should render status")

    call render_dashboard(buffer, snapshot, 1000, 7, "ready", layout_name="network")
    text = buffer_text(buffer)
    call require(index(text, "network") > 0, "dashboard should render active layout name")
  end subroutine test_dashboard_renders_metrics

  subroutine test_dashboard_scrolls_cpu_cores()
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot
    character(len=:), allocatable :: text

    snapshot = large_cpu_snapshot(8)
    buffer = allocate_screen(60, 16)
    call render_dashboard(buffer, snapshot, 1000, 1, "ready", focused_widget="cpu", zoomed=.true., &
                          cpu_core_scroll_offset=3)
    text = buffer_text(buffer)

    call require(index(text, "1m/5m/15m") > 0, "zoomed cpu should label load averages")
    call require(index(text, "c3") > 0, "cpu scroll offset should render requested first core")
    call require(index(text, "c0") == 0, "cpu scroll offset should hide earlier cores")
  end subroutine test_dashboard_scrolls_cpu_cores

  subroutine test_dashboard_renders_paused_indicator()
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot
    character(len=:), allocatable :: text

    snapshot = sample_snapshot()
    buffer = allocate_screen(80, 24)
    call render_dashboard(buffer, snapshot, 1000, 7, "Paused", paused=.true.)
    text = buffer_text(buffer)

    call require(index(text, "ftop PAUSED") > 0, "dashboard should render paused title indicator")
    call require(index(text, "Paused") > 0, "dashboard should render paused status")
  end subroutine test_dashboard_renders_paused_indicator

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
    call require(index(text, "memory zoom") > 0, "zoomed dashboard should render zoom status")
  end subroutine test_dashboard_renders_zoomed_widget

  subroutine test_dashboard_renders_active_disk_table()
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot
    type(disk_table_state) :: disk_state
    character(len=:), allocatable :: text

    snapshot = sample_snapshot()
    buffer = allocate_screen(120, 24)
    call render_dashboard(buffer, snapshot, 1000, 7, "disk-test", focused_widget="disk", &
                          disk_state=disk_state, disk_table_active=.true.)
    text = buffer_text(buffer)

    call require(index(text, "disk table") > 0, "active disk should render disk table status")
    call require(index(text, "Enter zoom") > 0, "active disk should render active-mode key hints")
    call require(disk_state%row_count == 2, "active disk should track filesystem rows")

    call render_dashboard(buffer, snapshot, 1000, 7, "disk-test", focused_widget="disk", zoomed=.true., &
                          disk_state=disk_state)
    text = buffer_text(buffer)
    call require(index(text, "MOUNT") > 0, "zoomed disk should render filesystem table")
    call require(index(text, "disk zoom") > 0, "zoomed disk should render zoom status")
  end subroutine test_dashboard_renders_active_disk_table

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
    call require(index(text, "sort state asc") > 0, "zoomed network should render active sort")
    call require(index(text, "curl") > 0, "zoomed network should render matching connection")
    call require(index(text, "443(https)") > 0, "zoomed network should render remote service names")
    call require(index(text, "sshd") == 0, "zoomed network should hide filtered listen connection")
    call require(index(text, "dnsmasq") == 0, "zoomed network should hide filtered udp connection")
    call require(network_state%row_count == 1, "network table state should track filtered rows")
    call require(network_state%total_row_count == 3, "network table state should track total rows")
    call require(network_state%viewport_rows > 0, "network table state should track viewport rows")

    call network_table_toggle_sort_direction(network_state)
    call render_dashboard(buffer, snapshot, 1000, 7, "network-test", focused_widget="network", zoomed=.true., &
                          network_state=network_state)
    text = buffer_text(buffer)
    call require(index(text, "sort state desc") > 0, "zoomed network should render flipped sort")
  end subroutine test_dashboard_renders_zoomed_network_table

  subroutine test_network_table_column_sizing()
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot
    type(network_table_state) :: network_state
    type(screen_style) :: style
    character(len=:), allocatable :: header
    character(len=:), allocatable :: protocol_header
    integer :: local_col
    integer :: remote_col

    snapshot = network_table_snapshot()
    network_state%sort_key = NETWORK_SORT_PROTOCOL
    style = clear_screen_style()
    buffer = allocate_screen(140, 16)
    call render_network_panel(buffer, widget_rect(1, 1, 140, 16), snapshot, style, style, style, network_state, &
                              expanded=.true.)
    header = first_row_containing(buffer, "PROTO")
    protocol_header = "PROTO " // table_sort_indicator(TABLE_SORT_ASCENDING)
    local_col = index(header, "LOCAL")
    remote_col = index(header, "REMOTE")

    call require(index(header, protocol_header) > 0, "network protocol header should keep active sort indicator")
    call require(local_col > 0 .and. remote_col > local_col, "network table should render local and remote headers")
    call require(remote_col - local_col <= len("192.0.2.10:53(domain)") + 1, &
                 "network local column should not expand beyond endpoint width")
  end subroutine test_network_table_column_sizing

  subroutine test_network_preview_uses_sort_state()
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot
    type(network_table_state) :: network_state
    type(screen_style) :: dim_style
    type(screen_style) :: title_style
    character(len=:), allocatable :: text

    title_style = style_from_rgb(fg=rgb(1, 2, 3))
    snapshot = network_table_snapshot()
    network_state%sort_key = NETWORK_SORT_PROCESS
    call network_table_toggle_sort_direction(network_state)
    buffer = allocate_screen(80, 12)
    call render_network_panel(buffer, widget_rect(1, 1, 80, 12), snapshot, dim_style, title_style, dim_style, &
                              network_state, expanded=.false.)
    text = buffer_text(buffer)

    call require(index(text, "Connections 3 sort process desc") > 0, &
                 "network preview should render active sort")
    call require(index(text, "sshd") > 0 .and. index(text, "dnsmasq") > 0 .and. index(text, "curl") > 0, &
                 "network preview should render connection rows")
    call require(index(text, "sshd") < index(text, "dnsmasq") .and. index(text, "dnsmasq") < index(text, "curl"), &
                 "network preview should sort connection rows")

    snapshot = large_network_table_snapshot(6)
    network_state = network_table_state()
    network_state%sort_key = NETWORK_SORT_PID
    network_state%selected_row = 4
    buffer = allocate_screen(80, 7)
    call render_network_panel(buffer, widget_rect(1, 1, 80, 7), snapshot, dim_style, title_style, dim_style, &
                              network_state, expanded=.false.)
    text = buffer_text(buffer)
    call require(index(text, "proc4") > 0, "network preview should render scrolled selected row")
    call require(index(text, "proc1") == 0, "network preview should not always render from first row")
    call require(text_cell_has_fg(buffer, "proc4"), "network preview should highlight selected row")
  end subroutine test_network_preview_uses_sort_state

  subroutine test_network_quick_jumps()
    type(collector_snapshot) :: snapshot
    type(network_table_state) :: network_state
    character(len=:), allocatable :: status
    logical :: handled

    snapshot = network_table_snapshot()
    call add_ipv6_connection(snapshot)
    network_state%sort_key = NETWORK_SORT_LOCAL

    handled = network_table_quick_input(snapshot, network_state, ":", status)
    call require(handled .and. index(status, "network port :") > 0, "network quick port should start on colon")
    handled = network_table_quick_input(snapshot, network_state, "8", status)
    call require(handled .and. network_state%selected_row == 2, "network quick port should match first digit")
    handled = network_table_quick_input(snapshot, network_state, "0", status)
    call require(handled .and. network_state%selected_row == 2, "network quick port should greedily refine digits")

    call network_table_quick_clear(network_state)
    handled = network_table_quick_input(snapshot, network_state, "1", status)
    call require(handled, "network quick ip should start on digit")
    handled = network_table_quick_input(snapshot, network_state, "9", status)
    handled = network_table_quick_input(snapshot, network_state, "2", status)
    handled = network_table_quick_input(snapshot, network_state, ".", status)
    call require(handled .and. network_state%selected_row == 3, "network quick ip should match IPv4 prefix")

    call network_table_quick_clear(network_state)
    handled = network_table_quick_input(snapshot, network_state, ":", status)
    handled = network_table_quick_input(snapshot, network_state, ":", status)
    handled = network_table_quick_input(snapshot, network_state, "1", status)
    call require(handled .and. network_state%selected_row == 4, "network quick ip should match IPv6 loopback")
  end subroutine test_network_quick_jumps

  subroutine test_dashboard_handles_large_network_table()
    integer, parameter :: connection_count = 2048
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot
    type(network_table_state) :: network_state
    character(len=:), allocatable :: text

    snapshot = large_network_table_snapshot(connection_count)
    buffer = allocate_screen(100, 28)
    call render_dashboard(buffer, snapshot, 1000, 7, "network-test", focused_widget="network", zoomed=.true., &
                          network_state=network_state)
    text = buffer_text(buffer)

    call require(index(text, "Connections 2048") > 0, "zoomed network should summarize large connection tables")
    call require(index(text, "PROTO") > 0, "zoomed network should render a large connection table")
    call require(network_state%row_count == connection_count, "network table state should track large visible rows")
    call require(network_state%total_row_count == connection_count, "network table state should track large total rows")
    call require(network_state%viewport_rows > 0, "network table state should keep a viewport for large tables")
  end subroutine test_dashboard_handles_large_network_table

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
    snapshot%memory%used_bytes = 8_int64 * GIB
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

    snapshot%disk%valid = .true.
    allocate(snapshot%disk%filesystems(2))
    snapshot%disk%filesystems(1)%valid = .true.
    snapshot%disk%filesystems(1)%device = "/dev/ada0p2"
    snapshot%disk%filesystems(1)%mountpoint = "/"
    snapshot%disk%filesystems(1)%fstype = "ufs"
    snapshot%disk%filesystems(1)%total_bytes = 100_int64 * GIB
    snapshot%disk%filesystems(1)%used_bytes = 72_int64 * GIB
    snapshot%disk%filesystems(1)%available_bytes = 28_int64 * GIB
    snapshot%disk%filesystems(2)%valid = .true.
    snapshot%disk%filesystems(2)%device = "zroot/home"
    snapshot%disk%filesystems(2)%mountpoint = "/home"
    snapshot%disk%filesystems(2)%fstype = "zfs"
    snapshot%disk%filesystems(2)%total_bytes = 200_int64 * GIB
    snapshot%disk%filesystems(2)%used_bytes = 90_int64 * GIB
    snapshot%disk%filesystems(2)%available_bytes = 110_int64 * GIB
  end function sample_snapshot

  function large_cpu_snapshot(core_count) result(snapshot)
    integer, intent(in) :: core_count
    type(collector_snapshot) :: snapshot
    integer :: core_index

    snapshot = sample_snapshot()
    if (allocated(snapshot%cpu_cores)) deallocate(snapshot%cpu_cores)
    if (allocated(snapshot%cpu_core_usage_history)) deallocate(snapshot%cpu_core_usage_history)

    snapshot%cpu_total%core_count = core_count
    snapshot%cpu_total%thread_count = core_count
    allocate(snapshot%cpu_cores(core_count))
    allocate(snapshot%cpu_core_usage_history(core_count, 4))
    do core_index = 1, core_count
      snapshot%cpu_cores(core_index)%valid = .true.
      snapshot%cpu_cores(core_index)%usage_percent = real(core_index * 10, real64)
      snapshot%cpu_core_usage_history(core_index, :) = real(core_index * 10, real64)
    end do
  end function large_cpu_snapshot

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

  subroutine add_ipv6_connection(snapshot)
    type(collector_snapshot), intent(inout) :: snapshot
    type(net_connection), allocatable :: connections(:)
    integer :: old_count

    old_count = 0
    if (allocated(snapshot%network%connections)) old_count = size(snapshot%network%connections)
    allocate(connections(old_count + 1))
    if (old_count > 0) connections(:old_count) = snapshot%network%connections
    connections(old_count + 1)%valid = .true.
    connections(old_count + 1)%protocol = "tcp"
    connections(old_count + 1)%local_addr = "2001:db8::1"
    connections(old_count + 1)%local_port = 8443
    connections(old_count + 1)%remote_addr = "::1"
    connections(old_count + 1)%remote_port = 443
    connections(old_count + 1)%state = "ESTABLISHED"
    connections(old_count + 1)%pid = 8443
    connections(old_count + 1)%process_name = "ipv6d"
    call move_alloc(connections, snapshot%network%connections)
  end subroutine add_ipv6_connection

  function large_network_table_snapshot(connection_count) result(snapshot)
    integer, intent(in) :: connection_count
    type(collector_snapshot) :: snapshot
    integer :: connection_index

    snapshot = sample_snapshot()
    if (allocated(snapshot%network%connections)) deallocate(snapshot%network%connections)
    allocate(snapshot%network%connections(connection_count))

    do connection_index = 1, connection_count
      snapshot%network%connections(connection_index)%valid = .true.
      if (mod(connection_index, 2) == 0) then
        snapshot%network%connections(connection_index)%protocol = "tcp"
        snapshot%network%connections(connection_index)%remote_port = 443
        snapshot%network%connections(connection_index)%state = "ESTABLISHED"
      else
        snapshot%network%connections(connection_index)%protocol = "udp"
        snapshot%network%connections(connection_index)%remote_port = 53
        snapshot%network%connections(connection_index)%state = "OPEN"
      end if
      snapshot%network%connections(connection_index)%local_addr = "127.0.0.1"
      snapshot%network%connections(connection_index)%local_port = 10000 + mod(connection_index, 40000)
      snapshot%network%connections(connection_index)%remote_addr = "10.0.0.2"
      snapshot%network%connections(connection_index)%pid = 1000 + connection_index
      write(snapshot%network%connections(connection_index)%process_name, '("proc", I0)') connection_index
    end do
  end function large_network_table_snapshot

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

  function first_row_containing(buffer, needle) result(text)
    type(screen_buffer), intent(in) :: buffer
    character(len=*), intent(in) :: needle
    character(len=:), allocatable :: text
    integer :: row

    text = ""
    do row = 1, buffer%size%height
      text = row_text(buffer, row)
      if (index(text, needle) > 0) return
    end do
  end function first_row_containing

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

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

end program test_dashboard
