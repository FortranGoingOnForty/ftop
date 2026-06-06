program test_dashboard_snapshots
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use fgof_screen, only : allocate_screen
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_collector, only : collector_snapshot
  use ftop_color, only : &
    COLOR_UI_ACCENT, &
    COLOR_UI_BORDER, &
    COLOR_UI_DIM, &
    COLOR_UI_PANEL, &
    style_from_rgb
  use ftop_cpu, only : render_cpu_panel
  use ftop_dashboard, only : render_dashboard
  use ftop_gpu, only : render_gpu_panel
  use ftop_memory, only : render_memory_panel
  use ftop_network, only : render_network_panel
  use ftop_services, only : load_service_cache_from_text
  use ftop_widgets, only : widget_rect
  implicit none

  integer, parameter :: CPU_PANEL_HEIGHT = 14
  integer, parameter :: MEMORY_PANEL_HEIGHT = 12
  integer, parameter :: NETWORK_PANEL_HEIGHT = 10
  integer, parameter :: GPU_PANEL_HEIGHT = 8
  integer, parameter :: PANEL_WIDTH = 44
  integer, parameter :: GRID_HEIGHT = 24
  integer, parameter :: GRID_WIDTH = 72
  integer(int64), parameter :: GIB = 1024_int64 * 1024_int64 * 1024_int64

  character(len=512) :: command
  character(len=512) :: golden_root
  logical :: print_snapshots

  call get_command_argument(1, golden_root)
  call get_command_argument(2, command)
  call require(len_trim(golden_root) > 0, "missing dashboard snapshot golden directory")
  print_snapshots = trim(command) == "--print"
  call load_service_cache_from_text(test_services_text())

  call compare_snapshot("cpu_panel", render_cpu_snapshot(), golden_file(golden_root, "dashboard_cpu_panel.txt"), &
                        CPU_PANEL_HEIGHT, print_snapshots)
  call compare_snapshot("memory_panel", render_memory_snapshot(), golden_file(golden_root, "dashboard_memory_panel.txt"), &
                        MEMORY_PANEL_HEIGHT, print_snapshots)
  call compare_snapshot("network_panel", render_network_snapshot(), golden_file(golden_root, "dashboard_network_panel.txt"), &
                        NETWORK_PANEL_HEIGHT, print_snapshots)
  call compare_snapshot("gpu_panel", render_gpu_snapshot(), golden_file(golden_root, "dashboard_gpu_panel.txt"), &
                        GPU_PANEL_HEIGHT, print_snapshots)
  call compare_snapshot("grid", render_grid_snapshot(), golden_file(golden_root, "dashboard_grid.txt"), &
                        GRID_HEIGHT, print_snapshots)

contains

  function render_cpu_snapshot() result(buffer)
    type(screen_buffer) :: buffer
    type(screen_style) :: border_style
    type(screen_style) :: dim_style
    type(screen_style) :: title_style

    call dashboard_styles(border_style, title_style, dim_style)
    buffer = allocate_screen(PANEL_WIDTH, CPU_PANEL_HEIGHT)
    call render_cpu_panel(buffer, widget_rect(1, 1, PANEL_WIDTH, CPU_PANEL_HEIGHT), sample_snapshot(), &
                          border_style, title_style, dim_style)
  end function render_cpu_snapshot

  function render_memory_snapshot() result(buffer)
    type(screen_buffer) :: buffer
    type(screen_style) :: border_style
    type(screen_style) :: dim_style
    type(screen_style) :: title_style

    call dashboard_styles(border_style, title_style, dim_style)
    buffer = allocate_screen(PANEL_WIDTH, MEMORY_PANEL_HEIGHT)
    call render_memory_panel(buffer, widget_rect(1, 1, PANEL_WIDTH, MEMORY_PANEL_HEIGHT), sample_snapshot(), &
                             border_style, title_style, dim_style)
  end function render_memory_snapshot

  function render_network_snapshot() result(buffer)
    type(screen_buffer) :: buffer
    type(screen_style) :: border_style
    type(screen_style) :: dim_style
    type(screen_style) :: title_style

    call dashboard_styles(border_style, title_style, dim_style)
    buffer = allocate_screen(PANEL_WIDTH, NETWORK_PANEL_HEIGHT)
    call render_network_panel(buffer, widget_rect(1, 1, PANEL_WIDTH, NETWORK_PANEL_HEIGHT), sample_snapshot(), &
                              border_style, title_style, dim_style)
  end function render_network_snapshot

  function render_gpu_snapshot() result(buffer)
    type(screen_buffer) :: buffer
    type(screen_style) :: border_style
    type(screen_style) :: dim_style
    type(screen_style) :: title_style

    call dashboard_styles(border_style, title_style, dim_style)
    buffer = allocate_screen(PANEL_WIDTH, GPU_PANEL_HEIGHT)
    call render_gpu_panel(buffer, widget_rect(1, 1, PANEL_WIDTH, GPU_PANEL_HEIGHT), sample_snapshot(), &
                          border_style, title_style, dim_style)
  end function render_gpu_snapshot

  function render_grid_snapshot() result(buffer)
    type(screen_buffer) :: buffer

    buffer = allocate_screen(GRID_WIDTH, GRID_HEIGHT)
    call render_dashboard(buffer, sample_snapshot(), 1000, 7, "ready", focused_widget="cpu", render_fps=1.0)
  end function render_grid_snapshot

  subroutine dashboard_styles(border_style, title_style, dim_style)
    type(screen_style), intent(out) :: border_style
    type(screen_style), intent(out) :: title_style
    type(screen_style), intent(out) :: dim_style

    border_style = style_from_rgb(fg=COLOR_UI_BORDER)
    title_style = style_from_rgb(fg=COLOR_UI_ACCENT, bg=COLOR_UI_PANEL, bold=.true.)
    dim_style = style_from_rgb(fg=COLOR_UI_DIM)
  end subroutine dashboard_styles

  function test_services_text() result(text)
    character(len=:), allocatable :: text

    text = "https 443/tcp"
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
    allocate(snapshot%network%interfaces(2))
    allocate(snapshot%network%connections(1))
    allocate(snapshot%network%processes(0))
    snapshot%network%interfaces(1)%valid = .true.
    snapshot%network%interfaces(1)%name = "eth0"
    snapshot%network%interfaces(1)%state = "up"
    snapshot%network%interfaces(1)%rx_bytes_per_sec = 1536.0_real64
    snapshot%network%interfaces(1)%tx_bytes_per_sec = 2.0_real64 * 1024.0_real64 * 1024.0_real64
    snapshot%network%interfaces(1)%history_count = 4
    snapshot%network%interfaces(1)%rx_history(:4) = [128.0_real64, 512.0_real64, 1024.0_real64, 1536.0_real64]
    snapshot%network%interfaces(1)%tx_history(:4) = [256.0_real64, 1024.0_real64, 4096.0_real64, 8192.0_real64]
    snapshot%network%interfaces(2)%valid = .true.
    snapshot%network%interfaces(2)%name = "wlan0"
    snapshot%network%interfaces(2)%state = "down"
    snapshot%network%interfaces(2)%rx_bytes_per_sec = 0.0_real64
    snapshot%network%interfaces(2)%tx_bytes_per_sec = 512.0_real64
    snapshot%network%interfaces(2)%history_count = 4
    snapshot%network%interfaces(2)%rx_history(:4) = [0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64]
    snapshot%network%interfaces(2)%tx_history(:4) = [0.0_real64, 128.0_real64, 256.0_real64, 512.0_real64]
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
  end function sample_snapshot

  function golden_file(root, name) result(path)
    character(len=*), intent(in) :: root
    character(len=*), intent(in) :: name
    character(len=:), allocatable :: path

    path = trim(root) // "/" // name
  end function golden_file

  subroutine compare_snapshot(name, buffer, path, rows, print_snapshot)
    character(len=*), intent(in) :: name
    type(screen_buffer), intent(in) :: buffer
    character(len=*), intent(in) :: path
    integer, intent(in) :: rows
    logical, intent(in) :: print_snapshot

    if (print_snapshot) then
      call print_buffer(name, buffer, rows)
      return
    end if
    call compare_golden(buffer, path, rows, name)
  end subroutine compare_snapshot

  subroutine compare_golden(buffer, path, rows, name)
    type(screen_buffer), intent(in) :: buffer
    character(len=*), intent(in) :: path
    integer, intent(in) :: rows
    character(len=*), intent(in) :: name
    character(len=512) :: expected
    integer :: io_status
    integer :: row
    integer :: unit

    open(newunit=unit, file=path, status="old", action="read", iostat=io_status)
    call require(io_status == 0, "failed to open dashboard snapshot golden file")

    do row = 1, rows
      read(unit, '(a)', iostat=io_status) expected
      call require(io_status == 0, "failed to read dashboard snapshot golden row")
      call require(trim(row_text(buffer, row)) == trim(expected), &
                   "dashboard snapshot " // name // " row " // integer_text(row) // " mismatch")
    end do

    read(unit, '(a)', iostat=io_status) expected
    call require(io_status /= 0, "dashboard snapshot golden file has extra rows")
    close(unit)
  end subroutine compare_golden

  subroutine print_buffer(name, buffer, rows)
    character(len=*), intent(in) :: name
    type(screen_buffer), intent(in) :: buffer
    integer, intent(in) :: rows
    integer :: row

    write(*, '(a)') "== " // name // " =="
    do row = 1, rows
      write(*, '(a)') trim(row_text(buffer, row))
    end do
  end subroutine print_buffer

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

end program test_dashboard_snapshots
