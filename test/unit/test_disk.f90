program test_disk
  use, intrinsic :: iso_c_binding, only : c_char, c_int, c_long_long, c_null_char
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use fgof_screen, only : allocate_screen, clear_screen_style
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_collector, only : collector_snapshot
  use ftop_disk, only : disk_table_state, disk_table_status, render_disk_panel
  use ftop_disk_data, only : assign_disk_io_metrics, c_filesystem_info, disk_io_info, disk_io_latency_from_delta, &
    disk_io_rate_from_delta, disk_io_rate_info, disk_latency_info, disk_table, disk_table_from_c, filesystem_usage_percent
  use ftop_widgets, only : widget_rect
  implicit none

  integer(int64), parameter :: GIB = 1024_int64 * 1024_int64 * 1024_int64

  call test_disk_usage_percent()
  call test_disk_table_filters_pseudo_filesystems()
  call test_disk_io_rates_from_delta()
  call test_disk_io_latency_from_delta()
  call test_assign_disk_io_metrics()
  call test_disk_io_delta_rejects_mismatched_samples()
  call test_disk_panel_renders_compact_and_expanded()

contains

  subroutine test_disk_usage_percent()
    type(collector_snapshot) :: snapshot

    call fill_disk_snapshot(snapshot)
    call require(abs(filesystem_usage_percent(snapshot%disk%filesystems(1)) - 90.0d0) < 0.01d0, &
                 "filesystem usage percent mismatch")
  end subroutine test_disk_usage_percent

  subroutine test_disk_table_filters_pseudo_filesystems()
    type(c_filesystem_info) :: raw(2)
    type(collector_snapshot) :: snapshot

    raw = c_filesystem_info()
    raw(1)%valid = 1_c_int
    call put_c_text(raw(1)%device, "/dev/ada0p2")
    call put_c_text(raw(1)%mountpoint, "/")
    call put_c_text(raw(1)%fstype, "ufs")
    raw(1)%total_bytes = 1000_c_long_long
    raw(1)%used_bytes = 500_c_long_long
    raw(1)%available_bytes = 500_c_long_long
    raw(2)%valid = 1_c_int
    call put_c_text(raw(2)%device, "devfs")
    call put_c_text(raw(2)%mountpoint, "/dev")
    call put_c_text(raw(2)%fstype, "devfs")
    raw(2)%total_bytes = 1000_c_long_long
    raw(2)%used_bytes = 10_c_long_long
    raw(2)%available_bytes = 990_c_long_long

    snapshot%disk = disk_table_from_c(raw, 2)
    call require(snapshot%disk%valid, "disk table from C should be valid")
    call require(size(snapshot%disk%filesystems) == 1, "disk table should filter pseudo filesystems")
    call require(trim(snapshot%disk%filesystems(1)%mountpoint) == "/", "disk table should keep real filesystem")
  end subroutine test_disk_table_filters_pseudo_filesystems

  subroutine test_disk_io_rates_from_delta()
    type(disk_io_info) :: previous
    type(disk_io_info) :: current
    type(disk_io_rate_info) :: rate

    previous = disk_io_sample("nvme0n1", 1000_int64, 2000_int64, 10_int64, 20_int64, 100_int64, 200_int64, 50_int64)
    current = disk_io_sample("nvme0n1", 3000_int64, 7000_int64, 30_int64, 50_int64, 160_int64, 260_int64, 125_int64)
    rate = disk_io_rate_from_delta(previous, current, 500_int64)

    call require(rate%valid, "disk rate should be valid")
    call require_close(rate%read_bytes_per_sec, 4000.0_real64, "disk read rate mismatch")
    call require_close(rate%write_bytes_per_sec, 10000.0_real64, "disk write rate mismatch")
    call require_close(rate%read_ops_per_sec, 40.0_real64, "disk read ops rate mismatch")
    call require_close(rate%write_ops_per_sec, 60.0_real64, "disk write ops rate mismatch")
    call require_close(rate%busy_percent, 15.0_real64, "disk busy percent mismatch")
  end subroutine test_disk_io_rates_from_delta

  subroutine test_disk_io_latency_from_delta()
    type(disk_io_info) :: previous
    type(disk_io_info) :: current
    type(disk_latency_info) :: latency

    previous = disk_io_sample("sda", 0_int64, 0_int64, 10_int64, 20_int64, 100_int64, 200_int64, 0_int64)
    current = disk_io_sample("sda", 0_int64, 0_int64, 14_int64, 25_int64, 116_int64, 230_int64, 0_int64)
    latency = disk_io_latency_from_delta(previous, current)

    call require(latency%valid, "disk latency should be valid")
    call require(latency%read_valid, "disk read latency should be valid")
    call require(latency%write_valid, "disk write latency should be valid")
    call require_close(latency%avg_read_latency_us, 4000.0_real64, "disk read latency mismatch")
    call require_close(latency%avg_write_latency_us, 6000.0_real64, "disk write latency mismatch")
  end subroutine test_disk_io_latency_from_delta

  subroutine test_assign_disk_io_metrics()
    type(disk_table) :: previous
    type(disk_table) :: current

    previous%valid = .true.
    allocate(previous%io(1))
    previous%io(1) = disk_io_sample("sda", 1000_int64, 2000_int64, 10_int64, 20_int64, 100_int64, 200_int64, 50_int64)
    current%valid = .true.
    allocate(current%io(2))
    current%io(1) = disk_io_sample("sda", 3000_int64, 5000_int64, 30_int64, 40_int64, 160_int64, 260_int64, 80_int64)
    current%io(2) = disk_io_sample("sdb", 100_int64, 200_int64, 1_int64, 2_int64, 3_int64, 4_int64, 5_int64)

    call assign_disk_io_metrics(current, previous, 1000_int64)
    call require(allocated(current%io_rates), "disk table should allocate IO rates")
    call require(allocated(current%latencies), "disk table should allocate latencies")
    call require(size(current%io_rates) == 2, "disk table IO rate count mismatch")
    call require(size(current%latencies) == 2, "disk table latency count mismatch")
    call require(current%io_rates(1)%valid, "matching disk IO rate should be valid")
    call require_close(current%io_rates(1)%read_bytes_per_sec, 2000.0_real64, "assigned disk read rate mismatch")
    call require(current%latencies(1)%valid, "matching disk latency should be valid")
    call require_close(current%latencies(1)%avg_read_latency_us, 3000.0_real64, "assigned disk read latency mismatch")
    call require(.not. current%io_rates(2)%valid, "unmatched disk IO rate should be invalid")
    call require(trim(current%io_rates(2)%device) == "sdb", "unmatched disk IO rate should keep device name")
    call require(.not. current%latencies(2)%valid, "unmatched disk latency should be invalid")
    call require(trim(current%latencies(2)%device) == "sdb", "unmatched disk latency should keep device name")
  end subroutine test_assign_disk_io_metrics

  subroutine test_disk_io_delta_rejects_mismatched_samples()
    type(disk_io_info) :: previous
    type(disk_io_info) :: current
    type(disk_io_rate_info) :: rate
    type(disk_latency_info) :: latency

    previous = disk_io_sample("sda", 100_int64, 100_int64, 10_int64, 10_int64, 10_int64, 10_int64, 10_int64)
    current = disk_io_sample("sdb", 200_int64, 200_int64, 20_int64, 20_int64, 20_int64, 20_int64, 20_int64)
    rate = disk_io_rate_from_delta(previous, current, 1000_int64)
    latency = disk_io_latency_from_delta(previous, current)
    call require(.not. rate%valid, "mismatched disk rate samples should be invalid")
    call require(.not. latency%valid, "mismatched disk latency samples should be invalid")

    current = disk_io_sample("sda", 90_int64, 200_int64, 20_int64, 20_int64, 20_int64, 20_int64, 20_int64)
    rate = disk_io_rate_from_delta(previous, current, 1000_int64)
    latency = disk_io_latency_from_delta(previous, current)
    call require(.not. rate%valid, "regressed disk rate samples should be invalid")
    call require(.not. latency%valid, "regressed disk latency samples should be invalid")
  end subroutine test_disk_io_delta_rejects_mismatched_samples

  subroutine test_disk_panel_renders_compact_and_expanded()
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot
    type(disk_table_state) :: state
    type(screen_style) :: style
    character(len=:), allocatable :: compact_row
    character(len=:), allocatable :: text

    call fill_disk_snapshot(snapshot)
    style = clear_screen_style()
    buffer = allocate_screen(72, 10)
    call render_disk_panel(buffer, widget_rect(1, 1, 72, 10), snapshot, style, style, style, state, expanded=.false.)
    text = buffer_text(buffer)
    call require(index(text, "Disk") > 0, "disk panel should render title")
    call require(index(text, "Filesystems 2") > 0, "disk panel should summarize filesystems")
    call require(index(text, "I/O ada0") > 0, "disk compact panel should render IO device")
    call require(index(text, "R: 2.0 MiB/s") > 0, "disk compact panel should render read throughput")
    call require(index(text, "W: 512.0 KiB/s") > 0, "disk compact panel should render write throughput")
    call require(index(text, "/var") > 0, "disk compact panel should sort hottest filesystem first")
    call require(index(text, "/var") < index(text, "/home"), "disk compact rows should sort by usage")
    compact_row = row_text(buffer, 5)
    call require(index(compact_row, "/var") > 0, "disk compact row should render mountpoint")
    call require(index(compact_row, "90.0%") > 0, "disk compact row should render usage percent")
    call require(index(compact_row, "90.0%") - index(compact_row, "/var") >= 8, &
                 "disk compact row should separate mountpoint and percent")
    call require(index(compact_row, "GiB") == 0, "disk compact row should omit byte totals")
    call require(index(disk_table_status(state), "disk row") > 0, "disk table state should produce status")

    buffer = allocate_screen(80, 12)
    call render_disk_panel(buffer, widget_rect(1, 1, 80, 12), snapshot, style, style, style, state, expanded=.true.)
    text = buffer_text(buffer)
    call require(index(text, "MOUNT") > 0, "expanded disk panel should render table header")
    call require(index(text, "AVAIL") > 0, "expanded disk panel should render available column")
    call require(index(text, "90.0%") > 0, "expanded disk panel should render usage percent")
    call require(index(text, "rlat 1.5 ms") > 0, "expanded disk panel should render read latency")
    call require(index(text, "wlat 3.0 ms") > 0, "expanded disk panel should render write latency")
  end subroutine test_disk_panel_renders_compact_and_expanded

  subroutine fill_disk_snapshot(snapshot)
    type(collector_snapshot), intent(out) :: snapshot

    snapshot%disk%valid = .true.
    allocate(snapshot%disk%filesystems(2))
    snapshot%disk%filesystems(1)%valid = .true.
    snapshot%disk%filesystems(1)%device = "/dev/ada0p3"
    snapshot%disk%filesystems(1)%mountpoint = "/var"
    snapshot%disk%filesystems(1)%fstype = "ufs"
    snapshot%disk%filesystems(1)%total_bytes = 10_int64 * GIB
    snapshot%disk%filesystems(1)%used_bytes = 9_int64 * GIB
    snapshot%disk%filesystems(1)%available_bytes = 1_int64 * GIB
    snapshot%disk%filesystems(2)%valid = .true.
    snapshot%disk%filesystems(2)%device = "zroot/home"
    snapshot%disk%filesystems(2)%mountpoint = "/home"
    snapshot%disk%filesystems(2)%fstype = "zfs"
    snapshot%disk%filesystems(2)%total_bytes = 100_int64 * GIB
    snapshot%disk%filesystems(2)%used_bytes = 25_int64 * GIB
    snapshot%disk%filesystems(2)%available_bytes = 75_int64 * GIB
    allocate(snapshot%disk%io(1))
    snapshot%disk%io(1) = disk_io_sample("ada0", 1000_int64, 2000_int64, 10_int64, 20_int64, &
                                         100_int64, 200_int64, 50_int64)
    allocate(snapshot%disk%io_rates(1))
    snapshot%disk%io_rates(1) = disk_io_rate_info()
    snapshot%disk%io_rates(1)%valid = .true.
    snapshot%disk%io_rates(1)%device = "ada0"
    snapshot%disk%io_rates(1)%read_bytes_per_sec = 2.0_real64 * 1024.0_real64 * 1024.0_real64
    snapshot%disk%io_rates(1)%write_bytes_per_sec = 512.0_real64 * 1024.0_real64
    allocate(snapshot%disk%latencies(1))
    snapshot%disk%latencies(1) = disk_latency_info()
    snapshot%disk%latencies(1)%valid = .true.
    snapshot%disk%latencies(1)%read_valid = .true.
    snapshot%disk%latencies(1)%write_valid = .true.
    snapshot%disk%latencies(1)%device = "ada0"
    snapshot%disk%latencies(1)%avg_read_latency_us = 1500.0_real64
    snapshot%disk%latencies(1)%avg_write_latency_us = 3000.0_real64
  end subroutine fill_disk_snapshot

  subroutine put_c_text(chars, text)
    character(kind=c_char), intent(out) :: chars(:)
    character(len=*), intent(in) :: text
    integer :: copied
    integer :: index

    chars = c_null_char
    copied = min(len_trim(text), size(chars) - 1)
    do index = 1, copied
      chars(index) = char(iachar(text(index:index)), kind=c_char)
    end do
  end subroutine put_c_text

  function disk_io_sample(device, read_bytes, write_bytes, read_ops, write_ops, read_time_ms, write_time_ms, &
                          io_time_ms) result(sample)
    character(len=*), intent(in) :: device
    integer(int64), intent(in) :: read_bytes
    integer(int64), intent(in) :: write_bytes
    integer(int64), intent(in) :: read_ops
    integer(int64), intent(in) :: write_ops
    integer(int64), intent(in) :: read_time_ms
    integer(int64), intent(in) :: write_time_ms
    integer(int64), intent(in) :: io_time_ms
    type(disk_io_info) :: sample

    sample = disk_io_info()
    sample%valid = .true.
    sample%device = device
    sample%read_bytes = read_bytes
    sample%write_bytes = write_bytes
    sample%read_ops = read_ops
    sample%write_ops = write_ops
    sample%read_time_ms = read_time_ms
    sample%write_time_ms = write_time_ms
    sample%io_time_ms = io_time_ms
    sample%weighted_io_time_ms = io_time_ms
  end function disk_io_sample

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

  subroutine require_close(actual, expected, message)
    real(real64), intent(in) :: actual
    real(real64), intent(in) :: expected
    character(len=*), intent(in) :: message

    if (abs(actual - expected) > 0.01_real64) error stop message
  end subroutine require_close

end program test_disk
