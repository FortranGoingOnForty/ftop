program test_disk
  use, intrinsic :: iso_c_binding, only : c_char, c_int, c_long_long, c_null_char
  use, intrinsic :: iso_fortran_env, only : int64
  use fgof_screen, only : allocate_screen, clear_screen_style
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_collector, only : collector_snapshot
  use ftop_disk, only : disk_table_state, disk_table_status, render_disk_panel
  use ftop_disk_data, only : c_filesystem_info, disk_table_from_c, filesystem_usage_percent
  use ftop_widgets, only : widget_rect
  implicit none

  integer(int64), parameter :: GIB = 1024_int64 * 1024_int64 * 1024_int64

  call test_disk_usage_percent()
  call test_disk_table_filters_pseudo_filesystems()
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
    call require(index(text, "/var") > 0, "disk compact panel should sort hottest filesystem first")
    call require(index(text, "/var") < index(text, "/home"), "disk compact rows should sort by usage")
    compact_row = row_text(buffer, 4)
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

end program test_disk
