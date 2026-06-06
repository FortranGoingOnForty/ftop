module ftop_disk_data
  use, intrinsic :: iso_c_binding, only : c_char, c_int, c_long_long, c_null_char
  use, intrinsic :: iso_fortran_env, only : int64, real64
  implicit none
  private

  integer, parameter, public :: DISK_DEVICE_LEN = 64
  integer, parameter, public :: DISK_MOUNTPOINT_LEN = 128
  integer, parameter, public :: DISK_FSTYPE_LEN = 32
  integer, parameter, public :: DISK_FILESYSTEM_CAPACITY = 128
  integer, parameter, public :: DISK_IO_CAPACITY = 256

  type, bind(C), public :: c_filesystem_info
    integer(c_int) :: valid = 0_c_int
    character(kind=c_char) :: device(DISK_DEVICE_LEN) = c_null_char
    character(kind=c_char) :: mountpoint(DISK_MOUNTPOINT_LEN) = c_null_char
    character(kind=c_char) :: fstype(DISK_FSTYPE_LEN) = c_null_char
    integer(c_long_long) :: total_bytes = 0_c_long_long
    integer(c_long_long) :: used_bytes = 0_c_long_long
    integer(c_long_long) :: available_bytes = 0_c_long_long
  end type c_filesystem_info

  type, public :: filesystem_info
    logical :: valid = .false.
    character(len=DISK_DEVICE_LEN) :: device = ""
    character(len=DISK_MOUNTPOINT_LEN) :: mountpoint = ""
    character(len=DISK_FSTYPE_LEN) :: fstype = ""
    integer(int64) :: total_bytes = 0_int64
    integer(int64) :: used_bytes = 0_int64
    integer(int64) :: available_bytes = 0_int64
  end type filesystem_info

  type, public :: disk_io_info
    logical :: valid = .false.
    character(len=DISK_DEVICE_LEN) :: device = ""
    integer(int64) :: read_bytes = 0_int64
    integer(int64) :: write_bytes = 0_int64
    integer(int64) :: read_ops = 0_int64
    integer(int64) :: write_ops = 0_int64
    integer(int64) :: reads_merged = 0_int64
    integer(int64) :: writes_merged = 0_int64
    integer(int64) :: read_time_ms = 0_int64
    integer(int64) :: write_time_ms = 0_int64
    integer(int64) :: ios_in_progress = 0_int64
    integer(int64) :: io_time_ms = 0_int64
    integer(int64) :: weighted_io_time_ms = 0_int64
    integer(int64) :: sector_size_bytes = 512_int64
  end type disk_io_info

  type, public :: disk_io_rate_info
    logical :: valid = .false.
    character(len=DISK_DEVICE_LEN) :: device = ""
    real(real64) :: read_bytes_per_sec = 0.0_real64
    real(real64) :: write_bytes_per_sec = 0.0_real64
    real(real64) :: read_ops_per_sec = 0.0_real64
    real(real64) :: write_ops_per_sec = 0.0_real64
    real(real64) :: busy_percent = 0.0_real64
  end type disk_io_rate_info

  type, public :: disk_latency_info
    logical :: valid = .false.
    logical :: read_valid = .false.
    logical :: write_valid = .false.
    character(len=DISK_DEVICE_LEN) :: device = ""
    real(real64) :: avg_read_latency_us = 0.0_real64
    real(real64) :: avg_write_latency_us = 0.0_real64
    real(real64) :: p99_latency_us = 0.0_real64
  end type disk_latency_info

  type, public :: disk_table
    logical :: valid = .false.
    type(filesystem_info), allocatable :: filesystems(:)
    type(disk_io_info), allocatable :: io(:)
  end type disk_table

  public :: disk_table_from_c
  public :: disk_io_latency_from_delta
  public :: disk_io_rate_from_delta
  public :: filesystem_usage_percent
  public :: filesystem_visible
  public :: real_filesystem_count

contains

  function disk_table_from_c(raw, raw_count) result(table)
    type(c_filesystem_info), intent(in) :: raw(:)
    integer, intent(in) :: raw_count
    type(disk_table) :: table
    type(filesystem_info) :: filesystem
    integer :: filesystem_index
    integer :: output_index
    integer :: visible_count

    table%valid = .true.
    visible_count = 0
    do filesystem_index = 1, min(max(0, raw_count), size(raw))
      filesystem = filesystem_from_c(raw(filesystem_index))
      if (filesystem_visible(filesystem)) visible_count = visible_count + 1
    end do

    allocate(table%filesystems(visible_count))
    output_index = 0
    do filesystem_index = 1, min(max(0, raw_count), size(raw))
      filesystem = filesystem_from_c(raw(filesystem_index))
      if (.not. filesystem_visible(filesystem)) cycle
      output_index = output_index + 1
      table%filesystems(output_index) = filesystem
    end do
  end function disk_table_from_c

  function filesystem_from_c(raw) result(filesystem)
    type(c_filesystem_info), intent(in) :: raw
    type(filesystem_info) :: filesystem

    filesystem%valid = raw%valid /= 0_c_int
    filesystem%device = c_chars_to_text(raw%device)
    filesystem%mountpoint = c_chars_to_text(raw%mountpoint)
    filesystem%fstype = c_chars_to_text(raw%fstype)
    filesystem%total_bytes = max(0_int64, int(raw%total_bytes, int64))
    filesystem%used_bytes = bounded_bytes(int(raw%used_bytes, int64), filesystem%total_bytes)
    filesystem%available_bytes = bounded_bytes(int(raw%available_bytes, int64), filesystem%total_bytes)
  end function filesystem_from_c

  logical function filesystem_visible(filesystem) result(visible)
    type(filesystem_info), intent(in) :: filesystem

    visible = .false.
    if (.not. filesystem%valid) return
    if (filesystem%total_bytes <= 0_int64) return
    if (len_trim(filesystem%mountpoint) <= 0) return
    if (filesystem_pseudo_type(filesystem%fstype)) return
    visible = .true.
  end function filesystem_visible

  integer function real_filesystem_count(table) result(count)
    type(disk_table), intent(in) :: table
    integer :: filesystem_index

    count = 0
    if (.not. allocated(table%filesystems)) return
    do filesystem_index = 1, size(table%filesystems)
      if (filesystem_visible(table%filesystems(filesystem_index))) count = count + 1
    end do
  end function real_filesystem_count

  real(real64) function filesystem_usage_percent(filesystem) result(percent)
    type(filesystem_info), intent(in) :: filesystem

    percent = 0.0_real64
    if (.not. filesystem%valid) return
    if (filesystem%total_bytes <= 0_int64) return
    percent = 100.0_real64 * real(bounded_bytes(filesystem%used_bytes, filesystem%total_bytes), real64) / &
              real(filesystem%total_bytes, real64)
    percent = max(0.0_real64, min(100.0_real64, percent))
  end function filesystem_usage_percent

  function disk_io_rate_from_delta(previous, current, elapsed_ms) result(rate)
    type(disk_io_info), intent(in) :: previous
    type(disk_io_info), intent(in) :: current
    integer(int64), intent(in) :: elapsed_ms
    type(disk_io_rate_info) :: rate
    real(real64) :: scale

    rate = disk_io_rate_info()
    if (.not. matching_disk_io_samples(previous, current)) return
    if (elapsed_ms <= 0_int64) return
    if (any_disk_io_counter_regressed(previous, current)) return

    scale = 1000.0_real64 / real(elapsed_ms, real64)
    rate%valid = .true.
    rate%device = current%device
    rate%read_bytes_per_sec = real(current%read_bytes - previous%read_bytes, real64) * scale
    rate%write_bytes_per_sec = real(current%write_bytes - previous%write_bytes, real64) * scale
    rate%read_ops_per_sec = real(current%read_ops - previous%read_ops, real64) * scale
    rate%write_ops_per_sec = real(current%write_ops - previous%write_ops, real64) * scale
    rate%busy_percent = max(0.0_real64, min(100.0_real64, &
                         real(current%io_time_ms - previous%io_time_ms, real64) * 100.0_real64 / &
                         real(elapsed_ms, real64)))
  end function disk_io_rate_from_delta

  function disk_io_latency_from_delta(previous, current) result(latency)
    type(disk_io_info), intent(in) :: previous
    type(disk_io_info), intent(in) :: current
    type(disk_latency_info) :: latency
    integer(int64) :: read_ops_delta
    integer(int64) :: write_ops_delta
    integer(int64) :: read_time_delta
    integer(int64) :: write_time_delta

    latency = disk_latency_info()
    if (.not. matching_disk_io_samples(previous, current)) return
    if (any_disk_io_counter_regressed(previous, current)) return

    read_ops_delta = current%read_ops - previous%read_ops
    write_ops_delta = current%write_ops - previous%write_ops
    read_time_delta = current%read_time_ms - previous%read_time_ms
    write_time_delta = current%write_time_ms - previous%write_time_ms

    latency%device = current%device
    if (read_ops_delta > 0_int64) then
      latency%read_valid = .true.
      latency%avg_read_latency_us = real(read_time_delta, real64) * 1000.0_real64 / real(read_ops_delta, real64)
    end if
    if (write_ops_delta > 0_int64) then
      latency%write_valid = .true.
      latency%avg_write_latency_us = real(write_time_delta, real64) * 1000.0_real64 / real(write_ops_delta, real64)
    end if
    latency%valid = latency%read_valid .or. latency%write_valid
  end function disk_io_latency_from_delta

  logical function matching_disk_io_samples(previous, current) result(matches)
    type(disk_io_info), intent(in) :: previous
    type(disk_io_info), intent(in) :: current

    matches = previous%valid .and. current%valid .and. len_trim(current%device) > 0 .and. &
              trim(previous%device) == trim(current%device)
  end function matching_disk_io_samples

  logical function any_disk_io_counter_regressed(previous, current) result(regressed)
    type(disk_io_info), intent(in) :: previous
    type(disk_io_info), intent(in) :: current

    regressed = current%read_bytes < previous%read_bytes .or. &
                current%write_bytes < previous%write_bytes .or. &
                current%read_ops < previous%read_ops .or. &
                current%write_ops < previous%write_ops .or. &
                current%read_time_ms < previous%read_time_ms .or. &
                current%write_time_ms < previous%write_time_ms .or. &
                current%io_time_ms < previous%io_time_ms .or. &
                current%weighted_io_time_ms < previous%weighted_io_time_ms
  end function any_disk_io_counter_regressed

  logical function filesystem_pseudo_type(fstype) result(pseudo)
    character(len=*), intent(in) :: fstype
    character(len=:), allocatable :: kind

    kind = trim(fstype)
    select case (kind)
    case ("", "autofs", "cgroup", "cgroup2", "debugfs", "devfs", "devpts", "devtmpfs", "fdescfs", &
          "fusectl", "kernfs", "linprocfs", "linsysfs", "mqueue", "proc", "procfs", "pstore", "securityfs", &
          "sysfs", "tmpfs", "tracefs")
      pseudo = .true.
    case default
      pseudo = .false.
    end select
  end function filesystem_pseudo_type

  function c_chars_to_text(chars) result(text)
    character(kind=c_char), intent(in) :: chars(:)
    character(len=:), allocatable :: text
    integer :: copied
    integer :: index

    copied = 0
    do index = 1, size(chars)
      if (chars(index) == c_null_char) exit
      copied = copied + 1
    end do

    allocate(character(len=copied) :: text)
    do index = 1, copied
      text(index:index) = achar(iachar(chars(index)))
    end do
  end function c_chars_to_text

  integer(int64) function bounded_bytes(bytes, limit) result(bounded)
    integer(int64), intent(in) :: bytes
    integer(int64), intent(in) :: limit

    bounded = max(0_int64, min(max(0_int64, limit), bytes))
  end function bounded_bytes

end module ftop_disk_data
