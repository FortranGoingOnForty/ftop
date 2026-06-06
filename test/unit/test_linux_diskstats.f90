program test_linux_diskstats
  use, intrinsic :: iso_fortran_env, only : int64
  use ftop_disk_data, only : disk_io_info
  use ftop_linux_diskstats, only : linux_diskstats_filter_whole_devices, linux_diskstats_parse, linux_diskstats_parse_line
  implicit none

  call test_parse_line()
  call test_parse_buffer()
  call test_parse_custom_sector_size()
  call test_filter_partitions_when_parent_exists()
  call test_rejects_malformed_lines()

contains

  subroutine test_parse_line()
    type(disk_io_info) :: device
    logical :: success

    success = linux_diskstats_parse_line("8 0 sda 10 2 100 30 4 1 20 5 0 35 40", device)
    call require(success, "diskstats line should parse")
    call require(device%valid, "diskstats device should be valid")
    call require(trim(device%device) == "sda", "diskstats device name mismatch")
    call require(device%read_ops == 10_int64, "diskstats read ops mismatch")
    call require(device%reads_merged == 2_int64, "diskstats reads merged mismatch")
    call require(device%read_bytes == 100_int64 * 512_int64, "diskstats read bytes mismatch")
    call require(device%read_time_ms == 30_int64, "diskstats read time mismatch")
    call require(device%write_ops == 4_int64, "diskstats write ops mismatch")
    call require(device%writes_merged == 1_int64, "diskstats writes merged mismatch")
    call require(device%write_bytes == 20_int64 * 512_int64, "diskstats write bytes mismatch")
    call require(device%write_time_ms == 5_int64, "diskstats write time mismatch")
    call require(device%ios_in_progress == 0_int64, "diskstats in-progress mismatch")
    call require(device%io_time_ms == 35_int64, "diskstats io time mismatch")
    call require(device%weighted_io_time_ms == 40_int64, "diskstats weighted io time mismatch")
  end subroutine test_parse_line

  subroutine test_parse_buffer()
    type(disk_io_info), allocatable :: devices(:)
    character(len=:), allocatable :: buffer
    logical :: success

    buffer = "" // new_line("a") // &
             "8 0 sda 10 0 100 20 30 0 200 40 1 60 70" // new_line("a") // &
             "259 0 nvme0n1 11 1 300 21 31 2 400 41 0 61 71 0 0 0 0" // new_line("a")
    success = linux_diskstats_parse(buffer, devices)
    call require(success, "diskstats buffer should parse")
    call require(size(devices) == 2, "diskstats buffer device count mismatch")
    call require(trim(devices(1)%device) == "sda", "first diskstats device mismatch")
    call require(trim(devices(2)%device) == "nvme0n1", "second diskstats device mismatch")
    call require(devices(2)%read_bytes == 300_int64 * 512_int64, "second diskstats read bytes mismatch")
    call require(devices(2)%write_bytes == 400_int64 * 512_int64, "second diskstats write bytes mismatch")
  end subroutine test_parse_buffer

  subroutine test_parse_custom_sector_size()
    type(disk_io_info) :: device
    logical :: success

    success = linux_diskstats_parse_line("8 16 sdb 1 0 2 3 4 0 5 6 0 7 8", device, 4096_int64)
    call require(success, "diskstats line with custom sector size should parse")
    call require(device%sector_size_bytes == 4096_int64, "diskstats sector size mismatch")
    call require(device%read_bytes == 2_int64 * 4096_int64, "diskstats custom read bytes mismatch")
    call require(device%write_bytes == 5_int64 * 4096_int64, "diskstats custom write bytes mismatch")
  end subroutine test_parse_custom_sector_size

  subroutine test_filter_partitions_when_parent_exists()
    type(disk_io_info), allocatable :: devices(:)
    type(disk_io_info), allocatable :: filtered(:)
    character(len=:), allocatable :: buffer
    logical :: success

    buffer = "8 0 sda 1 0 2 3 4 0 5 6 0 7 8" // new_line("a") // &
             "8 1 sda1 1 0 2 3 4 0 5 6 0 7 8" // new_line("a") // &
             "259 0 nvme0n1 1 0 2 3 4 0 5 6 0 7 8" // new_line("a") // &
             "259 1 nvme0n1p1 1 0 2 3 4 0 5 6 0 7 8" // new_line("a") // &
             "252 0 dm-0 1 0 2 3 4 0 5 6 0 7 8" // new_line("a")
    success = linux_diskstats_parse(buffer, devices)
    call require(success .and. size(devices) == 5, "diskstats partition fixture should parse")

    call linux_diskstats_filter_whole_devices(devices, filtered)
    call require(size(filtered) == 3, "diskstats partition filter count mismatch")
    call require(trim(filtered(1)%device) == "sda", "diskstats filter should keep sda")
    call require(trim(filtered(2)%device) == "nvme0n1", "diskstats filter should keep nvme parent")
    call require(trim(filtered(3)%device) == "dm-0", "diskstats filter should keep device mapper disk")
  end subroutine test_filter_partitions_when_parent_exists

  subroutine test_rejects_malformed_lines()
    type(disk_io_info) :: device
    type(disk_io_info), allocatable :: devices(:)
    logical :: success

    success = linux_diskstats_parse_line("8 0 sda 1 2 3", device)
    call require(.not. success .and. .not. device%valid, "short diskstats line should fail")
    success = linux_diskstats_parse_line("8 0 sda 1 2 -3 4 5 6 7 8 9 10 11", device)
    call require(.not. success .and. .not. device%valid, "negative diskstats field should fail")
    success = linux_diskstats_parse("not diskstats" // new_line("a"), devices)
    call require(.not. success .and. size(devices) == 0, "invalid diskstats buffer should fail empty")
  end subroutine test_rejects_malformed_lines

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

end program test_linux_diskstats
