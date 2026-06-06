program test_linux_intelgpu
  use, intrinsic :: iso_c_binding, only : c_char, c_int, c_null_char, c_size_t
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use ftop_gpu_data, only : gpu_table, gpu_table_count
  use ftop_linux_intelgpu, only : linux_intelgpu_snapshot
  implicit none

  integer(int64), parameter :: GIB = 1024_int64 * 1024_int64 * 1024_int64

  interface
    integer(c_int) function ftop_test_intelgpu_sysfs_create(path, path_capacity, path_len, sys_errno) &
        bind(C, name="ftop_test_intelgpu_sysfs_create")
      import :: c_char, c_int, c_size_t
      character(kind=c_char), intent(out) :: path(*)
      integer(c_size_t), value :: path_capacity
      integer(c_size_t), intent(out) :: path_len
      integer(c_int), intent(out) :: sys_errno
    end function ftop_test_intelgpu_sysfs_create

    subroutine ftop_test_intelgpu_sysfs_cleanup() bind(C, name="ftop_test_intelgpu_sysfs_cleanup")
    end subroutine ftop_test_intelgpu_sysfs_cleanup
  end interface

  call test_missing_tree()
  call test_fixture_tree()

contains

  subroutine test_missing_tree()
    type(gpu_table) :: table

    call require(.not. linux_intelgpu_snapshot(table, "/definitely/missing/drm"), &
                 "missing Intel sysfs root should not produce a snapshot")
    call require(table%valid, "missing Intel sysfs root should leave a valid empty GPU table")
    call require(allocated(table%gpus), "missing Intel sysfs root should allocate empty GPU array")
    call require(size(table%gpus) == 0, "missing Intel sysfs root should report zero GPUs")
  end subroutine test_missing_tree

  subroutine test_fixture_tree()
    character(kind=c_char) :: c_root(512)
    character(len=:), allocatable :: root
    integer(c_size_t) :: root_len
    integer(c_int) :: sys_errno
    type(gpu_table) :: table
    logical :: snapshot_ok

    c_root = c_null_char
    if (ftop_test_intelgpu_sysfs_create(c_root, int(size(c_root), c_size_t), root_len, sys_errno) /= 0_c_int) then
      error stop "failed to create Intel sysfs fixture"
    end if
    call c_chars_to_string(c_root, int(root_len), root)

    snapshot_ok = linux_intelgpu_snapshot(table, root)
    call ftop_test_intelgpu_sysfs_cleanup()
    call require(snapshot_ok, "fixture Intel sysfs root should produce a GPU snapshot")

    call require(table%valid, "fixture Intel snapshot should be valid")
    call require(gpu_table_count(table) == 1, "fixture Intel snapshot should include one Intel GPU")
    call require(table%gpus(1)%valid, "fixture Intel GPU should be valid")
    call require(trim(table%gpus(1)%vendor) == "intel", "fixture Intel GPU should report Intel vendor")
    call require(trim(table%gpus(1)%name) == "Fixture Arc", "fixture Intel GPU should report product name")
    call require(trim(table%gpus(1)%pci_id) == "0000:03:00.0", "fixture Intel GPU should report PCI slot")
    call require(table%gpus(1)%utilization_valid, "fixture Intel GPU should report utilization")
    call require_close(table%gpus(1)%utilization_percent, 23.0_real64, "fixture Intel utilization")
    call require(table%gpus(1)%memory_valid, "fixture Intel GPU should report VRAM")
    call require(table%gpus(1)%memory_total_bytes == 8_int64 * GIB, "fixture Intel total VRAM")
    call require(table%gpus(1)%memory_used_bytes == 2_int64 * GIB, "fixture Intel used VRAM")
    call require(table%gpus(1)%temperature_valid, "fixture Intel GPU should report temperature")
    call require_close(table%gpus(1)%temp_celsius, 52.0_real64, "fixture Intel temperature")
    call require(table%gpus(1)%power_valid, "fixture Intel GPU should report power")
    call require_close(table%gpus(1)%power_watts, 45.0_real64, "fixture Intel power")
    call require_close(table%gpus(1)%power_limit_watts, 65.0_real64, "fixture Intel power cap")
    call require(table%gpus(1)%core_clock_valid, "fixture Intel GPU should report core clock")
    call require_close(table%gpus(1)%clock_core_mhz, 700.0_real64, "fixture Intel core clock")
    call require_close(table%gpus(1)%clock_max_core_mhz, 1300.0_real64, "fixture Intel max core clock")
  end subroutine test_fixture_tree

  subroutine c_chars_to_string(c_buffer, value_len, text)
    character(kind=c_char), intent(in) :: c_buffer(:)
    integer, intent(in) :: value_len
    character(len=:), allocatable, intent(out) :: text
    integer :: copied_len
    integer :: index_value

    copied_len = max(0, min(value_len, size(c_buffer)))
    allocate(character(len=copied_len) :: text)
    do index_value = 1, copied_len
      text(index_value:index_value) = achar(iachar(c_buffer(index_value)))
    end do
  end subroutine c_chars_to_string

  subroutine require_close(actual, expected, message)
    real(real64), intent(in) :: actual
    real(real64), intent(in) :: expected
    character(len=*), intent(in) :: message

    call require(abs(actual - expected) < 0.001_real64, message)
  end subroutine require_close

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require
end program test_linux_intelgpu
