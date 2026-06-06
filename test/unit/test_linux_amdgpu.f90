program test_linux_amdgpu
  use, intrinsic :: iso_c_binding, only : c_char, c_int, c_null_char, c_size_t
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use ftop_gpu_data, only : gpu_table, gpu_table_count
  use ftop_linux_amdgpu, only : linux_amdgpu_active_dpm_clock_mhz, linux_amdgpu_snapshot
  implicit none

  integer(int64), parameter :: GIB = 1024_int64 * 1024_int64 * 1024_int64

  interface
    integer(c_int) function ftop_test_amdgpu_sysfs_create(path, path_capacity, path_len, sys_errno) &
        bind(C, name="ftop_test_amdgpu_sysfs_create")
      import :: c_char, c_int, c_size_t
      character(kind=c_char), intent(out) :: path(*)
      integer(c_size_t), value :: path_capacity
      integer(c_size_t), intent(out) :: path_len
      integer(c_int), intent(out) :: sys_errno
    end function ftop_test_amdgpu_sysfs_create

    subroutine ftop_test_amdgpu_sysfs_cleanup() bind(C, name="ftop_test_amdgpu_sysfs_cleanup")
    end subroutine ftop_test_amdgpu_sysfs_cleanup
  end interface

  call test_dpm_parser()
  call test_missing_tree()
  call test_fixture_tree()

contains

  subroutine test_dpm_parser()
    real(real64) :: mhz

    call require(linux_amdgpu_active_dpm_clock_mhz("0: 500Mhz" // new_line("a") // &
                                                   "1: 2500Mhz *" // new_line("a"), mhz), &
                 "AMD DPM parser should find active clock")
    call require_close(mhz, 2500.0_real64, "AMD DPM active clock")
    call require(.not. linux_amdgpu_active_dpm_clock_mhz("0: 500Mhz" // new_line("a"), mhz), &
                 "AMD DPM parser should reject missing active marker")
  end subroutine test_dpm_parser

  subroutine test_missing_tree()
    type(gpu_table) :: table

    call require(.not. linux_amdgpu_snapshot(table, "/definitely/missing/drm"), &
                 "missing AMD sysfs root should not produce a snapshot")
    call require(table%valid, "missing AMD sysfs root should leave a valid empty GPU table")
    call require(allocated(table%gpus), "missing AMD sysfs root should allocate empty GPU array")
    call require(size(table%gpus) == 0, "missing AMD sysfs root should report zero GPUs")
  end subroutine test_missing_tree

  subroutine test_fixture_tree()
    character(kind=c_char) :: c_root(512)
    character(len=:), allocatable :: root
    integer(c_size_t) :: root_len
    integer(c_int) :: sys_errno
    type(gpu_table) :: table
    logical :: snapshot_ok

    c_root = c_null_char
    if (ftop_test_amdgpu_sysfs_create(c_root, int(size(c_root), c_size_t), root_len, sys_errno) /= 0_c_int) then
      error stop "failed to create AMD sysfs fixture"
    end if
    call c_chars_to_string(c_root, int(root_len), root)

    snapshot_ok = linux_amdgpu_snapshot(table, root)
    call ftop_test_amdgpu_sysfs_cleanup()
    call require(snapshot_ok, "fixture AMD sysfs root should produce a GPU snapshot")

    call require(table%valid, "fixture AMD snapshot should be valid")
    call require(gpu_table_count(table) == 1, "fixture AMD snapshot should include one AMD GPU")
    call require(table%gpus(1)%valid, "fixture AMD GPU should be valid")
    call require(trim(table%gpus(1)%vendor) == "amd", "fixture AMD GPU should report AMD vendor")
    call require(trim(table%gpus(1)%name) == "Fixture Radeon", "fixture AMD GPU should report product name")
    call require(trim(table%gpus(1)%pci_id) == "0000:65:00.0", "fixture AMD GPU should report PCI slot")
    call require(table%gpus(1)%utilization_valid, "fixture AMD GPU should report utilization")
    call require_close(table%gpus(1)%utilization_percent, 67.0_real64, "fixture AMD utilization")
    call require(table%gpus(1)%memory_valid, "fixture AMD GPU should report VRAM")
    call require(table%gpus(1)%memory_total_bytes == 16_int64 * GIB, "fixture AMD total VRAM")
    call require(table%gpus(1)%memory_used_bytes == 8_int64 * GIB, "fixture AMD used VRAM")
    call require(table%gpus(1)%temperature_valid, "fixture AMD GPU should report temperature")
    call require_close(table%gpus(1)%temp_celsius, 65.0_real64, "fixture AMD temperature")
    call require(table%gpus(1)%power_valid, "fixture AMD GPU should report power")
    call require_close(table%gpus(1)%power_watts, 123.0_real64, "fixture AMD power")
    call require_close(table%gpus(1)%power_limit_watts, 250.0_real64, "fixture AMD power cap")
    call require(table%gpus(1)%core_clock_valid, "fixture AMD GPU should report core clock")
    call require_close(table%gpus(1)%clock_core_mhz, 2500.0_real64, "fixture AMD core clock")
    call require(table%gpus(1)%memory_clock_valid, "fixture AMD GPU should report memory clock")
    call require_close(table%gpus(1)%clock_memory_mhz, 1200.0_real64, "fixture AMD memory clock")
    call require(table%gpus(1)%fan_valid, "fixture AMD GPU should report fan speed")
    call require_close(table%gpus(1)%fan_speed_percent, 100.0_real64 * 128.0_real64 / 255.0_real64, &
                       "fixture AMD fan speed")
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
end program test_linux_amdgpu
