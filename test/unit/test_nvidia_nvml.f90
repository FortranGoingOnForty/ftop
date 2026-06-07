program test_nvidia_nvml
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use ftop_gpu_data, only : gpu_table, gpu_table_count
  use ftop_nvidia_nvml, only : nvidia_nvml_gpu_snapshot
  implicit none

  integer(int64), parameter :: GIB = 1024_int64 * 1024_int64 * 1024_int64
  character(len=512) :: fixture_path

  if (command_argument_count() /= 1) error stop "usage: test_nvidia_nvml <fixture>"
  call get_command_argument(1, fixture_path)

  call test_missing_library()
  call test_fixture_library(trim(fixture_path))

contains

  subroutine test_missing_library()
    type(gpu_table) :: table

    call require(.not. nvidia_nvml_gpu_snapshot(table, "/definitely/missing/libnvidia-ml.so.1"), &
                 "missing NVML library should not produce a GPU snapshot")
    call require(table%valid, "missing NVML library should leave a valid empty GPU table")
    call require(allocated(table%gpus), "missing NVML library should allocate empty GPU array")
    call require(size(table%gpus) == 0, "missing NVML library should report zero GPUs")
  end subroutine test_missing_library

  subroutine test_fixture_library(path)
    character(len=*), intent(in) :: path
    type(gpu_table) :: table

    call require(nvidia_nvml_gpu_snapshot(table, path), "fixture NVML library should produce a GPU snapshot")
    call require(table%valid, "fixture snapshot should be valid")
    call require(gpu_table_count(table) == 1, "fixture snapshot should contain one GPU")
    call require(table%gpus(1)%valid, "fixture GPU should be valid")
    call require(trim(table%gpus(1)%vendor) == "nvidia", "fixture GPU should report NVIDIA vendor")
    call require(trim(table%gpus(1)%name) == "Fixture RTX", "fixture GPU should report the device name")
    call require(trim(table%gpus(1)%pci_id) == "00000000:65:00.0", "fixture GPU should report PCI id")
    call require(table%gpus(1)%driver_version_valid, "fixture GPU should report driver version")
    call require(trim(table%gpus(1)%driver_version) == "555.42.01", "fixture GPU should copy driver version")
    call require(table%gpus(1)%utilization_valid, "fixture GPU should report utilization")
    call require_close(table%gpus(1)%utilization_percent, 75.0_real64, "fixture GPU utilization")
    call require(table%gpus(1)%memory_utilization_valid, "fixture GPU should report memory utilization")
    call require_close(table%gpus(1)%memory_utilization_percent, 41.0_real64, "fixture memory utilization")
    call require(table%gpus(1)%memory_valid, "fixture GPU should report memory")
    call require(table%gpus(1)%memory_total_bytes == 12_int64 * GIB, "fixture total memory")
    call require(table%gpus(1)%memory_used_bytes == 6_int64 * GIB, "fixture used memory")
    call require(table%gpus(1)%temperature_valid, "fixture GPU should report temperature")
    call require_close(table%gpus(1)%temp_celsius, 64.0_real64, "fixture temperature")
    call require(table%gpus(1)%power_valid, "fixture GPU should report power")
    call require_close(table%gpus(1)%power_watts, 123.0_real64, "fixture power")
    call require_close(table%gpus(1)%power_limit_watts, 250.0_real64, "fixture power limit")
    call require(table%gpus(1)%core_clock_valid, "fixture GPU should report core clock")
    call require_close(table%gpus(1)%clock_core_mhz, 1800.0_real64, "fixture core clock")
    call require_close(table%gpus(1)%clock_max_core_mhz, 2100.0_real64, "fixture max core clock")
    call require(table%gpus(1)%memory_clock_valid, "fixture GPU should report memory clock")
    call require_close(table%gpus(1)%clock_memory_mhz, 9500.0_real64, "fixture memory clock")
    call require(table%gpus(1)%fan_valid, "fixture GPU should report fan speed")
    call require_close(table%gpus(1)%fan_speed_percent, 55.0_real64, "fixture fan speed")
    call require(table%gpus(1)%encoder_valid, "fixture GPU should report encoder utilization")
    call require_close(table%gpus(1)%encoder_utilization_percent, 3.0_real64, "fixture encoder utilization")
    call require(table%gpus(1)%decoder_valid, "fixture GPU should report decoder utilization")
    call require_close(table%gpus(1)%decoder_utilization_percent, 4.0_real64, "fixture decoder utilization")
  end subroutine test_fixture_library

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
end program test_nvidia_nvml
