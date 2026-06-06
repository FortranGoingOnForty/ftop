program test_gpu_data
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use ftop_gpu_data, only : empty_gpu_table, gpu_info, gpu_table, gpu_table_count
  implicit none

  call test_gpu_info_defaults()
  call test_empty_gpu_table()

contains

  subroutine test_gpu_info_defaults()
    type(gpu_info) :: gpu

    call require(.not. gpu%valid, "gpu info should default invalid")
    call require(len_trim(gpu%vendor) == 0, "gpu vendor should default empty")
    call require(len_trim(gpu%name) == 0, "gpu name should default empty")
    call require(.not. gpu%utilization_valid, "gpu utilization should default invalid")
    call require_close(gpu%utilization_percent, 0.0_real64, "gpu utilization should default zero")
    call require(.not. gpu%memory_valid, "gpu memory should default invalid")
    call require(gpu%memory_used_bytes == 0_int64, "gpu used memory should default zero")
    call require(gpu%memory_total_bytes == 0_int64, "gpu total memory should default zero")
    call require(.not. gpu%temperature_valid, "gpu temperature should default invalid")
    call require(.not. gpu%power_valid, "gpu power should default invalid")
    call require(.not. gpu%core_clock_valid, "gpu core clock should default invalid")
    call require(.not. gpu%fan_valid, "gpu fan should default invalid")
    call require(.not. gpu%driver_version_valid, "gpu driver version should default invalid")
  end subroutine test_gpu_info_defaults

  subroutine test_empty_gpu_table()
    type(gpu_table) :: table

    table = empty_gpu_table()
    call require(table%valid, "empty gpu table should be valid")
    call require(allocated(table%gpus), "empty gpu table should allocate gpus")
    call require(size(table%gpus) == 0, "empty gpu table should have no rows")
    call require(gpu_table_count(table) == 0, "empty gpu table count mismatch")
  end subroutine test_empty_gpu_table

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

  subroutine require_close(actual, expected, message)
    real(real64), intent(in) :: actual
    real(real64), intent(in) :: expected
    character(len=*), intent(in) :: message

    if (abs(actual - expected) > 0.000001_real64) error stop message
  end subroutine require_close
end program test_gpu_data
