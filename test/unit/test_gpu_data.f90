program test_gpu_data
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use ftop_gpu_data, only : &
    assign_gpu_process_busy_percent, &
    empty_gpu_process_table, &
    empty_gpu_table, &
    gpu_info, &
    gpu_process_info, &
    gpu_process_table, &
    gpu_process_table_count, &
    gpu_table, &
    gpu_table_count, &
    merge_gpu_process, &
    sort_gpu_process_table
  implicit none

  call test_gpu_info_defaults()
  call test_empty_gpu_table()
  call test_empty_gpu_process_table()
  call test_gpu_process_rate_assignment()
  call test_gpu_process_merge()
  call test_gpu_process_sort()

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

  subroutine test_empty_gpu_process_table()
    type(gpu_process_table) :: table

    table = empty_gpu_process_table()
    call require(table%valid, "empty GPU process table should be valid")
    call require(allocated(table%processes), "empty GPU process table should allocate rows")
    call require(size(table%processes) == 0, "empty GPU process table should have no rows")
    call require(gpu_process_table_count(table) == 0, "empty GPU process table count mismatch")
  end subroutine test_empty_gpu_process_table

  subroutine test_gpu_process_rate_assignment()
    type(gpu_process_table) :: current
    type(gpu_process_table) :: previous

    current%valid = .true.
    allocate(current%processes(1))
    current%processes(1) = gpu_process_info(valid=.true., pid=42, start_time=100_int64, process_name="render", &
                                            engine="render", engine_time_ns=1500000000_int64)
    previous%valid = .true.
    allocate(previous%processes(1))
    previous%processes(1) = gpu_process_info(valid=.true., pid=42, start_time=100_int64, process_name="render", &
                                             engine="render", engine_time_ns=1000000000_int64)

    call assign_gpu_process_busy_percent(current, previous, 1000_int64)

    call require(current%processes(1)%busy_percent_valid, "matching GPU process should get busy percent")
    call require_close(current%processes(1)%busy_percent, 50.0_real64, "GPU process busy percent")
  end subroutine test_gpu_process_rate_assignment

  subroutine test_gpu_process_merge()
    type(gpu_process_info) :: processes(2)
    type(gpu_process_info) :: process
    integer :: process_count

    process_count = 0
    process = gpu_process_info(valid=.true., pid=7, start_time=30_int64, process_name="game", &
                               engine="render", engine_time_ns=100_int64)
    call merge_gpu_process(processes, process_count, process)
    process = gpu_process_info(valid=.true., pid=7, start_time=30_int64, process_name="game", &
                               engine="copy", engine_time_ns=25_int64, busy_percent_valid=.true., &
                               busy_percent=42.0_real64, memory_valid=.true., memory_bytes=4096_int64)
    call merge_gpu_process(processes, process_count, process)

    call require(process_count == 1, "GPU process merge should keep one row per PID")
    call require(processes(1)%engine_time_ns == 125_int64, "GPU process merge should add engine time")
    call require(trim(processes(1)%engine) == "mixed", "GPU process merge should mark mixed engines")
    call require(processes(1)%busy_percent_valid, "GPU process merge should preserve direct utilization")
    call require_close(processes(1)%busy_percent, 42.0_real64, "GPU process merge utilization")
    call require(processes(1)%memory_valid, "GPU process merge should preserve memory")
    call require(processes(1)%memory_bytes == 4096_int64, "GPU process merge memory")
  end subroutine test_gpu_process_merge

  subroutine test_gpu_process_sort()
    type(gpu_process_table) :: table

    table%valid = .true.
    allocate(table%processes(3))
    table%processes(1) = gpu_process_info(valid=.true., pid=1, engine_time_ns=100_int64)
    table%processes(2) = gpu_process_info(valid=.true., pid=2, engine_time_ns=300_int64)
    table%processes(3) = gpu_process_info(valid=.true., pid=3, engine_time_ns=200_int64)

    call sort_gpu_process_table(table)

    call require(table%processes(1)%pid == 2, "GPU process sort should put hottest row first")
    call require(table%processes(2)%pid == 3, "GPU process sort should put second row next")
    call require(table%processes(3)%pid == 1, "GPU process sort should put coolest row last")
  end subroutine test_gpu_process_sort

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
