program test_linux_gpu_fdinfo
  use, intrinsic :: iso_fortran_env, only : int64
  use ftop_gpu_data, only : gpu_process_info
  use ftop_linux_gpu_fdinfo, only : linux_gpu_fdinfo_parse
  implicit none

  call test_parse_engine_counters()
  call test_reject_missing_engine_counter()
  call test_parse_engine_units()

contains

  subroutine test_parse_engine_counters()
    type(gpu_process_info) :: process
    character(len=:), allocatable :: text

    text = "pos: 0" // new_line("a") // &
           "drm-driver: amdgpu" // new_line("a") // &
           "drm-engine-gfx: 1000000000 ns" // new_line("a") // &
           "drm-engine-compute: 250000000 ns" // new_line("a") // &
           "drm-memory-vram: 2048 KiB" // new_line("a")

    call require(linux_gpu_fdinfo_parse(text, 1234, 55_int64, "game", process), &
                 "fdinfo parser should accept DRM engine counters")
    call require(process%valid, "parsed GPU process should be valid")
    call require(process%pid == 1234, "parsed GPU process PID")
    call require(process%start_time == 55_int64, "parsed GPU process start time")
    call require(trim(process%process_name) == "game", "parsed GPU process name")
    call require(trim(process%engine) == "gfx", "parsed GPU process should keep busiest engine")
    call require(process%engine_time_ns == 1250000000_int64, "parsed GPU process engine time")
    call require(process%memory_valid, "parsed GPU process memory should be valid")
    call require(process%memory_bytes == 2048_int64 * 1024_int64, "parsed GPU process memory bytes")
  end subroutine test_parse_engine_counters

  subroutine test_reject_missing_engine_counter()
    type(gpu_process_info) :: process
    character(len=:), allocatable :: text

    text = "pos: 0" // new_line("a") // &
           "drm-driver: i915" // new_line("a") // &
           "drm-memory-vram: 2048 KiB" // new_line("a")

    call require(.not. linux_gpu_fdinfo_parse(text, 1234, 55_int64, "idle", process), &
                 "fdinfo parser should reject rows without engine time")
    call require(.not. process%valid, "rejected GPU process should be invalid")
  end subroutine test_reject_missing_engine_counter

  subroutine test_parse_engine_units()
    type(gpu_process_info) :: process
    character(len=:), allocatable :: text

    text = "drm-engine-render: 5 ms" // new_line("a") // &
           "drm-engine-copy: 1 us" // new_line("a")

    call require(linux_gpu_fdinfo_parse(text, 7, 9_int64, "render", process), &
                 "fdinfo parser should accept time units")
    call require(process%engine_time_ns == 5001000_int64, "fdinfo parser should convert time units")
  end subroutine test_parse_engine_units

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require
end program test_linux_gpu_fdinfo
