program test_linux_meminfo
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use ftop_linux_meminfo, only : linux_meminfo_parse
  use ftop_mem_data, only : memory_info, memory_usage_percent
  implicit none

  call test_parse_full_meminfo()
  call test_available_fallback()
  call test_available_clamps_to_total()
  call test_reject_missing_total()
  call test_reject_negative_value()

contains

  subroutine test_parse_full_meminfo()
    character(len=*), parameter :: meminfo = &
      "MemTotal:       16384 kB" // new_line("a") // &
      "MemFree:         1024 kB" // new_line("a") // &
      "MemAvailable:    4096 kB" // new_line("a") // &
      "Buffers:          256 kB" // new_line("a") // &
      "Cached:          2048 kB" // new_line("a") // &
      "SwapTotal:       8192 kB" // new_line("a") // &
      "SwapFree:        2048 kB" // new_line("a")
    type(memory_info) :: info

    call require(linux_meminfo_parse(meminfo, info), "full meminfo parse failed")
    call require(info%valid, "full meminfo must be valid")
    call require(info%total_bytes == kib(16384), "total bytes mismatch")
    call require(info%free_bytes == kib(1024), "free bytes mismatch")
    call require(info%available_bytes == kib(4096), "available bytes mismatch")
    call require(info%used_bytes == kib(12288), "used bytes mismatch")
    call require(info%buffers_bytes == kib(256), "buffers bytes mismatch")
    call require(info%cached_bytes == kib(2048), "cached bytes mismatch")
    call require(info%swap_total_bytes == kib(8192), "swap total bytes mismatch")
    call require(info%swap_used_bytes == kib(6144), "swap used bytes mismatch")
    call require_close(memory_usage_percent(info), 75.0_real64, "memory usage percent mismatch")
  end subroutine test_parse_full_meminfo

  subroutine test_available_fallback()
    character(len=*), parameter :: meminfo = &
      "MemTotal:        8000 kB" // new_line("a") // &
      "MemFree:         1000 kB" // new_line("a") // &
      "Buffers:          500 kB" // new_line("a") // &
      "Cached:          1500 kB" // new_line("a")
    type(memory_info) :: info

    call require(linux_meminfo_parse(meminfo, info), "fallback meminfo parse failed")
    call require(info%available_bytes == kib(3000), "fallback available bytes mismatch")
    call require(info%used_bytes == kib(5000), "fallback used bytes mismatch")
    call require(info%swap_total_bytes == 0_int64, "missing swap total must default zero")
    call require(info%swap_used_bytes == 0_int64, "missing swap used must default zero")
  end subroutine test_available_fallback

  subroutine test_available_clamps_to_total()
    character(len=*), parameter :: meminfo = &
      "MemTotal:        1000 kB" // new_line("a") // &
      "MemFree:         2000 kB" // new_line("a") // &
      "MemAvailable:   3000 kB" // new_line("a") // &
      "SwapTotal:      1000 kB" // new_line("a") // &
      "SwapFree:       2000 kB" // new_line("a")
    type(memory_info) :: info

    call require(linux_meminfo_parse(meminfo, info), "clamp meminfo parse failed")
    call require(info%free_bytes == kib(1000), "free bytes must clamp to total")
    call require(info%available_bytes == kib(1000), "available bytes must clamp to total")
    call require(info%used_bytes == 0_int64, "used bytes must clamp low")
    call require(info%swap_used_bytes == 0_int64, "swap used bytes must clamp low")
  end subroutine test_available_clamps_to_total

  subroutine test_reject_missing_total()
    type(memory_info) :: info

    call require(.not. linux_meminfo_parse("MemFree: 1 kB" // new_line("a"), info), "missing total must fail")
    call require(.not. info%valid, "missing total must leave info invalid")
  end subroutine test_reject_missing_total

  subroutine test_reject_negative_value()
    character(len=*), parameter :: meminfo = &
      "MemTotal:        1000 kB" // new_line("a") // &
      "MemFree:           -1 kB" // new_line("a")
    type(memory_info) :: info

    call require(.not. linux_meminfo_parse(meminfo, info), "negative meminfo value must fail")
  end subroutine test_reject_negative_value

  integer(int64) function kib(value) result(bytes)
    integer, intent(in) :: value

    bytes = int(value, int64) * 1024_int64
  end function kib

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

  subroutine require_close(actual, expected, message)
    real(real64), intent(in) :: actual
    real(real64), intent(in) :: expected
    character(len=*), intent(in) :: message

    if (abs(actual - expected) > 0.000000001_real64) error stop message
  end subroutine require_close

end program test_linux_meminfo
