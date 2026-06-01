program test_linux_proc_stat
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use ftop_cpu_data, only : cpu_core_info, cpu_state_delta_info, cpu_state_ticks, cpu_state_total_ticks
  use ftop_linux_proc_stat, only : linux_proc_stat_parse, linux_proc_stat_parse_line
  implicit none

  call test_parse_proc_stat_buffer()
  call test_parse_partial_cpu_line()
  call test_reject_invalid_cpu_line()
  call test_cpu_state_delta_percentages()
  call test_cpu_state_delta_no_change_is_zero_usage()
  call test_cpu_state_rollover_is_invalid()

contains

  subroutine test_parse_proc_stat_buffer()
    character(len=*), parameter :: proc_stat = &
      "cpu  100 20 30 400 10 5 5 2 0 0" // new_line("a") // &
      "cpu0 50 10 10 200 5 2 3 1 0 0" // new_line("a") // &
      "cpu1 50 10 20 200 5 3 2 1 0 0" // new_line("a") // &
      "intr 1 2 3" // new_line("a")
    type(cpu_state_ticks) :: total
    type(cpu_state_ticks), allocatable :: cores(:)

    call require(linux_proc_stat_parse(proc_stat, total, cores), "proc stat parse failed")
    call require(total%valid, "total CPU line must be valid")
    call require(size(cores) == 2, "core count mismatch")
    call require(cores(1)%valid, "first core line must be valid")
    call require(cores(2)%valid, "second core line must be valid")
    call require(total%user == 100_int64, "total user ticks mismatch")
    call require(total%iowait == 10_int64, "total iowait ticks mismatch")
    call require(total%steal == 2_int64, "total steal ticks mismatch")
    call require(cores(1)%system == 10_int64, "first core system ticks mismatch")
    call require(cores(2)%softirq == 2_int64, "second core softirq ticks mismatch")
    call require(cpu_state_total_ticks(total) == 572_int64, "total ticks mismatch")
  end subroutine test_parse_proc_stat_buffer

  subroutine test_parse_partial_cpu_line()
    type(cpu_state_ticks) :: sample

    call require(linux_proc_stat_parse_line("cpu 1 2 3 4", sample), "partial CPU line parse failed")
    call require(sample%valid, "partial CPU line must be valid")
    call require(sample%user == 1_int64, "partial user ticks mismatch")
    call require(sample%idle == 4_int64, "partial idle ticks mismatch")
    call require(sample%iowait == 0_int64, "partial iowait default mismatch")
    call require(sample%steal == 0_int64, "partial steal default mismatch")
  end subroutine test_parse_partial_cpu_line

  subroutine test_reject_invalid_cpu_line()
    type(cpu_state_ticks) :: sample

    call require(.not. linux_proc_stat_parse_line("cpu 1 2 x 4", sample), "invalid token must fail")
    call require(.not. sample%valid, "invalid token must leave sample invalid")
    call require(.not. linux_proc_stat_parse_line("cpu 1 2 3", sample), "short CPU line must fail")
    call require(.not. linux_proc_stat_parse_line("cpu 1 2 3 -4", sample), "negative CPU tick must fail")
  end subroutine test_reject_invalid_cpu_line

  subroutine test_cpu_state_delta_percentages()
    type(cpu_state_ticks) :: previous
    type(cpu_state_ticks) :: current
    type(cpu_core_info) :: info

    previous%valid = .true.
    previous%user = 100_int64
    previous%system = 100_int64
    previous%idle = 800_int64

    current%valid = .true.
    current%user = 150_int64
    current%nice = 10_int64
    current%system = 130_int64
    current%idle = 880_int64
    current%iowait = 20_int64
    current%irq = 5_int64
    current%softirq = 5_int64

    info = cpu_state_delta_info(previous, current)
    call require(info%valid, "CPU delta info must be valid")
    call require_close(info%usage_percent, 50.0_real64, "CPU usage percent mismatch")
    call require_close(info%user_percent, 30.0_real64, "CPU user percent mismatch")
    call require_close(info%system_percent, 20.0_real64, "CPU system percent mismatch")
    call require_close(info%iowait_percent, 10.0_real64, "CPU iowait percent mismatch")
  end subroutine test_cpu_state_delta_percentages

  subroutine test_cpu_state_delta_no_change_is_zero_usage()
    type(cpu_state_ticks) :: previous
    type(cpu_state_ticks) :: current
    type(cpu_core_info) :: info

    previous%valid = .true.
    previous%user = 100_int64
    previous%idle = 900_int64
    current = previous

    info = cpu_state_delta_info(previous, current)
    call require(info%valid, "unchanged CPU delta must be valid")
    call require_close(info%usage_percent, 0.0_real64, "unchanged CPU usage must be zero")
    call require_close(info%user_percent, 0.0_real64, "unchanged CPU user must be zero")
    call require_close(info%system_percent, 0.0_real64, "unchanged CPU system must be zero")
    call require_close(info%iowait_percent, 0.0_real64, "unchanged CPU iowait must be zero")
  end subroutine test_cpu_state_delta_no_change_is_zero_usage

  subroutine test_cpu_state_rollover_is_invalid()
    type(cpu_state_ticks) :: previous
    type(cpu_state_ticks) :: current
    type(cpu_core_info) :: info

    previous%valid = .true.
    previous%user = 100_int64
    previous%idle = 100_int64
    current%valid = .true.
    current%user = 90_int64
    current%idle = 120_int64

    info = cpu_state_delta_info(previous, current)
    call require(.not. info%valid, "CPU rollover delta must be invalid")
    call require_close(info%usage_percent, 0.0_real64, "CPU rollover usage must stay zero")
  end subroutine test_cpu_state_rollover_is_invalid

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

end program test_linux_proc_stat
