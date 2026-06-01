program test_proc_data
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use ftop_proc_data, only : assign_process_cpu_percent, process_count, process_display_command, process_info, &
                             process_state_label, process_table, process_user_label, PROCESS_SORT_CPU, &
                             PROCESS_SORT_PID, sort_process_table
  implicit none

  type(process_info) :: process
  type(process_table) :: table

  call test_process_labels()
  call test_process_cpu_percent()
  call test_process_sorting()

contains

  subroutine test_process_labels()
    allocate(table%items(2))
    call require(process_count(table) == 2, "process_count should use allocated item size")

    process%uid = 1001
    call require(process_user_label(process) == "1001", "process user label should fall back to uid")
    process%user_valid = .true.
    process%user = "tester"
    call require(process_user_label(process) == "tester", "process user label should prefer user name")

    call require(process_state_label(process) == "?", "empty process state should render unknown")
    process%state = "R"
    call require(process_state_label(process) == "R", "process state should trim state text")

    call require(process_display_command(process) == "[unknown]", "empty command and name should render unknown")
    process%name = "ftop"
    call require(process_display_command(process) == "ftop", "process name should be display fallback")
    process%command = "ftop --test"
    call require(process_display_command(process) == "ftop --test", "process command should be display text")
  end subroutine test_process_labels

  subroutine test_process_cpu_percent()
    type(process_table) :: current
    type(process_table) :: previous

    previous%valid = .true.
    allocate(previous%items(3))
    previous%items(1)%valid = .true.
    previous%items(1)%pid = 10
    previous%items(1)%start_time = 100
    previous%items(1)%cpu_time = 1000
    previous%items(2)%valid = .true.
    previous%items(2)%pid = 20
    previous%items(2)%start_time = 200
    previous%items(2)%cpu_time = 500
    previous%items(3)%valid = .true.
    previous%items(3)%pid = 30
    previous%items(3)%start_time = 300
    previous%items(3)%cpu_time = 100

    current%valid = .true.
    allocate(current%items(4))
    current%items(1)%valid = .true.
    current%items(1)%pid = 20
    current%items(1)%start_time = 200
    current%items(1)%cpu_time = 650
    current%items(2)%valid = .true.
    current%items(2)%pid = 10
    current%items(2)%start_time = 100
    current%items(2)%cpu_time = 1250
    current%items(3)%valid = .true.
    current%items(3)%pid = 30
    current%items(3)%start_time = 301
    current%items(3)%cpu_time = 200
    current%items(4)%valid = .true.
    current%items(4)%pid = 40
    current%items(4)%start_time = 400
    current%items(4)%cpu_time = 20

    call assign_process_cpu_percent(current, previous, 500_int64)
    call require(near(current%items(1)%cpu_percent, 30.0_real64), "process CPU should use matching pid/start delta")
    call require(near(current%items(2)%cpu_percent, 50.0_real64), "process CPU should preserve unsorted matches")
    call require(near(current%items(3)%cpu_percent, 0.0_real64), "process CPU should reject pid reuse")
    call require(near(current%items(4)%cpu_percent, 0.0_real64), "process CPU should ignore new processes")

    current%items%cpu_percent = 99.0_real64
    call assign_process_cpu_percent(current, previous, 0_int64)
    call require(all(current%items%cpu_percent == 0.0_real64), "process CPU should reset invalid elapsed deltas")
  end subroutine test_process_cpu_percent

  subroutine test_process_sorting()
    type(process_table) :: sorted

    allocate(sorted%items(3))
    sorted%items(1)%valid = .true.
    sorted%items(1)%pid = 30
    sorted%items(1)%cpu_percent = 5.0_real64
    sorted%items(2)%valid = .true.
    sorted%items(2)%pid = 10
    sorted%items(2)%cpu_percent = 5.0_real64
    sorted%items(3)%valid = .true.
    sorted%items(3)%pid = 20
    sorted%items(3)%cpu_percent = 9.0_real64

    call sort_process_table(sorted, PROCESS_SORT_PID)
    call require(all(sorted%items%pid == [10, 20, 30]), "process sort should order pid ascending")

    sorted%items(1)%pid = 30
    sorted%items(1)%cpu_percent = 5.0_real64
    sorted%items(2)%pid = 10
    sorted%items(2)%cpu_percent = 5.0_real64
    sorted%items(3)%pid = 20
    sorted%items(3)%cpu_percent = 9.0_real64
    call sort_process_table(sorted, PROCESS_SORT_CPU, descending=.true.)
    call require(all(sorted%items%pid == [20, 30, 10]), "process sort should be stable descending")
  end subroutine test_process_sorting

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

  logical function near(left, right) result(matches)
    real(real64), intent(in) :: left
    real(real64), intent(in) :: right

    matches = abs(left - right) < 0.0001_real64
  end function near

end program test_proc_data
