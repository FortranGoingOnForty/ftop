program test_proc_data
  use, intrinsic :: iso_fortran_env, only : real64
  use ftop_proc_data, only : process_count, process_display_command, process_info, process_state_label, &
                             process_table, process_user_label, PROCESS_SORT_CPU, PROCESS_SORT_PID, sort_process_table
  implicit none

  type(process_info) :: process
  type(process_table) :: table

  call test_process_labels()
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

end program test_proc_data
