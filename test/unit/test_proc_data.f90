program test_proc_data
  use ftop_proc_data, only : process_count, process_display_command, process_info, process_state_label, &
                             process_table, process_user_label
  implicit none

  type(process_info) :: process
  type(process_table) :: table

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

contains

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

end program test_proc_data
