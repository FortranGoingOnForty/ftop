program test_proc_data
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use ftop_proc_data, only : PROCESS_HISTORY_CAPACITY, PROCESS_SORT_CPU, PROCESS_SORT_PID, &
                             append_process_histories, assign_process_cpu_percent, build_process_tree, &
                             process_count, process_display_command, process_info, process_state_label, &
                             process_table, process_user_label, sort_process_table
  implicit none

  type(process_info) :: process
  type(process_table) :: table

  call test_process_labels()
  call test_process_cpu_percent()
  call test_process_history()
  call test_process_sorting()
  call test_process_tree()

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

  subroutine test_process_history()
    type(process_table) :: current
    type(process_table) :: previous
    integer :: history_index

    previous%valid = .true.
    allocate(previous%items(2))
    previous%items(1)%valid = .true.
    previous%items(1)%pid = 10
    previous%items(1)%start_time = 100
    previous%items(1)%history_count = 2
    previous%items(1)%cpu_history(1:2) = [1.0_real64, 2.0_real64]
    previous%items(1)%mem_history(1:2) = [3.0_real64, 4.0_real64]
    previous%items(2)%valid = .true.
    previous%items(2)%pid = 20
    previous%items(2)%start_time = 200
    previous%items(2)%history_count = PROCESS_HISTORY_CAPACITY
    do history_index = 1, PROCESS_HISTORY_CAPACITY
      previous%items(2)%cpu_history(history_index) = real(history_index, real64)
      previous%items(2)%mem_history(history_index) = real(history_index + 1, real64)
    end do

    current%valid = .true.
    allocate(current%items(3))
    current%items(1)%valid = .true.
    current%items(1)%pid = 10
    current%items(1)%start_time = 100
    current%items(1)%cpu_percent = 5.0_real64
    current%items(1)%mem_percent = 6.0_real64
    current%items(2)%valid = .true.
    current%items(2)%pid = 20
    current%items(2)%start_time = 200
    current%items(2)%cpu_percent = 101.0_real64
    current%items(2)%mem_percent = -1.0_real64
    current%items(3)%valid = .true.
    current%items(3)%pid = 10
    current%items(3)%start_time = 101
    current%items(3)%cpu_percent = 7.0_real64
    current%items(3)%mem_percent = 8.0_real64

    call append_process_histories(current, previous)
    call require(current%items(1)%history_count == 3, "process history should append matching process sample")
    call require(all(current%items(1)%cpu_history(1:3) == [1.0_real64, 2.0_real64, 5.0_real64]), &
                 "process CPU history should preserve previous samples")
    call require(all(current%items(1)%mem_history(1:3) == [3.0_real64, 4.0_real64, 6.0_real64]), &
                 "process memory history should preserve previous samples")
    call require(current%items(2)%history_count == PROCESS_HISTORY_CAPACITY, "process history should stay capped")
    call require(current%items(2)%cpu_history(1) == 2.0_real64, "capped process CPU history should drop oldest sample")
    call require(current%items(2)%cpu_history(PROCESS_HISTORY_CAPACITY) == 100.0_real64, &
                 "process CPU history should clamp high percentages")
    call require(current%items(2)%mem_history(PROCESS_HISTORY_CAPACITY) == 0.0_real64, &
                 "process memory history should clamp low percentages")
    call require(current%items(3)%history_count == 1, "process history should reset on pid reuse")
    call require(current%items(3)%cpu_history(1) == 7.0_real64, "pid reuse CPU history should start with current sample")
    call require(current%items(3)%mem_history(1) == 8.0_real64, "pid reuse memory history should start with current sample")
  end subroutine test_process_history

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

  subroutine test_process_tree()
    type(process_table) :: tree

    tree%valid = .true.
    allocate(tree%items(8))
    call set_tree_process(tree%items(1), 1, 0)
    call set_tree_process(tree%items(2), 2, 1)
    call set_tree_process(tree%items(3), 3, 1)
    call set_tree_process(tree%items(4), 4, 2)
    call set_tree_process(tree%items(5), 5, 99)
    call set_tree_process(tree%items(6), 6, 6)
    call set_tree_process(tree%items(7), 7, 8)
    call set_tree_process(tree%items(8), 8, 7)

    call build_process_tree(tree)

    call require(all(tree%items%pid == [1, 2, 4, 3, 5, 6, 7, 8]), &
                 "process tree should order parents before children")
    call require(all(tree%items%tree_depth == [0, 1, 2, 1, 0, 0, 0, 1]), &
                 "process tree should assign depths")
    call require(trim(tree%items(1)%tree_prefix) == "", "process tree root prefix should be empty")
    call require(trim(tree%items(2)%tree_prefix) == "├─", "process tree should mark non-last child")
    call require(trim(tree%items(3)%tree_prefix) == "│  └─", "process tree should keep ancestor continuation")
    call require(trim(tree%items(4)%tree_prefix) == "└─", "process tree should mark last child")
    call require(trim(tree%items(8)%tree_prefix) == "└─", "process tree should break cycles safely")
  end subroutine test_process_tree

  subroutine set_tree_process(process, pid, ppid)
    type(process_info), intent(out) :: process
    integer, intent(in) :: pid
    integer, intent(in) :: ppid

    process = process_info()
    process%valid = .true.
    process%pid = pid
    process%ppid = ppid
  end subroutine set_tree_process

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
