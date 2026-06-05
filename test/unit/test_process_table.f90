program test_process_table
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use fgof_screen, only : allocate_screen
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_collector, only : collector_snapshot
  use ftop_color, only : COLOR_UI_ACCENT, COLOR_UI_BORDER, COLOR_UI_DIM, COLOR_UI_PANEL, style_from_rgb
  use ftop_proc_data, only : PROCESS_SORT_COMMAND, PROCESS_SORT_CPU, PROCESS_SORT_USER, process_info, process_table
  use ftop_process_table, only : process_table_append_filter_text, process_table_begin_filter, &
                                  process_table_begin_signal, process_table_begin_tag_signal, &
                                  process_table_cancel_signal, process_table_clear_filter, &
                                  process_table_close_command_detail, &
                                  process_table_append_fuzzy_text, process_table_clear_fuzzy, &
                                  process_table_clear_tags, &
                                  process_table_cycle_sort_key, &
                                  process_table_delete_filter_char, process_table_delete_filter_right, &
                                  process_table_delete_fuzzy_char, &
                                  process_table_move_filter_cursor, process_table_page_delta, &
                                  process_table_finish_filter, &
                                  process_table_append_signal_digit, process_table_confirm_signal, &
                                  process_table_mark_signal_feedback, process_table_scroll_delta, &
                                  process_table_select_at, process_table_select_delta, &
                                  process_table_set_columns, &
                                  process_table_set_signal, process_table_signal_status, &
                                  process_table_signal_target_at, process_table_signal_target_count, &
                                  process_table_sort_at, process_table_step_fuzzy_match, &
                                  process_table_state, &
                                  process_table_status, &
                                  process_fuzzy_best_match, process_fuzzy_next_match, &
                                  process_table_process_tagged, process_table_prune_tags, &
                                  process_table_toggle_metric_sparklines, process_table_toggle_command_detail, &
                                  process_table_toggle_follow, process_table_toggle_tag, &
                                  process_table_toggle_selected_node, process_table_toggle_sort_direction, render_process_panel
  use ftop_table, only : TABLE_SORT_ASCENDING, TABLE_SORT_DESCENDING
  use ftop_widgets, only : widget_rect
  implicit none

  type(screen_buffer) :: buffer
  type(screen_style) :: border_style
  type(screen_style) :: dim_style
  type(screen_style) :: title_style
  type(process_table_state) :: state
  character(len=8) :: bad_columns(2)
  character(len=8) :: custom_columns(4)
  character(len=:), allocatable :: column_error
  logical :: toggled

  call test_process_state_updates()
  call test_process_fuzzy_matching()

  buffer = allocate_screen(80, 10)
  border_style = style_from_rgb(fg=COLOR_UI_BORDER)
  title_style = style_from_rgb(fg=COLOR_UI_ACCENT, bg=COLOR_UI_PANEL, bold=.true.)
  dim_style = style_from_rgb(fg=COLOR_UI_DIM)
  call test_process_follow_state()
  call test_process_tag_state()
  call test_process_tag_stress()
  bad_columns = [character(len=8) :: "pid", "bogus"]
  custom_columns = [character(len=8) :: "pid", "cpu", "mem", "command"]

  call render_process_panel(buffer, widget_rect(1, 1, 80, 10), sample_snapshot(), border_style, title_style, dim_style)
  call require(index(row_text(buffer, 2), "PID") > 0, "process table should render PID header")
  call require(index(row_text(buffer, 2), "PRI") > 0, "process table should render priority header")
  call require(index(row_text(buffer, 2), "TIME+") > 0, "process table should render CPU time header")
  call require(index(row_text(buffer, 3), "100") > 0, "process table should render pid")
  call require(index(row_text(buffer, 3), "parent --test") > 0, "process table should render command")
  call require(index(row_text(buffer, 3), "12.5%") > 0, "process table should render cpu percent")
  call require(index(row_text(buffer, 3), "64.0M") > 0, "process table should render resident memory")
  call require(index(row_text(buffer, 3), "0:01") > 0, "process table should render CPU time")
  call require(index(row_text(buffer, 4), "200") > 0, "process table should render child pid")
  call require(index(row_text(buffer, 4), "└") > 0, "process table should render child branch")
  call require(index(row_text(buffer, 4), "child --task") > 0, "process table should render child command")

  buffer = allocate_screen(80, 10)
  state = process_table_state(tree_view=.false., selected_row=1)
  call render_process_panel(buffer, widget_rect(1, 1, 80, 10), sample_long_command_snapshot(), border_style, title_style, &
                            dim_style, state)
  call require(state%selected_pid == 100, "process table should capture selected command pid")
  call require(state%selected_command_length > 0, "process table should capture selected command text")
  call require(process_table_toggle_command_detail(state), "command detail should open for selected process")
  buffer = allocate_screen(60, 10)
  call render_process_panel(buffer, widget_rect(1, 1, 60, 10), sample_long_command_snapshot(), border_style, title_style, &
                            dim_style, state)
  call require(buffer_contains(buffer, "Command PID 100"), "command detail should render selected pid title")
  call require(buffer_contains(buffer, "parent --test --alpha"), "command detail should render command start")
  call require(buffer_contains(buffer, "--epsilon"), "command detail should wrap long command")
  call require(buffer_contains(buffer, "F10/Esc close"), "command detail should render close hint")
  call require(index(process_table_status(state), "command detail") > 0, "process status should describe command detail mode")
  call process_table_close_command_detail(state)
  buffer = allocate_screen(60, 10)
  call render_process_panel(buffer, widget_rect(1, 1, 60, 10), sample_long_command_snapshot(), border_style, title_style, &
                            dim_style, state)
  call require(.not. buffer_contains(buffer, "Command PID 100"), "command detail should close")

  buffer = allocate_screen(80, 10)
  state = process_table_state(tree_view=.false.)
  call render_process_panel(buffer, widget_rect(1, 1, 80, 10), sample_history_snapshot(), border_style, title_style, &
                            dim_style, state)
  call require(index(row_text(buffer, 4), "▁") > 0, "process table should render process history sparkline low sample")
  call require(index(row_text(buffer, 4), "█") > 0, "process table should render process history sparkline high sample")

  buffer = allocate_screen(80, 10)
  call process_table_toggle_metric_sparklines(state)
  call render_process_panel(buffer, widget_rect(1, 1, 80, 10), sample_history_snapshot(), border_style, title_style, &
                            dim_style, state)
  call require(index(row_text(buffer, 4), "0.0%") > 0, "process table should render numeric metrics when toggled")
  call require(index(row_text(buffer, 4), "█") == 0, "process table should hide sparklines when toggled to numeric")

  buffer = allocate_screen(80, 10)
  state%selected_row = 2
  call render_process_panel(buffer, widget_rect(1, 1, 80, 10), sample_snapshot(), border_style, title_style, dim_style, &
                            state)
  call require(state%row_count == 2, "process table state should track row count")
  call require(state%viewport_rows > 0, "process table state should track viewport rows")
  call require(row_has_bold(buffer, 4), "process table should highlight selected row")

  state%selected_row = 1
  call require(process_table_select_at(state, widget_rect(1, 1, 80, 10), 4, 5), &
               "process table mouse row hit should select data row")
  call require(state%selected_row == 2, "process table mouse row hit should update selected row")
  call require(.not. process_table_select_at(state, widget_rect(1, 1, 80, 10), 2, 5), &
               "process table mouse row hit should ignore header row")

  state%sort_key = PROCESS_SORT_USER
  state%sort_direction = TABLE_SORT_DESCENDING
  call require(process_table_sort_at(state, widget_rect(1, 1, 80, 10), 2, 45), &
               "process table mouse header hit should sort clicked column")
  call require(state%sort_key == PROCESS_SORT_CPU, "process table mouse header hit should choose CPU sort")
  call require(state%sort_direction == TABLE_SORT_ASCENDING, "process table mouse header hit should reset new sort asc")
  call require(process_table_sort_at(state, widget_rect(1, 1, 80, 10), 2, 45), &
               "process table mouse header hit should toggle clicked column")
  call require(state%sort_direction == TABLE_SORT_DESCENDING, "process table mouse header hit should toggle current sort")

  state = process_table_state()
  call require(process_table_set_columns(state, custom_columns, 4, column_error), &
               "process table should accept configured columns")
  buffer = allocate_screen(80, 10)
  call render_process_panel(buffer, widget_rect(1, 1, 80, 10), sample_snapshot(), border_style, title_style, dim_style, &
                            state)
  call require(index(row_text(buffer, 2), "CPU%") > 0, "configured process columns should render visible columns")
  call require(index(row_text(buffer, 2), "USER") == 0, "configured process columns should hide omitted columns")
  state%sort_key = PROCESS_SORT_COMMAND
  state%sort_direction = TABLE_SORT_DESCENDING
  call require(process_table_sort_at(state, widget_rect(1, 1, 80, 10), 2, 10), &
               "configured process columns should sort clicked visible header")
  call require(state%sort_key == PROCESS_SORT_CPU, "configured process columns should map mouse hit to visible sort")
  call require(.not. process_table_set_columns(state, bad_columns, 2, column_error), &
               "process table should reject unknown configured columns")
  call require(index(column_error, "unknown process column") > 0, &
               "process table should explain unknown configured columns")

  buffer = allocate_screen(80, 10)
  state = process_table_state(tree_view=.false., selected_row=1)
  call process_table_mark_signal_feedback(state, 200, 2000_int64)
  state%signal_feedback_frames = 1
  call render_process_panel(buffer, widget_rect(1, 1, 80, 10), sample_snapshot(), border_style, title_style, dim_style, &
                            state)
  call require(row_has_inverse(buffer, 4), "process table should highlight signaled row")
  call require(state%signal_feedback_pid == 0, "process table signal feedback should expire after final frame")

  buffer = allocate_screen(80, 10)
  state = process_table_state(tree_view=.false., selected_row=1)
  call process_table_mark_signal_feedback(state, 200, 9999_int64)
  call render_process_panel(buffer, widget_rect(1, 1, 80, 10), sample_snapshot(), border_style, title_style, dim_style, &
                            state)
  call require(.not. row_has_inverse(buffer, 4), "process table should not highlight pid reused row")
  call require(state%signal_feedback_pid == 0, "process table should clear feedback for missing process identity")

  buffer = allocate_screen(80, 10)
  state = process_table_state(tree_view=.false., sort_key=PROCESS_SORT_COMMAND)
  call render_process_panel(buffer, widget_rect(1, 1, 80, 10), sample_snapshot(), border_style, title_style, dim_style, &
                            state)
  call require(index(row_text(buffer, 3), "child --task") > 0, "flat process table should sort by command")
  call require(index(row_text(buffer, 3), "└") == 0, "flat process table should omit tree branch")

  buffer = allocate_screen(80, 10)
  state = process_table_state(selected_row=1)
  call render_process_panel(buffer, widget_rect(1, 1, 80, 10), sample_snapshot(), border_style, title_style, dim_style, &
                            state)
  call require(state%selected_pid == 100, "process table should track selected pid")
  call require(state%selected_has_children, "process table should track selected child state")
  toggled = process_table_toggle_selected_node(state)
  call require(toggled, "process table should collapse selected parent")
  buffer = allocate_screen(80, 10)
  call render_process_panel(buffer, widget_rect(1, 1, 80, 10), sample_snapshot(), border_style, title_style, dim_style, &
                            state)
  call require(state%row_count == 1, "process table collapse should hide descendants")
  call require(state%collapsed_count == 1, "process table collapse should track collapsed node")
  call require(index(row_text(buffer, 3), "parent --test") > 0, "collapsed process table should keep parent")
  call require(index(row_text(buffer, 4), "child --task") == 0, "collapsed process table should hide child")
  toggled = process_table_toggle_selected_node(state)
  call require(toggled, "process table should expand selected parent")
  buffer = allocate_screen(80, 10)
  call render_process_panel(buffer, widget_rect(1, 1, 80, 10), sample_snapshot(), border_style, title_style, dim_style, &
                            state)
  call require(state%row_count == 2, "expanded process table should show descendants")

  buffer = allocate_screen(80, 10)
  state = process_table_state(tree_view=.false.)
  call process_table_begin_filter(state)
  call process_table_append_filter_text(state, "CHILD")
  call render_process_panel(buffer, widget_rect(1, 1, 80, 10), sample_snapshot(), border_style, title_style, dim_style, &
                            state)
  call require(state%row_count == 1, "process table filter should update filtered row count")
  call require(state%total_row_count == 2, "process table filter should track total row count")
  call require(index(row_text(buffer, 3), "child --task") > 0, "process table filter should match command")
  call require(index(row_text(buffer, 3), "parent --test") == 0, "process table filter should omit nonmatches")
  call require(index(row_text(buffer, 9), "showing 1 of 2") > 0, "process table should render filtered count")

  buffer = allocate_screen(80, 10)
  state = process_table_state(tree_view=.false.)
  call process_table_append_fuzzy_text(state, "chi")
  call render_process_panel(buffer, widget_rect(1, 1, 80, 10), sample_snapshot(), border_style, title_style, dim_style, state)
  call require(state%selected_pid == 200, "fuzzy query should select matching process command")
  call require(state%fuzzy_match_count == 1, "fuzzy query should track match count")
  call require(index(row_text(buffer, 9), "Find: chi_") > 0, "process table should render fuzzy find bar")

  buffer = allocate_screen(80, 10)
  state = process_table_state(tree_view=.false.)
  call process_table_append_fuzzy_text(state, "200")
  call render_process_panel(buffer, widget_rect(1, 1, 80, 10), sample_snapshot(), border_style, title_style, dim_style, state)
  call require(state%selected_pid == 200, "PID fuzzy query should select exact PID")
  call require(state%fuzzy_is_pid_mode, "numeric fuzzy query should use PID mode")
  call require(index(row_text(buffer, 9), "PID: 200_") > 0, "process table should render PID fuzzy bar")
  call require(index(row_text(buffer, 9), "(exact)") > 0, "process table should render exact PID fuzzy match")

  buffer = allocate_screen(80, 10)
  state = process_table_state(tree_view=.false.)
  call process_table_begin_filter(state)
  call process_table_append_filter_text(state, "child")
  call process_table_finish_filter(state)
  call process_table_append_fuzzy_text(state, "200")
  call render_process_panel(buffer, widget_rect(1, 1, 80, 10), sample_snapshot(), border_style, title_style, dim_style, state)
  call require(state%selected_pid == 200, "fuzzy query should search within filtered process rows")
  call require(state%fuzzy_exact_match, "fuzzy query should track exact PID inside filtered rows")
  call require(index(process_table_status(state), "PID: 200_ (exact)") > 0, &
               "process status should include fuzzy query while filter remains active")

contains

  subroutine test_process_state_updates()
    type(process_table_state) :: local_state
    integer :: signal_number
    logical :: changed

    local_state%row_count = 5
    local_state%viewport_rows = 2
    call process_table_select_delta(local_state, 3)
    call require(local_state%selected_row == 4, "process table selection should move by delta")
    call require(local_state%scroll_row == 3, "process table scroll should follow selection")

    call process_table_page_delta(local_state, -1)
    call require(local_state%selected_row == 2, "process table page movement should use viewport")
    call process_table_scroll_delta(local_state, 2)
    call require(local_state%selected_row == 4, "process table scroll movement should move selection")

    call process_table_cycle_sort_key(local_state, 1)
    call require(local_state%sort_key == PROCESS_SORT_USER, "process table sort key should cycle")
    call process_table_toggle_sort_direction(local_state)
    call require(local_state%sort_direction == TABLE_SORT_DESCENDING, "process table sort direction should toggle")
    call require(index(process_table_status(local_state), "sort user desc") > 0, &
                 "process table status should describe sort state")
    call require(index(process_table_status(local_state), "history spark") > 0, &
                 "process table status should describe sparkline metric state")
    call process_table_toggle_metric_sparklines(local_state)
    call require(index(process_table_status(local_state), "history numeric") > 0, &
                 "process table status should describe numeric metric state")

    call process_table_begin_filter(local_state)
    call process_table_append_filter_text(local_state, "daemon")
    call require(local_state%filter_active, "process table filter should enter edit mode")
    call require(local_state%filter_length == 6, "process table filter should append text")
    call require(index(process_table_status(local_state), "filter daemon") > 0, &
                 "process table status should describe filter state")
    call process_table_move_filter_cursor(local_state, -1)
    call process_table_append_filter_text(local_state, "x")
    call require(index(process_table_status(local_state), "filter daemoxn") > 0, &
                 "process table filter should insert text at lineedit cursor")
    call process_table_delete_filter_right(local_state)
    call require(index(process_table_status(local_state), "filter daemox") > 0, &
                 "process table filter should delete text right of lineedit cursor")
    call process_table_delete_filter_char(local_state)
    call require(local_state%filter_length == 5, "process table filter should delete text")
    call process_table_clear_filter(local_state)
    call require(.not. local_state%filter_active, "process table filter clear should leave edit mode")
    call require(local_state%filter_length == 0, "process table filter clear should remove text")

    local_state%selected_pid = 4242
    call require(process_table_begin_signal(local_state, 15, "SIGTERM", .false.), &
                 "process table should arm selected process signal")
    call require(local_state%signal_pending, "process table signal should be pending")
    call require(index(process_table_signal_status(local_state), "SIGTERM pid 4242") > 0, &
                 "process table signal status should include signal and pid")
    call require(process_table_set_signal(local_state, 9, "SIGKILL", .true.), &
                 "process table should update pending signal")
    call require(.not. local_state%signal_confirmed, "process table dangerous signal should require confirmation")
    call require(index(process_table_signal_status(local_state), "enter to confirm") > 0, &
                 "process table signal status should request confirmation")
    call process_table_confirm_signal(local_state)
    call require(local_state%signal_confirmed, "process table signal confirmation should be recorded")
    call require(index(process_table_signal_status(local_state), "confirmed enter to send") > 0, &
                 "process table signal status should show confirmed send state")
    call process_table_append_signal_digit(local_state, "0", signal_number, changed)
    call require(.not. changed .and. signal_number == 9, "process table should ignore leading zero signal input")
    call process_table_append_signal_digit(local_state, "1", signal_number, changed)
    call process_table_append_signal_digit(local_state, "2", signal_number, changed)
    call require(changed .and. signal_number == 12, "process table should parse typed signal number")
    call process_table_cancel_signal(local_state)
    call require(.not. local_state%signal_pending, "process table signal cancel should clear pending signal")
  end subroutine test_process_state_updates

  subroutine test_process_fuzzy_matching()
    type(collector_snapshot) :: snapshot
    type(process_table) :: table
    type(process_table_state) :: local_state
    integer :: match_count

    snapshot = sample_snapshot()
    table = snapshot%processes
    call require(process_fuzzy_best_match(table, "par", .false., match_count) == 1, &
                 "fuzzy name prefix should match parent")
    call require(match_count == 1, "fuzzy name prefix should count matches")
    call require(process_fuzzy_best_match(table, "200", .true., match_count) == 2, &
                 "fuzzy PID exact should match child")
    call require(process_fuzzy_next_match(table, "t", .false., 1, 1, match_count) == 2, &
                 "fuzzy next should wrap through matches")

    call process_table_append_fuzzy_text(local_state, "k")
    call require(local_state%fuzzy_query_length == 1, "old signal key should be usable as fuzzy query text")
    call process_table_append_fuzzy_text(local_state, "1")
    call require(.not. local_state%fuzzy_is_pid_mode, "mixed fuzzy query should stay in name mode")
    call process_table_delete_fuzzy_char(local_state)
    call require(local_state%fuzzy_query_length == 1, "fuzzy backspace should remove one character")
    call process_table_clear_fuzzy(local_state)
    call require(local_state%fuzzy_query_length == 0, "fuzzy clear should reset query")
    call process_table_step_fuzzy_match(local_state, 1)
    call require(local_state%fuzzy_step_direction == 0, "empty fuzzy query should not set next direction")

    table = fuzzy_scoring_table()
    call require(process_fuzzy_best_match(table, "fire", .false., match_count) == 1, &
                 "fuzzy name prefix should prefer shorter firefox over longer firewalld config")
    call require(process_fuzzy_best_match(table, "worker", .false., match_count) == 3, &
                 "fuzzy word boundary should beat plain substring matches")

    table = fuzzy_pid_table()
    call require(process_fuzzy_best_match(table, "123", .true., match_count) == 2, &
                 "fuzzy PID mode should prefer exact PID over prefix match")
    call require(process_fuzzy_best_match(table, "12", .true., match_count) == 3, &
                 "fuzzy PID prefix should prefer smallest matching PID")
  end subroutine test_process_fuzzy_matching

  subroutine test_process_follow_state()
    type(screen_buffer) :: local_buffer
    type(process_table_state) :: local_state
    type(collector_snapshot) :: snapshot
    logical :: following

    local_buffer = allocate_screen(80, 10)
    local_state = process_table_state(tree_view=.false., selected_row=2)
    snapshot = sample_snapshot()
    call render_process_panel(local_buffer, widget_rect(1, 1, 80, 10), snapshot, border_style, title_style, dim_style, local_state)
    call require(local_state%selected_pid == 200, "follow setup should select child process")
    following = process_table_toggle_follow(local_state)
    call require(following .and. local_state%follow_pid == 200, "follow toggle should capture selected process identity")

    local_state%sort_key = PROCESS_SORT_CPU
    local_state%sort_direction = TABLE_SORT_ASCENDING
    local_buffer = allocate_screen(80, 10)
    call render_process_panel(local_buffer, widget_rect(1, 1, 80, 10), snapshot, border_style, title_style, dim_style, local_state)
    call require(local_state%selected_pid == 200, "follow should keep selected process after sort")
    call require(local_state%selected_row == 1, "follow should update selected row after sort")
    call require(index(process_table_status(local_state), "following PID 200") > 0, "follow status should include followed PID")

    call process_table_begin_filter(local_state)
    call process_table_append_filter_text(local_state, "parent")
    call process_table_finish_filter(local_state)
    local_buffer = allocate_screen(80, 10)
    call render_process_panel(local_buffer, widget_rect(1, 1, 80, 10), snapshot, border_style, title_style, dim_style, local_state)
    call require(local_state%following .and. local_state%follow_filtered, "follow should pause when process is filtered out")
    call require(index(process_table_status(local_state), "following PID 200 filtered") > 0, &
                 "follow status should report filtered process")

    call process_table_clear_filter(local_state)
    snapshot = sample_snapshot()
    snapshot%processes%items(2)%valid = .false.
    local_buffer = allocate_screen(80, 10)
    call render_process_panel(local_buffer, widget_rect(1, 1, 80, 10), snapshot, border_style, title_style, dim_style, local_state)
    call require(.not. local_state%following .and. local_state%follow_exited, "follow should disable when process exits")
    call require(index(process_table_status(local_state), "process 200 exited") > 0, "follow status should report exited process")
  end subroutine test_process_follow_state

  subroutine test_process_tag_state()
    type(screen_buffer) :: local_buffer
    type(process_table_state) :: local_state
    type(collector_snapshot) :: snapshot
    logical :: tagged
    integer :: pid
    integer(int64) :: start_time
    logical :: valid_target

    snapshot = sample_snapshot()
    local_buffer = allocate_screen(80, 10)
    local_state = process_table_state(tree_view=.false., selected_row=1)
    call render_process_panel(local_buffer, widget_rect(1, 1, 80, 10), snapshot, border_style, title_style, dim_style, local_state)

    tagged = process_table_toggle_tag(local_state)
    call require(tagged, "tag toggle should add selected process tag")
    call require(local_state%tag_count == 1, "tag toggle should increment tag count")
    call require(process_table_process_tagged(local_state, 100, 1000_int64), "tagged process should be found by identity")
    call require(local_state%selected_row == 2, "tag toggle should move selection down")
    call require(index(process_table_status(local_state), "1 tagged") > 0, "process status should include tag count")

    local_buffer = allocate_screen(80, 10)
    call render_process_panel(local_buffer, widget_rect(1, 1, 80, 10), snapshot, border_style, title_style, dim_style, local_state)
    call require(index(row_text(local_buffer, 3), "*") > 0, "process table should render tag marker column")

    local_state%selected_row = 1
    call render_process_panel(local_buffer, widget_rect(1, 1, 80, 10), snapshot, border_style, title_style, dim_style, local_state)
    tagged = process_table_toggle_tag(local_state)
    call require(.not. tagged, "tag toggle should remove an existing selected process tag")
    call require(local_state%tag_count == 0, "untag should decrement tag count")

    local_state%selected_row = 1
    call render_process_panel(local_buffer, widget_rect(1, 1, 80, 10), snapshot, border_style, title_style, dim_style, local_state)
    tagged = process_table_toggle_tag(local_state)
    snapshot%processes%items(1)%start_time = 9999_int64
    call process_table_prune_tags(local_state, snapshot%processes)
    call require(local_state%tag_count == 0, "tag pruning should remove stale PID reuse identities")

    snapshot = sample_snapshot()
    local_state%selected_row = 1
    call render_process_panel(local_buffer, widget_rect(1, 1, 80, 10), snapshot, border_style, title_style, dim_style, local_state)
    tagged = process_table_toggle_tag(local_state)
    local_state%selected_row = 2
    call render_process_panel(local_buffer, widget_rect(1, 1, 80, 10), snapshot, border_style, title_style, dim_style, &
                              local_state)
    tagged = process_table_toggle_tag(local_state)
    call require(local_state%tag_count == 2, "tag toggle should support multiple tagged processes")
    call require(process_table_begin_tag_signal(local_state, 15, "SIGTERM"), "tag signal should start for tagged processes")
    call require(process_table_signal_target_count(local_state) == 2, "tag signal should target each tagged process")
    call require(index(process_table_signal_status(local_state), "Send SIGTERM to 2 tagged?") > 0, &
                 "tag signal status should include tagged target count")
    call require(local_state%signal_confirm_required .and. .not. local_state%signal_confirmed, &
                 "tag signal should require confirmation")
    call process_table_confirm_signal(local_state)
    call require(index(process_table_signal_status(local_state), "confirmed enter to send") > 0, &
                 "confirmed tag signal status should request final enter")
    call process_table_signal_target_at(local_state, 2, pid, start_time, valid_target)
    call require(valid_target .and. pid == 200 .and. start_time == 2000_int64, &
                 "tag signal target lookup should return tagged identity")
    call process_table_cancel_signal(local_state)

    call process_table_clear_tags(local_state)
    call require(local_state%tag_count == 0, "clear tags should remove every tag")
  end subroutine test_process_tag_state

  subroutine test_process_tag_stress()
    integer, parameter :: STRESS_COUNT = 100
    type(screen_buffer) :: local_buffer
    type(process_table_state) :: local_state
    type(collector_snapshot) :: snapshot
    integer :: index_value
    integer :: pid
    integer(int64) :: start_time
    logical :: tagged
    logical :: valid_target

    snapshot = stress_snapshot(STRESS_COUNT)
    local_buffer = allocate_screen(100, 20)
    local_state = process_table_state(tree_view=.false., selected_row=1)
    call render_process_panel(local_buffer, widget_rect(1, 1, 100, 20), snapshot, border_style, title_style, dim_style, &
                              local_state)

    do index_value = 1, STRESS_COUNT
      local_state%selected_row = index_value
      call render_process_panel(local_buffer, widget_rect(1, 1, 100, 20), snapshot, border_style, title_style, dim_style, &
                                local_state)
      tagged = process_table_toggle_tag(local_state)
      call require(tagged, "stress tag toggle should add each selected process")
    end do
    call require(local_state%tag_count == STRESS_COUNT, "stress tag toggle should tag 100 processes")
    call require(process_table_begin_tag_signal(local_state, 15, "SIGTERM"), "stress tag signal should start")
    call require(process_table_signal_target_count(local_state) == STRESS_COUNT, &
                 "stress tag signal should target all tagged processes")
    call process_table_signal_target_at(local_state, STRESS_COUNT, pid, start_time, valid_target)
    call require(valid_target .and. pid == 1000 + STRESS_COUNT .and. start_time == int(STRESS_COUNT, int64), &
                 "stress tag signal should enumerate final tagged target")
    call process_table_cancel_signal(local_state)

    snapshot%processes%items(STRESS_COUNT)%start_time = 9999_int64
    call process_table_prune_tags(local_state, snapshot%processes)
    call require(local_state%tag_count == STRESS_COUNT - 1, "stress prune should remove stale tagged process")
  end subroutine test_process_tag_stress

  function fuzzy_scoring_table() result(table)
    type(process_table) :: table

    allocate(table%items(4))
    table%valid = .true.
    table%items(1) = fuzzy_process(101, "firefox", "firefox --profile", "desktop")
    table%items(2) = fuzzy_process(102, "firewalld-config", "firewalld-config", "root")
    table%items(3) = fuzzy_process(103, "daemon", "background-worker", "service")
    table%items(4) = fuzzy_process(104, "systemd", "systemdworker", "root")
  end function fuzzy_scoring_table

  function fuzzy_pid_table() result(table)
    type(process_table) :: table

    allocate(table%items(3))
    table%valid = .true.
    table%items(1) = fuzzy_process(1234, "pid-long", "pid-long", "root")
    table%items(2) = fuzzy_process(123, "pid-exact", "pid-exact", "root")
    table%items(3) = fuzzy_process(120, "pid-small", "pid-small", "root")
  end function fuzzy_pid_table

  function fuzzy_process(pid, name, command, user) result(process)
    integer, intent(in) :: pid
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: command
    character(len=*), intent(in) :: user
    type(process_info) :: process

    process%valid = .true.
    process%pid = pid
    process%name = name
    process%command = command
    process%user_valid = .true.
    process%user = user
  end function fuzzy_process

  function sample_snapshot() result(snapshot)
    type(collector_snapshot) :: snapshot

    snapshot%processes%valid = .true.
    allocate(snapshot%processes%items(2))
    snapshot%processes%items(1)%valid = .true.
    snapshot%processes%items(1)%pid = 100
    snapshot%processes%items(1)%uid = 1001
    snapshot%processes%items(1)%user_valid = .true.
    snapshot%processes%items(1)%user = "tester"
    snapshot%processes%items(1)%name = "parent"
    snapshot%processes%items(1)%command = "parent --test"
    snapshot%processes%items(1)%state = "R"
    snapshot%processes%items(1)%start_time = 1000_int64
    snapshot%processes%items(1)%priority = 20
    snapshot%processes%items(1)%nice = 0
    snapshot%processes%items(1)%cpu_percent = 12.5_real64
    snapshot%processes%items(1)%mem_percent = 1.5_real64
    snapshot%processes%items(1)%cpu_time = 1250_int64
    snapshot%processes%items(1)%mem_virt_bytes = 128_int64 * 1024_int64 * 1024_int64
    snapshot%processes%items(1)%mem_rss_bytes = 64_int64 * 1024_int64 * 1024_int64
    snapshot%processes%items(1)%mem_shared_bytes = 8_int64 * 1024_int64 * 1024_int64
    snapshot%processes%items(2)%valid = .true.
    snapshot%processes%items(2)%pid = 200
    snapshot%processes%items(2)%ppid = 100
    snapshot%processes%items(2)%uid = 1001
    snapshot%processes%items(2)%user_valid = .true.
    snapshot%processes%items(2)%user = "tester"
    snapshot%processes%items(2)%name = "child"
    snapshot%processes%items(2)%command = "child --task"
    snapshot%processes%items(2)%state = "S"
    snapshot%processes%items(2)%start_time = 2000_int64
    snapshot%processes%items(2)%priority = 30
    snapshot%processes%items(2)%nice = 5
    snapshot%processes%items(2)%cpu_time = 2500_int64
    snapshot%processes%items(2)%mem_virt_bytes = 256_int64 * 1024_int64 * 1024_int64
    snapshot%processes%items(2)%mem_rss_bytes = 32_int64 * 1024_int64 * 1024_int64
    snapshot%processes%items(2)%mem_shared_bytes = 4_int64 * 1024_int64 * 1024_int64
  end function sample_snapshot

  function sample_long_command_snapshot() result(snapshot)
    type(collector_snapshot) :: snapshot

    snapshot = sample_snapshot()
    snapshot%processes%items(1)%command = &
      "parent --test --alpha --beta --gamma --delta --epsilon --zeta --eta --theta --iota --kappa"
  end function sample_long_command_snapshot

  function sample_history_snapshot() result(snapshot)
    type(collector_snapshot) :: snapshot

    snapshot = sample_snapshot()
    snapshot%processes%items(2)%history_count = 6
    snapshot%processes%items(2)%cpu_history(1:6) = [0.0_real64, 20.0_real64, 40.0_real64, &
                                                    60.0_real64, 80.0_real64, 100.0_real64]
    snapshot%processes%items(2)%mem_history(1:6) = [100.0_real64, 80.0_real64, 60.0_real64, &
                                                    40.0_real64, 20.0_real64, 0.0_real64]
  end function sample_history_snapshot

  function stress_snapshot(process_count) result(snapshot)
    integer, intent(in) :: process_count
    type(collector_snapshot) :: snapshot
    integer :: index_value
    character(len=32) :: scratch

    snapshot%processes%valid = .true.
    allocate(snapshot%processes%items(max(0, process_count)))
    do index_value = 1, process_count
      write(scratch, '(a,i0)') "stress-", index_value
      snapshot%processes%items(index_value)%valid = .true.
      snapshot%processes%items(index_value)%pid = 1000 + index_value
      snapshot%processes%items(index_value)%uid = 1001
      snapshot%processes%items(index_value)%user_valid = .true.
      snapshot%processes%items(index_value)%user = "tester"
      snapshot%processes%items(index_value)%name = trim(scratch)
      snapshot%processes%items(index_value)%command = trim(scratch)
      snapshot%processes%items(index_value)%state = "S"
      snapshot%processes%items(index_value)%start_time = int(index_value, int64)
      snapshot%processes%items(index_value)%priority = 20
      snapshot%processes%items(index_value)%nice = 0
    end do
  end function stress_snapshot

  function row_text(buffer, row) result(text)
    type(screen_buffer), intent(in) :: buffer
    integer, intent(in) :: row
    character(len=:), allocatable :: text
    integer :: col

    text = ""
    do col = 1, buffer%size%width
      if (allocated(buffer%cells(row, col)%glyph)) then
        text = text // buffer%cells(row, col)%glyph
      else
        text = text // " "
      end if
    end do
  end function row_text

  logical function buffer_contains(buffer, needle) result(found)
    type(screen_buffer), intent(in) :: buffer
    character(len=*), intent(in) :: needle
    integer :: row

    found = .false.
    do row = 1, buffer%size%height
      if (index(row_text(buffer, row), needle) > 0) then
        found = .true.
        return
      end if
    end do
  end function buffer_contains

  logical function row_has_bold(buffer, row) result(found)
    type(screen_buffer), intent(in) :: buffer
    integer, intent(in) :: row
    integer :: col

    found = .false.
    do col = 1, buffer%size%width
      if (buffer%cells(row, col)%style%bold) then
        found = .true.
        return
      end if
    end do
  end function row_has_bold

  logical function row_has_inverse(buffer, row) result(found)
    type(screen_buffer), intent(in) :: buffer
    integer, intent(in) :: row
    integer :: col

    found = .false.
    do col = 1, buffer%size%width
      if (buffer%cells(row, col)%style%inverse) then
        found = .true.
        return
      end if
    end do
  end function row_has_inverse

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

end program test_process_table
