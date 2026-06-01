program test_process_table
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use fgof_screen, only : allocate_screen
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_collector, only : collector_snapshot
  use ftop_color, only : COLOR_UI_ACCENT, COLOR_UI_BORDER, COLOR_UI_DIM, COLOR_UI_PANEL, style_from_rgb
  use ftop_proc_data, only : PROCESS_SORT_COMMAND, PROCESS_SORT_USER
  use ftop_process_table, only : process_table_append_filter_text, process_table_begin_filter, &
                                 process_table_clear_filter, process_table_cycle_sort_key, &
                                 process_table_delete_filter_char, process_table_page_delta, &
                                 process_table_select_delta, process_table_state, process_table_status, &
                                 process_table_toggle_sort_direction, render_process_panel
  use ftop_table, only : TABLE_SORT_DESCENDING
  use ftop_widgets, only : widget_rect
  implicit none

  type(screen_buffer) :: buffer
  type(screen_style) :: border_style
  type(screen_style) :: dim_style
  type(screen_style) :: title_style
  type(process_table_state) :: state

  call test_process_state_updates()

  buffer = allocate_screen(80, 10)
  border_style = style_from_rgb(fg=COLOR_UI_BORDER)
  title_style = style_from_rgb(fg=COLOR_UI_ACCENT, bg=COLOR_UI_PANEL, bold=.true.)
  dim_style = style_from_rgb(fg=COLOR_UI_DIM)

  call render_process_panel(buffer, widget_rect(1, 1, 80, 10), sample_snapshot(), border_style, title_style, dim_style)
  call require(index(row_text(buffer, 2), "PID") > 0, "process table should render PID header")
  call require(index(row_text(buffer, 3), "100") > 0, "process table should render pid")
  call require(index(row_text(buffer, 3), "parent --test") > 0, "process table should render command")
  call require(index(row_text(buffer, 3), "12.5%") > 0, "process table should render cpu percent")
  call require(index(row_text(buffer, 3), "64.0 MiB") > 0, "process table should render RSS")
  call require(index(row_text(buffer, 4), "200") > 0, "process table should render child pid")
  call require(index(row_text(buffer, 4), "└") > 0, "process table should render child branch")
  call require(index(row_text(buffer, 4), "child --task") > 0, "process table should render child command")

  buffer = allocate_screen(80, 10)
  state%selected_row = 2
  call render_process_panel(buffer, widget_rect(1, 1, 80, 10), sample_snapshot(), border_style, title_style, dim_style, &
                            state)
  call require(state%row_count == 2, "process table state should track row count")
  call require(state%viewport_rows > 0, "process table state should track viewport rows")
  call require(row_has_bold(buffer, 4), "process table should highlight selected row")

  buffer = allocate_screen(80, 10)
  state = process_table_state(tree_view=.false., sort_key=PROCESS_SORT_COMMAND)
  call render_process_panel(buffer, widget_rect(1, 1, 80, 10), sample_snapshot(), border_style, title_style, dim_style, &
                            state)
  call require(index(row_text(buffer, 3), "child --task") > 0, "flat process table should sort by command")
  call require(index(row_text(buffer, 3), "└") == 0, "flat process table should omit tree branch")

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

contains

  subroutine test_process_state_updates()
    type(process_table_state) :: local_state

    local_state%row_count = 5
    local_state%viewport_rows = 2
    call process_table_select_delta(local_state, 3)
    call require(local_state%selected_row == 4, "process table selection should move by delta")
    call require(local_state%scroll_row == 3, "process table scroll should follow selection")

    call process_table_page_delta(local_state, -1)
    call require(local_state%selected_row == 2, "process table page movement should use viewport")

    call process_table_cycle_sort_key(local_state, 1)
    call require(local_state%sort_key == PROCESS_SORT_USER, "process table sort key should cycle")
    call process_table_toggle_sort_direction(local_state)
    call require(local_state%sort_direction == TABLE_SORT_DESCENDING, "process table sort direction should toggle")
    call require(index(process_table_status(local_state), "sort user desc") > 0, &
                 "process table status should describe sort state")

    call process_table_begin_filter(local_state)
    call process_table_append_filter_text(local_state, "daemon")
    call require(local_state%filter_active, "process table filter should enter edit mode")
    call require(local_state%filter_length == 6, "process table filter should append text")
    call require(index(process_table_status(local_state), "filter daemon") > 0, &
                 "process table status should describe filter state")
    call process_table_delete_filter_char(local_state)
    call require(local_state%filter_length == 5, "process table filter should delete text")
    call process_table_clear_filter(local_state)
    call require(.not. local_state%filter_active, "process table filter clear should leave edit mode")
    call require(local_state%filter_length == 0, "process table filter clear should remove text")
  end subroutine test_process_state_updates

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
    snapshot%processes%items(1)%cpu_percent = 12.5_real64
    snapshot%processes%items(1)%mem_percent = 1.5_real64
    snapshot%processes%items(1)%mem_rss_bytes = 64_int64 * 1024_int64 * 1024_int64
    snapshot%processes%items(2)%valid = .true.
    snapshot%processes%items(2)%pid = 200
    snapshot%processes%items(2)%ppid = 100
    snapshot%processes%items(2)%uid = 1001
    snapshot%processes%items(2)%user_valid = .true.
    snapshot%processes%items(2)%user = "tester"
    snapshot%processes%items(2)%name = "child"
    snapshot%processes%items(2)%command = "child --task"
    snapshot%processes%items(2)%state = "S"
  end function sample_snapshot

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

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

end program test_process_table
