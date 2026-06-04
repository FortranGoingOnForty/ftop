module ftop_app
  use, intrinsic :: iso_fortran_env, only : error_unit, int64
  use fgof_keys, only : &
    buffer_input, &
    clear_decoder_state, &
    decode_next_event, &
    event_text, &
    has_pending_input
  use fgof_keys_types, only : &
    FGOF_KEY_BACKSPACE, &
    FGOF_KEY_DELETE, &
    FGOF_KEY_DOWN, &
    FGOF_KEY_ENTER, &
    FGOF_KEY_ESCAPE, &
    FGOF_KEY_END, &
    FGOF_KEY_F1, &
    FGOF_KEY_F2, &
    FGOF_KEY_F3, &
    FGOF_KEY_F4, &
    FGOF_KEY_F5, &
    FGOF_KEY_F6, &
    FGOF_KEY_F7, &
    FGOF_KEY_F8, &
    FGOF_KEY_F9, &
    FGOF_KEY_HOME, &
    FGOF_KEY_LEFT, &
    FGOF_KEY_PAGEDOWN, &
    FGOF_KEY_PAGEUP, &
    FGOF_KEY_RIGHT, &
    FGOF_KEY_TAB, &
    FGOF_KEY_UP, &
    key_decoder_state, &
    key_event
  use fgof_screen, only : &
    allocate_screen, &
    render_screen_ansi, &
    render_screen_diff_ansi
  use fgof_screen_types, only : screen_buffer
  use fgof_termios, only : bind_guard, enter_raw_mode, get_terminal_size, restore_guard
  use fgof_termios_types, only : FGOF_TERMIOS_ERR_NONE, terminal_size, termios_guard
  use ftop_collector, only : collector, collector_snapshot
  use ftop_dashboard, only : render_dashboard
  use ftop_help, only : render_help_overlay
  use ftop_layout, only : &
    dashboard_layout, &
    dashboard_layout_from_grid, &
    default_dashboard_grid, &
    default_dashboard_layout, &
    layout_error, &
    layout_focus_count, &
    layout_focus_widget, &
    layout_grid, &
    parse_layout_file
  use ftop_net_data, only : set_network_interface_filters
  use ftop_network, only : &
    network_table_clear_state_filter, &
    network_table_cycle_sort_key, &
    network_table_cycle_state_filter, &
    network_table_page_delta, &
    network_table_select_delta, &
    network_table_state, &
    network_table_status, &
    network_table_toggle_sort_direction
  use ftop_process_table, only : &
    process_table_append_filter_text, &
    process_table_append_fuzzy_text, &
    process_table_append_signal_digit, &
    process_table_begin_filter, &
    process_table_begin_signal, &
    process_table_begin_tag_signal, &
    process_table_cancel_signal, &
    process_table_clear_filter, &
    process_table_clear_fuzzy, &
    process_table_clear_signal_input, &
    process_table_clear_tags, &
    process_table_confirm_signal, &
    process_table_cycle_sort_key, &
    process_table_delete_filter_char, &
    process_table_delete_filter_right, &
    process_table_delete_fuzzy_char, &
    process_table_delete_signal_digit, &
    process_table_finish_filter, &
    process_table_mark_signal_feedback, &
    process_table_move_filter_cursor, &
    process_table_move_filter_end, &
    process_table_move_filter_home, &
    process_table_page_delta, &
    process_table_scroll_delta, &
    process_table_select_at, &
    process_table_select_delta, &
    process_table_set_columns, &
    process_table_set_signal, &
    process_table_signal_target_at, &
    process_table_signal_target_count, &
    process_table_step_fuzzy_match, &
    process_table_signal_status, &
    process_table_sort_at, &
    process_table_state, &
    process_table_status, &
    process_table_toggle_metric_sparklines, &
    process_table_toggle_follow, &
    process_table_toggle_tag, &
    process_table_toggle_selected_node, &
    process_table_toggle_sort_direction, &
    process_table_toggle_tree
  use ftop_signal, only : &
    FTOP_SIGNAL_CONT, &
    FTOP_SIGNAL_HUP, &
    FTOP_SIGNAL_INT, &
    FTOP_SIGNAL_KILL, &
    FTOP_SIGNAL_STOP, &
    FTOP_SIGNAL_TERM, &
    FTOP_SIGNAL_TSTP, &
    FTOP_SIGNAL_USR1, &
    FTOP_SIGNAL_USR2, &
    FTOP_SIGNAL_WINCH, &
    terminal_signal_clear => ftop_signal_clear, &
    terminal_kill => ftop_kill, &
    terminal_signal_pending => ftop_signal_check, &
    terminal_signal_number => ftop_signal_number, &
    terminal_signal_setup => ftop_signal_setup, &
    terminal_signal_suspend_self => ftop_signal_suspend_self
  use ftop_terminal_io, only : &
    read_terminal_input, &
    terminal_read_result, &
    write_terminal_output
  use ftop_widgets, only : widget_rect
  implicit none
  private

  integer, parameter :: DEFAULT_ROWS = 24
  integer, parameter :: DEFAULT_COLUMNS = 80
  integer, parameter :: DEFAULT_REFRESH_MS = 1000
  integer, parameter :: MIN_REFRESH_MS = 100
  integer, parameter :: MAX_REFRESH_MS = 60000
  integer, parameter :: DOUBLE_CLICK_MS = 500
  integer, parameter :: PROCESS_SIGNAL_CHOICE_COUNT = 7
  integer, parameter :: LAYOUT_PRESET_COUNT = 4
  integer, parameter :: LAYOUT_PRESET_NAME_LEN = 16
  integer, parameter :: LAYOUT_PRESET_FILE_LEN = 40
  character(len=*), parameter :: DEBUG_LOG_PATH = "ftop-debug.log"
  character(len=LAYOUT_PRESET_NAME_LEN), parameter :: LAYOUT_PRESET_NAMES(LAYOUT_PRESET_COUNT) = [ &
    character(len=LAYOUT_PRESET_NAME_LEN) :: &
    "full", &
    "compact", &
    "process", &
    "network" &
  ]
  character(len=LAYOUT_PRESET_FILE_LEN), parameter :: LAYOUT_PRESET_FILES(LAYOUT_PRESET_COUNT) = [ &
    character(len=LAYOUT_PRESET_FILE_LEN) :: &
    "default.toml", &
    "compact.toml", &
    "process-focused.toml", &
    "network-focused.toml" &
  ]

  type :: terminal_session
    type(termios_guard) :: guard
    type(screen_buffer) :: current
    type(screen_buffer) :: previous
    type(collector), allocatable :: metrics
    type(collector_snapshot) :: last_snapshot
    type(layout_grid) :: layout
    type(key_decoder_state) :: decoder
    type(network_table_state) :: network_state
    type(process_table_state) :: process_state
    character(len=:), allocatable :: config_path
    character(len=LAYOUT_PRESET_NAME_LEN) :: layout_preset_name = "full"
    character(len=:), allocatable :: status_text
    integer :: refresh_ms = DEFAULT_REFRESH_MS
    integer :: frame_count = 0
    integer :: focus_index = 1
    integer :: last_focus_index = 0
    integer :: layout_preset_index = 1
    integer :: last_render_count = 0
    integer :: last_refresh_count = 0
    integer :: last_process_click_count = 0
    integer :: last_process_click_row = 0
    integer :: clock_rate = 0
    real :: render_fps = 0.0
    logical :: guard_bound = .false.
    logical :: terminal_started = .false.
    logical :: collector_started = .false.
    logical :: layout_loaded = .false.
    logical :: zoomed = .false.
    logical :: help_visible = .false.
    logical :: paused = .false.
    logical :: running = .true.
    logical :: needs_full_render = .true.
    logical :: dirty = .true.
  end type terminal_session

  type :: sgr_mouse_event
    integer :: code = 0
    integer :: row = 0
    integer :: col = 0
    logical :: pressed = .false.
  end type sgr_mouse_event

  public :: run_ftop
  public :: render_test_frame
  public :: adjusted_refresh_ms
  public :: process_batch_signal_status
  public :: refresh_status_text
  public :: process_vim_navigation_delta
  public :: select_draw_snapshot
  public :: summarize_batch_signal_results
  public :: vim_navigation_delta

contains

  integer function run_ftop(refresh_ms, config_path) result(status)
    integer, intent(in), optional :: refresh_ms
    character(len=*), intent(in), optional :: config_path
    type(terminal_session) :: session
    type(terminal_read_result) :: input

    if (present(refresh_ms)) session%refresh_ms = bounded_refresh_ms(refresh_ms)
    if (present(config_path)) then
      if (len_trim(config_path) > 0) session%config_path = trim(config_path)
    end if

    status = start_terminal_session(session)
    if (status /= 0) then
      call stop_terminal_session(session)
      return
    end if

    call render_session(session)
    do while (session%running)
      call handle_pending_signals(session)
      if (.not. session%running) exit

      input = read_terminal_input(poll_timeout_ms(session))
      if (input%failed) then
        call set_status(session, "input read failed: errno=" // integer_text(input%error_code))
        session%running = .false.
      else if (input%has_data) then
        call handle_input(session, input%bytes)
      end if

      call handle_pending_signals(session)
      if (refresh_due(session)) call mark_refresh(session)
      if (session%dirty) call render_session(session)
    end do

    call stop_terminal_session(session)
  end function run_ftop

  function render_test_frame(width, height, refresh_ms, frame_count, status_text) result(rendered)
    integer, intent(in) :: width
    integer, intent(in) :: height
    integer, intent(in) :: refresh_ms
    integer, intent(in) :: frame_count
    character(len=*), intent(in) :: status_text
    character(len=:), allocatable :: rendered
    type(terminal_session) :: session

    session%current = allocate_screen(width, height)
    session%refresh_ms = refresh_ms
    session%frame_count = frame_count
    session%render_fps = target_fps(refresh_ms)
    session%status_text = status_text
    call draw_frame(session)
    rendered = render_screen_ansi(session%current)
  end function render_test_frame

  integer function start_terminal_session(session) result(status)
    type(terminal_session), intent(inout) :: session

    status = 0
    session%decoder = clear_decoder_state()
    call set_status(session, "ready")
    call initialize_refresh_timer(session)
    call initialize_layout(session)

    call bind_guard(session%guard)
    if (session%guard%last_error_code /= FGOF_TERMIOS_ERR_NONE) then
      call print_terminal_error("failed to bind terminal", session%guard%last_error_message)
      status = 1
      return
    end if
    session%guard_bound = .true.

    call enter_raw_mode(session%guard)
    if (session%guard%last_error_code /= FGOF_TERMIOS_ERR_NONE) then
      call print_terminal_error("failed to enter raw mode", session%guard%last_error_message)
      status = 1
      return
    end if

    if (.not. terminal_signal_setup()) then
      write(error_unit, '(a)') "ftop: failed to install signal handlers"
      status = 1
      return
    end if

    if (.not. allocated(session%metrics)) allocate(session%metrics)
    if (session%metrics%start(session%refresh_ms)) then
      session%collector_started = .true.
    else
      call set_status(session, "collector unavailable")
    end if

    call write_terminal_output(enter_terminal_control_sequence())
    session%terminal_started = .true.
    call resize_session_to_terminal(session)
  end function start_terminal_session

  subroutine stop_terminal_session(session)
    type(terminal_session), intent(inout) :: session
    logical :: ignored

    if (session%terminal_started) then
      call write_terminal_output(leave_terminal_control_sequence())
      session%terminal_started = .false.
    end if

    if (allocated(session%metrics)) then
      if (session%metrics%initialized()) ignored = session%metrics%destroy()
      deallocate(session%metrics)
      session%collector_started = .false.
    end if

    if (session%guard_bound) then
      call restore_guard(session%guard)
      session%guard_bound = .false.
    end if
  end subroutine stop_terminal_session

  subroutine render_session(session)
    type(terminal_session), intent(inout) :: session
    character(len=:), allocatable :: output

    call draw_frame(session)
    if (session%needs_full_render) then
      output = render_screen_ansi(session%current)
      session%needs_full_render = .false.
    else
      output = render_screen_diff_ansi(session%previous, session%current)
    end if

    call write_terminal_output(output)
    call update_render_fps(session)
    session%previous = session%current
    session%dirty = .false.
  end subroutine render_session

  subroutine draw_frame(session)
    type(terminal_session), intent(inout) :: session
    type(collector_snapshot) :: snapshot
    type(collector_snapshot) :: current_snapshot
    character(len=:), allocatable :: focus
    logical :: has_current_snapshot
    logical :: update_last_snapshot

    has_current_snapshot = .false.
    if (session%paused) then
      snapshot = session%last_snapshot
    else if (allocated(session%metrics)) then
      if (session%metrics%initialized()) then
        current_snapshot = session%metrics%snapshot()
        has_current_snapshot = .true.
      end if
    end if
    if (session%paused .or. has_current_snapshot) then
      snapshot = select_draw_snapshot(session%paused, session%last_snapshot, current_snapshot, update_last_snapshot)
      if (update_last_snapshot) session%last_snapshot = snapshot
    end if
    focus = focused_widget_name(session)
    if (session%layout_loaded) then
      call render_dashboard(session%current, snapshot, session%refresh_ms, session%frame_count, &
                               session%status_text, session%layout, focus, session%zoomed, &
                               layout_name=trim(session%layout_preset_name), render_fps=session%render_fps, &
                               process_state=session%process_state, &
                               network_state=session%network_state, paused=session%paused)
    else
      call render_dashboard(session%current, snapshot, session%refresh_ms, session%frame_count, &
                               session%status_text, focused_widget=focus, zoomed=session%zoomed, &
                               layout_name=trim(session%layout_preset_name), render_fps=session%render_fps, &
                               process_state=session%process_state, &
                               network_state=session%network_state, paused=session%paused)
    end if
    if (session%help_visible) call render_help_overlay(session%current, focus)
  end subroutine draw_frame

  function select_draw_snapshot(paused, last_snapshot, current_snapshot, update_last_snapshot) result(snapshot)
    logical, intent(in) :: paused
    type(collector_snapshot), intent(in) :: last_snapshot
    type(collector_snapshot), intent(in) :: current_snapshot
    logical, intent(out) :: update_last_snapshot
    type(collector_snapshot) :: snapshot

    update_last_snapshot = .not. paused
    if (paused) then
      snapshot = last_snapshot
    else
      snapshot = current_snapshot
    end if
  end function select_draw_snapshot

  subroutine initialize_layout(session)
    type(terminal_session), intent(inout) :: session
    type(layout_error) :: error
    character(len=:), allocatable :: path
    character(len=:), allocatable :: process_error

    if (allocated(session%config_path)) then
      session%layout_preset_index = 0
      session%layout_preset_name = "custom"
      path = session%config_path
      call parse_layout_file(path, session%layout, error)
      if (error%failed) then
        call set_status(session, "layout config failed: " // error%message)
      else if (.not. apply_layout_config(session, process_error)) then
        call set_status(session, "layout config failed: " // process_error)
      else
        session%layout_loaded = .true.
        call set_status(session, "layout config " // path)
      end if
      return
    end if

    path = discover_user_layout_config_path()
    if (len(path) > 0) then
      session%layout_preset_index = 0
      session%layout_preset_name = "custom"
      call parse_layout_file(path, session%layout, error)
      if (error%failed) then
        call set_status(session, "layout config failed: " // error%message)
      else if (.not. apply_layout_config(session, process_error)) then
        call set_status(session, "layout config failed: " // process_error)
      else
        session%layout_loaded = .true.
        call set_status(session, "layout custom")
      end if
      return
    end if

    if (load_layout_preset(session, 1, process_error)) then
      call set_status(session, "layout " // trim(session%layout_preset_name))
    else
      call set_status(session, process_error)
    end if
  end subroutine initialize_layout

  logical function load_layout_preset(session, preset_index, error_message) result(loaded)
    type(terminal_session), intent(inout) :: session
    integer, intent(in) :: preset_index
    character(len=:), allocatable, intent(out) :: error_message
    type(layout_error) :: error
    character(len=:), allocatable :: path
    character(len=:), allocatable :: process_error

    loaded = .false.
    error_message = ""
    if (preset_index < 1 .or. preset_index > LAYOUT_PRESET_COUNT) then
      error_message = "layout preset unavailable"
      return
    end if

    path = discover_layout_preset_path(trim(LAYOUT_PRESET_FILES(preset_index)))
    if (len(path) == 0) then
      error_message = "layout preset missing: " // trim(LAYOUT_PRESET_FILES(preset_index))
      return
    end if

    call parse_layout_file(path, session%layout, error)
    if (error%failed) then
      error_message = "layout preset failed: " // error%message
      return
    end if
    if (.not. apply_layout_config(session, process_error)) then
      error_message = "layout preset failed: " // process_error
      return
    end if

    session%layout_loaded = .true.
    session%layout_preset_index = preset_index
    session%layout_preset_name = LAYOUT_PRESET_NAMES(preset_index)
    loaded = .true.
  end function load_layout_preset

  function discover_user_layout_config_path() result(path)
    character(len=:), allocatable :: path
    character(len=:), allocatable :: config_home
    character(len=:), allocatable :: home

    config_home = environment_value("XDG_CONFIG_HOME")
    if (len(config_home) > 0) then
      path = join_path(config_home, "ftop/config.toml")
      if (path_exists(path)) return
    end if

    home = environment_value("HOME")
    if (len(home) > 0) then
      path = join_path(home, ".config/ftop/config.toml")
      if (path_exists(path)) return
    end if

    path = ""
  end function discover_user_layout_config_path

  subroutine switch_layout_preset(session, preset_index)
    type(terminal_session), intent(inout) :: session
    integer, intent(in) :: preset_index
    character(len=:), allocatable :: error_message
    character(len=:), allocatable :: old_focus
    logical :: ignored_focus
    logical :: old_zoomed

    old_focus = focused_widget_name(session)
    old_zoomed = session%zoomed
    if (.not. load_layout_preset(session, preset_index, error_message)) then
      call set_status(session, error_message)
      return
    end if

    session%focus_index = 1
    session%last_focus_index = 0
    if (old_zoomed .and. layout_contains_widget(session%layout, old_focus)) then
      session%zoomed = .false.
      ignored_focus = focus_widget_named(session, old_focus)
      session%zoomed = .true.
    else
      session%zoomed = .false.
    end if
    call set_status(session, "layout " // trim(session%layout_preset_name))
    session%needs_full_render = .true.
    session%dirty = .true.
  end subroutine switch_layout_preset

  subroutine cycle_layout_preset(session)
    type(terminal_session), intent(inout) :: session
    integer :: preset_index

    if (session%layout_preset_index <= 0) then
      preset_index = 1
    else
      preset_index = modulo(session%layout_preset_index, LAYOUT_PRESET_COUNT) + 1
    end if
    call switch_layout_preset(session, preset_index)
  end subroutine cycle_layout_preset

  logical function layout_contains_widget(grid, widget) result(contains_widget)
    type(layout_grid), intent(in) :: grid
    character(len=*), intent(in) :: widget
    integer :: count
    integer :: index

    contains_widget = .false.
    if (len_trim(widget) == 0) return
    count = layout_focus_count(grid)
    do index = 1, count
      if (layout_focus_widget(grid, index) == trim(widget)) then
        contains_widget = .true.
        return
      end if
    end do
  end function layout_contains_widget

  logical function apply_layout_config(session, error_message) result(applied)
    type(terminal_session), intent(inout) :: session
    character(len=:), allocatable, intent(out) :: error_message

    applied = .true.
    error_message = ""
    call set_network_interface_filters(session%layout%network)
    if (session%layout%process%column_count <= 0) return

    applied = process_table_set_columns(session%process_state, &
                                        session%layout%process%columns(:session%layout%process%column_count), &
                                        session%layout%process%column_count, error_message)
  end function apply_layout_config

  function discover_layout_preset_path(file_name) result(path)
    character(len=*), intent(in) :: file_name
    character(len=:), allocatable :: path
    character(len=:), allocatable :: executable_dir
    character(len=:), allocatable :: source_config

    source_config = join_path("config", trim(file_name))
    path = source_config
    if (path_exists(path)) return

    executable_dir = executable_directory()
    if (len(executable_dir) > 0) then
      path = join_path(join_path(executable_dir, "config"), trim(file_name))
      if (path_exists(path)) return
      path = join_path(join_path(executable_dir, "../config"), trim(file_name))
      if (path_exists(path)) return
      path = join_path(join_path(executable_dir, "../../config"), trim(file_name))
      if (path_exists(path)) return
      path = join_path(join_path(executable_dir, "../share/ftop"), trim(file_name))
      if (path_exists(path)) return
    end if

    path = ""
  end function discover_layout_preset_path

  function executable_directory() result(directory)
    character(len=:), allocatable :: directory
    character(len=4096) :: argument
    integer :: slash_index

    call get_command_argument(0, argument)
    slash_index = last_index(trim(argument), "/")
    if (slash_index > 0) then
      directory = trim(argument(:slash_index - 1))
    else
      directory = ""
    end if
  end function executable_directory

  function environment_value(name) result(value)
    character(len=*), intent(in) :: name
    character(len=:), allocatable :: value
    character(len=4096) :: scratch
    integer :: length
    integer :: status

    call get_environment_variable(name, scratch, length=length, status=status)
    if (status == 0 .and. length > 0) then
      value = scratch(:min(length, len(scratch)))
    else
      value = ""
    end if
  end function environment_value

  integer function last_index(text, needle) result(position)
    character(len=*), intent(in) :: text
    character(len=*), intent(in) :: needle
    integer :: index

    position = 0
    if (len(needle) <= 0) return
    do index = len_trim(text) - len(needle) + 1, 1, -1
      if (text(index:index + len(needle) - 1) == needle) then
        position = index
        return
      end if
    end do
  end function last_index

  function join_path(root, relative) result(path)
    character(len=*), intent(in) :: root
    character(len=*), intent(in) :: relative
    character(len=:), allocatable :: path
    integer :: root_len

    root_len = len_trim(root)
    if (root_len <= 0) then
      path = trim(relative)
    else if (root(root_len:root_len) == "/") then
      path = root(:root_len) // trim(relative)
    else
      path = root(:root_len) // "/" // trim(relative)
    end if
  end function join_path

  logical function path_exists(path) result(exists)
    character(len=*), intent(in) :: path

    inquire(file=path, exist=exists)
  end function path_exists

  integer function current_focus_count(session) result(count)
    type(terminal_session), intent(in) :: session
    type(layout_grid) :: fallback_grid

    if (session%layout_loaded) then
      count = layout_focus_count(session%layout)
    else
      fallback_grid = default_dashboard_grid(stacked=session%current%size%width < 96)
      count = layout_focus_count(fallback_grid)
    end if
  end function current_focus_count

  function focused_widget_name(session) result(widget)
    type(terminal_session), intent(in) :: session
    character(len=:), allocatable :: widget
    type(layout_grid) :: fallback_grid
    integer :: count
    integer :: index

    widget = ""
    count = current_focus_count(session)
    if (count <= 0) return
    index = max(1, min(count, session%focus_index))
    if (session%layout_loaded) then
      widget = layout_focus_widget(session%layout, index)
    else
        fallback_grid = default_dashboard_grid(stacked=session%current%size%width < 96)
      widget = layout_focus_widget(fallback_grid, index)
    end if
  end function focused_widget_name

  function current_process_panel_rect(session) result(rect)
    type(terminal_session), intent(in) :: session
    type(widget_rect) :: rect
    type(dashboard_layout) :: layout
    integer :: height
    integer :: width

    width = session%current%size%width
    height = session%current%size%height
    rect = widget_rect(0, 0, 0, 0)
    if (width <= 0 .or. height <= 0) return

    if (session%zoomed) then
      if (focused_widget_name(session) == "process") then
        rect = widget_rect(3, 3, max(0, width - 4), max(0, height - 5))
      end if
      return
    end if

    if (session%layout_loaded) then
      layout = dashboard_layout_from_grid(width, height, session%layout)
    else
      layout = default_dashboard_layout(width, height)
    end if
    rect = layout%process_panel
  end function current_process_panel_rect

  logical function focus_widget_named(session, name) result(focused)
    type(terminal_session), intent(inout) :: session
    character(len=*), intent(in) :: name
    type(layout_grid) :: fallback_grid
    character(len=:), allocatable :: widget
    integer :: count
    integer :: index

    focused = .false.
    if (session%zoomed) return
    count = current_focus_count(session)
    do index = 1, count
      if (session%layout_loaded) then
        widget = layout_focus_widget(session%layout, index)
      else
        fallback_grid = default_dashboard_grid(stacked=session%current%size%width < 96)
        widget = layout_focus_widget(fallback_grid, index)
      end if
      if (widget == name) then
        call set_focus_index(session, index)
        focused = .true.
        return
      end if
    end do
  end function focus_widget_named

  subroutine set_focus_index(session, index)
    type(terminal_session), intent(inout) :: session
    integer, intent(in) :: index
    integer :: count
    integer :: bounded

    count = current_focus_count(session)
    if (count <= 0) return
    bounded = max(1, min(count, index))
    if (bounded /= session%focus_index) session%last_focus_index = session%focus_index
    session%focus_index = bounded
  end subroutine set_focus_index

  subroutine cycle_focus(session, direction)
    type(terminal_session), intent(inout) :: session
    integer, intent(in) :: direction
    integer :: count

    if (session%zoomed) then
      call set_status(session, "zoom active")
      return
    end if

    count = current_focus_count(session)
    if (count <= 0) then
      call set_status(session, "no focusable widgets")
      return
    end if

    call set_focus_index(session, modulo(session%focus_index - 1 + direction, count) + 1)
    call set_status(session, "focus " // focused_widget_name(session))
  end subroutine cycle_focus

  subroutine focus_last_widget(session)
    type(terminal_session), intent(inout) :: session
    integer :: count

    count = current_focus_count(session)
    if (count <= 0) then
      call set_status(session, "no focusable widgets")
      return
    end if
    if (session%last_focus_index >= 1 .and. session%last_focus_index <= count .and. &
        session%last_focus_index /= session%focus_index) then
      call set_focus_index(session, session%last_focus_index)
      call set_status(session, "focus " // focused_widget_name(session))
    else
      call cycle_focus(session, -1)
    end if
  end subroutine focus_last_widget

  subroutine toggle_zoom(session)
    type(terminal_session), intent(inout) :: session
    character(len=:), allocatable :: focus

    focus = focused_widget_name(session)
    if (len(focus) == 0) then
      call set_status(session, "no focusable widgets")
      return
    end if

    session%zoomed = .not. session%zoomed
    if (session%zoomed) then
      call set_status(session, "zoom " // focus)
    else
      call set_status(session, "grid " // focus)
    end if
  end subroutine toggle_zoom

  subroutine handle_input(session, bytes)
    type(terminal_session), intent(inout) :: session
    character(len=*), intent(in) :: bytes
    type(key_event) :: event
    type(sgr_mouse_event) :: mouse
    character(len=:), allocatable :: input_bytes
    character(len=:), allocatable :: mouse_message
    character(len=:), allocatable :: remaining_bytes

    input_bytes = bytes
    do while (len(input_bytes) > 0)
      if (.not. parse_sgr_mouse(input_bytes, mouse, mouse_message, remaining_bytes)) exit
      call log_debug(mouse_message)
      call handle_mouse_event(session, mouse, mouse_message)
      input_bytes = remaining_bytes
    end do
    if (len(input_bytes) == 0) return

    if (session%help_visible .and. (input_bytes == achar(27) .or. input_bytes == "?")) then
      session%help_visible = .false.
      call set_status(session, "help closed")
      return
    end if

    if (input_bytes == achar(27) // "[Z") then
      call focus_last_widget(session)
      return
    end if

    call buffer_input(session%decoder, input_bytes)
    do while (has_pending_input(session%decoder))
      event = decode_next_event(session%decoder)
      if (event%incomplete) exit
      call handle_key_event(session, event)
      if (.not. session%running) exit
    end do
  end subroutine handle_input

  subroutine handle_mouse_event(session, mouse, fallback_message)
    type(terminal_session), intent(inout) :: session
    type(sgr_mouse_event), intent(in) :: mouse
    character(len=*), intent(in) :: fallback_message

    if (handle_process_mouse_event(session, mouse)) return
    call set_status(session, fallback_message)
  end subroutine handle_mouse_event

  logical function handle_process_mouse_event(session, mouse) result(handled)
    type(terminal_session), intent(inout) :: session
    type(sgr_mouse_event), intent(in) :: mouse
    type(widget_rect) :: panel
    logical :: double_clicked
    logical :: ignored_focus
    logical :: node_toggled
    logical :: selected
    integer :: scroll_step

    handled = .false.
    panel = current_process_panel_rect(session)
    if (.not. point_in_rect(panel, mouse%row, mouse%col)) return
    node_toggled = .false.

    if (mouse_scroll_up(mouse)) then
      scroll_step = -max(1, session%process_state%viewport_rows / 3)
      call clear_process_click(session)
      ignored_focus = focus_widget_named(session, "process")
      call process_table_scroll_delta(session%process_state, scroll_step)
    else if (mouse_scroll_down(mouse)) then
      scroll_step = max(1, session%process_state%viewport_rows / 3)
      call clear_process_click(session)
      ignored_focus = focus_widget_named(session, "process")
      call process_table_scroll_delta(session%process_state, scroll_step)
    else if (mouse_left_press(mouse)) then
      double_clicked = process_mouse_double_click(session, mouse)
      ignored_focus = focus_widget_named(session, "process")
      if (.not. process_table_sort_at(session%process_state, panel, mouse%row, mouse%col)) then
        selected = process_table_select_at(session%process_state, panel, mouse%row, mouse%col)
        if (.not. selected) then
          handled = .true.
          call clear_process_click(session)
          call set_status(session, "focus process")
          return
        end if
        if (double_clicked) then
          node_toggled = process_table_toggle_selected_node(session%process_state)
          if (node_toggled) call clear_process_click(session)
        end if
        if (.not. node_toggled) call remember_process_click(session, mouse)
      else
        call clear_process_click(session)
      end if
    else
      return
    end if

    handled = .true.
    call set_status(session, process_table_status(session%process_state))
  end function handle_process_mouse_event

  logical function process_mouse_double_click(session, mouse) result(double_clicked)
    type(terminal_session), intent(in) :: session
    type(sgr_mouse_event), intent(in) :: mouse
    integer :: elapsed_ms
    integer :: now_count
    integer :: rate

    double_clicked = .false.
    if (session%last_process_click_count <= 0) return
    if (session%last_process_click_row /= mouse%row) return

    call system_clock(now_count, rate)
    if (rate <= 0) rate = session%clock_rate
    if (rate <= 0) return

    elapsed_ms = int((real(now_count - session%last_process_click_count) / real(rate)) * 1000.0)
    double_clicked = elapsed_ms >= 0 .and. elapsed_ms <= DOUBLE_CLICK_MS
  end function process_mouse_double_click

  subroutine remember_process_click(session, mouse)
    type(terminal_session), intent(inout) :: session
    type(sgr_mouse_event), intent(in) :: mouse

    session%last_process_click_row = mouse%row
    call system_clock(session%last_process_click_count)
  end subroutine remember_process_click

  subroutine clear_process_click(session)
    type(terminal_session), intent(inout) :: session

    session%last_process_click_count = 0
    session%last_process_click_row = 0
  end subroutine clear_process_click

  subroutine handle_key_event(session, event)
    type(terminal_session), intent(inout) :: session
    type(key_event), intent(in) :: event
    character(len=:), allocatable :: text

    if (event%printable) then
      text = event_text(event)
      if (event%modifiers%ctrl .and. text == "c") then
        call set_status(session, "Ctrl+C")
        session%running = .false.
      else if (event%modifiers%ctrl .and. text == "z") then
        call suspend_session(session)
      else if (event%modifiers%ctrl .and. text == "l") then
        session%needs_full_render = .true.
        call set_status(session, "redraw")
      else if (event%modifiers%ctrl .and. text == "r") then
        call force_refresh(session)
      else if (event%modifiers%ctrl) then
        if (len(text) > 0) call set_status(session, "key: " // text)
      else if (handle_help_printable_key(session, text)) then
        continue
      else if (session%help_visible) then
        continue
      else if (handle_refresh_printable_key(session, text)) then
        continue
      else if (handle_global_printable_key(session, text)) then
        continue
      else if (handle_process_printable_key(session, text)) then
        continue
      else if (handle_network_printable_key(session, text)) then
        continue
      else if (text == "P") then
        call cycle_layout_preset(session)
      else if (handle_layout_preset_key(session, text)) then
        continue
      else if (text == "z") then
        call toggle_zoom(session)
      else if (text == "q") then
        call set_status(session, "q")
        session%running = .false.
      else if (len(text) > 0) then
        call set_status(session, "key: " // text)
      end if
      return
    end if

    if (.not. allocated(event%key_name)) return
    if (handle_help_named_key(session, event%key_name)) return
    if (session%help_visible) return
    if (event%key_name == FGOF_KEY_F7) then
      call toggle_pause(session)
      return
    end if
    if (handle_process_named_key(session, event%key_name)) return
    if (handle_network_named_key(session, event%key_name)) return
    select case (event%key_name)
    case (FGOF_KEY_UP, FGOF_KEY_DOWN, FGOF_KEY_LEFT, FGOF_KEY_RIGHT)
      call set_status(session, "arrow: " // event%key_name)
    case (FGOF_KEY_ENTER)
      call toggle_zoom(session)
    case (FGOF_KEY_TAB)
      if (event%modifiers%shift) then
        call focus_last_widget(session)
      else
        call cycle_focus(session, 1)
      end if
    case (FGOF_KEY_ESCAPE)
      call set_status(session, "escape")
    case default
      call set_status(session, "key: " // event%key_name)
    end select
  end subroutine handle_key_event

  logical function handle_global_printable_key(session, text) result(handled)
    type(terminal_session), intent(inout) :: session
    character(len=*), intent(in) :: text

    handled = .false.
    if (text_input_active(session)) return
    select case (text)
    case ("c")
      call jump_to_widget(session, "cpu", "CPU")
    case ("m")
      call jump_to_widget(session, "memory", "Memory")
    case ("n")
      call jump_to_widget(session, "network", "Network")
    case ("p")
      call jump_to_widget(session, "process", "Process")
    case ("d")
      call set_status(session, "Disk panel not yet available")
    case ("g")
      call set_status(session, "GPU panel not yet available")
    case default
      return
    end select
    handled = .true.
  end function handle_global_printable_key

  subroutine jump_to_widget(session, widget, label)
    type(terminal_session), intent(inout) :: session
    character(len=*), intent(in) :: widget
    character(len=*), intent(in) :: label
    logical :: focused

    if (session%zoomed) session%zoomed = .false.
    focused = focus_widget_named(session, widget)
    if (focused) then
      call set_status(session, "focus " // trim(label))
    else
      call set_status(session, "Switch to full layout first")
    end if
  end subroutine jump_to_widget

  logical function text_input_active(session) result(active)
    type(terminal_session), intent(in) :: session

    active = session%process_state%filter_active .or. session%process_state%signal_pending
  end function text_input_active

  logical function handle_help_printable_key(session, text) result(handled)
    type(terminal_session), intent(inout) :: session
    character(len=*), intent(in) :: text

    handled = .false.
    if (text /= "?") return
    session%help_visible = .not. session%help_visible
    if (session%help_visible) then
      call set_status(session, "help")
    else
      call set_status(session, "help closed")
    end if
    handled = .true.
  end function handle_help_printable_key

  logical function handle_help_named_key(session, key_name) result(handled)
    type(terminal_session), intent(inout) :: session
    character(len=*), intent(in) :: key_name

    handled = .false.
    select case (key_name)
    case (FGOF_KEY_F1)
      session%help_visible = .not. session%help_visible
      if (session%help_visible) then
        call set_status(session, "help")
      else
        call set_status(session, "help closed")
      end if
      handled = .true.
    case (FGOF_KEY_ESCAPE)
      if (.not. session%help_visible) return
      session%help_visible = .false.
      call set_status(session, "help closed")
      handled = .true.
    end select
  end function handle_help_named_key

  subroutine force_refresh(session)
    type(terminal_session), intent(inout) :: session
    type(collector_snapshot) :: snapshot

    if (.not. session%paused .and. allocated(session%metrics)) then
      if (session%metrics%initialized()) then
        snapshot = session%metrics%snapshot()
        session%last_snapshot = snapshot
      end if
    end if
    session%frame_count = session%frame_count + 1
    call system_clock(session%last_refresh_count)
    call set_status(session, "refresh")
  end subroutine force_refresh

  subroutine toggle_pause(session)
    type(terminal_session), intent(inout) :: session

    session%paused = .not. session%paused
    if (session%paused) then
      call set_status(session, "Paused")
    else
      call set_status(session, "Resumed")
      call mark_refresh(session)
    end if
    session%needs_full_render = .true.
  end subroutine toggle_pause

  logical function handle_refresh_printable_key(session, text) result(handled)
    type(terminal_session), intent(inout) :: session
    character(len=*), intent(in) :: text
    integer :: new_refresh_ms

    handled = .false.
    if (text_input_active(session)) return
    select case (text)
    case ("+", "=")
      new_refresh_ms = adjusted_refresh_ms(session%refresh_ms, -1)
    case ("-")
      new_refresh_ms = adjusted_refresh_ms(session%refresh_ms, 1)
    case default
      return
    end select

    session%refresh_ms = new_refresh_ms
    call system_clock(session%last_refresh_count)
    call set_status(session, refresh_status_text(new_refresh_ms))
    handled = .true.
  end function handle_refresh_printable_key

  integer function adjusted_refresh_ms(refresh_ms, direction) result(adjusted)
    integer, intent(in) :: refresh_ms
    integer, intent(in) :: direction
    integer :: step_ms

    step_ms = max(100, refresh_ms / 10)
    if (direction < 0) then
      adjusted = bounded_refresh_ms(refresh_ms - step_ms)
    else if (direction > 0) then
      adjusted = bounded_refresh_ms(refresh_ms + step_ms)
    else
      adjusted = bounded_refresh_ms(refresh_ms)
    end if
  end function adjusted_refresh_ms

  function refresh_status_text(refresh_ms) result(text)
    integer, intent(in) :: refresh_ms
    character(len=:), allocatable :: text

    text = "refresh " // integer_text(refresh_ms) // " ms"
  end function refresh_status_text

  logical function handle_layout_preset_key(session, text) result(handled)
    type(terminal_session), intent(inout) :: session
    character(len=*), intent(in) :: text
    integer :: preset_index
    integer :: read_status

    handled = .false.
    if (len_trim(text) /= 1) return
    read(text, *, iostat=read_status) preset_index
    if (read_status /= 0) return
    if (preset_index < 1 .or. preset_index > LAYOUT_PRESET_COUNT) return

    call switch_layout_preset(session, preset_index)
    handled = .true.
  end function handle_layout_preset_key

  logical function handle_process_printable_key(session, text) result(handled)
    type(terminal_session), intent(inout) :: session
    character(len=*), intent(in) :: text
    integer :: navigation_delta

    handled = .false.
    if (.not. process_widget_focused(session)) return
    if (session%process_state%signal_pending) then
      handled = .true.
      call handle_process_signal_text(session, text)
      return
    end if
    if (session%process_state%filter_active) then
      call process_table_append_filter_text(session%process_state, text)
      handled = .true.
      call set_status(session, process_table_status(session%process_state))
      return
    end if
    navigation_delta = process_vim_navigation_delta(text, session%process_state%fuzzy_query_length, &
                                                    session%process_state%row_count)
    if (text == " ") then
      if (process_table_toggle_tag(session%process_state)) then
        continue
      end if
    else if (text == "U") then
      call process_table_clear_tags(session%process_state)
    else if (navigation_delta /= 0) then
      call process_table_select_delta(session%process_state, navigation_delta)
    else
      if (.not. ascii_alnum_text(text)) return
      call process_table_append_fuzzy_text(session%process_state, text)
    end if
    handled = .true.
    call set_status(session, process_table_status(session%process_state))
  end function handle_process_printable_key

  logical function handle_process_named_key(session, key_name) result(handled)
    type(terminal_session), intent(inout) :: session
    character(len=*), intent(in) :: key_name
    logical :: node_toggled

    handled = .false.
    if (.not. process_widget_focused(session)) return
    if (session%process_state%signal_pending) then
      select case (key_name)
      case (FGOF_KEY_BACKSPACE, FGOF_KEY_DELETE)
        call delete_process_signal_digit(session)
      case (FGOF_KEY_ENTER)
        call send_pending_process_signal(session)
      case (FGOF_KEY_ESCAPE)
        call process_table_cancel_signal(session%process_state)
        call set_status(session, "signal cancelled")
      case (FGOF_KEY_LEFT)
        call cycle_process_signal(session, -1)
      case (FGOF_KEY_RIGHT)
        call cycle_process_signal(session, 1)
      case default
        call set_status(session, process_table_signal_status(session%process_state))
      end select
      handled = .true.
      return
    end if
    if (session%process_state%filter_active) then
      select case (key_name)
      case (FGOF_KEY_BACKSPACE)
        call process_table_delete_filter_char(session%process_state)
      case (FGOF_KEY_DELETE)
        call process_table_delete_filter_right(session%process_state)
      case (FGOF_KEY_ENTER)
        call process_table_finish_filter(session%process_state)
      case (FGOF_KEY_ESCAPE)
        call process_table_clear_filter(session%process_state)
      case (FGOF_KEY_LEFT)
        call process_table_move_filter_cursor(session%process_state, -1)
      case (FGOF_KEY_RIGHT)
        call process_table_move_filter_cursor(session%process_state, 1)
      case (FGOF_KEY_HOME)
        call process_table_move_filter_home(session%process_state)
      case (FGOF_KEY_END)
        call process_table_move_filter_end(session%process_state)
      case default
        ! Keep process table navigation available while a filter is active.
      end select
      handled = key_name == FGOF_KEY_BACKSPACE .or. key_name == FGOF_KEY_DELETE .or. &
                key_name == FGOF_KEY_ENTER .or. key_name == FGOF_KEY_ESCAPE .or. &
                key_name == FGOF_KEY_LEFT .or. key_name == FGOF_KEY_RIGHT .or. &
                key_name == FGOF_KEY_HOME .or. key_name == FGOF_KEY_END
      if (handled) then
        call set_status(session, process_table_status(session%process_state))
        return
      end if
    end if
    if (handle_process_function_key(session, key_name)) then
      handled = .true.
      return
    end if
    select case (key_name)
    case (FGOF_KEY_BACKSPACE, FGOF_KEY_DELETE)
      if (session%process_state%fuzzy_query_length <= 0) return
      call process_table_delete_fuzzy_char(session%process_state)
    case (FGOF_KEY_ENTER)
      if (session%process_state%fuzzy_query_length > 0) then
        call process_table_clear_fuzzy(session%process_state)
      else
        node_toggled = process_table_toggle_selected_node(session%process_state)
        if (.not. node_toggled) return
      end if
    case (FGOF_KEY_ESCAPE)
      if (session%process_state%fuzzy_query_length > 0) then
        call process_table_clear_fuzzy(session%process_state)
      else
        if (.not. session%process_state%filter_active .and. session%process_state%filter_length <= 0) return
        call process_table_clear_filter(session%process_state)
      end if
    case (FGOF_KEY_UP)
      if (session%process_state%fuzzy_query_length > 0) then
        call process_table_step_fuzzy_match(session%process_state, -1)
      else
        call process_table_select_delta(session%process_state, -1)
      end if
    case (FGOF_KEY_DOWN)
      if (session%process_state%fuzzy_query_length > 0) then
        call process_table_step_fuzzy_match(session%process_state, 1)
      else
        call process_table_select_delta(session%process_state, 1)
      end if
    case (FGOF_KEY_PAGEUP)
      call process_table_page_delta(session%process_state, -1)
    case (FGOF_KEY_PAGEDOWN)
      call process_table_page_delta(session%process_state, 1)
    case (FGOF_KEY_HOME)
      call process_table_select_delta(session%process_state, -session%process_state%row_count)
    case (FGOF_KEY_END)
      call process_table_select_delta(session%process_state, session%process_state%row_count)
    case (FGOF_KEY_LEFT)
      call process_table_cycle_sort_key(session%process_state, -1)
    case (FGOF_KEY_RIGHT)
      call process_table_cycle_sort_key(session%process_state, 1)
    case default
      return
    end select
    handled = .true.
    call set_status(session, process_table_status(session%process_state))
  end function handle_process_named_key

  logical function handle_process_function_key(session, key_name) result(handled)
    type(terminal_session), intent(inout) :: session
    character(len=*), intent(in) :: key_name

    handled = .false.
    if (.not. process_widget_focused(session)) return
    if (session%process_state%signal_pending .or. session%process_state%filter_active) return
    select case (key_name)
    case (FGOF_KEY_F2)
      call begin_process_signal(session)
    case (FGOF_KEY_F3)
      call process_table_toggle_tree(session%process_state)
    case (FGOF_KEY_F4)
      call process_table_begin_filter(session%process_state)
    case (FGOF_KEY_F5)
      call process_table_toggle_sort_direction(session%process_state)
    case (FGOF_KEY_F6)
      call process_table_toggle_metric_sparklines(session%process_state)
    case (FGOF_KEY_F8)
      call toggle_process_follow(session)
    case (FGOF_KEY_F9)
      call begin_tagged_process_signal(session)
      handled = .true.
      return
    case default
      return
    end select
    handled = .true.
    call set_status(session, process_table_status(session%process_state))
  end function handle_process_function_key

  subroutine toggle_process_follow(session)
    type(terminal_session), intent(inout) :: session
    logical :: following

    following = process_table_toggle_follow(session%process_state)
    if (session%process_state%selected_pid <= 0) then
      call set_status(session, "no process selected")
    else if (following) then
      call set_status(session, "following PID " // integer_text(max(0, session%process_state%follow_pid)))
    else
      call set_status(session, "follow disabled")
    end if
  end subroutine toggle_process_follow

  logical function ascii_alnum_text(text) result(alnum)
    character(len=*), intent(in) :: text
    integer :: code

    alnum = .false.
    if (len(text) /= 1) return
    code = iachar(text(1:1))
    alnum = (code >= iachar("0") .and. code <= iachar("9")) .or. &
            (code >= iachar("A") .and. code <= iachar("Z")) .or. &
            (code >= iachar("a") .and. code <= iachar("z"))
  end function ascii_alnum_text

  integer function process_vim_navigation_delta(text, fuzzy_query_length, row_count) result(delta)
    character(len=*), intent(in) :: text
    integer, intent(in) :: fuzzy_query_length
    integer, intent(in) :: row_count

    delta = 0
    if (fuzzy_query_length > 0) return
    delta = vim_navigation_delta(text, row_count)
  end function process_vim_navigation_delta

  integer function vim_navigation_delta(text, row_count) result(delta)
    character(len=*), intent(in) :: text
    integer, intent(in) :: row_count

    select case (text)
    case ("j")
      delta = 1
    case ("k")
      delta = -1
    case ("g")
      delta = -max(0, row_count)
    case ("G")
      delta = max(0, row_count)
    case default
      delta = 0
    end select
  end function vim_navigation_delta

  logical function handle_network_printable_key(session, text) result(handled)
    type(terminal_session), intent(inout) :: session
    character(len=*), intent(in) :: text

    handled = .false.
    if (.not. network_widget_focused(session)) return
    select case (text)
    case ("j", "k", "g", "G")
      call network_table_select_delta(session%network_state, vim_navigation_delta(text, session%network_state%row_count))
    case ("f")
      call network_table_cycle_state_filter(session%network_state)
    case ("s")
      call network_table_toggle_sort_direction(session%network_state)
    case default
      return
    end select
    handled = .true.
    call set_status(session, network_table_status(session%network_state))
  end function handle_network_printable_key

  logical function handle_network_named_key(session, key_name) result(handled)
    type(terminal_session), intent(inout) :: session
    character(len=*), intent(in) :: key_name

    handled = .false.
    if (.not. network_widget_focused(session)) return
    select case (key_name)
    case (FGOF_KEY_ESCAPE)
      if (len_trim(session%network_state%state_filter) <= 0) return
      call network_table_clear_state_filter(session%network_state)
    case (FGOF_KEY_UP)
      call network_table_select_delta(session%network_state, -1)
    case (FGOF_KEY_DOWN)
      call network_table_select_delta(session%network_state, 1)
    case (FGOF_KEY_PAGEUP)
      call network_table_page_delta(session%network_state, -1)
    case (FGOF_KEY_PAGEDOWN)
      call network_table_page_delta(session%network_state, 1)
    case (FGOF_KEY_HOME)
      call network_table_select_delta(session%network_state, -session%network_state%row_count)
    case (FGOF_KEY_END)
      call network_table_select_delta(session%network_state, session%network_state%row_count)
    case (FGOF_KEY_LEFT)
      call network_table_cycle_sort_key(session%network_state, -1)
    case (FGOF_KEY_RIGHT)
      call network_table_cycle_sort_key(session%network_state, 1)
    case default
      return
    end select
    handled = .true.
    call set_status(session, network_table_status(session%network_state))
  end function handle_network_named_key

  logical function process_widget_focused(session) result(focused)
    type(terminal_session), intent(in) :: session

    focused = focused_widget_name(session) == "process"
  end function process_widget_focused

  logical function network_widget_focused(session) result(focused)
    type(terminal_session), intent(in) :: session

    focused = focused_widget_name(session) == "network"
  end function network_widget_focused

  subroutine begin_process_signal(session)
    type(terminal_session), intent(inout) :: session
    integer :: signal_number
    logical :: started

    signal_number = terminal_signal_number(FTOP_SIGNAL_TERM)
    started = process_table_begin_signal(session%process_state, signal_number, "SIGTERM", &
                                         process_signal_requires_confirmation(signal_number))
    if (started) then
      call set_status(session, process_table_signal_status(session%process_state))
    else
      call set_status(session, "no process selected")
    end if
  end subroutine begin_process_signal

  subroutine begin_tagged_process_signal(session)
    type(terminal_session), intent(inout) :: session
    integer :: signal_number
    logical :: started

    signal_number = terminal_signal_number(FTOP_SIGNAL_TERM)
    if (session%process_state%tag_count <= 0) then
      call begin_process_signal(session)
      return
    end if
    started = process_table_begin_tag_signal(session%process_state, signal_number, "SIGTERM")
    if (started) then
      call set_status(session, process_table_signal_status(session%process_state))
    else
      call set_status(session, "no tagged processes")
    end if
  end subroutine begin_tagged_process_signal

  subroutine handle_process_signal_text(session, text)
    type(terminal_session), intent(inout) :: session
    character(len=*), intent(in) :: text
    integer :: signal_number
    logical :: changed

    call process_table_append_signal_digit(session%process_state, text, signal_number, changed)
    if (changed .and. signal_number > 0) then
      if (.not. process_table_set_signal(session%process_state, signal_number, "", &
                                         process_signal_requires_confirmation(signal_number))) then
        call set_status(session, "invalid signal " // integer_text(max(0, signal_number)))
        return
      end if
    end if
    call set_status(session, process_table_signal_status(session%process_state))
  end subroutine handle_process_signal_text

  subroutine delete_process_signal_digit(session)
    type(terminal_session), intent(inout) :: session
    integer :: signal_number
    logical :: changed

    call process_table_delete_signal_digit(session%process_state, signal_number, changed)
    if (changed) then
      if (signal_number > 0) then
        call set_process_signal_number(session, signal_number, "")
      else
        call process_table_clear_signal_input(session%process_state)
        call set_process_signal_choice(session, 1)
      end if
    end if
    call set_status(session, process_table_signal_status(session%process_state))
  end subroutine delete_process_signal_digit

  subroutine cycle_process_signal(session, direction)
    type(terminal_session), intent(inout) :: session
    integer, intent(in) :: direction
    integer :: choice_index

    call process_table_clear_signal_input(session%process_state)
    choice_index = process_signal_choice_index(session%process_state%signal_number)
    choice_index = modulo(choice_index - 1 + direction, PROCESS_SIGNAL_CHOICE_COUNT) + 1
    call set_process_signal_choice(session, choice_index)
    call set_status(session, process_table_signal_status(session%process_state))
  end subroutine cycle_process_signal

  subroutine set_process_signal_choice(session, choice_index)
    type(terminal_session), intent(inout) :: session
    integer, intent(in) :: choice_index

    call set_process_signal_number(session, process_signal_number_at(choice_index), process_signal_name_at(choice_index))
  end subroutine set_process_signal_choice

  subroutine set_process_signal_number(session, signal_number, signal_name)
    type(terminal_session), intent(inout) :: session
    integer, intent(in) :: signal_number
    character(len=*), intent(in) :: signal_name
    logical :: ignored

    ignored = process_table_set_signal(session%process_state, signal_number, signal_name, &
                                       process_signal_requires_confirmation(signal_number))
  end subroutine set_process_signal_number

  subroutine send_pending_process_signal(session)
    type(terminal_session), intent(inout) :: session
    character(len=:), allocatable :: signal_name
    integer :: error_code
    integer :: pid
    integer :: signal_number
    integer :: sent_count
    integer :: failed_count
    integer :: first_error_code
    integer :: target_count
    integer :: target_index
    integer(int64) :: start_time
    integer, allocatable :: error_codes(:)
    logical, allocatable :: signal_results(:)
    logical :: valid_target

    if (.not. session%process_state%signal_pending) return
    if (session%process_state%signal_confirm_required .and. .not. session%process_state%signal_confirmed) then
      call process_table_confirm_signal(session%process_state)
      call set_status(session, process_table_signal_status(session%process_state))
      return
    end if

    pid = session%process_state%signal_pid
    signal_number = session%process_state%signal_number
    signal_name = trim(session%process_state%signal_name)
    if (len(signal_name) <= 0) signal_name = "signal " // integer_text(max(0, signal_number))

    target_count = process_table_signal_target_count(session%process_state)
    if (session%process_state%signal_tagged) then
      allocate(signal_results(max(0, target_count)))
      allocate(error_codes(max(0, target_count)))
      signal_results = .false.
      error_codes = 0
      do target_index = 1, target_count
        call process_table_signal_target_at(session%process_state, target_index, pid, start_time, valid_target)
        if (.not. valid_target) cycle
        if (terminal_kill(pid, signal_number, error_code)) then
          signal_results(target_index) = .true.
          call process_table_mark_signal_feedback(session%process_state, pid, start_time)
        else
          error_codes(target_index) = error_code
        end if
      end do
      call summarize_batch_signal_results(signal_results, error_codes, sent_count, failed_count, first_error_code)
      call process_table_cancel_signal(session%process_state)
      if (sent_count > 0) call process_table_clear_tags(session%process_state)
      call set_status(session, process_batch_signal_status(signal_name, sent_count, failed_count, first_error_code))
      return
    end if

    if (terminal_kill(pid, signal_number, error_code)) then
      call process_table_mark_signal_feedback(session%process_state, pid, session%process_state%signal_start_time)
      call process_table_cancel_signal(session%process_state)
      call set_status(session, "sent " // signal_name // " to pid " // integer_text(max(0, pid)))
    else
      call process_table_mark_signal_feedback(session%process_state, pid, session%process_state%signal_start_time)
      call process_table_cancel_signal(session%process_state)
      call set_status(session, process_signal_error_status(pid, signal_name, error_code))
    end if
  end subroutine send_pending_process_signal

  function process_batch_signal_status(signal_name, sent_count, failed_count, error_code) result(message)
    character(len=*), intent(in) :: signal_name
    integer, intent(in) :: sent_count
    integer, intent(in) :: failed_count
    integer, intent(in) :: error_code
    character(len=:), allocatable :: message
    integer :: total_count

    total_count = max(0, sent_count) + max(0, failed_count)
    if (failed_count <= 0) then
      message = "sent " // signal_name // " to " // integer_text(total_count) // " processes"
    else if (sent_count > 0) then
      message = "sent " // signal_name // " to " // integer_text(max(0, sent_count)) // " processes (" // &
                integer_text(max(0, failed_count)) // " failed: " // process_signal_error_label(error_code) // ")"
    else
      message = "failed to send " // signal_name // ": " // process_signal_error_label(error_code)
    end if
  end function process_batch_signal_status

  subroutine summarize_batch_signal_results(results, error_codes, sent_count, failed_count, first_error_code)
    logical, intent(in) :: results(:)
    integer, intent(in) :: error_codes(:)
    integer, intent(out) :: sent_count
    integer, intent(out) :: failed_count
    integer, intent(out) :: first_error_code
    integer :: index_value
    integer :: limit

    sent_count = 0
    failed_count = 0
    first_error_code = 0
    limit = min(size(results), size(error_codes))
    do index_value = 1, limit
      if (results(index_value)) then
        sent_count = sent_count + 1
      else
        failed_count = failed_count + 1
        if (first_error_code == 0) first_error_code = error_codes(index_value)
      end if
    end do
  end subroutine summarize_batch_signal_results

  function process_signal_error_label(error_code) result(label)
    integer, intent(in) :: error_code
    character(len=:), allocatable :: label

    select case (error_code)
    case (1)
      label = "permission denied"
    case (3)
      label = "process not found"
    case default
      label = "errno=" // integer_text(error_code)
    end select
  end function process_signal_error_label

  function process_signal_error_status(pid, signal_name, error_code) result(message)
    integer, intent(in) :: pid
    character(len=*), intent(in) :: signal_name
    integer, intent(in) :: error_code
    character(len=:), allocatable :: message

    select case (error_code)
    case (1)
      message = "permission denied sending " // signal_name // " to pid " // integer_text(max(0, pid))
    case (3)
      message = "process not found for pid " // integer_text(max(0, pid))
    case default
      message = "failed sending " // signal_name // " to pid " // integer_text(max(0, pid)) // &
                " errno=" // integer_text(error_code)
    end select
  end function process_signal_error_status

  logical function process_signal_requires_confirmation(signal_number) result(required)
    integer, intent(in) :: signal_number

    required = signal_number /= terminal_signal_number(FTOP_SIGNAL_TERM)
  end function process_signal_requires_confirmation

  integer function process_signal_choice_index(signal_number) result(choice_index)
    integer, intent(in) :: signal_number
    integer :: candidate_index

    choice_index = 1
    do candidate_index = 1, PROCESS_SIGNAL_CHOICE_COUNT
      if (process_signal_number_at(candidate_index) == signal_number) then
        choice_index = candidate_index
        return
      end if
    end do
  end function process_signal_choice_index

  integer function process_signal_number_at(choice_index) result(signal_number)
    integer, intent(in) :: choice_index

    signal_number = terminal_signal_number(process_signal_id_at(choice_index))
  end function process_signal_number_at

  integer function process_signal_id_at(choice_index) result(signal_id)
    integer, intent(in) :: choice_index

    select case (choice_index)
    case (2)
      signal_id = FTOP_SIGNAL_KILL
    case (3)
      signal_id = FTOP_SIGNAL_STOP
    case (4)
      signal_id = FTOP_SIGNAL_CONT
    case (5)
      signal_id = FTOP_SIGNAL_HUP
    case (6)
      signal_id = FTOP_SIGNAL_USR1
    case (7)
      signal_id = FTOP_SIGNAL_USR2
    case default
      signal_id = FTOP_SIGNAL_TERM
    end select
  end function process_signal_id_at

  function process_signal_name_at(choice_index) result(signal_name)
    integer, intent(in) :: choice_index
    character(len=:), allocatable :: signal_name

    select case (choice_index)
    case (2)
      signal_name = "SIGKILL"
    case (3)
      signal_name = "SIGSTOP"
    case (4)
      signal_name = "SIGCONT"
    case (5)
      signal_name = "SIGHUP"
    case (6)
      signal_name = "SIGUSR1"
    case (7)
      signal_name = "SIGUSR2"
    case default
      signal_name = "SIGTERM"
    end select
  end function process_signal_name_at

  subroutine handle_pending_signals(session)
    type(terminal_session), intent(inout) :: session

    if (terminal_signal_pending(FTOP_SIGNAL_WINCH)) then
      call terminal_signal_clear(FTOP_SIGNAL_WINCH)
      call resize_session_to_terminal(session)
      call set_status(session, "resized")
    end if

    if (terminal_signal_pending(FTOP_SIGNAL_TSTP)) then
      call terminal_signal_clear(FTOP_SIGNAL_TSTP)
      call suspend_session(session)
    end if

    if (terminal_signal_pending(FTOP_SIGNAL_CONT)) then
      call terminal_signal_clear(FTOP_SIGNAL_CONT)
      session%needs_full_render = .true.
    end if

    if (terminal_signal_pending(FTOP_SIGNAL_INT)) then
      call terminal_signal_clear(FTOP_SIGNAL_INT)
      call set_status(session, "SIGINT")
      session%running = .false.
    end if

    if (terminal_signal_pending(FTOP_SIGNAL_TERM)) then
      call terminal_signal_clear(FTOP_SIGNAL_TERM)
      call set_status(session, "SIGTERM")
      session%running = .false.
    end if
  end subroutine handle_pending_signals

  subroutine suspend_session(session)
    type(terminal_session), intent(inout) :: session

    if (.not. session%terminal_started) return
    call write_terminal_output(leave_terminal_control_sequence())
    session%terminal_started = .false.
    call restore_guard(session%guard)

    call terminal_signal_clear(FTOP_SIGNAL_CONT)
    if (.not. terminal_signal_suspend_self()) then
      call set_status(session, "failed to suspend")
      session%running = .false.
      return
    end if

    call enter_raw_mode(session%guard)
    if (session%guard%last_error_code /= FGOF_TERMIOS_ERR_NONE) then
      call set_status(session, "failed to restore raw mode")
      session%running = .false.
      return
    end if

    call write_terminal_output(enter_terminal_control_sequence())
    session%terminal_started = .true.
    call terminal_signal_clear(FTOP_SIGNAL_CONT)
    call resize_session_to_terminal(session)
    call set_status(session, "resumed")
  end subroutine suspend_session

  subroutine resize_session_to_terminal(session)
    type(terminal_session), intent(inout) :: session
    type(terminal_size) :: size_info
    integer :: rows
    integer :: columns

    size_info = get_terminal_size(session%guard%fd)
    if (size_info%valid) then
      rows = size_info%rows
      columns = size_info%columns
    else
      rows = DEFAULT_ROWS
      columns = DEFAULT_COLUMNS
    end if

    session%current = allocate_screen(columns, rows)
    session%previous = allocate_screen(columns, rows)
    session%needs_full_render = .true.
    session%dirty = .true.
  end subroutine resize_session_to_terminal

  subroutine set_status(session, message)
    type(terminal_session), intent(inout) :: session
    character(len=*), intent(in) :: message

    session%status_text = trim(message)
    session%dirty = .true.
  end subroutine set_status

  subroutine initialize_refresh_timer(session)
    type(terminal_session), intent(inout) :: session

    call system_clock(session%last_refresh_count, session%clock_rate)
    session%frame_count = 0
    session%last_render_count = 0
    session%render_fps = 0.0
  end subroutine initialize_refresh_timer

  logical function refresh_due(session) result(due)
    type(terminal_session), intent(in) :: session

    due = elapsed_since_refresh_ms(session) >= session%refresh_ms
  end function refresh_due

  subroutine mark_refresh(session)
    type(terminal_session), intent(inout) :: session

    session%frame_count = session%frame_count + 1
    call system_clock(session%last_refresh_count)
    session%dirty = .true.
  end subroutine mark_refresh

  subroutine update_render_fps(session)
    type(terminal_session), intent(inout) :: session
    integer :: elapsed_count
    integer :: now_count
    integer :: rate
    real :: elapsed_seconds

    call system_clock(now_count, rate)
    if (rate <= 0) return

    if (session%last_render_count > 0) then
      elapsed_count = now_count - session%last_render_count
      if (elapsed_count > 0) then
        elapsed_seconds = real(elapsed_count) / real(rate)
        if (elapsed_seconds > 0.0) session%render_fps = 1.0 / elapsed_seconds
      end if
    end if
    session%last_render_count = now_count
  end subroutine update_render_fps

  integer function poll_timeout_ms(session) result(timeout_ms)
    type(terminal_session), intent(in) :: session

    integer :: remaining_ms

    if (session%dirty) then
      timeout_ms = 0
      return
    end if

    remaining_ms = session%refresh_ms - elapsed_since_refresh_ms(session)
    timeout_ms = max(0, remaining_ms)
  end function poll_timeout_ms

  integer function elapsed_since_refresh_ms(session) result(elapsed_ms)
    type(terminal_session), intent(in) :: session
    integer :: now_count

    if (session%clock_rate <= 0) then
      elapsed_ms = session%refresh_ms
      return
    end if

    call system_clock(now_count)
    elapsed_ms = int((real(now_count - session%last_refresh_count) / real(session%clock_rate)) * 1000.0)
    elapsed_ms = max(0, elapsed_ms)
  end function elapsed_since_refresh_ms

  integer function bounded_refresh_ms(refresh_ms) result(bounded)
    integer, intent(in) :: refresh_ms

    bounded = max(MIN_REFRESH_MS, min(MAX_REFRESH_MS, refresh_ms))
  end function bounded_refresh_ms

  real function target_fps(refresh_ms) result(fps)
    integer, intent(in) :: refresh_ms

    if (refresh_ms > 0) then
      fps = 1000.0 / real(refresh_ms)
    else
      fps = 0.0
    end if
  end function target_fps

  subroutine log_debug(message)
    character(len=*), intent(in) :: message
    integer :: unit
    integer :: status

    open(newunit=unit, file=DEBUG_LOG_PATH, status="unknown", position="append", action="write", iostat=status)
    if (status /= 0) return
    write(unit, '(a)', iostat=status) trim(message)
    close(unit)
  end subroutine log_debug

  subroutine print_terminal_error(prefix, message)
    character(len=*), intent(in) :: prefix
    character(len=*), intent(in) :: message

    if (len_trim(message) > 0) then
      write(error_unit, '(a)') "ftop: " // trim(prefix) // ": " // trim(message)
    else
      write(error_unit, '(a)') "ftop: " // trim(prefix)
    end if
  end subroutine print_terminal_error

  logical function parse_sgr_mouse(bytes, event, message, remaining) result(found)
    character(len=*), intent(in) :: bytes
    type(sgr_mouse_event), intent(out) :: event
    character(len=:), allocatable, intent(out) :: message
    character(len=:), allocatable, intent(out) :: remaining
    integer :: start_index
    integer :: end_index
    integer :: i
    integer :: code
    integer :: col
    integer :: row
    integer :: parse_status
    character(len=:), allocatable :: payload
    logical :: pressed

    found = .false.
    event = sgr_mouse_event()
    message = ""
    remaining = bytes
    start_index = index(bytes, achar(27) // "[<")
    if (start_index == 0) return

    end_index = 0
    do i = start_index + 3, len(bytes)
      if (bytes(i:i) == "M" .or. bytes(i:i) == "m") then
        end_index = i
        exit
      end if
    end do
    if (end_index == 0) return

    payload = bytes(start_index + 3:end_index - 1)
    call parse_mouse_payload(payload, code, col, row, parse_status)
    if (parse_status /= 0) return

    pressed = bytes(end_index:end_index) == "M"
    event%code = code
    event%row = row
    event%col = col
    event%pressed = pressed
    message = mouse_event_text(code, row, col, pressed)
    remaining = bytes(:start_index - 1) // bytes(end_index + 1:)
    found = .true.
  end function parse_sgr_mouse

  logical function mouse_left_press(mouse) result(is_left_press)
    type(sgr_mouse_event), intent(in) :: mouse

    is_left_press = mouse%pressed .and. iand(mouse%code, 64) == 0 .and. iand(mouse%code, 3) == 0
  end function mouse_left_press

  logical function mouse_scroll_up(mouse) result(is_scroll_up)
    type(sgr_mouse_event), intent(in) :: mouse

    is_scroll_up = mouse%pressed .and. iand(mouse%code, 64) /= 0 .and. iand(mouse%code, 3) == 0
  end function mouse_scroll_up

  logical function mouse_scroll_down(mouse) result(is_scroll_down)
    type(sgr_mouse_event), intent(in) :: mouse

    is_scroll_down = mouse%pressed .and. iand(mouse%code, 64) /= 0 .and. iand(mouse%code, 3) == 1
  end function mouse_scroll_down

  logical function point_in_rect(rect, row, col) result(inside)
    type(widget_rect), intent(in) :: rect
    integer, intent(in) :: row
    integer, intent(in) :: col

    inside = rect%width > 0 .and. rect%height > 0 .and. &
             row >= rect%row .and. row < rect%row + rect%height .and. &
             col >= rect%col .and. col < rect%col + rect%width
  end function point_in_rect

  subroutine parse_mouse_payload(payload, code, col, row, status)
    character(len=*), intent(in) :: payload
    integer, intent(out) :: code
    integer, intent(out) :: col
    integer, intent(out) :: row
    integer, intent(out) :: status
    integer :: first_sep
    integer :: second_sep

    code = 0
    col = 0
    row = 0
    status = 1
    first_sep = index(payload, ";")
    if (first_sep <= 1) return
    second_sep = index(payload(first_sep + 1:), ";")
    if (second_sep <= 1) return
    second_sep = first_sep + second_sep

    read(payload(:first_sep - 1), *, iostat=status) code
    if (status /= 0) return
    read(payload(first_sep + 1:second_sep - 1), *, iostat=status) col
    if (status /= 0) return
    read(payload(second_sep + 1:), *, iostat=status) row
  end subroutine parse_mouse_payload

  function mouse_event_text(code, row, col, pressed) result(message)
    integer, intent(in) :: code
    integer, intent(in) :: row
    integer, intent(in) :: col
    logical, intent(in) :: pressed
    character(len=:), allocatable :: message
    character(len=:), allocatable :: action

    if (pressed) then
      action = "press"
    else
      action = "release"
    end if

    message = "mouse " // action // " " // mouse_button_text(code) // &
              " row=" // integer_text(row) // " col=" // integer_text(col) // &
              mouse_modifier_text(code)
  end function mouse_event_text

  function mouse_button_text(code) result(text)
    integer, intent(in) :: code
    character(len=:), allocatable :: text
    integer :: button_code

    button_code = iand(code, 3)
    if (iand(code, 64) /= 0) then
      if (button_code == 0) then
        text = "scroll-up"
      else if (button_code == 1) then
        text = "scroll-down"
      else
        text = "scroll"
      end if
      return
    end if

    select case (button_code)
    case (0)
      text = "left"
    case (1)
      text = "middle"
    case (2)
      text = "right"
    case default
      text = "button"
    end select
  end function mouse_button_text

  function mouse_modifier_text(code) result(text)
    integer, intent(in) :: code
    character(len=:), allocatable :: text

    text = ""
    if (iand(code, 4) /= 0) text = text // " shift"
    if (iand(code, 8) /= 0) text = text // " alt"
    if (iand(code, 16) /= 0) text = text // " ctrl"
  end function mouse_modifier_text

  function enter_terminal_control_sequence() result(sequence)
    character(len=:), allocatable :: sequence

    sequence = esc() // "[?1049h" // esc() // "[?25l" // &
               esc() // "[?1000h" // esc() // "[?1006h"
  end function enter_terminal_control_sequence

  function leave_terminal_control_sequence() result(sequence)
    character(len=:), allocatable :: sequence

    sequence = esc() // "[?1006l" // esc() // "[?1000l" // &
               esc() // "[0m" // esc() // "[?25h" // esc() // "[?1049l"
  end function leave_terminal_control_sequence

  function esc() result(sequence)
    character(len=1) :: sequence

    sequence = achar(27)
  end function esc

  function integer_text(value) result(text)
    integer, intent(in) :: value
    character(len=:), allocatable :: text
    character(len=32) :: scratch

    write(scratch, '(i0)') value
    text = trim(scratch)
  end function integer_text

end module ftop_app
