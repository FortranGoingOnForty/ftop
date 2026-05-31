module ftop_app
  use, intrinsic :: iso_fortran_env, only : error_unit
  use fgof_keys, only : &
    buffer_input, &
    clear_decoder_state, &
    decode_next_event, &
    event_text, &
    has_pending_input
  use fgof_keys_types, only : &
    FGOF_KEY_DOWN, &
    FGOF_KEY_ENTER, &
    FGOF_KEY_ESCAPE, &
    FGOF_KEY_LEFT, &
    FGOF_KEY_RIGHT, &
    FGOF_KEY_UP, &
    key_decoder_state, &
    key_event
  use fgof_screen, only : &
    allocate_screen, &
    clear_screen, &
    clear_screen_style, &
    put_glyph, &
    render_screen_ansi, &
    render_screen_diff_ansi
  use fgof_screen_types, only : screen_buffer, screen_style
  use fgof_termios, only : bind_guard, enter_raw_mode, get_terminal_size, restore_guard
  use fgof_termios_types, only : FGOF_TERMIOS_ERR_NONE, terminal_size, termios_guard
  use ftop_signal, only : &
    FTOP_SIGNAL_CONT, &
    FTOP_SIGNAL_INT, &
    FTOP_SIGNAL_TERM, &
    FTOP_SIGNAL_TSTP, &
    FTOP_SIGNAL_WINCH, &
    terminal_signal_clear => ftop_signal_clear, &
    terminal_signal_pending => ftop_signal_check, &
    terminal_signal_setup => ftop_signal_setup, &
    terminal_signal_suspend_self => ftop_signal_suspend_self
  use ftop_terminal_io, only : &
    read_terminal_input, &
    terminal_read_result, &
    write_terminal_output
  implicit none
  private

  integer, parameter :: DEFAULT_ROWS = 24
  integer, parameter :: DEFAULT_COLUMNS = 80
  integer, parameter :: DEFAULT_REFRESH_MS = 1000
  integer, parameter :: MIN_REFRESH_MS = 100
  integer, parameter :: MAX_REFRESH_MS = 60000
  character(len=*), parameter :: DEBUG_LOG_PATH = "ftop-debug.log"

  type :: terminal_session
    type(termios_guard) :: guard
    type(screen_buffer) :: current
    type(screen_buffer) :: previous
    type(key_decoder_state) :: decoder
    character(len=:), allocatable :: status_text
    integer :: refresh_ms = DEFAULT_REFRESH_MS
    integer :: frame_count = 0
    integer :: last_refresh_count = 0
    integer :: clock_rate = 0
    logical :: guard_bound = .false.
    logical :: terminal_started = .false.
    logical :: running = .true.
    logical :: needs_full_render = .true.
    logical :: dirty = .true.
  end type terminal_session

  public :: run_ftop
  public :: render_test_frame

contains

  integer function run_ftop(refresh_ms) result(status)
    integer, intent(in), optional :: refresh_ms
    type(terminal_session) :: session
    type(terminal_read_result) :: input

    if (present(refresh_ms)) session%refresh_ms = bounded_refresh_ms(refresh_ms)

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

    call write_terminal_output(enter_terminal_control_sequence())
    session%terminal_started = .true.
    call resize_session_to_terminal(session)
  end function start_terminal_session

  subroutine stop_terminal_session(session)
    type(terminal_session), intent(inout) :: session

    if (session%terminal_started) then
      call write_terminal_output(leave_terminal_control_sequence())
      session%terminal_started = .false.
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
    session%previous = session%current
    session%dirty = .false.
  end subroutine render_session

  subroutine draw_frame(session)
    type(terminal_session), intent(inout) :: session
    type(screen_style) :: border_style
    type(screen_style) :: title_style
    type(screen_style) :: dim_style
    integer :: width
    integer :: height
    integer :: title_col
    integer :: body_row
    character(len=:), allocatable :: refresh_text

    width = session%current%size%width
    height = session%current%size%height
    if (width <= 0 .or. height <= 0) return

    border_style = clear_screen_style()
    border_style%fg_truecolor = .true.
    border_style%fg_rgb = [126, 87, 255]
    title_style = clear_screen_style()
    title_style%fg_truecolor = .true.
    title_style%fg_rgb = [164, 240, 255]
    title_style%bg_truecolor = .true.
    title_style%bg_rgb = [32, 22, 56]
    title_style%bold = .true.
    dim_style = clear_screen_style()
    dim_style%fg_truecolor = .true.
    dim_style%fg_rgb = [150, 156, 178]

    call clear_screen(session%current)
    session%current%cursor_visible = .false.

    if (width < 8 .or. height < 4) then
      call put_text(session%current, 1, 1, "ftop", title_style)
      return
    end if

    call draw_box(session%current, border_style, double_line=.true.)
    title_col = centered_col(width, " ftop ")
    call put_text(session%current, 1, title_col, " ftop ", title_style)

    body_row = max(3, height / 2)
    call put_text(session%current, body_row, centered_col(width, "ftop terminal core"), &
                  "ftop terminal core", title_style)
    call put_text(session%current, body_row + 1, centered_col(width, "q/Ctrl+C quit"), &
                  "q/Ctrl+C quit", dim_style)
    call put_text(session%current, body_row + 2, centered_col(width, "Ctrl+Z suspend"), &
                  "Ctrl+Z suspend", dim_style)
    refresh_text = "refresh " // integer_text(session%refresh_ms) // "ms frame " // integer_text(session%frame_count)
    call put_text(session%current, height - 2, 3, refresh_text, dim_style)
    call put_text(session%current, height - 1, 3, session%status_text, dim_style)
  end subroutine draw_frame

  subroutine draw_box(buffer, style, double_line)
    type(screen_buffer), intent(inout) :: buffer
    type(screen_style), intent(in) :: style
    logical, intent(in), optional :: double_line
    character(len=4) :: top_left
    character(len=4) :: top_right
    character(len=4) :: bottom_left
    character(len=4) :: bottom_right
    character(len=4) :: horizontal
    character(len=4) :: vertical
    integer :: row
    integer :: col
    integer :: width
    integer :: height

    width = buffer%size%width
    height = buffer%size%height

    if (present(double_line)) then
      if (double_line) then
        top_left = "╔"
        top_right = "╗"
        bottom_left = "╚"
        bottom_right = "╝"
        horizontal = "═"
        vertical = "║"
      else
        call set_single_line_box_glyphs(top_left, top_right, bottom_left, bottom_right, horizontal, vertical)
      end if
    else
      call set_single_line_box_glyphs(top_left, top_right, bottom_left, bottom_right, horizontal, vertical)
    end if

    call put_glyph(buffer, 1, 1, top_left, style)
    call put_glyph(buffer, 1, width, top_right, style)
    call put_glyph(buffer, height, 1, bottom_left, style)
    call put_glyph(buffer, height, width, bottom_right, style)

    do col = 2, width - 1
      call put_glyph(buffer, 1, col, horizontal, style)
      call put_glyph(buffer, height, col, horizontal, style)
    end do

    do row = 2, height - 1
      call put_glyph(buffer, row, 1, vertical, style)
      call put_glyph(buffer, row, width, vertical, style)
    end do
  end subroutine draw_box

  subroutine set_single_line_box_glyphs(top_left, top_right, bottom_left, bottom_right, horizontal, vertical)
    character(len=*), intent(out) :: top_left
    character(len=*), intent(out) :: top_right
    character(len=*), intent(out) :: bottom_left
    character(len=*), intent(out) :: bottom_right
    character(len=*), intent(out) :: horizontal
    character(len=*), intent(out) :: vertical

    top_left = "┌"
    top_right = "┐"
    bottom_left = "└"
    bottom_right = "┘"
    horizontal = "─"
    vertical = "│"
  end subroutine set_single_line_box_glyphs

  subroutine put_text(buffer, row, col, text, style)
    type(screen_buffer), intent(inout) :: buffer
    integer, intent(in) :: row
    integer, intent(in) :: col
    character(len=*), intent(in) :: text
    type(screen_style), intent(in) :: style
    integer :: i
    integer :: target_col

    if (row < 1 .or. row > buffer%size%height) return
    do i = 1, len_trim(text)
      target_col = col + i - 1
      if (target_col > buffer%size%width) exit
      if (target_col >= 1) call put_glyph(buffer, row, target_col, text(i:i), style)
    end do
  end subroutine put_text

  subroutine handle_input(session, bytes)
    type(terminal_session), intent(inout) :: session
    character(len=*), intent(in) :: bytes
    type(key_event) :: event
    character(len=:), allocatable :: input_bytes
    character(len=:), allocatable :: mouse_message
    character(len=:), allocatable :: remaining_bytes

    input_bytes = bytes
    do while (len(input_bytes) > 0)
      if (.not. describe_sgr_mouse(input_bytes, mouse_message, remaining_bytes)) exit
      call log_debug(mouse_message)
      call set_status(session, mouse_message)
      input_bytes = remaining_bytes
    end do
    if (len(input_bytes) == 0) return

    call buffer_input(session%decoder, input_bytes)
    do while (has_pending_input(session%decoder))
      event = decode_next_event(session%decoder)
      if (event%incomplete) exit
      call handle_key_event(session, event)
      if (.not. session%running) exit
    end do
  end subroutine handle_input

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
      else if (.not. event%modifiers%ctrl .and. text == "q") then
        call set_status(session, "q")
        session%running = .false.
      else if (len(text) > 0) then
        call set_status(session, "key: " // text)
      end if
      return
    end if

    if (.not. allocated(event%key_name)) return
    select case (event%key_name)
    case (FGOF_KEY_UP, FGOF_KEY_DOWN, FGOF_KEY_LEFT, FGOF_KEY_RIGHT)
      call set_status(session, "arrow: " // event%key_name)
    case (FGOF_KEY_ENTER)
      call set_status(session, "enter")
    case (FGOF_KEY_ESCAPE)
      call set_status(session, "escape")
    case default
      call set_status(session, "key: " // event%key_name)
    end select
  end subroutine handle_key_event

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

  logical function describe_sgr_mouse(bytes, message, remaining) result(found)
    character(len=*), intent(in) :: bytes
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
    message = mouse_event_text(code, row, col, pressed)
    remaining = bytes(:start_index - 1) // bytes(end_index + 1:)
    found = .true.
  end function describe_sgr_mouse

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

  integer function centered_col(width, text) result(col)
    integer, intent(in) :: width
    character(len=*), intent(in) :: text

    col = max(1, ((width - len_trim(text)) / 2) + 1)
  end function centered_col

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
