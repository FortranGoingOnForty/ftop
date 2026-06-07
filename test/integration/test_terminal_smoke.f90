program test_terminal_smoke
  use, intrinsic :: iso_c_binding, only : c_int
  use fgof_expect, only : &
    clear_expect_options, &
    close_expect, &
    send_text, &
    spawn_expect, &
    wait_for_string
  use fgof_expect_types, only : &
    FGOF_EXPECT_OK, &
    FGOF_EXPECT_STATUS_MATCHED, &
    expect_match, &
    expect_options, &
    expect_session
  use fgof_pty, only : resize_pty, terminal_size
  implicit none

  interface
    integer(c_int) function ftop_test_send_continue(pid) bind(C, name="ftop_test_send_continue")
      import :: c_int
      integer(c_int), value :: pid
    end function ftop_test_send_continue

    integer(c_int) function ftop_test_send_terminate(pid) bind(C, name="ftop_test_send_terminate")
      import :: c_int
      integer(c_int), value :: pid
    end function ftop_test_send_terminate
  end interface

  integer, parameter :: SMOKE_TIMEOUT_MS = 5000
  character(len=*), parameter :: SMOKE_LOG_PATH = "ftop-smoke.log"
  character(len=512) :: ftop_path
  character(len=512) :: process_config_path
  character(len=512) :: argv(5)
  type(expect_options) :: options
  type(expect_session) :: session
  type(expect_match) :: match
  type(terminal_size) :: resized_size
  logical :: closed

  call get_command_argument(1, ftop_path)
  if (len_trim(ftop_path) == 0) error stop "ftop path argument is required"
  call get_command_argument(2, process_config_path)
  if (len_trim(process_config_path) == 0) error stop "process config path argument is required"

  call delete_log(SMOKE_LOG_PATH)

  options = clear_expect_options()
  options%timeout_ms = SMOKE_TIMEOUT_MS
  options%size%rows = 24
  options%size%cols = 80

  argv = ""
  argv(1) = "--refresh-ms"
  argv(2) = "500"
  argv(3) = "--log-file"
  argv(4) = SMOKE_LOG_PATH
  argv(5) = "--verbose"
  session = spawn_expect(trim(ftop_path), argv, options)
  if (session%error_code /= FGOF_EXPECT_OK) error stop "failed to spawn ftop"

  match = wait_for_string(session, "Memory", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop frame did not render"

  if (.not. send_text(session, "P")) error stop "failed to send layout cycle key"
  match = wait_for_string(session, "layout compact", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not cycle layout preset"

  if (.not. send_text(session, achar(27) // "[18~")) error stop "failed to send pause key"
  match = wait_for_string(session, "ftop PAUSED", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not render paused indicator"

  if (.not. send_text(session, achar(27) // "[18~")) error stop "failed to send resume key"
  match = wait_for_string(session, "Resumed", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not resume from pause"

  if (.not. send_text(session, "1")) error stop "failed to send full layout key"
  match = wait_for_string(session, "layout full", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not jump to full layout preset"

  if (.not. send_text(session, "c")) error stop "failed to send CPU focus key before arrow right"
  match = wait_for_string(session, "focus CPU", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not focus CPU before arrow right"

  if (.not. send_text(session, achar(13))) error stop "failed to activate CPU core scroll"
  match = wait_for_string(session, "cpu cores active", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "enter did not activate CPU core scroll"

  if (.not. send_text(session, achar(27) // "[B")) error stop "failed to scroll CPU cores in grid"
  match = wait_for_string(session, "scroll", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "CPU grid arrow down did not scroll cores"

  if (.not. send_text(session, achar(13))) error stop "failed to zoom active CPU core scroll"
  match = wait_for_string(session, "zoom cpu", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "second enter did not zoom CPU"

  if (.not. send_text(session, achar(27) // "[B")) error stop "failed to scroll zoomed CPU cores"
  match = wait_for_string(session, "scroll", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "zoomed CPU arrow down did not scroll cores"

  if (.not. send_text(session, achar(27))) error stop "failed to escape CPU zoom"
  match = wait_for_string(session, "grid cpu", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "escape did not return from CPU zoom"

  if (.not. send_text(session, achar(27) // "[C")) error stop "failed to send arrow right"
  match = wait_for_string(session, "focus memory", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop arrow right did not focus memory"

  if (.not. send_text(session, "n")) error stop "failed to focus network before table active mode"
  match = wait_for_string(session, "focus Network", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not focus network before table active mode"

  if (.not. send_text(session, achar(13))) error stop "failed to activate network table"
  match = wait_for_string(session, "network table active", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "enter did not activate network table mode"

  if (.not. send_text(session, achar(27) // "[B")) error stop "failed to navigate active network table"
  match = wait_for_string(session, "navigate", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "active network arrow down did not navigate table"

  if (.not. send_text(session, achar(13))) error stop "failed to zoom active network table"
  match = wait_for_string(session, "zoom network", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "second enter did not zoom network table"

  if (.not. send_text(session, achar(27))) error stop "failed to escape network zoom"
  match = wait_for_string(session, "grid network", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "escape did not return from network zoom"

  if (.not. send_text(session, "c")) error stop "failed to send CPU focus key before arrow down"
  match = wait_for_string(session, "focus CPU", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not focus CPU before arrow down"

  if (.not. send_text(session, achar(27) // "[B")) error stop "failed to send arrow down"
  match = wait_for_string(session, "focus process", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop arrow down did not focus process"

  if (.not. send_text(session, achar(27) // "[A")) error stop "failed to send inactive process arrow up"
  match = wait_for_string(session, "focus memory", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "inactive process arrow up did not focus memory"

  if (.not. send_text(session, "p")) error stop "failed to refocus process before tree active mode"
  match = wait_for_string(session, "focus Process", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not refocus process before tree active mode"

  if (.not. send_text(session, achar(27) // "[B")) error stop "failed to activate process tree with bottom down"
  match = wait_for_string(session, "process tree active", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "bottom process down did not activate process tree mode"

  if (.not. send_text(session, achar(27))) error stop "failed to escape process tree active mode"
  match = wait_for_string(session, "focus process", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "escape did not leave process tree active mode"

  if (.not. send_text(session, achar(27) // "[A")) error stop "failed to send inactive process arrow up after escape"
  match = wait_for_string(session, "focus memory", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "escaped process arrow up did not focus memory"

  if (.not. send_text(session, "p")) error stop "failed to refocus process before active process navigation"
  match = wait_for_string(session, "focus Process", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not refocus process before active navigation"

  if (.not. send_text(session, achar(13))) error stop "failed to reactivate process tree"
  match = wait_for_string(session, "process tree active", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "enter did not reactivate process tree mode"

  if (.not. send_text(session, achar(27) // "[A")) error stop "failed to send active process arrow up"
  match = wait_for_string(session, "navigate", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "active process arrow up did not navigate process tree"

  if (.not. send_text(session, achar(13))) error stop "failed to zoom active process tree"
  match = wait_for_string(session, "zoom process", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "second enter did not zoom process tree"

  if (.not. send_text(session, achar(27))) error stop "failed to escape process zoom"
  match = wait_for_string(session, "grid process", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "escape did not return from process zoom"

  if (.not. send_text(session, achar(27) // "[Z")) error stop "failed to send Shift+Tab after arrow down"
  match = wait_for_string(session, "focus memory", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not leave process focus after arrow down"

  if (.not. send_text(session, "3")) error stop "failed to send direct layout key"
  match = wait_for_string(session, "layout process", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not jump to layout preset"

  if (.not. send_text(session, "c")) error stop "failed to send CPU focus key"
  match = wait_for_string(session, "focus CPU", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not focus CPU from global key"

  if (.not. send_text(session, "p")) error stop "failed to send process focus key"
  match = wait_for_string(session, "focus Process", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not focus process from global key"

  if (.not. send_text(session, achar(27) // "[Z")) error stop "failed to send Shift+Tab"
  match = wait_for_string(session, "focus cpu", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not jump to last focus"

  if (.not. send_text(session, "d")) error stop "failed to send disk focus key"
  match = wait_for_string(session, "focus Disk", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not focus disk from global key"

  if (.not. send_text(session, achar(13))) error stop "failed to activate disk table"
  match = wait_for_string(session, "disk table active", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "enter did not activate disk table mode"

  if (.not. send_text(session, achar(27) // "[B")) error stop "failed to send active disk arrow down"
  match = wait_for_string(session, "navigate", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "active disk arrow down did not navigate disk table"

  if (.not. send_text(session, achar(13))) error stop "failed to zoom active disk table"
  match = wait_for_string(session, "zoom disk", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "second enter did not zoom disk table"

  if (.not. send_text(session, achar(27))) error stop "failed to escape disk zoom"
  match = wait_for_string(session, "grid disk", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "escape did not return from disk zoom"

  if (.not. send_text(session, "c")) error stop "failed to reset CPU focus after disk"
  match = wait_for_string(session, "focus CPU", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not reset CPU focus after disk"

  if (.not. send_text(session, "?")) error stop "failed to send help key"
  match = wait_for_string(session, "ftop help", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not show help overlay"

  if (.not. send_text(session, "?")) error stop "failed to close help"

  if (.not. send_text(session, achar(9))) error stop "failed to send Tab"
  match = wait_for_string(session, "focus memory", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not cycle focus"

  if (.not. send_text(session, "z")) error stop "failed to send zoom key"
  match = wait_for_string(session, "zoom memory", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not zoom focused widget"

  if (.not. send_text(session, achar(27))) error stop "failed to escape memory zoom"
  match = wait_for_string(session, "grid memory", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "escape did not return from memory zoom"

  if (.not. send_text(session, achar(13))) error stop "failed to enter memory zoom"
  match = wait_for_string(session, "zoom memory", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "enter did not zoom focused memory widget"

  if (.not. send_text(session, achar(27))) error stop "failed to escape entered memory zoom"
  match = wait_for_string(session, "grid memory", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "escape did not return from entered memory zoom"

  resized_size%rows = 18
  resized_size%cols = 60
  if (.not. resize_pty(session%pty, resized_size)) error stop "failed to resize ftop PTY"

  match = wait_for_string(session, "resized", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not redraw after resize"

  if (.not. send_text(session, achar(26))) error stop "failed to send Ctrl+Z"

  match = wait_for_string(session, achar(27) // "[?1049l", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not leave alternate screen before suspend"

  if (ftop_test_send_continue(int(session%pty%child_pid, c_int)) /= 0_c_int) error stop "failed to send SIGCONT"

  match = wait_for_string(session, "resumed", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not redraw after resume"

  if (.not. send_text(session, achar(27) // "[<0;10;5M" // "q")) error stop "failed to send mouse and quit input"

  match = wait_for_string(session, achar(27) // "[?1049l", SMOKE_TIMEOUT_MS)
  if (match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not leave alternate screen"

  closed = close_expect(session)
  if (.not. closed) error stop "failed to close ftop expect session"

  if (.not. log_contains(SMOKE_LOG_PATH, "ftop v0.1.0 starting")) error stop "startup was not logged"
  if (.not. log_contains(SMOKE_LOG_PATH, "loaded layout preset")) error stop "layout load was not logged"
  if (.not. log_contains(SMOKE_LOG_PATH, "collector started")) error stop "collector start was not logged"
  if (.not. log_contains(SMOKE_LOG_PATH, "terminal size:")) error stop "terminal size was not logged"
  if (.not. log_contains(SMOKE_LOG_PATH, "ftop shutting down")) error stop "shutdown was not logged"
  if (.not. log_contains(SMOKE_LOG_PATH, "mouse press left row=5 col=10")) error stop "mouse event was not logged"

  call verify_process_filter(trim(ftop_path), trim(process_config_path), options)
  call verify_signal_exit(trim(ftop_path), argv, options)

contains

  subroutine verify_process_filter(program_path, config_path, launch_options)
    character(len=*), intent(in) :: program_path
    character(len=*), intent(in) :: config_path
    type(expect_options), intent(in) :: launch_options
    character(len=512) :: filter_argv(4)
    type(expect_session) :: filter_session
    type(expect_match) :: filter_match
    logical :: filter_closed

    filter_argv = ""
    filter_argv(1) = "--refresh-ms"
    filter_argv(2) = "500"
    filter_argv(3) = "--config"
    filter_argv(4) = config_path

    filter_session = spawn_expect(program_path, filter_argv, launch_options)
    if (filter_session%error_code /= FGOF_EXPECT_OK) error stop "failed to spawn ftop for process filter"

    filter_match = wait_for_string(filter_session, "Processes", SMOKE_TIMEOUT_MS)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "process filter frame did not render"

    filter_match = wait_for_string(filter_session, "PID", 5000)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "process table did not publish rows before signal prompt"

    if (.not. send_text(filter_session, achar(9))) error stop "failed to send filter memory Tab"
    filter_match = wait_for_string(filter_session, "focus memory", SMOKE_TIMEOUT_MS)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not focus memory before filter"

    if (.not. send_text(filter_session, achar(9))) error stop "failed to send filter disk Tab"
    filter_match = wait_for_string(filter_session, "focus disk", SMOKE_TIMEOUT_MS)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not focus disk before filter"

    if (.not. send_text(filter_session, achar(9))) error stop "failed to send filter process Tab"
    filter_match = wait_for_string(filter_session, "focus process", SMOKE_TIMEOUT_MS)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not focus process before filter"

    if (.not. send_text(filter_session, achar(27) // "[<0;18;12M")) error stop "failed to click process CPU header"
    filter_match = wait_for_string(filter_session, "sort cpu asc", SMOKE_TIMEOUT_MS)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not sort process table from mouse header"

    if (.not. send_text(filter_session, achar(27) // "[15~")) error stop "failed to send process F5"
    filter_match = wait_for_string(filter_session, "sort cpu desc", SMOKE_TIMEOUT_MS)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not sort process table from F5"

    if (.not. send_text(filter_session, "j")) error stop "failed to send process vim down"
    filter_match = wait_for_string(filter_session, "row 2/", SMOKE_TIMEOUT_MS)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "process j did not move selection down"
    if (.not. send_text(filter_session, "k")) error stop "failed to send process vim up"
    filter_match = wait_for_string(filter_session, "row 1/", SMOKE_TIMEOUT_MS)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "process k did not move selection up"
    if (.not. send_text(filter_session, achar(27) // "[19~")) error stop "failed to send process F8"
    filter_match = wait_for_string(filter_session, "following PID", SMOKE_TIMEOUT_MS)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not follow selected process from F8"
    if (.not. send_text(filter_session, achar(27) // "[19~")) error stop "failed to disable process follow"

    if (.not. send_text(filter_session, " ")) error stop "failed to tag selected process"
    filter_match = wait_for_string(filter_session, "1 tagged", SMOKE_TIMEOUT_MS)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not tag selected process"
    if (.not. send_text(filter_session, achar(27) // "[20~")) error stop "failed to send process F9"
    filter_match = wait_for_string(filter_session, "Enter send", SMOKE_TIMEOUT_MS)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not open tagged process signal prompt"
    if (.not. send_text(filter_session, achar(27) // achar(27))) error stop "failed to cancel tagged process signal prompt"
    filter_match = wait_for_string(filter_session, "cancelled", SMOKE_TIMEOUT_MS)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not cancel tagged process signal prompt"
    if (.not. send_text(filter_session, "U")) error stop "failed to clear process tags"
    filter_match = wait_for_string(filter_session, "tags cleared", SMOKE_TIMEOUT_MS)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not clear process tags"

    if (.not. send_text(filter_session, achar(27) // "[20~")) error stop "failed to send untagged process F9"
    filter_match = wait_for_string(filter_session, "signal SIGTERM pid", SMOKE_TIMEOUT_MS)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "untagged F9 did not fall back to selected process signal"
    if (.not. send_text(filter_session, achar(27) // achar(27))) error stop "failed to cancel untagged F9 signal prompt"
    filter_match = wait_for_string(filter_session, "cancelled", SMOKE_TIMEOUT_MS)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not cancel untagged F9 signal prompt"

    if (.not. send_text(filter_session, "x")) error stop "failed to send fuzzy text"
    filter_match = wait_for_string(filter_session, "Find: x_", SMOKE_TIMEOUT_MS)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "process text did not start fuzzy search"

    if (.not. send_text(filter_session, "j")) error stop "failed to send vim key in fuzzy query"
    filter_match = wait_for_string(filter_session, "Find: xj_", SMOKE_TIMEOUT_MS)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "vim key did not append to active fuzzy query"
    if (.not. send_text(filter_session, "c")) error stop "failed to send global key in fuzzy query"
    filter_match = wait_for_string(filter_session, "Find: xjc_", SMOKE_TIMEOUT_MS)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "global focus key did not append to active fuzzy query"

    filter_match = wait_for_string(filter_session, "__ftop_fuzzy_idle_timeout__", 1500)
    if (.not. send_text(filter_session, "s")) error stop "failed to send fuzzy text after idle"
    filter_match = wait_for_string(filter_session, "Find: s_", SMOKE_TIMEOUT_MS)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "idle fuzzy query did not reset before new text"

    if (.not. send_text(filter_session, achar(27) // achar(27))) error stop "failed to clear fuzzy query"
    filter_match = wait_for_string(filter_session, "history spark", SMOKE_TIMEOUT_MS)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not clear fuzzy query"

    if (.not. send_text(filter_session, achar(27) // "OQ")) error stop "failed to open process signal prompt"
    filter_match = wait_for_string(filter_session, "SIGTERM", SMOKE_TIMEOUT_MS)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not open process signal prompt"

    if (.not. send_text(filter_session, achar(27) // "[C")) error stop "failed to cycle process signal prompt"
    filter_match = wait_for_string(filter_session, "SIGKILL", SMOKE_TIMEOUT_MS)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not cycle process signal prompt"

    if (.not. send_text(filter_session, achar(13))) error stop "failed to confirm process signal prompt"
    filter_match = wait_for_string(filter_session, "confirmed enter to send", SMOKE_TIMEOUT_MS)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not confirm process signal prompt"

    if (.not. send_text(filter_session, achar(27) // achar(27))) error stop "failed to cancel process signal prompt"
    filter_match = wait_for_string(filter_session, "cancelled", SMOKE_TIMEOUT_MS)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not cancel process signal prompt"

    if (.not. send_text(filter_session, achar(27) // "OS" // "ftop")) error stop "failed to send process filter"
    filter_match = wait_for_string(filter_session, "filter ftop", SMOKE_TIMEOUT_MS)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not enter process filter"

    if (.not. send_text(filter_session, "c")) error stop "failed to send filter character c"
    filter_match = wait_for_string(filter_session, "filter ftopc", SMOKE_TIMEOUT_MS)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "global focus key captured filter text"

    if (.not. send_text(filter_session, achar(27) // achar(27))) error stop "failed to clear process filter"
    filter_match = wait_for_string(filter_session, "sort cpu desc", SMOKE_TIMEOUT_MS)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not clear process filter"

    if (.not. send_text(filter_session, achar(9))) error stop "failed to leave process focus before quit"
    if (.not. send_text(filter_session, "q")) error stop "failed to quit process filter ftop"
    filter_match = wait_for_string(filter_session, achar(27) // "[?1049l", SMOKE_TIMEOUT_MS)
    if (filter_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "process filter ftop did not exit"

    filter_closed = close_expect(filter_session)
    if (.not. filter_closed) error stop "failed to close process filter session"
  end subroutine verify_process_filter

  subroutine verify_signal_exit(program_path, launch_argv, launch_options)
    character(len=*), intent(in) :: program_path
    character(len=*), intent(in) :: launch_argv(:)
    type(expect_options), intent(in) :: launch_options
    type(expect_session) :: signal_session
    type(expect_match) :: signal_match
    logical :: signal_closed

    signal_session = spawn_expect(program_path, launch_argv, launch_options)
    if (signal_session%error_code /= FGOF_EXPECT_OK) error stop "failed to spawn ftop for signal exit"

    signal_match = wait_for_string(signal_session, "Memory", SMOKE_TIMEOUT_MS)
    if (signal_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "signal-exit frame did not render"

    if (ftop_test_send_terminate(int(signal_session%pty%child_pid, c_int)) /= 0_c_int) error stop "failed to send SIGTERM"

    signal_match = wait_for_string(signal_session, achar(27) // "[?1049l", SMOKE_TIMEOUT_MS)
    if (signal_match%status /= FGOF_EXPECT_STATUS_MATCHED) error stop "ftop did not clean up after SIGTERM"

    signal_closed = close_expect(signal_session)
    if (.not. signal_closed) error stop "failed to close signal-exit expect session"
  end subroutine verify_signal_exit

  subroutine delete_log(path)
    character(len=*), intent(in) :: path
    integer :: unit
    integer :: status

    open(newunit=unit, file=path, status="old", action="read", iostat=status)
    if (status == 0) close(unit, status="delete")
  end subroutine delete_log

  logical function log_contains(path, expected) result(found)
    character(len=*), intent(in) :: path
    character(len=*), intent(in) :: expected
    character(len=256) :: line
    integer :: unit
    integer :: status

    found = .false.
    open(newunit=unit, file=path, status="old", action="read", iostat=status)
    if (status /= 0) return

    do
      read(unit, '(a)', iostat=status) line
      if (status /= 0) exit
      if (index(trim(line), expected) > 0) then
        found = .true.
        exit
      end if
    end do

    close(unit)
  end function log_contains

end program test_terminal_smoke
