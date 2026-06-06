program test_log
  use, intrinsic :: iso_c_binding, only : c_char, c_int, c_null_char
  use ftop_log, only : &
    LOG_LEVEL_DEBUG, &
    LOG_LEVEL_ERROR, &
    LOG_LEVEL_INFO, &
    default_log_path, &
    log_debug, &
    log_error, &
    log_info, &
    log_init, &
    log_shutdown, &
    log_warn
  implicit none

  interface
    integer(c_int) function ftop_log_test_setenv(name, value) bind(C, name="ftop_log_test_setenv")
      import :: c_char, c_int
      character(kind=c_char), intent(in) :: name(*)
      character(kind=c_char), intent(in) :: value(*)
    end function ftop_log_test_setenv

    integer(c_int) function ftop_log_test_unsetenv(name) bind(C, name="ftop_log_test_unsetenv")
      import :: c_char, c_int
      character(kind=c_char), intent(in) :: name(*)
    end function ftop_log_test_unsetenv

    subroutine ftop_log_test_write_stderr() bind(C, name="ftop_log_test_write_stderr")
    end subroutine ftop_log_test_write_stderr
  end interface

  character(len=:), allocatable :: root

  root = temp_root()
  call test_default_log_path(root)
  call test_verbosity(root)
  call test_stderr_capture(root)

contains

  subroutine test_default_log_path(root)
    character(len=*), intent(in) :: root
    character(len=:), allocatable :: actual_path
    character(len=:), allocatable :: error_message
    character(len=:), allocatable :: expected_path
    character(len=:), allocatable :: xdg_root

    xdg_root = join_path(root, "xdg-data")
    call set_env("XDG_DATA_HOME", xdg_root)
    expected_path = join_path(join_path(xdg_root, "ftop"), "ftop.log")
    actual_path = default_log_path()
    call require(actual_path == expected_path, "default log path should use XDG_DATA_HOME")

    if (.not. log_init(verbosity=LOG_LEVEL_INFO, error_message=error_message)) then
      call unset_env("XDG_DATA_HOME")
      error stop "default log_init failed: " // error_message
    end if
    call log_info("default path info")
    call log_shutdown()
    call unset_env("XDG_DATA_HOME")

    call require(file_contains(expected_path, "[INFO ]"), "default log should contain info level")
    call require(file_contains(expected_path, "default path info"), "default log should contain message")
  end subroutine test_default_log_path

  subroutine test_verbosity(root)
    character(len=*), intent(in) :: root
    character(len=:), allocatable :: debug_path
    character(len=:), allocatable :: error_message
    character(len=:), allocatable :: info_path
    character(len=:), allocatable :: quiet_path

    info_path = join_path(join_path(root, "logs"), "info.log")
    if (.not. log_init(info_path, LOG_LEVEL_INFO, error_message)) then
      error stop "info log_init failed: " // error_message
    end if
    call log_error("info error")
    call log_warn("info warn")
    call log_info("info info")
    call log_debug("info debug")
    call log_shutdown()
    call require(file_contains(info_path, "info error"), "info level should include errors")
    call require(file_contains(info_path, "info warn"), "info level should include warnings")
    call require(file_contains(info_path, "info info"), "info level should include info")
    call require(.not. file_contains(info_path, "info debug"), "info level should suppress debug")

    debug_path = join_path(join_path(root, "logs"), "debug.log")
    if (.not. log_init(debug_path, LOG_LEVEL_DEBUG, error_message)) then
      error stop "debug log_init failed: " // error_message
    end if
    call log_debug("debug visible")
    call log_shutdown()
    call require(file_contains(debug_path, "debug visible"), "debug level should include debug")

    quiet_path = join_path(join_path(root, "logs"), "quiet.log")
    if (.not. log_init(quiet_path, LOG_LEVEL_ERROR, error_message)) then
      error stop "quiet log_init failed: " // error_message
    end if
    call log_error("quiet error")
    call log_warn("quiet warn")
    call log_info("quiet info")
    call log_shutdown()
    call require(file_contains(quiet_path, "quiet error"), "quiet level should include errors")
    call require(.not. file_contains(quiet_path, "quiet warn"), "quiet level should suppress warnings")
    call require(.not. file_contains(quiet_path, "quiet info"), "quiet level should suppress info")
  end subroutine test_verbosity

  subroutine test_stderr_capture(root)
    character(len=*), intent(in) :: root
    character(len=:), allocatable :: error_message
    character(len=:), allocatable :: log_path

    log_path = join_path(join_path(root, "logs"), "stderr.log")
    if (.not. log_init(log_path, LOG_LEVEL_INFO, error_message)) then
      error stop "stderr log_init failed: " // error_message
    end if
    call ftop_log_test_write_stderr()
    call log_shutdown()

    call require(file_contains(log_path, "stderr capture fixture"), "stderr should be captured")
  end subroutine test_stderr_capture

  subroutine set_env(name, value)
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: value
    character(kind=c_char), allocatable :: c_name(:)
    character(kind=c_char), allocatable :: c_value(:)

    call to_c_string(name, c_name)
    call to_c_string(value, c_value)
    if (ftop_log_test_setenv(c_name, c_value) /= 0_c_int) error stop "setenv failed"
  end subroutine set_env

  subroutine unset_env(name)
    character(len=*), intent(in) :: name
    character(kind=c_char), allocatable :: c_name(:)

    call to_c_string(name, c_name)
    if (ftop_log_test_unsetenv(c_name) /= 0_c_int) error stop "unsetenv failed"
  end subroutine unset_env

  logical function file_contains(path, expected) result(found)
    character(len=*), intent(in) :: path
    character(len=*), intent(in) :: expected
    character(len=512) :: line
    integer :: status
    integer :: unit

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
  end function file_contains

  function temp_root() result(root)
    character(len=:), allocatable :: root
    integer :: count

    call system_clock(count)
    root = "/tmp/ftop-log-test-" // integer_text(count)
  end function temp_root

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

  subroutine to_c_string(text, c_text)
    character(len=*), intent(in) :: text
    character(kind=c_char), allocatable, intent(out) :: c_text(:)
    integer :: index_value
    integer :: text_length

    text_length = len_trim(text)
    allocate(character(kind=c_char) :: c_text(text_length + 1))
    do index_value = 1, text_length
      c_text(index_value) = char(iachar(text(index_value:index_value)), kind=c_char)
    end do
    c_text(text_length + 1) = c_null_char
  end subroutine to_c_string

  function integer_text(value) result(text)
    integer, intent(in) :: value
    character(len=:), allocatable :: text
    character(len=32) :: buffer

    write(buffer, '(i0)') value
    text = trim(buffer)
  end function integer_text

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

end program test_log
