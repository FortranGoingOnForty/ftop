module ftop_log
  use, intrinsic :: iso_c_binding, only : c_char, c_int, c_null_char
  use fgof_fs, only : mkdir_p
  implicit none
  private

  integer, parameter, public :: LOG_LEVEL_ERROR = 0
  integer, parameter, public :: LOG_LEVEL_WARN = 1
  integer, parameter, public :: LOG_LEVEL_INFO = 2
  integer, parameter, public :: LOG_LEVEL_DEBUG = 3

  public :: default_log_path
  public :: log_debug
  public :: log_error
  public :: log_info
  public :: log_init
  public :: log_shutdown
  public :: log_warn

  interface
    integer(c_int) function c_ftop_log_open(path, level, sys_errno) bind(C, name="ftop_log_open")
      import :: c_char, c_int
      character(kind=c_char), intent(in) :: path(*)
      integer(c_int), value :: level
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_log_open

    subroutine c_ftop_log_close() bind(C, name="ftop_log_close")
    end subroutine c_ftop_log_close

    subroutine c_ftop_log_write(level, message) bind(C, name="ftop_log_write")
      import :: c_char, c_int
      integer(c_int), value :: level
      character(kind=c_char), intent(in) :: message(*)
    end subroutine c_ftop_log_write
  end interface

contains

  logical function log_init(log_path, verbosity, error_message) result(success)
    character(len=*), intent(in), optional :: log_path
    integer, intent(in), optional :: verbosity
    character(len=:), allocatable, intent(out), optional :: error_message
    character(len=:), allocatable :: path
    character(len=:), allocatable :: directory
    character(kind=c_char), allocatable :: c_path(:)
    integer(c_int) :: sys_errno
    integer :: level

    call set_error(error_message, "")

    if (present(log_path) .and. len_trim(log_path) > 0) then
      path = trim(log_path)
    else
      path = default_log_path()
    end if

    if (len_trim(path) == 0) then
      call set_error(error_message, "unable to resolve log file path")
      success = .false.
      return
    end if

    directory = parent_directory(path)
    if (len(directory) > 0 .and. directory /= ".") then
      if (.not. mkdir_p(directory)) then
        call set_error(error_message, "failed to create log directory: " // directory)
        success = .false.
        return
      end if
    end if

    level = LOG_LEVEL_INFO
    if (present(verbosity)) level = bounded_log_level(verbosity)
    call to_c_string(path, c_path)
    success = c_ftop_log_open(c_path, int(level, c_int), sys_errno) == 0_c_int
    if (.not. success) then
      call set_error(error_message, "failed to open log file: errno=" // integer_text(int(sys_errno)))
    end if
  end function log_init

  subroutine log_shutdown()
    call c_ftop_log_close()
  end subroutine log_shutdown

  subroutine log_error(message)
    character(len=*), intent(in) :: message

    call log_write(LOG_LEVEL_ERROR, message)
  end subroutine log_error

  subroutine log_warn(message)
    character(len=*), intent(in) :: message

    call log_write(LOG_LEVEL_WARN, message)
  end subroutine log_warn

  subroutine log_info(message)
    character(len=*), intent(in) :: message

    call log_write(LOG_LEVEL_INFO, message)
  end subroutine log_info

  subroutine log_debug(message)
    character(len=*), intent(in) :: message

    call log_write(LOG_LEVEL_DEBUG, message)
  end subroutine log_debug

  function default_log_path() result(path)
    character(len=:), allocatable :: path
    character(len=:), allocatable :: base
    character(len=:), allocatable :: home
    character(len=:), allocatable :: xdg_data_home

    xdg_data_home = environment_value("XDG_DATA_HOME")
    if (len(xdg_data_home) > 0) then
      base = xdg_data_home
    else
      home = environment_value("HOME")
      if (len(home) > 0) then
        base = join_path(join_path(home, ".local"), "share")
      else
        base = "."
      end if
    end if

    path = join_path(join_path(base, "ftop"), "ftop.log")
  end function default_log_path

  subroutine log_write(level, message)
    integer, intent(in) :: level
    character(len=*), intent(in) :: message
    character(kind=c_char), allocatable :: c_message(:)

    call to_c_string(trim(message), c_message)
    call c_ftop_log_write(int(level, c_int), c_message)
  end subroutine log_write

  integer function bounded_log_level(level) result(bounded)
    integer, intent(in) :: level

    bounded = max(LOG_LEVEL_ERROR, min(LOG_LEVEL_DEBUG, level))
  end function bounded_log_level

  function parent_directory(path) result(directory)
    character(len=*), intent(in) :: path
    character(len=:), allocatable :: directory
    integer :: index_value

    directory = "."
    do index_value = len_trim(path), 1, -1
      if (path(index_value:index_value) == "/") then
        if (index_value == 1) then
          directory = "/"
        else
          directory = path(:index_value - 1)
        end if
        return
      end if
    end do
  end function parent_directory

  function environment_value(name) result(value)
    character(len=*), intent(in) :: name
    character(len=:), allocatable :: value
    integer :: length
    integer :: status

    call get_environment_variable(name, length=length, status=status)
    if (status /= 0 .or. length <= 0) then
      value = ""
      return
    end if

    allocate(character(len=length) :: value)
    call get_environment_variable(name, value, status=status)
    if (status /= 0) value = ""
  end function environment_value

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

  subroutine set_error(error_message, message)
    character(len=:), allocatable, intent(out), optional :: error_message
    character(len=*), intent(in) :: message

    if (present(error_message)) error_message = message
  end subroutine set_error

  function integer_text(value) result(text)
    integer, intent(in) :: value
    character(len=:), allocatable :: text
    character(len=32) :: buffer

    write(buffer, '(i0)') value
    text = trim(buffer)
  end function integer_text

end module ftop_log
