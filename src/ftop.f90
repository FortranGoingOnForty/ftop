program ftop
  use ftop_app, only : FTOP_VERSION, run_ftop
  use ftop_log, only : LOG_LEVEL_DEBUG, LOG_LEVEL_ERROR, LOG_LEVEL_INFO
  use, intrinsic :: iso_fortran_env, only : error_unit
  implicit none

  character(len=256) :: argument
  character(len=:), allocatable :: config_path
  character(len=:), allocatable :: log_path
  integer :: argument_index
  integer :: log_level
  integer :: refresh_ms
  integer :: status
  logical :: config_set
  logical :: refresh_set

  refresh_ms = 0
  config_path = ""
  log_level = LOG_LEVEL_INFO
  log_path = ""
  config_set = .false.
  refresh_set = .false.

  argument_index = 1
  do while (argument_index <= command_argument_count())
    call get_command_argument(argument_index, argument)
    select case (trim(argument))
    case ("--version", "-V")
      print '(a)', "ftop " // FTOP_VERSION
      stop
    case ("--help", "-h")
      call print_usage()
      stop
    case ("--refresh-ms")
      if (argument_index + 1 > command_argument_count()) then
        write(error_unit, '(a)') "ftop: --refresh-ms requires a millisecond value"
        stop 2
      end if
      argument_index = argument_index + 1
      call get_command_argument(argument_index, argument)
      read(argument, *, iostat=status) refresh_ms
      if (status /= 0 .or. refresh_ms <= 0) then
        write(error_unit, '(a)') "ftop: --refresh-ms must be a positive integer"
        stop 2
      end if
      refresh_set = .true.
    case ("--config")
      if (argument_index + 1 > command_argument_count()) then
        write(error_unit, '(a)') "ftop: --config requires a path"
        stop 2
      end if
      argument_index = argument_index + 1
      call get_command_argument(argument_index, argument)
      if (len_trim(argument) == 0) then
        write(error_unit, '(a)') "ftop: --config requires a non-empty path"
        stop 2
      end if
      config_path = trim(argument)
      config_set = .true.
    case ("--log-file")
      if (argument_index + 1 > command_argument_count()) then
        write(error_unit, '(a)') "ftop: --log-file requires a path"
        stop 2
      end if
      argument_index = argument_index + 1
      call get_command_argument(argument_index, argument)
      if (len_trim(argument) == 0) then
        write(error_unit, '(a)') "ftop: --log-file requires a non-empty path"
        stop 2
      end if
      log_path = trim(argument)
    case ("--verbose")
      log_level = LOG_LEVEL_DEBUG
    case ("--quiet")
      log_level = LOG_LEVEL_ERROR
    case default
      write(error_unit, '(a)') "ftop: unknown option: " // trim(argument)
      write(error_unit, '(a)') "Try 'ftop --help'."
      stop 2
    end select
    argument_index = argument_index + 1
  end do

  if (refresh_set .and. config_set) then
    status = run_ftop(refresh_ms, config_path, log_path=log_path, log_level=log_level)
  else if (refresh_set) then
    status = run_ftop(refresh_ms, log_path=log_path, log_level=log_level)
  else if (config_set) then
    status = run_ftop(config_path=config_path, log_path=log_path, log_level=log_level)
  else
    status = run_ftop(log_path=log_path, log_level=log_level)
  end if
  if (status /= 0) stop status

contains

  subroutine print_usage()
    print '(a)', "Usage: ftop [--version] [--help] [--refresh-ms N] [--config PATH] [--log-file PATH]"
    print '(a)', "            [--verbose] [--quiet]"
    print '(a)', "A modern TUI system monitor written in Fortran."
  end subroutine print_usage
end program ftop
