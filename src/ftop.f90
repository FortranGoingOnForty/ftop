program ftop
  use ftop_app, only : run_ftop
  use, intrinsic :: iso_fortran_env, only : error_unit
  implicit none

  character(len=*), parameter :: version = "0.1.0"
  character(len=256) :: argument
  character(len=:), allocatable :: config_path
  integer :: argument_index
  integer :: refresh_ms
  integer :: status
  logical :: refresh_set

  refresh_ms = 0
  refresh_set = .false.

  argument_index = 1
  do while (argument_index <= command_argument_count())
    call get_command_argument(argument_index, argument)
    select case (trim(argument))
    case ("--version", "-V")
      print '(a)', "ftop " // version
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
    case default
      write(error_unit, '(a)') "ftop: unknown option: " // trim(argument)
      write(error_unit, '(a)') "Try 'ftop --help'."
      stop 2
    end select
    argument_index = argument_index + 1
  end do

  if (refresh_set .and. allocated(config_path)) then
    status = run_ftop(refresh_ms, config_path)
  else if (refresh_set) then
    status = run_ftop(refresh_ms)
  else if (allocated(config_path)) then
    status = run_ftop(config_path=config_path)
  else
    status = run_ftop()
  end if
  if (status /= 0) stop status

contains

  subroutine print_usage()
    print '(a)', "Usage: ftop [--version] [--help] [--refresh-ms N] [--config PATH]"
    print '(a)', "A modern TUI system monitor written in Fortran."
  end subroutine print_usage
end program ftop
