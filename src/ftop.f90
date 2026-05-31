program ftop
  use ftop_app, only : run_ftop
  use, intrinsic :: iso_fortran_env, only : error_unit
  implicit none

  character(len=*), parameter :: version = "0.1.0"
  character(len=256) :: argument
  integer :: refresh_ms
  integer :: status

  if (command_argument_count() == 0) then
    status = run_ftop()
    if (status /= 0) stop status
    stop
  end if

  call get_command_argument(1, argument)

  select case (trim(argument))
  case ("--version", "-V")
    print '(a)', "ftop " // version
  case ("--help", "-h")
    call print_usage()
  case ("--refresh-ms")
    if (command_argument_count() /= 2) then
      write(error_unit, '(a)') "ftop: --refresh-ms requires a millisecond value"
      stop 2
    end if
    call get_command_argument(2, argument)
    read(argument, *, iostat=status) refresh_ms
    if (status /= 0 .or. refresh_ms <= 0) then
      write(error_unit, '(a)') "ftop: --refresh-ms must be a positive integer"
      stop 2
    end if
    status = run_ftop(refresh_ms)
    if (status /= 0) stop status
  case default
    write(error_unit, '(a)') "ftop: unknown option: " // trim(argument)
    write(error_unit, '(a)') "Try 'ftop --help'."
    stop 2
  end select

contains

  subroutine print_usage()
    print '(a)', "Usage: ftop [--version] [--help] [--refresh-ms N]"
    print '(a)', "A modern TUI system monitor written in Fortran."
  end subroutine print_usage
end program ftop
