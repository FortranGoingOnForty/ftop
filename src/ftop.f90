program ftop
  use ftop_app, only : run_ftop
  use, intrinsic :: iso_fortran_env, only : error_unit
  implicit none

  character(len=*), parameter :: version = "0.1.0"
  character(len=256) :: argument
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
  case default
    write(error_unit, '(a)') "ftop: unknown option: " // trim(argument)
    write(error_unit, '(a)') "Try 'ftop --help'."
    stop 2
  end select

contains

  subroutine print_usage()
    print '(a)', "Usage: ftop [--version] [--help]"
    print '(a)', "A modern TUI system monitor written in Fortran."
  end subroutine print_usage
end program ftop
