program ftop
  use fgof_cache, only : cache_backend_name
  use fgof_expect, only : expect_backend_name
  use fgof_fs, only : path_exists
  use fgof_keys, only : clear_decoder_state
  use fgof_lineedit, only : init_lineedit
  use fgof_process, only : command
  use fgof_screen, only : screen_buffer
  use fgof_state, only : state_backend_name
  use fgof_termios, only : get_terminal_size
  use fgof_watch, only : reset_watch
  use, intrinsic :: iso_fortran_env, only : error_unit
  implicit none

  character(len=*), parameter :: version = "0.1.0"
  character(len=256) :: argument

  if (command_argument_count() == 0) then
    print '(a)', "ftop"
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
