module ftop_signal
  use, intrinsic :: iso_c_binding, only : c_int
  implicit none
  private

  integer, parameter, public :: FTOP_SIGNAL_WINCH = 1
  integer, parameter, public :: FTOP_SIGNAL_INT = 2
  integer, parameter, public :: FTOP_SIGNAL_TERM = 3
  integer, parameter, public :: FTOP_SIGNAL_TSTP = 4
  integer, parameter, public :: FTOP_SIGNAL_CONT = 5
  integer, parameter, public :: FTOP_SIGNAL_HUP = 6
  integer, parameter, public :: FTOP_SIGNAL_KILL = 7
  integer, parameter, public :: FTOP_SIGNAL_STOP = 8
  integer, parameter, public :: FTOP_SIGNAL_USR1 = 9
  integer, parameter, public :: FTOP_SIGNAL_USR2 = 10

  public :: ftop_current_pid
  public :: ftop_kill
  public :: ftop_signal_check
  public :: ftop_signal_clear
  public :: ftop_signal_number
  public :: ftop_signal_setup
  public :: ftop_signal_suspend_self

  interface
    integer(c_int) function c_ftop_signal_setup() bind(C, name="ftop_signal_setup")
      import :: c_int
    end function c_ftop_signal_setup

    integer(c_int) function c_ftop_signal_check(signal_id) bind(C, name="ftop_signal_check")
      import :: c_int
      integer(c_int), value :: signal_id
    end function c_ftop_signal_check

    subroutine c_ftop_signal_clear(signal_id) bind(C, name="ftop_signal_clear")
      import :: c_int
      integer(c_int), value :: signal_id
    end subroutine c_ftop_signal_clear

    integer(c_int) function c_ftop_signal_suspend_self() bind(C, name="ftop_signal_suspend_self")
      import :: c_int
    end function c_ftop_signal_suspend_self

    integer(c_int) function c_ftop_signal_number(signal_id) bind(C, name="ftop_signal_number")
      import :: c_int
      integer(c_int), value :: signal_id
    end function c_ftop_signal_number

    integer(c_int) function c_ftop_current_pid() bind(C, name="ftop_current_pid")
      import :: c_int
    end function c_ftop_current_pid

    integer(c_int) function c_ftop_kill(pid, signal_number, sys_errno) bind(C, name="ftop_kill")
      import :: c_int
      integer(c_int), value :: pid
      integer(c_int), value :: signal_number
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_kill
  end interface

contains

  logical function ftop_signal_setup() result(success)
    success = c_ftop_signal_setup() == 0_c_int
  end function ftop_signal_setup

  logical function ftop_signal_check(signal_id) result(pending)
    integer, intent(in) :: signal_id

    pending = c_ftop_signal_check(int(signal_id, c_int)) /= 0_c_int
  end function ftop_signal_check

  subroutine ftop_signal_clear(signal_id)
    integer, intent(in) :: signal_id

    call c_ftop_signal_clear(int(signal_id, c_int))
  end subroutine ftop_signal_clear

  logical function ftop_signal_suspend_self() result(success)
    success = c_ftop_signal_suspend_self() == 0_c_int
  end function ftop_signal_suspend_self

  integer function ftop_signal_number(signal_id) result(signal_number)
    integer, intent(in) :: signal_id

    signal_number = int(c_ftop_signal_number(int(signal_id, c_int)))
  end function ftop_signal_number

  integer function ftop_current_pid() result(pid)
    pid = int(c_ftop_current_pid())
  end function ftop_current_pid

  logical function ftop_kill(pid, signal_number, error_code) result(success)
    integer, intent(in) :: pid
    integer, intent(in) :: signal_number
    integer, intent(out), optional :: error_code
    integer(c_int) :: sys_errno

    success = c_ftop_kill(int(pid, c_int), int(signal_number, c_int), sys_errno) == 0_c_int
    if (present(error_code)) error_code = int(sys_errno)
  end function ftop_kill

end module ftop_signal
