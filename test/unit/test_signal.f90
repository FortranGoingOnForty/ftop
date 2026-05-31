program test_signal
  use ftop_signal, only : &
    FTOP_SIGNAL_TERM, &
    ftop_current_pid, &
    ftop_kill, &
    ftop_signal_check, &
    ftop_signal_clear, &
    ftop_signal_number, &
    ftop_signal_setup
  implicit none

  integer :: error_code
  integer :: pid
  integer :: signal_number

  call require(ftop_signal_setup(), "signal setup failed")

  pid = ftop_current_pid()
  call require(pid > 0, "current pid must be positive")

  signal_number = ftop_signal_number(FTOP_SIGNAL_TERM)
  call require(signal_number > 0, "SIGTERM number must be positive")

  call ftop_signal_clear(FTOP_SIGNAL_TERM)
  call require(.not. ftop_signal_check(FTOP_SIGNAL_TERM), "SIGTERM should start clear")

  call require(ftop_kill(pid, signal_number, error_code), "ftop_kill SIGTERM failed")
  call require(error_code == 0, "ftop_kill should clear error code")
  call require(wait_for_signal(FTOP_SIGNAL_TERM), "SIGTERM flag was not set")

  call ftop_signal_clear(FTOP_SIGNAL_TERM)
  call require(.not. ftop_signal_check(FTOP_SIGNAL_TERM), "SIGTERM clear failed")

  call require(.not. ftop_kill(pid, 0, error_code), "ftop_kill should reject signal zero")
  call require(error_code /= 0, "invalid ftop_kill should set an error code")

contains

  logical function wait_for_signal(signal_id) result(found)
    integer, intent(in) :: signal_id
    integer :: start_count
    integer :: current_count
    integer :: rate
    integer :: elapsed_ms

    found = .false.
    call system_clock(start_count, rate)
    do
      if (ftop_signal_check(signal_id)) then
        found = .true.
        return
      end if

      call system_clock(current_count)
      if (rate <= 0) return
      elapsed_ms = int((real(current_count - start_count) / real(rate)) * 1000.0)
      if (elapsed_ms > 1000) return
    end do
  end function wait_for_signal

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

end program test_signal
