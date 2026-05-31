program test_linux_loadavg
  use, intrinsic :: iso_fortran_env, only : real64
  use ftop_linux_loadavg, only : linux_loadavg_parse
  use ftop_platform_types, only : load_average_info
  implicit none

  call test_parse_loadavg()
  call test_reject_invalid_loadavg()
  call test_reject_negative_loadavg()

contains

  subroutine test_parse_loadavg()
    type(load_average_info) :: info

    call require(linux_loadavg_parse("0.12 1.25 5.50 1/123 456", info), "loadavg parse failed")
    call require(info%valid, "loadavg parse must mark valid")
    call require_close(info%values(1), 0.12_real64, "one-minute load mismatch")
    call require_close(info%values(2), 1.25_real64, "five-minute load mismatch")
    call require_close(info%values(3), 5.50_real64, "fifteen-minute load mismatch")
  end subroutine test_parse_loadavg

  subroutine test_reject_invalid_loadavg()
    type(load_average_info) :: info

    call require(.not. linux_loadavg_parse("0.12 nope 5.50", info), "invalid loadavg must fail")
    call require(.not. info%valid, "invalid loadavg must remain invalid")
  end subroutine test_reject_invalid_loadavg

  subroutine test_reject_negative_loadavg()
    type(load_average_info) :: info

    call require(.not. linux_loadavg_parse("0.12 -1.00 5.50", info), "negative loadavg must fail")
    call require(.not. info%valid, "negative loadavg must remain invalid")
  end subroutine test_reject_negative_loadavg

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

  subroutine require_close(actual, expected, message)
    real(real64), intent(in) :: actual
    real(real64), intent(in) :: expected
    character(len=*), intent(in) :: message

    if (abs(actual - expected) > 0.000000001_real64) error stop message
  end subroutine require_close

end program test_linux_loadavg
