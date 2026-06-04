program test_app_batch_signal
  use ftop_app, only : process_batch_signal_status, summarize_batch_signal_results
  implicit none

  call test_batch_signal_summary()
  call test_batch_signal_status_text()

contains

  subroutine test_batch_signal_summary()
    integer :: failed_count
    integer :: first_error_code
    integer :: sent_count

    call summarize_batch_signal_results([.true., .true., .true.], [0, 0, 0], sent_count, failed_count, first_error_code)
    call require(sent_count == 3, "summary should count successful signals")
    call require(failed_count == 0, "summary should count no failures")
    call require(first_error_code == 0, "summary should keep zero error when all succeed")

    call summarize_batch_signal_results([.true., .false., .false.], [0, 1, 3], sent_count, failed_count, first_error_code)
    call require(sent_count == 1, "summary should count partial successes")
    call require(failed_count == 2, "summary should count partial failures")
    call require(first_error_code == 1, "summary should keep first failure error")

    call summarize_batch_signal_results([.false., .false.], [3, 1], sent_count, failed_count, first_error_code)
    call require(sent_count == 0, "summary should count no successes")
    call require(failed_count == 2, "summary should count all failures")
    call require(first_error_code == 3, "summary should keep first all-failed error")
  end subroutine test_batch_signal_summary

  subroutine test_batch_signal_status_text()
    call require(process_batch_signal_status("SIGTERM", 3, 0, 0) == "sent SIGTERM to 3 processes", &
                 "status should report all-success batch signal")
    call require(process_batch_signal_status("SIGTERM", 2, 1, 1) == &
                 "sent SIGTERM to 2 processes (1 failed: permission denied)", &
                 "status should report partial permission failures")
    call require(process_batch_signal_status("SIGTERM", 0, 2, 3) == "failed to send SIGTERM: process not found", &
                 "status should report all-failed missing processes")
  end subroutine test_batch_signal_status_text

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require
end program test_app_batch_signal
