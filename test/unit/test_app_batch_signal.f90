program test_app_batch_signal
  use ftop_app, only : process_batch_signal_status, select_draw_snapshot, summarize_batch_signal_results
  use ftop_collector, only : collector_snapshot
  implicit none

  call test_pause_snapshot_selection()
  call test_batch_signal_summary()
  call test_batch_signal_status_text()

contains

  subroutine test_pause_snapshot_selection()
    type(collector_snapshot) :: current_snapshot
    type(collector_snapshot) :: last_snapshot
    type(collector_snapshot) :: selected_snapshot
    logical :: update_last_snapshot

    last_snapshot%sample_count = 12
    current_snapshot%sample_count = 87

    selected_snapshot = select_draw_snapshot(.true., last_snapshot, current_snapshot, update_last_snapshot)
    call require(selected_snapshot%sample_count == 12, "paused draw should reuse frozen snapshot")
    call require(.not. update_last_snapshot, "paused draw should not replace frozen snapshot")

    selected_snapshot = select_draw_snapshot(.false., last_snapshot, current_snapshot, update_last_snapshot)
    call require(selected_snapshot%sample_count == 87, "unpaused draw should use current snapshot")
    call require(update_last_snapshot, "unpaused draw should refresh frozen snapshot cache")
  end subroutine test_pause_snapshot_selection

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
