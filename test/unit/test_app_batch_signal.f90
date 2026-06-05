program test_app_batch_signal
  use ftop_app, only : adjusted_refresh_ms, process_batch_signal_status, refresh_status_text, select_draw_snapshot
  use ftop_app, only : summarize_batch_signal_results
  use ftop_app, only : process_vim_navigation_delta, vim_navigation_delta
  use ftop_collector, only : collector_snapshot
  implicit none

  call test_pause_snapshot_selection()
  call test_refresh_adjustment()
  call test_vim_navigation_deltas()
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

  subroutine test_refresh_adjustment()
    call require(adjusted_refresh_ms(1000, -1) == 500, "plus should reduce refresh interval to previous preset")
    call require(adjusted_refresh_ms(1000, 1) == 2000, "minus should increase refresh interval to next preset")
    call require(adjusted_refresh_ms(900, -1) == 500, "plus should move custom refresh to next faster preset")
    call require(adjusted_refresh_ms(900, 1) == 1000, "minus should move custom refresh to next slower preset")
    call require(adjusted_refresh_ms(250, -1) == 250, "faster refresh should clamp to minimum preset")
    call require(adjusted_refresh_ms(5000, 1) == 5000, "slower refresh should clamp to maximum preset")
    call require(adjusted_refresh_ms(50, 0) == 250, "neutral refresh adjustment should snap to nearest preset")
    call require(refresh_status_text(500) == "Refresh: 500ms", "refresh status should report preset milliseconds")
  end subroutine test_refresh_adjustment

  subroutine test_vim_navigation_deltas()
    call require(process_vim_navigation_delta("j", 0, 10) == 1, "process j should move down without fuzzy")
    call require(process_vim_navigation_delta("k", 0, 10) == -1, "process k should move up without fuzzy")
    call require(process_vim_navigation_delta("g", 0, 10) == -10, "process g should jump to first without fuzzy")
    call require(process_vim_navigation_delta("G", 0, 10) == 10, "process G should jump to last without fuzzy")
    call require(process_vim_navigation_delta("j", 1, 10) == 0, "process j should go to fuzzy when fuzzy active")
    call require(process_vim_navigation_delta("G", 1, 10) == 0, "process G should go to fuzzy when fuzzy active")
    call require(vim_navigation_delta("j", 4) == 1, "network j should move down")
    call require(vim_navigation_delta("k", 4) == -1, "network k should move up")
    call require(vim_navigation_delta("g", 4) == -4, "network g should jump first")
    call require(vim_navigation_delta("G", 4) == 4, "network G should jump last")
  end subroutine test_vim_navigation_deltas

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
