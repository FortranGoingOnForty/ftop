program test_gpu_widget
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use fgof_screen, only : allocate_screen, clear_screen_style
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_collector, only : collector_snapshot
  use ftop_gpu, only : render_gpu_panel
  use ftop_gpu_data, only : empty_gpu_table
  use ftop_widgets, only : widget_rect
  implicit none

  integer(int64), parameter :: GIB = 1024_int64 * 1024_int64 * 1024_int64

  call test_gpu_unavailable_state()
  call test_gpu_empty_state()
  call test_gpu_metrics_render()

contains

  subroutine test_gpu_unavailable_state()
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot
    type(screen_style) :: style
    character(len=:), allocatable :: text

    style = clear_screen_style()
    buffer = allocate_screen(48, 8)
    call render_gpu_panel(buffer, widget_rect(1, 1, 48, 8), snapshot, style, style, style)
    text = buffer_text(buffer)

    call require(index(text, "GPU") > 0, "gpu panel should draw title")
    call require(index(text, "gpu unavailable") > 0, "invalid GPU table should render unavailable state")
  end subroutine test_gpu_unavailable_state

  subroutine test_gpu_empty_state()
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot
    type(screen_style) :: style
    character(len=:), allocatable :: text

    style = clear_screen_style()
    snapshot%gpu = empty_gpu_table()
    buffer = allocate_screen(48, 8)
    call render_gpu_panel(buffer, widget_rect(1, 1, 48, 8), snapshot, style, style, style)
    text = buffer_text(buffer)

    call require(index(text, "No GPUs detected") > 0, "empty GPU table should render no-GPU state")
  end subroutine test_gpu_empty_state

  subroutine test_gpu_metrics_render()
    type(screen_buffer) :: buffer
    type(collector_snapshot) :: snapshot
    type(screen_style) :: style
    character(len=:), allocatable :: text

    style = clear_screen_style()
    call populate_gpu_snapshot(snapshot)
    buffer = allocate_screen(64, 9)
    call render_gpu_panel(buffer, widget_rect(1, 1, 64, 9), snapshot, style, style, style)
    text = buffer_text(buffer)

    call require(index(text, "GPUs 1") > 0, "gpu panel should render GPU count")
    call require(index(text, "nvidia Test RTX") > 0, "gpu panel should render vendor and name")
    call require(index(text, "GPU 72.5%") > 0, "gpu panel should render utilization label")
    call require(index(text, "VRAM 6.0 GiB / 12.0 GiB") > 0, "gpu panel should render VRAM usage")
    call require(index(text, "temp 63.5 C") > 0, "gpu panel should render temperature")
    call require(index(text, "power 120.0 W / 250.0 W") > 0, "gpu panel should render power draw")
  end subroutine test_gpu_metrics_render

  subroutine populate_gpu_snapshot(snapshot)
    type(collector_snapshot), intent(out) :: snapshot

    snapshot%gpu%valid = .true.
    allocate(snapshot%gpu%gpus(1))
    snapshot%gpu%gpus(1)%valid = .true.
    snapshot%gpu%gpus(1)%vendor = "nvidia"
    snapshot%gpu%gpus(1)%name = "Test RTX"
    snapshot%gpu%gpus(1)%utilization_valid = .true.
    snapshot%gpu%gpus(1)%utilization_percent = 72.5_real64
    snapshot%gpu%gpus(1)%memory_valid = .true.
    snapshot%gpu%gpus(1)%memory_used_bytes = 6_int64 * GIB
    snapshot%gpu%gpus(1)%memory_total_bytes = 12_int64 * GIB
    snapshot%gpu%gpus(1)%temperature_valid = .true.
    snapshot%gpu%gpus(1)%temp_celsius = 63.5_real64
    snapshot%gpu%gpus(1)%power_valid = .true.
    snapshot%gpu%gpus(1)%power_watts = 120.0_real64
    snapshot%gpu%gpus(1)%power_limit_watts = 250.0_real64
  end subroutine populate_gpu_snapshot

  function buffer_text(buffer) result(text)
    type(screen_buffer), intent(in) :: buffer
    character(len=:), allocatable :: text
    integer :: col
    integer :: row

    text = ""
    do row = 1, buffer%size%height
      do col = 1, buffer%size%width
        if (allocated(buffer%cells(row, col)%glyph)) then
          text = text // buffer%cells(row, col)%glyph
        else
          text = text // " "
        end if
      end do
    end do
  end function buffer_text

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require
end program test_gpu_widget
