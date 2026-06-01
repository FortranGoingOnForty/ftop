program test_process_table
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use fgof_screen, only : allocate_screen
  use fgof_screen_types, only : screen_buffer, screen_style
  use ftop_collector, only : collector_snapshot
  use ftop_color, only : COLOR_UI_ACCENT, COLOR_UI_BORDER, COLOR_UI_DIM, COLOR_UI_PANEL, style_from_rgb
  use ftop_process_table, only : render_process_panel
  use ftop_widgets, only : widget_rect
  implicit none

  type(screen_buffer) :: buffer
  type(screen_style) :: border_style
  type(screen_style) :: dim_style
  type(screen_style) :: title_style

  buffer = allocate_screen(80, 10)
  border_style = style_from_rgb(fg=COLOR_UI_BORDER)
  title_style = style_from_rgb(fg=COLOR_UI_ACCENT, bg=COLOR_UI_PANEL, bold=.true.)
  dim_style = style_from_rgb(fg=COLOR_UI_DIM)

  call render_process_panel(buffer, widget_rect(1, 1, 80, 10), sample_snapshot(), border_style, title_style, dim_style)
  call require(index(row_text(buffer, 2), "PID") > 0, "process table should render PID header")
  call require(index(row_text(buffer, 3), "100") > 0, "process table should render pid")
  call require(index(row_text(buffer, 3), "parent --test") > 0, "process table should render command")
  call require(index(row_text(buffer, 3), "12.5%") > 0, "process table should render cpu percent")
  call require(index(row_text(buffer, 3), "64.0 MiB") > 0, "process table should render RSS")
  call require(index(row_text(buffer, 4), "200") > 0, "process table should render child pid")
  call require(index(row_text(buffer, 4), "└") > 0, "process table should render child branch")
  call require(index(row_text(buffer, 4), "child --task") > 0, "process table should render child command")

contains

  function sample_snapshot() result(snapshot)
    type(collector_snapshot) :: snapshot

    snapshot%processes%valid = .true.
    allocate(snapshot%processes%items(2))
    snapshot%processes%items(1)%valid = .true.
    snapshot%processes%items(1)%pid = 100
    snapshot%processes%items(1)%uid = 1001
    snapshot%processes%items(1)%user_valid = .true.
    snapshot%processes%items(1)%user = "tester"
    snapshot%processes%items(1)%name = "parent"
    snapshot%processes%items(1)%command = "parent --test"
    snapshot%processes%items(1)%state = "R"
    snapshot%processes%items(1)%cpu_percent = 12.5_real64
    snapshot%processes%items(1)%mem_percent = 1.5_real64
    snapshot%processes%items(1)%mem_rss_bytes = 64_int64 * 1024_int64 * 1024_int64
    snapshot%processes%items(2)%valid = .true.
    snapshot%processes%items(2)%pid = 200
    snapshot%processes%items(2)%ppid = 100
    snapshot%processes%items(2)%uid = 1001
    snapshot%processes%items(2)%user_valid = .true.
    snapshot%processes%items(2)%user = "tester"
    snapshot%processes%items(2)%name = "child"
    snapshot%processes%items(2)%command = "child --task"
    snapshot%processes%items(2)%state = "S"
  end function sample_snapshot

  function row_text(buffer, row) result(text)
    type(screen_buffer), intent(in) :: buffer
    integer, intent(in) :: row
    character(len=:), allocatable :: text
    integer :: col

    text = ""
    do col = 1, buffer%size%width
      if (allocated(buffer%cells(row, col)%glyph)) then
        text = text // buffer%cells(row, col)%glyph
      else
        text = text // " "
      end if
    end do
  end function row_text

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

end program test_process_table
