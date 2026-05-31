program test_widget_snapshots
  use fgof_screen, only : allocate_screen
  use fgof_screen_types, only : screen_buffer
  use ftop_box, only : BOX_STYLE_ROUNDED, draw_box
  use ftop_graph, only : render_graph
  use ftop_meter, only : METER_FILL_BLOCK, render_meter
  use ftop_sparkline, only : render_sparkline
  use ftop_table, only : &
    TABLE_SEPARATOR_THIN, &
    TABLE_SORT_ASCENDING, &
    TABLE_WIDTH_FIXED, &
    make_table_cell, &
    render_table, &
    table_cell, &
    table_column
  use ftop_text, only : TEXT_ALIGN_RIGHT
  use ftop_widgets, only : widget_rect
  implicit none

  integer, parameter :: SNAPSHOT_ROWS = 8
  integer, parameter :: SNAPSHOT_WIDTH = 24

  character(len=512) :: golden_path
  type(screen_buffer) :: buffer

  call get_command_argument(1, golden_path)
  call require(len_trim(golden_path) > 0, "missing widget snapshot golden path")

  buffer = render_showcase()
  call compare_golden(buffer, trim(golden_path))

contains

  function render_showcase() result(buffer)
    type(screen_buffer) :: buffer
    real :: graph_values(2)
    real :: spark_values(5)
    type(table_cell), allocatable :: cells(:, :)
    type(table_column) :: columns(2)

    buffer = allocate_screen(SNAPSHOT_WIDTH, SNAPSHOT_ROWS)

    call draw_box(buffer, widget_rect(row=1, col=1, width=16, height=3), BOX_STYLE_ROUNDED, title="CPU")
    call render_meter(buffer, widget_rect(row=4, col=1, width=10, height=1), 0.5, &
                      compact=.false., fill_mode=METER_FILL_BLOCK)

    spark_values = [0.0, 0.25, 0.50, 0.75, 1.0]
    call render_sparkline(buffer, widget_rect(row=5, col=1, width=5, height=1), spark_values, &
                          min_value=0.0, max_value=1.0)

    graph_values = [0.0, 1.0]
    call render_graph(buffer, widget_rect(row=6, col=1, width=2, height=1), graph_values, &
                      min_value=0.0, max_value=1.0, area_fill=.false.)

    columns(1)%name = "Name"
    columns(1)%width_mode = TABLE_WIDTH_FIXED
    columns(1)%width = 6
    columns(1)%sort_direction = TABLE_SORT_ASCENDING
    columns(2)%name = "CPU"
    columns(2)%width_mode = TABLE_WIDTH_FIXED
    columns(2)%width = 3
    columns(2)%alignment = TEXT_ALIGN_RIGHT
    allocate(cells(1, 2))
    cells(1, 1) = make_table_cell("ftop")
    cells(1, 2) = make_table_cell("42")
    call render_table(buffer, widget_rect(row=7, col=1, width=10, height=2), columns, cells, &
                      separator=TABLE_SEPARATOR_THIN)
  end function render_showcase

  subroutine compare_golden(buffer, path)
    type(screen_buffer), intent(in) :: buffer
    character(len=*), intent(in) :: path
    character(len=512) :: expected
    integer :: io_status
    integer :: row
    integer :: unit

    open(newunit=unit, file=path, status="old", action="read", iostat=io_status)
    call require(io_status == 0, "failed to open widget snapshot golden file")

    do row = 1, SNAPSHOT_ROWS
      read(unit, '(a)', iostat=io_status) expected
      call require(io_status == 0, "failed to read widget snapshot golden row")
      call require(trim(row_text(buffer, row)) == trim(expected), &
                   "widget snapshot row " // integer_text(row) // " mismatch")
    end do

    read(unit, '(a)', iostat=io_status) expected
    call require(io_status /= 0, "widget snapshot golden file has extra rows")
    close(unit)
  end subroutine compare_golden

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

  function integer_text(value) result(text)
    integer, intent(in) :: value
    character(len=:), allocatable :: text
    character(len=32) :: scratch

    write(scratch, '(i0)') value
    text = trim(scratch)
  end function integer_text

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

end program test_widget_snapshots
