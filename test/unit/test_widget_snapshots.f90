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
  use ftop_text, only : TEXT_ALIGN_RIGHT, render_text
  use ftop_widgets, only : widget_rect
  implicit none

  integer, parameter :: SNAPSHOT_WIDTH = 24

  character(len=512) :: command
  character(len=512) :: golden_root
  logical :: print_snapshots

  call get_command_argument(1, golden_root)
  call get_command_argument(2, command)
  call require(len_trim(golden_root) > 0, "missing widget snapshot golden directory")
  print_snapshots = trim(command) == "--print"

  call compare_snapshot("text", render_text_snapshot(), golden_file(golden_root, "widget_text.txt"), 1, print_snapshots)
  call compare_snapshot("box", render_box_snapshot(), golden_file(golden_root, "widget_box.txt"), 3, print_snapshots)
  call compare_snapshot("meter", render_meter_snapshot(), golden_file(golden_root, "widget_meter.txt"), 1, print_snapshots)
  call compare_snapshot("sparkline", render_sparkline_snapshot(), golden_file(golden_root, "widget_sparkline.txt"), 1, &
                        print_snapshots)
  call compare_snapshot("graph", render_graph_snapshot(), golden_file(golden_root, "widget_graph.txt"), 1, print_snapshots)
  call compare_snapshot("table", render_table_snapshot(), golden_file(golden_root, "widget_table.txt"), 2, print_snapshots)
  call compare_snapshot("showcase", render_showcase(), golden_file(golden_root, "widget_showcase.txt"), 8, print_snapshots)

contains

  function render_text_snapshot() result(buffer)
    type(screen_buffer) :: buffer

    buffer = allocate_screen(SNAPSHOT_WIDTH, 1)
    call render_text(buffer, widget_rect(row=1, col=1, width=6, height=1), "Hi ▲")
  end function render_text_snapshot

  function render_box_snapshot() result(buffer)
    type(screen_buffer) :: buffer

    buffer = allocate_screen(SNAPSHOT_WIDTH, 3)
    call draw_box(buffer, widget_rect(row=1, col=1, width=16, height=3), BOX_STYLE_ROUNDED, title="CPU")
  end function render_box_snapshot

  function render_meter_snapshot() result(buffer)
    type(screen_buffer) :: buffer

    buffer = allocate_screen(SNAPSHOT_WIDTH, 1)
    call render_meter(buffer, widget_rect(row=1, col=1, width=10, height=1), 0.5, &
                      compact=.false., fill_mode=METER_FILL_BLOCK)
  end function render_meter_snapshot

  function render_sparkline_snapshot() result(buffer)
    type(screen_buffer) :: buffer
    real :: spark_values(5)

    buffer = allocate_screen(SNAPSHOT_WIDTH, 1)
    spark_values = [0.0, 0.25, 0.50, 0.75, 1.0]
    call render_sparkline(buffer, widget_rect(row=1, col=1, width=5, height=1), spark_values, &
                          min_value=0.0, max_value=1.0)
  end function render_sparkline_snapshot

  function render_graph_snapshot() result(buffer)
    type(screen_buffer) :: buffer
    real :: graph_values(2)

    buffer = allocate_screen(SNAPSHOT_WIDTH, 1)
    graph_values = [0.0, 1.0]
    call render_graph(buffer, widget_rect(row=1, col=1, width=2, height=1), graph_values, &
                      min_value=0.0, max_value=1.0, area_fill=.false.)
  end function render_graph_snapshot

  function render_table_snapshot() result(buffer)
    type(screen_buffer) :: buffer
    type(table_cell), allocatable :: cells(:, :)
    type(table_column) :: columns(2)

    buffer = allocate_screen(SNAPSHOT_WIDTH, 2)
    call table_fixture(columns, cells)
    call render_table(buffer, widget_rect(row=1, col=1, width=10, height=2), columns, cells, &
                      separator=TABLE_SEPARATOR_THIN)
  end function render_table_snapshot

  function render_showcase() result(buffer)
    type(screen_buffer) :: buffer
    type(table_cell), allocatable :: cells(:, :)
    type(table_column) :: columns(2)

    buffer = allocate_screen(SNAPSHOT_WIDTH, 8)

    call draw_box(buffer, widget_rect(row=1, col=1, width=16, height=3), BOX_STYLE_ROUNDED, title="CPU")
    call render_meter(buffer, widget_rect(row=4, col=1, width=10, height=1), 0.5, &
                      compact=.false., fill_mode=METER_FILL_BLOCK)

    call render_sparkline(buffer, widget_rect(row=5, col=1, width=5, height=1), &
                          [0.0, 0.25, 0.50, 0.75, 1.0], &
                          min_value=0.0, max_value=1.0)

    call render_graph(buffer, widget_rect(row=6, col=1, width=2, height=1), [0.0, 1.0], &
                      min_value=0.0, max_value=1.0, area_fill=.false.)

    call table_fixture(columns, cells)
    call render_table(buffer, widget_rect(row=7, col=1, width=10, height=2), columns, cells, &
                      separator=TABLE_SEPARATOR_THIN)
  end function render_showcase

  subroutine table_fixture(columns, cells)
    type(table_column), intent(out) :: columns(2)
    type(table_cell), allocatable, intent(out) :: cells(:, :)

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
  end subroutine table_fixture

  function golden_file(root, name) result(path)
    character(len=*), intent(in) :: root
    character(len=*), intent(in) :: name
    character(len=:), allocatable :: path

    path = trim(root) // "/" // name
  end function golden_file

  subroutine compare_snapshot(name, buffer, path, rows, print_snapshot)
    character(len=*), intent(in) :: name
    type(screen_buffer), intent(in) :: buffer
    character(len=*), intent(in) :: path
    integer, intent(in) :: rows
    logical, intent(in) :: print_snapshot

    if (print_snapshot) call print_buffer(name, buffer, rows)
    call compare_golden(buffer, path, rows, name)
  end subroutine compare_snapshot

  subroutine compare_golden(buffer, path, rows, name)
    type(screen_buffer), intent(in) :: buffer
    character(len=*), intent(in) :: path
    integer, intent(in) :: rows
    character(len=*), intent(in) :: name
    character(len=512) :: expected
    integer :: io_status
    integer :: row
    integer :: unit

    open(newunit=unit, file=path, status="old", action="read", iostat=io_status)
    call require(io_status == 0, "failed to open widget snapshot golden file")

    do row = 1, rows
      read(unit, '(a)', iostat=io_status) expected
      call require(io_status == 0, "failed to read widget snapshot golden row")
      call require(trim(row_text(buffer, row)) == trim(expected), &
                   "widget snapshot " // name // " row " // integer_text(row) // " mismatch")
    end do

    read(unit, '(a)', iostat=io_status) expected
    call require(io_status /= 0, "widget snapshot golden file has extra rows")
    close(unit)
  end subroutine compare_golden

  subroutine print_buffer(name, buffer, rows)
    character(len=*), intent(in) :: name
    type(screen_buffer), intent(in) :: buffer
    integer, intent(in) :: rows
    integer :: row

    write(*, '(a)') "== " // name // " =="
    do row = 1, rows
      write(*, '(a)') trim(row_text(buffer, row))
    end do
  end subroutine print_buffer

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
