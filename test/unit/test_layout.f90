program test_layout
  use ftop_layout, only : &
    LAYOUT_WIDGET_CPU, &
    LAYOUT_WIDGET_MEMORY, &
    default_dashboard_layout, &
    distribute_weighted_space, &
    layout_assignment, &
    layout_grid, &
    make_layout_column, &
    make_layout_row, &
    resolve_layout
  use ftop_widgets, only : widget_rect
  implicit none

  call test_weight_distribution()
  call test_grid_resolution()
  call test_dashboard_fallback_layout()

contains

  subroutine test_weight_distribution()
    integer, allocatable :: sizes(:)

    sizes = distribute_weighted_space(10, [1, 2], [2, 2])
    call require(size(sizes) == 2, "weighted distribution should preserve item count")
    call require(sum(sizes) == 10, "weighted distribution should fill available space")
    call require(sizes(1) == 4 .and. sizes(2) == 6, "weighted distribution should honor weights")

    sizes = distribute_weighted_space(4, [1, 1], [3, 3])
    call require(sizes(1) == 3 .and. sizes(2) == 3, "minimums should not be shrunk")
  end subroutine test_weight_distribution

  subroutine test_grid_resolution()
    type(layout_assignment), allocatable :: assignments(:)
    type(layout_grid) :: grid

    allocate(grid%rows(2))
    grid%rows(1) = make_layout_row(1, [ &
      make_layout_column(LAYOUT_WIDGET_CPU, 1, 10, 4), &
      make_layout_column(LAYOUT_WIDGET_MEMORY, 3, 10, 4) &
    ])
    grid%rows(2) = make_layout_row(1, [make_layout_column("process", 1, 20, 4)])

    assignments = resolve_layout(grid, widget_rect(2, 3, 80, 20))
    call require(size(assignments) == 3, "grid should produce one assignment per column")
    call require(assignments(1)%widget == LAYOUT_WIDGET_CPU, "first assignment should be cpu")
    call require(assignments(1)%rect%row == 2 .and. assignments(1)%rect%col == 3, &
                 "first assignment should start at grid origin")
    call require(assignments(1)%rect%width < assignments(2)%rect%width, &
                 "column weights should affect widths")
    call require(assignments(3)%rect%row > assignments(1)%rect%row, &
                 "second row should be below first row")
  end subroutine test_grid_resolution

  subroutine test_dashboard_fallback_layout()
    use ftop_layout, only : dashboard_layout
    type(dashboard_layout) :: layout

    layout = default_dashboard_layout(80, 24)
    call require(layout%cpu_panel%row == 3, "wide dashboard should place cpu in body")
    call require(layout%memory_panel%row == layout%cpu_panel%row, "wide dashboard should be one row")
    call require(layout%memory_panel%col > layout%cpu_panel%col, "wide dashboard should place memory right")

    layout = default_dashboard_layout(60, 20)
    call require(layout%memory_panel%row > layout%cpu_panel%row, "narrow dashboard should stack panels")
    call require(layout%memory_panel%col == layout%cpu_panel%col, "stacked panels should align")
  end subroutine test_dashboard_fallback_layout

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

end program test_layout
