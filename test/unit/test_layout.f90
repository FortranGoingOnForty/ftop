program test_layout
  use ftop_layout, only : &
    LAYOUT_WIDGET_CPU, &
    LAYOUT_WIDGET_MEMORY, &
    LAYOUT_WIDGET_NETWORK, &
    LAYOUT_WIDGET_PROCESS, &
    default_dashboard_layout, &
    distribute_weighted_space, &
    layout_assignment, &
    layout_error, &
    layout_grid, &
    make_layout_column, &
    make_layout_row, &
    layout_widget_registered, &
    layout_widget_renderable, &
    parse_layout_toml, &
    resolve_layout
  use ftop_widgets, only : widget_rect
  implicit none

  call test_weight_distribution()
  call test_widget_registry()
  call test_parse_layout_toml()
  call test_reject_invalid_layout_toml()
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

  subroutine test_widget_registry()
    call require(layout_widget_registered(LAYOUT_WIDGET_CPU), "cpu should be registered")
    call require(layout_widget_registered(LAYOUT_WIDGET_MEMORY), "memory should be registered")
    call require(layout_widget_registered(LAYOUT_WIDGET_NETWORK), "network should be registered")
    call require(layout_widget_registered(LAYOUT_WIDGET_PROCESS), "process should be registered")
    call require(layout_widget_renderable(LAYOUT_WIDGET_CPU), "cpu should be renderable")
    call require(layout_widget_renderable(LAYOUT_WIDGET_MEMORY), "memory should be renderable")
    call require(layout_widget_renderable(LAYOUT_WIDGET_NETWORK), "network should be renderable")
    call require(layout_widget_renderable(LAYOUT_WIDGET_PROCESS), "process should be renderable")
    call require(.not. layout_widget_registered("unknown"), "unknown widget should not be registered")
  end subroutine test_widget_registry

  subroutine test_parse_layout_toml()
    character(len=1) :: nl
    type(layout_error) :: error
    type(layout_grid) :: grid

    nl = new_line('a')
    call parse_layout_toml(&
      "[process]" // nl // &
      "columns = [""pid"", ""cpu"", ""command""]" // nl // &
      "[[row]]" // nl // &
      "weight = 2" // nl // &
      "[[row.column]]" // nl // &
      "widget = ""cpu""" // nl // &
      "weight = 1" // nl // &
      "min_width = 28" // nl // &
      "min_height = 8" // nl // &
      "[[row.column]]" // nl // &
      "widget = ""memory""" // nl // &
      "weight = 2" // nl, &
      grid, error)

    call require(.not. error%failed, "layout TOML should parse")
    call require(size(grid%rows) == 1, "layout TOML should produce one row")
    call require(grid%rows(1)%weight == 2, "layout TOML should parse row weight")
    call require(size(grid%rows(1)%columns) == 2, "layout TOML should produce columns")
    call require(grid%process%column_count == 3, "layout TOML should parse process columns")
    call require(trim(grid%process%columns(2)) == "cpu", "layout TOML should preserve process column names")
    call require(grid%rows(1)%columns(1)%widget == LAYOUT_WIDGET_CPU, "first column should be cpu")
    call require(grid%rows(1)%columns(1)%min_size%width == 28, "column min_width should parse")
    call require(grid%rows(1)%columns(1)%min_size%height == 8, "column min_height should parse")
    call require(grid%rows(1)%columns(2)%widget == LAYOUT_WIDGET_MEMORY, "second column should be memory")
    call require(grid%rows(1)%columns(2)%weight == 2, "column weight should parse")
  end subroutine test_parse_layout_toml

  subroutine test_reject_invalid_layout_toml()
    character(len=1) :: nl
    type(layout_error) :: error
    type(layout_grid) :: grid

    nl = new_line('a')
    call parse_layout_toml(&
      "[[row]]" // nl // &
      "weight = 1" // nl // &
      "[[row.column]]" // nl // &
      "widget = 1" // nl, &
      grid, error)

    call require(error%failed, "layout TOML should reject malformed widget")

    call parse_layout_toml(&
      "[[row]]" // nl // &
      "[[row.column]]" // nl // &
      "widget = ""unknown""" // nl, &
      grid, error)
    call require(error%failed, "layout TOML should reject unknown widget")
    call require(index(error%message, "unknown widget") > 0, "layout error should explain unknown widget")

    call parse_layout_toml(&
      "[process]" // nl // &
      "columns = ""pid""" // nl // &
      "[[row]]" // nl // &
      "[[row.column]]" // nl // &
      "widget = ""process""" // nl, &
      grid, error)
    call require(error%failed, "layout TOML should reject scalar process columns")
    call require(index(error%message, "process columns") > 0, &
                 "layout error should explain invalid process columns")
  end subroutine test_reject_invalid_layout_toml

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

    layout = default_dashboard_layout(120, 24)
    call require(layout%cpu_panel%row == 3, "wide dashboard should place cpu in body")
    call require(layout%memory_panel%row == layout%cpu_panel%row, "wide dashboard should be one row")
    call require(layout%memory_panel%col > layout%cpu_panel%col, "wide dashboard should place memory right")
    call require(layout%network_panel%row == layout%cpu_panel%row, "wide dashboard should place network in row")
    call require(layout%network_panel%col > layout%memory_panel%col, "wide dashboard should place network right")

    layout = default_dashboard_layout(80, 24)
    call require(layout%memory_panel%row > layout%cpu_panel%row, "narrow dashboard should stack panels")
    call require(layout%memory_panel%col == layout%cpu_panel%col, "stacked panels should align")
    call require(layout%network_panel%row > layout%memory_panel%row, "narrow dashboard should stack network")
    call require(layout%network_panel%col == layout%cpu_panel%col, "stacked network should align")
  end subroutine test_dashboard_fallback_layout

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

end program test_layout
