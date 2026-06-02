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
    parse_layout_file, &
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
  call test_preset_config_files()

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
      "[network]" // nl // &
      "include_loopback = true" // nl // &
      "exclude_interfaces = [""docker*"", ""veth*""]" // nl // &
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
    call require(grid%network%include_loopback, "layout TOML should parse network loopback filter")
    call require(grid%network%exclude_count == 2, "layout TOML should parse network exclude filters")
    call require(trim(grid%network%exclude_patterns(1)) == "docker*", &
                 "layout TOML should preserve network exclude patterns")
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

    call parse_layout_toml(&
      "[network]" // nl // &
      "exclude_interfaces = [1]" // nl // &
      "[[row]]" // nl // &
      "[[row.column]]" // nl // &
      "widget = ""network""" // nl, &
      grid, error)
    call require(error%failed, "layout TOML should reject invalid network filters")
    call require(index(error%message, "exclude_interfaces") > 0, &
                 "layout error should explain invalid network filters")
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
    call require(layout%memory_panel%row == layout%cpu_panel%row, "wide dashboard metrics should share a row")
    call require(layout%memory_panel%col > layout%cpu_panel%col, "wide dashboard should place memory right")
    call require(layout%network_panel%row == layout%cpu_panel%row, "wide dashboard should place network in row")
    call require(layout%network_panel%col > layout%memory_panel%col, "wide dashboard should place network right")
    call require(layout%process_panel%row > layout%cpu_panel%row, "wide dashboard should place process below metrics")
    call require(layout%process_panel%height >= 8, "wide dashboard should give process table usable height")

    layout = default_dashboard_layout(80, 24)
    call require(layout%memory_panel%row > layout%cpu_panel%row, "narrow dashboard should stack panels")
    call require(layout%memory_panel%col == layout%cpu_panel%col, "stacked panels should align")
    call require(layout%network_panel%row > layout%memory_panel%row, "narrow dashboard should stack network")
    call require(layout%network_panel%col == layout%cpu_panel%col, "stacked network should align")
    call require(layout%process_panel%row > layout%network_panel%row, "narrow dashboard should stack process")
    call require(layout%process_panel%col == layout%cpu_panel%col, "stacked process should align")
    call require(layout%process_panel%row + layout%process_panel%height <= layout%footer%row, &
                 "stacked process should stay above footer")
  end subroutine test_dashboard_fallback_layout

  subroutine test_preset_config_files()
    character(len=512) :: config_root
    type(layout_error) :: error
    type(layout_grid) :: grid

    if (command_argument_count() < 1) return
    call get_command_argument(1, config_root)

    call parse_layout_file(trim(config_root) // "/default.toml", grid, error)
    call require(.not. error%failed, "default preset should parse")
    call require(layout_has_widget(grid, LAYOUT_WIDGET_CPU), "default preset should include cpu")
    call require(layout_has_widget(grid, LAYOUT_WIDGET_MEMORY), "default preset should include memory")
    call require(layout_has_widget(grid, LAYOUT_WIDGET_NETWORK), "default preset should include network")
    call require(layout_has_widget(grid, LAYOUT_WIDGET_PROCESS), "default preset should include process")

    call parse_layout_file(trim(config_root) // "/compact.toml", grid, error)
    call require(.not. error%failed, "compact preset should parse")
    call require(layout_has_widget(grid, LAYOUT_WIDGET_CPU), "compact preset should include cpu")
    call require(layout_has_widget(grid, LAYOUT_WIDGET_MEMORY), "compact preset should include memory")
    call require(.not. layout_has_widget(grid, LAYOUT_WIDGET_PROCESS), "compact preset should omit process")

    call parse_layout_file(trim(config_root) // "/process-focused.toml", grid, error)
    call require(.not. error%failed, "process preset should parse")
    call require(layout_has_widget(grid, LAYOUT_WIDGET_PROCESS), "process preset should include process")

    call parse_layout_file(trim(config_root) // "/network-focused.toml", grid, error)
    call require(.not. error%failed, "network preset should parse")
    call require(layout_has_widget(grid, LAYOUT_WIDGET_NETWORK), "network preset should include network")
    call require(.not. layout_has_widget(grid, LAYOUT_WIDGET_PROCESS), "network preset should focus network")
  end subroutine test_preset_config_files

  logical function layout_has_widget(grid, widget) result(found)
    type(layout_grid), intent(in) :: grid
    character(len=*), intent(in) :: widget
    integer :: column_index
    integer :: row_index

    found = .false.
    if (.not. allocated(grid%rows)) return
    do row_index = 1, size(grid%rows)
      if (.not. allocated(grid%rows(row_index)%columns)) cycle
      do column_index = 1, size(grid%rows(row_index)%columns)
        if (grid%rows(row_index)%columns(column_index)%widget == trim(widget)) then
          found = .true.
          return
        end if
      end do
    end do
  end function layout_has_widget

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

end program test_layout
