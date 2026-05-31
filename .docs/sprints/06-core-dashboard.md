# Sprint 06: Core Dashboard

## Goal
Wire everything together into a working dashboard: CPU panel with per-core spark bars and total braille graph, memory panel with usage bars and history, configurable grid layout driven by TOML config, and widget zoom/expand. This is the first time ftop looks and feels like a real system monitor.

## Status: NOT STARTED

## Targets

### CPU Panel
- [ ] `src/widgets/ftop_cpu.f90`
  - [ ] Per-core spark bars: one row per core showing recent history as block characters
  - [ ] Color gradient on spark bars (green -> yellow -> red by usage)
  - [ ] Core label (core number)
  - [ ] Total CPU braille graph: multi-row braille time-series of total usage
  - [ ] CPU frequency display (current MHz/GHz per core or average)
  - [ ] CPU temperature display (per-core if available, else package temp)
  - [ ] Load average display (1, 5, 15 min)
  - [ ] Uptime display
  - [ ] Adaptive layout: fewer cores = more graph space; many cores = compact spark bars

### Memory Panel
- [ ] `src/widgets/ftop_memory.f90`
  - [ ] Horizontal bar: used / buffers / cached / free (segmented, different colors)
  - [ ] Numeric labels: "4.2/16.0 GiB (26.3%)"
  - [ ] Swap bar (same style, separate)
  - [ ] Braille history graph showing memory usage over time
  - [ ] Memory breakdown text: used, buffers, cached, available, free

### Layout Engine
- [ ] `src/ftop_layout.f90`
  - [ ] Parse layout from TOML config
  - [ ] Grid model: rows containing columns, each column is a widget with weight
  - [ ] Proportional sizing: widgets get space proportional to their weight
  - [ ] Minimum size enforcement: never shrink a widget below its min_size
  - [ ] Resize handling: recalculate layout on terminal resize
  - [ ] Widget registry: map string names to widget constructors

### Layout Config Format
```toml
[[row]]
weight = 1

[[row.column]]
widget = "cpu"
weight = 1

[[row.column]]
widget = "memory"
weight = 1

[[row]]
weight = 2

[[row.column]]
widget = "process"
weight = 1
```

### Default Layouts
- [ ] `config/default.toml` — full dashboard (CPU, memory, net, disk, process, GPU)
- [ ] `config/compact.toml` — minimal (CPU + memory + process only)
- [ ] `config/process-focused.toml` — small CPU/mem, large process table
- [ ] Fallback hardcoded layout if no config file exists

### Widget Zoom/Expand
- [ ] Keypress (e.g., `Enter` or `z`) expands focused widget to full screen
- [ ] Same key returns to grid view
- [ ] Focused widget gets a visual indicator (highlight border or different border style)
- [ ] Tab / Shift+Tab cycles focus between widgets

### Integration
- [ ] Connect collector snapshot data to CPU and memory panels
- [ ] Refresh cycle: collector thread -> snapshot -> layout render -> flush to screen
- [ ] Smooth redraw: only update changed cells (fgof-screen diff)
- [ ] FPS counter (debug mode) to verify render performance

## Definition of Done
- Launch ftop: CPU and memory panels display real system data
- Per-core spark bars update in real time with color gradients
- Total CPU braille graph shows scrolling history
- Memory bar shows segmented usage
- Resize terminal: layout recalculates, widgets adapt
- Press `z`: focused widget expands to full screen
- Tab cycles focus between panels
- Layout loads from TOML config file
- Default layout ships and works out of the box

## Dependencies
- Sprint 03 (CPU + memory metrics)
- Sprint 04 (widget library)
- Sprint 05 (fgof-toml for config parsing)

## Testing
- **Snapshot**: CPU panel rendered with synthetic data -> golden buffer comparison
- **Snapshot**: memory panel rendered with synthetic data -> golden buffer comparison
- **Snapshot**: grid layout with 2x2 widgets at known terminal size -> golden comparison
- **Unit**: layout weight calculation (given weights and terminal size, verify pixel allocations)
- **Unit**: layout minimum size enforcement (small terminal triggers minimum sizes)
- **Integration**: full render cycle with real collector data -> no crashes, plausible output
- **PTY**: launch ftop in expect harness, verify panels appear, press Tab to cycle focus

## Pitfalls
- **Layout rounding**: proportional sizing produces fractional cell counts. Must distribute remainder cells without gaps or overlaps. Use integer division with remainder distribution (largest-remainder method).
- **Adaptive core display**: a 128-core system needs a very different layout than a 4-core laptop. Detect core count and switch between single-column (few cores) and multi-column (many cores) spark bar arrangements.
- **Braille graph with few samples**: on first launch, the history ring buffer has < 10 samples. The graph should render what it has without stretching or leaving blank space. Right-align the data and let the left side fill in over time.
- **Focus management with zoom**: when zoomed into one widget, key events should only go to that widget. Other widgets should not receive input until zoom is exited.
- **Config file location**: follow XDG spec. `$XDG_CONFIG_HOME/ftop/config.toml`, falling back to `~/.config/ftop/config.toml`. Use fgof-state's XDG support.

## Deferred
(none yet)

## Notes
- This sprint is the "hello world" moment — the first time ftop is actually usable. It will be tempting to over-polish here. Resist: get the data flowing, get the layout working, get zoom functional. Polish comes later.
- The layout engine should be general enough to support arbitrary widget combinations. Don't hardcode CPU-left-memory-right. Let the TOML define it.
- Consider adding a status bar at the bottom (like htop's function key bar) showing key shortcuts. This is a natural place for keybinding hints.
