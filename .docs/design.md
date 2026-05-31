# ftop Design Document

Canonical record of all architectural decisions made during the planning phase.
Updated as decisions change during implementation.

## Technical Stack

| Component | Decision | Rationale |
|-----------|----------|-----------|
| Language | Fortran 2018 | Latest widely-supported standard. ISO_C_BINDING improvements, coarray support (unused but available), enhanced derived types. gfortran 10+ support. |
| Rendering | Direct ANSI escape sequences | No ncurses dependency. Full truecolor (24-bit) support. Braille character graphs. Maximum control over output. btop proves this works at scale. |
| Build system | CMake | Best C/Fortran mixed-language support. Cross-platform. Handles platform-specific conditional compilation. |
| Config format | TOML | Human-friendly, widely adopted. Requires new fgof-toml library (Sprint 05). |
| Concurrency | pthreads via C interop | One main thread (input + draw), one collector thread (metrics). Mutex-protected shared data. Proven pattern (btop model). |
| Compiler | gfortran (primary) | Most portable. ifx and flang-new support deferred to post-v1.0. |

## Platform Targets

All three from day one:

| Platform | Metrics Source | C Shim Contents |
|----------|---------------|-----------------|
| Linux | `/proc`, `/sys`, netlink | termios, ioctl, pthread, sysctl (for some), /proc parsers in Fortran, sysfs readers |
| FreeBSD | sysctl, kvm, devstat | sysctl struct access, kvm_open/kvm_getprocs, devstat |
| macOS | IOKit, mach, sysctl | host_processor_info, vm_stat, IOKit GPU/disk, sysctl |

### C Interop Boundary

**Principle: one small C file per platform.** All logic stays in Fortran. C shim exposes only:
- Struct layouts that ISO_C_BINDING can't reach (platform-specific structs with bitfields, unions)
- ioctl calls with complex argument types
- dlopen/dlsym for runtime library loading (NVML)
- pthread_create, pthread_mutex_init/lock/unlock, pthread_cond_wait/signal
- Platform-specific API calls (kvm_open, host_processor_info, IOServiceMatching)

The Fortran side owns: data parsing, metric computation, history tracking, all rendering, input handling, layout, theming.

## fgof Ecosystem Dependencies

| Library | Purpose in ftop | Maturity |
|---------|----------------|----------|
| fgof-screen | Virtual screen buffer, ANSI frame output, diff-based redraw | v1, polished |
| fgof-termios | Raw mode, terminal size queries, mode guards | working |
| fgof-keys | CSI/SS3 key decoding, modifier parsing, mouse events | working |
| fgof-state | Persistent settings (XDG-compliant), atomic saves | v1, polished |
| fgof-fs | Path helpers, file metadata, directory scanning | polished |
| fgof-process | Process execution, stdout/stderr capture | polished |
| fgof-watch | File polling, change events (for config hot-reload) | polished |
| fgof-expect | PTY automation for integration tests | working |
| fgof-lineedit | Line editing for search/filter input fields | working |
| fgof-cache | Disk cache for metric history persistence | v1, polished |
| **fgof-toml** | **TOML parser (to be built, Sprint 05)** | **new** |

## UI/UX Design

### Layout System
Configurable widget grid defined in TOML config. Users specify which widgets appear and where in a row/column grid. Ships with a default layout and several presets.

Default layout (approximate):
```
+------------------+------------------+
|    CPU Panel     |   Memory Panel   |
| (spark bars +    | (usage bars +    |
|  total graph)    |  history graph)  |
+------------------+------------------+
|   Network Panel  |    Disk Panel    |
| (throughput +    | (usage + I/O +   |
|  connections)    |  temp + latency) |
+------------------+------------------+
|            Process Table            |
| (tree view, signals, per-process    |
|  history sparklines, cgroup groups) |
+-------------------------------------+
|    GPU Panel (if GPUs detected)     |
+-------------------------------------+
```

### CPU Visualization
Per-core spark bars (block characters `▁▂▃▄▅▆▇█`) showing recent history, alongside a larger braille-character total CPU graph showing extended history. Color gradient green -> yellow -> red based on utilization.

### Widget Features
- **Zoom/expand**: any panel expands to full-screen on keypress
- **Mouse support**: click to select, scroll panels
- **Braille graphs**: Unicode braille patterns (U+2800-U+28FF) for high-resolution time-series

### Theming
Loadable `.toml` theme files with 24-bit color support. Color gradients for meters and graphs. Ship with multiple default themes.

## Feature Set (v1.0)

### Core Monitoring
- **CPU**: per-core utilization, frequency, temperature, total usage, history graphs
- **Memory**: used/free/cached/buffers/swap, history graphs
- **Network**: per-interface throughput (sparklines), TCP/UDP connection table, per-process bandwidth
- **Disk**: filesystem usage, per-device I/O rates (sparklines), temperature (S.M.A.R.T./hwmon), I/O latency percentiles
- **GPU**: NVIDIA (NVML via dlopen), AMD (amdgpu sysfs + ROCm SMI), Intel (i915 sysfs). Utilization, temp, VRAM, power, clocks.

### Process Table
- Sortable/filterable process list
- Tree view (parent-child hierarchy)
- Signal sending (kill panel with signal selection)
- Per-process resource history sparklines (novel)
- Process grouping by cgroup/systemd unit/container (novel)

### Novel Features
- **Per-process history**: sparkline graphs per-process showing CPU/memory trends in the process table
- **Smart alerts**: highlight processes with anomalous behavior (CPU spikes, memory leaks, runaway child spawning)
- **Snapshot/diff mode**: freeze state, compare against live, see deltas
- **cgroup/service grouping**: aggregate resource usage by systemd unit or container

## Testing Strategy

Layered approach:

| Layer | Tool | What it tests |
|-------|------|---------------|
| Unit | fgof-testing (or custom) | Individual modules: metric parsing, color math, layout calculations, TOML parsing |
| Snapshot | fgof-screen buffer comparison | Rendering correctness: compare screen buffer contents against golden files |
| Integration | fgof-expect PTY harness | Interaction flows: launch ftop, send keystrokes, verify screen output |
| Stress | Custom harness | Performance under high process counts, rapid metric changes, terminal resize storms |
| Sanity benchmarks | Custom | Ensure metric collection, rendering, and refresh cycles meet latency targets |

Testing is first-class: every sprint includes testing targets. No feature ships without tests.

## Performance Targets

- Refresh cycle: < 16ms draw time (60fps capable, default 1-2 Hz refresh)
- Input latency: < 10ms from keypress to screen update
- Memory: < 50MB RSS under normal operation
- Startup: < 500ms to first frame
- Process table: handle 10,000+ processes without lag

## Directory Structure (Planned)

```
ftop/
  CMakeLists.txt
  src/
    ftop.f90              -- main entry point
    ftop_config.f90       -- config loading (TOML)
    ftop_draw.f90         -- rendering engine
    ftop_input.f90        -- input handling (keyboard + mouse)
    ftop_layout.f90       -- widget grid layout engine
    ftop_theme.f90        -- theme loading and color management
    ftop_widgets.f90      -- widget base types
    widgets/
      ftop_cpu.f90        -- CPU panel widget
      ftop_memory.f90     -- memory panel widget
      ftop_network.f90    -- network panel widget
      ftop_disk.f90       -- disk panel widget
      ftop_gpu.f90        -- GPU panel widget
      ftop_process.f90    -- process table widget
      ftop_graph.f90      -- braille graph widget
      ftop_meter.f90      -- bar meter widget
      ftop_sparkline.f90  -- sparkline widget
    collect/
      ftop_collector.f90  -- threaded collector coordinator
      ftop_cpu_data.f90   -- CPU metric collection
      ftop_mem_data.f90   -- memory metric collection
      ftop_net_data.f90   -- network metric collection
      ftop_disk_data.f90  -- disk metric collection
      ftop_gpu_data.f90   -- GPU metric collection
      ftop_proc_data.f90  -- process table collection
    platform/
      ftop_platform.f90   -- platform abstraction interface
      linux/
        ftop_linux.f90    -- Linux metric backends
        ftop_linux_shim.c -- thin C shim for Linux
      freebsd/
        ftop_freebsd.f90  -- FreeBSD metric backends
        ftop_freebsd_shim.c
      macos/
        ftop_macos.f90    -- macOS metric backends
        ftop_macos_shim.c
    shim/
      ftop_pthread.c      -- pthread wrapper (cross-platform)
      ftop_dl.c           -- dlopen/dlsym wrapper (cross-platform)
  test/
    unit/
    snapshot/
    integration/
    stress/
    bench/
  themes/
    default.toml
    dark.toml
    light.toml
    gruvbox.toml
  config/
    default.toml          -- default layout config
    compact.toml
    process-focused.toml
    network-focused.toml
  .docs/
    design.md             -- this file
    overview.md
    sprints/
    refs/                 -- reference project clones (gitignored)
    audits/
```
