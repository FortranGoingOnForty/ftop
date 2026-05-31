# ftop Sprint Plan

14 sprints from empty repo to shipped product. These are living documents — update as targets are hit or deferred.

## Sprint Overview

| Sprint | Name | Focus | Key Dependencies |
|--------|------|-------|-----------------|
| [00](00-foundation.md) | Foundation | Repo structure, CMake, CI, fgof deps | none |
| [01](01-terminal-core.md) | Terminal Core | Raw mode, ANSI output, screen buffer, input, main loop | 00 |
| [02](02-platform-shims.md) | Platform Shims | C shims (Linux/FreeBSD/macOS), pthreads | 00 |
| [03](03-metrics-collection.md) | Metrics Collection | CPU + memory data, threaded collector, ring buffers | 02 |
| [04](04-widget-library.md) | Widget Library | Boxes, bars, sparklines, braille graphs, gradients | 00, 01 |
| [05](05-fgof-toml.md) | fgof-toml | TOML v1.0 parser library (new fgof lib) | 00 |
| [06](06-core-dashboard.md) | Core Dashboard | CPU + memory panels, grid layout, zoom/expand | 03, 04, 05 |
| [07](07-process-table.md) | Process Table | Process scanning, tree view, signals, search, history | 02, 03, 04, 06 |
| [08](08-network-monitoring.md) | Network | Interface throughput, connections, per-process bandwidth | 03, 04, 06 |
| [09](09-disk-monitoring.md) | Disk | Usage, I/O, temperature, latency | 03, 04, 06 |
| [10](10-gpu-monitoring.md) | GPU | NVIDIA + AMD + Intel monitoring | 02, 03, 04, 06 |
| [11](11-theming-config.md) | Theming + Config | Theme files, truecolor, keybindings, CLI args | 05, 06 |
| [12](12-novel-features.md) | Novel Features | Anomaly alerts, snapshot/diff, cgroup grouping | 07, 03, 06, 11 |
| [13](13-polish-release.md) | Polish + Release | Performance, CI, packaging, docs | all |

## Dependency Graph (Parallelizable Sprints)

```
00 ─┬─ 01 ──────────────┐
    ├─ 02 ── 03 ─────────┼── 06 ── 07 ── 12
    ├─ 04 ───────────────┘        │
    └─ 05 ─── 11 ─────────────────┘
                                  ├── 08
                                  ├── 09
                                  └── 10
                                       │
                                  13 ──┘
```

Sprints 01, 02, 04, 05 can run in parallel after 00 completes.
Sprints 08, 09, 10 can run in parallel after 06 completes.

## Conventions

- Each sprint file uses checkbox syntax (`- [ ]` / `- [x]`) for tracking
- Status field at top: `NOT STARTED` | `IN PROGRESS` | `BLOCKED` | `DONE`
- Deferred items get moved to a target sprint's file with a backlink
- Pitfalls section captures known risks; update as new ones are discovered
- Testing targets are per-sprint — tests ship with the feature, not after
