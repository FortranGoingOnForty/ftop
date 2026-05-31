# Sprint 12: Novel Features

## Goal
Implement the four features that differentiate ftop from btop/htop/bottom: per-process resource history sparklines (partially done in Sprint 07), smart anomaly alerts, snapshot/diff mode, and process grouping by cgroup/service/container.

## Status: NOT STARTED

## Targets

### Smart Alerts / Anomaly Detection
- [ ] `src/ftop_alerts.f90`
  - [ ] `type :: alert` — process_pid, alert_type, severity, message, timestamp, acknowledged
  - [ ] `type :: alert_engine` — holds detection rules and active alerts
  - [ ] Detection rules:
    - [ ] **CPU spike**: process CPU% jumps from < 5% to > 80% within one sample
    - [ ] **Memory leak pattern**: process RSS grows monotonically for N consecutive samples (e.g., 60 samples = 1 minute at 1Hz)
    - [ ] **Runaway children**: process spawns > N child processes within M seconds
    - [ ] **OOM risk**: system available memory < 5% and decreasing
    - [ ] **Disk I/O saturation**: device busy time > 95% for sustained period
    - [ ] **Network flood**: interface throughput > 90% of link speed
    - [ ] **Zombie accumulation**: zombie process count > N (default: 10)
    - [ ] **CPU thermal throttle**: CPU temp approaching critical threshold
  - [ ] Severity levels: info, warning, critical
  - [ ] Configurable thresholds via TOML config
  - [ ] Alert deduplication: same alert type + same PID within N seconds = suppress duplicate
  - [ ] Alert expiry: alerts auto-dismiss after condition clears + cooldown period

### Alert Visualization
- [ ] Process table: anomalous processes get a colored indicator (e.g., left-side marker)
  - [ ] Yellow dot for warning, red dot for critical
  - [ ] Tooltip/popup on hover showing alert details
- [ ] Status bar: alert count badge `⚠ 3 alerts`
- [ ] Alert log overlay: press key (e.g., `a`) to see all active + recent alerts
  - [ ] Columns: Time, Severity, PID, Process, Alert Type, Message
  - [ ] Acknowledge alerts to dismiss (but keep in log)
- [ ] Optional: flash/pulse animation on new critical alerts (if terminal supports it)

### Snapshot / Diff Mode
- [ ] `src/ftop_snapshot.f90`
  - [ ] `type :: system_snapshot` — complete capture of all metrics at a point in time
    - CPU state, memory state, process table, network stats, disk stats, GPU stats
  - [ ] `take_snapshot()` — deep copy current collector state
  - [ ] `diff_snapshot(before, after)` -> structured diff
    - New processes (PID exists in after but not before)
    - Dead processes (PID exists in before but not after)
    - CPU% change per process (delta)
    - Memory change per process (delta RSS)
    - Network throughput changes
    - Disk I/O changes
  - [ ] Activation: press `F5` (or configured key) to enter snapshot mode
    - First press: take snapshot, freeze display, show "SNAPSHOT" indicator
    - Second press: resume live, show diff overlay highlighting changes
    - Third press: exit diff mode, return to normal

### Snapshot/Diff Visualization
- [ ] Frozen state: "PAUSED" indicator in status bar, timestamp of snapshot
- [ ] Diff overlay when resumed:
  - [ ] New processes highlighted in green
  - [ ] Dead processes shown in red (struck through)
  - [ ] Increased CPU%: red up arrow (▲) with delta value
  - [ ] Decreased CPU%: green down arrow (▼) with delta value
  - [ ] Memory increase: red indicator
  - [ ] Memory decrease: green indicator
- [ ] Auto-diff timeout: diff overlay fades after configurable seconds
- [ ] Snapshot persistence: optionally save snapshot to file for later comparison

### Process Grouping by cgroup/service/container
- [ ] `src/ftop_groups.f90`
  - [ ] `type :: process_group` — name, group_type (cgroup/systemd/container), pids[], aggregate_cpu%, aggregate_mem%, aggregate_io
  - [ ] Grouping strategies:
    - [ ] **systemd unit**: read `/proc/[pid]/cgroup`, extract unit name (e.g., `docker.service`, `sshd.service`)
    - [ ] **cgroup v2**: read `/proc/[pid]/cgroup`, extract cgroup path
    - [ ] **container**: detect Docker/Podman container membership via cgroup path pattern (`/docker/<id>`, `/libpod-<id>`)
    - [ ] **user**: group by UID (existing data from process table)
  - [ ] Aggregate metrics: sum CPU%, sum mem%, sum I/O for all processes in group
  - [ ] Expand group to see individual processes

### Process Group Visualization
- [ ] Toggle between flat/tree/group views (key: `g` to cycle)
- [ ] Group view:
  - [ ] One row per group: `docker.service  [8 procs]  CPU: 23.4%  MEM: 1.2 GiB`
  - [ ] Expandable: press Enter/Space to show individual processes within group
  - [ ] Sort groups by aggregate CPU%, MEM%, or process count
  - [ ] Group-level sparkline showing aggregate history
- [ ] Container awareness:
  - [ ] Show container ID (short hash) and image name if available
  - [ ] Parse container info from `/proc/[pid]/cgroup` + Docker/Podman API (optional)

### Linux-Specific
- [ ] cgroup v1 support (legacy): parse `/proc/[pid]/cgroup` multi-line format
- [ ] cgroup v2 support: parse `/proc/[pid]/cgroup` unified hierarchy
- [ ] systemd unit detection: extract from cgroup path or read `/proc/[pid]/attr/exec.cgroup`

### FreeBSD-Specific
- [ ] Jail-based grouping: use jail ID from kvm_getprocs
- [ ] No cgroup equivalent — group by jail or user

### macOS-Specific
- [ ] No cgroup or systemd — group by user or application bundle
- [ ] Optional: launchd service grouping via `launchctl` output

## Definition of Done
- Anomaly alerts fire correctly for CPU spikes, memory leaks, and zombie accumulation
- Alert overlay shows active alerts with severity and details
- Snapshot mode freezes display, diff shows changes since snapshot
- Process grouping shows systemd units / cgroups / containers (Linux)
- Group view shows aggregate metrics and expands to show member processes
- All novel features are configurable (enable/disable, thresholds) via config
- Features degrade gracefully on platforms that don't support them

## Dependencies
- Sprint 07 (process table, per-process history)
- Sprint 03 (collector framework)
- Sprint 06 (layout engine, zoom)
- Sprint 11 (config system for feature settings)

## Testing
- **Unit**: anomaly detection rules with synthetic time-series data
  - Known CPU spike pattern -> alert fires
  - Known memory growth pattern -> leak alert fires
  - Known zombie count -> accumulation alert fires
  - No anomaly pattern -> no false alerts
- **Unit**: snapshot diff calculation with known before/after states
- **Unit**: cgroup path parsing -> correct group name extraction
- **Unit**: aggregate metric calculation for process groups
- **Snapshot**: alert overlay rendered with synthetic alerts -> golden comparison
- **Snapshot**: diff overlay with synthetic changes -> golden comparison
- **Integration**: launch stress test workload, verify CPU spike alert fires
- **PTY**: launch ftop, press F5 (snapshot), spawn a process, press F5 again, verify diff shows new process

## Pitfalls
- **False positive alerts**: overly sensitive thresholds generate noise. Default to conservative thresholds and let users tune via config. Memory leak detection is especially tricky — many programs grow RSS legitimately on startup.
- **Alert storm**: a system under load may trigger dozens of alerts simultaneously. Rate-limit alert generation and group similar alerts.
- **Snapshot memory**: a full system snapshot with 10,000 processes requires significant memory. Consider snapshotting only summary data + top N processes by resource usage.
- **cgroup path parsing**: cgroup v1 and v2 have different formats. v1 has multiple controllers per PID, v2 has a single unified path. Handle both.
- **Container detection**: not all cgroup paths follow standard Docker/Podman patterns. Custom container runtimes (containerd, CRI-O) use different path schemes. Provide configurable pattern matching.
- **Grouping performance**: aggregating metrics across all processes every second is O(n). Keep the group membership cache and update incrementally (processes rarely change groups).
- **Platform coverage**: cgroup/systemd are Linux-only. FreeBSD has jails. macOS has nothing equivalent. The group view must gracefully show "user groups only" on unsupported platforms.

## Deferred
- Docker/Podman API integration for container name/image resolution (nice-to-have)
- Snapshot persistence to file (nice-to-have)
- Custom user-defined alert rules (post-v1.0)
- Alert notification integration (desktop notifications, sound — post-v1.0)

## Notes
- These features are what make ftop novel. They should be prominent in the UI but not overwhelming — users who don't need them should be able to disable them cleanly.
- The anomaly detection should be lightweight. Don't run complex statistical models — simple heuristics (threshold crossing, monotonic increase, rate-of-change) are sufficient and won't impact performance.
- Consider a "quiet mode" config option that suppresses all alerts for users who prefer a clean display.
