# Sprint 07: Process Table

## Goal
Full-featured process table: scan all processes, display in a sortable/filterable table with tree view, send signals, and show per-process resource history sparklines. This is the most complex sprint — the process table is the heart of any system monitor.

## Status: NOT STARTED

## Targets

### Process Data Collection
- [ ] `src/collect/ftop_proc_data.f90`
  - [ ] `type :: process_info`
    - pid, ppid, uid, user, name, command, state
    - cpu_percent, mem_percent, mem_rss, mem_virt
    - threads, nice, priority
    - io_read_bytes, io_write_bytes (Linux: `/proc/[pid]/io`)
    - start_time, cpu_time
    - cgroup (Linux), jid (FreeBSD)
  - [ ] `type :: process_table` — collection of all processes
    - hashtable by PID for O(1) lookup
    - sorted view (current sort field + direction)
    - tree structure (parent-child links)
  - [ ] Per-process ring buffers for CPU% and mem% history (for sparklines)

### Linux Process Scanning
- [ ] Scan `/proc/[pid]/stat` — pid, comm, state, ppid, pgrp, nice, threads, starttime, utime, stime
- [ ] Scan `/proc/[pid]/status` — Name, Uid, VmRSS, VmSize, Threads
- [ ] Scan `/proc/[pid]/cmdline` — full command line (null-separated)
- [ ] Scan `/proc/[pid]/io` — read_bytes, write_bytes (requires root or same user)
- [ ] Scan `/proc/[pid]/cgroup` — cgroup membership
- [ ] UID -> username resolution (parse `/etc/passwd` once, cache)
- [ ] CPU% calculation: delta (utime + stime) / delta total_cpu_time * 100

### FreeBSD Process Scanning
- [ ] `kvm_getprocs(KERN_PROC_ALL)` via C shim
  - [ ] Extract: pid, ppid, uid, comm, state, nice, threads, rss, vsz, runtime
- [ ] CPU% calculation from runtime delta
- [ ] Jail ID for process grouping

### macOS Process Scanning
- [ ] `proc_listallpids()` + `proc_pidinfo()` via C shim
  - [ ] `PROC_PIDTASKALLINFO` struct for comprehensive per-process data
- [ ] `proc_pidpath()` for executable path
- [ ] CPU% from task_info user_time + system_time deltas

### Process Table Widget
- [ ] Integrate with table widget from Sprint 04
- [ ] Default columns: PID, USER, PRI, NI, VIRT, RES, SHR, S, CPU%, MEM%, TIME+, CMD
- [ ] Column visibility configurable in TOML
- [ ] Sort by any column (click header or keybinding)
- [ ] Sort direction toggle (ascending/descending)
- [ ] Current sort indicator in header (▲/▼)

### Tree View
- [ ] Build parent-child tree from ppid relationships
  - [ ] Handle orphan processes (ppid = 1 or ppid doesn't exist)
  - [ ] Handle zombie processes
- [ ] Tree display with indent and branch characters (`├─`, `└─`, `│`)
- [ ] Collapse/expand tree nodes (toggle with key or mouse click)
- [ ] Sort within tree levels (children sorted independently)
- [ ] Toggle between flat and tree view

### Filtering and Search
- [ ] Text filter: type to filter processes by name/command (uses fgof-lineedit)
- [ ] Filter bar appears at bottom when activated (e.g., `/` key)
- [ ] Case-insensitive by default, regex support optional (v1.0: simple substring match)
- [ ] Filter clears on Escape
- [ ] Filtered count display: "Showing 42 of 387 processes"

### Signal Sending
- [ ] Signal panel: press `k` (kill) to open signal selection
  - [ ] List common signals: SIGTERM(15), SIGKILL(9), SIGSTOP(19), SIGCONT(18), SIGHUP(1), SIGUSR1(10), SIGUSR2(12)
  - [ ] Allow typing signal number directly
  - [ ] Confirm before sending (unless SIGTERM)
- [ ] Send via `kill()` syscall (in C shim)
- [ ] Visual feedback: briefly highlight the process row after sending
- [ ] Handle permission errors gracefully (EPERM -> "Permission denied")

### Per-Process History Sparklines (Novel)
- [ ] Small sparkline in the CPU% and MEM% columns showing recent history
- [ ] Configurable: show sparkline vs numeric value (toggle with key)
- [ ] Sparkline uses last N samples from per-process ring buffer
- [ ] Newly spawned processes start with empty sparkline that fills over time

### Mouse Support
- [ ] Click row to select
- [ ] Click header to sort by column
- [ ] Scroll wheel to scroll process list
- [ ] Double-click to expand/collapse tree node

## Definition of Done
- Process table shows all system processes with correct stats
- CPU% and MEM% match `top` output within 2%
- Tree view correctly shows parent-child relationships
- Sort by any column works
- Type `/` + filter text -> only matching processes shown
- Press `k` -> signal panel -> send SIGTERM -> process receives it
- Per-process sparklines show recent CPU/mem history
- Mouse clicks select rows, sort by column, scroll
- Works on Linux, FreeBSD, macOS

## Dependencies
- Sprint 02 (platform shims for process scanning)
- Sprint 03 (collector framework, ring buffers)
- Sprint 04 (table widget, sparkline widget)
- Sprint 06 (layout engine for panel placement)

## Testing
- **Unit**: process info parsing from synthetic `/proc/[pid]/stat` content
- **Unit**: tree building from known pid/ppid pairs
- **Unit**: CPU% delta calculation with known tick values
- **Unit**: sort correctness for each column type
- **Unit**: filter matching (substring, case-insensitive)
- **Snapshot**: process table rendered with synthetic data -> golden comparison
- **Integration**: scan real processes, verify PID 1 exists, current process exists
- **Accuracy**: compare CPU%/MEM% against `top` output (within tolerance)
- **PTY**: launch ftop, navigate process table with arrow keys, sort with keypress, send signal
- **Stress**: handle 10,000+ processes without crash or excessive memory/CPU

## Pitfalls
- **PID recycling**: between scans, a process may die and a new process may reuse its PID. Detect this by comparing start_time — if it changed, it's a new process. Clear the old ring buffer.
- **Zombie processes**: zombies have minimal info in `/proc/[pid]/stat`. Some files may not exist. Handle `ENOENT` and `EACCES` gracefully.
- **Process permissions**: non-root users can't read `/proc/[pid]/io` for other users' processes. Handle `EACCES` and show "N/A" for those fields.
- **Tree cycles**: theoretically impossible but malformed data could create cycles. Guard against infinite loops in tree building (depth limit or visited set).
- **Command line parsing**: `/proc/[pid]/cmdline` uses null bytes as separators. Replace nulls with spaces for display. Empty cmdline means kernel thread — fall back to `[comm]` in brackets.
- **CPU% > 100%**: on multi-core systems, a single process can use > 100% CPU. Per-CPU normalization (divide by core count) is optional; btop shows raw %.
- **Rapid process creation**: some workloads spawn thousands of short-lived processes. Scanning must handle processes that disappear between directory listing and stat reading.
- **Sorting stability**: when two processes have the same sort key, maintain their relative order (stable sort). Fortran's intrinsic sort may not be stable — implement merge sort.
- **Signal sending race**: the selected PID may have exited between selection and kill(). Handle ESRCH (no such process) gracefully.

## Deferred
(none yet)

## Notes
- This is the largest sprint. Consider splitting into sub-sprints:
  - 07a: Data collection + flat table (no tree, no signals, no sparklines)
  - 07b: Tree view + filtering + sorting
  - 07c: Signal sending + sparklines + mouse
- Per-process ring buffers for 10,000 processes × 300 samples × 2 metrics (CPU, mem) = 6M data points. Use compact representation (uint8 for percentage values, 0-200 range).
- htop's process scanning is the most mature reference. Study its approach to handling edge cases, especially around `/proc` reading.
