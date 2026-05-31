# Sprint 03: System Metrics Collection

## Goal
Full CPU and memory metric collection on all three platforms, running in a background thread, with ring-buffer history tracking. The collector populates shared data structures that the draw loop can read. No rendering yet — this sprint proves data accuracy.

## Status: NOT STARTED

## Targets

### Data Structures
- [ ] `src/collect/ftop_ring_buffer.f90` — generic ring buffer for metric history
  - [ ] Fixed-size circular buffer with O(1) push/read
  - [ ] Configurable depth (default: 300 samples = 5 min at 1Hz)
  - [ ] Thread-safe read/write (snapshot for reader, append for writer)
- [ ] `src/collect/ftop_cpu_data.f90` — CPU data types
  - [ ] `type :: cpu_core_info` — per-core: usage%, user%, system%, iowait%, freq_mhz, temp_c
  - [ ] `type :: cpu_total_info` — total: usage%, load_avg(3), core_count, thread_count
  - [ ] Ring buffers for usage history (per-core and total)
- [ ] `src/collect/ftop_mem_data.f90` — memory data types
  - [ ] `type :: memory_info` — total, used, free, available, cached, buffers, swap_total, swap_used
  - [ ] Ring buffers for usage history
- [ ] `src/collect/ftop_collector.f90` — collector coordinator
  - [ ] `type :: collector` — owns all data, mutex, thread handle
  - [ ] `start()` — spawn collector thread
  - [ ] `stop()` — signal thread to exit, join
  - [ ] `snapshot()` — lock mutex, copy current data, unlock (for draw thread)
  - [ ] Configurable collection interval

### Linux Collection
- [ ] CPU usage: parse `/proc/stat` (user, nice, system, idle, iowait, irq, softirq, steal)
  - [ ] Calculate per-core delta percentages between samples
  - [ ] Calculate total CPU usage
- [ ] CPU frequency: read `/sys/devices/system/cpu/cpu*/cpufreq/scaling_cur_freq`
- [ ] CPU temperature: read `/sys/class/hwmon/hwmon*/temp*_input` (match to correct sensor)
- [ ] CPU topology: `/proc/cpuinfo` for core count, thread count, model name
- [ ] Memory: parse `/proc/meminfo` (MemTotal, MemFree, MemAvailable, Buffers, Cached, SwapTotal, SwapFree)
- [ ] Load average: parse `/proc/loadavg`

### FreeBSD Collection
- [ ] CPU usage: `kern.cp_times` sysctl (per-core ticks array)
  - [ ] Delta calculation same as Linux
- [ ] CPU frequency: `dev.cpu.N.freq` sysctl
- [ ] CPU temperature: `dev.cpu.N.temperature` sysctl (or `hw.acpi.thermal`)
- [ ] Memory: `vm.stats.vm.v_*` sysctls (page counts × page size)
- [ ] Swap: `vm.swap_info` sysctl or `swapctl()`
- [ ] Load average: `vm.loadavg` sysctl

### macOS Collection
- [ ] CPU usage: `host_processor_info()` -> `CPU_STATE_USER/SYSTEM/IDLE/NICE` per-core
  - [ ] Delta calculation from tick counters
- [ ] CPU frequency: not directly available via public API (sysctl `hw.cpufrequency` for nominal)
- [ ] CPU temperature: SMC via IOKit (complex, may defer to later sprint)
- [ ] Memory: `host_statistics64()` -> `vm_statistics64_data_t`
  - [ ] `free_count`, `active_count`, `inactive_count`, `wire_count`, `speculative_count`
  - [ ] Page size via `vm_kernel_page_size`
- [ ] Swap: `sysctl vm.swapusage`
- [ ] Load average: `getloadavg()` (POSIX)

### Accuracy Validation
- [ ] Compare ftop CPU readings against `top -bn1` output (Linux), `top -P` (FreeBSD), `top -l1` (macOS)
- [ ] Compare memory readings against `free -m` (Linux), `top` (FreeBSD/macOS)
- [ ] Verify readings match within 2% tolerance

## Definition of Done
- Collector thread runs, gathers CPU + memory data at configurable interval
- `collector%snapshot()` returns current + historical data safely from draw thread
- CPU per-core usage matches system tools within 2%
- Memory readings match system tools within 1%
- Ring buffers store 300 samples without corruption
- Works on Linux, FreeBSD, macOS
- Clean shutdown: collector thread exits within 1 second of stop signal

## Dependencies
- Sprint 02 (platform shims, threading)

## Testing
- **Unit**: ring buffer push/read/wrap-around, capacity limits
- **Unit**: `/proc/stat` parser with synthetic input files
- **Unit**: `/proc/meminfo` parser with synthetic input files
- **Unit**: CPU delta percentage calculation (known tick values -> expected %)
- **Integration**: collector runs for 5 seconds, data is non-zero and plausible
- **Accuracy**: automated comparison against system tools (within tolerance)
- **Stress**: rapid start/stop of collector thread (no leaks, no deadlocks)
- **Platform**: same test suite on Linux, FreeBSD, macOS

## Pitfalls
- **CPU tick overflow**: `/proc/stat` values are unsigned 64-bit counters. Delta calculation must handle rollover (unlikely but possible on long-running systems). Use unsigned arithmetic or detect rollover and skip the sample.
- **First sample is garbage**: CPU usage requires TWO samples to compute a delta. The first collection cycle produces no valid usage data. Initialize to zero and mark as "warming up."
- **hwmon sensor mapping**: On Linux, hwmon device numbering is not stable across boots. Must match by `name` sysfs attribute (e.g., `coretemp`, `k10temp`) not by hwmon index.
- **macOS memory accounting**: macOS doesn't have a simple "used = total - free" model. "Used" = wired + active + (some of) inactive. Match Activity Monitor's definition.
- **Thread safety of /proc reads**: Reading /proc files is not atomic. A file can change between read() calls. Read the entire file in one read() call into a buffer, then parse the buffer.
- **FreeBSD kern.cp_times**: returns an array of ALL core ticks concatenated. Must slice correctly based on CPUSTATES (5 states per core).
- **Collection interval drift**: don't use `sleep(interval)`. Use absolute wall-clock targets to avoid drift. `clock_gettime(CLOCK_MONOTONIC)` via C shim.

## Deferred
(none yet)

## Notes
- The ring buffer should be a standalone, reusable module. It will be used by every metric type (CPU, memory, network, disk, GPU, per-process).
- Consider pre-opening `/proc/stat` and `/proc/meminfo` file descriptors and seeking to beginning on each read, rather than open/read/close per cycle. This avoids file descriptor churn.
- The snapshot() mechanism should do a deep copy of all relevant data. The draw thread must never hold a reference to collector-owned memory.
