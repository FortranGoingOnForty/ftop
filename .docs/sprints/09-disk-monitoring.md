# Sprint 09: Disk Monitoring

## Goal
Disk panel with filesystem usage bars, per-device I/O rates with sparkline history, disk temperature via S.M.A.R.T./hwmon, and I/O latency percentiles. Comprehensive disk health and performance visibility.

## Status: NOT STARTED

## Targets

### Disk Data Collection
- [ ] `src/collect/ftop_disk_data.f90`
  - [ ] `type :: filesystem_info` — device, mountpoint, fstype, total, used, available, use_percent
  - [ ] `type :: disk_io_info` — device, read_bytes, write_bytes, read_ops, write_ops, io_time, weighted_io_time
  - [ ] `type :: disk_temp_info` — device, temp_celsius, temp_status (ok/warning/critical)
  - [ ] `type :: disk_latency_info` — device, avg_read_latency_us, avg_write_latency_us, p99_latency_us
  - [ ] Ring buffers for I/O rate and latency history

### Linux Disk Collection
- [ ] Filesystem usage: parse `/proc/mounts` + `statvfs()` per mount
  - [ ] Filter: skip pseudo-filesystems (proc, sysfs, tmpfs, devtmpfs, cgroup, etc.)
  - [ ] Configurable: show/hide tmpfs, network mounts
- [ ] I/O stats: parse `/proc/diskstats`
  - [ ] Fields: reads_completed, reads_merged, sectors_read, ms_reading, writes_completed, writes_merged, sectors_written, ms_writing, ios_in_progress, ms_io, weighted_ms_io
  - [ ] Sector size: typically 512 bytes (verify via `/sys/block/<dev>/queue/hw_sector_size`)
  - [ ] Delta calculation for rates (bytes/sec, ops/sec)
  - [ ] Filter: skip partition entries if whole-disk entry exists (avoid double counting)
- [ ] Temperature: `/sys/class/hwmon/hwmon*/temp*_input`
  - [ ] Match to disk by: `hwmon*/name` = `drivetemp`, or by device path correlation
  - [ ] Alternatively: `/sys/class/block/<dev>/device/hwmon/hwmon*/temp1_input` (direct path)
  - [ ] Fallback: call `smartctl -A <dev>` via fgof-process (requires smartmontools, root)
- [ ] Latency: derived from `/proc/diskstats`
  - [ ] avg_read_latency = ms_reading_delta / reads_completed_delta
  - [ ] avg_write_latency = ms_writing_delta / writes_completed_delta
  - [ ] p99 estimation: track histogram of per-sample average latencies over time

### FreeBSD Disk Collection
- [ ] Filesystem usage: `getmntinfo()` via C shim or `statvfs()`
- [ ] I/O stats: `devstat_getdevs()` via C shim
  - [ ] Read/write bytes, operations, busy time per device
- [ ] Temperature: `sysctl kern.cam.da*.temperature` or atacam(8) interface
- [ ] Alternatively: call `smartctl` via fgof-process

### macOS Disk Collection
- [ ] Filesystem usage: `getmntinfo()` or `statvfs()`
- [ ] I/O stats: `IOServiceGetMatchingServices()` + `IORegistryEntryCreateCFProperties()` for IOBlockStorageDriver
  - [ ] "Statistics" dictionary contains: BytesRead, BytesWritten, Operations
- [ ] Temperature: IOKit SMARTLib (complex) or `smartctl` fallback

### Disk Panel Widget
- [ ] `src/widgets/ftop_disk.f90`
  - [ ] Filesystem usage section:
    - [ ] One row per mounted filesystem: `device  mountpoint  [████░░░░] 63% 120G/190G`
    - [ ] Color gradient on bar: green < 70%, yellow 70-90%, red > 90%
    - [ ] Sort by usage% or mountpoint
  - [ ] I/O throughput section:
    - [ ] Per-device read/write rates: `sda  R: 45.2 MB/s  W: 12.1 MB/s`
    - [ ] Sparkline or braille graph of I/O history
    - [ ] Auto-scale units (KB/s, MB/s, GB/s)
  - [ ] Temperature display:
    - [ ] Per-device temperature with color coding (green < 40C, yellow 40-55C, red > 55C)
  - [ ] Latency display (expanded view):
    - [ ] Average read/write latency per device
    - [ ] Visual indicator for latency health (green < 1ms, yellow 1-10ms, red > 10ms)

### Human-Readable Formatting
- [ ] Size formatting: auto-select KiB, MiB, GiB, TiB (binary) or KB, MB, GB, TB (decimal, configurable)
- [ ] Rate formatting: auto-select B/s through GB/s
- [ ] Latency formatting: us, ms, s as appropriate

## Definition of Done
- Disk panel shows all mounted real filesystems with usage bars
- I/O rates match `iostat` output within 5%
- Temperature reads match `smartctl` output (where available)
- Latency values are plausible and consistent with I/O load
- Panel adapts to expanded/compact view
- Works on Linux, FreeBSD, macOS

## Dependencies
- Sprint 03 (collector framework, ring buffers)
- Sprint 04 (bar meter, sparkline, table widgets)
- Sprint 06 (layout engine)

## Testing
- **Unit**: `/proc/diskstats` parser with synthetic data
- **Unit**: filesystem filtering (exclude pseudo-filesystems)
- **Unit**: I/O rate delta calculation (known before/after -> expected rate)
- **Unit**: size/rate formatting (known bytes -> expected human-readable string)
- **Unit**: latency calculation (known ms_reading and reads_completed -> expected average)
- **Integration**: filesystem usage matches `df -h` output
- **Accuracy**: I/O rates during `dd` match `iostat` within tolerance
- **Snapshot**: disk panel rendered with synthetic data -> golden comparison

## Pitfalls
- **Disk naming**: Linux uses `/dev/sda`, `/dev/nvme0n1`, etc. FreeBSD uses `/dev/da0`, `/dev/ada0`. macOS uses `/dev/disk0`. The C shim may need to translate between diskstats names and device paths.
- **Partition vs whole disk**: `/proc/diskstats` lists both `sda` and `sda1`, `sda2`, etc. Don't double count. Prefer whole-disk stats and skip partitions.
- **ZFS**: ZFS pools appear as mount points but don't have traditional block device stats. Handle ZFS specially (pool I/O stats via `/proc/spl/kstat/zfs/<pool>/io` on Linux, `sysctl kstat.zfs` on FreeBSD).
- **NFS/CIFS**: network mounts may be slow to stat. Use a timeout or skip them by default.
- **Temperature availability**: drivetemp kernel module may not be loaded. Not all drives support S.M.A.R.T. Handle missing temperature data gracefully (show "N/A").
- **I/O latency spikes**: a single slow I/O can skew average latency enormously. The p99 estimation helps, but it's an approximation from per-sample averages, not per-IO percentiles.
- **Disk hotplug**: USB drives and NVMe drives can appear/disappear. Rescan device list periodically, don't assume static device set.

## Deferred
(none yet)

## Notes
- ZFS support is a differentiator — btop shows basic ZFS stats but not pool-level I/O. Consider dedicated ZFS awareness.
- For temperature, the hwmon/drivetemp path is much lighter weight than calling smartctl. Prefer it when available.
- Consider adding IOPS display alongside throughput — some workloads are IOPS-bound, not bandwidth-bound.
