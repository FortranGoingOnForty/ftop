# Sprint 08: Network Monitoring

## Goal
Network panel with per-interface throughput sparklines, TCP/UDP connection table, and per-process bandwidth correlation. The most ambitious network monitoring in a TUI system monitor.

## Status: NOT STARTED

## Targets

### Network Data Collection
- [ ] `src/collect/ftop_net_data.f90`
  - [ ] `type :: interface_info` — name, rx_bytes, tx_bytes, rx_packets, tx_packets, state (up/down), speed, mtu
  - [ ] `type :: net_connection` — protocol (tcp/udp), local_addr, local_port, remote_addr, remote_port, state (ESTABLISHED, LISTEN, etc.), pid, process_name
  - [ ] `type :: process_bandwidth` — pid, process_name, rx_bytes_delta, tx_bytes_delta
  - [ ] Ring buffers for per-interface throughput history
  - [ ] Delta calculation: bytes/sec from consecutive samples

### Linux Network Collection
- [ ] Interface stats: parse `/proc/net/dev`
  - [ ] Columns: bytes, packets, errs, drop, fifo, frame, compressed, multicast (rx + tx)
  - [ ] Delta calculation between samples
  - [ ] Filter out loopback (`lo`) by default (configurable)
- [ ] Interface speed: read `/sys/class/net/<iface>/speed` (Mbps)
- [ ] Connection table: parse `/proc/net/tcp` and `/proc/net/udp`
  - [ ] Decode hex addresses and port numbers
  - [ ] Map inode -> PID via `/proc/[pid]/fd/` -> socket inode lookup
  - [ ] TCP states: ESTABLISHED, SYN_SENT, SYN_RECV, FIN_WAIT1, FIN_WAIT2, TIME_WAIT, CLOSE, CLOSE_WAIT, LAST_ACK, LISTEN, CLOSING
- [ ] IPv6: parse `/proc/net/tcp6` and `/proc/net/udp6`
- [ ] Per-process bandwidth: correlate connection table with interface deltas
  - [ ] Via netlink INET_DIAG (most accurate, requires C shim for netlink socket handling)
  - [ ] Fallback: delta of `/proc/[pid]/net/dev` per-process (less accurate)

### FreeBSD Network Collection
- [ ] Interface stats: `sysctl net.link.generic.ifdata`
- [ ] Connection table: `sysctl net.inet.tcp.pcblist` / `net.inet.udp.pcblist`
- [ ] Alternatively: parse `netstat -an` output via fgof-process (simpler but slower)

### macOS Network Collection
- [ ] Interface stats: `sysctl net.link.generic.system.ifcount` + per-interface sysctls
- [ ] Alternatively: `getifaddrs()` via C shim for interface addresses
- [ ] Connection table: `proc_pidinfo(PROC_PIDLISTFDS)` + `proc_pidfdinfo()` for socket info
- [ ] Alternatively: parse `netstat -an` output

### Network Panel Widget
- [ ] `src/widgets/ftop_network.f90`
  - [ ] Per-interface section:
    - [ ] Interface name + state indicator (▲ up / ▼ down)
    - [ ] RX/TX throughput: current rate (e.g., "↓ 12.3 MB/s  ↑ 1.2 MB/s")
    - [ ] Sparkline or braille graph showing throughput history
    - [ ] Auto-scale Y axis (KB/s, MB/s, GB/s as appropriate)
  - [ ] Connection table (optional, shown when panel is expanded/focused):
    - [ ] Columns: Proto, Local Address, Remote Address, State, PID, Process
    - [ ] Sortable by any column
    - [ ] Filter by state (e.g., show only ESTABLISHED)
  - [ ] Per-process bandwidth (optional, in expanded view):
    - [ ] Top N processes by bandwidth
    - [ ] RX and TX rates per process

### Human-Readable Formatting
- [ ] Byte rate formatting: auto-select B/s, KB/s, MB/s, GB/s
- [ ] IP address formatting: IPv4 dotted decimal, IPv6 compressed notation
- [ ] Port -> service name lookup (parse `/etc/services` once, cache)

## Definition of Done
- Network panel shows per-interface throughput with live-updating sparklines
- Throughput values match `iftop` or `nload` within 5%
- Connection table shows active TCP/UDP connections with correct states
- Per-process bandwidth shows which processes are using network
- Works on Linux, FreeBSD, macOS (connection table may be Linux-only initially)
- Expanded view shows full connection + per-process detail

## Dependencies
- Sprint 03 (collector framework, ring buffers)
- Sprint 04 (sparkline, graph, table widgets)
- Sprint 06 (layout engine for panel placement)

## Testing
- **Unit**: `/proc/net/dev` parser with synthetic content
- **Unit**: `/proc/net/tcp` hex address decoder (known hex -> expected IP:port)
- **Unit**: throughput delta calculation (known byte counts -> expected rates)
- **Unit**: human-readable formatting (known bytes -> expected string)
- **Integration**: interface stats match `ip -s link` output
- **Accuracy**: throughput measurement during `dd if=/dev/zero | nc ...` transfer -> verify rate
- **Snapshot**: network panel rendered with synthetic data -> golden comparison

## Pitfalls
- **Interface enumeration order**: `/proc/net/dev` lists interfaces in arbitrary order. Sort alphabetically or by type (physical first, virtual last) for consistent display.
- **Virtual interfaces**: Docker, VMs, VPNs create many virtual interfaces. Allow hiding interfaces by pattern (e.g., `veth*`, `docker*`).
- **Per-process bandwidth accuracy**: Linux doesn't natively expose per-process bandwidth. The inode -> PID mapping via `/proc/[pid]/fd/` is expensive (requires scanning all PIDs' fd directories). Do this scan less frequently (every 5 seconds) and cache the mapping.
- **Connection table size**: a busy server may have 10,000+ connections. Must handle efficiently — don't re-parse the full table if only showing top N.
- **IPv6 addresses**: extremely long when fully expanded. Use compressed notation and ellipsis in narrow columns.
- **Netlink complexity**: INET_DIAG via netlink is the proper way to get per-socket statistics but requires a non-trivial C shim for netlink socket setup, message construction, and response parsing. Consider starting with the `/proc` approach and adding netlink as an optimization.

## Deferred
(none yet)

## Notes
- Per-process bandwidth is the most complex part. If it proves too fragile, the connection table alone (with PID column) provides most of the value.
- Consider adding a "top talkers" summary: top 5 processes by bandwidth, always visible even when the panel isn't focused.
- btop doesn't show individual connections or per-process bandwidth. This is a differentiator for ftop.
