# Sprint 02: Platform Shims + Threading

## Goal
Build the thin C shim layer for all three platforms and the pthreads abstraction. A Fortran program that spawns a collector thread, reads basic CPU info from the OS, and prints it — proving the full stack works: Fortran -> C shim -> OS API -> data back to Fortran.

## Status: NOT STARTED

## Targets

### Cross-Platform C Shims
- [ ] `src/shim/ftop_pthread.c` — pthreads wrapper
  - [ ] `ftop_thread_create(fn_ptr, arg_ptr)` -> thread handle
  - [ ] `ftop_thread_join(handle)`
  - [ ] `ftop_mutex_init/lock/unlock/destroy`
  - [ ] `ftop_cond_init/wait/signal/broadcast/destroy`
  - [ ] Corresponding Fortran `interface` block with ISO_C_BINDING
- [ ] `src/shim/ftop_dl.c` — dynamic library loader
  - [ ] `ftop_dlopen(path)` -> handle
  - [ ] `ftop_dlsym(handle, symbol)` -> fn_ptr
  - [ ] `ftop_dlclose(handle)`
  - [ ] Corresponding Fortran interfaces
- [ ] `src/shim/ftop_signal.c` — signal handling
  - [ ] `ftop_signal_setup()` — install SIGWINCH, SIGINT, SIGTERM, SIGTSTP, SIGCONT handlers
  - [ ] `ftop_signal_check(sig_id)` -> bool (poll flags set by handlers)
  - [ ] `ftop_signal_clear(sig_id)` — reset flag after handling
  - [ ] `ftop_kill(pid, signal)` — send signal to process

### Linux Shim
- [ ] `src/platform/linux/ftop_linux_shim.c`
  - [ ] CPU topology: parse `/proc/cpuinfo` field helpers (or pure Fortran for this)
  - [ ] ioctl wrappers for terminal (if not covered by fgof-termios)
  - [ ] hwmon sensor path discovery
  - [ ] netlink socket helpers (for per-process bandwidth, optional)

### FreeBSD Shim
- [ ] `src/platform/freebsd/ftop_freebsd_shim.c`
  - [ ] `sysctl()` wrapper with typed output (int, long, string, struct)
  - [ ] `kvm_open()` / `kvm_getprocs()` / `kvm_close()` wrappers
  - [ ] `devstat_getdevs()` for disk I/O
  - [ ] CPU topology via `kern.smp.cpus` sysctl

### macOS Shim
- [ ] `src/platform/macos/ftop_macos_shim.c`
  - [ ] `host_processor_info()` for CPU per-core ticks
  - [ ] `vm_stat` / `host_statistics64()` for memory
  - [ ] `sysctl()` wrapper (shared pattern with FreeBSD)
  - [ ] IOKit helpers (for GPU, disk — stubbed now, filled in later sprints)

### Platform Abstraction (Fortran)
- [ ] `src/platform/ftop_platform.f90` — abstract interface module
  - [ ] `type, abstract :: platform_backend`
  - [ ] Deferred procedures: `get_cpu_count`, `get_cpu_usage`, `get_memory_info`, etc.
  - [ ] Factory function: `create_platform()` returns the correct backend for the OS
- [ ] Concrete implementations:
  - [ ] `src/platform/linux/ftop_linux.f90`
  - [ ] `src/platform/freebsd/ftop_freebsd.f90`
  - [ ] `src/platform/macos/ftop_macos.f90`

### CMake Plumbing
- [ ] Conditional compilation: only compile the current platform's shim + backend
- [ ] Platform detection: `CMAKE_SYSTEM_NAME` -> Linux/FreeBSD/Darwin
- [ ] Link flags: `-lpthread`, `-lkvm` (FreeBSD), framework links (macOS)

### Proof of Life
- [ ] Demo: spawn a thread, read CPU core count + current usage, print from main thread
- [ ] Verify on all three platforms (or at least Linux + one other)

## Definition of Done
- `ftop_pthread` module: can create threads, lock/unlock mutexes from Fortran
- `ftop_platform` module: returns correct CPU count and basic usage on the build platform
- Threaded demo: collector thread writes data, main thread reads it through mutex
- All C shims compile on Linux, FreeBSD, macOS
- CI builds pass on all platforms

## Dependencies
- Sprint 00 (build system)
- Sprint 01 (signal handling concepts, but Sprint 02 can start in parallel with 01)

## Testing
- **Unit**: pthread create/join, mutex lock/unlock correctness
- **Unit**: platform factory returns correct backend type
- **Unit**: sysctl wrapper returns sensible values (CPU count > 0, memory > 0)
- **Integration**: threaded read of CPU usage matches single-threaded read
- **Platform**: same test suite runs on Linux, FreeBSD, macOS

## Pitfalls
- **ISO_C_BINDING limitations**: Fortran can't directly represent C unions, bitfields, or anonymous structs. The C shim must flatten these into simple types (int, double, arrays).
- **Thread safety in Fortran**: Fortran I/O is not thread-safe by default. The collector thread must NOT do Fortran I/O — communicate via shared memory (C-interop arrays/structs) protected by mutex.
- **kvm on FreeBSD**: `kvm_getprocs()` returns a pointer to internal static storage. Must copy data before the next call. The C shim should copy into a Fortran-allocated buffer.
- **macOS mach API deprecation**: some `host_statistics` calls are deprecated in newer macOS. Check current API status. `host_statistics64` is the modern variant.
- **dlopen paths**: NVML location varies by distro (`/usr/lib/x86_64-linux-gnu/libnvidia-ml.so`, `/usr/lib/libnvidia-ml.so`). Try multiple paths or use `dlopen("libnvidia-ml.so.1", RTLD_LAZY)`.
- **Fortran procedure pointers + C**: passing a Fortran procedure as a `pthread_create` callback requires `bind(C)` on the Fortran procedure and careful argument marshaling.

## Deferred
(none yet)

## Notes
- The C shim files should be as thin as possible. Ideally each function is < 20 lines. All logic stays in Fortran.
- Consider writing a Fortran `ftop_c_util` module that provides convenience wrappers around the raw C bindings (e.g., converting C strings to Fortran strings, handling C null pointers).
- The platform abstraction uses Fortran 2003/2008 OOP (abstract types, deferred procedures). This is well-supported in gfortran.
