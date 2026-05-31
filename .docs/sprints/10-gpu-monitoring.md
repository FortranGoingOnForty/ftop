# Sprint 10: GPU Monitoring

## Goal
GPU panel supporting NVIDIA, AMD, and Intel GPUs. Shows utilization, temperature, VRAM usage, power draw, and clock speeds. NVIDIA via NVML (runtime dlopen), AMD via amdgpu sysfs + ROCm SMI, Intel via i915 sysfs.

## Status: NOT STARTED

## Targets

### GPU Data Collection
- [ ] `src/collect/ftop_gpu_data.f90`
  - [ ] `type :: gpu_info`
    - vendor (nvidia/amd/intel), name, pci_id
    - utilization_percent (compute), memory_utilization_percent
    - temp_celsius, temp_max
    - memory_used, memory_total
    - power_watts, power_limit_watts
    - clock_core_mhz, clock_memory_mhz, clock_max_core_mhz
    - fan_speed_percent
    - encoder_utilization, decoder_utilization (optional)
    - driver_version
  - [ ] `type :: gpu_table` — collection of all detected GPUs
  - [ ] Ring buffers for utilization and temperature history
  - [ ] Auto-detection: enumerate GPUs at startup, handle hotplug (eGPU)

### NVIDIA Collection (Linux + FreeBSD)
- [ ] `src/platform/ftop_nvidia.f90` + C shim additions to `ftop_dl.c`
  - [ ] Runtime loading: `dlopen("libnvidia-ml.so.1", RTLD_LAZY)`
  - [ ] NVML API calls via function pointers:
    - [ ] `nvmlInit_v2()` — initialize library
    - [ ] `nvmlDeviceGetCount_v2()` — GPU count
    - [ ] `nvmlDeviceGetHandleByIndex_v2()` — get device handle
    - [ ] `nvmlDeviceGetName()` — GPU name
    - [ ] `nvmlDeviceGetUtilizationRates()` — GPU + memory utilization
    - [ ] `nvmlDeviceGetTemperature()` — current temp
    - [ ] `nvmlDeviceGetMemoryInfo()` — used/total VRAM
    - [ ] `nvmlDeviceGetPowerUsage()` — power in milliwatts
    - [ ] `nvmlDeviceGetClockInfo()` — core/memory clocks
    - [ ] `nvmlDeviceGetFanSpeed()` — fan RPM percentage
    - [ ] `nvmlDeviceGetEncoderUtilization()` — NVENC usage
    - [ ] `nvmlDeviceGetDecoderUtilization()` — NVDEC usage
    - [ ] `nvmlShutdown()` — cleanup
  - [ ] Handle NVML not present (no NVIDIA GPU or driver not installed) gracefully
  - [ ] Handle multiple NVIDIA GPUs

### AMD Collection (Linux)
- [ ] `src/platform/ftop_amd_gpu.f90`
  - [ ] Device discovery: scan `/sys/class/drm/card*/device/vendor` for `0x1002` (AMD)
  - [ ] Utilization: read `/sys/class/drm/card*/device/gpu_busy_percent`
  - [ ] Temperature: read `/sys/class/drm/card*/device/hwmon/hwmon*/temp1_input` (millidegrees)
  - [ ] VRAM: read `/sys/class/drm/card*/device/mem_info_vram_used` and `mem_info_vram_total`
  - [ ] Power: read `/sys/class/drm/card*/device/hwmon/hwmon*/power1_average` (microwatts)
  - [ ] Clock: read `/sys/class/drm/card*/device/pp_dpm_sclk` (core) and `pp_dpm_mclk` (memory)
    - [ ] Parse active frequency (line with `*` marker)
  - [ ] Fan: read `/sys/class/drm/card*/device/hwmon/hwmon*/pwm1` (0-255 -> 0-100%)
  - [ ] GPU name: read `/sys/class/drm/card*/device/product_name` or PCI ID lookup
  - [ ] Handle multiple AMD GPUs
  - [ ] Optional ROCm SMI: if `librocm_smi64.so` available via dlopen, use for richer data

### Intel Collection (Linux)
- [ ] `src/platform/ftop_intel_gpu.f90`
  - [ ] Device discovery: scan `/sys/class/drm/card*/device/vendor` for `0x8086` (Intel)
  - [ ] Utilization: read `/sys/class/drm/card*/gt/gt_cur_freq_mhz` and `gt_act_freq_mhz`
    - [ ] Alternatively: i915 PMU counters via perf_event_open (complex, requires C shim)
    - [ ] Simple approach: frequency ratio as utilization proxy
  - [ ] Temperature: read associated hwmon sensor
  - [ ] VRAM: Intel integrated GPUs share system memory — report allocated via i915 sysfs if available
  - [ ] Power: read from hwmon if available (Xe GPUs)
  - [ ] Handle both integrated (UHD/Iris) and discrete (Arc) Intel GPUs

### macOS GPU Collection
- [ ] Apple Silicon: `IOReport` framework for GPU utilization
  - [ ] Complex IOKit interaction via C shim
  - [ ] Power, utilization, temperature channels
- [ ] AMD discrete (older Macs): IOKit `AGPMController` stats
- [ ] This is the most complex platform for GPU. Accept partial coverage initially.

### FreeBSD GPU Collection
- [ ] NVIDIA: same NVML approach as Linux (NVML is available on FreeBSD)
- [ ] AMD: DRM/KMS sysfs may differ from Linux. Check `/dev/dri/` + ioctl approach.
- [ ] Intel: similar sysfs paths under DRM

### GPU Panel Widget
- [ ] `src/widgets/ftop_gpu.f90`
  - [ ] One section per detected GPU:
    - [ ] GPU name + vendor icon/label
    - [ ] Utilization bar with color gradient
    - [ ] VRAM bar: `[████░░░░] 4.2/8.0 GiB`
    - [ ] Temperature with color coding
    - [ ] Power draw: `123W / 250W`
    - [ ] Clock speeds: `Core: 1850 MHz  Mem: 8001 MHz`
    - [ ] Fan speed percentage
  - [ ] Braille history graph for utilization
  - [ ] Panel hides automatically if no GPUs detected
  - [ ] Multi-GPU: tabs or stacked sections

## Definition of Done
- NVIDIA GPUs detected via NVML with all metrics displayed
- AMD GPUs detected via sysfs with utilization, temp, VRAM, power, clocks
- Intel GPUs detected with at least frequency and temperature
- GPU panel renders correctly with sparkline history
- Graceful handling when no GPU present (panel hidden)
- Graceful handling when GPU vendor library unavailable
- Works on Linux (all three vendors). FreeBSD/macOS: NVIDIA at minimum.

## Dependencies
- Sprint 02 (dlopen shim for NVML)
- Sprint 03 (collector framework, ring buffers)
- Sprint 04 (bar meter, graph widgets)
- Sprint 06 (layout engine)

## Testing
- **Unit**: NVML function pointer loading (mock dlopen in test)
- **Unit**: AMD sysfs parsing with synthetic file content
- **Unit**: Intel sysfs parsing with synthetic file content
- **Unit**: GPU info struct population from known values
- **Integration**: GPU detection matches `lspci | grep VGA` output
- **Integration**: NVIDIA metrics match `nvidia-smi` output (within tolerance)
- **Snapshot**: GPU panel rendered with synthetic data -> golden comparison
- **Graceful degradation**: test with NVML absent, sysfs paths missing

## Pitfalls
- **NVML API versioning**: NVML changes function signatures between versions (hence `_v2` suffixes). Use the latest `_v2` variants. Check for `NULL` function pointers after `dlsym` and fall back to older variants.
- **NVML initialization**: `nvmlInit()` can block for several seconds if the NVIDIA driver is loading. Call it in the collector thread, not the main thread, to avoid startup delay.
- **AMD sysfs path instability**: DRM card numbering (`card0`, `card1`) can change between boots or when GPUs are added/removed. Always re-enumerate by scanning vendor IDs.
- **Intel GPU utilization**: there's no simple "gpu_busy_percent" like AMD. True utilization requires i915 PMU counters (perf_event_open with i915-specific events). The frequency-based proxy is imprecise but works without special permissions.
- **Multi-GPU systems**: a system may have NVIDIA + Intel, or AMD + Intel, or multiple discrete GPUs. Handle mixed vendors cleanly.
- **Permission issues**: some GPU sysfs files may require root or membership in the `video` group. Handle EACCES gracefully.
- **eGPU hotplug**: Thunderbolt eGPUs can appear/disappear at runtime. Rescan GPU list periodically (every 30 seconds).

## Deferred
- macOS Apple Silicon GPU monitoring (complex IOKit, defer to post-v1.0 if too much effort)
- NVENC/NVDEC encoder/decoder utilization (nice to have, defer if needed)

## Notes
- btop's GPU implementation is the best reference. It handles NVIDIA (NVML), AMD (sysfs), and Intel (sysfs + embedded i915 monitoring code) on Linux.
- The NVML header is available as a standalone download from NVIDIA — don't need the full CUDA SDK. We only need the C types and function signatures for the Fortran interface block.
- Consider shipping a `gpu.toml` config that lets users override GPU detection or disable specific vendor backends.
