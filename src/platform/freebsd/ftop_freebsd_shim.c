#include <errno.h>
#include <devstat.h>
#include <fcntl.h>
#include <kvm.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>
#include <sys/resource.h>
#include <sys/sysctl.h>
#include <sys/types.h>
#include <sys/user.h>
#include <unistd.h>

#define FTOP_FREEBSD_COMMAND_LEN 32
#define FTOP_FREEBSD_DEVSTAT_NAME_LEN 16

struct ftop_freebsd_process_info {
  int pid;
  int ppid;
  int uid;
  int state;
  char command[FTOP_FREEBSD_COMMAND_LEN];
};

struct ftop_freebsd_devstat_info {
  int device_number;
  int unit_number;
  int device_type;
  int priority;
  int block_size;
  long long bytes_read;
  long long bytes_written;
  long long bytes_freed;
  long long transfers_read;
  long long transfers_written;
  long long transfers_freed;
  char name[FTOP_FREEBSD_DEVSTAT_NAME_LEN];
};

#ifndef CPUSTATES
#define CPUSTATES 5
#endif

#ifndef CP_USER
#define CP_USER 0
#endif

#ifndef CP_NICE
#define CP_NICE 1
#endif

#ifndef CP_SYS
#define CP_SYS 2
#endif

#ifndef CP_INTR
#define CP_INTR 3
#endif

#ifndef CP_IDLE
#define CP_IDLE 4
#endif

struct ftop_freebsd_cpu_state_ticks {
  long long user;
  long long nice;
  long long system;
  long long idle;
  long long irq;
};

int ftop_freebsd_cpu_count(int *count, int *sys_errno) {
  int value;
  size_t value_len;

  if (count == NULL || sys_errno == NULL) return -1;

  *count = 0;
  *sys_errno = 0;
  value = 0;
  value_len = sizeof(value);
  if (sysctlbyname("kern.smp.cpus", &value, &value_len, NULL, 0) != 0) {
    *sys_errno = errno;
    return -1;
  }

  if (value <= 0) {
    *sys_errno = EINVAL;
    return -1;
  }

  *count = value;
  return 0;
}

int ftop_freebsd_cpu_ticks(long long *total_ticks, long long *idle_ticks, int *sys_errno) {
  long cpu_time[CPUSTATES];
  long long total;
  size_t cpu_time_len;
  int i;

  if (total_ticks == NULL || idle_ticks == NULL || sys_errno == NULL) return -1;

  *total_ticks = 0;
  *idle_ticks = 0;
  *sys_errno = 0;
  cpu_time_len = sizeof(cpu_time);
  if (sysctlbyname("kern.cp_time", cpu_time, &cpu_time_len, NULL, 0) != 0) {
    *sys_errno = errno;
    return -1;
  }

  total = 0;
  for (i = 0; i < CPUSTATES; ++i) {
    if (cpu_time[i] > 0) total += (long long)cpu_time[i];
  }

  if (total <= 0) {
    *sys_errno = EINVAL;
    return -1;
  }

  *total_ticks = total;
  *idle_ticks = cpu_time[CP_IDLE] > 0 ? (long long)cpu_time[CP_IDLE] : 0;
  return 0;
}

static long long ftop_nonnegative_long_tick(long value) {
  return value > 0 ? (long long)value : 0;
}

int ftop_freebsd_cpu_state_ticks(
    struct ftop_freebsd_cpu_state_ticks *buffer, size_t capacity, size_t *cpu_count, int *sys_errno) {
  long *cpu_times;
  size_t cpu_times_len;
  size_t available_cpus;
  size_t copy_count;
  size_t i;

  if (buffer == NULL || cpu_count == NULL || sys_errno == NULL || capacity == 0) return -1;

  *cpu_count = 0U;
  *sys_errno = 0;
  cpu_times = NULL;
  cpu_times_len = 0U;
  if (sysctlbyname("kern.cp_times", NULL, &cpu_times_len, NULL, 0) != 0) {
    *sys_errno = errno;
    return -1;
  }
  if (cpu_times_len < sizeof(long) * CPUSTATES) {
    *sys_errno = EINVAL;
    return -1;
  }

  cpu_times = (long *)malloc(cpu_times_len);
  if (cpu_times == NULL) {
    *sys_errno = ENOMEM;
    return -1;
  }

  if (sysctlbyname("kern.cp_times", cpu_times, &cpu_times_len, NULL, 0) != 0) {
    *sys_errno = errno;
    free(cpu_times);
    return -1;
  }

  available_cpus = cpu_times_len / (sizeof(long) * CPUSTATES);
  copy_count = available_cpus < capacity ? available_cpus : capacity;
  for (i = 0U; i < copy_count; ++i) {
    long *cpu = &cpu_times[i * CPUSTATES];
    buffer[i].user = ftop_nonnegative_long_tick(cpu[CP_USER]);
    buffer[i].nice = ftop_nonnegative_long_tick(cpu[CP_NICE]);
    buffer[i].system = ftop_nonnegative_long_tick(cpu[CP_SYS]);
    buffer[i].idle = ftop_nonnegative_long_tick(cpu[CP_IDLE]);
    buffer[i].irq = ftop_nonnegative_long_tick(cpu[CP_INTR]);
  }

  free(cpu_times);
  if (copy_count == 0U) {
    *sys_errno = EINVAL;
    return -1;
  }
  *cpu_count = copy_count;
  return 0;
}

static int ftop_freebsd_sysctl_int_value(const char *name, int *value, int *sys_errno) {
  size_t value_len;

  if (name == NULL || value == NULL || sys_errno == NULL) return -1;

  *value = 0;
  *sys_errno = 0;
  value_len = sizeof(*value);
  if (sysctlbyname(name, value, &value_len, NULL, 0) != 0) {
    *sys_errno = errno;
    return -1;
  }
  if (value_len != sizeof(*value)) {
    *sys_errno = EOVERFLOW;
    return -1;
  }

  return 0;
}

int ftop_freebsd_sysctl_int(const char *name, int *value, int *sys_errno) {
  return ftop_freebsd_sysctl_int_value(name, value, sys_errno);
}

int ftop_freebsd_cpu_frequency(int cpu_index, int *freq_mhz, int *sys_errno) {
  char name[64];
  int written;

  if (freq_mhz == NULL || sys_errno == NULL) return -1;
  *freq_mhz = 0;
  *sys_errno = 0;
  if (cpu_index < 0) {
    *sys_errno = EINVAL;
    return -1;
  }

  written = snprintf(name, sizeof(name), "dev.cpu.%d.freq", cpu_index);
  if (written < 0 || (size_t)written >= sizeof(name)) {
    *sys_errno = EOVERFLOW;
    return -1;
  }

  return ftop_freebsd_sysctl_int_value(name, freq_mhz, sys_errno);
}

static int ftop_freebsd_deci_kelvin_to_celsius(int raw_temperature, double *temperature_c, int *sys_errno) {
  double value;

  value = ((double)raw_temperature / 10.0) - 273.15;
  if (value < -100.0 || value > 150.0) {
    *sys_errno = EINVAL;
    return -1;
  }

  *temperature_c = value;
  return 0;
}

int ftop_freebsd_cpu_temperature(int cpu_index, double *temperature_c, int *sys_errno) {
  char name[64];
  int raw_temperature;
  int written;

  if (temperature_c == NULL || sys_errno == NULL) return -1;
  *temperature_c = 0.0;
  *sys_errno = 0;
  if (cpu_index < 0) {
    *sys_errno = EINVAL;
    return -1;
  }

  written = snprintf(name, sizeof(name), "dev.cpu.%d.temperature", cpu_index);
  if (written < 0 || (size_t)written >= sizeof(name)) {
    *sys_errno = EOVERFLOW;
    return -1;
  }
  if (ftop_freebsd_sysctl_int_value(name, &raw_temperature, sys_errno) != 0) return -1;

  return ftop_freebsd_deci_kelvin_to_celsius(raw_temperature, temperature_c, sys_errno);
}

int ftop_freebsd_acpi_temperature(double *temperature_c, int *sys_errno) {
  int raw_temperature;

  if (temperature_c == NULL || sys_errno == NULL) return -1;
  *temperature_c = 0.0;
  *sys_errno = 0;
  if (ftop_freebsd_sysctl_int_value("hw.acpi.thermal.tz0.temperature", &raw_temperature, sys_errno) != 0) return -1;

  return ftop_freebsd_deci_kelvin_to_celsius(raw_temperature, temperature_c, sys_errno);
}

int ftop_freebsd_sysctl_long(const char *name, long *value, int *sys_errno) {
  size_t value_len;

  if (name == NULL || value == NULL || sys_errno == NULL) return -1;

  *value = 0L;
  *sys_errno = 0;
  value_len = sizeof(*value);
  if (sysctlbyname(name, value, &value_len, NULL, 0) != 0) {
    *sys_errno = errno;
    return -1;
  }
  if (value_len != sizeof(*value)) {
    *sys_errno = EOVERFLOW;
    return -1;
  }

  return 0;
}

int ftop_freebsd_sysctl_string(const char *name, char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
  size_t actual_len;

  if (name == NULL || buffer == NULL || value_len == NULL || sys_errno == NULL || buffer_len == 0) return -1;

  buffer[0] = '\0';
  *value_len = 0U;
  *sys_errno = 0;
  actual_len = buffer_len;
  if (sysctlbyname(name, buffer, &actual_len, NULL, 0) != 0) {
    *sys_errno = errno;
    return -1;
  }

  if (actual_len == 0 || buffer[actual_len - 1U] != '\0') {
    if (actual_len < buffer_len) buffer[actual_len] = '\0';
    if (actual_len >= buffer_len) buffer[buffer_len - 1U] = '\0';
  }
  *value_len = strnlen(buffer, buffer_len);

  return 0;
}

int ftop_freebsd_sysctl_bytes(const char *name, void *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
  size_t actual_len;

  if (name == NULL || buffer == NULL || value_len == NULL || sys_errno == NULL || buffer_len == 0) return -1;

  *value_len = 0U;
  *sys_errno = 0;
  actual_len = buffer_len;
  if (sysctlbyname(name, buffer, &actual_len, NULL, 0) != 0) {
    *sys_errno = errno;
    return -1;
  }

  *value_len = actual_len;
  return 0;
}

void *ftop_freebsd_kvm_open(int *sys_errno) {
  char error_buffer[256];
  kvm_t *handle;

  if (sys_errno == NULL) return NULL;

  *sys_errno = 0;
  error_buffer[0] = '\0';
  handle = kvm_openfiles(NULL, "/dev/null", NULL, O_RDONLY, error_buffer);
  if (handle == NULL) {
    *sys_errno = errno != 0 ? errno : ENOENT;
    return NULL;
  }

  return handle;
}

int ftop_freebsd_kvm_close(void *handle, int *sys_errno) {
  if (handle == NULL || sys_errno == NULL) return -1;

  *sys_errno = 0;
  if (kvm_close((kvm_t *)handle) != 0) {
    *sys_errno = errno;
    return -1;
  }

  return 0;
}

static void ftop_copy_process_command(char *destination, const char *source) {
  size_t i;

  for (i = 0U; i + 1U < FTOP_FREEBSD_COMMAND_LEN && source[i] != '\0'; ++i) destination[i] = source[i];
  destination[i] = '\0';
}

int ftop_freebsd_kvm_getprocs(
    void *handle, struct ftop_freebsd_process_info *buffer, size_t capacity, size_t *process_count, int *sys_errno) {
  struct kinfo_proc *processes;
  size_t copy_count;
  int count;
  size_t i;

  if (handle == NULL || buffer == NULL || process_count == NULL || sys_errno == NULL || capacity == 0) return -1;

  *process_count = 0U;
  *sys_errno = 0;
  count = 0;
  processes = kvm_getprocs((kvm_t *)handle, KERN_PROC_PROC, 0, &count);
  if (processes == NULL || count < 0) {
    *sys_errno = errno != 0 ? errno : EINVAL;
    return -1;
  }

  copy_count = (size_t)count < capacity ? (size_t)count : capacity;
  for (i = 0U; i < copy_count; ++i) {
    buffer[i].pid = (int)processes[i].ki_pid;
    buffer[i].ppid = (int)processes[i].ki_ppid;
    buffer[i].uid = (int)processes[i].ki_uid;
    buffer[i].state = (int)processes[i].ki_stat;
    ftop_copy_process_command(buffer[i].command, processes[i].ki_comm);
  }

  *process_count = copy_count;
  return 0;
}

static void ftop_copy_devstat_name(char *destination, const char *source) {
  size_t i;

  for (i = 0U; i + 1U < FTOP_FREEBSD_DEVSTAT_NAME_LEN && source[i] != '\0'; ++i) destination[i] = source[i];
  destination[i] = '\0';
}

static void ftop_copy_devstat_device(struct ftop_freebsd_devstat_info *destination, const struct devstat *source) {
  destination->device_number = (int)source->device_number;
  destination->unit_number = source->unit_number;
  destination->device_type = (int)source->device_type;
  destination->priority = (int)source->priority;
  destination->block_size = (int)source->block_size;
  destination->bytes_read = (long long)source->bytes[DEVSTAT_READ];
  destination->bytes_written = (long long)source->bytes[DEVSTAT_WRITE];
  destination->bytes_freed = (long long)source->bytes[DEVSTAT_FREE];
  destination->transfers_read = (long long)source->operations[DEVSTAT_READ];
  destination->transfers_written = (long long)source->operations[DEVSTAT_WRITE];
  destination->transfers_freed = (long long)source->operations[DEVSTAT_FREE];
  ftop_copy_devstat_name(destination->name, source->device_name);
}

int ftop_freebsd_devstat_getdevs(
    struct ftop_freebsd_devstat_info *buffer, size_t capacity, size_t *device_count, long long *generation, int *sys_errno) {
  struct devinfo devinfo;
  struct statinfo statinfo;
  size_t copy_count;
  int rc;
  size_t i;

  if (buffer == NULL || device_count == NULL || generation == NULL || sys_errno == NULL || capacity == 0) return -1;

  *device_count = 0U;
  *generation = 0;
  *sys_errno = 0;
  memset(&devinfo, 0, sizeof(devinfo));
  memset(&statinfo, 0, sizeof(statinfo));
  statinfo.dinfo = &devinfo;

  if (devstat_checkversion(NULL) != 0) {
    *sys_errno = EINVAL;
    return -1;
  }

  rc = devstat_getdevs(NULL, &statinfo);
  if (rc < 0) {
    *sys_errno = errno != 0 ? errno : EINVAL;
    free(devinfo.mem_ptr);
    return -1;
  }

  copy_count = (size_t)devinfo.numdevs < capacity ? (size_t)devinfo.numdevs : capacity;
  for (i = 0U; i < copy_count; ++i) ftop_copy_devstat_device(&buffer[i], &devinfo.devices[i]);

  *device_count = copy_count;
  *generation = (long long)devinfo.generation;
  free(devinfo.mem_ptr);
  return 0;
}

static int ftop_sysctl_ulong(const char *name, unsigned long *value) {
  size_t value_len;

  value_len = sizeof(*value);
  *value = 0UL;
  return sysctlbyname(name, value, &value_len, NULL, 0);
}

static int ftop_sysctl_uint(const char *name, unsigned int *value) {
  size_t value_len;

  value_len = sizeof(*value);
  *value = 0U;
  return sysctlbyname(name, value, &value_len, NULL, 0);
}

static unsigned int ftop_optional_sysctl_uint(const char *name) {
  unsigned int value;

  if (ftop_sysctl_uint(name, &value) != 0) return 0U;
  return value;
}

static int ftop_freebsd_swap_info(long long *total_bytes, long long *used_bytes, int *sys_errno) {
  char error_buffer[256];
  struct kvm_swap swap_info;
  kvm_t *handle;
  long long page_size;
  int rc;

  if (total_bytes == NULL || used_bytes == NULL || sys_errno == NULL) return -1;

  *total_bytes = 0;
  *used_bytes = 0;
  *sys_errno = 0;
  memset(&swap_info, 0, sizeof(swap_info));
  error_buffer[0] = '\0';

  handle = kvm_openfiles(NULL, "/dev/null", NULL, O_RDONLY, error_buffer);
  if (handle == NULL) {
    *sys_errno = errno != 0 ? errno : ENOENT;
    return -1;
  }

  rc = kvm_getswapinfo(handle, &swap_info, 1, 0);
  if (rc < 0) {
    *sys_errno = errno != 0 ? errno : EINVAL;
    (void)kvm_close(handle);
    return -1;
  }
  (void)kvm_close(handle);

  page_size = (long long)getpagesize();
  *total_bytes = (long long)swap_info.ksw_total * page_size;
  *used_bytes = (long long)swap_info.ksw_used * page_size;
  if (*used_bytes > *total_bytes) *used_bytes = *total_bytes;

  return 0;
}

int ftop_freebsd_memory_info(long long *total_bytes, long long *available_bytes, long long *swap_total_bytes,
    long long *swap_used_bytes, int *sys_errno) {
  unsigned long physical_memory;
  unsigned int free_pages;
  unsigned int inactive_pages;
  unsigned int available_pages;
  long long swap_total;
  long long swap_used;
  long long page_size;
  int swap_errno;

  if (total_bytes == NULL || available_bytes == NULL || swap_total_bytes == NULL || swap_used_bytes == NULL ||
      sys_errno == NULL) {
    return -1;
  }

  *total_bytes = 0;
  *available_bytes = 0;
  *swap_total_bytes = 0;
  *swap_used_bytes = 0;
  *sys_errno = 0;
  if (ftop_sysctl_ulong("hw.physmem", &physical_memory) != 0) {
    *sys_errno = errno;
    return -1;
  }
  if (ftop_sysctl_uint("vm.stats.vm.v_free_count", &free_pages) != 0) {
    *sys_errno = errno;
    return -1;
  }
  if (ftop_sysctl_uint("vm.stats.vm.v_inactive_count", &inactive_pages) != 0) {
    *sys_errno = errno;
    return -1;
  }

  page_size = (long long)getpagesize();
  available_pages = free_pages + inactive_pages;
  available_pages += ftop_optional_sysctl_uint("vm.stats.vm.v_cache_count");
  available_pages += ftop_optional_sysctl_uint("vm.stats.vm.v_laundry_count");

  *total_bytes = (long long)physical_memory;
  *available_bytes = (long long)available_pages * page_size;
  if (*total_bytes <= 0) {
    *sys_errno = EINVAL;
    return -1;
  }
  if (*available_bytes > *total_bytes) *available_bytes = *total_bytes;
  swap_total = 0;
  swap_used = 0;
  swap_errno = 0;
  if (ftop_freebsd_swap_info(&swap_total, &swap_used, &swap_errno) == 0) {
    *swap_total_bytes = swap_total;
    *swap_used_bytes = swap_used;
  }

  return 0;
}

int ftop_freebsd_load_average(double *loads, int *sys_errno) {
  int count;

  if (loads == NULL || sys_errno == NULL) return -1;

  loads[0] = 0.0;
  loads[1] = 0.0;
  loads[2] = 0.0;
  *sys_errno = 0;
  count = getloadavg(loads, 3);
  if (count < 3) {
    *sys_errno = errno != 0 ? errno : EINVAL;
    return -1;
  }

  return 0;
}
