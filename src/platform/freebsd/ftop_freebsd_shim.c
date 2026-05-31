#include <errno.h>
#include <fcntl.h>
#include <kvm.h>
#include <stddef.h>
#include <string.h>
#include <sys/param.h>
#include <sys/resource.h>
#include <sys/sysctl.h>
#include <sys/types.h>
#include <sys/user.h>
#include <unistd.h>

#define FTOP_FREEBSD_COMMAND_LEN 32

struct ftop_freebsd_process_info {
  int pid;
  int ppid;
  int uid;
  int state;
  char command[FTOP_FREEBSD_COMMAND_LEN];
};

#ifndef CPUSTATES
#define CPUSTATES 5
#endif

#ifndef CP_IDLE
#define CP_IDLE 4
#endif

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

int ftop_freebsd_sysctl_int(const char *name, int *value, int *sys_errno) {
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

int ftop_freebsd_memory_info(long long *total_bytes, long long *available_bytes, int *sys_errno) {
  unsigned long physical_memory;
  unsigned int free_pages;
  unsigned int inactive_pages;
  unsigned int available_pages;
  long long page_size;

  if (total_bytes == NULL || available_bytes == NULL || sys_errno == NULL) return -1;

  *total_bytes = 0;
  *available_bytes = 0;
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

  return 0;
}
