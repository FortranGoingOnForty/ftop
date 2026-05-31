#include <errno.h>
#include <mach/host_info.h>
#include <mach/mach_host.h>
#include <mach/mach_init.h>
#include <mach/processor_info.h>
#include <mach/vm_map.h>
#include <stddef.h>
#include <string.h>
#include <sys/sysctl.h>
#include <sys/types.h>
#include <unistd.h>

struct ftop_macos_processor_ticks {
  long long user;
  long long system;
  long long idle;
  long long nice;
};

int ftop_macos_cpu_count(int *count, int *sys_errno) {
  int value;
  size_t value_len;

  if (count == NULL || sys_errno == NULL) return -1;

  *count = 0;
  *sys_errno = 0;
  value = 0;
  value_len = sizeof(value);
  if (sysctlbyname("hw.logicalcpu", &value, &value_len, NULL, 0) != 0) {
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

int ftop_macos_cpu_ticks(long long *total_ticks, long long *idle_ticks, int *sys_errno) {
  host_cpu_load_info_data_t cpu_info;
  mach_msg_type_number_t count;
  kern_return_t rc;
  long long user;
  long long system;
  long long idle;
  long long nice;

  if (total_ticks == NULL || idle_ticks == NULL || sys_errno == NULL) return -1;

  *total_ticks = 0;
  *idle_ticks = 0;
  *sys_errno = 0;
  count = HOST_CPU_LOAD_INFO_COUNT;
  rc = host_statistics(mach_host_self(), HOST_CPU_LOAD_INFO, (host_info_t)&cpu_info, &count);
  if (rc != KERN_SUCCESS) {
    *sys_errno = (int)rc;
    return -1;
  }

  user = (long long)cpu_info.cpu_ticks[CPU_STATE_USER];
  system = (long long)cpu_info.cpu_ticks[CPU_STATE_SYSTEM];
  idle = (long long)cpu_info.cpu_ticks[CPU_STATE_IDLE];
  nice = (long long)cpu_info.cpu_ticks[CPU_STATE_NICE];
  *idle_ticks = idle;
  *total_ticks = user + system + idle + nice;
  if (*total_ticks <= 0) {
    *sys_errno = EINVAL;
    return -1;
  }

  return 0;
}

int ftop_macos_memory_info(long long *total_bytes, long long *available_bytes, int *sys_errno) {
  vm_statistics64_data_t vm_info;
  mach_msg_type_number_t count;
  unsigned long long total;
  size_t total_len;
  kern_return_t rc;
  long long page_size;
  long long available_pages;

  if (total_bytes == NULL || available_bytes == NULL || sys_errno == NULL) return -1;

  *total_bytes = 0;
  *available_bytes = 0;
  *sys_errno = 0;
  total = 0ULL;
  total_len = sizeof(total);
  if (sysctlbyname("hw.memsize", &total, &total_len, NULL, 0) != 0) {
    *sys_errno = errno;
    return -1;
  }

  count = HOST_VM_INFO64_COUNT;
  rc = host_statistics64(mach_host_self(), HOST_VM_INFO64, (host_info64_t)&vm_info, &count);
  if (rc != KERN_SUCCESS) {
    *sys_errno = (int)rc;
    return -1;
  }

  page_size = (long long)sysconf(_SC_PAGESIZE);
  available_pages = (long long)vm_info.free_count + (long long)vm_info.inactive_count;
  available_pages += (long long)vm_info.speculative_count;

  *total_bytes = (long long)total;
  *available_bytes = available_pages * page_size;
  if (*total_bytes <= 0) {
    *sys_errno = EINVAL;
    return -1;
  }
  if (*available_bytes > *total_bytes) *available_bytes = *total_bytes;

  return 0;
}

int ftop_macos_processor_ticks(
    struct ftop_macos_processor_ticks *buffer, size_t capacity, size_t *processor_count, int *sys_errno) {
  processor_cpu_load_info_t cpu_load;
  processor_info_array_t cpu_info;
  mach_msg_type_number_t info_count;
  natural_t cpu_count;
  kern_return_t rc;
  size_t copy_count;
  size_t i;

  if (buffer == NULL || processor_count == NULL || sys_errno == NULL || capacity == 0) return -1;

  *processor_count = 0U;
  *sys_errno = 0;
  cpu_info = NULL;
  info_count = 0;
  cpu_count = 0;
  rc = host_processor_info(mach_host_self(), PROCESSOR_CPU_LOAD_INFO, &cpu_count, &cpu_info, &info_count);
  if (rc != KERN_SUCCESS) {
    *sys_errno = (int)rc;
    return -1;
  }

  cpu_load = (processor_cpu_load_info_t)cpu_info;
  copy_count = (size_t)cpu_count < capacity ? (size_t)cpu_count : capacity;
  for (i = 0U; i < copy_count; ++i) {
    buffer[i].user = (long long)cpu_load[i].cpu_ticks[CPU_STATE_USER];
    buffer[i].system = (long long)cpu_load[i].cpu_ticks[CPU_STATE_SYSTEM];
    buffer[i].idle = (long long)cpu_load[i].cpu_ticks[CPU_STATE_IDLE];
    buffer[i].nice = (long long)cpu_load[i].cpu_ticks[CPU_STATE_NICE];
  }

  rc = vm_deallocate(mach_task_self(), (vm_address_t)cpu_info, (vm_size_t)info_count * sizeof(integer_t));
  if (rc != KERN_SUCCESS) {
    *sys_errno = (int)rc;
    return -1;
  }

  *processor_count = copy_count;
  return 0;
}

static size_t ftop_bounded_strlen(const char *text, size_t capacity) {
  size_t i;

  for (i = 0U; i < capacity && text[i] != '\0'; ++i) {
  }
  return i;
}

int ftop_macos_sysctl_int(const char *name, int *value, int *sys_errno) {
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

int ftop_macos_sysctl_long_long(const char *name, long long *value, int *sys_errno) {
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

int ftop_macos_sysctl_string(const char *name, char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
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
  *value_len = ftop_bounded_strlen(buffer, buffer_len);

  return 0;
}

int ftop_macos_sysctl_bytes(const char *name, void *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
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

int ftop_macos_iokit_gpu_count(int *count, int *sys_errno) {
  if (count == NULL || sys_errno == NULL) return -1;

  *count = 0;
  *sys_errno = 0;
  return 0;
}

int ftop_macos_iokit_disk_count(int *count, int *sys_errno) {
  if (count == NULL || sys_errno == NULL) return -1;

  *count = 0;
  *sys_errno = 0;
  return 0;
}
