#include <errno.h>
#include <mach/host_info.h>
#include <mach/mach_host.h>
#include <mach/mach_init.h>
#include <stddef.h>
#include <sys/sysctl.h>
#include <sys/types.h>

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
