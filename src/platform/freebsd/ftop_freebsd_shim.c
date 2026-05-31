#include <errno.h>
#include <stddef.h>
#include <sys/resource.h>
#include <sys/sysctl.h>
#include <sys/types.h>

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
