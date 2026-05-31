#include <errno.h>
#include <fcntl.h>
#include <stddef.h>
#include <stdio.h>
#include <unistd.h>

int ftop_linux_cpu_count(int *count, int *sys_errno) {
  long value;

  if (count == NULL || sys_errno == NULL) return -1;

  *count = 0;
  *sys_errno = 0;
  value = sysconf(_SC_NPROCESSORS_ONLN);
  if (value <= 0L) {
    *sys_errno = errno != 0 ? errno : EINVAL;
    return -1;
  }

  *count = (int)value;
  return 0;
}

int ftop_linux_cpu_ticks(long long *total_ticks, long long *idle_ticks, int *sys_errno) {
  char buffer[4096];
  long long user;
  long long nice;
  long long system;
  long long idle;
  long long iowait;
  long long irq;
  long long softirq;
  long long steal;
  ssize_t bytes_read;
  int fd;
  int fields;

  if (total_ticks == NULL || idle_ticks == NULL || sys_errno == NULL) return -1;

  *total_ticks = 0;
  *idle_ticks = 0;
  *sys_errno = 0;
  fd = open("/proc/stat", O_RDONLY);
  if (fd < 0) {
    *sys_errno = errno;
    return -1;
  }

  bytes_read = read(fd, buffer, sizeof(buffer) - 1U);
  if (bytes_read < 0) {
    *sys_errno = errno;
    close(fd);
    return -1;
  }
  close(fd);

  buffer[bytes_read] = '\0';
  fields = sscanf(buffer, "cpu %lld %lld %lld %lld %lld %lld %lld %lld", &user, &nice, &system,
                  &idle, &iowait, &irq, &softirq, &steal);
  if (fields < 4) {
    *sys_errno = EINVAL;
    return -1;
  }
  if (fields < 5) iowait = 0;
  if (fields < 6) irq = 0;
  if (fields < 7) softirq = 0;
  if (fields < 8) steal = 0;

  *idle_ticks = idle + iowait;
  *total_ticks = user + nice + system + idle + iowait + irq + softirq + steal;
  if (*total_ticks <= 0) {
    *sys_errno = EINVAL;
    return -1;
  }

  return 0;
}
