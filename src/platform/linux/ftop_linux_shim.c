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

static long long ftop_linux_meminfo_value(const char *buffer, const char *key) {
  const char *line;
  const char *cursor;
  long long value;

  line = buffer;
  while (*line != '\0') {
    cursor = key;
    while (*cursor != '\0' && line[cursor - key] == *cursor) ++cursor;
    if (*cursor == '\0') {
      value = 0;
      if (sscanf(line + (cursor - key), " %lld kB", &value) == 1) return value * 1024LL;
      return 0;
    }

    while (*line != '\0' && *line != '\n') ++line;
    if (*line == '\n') ++line;
  }

  return 0;
}

int ftop_linux_memory_info(long long *total_bytes, long long *available_bytes, int *sys_errno) {
  char buffer[8192];
  long long mem_free;
  long long buffers;
  long long cached;
  ssize_t bytes_read;
  int fd;

  if (total_bytes == NULL || available_bytes == NULL || sys_errno == NULL) return -1;

  *total_bytes = 0;
  *available_bytes = 0;
  *sys_errno = 0;
  fd = open("/proc/meminfo", O_RDONLY);
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
  *total_bytes = ftop_linux_meminfo_value(buffer, "MemTotal:");
  *available_bytes = ftop_linux_meminfo_value(buffer, "MemAvailable:");
  if (*available_bytes <= 0) {
    mem_free = ftop_linux_meminfo_value(buffer, "MemFree:");
    buffers = ftop_linux_meminfo_value(buffer, "Buffers:");
    cached = ftop_linux_meminfo_value(buffer, "Cached:");
    *available_bytes = mem_free + buffers + cached;
  }

  if (*total_bytes <= 0) {
    *sys_errno = EINVAL;
    return -1;
  }
  if (*available_bytes > *total_bytes) *available_bytes = *total_bytes;

  return 0;
}
