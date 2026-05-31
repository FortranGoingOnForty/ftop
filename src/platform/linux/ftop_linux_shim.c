#include <errno.h>
#include <dirent.h>
#include <fcntl.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define FTOP_LINUX_CPUINFO_BUFFER_LEN 1048576U
#define FTOP_LINUX_HWMON_NAME_LEN 128
#define FTOP_LINUX_HWMON_PATH_LEN 256

struct ftop_linux_hwmon_sensor {
  char path[FTOP_LINUX_HWMON_PATH_LEN];
  char name[FTOP_LINUX_HWMON_NAME_LEN];
};

static int ftop_read_file_into_buffer(const char *path, char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
  ssize_t bytes_read;
  int fd;

  if (path == NULL || buffer == NULL || value_len == NULL || sys_errno == NULL || buffer_len < 2U) return -1;

  buffer[0] = '\0';
  *value_len = 0U;
  *sys_errno = 0;

  fd = open(path, O_RDONLY);
  if (fd < 0) {
    *sys_errno = errno;
    return -1;
  }

  bytes_read = read(fd, buffer, buffer_len - 1U);
  if (bytes_read < 0) {
    *sys_errno = errno;
    close(fd);
    return -1;
  }

  close(fd);
  buffer[bytes_read] = '\0';
  *value_len = (size_t)bytes_read;
  return 0;
}

int ftop_linux_read_proc_stat(char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
  return ftop_read_file_into_buffer("/proc/stat", buffer, buffer_len, value_len, sys_errno);
}

int ftop_linux_read_proc_meminfo(char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
  return ftop_read_file_into_buffer("/proc/meminfo", buffer, buffer_len, value_len, sys_errno);
}

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

static char *ftop_read_file_once(const char *path, size_t capacity, ssize_t *bytes_read, int *sys_errno) {
  char *buffer;
  int fd;

  buffer = malloc(capacity + 1U);
  if (buffer == NULL) {
    *sys_errno = errno != 0 ? errno : ENOMEM;
    return NULL;
  }

  fd = open(path, O_RDONLY);
  if (fd < 0) {
    *sys_errno = errno;
    free(buffer);
    return NULL;
  }

  *bytes_read = read(fd, buffer, capacity);
  if (*bytes_read < 0) {
    *sys_errno = errno;
    close(fd);
    free(buffer);
    return NULL;
  }

  close(fd);
  buffer[*bytes_read] = '\0';
  return buffer;
}

static int ftop_cpuinfo_match_line(
    const char *line, size_t line_len, const char *field, size_t field_len, const char **value, size_t *value_len) {
  size_t colon;
  size_t key_end;
  size_t key_start;
  size_t value_end;
  size_t value_start;

  key_start = 0U;
  while (key_start < line_len && (line[key_start] == ' ' || line[key_start] == '\t')) ++key_start;
  colon = key_start;
  while (colon < line_len && line[colon] != ':') ++colon;
  if (colon >= line_len) return 0;

  key_end = colon;
  while (key_end > key_start && (line[key_end - 1U] == ' ' || line[key_end - 1U] == '\t')) --key_end;
  if (key_end - key_start != field_len) return 0;
  if (strncmp(line + key_start, field, field_len) != 0) return 0;

  value_start = colon + 1U;
  while (value_start < line_len && (line[value_start] == ' ' || line[value_start] == '\t')) ++value_start;
  value_end = line_len;
  while (value_end > value_start &&
         (line[value_end - 1U] == ' ' || line[value_end - 1U] == '\t' || line[value_end - 1U] == '\r')) {
    --value_end;
  }

  *value = line + value_start;
  *value_len = value_end - value_start;
  return 1;
}

int ftop_linux_cpuinfo_field(const char *field_name, char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
  const char *line;
  const char *line_end;
  const char *value;
  char *cpuinfo;
  size_t copied_len;
  size_t field_len;
  size_t line_len;
  size_t matched_len;
  ssize_t bytes_read;

  if (field_name == NULL || buffer == NULL || value_len == NULL || sys_errno == NULL || buffer_len == 0) return -1;

  buffer[0] = '\0';
  *value_len = 0U;
  *sys_errno = 0;
  field_len = strlen(field_name);
  cpuinfo = ftop_read_file_once("/proc/cpuinfo", FTOP_LINUX_CPUINFO_BUFFER_LEN, &bytes_read, sys_errno);
  if (cpuinfo == NULL) return -1;

  line = cpuinfo;
  while (*line != '\0') {
    line_end = strchr(line, '\n');
    if (line_end == NULL) line_end = line + strlen(line);
    line_len = (size_t)(line_end - line);
    if (ftop_cpuinfo_match_line(line, line_len, field_name, field_len, &value, &matched_len)) {
      copied_len = matched_len < buffer_len - 1U ? matched_len : buffer_len - 1U;
      memcpy(buffer, value, copied_len);
      buffer[copied_len] = '\0';
      *value_len = copied_len;
      free(cpuinfo);
      return 0;
    }
    line = *line_end == '\n' ? line_end + 1 : line_end;
  }

  free(cpuinfo);
  *sys_errno = ENOENT;
  return -1;
}

int ftop_linux_cpuinfo_field_count(const char *field_name, int *count, int *sys_errno) {
  const char *line;
  const char *line_end;
  const char *value;
  char *cpuinfo;
  size_t field_len;
  size_t line_len;
  size_t value_len;
  ssize_t bytes_read;

  if (field_name == NULL || count == NULL || sys_errno == NULL) return -1;

  *count = 0;
  *sys_errno = 0;
  field_len = strlen(field_name);
  cpuinfo = ftop_read_file_once("/proc/cpuinfo", FTOP_LINUX_CPUINFO_BUFFER_LEN, &bytes_read, sys_errno);
  if (cpuinfo == NULL) return -1;

  line = cpuinfo;
  while (*line != '\0') {
    line_end = strchr(line, '\n');
    if (line_end == NULL) line_end = line + strlen(line);
    line_len = (size_t)(line_end - line);
    if (ftop_cpuinfo_match_line(line, line_len, field_name, field_len, &value, &value_len)) ++(*count);
    line = *line_end == '\n' ? line_end + 1 : line_end;
  }

  free(cpuinfo);
  return 0;
}

static void ftop_copy_string(char *destination, size_t destination_len, const char *source) {
  size_t i;

  for (i = 0U; i + 1U < destination_len && source[i] != '\0' && source[i] != '\n'; ++i) destination[i] = source[i];
  destination[i] = '\0';
}

static int ftop_read_hwmon_name(const char *path, char *name, size_t name_len) {
  char buffer[FTOP_LINUX_HWMON_NAME_LEN];
  ssize_t bytes_read;
  int fd;

  fd = open(path, O_RDONLY);
  if (fd < 0) return -1;
  bytes_read = read(fd, buffer, sizeof(buffer) - 1U);
  close(fd);
  if (bytes_read <= 0) return -1;
  buffer[bytes_read] = '\0';
  ftop_copy_string(name, name_len, buffer);
  return 0;
}

int ftop_linux_hwmon_discover(struct ftop_linux_hwmon_sensor *buffer, size_t capacity, size_t *sensor_count, int *sys_errno) {
  char name_path[512];
  char sensor_path[512];
  DIR *directory;
  struct dirent *entry;
  size_t count;
  int written;

  if (buffer == NULL || sensor_count == NULL || sys_errno == NULL || capacity == 0) return -1;

  *sensor_count = 0U;
  *sys_errno = 0;
  directory = opendir("/sys/class/hwmon");
  if (directory == NULL) {
    if (errno == ENOENT) return 0;
    *sys_errno = errno;
    return -1;
  }

  count = 0U;
  while ((entry = readdir(directory)) != NULL) {
    if (strncmp(entry->d_name, "hwmon", 5U) != 0) continue;
    written = snprintf(sensor_path, sizeof(sensor_path), "/sys/class/hwmon/%s", entry->d_name);
    if (written < 0 || (size_t)written >= sizeof(sensor_path)) continue;
    written = snprintf(name_path, sizeof(name_path), "%s/name", sensor_path);
    if (written < 0 || (size_t)written >= sizeof(name_path)) continue;
    if (count < capacity) {
      ftop_copy_string(buffer[count].path, sizeof(buffer[count].path), sensor_path);
      if (ftop_read_hwmon_name(name_path, buffer[count].name, sizeof(buffer[count].name)) != 0) {
        ftop_copy_string(buffer[count].name, sizeof(buffer[count].name), entry->d_name);
      }
    }
    ++count;
  }

  closedir(directory);
  *sensor_count = count < capacity ? count : capacity;
  return 0;
}
