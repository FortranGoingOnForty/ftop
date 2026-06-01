#include <errno.h>
#include <ctype.h>
#include <dirent.h>
#include <fcntl.h>
#include <pwd.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define FTOP_LINUX_CPUINFO_BUFFER_LEN 1048576U
#define FTOP_LINUX_HWMON_NAME_LEN 128
#define FTOP_LINUX_HWMON_PATH_LEN 256
#define FTOP_LINUX_PROCESS_COMMAND_LEN 256
#define FTOP_LINUX_PROCESS_STAT_LEN 512
#define FTOP_LINUX_PROCESS_STATUS_LEN 2048
#define FTOP_USER_LOOKUP_BUFFER_LEN 16384

struct ftop_linux_hwmon_sensor {
  char path[FTOP_LINUX_HWMON_PATH_LEN];
  char name[FTOP_LINUX_HWMON_NAME_LEN];
};

struct ftop_linux_process_raw {
  int pid;
  char stat[FTOP_LINUX_PROCESS_STAT_LEN];
  size_t stat_len;
  char status[FTOP_LINUX_PROCESS_STATUS_LEN];
  size_t status_len;
  char cmdline[FTOP_LINUX_PROCESS_COMMAND_LEN];
  size_t cmdline_len;
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

int ftop_linux_read_proc_loadavg(char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
  return ftop_read_file_into_buffer("/proc/loadavg", buffer, buffer_len, value_len, sys_errno);
}

int ftop_linux_read_proc_uptime(char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
  return ftop_read_file_into_buffer("/proc/uptime", buffer, buffer_len, value_len, sys_errno);
}

static int ftop_linux_pid_from_name(const char *name, int *pid) {
  char *end;
  long value;

  if (name == NULL || pid == NULL || !isdigit((unsigned char)name[0])) return 0;
  errno = 0;
  value = strtol(name, &end, 10);
  if (end == name || *end != '\0' || errno != 0 || value <= 0 || value > 2147483647L) return 0;
  *pid = (int)value;
  return 1;
}

static int ftop_linux_read_process_file(
    int pid, const char *name, char *buffer, size_t buffer_len, size_t *value_len) {
  char path[128];
  int read_errno;
  int written;

  written = snprintf(path, sizeof(path), "/proc/%d/%s", pid, name);
  if (written < 0 || (size_t)written >= sizeof(path)) return -1;
  read_errno = 0;
  return ftop_read_file_into_buffer(path, buffer, buffer_len, value_len, &read_errno);
}

int ftop_linux_process_snapshot(
    struct ftop_linux_process_raw *buffer, size_t capacity, size_t *process_count, int *sys_errno) {
  DIR *directory;
  struct dirent *entry;
  struct ftop_linux_process_raw process;
  int pid;
  size_t count;

  if (buffer == NULL || process_count == NULL || sys_errno == NULL || capacity == 0) return -1;

  *process_count = 0U;
  *sys_errno = 0;
  directory = opendir("/proc");
  if (directory == NULL) {
    *sys_errno = errno;
    return -1;
  }

  count = 0U;
  while ((entry = readdir(directory)) != NULL && count < capacity) {
    if (!ftop_linux_pid_from_name(entry->d_name, &pid)) continue;
    memset(&process, 0, sizeof(process));
    process.pid = pid;
    if (ftop_linux_read_process_file(pid, "stat", process.stat, sizeof(process.stat), &process.stat_len) != 0) continue;
    (void)ftop_linux_read_process_file(pid, "status", process.status, sizeof(process.status), &process.status_len);
    (void)ftop_linux_read_process_file(pid, "cmdline", process.cmdline, sizeof(process.cmdline), &process.cmdline_len);
    buffer[count] = process;
    ++count;
  }

  closedir(directory);
  *process_count = count;
  return 0;
}

int ftop_linux_page_size(long long *page_size, int *sys_errno) {
  long value;

  if (page_size == NULL || sys_errno == NULL) return -1;

  *page_size = 0;
  *sys_errno = 0;
  value = sysconf(_SC_PAGESIZE);
  if (value <= 0L) {
    *sys_errno = errno != 0 ? errno : EINVAL;
    return -1;
  }

  *page_size = (long long)value;
  return 0;
}

int ftop_linux_user_name(int uid, char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
  char scratch[FTOP_USER_LOOKUP_BUFFER_LEN];
  struct passwd password;
  struct passwd *result;
  size_t copied_len;
  int rc;

  if (buffer == NULL || value_len == NULL || sys_errno == NULL || buffer_len == 0) return -1;

  buffer[0] = '\0';
  *value_len = 0U;
  *sys_errno = 0;
  if (uid < 0) {
    *sys_errno = EINVAL;
    return -1;
  }

  memset(&password, 0, sizeof(password));
  result = NULL;
  rc = getpwuid_r((uid_t)uid, &password, scratch, sizeof(scratch), &result);
  if (rc != 0) {
    *sys_errno = rc;
    return -1;
  }
  if (result == NULL || password.pw_name == NULL) {
    *sys_errno = ENOENT;
    return -1;
  }

  copied_len = strlen(password.pw_name);
  if (copied_len >= buffer_len) copied_len = buffer_len - 1U;
  memcpy(buffer, password.pw_name, copied_len);
  buffer[copied_len] = '\0';
  *value_len = copied_len;
  return 0;
}

int ftop_linux_read_cpu_frequency(int cpu_index, char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
  char path[128];
  int written;
  int rc;

  if (cpu_index < 0) {
    if (sys_errno != NULL) *sys_errno = EINVAL;
    return -1;
  }

  written = snprintf(path, sizeof(path), "/sys/devices/system/cpu/cpu%d/cpufreq/scaling_cur_freq", cpu_index);
  if (written < 0 || (size_t)written >= sizeof(path)) {
    if (sys_errno != NULL) *sys_errno = EOVERFLOW;
    return -1;
  }
  rc = ftop_read_file_into_buffer(path, buffer, buffer_len, value_len, sys_errno);
  if (rc == 0) return 0;

  written = snprintf(path, sizeof(path), "/sys/devices/system/cpu/cpu%d/cpufreq/cpuinfo_cur_freq", cpu_index);
  if (written < 0 || (size_t)written >= sizeof(path)) {
    if (sys_errno != NULL) *sys_errno = EOVERFLOW;
    return -1;
  }
  return ftop_read_file_into_buffer(path, buffer, buffer_len, value_len, sys_errno);
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

static int ftop_linux_is_cpu_hwmon_name(const char *name) {
  return strcmp(name, "coretemp") == 0 || strcmp(name, "k10temp") == 0 || strcmp(name, "zenpower") == 0 ||
         strcmp(name, "cpu_thermal") == 0 || strcmp(name, "soc_thermal") == 0;
}

static int ftop_linux_is_temp_input_name(const char *name) {
  size_t len;

  if (strncmp(name, "temp", 4U) != 0) return 0;
  len = strlen(name);
  if (len <= 10U) return 0;
  return strcmp(name + len - 6U, "_input") == 0;
}

static int ftop_linux_read_temperature_millicelsius(const char *path, long long *temperature) {
  char buffer[64];
  char *end;
  size_t value_len;
  long long value;
  int sys_errno;

  if (ftop_read_file_into_buffer(path, buffer, sizeof(buffer), &value_len, &sys_errno) != 0) return -1;
  errno = 0;
  value = strtoll(buffer, &end, 10);
  if (end == buffer || errno != 0) return -1;
  if (value < -100000LL || value > 150000LL) return -1;

  *temperature = value;
  return 0;
}

static int ftop_linux_scan_cpu_hwmon_temperatures(const char *sensor_path, long long *max_temperature) {
  char input_path[512];
  DIR *directory;
  struct dirent *entry;
  long long temperature;
  int found;
  int written;

  directory = opendir(sensor_path);
  if (directory == NULL) return 0;

  found = 0;
  while ((entry = readdir(directory)) != NULL) {
    if (!ftop_linux_is_temp_input_name(entry->d_name)) continue;
    written = snprintf(input_path, sizeof(input_path), "%s/%s", sensor_path, entry->d_name);
    if (written < 0 || (size_t)written >= sizeof(input_path)) continue;
    if (ftop_linux_read_temperature_millicelsius(input_path, &temperature) != 0) continue;
    if (!found || temperature > *max_temperature) *max_temperature = temperature;
    found = 1;
  }

  closedir(directory);
  return found;
}

int ftop_linux_read_cpu_temperature(double *temperature_c, int *sys_errno) {
  char name[FTOP_LINUX_HWMON_NAME_LEN];
  char name_path[512];
  char sensor_path[512];
  DIR *directory;
  struct dirent *entry;
  long long max_temperature;
  int found;
  int written;

  if (temperature_c == NULL || sys_errno == NULL) return -1;

  *temperature_c = 0.0;
  *sys_errno = 0;
  directory = opendir("/sys/class/hwmon");
  if (directory == NULL) {
    *sys_errno = errno;
    return -1;
  }

  found = 0;
  max_temperature = 0;
  while ((entry = readdir(directory)) != NULL) {
    if (strncmp(entry->d_name, "hwmon", 5U) != 0) continue;
    written = snprintf(sensor_path, sizeof(sensor_path), "/sys/class/hwmon/%s", entry->d_name);
    if (written < 0 || (size_t)written >= sizeof(sensor_path)) continue;
    written = snprintf(name_path, sizeof(name_path), "%s/name", sensor_path);
    if (written < 0 || (size_t)written >= sizeof(name_path)) continue;
    if (ftop_read_hwmon_name(name_path, name, sizeof(name)) != 0) continue;
    if (!ftop_linux_is_cpu_hwmon_name(name)) continue;
    if (ftop_linux_scan_cpu_hwmon_temperatures(sensor_path, &max_temperature)) found = 1;
  }

  closedir(directory);
  if (!found) {
    *sys_errno = ENOENT;
    return -1;
  }

  *temperature_c = (double)max_temperature / 1000.0;
  return 0;
}
