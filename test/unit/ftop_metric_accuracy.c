#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(FTOP_PLATFORM_freebsd) || defined(FTOP_PLATFORM_macos)
#include <sys/sysctl.h>
#endif

static int read_command_output(const char *command, char *buffer, size_t capacity) {
  FILE *pipe;
  size_t used;
  size_t bytes_read;

  if (command == NULL || buffer == NULL || capacity < 2U) return -1;
  buffer[0] = '\0';
  pipe = popen(command, "r");
  if (pipe == NULL) return -1;

  used = 0U;
  while (used + 1U < capacity) {
    bytes_read = fread(buffer + used, 1U, capacity - used - 1U, pipe);
    used += bytes_read;
    if (bytes_read == 0U) break;
  }
  buffer[used] = '\0';
  if (pclose(pipe) < 0) return -1;
  return used > 0U ? 0 : -1;
}

static const char *last_matching_line(const char *text, const char *needle) {
  const char *cursor;
  const char *match;
  const char *line_start;

  match = NULL;
  cursor = text;
  while ((cursor = strstr(cursor, needle)) != NULL) {
    line_start = cursor;
    while (line_start > text && line_start[-1] != '\n') --line_start;
    match = line_start;
    ++cursor;
  }
  return match;
}

static int copy_line(const char *line_start, char *line, size_t capacity) {
  size_t len;

  if (line_start == NULL || line == NULL || capacity == 0U) return -1;
  len = 0U;
  while (line_start[len] != '\0' && line_start[len] != '\n' && len + 1U < capacity) {
    line[len] = line_start[len];
    ++len;
  }
  line[len] = '\0';
  return len > 0U ? 0 : -1;
}

static int scaled_bytes(const char *text, const char **end, long long *bytes) {
  char *parse_end;
  double value;
  double scale;

  if (text == NULL || bytes == NULL) return -1;
  errno = 0;
  value = strtod(text, &parse_end);
  if (parse_end == text || errno != 0 || value < 0.0) return -1;

  scale = 1.0;
  if (*parse_end == 'K' || *parse_end == 'k') {
    scale = 1024.0;
    ++parse_end;
  } else if (*parse_end == 'M' || *parse_end == 'm') {
    scale = 1024.0 * 1024.0;
    ++parse_end;
  } else if (*parse_end == 'G' || *parse_end == 'g') {
    scale = 1024.0 * 1024.0 * 1024.0;
    ++parse_end;
  } else if (*parse_end == 'T' || *parse_end == 't') {
    scale = 1024.0 * 1024.0 * 1024.0 * 1024.0;
    ++parse_end;
  }

  if (value > (double)9223372036854775807LL / scale) return -1;
  *bytes = (long long)llround(value * scale);
  if (end != NULL) *end = parse_end;
  return 0;
}

#if defined(FTOP_PLATFORM_linux)
static int parse_linux_cpu(const char *output, double *usage_percent) {
  char line[512];
  double user;
  double system;
  double nice;
  double idle;

  if (copy_line(last_matching_line(output, "%Cpu"), line, sizeof(line)) != 0) return -1;
  if (sscanf(line, "%%Cpu(s): %lf us, %lf sy, %lf ni, %lf id", &user, &system, &nice, &idle) != 4) return -1;
  if (idle < 0.0 || idle > 100.0) return -1;
  *usage_percent = 100.0 - idle;
  return 0;
}

static int parse_linux_free(const char *output, long long *total_bytes, long long *used_bytes) {
  const char *line;

  line = last_matching_line(output, "Mem:");
  if (line == NULL) return -1;
  return sscanf(line, "Mem: %lld %lld", total_bytes, used_bytes) == 2 ? 0 : -1;
}
#endif

#if defined(FTOP_PLATFORM_freebsd)
static int parse_freebsd_cpu(const char *output, double *usage_percent) {
  char line[512];
  double user;
  double nice;
  double system;
  double interrupt;
  double idle;

  if (copy_line(last_matching_line(output, "CPU:"), line, sizeof(line)) != 0) return -1;
  if (sscanf(line, "CPU: %lf%% user, %lf%% nice, %lf%% system, %lf%% interrupt, %lf%% idle", &user, &nice, &system,
             &interrupt, &idle) != 5) {
    return -1;
  }
  if (idle < 0.0 || idle > 100.0) return -1;
  *usage_percent = 100.0 - idle;
  return 0;
}

static int freebsd_total_memory(long long *total_bytes) {
  unsigned long value;
  size_t value_len;

  value = 0UL;
  value_len = sizeof(value);
  if (sysctlbyname("hw.physmem", &value, &value_len, NULL, 0) != 0) return -1;
  *total_bytes = (long long)value;
  return *total_bytes > 0 ? 0 : -1;
}

static int parse_freebsd_memory(const char *output, long long *total_bytes, long long *used_bytes) {
  char line[512];
  char *token;
  char *saveptr;
  long long active;
  long long wired;
  long long value;
  char *label;

  if (copy_line(last_matching_line(output, "Mem:"), line, sizeof(line)) != 0) return -1;
  active = 0;
  wired = 0;
  token = strtok_r(line, " ,", &saveptr);
  while (token != NULL) {
    if (strcmp(token, "Mem:") != 0 && scaled_bytes(token, NULL, &value) == 0) {
      label = strtok_r(NULL, " ,", &saveptr);
      if (label == NULL) break;
      if (strcmp(label, "Active") == 0) active = value;
      if (strcmp(label, "Wired") == 0) wired = value;
    }
    token = strtok_r(NULL, " ,", &saveptr);
  }

  if (active <= 0 || wired <= 0) return -1;
  if (freebsd_total_memory(total_bytes) != 0) return -1;
  *used_bytes = active + wired;
  return 0;
}
#endif

#if defined(FTOP_PLATFORM_macos)
static int parse_macos_cpu(const char *output, double *usage_percent) {
  char line[512];
  double user;
  double system;
  double idle;

  if (copy_line(last_matching_line(output, "CPU usage:"), line, sizeof(line)) != 0) return -1;
  if (sscanf(line, "CPU usage: %lf%% user, %lf%% sys, %lf%% idle", &user, &system, &idle) != 3) return -1;
  if (idle < 0.0 || idle > 100.0) return -1;
  *usage_percent = 100.0 - idle;
  return 0;
}

static int macos_total_memory(long long *total_bytes) {
  unsigned long long value;
  size_t value_len;

  value = 0ULL;
  value_len = sizeof(value);
  if (sysctlbyname("hw.memsize", &value, &value_len, NULL, 0) != 0) return -1;
  *total_bytes = (long long)value;
  return *total_bytes > 0 ? 0 : -1;
}

static int parse_macos_memory(const char *output, long long *total_bytes, long long *used_bytes) {
  const char *cursor;

  cursor = last_matching_line(output, "PhysMem:");
  if (cursor == NULL) return -1;
  cursor = strstr(cursor, "PhysMem:");
  if (cursor == NULL) return -1;
  cursor += strlen("PhysMem:");
  while (*cursor == ' ') ++cursor;
  if (scaled_bytes(cursor, NULL, used_bytes) != 0) return -1;
  return macos_total_memory(total_bytes);
}
#endif

int ftop_accuracy_reference_cpu(double *usage_percent, int *sys_errno) {
  char output[65536];
  int rc;

  if (usage_percent == NULL || sys_errno == NULL) return -1;
  *usage_percent = 0.0;
  *sys_errno = 0;
#if defined(FTOP_PLATFORM_linux)
  rc = read_command_output("top -bn2 -d 1", output, sizeof(output));
  if (rc == 0) rc = parse_linux_cpu(output, usage_percent);
#elif defined(FTOP_PLATFORM_freebsd)
  rc = read_command_output("top -b -d 2 -s 1", output, sizeof(output));
  if (rc == 0) rc = parse_freebsd_cpu(output, usage_percent);
#elif defined(FTOP_PLATFORM_macos)
  rc = read_command_output("top -l 2 -s 1 -n 0", output, sizeof(output));
  if (rc == 0) rc = parse_macos_cpu(output, usage_percent);
#else
  rc = -1;
#endif
  if (rc != 0) *sys_errno = errno != 0 ? errno : EINVAL;
  return rc;
}

int ftop_accuracy_reference_memory(long long *total_bytes, long long *used_bytes, int *sys_errno) {
  char output[65536];
  int rc;

  if (total_bytes == NULL || used_bytes == NULL || sys_errno == NULL) return -1;
  *total_bytes = 0;
  *used_bytes = 0;
  *sys_errno = 0;
#if defined(FTOP_PLATFORM_linux)
  rc = read_command_output("free -b", output, sizeof(output));
  if (rc == 0) rc = parse_linux_free(output, total_bytes, used_bytes);
#elif defined(FTOP_PLATFORM_freebsd)
  rc = read_command_output("top -b -d 1", output, sizeof(output));
  if (rc == 0) rc = parse_freebsd_memory(output, total_bytes, used_bytes);
#elif defined(FTOP_PLATFORM_macos)
  rc = read_command_output("top -l 1 -n 0", output, sizeof(output));
  if (rc == 0) rc = parse_macos_memory(output, total_bytes, used_bytes);
#else
  rc = -1;
#endif
  if (rc != 0) *sys_errno = errno != 0 ? errno : EINVAL;
  return rc;
}
