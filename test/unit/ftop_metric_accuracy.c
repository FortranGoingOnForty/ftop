#include <errno.h>
#include <ctype.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(FTOP_PLATFORM_freebsd) || defined(FTOP_PLATFORM_macos)
#include <sys/sysctl.h>
#endif

#define FTOP_ACCURACY_PROCESS_CAPACITY 8

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

static int append_command(char *command, size_t capacity, size_t *used, const char *format, ...) {
  int written;
  va_list args;

  if (command == NULL || used == NULL || format == NULL || *used >= capacity) return -1;

  va_start(args, format);
  written = vsnprintf(command + *used, capacity - *used, format, args);
  va_end(args);
  if (written < 0 || (size_t)written >= capacity - *used) return -1;

  *used += (size_t)written;
  return 0;
}

static int line_first_pid_matches(const char *line_start, const char *line_end, int pid) {
  char *parse_end;
  long value;

  if (line_start == NULL || line_end == NULL || pid <= 0) return 0;
  while (line_start < line_end && isspace((unsigned char)*line_start)) ++line_start;
  errno = 0;
  value = strtol(line_start, &parse_end, 10);
  if (parse_end == line_start || errno != 0 || value != (long)pid) return 0;
  return parse_end < line_end && isspace((unsigned char)*parse_end);
}

static int last_process_line(const char *text, int pid, char *line, size_t capacity) {
  const char *cursor;
  const char *line_start;
  int found;

  if (text == NULL || line == NULL || capacity == 0U || pid <= 0) return -1;
  line[0] = '\0';
  cursor = text;
  found = 0;
  while (*cursor != '\0') {
    line_start = cursor;
    while (*cursor != '\0' && *cursor != '\n') ++cursor;
    if (line_first_pid_matches(line_start, cursor, pid)) {
      if (copy_line(line_start, line, capacity) != 0) return -1;
      found = 1;
    }
    if (*cursor == '\n') ++cursor;
  }
  return found ? 0 : -1;
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

static int parse_linux_process_line(const char *line, double *cpu_percent, double *mem_percent) {
  char priority[16];
  char nice[16];
  char res[32];
  char shared[32];
  char state[16];
  char user[64];
  char virt[32];
  int pid;

  if (line == NULL || cpu_percent == NULL || mem_percent == NULL) return -1;
  if (sscanf(line, " %d %63s %15s %15s %31s %31s %31s %15s %lf %lf", &pid, user, priority, nice, virt, res,
             shared, state, cpu_percent, mem_percent) != 10) {
    return -1;
  }
  return 0;
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

static int parse_freebsd_process_line(const char *line, double *cpu_percent, double *mem_percent) {
  char res[32];
  char size[32];
  char state[16];
  char time[32];
  char user[64];
  int cpu_core;
  int nice;
  int pid;
  int priority;
  int threads;
  long long res_bytes;
  long long total_bytes;

  if (line == NULL || cpu_percent == NULL || mem_percent == NULL) return -1;
  if (sscanf(line, " %d %63s %d %d %d %31s %31s %15s %d %31s %lf", &pid, user, &threads, &priority, &nice,
             size, res, state, &cpu_core, time, cpu_percent) != 11) {
    return -1;
  }
  if (scaled_bytes(res, NULL, &res_bytes) != 0) return -1;
  if (freebsd_total_memory(&total_bytes) != 0 || total_bytes <= 0) return -1;
  *mem_percent = 100.0 * (double)res_bytes / (double)total_bytes;
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

static int parse_macos_process_line(const char *line, double *cpu_percent, double *mem_percent) {
  char command[256];
  char mem[32];
  int pid;
  long long mem_bytes;
  long long total_bytes;

  if (line == NULL || cpu_percent == NULL || mem_percent == NULL) return -1;
  if (sscanf(line, " %d %255s %lf %31s", &pid, command, cpu_percent, mem) != 4) return -1;
  if (scaled_bytes(mem, NULL, &mem_bytes) != 0) return -1;
  if (macos_total_memory(&total_bytes) != 0 || total_bytes <= 0) return -1;
  *mem_percent = 100.0 * (double)mem_bytes / (double)total_bytes;
  return 0;
}

static int parse_macos_ps_process_line(const char *line, double *cpu_percent, double *mem_percent) {
  int pid;
  long long rss_kb;
  long long total_bytes;

  if (line == NULL || cpu_percent == NULL || mem_percent == NULL) return -1;
  if (sscanf(line, " %d %lf %lld", &pid, cpu_percent, &rss_kb) != 3) return -1;
  if (rss_kb < 0) return -1;
  if (macos_total_memory(&total_bytes) != 0 || total_bytes <= 0) return -1;
  *mem_percent = 100.0 * (double)(rss_kb * 1024LL) / (double)total_bytes;
  return 0;
}
#endif

static int build_processes_command(const int *pids, int count, char *command, size_t capacity) {
  int index;
  size_t used;

  if (pids == NULL || command == NULL || capacity == 0U || count <= 0 || count > FTOP_ACCURACY_PROCESS_CAPACITY) {
    return -1;
  }
  for (index = 0; index < count; ++index) {
    if (pids[index] <= 0) return -1;
  }

  command[0] = '\0';
  used = 0U;
#if defined(FTOP_PLATFORM_linux)
  if (append_command(command, capacity, &used, "top -bn2 -d 1 -p ") != 0) return -1;
  for (index = 0; index < count; ++index) {
    if (append_command(command, capacity, &used, "%s%d", index == 0 ? "" : ",", pids[index]) != 0) return -1;
  }
#elif defined(FTOP_PLATFORM_freebsd)
  (void)pids;
  if (append_command(command, capacity, &used, "top -b -d 2 -s 1 -o cpu 128") != 0) return -1;
#elif defined(FTOP_PLATFORM_macos)
  if (append_command(command, capacity, &used, "sleep 1; ps -p ") != 0) return -1;
  for (index = 0; index < count; ++index) {
    if (append_command(command, capacity, &used, "%s%d", index == 0 ? "" : ",", pids[index]) != 0) return -1;
  }
  if (append_command(command, capacity, &used, " -o pid=,pcpu=,rss=") != 0) return -1;
#else
  return -1;
#endif
  return 0;
}

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

int ftop_accuracy_reference_process(int pid, double *cpu_percent, double *mem_percent, int *sys_errno) {
  char command[128];
  char line[512];
  char output[65536];
  int rc;

  if (pid <= 0 || cpu_percent == NULL || mem_percent == NULL || sys_errno == NULL) return -1;
  *cpu_percent = 0.0;
  *mem_percent = 0.0;
  *sys_errno = 0;
#if defined(FTOP_PLATFORM_linux)
  rc = snprintf(command, sizeof(command), "top -bn2 -d 1 -p %d", pid);
#elif defined(FTOP_PLATFORM_freebsd)
  rc = snprintf(command, sizeof(command), "top -b -d 2 -s 1 -p %d", pid);
#elif defined(FTOP_PLATFORM_macos)
  rc = snprintf(command, sizeof(command), "top -l 2 -s 1 -pid %d -stats pid,command,cpu,mem", pid);
#else
  rc = -1;
#endif
  if (rc <= 0 || (size_t)rc >= sizeof(command)) {
    *sys_errno = EINVAL;
    return -1;
  }

  rc = read_command_output(command, output, sizeof(output));
  if (rc == 0) rc = last_process_line(output, pid, line, sizeof(line));
#if defined(FTOP_PLATFORM_linux)
  if (rc == 0) rc = parse_linux_process_line(line, cpu_percent, mem_percent);
#elif defined(FTOP_PLATFORM_freebsd)
  if (rc == 0) rc = parse_freebsd_process_line(line, cpu_percent, mem_percent);
#elif defined(FTOP_PLATFORM_macos)
  if (rc == 0) rc = parse_macos_process_line(line, cpu_percent, mem_percent);
#else
  rc = -1;
#endif
  if (rc != 0) *sys_errno = errno != 0 ? errno : EINVAL;
  return rc;
}

int ftop_accuracy_reference_processes(const int *pids, int count, double *cpu_percents, double *mem_percents,
                                      int *matched_count, int *sys_errno) {
  char command[512];
  char line[512];
  char output[131072];
  int index;
  int rc;

  if (matched_count != NULL) *matched_count = 0;
  if (sys_errno != NULL) *sys_errno = 0;
  if (pids == NULL || cpu_percents == NULL || mem_percents == NULL || matched_count == NULL || sys_errno == NULL ||
      count <= 0 || count > FTOP_ACCURACY_PROCESS_CAPACITY) {
    if (sys_errno != NULL) *sys_errno = EINVAL;
    return -1;
  }

  for (index = 0; index < count; ++index) {
    cpu_percents[index] = 0.0;
    mem_percents[index] = 0.0;
  }

  rc = build_processes_command(pids, count, command, sizeof(command));
  if (rc != 0) {
    *sys_errno = EINVAL;
    return -1;
  }

  rc = read_command_output(command, output, sizeof(output));
  if (rc != 0) {
    *sys_errno = errno != 0 ? errno : EINVAL;
    return -1;
  }

  for (index = 0; index < count; ++index) {
    rc = last_process_line(output, pids[index], line, sizeof(line));
#if defined(FTOP_PLATFORM_linux)
    if (rc == 0) rc = parse_linux_process_line(line, &cpu_percents[index], &mem_percents[index]);
#elif defined(FTOP_PLATFORM_freebsd)
    if (rc == 0) rc = parse_freebsd_process_line(line, &cpu_percents[index], &mem_percents[index]);
#elif defined(FTOP_PLATFORM_macos)
    if (rc == 0) rc = parse_macos_ps_process_line(line, &cpu_percents[index], &mem_percents[index]);
#else
    rc = -1;
#endif
    if (rc != 0) {
      *sys_errno = errno != 0 ? errno : EINVAL;
      return -1;
    }
    *matched_count += 1;
  }

  return 0;
}
