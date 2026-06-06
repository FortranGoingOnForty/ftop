#define _POSIX_C_SOURCE 200809L

#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

enum {
  FTOP_LOG_ERROR = 0,
  FTOP_LOG_WARN = 1,
  FTOP_LOG_INFO = 2,
  FTOP_LOG_DEBUG = 3
};

static pthread_mutex_t ftop_log_mutex;
static int ftop_log_mutex_ready = 0;
static int ftop_log_fd = -1;
static int ftop_saved_stderr = -1;
static int ftop_log_level = FTOP_LOG_INFO;
static struct timespec ftop_log_start;
static int ftop_log_start_valid = 0;

void ftop_log_close(void);

static void ftop_set_error(int *sys_errno, int code) {
  if (sys_errno != NULL) *sys_errno = code;
}

static int ftop_clamped_log_level(int level) {
  if (level < FTOP_LOG_ERROR || level > FTOP_LOG_DEBUG) return FTOP_LOG_INFO;
  return level;
}

static const char *ftop_log_level_name(int level) {
  switch (level) {
  case FTOP_LOG_ERROR:
    return "ERROR";
  case FTOP_LOG_WARN:
    return "WARN ";
  case FTOP_LOG_DEBUG:
    return "DEBUG";
  case FTOP_LOG_INFO:
  default:
    return "INFO ";
  }
}

static void ftop_write_all(int fd, const char *buffer, size_t length) {
  const char *cursor = buffer;
  size_t remaining = length;

  while (remaining > 0) {
    ssize_t written = write(fd, cursor, remaining);
    if (written < 0) {
      if (errno == EINTR) continue;
      return;
    }
    if (written == 0) return;
    cursor += written;
    remaining -= (size_t)written;
  }
}

static void ftop_elapsed_time(long long *seconds, long *milliseconds) {
  struct timespec now;
  time_t elapsed_seconds;
  long elapsed_nanoseconds;

  *seconds = 0;
  *milliseconds = 0;
  if (!ftop_log_start_valid) return;
  if (clock_gettime(CLOCK_MONOTONIC, &now) != 0) return;

  elapsed_seconds = now.tv_sec - ftop_log_start.tv_sec;
  elapsed_nanoseconds = now.tv_nsec - ftop_log_start.tv_nsec;
  if (elapsed_nanoseconds < 0) {
    elapsed_seconds -= 1;
    elapsed_nanoseconds += 1000000000L;
  }
  if (elapsed_seconds < 0) return;

  *seconds = (long long)elapsed_seconds;
  *milliseconds = elapsed_nanoseconds / 1000000L;
}

int ftop_log_open(const char *path, int level, int *sys_errno) {
  int fd;
  int saved_stderr;
  int saved_errno;
  int rc;

  ftop_set_error(sys_errno, 0);
  if (path == NULL || path[0] == '\0') {
    ftop_set_error(sys_errno, EINVAL);
    return -1;
  }

  if (ftop_log_mutex_ready || ftop_log_fd >= 0 || ftop_saved_stderr >= 0) ftop_log_close();

  rc = pthread_mutex_init(&ftop_log_mutex, NULL);
  if (rc != 0) {
    ftop_set_error(sys_errno, rc);
    return -1;
  }
  ftop_log_mutex_ready = 1;

  saved_stderr = dup(STDERR_FILENO);
  if (saved_stderr < 0) {
    saved_errno = errno;
    (void)pthread_mutex_destroy(&ftop_log_mutex);
    ftop_log_mutex_ready = 0;
    ftop_set_error(sys_errno, saved_errno);
    return -1;
  }

  fd = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0644);
  if (fd < 0) {
    saved_errno = errno;
    (void)close(saved_stderr);
    (void)pthread_mutex_destroy(&ftop_log_mutex);
    ftop_log_mutex_ready = 0;
    ftop_set_error(sys_errno, saved_errno);
    return -1;
  }

  if (dup2(fd, STDERR_FILENO) < 0) {
    saved_errno = errno;
    (void)close(saved_stderr);
    (void)close(fd);
    (void)pthread_mutex_destroy(&ftop_log_mutex);
    ftop_log_mutex_ready = 0;
    ftop_set_error(sys_errno, saved_errno);
    return -1;
  }

  ftop_log_fd = fd;
  ftop_saved_stderr = saved_stderr;
  ftop_log_level = ftop_clamped_log_level(level);
  ftop_log_start_valid = (clock_gettime(CLOCK_MONOTONIC, &ftop_log_start) == 0);
  return 0;
}

void ftop_log_close(void) {
  if (!ftop_log_mutex_ready) return;

  (void)pthread_mutex_lock(&ftop_log_mutex);
  if (ftop_saved_stderr >= 0) {
    (void)dup2(ftop_saved_stderr, STDERR_FILENO);
    (void)close(ftop_saved_stderr);
    ftop_saved_stderr = -1;
  }
  if (ftop_log_fd >= 0) {
    (void)close(ftop_log_fd);
    ftop_log_fd = -1;
  }
  ftop_log_start_valid = 0;
  (void)pthread_mutex_unlock(&ftop_log_mutex);

  (void)pthread_mutex_destroy(&ftop_log_mutex);
  ftop_log_mutex_ready = 0;
}

void ftop_log_write(int level, const char *message) {
  char header[64];
  int header_length;
  long long elapsed_seconds;
  long elapsed_milliseconds;

  if (!ftop_log_mutex_ready) return;
  if (level < FTOP_LOG_ERROR || level > FTOP_LOG_DEBUG) return;
  if (message == NULL) message = "";

  (void)pthread_mutex_lock(&ftop_log_mutex);
  if (ftop_log_fd >= 0 && level <= ftop_log_level) {
    ftop_elapsed_time(&elapsed_seconds, &elapsed_milliseconds);
    header_length = snprintf(header, sizeof(header), "[%s] [%3lld.%03ld] ",
                             ftop_log_level_name(level), elapsed_seconds, elapsed_milliseconds);
    if (header_length > 0) {
      if ((size_t)header_length >= sizeof(header)) header_length = (int)sizeof(header) - 1;
      ftop_write_all(ftop_log_fd, header, (size_t)header_length);
      ftop_write_all(ftop_log_fd, message, strlen(message));
      ftop_write_all(ftop_log_fd, "\n", 1);
    }
  }
  (void)pthread_mutex_unlock(&ftop_log_mutex);
}
