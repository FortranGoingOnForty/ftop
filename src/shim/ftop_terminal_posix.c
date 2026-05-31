#include <errno.h>
#include <poll.h>
#include <stddef.h>
#include <unistd.h>

int ftop_poll_stdin(int timeout_ms, int *sys_errno) {
  struct pollfd input;
  int rc;

  if (sys_errno != NULL) *sys_errno = 0;
  input.fd = STDIN_FILENO;
  input.events = POLLIN;
  input.revents = 0;

  rc = poll(&input, 1, timeout_ms);
  if (rc > 0) {
    if ((input.revents & POLLIN) != 0) return 1;
    if (sys_errno != NULL) *sys_errno = EIO;
    return -1;
  }
  if (rc == 0) return 0;
  if (errno == EINTR) {
    if (sys_errno != NULL) *sys_errno = errno;
    return -2;
  }
  if (sys_errno != NULL) *sys_errno = errno;
  return -1;
}

int ftop_read_stdin(char *buffer, int buffer_len, int *bytes_read, int *sys_errno) {
  ssize_t nread;

  if (bytes_read != NULL) *bytes_read = 0;
  if (sys_errno != NULL) *sys_errno = 0;
  if (buffer == NULL || buffer_len <= 0) {
    if (sys_errno != NULL) *sys_errno = EINVAL;
    return -1;
  }

  nread = read(STDIN_FILENO, buffer, (size_t)buffer_len);
  if (nread >= 0) {
    if (bytes_read != NULL) *bytes_read = (int)nread;
    return 0;
  }
  if (errno == EINTR) {
    if (sys_errno != NULL) *sys_errno = errno;
    return -2;
  }
  if (sys_errno != NULL) *sys_errno = errno;
  return -1;
}

int ftop_write_stdout(const char *buffer, int buffer_len, int *sys_errno) {
  const char *cursor;
  size_t remaining;

  if (sys_errno != NULL) *sys_errno = 0;
  if (buffer == NULL || buffer_len < 0) {
    if (sys_errno != NULL) *sys_errno = EINVAL;
    return -1;
  }

  cursor = buffer;
  remaining = (size_t)buffer_len;
  while (remaining > 0) {
    ssize_t nwritten = write(STDOUT_FILENO, cursor, remaining);
    if (nwritten < 0) {
      if (errno == EINTR) continue;
      if (sys_errno != NULL) *sys_errno = errno;
      return -1;
    }
    cursor += nwritten;
    remaining -= (size_t)nwritten;
  }

  return 0;
}
