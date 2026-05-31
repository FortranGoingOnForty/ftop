#include <errno.h>
#include <poll.h>
#include <signal.h>
#include <stddef.h>
#include <unistd.h>

enum {
  FTOP_SIGNAL_WINCH = 1,
  FTOP_SIGNAL_INT = 2,
  FTOP_SIGNAL_TERM = 3,
  FTOP_SIGNAL_TSTP = 4,
  FTOP_SIGNAL_CONT = 5
};

static volatile sig_atomic_t ftop_winch_pending = 0;
static volatile sig_atomic_t ftop_int_pending = 0;
static volatile sig_atomic_t ftop_term_pending = 0;
static volatile sig_atomic_t ftop_tstp_pending = 0;
static volatile sig_atomic_t ftop_cont_pending = 0;

static void ftop_signal_handler(int signo) {
  switch (signo) {
  case SIGWINCH:
    ftop_winch_pending = 1;
    break;
  case SIGINT:
    ftop_int_pending = 1;
    break;
  case SIGTERM:
    ftop_term_pending = 1;
    break;
  case SIGTSTP:
    ftop_tstp_pending = 1;
    break;
  case SIGCONT:
    ftop_cont_pending = 1;
    break;
  default:
    break;
  }
}

static int ftop_install_signal(int signo) {
  struct sigaction action;

  action.sa_handler = ftop_signal_handler;
  sigemptyset(&action.sa_mask);
  action.sa_flags = 0;
  return sigaction(signo, &action, NULL);
}

int ftop_signal_setup(void) {
  if (ftop_install_signal(SIGWINCH) != 0) return -1;
  if (ftop_install_signal(SIGINT) != 0) return -1;
  if (ftop_install_signal(SIGTERM) != 0) return -1;
  if (ftop_install_signal(SIGTSTP) != 0) return -1;
  if (ftop_install_signal(SIGCONT) != 0) return -1;
  return 0;
}

int ftop_signal_pending(int signal_id) {
  switch (signal_id) {
  case FTOP_SIGNAL_WINCH:
    return ftop_winch_pending != 0;
  case FTOP_SIGNAL_INT:
    return ftop_int_pending != 0;
  case FTOP_SIGNAL_TERM:
    return ftop_term_pending != 0;
  case FTOP_SIGNAL_TSTP:
    return ftop_tstp_pending != 0;
  case FTOP_SIGNAL_CONT:
    return ftop_cont_pending != 0;
  default:
    return 0;
  }
}

void ftop_signal_clear(int signal_id) {
  switch (signal_id) {
  case FTOP_SIGNAL_WINCH:
    ftop_winch_pending = 0;
    break;
  case FTOP_SIGNAL_INT:
    ftop_int_pending = 0;
    break;
  case FTOP_SIGNAL_TERM:
    ftop_term_pending = 0;
    break;
  case FTOP_SIGNAL_TSTP:
    ftop_tstp_pending = 0;
    break;
  case FTOP_SIGNAL_CONT:
    ftop_cont_pending = 0;
    break;
  default:
    break;
  }
}

int ftop_signal_suspend_self(void) {
  struct sigaction action;

  action.sa_handler = SIG_DFL;
  sigemptyset(&action.sa_mask);
  action.sa_flags = 0;
  if (sigaction(SIGTSTP, &action, NULL) != 0) return -1;
  if (raise(SIGTSTP) != 0) return -1;
  return ftop_install_signal(SIGTSTP);
}

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
