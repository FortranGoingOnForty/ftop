#include <errno.h>
#include <signal.h>
#include <sys/types.h>

static int ftop_test_send_signal(int pid, int signo) {
  if (pid <= 0) return EINVAL;
  if (kill((pid_t)pid, signo) != 0) return errno;
  return 0;
}

int ftop_test_send_continue(int pid) {
  return ftop_test_send_signal(pid, SIGCONT);
}

int ftop_test_send_terminate(int pid) {
  return ftop_test_send_signal(pid, SIGTERM);
}
