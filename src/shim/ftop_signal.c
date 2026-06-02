#include <errno.h>
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>

enum {
  FTOP_SIGNAL_WINCH = 1,
  FTOP_SIGNAL_INT = 2,
  FTOP_SIGNAL_TERM = 3,
  FTOP_SIGNAL_TSTP = 4,
  FTOP_SIGNAL_CONT = 5,
  FTOP_SIGNAL_HUP = 6,
  FTOP_SIGNAL_KILL = 7,
  FTOP_SIGNAL_STOP = 8,
  FTOP_SIGNAL_USR1 = 9,
  FTOP_SIGNAL_USR2 = 10
};

static volatile sig_atomic_t ftop_winch_pending = 0;
static volatile sig_atomic_t ftop_int_pending = 0;
static volatile sig_atomic_t ftop_term_pending = 0;
static volatile sig_atomic_t ftop_tstp_pending = 0;
static volatile sig_atomic_t ftop_cont_pending = 0;

static void ftop_set_error(int *sys_errno, int code) {
  if (sys_errno != NULL) *sys_errno = code;
}

static int ftop_signal_number_impl(int signal_id) {
  switch (signal_id) {
  case FTOP_SIGNAL_WINCH:
    return SIGWINCH;
  case FTOP_SIGNAL_INT:
    return SIGINT;
  case FTOP_SIGNAL_TERM:
    return SIGTERM;
  case FTOP_SIGNAL_TSTP:
    return SIGTSTP;
  case FTOP_SIGNAL_CONT:
    return SIGCONT;
  case FTOP_SIGNAL_HUP:
    return SIGHUP;
  case FTOP_SIGNAL_KILL:
    return SIGKILL;
  case FTOP_SIGNAL_STOP:
    return SIGSTOP;
  case FTOP_SIGNAL_USR1:
    return SIGUSR1;
  case FTOP_SIGNAL_USR2:
    return SIGUSR2;
  default:
    return 0;
  }
}

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

int ftop_signal_check(int signal_id) {
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

int ftop_signal_number(int signal_id) {
  return ftop_signal_number_impl(signal_id);
}

int ftop_current_pid(void) {
  return (int)getpid();
}

int ftop_kill(int pid, int signal_number, int *sys_errno) {
  ftop_set_error(sys_errno, 0);
  if (pid <= 0 || signal_number <= 0) {
    ftop_set_error(sys_errno, EINVAL);
    return -1;
  }

  if (kill((pid_t)pid, signal_number) != 0) {
    ftop_set_error(sys_errno, errno);
    return -1;
  }

  return 0;
}
