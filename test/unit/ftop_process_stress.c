#include <errno.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

static void sleep_milliseconds(int milliseconds) {
  struct timespec delay;
  struct timespec remaining;

  if (milliseconds <= 0) return;
  delay.tv_sec = milliseconds / 1000;
  delay.tv_nsec = (long)(milliseconds % 1000) * 1000000L;
  while (nanosleep(&delay, &remaining) != 0) {
    if (errno != EINTR) return;
    delay = remaining;
  }
}

int ftop_process_stress_spawn(int requested, int *pids, int capacity, int *spawned, int *sys_errno) {
  int index;

  if (spawned != 0) *spawned = 0;
  if (sys_errno != 0) *sys_errno = 0;
  if (requested < 0 || capacity < 0 || pids == 0 || spawned == 0 || sys_errno == 0) {
    if (sys_errno != 0) *sys_errno = EINVAL;
    return -1;
  }

  for (index = 0; index < requested && index < capacity; ++index) {
    pid_t pid = fork();
    if (pid < 0) {
      *sys_errno = errno;
      return -1;
    }

    if (pid == 0) {
      signal(SIGTERM, SIG_DFL);
      signal(SIGINT, SIG_DFL);
      signal(SIGCHLD, SIG_DFL);
      for (;;) sleep(60);
    }

    pids[index] = (int)pid;
    *spawned = index + 1;
  }

  if (*spawned != requested) {
    *sys_errno = E2BIG;
    return -1;
  }
  return 0;
}

int ftop_process_stress_cleanup(const int *pids, int count, int *sys_errno) {
  int index;
  int first_errno = 0;

  if (sys_errno != 0) *sys_errno = 0;
  if (count < 0 || pids == 0 || sys_errno == 0) {
    if (sys_errno != 0) *sys_errno = EINVAL;
    return -1;
  }

  for (index = 0; index < count; ++index) {
    if (pids[index] <= 0) continue;
    if (kill((pid_t)pids[index], SIGTERM) != 0 && errno != ESRCH && first_errno == 0) {
      first_errno = errno;
    }
  }

  for (index = 0; index < count; ++index) {
    int attempts;
    int reaped;
    if (pids[index] <= 0) continue;

    reaped = 0;
    for (attempts = 0; attempts < 100; ++attempts) {
      int status;
      pid_t waited = waitpid((pid_t)pids[index], &status, WNOHANG);
      if (waited == (pid_t)pids[index] || (waited < 0 && errno == ECHILD)) {
        reaped = 1;
        break;
      }
      if (waited < 0 && errno != EINTR) {
        if (first_errno == 0) first_errno = errno;
        break;
      }
      sleep_milliseconds(10);
    }

    if (!reaped) {
      if (kill((pid_t)pids[index], SIGKILL) != 0 && errno != ESRCH && first_errno == 0) first_errno = errno;
      for (;;) {
        int status;
        pid_t waited = waitpid((pid_t)pids[index], &status, 0);
        if (waited == (pid_t)pids[index] || (waited < 0 && errno == ECHILD)) break;
        if (waited < 0 && errno != EINTR) {
          if (first_errno == 0) first_errno = errno;
          break;
        }
      }
    }
  }

  if (first_errno != 0) {
    *sys_errno = first_errno;
    return -1;
  }
  return 0;
}

void ftop_process_stress_sleep_ms(int milliseconds) {
  sleep_milliseconds(milliseconds);
}
