#include <errno.h>
#include <time.h>

static int ftop_collector_get_monotonic_ms(long long *milliseconds) {
  struct timespec now;

  if (clock_gettime(CLOCK_MONOTONIC, &now) != 0) {
    return -1;
  }

  *milliseconds = (long long)now.tv_sec * 1000LL + (long long)now.tv_nsec / 1000000LL;
  return 0;
}

int ftop_collector_monotonic_ms(long long *milliseconds, int *sys_errno) {
  *sys_errno = 0;
  if (milliseconds == 0) {
    return -1;
  }

  if (ftop_collector_get_monotonic_ms(milliseconds) != 0) {
    *sys_errno = errno;
    return -1;
  }

  return 0;
}

int ftop_collector_sleep_until_ms(long long deadline_ms, int *sys_errno) {
  long long now_ms;
  long long remaining_ms;
  struct timespec delay;

  *sys_errno = 0;

  for (;;) {
    if (ftop_collector_get_monotonic_ms(&now_ms) != 0) {
      *sys_errno = errno;
      return -1;
    }

    remaining_ms = deadline_ms - now_ms;
    if (remaining_ms <= 0) {
      return 0;
    }

    delay.tv_sec = (time_t)(remaining_ms / 1000LL);
    delay.tv_nsec = (long)((remaining_ms % 1000LL) * 1000000LL);

    while (nanosleep(&delay, &delay) != 0) {
      if (errno != EINTR) {
        *sys_errno = errno;
        return -1;
      }
    }
  }
}
