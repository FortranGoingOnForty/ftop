#include <errno.h>
#include <stddef.h>
#include <unistd.h>

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
