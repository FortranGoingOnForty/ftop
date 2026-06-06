#define _POSIX_C_SOURCE 200809L

#include <stdio.h>
#include <stdlib.h>

int ftop_log_test_setenv(const char *name, const char *value) {
  return setenv(name, value, 1);
}

int ftop_log_test_unsetenv(const char *name) {
  return unsetenv(name);
}

void ftop_log_test_write_stderr(void) {
  (void)fprintf(stderr, "stderr capture fixture\n");
  (void)fflush(stderr);
}
