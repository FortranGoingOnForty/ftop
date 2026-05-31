#include <dlfcn.h>
#include <errno.h>
#include <stddef.h>

typedef void (*ftop_dl_function)(void);

union ftop_dl_symbol {
  void *object;
  ftop_dl_function function;
};

static void ftop_set_error(int *sys_errno, int code) {
  if (sys_errno != NULL) *sys_errno = code;
}

void *ftop_dlopen(const char *path, int *sys_errno) {
  void *handle;

  ftop_set_error(sys_errno, 0);
  if (path == NULL || path[0] == '\0') {
    ftop_set_error(sys_errno, EINVAL);
    return NULL;
  }

  handle = dlopen(path, RTLD_LAZY | RTLD_LOCAL);
  if (handle == NULL) {
    ftop_set_error(sys_errno, ENOENT);
    return NULL;
  }

  return handle;
}

ftop_dl_function ftop_dlsym(void *handle, const char *symbol_name, int *sys_errno) {
  union ftop_dl_symbol symbol;

  ftop_set_error(sys_errno, 0);
  symbol.function = NULL;
  if (handle == NULL || symbol_name == NULL || symbol_name[0] == '\0') {
    ftop_set_error(sys_errno, EINVAL);
    return symbol.function;
  }

  (void)dlerror();
  symbol.object = dlsym(handle, symbol_name);
  if (dlerror() != NULL || symbol.object == NULL) {
    ftop_set_error(sys_errno, ENOENT);
    symbol.function = NULL;
  }

  return symbol.function;
}

int ftop_dlclose(void *handle, int *sys_errno) {
  int rc;

  ftop_set_error(sys_errno, 0);
  if (handle == NULL) {
    ftop_set_error(sys_errno, EINVAL);
    return -1;
  }

  rc = dlclose(handle);
  if (rc != 0) {
    ftop_set_error(sys_errno, EINVAL);
    return -1;
  }

  return 0;
}
