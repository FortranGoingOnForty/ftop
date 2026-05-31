#include <errno.h>
#include <pthread.h>
#include <stdlib.h>

struct ftop_thread_handle {
  pthread_t thread;
};

struct ftop_mutex_handle {
  pthread_mutex_t mutex;
};

struct ftop_cond_handle {
  pthread_cond_t cond;
};

static void ftop_set_error(int *sys_errno, int code) {
  if (sys_errno != NULL) *sys_errno = code;
}

void *ftop_thread_create(void *(*start_routine)(void *), void *arg, int *sys_errno) {
  struct ftop_thread_handle *handle;
  int rc;

  ftop_set_error(sys_errno, 0);
  if (start_routine == NULL) {
    ftop_set_error(sys_errno, EINVAL);
    return NULL;
  }

  handle = malloc(sizeof(*handle));
  if (handle == NULL) {
    ftop_set_error(sys_errno, ENOMEM);
    return NULL;
  }

  rc = pthread_create(&handle->thread, NULL, start_routine, arg);
  if (rc != 0) {
    ftop_set_error(sys_errno, rc);
    free(handle);
    return NULL;
  }

  return handle;
}

int ftop_thread_join(void *thread_handle, int *sys_errno) {
  struct ftop_thread_handle *handle;
  int rc;

  ftop_set_error(sys_errno, 0);
  if (thread_handle == NULL) {
    ftop_set_error(sys_errno, EINVAL);
    return -1;
  }

  handle = thread_handle;
  rc = pthread_join(handle->thread, NULL);
  if (rc != 0) {
    ftop_set_error(sys_errno, rc);
    return -1;
  }

  free(handle);
  return 0;
}

void *ftop_mutex_init(int *sys_errno) {
  struct ftop_mutex_handle *handle;
  int rc;

  ftop_set_error(sys_errno, 0);
  handle = malloc(sizeof(*handle));
  if (handle == NULL) {
    ftop_set_error(sys_errno, ENOMEM);
    return NULL;
  }

  rc = pthread_mutex_init(&handle->mutex, NULL);
  if (rc != 0) {
    ftop_set_error(sys_errno, rc);
    free(handle);
    return NULL;
  }

  return handle;
}

int ftop_mutex_lock(void *mutex_handle, int *sys_errno) {
  struct ftop_mutex_handle *handle;
  int rc;

  ftop_set_error(sys_errno, 0);
  if (mutex_handle == NULL) {
    ftop_set_error(sys_errno, EINVAL);
    return -1;
  }

  handle = mutex_handle;
  rc = pthread_mutex_lock(&handle->mutex);
  if (rc != 0) {
    ftop_set_error(sys_errno, rc);
    return -1;
  }

  return 0;
}

int ftop_mutex_unlock(void *mutex_handle, int *sys_errno) {
  struct ftop_mutex_handle *handle;
  int rc;

  ftop_set_error(sys_errno, 0);
  if (mutex_handle == NULL) {
    ftop_set_error(sys_errno, EINVAL);
    return -1;
  }

  handle = mutex_handle;
  rc = pthread_mutex_unlock(&handle->mutex);
  if (rc != 0) {
    ftop_set_error(sys_errno, rc);
    return -1;
  }

  return 0;
}

int ftop_mutex_destroy(void *mutex_handle, int *sys_errno) {
  struct ftop_mutex_handle *handle;
  int rc;

  ftop_set_error(sys_errno, 0);
  if (mutex_handle == NULL) {
    ftop_set_error(sys_errno, EINVAL);
    return -1;
  }

  handle = mutex_handle;
  rc = pthread_mutex_destroy(&handle->mutex);
  if (rc != 0) {
    ftop_set_error(sys_errno, rc);
    return -1;
  }

  free(handle);
  return 0;
}

void *ftop_cond_init(int *sys_errno) {
  struct ftop_cond_handle *handle;
  int rc;

  ftop_set_error(sys_errno, 0);
  handle = malloc(sizeof(*handle));
  if (handle == NULL) {
    ftop_set_error(sys_errno, ENOMEM);
    return NULL;
  }

  rc = pthread_cond_init(&handle->cond, NULL);
  if (rc != 0) {
    ftop_set_error(sys_errno, rc);
    free(handle);
    return NULL;
  }

  return handle;
}

int ftop_cond_wait(void *cond_handle, void *mutex_handle, int *sys_errno) {
  struct ftop_cond_handle *cond;
  struct ftop_mutex_handle *mutex;
  int rc;

  ftop_set_error(sys_errno, 0);
  if (cond_handle == NULL || mutex_handle == NULL) {
    ftop_set_error(sys_errno, EINVAL);
    return -1;
  }

  cond = cond_handle;
  mutex = mutex_handle;
  rc = pthread_cond_wait(&cond->cond, &mutex->mutex);
  if (rc != 0) {
    ftop_set_error(sys_errno, rc);
    return -1;
  }

  return 0;
}

int ftop_cond_signal(void *cond_handle, int *sys_errno) {
  struct ftop_cond_handle *handle;
  int rc;

  ftop_set_error(sys_errno, 0);
  if (cond_handle == NULL) {
    ftop_set_error(sys_errno, EINVAL);
    return -1;
  }

  handle = cond_handle;
  rc = pthread_cond_signal(&handle->cond);
  if (rc != 0) {
    ftop_set_error(sys_errno, rc);
    return -1;
  }

  return 0;
}

int ftop_cond_broadcast(void *cond_handle, int *sys_errno) {
  struct ftop_cond_handle *handle;
  int rc;

  ftop_set_error(sys_errno, 0);
  if (cond_handle == NULL) {
    ftop_set_error(sys_errno, EINVAL);
    return -1;
  }

  handle = cond_handle;
  rc = pthread_cond_broadcast(&handle->cond);
  if (rc != 0) {
    ftop_set_error(sys_errno, rc);
    return -1;
  }

  return 0;
}

int ftop_cond_destroy(void *cond_handle, int *sys_errno) {
  struct ftop_cond_handle *handle;
  int rc;

  ftop_set_error(sys_errno, 0);
  if (cond_handle == NULL) {
    ftop_set_error(sys_errno, EINVAL);
    return -1;
  }

  handle = cond_handle;
  rc = pthread_cond_destroy(&handle->cond);
  if (rc != 0) {
    ftop_set_error(sys_errno, rc);
    return -1;
  }

  free(handle);
  return 0;
}
