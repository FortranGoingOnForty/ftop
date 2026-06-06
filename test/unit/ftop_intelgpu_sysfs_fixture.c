#include <errno.h>
#include <limits.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

static char fixture_root[PATH_MAX];

static int join_path(char *buffer, size_t buffer_len, const char *base, const char *leaf) {
  int written;

  written = snprintf(buffer, buffer_len, "%s/%s", base, leaf);
  return written >= 0 && (size_t)written < buffer_len ? 0 : -1;
}

static int make_dir(const char *path, int *sys_errno) {
  if (mkdir(path, 0700) == 0) return 0;
  if (sys_errno != NULL) *sys_errno = errno;
  return -1;
}

static int write_text(const char *path, const char *text, int *sys_errno) {
  FILE *file;

  file = fopen(path, "w");
  if (file == NULL) {
    if (sys_errno != NULL) *sys_errno = errno;
    return -1;
  }
  if (fputs(text, file) < 0) {
    if (sys_errno != NULL) *sys_errno = errno;
    fclose(file);
    return -1;
  }
  if (fclose(file) != 0) {
    if (sys_errno != NULL) *sys_errno = errno;
    return -1;
  }
  return 0;
}

static int make_child_dir(char *path, size_t path_len, const char *parent, const char *name, int *sys_errno) {
  if (join_path(path, path_len, parent, name) != 0) {
    if (sys_errno != NULL) *sys_errno = ENAMETOOLONG;
    return -1;
  }
  return make_dir(path, sys_errno);
}

static int write_child_file(const char *parent, const char *name, const char *text, int *sys_errno) {
  char path[PATH_MAX];

  if (join_path(path, sizeof(path), parent, name) != 0) {
    if (sys_errno != NULL) *sys_errno = ENAMETOOLONG;
    return -1;
  }
  return write_text(path, text, sys_errno);
}

void ftop_test_intelgpu_sysfs_cleanup(void) {
  char path[PATH_MAX];

  if (fixture_root[0] == '\0') return;
  if (join_path(path, sizeof(path), fixture_root, "card0/device/hwmon/hwmon3/temp1_input") == 0) (void)unlink(path);
  if (join_path(path, sizeof(path), fixture_root, "card0/device/hwmon/hwmon3/power1_average") == 0) (void)unlink(path);
  if (join_path(path, sizeof(path), fixture_root, "card0/device/hwmon/hwmon3/power1_cap") == 0) (void)unlink(path);
  if (join_path(path, sizeof(path), fixture_root, "card0/device/hwmon/hwmon3/name") == 0) (void)unlink(path);
  if (join_path(path, sizeof(path), fixture_root, "card0/device/hwmon/hwmon3") == 0) (void)rmdir(path);
  if (join_path(path, sizeof(path), fixture_root, "card0/device/hwmon") == 0) (void)rmdir(path);
  if (join_path(path, sizeof(path), fixture_root, "card0/device/vendor") == 0) (void)unlink(path);
  if (join_path(path, sizeof(path), fixture_root, "card0/device/device") == 0) (void)unlink(path);
  if (join_path(path, sizeof(path), fixture_root, "card0/device/product_name") == 0) (void)unlink(path);
  if (join_path(path, sizeof(path), fixture_root, "card0/device/uevent") == 0) (void)unlink(path);
  if (join_path(path, sizeof(path), fixture_root, "card0/device/gpu_busy_percent") == 0) (void)unlink(path);
  if (join_path(path, sizeof(path), fixture_root, "card0/device/mem_info_vram_total") == 0) (void)unlink(path);
  if (join_path(path, sizeof(path), fixture_root, "card0/device/mem_info_vram_used") == 0) (void)unlink(path);
  if (join_path(path, sizeof(path), fixture_root, "card0/device") == 0) (void)rmdir(path);
  if (join_path(path, sizeof(path), fixture_root, "card0/gt_cur_freq_mhz") == 0) (void)unlink(path);
  if (join_path(path, sizeof(path), fixture_root, "card0/gt_max_freq_mhz") == 0) (void)unlink(path);
  if (join_path(path, sizeof(path), fixture_root, "card0") == 0) (void)rmdir(path);
  if (join_path(path, sizeof(path), fixture_root, "card1/device/vendor") == 0) (void)unlink(path);
  if (join_path(path, sizeof(path), fixture_root, "card1/device") == 0) (void)rmdir(path);
  if (join_path(path, sizeof(path), fixture_root, "card1") == 0) (void)rmdir(path);
  (void)rmdir(fixture_root);
  fixture_root[0] = '\0';
}

static int create_card0(const char *root, int *sys_errno) {
  char card[PATH_MAX];
  char device[PATH_MAX];
  char hwmon[PATH_MAX];
  char sensor[PATH_MAX];

  if (make_child_dir(card, sizeof(card), root, "card0", sys_errno) != 0) return -1;
  if (make_child_dir(device, sizeof(device), card, "device", sys_errno) != 0) return -1;
  if (make_child_dir(hwmon, sizeof(hwmon), device, "hwmon", sys_errno) != 0) return -1;
  if (make_child_dir(sensor, sizeof(sensor), hwmon, "hwmon3", sys_errno) != 0) return -1;

  if (write_child_file(device, "vendor", "0x8086\n", sys_errno) != 0) return -1;
  if (write_child_file(device, "device", "0x56c0\n", sys_errno) != 0) return -1;
  if (write_child_file(device, "product_name", "Fixture Arc\n", sys_errno) != 0) return -1;
  if (write_child_file(device, "uevent", "DRIVER=i915\nPCI_SLOT_NAME=0000:03:00.0\n", sys_errno) != 0) return -1;
  if (write_child_file(device, "gpu_busy_percent", "23\n", sys_errno) != 0) return -1;
  if (write_child_file(device, "mem_info_vram_total", "8589934592\n", sys_errno) != 0) return -1;
  if (write_child_file(device, "mem_info_vram_used", "2147483648\n", sys_errno) != 0) return -1;
  if (write_child_file(card, "gt_cur_freq_mhz", "700\n", sys_errno) != 0) return -1;
  if (write_child_file(card, "gt_max_freq_mhz", "1300\n", sys_errno) != 0) return -1;
  if (write_child_file(sensor, "name", "i915\n", sys_errno) != 0) return -1;
  if (write_child_file(sensor, "temp1_input", "52000\n", sys_errno) != 0) return -1;
  if (write_child_file(sensor, "power1_average", "45000000\n", sys_errno) != 0) return -1;
  return write_child_file(sensor, "power1_cap", "65000000\n", sys_errno);
}

static int create_card1(const char *root, int *sys_errno) {
  char card[PATH_MAX];
  char device[PATH_MAX];

  if (make_child_dir(card, sizeof(card), root, "card1", sys_errno) != 0) return -1;
  if (make_child_dir(device, sizeof(device), card, "device", sys_errno) != 0) return -1;
  return write_child_file(device, "vendor", "0x1002\n", sys_errno);
}

int ftop_test_intelgpu_sysfs_create(char *path, size_t path_len, size_t *value_len, int *sys_errno) {
  char template_path[PATH_MAX];
  size_t root_len;

  if (path == NULL || value_len == NULL || sys_errno == NULL || path_len == 0U) return -1;
  *value_len = 0U;
  *sys_errno = 0;
  ftop_test_intelgpu_sysfs_cleanup();

  if (snprintf(template_path, sizeof(template_path), "/tmp/ftop-intelgpu.XXXXXX") >= (int)sizeof(template_path)) {
    *sys_errno = ENAMETOOLONG;
    return -1;
  }
  if (mkdtemp(template_path) == NULL) {
    *sys_errno = errno;
    return -1;
  }
  snprintf(fixture_root, sizeof(fixture_root), "%s", template_path);

  if (create_card0(fixture_root, sys_errno) != 0 || create_card1(fixture_root, sys_errno) != 0) {
    ftop_test_intelgpu_sysfs_cleanup();
    return -1;
  }

  root_len = strlen(fixture_root);
  if (root_len + 1U > path_len) {
    *sys_errno = ENAMETOOLONG;
    ftop_test_intelgpu_sysfs_cleanup();
    return -1;
  }
  memcpy(path, fixture_root, root_len + 1U);
  *value_len = root_len;
  return 0;
}
