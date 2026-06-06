#include <errno.h>
#include <ctype.h>
#include <dirent.h>
#include <fcntl.h>
#include <limits.h>
#include <mntent.h>
#include <netinet/in.h>
#include <pwd.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <sys/socket.h>
#include <sys/statvfs.h>
#include <unistd.h>

#include <linux/inet_diag.h>
#include <linux/netlink.h>
#include <linux/rtnetlink.h>
#include <linux/sock_diag.h>

#define FTOP_LINUX_CPUINFO_BUFFER_LEN 1048576U
#define FTOP_LINUX_HWMON_NAME_LEN 128
#define FTOP_LINUX_HWMON_PATH_LEN 256
#define FTOP_LINUX_PROCESS_COMMAND_LEN 256
#define FTOP_LINUX_PROCESS_STAT_LEN 512
#define FTOP_LINUX_PROCESS_STATUS_LEN 2048
#define FTOP_LINUX_PROCESS_IO_LEN 512
#define FTOP_LINUX_PROCESS_CGROUP_LEN 1024
#define FTOP_LINUX_SOCKET_OWNER_PROCESS_NAME_LEN 64
#define FTOP_LINUX_DRM_CARD_NAME_LEN 32
#define FTOP_LINUX_DRM_DEVICE_PATH_LEN 512
#define FTOP_DISK_DEVICE_LEN 64
#define FTOP_DISK_MOUNTPOINT_LEN 128
#define FTOP_DISK_FSTYPE_LEN 32
#define FTOP_USER_LOOKUP_BUFFER_LEN 16384

#ifndef SOCK_CLOEXEC
#define SOCK_CLOEXEC 0
#endif

#ifndef TCPF_ALL
#define TCPF_ALL 0xFFFU
#endif

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

struct ftop_linux_hwmon_sensor {
  char path[FTOP_LINUX_HWMON_PATH_LEN];
  char name[FTOP_LINUX_HWMON_NAME_LEN];
};

struct ftop_linux_process_raw {
  int pid;
  char stat[FTOP_LINUX_PROCESS_STAT_LEN];
  size_t stat_len;
  char status[FTOP_LINUX_PROCESS_STATUS_LEN];
  size_t status_len;
  char cmdline[FTOP_LINUX_PROCESS_COMMAND_LEN];
  size_t cmdline_len;
  char io[FTOP_LINUX_PROCESS_IO_LEN];
  size_t io_len;
  char cgroup[FTOP_LINUX_PROCESS_CGROUP_LEN];
  size_t cgroup_len;
};

struct ftop_linux_socket_owner {
  long long inode;
  long long start_time;
  int pid;
  char process_name[FTOP_LINUX_SOCKET_OWNER_PROCESS_NAME_LEN];
};

struct ftop_linux_socket_traffic {
  long long inode;
  long long rx_bytes;
  long long tx_bytes;
};

struct ftop_linux_drm_card {
  char name[FTOP_LINUX_DRM_CARD_NAME_LEN];
  char device_path[FTOP_LINUX_DRM_DEVICE_PATH_LEN];
};

struct ftop_linux_tcp_info_bytes {
  uint8_t byte_fields[8];
  uint32_t rto;
  uint32_t ato;
  uint32_t snd_mss;
  uint32_t rcv_mss;
  uint32_t unacked;
  uint32_t sacked;
  uint32_t lost;
  uint32_t retrans;
  uint32_t fackets;
  uint32_t last_data_sent;
  uint32_t last_ack_sent;
  uint32_t last_data_recv;
  uint32_t last_ack_recv;
  uint32_t pmtu;
  uint32_t rcv_ssthresh;
  uint32_t rtt;
  uint32_t rttvar;
  uint32_t snd_ssthresh;
  uint32_t snd_cwnd;
  uint32_t advmss;
  uint32_t reordering;
  uint32_t rcv_rtt;
  uint32_t rcv_space;
  uint32_t total_retrans;
  uint64_t pacing_rate;
  uint64_t max_pacing_rate;
  uint64_t bytes_acked;
  uint64_t bytes_received;
};

struct ftop_filesystem_info {
  int valid;
  char device[FTOP_DISK_DEVICE_LEN];
  char mountpoint[FTOP_DISK_MOUNTPOINT_LEN];
  char fstype[FTOP_DISK_FSTYPE_LEN];
  long long total_bytes;
  long long used_bytes;
  long long available_bytes;
};

static int ftop_read_file_into_buffer(const char *path, char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
  ssize_t bytes_read;
  int fd;

  if (path == NULL || buffer == NULL || value_len == NULL || sys_errno == NULL || buffer_len < 2U) return -1;

  buffer[0] = '\0';
  *value_len = 0U;
  *sys_errno = 0;

  fd = open(path, O_RDONLY);
  if (fd < 0) {
    *sys_errno = errno;
    return -1;
  }

  bytes_read = read(fd, buffer, buffer_len - 1U);
  if (bytes_read < 0) {
    *sys_errno = errno;
    close(fd);
    return -1;
  }

  close(fd);
  buffer[bytes_read] = '\0';
  *value_len = (size_t)bytes_read;
  return 0;
}

int ftop_linux_read_proc_stat(char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
  return ftop_read_file_into_buffer("/proc/stat", buffer, buffer_len, value_len, sys_errno);
}

int ftop_linux_read_proc_meminfo(char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
  return ftop_read_file_into_buffer("/proc/meminfo", buffer, buffer_len, value_len, sys_errno);
}

int ftop_linux_read_proc_loadavg(char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
  return ftop_read_file_into_buffer("/proc/loadavg", buffer, buffer_len, value_len, sys_errno);
}

int ftop_linux_read_proc_uptime(char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
  return ftop_read_file_into_buffer("/proc/uptime", buffer, buffer_len, value_len, sys_errno);
}

int ftop_linux_read_proc_net_dev(char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
  return ftop_read_file_into_buffer("/proc/net/dev", buffer, buffer_len, value_len, sys_errno);
}

int ftop_linux_read_proc_diskstats(char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
  return ftop_read_file_into_buffer("/proc/diskstats", buffer, buffer_len, value_len, sys_errno);
}

int ftop_linux_read_sysfs_file(const char *path, char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
  return ftop_read_file_into_buffer(path, buffer, buffer_len, value_len, sys_errno);
}

int ftop_linux_read_proc_net_tcp(char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
  return ftop_read_file_into_buffer("/proc/net/tcp", buffer, buffer_len, value_len, sys_errno);
}

int ftop_linux_read_proc_net_udp(char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
  return ftop_read_file_into_buffer("/proc/net/udp", buffer, buffer_len, value_len, sys_errno);
}

int ftop_linux_read_proc_net_tcp6(char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
  return ftop_read_file_into_buffer("/proc/net/tcp6", buffer, buffer_len, value_len, sys_errno);
}

int ftop_linux_read_proc_net_udp6(char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
  return ftop_read_file_into_buffer("/proc/net/udp6", buffer, buffer_len, value_len, sys_errno);
}

static int ftop_linux_safe_net_name(const char *name) {
  size_t i;

  if (name == NULL || name[0] == '\0') return 0;
  for (i = 0U; name[i] != '\0'; ++i) {
    if (name[i] == '/') return 0;
  }
  return 1;
}

static int ftop_linux_allowed_net_field(const char *field) {
  if (field == NULL) return 0;
  return strcmp(field, "mtu") == 0 || strcmp(field, "operstate") == 0 || strcmp(field, "speed") == 0;
}

int ftop_linux_read_net_interface_file(
    const char *interface_name, const char *field_name, char *buffer, size_t buffer_len,
    size_t *value_len, int *sys_errno) {
  char path[256];
  int written;

  if (!ftop_linux_safe_net_name(interface_name) || !ftop_linux_allowed_net_field(field_name)) {
    if (sys_errno != NULL) *sys_errno = EINVAL;
    return -1;
  }

  written = snprintf(path, sizeof(path), "/sys/class/net/%s/%s", interface_name, field_name);
  if (written < 0 || (size_t)written >= sizeof(path)) {
    if (sys_errno != NULL) *sys_errno = EOVERFLOW;
    return -1;
  }
  return ftop_read_file_into_buffer(path, buffer, buffer_len, value_len, sys_errno);
}

static int ftop_linux_pid_from_name(const char *name, int *pid) {
  char *end;
  long value;

  if (name == NULL || pid == NULL || !isdigit((unsigned char)name[0])) return 0;
  errno = 0;
  value = strtol(name, &end, 10);
  if (end == name || *end != '\0' || errno != 0 || value <= 0 || value > 2147483647L) return 0;
  *pid = (int)value;
  return 1;
}

static int ftop_linux_read_process_file(
    int pid, const char *name, char *buffer, size_t buffer_len, size_t *value_len) {
  char path[128];
  int read_errno;
  int written;

  written = snprintf(path, sizeof(path), "/proc/%d/%s", pid, name);
  if (written < 0 || (size_t)written >= sizeof(path)) return -1;
  read_errno = 0;
  return ftop_read_file_into_buffer(path, buffer, buffer_len, value_len, &read_errno);
}

int ftop_linux_process_snapshot(
    struct ftop_linux_process_raw *buffer, size_t capacity, size_t *process_count, int *sys_errno) {
  DIR *directory;
  struct dirent *entry;
  struct ftop_linux_process_raw process;
  int pid;
  size_t count;

  if (buffer == NULL || process_count == NULL || sys_errno == NULL || capacity == 0) return -1;

  *process_count = 0U;
  *sys_errno = 0;
  directory = opendir("/proc");
  if (directory == NULL) {
    *sys_errno = errno;
    return -1;
  }

  count = 0U;
  while ((entry = readdir(directory)) != NULL && count < capacity) {
    if (!ftop_linux_pid_from_name(entry->d_name, &pid)) continue;
    memset(&process, 0, sizeof(process));
    process.pid = pid;
    if (ftop_linux_read_process_file(pid, "stat", process.stat, sizeof(process.stat), &process.stat_len) != 0) continue;
    (void)ftop_linux_read_process_file(pid, "status", process.status, sizeof(process.status), &process.status_len);
    (void)ftop_linux_read_process_file(pid, "cmdline", process.cmdline, sizeof(process.cmdline), &process.cmdline_len);
    (void)ftop_linux_read_process_file(pid, "io", process.io, sizeof(process.io), &process.io_len);
    (void)ftop_linux_read_process_file(pid, "cgroup", process.cgroup, sizeof(process.cgroup), &process.cgroup_len);
    buffer[count] = process;
    ++count;
  }

  closedir(directory);
  *process_count = count;
  return 0;
}

static void ftop_copy_owner_process_name(char *destination, const char *source, size_t source_len) {
  size_t copied_len;

  if (destination == NULL) return;
  destination[0] = '\0';
  if (source == NULL || source_len == 0U) return;
  while (source_len > 0U && (source[source_len - 1U] == '\n' || source[source_len - 1U] == '\r')) --source_len;
  copied_len = source_len < FTOP_LINUX_SOCKET_OWNER_PROCESS_NAME_LEN - 1U ?
      source_len : FTOP_LINUX_SOCKET_OWNER_PROCESS_NAME_LEN - 1U;
  memcpy(destination, source, copied_len);
  destination[copied_len] = '\0';
}

static int ftop_linux_read_process_comm(int pid, char *buffer, size_t buffer_len) {
  char comm[FTOP_LINUX_SOCKET_OWNER_PROCESS_NAME_LEN];
  size_t value_len;

  if (buffer == NULL || buffer_len == 0U) return -1;
  buffer[0] = '\0';
  value_len = 0U;
  if (ftop_linux_read_process_file(pid, "comm", comm, sizeof(comm), &value_len) != 0) return -1;
  ftop_copy_owner_process_name(buffer, comm, value_len);
  return buffer[0] == '\0' ? -1 : 0;
}

static int ftop_socket_inode_from_link(const char *target, long long *inode) {
  const char prefix[] = "socket:[";
  char *end;
  long long value;

  if (target == NULL || inode == NULL) return 0;
  if (strncmp(target, prefix, sizeof(prefix) - 1U) != 0) return 0;
  errno = 0;
  value = strtoll(target + sizeof(prefix) - 1U, &end, 10);
  if (end == target + sizeof(prefix) - 1U || *end != ']' || errno != 0 || value <= 0) return 0;
  *inode = value;
  return 1;
}

static int ftop_socket_owner_exists(
    const struct ftop_linux_socket_owner *owners, size_t count, long long inode, int pid) {
  size_t i;

  for (i = 0U; i < count; ++i) {
    if (owners[i].inode == inode && owners[i].pid == pid) return 1;
  }
  return 0;
}

static void ftop_store_socket_owner(
    struct ftop_linux_socket_owner *owner, int pid, long long start_time, long long inode, const char *process_name) {
  memset(owner, 0, sizeof(*owner));
  owner->inode = inode;
  owner->start_time = start_time;
  owner->pid = pid;
  if (process_name != NULL) {
    ftop_copy_owner_process_name(owner->process_name, process_name, strlen(process_name));
  }
}

static int ftop_linux_read_process_start_time(int pid, long long *start_time) {
  char stat[FTOP_LINUX_PROCESS_STAT_LEN];
  char *end;
  const char *cursor;
  long long value;
  size_t value_len;
  int field;

  if (start_time == NULL) return -1;
  *start_time = 0LL;
  value_len = 0U;
  if (ftop_linux_read_process_file(pid, "stat", stat, sizeof(stat), &value_len) != 0) return -1;
  cursor = strrchr(stat, ')');
  if (cursor == NULL) return -1;
  ++cursor;

  for (field = 3; field <= 22; ++field) {
    while (*cursor == ' ' || *cursor == '\t') ++cursor;
    if (*cursor == '\0' || *cursor == '\n') return -1;
    if (field == 22) {
      errno = 0;
      value = strtoll(cursor, &end, 10);
      if (end == cursor || errno != 0 || value <= 0LL) return -1;
      *start_time = value;
      return 0;
    }
    while (*cursor != '\0' && *cursor != '\n' && *cursor != ' ' && *cursor != '\t') ++cursor;
  }

  return -1;
}

static void ftop_scan_process_socket_owners(
    int pid, struct ftop_linux_socket_owner *owners, size_t capacity, size_t *count) {
  char fd_directory_path[PATH_MAX];
  char link_path[PATH_MAX];
  char process_name[FTOP_LINUX_SOCKET_OWNER_PROCESS_NAME_LEN];
  char target[128];
  DIR *fd_directory;
  struct dirent *entry;
  long long inode;
  long long start_time;
  ssize_t target_len;
  int written;

  if (count == NULL || *count >= capacity) return;
  written = snprintf(fd_directory_path, sizeof(fd_directory_path), "/proc/%d/fd", pid);
  if (written < 0 || (size_t)written >= sizeof(fd_directory_path)) return;
  fd_directory = opendir(fd_directory_path);
  if (fd_directory == NULL) return;

  process_name[0] = '\0';
  start_time = 0LL;
  (void)ftop_linux_read_process_comm(pid, process_name, sizeof(process_name));
  (void)ftop_linux_read_process_start_time(pid, &start_time);
  while ((entry = readdir(fd_directory)) != NULL && *count < capacity) {
    if (entry->d_name[0] == '.') continue;
    written = snprintf(link_path, sizeof(link_path), "%s/%s", fd_directory_path, entry->d_name);
    if (written < 0 || (size_t)written >= sizeof(link_path)) continue;
    target_len = readlink(link_path, target, sizeof(target) - 1U);
    if (target_len <= 0) continue;
    target[target_len] = '\0';
    if (!ftop_socket_inode_from_link(target, &inode)) continue;
    if (ftop_socket_owner_exists(owners, *count, inode, pid)) continue;
    ftop_store_socket_owner(&owners[*count], pid, start_time, inode, process_name);
    ++(*count);
  }

  closedir(fd_directory);
}

int ftop_linux_socket_owners(
    struct ftop_linux_socket_owner *owners, size_t capacity, size_t *owner_count, int *sys_errno) {
  DIR *directory;
  struct dirent *entry;
  int pid;
  size_t count;

  if (owners == NULL || owner_count == NULL || sys_errno == NULL || capacity == 0U) return -1;

  *owner_count = 0U;
  *sys_errno = 0;
  directory = opendir("/proc");
  if (directory == NULL) {
    *sys_errno = errno;
    return -1;
  }

  count = 0U;
  while ((entry = readdir(directory)) != NULL && count < capacity) {
    if (!ftop_linux_pid_from_name(entry->d_name, &pid)) continue;
    ftop_scan_process_socket_owners(pid, owners, capacity, &count);
  }

  closedir(directory);
  *owner_count = count;
  return 0;
}

static long long ftop_linux_u64_to_long_long(uint64_t value) {
  if (value > (uint64_t)LLONG_MAX) return LLONG_MAX;
  return (long long)value;
}

static long long ftop_linux_saturating_add_long_long(long long left, long long right) {
  if (left < 0LL) left = 0LL;
  if (right < 0LL) right = 0LL;
  if (right > LLONG_MAX - left) return LLONG_MAX;
  return left + right;
}

static void ftop_linux_store_socket_traffic(
    struct ftop_linux_socket_traffic *traffic, size_t capacity, size_t *count,
    long long inode, long long rx_bytes, long long tx_bytes) {
  size_t index;

  if (traffic == NULL || count == NULL || inode <= 0LL) return;
  for (index = 0U; index < *count; ++index) {
    if (traffic[index].inode != inode) continue;
    traffic[index].rx_bytes = ftop_linux_saturating_add_long_long(traffic[index].rx_bytes, rx_bytes);
    traffic[index].tx_bytes = ftop_linux_saturating_add_long_long(traffic[index].tx_bytes, tx_bytes);
    return;
  }

  if (*count >= capacity) return;
  memset(&traffic[*count], 0, sizeof(traffic[*count]));
  traffic[*count].inode = inode;
  traffic[*count].rx_bytes = rx_bytes;
  traffic[*count].tx_bytes = tx_bytes;
  ++(*count);
}

static int ftop_linux_tcp_info_bytes(const struct rtattr *attribute, long long *rx_bytes, long long *tx_bytes) {
  struct ftop_linux_tcp_info_bytes info;

  if (attribute == NULL || rx_bytes == NULL || tx_bytes == NULL) return 0;
  *rx_bytes = 0LL;
  *tx_bytes = 0LL;
  if ((size_t)RTA_PAYLOAD(attribute) < sizeof(info)) return 0;

  memset(&info, 0, sizeof(info));
  memcpy(&info, RTA_DATA(attribute), sizeof(info));
  *rx_bytes = ftop_linux_u64_to_long_long(info.bytes_received);
  *tx_bytes = ftop_linux_u64_to_long_long(info.bytes_acked);
  return *rx_bytes > 0LL || *tx_bytes > 0LL;
}

static void ftop_linux_parse_inet_diag_message(
    const struct nlmsghdr *header, struct ftop_linux_socket_traffic *traffic, size_t capacity, size_t *count) {
  const struct inet_diag_msg *message;
  struct rtattr *attribute;
  long long rx_bytes;
  long long tx_bytes;
  int attribute_len;

  if (header == NULL || traffic == NULL || count == NULL) return;
  if (header->nlmsg_len < NLMSG_LENGTH(sizeof(*message))) return;
  message = (const struct inet_diag_msg *)NLMSG_DATA(header);
  if (message->idiag_inode == 0U) return;

  rx_bytes = 0LL;
  tx_bytes = 0LL;
  attribute_len = (int)(header->nlmsg_len - NLMSG_LENGTH(sizeof(*message)));
  attribute = (struct rtattr *)((char *)message + NLMSG_ALIGN(sizeof(*message)));
  while (RTA_OK(attribute, attribute_len)) {
    if (attribute->rta_type == INET_DIAG_INFO) {
      if (ftop_linux_tcp_info_bytes(attribute, &rx_bytes, &tx_bytes)) break;
    }
    attribute = RTA_NEXT(attribute, attribute_len);
  }

  if (rx_bytes <= 0LL && tx_bytes <= 0LL) return;
  ftop_linux_store_socket_traffic(traffic, capacity, count, (long long)message->idiag_inode, rx_bytes, tx_bytes);
}

static int ftop_linux_dump_tcp_socket_traffic(
    int family, struct ftop_linux_socket_traffic *traffic, size_t capacity, size_t *count, int *sys_errno) {
  struct {
    struct nlmsghdr header;
    struct inet_diag_req_v2 request;
  } request;
  char buffer[65536];
  struct nlmsghdr *header;
  struct nlmsgerr *error;
  ssize_t bytes_received;
  ssize_t bytes_sent;
  int done;
  int fd;
  int remaining;

  fd = socket(AF_NETLINK, SOCK_RAW | SOCK_CLOEXEC, NETLINK_INET_DIAG);
  if (fd < 0) {
    *sys_errno = errno;
    return -1;
  }

  memset(&request, 0, sizeof(request));
  request.header.nlmsg_len = sizeof(request);
  request.header.nlmsg_type = SOCK_DIAG_BY_FAMILY;
  request.header.nlmsg_flags = NLM_F_REQUEST | NLM_F_DUMP;
  request.request.sdiag_family = (uint8_t)family;
  request.request.sdiag_protocol = IPPROTO_TCP;
  request.request.idiag_ext = 1U << (INET_DIAG_INFO - 1U);
  request.request.idiag_states = TCPF_ALL;

  bytes_sent = send(fd, &request, sizeof(request), 0);
  if (bytes_sent != (ssize_t)sizeof(request)) {
    *sys_errno = errno != 0 ? errno : EIO;
    close(fd);
    return -1;
  }

  done = 0;
  while (!done) {
    bytes_received = recv(fd, buffer, sizeof(buffer), 0);
    if (bytes_received < 0) {
      if (errno == EINTR) continue;
      *sys_errno = errno;
      close(fd);
      return -1;
    }
    if (bytes_received == 0) break;

    remaining = (int)bytes_received;
    header = (struct nlmsghdr *)buffer;
    while (NLMSG_OK(header, remaining)) {
      if (header->nlmsg_type == NLMSG_DONE) {
        done = 1;
        break;
      }
      if (header->nlmsg_type == NLMSG_ERROR) {
        if (header->nlmsg_len >= NLMSG_LENGTH(sizeof(*error))) {
          error = (struct nlmsgerr *)NLMSG_DATA(header);
          if (error->error != 0) {
            *sys_errno = -error->error;
            close(fd);
            return -1;
          }
        }
        done = 1;
        break;
      }
      ftop_linux_parse_inet_diag_message(header, traffic, capacity, count);
      header = NLMSG_NEXT(header, remaining);
    }
  }

  close(fd);
  return 0;
}

int ftop_linux_socket_traffic(
    struct ftop_linux_socket_traffic *traffic, size_t capacity, size_t *traffic_count, int *sys_errno) {
  int ipv4_errno;
  int ipv4_rc;
  int ipv6_errno;
  int ipv6_rc;
  size_t count;

  if (traffic == NULL || traffic_count == NULL || sys_errno == NULL || capacity == 0U) return -1;

  memset(traffic, 0, sizeof(traffic[0]) * capacity);
  *traffic_count = 0U;
  *sys_errno = 0;
  count = 0U;

  ipv4_errno = 0;
  ipv6_errno = 0;
  ipv4_rc = ftop_linux_dump_tcp_socket_traffic(AF_INET, traffic, capacity, &count, &ipv4_errno);
  ipv6_rc = ftop_linux_dump_tcp_socket_traffic(AF_INET6, traffic, capacity, &count, &ipv6_errno);
  if (ipv4_rc != 0 && ipv6_rc != 0) {
    *sys_errno = ipv4_errno != 0 ? ipv4_errno : ipv6_errno;
    return -1;
  }

  *traffic_count = count;
  return 0;
}

int ftop_linux_page_size(long long *page_size, int *sys_errno) {
  long value;

  if (page_size == NULL || sys_errno == NULL) return -1;

  *page_size = 0;
  *sys_errno = 0;
  value = sysconf(_SC_PAGESIZE);
  if (value <= 0L) {
    *sys_errno = errno != 0 ? errno : EINVAL;
    return -1;
  }

  *page_size = (long long)value;
  return 0;
}

int ftop_linux_clock_ticks_per_second(long long *clock_ticks, int *sys_errno) {
  long value;

  if (clock_ticks == NULL || sys_errno == NULL) return -1;

  *clock_ticks = 0;
  *sys_errno = 0;
  value = sysconf(_SC_CLK_TCK);
  if (value <= 0L) {
    *sys_errno = errno != 0 ? errno : EINVAL;
    return -1;
  }

  *clock_ticks = (long long)value;
  return 0;
}

int ftop_linux_user_name(int uid, char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
  char scratch[FTOP_USER_LOOKUP_BUFFER_LEN];
  struct passwd password;
  struct passwd *result;
  size_t copied_len;
  int rc;

  if (buffer == NULL || value_len == NULL || sys_errno == NULL || buffer_len == 0) return -1;

  buffer[0] = '\0';
  *value_len = 0U;
  *sys_errno = 0;
  if (uid < 0) {
    *sys_errno = EINVAL;
    return -1;
  }

  memset(&password, 0, sizeof(password));
  result = NULL;
  rc = getpwuid_r((uid_t)uid, &password, scratch, sizeof(scratch), &result);
  if (rc != 0) {
    *sys_errno = rc;
    return -1;
  }
  if (result == NULL || password.pw_name == NULL) {
    *sys_errno = ENOENT;
    return -1;
  }

  copied_len = strlen(password.pw_name);
  if (copied_len >= buffer_len) copied_len = buffer_len - 1U;
  memcpy(buffer, password.pw_name, copied_len);
  buffer[copied_len] = '\0';
  *value_len = copied_len;
  return 0;
}

int ftop_linux_read_cpu_frequency(int cpu_index, char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
  char path[128];
  int written;
  int rc;

  if (cpu_index < 0) {
    if (sys_errno != NULL) *sys_errno = EINVAL;
    return -1;
  }

  written = snprintf(path, sizeof(path), "/sys/devices/system/cpu/cpu%d/cpufreq/scaling_cur_freq", cpu_index);
  if (written < 0 || (size_t)written >= sizeof(path)) {
    if (sys_errno != NULL) *sys_errno = EOVERFLOW;
    return -1;
  }
  rc = ftop_read_file_into_buffer(path, buffer, buffer_len, value_len, sys_errno);
  if (rc == 0) return 0;

  written = snprintf(path, sizeof(path), "/sys/devices/system/cpu/cpu%d/cpufreq/cpuinfo_cur_freq", cpu_index);
  if (written < 0 || (size_t)written >= sizeof(path)) {
    if (sys_errno != NULL) *sys_errno = EOVERFLOW;
    return -1;
  }
  return ftop_read_file_into_buffer(path, buffer, buffer_len, value_len, sys_errno);
}

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

int ftop_linux_cpu_ticks(long long *total_ticks, long long *idle_ticks, int *sys_errno) {
  char buffer[4096];
  long long user;
  long long nice;
  long long system;
  long long idle;
  long long iowait;
  long long irq;
  long long softirq;
  long long steal;
  ssize_t bytes_read;
  int fd;
  int fields;

  if (total_ticks == NULL || idle_ticks == NULL || sys_errno == NULL) return -1;

  *total_ticks = 0;
  *idle_ticks = 0;
  *sys_errno = 0;
  fd = open("/proc/stat", O_RDONLY);
  if (fd < 0) {
    *sys_errno = errno;
    return -1;
  }

  bytes_read = read(fd, buffer, sizeof(buffer) - 1U);
  if (bytes_read < 0) {
    *sys_errno = errno;
    close(fd);
    return -1;
  }
  close(fd);

  buffer[bytes_read] = '\0';
  fields = sscanf(buffer, "cpu %lld %lld %lld %lld %lld %lld %lld %lld", &user, &nice, &system,
                  &idle, &iowait, &irq, &softirq, &steal);
  if (fields < 4) {
    *sys_errno = EINVAL;
    return -1;
  }
  if (fields < 5) iowait = 0;
  if (fields < 6) irq = 0;
  if (fields < 7) softirq = 0;
  if (fields < 8) steal = 0;

  *idle_ticks = idle + iowait;
  *total_ticks = user + nice + system + idle + iowait + irq + softirq + steal;
  if (*total_ticks <= 0) {
    *sys_errno = EINVAL;
    return -1;
  }

  return 0;
}

static long long ftop_linux_meminfo_value(const char *buffer, const char *key) {
  const char *line;
  const char *cursor;
  long long value;

  line = buffer;
  while (*line != '\0') {
    cursor = key;
    while (*cursor != '\0' && line[cursor - key] == *cursor) ++cursor;
    if (*cursor == '\0') {
      value = 0;
      if (sscanf(line + (cursor - key), " %lld kB", &value) == 1) return value * 1024LL;
      return 0;
    }

    while (*line != '\0' && *line != '\n') ++line;
    if (*line == '\n') ++line;
  }

  return 0;
}

int ftop_linux_memory_info(long long *total_bytes, long long *available_bytes, int *sys_errno) {
  char buffer[8192];
  long long mem_free;
  long long buffers;
  long long cached;
  ssize_t bytes_read;
  int fd;

  if (total_bytes == NULL || available_bytes == NULL || sys_errno == NULL) return -1;

  *total_bytes = 0;
  *available_bytes = 0;
  *sys_errno = 0;
  fd = open("/proc/meminfo", O_RDONLY);
  if (fd < 0) {
    *sys_errno = errno;
    return -1;
  }

  bytes_read = read(fd, buffer, sizeof(buffer) - 1U);
  if (bytes_read < 0) {
    *sys_errno = errno;
    close(fd);
    return -1;
  }
  close(fd);

  buffer[bytes_read] = '\0';
  *total_bytes = ftop_linux_meminfo_value(buffer, "MemTotal:");
  *available_bytes = ftop_linux_meminfo_value(buffer, "MemAvailable:");
  if (*available_bytes <= 0) {
    mem_free = ftop_linux_meminfo_value(buffer, "MemFree:");
    buffers = ftop_linux_meminfo_value(buffer, "Buffers:");
    cached = ftop_linux_meminfo_value(buffer, "Cached:");
    *available_bytes = mem_free + buffers + cached;
  }

  if (*total_bytes <= 0) {
    *sys_errno = EINVAL;
    return -1;
  }
  if (*available_bytes > *total_bytes) *available_bytes = *total_bytes;

  return 0;
}

static char *ftop_read_file_once(const char *path, size_t capacity, ssize_t *bytes_read, int *sys_errno) {
  char *buffer;
  int fd;

  buffer = malloc(capacity + 1U);
  if (buffer == NULL) {
    *sys_errno = errno != 0 ? errno : ENOMEM;
    return NULL;
  }

  fd = open(path, O_RDONLY);
  if (fd < 0) {
    *sys_errno = errno;
    free(buffer);
    return NULL;
  }

  *bytes_read = read(fd, buffer, capacity);
  if (*bytes_read < 0) {
    *sys_errno = errno;
    close(fd);
    free(buffer);
    return NULL;
  }

  close(fd);
  buffer[*bytes_read] = '\0';
  return buffer;
}

static int ftop_cpuinfo_match_line(
    const char *line, size_t line_len, const char *field, size_t field_len, const char **value, size_t *value_len) {
  size_t colon;
  size_t key_end;
  size_t key_start;
  size_t value_end;
  size_t value_start;

  key_start = 0U;
  while (key_start < line_len && (line[key_start] == ' ' || line[key_start] == '\t')) ++key_start;
  colon = key_start;
  while (colon < line_len && line[colon] != ':') ++colon;
  if (colon >= line_len) return 0;

  key_end = colon;
  while (key_end > key_start && (line[key_end - 1U] == ' ' || line[key_end - 1U] == '\t')) --key_end;
  if (key_end - key_start != field_len) return 0;
  if (strncmp(line + key_start, field, field_len) != 0) return 0;

  value_start = colon + 1U;
  while (value_start < line_len && (line[value_start] == ' ' || line[value_start] == '\t')) ++value_start;
  value_end = line_len;
  while (value_end > value_start &&
         (line[value_end - 1U] == ' ' || line[value_end - 1U] == '\t' || line[value_end - 1U] == '\r')) {
    --value_end;
  }

  *value = line + value_start;
  *value_len = value_end - value_start;
  return 1;
}

int ftop_linux_cpuinfo_field(const char *field_name, char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
  const char *line;
  const char *line_end;
  const char *value;
  char *cpuinfo;
  size_t copied_len;
  size_t field_len;
  size_t line_len;
  size_t matched_len;
  ssize_t bytes_read;

  if (field_name == NULL || buffer == NULL || value_len == NULL || sys_errno == NULL || buffer_len == 0) return -1;

  buffer[0] = '\0';
  *value_len = 0U;
  *sys_errno = 0;
  field_len = strlen(field_name);
  cpuinfo = ftop_read_file_once("/proc/cpuinfo", FTOP_LINUX_CPUINFO_BUFFER_LEN, &bytes_read, sys_errno);
  if (cpuinfo == NULL) return -1;

  line = cpuinfo;
  while (*line != '\0') {
    line_end = strchr(line, '\n');
    if (line_end == NULL) line_end = line + strlen(line);
    line_len = (size_t)(line_end - line);
    if (ftop_cpuinfo_match_line(line, line_len, field_name, field_len, &value, &matched_len)) {
      copied_len = matched_len < buffer_len - 1U ? matched_len : buffer_len - 1U;
      memcpy(buffer, value, copied_len);
      buffer[copied_len] = '\0';
      *value_len = copied_len;
      free(cpuinfo);
      return 0;
    }
    line = *line_end == '\n' ? line_end + 1 : line_end;
  }

  free(cpuinfo);
  *sys_errno = ENOENT;
  return -1;
}

int ftop_linux_cpuinfo_field_count(const char *field_name, int *count, int *sys_errno) {
  const char *line;
  const char *line_end;
  const char *value;
  char *cpuinfo;
  size_t field_len;
  size_t line_len;
  size_t value_len;
  ssize_t bytes_read;

  if (field_name == NULL || count == NULL || sys_errno == NULL) return -1;

  *count = 0;
  *sys_errno = 0;
  field_len = strlen(field_name);
  cpuinfo = ftop_read_file_once("/proc/cpuinfo", FTOP_LINUX_CPUINFO_BUFFER_LEN, &bytes_read, sys_errno);
  if (cpuinfo == NULL) return -1;

  line = cpuinfo;
  while (*line != '\0') {
    line_end = strchr(line, '\n');
    if (line_end == NULL) line_end = line + strlen(line);
    line_len = (size_t)(line_end - line);
    if (ftop_cpuinfo_match_line(line, line_len, field_name, field_len, &value, &value_len)) ++(*count);
    line = *line_end == '\n' ? line_end + 1 : line_end;
  }

  free(cpuinfo);
  return 0;
}

static void ftop_copy_string(char *destination, size_t destination_len, const char *source) {
  size_t i;

  for (i = 0U; i + 1U < destination_len && source[i] != '\0' && source[i] != '\n'; ++i) destination[i] = source[i];
  destination[i] = '\0';
}

static long long ftop_linux_blocks_to_bytes(unsigned long blocks, unsigned long block_size) {
  unsigned long long value;

  if (blocks == 0UL || block_size == 0UL) return 0LL;
  if ((unsigned long long)blocks > (unsigned long long)LLONG_MAX / (unsigned long long)block_size) return LLONG_MAX;
  value = (unsigned long long)blocks * (unsigned long long)block_size;
  return value > (unsigned long long)LLONG_MAX ? LLONG_MAX : (long long)value;
}

static void ftop_linux_copy_filesystem_info(
    struct ftop_filesystem_info *destination, const struct mntent *mount, const struct statvfs *stats) {
  long long free_bytes;

  memset(destination, 0, sizeof(*destination));
  destination->valid = 1;
  ftop_copy_string(destination->device, sizeof(destination->device), mount->mnt_fsname != NULL ? mount->mnt_fsname : "");
  ftop_copy_string(
      destination->mountpoint, sizeof(destination->mountpoint), mount->mnt_dir != NULL ? mount->mnt_dir : "");
  ftop_copy_string(destination->fstype, sizeof(destination->fstype), mount->mnt_type != NULL ? mount->mnt_type : "");
  destination->total_bytes = ftop_linux_blocks_to_bytes(stats->f_blocks, stats->f_frsize);
  free_bytes = ftop_linux_blocks_to_bytes(stats->f_bfree, stats->f_frsize);
  destination->available_bytes = ftop_linux_blocks_to_bytes(stats->f_bavail, stats->f_frsize);
  destination->used_bytes = destination->total_bytes > free_bytes ? destination->total_bytes - free_bytes : 0LL;
}

int ftop_linux_filesystems(
    struct ftop_filesystem_info *filesystems, int capacity, int *filesystem_count, int *sys_errno) {
  FILE *mounts;
  struct mntent *mount;
  struct statvfs stats;
  int count;

  if (filesystems == NULL || filesystem_count == NULL || sys_errno == NULL || capacity < 0) return -1;
  memset(filesystems, 0, (size_t)capacity * sizeof(*filesystems));
  *filesystem_count = 0;
  *sys_errno = 0;

  mounts = setmntent("/proc/mounts", "r");
  if (mounts == NULL) {
    *sys_errno = errno != 0 ? errno : EINVAL;
    return -1;
  }

  count = 0;
  while (count < capacity && (mount = getmntent(mounts)) != NULL) {
    if (mount->mnt_dir == NULL || statvfs(mount->mnt_dir, &stats) != 0) continue;
    ftop_linux_copy_filesystem_info(&filesystems[count], mount, &stats);
    ++count;
  }
  endmntent(mounts);
  *filesystem_count = count;
  return 0;
}

static int ftop_read_hwmon_name(const char *path, char *name, size_t name_len) {
  char buffer[FTOP_LINUX_HWMON_NAME_LEN];
  ssize_t bytes_read;
  int fd;

  fd = open(path, O_RDONLY);
  if (fd < 0) return -1;
  bytes_read = read(fd, buffer, sizeof(buffer) - 1U);
  close(fd);
  if (bytes_read <= 0) return -1;
  buffer[bytes_read] = '\0';
  ftop_copy_string(name, name_len, buffer);
  return 0;
}

static int ftop_linux_is_drm_card_name(const char *name) {
  size_t index;

  if (name == NULL || strncmp(name, "card", 4U) != 0) return 0;
  index = 4U;
  if (!isdigit((unsigned char)name[index])) return 0;
  while (isdigit((unsigned char)name[index])) ++index;
  return name[index] == '\0';
}

int ftop_linux_drm_card_discover(
    const char *root, struct ftop_linux_drm_card *buffer, size_t capacity, size_t *card_count, int *sys_errno) {
  char device_path[FTOP_LINUX_DRM_DEVICE_PATH_LEN];
  DIR *directory;
  struct dirent *entry;
  size_t count;
  int written;

  if (root == NULL || buffer == NULL || card_count == NULL || sys_errno == NULL || capacity == 0U) return -1;

  *card_count = 0U;
  *sys_errno = 0;
  directory = opendir(root);
  if (directory == NULL) {
    if (errno == ENOENT) return 0;
    *sys_errno = errno;
    return -1;
  }

  count = 0U;
  while ((entry = readdir(directory)) != NULL) {
    if (!ftop_linux_is_drm_card_name(entry->d_name)) continue;
    written = snprintf(device_path, sizeof(device_path), "%s/%s/device", root, entry->d_name);
    if (written < 0 || (size_t)written >= sizeof(device_path)) continue;
    if (access(device_path, F_OK) != 0) continue;
    if (count < capacity) {
      ftop_copy_string(buffer[count].name, sizeof(buffer[count].name), entry->d_name);
      ftop_copy_string(buffer[count].device_path, sizeof(buffer[count].device_path), device_path);
    }
    ++count;
  }

  closedir(directory);
  *card_count = count < capacity ? count : capacity;
  return 0;
}

static int ftop_linux_hwmon_discover_root(
    const char *root, struct ftop_linux_hwmon_sensor *buffer, size_t capacity, size_t *sensor_count, int *sys_errno) {
  char name_path[512];
  char sensor_path[512];
  DIR *directory;
  struct dirent *entry;
  size_t count;
  int written;

  if (root == NULL || buffer == NULL || sensor_count == NULL || sys_errno == NULL || capacity == 0U) return -1;

  *sensor_count = 0U;
  *sys_errno = 0;
  directory = opendir(root);
  if (directory == NULL) {
    if (errno == ENOENT) return 0;
    *sys_errno = errno;
    return -1;
  }

  count = 0U;
  while ((entry = readdir(directory)) != NULL) {
    if (strncmp(entry->d_name, "hwmon", 5U) != 0) continue;
    written = snprintf(sensor_path, sizeof(sensor_path), "%s/%s", root, entry->d_name);
    if (written < 0 || (size_t)written >= sizeof(sensor_path)) continue;
    written = snprintf(name_path, sizeof(name_path), "%s/name", sensor_path);
    if (written < 0 || (size_t)written >= sizeof(name_path)) continue;
    if (count < capacity) {
      ftop_copy_string(buffer[count].path, sizeof(buffer[count].path), sensor_path);
      if (ftop_read_hwmon_name(name_path, buffer[count].name, sizeof(buffer[count].name)) != 0) {
        ftop_copy_string(buffer[count].name, sizeof(buffer[count].name), entry->d_name);
      }
    }
    ++count;
  }

  closedir(directory);
  *sensor_count = count < capacity ? count : capacity;
  return 0;
}

int ftop_linux_hwmon_discover(struct ftop_linux_hwmon_sensor *buffer, size_t capacity, size_t *sensor_count, int *sys_errno) {
  return ftop_linux_hwmon_discover_root("/sys/class/hwmon", buffer, capacity, sensor_count, sys_errno);
}

int ftop_linux_hwmon_discover_at(
    const char *root, struct ftop_linux_hwmon_sensor *buffer, size_t capacity, size_t *sensor_count, int *sys_errno) {
  return ftop_linux_hwmon_discover_root(root, buffer, capacity, sensor_count, sys_errno);
}

static int ftop_linux_is_cpu_hwmon_name(const char *name) {
  return strcmp(name, "coretemp") == 0 || strcmp(name, "k10temp") == 0 || strcmp(name, "zenpower") == 0 ||
         strcmp(name, "cpu_thermal") == 0 || strcmp(name, "soc_thermal") == 0;
}

static int ftop_linux_is_temp_input_name(const char *name) {
  size_t len;

  if (strncmp(name, "temp", 4U) != 0) return 0;
  len = strlen(name);
  if (len <= 10U) return 0;
  return strcmp(name + len - 6U, "_input") == 0;
}

static int ftop_linux_read_temperature_millicelsius(const char *path, long long *temperature) {
  char buffer[64];
  char *end;
  size_t value_len;
  long long value;
  int sys_errno;

  if (ftop_read_file_into_buffer(path, buffer, sizeof(buffer), &value_len, &sys_errno) != 0) return -1;
  errno = 0;
  value = strtoll(buffer, &end, 10);
  if (end == buffer || errno != 0) return -1;
  if (value < -100000LL || value > 150000LL) return -1;

  *temperature = value;
  return 0;
}

static int ftop_linux_scan_cpu_hwmon_temperatures(const char *sensor_path, long long *max_temperature) {
  char input_path[512];
  DIR *directory;
  struct dirent *entry;
  long long temperature;
  int found;
  int written;

  directory = opendir(sensor_path);
  if (directory == NULL) return 0;

  found = 0;
  while ((entry = readdir(directory)) != NULL) {
    if (!ftop_linux_is_temp_input_name(entry->d_name)) continue;
    written = snprintf(input_path, sizeof(input_path), "%s/%s", sensor_path, entry->d_name);
    if (written < 0 || (size_t)written >= sizeof(input_path)) continue;
    if (ftop_linux_read_temperature_millicelsius(input_path, &temperature) != 0) continue;
    if (!found || temperature > *max_temperature) *max_temperature = temperature;
    found = 1;
  }

  closedir(directory);
  return found;
}

int ftop_linux_read_cpu_temperature(double *temperature_c, int *sys_errno) {
  char name[FTOP_LINUX_HWMON_NAME_LEN];
  char name_path[512];
  char sensor_path[512];
  DIR *directory;
  struct dirent *entry;
  long long max_temperature;
  int found;
  int written;

  if (temperature_c == NULL || sys_errno == NULL) return -1;

  *temperature_c = 0.0;
  *sys_errno = 0;
  directory = opendir("/sys/class/hwmon");
  if (directory == NULL) {
    *sys_errno = errno;
    return -1;
  }

  found = 0;
  max_temperature = 0;
  while ((entry = readdir(directory)) != NULL) {
    if (strncmp(entry->d_name, "hwmon", 5U) != 0) continue;
    written = snprintf(sensor_path, sizeof(sensor_path), "/sys/class/hwmon/%s", entry->d_name);
    if (written < 0 || (size_t)written >= sizeof(sensor_path)) continue;
    written = snprintf(name_path, sizeof(name_path), "%s/name", sensor_path);
    if (written < 0 || (size_t)written >= sizeof(name_path)) continue;
    if (ftop_read_hwmon_name(name_path, name, sizeof(name)) != 0) continue;
    if (!ftop_linux_is_cpu_hwmon_name(name)) continue;
    if (ftop_linux_scan_cpu_hwmon_temperatures(sensor_path, &max_temperature)) found = 1;
  }

  closedir(directory);
  if (!found) {
    *sys_errno = ENOENT;
    return -1;
  }

  *temperature_c = (double)max_temperature / 1000.0;
  return 0;
}
