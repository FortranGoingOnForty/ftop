#include <arpa/inet.h>
#include <errno.h>
#include <devstat.h>
#include <fcntl.h>
#include <ifaddrs.h>
#include <kvm.h>
#include <limits.h>
#include <net/if.h>
#include <netinet/in.h>
#include <netinet/in_systm.h>
#include <netinet/ip.h>
#include <netinet/tcp.h>
#include <netinet/tcp_seq.h>
#include <pwd.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>
#include <sys/queue.h>
#include <sys/domain.h>
#include <sys/mount.h>
#define _WANT_PROTOSW
#include <sys/protosw.h>
#include <sys/resource.h>
#include <sys/socket.h>
#define _WANT_SOCKET
#include <sys/socketvar.h>
#include <sys/sysctl.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/user.h>
#include <netinet/in_pcb.h>
#include <netinet/tcp_fsm.h>
#include <netinet/tcp_var.h>
#include <libprocstat.h>
#include <time.h>
#include <unistd.h>

#define FTOP_FREEBSD_COMMAND_LEN 32
#define FTOP_FREEBSD_DEVSTAT_NAME_LEN 16
#define FTOP_NET_PROTOCOL_LEN 8
#define FTOP_NET_ADDRESS_LEN 64
#define FTOP_NET_INTERFACE_NAME_LEN 32
#define FTOP_NET_PROCESS_NAME_LEN 64
#define FTOP_NET_STATE_LEN 16
#define FTOP_DISK_DEVICE_LEN 64
#define FTOP_DISK_MOUNTPOINT_LEN 128
#define FTOP_DISK_FSTYPE_LEN 32
#define FTOP_USER_LOOKUP_BUFFER_LEN 16384

struct ftop_freebsd_process_info {
  int pid;
  int ppid;
  int uid;
  int state;
  int jid;
  char command[FTOP_FREEBSD_COMMAND_LEN];
  long long mem_rss_bytes;
  long long mem_virt_bytes;
  int threads;
  int nice;
  int priority;
  long long start_time;
  long long cpu_time;
};

struct ftop_freebsd_devstat_info {
  int device_number;
  int unit_number;
  int device_type;
  int priority;
  int block_size;
  long long bytes_read;
  long long bytes_written;
  long long bytes_freed;
  long long transfers_read;
  long long transfers_written;
  long long transfers_freed;
  char name[FTOP_FREEBSD_DEVSTAT_NAME_LEN];
};

struct ftop_freebsd_net_interface_info {
  int valid;
  char name[FTOP_NET_INTERFACE_NAME_LEN];
  long long rx_bytes;
  long long tx_bytes;
  long long rx_packets;
  long long tx_packets;
  char state[FTOP_NET_STATE_LEN];
  int speed_mbps;
  int mtu;
};

struct ftop_freebsd_net_connection_info {
  int valid;
  char protocol[FTOP_NET_PROTOCOL_LEN];
  char local_addr[FTOP_NET_ADDRESS_LEN];
  int local_port;
  char remote_addr[FTOP_NET_ADDRESS_LEN];
  int remote_port;
  char state[FTOP_NET_STATE_LEN];
  long long socket_id;
  int pid;
  char process_name[FTOP_NET_PROCESS_NAME_LEN];
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

#ifndef CPUSTATES
#define CPUSTATES 5
#endif

#ifndef CP_USER
#define CP_USER 0
#endif

#ifndef CP_NICE
#define CP_NICE 1
#endif

#ifndef CP_SYS
#define CP_SYS 2
#endif

#ifndef CP_INTR
#define CP_INTR 3
#endif

#ifndef CP_IDLE
#define CP_IDLE 4
#endif

struct ftop_freebsd_cpu_state_ticks {
  long long user;
  long long nice;
  long long system;
  long long idle;
  long long irq;
};

int ftop_freebsd_cpu_count(int *count, int *sys_errno) {
  int value;
  size_t value_len;

  if (count == NULL || sys_errno == NULL) return -1;

  *count = 0;
  *sys_errno = 0;
  value = 0;
  value_len = sizeof(value);
  if (sysctlbyname("kern.smp.cpus", &value, &value_len, NULL, 0) != 0) {
    *sys_errno = errno;
    return -1;
  }

  if (value <= 0) {
    *sys_errno = EINVAL;
    return -1;
  }

  *count = value;
  return 0;
}

int ftop_freebsd_cpu_ticks(long long *total_ticks, long long *idle_ticks, int *sys_errno) {
  long cpu_time[CPUSTATES];
  long long total;
  size_t cpu_time_len;
  int i;

  if (total_ticks == NULL || idle_ticks == NULL || sys_errno == NULL) return -1;

  *total_ticks = 0;
  *idle_ticks = 0;
  *sys_errno = 0;
  cpu_time_len = sizeof(cpu_time);
  if (sysctlbyname("kern.cp_time", cpu_time, &cpu_time_len, NULL, 0) != 0) {
    *sys_errno = errno;
    return -1;
  }

  total = 0;
  for (i = 0; i < CPUSTATES; ++i) {
    if (cpu_time[i] > 0) total += (long long)cpu_time[i];
  }

  if (total <= 0) {
    *sys_errno = EINVAL;
    return -1;
  }

  *total_ticks = total;
  *idle_ticks = cpu_time[CP_IDLE] > 0 ? (long long)cpu_time[CP_IDLE] : 0;
  return 0;
}

static long long ftop_nonnegative_long_tick(long value) {
  return value > 0 ? (long long)value : 0;
}

int ftop_freebsd_cpu_state_ticks(
    struct ftop_freebsd_cpu_state_ticks *buffer, size_t capacity, size_t *cpu_count, int *sys_errno) {
  long *cpu_times;
  size_t cpu_times_len;
  size_t available_cpus;
  size_t copy_count;
  size_t i;

  if (buffer == NULL || cpu_count == NULL || sys_errno == NULL || capacity == 0) return -1;

  *cpu_count = 0U;
  *sys_errno = 0;
  cpu_times = NULL;
  cpu_times_len = 0U;
  if (sysctlbyname("kern.cp_times", NULL, &cpu_times_len, NULL, 0) != 0) {
    *sys_errno = errno;
    return -1;
  }
  if (cpu_times_len < sizeof(long) * CPUSTATES) {
    *sys_errno = EINVAL;
    return -1;
  }

  cpu_times = (long *)malloc(cpu_times_len);
  if (cpu_times == NULL) {
    *sys_errno = ENOMEM;
    return -1;
  }

  if (sysctlbyname("kern.cp_times", cpu_times, &cpu_times_len, NULL, 0) != 0) {
    *sys_errno = errno;
    free(cpu_times);
    return -1;
  }

  available_cpus = cpu_times_len / (sizeof(long) * CPUSTATES);
  copy_count = available_cpus < capacity ? available_cpus : capacity;
  for (i = 0U; i < copy_count; ++i) {
    long *cpu = &cpu_times[i * CPUSTATES];
    buffer[i].user = ftop_nonnegative_long_tick(cpu[CP_USER]);
    buffer[i].nice = ftop_nonnegative_long_tick(cpu[CP_NICE]);
    buffer[i].system = ftop_nonnegative_long_tick(cpu[CP_SYS]);
    buffer[i].idle = ftop_nonnegative_long_tick(cpu[CP_IDLE]);
    buffer[i].irq = ftop_nonnegative_long_tick(cpu[CP_INTR]);
  }

  free(cpu_times);
  if (copy_count == 0U) {
    *sys_errno = EINVAL;
    return -1;
  }
  *cpu_count = copy_count;
  return 0;
}

static int ftop_freebsd_sysctl_int_value(const char *name, int *value, int *sys_errno) {
  size_t value_len;

  if (name == NULL || value == NULL || sys_errno == NULL) return -1;

  *value = 0;
  *sys_errno = 0;
  value_len = sizeof(*value);
  if (sysctlbyname(name, value, &value_len, NULL, 0) != 0) {
    *sys_errno = errno;
    return -1;
  }
  if (value_len != sizeof(*value)) {
    *sys_errno = EOVERFLOW;
    return -1;
  }

  return 0;
}

int ftop_freebsd_sysctl_int(const char *name, int *value, int *sys_errno) {
  return ftop_freebsd_sysctl_int_value(name, value, sys_errno);
}

int ftop_freebsd_cpu_frequency(int cpu_index, int *freq_mhz, int *sys_errno) {
  char name[64];
  int written;

  if (freq_mhz == NULL || sys_errno == NULL) return -1;
  *freq_mhz = 0;
  *sys_errno = 0;
  if (cpu_index < 0) {
    *sys_errno = EINVAL;
    return -1;
  }

  written = snprintf(name, sizeof(name), "dev.cpu.%d.freq", cpu_index);
  if (written < 0 || (size_t)written >= sizeof(name)) {
    *sys_errno = EOVERFLOW;
    return -1;
  }

  return ftop_freebsd_sysctl_int_value(name, freq_mhz, sys_errno);
}

static int ftop_freebsd_deci_kelvin_to_celsius(int raw_temperature, double *temperature_c, int *sys_errno) {
  double value;

  value = ((double)raw_temperature / 10.0) - 273.15;
  if (value < -100.0 || value > 150.0) {
    *sys_errno = EINVAL;
    return -1;
  }

  *temperature_c = value;
  return 0;
}

int ftop_freebsd_cpu_temperature(int cpu_index, double *temperature_c, int *sys_errno) {
  char name[64];
  int raw_temperature;
  int written;

  if (temperature_c == NULL || sys_errno == NULL) return -1;
  *temperature_c = 0.0;
  *sys_errno = 0;
  if (cpu_index < 0) {
    *sys_errno = EINVAL;
    return -1;
  }

  written = snprintf(name, sizeof(name), "dev.cpu.%d.temperature", cpu_index);
  if (written < 0 || (size_t)written >= sizeof(name)) {
    *sys_errno = EOVERFLOW;
    return -1;
  }
  if (ftop_freebsd_sysctl_int_value(name, &raw_temperature, sys_errno) != 0) return -1;

  return ftop_freebsd_deci_kelvin_to_celsius(raw_temperature, temperature_c, sys_errno);
}

int ftop_freebsd_acpi_temperature(double *temperature_c, int *sys_errno) {
  int raw_temperature;

  if (temperature_c == NULL || sys_errno == NULL) return -1;
  *temperature_c = 0.0;
  *sys_errno = 0;
  if (ftop_freebsd_sysctl_int_value("hw.acpi.thermal.tz0.temperature", &raw_temperature, sys_errno) != 0) return -1;

  return ftop_freebsd_deci_kelvin_to_celsius(raw_temperature, temperature_c, sys_errno);
}

int ftop_freebsd_sysctl_long(const char *name, long *value, int *sys_errno) {
  size_t value_len;

  if (name == NULL || value == NULL || sys_errno == NULL) return -1;

  *value = 0L;
  *sys_errno = 0;
  value_len = sizeof(*value);
  if (sysctlbyname(name, value, &value_len, NULL, 0) != 0) {
    *sys_errno = errno;
    return -1;
  }
  if (value_len != sizeof(*value)) {
    *sys_errno = EOVERFLOW;
    return -1;
  }

  return 0;
}

int ftop_freebsd_sysctl_string(const char *name, char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
  size_t actual_len;

  if (name == NULL || buffer == NULL || value_len == NULL || sys_errno == NULL || buffer_len == 0) return -1;

  buffer[0] = '\0';
  *value_len = 0U;
  *sys_errno = 0;
  actual_len = buffer_len;
  if (sysctlbyname(name, buffer, &actual_len, NULL, 0) != 0) {
    *sys_errno = errno;
    return -1;
  }

  if (actual_len == 0 || buffer[actual_len - 1U] != '\0') {
    if (actual_len < buffer_len) buffer[actual_len] = '\0';
    if (actual_len >= buffer_len) buffer[buffer_len - 1U] = '\0';
  }
  *value_len = strnlen(buffer, buffer_len);

  return 0;
}

int ftop_freebsd_sysctl_bytes(const char *name, void *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
  size_t actual_len;

  if (name == NULL || buffer == NULL || value_len == NULL || sys_errno == NULL || buffer_len == 0) return -1;

  *value_len = 0U;
  *sys_errno = 0;
  actual_len = buffer_len;
  if (sysctlbyname(name, buffer, &actual_len, NULL, 0) != 0) {
    *sys_errno = errno;
    return -1;
  }

  *value_len = actual_len;
  return 0;
}

void *ftop_freebsd_kvm_open(int *sys_errno) {
  char error_buffer[256];
  kvm_t *handle;

  if (sys_errno == NULL) return NULL;

  *sys_errno = 0;
  error_buffer[0] = '\0';
  handle = kvm_openfiles(NULL, "/dev/null", NULL, O_RDONLY, error_buffer);
  if (handle == NULL) {
    *sys_errno = errno != 0 ? errno : ENOENT;
    return NULL;
  }

  return handle;
}

int ftop_freebsd_kvm_close(void *handle, int *sys_errno) {
  if (handle == NULL || sys_errno == NULL) return -1;

  *sys_errno = 0;
  if (kvm_close((kvm_t *)handle) != 0) {
    *sys_errno = errno;
    return -1;
  }

  return 0;
}

static void ftop_copy_process_command(char *destination, const char *source) {
  size_t i;

  for (i = 0U; i + 1U < FTOP_FREEBSD_COMMAND_LEN && source[i] != '\0'; ++i) destination[i] = source[i];
  destination[i] = '\0';
}

int ftop_freebsd_kvm_getprocs(
    void *handle, struct ftop_freebsd_process_info *buffer, size_t capacity, size_t *process_count, int *sys_errno) {
  struct kinfo_proc *processes;
  long long page_size;
  size_t copy_count;
  int count;
  size_t i;

  if (handle == NULL || buffer == NULL || process_count == NULL || sys_errno == NULL || capacity == 0) return -1;

  *process_count = 0U;
  *sys_errno = 0;
  count = 0;
  processes = kvm_getprocs((kvm_t *)handle, KERN_PROC_PROC, 0, &count);
  if (processes == NULL || count < 0) {
    *sys_errno = errno != 0 ? errno : EINVAL;
    return -1;
  }

  page_size = (long long)getpagesize();
  copy_count = (size_t)count < capacity ? (size_t)count : capacity;
  for (i = 0U; i < copy_count; ++i) {
    buffer[i].pid = (int)processes[i].ki_pid;
    buffer[i].ppid = (int)processes[i].ki_ppid;
    buffer[i].uid = (int)processes[i].ki_uid;
    buffer[i].state = (int)processes[i].ki_stat;
    buffer[i].jid = processes[i].ki_jid > 0 ? (int)processes[i].ki_jid : 0;
    ftop_copy_process_command(buffer[i].command, processes[i].ki_comm);
    buffer[i].mem_rss_bytes = processes[i].ki_rssize > 0 ? (long long)processes[i].ki_rssize * page_size : 0;
    buffer[i].mem_virt_bytes = processes[i].ki_size > 0 ? (long long)processes[i].ki_size : 0;
    buffer[i].threads = processes[i].ki_numthreads > 0 ? (int)processes[i].ki_numthreads : 0;
    buffer[i].nice = (int)processes[i].ki_nice;
    buffer[i].priority = (int)processes[i].ki_pri.pri_level;
    buffer[i].start_time = processes[i].ki_start.tv_sec > 0 ? (long long)processes[i].ki_start.tv_sec : 0;
    buffer[i].cpu_time = processes[i].ki_runtime > 0 ? (long long)(processes[i].ki_runtime / 1000ULL) : 0;
  }

  *process_count = copy_count;
  return 0;
}

static void ftop_copy_devstat_name(char *destination, const char *source) {
  size_t i;

  for (i = 0U; i + 1U < FTOP_FREEBSD_DEVSTAT_NAME_LEN && source[i] != '\0'; ++i) destination[i] = source[i];
  destination[i] = '\0';
}

static void ftop_copy_devstat_device(struct ftop_freebsd_devstat_info *destination, const struct devstat *source) {
  destination->device_number = (int)source->device_number;
  destination->unit_number = source->unit_number;
  destination->device_type = (int)source->device_type;
  destination->priority = (int)source->priority;
  destination->block_size = (int)source->block_size;
  destination->bytes_read = (long long)source->bytes[DEVSTAT_READ];
  destination->bytes_written = (long long)source->bytes[DEVSTAT_WRITE];
  destination->bytes_freed = (long long)source->bytes[DEVSTAT_FREE];
  destination->transfers_read = (long long)source->operations[DEVSTAT_READ];
  destination->transfers_written = (long long)source->operations[DEVSTAT_WRITE];
  destination->transfers_freed = (long long)source->operations[DEVSTAT_FREE];
  ftop_copy_devstat_name(destination->name, source->device_name);
}

int ftop_freebsd_devstat_getdevs(
    struct ftop_freebsd_devstat_info *buffer, size_t capacity, size_t *device_count, long long *generation, int *sys_errno) {
  struct devinfo devinfo;
  struct statinfo statinfo;
  size_t copy_count;
  int rc;
  size_t i;

  if (buffer == NULL || device_count == NULL || generation == NULL || sys_errno == NULL || capacity == 0) return -1;

  *device_count = 0U;
  *generation = 0;
  *sys_errno = 0;
  memset(&devinfo, 0, sizeof(devinfo));
  memset(&statinfo, 0, sizeof(statinfo));
  statinfo.dinfo = &devinfo;

  if (devstat_checkversion(NULL) != 0) {
    *sys_errno = EINVAL;
    return -1;
  }

  rc = devstat_getdevs(NULL, &statinfo);
  if (rc < 0) {
    *sys_errno = errno != 0 ? errno : EINVAL;
    free(devinfo.mem_ptr);
    return -1;
  }

  copy_count = (size_t)devinfo.numdevs < capacity ? (size_t)devinfo.numdevs : capacity;
  for (i = 0U; i < copy_count; ++i) ftop_copy_devstat_device(&buffer[i], &devinfo.devices[i]);

  *device_count = copy_count;
  *generation = (long long)devinfo.generation;
  free(devinfo.mem_ptr);
  return 0;
}

static void ftop_copy_bounded_string(char *destination, size_t capacity, const char *source) {
  size_t copied_len;
  size_t source_len;

  if (destination == NULL || capacity == 0U) return;
  destination[0] = '\0';
  if (source == NULL) return;
  source_len = strlen(source);
  copied_len = source_len < capacity - 1U ? source_len : capacity - 1U;
  memcpy(destination, source, copied_len);
  destination[copied_len] = '\0';
}

static long long ftop_blocks_to_bytes(long long blocks, unsigned long long block_size) {
  unsigned long long value;

  if (blocks <= 0 || block_size == 0ULL) return 0;
  if ((unsigned long long)blocks > (unsigned long long)LLONG_MAX / block_size) return LLONG_MAX;
  value = (unsigned long long)blocks * block_size;
  return value > (unsigned long long)LLONG_MAX ? LLONG_MAX : (long long)value;
}

static void ftop_copy_filesystem_info(struct ftop_filesystem_info *destination, const struct statfs *source) {
  long long block_size;
  long long free_bytes;

  memset(destination, 0, sizeof(*destination));
  if (source == NULL) return;
  block_size = source->f_bsize > 0 ? (long long)source->f_bsize : 0LL;
  destination->valid = 1;
  ftop_copy_bounded_string(destination->device, sizeof(destination->device), source->f_mntfromname);
  ftop_copy_bounded_string(destination->mountpoint, sizeof(destination->mountpoint), source->f_mntonname);
  ftop_copy_bounded_string(destination->fstype, sizeof(destination->fstype), source->f_fstypename);
  destination->total_bytes = ftop_blocks_to_bytes((long long)source->f_blocks, (unsigned long long)block_size);
  free_bytes = ftop_blocks_to_bytes((long long)source->f_bfree, (unsigned long long)block_size);
  destination->available_bytes = ftop_blocks_to_bytes((long long)source->f_bavail, (unsigned long long)block_size);
  destination->used_bytes = destination->total_bytes > free_bytes ? destination->total_bytes - free_bytes : 0LL;
}

int ftop_freebsd_filesystems(struct ftop_filesystem_info *filesystems, int capacity, int *filesystem_count,
    int *sys_errno) {
  struct statfs *mounts;
  int mount_count;
  int copy_count;
  int i;

  if (filesystems == NULL || filesystem_count == NULL || sys_errno == NULL || capacity < 0) return -1;
  memset(filesystems, 0, (size_t)capacity * sizeof(*filesystems));
  *filesystem_count = 0;
  *sys_errno = 0;

  mount_count = getmntinfo(&mounts, MNT_NOWAIT);
  if (mount_count <= 0) {
    *sys_errno = errno != 0 ? errno : EINVAL;
    return -1;
  }

  copy_count = mount_count < capacity ? mount_count : capacity;
  for (i = 0; i < copy_count; ++i) ftop_copy_filesystem_info(&filesystems[i], &mounts[i]);
  *filesystem_count = copy_count;
  return 0;
}

static long long ftop_nonnegative_unsigned_long_long(unsigned long long value) {
  return value > (unsigned long long)LLONG_MAX ? LLONG_MAX : (long long)value;
}

static int ftop_baud_to_mbps(unsigned long long baudrate) {
  unsigned long long mbps;

  mbps = baudrate / 1000000ULL;
  if (mbps > (unsigned long long)INT_MAX) return INT_MAX;
  return (int)mbps;
}

static int ftop_network_interface_exists(
    const struct ftop_freebsd_net_interface_info *buffer, size_t count, const char *name) {
  size_t i;

  for (i = 0U; i < count; ++i) {
    if (strncmp(buffer[i].name, name, FTOP_NET_INTERFACE_NAME_LEN) == 0) return 1;
  }
  return 0;
}

static void ftop_copy_network_interface(
    struct ftop_freebsd_net_interface_info *destination, const struct ifaddrs *source) {
  const struct if_data *data;
  unsigned long long baudrate;

  memset(destination, 0, sizeof(*destination));
  data = (const struct if_data *)source->ifa_data;
  destination->valid = 1;
  ftop_copy_bounded_string(destination->name, sizeof(destination->name), source->ifa_name);
  destination->rx_bytes = ftop_nonnegative_unsigned_long_long((unsigned long long)data->ifi_ibytes);
  destination->tx_bytes = ftop_nonnegative_unsigned_long_long((unsigned long long)data->ifi_obytes);
  destination->rx_packets = ftop_nonnegative_unsigned_long_long((unsigned long long)data->ifi_ipackets);
  destination->tx_packets = ftop_nonnegative_unsigned_long_long((unsigned long long)data->ifi_opackets);
  ftop_copy_bounded_string(destination->state, sizeof(destination->state),
      (source->ifa_flags & IFF_UP) != 0U ? "up" : "down");
  baudrate = (unsigned long long)data->ifi_baudrate;
  destination->speed_mbps = ftop_baud_to_mbps(baudrate);
  destination->mtu = data->ifi_mtu > 0 ? (int)data->ifi_mtu : 0;
}

int ftop_freebsd_network_interfaces(
    struct ftop_freebsd_net_interface_info *buffer, size_t capacity, size_t *interface_count, int *sys_errno) {
  struct ifaddrs *interfaces;
  struct ifaddrs *interface;
  size_t count;

  if (buffer == NULL || interface_count == NULL || sys_errno == NULL || capacity == 0U) return -1;

  *interface_count = 0U;
  *sys_errno = 0;
  interfaces = NULL;
  if (getifaddrs(&interfaces) != 0) {
    *sys_errno = errno;
    return -1;
  }

  count = 0U;
  for (interface = interfaces; interface != NULL && count < capacity; interface = interface->ifa_next) {
    if (interface->ifa_name == NULL || interface->ifa_addr == NULL || interface->ifa_data == NULL) continue;
    if (interface->ifa_addr->sa_family != AF_LINK) continue;
    if ((interface->ifa_flags & IFF_LOOPBACK) != 0U) continue;
    if (ftop_network_interface_exists(buffer, count, interface->ifa_name)) continue;
    ftop_copy_network_interface(&buffer[count], interface);
    ++count;
  }

  freeifaddrs(interfaces);
  *interface_count = count;
  return 0;
}

static long long ftop_uint64_to_nonnegative_long_long(uint64_t value) {
  return value > (uint64_t)LLONG_MAX ? LLONG_MAX : (long long)value;
}

static int ftop_freebsd_endpoint_from_sockaddr(
    const struct sockaddr_storage *storage, char *address, size_t address_capacity, int *port) {
  const struct sockaddr_in *ipv4;
  const struct sockaddr_in6 *ipv6;
  const void *source;
  int family;

  if (storage == NULL || address == NULL || port == NULL || address_capacity == 0U) return 0;
  address[0] = '\0';
  *port = 0;
  family = storage->ss_family;
  if (family == AF_INET) {
    ipv4 = (const struct sockaddr_in *)(const void *)storage;
    source = &ipv4->sin_addr;
    *port = (int)ntohs(ipv4->sin_port);
  } else if (family == AF_INET6) {
    ipv6 = (const struct sockaddr_in6 *)(const void *)storage;
    source = &ipv6->sin6_addr;
    *port = (int)ntohs(ipv6->sin6_port);
  } else {
    return 0;
  }
  return inet_ntop(family, source, address, (socklen_t)address_capacity) != NULL;
}

static void ftop_freebsd_unspecified_address(int family, char *address, size_t address_capacity, int *port) {
  if (address == NULL || port == NULL || address_capacity == 0U) return;
  *port = 0;
  ftop_copy_bounded_string(address, address_capacity, family == AF_INET6 ? "::" : "0.0.0.0");
}

static int ftop_freebsd_socket_protocol(const struct sockstat *socket_info, const char **protocol) {
  if (socket_info == NULL || protocol == NULL) return 0;
  if (socket_info->dom_family != AF_INET && socket_info->dom_family != AF_INET6) return 0;
  if (socket_info->proto == IPPROTO_TCP) {
    *protocol = "tcp";
    return 1;
  }
  if (socket_info->proto == IPPROTO_UDP) {
    *protocol = "udp";
    return 1;
  }
  return 0;
}

static int ftop_freebsd_connection_exists(
    const struct ftop_freebsd_net_connection_info *buffer, size_t count, long long socket_id, int pid) {
  size_t i;

  for (i = 0U; i < count; ++i) {
    if (buffer[i].socket_id == socket_id && buffer[i].pid == pid) return 1;
  }
  return 0;
}

static const char *ftop_freebsd_tcp_state_label(int state) {
  switch (state) {
  case TCPS_CLOSED:
    return "CLOSE";
  case TCPS_LISTEN:
    return "LISTEN";
  case TCPS_SYN_SENT:
    return "SYN_SENT";
  case TCPS_SYN_RECEIVED:
    return "SYN_RECV";
  case TCPS_ESTABLISHED:
    return "ESTABLISHED";
  case TCPS_CLOSE_WAIT:
    return "CLOSE_WAIT";
  case TCPS_FIN_WAIT_1:
    return "FIN_WAIT1";
  case TCPS_CLOSING:
    return "CLOSING";
  case TCPS_LAST_ACK:
    return "LAST_ACK";
  case TCPS_FIN_WAIT_2:
    return "FIN_WAIT2";
  case TCPS_TIME_WAIT:
    return "TIME_WAIT";
  default:
    return "UNKNOWN";
  }
}

static void ftop_freebsd_store_connection(
    struct ftop_freebsd_net_connection_info *destination, const struct kinfo_proc *process,
    const struct sockstat *socket_info, const char *protocol) {
  char local_address[FTOP_NET_ADDRESS_LEN];
  char remote_address[FTOP_NET_ADDRESS_LEN];
  int local_port;
  int remote_port;

  memset(destination, 0, sizeof(*destination));
  if (!ftop_freebsd_endpoint_from_sockaddr(&socket_info->sa_local, local_address, sizeof(local_address), &local_port)) {
    return;
  }
  if (!ftop_freebsd_endpoint_from_sockaddr(&socket_info->sa_peer, remote_address, sizeof(remote_address), &remote_port)) {
    ftop_freebsd_unspecified_address(socket_info->dom_family, remote_address, sizeof(remote_address), &remote_port);
  }

  destination->valid = 1;
  ftop_copy_bounded_string(destination->protocol, sizeof(destination->protocol), protocol);
  ftop_copy_bounded_string(destination->local_addr, sizeof(destination->local_addr), local_address);
  destination->local_port = local_port;
  ftop_copy_bounded_string(destination->remote_addr, sizeof(destination->remote_addr), remote_address);
  destination->remote_port = remote_port;
  ftop_copy_bounded_string(destination->state, sizeof(destination->state),
      socket_info->proto == IPPROTO_UDP ? "OPEN" : "UNKNOWN");
  destination->socket_id = ftop_uint64_to_nonnegative_long_long(socket_info->so_pcb);
  destination->pid = process->ki_pid > 0 ? (int)process->ki_pid : 0;
  ftop_copy_bounded_string(destination->process_name, sizeof(destination->process_name), process->ki_comm);
}

static int ftop_freebsd_read_sysctl_buffer(const char *name, void **buffer, size_t *buffer_len) {
  void *value;
  size_t value_len;

  if (name == NULL || buffer == NULL || buffer_len == NULL) return -1;
  *buffer = NULL;
  *buffer_len = 0U;
  value_len = 0U;
  if (sysctlbyname(name, NULL, &value_len, NULL, 0) != 0) return -1;
  if (value_len == 0U) return -1;
  value = malloc(value_len);
  if (value == NULL) return -1;
  if (sysctlbyname(name, value, &value_len, NULL, 0) != 0) {
    free(value);
    return -1;
  }
  *buffer = value;
  *buffer_len = value_len;
  return 0;
}

static void ftop_freebsd_assign_tcp_state(
    struct ftop_freebsd_net_connection_info *buffer, size_t count, long long socket_id, int state) {
  size_t i;

  if (socket_id <= 0) return;
  for (i = 0U; i < count; ++i) {
    if (strncmp(buffer[i].protocol, "tcp", sizeof(buffer[i].protocol)) != 0) continue;
    if (buffer[i].socket_id != socket_id) continue;
    ftop_copy_bounded_string(buffer[i].state, sizeof(buffer[i].state), ftop_freebsd_tcp_state_label(state));
  }
}

static void ftop_freebsd_assign_tcp_states(struct ftop_freebsd_net_connection_info *buffer, size_t count) {
  struct xinpgen *entry;
  struct xtcpcb *tcp;
  char *cursor;
  char *end;
  void *sysctl_buffer;
  size_t buffer_len;
  size_t entry_len;
  long long socket_id;

  if (buffer == NULL || count == 0U) return;
  if (ftop_freebsd_read_sysctl_buffer("net.inet.tcp.pcblist", &sysctl_buffer, &buffer_len) != 0) return;
  cursor = (char *)sysctl_buffer;
  end = cursor + buffer_len;
  if (buffer_len < sizeof(struct xinpgen)) {
    free(sysctl_buffer);
    return;
  }
  entry = (struct xinpgen *)(void *)cursor;
  if (entry->xig_len < sizeof(struct xinpgen) || (size_t)entry->xig_len > buffer_len) {
    free(sysctl_buffer);
    return;
  }
  cursor += entry->xig_len;

  while (cursor + sizeof(struct xinpgen) <= end) {
    entry = (struct xinpgen *)(void *)cursor;
    entry_len = (size_t)entry->xig_len;
    if (entry_len <= sizeof(struct xinpgen)) break;
    if (entry_len > (size_t)(end - cursor)) break;
    if (entry_len >= sizeof(struct xtcpcb)) {
      tcp = (struct xtcpcb *)(void *)entry;
      socket_id = ftop_uint64_to_nonnegative_long_long((uint64_t)tcp->xt_inp.xi_socket.so_pcb);
      ftop_freebsd_assign_tcp_state(buffer, count, socket_id, tcp->t_state);
    }
    cursor += entry_len;
  }

  free(sysctl_buffer);
}

int ftop_freebsd_network_connections(
    struct ftop_freebsd_net_connection_info *buffer, size_t capacity, size_t *connection_count, int *sys_errno) {
  struct filestat *file;
  struct filestat_list *files;
  struct kinfo_proc *processes;
  struct procstat *procstat;
  struct sockstat socket_info;
  const char *protocol;
  char error_buffer[256];
  unsigned int process_count;
  size_t count;
  long long socket_id;
  unsigned int process_index;

  if (buffer == NULL || connection_count == NULL || sys_errno == NULL || capacity == 0U) return -1;

  *connection_count = 0U;
  *sys_errno = 0;
  procstat = procstat_open_sysctl();
  if (procstat == NULL) {
    *sys_errno = errno != 0 ? errno : ENOENT;
    return -1;
  }

  process_count = 0U;
  processes = procstat_getprocs(procstat, KERN_PROC_PROC, 0, &process_count);
  if (processes == NULL) {
    *sys_errno = errno != 0 ? errno : EINVAL;
    procstat_close(procstat);
    return -1;
  }

  count = 0U;
  for (process_index = 0U; process_index < process_count && count < capacity; ++process_index) {
    files = procstat_getfiles(procstat, &processes[process_index], 0);
    if (files == NULL) continue;
    STAILQ_FOREACH(file, files, next) {
      if (count >= capacity) break;
      if (file->fs_type != PS_FST_TYPE_SOCKET) continue;
      memset(&socket_info, 0, sizeof(socket_info));
      error_buffer[0] = '\0';
      if (procstat_get_socket_info(procstat, file, &socket_info, error_buffer) != 0) continue;
      if (!ftop_freebsd_socket_protocol(&socket_info, &protocol)) continue;
      socket_id = ftop_uint64_to_nonnegative_long_long(socket_info.so_pcb);
      if (socket_id <= 0) continue;
      if (ftop_freebsd_connection_exists(buffer, count, socket_id, (int)processes[process_index].ki_pid)) continue;
      ftop_freebsd_store_connection(&buffer[count], &processes[process_index], &socket_info, protocol);
      if (buffer[count].valid != 0) ++count;
    }
    procstat_freefiles(procstat, files);
  }

  ftop_freebsd_assign_tcp_states(buffer, count);
  procstat_freeprocs(procstat, processes);
  procstat_close(procstat);
  *connection_count = count;
  return 0;
}

int ftop_freebsd_user_name(int uid, char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
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

static int ftop_sysctl_ulong(const char *name, unsigned long *value) {
  size_t value_len;

  value_len = sizeof(*value);
  *value = 0UL;
  return sysctlbyname(name, value, &value_len, NULL, 0);
}

static int ftop_sysctl_uint(const char *name, unsigned int *value) {
  size_t value_len;

  value_len = sizeof(*value);
  *value = 0U;
  return sysctlbyname(name, value, &value_len, NULL, 0);
}

static unsigned int ftop_optional_sysctl_uint(const char *name) {
  unsigned int value;

  if (ftop_sysctl_uint(name, &value) != 0) return 0U;
  return value;
}

static int ftop_freebsd_swap_info(long long *total_bytes, long long *used_bytes, int *sys_errno) {
  char error_buffer[256];
  struct kvm_swap swap_info;
  kvm_t *handle;
  long long page_size;
  int rc;

  if (total_bytes == NULL || used_bytes == NULL || sys_errno == NULL) return -1;

  *total_bytes = 0;
  *used_bytes = 0;
  *sys_errno = 0;
  memset(&swap_info, 0, sizeof(swap_info));
  error_buffer[0] = '\0';

  handle = kvm_openfiles(NULL, "/dev/null", NULL, O_RDONLY, error_buffer);
  if (handle == NULL) {
    *sys_errno = errno != 0 ? errno : ENOENT;
    return -1;
  }

  rc = kvm_getswapinfo(handle, &swap_info, 1, 0);
  if (rc < 0) {
    *sys_errno = errno != 0 ? errno : EINVAL;
    (void)kvm_close(handle);
    return -1;
  }
  (void)kvm_close(handle);

  page_size = (long long)getpagesize();
  *total_bytes = (long long)swap_info.ksw_total * page_size;
  *used_bytes = (long long)swap_info.ksw_used * page_size;
  if (*used_bytes > *total_bytes) *used_bytes = *total_bytes;

  return 0;
}

int ftop_freebsd_memory_info(long long *total_bytes, long long *free_bytes, long long *available_bytes,
    long long *swap_total_bytes, long long *swap_used_bytes, int *sys_errno) {
  unsigned long physical_memory;
  unsigned int free_pages;
  unsigned int inactive_pages;
  unsigned int available_pages;
  long long swap_total;
  long long swap_used;
  long long page_size;
  int swap_errno;

  if (total_bytes == NULL || free_bytes == NULL || available_bytes == NULL || swap_total_bytes == NULL ||
      swap_used_bytes == NULL || sys_errno == NULL) {
    return -1;
  }

  *total_bytes = 0;
  *free_bytes = 0;
  *available_bytes = 0;
  *swap_total_bytes = 0;
  *swap_used_bytes = 0;
  *sys_errno = 0;
  if (ftop_sysctl_ulong("hw.physmem", &physical_memory) != 0) {
    *sys_errno = errno;
    return -1;
  }
  if (ftop_sysctl_uint("vm.stats.vm.v_free_count", &free_pages) != 0) {
    *sys_errno = errno;
    return -1;
  }
  if (ftop_sysctl_uint("vm.stats.vm.v_inactive_count", &inactive_pages) != 0) {
    *sys_errno = errno;
    return -1;
  }

  page_size = (long long)getpagesize();
  available_pages = free_pages + inactive_pages;
  available_pages += ftop_optional_sysctl_uint("vm.stats.vm.v_cache_count");
  available_pages += ftop_optional_sysctl_uint("vm.stats.vm.v_laundry_count");

  *total_bytes = (long long)physical_memory;
  *free_bytes = (long long)free_pages * page_size;
  *available_bytes = (long long)available_pages * page_size;
  if (*total_bytes <= 0) {
    *sys_errno = EINVAL;
    return -1;
  }
  if (*free_bytes > *total_bytes) *free_bytes = *total_bytes;
  if (*available_bytes > *total_bytes) *available_bytes = *total_bytes;
  swap_total = 0;
  swap_used = 0;
  swap_errno = 0;
  if (ftop_freebsd_swap_info(&swap_total, &swap_used, &swap_errno) == 0) {
    *swap_total_bytes = swap_total;
    *swap_used_bytes = swap_used;
  }

  return 0;
}

int ftop_freebsd_load_average(double *loads, int *sys_errno) {
  int count;

  if (loads == NULL || sys_errno == NULL) return -1;

  loads[0] = 0.0;
  loads[1] = 0.0;
  loads[2] = 0.0;
  *sys_errno = 0;
  count = getloadavg(loads, 3);
  if (count < 3) {
    *sys_errno = errno != 0 ? errno : EINVAL;
    return -1;
  }

  return 0;
}

int ftop_freebsd_uptime_seconds(long long *uptime_seconds, int *sys_errno) {
  struct timeval boottime;
  size_t value_len;
  time_t now;
  long long uptime;

  if (uptime_seconds == NULL || sys_errno == NULL) return -1;

  *uptime_seconds = 0;
  *sys_errno = 0;
  memset(&boottime, 0, sizeof(boottime));
  value_len = sizeof(boottime);
  if (sysctlbyname("kern.boottime", &boottime, &value_len, NULL, 0) != 0) {
    *sys_errno = errno;
    return -1;
  }
  if (value_len < sizeof(boottime.tv_sec) || boottime.tv_sec <= 0) {
    *sys_errno = EINVAL;
    return -1;
  }

  now = time(NULL);
  if (now == (time_t)-1) {
    *sys_errno = errno != 0 ? errno : EINVAL;
    return -1;
  }

  uptime = (long long)(now - boottime.tv_sec);
  if (uptime < 0) {
    *sys_errno = EINVAL;
    return -1;
  }

  *uptime_seconds = uptime;
  return 0;
}
