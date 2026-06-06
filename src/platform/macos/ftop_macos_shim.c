#include <errno.h>
#include <arpa/inet.h>
#include <CoreFoundation/CoreFoundation.h>
#include <IOKit/IOKitLib.h>
#include <mach/host_info.h>
#include <mach/mach_host.h>
#include <mach/mach_init.h>
#include <mach/mach_time.h>
#include <mach/processor_info.h>
#include <mach/vm_map.h>
#include <ifaddrs.h>
#include <libproc.h>
#include <limits.h>
#include <net/if.h>
#include <netinet/in.h>
#include <pwd.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/proc_info.h>
#include <sys/mount.h>
#include <sys/sysctl.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

struct ftop_macos_processor_ticks {
  long long user;
  long long system;
  long long idle;
  long long nice;
};

#define FTOP_MACOS_PROCESS_COMMAND_LEN 256
#define FTOP_NET_INTERFACE_NAME_LEN 32
#define FTOP_NET_PROTOCOL_LEN 8
#define FTOP_NET_ADDRESS_LEN 64
#define FTOP_NET_STATE_LEN 16
#define FTOP_NET_PROCESS_NAME_LEN 64
#define FTOP_DISK_DEVICE_LEN 64
#define FTOP_DISK_MOUNTPOINT_LEN 128
#define FTOP_DISK_FSTYPE_LEN 32
#define FTOP_USER_LOOKUP_BUFFER_LEN 16384

struct ftop_macos_process_info {
  int pid;
  int ppid;
  int uid;
  int state;
  char command[FTOP_MACOS_PROCESS_COMMAND_LEN];
  long long mem_rss_bytes;
  long long mem_virt_bytes;
  int threads;
  int nice;
  long long start_time;
  long long cpu_time;
};

struct ftop_macos_net_interface_info {
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

struct ftop_macos_net_connection_info {
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

typedef struct __IOHIDEvent *IOHIDEventRef;
typedef struct __IOHIDEventSystemClient *IOHIDEventSystemClientRef;
typedef struct __IOHIDServiceClient *IOHIDServiceClientRef;

#ifdef __LP64__
typedef double IOHIDFloat;
#else
typedef float IOHIDFloat;
#endif

#define FTOP_IOHID_EVENT_FIELD_BASE(type) ((type) << 16)
#define FTOP_IOHID_EVENT_TYPE_TEMPERATURE 15

IOHIDEventSystemClientRef IOHIDEventSystemClientCreate(CFAllocatorRef allocator);
void IOHIDEventSystemClientSetMatching(IOHIDEventSystemClientRef client, CFDictionaryRef match);
CFArrayRef IOHIDEventSystemClientCopyServices(IOHIDEventSystemClientRef client);
IOHIDEventRef IOHIDServiceClientCopyEvent(IOHIDServiceClientRef service, int64_t type, int32_t options, int64_t timeout);
CFTypeRef IOHIDServiceClientCopyProperty(IOHIDServiceClientRef service, CFStringRef property);
IOHIDFloat IOHIDEventGetFloatValue(IOHIDEventRef event, int32_t field);

#define FTOP_SMC_KERNEL_INDEX 2
#define FTOP_SMC_CMD_READ_BYTES 5
#define FTOP_SMC_CMD_READ_KEYINFO 9

typedef char ftop_smc_bytes_t[32];

struct ftop_smc_key_data_vers {
  char major;
  char minor;
  char build;
  char reserved[1];
  uint16_t release;
};

struct ftop_smc_key_data_plimit {
  uint16_t version;
  uint16_t length;
  uint32_t cpu_plimit;
  uint32_t gpu_plimit;
  uint32_t mem_plimit;
};

struct ftop_smc_key_info {
  uint32_t data_size;
  uint32_t data_type;
  char data_attributes;
};

struct ftop_smc_key_data {
  uint32_t key;
  struct ftop_smc_key_data_vers vers;
  struct ftop_smc_key_data_plimit plimit;
  struct ftop_smc_key_info key_info;
  char result;
  char status;
  char data8;
  uint32_t data32;
  ftop_smc_bytes_t bytes;
};

struct ftop_smc_value {
  char key[5];
  uint32_t data_size;
  char data_type[5];
  ftop_smc_bytes_t bytes;
};

int ftop_macos_cpu_count(int *count, int *sys_errno) {
  int value;
  size_t value_len;

  if (count == NULL || sys_errno == NULL) return -1;

  *count = 0;
  *sys_errno = 0;
  value = 0;
  value_len = sizeof(value);
  if (sysctlbyname("hw.logicalcpu", &value, &value_len, NULL, 0) != 0) {
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

int ftop_macos_cpu_ticks(long long *total_ticks, long long *idle_ticks, int *sys_errno) {
  host_cpu_load_info_data_t cpu_info;
  mach_msg_type_number_t count;
  kern_return_t rc;
  long long user;
  long long system;
  long long idle;
  long long nice;

  if (total_ticks == NULL || idle_ticks == NULL || sys_errno == NULL) return -1;

  *total_ticks = 0;
  *idle_ticks = 0;
  *sys_errno = 0;
  count = HOST_CPU_LOAD_INFO_COUNT;
  rc = host_statistics(mach_host_self(), HOST_CPU_LOAD_INFO, (host_info_t)&cpu_info, &count);
  if (rc != KERN_SUCCESS) {
    *sys_errno = (int)rc;
    return -1;
  }

  user = (long long)cpu_info.cpu_ticks[CPU_STATE_USER];
  system = (long long)cpu_info.cpu_ticks[CPU_STATE_SYSTEM];
  idle = (long long)cpu_info.cpu_ticks[CPU_STATE_IDLE];
  nice = (long long)cpu_info.cpu_ticks[CPU_STATE_NICE];
  *idle_ticks = idle;
  *total_ticks = user + system + idle + nice;
  if (*total_ticks <= 0) {
    *sys_errno = EINVAL;
    return -1;
  }

  return 0;
}

static uint32_t ftop_smc_key_code(const char *key) {
  return ((uint32_t)(unsigned char)key[0] << 24) | ((uint32_t)(unsigned char)key[1] << 16) |
         ((uint32_t)(unsigned char)key[2] << 8) | (uint32_t)(unsigned char)key[3];
}

static void ftop_smc_type_string(char *text, uint32_t value) {
  text[0] = (char)((value >> 24) & 0xffU);
  text[1] = (char)((value >> 16) & 0xffU);
  text[2] = (char)((value >> 8) & 0xffU);
  text[3] = (char)(value & 0xffU);
  text[4] = '\0';
}

static kern_return_t ftop_smc_call(
    io_connect_t connection, struct ftop_smc_key_data *input, struct ftop_smc_key_data *output) {
  size_t input_size;
  size_t output_size;

  input_size = sizeof(*input);
  output_size = sizeof(*output);
  return IOConnectCallStructMethod(connection, FTOP_SMC_KERNEL_INDEX, input, input_size, output, &output_size);
}

static int ftop_smc_open(io_connect_t *connection) {
  static const char *service_names[] = {"AppleSMC", "AppleSMCKeysEndpoint"};
  CFMutableDictionaryRef matching;
  io_iterator_t iterator;
  io_object_t device;
  kern_return_t rc;
  size_t i;

  if (connection == NULL) return -1;
  *connection = IO_OBJECT_NULL;

  for (i = 0U; i < sizeof(service_names) / sizeof(service_names[0]); ++i) {
    iterator = IO_OBJECT_NULL;
    matching = IOServiceMatching(service_names[i]);
    if (matching == NULL) continue;
    rc = IOServiceGetMatchingServices(0, matching, &iterator);
    if (rc != KERN_SUCCESS) continue;

    while ((device = IOIteratorNext(iterator)) != IO_OBJECT_NULL) {
      rc = IOServiceOpen(device, mach_task_self(), 0, connection);
      IOObjectRelease(device);
      if (rc == KERN_SUCCESS) {
        IOObjectRelease(iterator);
        return 0;
      }
    }
    IOObjectRelease(iterator);
  }

  return -1;
}

static kern_return_t ftop_smc_read_key(io_connect_t connection, const char *key, struct ftop_smc_value *value) {
  struct ftop_smc_key_data input;
  struct ftop_smc_key_data output;
  kern_return_t rc;

  memset(&input, 0, sizeof(input));
  memset(&output, 0, sizeof(output));
  memset(value, 0, sizeof(*value));

  memcpy(value->key, key, 4U);
  value->key[4] = '\0';
  input.key = ftop_smc_key_code(key);
  input.data8 = FTOP_SMC_CMD_READ_KEYINFO;
  rc = ftop_smc_call(connection, &input, &output);
  if (rc != KERN_SUCCESS) return rc;

  value->data_size = output.key_info.data_size;
  ftop_smc_type_string(value->data_type, output.key_info.data_type);

  input.key_info.data_size = value->data_size;
  input.data8 = FTOP_SMC_CMD_READ_BYTES;
  rc = ftop_smc_call(connection, &input, &output);
  if (rc != KERN_SUCCESS) return rc;

  memcpy(value->bytes, output.bytes, sizeof(value->bytes));
  return KERN_SUCCESS;
}

static int ftop_smc_read_temperature_key(io_connect_t connection, const char *key, double *temperature_c) {
  struct ftop_smc_value value;
  int raw_value;

  if (ftop_smc_read_key(connection, key, &value) != KERN_SUCCESS) return -1;
  if (value.data_size < 2U) return -1;
  if (strcmp(value.data_type, "sp78") != 0) return -1;

  raw_value = ((int)(unsigned char)value.bytes[0] << 8) | (int)(unsigned char)value.bytes[1];
  *temperature_c = (double)raw_value / 256.0;
  if (*temperature_c <= 0.0 || *temperature_c >= 150.0) return -1;
  return 0;
}

static int ftop_smc_cpu_temperature(double *temperature_c, int *sys_errno) {
  static const char *package_keys[] = {"TC0P", "TC0D", "TC0F", "TC0C"};
  static const char key_indexes[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  char key[5];
  double best_temperature;
  double value;
  io_connect_t connection;
  size_t i;
  int found;

  if (ftop_smc_open(&connection) != 0) {
    *sys_errno = ENOENT;
    return -1;
  }

  found = 0;
  best_temperature = 0.0;
  for (i = 0U; i < sizeof(package_keys) / sizeof(package_keys[0]); ++i) {
    if (ftop_smc_read_temperature_key(connection, package_keys[i], &value) == 0) {
      if (!found || value > best_temperature) best_temperature = value;
      found = 1;
    }
  }

  for (i = 0U; i + 1U < sizeof(key_indexes); ++i) {
    (void)snprintf(key, sizeof(key), "TC%cC", key_indexes[i]);
    if (ftop_smc_read_temperature_key(connection, key, &value) == 0) {
      if (!found || value > best_temperature) best_temperature = value;
      found = 1;
    }

    (void)snprintf(key, sizeof(key), "TC%cc", key_indexes[i]);
    if (ftop_smc_read_temperature_key(connection, key, &value) == 0) {
      if (!found || value > best_temperature) best_temperature = value;
      found = 1;
    }
  }

  IOServiceClose(connection);
  if (!found) {
    *sys_errno = ENOENT;
    return -1;
  }

  *temperature_c = best_temperature;
  return 0;
}

static CFDictionaryRef ftop_hid_matching_dictionary(int usage_page, int usage) {
  CFDictionaryRef dictionary;
  CFNumberRef values[2];
  CFStringRef keys[2];

  keys[0] = CFStringCreateWithCString(kCFAllocatorDefault, "PrimaryUsagePage", kCFStringEncodingASCII);
  keys[1] = CFStringCreateWithCString(kCFAllocatorDefault, "PrimaryUsage", kCFStringEncodingASCII);
  values[0] = CFNumberCreate(kCFAllocatorDefault, kCFNumberSInt32Type, &usage_page);
  values[1] = CFNumberCreate(kCFAllocatorDefault, kCFNumberSInt32Type, &usage);
  if (keys[0] == NULL || keys[1] == NULL || values[0] == NULL || values[1] == NULL) {
    if (keys[0] != NULL) CFRelease(keys[0]);
    if (keys[1] != NULL) CFRelease(keys[1]);
    if (values[0] != NULL) CFRelease(values[0]);
    if (values[1] != NULL) CFRelease(values[1]);
    return NULL;
  }

  dictionary = CFDictionaryCreate(kCFAllocatorDefault, (const void **)keys, (const void **)values, 2,
                                  &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
  CFRelease(keys[0]);
  CFRelease(keys[1]);
  CFRelease(values[0]);
  CFRelease(values[1]);
  return dictionary;
}

static double ftop_hid_service_temperature(IOHIDServiceClientRef service) {
  IOHIDEventRef event;
  double temperature;

  event = IOHIDServiceClientCopyEvent(service, FTOP_IOHID_EVENT_TYPE_TEMPERATURE, 0, 0);
  if (event == NULL) return 0.0;

  temperature = (double)IOHIDEventGetFloatValue(event, FTOP_IOHID_EVENT_FIELD_BASE(FTOP_IOHID_EVENT_TYPE_TEMPERATURE));
  CFRelease(event);
  if (temperature <= 0.0 || temperature >= 150.0) return 0.0;
  return temperature;
}

static int ftop_hid_service_name(IOHIDServiceClientRef service, char *buffer, size_t buffer_len) {
  CFTypeRef property;
  int success;

  if (buffer == NULL || buffer_len == 0U) return 0;

  buffer[0] = '\0';
  property = IOHIDServiceClientCopyProperty(service, CFSTR("Product"));
  if (property == NULL) return 0;

  success = 0;
  if (CFGetTypeID(property) == CFStringGetTypeID()) {
    success = CFStringGetCString((CFStringRef)property, buffer, (CFIndex)buffer_len, kCFStringEncodingASCII) != 0;
  }
  CFRelease(property);
  return success;
}

static int ftop_hid_is_cpu_temperature_name(const char *name) {
  if (strncmp(name, "eACC", 4U) == 0 || strncmp(name, "pACC", 4U) == 0) return 1;
  if (strncmp(name, "PMU tdie", 8U) == 0) return 1;
  if (strncmp(name, "SOC MTR Temp Sensor", 19U) == 0) return 1;
  return strstr(name, "CPU") != NULL || strstr(name, "cpu") != NULL;
}

static int ftop_hid_cpu_temperature(double *temperature_c, int *sys_errno) {
  CFArrayRef services;
  CFDictionaryRef matching;
  IOHIDEventSystemClientRef client;
  char name[128];
  double cpu_total;
  double fallback_total;
  double value;
  CFIndex count;
  CFIndex i;
  int cpu_count;
  int fallback_count;

  matching = ftop_hid_matching_dictionary(0xff00, 5);
  if (matching == NULL) {
    *sys_errno = ENOMEM;
    return -1;
  }

  client = IOHIDEventSystemClientCreate(kCFAllocatorDefault);
  if (client == NULL) {
    CFRelease(matching);
    *sys_errno = ENOENT;
    return -1;
  }

  IOHIDEventSystemClientSetMatching(client, matching);
  services = IOHIDEventSystemClientCopyServices(client);
  CFRelease(matching);
  if (services == NULL) {
    CFRelease(client);
    *sys_errno = ENOENT;
    return -1;
  }

  cpu_total = 0.0;
  fallback_total = 0.0;
  cpu_count = 0;
  fallback_count = 0;
  count = CFArrayGetCount(services);
  for (i = 0; i < count; ++i) {
    IOHIDServiceClientRef service;

    service = (IOHIDServiceClientRef)CFArrayGetValueAtIndex(services, i);
    if (service == NULL) continue;
    value = ftop_hid_service_temperature(service);
    if (value <= 0.0) continue;

    if (ftop_hid_service_name(service, name, sizeof(name)) && ftop_hid_is_cpu_temperature_name(name)) {
      cpu_total += value;
      ++cpu_count;
    } else {
      fallback_total += value;
      ++fallback_count;
    }
  }

  CFRelease(services);
  CFRelease(client);
  if (cpu_count > 0) {
    *temperature_c = cpu_total / (double)cpu_count;
    return 0;
  }
  if (fallback_count > 0) {
    *temperature_c = fallback_total / (double)fallback_count;
    return 0;
  }

  *sys_errno = ENOENT;
  return -1;
}

int ftop_macos_cpu_temperature(double *temperature_c, int *sys_errno) {
  if (temperature_c == NULL || sys_errno == NULL) return -1;

  *temperature_c = 0.0;
  *sys_errno = 0;
  if (ftop_hid_cpu_temperature(temperature_c, sys_errno) == 0) return 0;
  *sys_errno = 0;
  if (ftop_smc_cpu_temperature(temperature_c, sys_errno) == 0) return 0;
  if (*sys_errno == 0) *sys_errno = ENOENT;
  return -1;
}

static int ftop_macos_swap_info(long long *total_bytes, long long *used_bytes, int *sys_errno) {
  struct xsw_usage usage;
  size_t usage_len;

  if (total_bytes == NULL || used_bytes == NULL || sys_errno == NULL) return -1;

  *total_bytes = 0;
  *used_bytes = 0;
  *sys_errno = 0;
  memset(&usage, 0, sizeof(usage));
  usage_len = sizeof(usage);
  if (sysctlbyname("vm.swapusage", &usage, &usage_len, NULL, 0) != 0) {
    *sys_errno = errno;
    return -1;
  }

  *total_bytes = (long long)usage.xsu_total;
  *used_bytes = (long long)usage.xsu_used;
  if (*used_bytes > *total_bytes) *used_bytes = *total_bytes;

  return 0;
}

int ftop_macos_memory_info(
    long long *total_bytes, long long *used_bytes, long long *free_bytes, long long *available_bytes,
    long long *swap_total_bytes, long long *swap_used_bytes, int *sys_errno) {
  vm_statistics64_data_t vm_info;
  mach_msg_type_number_t count;
  unsigned long long total;
  size_t total_len;
  kern_return_t rc;
  long long page_size;
  long long free_pages;
  long long available_pages;
  long long swap_total;
  long long swap_used;
  int swap_errno;

  if (total_bytes == NULL || used_bytes == NULL || free_bytes == NULL || available_bytes == NULL ||
      swap_total_bytes == NULL || swap_used_bytes == NULL || sys_errno == NULL) {
    return -1;
  }

  *total_bytes = 0;
  *used_bytes = 0;
  *free_bytes = 0;
  *available_bytes = 0;
  *swap_total_bytes = 0;
  *swap_used_bytes = 0;
  *sys_errno = 0;
  total = 0ULL;
  total_len = sizeof(total);
  if (sysctlbyname("hw.memsize", &total, &total_len, NULL, 0) != 0) {
    *sys_errno = errno;
    return -1;
  }

  count = HOST_VM_INFO64_COUNT;
  rc = host_statistics64(mach_host_self(), HOST_VM_INFO64, (host_info64_t)&vm_info, &count);
  if (rc != KERN_SUCCESS) {
    *sys_errno = (int)rc;
    return -1;
  }

  page_size = (long long)sysconf(_SC_PAGESIZE);
  free_pages = (long long)vm_info.free_count + (long long)vm_info.speculative_count;
  available_pages = free_pages + (long long)vm_info.inactive_count;

  *total_bytes = (long long)total;
  *free_bytes = free_pages * page_size;
  *available_bytes = available_pages * page_size;
  if (*total_bytes <= 0) {
    *sys_errno = EINVAL;
    return -1;
  }
  if (*free_bytes > *total_bytes) *free_bytes = *total_bytes;
  if (*available_bytes > *total_bytes) *available_bytes = *total_bytes;
  *used_bytes = *total_bytes - *free_bytes;
  swap_total = 0;
  swap_used = 0;
  swap_errno = 0;
  if (ftop_macos_swap_info(&swap_total, &swap_used, &swap_errno) == 0) {
    *swap_total_bytes = swap_total;
    *swap_used_bytes = swap_used;
  }

  return 0;
}

int ftop_macos_load_average(double *loads, int *sys_errno) {
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

int ftop_macos_uptime_seconds(long long *uptime_seconds, int *sys_errno) {
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

int ftop_macos_processor_ticks(
    struct ftop_macos_processor_ticks *buffer, size_t capacity, size_t *processor_count, int *sys_errno) {
  processor_cpu_load_info_t cpu_load;
  processor_info_array_t cpu_info;
  mach_msg_type_number_t info_count;
  natural_t cpu_count;
  kern_return_t rc;
  size_t copy_count;
  size_t i;

  if (buffer == NULL || processor_count == NULL || sys_errno == NULL || capacity == 0) return -1;

  *processor_count = 0U;
  *sys_errno = 0;
  cpu_info = NULL;
  info_count = 0;
  cpu_count = 0;
  rc = host_processor_info(mach_host_self(), PROCESSOR_CPU_LOAD_INFO, &cpu_count, &cpu_info, &info_count);
  if (rc != KERN_SUCCESS) {
    *sys_errno = (int)rc;
    return -1;
  }

  cpu_load = (processor_cpu_load_info_t)cpu_info;
  copy_count = (size_t)cpu_count < capacity ? (size_t)cpu_count : capacity;
  for (i = 0U; i < copy_count; ++i) {
    buffer[i].user = (long long)cpu_load[i].cpu_ticks[CPU_STATE_USER];
    buffer[i].system = (long long)cpu_load[i].cpu_ticks[CPU_STATE_SYSTEM];
    buffer[i].idle = (long long)cpu_load[i].cpu_ticks[CPU_STATE_IDLE];
    buffer[i].nice = (long long)cpu_load[i].cpu_ticks[CPU_STATE_NICE];
  }

  rc = vm_deallocate(mach_task_self(), (vm_address_t)cpu_info, (vm_size_t)info_count * sizeof(integer_t));
  if (rc != KERN_SUCCESS) {
    *sys_errno = (int)rc;
    return -1;
  }

  *processor_count = copy_count;
  return 0;
}

static void ftop_macos_copy_process_command(char *destination, const char *source) {
  size_t i;

  for (i = 0U; i + 1U < FTOP_MACOS_PROCESS_COMMAND_LEN && source[i] != '\0'; ++i) destination[i] = source[i];
  destination[i] = '\0';
}

static uint64_t ftop_macos_absolute_to_nanoseconds(uint64_t value) {
  static mach_timebase_info_data_t timebase;
  static int initialized;
  long double nanoseconds;

  if (!initialized) {
    if (mach_timebase_info(&timebase) != KERN_SUCCESS || timebase.denom == 0U) {
      timebase.numer = 1U;
      timebase.denom = 1U;
    }
    initialized = 1;
  }

  nanoseconds = ((long double)value * (long double)timebase.numer) / (long double)timebase.denom;
  if (nanoseconds <= 0.0L) return 0ULL;
  if (nanoseconds >= (long double)UINT64_MAX) return UINT64_MAX;
  return (uint64_t)nanoseconds;
}

static void ftop_macos_fill_task_info(pid_t pid, struct ftop_macos_process_info *process) {
  struct proc_taskinfo task;
  uint64_t total_time;
  uint64_t total_time_ns;
  int rc;

  if (pid <= 0 || process == NULL) return;

  memset(&task, 0, sizeof(task));
  rc = proc_pidinfo(pid, PROC_PIDTASKINFO, 0, &task, sizeof(task));
  if (rc != (int)sizeof(task)) return;

  process->mem_rss_bytes = task.pti_resident_size > 0 ? (long long)task.pti_resident_size : 0;
  process->mem_virt_bytes = task.pti_virtual_size > 0 ? (long long)task.pti_virtual_size : 0;
  process->threads = task.pti_threadnum > 0 ? (int)task.pti_threadnum : 0;
  total_time = task.pti_total_user + task.pti_total_system;
  total_time_ns = ftop_macos_absolute_to_nanoseconds(total_time);
  process->cpu_time = total_time_ns > 0 ? (long long)(total_time_ns / 1000000ULL) : 0;
}

static int ftop_macos_fill_process_info(pid_t pid, struct ftop_macos_process_info *process) {
  struct proc_bsdinfo bsd;
  int rc;

  if (pid <= 0 || process == NULL) return -1;

  memset(&bsd, 0, sizeof(bsd));
  rc = proc_pidinfo(pid, PROC_PIDTBSDINFO, 0, &bsd, sizeof(bsd));
  if (rc != (int)sizeof(bsd)) return -1;

  memset(process, 0, sizeof(*process));
  process->pid = (int)bsd.pbi_pid;
  process->ppid = (int)bsd.pbi_ppid;
  process->uid = (int)bsd.pbi_uid;
  process->state = (int)bsd.pbi_status;
  process->nice = (int)bsd.pbi_nice;
  process->start_time = bsd.pbi_start_tvsec > 0 ? (long long)bsd.pbi_start_tvsec : 0;
  ftop_macos_copy_process_command(process->command, bsd.pbi_comm);
  ftop_macos_fill_task_info(pid, process);
  return 0;
}

static int ftop_macos_fill_process_info_sysctl(pid_t pid, struct ftop_macos_process_info *process) {
  struct kinfo_proc kinfo;
  int mib[4];
  size_t value_len;

  if (pid <= 0 || process == NULL) return -1;

  mib[0] = CTL_KERN;
  mib[1] = KERN_PROC;
  mib[2] = KERN_PROC_PID;
  mib[3] = (int)pid;
  memset(&kinfo, 0, sizeof(kinfo));
  value_len = sizeof(kinfo);
  if (sysctl(mib, 4U, &kinfo, &value_len, NULL, 0U) != 0 || value_len < sizeof(kinfo)) return -1;
  if (kinfo.kp_proc.p_pid <= 0) return -1;

  memset(process, 0, sizeof(*process));
  process->pid = (int)kinfo.kp_proc.p_pid;
  process->ppid = (int)kinfo.kp_eproc.e_ppid;
  process->uid = (int)kinfo.kp_eproc.e_ucred.cr_uid;
  process->state = (int)kinfo.kp_proc.p_stat;
  process->nice = (int)kinfo.kp_proc.p_nice;
  process->start_time = kinfo.kp_proc.p_starttime.tv_sec > 0 ? (long long)kinfo.kp_proc.p_starttime.tv_sec : 0;
  ftop_macos_copy_process_command(process->command, kinfo.kp_proc.p_comm);
  ftop_macos_fill_task_info(pid, process);
  return 0;
}

int ftop_macos_process_snapshot(
    struct ftop_macos_process_info *buffer, size_t capacity, size_t *process_count, int *sys_errno) {
  pid_t *pids;
  int byte_count;
  int pid_count;
  int i;
  int found_pid_one;
  size_t count;

  if (buffer == NULL || process_count == NULL || sys_errno == NULL || capacity == 0) return -1;

  *process_count = 0U;
  *sys_errno = 0;
  byte_count = proc_listpids(PROC_ALL_PIDS, 0, NULL, 0);
  if (byte_count <= 0) {
    *sys_errno = errno != 0 ? errno : EINVAL;
    return -1;
  }

  pids = (pid_t *)malloc((size_t)byte_count);
  if (pids == NULL) {
    *sys_errno = errno != 0 ? errno : ENOMEM;
    return -1;
  }

  byte_count = proc_listpids(PROC_ALL_PIDS, 0, pids, byte_count);
  if (byte_count <= 0) {
    *sys_errno = errno != 0 ? errno : EINVAL;
    free(pids);
    return -1;
  }

  pid_count = byte_count / (int)sizeof(pid_t);
  count = 0U;
  found_pid_one = 0;
  for (i = 0; i < pid_count && count < capacity; ++i) {
    if (pids[i] <= 0) continue;
    if (ftop_macos_fill_process_info(pids[i], &buffer[count]) != 0 &&
        ftop_macos_fill_process_info_sysctl(pids[i], &buffer[count]) != 0)
      continue;
    if (buffer[count].pid == 1) found_pid_one = 1;
    ++count;
  }

  if (!found_pid_one && count < capacity) {
    if (ftop_macos_fill_process_info((pid_t)1, &buffer[count]) == 0 ||
        ftop_macos_fill_process_info_sysctl((pid_t)1, &buffer[count]) == 0)
      ++count;
  }

  free(pids);
  *process_count = count;
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

int ftop_macos_filesystems(struct ftop_filesystem_info *filesystems, int capacity, int *filesystem_count,
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

static const char *ftop_macos_tcp_state_label(int state) {
  switch (state) {
  case TSI_S_CLOSED:
    return "CLOSE";
  case TSI_S_LISTEN:
    return "LISTEN";
  case TSI_S_SYN_SENT:
    return "SYN_SENT";
  case TSI_S_SYN_RECEIVED:
    return "SYN_RECV";
  case TSI_S_ESTABLISHED:
    return "ESTABLISHED";
  case TSI_S__CLOSE_WAIT:
    return "CLOSE_WAIT";
  case TSI_S_FIN_WAIT_1:
    return "FIN_WAIT1";
  case TSI_S_CLOSING:
    return "CLOSING";
  case TSI_S_LAST_ACK:
    return "LAST_ACK";
  case TSI_S_FIN_WAIT_2:
    return "FIN_WAIT2";
  case TSI_S_TIME_WAIT:
    return "TIME_WAIT";
  case TSI_S_RESERVED:
    return "RESERVED";
  default:
    return "UNKNOWN";
  }
}

static int ftop_macos_socket_protocol(const struct socket_info *socket_info, const char **protocol) {
  if (socket_info == NULL || protocol == NULL) return 0;
  if (socket_info->soi_family != AF_INET && socket_info->soi_family != AF_INET6) return 0;
  if (socket_info->soi_protocol == IPPROTO_TCP) {
    *protocol = "tcp";
    return 1;
  }
  if (socket_info->soi_protocol == IPPROTO_UDP) {
    *protocol = "udp";
    return 1;
  }
  return 0;
}

static const struct in_sockinfo *ftop_macos_socket_in_info(const struct socket_info *socket_info) {
  if (socket_info == NULL) return NULL;
  if (socket_info->soi_protocol == IPPROTO_TCP || socket_info->soi_kind == SOCKINFO_TCP) {
    return &socket_info->soi_proto.pri_tcp.tcpsi_ini;
  }
  if (socket_info->soi_protocol == IPPROTO_UDP || socket_info->soi_kind == SOCKINFO_IN) {
    return &socket_info->soi_proto.pri_in;
  }
  return NULL;
}

static int ftop_macos_in_sockinfo_family(const struct in_sockinfo *socket_info, int socket_family) {
  if (socket_info == NULL) return AF_UNSPEC;
  if ((socket_info->insi_vflag & INI_IPV4) != 0U || socket_family == AF_INET) return AF_INET;
  if ((socket_info->insi_vflag & INI_IPV6) != 0U || socket_family == AF_INET6) return AF_INET6;
  return AF_UNSPEC;
}

static int ftop_macos_endpoint_from_in_sockinfo(
    const struct in_sockinfo *socket_info, int socket_family, int local, char *address, size_t address_capacity, int *port) {
  const void *source;
  int family;
  int raw_port;

  if (socket_info == NULL || address == NULL || port == NULL || address_capacity == 0U) return 0;
  address[0] = '\0';
  *port = 0;
  family = ftop_macos_in_sockinfo_family(socket_info, socket_family);
  if (family == AF_INET) {
    source = local != 0 ? (const void *)&socket_info->insi_laddr.ina_46.i46a_addr4
                        : (const void *)&socket_info->insi_faddr.ina_46.i46a_addr4;
  } else if (family == AF_INET6) {
    source = local != 0 ? (const void *)&socket_info->insi_laddr.ina_6
                        : (const void *)&socket_info->insi_faddr.ina_6;
  } else {
    return 0;
  }

  raw_port = local != 0 ? socket_info->insi_lport : socket_info->insi_fport;
  *port = (int)ntohs((uint16_t)raw_port);
  return inet_ntop(family, source, address, (socklen_t)address_capacity) != NULL;
}

static void ftop_macos_unspecified_address(int family, char *address, size_t address_capacity, int *port) {
  if (address == NULL || port == NULL || address_capacity == 0U) return;
  *port = 0;
  ftop_copy_bounded_string(address, address_capacity, family == AF_INET6 ? "::" : "0.0.0.0");
}

static long long ftop_macos_socket_id(const struct socket_info *socket_info) {
  uint64_t value;

  if (socket_info == NULL) return 0;
  value = socket_info->soi_so != 0ULL ? socket_info->soi_so : socket_info->soi_pcb;
  return ftop_nonnegative_unsigned_long_long((unsigned long long)value);
}

static int ftop_macos_connection_exists(
    const struct ftop_macos_net_connection_info *buffer, size_t count, long long socket_id, int pid) {
  size_t i;

  for (i = 0U; i < count; ++i) {
    if (buffer[i].socket_id == socket_id && buffer[i].pid == pid) return 1;
  }
  return 0;
}

static void ftop_macos_process_name(pid_t pid, char *buffer, size_t buffer_len) {
  struct proc_bsdinfo bsd;
  int rc;

  if (buffer == NULL || buffer_len == 0U) return;
  ftop_copy_bounded_string(buffer, buffer_len, "unknown");
  if (pid <= 0) return;

  rc = proc_name(pid, buffer, (uint32_t)buffer_len);
  buffer[buffer_len - 1U] = '\0';
  if (rc > 0 && buffer[0] != '\0') return;

  memset(&bsd, 0, sizeof(bsd));
  rc = proc_pidinfo(pid, PROC_PIDTBSDINFO, 0, &bsd, sizeof(bsd));
  if (rc == (int)sizeof(bsd) && bsd.pbi_comm[0] != '\0') {
    ftop_copy_bounded_string(buffer, buffer_len, bsd.pbi_comm);
  }
}

static int ftop_macos_store_connection(struct ftop_macos_net_connection_info *destination, pid_t pid,
    const char *process_name, const struct socket_fdinfo *fdinfo) {
  const struct in_sockinfo *in_info;
  const struct socket_info *socket_info;
  const char *protocol;
  char local_address[FTOP_NET_ADDRESS_LEN];
  char remote_address[FTOP_NET_ADDRESS_LEN];
  int family;
  int local_port;
  int remote_port;

  if (destination == NULL || fdinfo == NULL) return 0;
  memset(destination, 0, sizeof(*destination));
  socket_info = &fdinfo->psi;
  if (!ftop_macos_socket_protocol(socket_info, &protocol)) return 0;
  in_info = ftop_macos_socket_in_info(socket_info);
  if (in_info == NULL) return 0;
  family = ftop_macos_in_sockinfo_family(in_info, socket_info->soi_family);
  if (!ftop_macos_endpoint_from_in_sockinfo(in_info, socket_info->soi_family, 1, local_address, sizeof(local_address),
          &local_port)) {
    return 0;
  }
  if (!ftop_macos_endpoint_from_in_sockinfo(in_info, socket_info->soi_family, 0, remote_address, sizeof(remote_address),
          &remote_port)) {
    ftop_macos_unspecified_address(family, remote_address, sizeof(remote_address), &remote_port);
  }

  destination->socket_id = ftop_macos_socket_id(socket_info);
  if (destination->socket_id <= 0) return 0;
  destination->valid = 1;
  ftop_copy_bounded_string(destination->protocol, sizeof(destination->protocol), protocol);
  ftop_copy_bounded_string(destination->local_addr, sizeof(destination->local_addr), local_address);
  destination->local_port = local_port;
  ftop_copy_bounded_string(destination->remote_addr, sizeof(destination->remote_addr), remote_address);
  destination->remote_port = remote_port;
  ftop_copy_bounded_string(destination->state, sizeof(destination->state),
      socket_info->soi_protocol == IPPROTO_UDP ? "OPEN" : ftop_macos_tcp_state_label(socket_info->soi_proto.pri_tcp.tcpsi_state));
  destination->pid = pid > 0 ? (int)pid : 0;
  ftop_copy_bounded_string(destination->process_name, sizeof(destination->process_name), process_name);
  return 1;
}

static void ftop_macos_append_process_connections(pid_t pid, const char *process_name,
    struct ftop_macos_net_connection_info *buffer, size_t capacity, size_t *count) {
  struct proc_fdinfo *fd_infos;
  struct socket_fdinfo socket_fd_info;
  int byte_count;
  int rc;
  long long socket_id;
  size_t fd_count;
  size_t fd_index;

  if (pid <= 0 || process_name == NULL || buffer == NULL || count == NULL || *count >= capacity) return;

  byte_count = proc_pidinfo(pid, PROC_PIDLISTFDS, 0, NULL, 0);
  if (byte_count <= 0) return;
  fd_infos = (struct proc_fdinfo *)malloc((size_t)byte_count);
  if (fd_infos == NULL) return;

  byte_count = proc_pidinfo(pid, PROC_PIDLISTFDS, 0, fd_infos, byte_count);
  if (byte_count <= 0) {
    free(fd_infos);
    return;
  }

  fd_count = (size_t)byte_count / sizeof(*fd_infos);
  for (fd_index = 0U; fd_index < fd_count && *count < capacity; ++fd_index) {
    if (fd_infos[fd_index].proc_fd < 0) continue;
    if (fd_infos[fd_index].proc_fdtype != PROX_FDTYPE_SOCKET) continue;
    memset(&socket_fd_info, 0, sizeof(socket_fd_info));
    rc = proc_pidfdinfo(pid, fd_infos[fd_index].proc_fd, PROC_PIDFDSOCKETINFO, &socket_fd_info, sizeof(socket_fd_info));
    if (rc != (int)sizeof(socket_fd_info)) continue;
    if (!ftop_macos_store_connection(&buffer[*count], pid, process_name, &socket_fd_info)) continue;
    socket_id = buffer[*count].socket_id;
    if (ftop_macos_connection_exists(buffer, *count, socket_id, (int)pid)) continue;
    ++(*count);
  }

  free(fd_infos);
}

static int ftop_network_interface_exists(
    const struct ftop_macos_net_interface_info *buffer, size_t count, const char *name) {
  size_t i;

  for (i = 0U; i < count; ++i) {
    if (strncmp(buffer[i].name, name, FTOP_NET_INTERFACE_NAME_LEN) == 0) return 1;
  }
  return 0;
}

static void ftop_copy_network_interface(
    struct ftop_macos_net_interface_info *destination, const struct ifaddrs *source) {
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

int ftop_macos_network_interfaces(
    struct ftop_macos_net_interface_info *buffer, size_t capacity, size_t *interface_count, int *sys_errno) {
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

int ftop_macos_network_connections(
    struct ftop_macos_net_connection_info *buffer, size_t capacity, size_t *connection_count, int *sys_errno) {
  char process_name[FTOP_NET_PROCESS_NAME_LEN];
  pid_t *pids;
  int byte_count;
  int pid_count;
  int i;
  size_t count;

  if (buffer == NULL || connection_count == NULL || sys_errno == NULL || capacity == 0U) return -1;

  *connection_count = 0U;
  *sys_errno = 0;
  byte_count = proc_listpids(PROC_ALL_PIDS, 0, NULL, 0);
  if (byte_count <= 0) {
    *sys_errno = errno != 0 ? errno : EINVAL;
    return -1;
  }

  pids = (pid_t *)malloc((size_t)byte_count);
  if (pids == NULL) {
    *sys_errno = errno != 0 ? errno : ENOMEM;
    return -1;
  }

  byte_count = proc_listpids(PROC_ALL_PIDS, 0, pids, byte_count);
  if (byte_count <= 0) {
    *sys_errno = errno != 0 ? errno : EINVAL;
    free(pids);
    return -1;
  }

  pid_count = byte_count / (int)sizeof(pid_t);
  count = 0U;
  for (i = 0; i < pid_count && count < capacity; ++i) {
    if (pids[i] <= 0) continue;
    ftop_macos_process_name(pids[i], process_name, sizeof(process_name));
    ftop_macos_append_process_connections(pids[i], process_name, buffer, capacity, &count);
  }

  free(pids);
  *connection_count = count;
  return 0;
}

int ftop_macos_user_name(int uid, char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
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

static size_t ftop_bounded_strlen(const char *text, size_t capacity) {
  size_t i;

  for (i = 0U; i < capacity && text[i] != '\0'; ++i) {
  }
  return i;
}

int ftop_macos_sysctl_int(const char *name, int *value, int *sys_errno) {
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

int ftop_macos_sysctl_long_long(const char *name, long long *value, int *sys_errno) {
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

int ftop_macos_sysctl_string(const char *name, char *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
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
  *value_len = ftop_bounded_strlen(buffer, buffer_len);

  return 0;
}

int ftop_macos_sysctl_bytes(const char *name, void *buffer, size_t buffer_len, size_t *value_len, int *sys_errno) {
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

int ftop_macos_iokit_gpu_count(int *count, int *sys_errno) {
  if (count == NULL || sys_errno == NULL) return -1;

  *count = 0;
  *sys_errno = 0;
  return 0;
}

int ftop_macos_iokit_disk_count(int *count, int *sys_errno) {
  if (count == NULL || sys_errno == NULL) return -1;

  *count = 0;
  *sys_errno = 0;
  return 0;
}
