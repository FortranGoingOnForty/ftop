#include <errno.h>
#include <CoreFoundation/CoreFoundation.h>
#include <IOKit/IOKitLib.h>
#include <mach/host_info.h>
#include <mach/mach_host.h>
#include <mach/mach_init.h>
#include <mach/processor_info.h>
#include <mach/vm_map.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/sysctl.h>
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
