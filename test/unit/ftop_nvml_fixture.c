#include <stdint.h>
#include <stdio.h>
#include <string.h>

#define NVML_SUCCESS 0
#define NVML_ERROR_INVALID_ARGUMENT 2
#define NVML_ERROR_INSUFFICIENT_SIZE 7
#define GIB 1073741824ULL

typedef void *nvmlDevice_t;

struct fake_device {
  unsigned int index;
};

struct nvmlUtilization_st {
  unsigned int gpu;
  unsigned int memory;
};

struct nvmlMemory_st {
  unsigned long long total;
  unsigned long long free;
  unsigned long long used;
};

struct nvmlPciInfo_st {
  char busId[32];
  unsigned int domain;
  unsigned int bus;
  unsigned int device;
  unsigned int pciDeviceId;
  unsigned int pciSubSystemId;
  unsigned int reserved0;
  unsigned int reserved1;
};

struct nvmlProcessInfo_st {
  unsigned int pid;
  unsigned long long usedGpuMemory;
};

struct nvmlProcessUtilizationSample_st {
  unsigned int pid;
  unsigned long long timeStamp;
  unsigned int smUtil;
  unsigned int memUtil;
  unsigned int encUtil;
  unsigned int decUtil;
};

static struct fake_device devices[1] = {{0U}};

static int copy_string(char *value, unsigned int value_len, const char *source) {
  int written;

  if (value == NULL || source == NULL || value_len == 0U) return NVML_ERROR_INVALID_ARGUMENT;
  written = snprintf(value, value_len, "%s", source);
  if (written < 0) return NVML_ERROR_INVALID_ARGUMENT;
  value[value_len - 1U] = '\0';
  return NVML_SUCCESS;
}

static int device_index(nvmlDevice_t device, unsigned int *index) {
  struct fake_device *fake;

  if (device == NULL || index == NULL) return NVML_ERROR_INVALID_ARGUMENT;
  fake = (struct fake_device *)device;
  *index = fake->index;
  return *index < 1U ? NVML_SUCCESS : NVML_ERROR_INVALID_ARGUMENT;
}

int nvmlInit_v2(void) {
  return NVML_SUCCESS;
}

int nvmlShutdown(void) {
  return NVML_SUCCESS;
}

int nvmlSystemGetDriverVersion(char *version, unsigned int version_len) {
  return copy_string(version, version_len, "555.42.01");
}

int nvmlSystemGetProcessName(unsigned int pid, char *name, unsigned int name_len) {
  if (pid == 4242U) return copy_string(name, name_len, "cuda-work");
  if (pid == 5151U) return copy_string(name, name_len, "graphics-app");
  return copy_string(name, name_len, "unknown");
}

int nvmlDeviceGetCount_v2(unsigned int *device_count) {
  if (device_count == NULL) return NVML_ERROR_INVALID_ARGUMENT;
  *device_count = 1U;
  return NVML_SUCCESS;
}

int nvmlDeviceGetHandleByIndex_v2(unsigned int index, nvmlDevice_t *device) {
  if (device == NULL || index >= 1U) return NVML_ERROR_INVALID_ARGUMENT;
  *device = &devices[index];
  return NVML_SUCCESS;
}

int nvmlDeviceGetName(nvmlDevice_t device, char *name, unsigned int name_len) {
  unsigned int index;

  if (device_index(device, &index) != NVML_SUCCESS) return NVML_ERROR_INVALID_ARGUMENT;
  return copy_string(name, name_len, index == 0U ? "Fixture RTX" : "unknown");
}

int nvmlDeviceGetPciInfo_v3(nvmlDevice_t device, struct nvmlPciInfo_st *pci) {
  unsigned int index;

  if (device_index(device, &index) != NVML_SUCCESS || pci == NULL) return NVML_ERROR_INVALID_ARGUMENT;
  memset(pci, 0, sizeof(*pci));
  (void)copy_string(pci->busId, sizeof(pci->busId), "00000000:65:00.0");
  pci->domain = 0U;
  pci->bus = 0x65U;
  pci->device = 0U;
  pci->pciDeviceId = 0x268410DEU;
  return NVML_SUCCESS;
}

int nvmlDeviceGetUtilizationRates(nvmlDevice_t device, struct nvmlUtilization_st *utilization) {
  unsigned int index;

  if (device_index(device, &index) != NVML_SUCCESS || utilization == NULL) return NVML_ERROR_INVALID_ARGUMENT;
  utilization->gpu = 75U;
  utilization->memory = 41U;
  return NVML_SUCCESS;
}

int nvmlDeviceGetMemoryInfo(nvmlDevice_t device, struct nvmlMemory_st *memory) {
  unsigned int index;

  if (device_index(device, &index) != NVML_SUCCESS || memory == NULL) return NVML_ERROR_INVALID_ARGUMENT;
  memory->total = 12ULL * GIB;
  memory->used = 6ULL * GIB;
  memory->free = memory->total - memory->used;
  return NVML_SUCCESS;
}

int nvmlDeviceGetTemperature(nvmlDevice_t device, unsigned int sensor_type, unsigned int *temperature) {
  unsigned int index;

  if (device_index(device, &index) != NVML_SUCCESS || temperature == NULL) return NVML_ERROR_INVALID_ARGUMENT;
  if (sensor_type != 0U) return NVML_ERROR_INVALID_ARGUMENT;
  *temperature = 64U;
  return NVML_SUCCESS;
}

int nvmlDeviceGetPowerUsage(nvmlDevice_t device, unsigned int *power_mw) {
  unsigned int index;

  if (device_index(device, &index) != NVML_SUCCESS || power_mw == NULL) return NVML_ERROR_INVALID_ARGUMENT;
  *power_mw = 123000U;
  return NVML_SUCCESS;
}

int nvmlDeviceGetEnforcedPowerLimit(nvmlDevice_t device, unsigned int *power_mw) {
  unsigned int index;

  if (device_index(device, &index) != NVML_SUCCESS || power_mw == NULL) return NVML_ERROR_INVALID_ARGUMENT;
  *power_mw = 250000U;
  return NVML_SUCCESS;
}

int nvmlDeviceGetClockInfo(nvmlDevice_t device, unsigned int clock_type, unsigned int *clock_mhz) {
  unsigned int index;

  if (device_index(device, &index) != NVML_SUCCESS || clock_mhz == NULL) return NVML_ERROR_INVALID_ARGUMENT;
  if (clock_type == 0U) {
    *clock_mhz = 1800U;
  } else if (clock_type == 2U) {
    *clock_mhz = 9500U;
  } else {
    return NVML_ERROR_INVALID_ARGUMENT;
  }
  return NVML_SUCCESS;
}

int nvmlDeviceGetMaxClockInfo(nvmlDevice_t device, unsigned int clock_type, unsigned int *clock_mhz) {
  unsigned int index;

  if (device_index(device, &index) != NVML_SUCCESS || clock_mhz == NULL) return NVML_ERROR_INVALID_ARGUMENT;
  if (clock_type != 0U) return NVML_ERROR_INVALID_ARGUMENT;
  *clock_mhz = 2100U;
  return NVML_SUCCESS;
}

int nvmlDeviceGetFanSpeed(nvmlDevice_t device, unsigned int *fan_speed) {
  unsigned int index;

  if (device_index(device, &index) != NVML_SUCCESS || fan_speed == NULL) return NVML_ERROR_INVALID_ARGUMENT;
  *fan_speed = 55U;
  return NVML_SUCCESS;
}

int nvmlDeviceGetEncoderUtilization(nvmlDevice_t device, unsigned int *utilization, unsigned int *period_us) {
  unsigned int index;

  if (device_index(device, &index) != NVML_SUCCESS || utilization == NULL || period_us == NULL) {
    return NVML_ERROR_INVALID_ARGUMENT;
  }
  *utilization = 3U;
  *period_us = 1000000U;
  return NVML_SUCCESS;
}

int nvmlDeviceGetDecoderUtilization(nvmlDevice_t device, unsigned int *utilization, unsigned int *period_us) {
  unsigned int index;

  if (device_index(device, &index) != NVML_SUCCESS || utilization == NULL || period_us == NULL) {
    return NVML_ERROR_INVALID_ARGUMENT;
  }
  *utilization = 4U;
  *period_us = 1000000U;
  return NVML_SUCCESS;
}

int nvmlDeviceGetComputeRunningProcesses_v2(
    nvmlDevice_t device, unsigned int *process_count, struct nvmlProcessInfo_st *processes) {
  unsigned int index;

  if (device_index(device, &index) != NVML_SUCCESS || process_count == NULL) return NVML_ERROR_INVALID_ARGUMENT;
  if (processes == NULL || *process_count < 1U) {
    *process_count = 1U;
    return NVML_ERROR_INSUFFICIENT_SIZE;
  }
  *process_count = 1U;
  processes[0].pid = 4242U;
  processes[0].usedGpuMemory = 512ULL * 1024ULL * 1024ULL;
  return NVML_SUCCESS;
}

int nvmlDeviceGetGraphicsRunningProcesses_v2(
    nvmlDevice_t device, unsigned int *process_count, struct nvmlProcessInfo_st *processes) {
  unsigned int index;

  if (device_index(device, &index) != NVML_SUCCESS || process_count == NULL) return NVML_ERROR_INVALID_ARGUMENT;
  if (processes == NULL || *process_count < 1U) {
    *process_count = 1U;
    return NVML_ERROR_INSUFFICIENT_SIZE;
  }
  *process_count = 1U;
  processes[0].pid = 5151U;
  processes[0].usedGpuMemory = 256ULL * 1024ULL * 1024ULL;
  return NVML_SUCCESS;
}

int nvmlDeviceGetProcessUtilization(
    nvmlDevice_t device, struct nvmlProcessUtilizationSample_st *samples, unsigned int *sample_count,
    unsigned long long last_seen_timestamp) {
  unsigned int index;

  (void)last_seen_timestamp;
  if (device_index(device, &index) != NVML_SUCCESS || sample_count == NULL) return NVML_ERROR_INVALID_ARGUMENT;
  if (samples == NULL || *sample_count < 2U) {
    *sample_count = 2U;
    return NVML_ERROR_INSUFFICIENT_SIZE;
  }
  *sample_count = 2U;
  samples[0].pid = 4242U;
  samples[0].timeStamp = 1000000ULL;
  samples[0].smUtil = 65U;
  samples[0].memUtil = 20U;
  samples[0].encUtil = 0U;
  samples[0].decUtil = 0U;
  samples[1].pid = 5151U;
  samples[1].timeStamp = 1000001ULL;
  samples[1].smUtil = 35U;
  samples[1].memUtil = 12U;
  samples[1].encUtil = 0U;
  samples[1].decUtil = 0U;
  return NVML_SUCCESS;
}
