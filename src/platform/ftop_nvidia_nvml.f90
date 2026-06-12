module ftop_nvidia_nvml
  use, intrinsic :: iso_c_binding, only : &
    c_associated, &
    c_char, &
    c_f_procpointer, &
    c_funptr, &
    c_int, &
    c_loc, &
    c_long_long, &
    c_null_char, &
    c_null_ptr, &
    c_ptr
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use ftop_dl, only : ftop_dl_handle, ftop_dlclose, ftop_dlopen, ftop_dlsym
  use ftop_gpu_data, only : &
    GPU_CAPACITY, &
    GPU_DRIVER_VERSION_LEN, &
    GPU_NAME_LEN, &
    GPU_PROCESS_CAPACITY, &
    GPU_PROCESS_NAME_LEN, &
    empty_gpu_process_table, &
    empty_gpu_table, &
    gpu_info, &
    gpu_process_info, &
    gpu_process_table, &
    merge_gpu_process, &
    sort_gpu_process_table, &
    gpu_table
  implicit none
  private

  integer(c_int), parameter :: NVML_SUCCESS = 0_c_int
  integer(c_int), parameter :: NVML_ERROR_INSUFFICIENT_SIZE = 7_c_int
  integer(c_int), parameter :: NVML_TEMPERATURE_GPU = 0_c_int
  integer(c_int), parameter :: NVML_CLOCK_GRAPHICS = 0_c_int
  integer(c_int), parameter :: NVML_CLOCK_MEMORY = 2_c_int
  integer, parameter :: NVML_PCI_BUS_ID_LEN = 32

  type, bind(C) :: nvml_utilization_rates
    integer(c_int) :: gpu
    integer(c_int) :: memory
  end type nvml_utilization_rates

  type, bind(C) :: nvml_memory_info
    integer(c_long_long) :: total
    integer(c_long_long) :: free
    integer(c_long_long) :: used
  end type nvml_memory_info

  type, bind(C) :: nvml_pci_info
    character(kind=c_char) :: bus_id(NVML_PCI_BUS_ID_LEN)
    integer(c_int) :: domain
    integer(c_int) :: bus
    integer(c_int) :: device
    integer(c_int) :: pci_device_id
    integer(c_int) :: pci_subsystem_id
    integer(c_int) :: reserved0
    integer(c_int) :: reserved1
  end type nvml_pci_info

  type, bind(C) :: nvml_process_info
    integer(c_int) :: pid
    integer(c_long_long) :: used_gpu_memory
  end type nvml_process_info

  type, bind(C) :: nvml_process_utilization_sample
    integer(c_int) :: pid
    integer(c_long_long) :: time_stamp
    integer(c_int) :: sm_util
    integer(c_int) :: memory_util
    integer(c_int) :: encoder_util
    integer(c_int) :: decoder_util
  end type nvml_process_utilization_sample

  abstract interface
    integer(c_int) function nvml_init_fn() bind(C)
      import :: c_int
    end function nvml_init_fn

    integer(c_int) function nvml_get_count_fn(device_count) bind(C)
      import :: c_int
      integer(c_int), intent(out) :: device_count
    end function nvml_get_count_fn

    integer(c_int) function nvml_get_handle_fn(index, device) bind(C)
      import :: c_int, c_ptr
      integer(c_int), value :: index
      type(c_ptr), intent(out) :: device
    end function nvml_get_handle_fn

    integer(c_int) function nvml_get_device_string_fn(device, value, value_len) bind(C)
      import :: c_char, c_int, c_ptr
      type(c_ptr), value :: device
      character(kind=c_char), intent(out) :: value(*)
      integer(c_int), value :: value_len
    end function nvml_get_device_string_fn

    integer(c_int) function nvml_get_system_string_fn(value, value_len) bind(C)
      import :: c_char, c_int
      character(kind=c_char), intent(out) :: value(*)
      integer(c_int), value :: value_len
    end function nvml_get_system_string_fn

    integer(c_int) function nvml_get_process_name_fn(pid, value, value_len) bind(C)
      import :: c_char, c_int
      integer(c_int), value :: pid
      character(kind=c_char), intent(out) :: value(*)
      integer(c_int), value :: value_len
    end function nvml_get_process_name_fn

    integer(c_int) function nvml_get_utilization_fn(device, utilization) bind(C)
      import :: c_int, c_ptr, nvml_utilization_rates
      type(c_ptr), value :: device
      type(nvml_utilization_rates), intent(out) :: utilization
    end function nvml_get_utilization_fn

    integer(c_int) function nvml_get_memory_fn(device, memory) bind(C)
      import :: c_int, c_ptr, nvml_memory_info
      type(c_ptr), value :: device
      type(nvml_memory_info), intent(out) :: memory
    end function nvml_get_memory_fn

    integer(c_int) function nvml_get_pci_fn(device, pci) bind(C)
      import :: c_int, c_ptr, nvml_pci_info
      type(c_ptr), value :: device
      type(nvml_pci_info), intent(out) :: pci
    end function nvml_get_pci_fn

    integer(c_int) function nvml_get_temperature_fn(device, sensor_type, temperature) bind(C)
      import :: c_int, c_ptr
      type(c_ptr), value :: device
      integer(c_int), value :: sensor_type
      integer(c_int), intent(out) :: temperature
    end function nvml_get_temperature_fn

    integer(c_int) function nvml_get_unsigned_fn(device, value) bind(C)
      import :: c_int, c_ptr
      type(c_ptr), value :: device
      integer(c_int), intent(out) :: value
    end function nvml_get_unsigned_fn

    integer(c_int) function nvml_get_clock_fn(device, clock_type, clock_mhz) bind(C)
      import :: c_int, c_ptr
      type(c_ptr), value :: device
      integer(c_int), value :: clock_type
      integer(c_int), intent(out) :: clock_mhz
    end function nvml_get_clock_fn

    integer(c_int) function nvml_get_codec_utilization_fn(device, utilization, sample_period_us) bind(C)
      import :: c_int, c_ptr
      type(c_ptr), value :: device
      integer(c_int), intent(out) :: utilization
      integer(c_int), intent(out) :: sample_period_us
    end function nvml_get_codec_utilization_fn

    integer(c_int) function nvml_get_processes_fn(device, process_count, processes) bind(C)
      import :: c_int, c_ptr
      type(c_ptr), value :: device
      integer(c_int), intent(inout) :: process_count
      type(c_ptr), value :: processes
    end function nvml_get_processes_fn

    integer(c_int) function nvml_get_process_utilization_fn(device, samples, sample_count, last_seen_timestamp) bind(C)
      import :: c_int, c_long_long, c_ptr
      type(c_ptr), value :: device
      type(c_ptr), value :: samples
      integer(c_int), intent(inout) :: sample_count
      integer(c_long_long), value :: last_seen_timestamp
    end function nvml_get_process_utilization_fn
  end interface

  type :: nvml_api
    type(ftop_dl_handle) :: library
    procedure(nvml_init_fn), pointer, nopass :: init => null()
    procedure(nvml_init_fn), pointer, nopass :: shutdown => null()
    procedure(nvml_get_count_fn), pointer, nopass :: get_count => null()
    procedure(nvml_get_handle_fn), pointer, nopass :: get_handle => null()
    procedure(nvml_get_device_string_fn), pointer, nopass :: get_name => null()
    procedure(nvml_get_system_string_fn), pointer, nopass :: get_driver_version => null()
    procedure(nvml_get_process_name_fn), pointer, nopass :: get_process_name => null()
    procedure(nvml_get_utilization_fn), pointer, nopass :: get_utilization => null()
    procedure(nvml_get_memory_fn), pointer, nopass :: get_memory => null()
    procedure(nvml_get_pci_fn), pointer, nopass :: get_pci => null()
    procedure(nvml_get_temperature_fn), pointer, nopass :: get_temperature => null()
    procedure(nvml_get_unsigned_fn), pointer, nopass :: get_power_usage => null()
    procedure(nvml_get_unsigned_fn), pointer, nopass :: get_power_limit => null()
    procedure(nvml_get_unsigned_fn), pointer, nopass :: get_fan_speed => null()
    procedure(nvml_get_clock_fn), pointer, nopass :: get_clock => null()
    procedure(nvml_get_clock_fn), pointer, nopass :: get_max_clock => null()
    procedure(nvml_get_codec_utilization_fn), pointer, nopass :: get_encoder_utilization => null()
    procedure(nvml_get_codec_utilization_fn), pointer, nopass :: get_decoder_utilization => null()
    procedure(nvml_get_processes_fn), pointer, nopass :: get_compute_processes => null()
    procedure(nvml_get_processes_fn), pointer, nopass :: get_graphics_processes => null()
    procedure(nvml_get_process_utilization_fn), pointer, nopass :: get_process_utilization => null()
  end type nvml_api

  public :: nvidia_nvml_gpu_snapshot
  public :: nvidia_nvml_gpu_process_snapshot

contains

  logical function nvidia_nvml_gpu_snapshot(table, library_path) result(success)
    type(gpu_table), intent(out) :: table
    character(len=*), intent(in), optional :: library_path
    type(nvml_api) :: api
    logical :: initialized

    table = empty_gpu_table()
    success = .false.
    initialized = .false.
    if (.not. open_nvml(api, library_path)) return

    if (api%init() == NVML_SUCCESS) then
      initialized = .true.
      success = collect_nvml_gpus(api, table)
    end if

    if (initialized) call shutdown_nvml(api)
    call close_nvml(api)
    if (.not. success) table = empty_gpu_table()
  end function nvidia_nvml_gpu_snapshot

  logical function nvidia_nvml_gpu_process_snapshot(table, library_path) result(success)
    type(gpu_process_table), intent(out) :: table
    character(len=*), intent(in), optional :: library_path
    type(nvml_api) :: api
    logical :: initialized

    table = gpu_process_table()
    success = .false.
    initialized = .false.
    if (.not. open_nvml(api, library_path)) return

    if (api%init() == NVML_SUCCESS) then
      initialized = .true.
      success = collect_nvml_gpu_processes(api, table)
    end if

    if (initialized) call shutdown_nvml(api)
    call close_nvml(api)
    if (.not. success) table = gpu_process_table()
  end function nvidia_nvml_gpu_process_snapshot

  logical function open_nvml(api, library_path) result(success)
    type(nvml_api), intent(out) :: api
    character(len=*), intent(in), optional :: library_path

    api = nvml_api()
    if (present(library_path)) then
      success = open_nvml_library(api, trim(library_path))
    else
      success = open_nvml_library(api, "libnvidia-ml.so.1")
      if (.not. success) success = open_nvml_library(api, "libnvidia-ml.so")
      if (.not. success) success = open_nvml_library(api, "/usr/local/lib/libnvidia-ml.so.1")
      if (.not. success) success = open_nvml_library(api, "/usr/local/lib/libnvidia-ml.so")
    end if
    if (.not. success) return

    call resolve_nvml_symbols(api)
    success = associated(api%init) .and. associated(api%shutdown) .and. associated(api%get_count) .and. &
              associated(api%get_handle)
    if (.not. success) call close_nvml(api)
  end function open_nvml

  logical function open_nvml_library(api, path) result(success)
    type(nvml_api), intent(inout) :: api
    character(len=*), intent(in) :: path
    integer :: error_code

    success = ftop_dlopen(path, api%library, error_code)
    associate(unused_error_code => error_code)
    end associate
  end function open_nvml_library

  subroutine resolve_nvml_symbols(api)
    type(nvml_api), intent(inout) :: api
    type(c_funptr) :: symbol

    if (ftop_dlsym(api%library, "nvmlInit_v2", symbol)) call c_f_procpointer(symbol, api%init)
    if (.not. associated(api%init)) then
      if (ftop_dlsym(api%library, "nvmlInit", symbol)) call c_f_procpointer(symbol, api%init)
    end if
    if (ftop_dlsym(api%library, "nvmlShutdown", symbol)) call c_f_procpointer(symbol, api%shutdown)
    if (ftop_dlsym(api%library, "nvmlDeviceGetCount_v2", symbol)) call c_f_procpointer(symbol, api%get_count)
    if (.not. associated(api%get_count)) then
      if (ftop_dlsym(api%library, "nvmlDeviceGetCount", symbol)) call c_f_procpointer(symbol, api%get_count)
    end if
    if (ftop_dlsym(api%library, "nvmlDeviceGetHandleByIndex_v2", symbol)) then
      call c_f_procpointer(symbol, api%get_handle)
    end if
    if (.not. associated(api%get_handle)) then
      if (ftop_dlsym(api%library, "nvmlDeviceGetHandleByIndex", symbol)) call c_f_procpointer(symbol, api%get_handle)
    end if
    if (ftop_dlsym(api%library, "nvmlDeviceGetName", symbol)) call c_f_procpointer(symbol, api%get_name)
    if (ftop_dlsym(api%library, "nvmlSystemGetDriverVersion", symbol)) then
      call c_f_procpointer(symbol, api%get_driver_version)
    end if
    if (ftop_dlsym(api%library, "nvmlSystemGetProcessName", symbol)) then
      call c_f_procpointer(symbol, api%get_process_name)
    end if
    if (ftop_dlsym(api%library, "nvmlDeviceGetUtilizationRates", symbol)) then
      call c_f_procpointer(symbol, api%get_utilization)
    end if
    if (ftop_dlsym(api%library, "nvmlDeviceGetMemoryInfo", symbol)) call c_f_procpointer(symbol, api%get_memory)
    if (ftop_dlsym(api%library, "nvmlDeviceGetPciInfo_v3", symbol)) call c_f_procpointer(symbol, api%get_pci)
    if (ftop_dlsym(api%library, "nvmlDeviceGetTemperature", symbol)) call c_f_procpointer(symbol, api%get_temperature)
    if (ftop_dlsym(api%library, "nvmlDeviceGetPowerUsage", symbol)) call c_f_procpointer(symbol, api%get_power_usage)
    if (ftop_dlsym(api%library, "nvmlDeviceGetEnforcedPowerLimit", symbol)) then
      call c_f_procpointer(symbol, api%get_power_limit)
    end if
    if (ftop_dlsym(api%library, "nvmlDeviceGetFanSpeed", symbol)) call c_f_procpointer(symbol, api%get_fan_speed)
    if (ftop_dlsym(api%library, "nvmlDeviceGetClockInfo", symbol)) call c_f_procpointer(symbol, api%get_clock)
    if (ftop_dlsym(api%library, "nvmlDeviceGetMaxClockInfo", symbol)) call c_f_procpointer(symbol, api%get_max_clock)
    if (ftop_dlsym(api%library, "nvmlDeviceGetEncoderUtilization", symbol)) then
      call c_f_procpointer(symbol, api%get_encoder_utilization)
    end if
    if (ftop_dlsym(api%library, "nvmlDeviceGetDecoderUtilization", symbol)) then
      call c_f_procpointer(symbol, api%get_decoder_utilization)
    end if
    if (ftop_dlsym(api%library, "nvmlDeviceGetComputeRunningProcesses_v2", symbol)) then
      call c_f_procpointer(symbol, api%get_compute_processes)
    end if
    if (.not. associated(api%get_compute_processes)) then
      if (ftop_dlsym(api%library, "nvmlDeviceGetComputeRunningProcesses", symbol)) then
        call c_f_procpointer(symbol, api%get_compute_processes)
      end if
    end if
    if (ftop_dlsym(api%library, "nvmlDeviceGetGraphicsRunningProcesses_v2", symbol)) then
      call c_f_procpointer(symbol, api%get_graphics_processes)
    end if
    if (.not. associated(api%get_graphics_processes)) then
      if (ftop_dlsym(api%library, "nvmlDeviceGetGraphicsRunningProcesses", symbol)) then
        call c_f_procpointer(symbol, api%get_graphics_processes)
      end if
    end if
    if (ftop_dlsym(api%library, "nvmlDeviceGetProcessUtilization", symbol)) then
      call c_f_procpointer(symbol, api%get_process_utilization)
    end if
  end subroutine resolve_nvml_symbols

  logical function collect_nvml_gpus(api, table) result(success)
    type(nvml_api), intent(in) :: api
    type(gpu_table), intent(out) :: table
    character(len=GPU_DRIVER_VERSION_LEN) :: driver_version
    type(c_ptr) :: device
    integer(c_int) :: c_count
    integer :: gpu_count
    integer :: gpu_index

    table = empty_gpu_table()
    success = .false.
    if (api%get_count(c_count) /= NVML_SUCCESS) return
    if (c_count < 0_c_int) return

    driver_version = nvml_driver_version(api)
    gpu_count = min(int(c_count), GPU_CAPACITY)
    if (allocated(table%gpus)) deallocate(table%gpus)
    allocate(table%gpus(gpu_count))
    table%valid = .true.

    do gpu_index = 1, gpu_count
      if (api%get_handle(int(gpu_index - 1, c_int), device) /= NVML_SUCCESS) cycle
      if (.not. c_associated(device)) cycle
      call populate_nvml_gpu(api, device, driver_version, table%gpus(gpu_index))
    end do
    success = .true.
  end function collect_nvml_gpus

  logical function collect_nvml_gpu_processes(api, table) result(success)
    type(nvml_api), intent(in) :: api
    type(gpu_process_table), intent(out) :: table
    type(gpu_process_info) :: processes(GPU_PROCESS_CAPACITY)
    type(c_ptr) :: device
    integer(c_int) :: c_count
    integer :: device_count
    integer :: device_index
    integer :: process_count

    table = empty_gpu_process_table()
    processes = gpu_process_info()
    process_count = 0
    success = .false.
    if (api%get_count(c_count) /= NVML_SUCCESS) return
    if (c_count < 0_c_int) return

    device_count = min(int(c_count), GPU_CAPACITY)
    do device_index = 1, device_count
      if (api%get_handle(int(device_index - 1, c_int), device) /= NVML_SUCCESS) cycle
      if (.not. c_associated(device)) cycle
      call collect_nvml_device_processes(api, device, processes, process_count)
    end do

    if (allocated(table%processes)) deallocate(table%processes)
    allocate(table%processes(process_count))
    if (process_count > 0) table%processes = processes(:process_count)
    table%valid = .true.
    call sort_gpu_process_table(table)
    success = .true.
  end function collect_nvml_gpu_processes

  subroutine collect_nvml_device_processes(api, device, processes, process_count)
    type(nvml_api), intent(in) :: api
    type(c_ptr), intent(in) :: device
    type(gpu_process_info), intent(inout) :: processes(:)
    integer, intent(inout) :: process_count

    if (associated(api%get_compute_processes)) then
      call collect_nvml_running_processes(api, device, "compute", api%get_compute_processes, processes, process_count)
    end if
    if (associated(api%get_graphics_processes)) then
      call collect_nvml_running_processes(api, device, "graphics", api%get_graphics_processes, processes, process_count)
    end if
    if (associated(api%get_process_utilization)) then
      call collect_nvml_process_utilization(api, device, processes, process_count)
    end if
  end subroutine collect_nvml_device_processes

  subroutine collect_nvml_running_processes(api, device, engine, get_processes, processes, process_count)
    type(nvml_api), intent(in) :: api
    type(c_ptr), intent(in) :: device
    character(len=*), intent(in) :: engine
    procedure(nvml_get_processes_fn), pointer, intent(in) :: get_processes
    type(gpu_process_info), intent(inout) :: processes(:)
    integer, intent(inout) :: process_count
    type(nvml_process_info), target :: nvml_processes(GPU_PROCESS_CAPACITY)
    type(gpu_process_info) :: process
    integer(c_int) :: info_count
    integer(c_int) :: rc
    integer :: info_index
    integer :: limit

    nvml_processes = nvml_process_info(0_c_int, 0_c_long_long)
    info_count = 0_c_int
    rc = get_processes(device, info_count, c_null_ptr)
    if (rc == NVML_SUCCESS .and. info_count <= 0_c_int) return
    if (rc /= NVML_SUCCESS .and. rc /= NVML_ERROR_INSUFFICIENT_SIZE) then
      info_count = int(size(nvml_processes), c_int)
    else
      info_count = int(min(int(info_count), size(nvml_processes)), c_int)
    end if
    if (info_count <= 0_c_int) return

    rc = get_processes(device, info_count, c_loc(nvml_processes(1)))
    if (rc /= NVML_SUCCESS) return
    limit = min(int(info_count), size(nvml_processes))
    do info_index = 1, limit
      process = nvml_running_process_info(api, nvml_processes(info_index), engine)
      call merge_gpu_process(processes, process_count, process)
    end do
  end subroutine collect_nvml_running_processes

  subroutine collect_nvml_process_utilization(api, device, processes, process_count)
    type(nvml_api), intent(in) :: api
    type(c_ptr), intent(in) :: device
    type(gpu_process_info), intent(inout) :: processes(:)
    integer, intent(inout) :: process_count
    type(nvml_process_utilization_sample), target :: samples(GPU_PROCESS_CAPACITY)
    type(gpu_process_info) :: process
    integer(c_int) :: sample_count
    integer(c_int) :: rc
    integer :: limit
    integer :: sample_index

    samples = nvml_process_utilization_sample(0_c_int, 0_c_long_long, 0_c_int, 0_c_int, 0_c_int, 0_c_int)
    sample_count = 0_c_int
    rc = api%get_process_utilization(device, c_null_ptr, sample_count, 0_c_long_long)
    if (rc == NVML_SUCCESS .and. sample_count <= 0_c_int) return
    if (rc /= NVML_SUCCESS .and. rc /= NVML_ERROR_INSUFFICIENT_SIZE) then
      sample_count = int(size(samples), c_int)
    else
      sample_count = int(min(int(sample_count), size(samples)), c_int)
    end if
    if (sample_count <= 0_c_int) return

    rc = api%get_process_utilization(device, c_loc(samples(1)), sample_count, 0_c_long_long)
    if (rc /= NVML_SUCCESS) return
    limit = min(int(sample_count), size(samples))
    do sample_index = 1, limit
      process = nvml_process_utilization_info(api, samples(sample_index))
      call merge_gpu_process(processes, process_count, process)
    end do
  end subroutine collect_nvml_process_utilization

  function nvml_running_process_info(api, nvml_process, engine) result(process)
    type(nvml_api), intent(in) :: api
    type(nvml_process_info), intent(in) :: nvml_process
    character(len=*), intent(in) :: engine
    type(gpu_process_info) :: process

    process = gpu_process_info()
    if (nvml_process%pid <= 0_c_int) return
    process%valid = .true.
    process%pid = int(nvml_process%pid)
    process%process_name = nvml_process_name(api, process%pid)
    process%engine = engine
    if (nvml_process%used_gpu_memory > 0_c_long_long) then
      process%memory_valid = .true.
      process%memory_bytes = int(nvml_process%used_gpu_memory, int64)
    end if
  end function nvml_running_process_info

  function nvml_process_utilization_info(api, sample) result(process)
    type(nvml_api), intent(in) :: api
    type(nvml_process_utilization_sample), intent(in) :: sample
    type(gpu_process_info) :: process
    integer :: utilization

    process = gpu_process_info()
    if (sample%pid <= 0_c_int) return
    utilization = max(0, int(sample%sm_util))
    utilization = max(utilization, max(0, int(sample%encoder_util)))
    utilization = max(utilization, max(0, int(sample%decoder_util)))
    process%valid = .true.
    process%pid = int(sample%pid)
    process%process_name = nvml_process_name(api, process%pid)
    process%busy_percent_valid = .true.
    process%busy_percent = real(utilization, real64)
  end function nvml_process_utilization_info

  subroutine populate_nvml_gpu(api, device, driver_version, gpu)
    type(nvml_api), intent(in) :: api
    type(c_ptr), intent(in) :: device
    character(len=*), intent(in) :: driver_version
    type(gpu_info), intent(out) :: gpu

    gpu = gpu_info()
    gpu%valid = .true.
    gpu%vendor = "nvidia"
    if (len_trim(driver_version) > 0) then
      gpu%driver_version_valid = .true.
      gpu%driver_version = driver_version
    end if

    call read_nvml_name(api, device, gpu)
    call read_nvml_pci(api, device, gpu)
    call read_nvml_utilization(api, device, gpu)
    call read_nvml_memory(api, device, gpu)
    call read_nvml_temperature(api, device, gpu)
    call read_nvml_power(api, device, gpu)
    call read_nvml_clocks(api, device, gpu)
    call read_nvml_fan(api, device, gpu)
    call read_nvml_codecs(api, device, gpu)
  end subroutine populate_nvml_gpu

  function nvml_driver_version(api) result(version)
    type(nvml_api), intent(in) :: api
    character(len=GPU_DRIVER_VERSION_LEN) :: version
    character(kind=c_char) :: buffer(GPU_DRIVER_VERSION_LEN)

    version = ""
    if (.not. associated(api%get_driver_version)) return
    buffer = c_null_char
    if (api%get_driver_version(buffer, int(size(buffer), c_int)) == NVML_SUCCESS) then
      call assign_c_string(buffer, version)
    end if
  end function nvml_driver_version

  function nvml_process_name(api, pid) result(name)
    type(nvml_api), intent(in) :: api
    integer, intent(in) :: pid
    character(len=GPU_PROCESS_NAME_LEN) :: name
    character(kind=c_char) :: buffer(GPU_PROCESS_NAME_LEN)

    name = ""
    if (.not. associated(api%get_process_name)) return
    if (pid <= 0) return
    buffer = c_null_char
    if (api%get_process_name(int(pid, c_int), buffer, int(size(buffer), c_int)) == NVML_SUCCESS) then
      call assign_c_string(buffer, name)
    end if
  end function nvml_process_name

  subroutine read_nvml_name(api, device, gpu)
    type(nvml_api), intent(in) :: api
    type(c_ptr), intent(in) :: device
    type(gpu_info), intent(inout) :: gpu
    character(kind=c_char) :: buffer(GPU_NAME_LEN)

    if (.not. associated(api%get_name)) return
    buffer = c_null_char
    if (api%get_name(device, buffer, int(size(buffer), c_int)) == NVML_SUCCESS) then
      call assign_c_string(buffer, gpu%name)
    end if
  end subroutine read_nvml_name

  subroutine read_nvml_pci(api, device, gpu)
    type(nvml_api), intent(in) :: api
    type(c_ptr), intent(in) :: device
    type(gpu_info), intent(inout) :: gpu
    type(nvml_pci_info) :: pci

    if (.not. associated(api%get_pci)) return
    call clear_pci_info(pci)
    if (api%get_pci(device, pci) == NVML_SUCCESS) call assign_c_string(pci%bus_id, gpu%pci_id)
  end subroutine read_nvml_pci

  subroutine read_nvml_utilization(api, device, gpu)
    type(nvml_api), intent(in) :: api
    type(c_ptr), intent(in) :: device
    type(gpu_info), intent(inout) :: gpu
    type(nvml_utilization_rates) :: utilization

    if (.not. associated(api%get_utilization)) return
    utilization = nvml_utilization_rates(0_c_int, 0_c_int)
    if (api%get_utilization(device, utilization) /= NVML_SUCCESS) return
    gpu%utilization_valid = .true.
    gpu%utilization_percent = real(max(0, int(utilization%gpu)), real64)
    gpu%memory_utilization_valid = .true.
    gpu%memory_utilization_percent = real(max(0, int(utilization%memory)), real64)
  end subroutine read_nvml_utilization

  subroutine read_nvml_memory(api, device, gpu)
    type(nvml_api), intent(in) :: api
    type(c_ptr), intent(in) :: device
    type(gpu_info), intent(inout) :: gpu
    type(nvml_memory_info) :: memory

    if (.not. associated(api%get_memory)) return
    memory = nvml_memory_info(0_c_long_long, 0_c_long_long, 0_c_long_long)
    if (api%get_memory(device, memory) /= NVML_SUCCESS) return
    if (memory%total <= 0_c_long_long) return
    gpu%memory_valid = .true.
    gpu%memory_total_bytes = int(max(0_c_long_long, memory%total), int64)
    gpu%memory_used_bytes = int(max(0_c_long_long, memory%used), int64)
  end subroutine read_nvml_memory

  subroutine read_nvml_temperature(api, device, gpu)
    type(nvml_api), intent(in) :: api
    type(c_ptr), intent(in) :: device
    type(gpu_info), intent(inout) :: gpu
    integer(c_int) :: temperature

    if (.not. associated(api%get_temperature)) return
    if (api%get_temperature(device, NVML_TEMPERATURE_GPU, temperature) /= NVML_SUCCESS) return
    gpu%temperature_valid = .true.
    gpu%temp_celsius = real(max(0, int(temperature)), real64)
  end subroutine read_nvml_temperature

  subroutine read_nvml_power(api, device, gpu)
    type(nvml_api), intent(in) :: api
    type(c_ptr), intent(in) :: device
    type(gpu_info), intent(inout) :: gpu
    integer(c_int) :: milliwatts

    if (associated(api%get_power_usage)) then
      if (api%get_power_usage(device, milliwatts) == NVML_SUCCESS) then
        gpu%power_valid = .true.
        gpu%power_watts = real(max(0, int(milliwatts)), real64) / 1000.0_real64
      end if
    end if
    if (associated(api%get_power_limit)) then
      if (api%get_power_limit(device, milliwatts) == NVML_SUCCESS) then
        gpu%power_limit_watts = real(max(0, int(milliwatts)), real64) / 1000.0_real64
      end if
    end if
  end subroutine read_nvml_power

  subroutine read_nvml_clocks(api, device, gpu)
    type(nvml_api), intent(in) :: api
    type(c_ptr), intent(in) :: device
    type(gpu_info), intent(inout) :: gpu
    integer(c_int) :: clock_mhz

    if (associated(api%get_clock)) then
      if (api%get_clock(device, NVML_CLOCK_GRAPHICS, clock_mhz) == NVML_SUCCESS) then
        gpu%core_clock_valid = .true.
        gpu%clock_core_mhz = real(max(0, int(clock_mhz)), real64)
      end if
      if (api%get_clock(device, NVML_CLOCK_MEMORY, clock_mhz) == NVML_SUCCESS) then
        gpu%memory_clock_valid = .true.
        gpu%clock_memory_mhz = real(max(0, int(clock_mhz)), real64)
      end if
    end if
    if (associated(api%get_max_clock)) then
      if (api%get_max_clock(device, NVML_CLOCK_GRAPHICS, clock_mhz) == NVML_SUCCESS) then
        gpu%clock_max_core_mhz = real(max(0, int(clock_mhz)), real64)
      end if
    end if
  end subroutine read_nvml_clocks

  subroutine read_nvml_fan(api, device, gpu)
    type(nvml_api), intent(in) :: api
    type(c_ptr), intent(in) :: device
    type(gpu_info), intent(inout) :: gpu
    integer(c_int) :: fan_percent

    if (.not. associated(api%get_fan_speed)) return
    if (api%get_fan_speed(device, fan_percent) /= NVML_SUCCESS) return
    gpu%fan_valid = .true.
    gpu%fan_speed_percent = real(max(0, int(fan_percent)), real64)
  end subroutine read_nvml_fan

  subroutine read_nvml_codecs(api, device, gpu)
    type(nvml_api), intent(in) :: api
    type(c_ptr), intent(in) :: device
    type(gpu_info), intent(inout) :: gpu
    integer(c_int) :: sample_period_us
    integer(c_int) :: utilization

    if (associated(api%get_encoder_utilization)) then
      if (api%get_encoder_utilization(device, utilization, sample_period_us) == NVML_SUCCESS) then
        gpu%encoder_valid = .true.
        gpu%encoder_utilization_percent = real(max(0, int(utilization)), real64)
      end if
    end if
    if (associated(api%get_decoder_utilization)) then
      if (api%get_decoder_utilization(device, utilization, sample_period_us) == NVML_SUCCESS) then
        gpu%decoder_valid = .true.
        gpu%decoder_utilization_percent = real(max(0, int(utilization)), real64)
      end if
    end if
    associate(unused_sample_period_us => sample_period_us)
    end associate
  end subroutine read_nvml_codecs

  subroutine clear_pci_info(pci)
    type(nvml_pci_info), intent(out) :: pci

    pci%bus_id = c_null_char
    pci%domain = 0_c_int
    pci%bus = 0_c_int
    pci%device = 0_c_int
    pci%pci_device_id = 0_c_int
    pci%pci_subsystem_id = 0_c_int
    pci%reserved0 = 0_c_int
    pci%reserved1 = 0_c_int
  end subroutine clear_pci_info

  subroutine assign_c_string(source, target)
    character(kind=c_char), intent(in) :: source(:)
    character(len=*), intent(out) :: target
    integer :: char_index
    integer :: limit

    target = ""
    limit = min(size(source), len(target))
    do char_index = 1, limit
      if (source(char_index) == c_null_char) exit
      target(char_index:char_index) = achar(iachar(source(char_index)))
    end do
  end subroutine assign_c_string

  subroutine shutdown_nvml(api)
    type(nvml_api), intent(in) :: api
    integer(c_int) :: rc

    if (.not. associated(api%shutdown)) return
    rc = api%shutdown()
    associate(unused_rc => rc)
    end associate
  end subroutine shutdown_nvml

  subroutine close_nvml(api)
    type(nvml_api), intent(inout) :: api
    integer :: error_code
    logical :: closed

    if (.not. c_associated(api%library%handle)) return
    closed = ftop_dlclose(api%library, error_code)
    associate(unused_closed => closed, unused_error_code => error_code)
    end associate
  end subroutine close_nvml

end module ftop_nvidia_nvml
