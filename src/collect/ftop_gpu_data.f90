module ftop_gpu_data
  use, intrinsic :: iso_fortran_env, only : int64, real64
  implicit none
  private

  integer, parameter, public :: GPU_CAPACITY = 16
  integer, parameter, public :: GPU_VENDOR_LEN = 16
  integer, parameter, public :: GPU_NAME_LEN = 128
  integer, parameter, public :: GPU_PCI_ID_LEN = 32
  integer, parameter, public :: GPU_DRIVER_VERSION_LEN = 64

  type, public :: gpu_info
    logical :: valid = .false.
    character(len=GPU_VENDOR_LEN) :: vendor = ""
    character(len=GPU_NAME_LEN) :: name = ""
    character(len=GPU_PCI_ID_LEN) :: pci_id = ""
    logical :: utilization_valid = .false.
    real(real64) :: utilization_percent = 0.0_real64
    logical :: memory_utilization_valid = .false.
    real(real64) :: memory_utilization_percent = 0.0_real64
    logical :: temperature_valid = .false.
    real(real64) :: temp_celsius = 0.0_real64
    real(real64) :: temp_max_celsius = 0.0_real64
    logical :: memory_valid = .false.
    integer(int64) :: memory_used_bytes = 0_int64
    integer(int64) :: memory_total_bytes = 0_int64
    logical :: power_valid = .false.
    real(real64) :: power_watts = 0.0_real64
    real(real64) :: power_limit_watts = 0.0_real64
    logical :: core_clock_valid = .false.
    real(real64) :: clock_core_mhz = 0.0_real64
    real(real64) :: clock_max_core_mhz = 0.0_real64
    logical :: memory_clock_valid = .false.
    real(real64) :: clock_memory_mhz = 0.0_real64
    logical :: fan_valid = .false.
    real(real64) :: fan_speed_percent = 0.0_real64
    logical :: encoder_valid = .false.
    real(real64) :: encoder_utilization_percent = 0.0_real64
    logical :: decoder_valid = .false.
    real(real64) :: decoder_utilization_percent = 0.0_real64
    logical :: driver_version_valid = .false.
    character(len=GPU_DRIVER_VERSION_LEN) :: driver_version = ""
  end type gpu_info

  type, public :: gpu_table
    logical :: valid = .false.
    type(gpu_info), allocatable :: gpus(:)
  end type gpu_table

  public :: empty_gpu_table
  public :: gpu_table_count

contains

  function empty_gpu_table() result(table)
    type(gpu_table) :: table

    table%valid = .true.
    allocate(table%gpus(0))
  end function empty_gpu_table

  integer function gpu_table_count(table) result(count)
    type(gpu_table), intent(in) :: table

    count = 0
    if (allocated(table%gpus)) count = size(table%gpus)
  end function gpu_table_count
end module ftop_gpu_data
