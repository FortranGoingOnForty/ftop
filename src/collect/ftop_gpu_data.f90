module ftop_gpu_data
  use, intrinsic :: iso_fortran_env, only : int64, real64
  implicit none
  private

  integer, parameter, public :: GPU_CAPACITY = 16
  integer, parameter, public :: GPU_VENDOR_LEN = 16
  integer, parameter, public :: GPU_NAME_LEN = 128
  integer, parameter, public :: GPU_PCI_ID_LEN = 32
  integer, parameter, public :: GPU_DRIVER_VERSION_LEN = 64
  integer, parameter, public :: GPU_PROCESS_CAPACITY = 64
  integer, parameter, public :: GPU_PROCESS_NAME_LEN = 64
  integer, parameter, public :: GPU_ENGINE_LEN = 32

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

  type, public :: gpu_process_info
    logical :: valid = .false.
    integer :: pid = 0
    integer(int64) :: start_time = 0_int64
    character(len=GPU_PROCESS_NAME_LEN) :: process_name = ""
    character(len=GPU_ENGINE_LEN) :: engine = ""
    integer(int64) :: engine_time_ns = 0_int64
    logical :: busy_percent_valid = .false.
    real(real64) :: busy_percent = 0.0_real64
    logical :: memory_valid = .false.
    integer(int64) :: memory_bytes = 0_int64
  end type gpu_process_info

  type, public :: gpu_process_table
    logical :: valid = .false.
    type(gpu_process_info), allocatable :: processes(:)
  end type gpu_process_table

  public :: empty_gpu_table
  public :: empty_gpu_process_table
  public :: assign_gpu_process_busy_percent
  public :: gpu_process_table_count
  public :: gpu_table_count
  public :: merge_gpu_process
  public :: sort_gpu_process_table

contains

  function empty_gpu_table() result(table)
    type(gpu_table) :: table

    table%valid = .true.
    allocate(table%gpus(0))
  end function empty_gpu_table

  function empty_gpu_process_table() result(table)
    type(gpu_process_table) :: table

    table%valid = .true.
    allocate(table%processes(0))
  end function empty_gpu_process_table

  integer function gpu_table_count(table) result(count)
    type(gpu_table), intent(in) :: table

    count = 0
    if (allocated(table%gpus)) count = size(table%gpus)
  end function gpu_table_count

  integer function gpu_process_table_count(table) result(count)
    type(gpu_process_table), intent(in) :: table

    count = 0
    if (allocated(table%processes)) count = size(table%processes)
  end function gpu_process_table_count

  subroutine assign_gpu_process_busy_percent(current, previous, elapsed_ms)
    type(gpu_process_table), intent(inout) :: current
    type(gpu_process_table), intent(in) :: previous
    integer(int64), intent(in) :: elapsed_ms
    integer :: current_index
    integer :: previous_index
    integer(int64) :: elapsed_ns
    integer(int64) :: time_delta_ns

    if (.not. allocated(current%processes)) return
    if (.not. current%valid .or. .not. previous%valid) return
    if (.not. allocated(previous%processes)) return
    if (elapsed_ms <= 0_int64) return

    elapsed_ns = elapsed_ms * 1000000_int64
    if (elapsed_ns <= 0_int64) return
    do current_index = 1, size(current%processes)
      if (.not. current%processes(current_index)%valid) cycle
      if (current%processes(current_index)%busy_percent_valid) cycle
      previous_index = matching_previous_gpu_process(previous, current%processes(current_index))
      if (previous_index <= 0) cycle

      time_delta_ns = current%processes(current_index)%engine_time_ns - &
                      previous%processes(previous_index)%engine_time_ns
      if (time_delta_ns <= 0_int64) cycle
      current%processes(current_index)%busy_percent_valid = .true.
      current%processes(current_index)%busy_percent = &
        100.0_real64 * real(time_delta_ns, real64) / real(elapsed_ns, real64)
    end do
  end subroutine assign_gpu_process_busy_percent

  integer function matching_previous_gpu_process(previous, process) result(item_index)
    type(gpu_process_table), intent(in) :: previous
    type(gpu_process_info), intent(in) :: process
    integer :: candidate

    item_index = 0
    if (process%pid <= 0) return
    if (.not. allocated(previous%processes)) return
    do candidate = 1, size(previous%processes)
      if (same_gpu_process(previous%processes(candidate), process)) then
        item_index = candidate
        return
      end if
    end do
  end function matching_previous_gpu_process

  logical function same_gpu_process(left, right) result(same)
    type(gpu_process_info), intent(in) :: left
    type(gpu_process_info), intent(in) :: right

    same = .false.
    if (.not. left%valid .or. .not. right%valid) return
    if (left%pid <= 0 .or. left%pid /= right%pid) return
    if (left%start_time > 0_int64 .and. right%start_time > 0_int64) then
      if (left%start_time /= right%start_time) return
    end if
    same = trim(left%engine) == trim(right%engine)
  end function same_gpu_process

  subroutine merge_gpu_process(processes, process_count, process)
    type(gpu_process_info), intent(inout) :: processes(:)
    integer, intent(inout) :: process_count
    type(gpu_process_info), intent(in) :: process
    integer :: process_index

    if (.not. process%valid) return
    do process_index = 1, process_count
      if (.not. merge_target_matches(processes(process_index), process)) cycle
      call merge_gpu_process_values(processes(process_index), process)
      return
    end do

    if (process_count >= size(processes)) return
    process_count = process_count + 1
    processes(process_count) = process
  end subroutine merge_gpu_process

  logical function merge_target_matches(left, right) result(matches)
    type(gpu_process_info), intent(in) :: left
    type(gpu_process_info), intent(in) :: right

    matches = .false.
    if (.not. left%valid .or. .not. right%valid) return
    if (left%pid <= 0 .or. left%pid /= right%pid) return
    if (left%start_time > 0_int64 .and. right%start_time > 0_int64) then
      if (left%start_time /= right%start_time) return
    end if
    matches = .true.
  end function merge_target_matches

  subroutine merge_gpu_process_values(destination, source)
    type(gpu_process_info), intent(inout) :: destination
    type(gpu_process_info), intent(in) :: source

    destination%engine_time_ns = saturating_add_int64(destination%engine_time_ns, source%engine_time_ns)
    if (len_trim(destination%process_name) <= 0 .and. len_trim(source%process_name) > 0) then
      destination%process_name = source%process_name
    end if
    if (len_trim(destination%engine) <= 0) then
      destination%engine = source%engine
    else if (len_trim(source%engine) > 0 .and. trim(destination%engine) /= trim(source%engine)) then
      destination%engine = "mixed"
    end if
    if (source%memory_valid) then
      destination%memory_valid = .true.
      destination%memory_bytes = max(destination%memory_bytes, source%memory_bytes)
    end if
    if (source%busy_percent_valid) then
      destination%busy_percent_valid = .true.
      destination%busy_percent = max(destination%busy_percent, source%busy_percent)
    end if
  end subroutine merge_gpu_process_values

  integer(int64) function saturating_add_int64(left, right) result(value)
    integer(int64), intent(in) :: left
    integer(int64), intent(in) :: right

    if (left < 0_int64 .or. right < 0_int64) then
      value = max(0_int64, left) + max(0_int64, right)
    else if (right > huge(value) - left) then
      value = huge(value)
    else
      value = left + right
    end if
  end function saturating_add_int64

  subroutine sort_gpu_process_table(table)
    type(gpu_process_table), intent(inout) :: table
    type(gpu_process_info) :: item
    integer :: item_index
    integer :: scan_index

    if (.not. allocated(table%processes)) return
    do item_index = 2, size(table%processes)
      item = table%processes(item_index)
      scan_index = item_index - 1
      do while (scan_index >= 1)
        if (.not. gpu_process_less_than(table%processes(scan_index), item)) exit
        table%processes(scan_index + 1) = table%processes(scan_index)
        scan_index = scan_index - 1
      end do
      table%processes(scan_index + 1) = item
    end do
  end subroutine sort_gpu_process_table

  logical function gpu_process_less_than(left, right) result(less_than)
    type(gpu_process_info), intent(in) :: left
    type(gpu_process_info), intent(in) :: right

    less_than = .false.
    if (left%valid .neqv. right%valid) then
      less_than = right%valid
      return
    end if
    if (left%busy_percent_valid .and. right%busy_percent_valid) then
      less_than = left%busy_percent < right%busy_percent
      return
    end if
    if (left%busy_percent_valid .neqv. right%busy_percent_valid) then
      less_than = right%busy_percent_valid
      return
    end if
    if (left%engine_time_ns /= right%engine_time_ns) then
      less_than = left%engine_time_ns < right%engine_time_ns
      return
    end if
    less_than = left%pid > right%pid
  end function gpu_process_less_than
end module ftop_gpu_data
