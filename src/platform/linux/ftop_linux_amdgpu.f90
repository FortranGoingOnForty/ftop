module ftop_linux_amdgpu
  use, intrinsic :: iso_c_binding, only : c_char, c_int, c_null_char, c_size_t
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use ftop_gpu_data, only : GPU_CAPACITY, empty_gpu_table, gpu_info, gpu_table
  implicit none
  private

  integer, parameter :: LINUX_DRM_CARD_NAME_LEN = 32
  integer, parameter :: LINUX_DRM_DEVICE_PATH_LEN = 512
  integer, parameter :: LINUX_GPU_HWMON_NAME_LEN = 128
  integer, parameter :: LINUX_GPU_HWMON_PATH_LEN = 256
  integer, parameter :: LINUX_GPU_HWMON_CAPACITY = 8
  integer, parameter :: LINUX_SYSFS_BUFFER_LEN = 4096

  type, bind(C) :: linux_drm_card
    character(kind=c_char) :: name(LINUX_DRM_CARD_NAME_LEN)
    character(kind=c_char) :: device_path(LINUX_DRM_DEVICE_PATH_LEN)
  end type linux_drm_card

  type, bind(C) :: linux_gpu_hwmon_sensor
    character(kind=c_char) :: path(LINUX_GPU_HWMON_PATH_LEN)
    character(kind=c_char) :: name(LINUX_GPU_HWMON_NAME_LEN)
  end type linux_gpu_hwmon_sensor

  public :: linux_amdgpu_active_dpm_clock_mhz
  public :: linux_amdgpu_snapshot

  interface
    integer(c_int) function c_ftop_linux_drm_card_discover(root, cards, capacity, card_count, sys_errno) &
        bind(C, name="ftop_linux_drm_card_discover")
      import :: c_char, c_int, c_size_t, linux_drm_card
      character(kind=c_char), intent(in) :: root(*)
      type(linux_drm_card), intent(out) :: cards(*)
      integer(c_size_t), value :: capacity
      integer(c_size_t), intent(out) :: card_count
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_drm_card_discover

    integer(c_int) function c_ftop_linux_hwmon_discover_at(root, sensors, capacity, sensor_count, sys_errno) &
        bind(C, name="ftop_linux_hwmon_discover_at")
      import :: c_char, c_int, c_size_t, linux_gpu_hwmon_sensor
      character(kind=c_char), intent(in) :: root(*)
      type(linux_gpu_hwmon_sensor), intent(out) :: sensors(*)
      integer(c_size_t), value :: capacity
      integer(c_size_t), intent(out) :: sensor_count
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_hwmon_discover_at

    integer(c_int) function c_ftop_linux_read_sysfs_file(path, buffer, buffer_capacity, value_len, sys_errno) &
        bind(C, name="ftop_linux_read_sysfs_file")
      import :: c_char, c_int, c_size_t
      character(kind=c_char), intent(in) :: path(*)
      character(kind=c_char), intent(out) :: buffer(*)
      integer(c_size_t), value :: buffer_capacity
      integer(c_size_t), intent(out) :: value_len
      integer(c_int), intent(out) :: sys_errno
    end function c_ftop_linux_read_sysfs_file
  end interface

contains

  logical function linux_amdgpu_snapshot(table, sysfs_root) result(success)
    type(gpu_table), intent(out) :: table
    character(len=*), intent(in), optional :: sysfs_root
    character(kind=c_char), allocatable :: c_root(:)
    character(len=:), allocatable :: root
    type(gpu_info), allocatable :: found_gpus(:)
    type(linux_drm_card) :: cards(GPU_CAPACITY)
    integer(c_size_t) :: c_card_count
    integer(c_int) :: sys_errno
    integer :: card_count
    integer :: card_index
    integer :: gpu_count

    table = empty_gpu_table()
    success = .false.
    root = "/sys/class/drm"
    if (present(sysfs_root)) root = trim(sysfs_root)

    call to_c_string(root, c_root)
    if (c_ftop_linux_drm_card_discover(c_root, cards, int(size(cards), c_size_t), c_card_count, sys_errno) /= 0_c_int) then
      associate(unused_sys_errno => sys_errno)
      end associate
      return
    end if

    card_count = min(int(c_card_count), size(cards))
    if (card_count <= 0) return

    allocate(found_gpus(GPU_CAPACITY))
    gpu_count = 0
    do card_index = 1, card_count
      if (gpu_count >= GPU_CAPACITY) exit
      if (.not. append_amdgpu_card(cards(card_index), found_gpus(gpu_count + 1))) cycle
      gpu_count = gpu_count + 1
    end do
    if (gpu_count <= 0) return

    if (allocated(table%gpus)) deallocate(table%gpus)
    allocate(table%gpus(gpu_count))
    table%gpus = found_gpus(1:gpu_count)
    table%valid = .true.
    success = .true.
  end function linux_amdgpu_snapshot

  logical function append_amdgpu_card(card, gpu) result(success)
    type(linux_drm_card), intent(in) :: card
    type(gpu_info), intent(out) :: gpu
    character(len=LINUX_DRM_DEVICE_PATH_LEN) :: device_path

    gpu = gpu_info()
    call assign_c_string(card%device_path, device_path)
    if (.not. is_amdgpu_device(device_path)) then
      success = .false.
      return
    end if

    gpu%valid = .true.
    gpu%vendor = "amd"
    call read_amdgpu_identity(device_path, gpu)
    call read_amdgpu_utilization(device_path, gpu)
    call read_amdgpu_memory(device_path, gpu)
    call read_amdgpu_clocks(device_path, gpu)
    call read_amdgpu_hwmon(device_path, gpu)
    success = .true.
  end function append_amdgpu_card

  logical function is_amdgpu_device(device_path) result(matches)
    character(len=*), intent(in) :: device_path
    character(len=:), allocatable :: vendor

    matches = .false.
    if (.not. read_sysfs_text(path_join(device_path, "vendor"), vendor)) return
    select case (trim(lower_ascii(first_line(vendor))))
    case ("0x1002", "0x1022")
      matches = .true.
    case default
      matches = .false.
    end select
  end function is_amdgpu_device

  subroutine read_amdgpu_identity(device_path, gpu)
    character(len=*), intent(in) :: device_path
    type(gpu_info), intent(inout) :: gpu
    character(len=:), allocatable :: device_id
    character(len=:), allocatable :: name
    character(len=:), allocatable :: pci_id
    character(len=:), allocatable :: uevent
    logical :: have_device_id
    logical :: have_name

    have_name = read_sysfs_text(path_join(device_path, "product_name"), name)
    if (have_name) then
      if (len_trim(name) > 0) call assign_bounded(name, gpu%name)
    end if
    if (len_trim(gpu%name) <= 0) then
      have_device_id = read_sysfs_text(path_join(device_path, "device"), device_id)
      if (have_device_id) then
        if (len_trim(device_id) > 0) call assign_bounded("AMD " // trim(first_line(device_id)), gpu%name)
      end if
    end if
    if (len_trim(gpu%name) <= 0) then
      gpu%name = "AMD GPU"
    end if

    if (read_sysfs_text(path_join(device_path, "uevent"), uevent)) then
      pci_id = uevent_value(uevent, "PCI_SLOT_NAME=")
      if (len_trim(pci_id) > 0) call assign_bounded(pci_id, gpu%pci_id)
    end if
  end subroutine read_amdgpu_identity

  subroutine read_amdgpu_utilization(device_path, gpu)
    character(len=*), intent(in) :: device_path
    type(gpu_info), intent(inout) :: gpu
    integer(int64) :: value

    if (.not. read_int64_sysfs(path_join(device_path, "gpu_busy_percent"), value)) return
    gpu%utilization_valid = .true.
    gpu%utilization_percent = real(max(0_int64, value), real64)
  end subroutine read_amdgpu_utilization

  subroutine read_amdgpu_memory(device_path, gpu)
    character(len=*), intent(in) :: device_path
    type(gpu_info), intent(inout) :: gpu
    integer(int64) :: total_bytes
    integer(int64) :: used_bytes

    if (.not. read_int64_sysfs(path_join(device_path, "mem_info_vram_total"), total_bytes)) return
    if (.not. read_int64_sysfs(path_join(device_path, "mem_info_vram_used"), used_bytes)) return
    if (total_bytes <= 0_int64) return
    gpu%memory_valid = .true.
    gpu%memory_total_bytes = total_bytes
    gpu%memory_used_bytes = max(0_int64, used_bytes)
  end subroutine read_amdgpu_memory

  subroutine read_amdgpu_clocks(device_path, gpu)
    character(len=*), intent(in) :: device_path
    type(gpu_info), intent(inout) :: gpu
    character(len=:), allocatable :: text
    real(real64) :: mhz

    if (read_sysfs_text(path_join(device_path, "pp_dpm_sclk"), text)) then
      if (linux_amdgpu_active_dpm_clock_mhz(text, mhz)) then
        gpu%core_clock_valid = .true.
        gpu%clock_core_mhz = mhz
      end if
    end if
    if (read_sysfs_text(path_join(device_path, "pp_dpm_mclk"), text)) then
      if (linux_amdgpu_active_dpm_clock_mhz(text, mhz)) then
        gpu%memory_clock_valid = .true.
        gpu%clock_memory_mhz = mhz
      end if
    end if
  end subroutine read_amdgpu_clocks

  subroutine read_amdgpu_hwmon(device_path, gpu)
    character(len=*), intent(in) :: device_path
    type(gpu_info), intent(inout) :: gpu
    character(kind=c_char), allocatable :: c_root(:)
    character(len=LINUX_GPU_HWMON_NAME_LEN) :: sensor_name
    character(len=LINUX_GPU_HWMON_PATH_LEN) :: sensor_path
    type(linux_gpu_hwmon_sensor) :: sensors(LINUX_GPU_HWMON_CAPACITY)
    integer(c_size_t) :: c_sensor_count
    integer(c_int) :: sys_errno
    integer :: sensor_count
    integer :: sensor_index

    call to_c_string(path_join(device_path, "hwmon"), c_root)
    if (c_ftop_linux_hwmon_discover_at(c_root, sensors, int(size(sensors), c_size_t), c_sensor_count, sys_errno) &
        /= 0_c_int) then
      associate(unused_sys_errno => sys_errno)
      end associate
      return
    end if

    sensor_count = min(int(c_sensor_count), size(sensors))
    do sensor_index = 1, sensor_count
      call assign_c_string(sensors(sensor_index)%name, sensor_name)
      if (trim(lower_ascii(sensor_name)) /= "amdgpu") cycle
      call assign_c_string(sensors(sensor_index)%path, sensor_path)
      call read_amdgpu_hwmon_metrics(sensor_path, gpu)
      return
    end do
  end subroutine read_amdgpu_hwmon

  subroutine read_amdgpu_hwmon_metrics(sensor_path, gpu)
    character(len=*), intent(in) :: sensor_path
    type(gpu_info), intent(inout) :: gpu
    integer(int64) :: average_power_uw
    integer(int64) :: cap_power_uw
    integer(int64) :: pwm
    integer(int64) :: pwm_max
    integer(int64) :: temp_millidegrees
    logical :: have_pwm
    logical :: have_pwm_max

    if (read_int64_sysfs(path_join(sensor_path, "temp1_input"), temp_millidegrees)) then
      gpu%temperature_valid = .true.
      gpu%temp_celsius = real(temp_millidegrees, real64) / 1000.0_real64
    end if
    if (read_int64_sysfs(path_join(sensor_path, "power1_average"), average_power_uw)) then
      gpu%power_valid = .true.
      gpu%power_watts = real(max(0_int64, average_power_uw), real64) / 1000000.0_real64
    end if
    if (read_int64_sysfs(path_join(sensor_path, "power1_cap"), cap_power_uw)) then
      gpu%power_limit_watts = real(max(0_int64, cap_power_uw), real64) / 1000000.0_real64
    end if
    have_pwm = read_int64_sysfs(path_join(sensor_path, "pwm1"), pwm)
    have_pwm_max = read_int64_sysfs(path_join(sensor_path, "pwm1_max"), pwm_max)
    if (have_pwm .and. have_pwm_max) then
      if (pwm_max > 0_int64) then
        gpu%fan_valid = .true.
        gpu%fan_speed_percent = 100.0_real64 * real(max(0_int64, pwm), real64) / real(pwm_max, real64)
      end if
    end if
  end subroutine read_amdgpu_hwmon_metrics

  logical function linux_amdgpu_active_dpm_clock_mhz(text, mhz) result(success)
    character(len=*), intent(in) :: text
    real(real64), intent(out) :: mhz
    integer :: line_end
    integer :: line_start
    integer :: star

    success = .false.
    mhz = 0.0_real64
    star = index(text, "*")
    if (star <= 0) return

    line_start = star
    do while (line_start > 1 .and. text(line_start - 1:line_start - 1) /= new_line("a"))
      line_start = line_start - 1
    end do
    line_end = star
    do while (line_end <= len(text) .and. text(line_end:line_end) /= new_line("a"))
      line_end = line_end + 1
    end do
    success = parse_dpm_clock_line(text(line_start:line_end - 1), mhz)
  end function linux_amdgpu_active_dpm_clock_mhz

  logical function parse_dpm_clock_line(line, mhz) result(success)
    character(len=*), intent(in) :: line
    real(real64), intent(out) :: mhz
    character(len=:), allocatable :: lower
    integer :: colon
    integer :: read_status
    integer :: unit

    success = .false.
    mhz = 0.0_real64
    colon = index(line, ":")
    if (colon <= 0) return
    lower = lower_ascii(line)
    unit = index(lower, "mhz")
    if (unit <= colon) return
    read(line(colon + 1:unit - 1), *, iostat=read_status) mhz
    success = read_status == 0 .and. mhz >= 0.0_real64
  end function parse_dpm_clock_line

  logical function read_int64_sysfs(path, value) result(success)
    character(len=*), intent(in) :: path
    integer(int64), intent(out) :: value
    character(len=:), allocatable :: text
    integer :: read_status

    value = 0_int64
    success = read_sysfs_text(path, text)
    if (.not. success) return
    read(text, *, iostat=read_status) value
    success = read_status == 0
  end function read_int64_sysfs

  logical function read_sysfs_text(path, text) result(success)
    character(len=*), intent(in) :: path
    character(len=:), allocatable, intent(out) :: text
    character(kind=c_char), allocatable :: c_path(:)
    character(kind=c_char) :: c_buffer(LINUX_SYSFS_BUFFER_LEN)
    integer(c_size_t) :: value_len
    integer(c_int) :: sys_errno

    c_buffer = c_null_char
    call to_c_string(path, c_path)
    success = c_ftop_linux_read_sysfs_file(c_path, c_buffer, int(size(c_buffer), c_size_t), value_len, sys_errno) == 0_c_int
    if (success) call c_chars_to_string(c_buffer, int(value_len), text)
    associate(unused_sys_errno => sys_errno)
    end associate
  end function read_sysfs_text

  function uevent_value(text, key) result(value)
    character(len=*), intent(in) :: text
    character(len=*), intent(in) :: key
    character(len=:), allocatable :: value
    integer :: finish
    integer :: start

    value = ""
    start = index(text, key)
    if (start <= 0) return
    start = start + len(key)
    finish = start
    do while (finish <= len(text) .and. text(finish:finish) /= new_line("a"))
      finish = finish + 1
    end do
    value = trim(text(start:finish - 1))
  end function uevent_value

  function path_join(base, leaf) result(path)
    character(len=*), intent(in) :: base
    character(len=*), intent(in) :: leaf
    character(len=:), allocatable :: path
    integer :: base_len

    base_len = len_trim(base)
    if (base_len <= 0) then
      path = trim(leaf)
    else if (base(base_len:base_len) == "/") then
      path = base(1:base_len) // trim(leaf)
    else
      path = base(1:base_len) // "/" // trim(leaf)
    end if
  end function path_join

  function lower_ascii(text) result(lower)
    character(len=*), intent(in) :: text
    character(len=:), allocatable :: lower
    integer :: code
    integer :: index_value

    lower = text
    do index_value = 1, len(lower)
      code = iachar(lower(index_value:index_value))
      if (code >= iachar("A") .and. code <= iachar("Z")) then
        lower(index_value:index_value) = achar(code + iachar("a") - iachar("A"))
      end if
    end do
  end function lower_ascii

  subroutine assign_bounded(source, target)
    character(len=*), intent(in) :: source
    character(len=*), intent(out) :: target
    integer :: char_index
    integer :: copy_len

    target = ""
    copy_len = 0
    do char_index = 1, min(len(source), len(target))
      if (source(char_index:char_index) == new_line("a")) exit
      if (source(char_index:char_index) == achar(13)) exit
      copy_len = char_index
    end do
    do while (copy_len > 0 .and. source(copy_len:copy_len) == " ")
      copy_len = copy_len - 1
    end do
    if (copy_len > 0) target(1:copy_len) = source(1:copy_len)
  end subroutine assign_bounded

  function first_line(text) result(line)
    character(len=*), intent(in) :: text
    character(len=:), allocatable :: line
    integer :: finish

    finish = 1
    do while (finish <= len(text))
      if (text(finish:finish) == new_line("a")) exit
      if (text(finish:finish) == achar(13)) exit
      finish = finish + 1
    end do
    line = trim(text(1:finish - 1))
  end function first_line

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

  subroutine c_chars_to_string(c_buffer, value_len, text)
    character(kind=c_char), intent(in) :: c_buffer(:)
    integer, intent(in) :: value_len
    character(len=:), allocatable, intent(out) :: text
    integer :: copied_len
    integer :: index_value

    copied_len = 0
    do index_value = 1, max(0, min(value_len, size(c_buffer)))
      if (c_buffer(index_value) == c_null_char) exit
      copied_len = copied_len + 1
    end do
    allocate(character(len=copied_len) :: text)
    do index_value = 1, copied_len
      text(index_value:index_value) = achar(iachar(c_buffer(index_value)))
    end do
  end subroutine c_chars_to_string

  subroutine to_c_string(text, buffer)
    character(len=*), intent(in) :: text
    character(kind=c_char), allocatable, intent(out) :: buffer(:)
    integer :: index_value
    integer :: text_len

    text_len = len_trim(text)
    allocate(buffer(text_len + 1))
    do index_value = 1, text_len
      buffer(index_value) = char(iachar(text(index_value:index_value)), kind=c_char)
    end do
    buffer(text_len + 1) = c_null_char
  end subroutine to_c_string

end module ftop_linux_amdgpu
