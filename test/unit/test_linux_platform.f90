program test_linux_platform
  use, intrinsic :: iso_c_binding, only : c_null_char
  use ftop_platform, only : &
    linux_cpuinfo_field, &
    linux_cpuinfo_field_count, &
    linux_hwmon_discover, &
    linux_hwmon_sensor
  implicit none

  type(linux_hwmon_sensor), allocatable :: sensors(:)
  character(len=128) :: processor_id
  integer :: i
  integer :: processor_count
  integer :: processor_id_len
  integer :: sensor_count

  if (.not. linux_cpuinfo_field_count("processor", processor_count)) error stop "processor count failed"
  if (processor_count <= 0) error stop "processor count must be positive"

  if (.not. linux_cpuinfo_field("processor", processor_id, processor_id_len)) error stop "processor field failed"
  if (processor_id_len <= 0) error stop "processor field must not be empty"

  allocate(sensors(64))
  if (.not. linux_hwmon_discover(sensors, sensor_count)) error stop "hwmon discovery failed"
  if (sensor_count < 0) error stop "hwmon sensor count must not be negative"

  do i = 1, sensor_count
    if (sensors(i)%path(1) == c_null_char) error stop "hwmon sensor path must not be empty"
    if (sensors(i)%name(1) == c_null_char) error stop "hwmon sensor name must not be empty"
  end do
end program test_linux_platform
