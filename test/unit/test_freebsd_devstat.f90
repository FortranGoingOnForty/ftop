program test_freebsd_devstat
  use, intrinsic :: iso_c_binding, only : c_long_long, c_null_char
  use, intrinsic :: iso_fortran_env, only : int64
  use ftop_platform, only : freebsd_devstat_getdevs, freebsd_devstat_info
  implicit none

  type(freebsd_devstat_info), allocatable :: devices(:)
  integer :: device_count
  integer :: i
  integer(int64) :: generation

  allocate(devices(1024))

  if (.not. freebsd_devstat_getdevs(devices, device_count, generation)) error stop "devstat_getdevs failed"
  if (device_count <= 0) error stop "devstat_getdevs must return devices"
  if (generation < 0_int64) error stop "devstat generation must not be negative"

  do i = 1, device_count
    if (devices(i)%name(1) == c_null_char) error stop "devstat device name must not be empty"
    if (devices(i)%bytes_read < 0_c_long_long) error stop "devstat read bytes must not be negative"
    if (devices(i)%bytes_written < 0_c_long_long) error stop "devstat write bytes must not be negative"
    if (devices(i)%transfers_read < 0_c_long_long) error stop "devstat read transfers must not be negative"
    if (devices(i)%transfers_written < 0_c_long_long) error stop "devstat write transfers must not be negative"
  end do
end program test_freebsd_devstat
