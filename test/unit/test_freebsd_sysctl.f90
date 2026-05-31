program test_freebsd_sysctl
  use, intrinsic :: iso_c_binding, only : c_char, c_long
  use ftop_platform, only : &
    freebsd_sysctl_bytes, &
    freebsd_sysctl_int, &
    freebsd_sysctl_long, &
    freebsd_sysctl_string
  implicit none

  character(kind=c_char) :: boot_time(128)
  character(len=64) :: os_type
  integer :: bytes_read
  integer :: cpu_count
  integer :: string_len
  integer(c_long) :: physical_memory

  if (.not. freebsd_sysctl_int("kern.smp.cpus", cpu_count)) error stop "kern.smp.cpus sysctl failed"
  if (cpu_count <= 0) error stop "kern.smp.cpus must be positive"

  if (.not. freebsd_sysctl_long("hw.physmem", physical_memory)) error stop "hw.physmem sysctl failed"
  if (physical_memory <= 0_c_long) error stop "hw.physmem must be positive"

  if (.not. freebsd_sysctl_string("kern.ostype", os_type, string_len)) error stop "kern.ostype sysctl failed"
  if (string_len <= 0) error stop "kern.ostype must not be empty"
  if (index(os_type, "FreeBSD") /= 1) error stop "kern.ostype must start with FreeBSD"

  if (.not. freebsd_sysctl_bytes("kern.boottime", boot_time, bytes_read)) error stop "kern.boottime sysctl failed"
  if (bytes_read <= 0) error stop "kern.boottime must return bytes"
end program test_freebsd_sysctl
