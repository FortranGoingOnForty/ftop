program test_freebsd_kvm
  use ftop_platform, only : &
    freebsd_kvm_close, &
    freebsd_kvm_getprocs, &
    freebsd_kvm_handle, &
    freebsd_kvm_open, &
    freebsd_process_info
  use ftop_signal, only : ftop_current_pid
  implicit none

  type(freebsd_kvm_handle) :: handle
  type(freebsd_process_info), allocatable :: processes(:)
  integer :: current_pid
  integer :: i
  integer :: process_count
  logical :: found_current_process

  allocate(processes(4096))

  if (.not. freebsd_kvm_open(handle)) error stop "kvm_open failed"
  if (.not. freebsd_kvm_getprocs(handle, processes, process_count)) error stop "kvm_getprocs failed"

  if (process_count <= 0) error stop "kvm_getprocs must return processes"

  current_pid = ftop_current_pid()
  found_current_process = .false.
  do i = 1, process_count
    if (processes(i)%pid < 0) error stop "kvm_getprocs returned invalid pid"
    if (processes(i)%pid == current_pid) found_current_process = .true.
  end do
  if (.not. found_current_process) error stop "kvm_getprocs did not include current process"

  if (.not. freebsd_kvm_close(handle)) error stop "kvm_close failed"
end program test_freebsd_kvm
