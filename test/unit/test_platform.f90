program test_platform
  use ftop_platform, only : create_platform, platform_backend
  implicit none

  class(platform_backend), allocatable :: backend
  integer :: cpu_count

  backend = create_platform()
  if (.not. allocated(backend)) error stop "platform factory did not allocate a backend"

  cpu_count = backend%get_cpu_count()
  if (cpu_count <= 0) error stop "platform CPU count must be positive"
end program test_platform
