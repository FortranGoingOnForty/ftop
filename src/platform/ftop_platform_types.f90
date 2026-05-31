module ftop_platform_types
  implicit none
  private

  type, abstract, public :: platform_backend
  contains
    procedure(get_cpu_count_interface), deferred :: get_cpu_count
  end type platform_backend

  abstract interface
    integer function get_cpu_count_interface(self) result(count)
      import :: platform_backend
      class(platform_backend), intent(in) :: self
    end function get_cpu_count_interface
  end interface

end module ftop_platform_types
