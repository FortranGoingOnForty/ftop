module ftop_linux_loadavg
  use, intrinsic :: iso_fortran_env, only : real64
  use ftop_platform_types, only : load_average_info
  implicit none
  private

  public :: linux_loadavg_parse

contains

  logical function linux_loadavg_parse(buffer, info) result(success)
    character(len=*), intent(in) :: buffer
    type(load_average_info), intent(out) :: info
    integer :: read_status

    info = load_average_info()
    read(buffer, *, iostat=read_status) info%values(1), info%values(2), info%values(3)
    if (read_status /= 0) then
      success = .false.
      return
    end if

    success = all(info%values >= 0.0_real64)
    info%valid = success
    if (.not. success) info%values = 0.0_real64
  end function linux_loadavg_parse

end module ftop_linux_loadavg
