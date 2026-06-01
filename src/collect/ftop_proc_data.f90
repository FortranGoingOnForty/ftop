module ftop_proc_data
  use, intrinsic :: iso_fortran_env, only : int64, real64
  implicit none
  private

  integer, parameter, public :: PROCESS_NAME_LEN = 64
  integer, parameter, public :: PROCESS_USER_LEN = 32
  integer, parameter, public :: PROCESS_COMMAND_LEN = 256
  integer, parameter, public :: PROCESS_STATE_LEN = 16
  integer, parameter, public :: PROCESS_CGROUP_LEN = 128

  type, public :: process_info
    logical :: valid = .false.
    integer :: pid = 0
    integer :: ppid = 0
    integer :: uid = 0
    logical :: user_valid = .false.
    character(len=PROCESS_USER_LEN) :: user = ""
    character(len=PROCESS_NAME_LEN) :: name = ""
    character(len=PROCESS_COMMAND_LEN) :: command = ""
    character(len=PROCESS_STATE_LEN) :: state = ""
    real(real64) :: cpu_percent = 0.0_real64
    real(real64) :: mem_percent = 0.0_real64
    integer(int64) :: mem_rss_bytes = 0_int64
    integer(int64) :: mem_virt_bytes = 0_int64
    integer :: threads = 0
    integer :: nice = 0
    integer :: priority = 0
    integer(int64) :: io_read_bytes = 0_int64
    integer(int64) :: io_write_bytes = 0_int64
    integer(int64) :: start_time = 0_int64
    integer(int64) :: cpu_time = 0_int64
    character(len=PROCESS_CGROUP_LEN) :: cgroup = ""
    integer :: jid = 0
  end type process_info

  type, public :: process_table
    logical :: valid = .false.
    type(process_info), allocatable :: items(:)
  end type process_table

  public :: process_count
  public :: process_display_command
  public :: process_state_label
  public :: process_user_label

contains

  integer function process_count(table) result(count)
    type(process_table), intent(in) :: table

    count = 0
    if (allocated(table%items)) count = size(table%items)
  end function process_count

  function process_display_command(process) result(text)
    type(process_info), intent(in) :: process
    character(len=:), allocatable :: text

    if (len_trim(process%command) > 0) then
      text = trim(process%command)
    else if (len_trim(process%name) > 0) then
      text = trim(process%name)
    else
      text = "[unknown]"
    end if
  end function process_display_command

  function process_user_label(process) result(text)
    type(process_info), intent(in) :: process
    character(len=:), allocatable :: text

    if (process%user_valid .and. len_trim(process%user) > 0) then
      text = trim(process%user)
    else
      text = integer_text(process%uid)
    end if
  end function process_user_label

  function process_state_label(process) result(text)
    type(process_info), intent(in) :: process
    character(len=:), allocatable :: text

    if (len_trim(process%state) > 0) then
      text = trim(process%state)
    else
      text = "?"
    end if
  end function process_state_label

  function integer_text(value) result(text)
    integer, intent(in) :: value
    character(len=:), allocatable :: text
    character(len=32) :: scratch

    write(scratch, '(i0)') value
    text = trim(scratch)
  end function integer_text

end module ftop_proc_data
