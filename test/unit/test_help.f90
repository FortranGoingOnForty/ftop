program test_help
  use fgof_screen, only : allocate_screen
  use fgof_screen_types, only : screen_buffer
  use ftop_help, only : render_help_overlay
  implicit none

  call test_help_overlay_renders()

contains

  subroutine test_help_overlay_renders()
    type(screen_buffer) :: buffer
    character(len=:), allocatable :: text

    buffer = allocate_screen(80, 24)
    call render_help_overlay(buffer, "process")
    text = buffer_text(buffer)

    call require(index(text, "ftop help") > 0, "help overlay should render title")
    call require(index(text, "Global") > 0, "help overlay should render global section")
    call require(index(text, "[ Process ]") > 0, "help overlay should highlight process section")
    call require(index(text, "F2: signal") > 0, "help overlay should render migrated process bindings")
    call require(index(text, "Network") > 0, "help overlay should render network section")
    call require(index(text, "Press ? or Escape to close") > 0, "help overlay should render close hint")
  end subroutine test_help_overlay_renders

  function buffer_text(buffer) result(text)
    type(screen_buffer), intent(in) :: buffer
    character(len=:), allocatable :: text
    integer :: row

    text = ""
    do row = 1, buffer%size%height
      text = text // row_text(buffer, row) // new_line("a")
    end do
  end function buffer_text

  function row_text(buffer, row) result(text)
    type(screen_buffer), intent(in) :: buffer
    integer, intent(in) :: row
    character(len=:), allocatable :: text
    integer :: col

    text = ""
    do col = 1, buffer%size%width
      if (allocated(buffer%cells(row, col)%glyph)) then
        text = text // buffer%cells(row, col)%glyph
      else
        text = text // " "
      end if
    end do
  end function row_text

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) error stop message
  end subroutine require

end program test_help
