add_test(NAME ftop_version COMMAND $<TARGET_FILE:ftop> --version)
set_tests_properties(
  ftop_version
  PROPERTIES
    PASS_REGULAR_EXPRESSION "ftop 0\\.1\\.0"
)

add_executable(ftop_terminal_frame unit/test_terminal_frame.f90)
target_link_libraries(ftop_terminal_frame PRIVATE ftop_core)

add_test(NAME ftop_terminal_frame COMMAND ftop_terminal_frame)
