add_test(NAME ftop_version COMMAND $<TARGET_FILE:ftop> --version)
set_tests_properties(
  ftop_version
  PROPERTIES
    PASS_REGULAR_EXPRESSION "ftop 0\\.1\\.0"
)

add_executable(ftop_terminal_frame unit/test_terminal_frame.f90)
target_link_libraries(ftop_terminal_frame PRIVATE ftop_core)

add_test(NAME ftop_terminal_frame COMMAND ftop_terminal_frame)

file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/test-runtime")

add_executable(
  ftop_terminal_smoke
  integration/test_terminal_smoke.f90
  integration/ftop_terminal_test_signals.c
)
target_link_libraries(ftop_terminal_smoke PRIVATE fgof::expect)

add_test(NAME ftop_terminal_smoke COMMAND ftop_terminal_smoke $<TARGET_FILE:ftop>)
set_tests_properties(
  ftop_terminal_smoke
  PROPERTIES
    WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/test-runtime"
)
