add_test(NAME ftop_version COMMAND $<TARGET_FILE:ftop> --version)
set_tests_properties(
  ftop_version
  PROPERTIES
    PASS_REGULAR_EXPRESSION "ftop 0\\.1\\.0"
)

add_executable(ftop_terminal_frame unit/test_terminal_frame.f90)
target_link_libraries(ftop_terminal_frame PRIVATE ftop_core)

add_test(NAME ftop_terminal_frame COMMAND ftop_terminal_frame)

add_executable(ftop_pthread_test unit/test_pthread.f90)
target_link_libraries(ftop_pthread_test PRIVATE ftop_core)

add_test(NAME ftop_pthread COMMAND ftop_pthread_test)
set_tests_properties(ftop_pthread PROPERTIES TIMEOUT 5)

add_executable(ftop_platform_test unit/test_platform.f90)
target_link_libraries(ftop_platform_test PRIVATE ftop_core)

add_test(NAME ftop_platform COMMAND ftop_platform_test)

add_library(ftop_dl_fixture SHARED unit/ftop_dl_fixture.c)

add_executable(ftop_dl_test unit/test_dl.f90)
target_link_libraries(ftop_dl_test PRIVATE ftop_core)

add_test(NAME ftop_dl COMMAND ftop_dl_test $<TARGET_FILE:ftop_dl_fixture>)

add_executable(ftop_signal_test unit/test_signal.f90)
target_link_libraries(ftop_signal_test PRIVATE ftop_core)

add_test(NAME ftop_signal COMMAND ftop_signal_test)
set_tests_properties(ftop_signal PROPERTIES TIMEOUT 5)

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
