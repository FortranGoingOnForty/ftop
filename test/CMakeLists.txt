add_test(NAME ftop_version COMMAND $<TARGET_FILE:ftop> --version)
set_tests_properties(
  ftop_version
  PROPERTIES
    PASS_REGULAR_EXPRESSION "ftop 0\\.1\\.0"
)
