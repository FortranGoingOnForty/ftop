function(ftop_avoid_linux_gfortran_callback_pie_warning target_name)
  if(FTOP_PLATFORM STREQUAL "linux" AND CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    target_link_options(${target_name} PRIVATE -no-pie)
  endif()
endfunction()

add_test(NAME ftop_version COMMAND $<TARGET_FILE:ftop> --version)
set_tests_properties(
  ftop_version
  PROPERTIES
    PASS_REGULAR_EXPRESSION "ftop 0\\.1\\.0"
)

add_executable(ftop_terminal_frame unit/test_terminal_frame.f90)
target_link_libraries(ftop_terminal_frame PRIVATE ftop_core)

add_test(NAME ftop_terminal_frame COMMAND ftop_terminal_frame)

add_executable(ftop_ring_buffer_test unit/test_ring_buffer.f90)
target_link_libraries(ftop_ring_buffer_test PRIVATE ftop_core)

add_test(NAME ftop_ring_buffer COMMAND ftop_ring_buffer_test)

add_executable(ftop_metric_data_test unit/test_metric_data.f90)
target_link_libraries(ftop_metric_data_test PRIVATE ftop_core)

add_test(NAME ftop_metric_data COMMAND ftop_metric_data_test)

add_executable(ftop_net_data_test unit/test_net_data.f90)
target_link_libraries(ftop_net_data_test PRIVATE ftop_core)

add_test(NAME ftop_net_data COMMAND ftop_net_data_test)

add_executable(ftop_proc_data_test unit/test_proc_data.f90)
target_link_libraries(ftop_proc_data_test PRIVATE ftop_core)

add_test(NAME ftop_proc_data COMMAND ftop_proc_data_test)

add_executable(ftop_process_stress_test unit/test_process_stress.f90 unit/ftop_process_stress.c)
target_link_libraries(ftop_process_stress_test PRIVATE ftop_core)

add_test(NAME ftop_process_stress COMMAND ftop_process_stress_test)
set_tests_properties(ftop_process_stress PROPERTIES TIMEOUT 30)

add_executable(ftop_color_test unit/test_color.f90)
target_link_libraries(ftop_color_test PRIVATE ftop_core)

add_test(NAME ftop_color COMMAND ftop_color_test)

add_executable(ftop_widgets_test unit/test_widgets.f90)
target_link_libraries(ftop_widgets_test PRIVATE ftop_core)

add_test(NAME ftop_widgets COMMAND ftop_widgets_test)

add_executable(ftop_layout_test unit/test_layout.f90)
target_link_libraries(ftop_layout_test PRIVATE ftop_core)

add_test(NAME ftop_layout COMMAND ftop_layout_test "${CMAKE_SOURCE_DIR}/config")

add_executable(ftop_meter_test unit/test_meter.f90)
target_link_libraries(ftop_meter_test PRIVATE ftop_core)

add_test(NAME ftop_meter COMMAND ftop_meter_test)

add_executable(ftop_memory_test unit/test_memory.f90)
target_link_libraries(ftop_memory_test PRIVATE ftop_core)

add_test(NAME ftop_memory COMMAND ftop_memory_test)

add_executable(ftop_disk_test unit/test_disk.f90)
target_link_libraries(ftop_disk_test PRIVATE ftop_core)

add_test(NAME ftop_disk COMMAND ftop_disk_test)

add_executable(ftop_sparkline_test unit/test_sparkline.f90)
target_link_libraries(ftop_sparkline_test PRIVATE ftop_core)

add_test(NAME ftop_sparkline COMMAND ftop_sparkline_test)

add_executable(ftop_graph_test unit/test_graph.f90)
target_link_libraries(ftop_graph_test PRIVATE ftop_core)

add_test(NAME ftop_graph COMMAND ftop_graph_test)

add_executable(ftop_dashboard_test unit/test_dashboard.f90)
target_link_libraries(ftop_dashboard_test PRIVATE ftop_core)

add_test(NAME ftop_dashboard COMMAND ftop_dashboard_test)

add_executable(ftop_help_test unit/test_help.f90)
target_link_libraries(ftop_help_test PRIVATE ftop_core)

add_test(NAME ftop_help COMMAND ftop_help_test)

add_executable(ftop_dashboard_snapshots_test unit/test_dashboard_snapshots.f90)
target_link_libraries(ftop_dashboard_snapshots_test PRIVATE ftop_core)
add_test(
  NAME ftop_dashboard_snapshots
  COMMAND ftop_dashboard_snapshots_test "${CMAKE_CURRENT_SOURCE_DIR}/golden"
)

add_executable(ftop_app_batch_signal_test unit/test_app_batch_signal.f90)
target_link_libraries(ftop_app_batch_signal_test PRIVATE ftop_core)

add_test(NAME ftop_app_batch_signal COMMAND ftop_app_batch_signal_test)

add_executable(ftop_log_test unit/test_log.f90 unit/ftop_log_test.c)
target_link_libraries(ftop_log_test PRIVATE ftop_core)

add_test(NAME ftop_log COMMAND ftop_log_test)

add_executable(ftop_table_test unit/test_table.f90)
target_link_libraries(ftop_table_test PRIVATE ftop_core)

add_test(NAME ftop_table COMMAND ftop_table_test)

add_executable(ftop_process_table_test unit/test_process_table.f90)
target_link_libraries(ftop_process_table_test PRIVATE ftop_core)

add_test(NAME ftop_process_table COMMAND ftop_process_table_test)

add_executable(ftop_widget_snapshots_test unit/test_widget_snapshots.f90)
target_link_libraries(ftop_widget_snapshots_test PRIVATE ftop_core)
add_test(
  NAME ftop_widget_snapshots
  COMMAND ftop_widget_snapshots_test "${CMAKE_CURRENT_SOURCE_DIR}/golden"
)

add_executable(ftop_linux_proc_stat_test unit/test_linux_proc_stat.f90)
target_link_libraries(ftop_linux_proc_stat_test PRIVATE ftop_core)

add_test(NAME ftop_linux_proc_stat COMMAND ftop_linux_proc_stat_test)

add_executable(ftop_linux_meminfo_test unit/test_linux_meminfo.f90)
target_link_libraries(ftop_linux_meminfo_test PRIVATE ftop_core)

add_test(NAME ftop_linux_meminfo COMMAND ftop_linux_meminfo_test)

add_executable(ftop_linux_loadavg_test unit/test_linux_loadavg.f90)
target_link_libraries(ftop_linux_loadavg_test PRIVATE ftop_core)

add_test(NAME ftop_linux_loadavg COMMAND ftop_linux_loadavg_test)

add_executable(ftop_collector_test unit/test_collector.f90)
target_link_libraries(ftop_collector_test PRIVATE ftop_core)
ftop_avoid_linux_gfortran_callback_pie_warning(ftop_collector_test)

add_test(NAME ftop_collector COMMAND ftop_collector_test)
set_tests_properties(ftop_collector PROPERTIES TIMEOUT 20)

add_executable(ftop_pthread_test unit/test_pthread.f90)
target_link_libraries(ftop_pthread_test PRIVATE ftop_core)
ftop_avoid_linux_gfortran_callback_pie_warning(ftop_pthread_test)

add_test(NAME ftop_pthread COMMAND ftop_pthread_test)
set_tests_properties(ftop_pthread PROPERTIES TIMEOUT 5)

add_executable(ftop_platform_test unit/test_platform.f90)
target_link_libraries(ftop_platform_test PRIVATE ftop_core)

add_test(NAME ftop_platform COMMAND ftop_platform_test)

add_executable(ftop_metric_accuracy_test unit/test_metric_accuracy.f90 unit/ftop_metric_accuracy.c unit/ftop_process_stress.c)
target_link_libraries(ftop_metric_accuracy_test PRIVATE ftop_core)
target_compile_definitions(ftop_metric_accuracy_test PRIVATE FTOP_PLATFORM_${FTOP_PLATFORM}=1)

add_test(NAME ftop_metric_accuracy COMMAND ftop_metric_accuracy_test)
set_tests_properties(ftop_metric_accuracy PROPERTIES TIMEOUT 60)

if(FTOP_PLATFORM STREQUAL "freebsd")
  add_executable(ftop_freebsd_sysctl_test unit/test_freebsd_sysctl.f90)
  target_link_libraries(ftop_freebsd_sysctl_test PRIVATE ftop_core)

  add_test(NAME ftop_freebsd_sysctl COMMAND ftop_freebsd_sysctl_test)

  add_executable(ftop_freebsd_kvm_test unit/test_freebsd_kvm.f90)
  target_link_libraries(ftop_freebsd_kvm_test PRIVATE ftop_core)

  add_test(NAME ftop_freebsd_kvm COMMAND ftop_freebsd_kvm_test)

  add_executable(ftop_freebsd_devstat_test unit/test_freebsd_devstat.f90)
  target_link_libraries(ftop_freebsd_devstat_test PRIVATE ftop_core)

  add_test(NAME ftop_freebsd_devstat COMMAND ftop_freebsd_devstat_test)

  add_executable(ftop_freebsd_network_test unit/test_freebsd_network.f90)
  target_link_libraries(ftop_freebsd_network_test PRIVATE ftop_core)

  add_test(NAME ftop_freebsd_network COMMAND ftop_freebsd_network_test)
endif()

if(FTOP_PLATFORM STREQUAL "linux")
  add_executable(ftop_linux_platform_test unit/test_linux_platform.f90 unit/ftop_linux_network.c)
  target_link_libraries(ftop_linux_platform_test PRIVATE ftop_core)

  add_test(NAME ftop_linux_platform COMMAND ftop_linux_platform_test)
endif()

if(FTOP_PLATFORM STREQUAL "macos")
  add_executable(ftop_macos_platform_test unit/test_macos_platform.f90)
  target_link_libraries(ftop_macos_platform_test PRIVATE ftop_core)

  add_test(NAME ftop_macos_platform COMMAND ftop_macos_platform_test)
endif()

add_executable(ftop_threaded_platform_test unit/test_threaded_platform.f90)
target_link_libraries(ftop_threaded_platform_test PRIVATE ftop_core)
ftop_avoid_linux_gfortran_callback_pie_warning(ftop_threaded_platform_test)

add_test(NAME ftop_threaded_platform COMMAND ftop_threaded_platform_test)
set_tests_properties(ftop_threaded_platform PROPERTIES TIMEOUT 5)

add_library(ftop_dl_fixture SHARED unit/ftop_dl_fixture.c)

add_executable(ftop_dl_test unit/test_dl.f90)
target_link_libraries(ftop_dl_test PRIVATE ftop_core)

add_test(NAME ftop_dl COMMAND ftop_dl_test $<TARGET_FILE:ftop_dl_fixture>)

add_executable(ftop_signal_test unit/test_signal.f90)
target_link_libraries(ftop_signal_test PRIVATE ftop_core)

add_test(NAME ftop_signal COMMAND ftop_signal_test)
set_tests_properties(ftop_signal PROPERTIES TIMEOUT 5)

file(MAKE_DIRECTORY
  "${CMAKE_BINARY_DIR}/test-runtime"
  "${CMAKE_BINARY_DIR}/test-runtime/home"
  "${CMAKE_BINARY_DIR}/test-runtime/xdg-config"
)
file(COPY "${CMAKE_SOURCE_DIR}/config" DESTINATION "${CMAKE_BINARY_DIR}/test-runtime")

add_executable(
  ftop_terminal_smoke
  integration/test_terminal_smoke.f90
  integration/ftop_terminal_test_signals.c
)
target_link_libraries(ftop_terminal_smoke PRIVATE fgof::expect)

add_test(
  NAME ftop_terminal_smoke
  COMMAND ftop_terminal_smoke $<TARGET_FILE:ftop> "${CMAKE_SOURCE_DIR}/config/process-focused.toml"
)
set_tests_properties(
  ftop_terminal_smoke
  PROPERTIES
    WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/test-runtime"
    ENVIRONMENT "HOME=${CMAKE_BINARY_DIR}/test-runtime/home;XDG_CONFIG_HOME=${CMAKE_BINARY_DIR}/test-runtime/xdg-config"
)
