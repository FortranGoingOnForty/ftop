cmake_minimum_required(VERSION 3.25)

project(
  ftop
  VERSION 0.1.0
  LANGUAGES Fortran C
)

set(CMAKE_Fortran_STANDARD 2018)
set(CMAKE_Fortran_STANDARD_REQUIRED ON)
set(CMAKE_Fortran_EXTENSIONS OFF)

if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  set(CMAKE_BUILD_TYPE Debug CACHE STRING "Build type" FORCE)
endif()

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib")

set(THREADS_PREFER_PTHREAD_FLAG ON)
find_package(Threads REQUIRED)

set(FTOP_PLATFORM_LIBS)

set(FTOP_FGOF_ROOT "${CMAKE_CURRENT_LIST_DIR}/.." CACHE PATH "Path to the FortranGoingOnForty workspace containing fgof-* repos")

function(ftop_add_fgof_dependency repo_name target_name)
  if(TARGET "${target_name}")
    return()
  endif()

  set(dependency_source "${FTOP_FGOF_ROOT}/${repo_name}")
  if(NOT EXISTS "${dependency_source}/CMakeLists.txt")
    message(FATAL_ERROR "Missing ${repo_name} CMake project at ${dependency_source}")
  endif()

  add_subdirectory("${dependency_source}" "${CMAKE_BINARY_DIR}/_deps/${repo_name}")
endfunction()

ftop_add_fgof_dependency(fgof-temp fgof::temp)
ftop_add_fgof_dependency(fgof-pty fgof::pty)
ftop_add_fgof_dependency(fgof-screen fgof::screen)
ftop_add_fgof_dependency(fgof-termios fgof::termios)
ftop_add_fgof_dependency(fgof-keys fgof::keys)
ftop_add_fgof_dependency(fgof-lineedit fgof::lineedit)
ftop_add_fgof_dependency(fgof-fs fgof::fs)
ftop_add_fgof_dependency(fgof-process fgof::process)
ftop_add_fgof_dependency(fgof-watch fgof::watch)
ftop_add_fgof_dependency(fgof-state fgof::state)
ftop_add_fgof_dependency(fgof-cache fgof::cache)
ftop_add_fgof_dependency(fgof-expect fgof::expect)

if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
  set(FTOP_PLATFORM "linux")
  set(FTOP_PLATFORM_SOURCES
    src/platform/ftop_platform_types.f90
    src/platform/linux/ftop_linux.f90
    src/platform/linux/ftop_linux_shim.c
  )
elseif(CMAKE_SYSTEM_NAME STREQUAL "FreeBSD")
  set(FTOP_PLATFORM "freebsd")
  set(FTOP_PLATFORM_SOURCES
    src/platform/ftop_platform_types.f90
    src/platform/freebsd/ftop_freebsd.f90
    src/platform/freebsd/ftop_freebsd_shim.c
  )
  list(APPEND FTOP_PLATFORM_LIBS devstat kvm)
elseif(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  set(FTOP_PLATFORM "macos")
  set(FTOP_PLATFORM_SOURCES
    src/platform/ftop_platform_types.f90
    src/platform/macos/ftop_macos.f90
    src/platform/macos/ftop_macos_shim.c
  )
else()
  message(FATAL_ERROR "Unsupported platform: ${CMAKE_SYSTEM_NAME}")
endif()

add_library(
  ftop_core STATIC
  src/shim/ftop_dl.f90
  src/shim/ftop_pthread.f90
  src/shim/ftop_signal.f90
  src/shim/ftop_terminal_io.f90
  src/ftop_app.f90
  src/shim/ftop_dl.c
  src/shim/ftop_pthread.c
  src/shim/ftop_signal.c
  src/shim/ftop_terminal_posix.c
  ${FTOP_PLATFORM_SOURCES}
)

set_target_properties(ftop_core PROPERTIES
  Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/mod/ftop_core"
)

target_include_directories(ftop_core PUBLIC
  $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/mod/ftop_core>
)

target_link_libraries(
  ftop_core
  PUBLIC
    Threads::Threads
    ${CMAKE_DL_LIBS}
    fgof::screen
    fgof::termios
    fgof::keys
    fgof::state
    fgof::fs
    fgof::process
    fgof::watch
    fgof::lineedit
    fgof::cache
    fgof::expect
    ${FTOP_PLATFORM_LIBS}
)

target_compile_options(
  ftop_core
  PRIVATE
    $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-std=f2018 -Wall -Wextra -Wpedantic -Werror>
    $<$<COMPILE_LANG_AND_ID:C,GNU,Clang,AppleClang>:-Wall -Wextra -Wpedantic -Werror>
)

target_compile_definitions(
  ftop_core
  PRIVATE
    FTOP_PLATFORM_${FTOP_PLATFORM}=1
)

add_executable(ftop src/ftop.f90)
target_link_libraries(ftop PRIVATE ftop_core)

target_compile_options(
  ftop
  PRIVATE
    $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-std=f2018 -Wall -Wextra -Wpedantic -Werror>
)

include(CTest)
if(BUILD_TESTING)
  add_subdirectory(test)
endif()
