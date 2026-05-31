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
  set(FTOP_PLATFORM_SOURCES)
elseif(CMAKE_SYSTEM_NAME STREQUAL "FreeBSD")
  set(FTOP_PLATFORM "freebsd")
  set(FTOP_PLATFORM_SOURCES)
elseif(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  set(FTOP_PLATFORM "macos")
  set(FTOP_PLATFORM_SOURCES)
else()
  message(FATAL_ERROR "Unsupported platform: ${CMAKE_SYSTEM_NAME}")
endif()

add_executable(ftop src/ftop.f90 ${FTOP_PLATFORM_SOURCES})

target_link_libraries(
  ftop
  PRIVATE
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
)

target_compile_options(
  ftop
  PRIVATE
    $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-std=f2018 -Wall -Wextra -Wpedantic -Werror>
    $<$<COMPILE_LANG_AND_ID:C,GNU,Clang,AppleClang>:-Wall -Wextra -Wpedantic -Werror>
)

target_compile_definitions(
  ftop
  PRIVATE
    FTOP_PLATFORM_${FTOP_PLATFORM}=1
)

include(CTest)
if(BUILD_TESTING)
  add_subdirectory(test)
endif()
