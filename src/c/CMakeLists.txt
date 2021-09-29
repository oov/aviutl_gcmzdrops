set(GCMZ_VERSION v0.4.0)

option(JANSSON_WITHOUT_TESTS "" ON)
option(JANSSON_BUILD_DOCS "" OFF)
set(CMAKE_REQUIRED_LINK_OPTIONS -no-pthread)
add_subdirectory(3rd/jansson)

add_custom_target(generate_version_h COMMAND
  ${CMAKE_COMMAND}
  -Dversion="${GCMZ_VERSION}"
  -Dlocal_dir="${CMAKE_CURRENT_SOURCE_DIR}"
  -Dinput_file="${CMAKE_CURRENT_SOURCE_DIR}/version.h.in"
  -Doutput_file="${CMAKE_CURRENT_BINARY_DIR}/version.h"
  -P "${CMAKE_CURRENT_SOURCE_DIR}/../cmake/version.cmake"
)

add_custom_target(generate_lua51dl COMMAND
  ${CMAKE_COMMAND}
  -Dlocal_dir="${CMAKE_CURRENT_BINARY_DIR}"
  -P "${CMAKE_CURRENT_SOURCE_DIR}/../cmake/lua51dl.cmake"
)

add_library(aviutl_plugin SHARED)
set_target_properties(aviutl_plugin PROPERTIES OUTPUT_NAME "GCMZDrops.auf" PREFIX "" SUFFIX "" RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")
target_sources(aviutl_plugin PRIVATE
  3rd/crc64/crc64.c
  3rd/detect/detect.c
  api.c
  aviutl.c
  datauri.c
  droptarget.c
  error_gcmz.c
  error_win32.c
  files.c
  gcmz.rc
  gcmzdrops.c
  gcmzfuncs.c
  gui.c
  lua.c
  luafuncs.c
  main.c
  base.c
  scpopup.c
  sniffer.c
  task.c
  util.c
)
target_include_directories(aviutl_plugin BEFORE PRIVATE
  "${CMAKE_CURRENT_BINARY_DIR}/3rd/jansson/include"
  "${CMAKE_CURRENT_BINARY_DIR}" # for version.h
)
target_link_directories(aviutl_plugin BEFORE PRIVATE
  "${CMAKE_CURRENT_BINARY_DIR}/3rd/jansson/lib"
  "${CMAKE_CURRENT_BINARY_DIR}" # for lua51dl.a
)
target_link_libraries(aviutl_plugin PRIVATE
  comctl32
  shlwapi
  lua51dl
  jansson
)
file(COPY "${CMAKE_CURRENT_SOURCE_DIR}/../lua/" DESTINATION "${CMAKE_BINARY_DIR}/bin/GCMZDrops/")
file(GENERATE OUTPUT "${CMAKE_BINARY_DIR}/bin/GCMZDrops.txt" INPUT "${CMAKE_CURRENT_SOURCE_DIR}/../../README.md")
add_dependencies(aviutl_plugin generate_version_h generate_lua51dl)
list(APPEND targets aviutl_plugin)

foreach(target ${targets})
  if (("${CMAKE_C_COMPILER_ID}" STREQUAL "GNU") OR ("${CMAKE_C_COMPILER_ID}" STREQUAL "Clang"))
    target_compile_definitions(${target} PRIVATE
      __STDC_NO_THREADS__
      _WIN32_WINNT=0x0502
      _WINDOWS
      USE_STR
      USE_WSTR
      $<$<CONFIG:Release>:NDEBUG>
      $<$<CONFIG:Debug>:LEAK_DETECTOR>
      $<$<CONFIG:Debug>:ALLOCATE_LOGGER>
      # for local testing
      # $<$<CONFIG:Release>:LEAK_DETECTOR>
      # $<$<CONFIG:Release>:ALLOCATE_LOGGER>
    )
    target_compile_options(${target} PRIVATE
      -mstackrealign
      -Wall
      -Wextra
      -Werror=return-type
      -pedantic-errors
      $<$<CONFIG:Debug>:-O0>
      $<$<CONFIG:Release>:-O2>
    )
    target_link_options(${target} PRIVATE
      $<$<CONFIG:Release>:-s>
      -Wl,--kill-at
      -static-libgcc
      -no-pthread
    )
  endif()
endforeach(target)

add_executable(test_base error_win32.c error_gcmz.c base_test.c)
list(APPEND tests test_base)
add_executable(test_util base.c error_win32.c error_gcmz.c util_test.c)
list(APPEND tests test_util)
add_executable(test_datauri base.c error_win32.c error_gcmz.c util.c sniffer.c datauri_test.c)
list(APPEND tests test_datauri)
add_executable(test_gcmzfuncs base.c error_win32.c error_gcmz.c task.c util.c gcmzfuncs_test.c)
list(APPEND tests test_gcmzfuncs)
add_executable(test_luafuncs base.c error_win32.c error_gcmz.c util.c files.c 3rd/crc64/crc64.c 3rd/detect/detect.c luafuncs_test.c)
list(APPEND tests test_luafuncs)

foreach(target ${tests})
  add_test(NAME ${target} COMMAND ${target})
  if (("${CMAKE_C_COMPILER_ID}" STREQUAL "GNU") OR ("${CMAKE_C_COMPILER_ID}" STREQUAL "Clang"))
    target_compile_definitions(${target} PRIVATE
      __STDC_NO_THREADS__
      _WIN32_WINNT=0x0502
      _CONSOLE
      USE_STR
      USE_WSTR
      LEAK_DETECTOR
      ALLOCATE_LOGGER
      TEST_IMAGE_DIR=${CMAKE_CURRENT_SOURCE_DIR}/test
    )
    target_compile_options(${target} PRIVATE
      -mstackrealign
      -Wall
      -Wextra
      -Werror=return-type
      -pedantic-errors
      -O0
    )
    target_link_options(${target} PRIVATE
      -Wl,--kill-at
      -static-libgcc
      -no-pthread
    )
    target_link_libraries(${target} PRIVATE
      shlwapi
      lua5.1
    )
  endif()
endforeach(target)
