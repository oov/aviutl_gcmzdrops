option(JANSSON_WITHOUT_TESTS "" ON)
option(JANSSON_BUILD_DOCS "" OFF)
set(CMAKE_REQUIRED_LINK_OPTIONS -no-pthread)
add_subdirectory(3rd/jansson)

add_custom_target(generate_version_h COMMAND
  ${CMAKE_COMMAND}
  -Dlocal_dir="${CMAKE_CURRENT_SOURCE_DIR}"
  -Dinput_file="${CMAKE_CURRENT_SOURCE_DIR}/version.h.in"
  -Doutput_file="${CMAKE_CURRENT_BINARY_DIR}/version.h"
  -P "${CMAKE_CURRENT_SOURCE_DIR}/../cmake/version.cmake"
)

find_program(LUA51DLL lua51.dll REQUIRED CMAKE_FIND_ROOT_PATH_BOTH)
add_custom_target(generate_lua51dl COMMAND
  ${CMAKE_COMMAND}
  -Dlocal_dir="${CMAKE_CURRENT_BINARY_DIR}"
  -Ddll="${LUA51DLL}"
  -Ddef="${CMAKE_CURRENT_BINARY_DIR}/lua51.def"
  -Dlib="${CMAKE_CURRENT_BINARY_DIR}/liblua51.a"
  -Ddelayed_lib="${CMAKE_CURRENT_BINARY_DIR}/liblua51dl.a"
  -P "${CMAKE_CURRENT_SOURCE_DIR}/../cmake/makelib.cmake"
)

find_program(CLANG_FORMAT_EXE clang-format)
file(GLOB sources "*.c" "*.h")
add_custom_target(${PROJECT_NAME}-format ALL
  COMMAND ${CLANG_FORMAT_EXE} -style=file -i ${sources}
)

option(LEAK_DETECTOR "" ON)
option(ALLOCATE_LOGGER "" ON)
add_subdirectory(3rd/base.c)

include_directories(
  "${CMAKE_CURRENT_BINARY_DIR}/3rd/jansson/include"
  "${CMAKE_CURRENT_BINARY_DIR}" # for version.h
)
link_directories(
  "${CMAKE_CURRENT_BINARY_DIR}/3rd/jansson/lib"
  "${CMAKE_CURRENT_BINARY_DIR}" # for lua51dl.a
)
link_libraries(
  base
  comctl32
  shlwapi
  lua51dl
  jansson
)
add_compile_definitions(
  __STDC_NO_THREADS__
  _WIN32_WINNT=0x0502
  _WINDOWS
  USE_STR
  USE_WSTR
  $<$<BOOL:${LEAK_DETECTOR}>:LEAK_DETECTOR>
  $<$<BOOL:${ALLOCATE_LOGGER}>:ALLOCATE_LOGGER>
  $<$<CONFIG:Release>:NDEBUG>
)
add_compile_options(
  --rtlib=compiler-rt
  -mstackrealign
  -Wall
  -Wextra
  -Werror=return-type
  -pedantic-errors
  -ffunction-sections
  -fdata-sections
  $<$<CONFIG:Debug>:-O0>
  $<$<CONFIG:Release>:-O2>
)
add_link_options(
  -Wl,--gc-sections
  # -Wl,--print-gc-sections
  --rtlib=compiler-rt
  -Wl,--kill-at
  -no-pthread
  -static
  $<$<CONFIG:Release>:-s>
)

add_library(gcmzdrops_auf SHARED
  3rd/base.c/base.c
  3rd/crc64/crc64.c
  3rd/detect/detect.c
  api.c
  aviutl.c
  datauri.c
  droptarget.c
  error_gcmz.c
  files.c
  gcmz.rc
  gcmzdrops.c
  gcmzfuncs.c
  gui.c
  lua.c
  luafuncs.c
  main.c
  scpopup.c
  sniffer.c
  task.c
  util.c
)
set_target_properties(gcmzdrops_auf PROPERTIES
  OUTPUT_NAME "GCMZDrops.auf"
  PREFIX ""
  SUFFIX ""
  RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
)
add_dependencies(gcmzdrops_auf generate_version_h generate_lua51dl)

file(COPY "${CMAKE_CURRENT_SOURCE_DIR}/../lua/" DESTINATION "${CMAKE_BINARY_DIR}/bin/GCMZDrops/")
file(GENERATE OUTPUT "${CMAKE_BINARY_DIR}/bin/GCMZDrops.txt" INPUT "${CMAKE_CURRENT_SOURCE_DIR}/../../README.md")

add_executable(test_util error_gcmz.c util_test.c)
list(APPEND tests test_util)
add_executable(test_datauri error_gcmz.c util.c sniffer.c datauri_test.c)
list(APPEND tests test_datauri)
add_executable(test_gcmzfuncs error_gcmz.c task.c util.c gcmzfuncs_test.c)
list(APPEND tests test_gcmzfuncs)
add_executable(test_luafuncs error_gcmz.c util.c files.c 3rd/crc64/crc64.c 3rd/detect/detect.c luafuncs_test.c)
list(APPEND tests test_luafuncs)

foreach(target ${tests})
  add_test(NAME ${target} COMMAND ${target})
  target_compile_definitions(${target} PRIVATE
    TEST_IMAGE_DIR=${CMAKE_CURRENT_SOURCE_DIR}/test
  )
endforeach(target)
